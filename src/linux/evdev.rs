//! Linux kernel-level user input support.
//!
//! Evdev provides low-level, mostly-unfiltered data for a much larger set of device types than are exposed by the GUI
//! stack. Normally used for joysticks and gamepads, but with suitable permissions (provided by, for example, a udev
//! rule) it will supply input from such esoterica as laptop lid switches and touchpad pressure.
//!
//! Because it operates below the level of configurable keymaps, this module exposes key presses on keyboards as button
//! events rather than attempting to determine an appropriate symbol and text input.

extern crate evdev;

use std::{io, fmt, str};
use std::ffi::{CStr, OsStr};
use std::os::unix::io::AsRawFd;
use std::os::unix::ffi::OsStrExt;
use std::collections::{HashMap, VecDeque};
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::cell::RefCell;

use void::Void;
use mio;
use tokio_core::reactor::{PollEvented, Handle};
use futures;
use futures::{Poll, Async};
use super::nix;
use self::nix::sys::stat::dev_t;

use {Event, RelAxisId, AbsAxisId, ButtonId, DeviceHwId};

use super::libudev as udev;
use super::helpers::dev_hwid;
use super::{Error, Result};

const EV_SYN: u16 = 0x00;
const EV_KEY: u16 = 0x01;
const EV_REL: u16 = 0x02;
const EV_ABS: u16 = 0x03;
const EV_MSC: u16 = 0x04;

struct DeviceInner(evdev::Device);

impl mio::Evented for DeviceInner {
    fn register(&self, poll: &mio::Poll, token: mio::Token,
                interest: mio::Ready, opts: mio::PollOpt) -> io::Result<()> {
        mio::unix::EventedFd(&self.0.fd()).register(poll, token, interest, opts)
    }

    fn reregister(&self, poll: &mio::Poll, token: mio::Token,
                  interest: mio::Ready, opts: mio::PollOpt) -> io::Result<()> {
        mio::unix::EventedFd(&self.0.fd()).reregister(poll, token, interest, opts)
    }

    fn deregister(&self, poll: &mio::Poll) -> io::Result<()> {
        mio::unix::EventedFd(&self.0.fd()).deregister(poll)
    }
}

struct Device {
    io: PollEvented<DeviceInner>,
    devnum: dev_t,
}

fn map_abs(axis: &evdev::raw::input_absinfo, value: i32) -> f64 {
    if axis.maximum > axis.minimum {
        (value as f64 - axis.minimum as f64) / (axis.maximum as f64 - axis.minimum as f64)
    } else {
        value as f64
    }
}

impl Device {
    pub fn new(devnum: dev_t, syspath: &Path, handle: &Handle) -> Result<Self> {
        let dev = evdev::Device::open(&syspath)?;
        let inner = DeviceInner(dev);
        let poll = PollEvented::new(inner, handle)?;

        trace!("opened \"{}\"", poll.get_ref().0.name().to_string_lossy());
        Ok(Device { devnum: devnum, io: poll })
    }

    fn parse_event(&self, event: evdev::raw::input_event) -> Option<Event> {
        use Event::*;

        let state = self.io.get_ref().0.state();
        match event._type {
            EV_SYN => None,
            EV_KEY => match event.value {
                0 => Some(ButtonRelease {
                    button: ButtonId(event.code as u32),
                }),
                1 => Some(ButtonPress {
                    button: ButtonId(event.code as u32),
                }),
                2 => None,      // Key repeat
                x => {
                    warn!("unrecognised evdev key state: {}", x);
                    None
                },
            },
            // Filter multitouch data
            EV_ABS if event.code >= evdev::ABS_MT_SLOT.number() => None,
            EV_ABS => Some(AbsMotion {
                axis: AbsAxisId(event.code as u32),
                value: map_abs(&state.abs_vals[event.code as usize], event.value),
            }),
            EV_REL => Some(RelMotion {
                axis: RelAxisId(event.code as u32),
                value: event.value as f64,
            }),
            EV_MSC => None,
            _ => {
                warn!("unrecognized evdev event: type {}, code {}", event._type, event.code);
                None
            },
        }
    }
}

impl futures::Stream for Device {
    type Item = Event;
    type Error = Error;

    fn poll(&mut self) -> Poll<Option<Self::Item>, Self::Error> {
        if Async::NotReady == self.io.poll_read() {
            return Ok(Async::NotReady);
        }
        loop {
            let e = match self.io.get_mut().0.events() {
                Ok(mut x) => x.next(),
                Err(nix::Error::Sys(nix::Errno::ENODEV)) => return Ok(Async::Ready(None)),
                Err(e) => return Err(e.into()),
            };
            if let Some(e) = e {
                if let Some(e) = self.parse_event(e) {
                    return Ok(Async::Ready(Some(e)));
                }
            } else {
                break;
            }
        }
        self.io.need_read();
        Ok(Async::NotReady)
    }
}

struct MonitorInner(udev::Monitor);

impl mio::Evented for MonitorInner {
    fn register(&self, poll: &mio::Poll, token: mio::Token,
                interest: mio::Ready, opts: mio::PollOpt) -> io::Result<()> {
        mio::unix::EventedFd(&self.0.as_raw_fd()).register(poll, token, interest, opts)
    }

    fn reregister(&self, poll: &mio::Poll, token: mio::Token,
                  interest: mio::Ready, opts: mio::PollOpt) -> io::Result<()> {
        mio::unix::EventedFd(&self.0.as_raw_fd()).reregister(poll, token, interest, opts)
    }

    fn deregister(&self, poll: &mio::Poll) -> io::Result<()> {
        mio::unix::EventedFd(&self.0.as_raw_fd()).deregister(poll)
    }
}

struct Monitor {
    io: PollEvented<MonitorInner>,
    initial: VecDeque<Hotplug>,
}

impl Monitor {
    pub fn new(handle: &Handle) -> Result<Self> {
        let udev = try!(udev::Context::new());

        let initial = {
            let mut enumerate = try!(udev::Enumerate::new(&udev));
            try!(enumerate.add_match_subsystem(CStr::from_bytes_with_nul(b"input\0").unwrap()));
            enumerate.into_iter()
                .filter_map(|x| {
                    let devnode = x.devnode();
                    let devnum = x.devnum().ok();
                    let id = parse_sysnum(x.sysnum());
                    devnode.and_then(|devnode| devnum.and_then(|devnum| id.map(|id| {
                        Hotplug {
                            id: id,
                            devnum: devnum,
                            devnode: PathBuf::from(OsStr::from_bytes(devnode.to_bytes()))
                        }
                    })))
                }).collect()
        };
        
        let mut monitor = try!(udev::Monitor::new(udev));
        try!(monitor.filter_add_match_subsystem(CStr::from_bytes_with_nul(b"input\0").unwrap()));
        monitor.enable_receiving();
        let io = try!(PollEvented::new(MonitorInner(monitor), handle));

        Ok(Monitor { io: io, initial: initial })
    }
}


#[derive(Debug, Clone)]
struct Hotplug {
    id: DeviceId,
    devnum: dev_t,
    devnode: PathBuf,
}

fn parse_sysnum(sysnum: Option<&CStr>) -> Option<DeviceId> {
    sysnum.and_then(|sysnum| sysnum.to_str().ok().and_then(|sysnum| sysnum.parse().ok()))
}

impl futures::Stream for Monitor {
    type Item = Hotplug;
    type Error = Void;

    fn poll(&mut self) -> Poll<Option<Self::Item>, Self::Error> {
        if let Some(x) = self.initial.pop_front() {
            return Ok(Async::Ready(Some(x)));
        }

        if Async::NotReady == self.io.poll_read() {
            return Ok(Async::NotReady);
        }

        loop {
            match self.io.get_ref().0.receive_device() {
                None => {
                    self.io.need_read();
                    return Ok(Async::NotReady);
                },
                Some(device) => {
                    match device.action().to_bytes() {
                        b"add" => match device.devnode() {
                            None => {},
                            Some(node) => {
                                if let Some(id) = parse_sysnum(device.sysnum()) {
                                    return Ok(Async::Ready(Some(Hotplug {
                                        id: id,
                                        devnum: device.devnum().unwrap(),
                                        devnode: PathBuf::from(OsStr::from_bytes(node.to_bytes())),
                                    })));
                                }
                            }
                        },
                        b"remove" => {} // We detect device removal by ENODEV on read
                        _ => { warn!("unknown libudev action type {:?}", device.action()); },
                    }
                }
            }
        }
    }
}

struct Core {
    monitor: Monitor,
    devices: HashMap<DeviceId, Device>,
    handle: Handle,
    queued: Option<(DeviceId, Vec<Event>)>,
}

impl Core {
    fn new(handle: Handle) -> Result<Self> {
        let monitor = Monitor::new(&handle)?;
        Ok(Core { monitor: monitor, devices: HashMap::new(), handle: handle, queued: None })
    }

    fn open_device(&mut self, id: DeviceId, devnum: dev_t, devnode: &Path) -> Result<()> {
        trace!("opening {}", devnode.to_string_lossy());
        let d = Device::new(devnum, devnode, &self.handle)?;

        {
            use Event::*;
            assert!(self.queued.is_none());
            let d = &d.io.get_ref().0;

            let axes = d.absolute_axes_supported();
            let state = d.state();
            let init = state.key_vals.ones().map(|key| ButtonPress { button: ButtonId(key as u32) })
                .chain(state.abs_vals.iter()
                       .take(evdev::ABS_MT_SLOT.number()) // Filter multitouch data
                       .enumerate()
                       .filter(|&(axis, _)| axes.bits() as usize & (1 << axis) != 0)
                       .map(|(axis, info)| AbsMotion {
                           axis: AbsAxisId(axis as u32),
                           value: map_abs(info, info.value),
                       }))
                .collect();
            self.queued = Some((id, init));
        }

        self.devices.insert(id, d);
        Ok(())
    }

    fn poll(&mut self) -> Poll<Option<(DeviceId, Event)>, Error> {
        if let Some((id, mut es)) = self.queued.take() {
            if let Some(e) = es.pop() {
                self.queued = Some((id, es));
                return Ok(Async::Ready(Some((id, e))));
            }
        }

        // Loop to ensure that if we ignore a monitor event then we don't inadvertently leave buffered events unread and
        // return NotReady
        loop {
            match <Monitor as futures::Stream>::poll(&mut self.monitor).unwrap() {
                Async::NotReady => break,
                Async::Ready(None) => panic!("Monitor should be an infinite stream"),
                Async::Ready(Some(Hotplug { id, devnum, devnode })) => match self.open_device(id, devnum, &devnode) {
                    Ok(()) => return Ok(Async::Ready(Some((id, Event::Added)))),
                    Err(e) => debug!("unable to open {}: {}", devnode.to_string_lossy(), e),
                },
            }
        }

        let mut remove = None;
        for (&id, mut device) in &mut self.devices {
            match <Device as futures::Stream>::poll(&mut device)? {
                Async::NotReady => (),
                Async::Ready(None) => {
                    remove = Some(id);
                    break;
                }
                Async::Ready(Some(e)) => return Ok(Async::Ready(Some((id, e)))),
            }
        }

        if let Some(id) = remove {
            self.devices.remove(&id).unwrap();
            Ok(Async::Ready(Some((id, Event::Removed))))
        } else {
            Ok(Async::NotReady)
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct DeviceId(u32);

impl fmt::Debug for DeviceId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl str::FromStr for DeviceId {
    type Err = ::std::num::ParseIntError;

    fn from_str(s: &str) -> ::std::result::Result<Self, Self::Err> {
        Ok(DeviceId(u32::from_str_radix(s, 10)?))
    }
}

pub struct Context(Rc<RefCell<Core>>);

impl Context {
    pub fn new(handle: Handle) -> Result<(Context, Stream)> {
        let core = Rc::new(RefCell::new(Core::new(handle)?));
        let ctx = Context(core.clone());
        Ok((ctx, Stream(core)))
    }
    
    pub fn device_hw_id(&self, device: DeviceId) -> Option<DeviceHwId> {
        let core = self.0.borrow();
        dev_hwid(&core.monitor.io.get_ref().0, udev::DevType::Character, core.devices[&device].devnum)
        // let id = self.0.borrow().devices[&device].io.get_ref().0.input_id();
        // if id.vendor == 0 { None } else {
        //     Some(DeviceHwId {
        //         vendor: id.vendor,
        //         product: id.product,
        //     })
        // }
    }

    pub fn device_name(&self, device: DeviceId) -> String {
        self.0.borrow().devices[&device].io.get_ref().0.name().to_string_lossy().into_owned()
    }

    pub fn device_port(&self, device: DeviceId) -> Option<String> {
        self.0.borrow().devices[&device].io.get_ref().0.physical_path().as_ref().map(|x| x.to_string_lossy().into_owned())
    }

    pub fn device_scancode_name(&self, _device: DeviceId, _code: ::Scancode) -> String {
        panic!("tried to look up scancode from an evdev device, which emits only buttons")
    }
}

pub struct Stream(Rc<RefCell<Core>>);

impl futures::Stream for Stream {
    type Item = (DeviceId, Event);
    type Error = Error;

    fn poll(&mut self) -> Poll<Option<Self::Item>, Self::Error> {
        self.0.borrow_mut().poll()
    }
}
