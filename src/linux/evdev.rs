//! Linux kernel-level user input support.
//!
//! Evdev provides low-level, mostly-unfiltered data for a much larger set of device types than are exposed by the GUI
//! stack. Normally used for joysticks and gamepads, but with suitable permissions (provided by, for example, a udev
//! rule) it will supply input from such esoterica as laptop lid switches and touchpad pressure.
//!
//! Because it operates below the level of configurable keymaps, this module exposes key presses on keyboards as button
//! events rather than attempting to determine an appropriate symbol and text input.

extern crate evdev;

use std::{io, fmt};
use std::ffi::{CStr, CString, OsStr};
use std::os::unix::io::AsRawFd;
use std::os::unix::ffi::OsStrExt;
use std::collections::{HashMap, VecDeque};
use std::path::{Path, PathBuf};

use void::Void;
use mio;
use tokio_core::reactor::{PollEvented, Handle};
use futures;
use futures::{Poll, Async};
use super::nix;

use {Event, AxisId, ButtonId};

use super::libudev as udev;

const EV_SYN: u16 = 0x00;
const EV_KEY: u16 = 0x01;
const EV_REL: u16 = 0x02;
const EV_ABS: u16 = 0x03;
const EV_MSC: u16 = 0x04;

const REL_MAX: u16 = 0x0f;
const REL_CNT: u16 = REL_MAX + 1;

error_chain! {
    foreign_links {
        Nix(nix::Error);
        Io(io::Error);
    }
}

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
}

impl Device {
    pub fn new(syspath: &Path, handle: &Handle) -> Result<Self> {
        let inner = DeviceInner(evdev::Device::open(&syspath)?);
        let poll = PollEvented::new(inner, handle)?;
        trace!("opened \"{}\"", poll.get_ref().0.name().to_string_lossy());
        Ok(Device { io: poll })
    }
}

fn parse_event(event: evdev::raw::input_event) -> Option<Event> {
    use Event::*;

    trace!("processing event: {:?}", event);

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
        EV_ABS => Some(Motion {
            axis: AxisId((REL_CNT + event.code) as u32),
            value: event.value as f64,
        }),
        EV_REL => Some(Motion {
            axis: AxisId(event.code as u32),
            value: event.value as f64,
        }),
        EV_MSC => None,
        _ => {
            warn!("unrecognized evdev event: type {}, code {}", event._type, event.code);
            None
        },
    }
}


impl futures::stream::Stream for Device {
    type Item = Event;
    type Error = Error;

    fn poll(&mut self) -> Poll<Option<Self::Item>, Self::Error> {
        if Async::NotReady == self.io.poll_read() {
            return Ok(Async::NotReady);
        }
        let e = self.io.get_mut().0.events()?.filter_map(parse_event).next();
        match e {
            Some(x) => Ok(Async::Ready(Some(x))),
            None => {
                self.io.need_read();
                Ok(Async::NotReady)
            }
        }
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
    initial: VecDeque<MonitorEvent>,
}

impl Monitor {
    pub fn new(handle: &Handle) -> io::Result<Self> {
        let udev = try!(udev::Context::new());

        let initial = {
            let mut enumerate = try!(udev::Enumerate::new(&udev));
            try!(enumerate.add_match_subsystem(CStr::from_bytes_with_nul(b"input\0").unwrap()));
            enumerate.into_iter().filter_map(|x| x.devnode().map(|dev| {
                MonitorEvent::Add { sysname: x.sysname().to_owned(), devnode: PathBuf::from(OsStr::from_bytes(dev.to_bytes())) }
            })).collect()
        };
        
        let mut monitor = try!(udev::Monitor::new(udev));
        try!(monitor.filter_add_match_subsystem(CStr::from_bytes_with_nul(b"input\0").unwrap()));
        monitor.enable_receiving();
        let io = try!(PollEvented::new(MonitorInner(monitor), handle));

        Ok(Monitor { io: io, initial: initial })
    }
}

#[derive(Debug, Clone)]
enum MonitorEvent {
    Add { sysname: CString, devnode: PathBuf },
    Remove { sysname: CString },
}

impl futures::stream::Stream for Monitor {
    type Item = MonitorEvent;
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
                    use self::MonitorEvent::*;
                    match device.action().to_bytes() {
                        b"add" => match device.devnode() {
                            None => {},
                            Some(node) => return Ok(Async::Ready(Some(Add {
                                sysname: device.sysname().to_owned(),
                                devnode: PathBuf::from(OsStr::from_bytes(node.to_bytes())),
                            }))),
                        },
                        b"remove" =>
                            return Ok(Async::Ready(Some(Remove { sysname: device.sysname().to_owned() }))),
                        _ => { warn!("unknown libudev action type {:?}", device.action()); },
                    }
                }
            }
        }
    }
}

pub struct Stream {
    monitor: Monitor,
    devices: HashMap<CString, Device>,
    handle: Handle,
}

impl Stream {
    pub fn new(handle: Handle) -> io::Result<Self> {
        let monitor = Monitor::new(&handle)?;
        Ok(Stream { monitor: monitor, devices: HashMap::new(), handle: handle })
    }

    fn open_device(&mut self, sysname: &CStr, syspath: &Path) -> Result<()> {
        trace!("opening {}", syspath.to_string_lossy());
        let d = Device::new(syspath, &self.handle)?;
        self.devices.insert(sysname.to_owned(), d);
        Ok(())
    }
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct DeviceId(CString);

impl fmt::Debug for DeviceId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl futures::stream::Stream for Stream {
    type Item = (DeviceId, Event);
    type Error = Error;

    fn poll(&mut self) -> Poll<Option<Self::Item>, Self::Error> {
        // Loop to ensure that if we ignore a monitor event then we don't inadvertently leave buffered events unread and
        // return NotReady
        loop {
            match self.monitor.poll().unwrap() {
                Async::NotReady => break,
                Async::Ready(None) => panic!("Monitor should be an infinite stream"),
                Async::Ready(Some(MonitorEvent::Add { sysname, devnode })) => match self.open_device(&sysname, &devnode) {
                    Ok(()) => return Ok(Async::Ready(Some((DeviceId(sysname), Event::Added)))),
                    Err(e) => debug!("unable to open {}: {}", devnode.to_string_lossy(), e),
                },
                Async::Ready(Some(MonitorEvent::Remove { sysname })) => match self.devices.remove(&sysname) {
                    Some(_) => return Ok(Async::Ready(Some((DeviceId(sysname), Event::Removed)))),
                    None => (),
                }
            }
        }

        for (sysname, device) in &mut self.devices {
            match device.poll()? {
                Async::NotReady => (),
                Async::Ready(None) => panic!("Device should be an infinite stream"),
                Async::Ready(Some(e)) => return Ok(Async::Ready(Some((DeviceId(sysname.clone()), e)))),
            }
        }

        Ok(Async::NotReady)
    }
}
