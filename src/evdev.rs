//! Linux kernel-level user input support.
//!
//! Evdev provides low-level, mostly-unfiltered data for a much larger set of device types than are exposed by the GUI
//! stack. Normally used for joysticks and gamepads, but with suitable permissions (provided by, for example, a udev
//! rule) it will supply input from such esoterica as laptop lid switches and touchpad pressure.
//!
//! Because it operates below the level of configurable keymaps, this module exposes key presses on keyboards as button
//! events rather than attempting to determine an appropriate symbol and text input.

use std::ffi::{CStr, CString, OsStr};
use std::os::unix::io::AsRawFd;
use std::collections::{HashMap, VecDeque};
use std::io;
use std::borrow::Cow;

use void::Void;
use mio;
use tokio_core::reactor::{PollEvented, Handle};
use futures;
use futures::{Poll, Async};
use libc;

use common;
use common::{DeviceEvent, InputEvent, AxisID, ButtonID};

use super::platform::libudev as udev;
use super::platform::libevdev;
use super::platform::linux_event_codes as codes;

struct DeviceInner(libevdev::Device);

impl Drop for DeviceInner {
    fn drop(&mut self) {
        unsafe { libc::close(self.0.as_raw_fd()); };
    }
}

impl mio::Evented for DeviceInner {
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

struct Device {
    io: PollEvented<DeviceInner>,
    flag: libevdev::ReadFlag,
}

impl Device {
    pub fn new(syspath: &CStr, handle: &Handle) -> io::Result<Self> {
        let fd = unsafe { libc::open(syspath.as_ptr(), libc::O_RDONLY | libc::O_NONBLOCK) };
        if fd == -1 {
            return Err(io::Error::last_os_error());
        }
        let inner = match libevdev::Device::new_from_fd(fd) {
            Err(e) => {
                unsafe { libc::close(fd) };
                return Err(e)
            },
            Ok(x) => DeviceInner(x),
        };
        let poll = PollEvented::new(inner, handle)?;
        trace!("opened \"{}\"", poll.get_ref().0.get_name().to_string_lossy());
        Ok(Device { io: poll, flag: libevdev::READ_FLAG_NORMAL })
    }
}

fn parse_event(event: libevdev::InputEvent) -> Option<InputEvent> {
    use InputEvent::*;
    match event.type_ {
        codes::EV_SYN => None,
        codes::EV_KEY => match event.value {
            0 => Some(ButtonRelease {
                button: ButtonID(event.code as u32),
            }),
            1 => Some(ButtonPress {
                button: ButtonID(event.code as u32),
            }),
            2 => None,      // Key repeat
            x => {
                warn!("unrecognised evdev key state: {}", x);
                None
            },
        },
        codes::EV_ABS => Some(Motion {
            axis: AxisID((codes::REL_CNT + event.code) as u32),
            value: event.value as f64,
        }),
        codes::EV_REL => Some(Motion {
            axis: AxisID(event.code as u32),
            value: event.value as f64,
        }),
        codes::EV_MSC => None,
        ty => {
            warn!("unrecognized evdev event: type {}, code {}",
                  libevdev::event_type_get_name(ty as u32)
                  .map(OsStr::to_string_lossy)
                  .unwrap_or(Cow::Owned(ty.to_string())),
                  libevdev::event_code_get_name(ty as u32, event.code as u32)
                  .map(OsStr::to_string_lossy)
                  .unwrap_or(Cow::Owned(event.code.to_string())));
            None
        },
    }
}


impl futures::stream::Stream for Device {
    type Item = InputEvent;
    type Error = Void;

    fn poll(&mut self) -> Poll<Option<Self::Item>, Self::Error> {
        if Async::NotReady == self.io.poll_read() {
            return Ok(Async::NotReady);
        }
        loop {
            use super::platform::libevdev::ReadStatus::*;
            match self.io.get_ref().0.next_event(self.flag) {
                Again => {
                    self.flag = libevdev::READ_FLAG_NORMAL;
                    self.io.need_read();
                    return Ok(Async::NotReady);
                },
                Sync(e) => {
                    self.flag = libevdev::READ_FLAG_SYNC;
                    if let Some(e) = parse_event(e) {
                        return Ok(Async::Ready(Some(e)));
                    }
                },
                Success(e) => {
                    if let Some(e) = parse_event(e) {
                        return Ok(Async::Ready(Some(e)));
                    }
                },
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
                MonitorEvent::Add { sysname: x.sysname().to_owned(), devnode: dev.to_owned() }
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
    Add { sysname: CString, devnode: CString },
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
                                devnode: node.to_owned(),
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

    fn open_device(&mut self, sysname: &CStr, syspath: &CStr) -> io::Result<()> {
        trace!("opening {}", syspath.to_string_lossy());
        let d = Device::new(syspath, &self.handle)?;
        self.devices.insert(sysname.to_owned(), d);
        Ok(())
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct DeviceID(CString);

impl common::DeviceID for DeviceID {}

impl futures::stream::Stream for Stream {
    type Item = (DeviceID, DeviceEvent);
    type Error = Void;

    fn poll(&mut self) -> Poll<Option<Self::Item>, Self::Error> {
        use DeviceEvent::*;

        // Loop to ensure that if we ignore a monitor event then we don't inadvertently leave buffered events unread and
        // return NotReady
        loop {
            match self.monitor.poll()? {
                Async::NotReady => break,
                Async::Ready(None) => panic!("Monitor should be an infinite stream"),
                Async::Ready(Some(MonitorEvent::Add { sysname, devnode })) => match self.open_device(&sysname, &devnode) {
                    Ok(()) => return Ok(Async::Ready(Some((DeviceID(sysname), DeviceEvent::Added)))),
                    Err(e) => debug!("unable to open {}: {}", devnode.to_string_lossy(), e),
                },
                Async::Ready(Some(MonitorEvent::Remove { sysname })) => match self.devices.remove(&sysname) {
                    Some(_) => return Ok(Async::Ready(Some((DeviceID(sysname), DeviceEvent::Removed)))),
                    None => (),
                }
            }
        }

        for (sysname, device) in &mut self.devices {
            match device.poll()? {
                Async::NotReady => (),
                Async::Ready(None) => panic!("Device should be an infinite stream"),
                Async::Ready(Some(e)) => return Ok(Async::Ready(Some((DeviceID(sysname.clone()), Input(e))))),
            }
        }

        Ok(Async::NotReady)
    }
}
