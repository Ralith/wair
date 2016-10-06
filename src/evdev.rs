extern crate libudev;

use std::error::Error;
use std::ffi::{OsStr, OsString};
use std::os::unix::ffi::OsStrExt;
use std::os::unix::io::AsRawFd;
use std::collections::{HashMap, VecDeque};
use std::hash::{Hash, Hasher};
use std::cell::RefCell;
use std::path::Path;
use std::io;
use std::borrow::Cow;

use mio;
use tokio_core::reactor::{PollEvented, Handle};
use futures;
use void::Void;
use libc;
use libc::c_char;

use common;
use common::{Event, AxisID, ButtonID};

use super::platform::libevdev;
use super::platform::linux_event_codes as codes;

pub struct Context {
    udev: libudev::Context,
}

impl Context {
    pub fn new() -> Result<Self, String> {
        from_result(libudev::Context::new()).map(|x| Context { udev: x })
    }
}

struct UdevStream<'a> {
    monitor: RefCell<libudev::MonitorSocket<'a>>,
}

impl<'a> UdevStream<'a> {
    fn new(ctx: &'a Context) -> libudev::Result<Self> {
        let mut monitor = try!(libudev::Monitor::new(&ctx.udev));
        try!(monitor.match_subsystem("input"));
        let sock = try!(monitor.listen());
        Ok(UdevStream { monitor: RefCell::new(sock) })
    }
}

impl<'a> mio::Evented for UdevStream<'a> {
    fn register(&self, poll: &mio::Poll, token: mio::Token,
                interest: mio::Ready, opts: mio::PollOpt) -> ::std::io::Result<()> {
        mio::unix::EventedFd(&self.monitor.borrow().as_raw_fd()).register(poll, token, interest, opts)
    }

    fn reregister(&self, poll: &mio::Poll, token: mio::Token,
                  interest: mio::Ready, opts: mio::PollOpt) -> ::std::io::Result<()> {
        mio::unix::EventedFd(&self.monitor.borrow().as_raw_fd()).reregister(poll, token, interest, opts)
    }

    fn deregister(&self, poll: &mio::Poll) -> ::std::io::Result<()> {
        mio::unix::EventedFd(&self.monitor.borrow().as_raw_fd()).deregister(poll)
    }
}

struct Device(libevdev::Device);

impl Device {
    fn new(evdev: libevdev::Device) -> Self {
        trace!("opened \"{}\"", evdev.get_name().to_string_lossy());
        Device(evdev)
    }
}

impl Drop for Device {
    fn drop(&mut self) {
        unsafe { libc::close(self.0.as_raw_fd()); };
    }
}

impl mio::Evented for Device {
    fn register(&self, poll: &mio::Poll, token: mio::Token,
                interest: mio::Ready, opts: mio::PollOpt) -> ::std::io::Result<()> {
        mio::unix::EventedFd(&self.0.as_raw_fd()).register(poll, token, interest, opts)
    }

    fn reregister(&self, poll: &mio::Poll, token: mio::Token,
                  interest: mio::Ready, opts: mio::PollOpt) -> ::std::io::Result<()> {
        mio::unix::EventedFd(&self.0.as_raw_fd()).reregister(poll, token, interest, opts)
    }

    fn deregister(&self, poll: &mio::Poll) -> ::std::io::Result<()> {
        mio::unix::EventedFd(&self.0.as_raw_fd()).deregister(poll)
    }
}

pub struct Stream<'a> {
    udev: PollEvented<UdevStream<'a>>,
    tokio: Handle,
    devices: RefCell<HashMap<OsString, PollEvented<Device>>>,
    buffer: RefCell<VecDeque<Event<WindowID, DeviceID>>>,
}

impl<'a> Stream<'a> {
    pub fn new(ctx: &'a Context, handle: &Handle) -> Result<Self, String> {
        let inner = try!(from_result(UdevStream::new(ctx)));
        let poll = try!(from_result(PollEvented::new(inner, handle)));

        let result = Stream {
            udev: poll,
            tokio: handle.clone(),
            devices: RefCell::new(HashMap::new()),
            buffer: RefCell::new(VecDeque::new()),
        };

        try!(result.open_existing_devices(&ctx.udev).map_err(|e| e.description().to_string()));

        Ok(result)
    }

    fn map_udev_event(&self, event: libudev::Event<'a>) -> Option<Event<WindowID, DeviceID>> {
        let device = event.device();
        use self::libudev::EventType::*;
        match event.event_type() {
            Add => {
                match device.devnode() {
                    None => {
                        debug!("unable to open {} as it has no devnode", device.sysname().to_string_lossy());
                        None
                    },
                    Some(node) => {
                        match self.open_device(device.sysname(), node) {
                            Err(e) => {
                                debug!("unable to open {}: {}", node.to_string_lossy(), e.description());
                                None
                            },
                            Ok(()) => Some(Event::DeviceAdded { device: DeviceID(device.sysname().to_owned()) })
                        }
                    }
                }
            },
            Remove => {
                match self.devices.borrow_mut().remove(device.sysname()) {
                    None => {
                        debug!("unknown device {} removed", device.sysname().to_string_lossy());
                        None
                    },
                    Some(_) => Some(Event::DeviceRemoved { device: DeviceID(device.sysname().to_owned()) }),
                }
            },
            x => { warn!("unknown libudev action type {:?}", x); None },
        }
    }

    fn open_device(&self, sysname: &OsStr, path: &Path) -> io::Result<()> {
        let fd = unsafe { libc::open(&path.as_os_str().as_bytes()[0] as *const u8 as *const c_char,
                                     libc::O_RDONLY | libc::O_NONBLOCK) };
        if fd == -1 {
            Err(io::Error::last_os_error())
        } else {
            match libevdev::Device::new_from_fd(fd) {
                Err(e) => {
                    unsafe { libc::close(fd); };
                    Err(e)
                },
                Ok(d) => {
                    let dev = Device::new(d);
                    let poll = try!(PollEvented::new(dev, &self.tokio));
                    self.devices.borrow_mut().insert(sysname.to_owned(), poll);
                    Ok(())
                }
            }
        }
    }

    fn open_existing_devices(&self, udev: &libudev::Context) -> io::Result<()> {
        let mut enumerator = try!(libudev::Enumerator::new(&udev));
        try!(enumerator.match_subsystem("input"));
        let ds = try!(enumerator.scan_devices());
        for device in ds {
            match device.devnode() {
                None => debug!("unable to open {} as it has no devnode", device.sysname().to_string_lossy()),
                Some(node) => match self.open_device(device.sysname(), node) {
                    Err(e) => debug!("unable to open {}: {}", node.to_string_lossy(), e.description()),
                    Ok(()) => (),
                },
            }
        }
        Ok(())
    }

    fn map_device_event(&self, id: &OsStr, event: libevdev::InputEvent) -> Option<Event<WindowID, DeviceID>> {
        match event.type_ {
            codes::EV_SYN => None,
            codes::EV_KEY => match event.value {
                0 => Some(Event::RawButtonPress {
                    device: DeviceID(id.to_owned()),
                    button: ButtonID(event.code as u32),
                }),
                1 => Some(Event::RawButtonRelease {
                    device: DeviceID(id.to_owned()),
                    button: ButtonID(event.code as u32),
                }),
                2 => None,      // Key repeat
                x => {
                    warn!("unrecognised evdev key state: {}", x);
                    None
                },
            },
            codes::EV_ABS => Some(Event::RawMotion {
                device: DeviceID(id.to_owned()),
                axis: AxisID((codes::REL_CNT + event.code) as u32),
                value: event.value as f64,
            }),
            codes::EV_REL => Some(Event::RawMotion {
                device: DeviceID(id.to_owned()),
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

    fn poll_device(&self, id: &OsStr, device: &mut Device) {
        use super::platform::libevdev::ReadStatus::*;
        let mut buffer = self.buffer.borrow_mut();
        let mut flag = libevdev::READ_FLAG_NORMAL;
        loop {
            match device.0.next_event(flag) {
                Again => break,
                Sync(e) => {
                    flag = libevdev::READ_FLAG_SYNC;
                    if let Some(x) = self.map_device_event(id, e) {
                        buffer.push_back(x)
                    }
                },
                Success(e) => {
                    if let Some(x) = self.map_device_event(id, e) {
                        buffer.push_back(x)
                    }
                },
            }
        }
    }
}

fn from_result<T, E: Error>(x: Result<T, E>) -> Result<T, String> {
    x.map_err(|x| x.description().to_string())
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct DeviceID(OsString);

impl common::DeviceID for DeviceID {}

#[derive(Debug, Clone, PartialEq)]
pub struct WindowID(pub Void);

impl Hash for WindowID {
    #[allow(unused_variables)]
    fn hash<H: Hasher>(&self, state: &mut H) {}
}

impl Eq for WindowID {}

impl common::WindowID for WindowID {}

impl<'a, 'b> futures::stream::Stream for &'a Stream<'b> {
    type Item = Event<WindowID, DeviceID>;
    type Error = ();

    fn poll(&mut self) -> futures::Poll<Option<Self::Item>, Self::Error> {
        if futures::Async::NotReady == self.udev.poll_read()
            && self.devices.borrow().values().map(PollEvented::poll_read).all(|x| x == futures::Async::NotReady) {
            return Ok(futures::Async::NotReady);
        }

        for (&ref id, &mut ref mut device) in &mut *self.devices.borrow_mut() {
            self.poll_device(&id, device.get_mut());
        }

        let mut buffer = self.buffer.borrow_mut();

        while let Some(event) = self.udev.get_ref().monitor.borrow_mut().receive_event() {
            if let Some(e) = self.map_udev_event(event) {
                buffer.push_back(e);
            }
        }

        Ok(match buffer.pop_front() {
            None => {
                self.udev.need_read();
                futures::Async::NotReady
            },
            Some(i) => futures::Async::Ready(Some(i)),
        })
    }
}
