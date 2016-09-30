extern crate libudev;

use std::error::Error;
use std::ffi::OsStr;
use std::os::unix::ffi::OsStrExt;
use std::os::unix::io::AsRawFd;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::cell::RefCell;
use std::path::Path;
use std::io;

use mio;
use tokio_core::reactor::{PollEvented, Handle};
use futures;
use void::Void;
use libc;
use libc::c_char;

use common;
use common::{Event, AxisID, ButtonID, ScanCode, KeySym};

use super::platform::libevdev;

pub struct Context {
    udev: libudev::Context,
}

impl Context {
    pub fn new() -> Result<Self, String> {
        from_result(libudev::Context::new()).map(|x| Context { udev: x })
    }
}

struct InnerStream<'a> {
    monitor: RefCell<libudev::MonitorSocket<'a>>,
}

impl<'a> InnerStream<'a> {
    fn new(ctx: &'a Context) -> libudev::Result<Self> {
        let mut monitor = try!(libudev::Monitor::new(&ctx.udev));
        try!(monitor.match_subsystem("input"));
        let sock = try!(monitor.listen());
        Ok(InnerStream { monitor: RefCell::new(sock) })
    }
}

impl<'a> mio::Evented for InnerStream<'a> {
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

impl Drop for Device {
    fn drop(&mut self) {
        unsafe { libc::close(self.0.as_raw_fd()); };
    }
}

pub struct Stream<'a> {
    io: PollEvented<InnerStream<'a>>,
    devices: RefCell<HashMap<String, Device>>,
}

impl<'a> Stream<'a> {
    pub fn new(ctx: &'a Context, handle: &Handle) -> Result<Self, String> {
        let inner = try!(from_result(InnerStream::new(ctx)));
        let poll = try!(from_result(PollEvented::new(inner, handle)));
        Ok(Stream {
            io: poll,
            devices: RefCell::new(HashMap::new()),
        })
    }

    fn map_event(&self, event: libudev::Event<'a>) -> Option<Event<WindowID, DeviceID>> {
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
                            Ok(()) => Some(Event::DeviceAdded { device: DeviceID(device.sysname().to_string_lossy().into_owned()) })
                        }
                    }
                }
            },
            Remove => {
                match self.devices.borrow_mut().remove(&*device.sysname().to_string_lossy()) {
                    None => {
                        debug!("unknown device {} removed", device.sysname().to_string_lossy());
                        None
                    },
                    Some(_) => Some(Event::DeviceRemoved { device: DeviceID(device.sysname().to_string_lossy().into_owned()) }),
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
                Err(e) => Err(e),
                Ok(d) => {
                    self.devices.borrow_mut().insert(sysname.to_string_lossy().into_owned(), Device(d));
                    Ok(())
                }
            }
        }
    }
}

fn from_result<T, E: Error>(x: Result<T, E>) -> Result<T, String> {
    x.map_err(|x| x.description().to_string())
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct DeviceID(String);

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
        if let futures::Async::NotReady = self.io.poll_read() {
            return Ok(futures::Async::NotReady);
        }

        while let Some(event) = self.io.get_ref().monitor.borrow_mut().receive_event() {
            if let Some(e) = self.map_event(event) {
                return Ok(futures::Async::Ready(Some(e)));
            }
        }

        self.io.need_read();
        Ok(futures::Async::NotReady)
    }
}
