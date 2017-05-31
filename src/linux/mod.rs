extern crate nix;

#[cfg(feature = "x11-backend")]
extern crate xcb;

mod evdev;
mod libudev;
mod helpers;

#[cfg(feature = "x11-backend")]
mod x11;

use std::io;

use futures::{self, Poll, Async};
use tokio_core::reactor::Handle;

use {Event, Scancode, DeviceHwId};

error_chain! {
    foreign_links {
        Nix(nix::Error);
        Io(io::Error);
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct DeviceId(SysDeviceId);

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum SysDeviceId {
    #[cfg(feature = "x11-backend")]
    X11(x11::DeviceId),
    Evdev(evdev::DeviceId),
}

pub struct Context {
    ws: WindowSystem,
    evdev: evdev::Context,
}

macro_rules! device_method {
    ($f:ident($($arg:ident : $argty:ty),*) -> $ret:ty) => {
        pub fn $f(&self, device: DeviceId, $($arg : $argty),*) -> $ret {
            use self::SysDeviceId::*;
            match device.0 {
                #[cfg(feature = "x11-backend")]
                X11(id) => self.ws.get_x11().$f(id, $($arg),*),
                Evdev(id) => self.evdev.$f(id, $($arg),*),
            }
        }
    };
}

impl Context {
    pub fn new(handle: &Handle) -> Result<(Context, Stream)> {
        #[cfg(feature = "x11-backend")]
        let (ws, ws_events) = {
            let (x, s) = x11::Context::new(handle)?;
            (WindowSystem::X11(x), WSStream::X11(s))
        };

        let (evdev_ctx, evdev_s) = evdev::Context::new(handle.clone())?;

        Ok((Context { ws: ws, evdev: evdev_ctx }, Stream { ws: ws_events, evdev: evdev_s }))
    }

    device_method!(device_scancode_name(code: Scancode) -> String);
    device_method!(device_name() -> String);
    device_method!(device_hw_id() -> Option<DeviceHwId>);
    device_method!(device_port() -> Option<String>);
}

enum WindowSystem {
    #[cfg(feature = "x11-backend")]
    X11(x11::Context),
    // TODO: wayland
}

impl WindowSystem {
    fn get_x11(&self) -> &x11::Context { match self { &WindowSystem::X11(ref x) => x, _ => panic!("unexpected window system"), } }
}

enum WSStream {
    #[cfg(feature = "x11-backend")]
    X11(x11::Stream),
    // TODO: wayland
}

pub struct Stream {
    ws: WSStream,
    evdev: evdev::Stream,
}

impl futures::Stream for Stream {
    type Item = (DeviceId, Event);
    type Error = Error;

    fn poll(&mut self) -> Poll<Option<Self::Item>, Self::Error> {
        use self::WSStream::*;
        match &mut self.ws {
            #[cfg(feature = "x11-backend")]
            &mut X11(ref mut s) => match s.poll().unwrap() {
                Async::NotReady => (),
                Async::Ready(None) => panic!("stream expected to be infinite"),
                Async::Ready(Some((id, e))) => return Ok(Async::Ready(Some((DeviceId(SysDeviceId::X11(id)), e)))),
            }
        }

        self.evdev.poll().map(|x| x.map(|x| x.map(|(id, e)| (DeviceId(SysDeviceId::Evdev(id)), e))))
    }
}
