extern crate nix;

#[cfg(feature = "x11-backend")]
extern crate xcb;

mod evdev;
mod libudev;

#[cfg(feature = "x11-backend")]
mod x11;

use std::io;

use futures::{self, Poll, Async};
use tokio_core::reactor::Handle;

use {Event, Scancode};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum DeviceId {
    #[cfg(feature = "x11-backend")]
    X11(x11::DeviceId),
    Evdev(evdev::DeviceId),
}

pub struct Context {
    ws: WindowSystem,
}

impl Context {
    pub fn new(handle: &Handle) -> io::Result<(Self, Stream)> {
        #[cfg(feature = "x11-backend")]
        let (ws, ws_events) = {
            let (x, s) = x11::Context::new(handle)?;
            (WindowSystem::X11(x), WSStream::X11(s))
        };

        let evdev = evdev::Stream::new(handle.clone())?;

        Ok((Context { ws: ws }, Stream { ws: ws_events, evdev: evdev }))
    }

    pub fn device_scancode_name(&self, device: DeviceId, code: Scancode) -> String {
        use self::DeviceId::*;
        match device {
            #[cfg(feature = "x11-backend")]
            X11(id) => self.ws.get_x11().device_scancode_name(id, code),
            Evdev(_) => panic!("tried to look up scancode from an evdev device, which emits only buttons"),
        }
    }

    // TODO: device info accessors
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
    type Error = evdev::Error;

    fn poll(&mut self) -> Poll<Option<Self::Item>, Self::Error> {
        use self::WSStream::*;
        match &mut self.ws {
            #[cfg(feature = "x11-backend")]
            &mut X11(ref mut s) => match s.poll().unwrap() {
                Async::NotReady => (),
                Async::Ready(None) => panic!("stream expected to be infinite"),
                Async::Ready(Some((id, e))) => return Ok(Async::Ready(Some((DeviceId::X11(id), e)))),
            }
        }

        self.evdev.poll().map(|x| x.map(|x| x.map(|(id, e)| (DeviceId::Evdev(id), e))))
    }
}
