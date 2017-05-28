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

use Event;

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

    // TODO: device info accessors
}

enum WindowSystem {
    #[cfg(feature = "x11-backend")]
    X11(x11::Context),
    // TODO: wayland
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
