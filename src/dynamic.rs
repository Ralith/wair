//! Window creation and event processing which dispatches to appropriate backends dynamically. Use for portable
//! programs.

use std::io;
#[cfg(feature = "vulkano")]
use std::sync::Arc;

use futures;
use futures::{Poll, Async};
use futures::stream::Stream;
use void;
use void::Void;
use xcb;
use tokio_core::reactor::Handle;

use common;
use common::{Wrapper, Event, DeviceEvent};

#[cfg(feature = "x11-backend")]
use x11;
#[cfg(feature = "evdev-backend")]
use evdev;
#[cfg(feature = "vulkano")]
use vulkano;

pub enum WindowSystem {
    #[cfg(feature = "x11-backend")]
    X11(x11::Context),
    // TODO: win32, OSX, wayland
}

pub enum NativeWindowSystem {
    #[cfg(feature = "x11-backend")]
    X11(*mut xcb::ffi::base::xcb_connection_t),
}

impl Wrapper for WindowSystem {
    type Native = NativeWindowSystem;

    fn get_native(&self) -> Self::Native {
        use self::WindowSystem::*;
        match self {
            #[cfg(feature = "x11-backend")]
            &X11(ref c) => NativeWindowSystem::X11(c.get_native()),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Impossible(Void);
impl Eq for Impossible {}
impl ::std::hash::Hash for Impossible {
    fn hash<H: ::std::hash::Hasher>(&self, _state: &mut H) {}
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum WindowID {
    #[cfg(feature = "x11-backend")]
    X11(x11::WindowID),
    Dummy(Impossible),         // Suppress "unreachable pattern" error in unwraps if only one window system is enabled
}

impl WindowID {
    #[cfg(feature = "x11-backend")]
    fn unwrap_x11(self) -> x11::WindowID { match self { WindowID::X11(x) => x, _ => panic!("unexpected window ID"), } }
}

pub enum NativeWindow {
    #[cfg(feature = "x11-backend")]
    X11(xcb::ffi::xproto::xcb_window_t),
}

impl Wrapper for WindowID {
    type Native = NativeWindow;

    fn get_native(&self) -> Self::Native {
        match self {
            #[cfg(feature = "x11-backend")]
            &WindowID::X11(ref w) => NativeWindow::X11(w.get_native()),
            &WindowID::Dummy(Impossible(x)) => void::unreachable(x),
        }
    }
}

impl common::WindowID for WindowID {}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum DeviceID {
    #[cfg(feature = "x11-backend")]
    X11(x11::DeviceID),
    #[cfg(feature = "evdev-backend")]
    Evdev(evdev::DeviceID),
}
impl common::DeviceID for DeviceID {}

enum WSIStream {
    #[cfg(feature = "x11-backend")]
    X11(x11::Stream),
}

pub struct Events {
    wsi: WSIStream,
    #[cfg(feature = "evdev-backend")]
    evdev: Option<evdev::Stream>,
    #[cfg(not(feature = "evdev-backend"))]
    evdev: (),
}

impl Events {
    #[cfg(feature = "evdev-backend")]
    fn poll_evdev(&mut self) -> Option<(DeviceID, DeviceEvent)> {
        self.evdev.as_mut().and_then(|evdev| {
            match evdev.poll() {
                Err(e) => void::unreachable(e),
                Ok(Async::NotReady) => None,
                Ok(Async::Ready(None)) => panic!("stream expected to be infinite"),
                Ok(Async::Ready(Some((d, e)))) => Some((DeviceID::Evdev(d), e)),
            }
        })
    }

}

const POLL_METHODS: &'static [fn(&mut Events) -> Option<(DeviceID, DeviceEvent)>] = &[
    #[cfg(feature = "evdev-backend")]
    Events::poll_evdev,
];

impl futures::stream::Stream for Events {
    type Item = Event<WindowID, DeviceID>;
    type Error = Void;

    fn poll(&mut self) -> Poll<Option<Self::Item>, Self::Error> {
        use self::WSIStream::*;
        match &mut self.wsi {
            #[cfg(feature = "x11-backend")]
            &mut X11(ref mut s) => match s.poll()? {
                Async::NotReady => (),
                Async::Ready(None) => panic!("stream expected to be infinite"),
                Async::Ready(Some(x)) => return Ok(Async::Ready(Some(x.map(WindowID::X11, DeviceID::X11)))),
            }
        }

        match POLL_METHODS.into_iter().filter_map(|f| f(self)).next() {
            Some((d, e)) => Ok(Async::Ready(Some(Event::Device { device: d, event: e }))),
            None => Ok(Async::NotReady)
        }
    }
}

#[cfg(feature = "evdev-backend")]
fn init_evdev(handle: &Handle) -> Option<evdev::Stream> {
    match evdev::Stream::new(handle.clone()) {
        Err(e) => { warn!("couldn't initialize evdev input system: {}", e); None },
        Ok(x) => Some(x),
    }
}

#[cfg(not(feature = "evdev-backend"))]
fn init_evdev(handle: &Handle) -> () { () }

impl common::WindowSystem for WindowSystem {
    type WindowID = WindowID;
    type DeviceID = DeviceID;
    type EventStream = Events;

    fn open(handle: &Handle) -> io::Result<(Self, Self::EventStream)> {
        // TODO: Select most attractive of compiled-in options
        #[cfg(feature = "x11-backend")]
        let (wsi, wsi_events) = {
            let (x, s) = x11::Context::open(handle)?;
            (WindowSystem::X11(x), WSIStream::X11(s))
        };
        
        Ok((wsi, Events {
            wsi: wsi_events,
            evdev: init_evdev(handle),
        }))
    }

    fn new_window(&self, position: (i32, i32), size: (u32, u32), name: Option<&str>) -> Self::WindowID {
        use self::WindowSystem::*;
        match self {
            #[cfg(feature = "x11-backend")]
            &X11(ref w) => WindowID::X11(w.new_window(position, size, name)),
        }
    }

    #[cfg(feature = "vulkano")]
    unsafe fn create_vulkan_surface(&self, instance: &Arc<vulkano::instance::Instance>, window: Self::WindowID)
                                    -> Result<Arc<vulkano::swapchain::Surface>, vulkano::swapchain::SurfaceCreationError> {
        use self::WindowSystem::*;
        match self {
            #[cfg(feature = "x11-backend")]
            &X11(ref w) => w.create_vulkan_surface(instance, window.unwrap_x11()),
        }
    }
}
