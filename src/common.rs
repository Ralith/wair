use std::{io, fmt};
use std::hash::Hash;
#[cfg(feature = "vulkano")]
use std::sync::Arc;

use tokio_core::reactor::Handle;
use futures::stream::Stream;

#[cfg(feature = "vulkano")]
use vulkano;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct AxisID(pub u32);

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct ButtonID(pub u32);

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct Keycode(pub u32);

impl fmt::Debug for Keycode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Keycode({:X})", self.0)
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct Keysym(pub u32);

impl fmt::Debug for Keysym {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Keysym({:X})", self.0)
    }
}

#[derive(Debug, Clone)]
pub enum WindowEvent<D: DeviceID> {
    Map, Unmap, Quit,
    PointerMotion { device: D, pos: (f64, f64) },
    Input { device: D, event: InputEvent },
}

impl<D: DeviceID> WindowEvent<D> {
    pub fn map<E: DeviceID, F: FnOnce(D) -> E>(self, f: F) -> WindowEvent<E> {
        use WindowEvent::*;
        match self {
            Map => Map,
            Unmap => Unmap,
            Quit => Quit,
            PointerMotion { device, pos } => PointerMotion { device: f(device), pos: pos },
            Input { device, event } => Input { device: f(device), event: event },
        }
    }
}

#[derive(Debug, Clone)]
pub enum InputEvent {
    Motion { axis: AxisID, value: f64 },
    ButtonPress { button: ButtonID },
    ButtonRelease { button: ButtonID },
    KeyPress { keycode: Keycode, keysym: Keysym, text: String },
    KeyRelease { keycode: Keycode, keysym: Keysym },
}

#[derive(Debug, Clone)]
pub enum Event<W: WindowID, D: DeviceID> {
    Window { window: W, event: WindowEvent<D> },
    Device { device: D, event: DeviceEvent },
}

#[derive(Debug, Clone)]
pub enum DeviceEvent {
    Input(InputEvent),
    Added,
    Removed,
}

impl<W: WindowID, D: DeviceID> Event<W, D> {
    pub fn map<T: WindowID, U: DeviceID, F: FnOnce(W) -> T, G: FnOnce(D) -> U>(self, f: F, g: G) -> Event<T, U> {
        use Event::*;
        match self {
            Window { window, event } => Window { window: f(window), event: event.map(g) },
            Device { device, event } => Device { device: g(device), event: event },
        }
    }
}

pub trait Wrapper {
    type Native;
    fn get_native(&self) -> Self::Native;
}

pub trait WindowSystem: Wrapper {
    type WindowID: WindowID;
    type DeviceID: DeviceID;
    type EventStream: Stream<Item=Event<Self::WindowID, Self::DeviceID>>;
    fn open(handle: &Handle) -> io::Result<(Self, Self::EventStream)> where Self: Sized;
    fn new_window(&self, position: (i32, i32), size: (u32, u32), name: Option<&str>) -> Self::WindowID;
    #[cfg(feature = "vulkano")]
    unsafe fn create_vulkan_surface(&self, instance: &Arc<vulkano::instance::Instance>, window: Self::WindowID) -> Result<Arc<vulkano::swapchain::Surface>, vulkano::swapchain::SurfaceCreationError>;
}

pub trait WindowID: fmt::Debug + Copy + Clone + Hash + Eq + Wrapper {}
pub trait DeviceID: fmt::Debug + Clone + Hash + Eq {}

#[derive(Debug, Copy, Clone)]
pub struct WindowBuilder<'a> {
    position: (i32, i32),
    size: (u32, u32),
    name: Option<&'a str>,
}

impl<'a> WindowBuilder<'a> {
    pub fn new() -> WindowBuilder<'a> {
        WindowBuilder { name: None, position: (0, 0), size: (256, 256) }
    }

    pub fn position(&mut self, position: (i32, i32)) -> &mut Self {
        self.position = position;
        self
    }

    pub fn size(&mut self, size: (u32, u32)) -> &mut Self {
        self.size = size;
        self
    }

    pub fn name(&mut self, name: &'a str) -> &mut Self {
        self.name = Some(name);
        self
    }

    pub fn build<T: WindowSystem>(&self, ws: &T) -> T::WindowID {
        ws.new_window(self.position, self.size, self.name)
    }
}
