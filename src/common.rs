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
    Map, Unmap, Quit, FocusIn, FocusOut,
    Resize((u32, u32)), Move((i32, i32)),

    /// Change in position of a mouse pointer
    ///
    /// These events are delivered only when the pointer is over a window you created. Coordinates are in absolute
    /// window pixels, and may have higher-than-pixel precision. Some systems may provide multiple pointers,
    /// distinguished by `device`.
    ///
    /// Pointer motion should be used only to implement pointer-based GUI interaction in a manner consistent with the
    /// host system. It should never be used for non-pointer tasks such as pointing a 3D camera.
    PointerMotion { device: D, pos: (f64, f64) },

    Input { device: D, event: InputEvent },
}

impl<D: DeviceID> WindowEvent<D> {
    pub fn map<E: DeviceID, F: FnOnce(D) -> E>(self, f: F) -> WindowEvent<E> {
        use WindowEvent::*;
        match self {
            FocusIn => FocusIn,
            FocusOut => FocusOut,
            Map => Map,
            Unmap => Unmap,
            Quit => Quit,
            Resize(x) => Resize(x),
            Move(x) => Move(x),
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
    KeyPress {
        /// Identifies the physical key pressed
        ///
        /// This should not change if the user adjusts the host's keyboard map. Use when the physical location of the
        /// key is more important than the key's host GUI semantics, such as for movement controls in a first-person
        /// game.
        keycode: Keycode,

        /// Identifies the host GUI semantics of the key
        ///
        /// Use when the host GUI semantics of the key are more important than the physical location of the key, such as
        /// when implementing appropriate behavior for "page up." Do not use for text input.
        keysym: Keysym,

        /// Specifies the text input arising from a keypress
        text: String
    },
    KeyRelease { keycode: Keycode, keysym: Keysym },
}

#[derive(Debug, Clone)]
pub enum Event<W: WindowID, D: DeviceID> {
    /// An event associated with a particular window you created
    ///
    /// Includes both window-related events such as resizing and focus changes and device inputs directed specifically
    /// to that window.
    ///
    /// This class of events should be used to interoperate with the host GUI stack and implement host-like GUI
    /// semantics.
    Window { window: W, event: WindowEvent<D> },

    /// An event arising from an input device regardless of association with any particular window
    ///
    /// This class of events should be used when physical, unfiltered input data is desired, and is therefore
    /// appropriate for tasks such as 3D camera control which are not consistent with host system GUI semantics.
    Device { device: D, event: DeviceEvent },
}

#[derive(Debug, Clone)]
pub enum DeviceEvent {
    Input(InputEvent),

    /// Emitted before any input events to indicate that the device has been connected
    Added,

    /// Emitted after all input events to indicate that the device has been removed
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

    /// Creates a vulkan surface on a given window
    ///
    /// # Safety
    /// Must be called at most once per window.
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
