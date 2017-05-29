//! Wai provides facilities for processing mouse, keyboard, joystick, and other user inputs asynchronously.

#[macro_use]
extern crate log;
extern crate futures;
extern crate mio;
extern crate tokio_core;
extern crate libc;
extern crate void;

#[macro_use]
extern crate error_chain;

#[cfg(target_os = "windows")]
#[path="windows/mod.rs"]
mod platform;
#[cfg(target_os = "linux")]
#[path="linux/mod.rs"]
mod platform;

pub use platform::{DeviceId, Context};

use std::fmt;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct AxisId(pub u32);

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct ButtonId(pub u32);

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct Scancode(pub u32);

impl fmt::Debug for Scancode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Scancode({:X})", self.0)
    }
}

#[derive(Debug, Clone)]
pub enum Event {
    /// A new device has been added to the system
    Added,
    /// An existing device has been removed from the system
    Removed,
    Motion { axis: AxisId, value: f64 },
    ButtonPress { button: ButtonId },
    ButtonRelease { button: ButtonId },
    KeyPress {
        /// Identifies the physical key pressed
        ///
        /// This should not change if the user adjusts the host's keyboard map. Use when the physical location of the
        /// key is more important than the key's host GUI semantics, such as for movement controls in a first-person
        /// game.
        scancode: Scancode,

        /// Specifies the text input arising from a keypress
        text: String
    },
    KeyRelease { scancode: Scancode },
    /// `Context::device_scancode_name` may now give different results.
    KeymapChanged,
}
