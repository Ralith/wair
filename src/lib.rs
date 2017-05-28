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
        keycode: Keycode,

        /// Identifies the host GUI semantics of the key
        ///
        /// Use when the host GUI semantics of the key are more important than the physical location of the key, such as
        /// when implementing appropriate behavior for "page up." The keysym produced by a key may vary according to
        /// keymap and modifier state. Do not use for text input.
        keysym: Keysym,

        /// Specifies the text input arising from a keypress
        text: String
    },
    KeyRelease { keycode: Keycode, keysym: Keysym },
}
