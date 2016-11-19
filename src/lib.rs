#[macro_use]
extern crate log;
extern crate futures;
extern crate mio;
extern crate tokio_core;
extern crate libc;
extern crate void;

#[cfg(feature = "x11-backend")]
extern crate xcb;

mod platform;
mod common;

pub mod dynamic;

pub use common::*;

#[cfg(feature = "x11-backend")]
pub mod x11;

#[cfg(feature = "evdev-backend")]
pub mod evdev;

#[cfg(feature = "x11-backend")]
pub use x11::Context as DefaultWindowSystem;
