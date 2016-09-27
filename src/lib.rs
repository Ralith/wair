#[macro_use]
extern crate log;
extern crate futures;
extern crate mio;
extern crate tokio_core;
extern crate libc;

mod common;
pub use common::*;

#[cfg(feature = "x11-backend")]
pub mod x11;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}
