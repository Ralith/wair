#[macro_use]
extern crate log;
extern crate futures;
extern crate mio;
extern crate tokio_core;

mod common;
pub use common::*;

pub mod dynamic;

#[cfg(feature = "x11")]
pub mod x11;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}
