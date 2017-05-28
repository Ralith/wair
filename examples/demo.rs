extern crate wai;
extern crate futures;
extern crate tokio_core;
extern crate env_logger;

use tokio_core::reactor::Core;
use futures::stream::Stream;
use wai::*;

fn main() {
    env_logger::init().unwrap();

    let mut l = Core::new().unwrap();
    let handle = l.handle();

    let (_, stream) = Context::new(&handle).unwrap();

    l.run(stream.for_each(|e| {
        println!("{:?}", e);
        Ok(())
    })).unwrap();
}
