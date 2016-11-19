extern crate wair;
extern crate futures;
extern crate tokio_core;
extern crate env_logger;
extern crate void;

use tokio_core::reactor::Core;
use wair::evdev;
use futures::stream::Stream;

fn main() {
    env_logger::init().unwrap();

    let mut l = Core::new().unwrap();
    let handle = l.handle();

    let events = evdev::Stream::new(handle).unwrap();
    l.run(events.for_each(|e| {
        println!("{:?}", e);
        Ok(())
    })).unwrap();
}
