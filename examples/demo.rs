extern crate wair;
extern crate futures;
extern crate tokio_core;
extern crate env_logger;

use tokio_core::reactor::Core;
use futures::stream::Stream;

fn main() {
    env_logger::init().unwrap();

    let mut l = Core::new().unwrap();
    let handle = l.handle();

    let stream : wair::x11::EventStream = wair::x11::EventStream::new(wair::x11::Context::new().unwrap(), &handle).unwrap();
    let window = stream.new_window(wair::WindowBuilder::new().name("wair input demo")).unwrap();
    window.map();
    stream.flush();

    l.run(stream.for_each(|e| {
        println!("got event: {:?}", e);
        Ok(())
    })).unwrap();
}
