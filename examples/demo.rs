extern crate wair;
extern crate futures;
extern crate tokio_core;
extern crate env_logger;

use tokio_core::reactor::Core;
use futures::stream::Stream;
use wair::*;

fn main() {
    env_logger::init().unwrap();

    let mut l = Core::new().unwrap();
    let handle = l.handle();

    let stream : x11::EventStream = x11::EventStream::new(x11::Context::new().unwrap(), &handle).unwrap();
    let window = stream.new_window(WindowBuilder::new().name("wair input demo")).unwrap();
    window.map();
    stream.flush();

    let _ = l.run(stream.for_each(|e| {
        println!("got event: {:?}", e);
        if let Event::Quit(_) = e {
            Err(())
        } else {
            Ok(())
        }
    }));
}
