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

    let (context, stream) = Context::new(&handle).unwrap();

    l.run(stream.for_each(|(id, e)| {
        println!("{:?}: {:?}", id, e);
        use Event::*;
        match e {
            KeyPress { scancode, .. } => println!("{}", context.device_scancode_name(id, scancode)),
            _ => {}
        }
        Ok(())
    })).unwrap();
}
