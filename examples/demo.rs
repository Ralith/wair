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
            Added => {
                print!("{}", context.device_name(id));
                if let Some(port) = context.device_port(id.clone()) {
                    print!(" on {}", port);
                }
                if let Some(hwid) = context.device_hw_id(id.clone()) {
                    println!(": {}", hwid);
                } else {
                    println!("");
                }
            },
            KeyPress { scancode, .. } => println!("{}", context.device_scancode_name(id, scancode)),
            _ => {}
        }
        Ok(())
    })).unwrap();
}
