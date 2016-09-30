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

    let mut x_context = x11::Context::new().unwrap();
    let stream = x11::EventStream::new(&mut x_context, &handle).unwrap();
    let window = stream.new_window(WindowBuilder::new().name("wair input demo")).unwrap();
    stream.window_map(window);
    stream.flush();

    let _ = l.run(stream.for_each(|e| {
        println!("got event: {:?}", e);
        match e {
            Event::Quit(_) => Err(()),
            Event::RawKeyPress { key_sym: sym, .. } => {
                println!("sym: {}", x11::EventStream::key_sym_name(sym));
                Ok(())
            },
            Event::KeyPress { key_sym: sym, .. } => {
                println!("sym: {}", x11::EventStream::key_sym_name(sym));
                Ok(())
            },
            _ => Ok(()),
        }
    }));
}
