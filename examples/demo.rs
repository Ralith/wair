extern crate wair;
extern crate futures;
extern crate tokio_core;
extern crate env_logger;
extern crate void;

use tokio_core::reactor::Core;
use futures::stream::Stream;
use wair::*;

fn main() {
    env_logger::init().unwrap();

    let mut l = Core::new().unwrap();
    let handle = l.handle();

    let (context, stream) = dynamic::WindowSystem::open(&handle).unwrap();
    context.new_window(WindowBuilder::new().name("wair input demo"));

    let _ = l.run(stream.map_err(|e| void::unreachable(e)).for_each(|e| {
        println!("got event: {:?}", e);
        match e {
            Event::Window { event: WindowEvent::Quit, .. } => Err(()),
            Event::Window { event: WindowEvent::Input { event: InputEvent::KeyPress { keysym: sym, .. }, .. }, .. } => {
                println!("sym: {}", x11::Context::keysym_name(sym));
                Ok(())
            },
            _ => Ok(()),
        }
    }));
}
