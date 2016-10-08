extern crate wair;
extern crate futures;
extern crate tokio_core;
extern crate env_logger;
extern crate void;

use tokio_core::reactor::Core;
use futures::stream::Stream;
use wair::*;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
enum GenericDevice {
    X(x11::DeviceID),
    Evdev(evdev::DeviceID)
}

impl DeviceID for GenericDevice {}

fn handle_event<W: WindowID, D: DeviceID>(e: Event<W, D>) -> Result<(), ()> {
    println!("got event: {:?}", e);
    match e {
        Event::Quit(_) => Err(()),
        Event::KeyPress { keysym: sym, .. } => {
            println!("sym: {}", x11::Stream::keysym_name(sym));
            Ok(())
        },
        _ => Ok(()),
    }
}

fn main() {
    env_logger::init().unwrap();

    let mut l = Core::new().unwrap();
    let handle = l.handle();

    let x_stream = x11::Stream::new(&handle).unwrap();
    let window = x_stream.new_window(WindowBuilder::new().name("wair input demo")).unwrap();
    x_stream.window_map(window);
    x_stream.flush();

    let ev_stream = evdev::Stream::new(&handle).unwrap();

    let stream = x_stream
        .map(|e| e.map(|w| w, GenericDevice::X))
        .merge(ev_stream.map(|e| e.map(|w| -> x11::WindowID { void::unreachable(w.0) }, GenericDevice::Evdev)));

    let _ = l.run(stream.for_each(|e| {
        use futures::stream::MergedItem::*;
        match e {
            First(x) => handle_event(x),
            Second(x) => handle_event(x),
            Both(x, y) => handle_event(x).and_then(|_| handle_event(y)),
        }
    }));
}
