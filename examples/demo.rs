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
    #[cfg(feature = "x11-backend")]
    X(x11::DeviceID),
    #[cfg(feature = "evdev-backend")]
    Evdev(evdev::DeviceID),
}

impl DeviceID for GenericDevice {}

fn handle_event<W: WindowID, D: DeviceID>(e: Event<W, D>) -> Result<(), ()> {
    println!("got event: {:?}", e);
    match e {
        Event::Window { event: WindowEvent::Quit, .. } => Err(()),
        Event::Window { event: WindowEvent::Input { event: InputEvent::KeyPress { keysym: sym, .. }, .. }, .. } => {
            println!("sym: {}", x11::Context::keysym_name(sym));
            Ok(())
        },
        _ => Ok(()),
    }
}

fn main() {
    env_logger::init().unwrap();

    let mut l = Core::new().unwrap();
    let handle = l.handle();

    let (context, stream) = DefaultWindowSystem::open(&handle).unwrap();
    context.new_window(WindowBuilder::new().name("wair input demo"));
    context.flush();

    let ev_stream = evdev::Stream::new(handle).unwrap();

    let stream = stream
        .map(|e| e.map(|w| w, GenericDevice::X))
        .merge(ev_stream.map(|(d, e)| -> Event<x11::WindowID, GenericDevice> { Event::Device { device: GenericDevice::Evdev(d), event: e } }))
        ;

    let _ = l.run(stream.map_err(|e| void::unreachable(e)).for_each(|e| {
        use futures::stream::MergedItem::*;
        match e {
            First(x) => handle_event(x),
            Second(x) => handle_event(x),
            Both(x, y) => handle_event(x).and_then(|_| handle_event(y)),
        }
    }));
}
