extern crate wair;
extern crate futures;
extern crate tokio_core;
extern crate env_logger;
extern crate void;

use tokio_core::reactor::Core;
use futures::stream::Stream;
use futures::*;
use wair::*;

struct Interleave<L, R> {
    left: L,
    right: R,
    state: bool,
    left_ended: bool,
    right_ended: bool,
}

fn interleave<L, R>(left: L, right: R) -> Interleave<L, R> {
    Interleave { left: left, right: right, state: false, left_ended: false, right_ended: false }
}

impl<L, R, I, E> Stream for Interleave<L, R> where L: Stream<Item=I, Error=E>, R: Stream<Item=I, Error=E> {
    type Item = I;
    type Error = E;

    fn poll(&mut self) -> Poll<Option<Self::Item>, Self::Error> {
        fn poll_one<T, U, I, E>(a: &mut T, a_ended: &mut bool, b: &mut U, b_ended: &mut bool) -> Poll<Option<I>, E>
            where T: Stream<Item=I, Error=E>, U: Stream<Item=I, Error=E> {
            match a.poll() {
                e@Err(_) => e,
                Ok(Async::NotReady) => match b.poll() {
                    Ok(Async::Ready(None)) => {
                        *b_ended = true;
                        Ok(Async::NotReady)
                    }
                    x => x,
                },
                Ok(Async::Ready(None)) => {
                    *a_ended = true;
                    b.poll()
                },
                x => x,
            }
        }

        if !self.left_ended && !self.right_ended {
            self.state = !self.state;
            if self.state {
                poll_one(&mut self.left, &mut self.left_ended, &mut self.right, &mut self.right_ended)
            } else {
                poll_one(&mut self.right, &mut self.right_ended, &mut self.left, &mut self.left_ended)
            }
        } else if self.left_ended {
            self.right.poll()
        } else {
            self.left.poll()
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
enum GenericDevice {
    X(x11::DeviceID),
    Evdev(evdev::DeviceID)
}

impl DeviceID for GenericDevice {}

fn main() {
    env_logger::init().unwrap();

    let mut l = Core::new().unwrap();
    let handle = l.handle();

    let mut x_context = x11::Context::new().unwrap();
    let x_stream = x11::EventStream::new(&mut x_context, &handle).unwrap();
    let window = x_stream.new_window(WindowBuilder::new().name("wair input demo")).unwrap();
    x_stream.window_map(window);
    x_stream.flush();

    let evdev = evdev::Context::new().unwrap();
    let ev_stream = evdev::Stream::new(&evdev, &handle).unwrap();

    let stream = interleave(
        x_stream.map(|e| e.map(|w| w, GenericDevice::X)),
        ev_stream.map(|e| e.map(|w| -> x11::WindowID { void::unreachable(w.0) }, GenericDevice::Evdev)));

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
