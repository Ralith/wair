extern crate x11_dl;
extern crate libc;

use self::x11_dl::xlib;

use std::io;
use std::fmt;
use std::ops::Deref;
use std::ffi::CString;
use std::ptr;
use std::os::unix::io::{AsRawFd, RawFd};

use mio;
use tokio_core::reactor::{PollEvented, Handle};
use futures;

use common::*;
use dynamic;

pub struct Context {
    xlib: xlib::Xlib,
    display: *mut xlib::Display,
}

impl AsRawFd for Context {
    fn as_raw_fd(&self) -> RawFd {
        unsafe { (self.xlib.XConnectionNumber)(self.display) }
    }
}

pub struct Window<'a> {
    context: &'a Context,
    handle: xlib::Window,
}

pub struct EventStream {
    io: PollEvented<Context>,
}

impl Drop for Context {
    fn drop(&mut self) {
        unsafe { (self.xlib.XCloseDisplay)(self.display) };
    }
}

unsafe impl Send for Context {}
unsafe impl Sync for Context {}

impl Context {
    pub fn new() -> Result<Context, String> {
        match xlib::Xlib::open() {
            Err(e) => Err(e.detail().to_string()),
            Ok(l) => {
                unsafe { (l.XInitThreads)(); };
                let d = unsafe { (l.XOpenDisplay)(ptr::null()) };
                if d.is_null() {
                    Err("unable to open display".to_string())
                } else {
                    Ok(Context { xlib: l,
                                 display: d })
                }
            }
        }
    }

    pub fn flush(&self) {
        unsafe { (self.xlib.XFlush)(self.display); };
    }

    pub fn new_window(&self, builder: WindowBuilder) -> Result<Window, String> {
        let border_width = 0;
        let handle = unsafe {
            let screen = (self.xlib.XDefaultScreenOfDisplay)(self.display);
            let root = (self.xlib.XRootWindowOfScreen)(screen);
            let depth = (self.xlib.XDefaultDepthOfScreen)(screen);
            let visual = (self.xlib.XDefaultVisualOfScreen)(screen);
            let mut attrs : xlib::XSetWindowAttributes = ::std::mem::uninitialized();
            attrs.event_mask = xlib::StructureNotifyMask;
            attrs.background_pixel = (self.xlib.XBlackPixelOfScreen)(screen);
            (self.xlib.XCreateWindow)(self.display, root, builder.position.0, builder.position.1, builder.size.0, builder.size.1, border_width, depth, xlib::InputOutput as u32, visual, xlib::CWEventMask | xlib::CWBackPixel, &mut attrs as *mut xlib::XSetWindowAttributes)
        };
        let window = Window { context: &self, handle: handle };
        window.set_name(builder.name);
        Ok(window)
    }

    fn pending(&self) -> libc::c_int {
        unsafe { (self.xlib.XPending)(self.display) }
    }

    fn next_event(&self) -> xlib::XEvent {
        unsafe {
            let mut event = ::std::mem::uninitialized();
            (self.xlib.XNextEvent)(self.display, &mut event as *mut xlib::XEvent);
            event
        }
    }
}

impl mio::Evented for Context {
    fn register(&self, poll: &mio::Poll, token: mio::Token,
                interest: mio::Ready, opts: mio::PollOpt) -> io::Result<()> {
        mio::unix::EventedFd(&self.as_raw_fd()).register(poll, token, interest, opts)
    }

    fn reregister(&self, poll: &mio::Poll, token: mio::Token,
                  interest: mio::Ready, opts: mio::PollOpt) -> io::Result<()> {
        mio::unix::EventedFd(&self.as_raw_fd()).reregister(poll, token, interest, opts)
    }

    fn deregister(&self, poll: &mio::Poll) -> io::Result<()> {
        mio::unix::EventedFd(&self.as_raw_fd()).deregister(poll)
    }
}


impl<'a> Window<'a> {
    pub fn set_name(&self, name: &str) {
        let c_name = CString::new(name).unwrap();
        unsafe { (self.context.xlib.XStoreName)(self.context.display, self.handle, c_name.as_ptr()); };
    }

    pub fn map(&self) {
        unsafe { (self.context.xlib.XMapWindow)(self.context.display, self.handle); };
    }
}

impl dynamic::Context for Context {
    fn new_window<'a>(&'a self, builder: WindowBuilder) -> Result<Box<dynamic::Window + 'a>, String> {
        Context::new_window(&self, builder).map(|x| -> Box<dynamic::Window + 'a> { Box::new(x) })
    }
}

impl<'a> fmt::Debug for Window<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Window({:X})", self.handle)
    }
}

impl<'a> dynamic::Window for Window<'a> {
    fn set_name(&self, name: &str) { Window::set_name(self, name) }
    fn map(&self) { Window::map(self) }
}

impl EventStream {
    pub fn new(context: Context, handle: &Handle) -> io::Result<EventStream> {
        PollEvented::new(context, handle).map(|x| EventStream { io: x })
    }
}

impl Deref for EventStream {
    type Target = Context;

    fn deref(&self) -> &Context { self.io.get_ref() }
}

impl<'a> futures::stream::Stream for &'a EventStream {
    type Item = Event<Window<'a>>;
    type Error = ();

    fn poll(&mut self) -> futures::Poll<Option<Self::Item>, Self::Error> {
        let mut x: Option<Self::Item> = None;

        trace!("polled");

        while self.pending() != 0 {
            let storage = self.next_event();
            let event = &xlib::XAnyEvent::from(&storage);
            let window = Window { context: &self.io.get_ref(), handle: event.window };
            trace!("got event type {}", event.type_);
            { use self::x11_dl::xlib::*; match event.type_ {
                MapNotify => {
                    x = Some(Event::Map(window));
                    break;
                },
                UnmapNotify => {
                    x = Some(Event::Unmap(window));
                    break;
                },
                ClientMessage => {
                    let xclient = &XClientMessageEvent::from(&storage);
                    continue;   // TODO
                },
                _ => {
                    debug!("unhandled event type {}", event.type_);
                    continue;
                },
            }};
        }

        Ok(match x {
            None => {
                self.io.need_read();
                trace!("not ready"); futures::Async::NotReady
            },
            Some(i) => futures::Async::Ready(Some(i)),
        })
    }
}
