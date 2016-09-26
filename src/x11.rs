extern crate x11_dl;
extern crate libc;

use self::x11_dl::xlib;
use self::x11_dl::xinput2;

use std::{io, fmt, mem, slice};
use std::collections::VecDeque;
use std::ops::Deref;
use std::ffi::{CString, CStr};
use std::ptr;
use std::os::unix::io::{AsRawFd, RawFd};

use mio;
use tokio_core::reactor::{PollEvented, Handle};
use futures;

use common;
use common::{Event, AxisID, ButtonID};

struct Extension<T> {
    lib: T,
    opcode: libc::c_int,
    first_event_id: libc::c_int,
    first_error_id: libc::c_int,
}

pub struct Context {
    xlib: xlib::Xlib,
    atoms: Atoms,
    display: *mut xlib::Display,
    xinput2: Option<Extension<xinput2::XInput2>>,
    buffer: VecDeque<<EventStream as futures::stream::Stream>::Item>
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct WindowID(xlib::Window);

impl common::WindowID for WindowID {}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct DeviceID(i32);

impl common::DeviceID for DeviceID {}

struct Atoms {
    wm_protocols: xlib::Atom,
    wm_delete_window: xlib::Atom,
    device_node: xlib::Atom,
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

impl AsRawFd for Context {
    fn as_raw_fd(&self) -> RawFd {
        unsafe { (self.xlib.XConnectionNumber)(self.display) }
    }
}

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
                    let atoms = Atoms {
                        wm_protocols: unsafe { (l.XInternAtom)(d, b"WM_PROTOCOLS\0".as_ptr() as *const libc::c_char, 0) },
                        wm_delete_window: unsafe { (l.XInternAtom)(d, b"WM_DELETE_WINDOW\0".as_ptr() as *const libc::c_char, 0) },
                        device_node: unsafe { (l.XInternAtom)(d, b"Device Node\0".as_ptr() as *const libc::c_char, 0) }
                    };
                    let xinput2 = match xinput2::XInput2::open() {
                        Err(e) => {
                            warn!("unable to load XInput2 library: {}", e.detail().to_string());
                            None
                        },
                        Ok(xi) => {
                            unsafe {
                                let mut result = Extension {
                                    lib: xi,
                                    opcode: mem::uninitialized(),
                                    first_event_id: mem::uninitialized(),
                                    first_error_id: mem::uninitialized(),
                                };
                                if 0 != (l.XQueryExtension)(d, b"XInputExtension\0".as_ptr() as *const libc::c_char,
                                                            &mut result.opcode as *mut libc::c_int,
                                                            &mut result.first_event_id as *mut libc::c_int,
                                                            &mut result.first_error_id as *mut libc::c_int
                                                            ) {
                                    let mut major = xinput2::XI_2_Major;
                                    let mut minor = xinput2::XI_2_Minor;
                                    if (result.lib.XIQueryVersion)(d, &mut major as *mut libc::c_int, &mut minor as *mut libc::c_int)
                                        == xlib::Success as i32 {
                                        Some(result)
                                    } else {
                                        warn!("X server missing support for required XInput2 version {}.{}, has {}.{}",
                                              xinput2::XI_2_Major, xinput2::XI_2_Minor,
                                              major, minor);
                                        None
                                    }
                                } else {
                                    warn!("X server missing XInput2 extension");
                                    None
                                }
                            }
                        }
                    };
                    let mut result = Context {
                        xlib: l,
                        atoms: atoms,
                        display: d,
                        buffer: VecDeque::new(),
                        xinput2: xinput2,
                    };
                    result.init();
                    Ok(result)
                }
            }
        }
    }

    pub fn flush(&self) {
        unsafe { (self.xlib.XFlush)(self.display); };
    }

    pub fn new_window(&self, builder: common::WindowBuilder) -> Result<WindowID, String> {
        let border_width = 0;
        let handle = unsafe {
            let screen = (self.xlib.XDefaultScreenOfDisplay)(self.display);
            let root = (self.xlib.XRootWindowOfScreen)(screen);
            let depth = (self.xlib.XDefaultDepthOfScreen)(screen);
            let visual = (self.xlib.XDefaultVisualOfScreen)(screen);
            let mut attrs : xlib::XSetWindowAttributes = mem::uninitialized();
            attrs.event_mask = xlib::StructureNotifyMask;
            attrs.background_pixel = (self.xlib.XBlackPixelOfScreen)(screen);
            (self.xlib.XCreateWindow)(self.display, root, builder.position.0, builder.position.1, builder.size.0, builder.size.1, border_width, depth,
                                      xlib::InputOutput as u32, visual, xlib::CWEventMask | xlib::CWBackPixel,
                                      &mut attrs as *mut xlib::XSetWindowAttributes)
        };
        // Opt into application handling of window close
        unsafe { (self.xlib.XSetWMProtocols)(self.display, handle,
                                             mem::transmute::<*const xlib::Atom, *mut xlib::Atom>(&self.atoms.wm_delete_window as *const xlib::Atom), 1); };
        let window = WindowID(handle);
        self.window_set_name(window, builder.name);
        Ok(window)
    }

    pub fn window_set_name(&self, window: WindowID, name: &str) {
        let c_name = CString::new(name).unwrap();
        unsafe { (self.xlib.XStoreName)(self.display, window.0, c_name.as_ptr()); };
    }

    pub fn window_map(&self, window: WindowID) {
        unsafe { (self.xlib.XMapWindow)(self.display, window.0); };
    }

    pub fn window_unmap(&self, window: WindowID) {
        unsafe { (self.xlib.XUnmapWindow)(self.display, window.0); };
    }

    fn pending(&self) -> libc::c_int {
        unsafe { (self.xlib.XPending)(self.display) }
    }

    fn next_event(&self) -> xlib::XEvent {
        unsafe {
            let mut event = mem::uninitialized();
            (self.xlib.XNextEvent)(self.display, &mut event as *mut xlib::XEvent);
            event
        }
    }

    fn query_device<'a>(&'a self, device: libc::c_int) -> Vec<xinput2::XIDeviceInfo> {
        let ext = self.xinput2.as_ref().unwrap();
        unsafe {
            let mut size = mem::uninitialized();
            let info = (ext.lib.XIQueryDevice)(self.display, device, &mut size as *mut libc::c_int);
            slice::from_raw_parts(info, size as usize).to_vec()
        }
    }

    fn default_root(&self) -> xlib::Window {
        unsafe { (self.xlib.XDefaultRootWindow)(self.display) }
    }

    fn init(&mut self) {
        let devices =
            if let &Some(ref ext) = &self.xinput2 {
                // Register for device hotplug events
                let mask = xinput2::XI_HierarchyChanged;
                unsafe {
                    let mut event_mask = xinput2::XIEventMask{
                        deviceid: xinput2::XIAllDevices,
                        mask: mem::transmute::<*const i32, *mut libc::c_uchar>(&mask as *const i32),
                        mask_len: mem::size_of_val(&mask) as libc::c_int,
                    };
                    (ext.lib.XISelectEvents)(self.display, self.default_root(),
                                             &mut event_mask as *mut xinput2::XIEventMask, 1);
                };

                self.query_device(xinput2::XIAllDevices)
            } else {
                Vec::new()
            };
        for device in devices {
            self.open_device(&device);
        }
    }

    fn open_device(&mut self, info: &xinput2::XIDeviceInfo) {
        let device = DeviceID(info.deviceid);
        let name = unsafe { CStr::from_ptr(info.name).to_string_lossy() };
        trace!("opening device {}: \"{}\"", info.deviceid, name);
        if info.enabled == 0 {
            trace!("device {:?} is disabled, bailing out", device);
            return;
        }

        if info._use == xinput2::XISlaveKeyboard || info._use == xinput2::XISlavePointer || info._use == xinput2::XIFloatingSlave {
            // Real hardware
            let xinput2 = &self.xinput2.as_ref().unwrap().lib;
            let mask = xinput2::XI_RawMotionMask | xinput2::XI_RawButtonPressMask | xinput2::XI_RawButtonReleaseMask
                | xinput2::XI_RawKeyPressMask | xinput2::XI_RawKeyReleaseMask;
            unsafe {
                let mut event_mask = xinput2::XIEventMask{
                    deviceid: info.deviceid,
                    mask: mem::transmute::<*const i32, *mut libc::c_uchar>(&mask as *const i32),
                    mask_len: mem::size_of_val(&mask) as libc::c_int,
                };
                (xinput2.XISelectEvents)(self.display, self.default_root(), &mut event_mask as *mut xinput2::XIEventMask, 1);
            };
        } else {
            trace!("deice {:?} is virtual, bailing out", device);
        }

        let classes : &[*const xinput2::XIAnyClassInfo] =
            unsafe { slice::from_raw_parts(info.classes as *const *const xinput2::XIAnyClassInfo, info.num_classes as usize) };
        for class_ptr in classes {
            use self::x11_dl::xinput2::*;
            let class = unsafe { &**class_ptr };
            match class._type {
                XIButtonClass => {
                    let button_info = unsafe { mem::transmute::<&XIAnyClassInfo, &XIButtonClassInfo>(class) };
                    let mask = unsafe { slice::from_raw_parts(button_info.state.mask, button_info.state.mask_len as usize) };
                    for i in 0..button_info.num_buttons {
                        self.buffer.push_back(Event::RawButton { device: device, button: ButtonID(i), pressed: XIMaskIsSet(mask, i) });
                    }
                },
                XIValuatorClass => {
                    let axis_info = unsafe { mem::transmute::<&XIAnyClassInfo, &XIValuatorClassInfo>(class) };
                    self.buffer.push_back(Event::RawMotion{ device: device, axis: AxisID(axis_info.number), value: axis_info.value });
                },
                ty => {
                    trace!("device {:?} has unrecognized class type {}", device, ty);
                },
            }
        }
    }

    fn atom_name(&self, atom: xlib::Atom) -> String {
        unsafe { CStr::from_ptr((self.xlib.XGetAtomName)(self.display, atom)).to_string_lossy().into_owned() }
    }

    #[allow(non_upper_case_globals)]
    fn queue_event(&mut self) {
        let storage = self.next_event();
        use self::x11_dl::xlib::*;
        let event = &XAnyEvent::from(&storage);
        let window = WindowID(event.window);
        match event.type_ {
            MapNotify => self.buffer.push_back(Event::Map(window)),
            UnmapNotify => self.buffer.push_back(Event::Unmap(window)),
            ClientMessage => {
                let xclient = &XClientMessageEvent::from(&storage);
                if xclient.message_type == self.atoms.wm_protocols {
                    let atom = xclient.data.get_long(0) as xlib::Atom;
                    if atom == self.atoms.wm_delete_window {
                        self.buffer.push_back(Event::Quit(window));
                    } else {
                        debug!("unhandled WM_PROTOCOLS message {}", self.atom_name(atom));
                    }
                } else {
                    debug!("unhandled client message type {}", self.atom_name(xclient.message_type));
                }
            },
            GenericEvent => {
                let mut xcookie = &mut XGenericEventCookie::from(storage);
                let mut handled = false;
                if let &Some(ref ext) = &self.xinput2 {
                    if xcookie.extension == ext.opcode {
                        handled = true;
                        unsafe { (self.xlib.XGetEventData)(self.display, xcookie as *mut xlib::XGenericEventCookie); };
                        use self::x11_dl::xinput2::*;
                        match xcookie.evtype {
                            XI_RawMotion => {
                                let evt = unsafe { &*mem::transmute::<*const ::std::os::raw::c_void, *const XIRawEvent>(xcookie.data) };
                                let mask = unsafe { slice::from_raw_parts(evt.valuators.mask, evt.valuators.mask_len as usize) };
                                let mut raw_value = evt.raw_values;
                                for i in 0..evt.valuators.mask_len*8 {
                                    if XIMaskIsSet(mask, i) {
                                        self.buffer.push_back(Event::RawMotion {
                                            device: DeviceID(evt.deviceid),
                                            axis: AxisID(i),
                                            value: unsafe { *raw_value }
                                        });
                                        raw_value = unsafe { raw_value.offset(1) };
                                    }
                                }
                            },
                            XI_RawButtonPress => {
                                let evt = unsafe { &*mem::transmute::<*const ::std::os::raw::c_void, *const XIRawEvent>(xcookie.data) };
                                self.buffer.push_back(Event::RawButton {
                                    device: DeviceID(evt.deviceid),
                                    button: ButtonID(evt.detail),
                                    pressed: true
                                });
                            },
                            XI_RawButtonRelease => {
                                let evt = unsafe { &*mem::transmute::<*const ::std::os::raw::c_void, *const XIRawEvent>(xcookie.data) };
                                self.buffer.push_back(Event::RawButton {
                                    device: DeviceID(evt.deviceid),
                                    button: ButtonID(evt.detail),
                                    pressed: false
                                });
                            },
                            ty => {
                                debug!("unhandled XInput2 event type {}", ty);
                            },
                        }
                        unsafe { (self.xlib.XFreeEventData)(self.display, xcookie as *mut xlib::XGenericEventCookie); };
                    }
                }
                if !handled {
                    debug!("unhandled event for unknown extension {}", xcookie.extension);
                }
            },
            ty => {
                debug!("unhandled event type {}", ty);
            },

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

impl EventStream {
    pub fn new(context: Context, handle: &Handle) -> io::Result<EventStream> {
        PollEvented::new(context, handle).map(|x| EventStream { io: x })
    }
}

impl Deref for EventStream {
    type Target = Context;

    fn deref(&self) -> &Context { self.io.get_ref() }
}

impl futures::stream::Stream for EventStream {
    type Item = Event<WindowID, DeviceID>;
    type Error = ();

    fn poll(&mut self) -> futures::Poll<Option<Self::Item>, Self::Error> {
        // Workaround for https://github.com/tokio-rs/tokio-core/issues/44 . Also an optimization regardless.
        if let futures::Async::NotReady = self.io.poll_read() {
            return Ok(futures::Async::NotReady);
        }

        while self.pending() != 0 {
            self.io.get_mut().queue_event();
        }

        Ok(match self.io.get_mut().buffer.pop_front() {
            None => {
                self.io.need_read();
                futures::Async::NotReady
            },
            Some(i) => futures::Async::Ready(Some(i)),
        })
    }
}
