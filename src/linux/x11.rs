//! X11 event handling
//!
//! This module employs the XInput2 extension to provide motion events with greater-than-pixel resolution when
//! available, allowing applications to make full use of, for example, highly sensitive mice, and provide smooth,
//! precise response for applications such as first-person games or hand-drawing.
//!
//! Text input is provided by the XKB extension and xkbcommon, and should behave consistently with the host system in
//! most circumstances, even in the face of unconventional and dynamically changing keymaps. Unfortunately, there is no
//! good standard for advanced input methods such as used for emoji or CJK text under X11, so such input is not handled.

extern crate x11;
extern crate xkbcommon;

use self::x11::xlib;
use self::x11::xinput2;
use self::xkbcommon::xkb;

use std::{io, mem, slice, ptr, fmt};
use std::collections::{VecDeque, HashMap};
use std::ffi::CStr;
use std::os::unix::io::{AsRawFd, RawFd};
use std::rc::Rc;
use std::cell::RefCell;

use libc::*;
use super::xcb;

use void::Void;
use mio;
use tokio_core::reactor::{PollEvented, Handle};
use futures;

use {Event, AxisId, ButtonId, Scancode};

#[derive(Debug, Copy, Clone)]
struct Extension {
    opcode: c_int,
    first_event_id: c_int,
    first_error_id: c_int,
}

struct Keyboard {
    map: xkb::Keymap,
    state: xkb::State,
    initial_state: xkb::State,
}

struct Shared {
    xcb: xcb::Connection,
    xinput2: Option<Extension>,
    atoms: Atoms,
    keyboards: HashMap<DeviceId, Keyboard>,
}

impl Shared {
    fn new() -> Option<Shared> {
        unsafe { xlib::XInitThreads(); };
        let d = unsafe { xlib::XOpenDisplay(ptr::null()) };
        if d.is_null() {
            return None;
        }
        let atoms = Atoms {
            device_node: unsafe { xlib::XInternAtom(d, b"Device Node\0".as_ptr() as *const c_char, 0) },
        };
        let xcb = unsafe { xcb::Connection::new_from_xlib_display(d as *mut x11::xlib::_XDisplay) };
        let xinput2 = unsafe {
            let mut result = Extension {
                opcode: mem::uninitialized(),
                first_event_id: mem::uninitialized(),
                first_error_id: mem::uninitialized(),
            };
            if 0 != xlib::XQueryExtension(d, b"XInputExtension\0".as_ptr() as *const c_char,
                                          &mut result.opcode as *mut c_int,
                                          &mut result.first_event_id as *mut c_int,
                                          &mut result.first_error_id as *mut c_int
            ) {
                let mut major = xinput2::XI_2_Major;
                let mut minor = xinput2::XI_2_Minor;
                if xinput2::XIQueryVersion(d, &mut major as *mut c_int, &mut minor as *mut c_int)
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
        };
        Some(Shared {
            xcb: xcb,
            xinput2: xinput2,
            atoms: atoms,
            keyboards: HashMap::new(),
        })
    }

    fn display(&self) -> *mut xlib::Display {
        self.xcb.get_raw_dpy()
    }

    fn query_device(&self, device: c_int) -> Vec<xinput2::XIDeviceInfo> {
        unsafe {
            let mut size = mem::uninitialized();
            let info = xinput2::XIQueryDevice(self.display(), device, &mut size as *mut c_int);
            slice::from_raw_parts(info, size as usize).to_vec()
        }
    }
}

pub struct Context(Rc<RefCell<Shared>>);

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct DeviceId(i32);

impl fmt::Debug for DeviceId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Copy, Clone)]
struct Atoms {
    device_node: xlib::Atom,
}

struct StreamInner(Rc<RefCell<Shared>>);

pub struct Stream {
    io: PollEvented<StreamInner>,
    buffer: VecDeque<(DeviceId, Event)>,
    xkb: xkb::Context,
    xkb_base_event: u8,
}


impl AsRawFd for StreamInner {
    fn as_raw_fd(&self) -> RawFd {
        unsafe { xlib::XConnectionNumber(self.0.borrow().display()) }
    }
}

impl Context {
    pub fn new(handle: &Handle) -> io::Result<(Context, Stream)> {
        let shared = match Shared::new() {
            None => return Err(io::Error::new(io::ErrorKind::Other, "unable to connect to display")),
            Some(x) => Rc::new(RefCell::new(x)),
        };
        
        let ctx = Context(shared.clone());
        let stream = Stream::new(shared, handle)?;
        Ok((ctx, stream))
    }

    pub fn device_scancode_name(&self, device: DeviceId, scancode: Scancode) -> String {
        let kb: &Keyboard = &self.0.borrow().keyboards[&device];
        let sym = kb.initial_state.key_get_one_sym(scancode.0);
        xkb::keysym_get_name(sym)
    }
}

impl mio::Evented for StreamInner {
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

impl Stream {
    fn new(shared: Rc<RefCell<Shared>>, handle: &Handle) -> io::Result<Stream> {
        let io = try!(PollEvented::new(StreamInner(shared), handle));
        let xkb_base_event = unsafe {
            let mut major_xkb_version = mem::uninitialized();
            let mut minor_xkb_version = mem::uninitialized();
            let mut base_event = mem::uninitialized();
            let mut base_error = mem::uninitialized();
            let success = xkb::x11::setup_xkb_extension(
                &io.get_ref().0.borrow().xcb, xkb::x11::MIN_MAJOR_XKB_VERSION, xkb::x11::MIN_MINOR_XKB_VERSION,
                xkb::x11::SetupXkbExtensionFlags::NoFlags,
                &mut major_xkb_version, &mut minor_xkb_version,
                &mut base_event, &mut base_error);
            if !success {
                return Err(io::Error::new(io::ErrorKind::Other, format!("X server does not support XKB {}.{}", xkb::x11::MIN_MAJOR_XKB_VERSION, xkb::x11::MIN_MINOR_XKB_VERSION)))
            }
            base_event
        };
        let mut result = Stream {
            io: io,
            buffer: VecDeque::new(),
            xkb: xkb::Context::new(0),
            xkb_base_event: xkb_base_event,
        };

        let devices =
            if result.io.get_ref().0.borrow().xinput2.is_some() {
                // Register for device hotplug events
                let mask = xinput2::XI_HierarchyChangedMask;
                unsafe {
                    let mut event_mask = xinput2::XIEventMask{
                        deviceid: xinput2::XIAllDevices,
                        mask: mem::transmute::<*const i32, *mut c_uchar>(&mask as *const i32),
                        mask_len: mem::size_of_val(&mask) as c_int,
                    };
                    xinput2::XISelectEvents(result.display(), result.default_root(),
                                            &mut event_mask as *mut xinput2::XIEventMask, 1);
                };

                result.io.get_ref().0.borrow().query_device(xinput2::XIAllDevices)
            } else {
                Vec::new()
            };
        for device in devices {
            result.open_device(&device);
        }

        Ok(result)
    }

    fn display(&self) -> *mut xlib::Display {
        self.io.get_ref().0.borrow().display()
    }

    fn default_root(&self) -> xlib::Window {
        unsafe { xlib::XDefaultRootWindow(self.display()) }
    }

    fn next_event(&self) -> xlib::XEvent {
        unsafe {
            let mut event = mem::uninitialized();
            xlib::XNextEvent(self.display(), &mut event as *mut xlib::XEvent);
            event
        }
    }

    fn pending(&self) -> c_int {
        unsafe { xlib::XPending(self.display()) }
    }

    fn atom_name(&self, atom: xlib::Atom) -> String {
        unsafe { CStr::from_ptr(xlib::XGetAtomName(self.display(), atom)).to_string_lossy().into_owned() }
    }


    #[allow(non_upper_case_globals)]
    fn open_device(&mut self, info: &xinput2::XIDeviceInfo) {
        let device = DeviceId(info.deviceid);
        let name = unsafe { CStr::from_ptr(info.name).to_string_lossy() };
        trace!("opening device {}: \"{}\"", info.deviceid, name);

        self.buffer.push_back((device, Event::Added));

        let real_device = info._use == xinput2::XISlaveKeyboard || info._use == xinput2::XISlavePointer || info._use == xinput2::XIFloatingSlave;
        if real_device {
            // Register for raw events
            let mask = xinput2::XI_RawMotionMask | xinput2::XI_RawButtonPressMask | xinput2::XI_RawButtonReleaseMask
                | xinput2::XI_RawKeyPressMask | xinput2::XI_RawKeyReleaseMask;
            unsafe {
                let mut event_mask = xinput2::XIEventMask{
                    deviceid: info.deviceid,
                    mask: mem::transmute::<*const i32, *mut c_uchar>(&mask as *const i32),
                    mask_len: mem::size_of_val(&mask) as c_int,
                };
                xinput2::XISelectEvents(self.display(), self.default_root(), &mut event_mask as *mut xinput2::XIEventMask, 1);
            };
        }

        let classes : &[*const xinput2::XIAnyClassInfo] =
            unsafe { slice::from_raw_parts(info.classes as *const *const xinput2::XIAnyClassInfo, info.num_classes as usize) };
        for class_ptr in classes {
            use self::x11::xinput2::*;
            let class = unsafe { &**class_ptr };
            match class._type {
                XIKeyClass => {
                    {
                        let mut shared = self.io.get_mut().0.borrow_mut();
                        let map = xkb::x11::keymap_new_from_device(&self.xkb, &shared.xcb, device.0, 0);
                        let state = xkb::x11::state_new_from_device(&map, &shared.xcb, device.0);
                        shared.keyboards.insert(device, Keyboard {
                            map: map,
                            initial_state: xkb::State::new(&state.get_keymap()),
                            state: state,
                        });
                    }
                    let mask = (xlib::XkbNewKeyboardNotifyMask | xlib::XkbMapNotifyMask | xlib::XkbStateNotifyMask) as u32;
                    unsafe { xlib::XkbSelectEvents(self.display(), device.0 as u32, mask, mask); };
                },
                XIButtonClass => {
                    if real_device {
                        let button_info = unsafe { mem::transmute::<&XIAnyClassInfo, &XIButtonClassInfo>(class) };
                        let mask = unsafe { slice::from_raw_parts(button_info.state.mask, button_info.state.mask_len as usize) };
                        for i in 0..button_info.num_buttons {
                            self.buffer.push_back((device, if XIMaskIsSet(mask, i) {
                                Event::ButtonPress { button: ButtonId(i as u32) }
                            } else {
                                Event::ButtonRelease { button: ButtonId(i as u32) }
                            }));
                        }
                    }
                },
                XIValuatorClass => {
                    if real_device {
                        let axis_info = unsafe { mem::transmute::<&XIAnyClassInfo, &XIValuatorClassInfo>(class) };
                        self.buffer.push_back((device, Event::Motion {
                            axis: AxisId(axis_info.number as u32),
                            value: axis_info.value
                        }));
                    }
                },
                ty => {
                    trace!("device {:?} has unrecognized class type {}", device, ty);
                },
            }
        }
    }

    #[allow(non_upper_case_globals)]
    fn queue_event(&mut self) {
        let storage = self.next_event();
        use self::x11::xlib::*;
        let event = &XAnyEvent::from(&storage);
        match event.type_ {
            GenericEvent => {
                let mut xcookie = &mut XGenericEventCookie::from(storage);
                let mut handled = false;
                let opcode = self.io.get_ref().0.borrow().xinput2.as_ref().map(|ext| ext.opcode);
                if let Some(opcode) = opcode {
                    if xcookie.extension == opcode {
                        handled = true;
                        unsafe { xlib::XGetEventData(self.display(), xcookie as *mut xlib::XGenericEventCookie); };
                        use self::x11::xinput2::*;
                        match xcookie.evtype {
                            XI_RawMotion => {
                                let evt = unsafe { &*mem::transmute::<*const ::std::os::raw::c_void, *const XIRawEvent>(xcookie.data) };
                                let mask = unsafe { slice::from_raw_parts(evt.valuators.mask, evt.valuators.mask_len as usize) };
                                let mut raw_value = evt.raw_values;
                                for i in 0..evt.valuators.mask_len*8 {
                                    if XIMaskIsSet(mask, i) {
                                        self.buffer.push_back((DeviceId(evt.deviceid), Event::Motion {
                                            axis: AxisId(i as u32),
                                            value: unsafe { *raw_value }
                                        }));
                                        raw_value = unsafe { raw_value.offset(1) };
                                    }
                                }
                            },
                            XI_RawButtonPress => {
                                let evt = unsafe { &*mem::transmute::<*const ::std::os::raw::c_void, *const XIRawEvent>(xcookie.data) };
                                self.buffer.push_back((DeviceId(evt.deviceid), Event::ButtonPress {
                                    button: ButtonId(evt.detail as u32),
                                }));
                            },
                            XI_RawButtonRelease => {
                                let evt = unsafe { &*mem::transmute::<*const ::std::os::raw::c_void, *const XIRawEvent>(xcookie.data) };
                                self.buffer.push_back((DeviceId(evt.deviceid), Event::ButtonRelease {
                                    button: ButtonId(evt.detail as u32),
                                }));
                            },
                            XI_RawKeyPress => {
                                let evt = unsafe { &*mem::transmute::<*const ::std::os::raw::c_void, *const XIRawEvent>(xcookie.data) };
                                let kb = &self.io.get_ref().0.borrow().keyboards[&DeviceId(evt.deviceid)];
                                self.buffer.push_back((DeviceId(evt.deviceid), Event::KeyPress {
                                    scancode: Scancode(evt.detail as u32),
                                    text: kb.state.key_get_utf8(evt.detail as u32),
                                }));
                            },
                            XI_RawKeyRelease => {
                                let evt = unsafe { &*mem::transmute::<*const ::std::os::raw::c_void, *const XIRawEvent>(xcookie.data) };
                                self.buffer.push_back((DeviceId(evt.deviceid), Event::KeyRelease {
                                    scancode: Scancode(evt.detail as u32),
                                }));
                            },
                            XI_HierarchyChanged => {
                                let evt = unsafe { &*mem::transmute::<*const ::std::os::raw::c_void, *const XIHierarchyEvent>(xcookie.data) };
                                for info in unsafe { slice::from_raw_parts(evt.info, evt.num_info as usize) } {
                                    if 0 != info.flags & (XISlaveAdded | XIMasterAdded) {
                                        let infos = self.io.get_ref().0.borrow().query_device(info.deviceid);
                                        for di in infos {
                                            self.buffer.push_back((DeviceId(di.deviceid), Event::Added));
                                            self.open_device(&di);
                                        }
                                    } else if 0 != info.flags & (XISlaveRemoved | XIMasterRemoved) {
                                        let id = DeviceId(info.deviceid);
                                        self.io.get_mut().0.borrow_mut().keyboards.remove(&id);
                                        self.buffer.push_back((id, Event::Removed));
                                    }
                                }
                            },
                            ty => {
                                debug!("unhandled XInput2 event type {}", ty);
                            },
                        }
                        unsafe { xlib::XFreeEventData(self.display(), xcookie as *mut xlib::XGenericEventCookie); };
                    }
                }
                if !handled {
                    debug!("unhandled event for unknown extension {}", xcookie.extension);
                }
            },
            ty => {
                if ty == self.xkb_base_event as i32 {
                    let mut shared = self.io.get_mut().0.borrow_mut();
                    let xkb_event = unsafe { mem::transmute::<&XEvent, &XkbAnyEvent>(&storage) };
                    let id = DeviceId(xkb_event.device as i32);
                    match xkb_event.xkb_type {
                        XkbNewKeyboardNotify => {} // Handled by hierarchy change events
                        XkbMapNotify => {
                            let map = self::xkbcommon::xkb::x11::keymap_new_from_device(&self.xkb, &shared.xcb, xkb_event.device as i32, 0);
                            let state = self::xkbcommon::xkb::x11::state_new_from_device(&map, &shared.xcb, xkb_event.device as i32);
                            let initial_state = self::xkbcommon::xkb::State::new(&state.get_keymap());
                            let mut kb = shared.keyboards.get_mut(&id).expect("unknown keyboard");
                            kb.map = map;
                            kb.state = state;
                            kb.initial_state = initial_state;
                            self.buffer.push_back((id, Event::KeymapChanged));
                        },
                        XkbStateNotify => {
                            let mut kb = shared.keyboards.get_mut(&id).expect("unknown keyboard");
                            let state = unsafe { mem::transmute::<&XEvent, &XkbStateNotifyEvent>(&storage) };
                            kb.state.update_mask(state.base_mods, state.latched_mods, state.locked_mods,
                                                 state.base_group as u32, state.latched_group as u32, state.locked_group as u32);
                        }
                        xkb_ty => {
                            debug!("unhandled XKB event type {}", xkb_ty);
                        }
                    }
                } else {
                    debug!("unhandled event type {}", ty);
                }
            },
        }
    }
}

impl futures::stream::Stream for Stream {
    type Item = (DeviceId, Event);
    type Error = Void;

    fn poll(&mut self) -> futures::Poll<Option<Self::Item>, Self::Error> {
        if let Some(x) = self.buffer.pop_front() {
            return Ok(futures::Async::Ready(Some(x)));
        }

        if self.pending() == 0 && futures::Async::NotReady == self.io.poll_read() {
            return Ok(futures::Async::NotReady);
        }

        while self.pending() != 0 && self.buffer.is_empty() {
            self.queue_event();
        }

        Ok(match self.buffer.pop_front() {
            None => {
                self.io.need_read();
                futures::Async::NotReady
            },
            Some(x) => futures::Async::Ready(Some(x)),
        })
    }
}
