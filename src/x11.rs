extern crate x11;
extern crate x11_dl;
extern crate xcb;
extern crate xkbcommon;

use self::x11_dl::xlib;
use self::x11_dl::xinput2;
use self::xkbcommon::xkb;

use std::{io, mem, slice, fmt};
use std::cell::RefCell;
use std::collections::{VecDeque, HashMap};
use std::ops::Deref;
use std::ffi::{CString, CStr};
use std::ptr;
use std::os::unix::io::{AsRawFd, RawFd};

use libc::*;

use mio;
use tokio_core::reactor::{PollEvented, Handle};
use futures;

use common;
use common::{Event, AxisID, ButtonID, ScanCode, KeySym};

struct Extension<T> {
    lib: T,
    opcode: c_int,
    first_event_id: c_int,
    first_error_id: c_int,
}

struct Keyboard {
    map: xkb::Keymap,
    state: xkb::State,
}

pub struct Context {
    xlib: xlib::Xlib,
    atoms: Atoms,
    xcb: xcb::base::Connection,
    display: *mut xlib::Display,
    xinput2: Option<Extension<xinput2::XInput2>>,
    buffer: RefCell<VecDeque<Event<WindowID, DeviceID>>>,
    xkb: xkb::Context,
    xkb_base_event: u8,
    keyboards: RefCell<HashMap<DeviceID, Keyboard>>,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct WindowID(xlib::Window);

impl fmt::Debug for WindowID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "WindowID({:X})", self.0)
    }
}

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
                        wm_protocols: unsafe { (l.XInternAtom)(d, b"WM_PROTOCOLS\0".as_ptr() as *const c_char, 0) },
                        wm_delete_window: unsafe { (l.XInternAtom)(d, b"WM_DELETE_WINDOW\0".as_ptr() as *const c_char, 0) },
                        device_node: unsafe { (l.XInternAtom)(d, b"Device Node\0".as_ptr() as *const c_char, 0) },
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
                                if 0 != (l.XQueryExtension)(d, b"XInputExtension\0".as_ptr() as *const c_char,
                                                            &mut result.opcode as *mut c_int,
                                                            &mut result.first_event_id as *mut c_int,
                                                            &mut result.first_error_id as *mut c_int
                                                            ) {
                                    let mut major = xinput2::XI_2_Major;
                                    let mut minor = xinput2::XI_2_Minor;
                                    if (result.lib.XIQueryVersion)(d, &mut major as *mut c_int, &mut minor as *mut c_int)
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
                    let xcb = unsafe { xcb::base::Connection::new_from_xlib_display(d as *mut x11::xlib::_XDisplay) };
                    let xkb_base_event = unsafe {
                        let mut major_xkb_version = mem::uninitialized();
                        let mut minor_xkb_version = mem::uninitialized();
                        let mut base_event = mem::uninitialized();
                        let mut base_error = mem::uninitialized();
                        let success = xkb::x11::setup_xkb_extension(
                            &xcb, xkb::x11::MIN_MAJOR_XKB_VERSION, xkb::x11::MIN_MINOR_XKB_VERSION,
                            xkb::x11::SetupXkbExtensionFlags::NoFlags,
                            &mut major_xkb_version, &mut minor_xkb_version,
                            &mut base_event, &mut base_error);
                        if !success {
                            panic!("X server missing XKB {}.{}", xkb::x11::MIN_MAJOR_XKB_VERSION, xkb::x11::MIN_MINOR_XKB_VERSION);
                        }
                        base_event
                    };
                                                  
                    let result = Context {
                        xlib: l,
                        atoms: atoms,
                        xcb: xcb,
                        display: d,
                        buffer: RefCell::new(VecDeque::new()),
                        xinput2: xinput2,
                        xkb: xkb::Context::new(0),
                        xkb_base_event: xkb_base_event,
                        keyboards: RefCell::new(HashMap::new()),
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

        // Subscribe to non-raw events
        if let &Some(ref ext) = &self.xinput2 {
            // Register for device hotplug events
            let mask = xinput2::XI_ButtonPressMask | xinput2::XI_ButtonReleaseMask | xinput2::XI_MotionMask
                | xinput2::XI_KeyPressMask | xinput2::XI_KeyReleaseMask;
            unsafe {
                let mut event_mask = xinput2::XIEventMask{
                    deviceid: xinput2::XIAllMasterDevices,
                    mask: mem::transmute::<*const i32, *mut c_uchar>(&mask as *const i32),
                    mask_len: mem::size_of_val(&mask) as c_int,
                };
                (ext.lib.XISelectEvents)(self.display, handle,
                                         &mut event_mask as *mut xinput2::XIEventMask, 1);
            };

            self.query_device(xinput2::XIAllDevices);
        }

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

    pub fn key_sym_name(key: KeySym) -> String {
        self::xkbcommon::xkb::keysym_get_name(key.0)
    }

    pub fn key_sym_from_name(name: &str) -> Option<KeySym> {
        let sym = self::xkbcommon::xkb::keysym_from_name(name, 0);
        if sym == xkb::keysyms::KEY_NoSymbol {
            None
        } else {
            Some(KeySym(sym))
        }
    }

    fn pending(&self) -> c_int {
        unsafe { (self.xlib.XPending)(self.display) }
    }

    fn next_event(&self) -> xlib::XEvent {
        unsafe {
            let mut event = mem::uninitialized();
            (self.xlib.XNextEvent)(self.display, &mut event as *mut xlib::XEvent);
            event
        }
    }

    fn query_device<'a>(&'a self, device: c_int) -> Vec<xinput2::XIDeviceInfo> {
        let ext = self.xinput2.as_ref().unwrap();
        unsafe {
            let mut size = mem::uninitialized();
            let info = (ext.lib.XIQueryDevice)(self.display, device, &mut size as *mut c_int);
            slice::from_raw_parts(info, size as usize).to_vec()
        }
    }

    fn default_root(&self) -> xlib::Window {
        unsafe { (self.xlib.XDefaultRootWindow)(self.display) }
    }

    fn init(&self) {
        let devices =
            if let &Some(ref ext) = &self.xinput2 {
                // Register for device hotplug events
                let mask = xinput2::XI_HierarchyChangedMask;
                unsafe {
                    let mut event_mask = xinput2::XIEventMask{
                        deviceid: xinput2::XIAllDevices,
                        mask: mem::transmute::<*const i32, *mut c_uchar>(&mask as *const i32),
                        mask_len: mem::size_of_val(&mask) as c_int,
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

    #[allow(non_upper_case_globals)]
    fn open_device(&self, info: &xinput2::XIDeviceInfo) {
        let device = DeviceID(info.deviceid);
        let name = unsafe { CStr::from_ptr(info.name).to_string_lossy() };
        trace!("opening device {}: \"{}\"", info.deviceid, name);

        let real_device = info._use == xinput2::XISlaveKeyboard || info._use == xinput2::XISlavePointer || info._use == xinput2::XIFloatingSlave;
        if real_device {
            // Register for raw events
            let xinput2 = &self.xinput2.as_ref().unwrap().lib;
            let mask = xinput2::XI_RawMotionMask | xinput2::XI_RawButtonPressMask | xinput2::XI_RawButtonReleaseMask
                | xinput2::XI_RawKeyPressMask | xinput2::XI_RawKeyReleaseMask;
            unsafe {
                let mut event_mask = xinput2::XIEventMask{
                    deviceid: info.deviceid,
                    mask: mem::transmute::<*const i32, *mut c_uchar>(&mask as *const i32),
                    mask_len: mem::size_of_val(&mask) as c_int,
                };
                (xinput2.XISelectEvents)(self.display, self.default_root(), &mut event_mask as *mut xinput2::XIEventMask, 1);
            };
        }

        let classes : &[*const xinput2::XIAnyClassInfo] =
            unsafe { slice::from_raw_parts(info.classes as *const *const xinput2::XIAnyClassInfo, info.num_classes as usize) };
        for class_ptr in classes {
            use self::x11_dl::xinput2::*;
            let class = unsafe { &**class_ptr };
            match class._type {
                XIKeyClass => {
                    let map = xkb::x11::keymap_new_from_device(&self.xkb, &self.xcb, device.0, 0);
                    let state = xkb::x11::state_new_from_device(&map, &self.xcb, device.0);
                    self.keyboards.borrow_mut().insert(device, Keyboard {
                        map: map,
                        state: state,
                    });
                    let mask = (1 << XkbNewKeyboardNotify) | (1 << XkbMapNotify) | (1 << XkbStateNotify);
                    unsafe { (self.xlib.XkbSelectEvents)(self.display, device.0 as u32, mask, mask); };
                },
                XIButtonClass => {
                    if real_device {
                        let button_info = unsafe { mem::transmute::<&XIAnyClassInfo, &XIButtonClassInfo>(class) };
                        let mask = unsafe { slice::from_raw_parts(button_info.state.mask, button_info.state.mask_len as usize) };
                        for i in 0..button_info.num_buttons {
                            self.buffer.borrow_mut().push_back(
                                if XIMaskIsSet(mask, i) {
                                    Event::RawButtonPress { device: device, button: ButtonID(i) }
                                } else {
                                    Event::RawButtonRelease { device: device, button: ButtonID(i) }
                                });
                        }
                    }
                },
                XIValuatorClass => {
                    if real_device {
                        let axis_info = unsafe { mem::transmute::<&XIAnyClassInfo, &XIValuatorClassInfo>(class) };
                        self.buffer.borrow_mut().push_back(Event::RawMotion{ device: device, axis: AxisID(axis_info.number), value: axis_info.value });
                    }
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
    fn queue_event(&self) {
        let storage = self.next_event();
        use self::x11_dl::xlib::*;
        let event = &XAnyEvent::from(&storage);
        let window = WindowID(event.window);
        let mut buffer = self.buffer.borrow_mut();
        let mut keyboards = self.keyboards.borrow_mut();
        match event.type_ {
            MapNotify => buffer.push_back(Event::Map(window)),
            UnmapNotify => buffer.push_back(Event::Unmap(window)),
            ClientMessage => {
                let xclient = &XClientMessageEvent::from(&storage);
                if xclient.message_type == self.atoms.wm_protocols {
                    let atom = xclient.data.get_long(0) as xlib::Atom;
                    if atom == self.atoms.wm_delete_window {
                        buffer.push_back(Event::Quit(window));
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
                if let Some(opcode) = self.xinput2.as_ref().map(|ext| ext.opcode) {
                    if xcookie.extension == opcode {
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
                                        buffer.push_back(Event::RawMotion {
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
                                buffer.push_back(Event::RawButtonPress {
                                    device: DeviceID(evt.deviceid),
                                    button: ButtonID(evt.detail),
                                });
                            },
                            XI_RawButtonRelease => {
                                let evt = unsafe { &*mem::transmute::<*const ::std::os::raw::c_void, *const XIRawEvent>(xcookie.data) };
                                buffer.push_back(Event::RawButtonRelease {
                                    device: DeviceID(evt.deviceid),
                                    button: ButtonID(evt.detail),
                                });
                            },
                            XI_RawKeyPress => {
                                let evt = unsafe { &*mem::transmute::<*const ::std::os::raw::c_void, *const XIRawEvent>(xcookie.data) };
                                let kb = &keyboards[&DeviceID(evt.deviceid)];
                                buffer.push_back(Event::RawKeyPress {
                                    device: DeviceID(evt.deviceid),
                                    scan_code: ScanCode(evt.detail as u32),
                                    key_sym: KeySym(kb.state.key_get_one_sym(evt.detail as u32)),
                                    text: kb.state.key_get_utf8(evt.detail as u32),
                                });
                            },
                            XI_RawKeyRelease => {
                                let evt = unsafe { &*mem::transmute::<*const ::std::os::raw::c_void, *const XIRawEvent>(xcookie.data) };
                                let kb = &keyboards[&DeviceID(evt.deviceid)];
                                buffer.push_back(Event::RawKeyRelease {
                                    device: DeviceID(evt.deviceid),
                                    scan_code: ScanCode(evt.detail as u32),
                                    key_sym: KeySym(kb.state.key_get_one_sym(evt.detail as u32)),
                                });
                            },
                            XI_KeyPress => {
                                let evt = unsafe { &*mem::transmute::<*const ::std::os::raw::c_void, *const XIDeviceEvent>(xcookie.data) };
                                let kb = &keyboards[&DeviceID(evt.deviceid)];
                                buffer.push_back(Event::KeyPress {
                                    device: DeviceID(evt.deviceid),
                                    scan_code: ScanCode(evt.detail as u32),
                                    key_sym: KeySym(kb.state.key_get_one_sym(evt.detail as u32)),
                                    text: kb.state.key_get_utf8(evt.detail as u32),
                                });
                            },
                            XI_KeyRelease => {
                                let evt = unsafe { &*mem::transmute::<*const ::std::os::raw::c_void, *const XIDeviceEvent>(xcookie.data) };
                                let kb = &keyboards[&DeviceID(evt.deviceid)];
                                buffer.push_back(Event::KeyRelease {
                                    device: DeviceID(evt.deviceid),
                                    key_sym: KeySym(kb.state.key_get_one_sym(evt.detail as u32)),
                                    scan_code: ScanCode(evt.detail as u32),
                                });
                            },
                            XI_ButtonPress => {
                                let evt = unsafe { &*mem::transmute::<*const ::std::os::raw::c_void, *const XIDeviceEvent>(xcookie.data) };
                                buffer.push_back(Event::ButtonPress {
                                    device: DeviceID(evt.deviceid),
                                    button: ButtonID(evt.detail),
                                });
                            },
                            XI_ButtonRelease => {
                                let evt = unsafe { &*mem::transmute::<*const ::std::os::raw::c_void, *const XIDeviceEvent>(xcookie.data) };
                                buffer.push_back(Event::ButtonRelease {
                                    device: DeviceID(evt.deviceid),
                                    button: ButtonID(evt.detail),
                                });
                            },
                            XI_Motion => {
                                let evt = unsafe { &*mem::transmute::<*const ::std::os::raw::c_void, *const XIDeviceEvent>(xcookie.data) };
                                let mask = unsafe { slice::from_raw_parts(evt.valuators.mask, evt.valuators.mask_len as usize) };
                                let mut value = evt.valuators.values;
                                for i in 0..evt.valuators.mask_len*8 {
                                    if XIMaskIsSet(mask, i) {
                                        buffer.push_back(Event::Motion {
                                            device: DeviceID(evt.deviceid),
                                            axis: AxisID(i),
                                            value: unsafe { *value }
                                        });
                                        value = unsafe { value.offset(1) };
                                    }
                                };
                                buffer.push_back(Event::PointerMotion {
                                    device: DeviceID(evt.deviceid),
                                    pos: (evt.event_x, evt.event_y),
                                });
                            },
                            XI_HierarchyChanged => {
                                let evt = unsafe { &*mem::transmute::<*const ::std::os::raw::c_void, *const XIHierarchyEvent>(xcookie.data) };
                                for info in unsafe { slice::from_raw_parts(evt.info, evt.num_info as usize) } {
                                    if 0 != info.flags & (XISlaveAdded | XIMasterAdded) {
                                        for di in self.query_device(info.deviceid) {
                                            buffer.push_back(Event::DeviceChange {
                                                device: DeviceID(di.deviceid),
                                                connected: true,
                                            });
                                            self.open_device(&di);
                                        }
                                    } else if 0 != info.flags & (XISlaveRemoved | XIMasterRemoved) {
                                        buffer.push_back(Event::DeviceChange {
                                            device: DeviceID(info.deviceid),
                                            connected: false,
                                        });
                                    }
                                }
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
                if ty == self.xkb_base_event as i32 {
                    let xkb_event = unsafe { mem::transmute::<&XEvent, &XkbAnyEvent>(&storage) };
                    let mut kb = keyboards.get_mut(&DeviceID(xkb_event.device as i32)).unwrap();
                    match xkb_event.xkb_type {
                        XkbNewKeyboardNotify => {
                            kb.map = self::xkbcommon::xkb::x11::keymap_new_from_device(&self.xkb, &self.xcb, xkb_event.device as i32, 0);
                            kb.state = self::xkbcommon::xkb::x11::state_new_from_device(&kb.map, &self.xcb, xkb_event.device as i32);
                        },
                        XkbMapNotify => {
                            kb.map = self::xkbcommon::xkb::x11::keymap_new_from_device(&self.xkb, &self.xcb, xkb_event.device as i32, 0);
                            kb.state = self::xkbcommon::xkb::x11::state_new_from_device(&kb.map, &self.xcb, xkb_event.device as i32);
                        },
                        XkbStateNotify => {
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

impl<'a> futures::stream::Stream for &'a EventStream {
    type Item = Event<WindowID, DeviceID>;
    type Error = ();

    fn poll(&mut self) -> futures::Poll<Option<Self::Item>, Self::Error> {
        // Workaround for https://github.com/tokio-rs/tokio-core/issues/44 . Also an optimization regardless.
        if let futures::Async::NotReady = self.io.poll_read() {
            return Ok(futures::Async::NotReady);
        }

        while self.pending() != 0 {
            self.queue_event();
        }

        Ok(match self.buffer.borrow_mut().pop_front() {
            None => {
                self.io.need_read();
                futures::Async::NotReady
            },
            Some(i) => futures::Async::Ready(Some(i)),
        })
    }
}

// Missing from x11 for some reason
#[repr(C)]
struct XkbAnyEvent {
    type_: c_int,
    serial: c_ulong,
    send_event: c_int,
    display: *mut xlib::Display,
    time: c_ulong,
    xkb_type: c_int,
    device: c_uint,
}

#[repr(C)]
struct XkbStateNotifyEvent {
    type_: c_int,
    serial: c_ulong,
    send_event: c_int,
    display: *mut xlib::Display,
    time: c_ulong,
    xkb_type: c_int,
    device: c_uint,
    changed: c_uint,
    group: c_int,
    base_group: c_int,
    latched_group: c_int, // latched keyboard group
    locked_group: c_int, // locked keyboard group
    mods: c_uint, // modifier state
    base_mods: c_uint, // base modifier state
    latched_mods: c_uint, // latched modifiers
    locked_mods: c_uint, // locked modifiers
    compat_state: c_int, // compatibility state
    grab_mods: c_uchar, // mods used for grabs
    compat_grab_mods: c_uchar, // grab mods for non-XKB clients
    lookup_mods: c_uchar, // mods sent to clients
    compat_lookup_mods: c_uchar, // mods sent to non-XKB clients
    ptr_buttons: c_int, // pointer button state
    keycode: c_uchar, // keycode that caused the change
    event_type: c_char, // KeyPress or KeyRelease
    req_major: c_char, // Major opcode of request
    req_minor: c_char, // Minor opcode of request
}

#[allow(non_upper_case_globals)]
const XkbNewKeyboardNotify: i32 = 0;
#[allow(non_upper_case_globals)]
const XkbMapNotify: i32 = 1;
#[allow(non_upper_case_globals)]
const XkbStateNotify: i32 = 2;
