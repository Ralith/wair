//! X11 window creation and event handling
//!
//! This module employs the XInput2 extension to provide motion events with greater-than-pixel resolution when
//! available, allowing applications to make full use of, for example, highly sensitive mice, and provide smooth,
//! precise response for applications such as first-person games or hand-drawing.
//!
//! Note that the mouse pointer and any physical mouse devices are different X11 devices. The former emits window events
//! and only does so when the mouse pointer is over one of your windows and operates in absolute coordinates based on
//! the origin of the window, while the latter emit device events in response to all motion and operate in relative
//! coordinates.
//!
//! Text input is provided by the XKB extension and xkbcommon, and should behave consistently with the host system in
//! most circumstances, even in the face of unconventional and dynamically changing keymaps. Unfortunately, there is no
//! good standard for advanced input methods such as used for emoji or japanese text under X11, so such input is not
//! handled.

extern crate x11;
extern crate xkbcommon;

use self::x11::xlib;
use self::x11::xinput2;
use self::xkbcommon::xkb;

use std::{io, mem, slice, fmt, ptr};
use std::collections::{VecDeque, HashMap};
use std::collections::hash_map::Entry;
use std::ffi::{CString, CStr};
use std::os::unix::io::{AsRawFd, RawFd};
use std::rc::Rc;
#[cfg(feature = "vulkano")]
use std::sync::Arc;

use libc::*;
use xcb;

#[cfg(feature = "vulkano")]
use vulkano;
use void::Void;
use mio;
use tokio_core::reactor::{PollEvented, Handle};
use futures;

use common;
use common::{Event, WindowEvent, DeviceEvent, InputEvent, AxisID, ButtonID, Keycode, Keysym, Wrapper};

#[derive(Debug, Copy, Clone)]
struct Extension {
    opcode: c_int,
    first_event_id: c_int,
    first_error_id: c_int,
}

struct Keyboard {
    map: xkb::Keymap,
    state: xkb::State,
}

struct Shared {
    xcb: xcb::Connection,
    xinput2: Option<Extension>,
    atoms: Atoms,
}

impl Shared {
    fn new() -> Option<Shared> {
        unsafe { xlib::XInitThreads(); };
        let d = unsafe { xlib::XOpenDisplay(ptr::null()) };
        if d.is_null() {
            return None;
        }
        let atoms = Atoms {
            wm_protocols: unsafe { xlib::XInternAtom(d, b"WM_PROTOCOLS\0".as_ptr() as *const c_char, 0) },
            wm_delete_window: unsafe { xlib::XInternAtom(d, b"WM_DELETE_WINDOW\0".as_ptr() as *const c_char, 0) },
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

pub struct Context(Rc<Shared>);

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct WindowID(xlib::Window);

impl fmt::Debug for WindowID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_tuple("WindowID").field(&self.0).finish()
    }
}

impl Wrapper for WindowID {
    // It's safe to pun xlib/xcb window types according to https://xcb.freedesktop.org/MixingCalls/
    type Native = xcb::ffi::xproto::xcb_window_t;
    fn get_native(&self) -> Self::Native { self.0 as Self::Native }
}

impl common::WindowID for WindowID {}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct DeviceID(i32);

impl common::DeviceID for DeviceID {}

#[derive(Copy, Clone)]
struct Atoms {
    wm_protocols: xlib::Atom,
    wm_delete_window: xlib::Atom,
    device_node: xlib::Atom,
}

struct StreamInner(Rc<Shared>);

struct WindowData {
    size: (u32, u32),
    position: (i32, i32),
}

impl WindowData {
    fn new(event: &xlib::XConfigureEvent) -> WindowData {
        WindowData {
            size: (event.width as u32, event.height as u32),
            position: (event.x as i32, event.y as i32),
        }
    }
}

pub struct Stream {
    io: PollEvented<StreamInner>,
    buffer: VecDeque<Event<WindowID, DeviceID>>,
    xkb: xkb::Context,
    xkb_base_event: u8,
    keyboards: HashMap<DeviceID, Keyboard>,
    windows: HashMap<WindowID, WindowData>,
}


impl AsRawFd for StreamInner {
    fn as_raw_fd(&self) -> RawFd {
        unsafe { xlib::XConnectionNumber(self.0.display()) }
    }
}

impl Context {
    pub fn flush(&self) {
        unsafe { xlib::XFlush(self.0.display()); };
    }

    pub fn window_set_name(&self, window: WindowID, name: &str) {
        let c_name = CString::new(name).unwrap(); // FIXME: Filter nulls
        unsafe { xlib::XStoreName(self.0.display(), window.0, c_name.as_ptr()); };
    }

    pub fn keysym_name(key: Keysym) -> String {
        self::xkbcommon::xkb::keysym_get_name(key.0)
    }

    pub fn keysym_from_name(name: &str) -> Option<Keysym> {
        let sym = self::xkbcommon::xkb::keysym_from_name(name, 0);
        if sym == xkb::keysyms::KEY_NoSymbol {
            None
        } else {
            Some(Keysym(sym))
        }
    }

    fn window_map(&self, window: WindowID) {
        unsafe { xlib::XMapWindow(self.0.display(), window.0); };
    }

    fn window_unmap(&self, window: WindowID) {
        unsafe { xlib::XUnmapWindow(self.0.display(), window.0); };
    }
}

impl Wrapper for Context {
    type Native = *mut xcb::ffi::base::xcb_connection_t;
    fn get_native(&self) -> Self::Native { self.0.xcb.get_raw_conn() }
}

impl common::WindowSystem for Context {
    type WindowID = WindowID;
    type DeviceID = DeviceID;
    type EventStream = Stream;
    fn open(handle: &Handle) -> io::Result<(Context, Stream)> {
        let shared = match Shared::new() {
            None => return Err(io::Error::new(io::ErrorKind::Other, "unable to connect to display")),
            Some(x) => Rc::new(x),
        };
        
        let ctx = Context(shared.clone());
        let stream = try!(Stream::new(shared, handle));
        Ok((ctx, stream))
    }

    fn new_window(&self, position: (i32, i32), size: (u32, u32), name: Option<&str>) -> WindowID {
        let border_width = 0;
        let handle = unsafe {
            let screen = xlib::XDefaultScreenOfDisplay(self.0.display());
            let root = xlib::XRootWindowOfScreen(screen);
            let depth = xlib::XDefaultDepthOfScreen(screen);
            let visual = xlib::XDefaultVisualOfScreen(screen);
            let mut attrs : xlib::XSetWindowAttributes = mem::uninitialized();
            attrs.event_mask = xlib::StructureNotifyMask | xlib::FocusChangeMask;
            attrs.background_pixel = xlib::XBlackPixelOfScreen(screen);
            xlib::XCreateWindow(self.0.display(), root, position.0, position.1, size.0, size.1, border_width, depth,
                                xlib::InputOutput as u32, visual, xlib::CWEventMask | xlib::CWBackPixel,
                                &mut attrs as *mut xlib::XSetWindowAttributes)
        };

        // Subscribe to non-raw events
        if self.0.xinput2.is_some() {
            let mask = xinput2::XI_ButtonPressMask | xinput2::XI_ButtonReleaseMask | xinput2::XI_MotionMask
                | xinput2::XI_KeyPressMask | xinput2::XI_KeyReleaseMask;
            unsafe {
                let mut event_mask = xinput2::XIEventMask{
                    deviceid: xinput2::XIAllMasterDevices,
                    mask: mem::transmute::<*const i32, *mut c_uchar>(&mask as *const i32),
                    mask_len: mem::size_of_val(&mask) as c_int,
                };
                xinput2::XISelectEvents(self.0.display(), handle,
                                        &mut event_mask as *mut xinput2::XIEventMask, 1);
            };
        }

        // Opt into application handling of window close
        unsafe { xlib::XSetWMProtocols(self.0.display(), handle,
                                       mem::transmute::<*const xlib::Atom, *mut xlib::Atom>(&self.0.atoms.wm_delete_window as *const xlib::Atom), 1); };
        let window = WindowID(handle);
        if let Some(name) = name { self.window_set_name(window, name); }
        self.window_map(window);
        trace!("created window {:?}", window);
        window
    }

    #[cfg(feature = "vulkano")]
    unsafe fn create_vulkan_surface(&self, instance: &Arc<vulkano::instance::Instance>, window: Self::WindowID)
                                    -> Result<Arc<vulkano::swapchain::Surface>, vulkano::swapchain::SurfaceCreationError> {
        vulkano::swapchain::Surface::from_xcb(instance, self.get_native(), window.get_native())
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
    fn new(shared: Rc<Shared>, handle: &Handle) -> io::Result<Stream> {
        let io = try!(PollEvented::new(StreamInner(shared), handle));
        let xkb_base_event = unsafe {
            let mut major_xkb_version = mem::uninitialized();
            let mut minor_xkb_version = mem::uninitialized();
            let mut base_event = mem::uninitialized();
            let mut base_error = mem::uninitialized();
            let success = xkb::x11::setup_xkb_extension(
                &io.get_ref().0.xcb, xkb::x11::MIN_MAJOR_XKB_VERSION, xkb::x11::MIN_MINOR_XKB_VERSION,
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
            keyboards: HashMap::new(),
            windows: HashMap::new(),
        };

        let devices =
            if result.io.get_ref().0.xinput2.is_some() {
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

                result.io.get_ref().0.query_device(xinput2::XIAllDevices)
            } else {
                Vec::new()
            };
        for device in devices {
            result.open_device(&device);
        }

        Ok(result)
    }

    fn display(&self) -> *mut xlib::Display {
        self.io.get_ref().0.display()
    }

    fn connection(&self) -> &xcb::Connection {
        &self.io.get_ref().0.xcb
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
        let device = DeviceID(info.deviceid);
        let name = unsafe { CStr::from_ptr(info.name).to_string_lossy() };
        trace!("opening device {}: \"{}\"", info.deviceid, name);

        self.buffer.push_back(Event::Device {
            device: device,
            event: DeviceEvent::Added,
        });

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
                    let map = xkb::x11::keymap_new_from_device(&self.xkb, self.connection(), device.0, 0);
                    let state = xkb::x11::state_new_from_device(&map, self.connection(), device.0);
                    self.keyboards.insert(device, Keyboard {
                        map: map,
                        state: state,
                    });
                    let mask = (xlib::XkbNewKeyboardNotifyMask | xlib::XkbMapNotifyMask | xlib::XkbStateNotifyMask) as u32;
                    unsafe { xlib::XkbSelectEvents(self.display(), device.0 as u32, mask, mask); };
                },
                XIButtonClass => {
                    if real_device {
                        let button_info = unsafe { mem::transmute::<&XIAnyClassInfo, &XIButtonClassInfo>(class) };
                        let mask = unsafe { slice::from_raw_parts(button_info.state.mask, button_info.state.mask_len as usize) };
                        for i in 0..button_info.num_buttons {
                            self.buffer.push_back(Event::Device {
                                device: device,
                                event: DeviceEvent::Input(if XIMaskIsSet(mask, i) {
                                    InputEvent::ButtonPress { button: ButtonID(i as u32) }
                                } else {
                                    InputEvent::ButtonRelease { button: ButtonID(i as u32) }
                                })
                            });
                        }
                    }
                },
                XIValuatorClass => {
                    if real_device {
                        let axis_info = unsafe { mem::transmute::<&XIAnyClassInfo, &XIValuatorClassInfo>(class) };
                        self.buffer.push_back(Event::Device {
                            device: device,
                            event: DeviceEvent::Input(InputEvent::Motion {
                                axis: AxisID(axis_info.number as u32),
                                value: axis_info.value
                            })
                        });
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
        use DeviceEvent::*;

        let storage = self.next_event();
        use self::x11::xlib::*;
        let event = &XAnyEvent::from(&storage);
        let window = WindowID(event.window);
        trace!("hadling event for window {:?}", window);
        match event.type_ {
            DestroyNotify => {
                self.windows.remove(&window);
                trace!("window destroyed")
            },
            MapNotify => self.buffer.push_back(Event::Window { window: window, event: WindowEvent::Map }),
            UnmapNotify => self.buffer.push_back(Event::Window { window: window, event: WindowEvent::Unmap }),
            ClientMessage => {
                let xclient = &XClientMessageEvent::from(&storage);
                if xclient.message_type == self.io.get_ref().0.atoms.wm_protocols {
                    let atom = xclient.data.get_long(0) as xlib::Atom;
                    if atom == self.io.get_ref().0.atoms.wm_delete_window {
                        self.buffer.push_back(Event::Window { window: window, event: WindowEvent::Quit });
                    } else {
                        debug!("unhandled WM_PROTOCOLS message {}", self.atom_name(atom));
                    }
                } else {
                    debug!("unhandled client message type {}", self.atom_name(xclient.message_type));
                }
            },
            ConfigureNotify => {
                let configure = XConfigureEvent::from(storage);
                let size = (configure.width as u32, configure.height as u32);
                let position = (configure.x as i32, configure.y as i32);
                match self.windows.entry(window) {
                    Entry::Vacant(e) => {
                        e.insert(WindowData::new(&configure));
                        self.buffer.push_back(Event::Window { window: window, event: WindowEvent::Move(position) });
                        self.buffer.push_back(Event::Window { window: window, event: WindowEvent::Resize(size) });
                    },
                    Entry::Occupied(mut e) => {
                        let info = e.get_mut();
                        if info.position != position {
                            self.buffer.push_back(Event::Window { window: window, event: WindowEvent::Move(position) });
                            info.position = position;
                        }
                        if info.size != size {
                            self.buffer.push_back(Event::Window { window: window, event: WindowEvent::Resize(size) });
                            info.size = size;
                        }
                    },
                }
            },
            GenericEvent => {
                let mut xcookie = &mut XGenericEventCookie::from(storage);
                let mut handled = false;
                if let Some(opcode) = self.io.get_ref().0.xinput2.as_ref().map(|ext| ext.opcode) {
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
                                        self.buffer.push_back(Event::Device {
                                            device: DeviceID(evt.deviceid),
                                            event: Input(InputEvent::Motion {
                                                axis: AxisID(i as u32),
                                                value: unsafe { *raw_value }
                                            })
                                        });
                                        raw_value = unsafe { raw_value.offset(1) };
                                    }
                                }
                            },
                            XI_RawButtonPress => {
                                let evt = unsafe { &*mem::transmute::<*const ::std::os::raw::c_void, *const XIRawEvent>(xcookie.data) };
                                self.buffer.push_back(Event::Device {
                                    device: DeviceID(evt.deviceid),
                                    event: Input(InputEvent::ButtonPress {
                                        button: ButtonID(evt.detail as u32),
                                    })
                                });
                            },
                            XI_RawButtonRelease => {
                                let evt = unsafe { &*mem::transmute::<*const ::std::os::raw::c_void, *const XIRawEvent>(xcookie.data) };
                                self.buffer.push_back(Event::Device {
                                    device: DeviceID(evt.deviceid),
                                    event: Input(InputEvent::ButtonRelease {
                                        button: ButtonID(evt.detail as u32),
                                    })
                                });
                            },
                            XI_RawKeyPress => {
                                let evt = unsafe { &*mem::transmute::<*const ::std::os::raw::c_void, *const XIRawEvent>(xcookie.data) };
                                let kb = &self.keyboards[&DeviceID(evt.deviceid)];
                                self.buffer.push_back(Event::Device {
                                    device: DeviceID(evt.deviceid),
                                    event: Input(InputEvent::KeyPress {
                                        keycode: Keycode(evt.detail as u32),
                                        keysym: Keysym(kb.state.key_get_one_sym(evt.detail as u32)),
                                        text: kb.state.key_get_utf8(evt.detail as u32),
                                    })
                                });
                            },
                            XI_RawKeyRelease => {
                                let evt = unsafe { &*mem::transmute::<*const ::std::os::raw::c_void, *const XIRawEvent>(xcookie.data) };
                                let kb = &self.keyboards[&DeviceID(evt.deviceid)];
                                self.buffer.push_back(Event::Device {
                                    device: DeviceID(evt.deviceid),
                                    event: Input(InputEvent::KeyRelease {
                                        keycode: Keycode(evt.detail as u32),
                                        keysym: Keysym(kb.state.key_get_one_sym(evt.detail as u32)),
                                    })
                                });
                            },
                            XI_KeyPress => {
                                let evt = unsafe { &*mem::transmute::<*const ::std::os::raw::c_void, *const XIDeviceEvent>(xcookie.data) };
                                let kb = &self.keyboards[&DeviceID(evt.deviceid)];
                                self.buffer.push_back(Event::Window {
                                    window: WindowID(evt.event),
                                    event: WindowEvent::Input {
                                        device: DeviceID(evt.deviceid),
                                        event: InputEvent::KeyPress {
                                            keycode: Keycode(evt.detail as u32),
                                            keysym: Keysym(kb.state.key_get_one_sym(evt.detail as u32)),
                                            text: kb.state.key_get_utf8(evt.detail as u32),
                                        }
                                    }
                                });
                            },
                            XI_KeyRelease => {
                                let evt = unsafe { &*mem::transmute::<*const ::std::os::raw::c_void, *const XIDeviceEvent>(xcookie.data) };
                                let kb = &self.keyboards[&DeviceID(evt.deviceid)];
                                self.buffer.push_back(Event::Window {
                                    window: WindowID(evt.event),
                                    event: WindowEvent::Input {
                                        device: DeviceID(evt.deviceid),
                                        event: InputEvent::KeyRelease {
                                            keycode: Keycode(evt.detail as u32),
                                            keysym: Keysym(kb.state.key_get_one_sym(evt.detail as u32)),
                                        }
                                    }
                                });
                            },
                            XI_ButtonPress => {
                                let evt = unsafe { &*mem::transmute::<*const ::std::os::raw::c_void, *const XIDeviceEvent>(xcookie.data) };
                                self.buffer.push_back(Event::Window {
                                    window: WindowID(evt.event),
                                    event: WindowEvent::Input {
                                        device: DeviceID(evt.deviceid),
                                        event: InputEvent::ButtonPress {
                                            button: ButtonID(evt.detail as u32),
                                        }
                                    }
                                });
                            },
                            XI_ButtonRelease => {
                                let evt = unsafe { &*mem::transmute::<*const ::std::os::raw::c_void, *const XIDeviceEvent>(xcookie.data) };
                                self.buffer.push_back(Event::Window {
                                    window: WindowID(evt.event),
                                    event: WindowEvent::Input {
                                        device: DeviceID(evt.deviceid),
                                        event: InputEvent::ButtonRelease {
                                            button: ButtonID(evt.detail as u32),
                                        }
                                    }
                                });
                            },
                            XI_Motion => {
                                let evt = unsafe { &*mem::transmute::<*const ::std::os::raw::c_void, *const XIDeviceEvent>(xcookie.data) };
                                let mask = unsafe { slice::from_raw_parts(evt.valuators.mask, evt.valuators.mask_len as usize) };
                                let mut value = evt.valuators.values;
                                for i in 0..evt.valuators.mask_len*8 {
                                    if XIMaskIsSet(mask, i) {
                                        self.buffer.push_back(Event::Window {
                                            window: WindowID(evt.event),
                                            event: WindowEvent::Input {
                                                device: DeviceID(evt.deviceid),
                                                event: InputEvent::Motion {
                                                    axis: AxisID(i as u32),
                                                    value: unsafe { *value }
                                                }
                                            }
                                        });
                                        value = unsafe { value.offset(1) };
                                    }
                                };
                                self.buffer.push_back(Event::Window {
                                    window: WindowID(evt.event),
                                    event: WindowEvent::PointerMotion {
                                        device: DeviceID(evt.deviceid),
                                        pos: (evt.event_x, evt.event_y),
                                    }
                                });
                            },
                            XI_HierarchyChanged => {
                                let evt = unsafe { &*mem::transmute::<*const ::std::os::raw::c_void, *const XIHierarchyEvent>(xcookie.data) };
                                for info in unsafe { slice::from_raw_parts(evt.info, evt.num_info as usize) } {
                                    if 0 != info.flags & (XISlaveAdded | XIMasterAdded) {
                                        for di in self.io.get_ref().0.query_device(info.deviceid) {
                                            self.buffer.push_back(Event::Device {
                                                device: DeviceID(di.deviceid),
                                                event: Added,
                                            });
                                            self.open_device(&di);
                                        }
                                    } else if 0 != info.flags & (XISlaveRemoved | XIMasterRemoved) {
                                        let id = DeviceID(info.deviceid);
                                        self.keyboards.remove(&id);
                                        self.buffer.push_back(Event::Device {
                                            device: id,
                                            event: Removed,
                                        });
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
            FocusIn => {
                self.buffer.push_back(Event::Window {
                    window: window,
                    event: WindowEvent::FocusIn
                });
            }
            FocusOut => {
                self.buffer.push_back(Event::Window {
                    window: window,
                    event: WindowEvent::FocusOut
                });
            }
            ty => {
                if ty == self.xkb_base_event as i32 {
                    let conn = &self.io.get_ref().0.xcb; // Manually inlined call to connection() to satisfy borrowck
                    let xkb_event = unsafe { mem::transmute::<&XEvent, &XkbAnyEvent>(&storage) };
                    let mut kb = self.keyboards.get_mut(&DeviceID(xkb_event.device as i32)).expect("unknown keyboard");
                    match xkb_event.xkb_type {
                        XkbNewKeyboardNotify => {
                            kb.map = self::xkbcommon::xkb::x11::keymap_new_from_device(&self.xkb, conn, xkb_event.device as i32, 0);
                            kb.state = self::xkbcommon::xkb::x11::state_new_from_device(&kb.map, conn, xkb_event.device as i32);
                        },
                        XkbMapNotify => {
                            kb.map = self::xkbcommon::xkb::x11::keymap_new_from_device(&self.xkb, conn, xkb_event.device as i32, 0);
                            kb.state = self::xkbcommon::xkb::x11::state_new_from_device(&kb.map, conn, xkb_event.device as i32);
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

impl futures::stream::Stream for Stream {
    type Item = Event<WindowID, DeviceID>;
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
