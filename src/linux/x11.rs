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
use super::nix;
use super::xcb;
use super::libudev as udev;
use super::helpers::dev_hwid;

use void::Void;
use mio;
use tokio_core::reactor::{PollEvented, Handle};
use futures;

use {Event, RelAxisId, AbsAxisId, ButtonId, Scancode, DeviceHwId};

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
    udev: udev::Context,
}

impl Shared {
    fn new(udev: udev::Context) -> Option<Shared> {
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
            udev: udev,
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

    fn atom_name(&self, atom: xlib::Atom) -> String {
        unsafe { CStr::from_ptr(xlib::XGetAtomName(self.display(), atom)).to_string_lossy().into_owned() }
    }

    fn get_property(&self, device: DeviceId, property: xlib::Atom) -> Option<Vec<u8>> {
        let mut value = Vec::new();

        unsafe {
            let mut actual_type = mem::uninitialized();
            let mut actual_format = mem::uninitialized();
            let mut raw_data = mem::uninitialized();
            let mut size = 128;
            let mut bytes_after = 1;
            while bytes_after != 0 {
                let result = xinput2::XIGetProperty(self.display(), device.0, property, value.len() as i64, 128, xlib::False, xlib::AnyPropertyType as u64,
                                                    &mut actual_type, &mut actual_format, &mut size, &mut bytes_after, &mut raw_data);
                if result != xlib::Success as i32 { xlib::XFree(raw_data as *mut _); return None; }
                if actual_format == 0 { return None; } // No such property
                assert!(actual_format == 8);
                value.extend_from_slice(slice::from_raw_parts(raw_data, size as usize));
                xlib::XFree(raw_data as *mut _);
            }
        }

        Some(value)
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

struct DeviceInfo {
    abs_axes: Vec<u32>,
}

pub struct Stream {
    io: PollEvented<StreamInner>,
    buffer: VecDeque<(DeviceId, Event)>,
    xkb: xkb::Context,
    xkb_base_event: u8,
    device_info: HashMap<DeviceId, DeviceInfo>,
}


impl AsRawFd for StreamInner {
    fn as_raw_fd(&self) -> RawFd {
        unsafe { xlib::XConnectionNumber(self.0.borrow().display()) }
    }
}

impl Context {
    pub fn new(handle: &Handle) -> io::Result<(Context, Stream)> {
        let udev = udev::Context::new()?;
        let shared = match Shared::new(udev) {
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

    pub fn device_hw_id(&self, device: DeviceId) -> Option<DeviceHwId> {
        let shared = self.0.borrow();

        let devnode = if let Some(x) = shared.get_property(device, shared.atoms.device_node) { x } else { return None };
        let statbuf = if let Ok(x) = nix::sys::stat::stat(&devnode[..]) { x } else { return None };
        let devty = if statbuf.st_mode & nix::sys::stat::S_IFCHR.bits() != 0 {
            udev::DevType::Character
        } else if statbuf.st_mode & nix::sys::stat::S_IFBLK.bits() != 0 {
            udev::DevType::Block
        } else {
            return None
        };
        dev_hwid(&shared.udev, devty, statbuf.st_rdev)
    }

    pub fn device_name(&self, device: DeviceId) -> String {
        let name = unsafe {
            let mut size = mem::uninitialized();
            let info = xinput2::XIQueryDevice(self.0.borrow().display(), device.0, &mut size);
            CStr::from_ptr((*info).name)
        };
        name.to_string_lossy().into_owned()
    }

    pub fn device_port(&self, device: DeviceId) -> Option<String> {
        let shared = self.0.borrow();

        let devnode = if let Some(x) = shared.get_property(device, shared.atoms.device_node) { x } else { return None };
        let statbuf = if let Ok(x) = nix::sys::stat::stat(&devnode[..]) { x } else { return None };
        let devty = if statbuf.st_mode & nix::sys::stat::S_IFCHR.bits() != 0 {
            udev::DevType::Character
        } else if statbuf.st_mode & nix::sys::stat::S_IFBLK.bits() != 0 {
            udev::DevType::Block
        } else {
            return None
        };
        let udevice = if let Ok(x) = udev::Device::new_from_devnum(&shared.udev, devty, statbuf.st_rdev) { x } else { return None };
        let input = unsafe { udevice.find_parent(Some(CStr::from_bytes_with_nul_unchecked(b"input\0")), None) };
        let port = unsafe { input.sysattr_value(CStr::from_bytes_with_nul_unchecked(b"phys\0")) };
        port.map(|x| x.to_string_lossy().into_owned())
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
            device_info: HashMap::new(),
        };

        let devices =
            if result.io.get_ref().0.borrow().xinput2.is_some() {
                // Register for device hotplug events
                let mask = xinput2::XI_HierarchyChangedMask;
                unsafe {
                    let mut event_mask = xinput2::XIEventMask{
                        deviceid: xinput2::XIAllDevices,
                        mask: &mask as *const i32 as *mut _,
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
                    mask: &mask as *const i32 as *mut _,
                    mask_len: mem::size_of_val(&mask) as c_int,
                };
                xinput2::XISelectEvents(self.display(), self.default_root(), &mut event_mask as *mut xinput2::XIEventMask, 1);
            };
        }

        let mut info_out = DeviceInfo {
            abs_axes: Vec::new(),
        };

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
                        match axis_info.mode {
                            xinput2::XIModeRelative => self.buffer.push_back((device, Event::RelMotion {
                                axis: RelAxisId(axis_info.number as u32),
                                value: axis_info.value
                            })),
                            xinput2::XIModeAbsolute => {
                                info_out.abs_axes.push(axis_info.number as u32);
                                self.buffer.push_back((device, Event::AbsMotion {
                                    axis: AbsAxisId(axis_info.number as u32),
                                    value: axis_info.value
                                }))
                            }
                            _ => unreachable!(),
                        }
                    }
                },
                ty => {
                    trace!("device {:?} has unrecognized class type {}", device, ty);
                },
            }
        }

        self.device_info.insert(device, info_out);
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
                                let evt = unsafe { &*(xcookie.data as *const XIRawEvent) };
                                let mask = unsafe { slice::from_raw_parts(evt.valuators.mask, evt.valuators.mask_len as usize) };
                                let mut raw_value = evt.raw_values;
                                let id = DeviceId(evt.deviceid);
                                let info = &self.device_info[&id];
                                for i in 0..evt.valuators.mask_len*8 {
                                    if XIMaskIsSet(mask, i) {
                                        self.buffer.push_back((id, if info.abs_axes.contains(&(i as u32)) {
                                            Event::AbsMotion {
                                                axis: AbsAxisId(i as u32),
                                                value: unsafe { *raw_value }
                                            }
                                        } else {
                                            Event::RelMotion {
                                                axis: RelAxisId(i as u32),
                                                value: unsafe { *raw_value }
                                            }
                                        }));
                                        raw_value = unsafe { raw_value.offset(1) };
                                    }
                                }
                            },
                            XI_RawButtonPress => {
                                let evt = unsafe { &*(xcookie.data as *const XIRawEvent) };
                                self.buffer.push_back((DeviceId(evt.deviceid), Event::ButtonPress {
                                    button: ButtonId(evt.detail as u32),
                                }));
                            },
                            XI_RawButtonRelease => {
                                let evt = unsafe { &*(xcookie.data as *const XIRawEvent) };
                                self.buffer.push_back((DeviceId(evt.deviceid), Event::ButtonRelease {
                                    button: ButtonId(evt.detail as u32),
                                }));
                            },
                            XI_RawKeyPress => {
                                let evt = unsafe { &*(xcookie.data as *const XIRawEvent) };
                                let kb = &self.io.get_ref().0.borrow().keyboards[&DeviceId(evt.deviceid)];
                                self.buffer.push_back((DeviceId(evt.deviceid), Event::KeyPress {
                                    scancode: Scancode(evt.detail as u32),
                                    text: kb.state.key_get_utf8(evt.detail as u32),
                                }));
                            },
                            XI_RawKeyRelease => {
                                let evt = unsafe { &*(xcookie.data as *const XIRawEvent) };
                                self.buffer.push_back((DeviceId(evt.deviceid), Event::KeyRelease {
                                    scancode: Scancode(evt.detail as u32),
                                }));
                            },
                            XI_HierarchyChanged => {
                                let evt = unsafe { &*(xcookie.data as *const XIHierarchyEvent) };
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
