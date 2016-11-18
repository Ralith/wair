#![allow(dead_code)]

extern crate libudev_sys;

use std::{io, ptr};
use std::ffi::CStr;
use std::ops::Deref;
use std::os::unix::io::{AsRawFd, RawFd};
use std::marker::PhantomData;

use libc::c_char;

use self::libudev_sys::*;

// from libudev-rs
macro_rules! try_alloc {
    ($exp:expr) => {{
        let ptr = $exp;

        if ptr.is_null() {
            return Err(io::Error::from_raw_os_error(::libc::ENOMEM));
        }

        ptr
    }}
}

fn errno_to_result(x: i32) -> io::Result<()> {
    if x == 0 {
        Ok(())
    } else {
        Err(io::Error::from_raw_os_error(x))
    }
}

pub struct Context(*mut udev);      // nonzero

impl Context {
    pub fn new() -> io::Result<Context> {
        Ok(Context(try_alloc!(unsafe { udev_new() })))
    }
}

impl Drop for Context {
    fn drop(&mut self) {
        unsafe { udev_unref(self.0); }
    }
}

pub struct Device<'a>(*mut udev_device, PhantomData<&'a Context>); // nonzero

unsafe fn option_cstr<'a>(x: *const c_char) -> Option<&'a CStr> {
    if x.is_null() {
        None
    } else {
        Some(CStr::from_ptr(x))
    }
}

impl<'a> Device<'a> {
    pub fn new_from_syspath(context: &'a Context, path: &CStr) -> io::Result<Device<'a>> {
        Ok(Device(try_alloc!(unsafe { udev_device_new_from_syspath(context.0, path.as_ptr()) }), PhantomData))
    }

    pub fn action(&self) -> &CStr {
        unsafe { CStr::from_ptr(udev_device_get_action(self.0)) }
    }

    pub fn subsystem(&self) -> Option<&CStr> {
        unsafe { option_cstr(udev_device_get_subsystem(self.0)) }
    }

    pub fn sysname(&self) -> &CStr {
        unsafe { CStr::from_ptr(udev_device_get_sysname(self.0)) }
    }

    pub fn sysnum(&self) -> Option<&CStr> {
        unsafe { option_cstr(udev_device_get_sysnum(self.0)) }
    }

    pub fn devnode(&self) -> Option<&CStr> {
        unsafe { option_cstr(udev_device_get_devnode(self.0)) }
    }

    pub fn devtype(&self) -> Option<&CStr> {
        unsafe { option_cstr(udev_device_get_devtype(self.0)) }
    }

    pub fn driver(&self) -> Option<&CStr> {
        unsafe { option_cstr(udev_device_get_driver(self.0)) }
    }
}

impl<'a> Drop for Device<'a> {
    fn drop(&mut self) {
        unsafe { udev_device_unref(self.0) };
    }
}

pub struct Monitor(*mut udev_monitor, Context); // nonzero

impl Monitor {
    pub fn new(context: Context) -> io::Result<Monitor> {
        Ok(Monitor(try_alloc!(unsafe { udev_monitor_new_from_netlink(context.0, b"udev\0".as_ptr() as *const c_char) }), context))
    }

    pub fn filter_add_match_subsystem(&mut self, subsystem: &CStr) -> io::Result<()> {
        errno_to_result(unsafe {
            udev_monitor_filter_add_match_subsystem_devtype(self.0, subsystem.as_ptr(), ptr::null())
        })
    }

    pub fn enable_receiving(&mut self) {
        unsafe { udev_monitor_enable_receiving(self.0); }
    }

    pub fn receive_device(&self) -> Option<Device> {
        let d = unsafe { udev_monitor_receive_device(self.0) };
        if d == ptr::null_mut() {
            None
        } else {
            Some(Device(d, PhantomData))
        }
    }
}

impl Drop for Monitor {
    fn drop(&mut self) {
        unsafe { udev_monitor_unref(self.0); }
    }
}

impl AsRawFd for Monitor {
    fn as_raw_fd(&self) -> RawFd {
        unsafe { udev_monitor_get_fd(self.0) }
    }
}

impl Deref for Monitor {
    type Target = Context;
    fn deref(&self) -> &Context { &self.1 }
}

pub struct Enumerate<'a>(*mut udev_enumerate, PhantomData<&'a Context>);

impl<'a> Drop for Enumerate<'a> {
    fn drop(&mut self) {
        unsafe { udev_enumerate_unref(self.0) };
    }
}

impl<'a> Enumerate<'a> {
    pub fn new(context: &'a Context) -> io::Result<Enumerate> {
        Ok(Enumerate(try_alloc!(unsafe { udev_enumerate_new(context.0) }), PhantomData))
    }

    pub fn add_match_subsystem(&mut self, subsystem: &CStr) -> io::Result<()> {
        errno_to_result(unsafe { udev_enumerate_add_match_subsystem(self.0, subsystem.as_ptr()) })
    }
}

pub struct EnumerateIter<'a>(Enumerate<'a>, *mut udev_list_entry);

impl<'a> IntoIterator for Enumerate<'a> {
    type Item = Device<'a>;
    type IntoIter = EnumerateIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        unsafe {
            udev_enumerate_scan_devices(self.0);
            let entry = udev_enumerate_get_list_entry(self.0);
            EnumerateIter(self, entry)
        }
    }
}

impl<'a> Iterator for EnumerateIter<'a> {
    type Item = Device<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.1 == ptr::null_mut() {
                return None;
            } else {
                let path = unsafe { udev_list_entry_get_name(self.1) };
                let dev = unsafe { udev_device_new_from_syspath(udev_enumerate_get_udev((self.0).0), path) };
                self.1 = unsafe { udev_list_entry_get_next(self.1) };
                if dev != ptr::null_mut() {
                    return Some(Device(dev, PhantomData));
                }
            }
        }
    }
}
