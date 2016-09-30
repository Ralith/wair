use super::libevdev_sys as libevdev;

use std::{io, mem, slice, ptr};
use std::ffi::OsStr;
use std::os::unix::ffi::OsStrExt;
use std::os::unix::io::{AsRawFd, RawFd};

use libc::{c_char, c_uint, c_int};

pub type AbsInfo = libevdev::input_absinfo;
pub type InputEvent = libevdev::input_event;
pub enum LEDValue {
    On = libevdev::LED_ON as isize,
    Off = libevdev::LED_OFF as isize,
}

pub enum ReadStatus {
    Success = libevdev::READ_STATUS_SUCCESS as isize,
    Sync = libevdev::READ_STATUS_SYNC as isize,
}

pub type ReadFlag = libevdev::read_flag;
pub use super::libevdev_sys::READ_FLAG_SYNC;
pub use super::libevdev_sys::READ_FLAG_NORMAL;
pub use super::libevdev_sys::READ_FLAG_FORCE_SYNC;
pub use super::libevdev_sys::READ_FLAG_BLOCKING;

pub struct Device(*mut libevdev::libevdev);

impl Device {
    pub fn new_from_fd(fd: RawFd) -> io::Result<Self> {
        unsafe {
            let mut handle = mem::uninitialized();
            let rc = libevdev::new_from_fd(fd, &mut handle as *mut *mut libevdev::libevdev);
            if rc == 0 {
                Ok(Device(handle))
            } else {
                Err(io::Error::from_raw_os_error(-rc))
            }
        }
    }

    pub fn get_name(&self) -> &OsStr { unsafe { to_os_str(libevdev::get_name(self.0)) } }
    pub fn get_phys(&self) -> Option<&OsStr> { unsafe { nullable_to_os_str(libevdev::get_phys(self.0)) } }
    pub fn get_uniq(&self) -> Option<&OsStr> { unsafe { nullable_to_os_str(libevdev::get_uniq(self.0)) } }

    pub fn has_event_code(&self, type_: c_uint, code: c_uint) -> bool { unsafe { libevdev::has_event_code(self.0, type_, code) != 0 } }
    pub fn get_event_value(&self, type_: c_uint, code: c_uint) -> c_int { unsafe { libevdev::get_event_value(self.0, type_, code) } }

    pub fn get_id_vendor(&self) -> u16 { (unsafe { libevdev::get_id_vendor(self.0) } as u16) }
    pub fn get_id_product(&self) -> u16 { (unsafe { libevdev::get_id_product(self.0) } as u16) }

    pub fn get_abs_minimum(&self, code: c_uint) -> c_int { unsafe { libevdev::get_abs_minimum(self.0, code) } }
    pub fn get_abs_maximum(&self, code: c_uint) -> c_int { unsafe { libevdev::get_abs_maximum(self.0, code) } }
    pub fn get_abs_resolution(&self, code: c_uint) -> c_int { unsafe { libevdev::get_abs_resolution(self.0, code) } }

    pub fn get_absinfo(&self, code: c_uint) -> Option<&AbsInfo> {
        let x = unsafe { libevdev::get_abs_info(self.0, code) };
        if x == ptr::null() {
            None
        } else {
            Some(unsafe { &*x } )
        }
    }

    pub fn set_led_value(&self, code: c_uint, value: LEDValue) -> io::Result<()> {
        let rc = unsafe { libevdev::kernel_set_led_value(self.0, code, value as libevdev::led_value) };
        if rc == 0 {
            Ok(())
        } else {
            Err(io::Error::from_raw_os_error(-rc))
        }
    }

    pub fn next_event(&self, flags: ReadFlag) -> io::Result<(ReadStatus, InputEvent)> {
        unsafe {
            let mut result = mem::uninitialized();
            let rc = libevdev::next_event(self.0, flags, &mut result as *mut libevdev::input_event);
            match rc as u32 {
                libevdev::READ_STATUS_SUCCESS => Ok((ReadStatus::Success, result)),
                libevdev::READ_STATUS_SYNC => Ok((ReadStatus::Sync, result)),
                _ => Err(io::Error::from_raw_os_error(-rc)),
            }
        }
    }
}

impl Drop for Device {
    fn drop(&mut self) {
        unsafe { libevdev::free(self.0); };
    }
}

impl AsRawFd for Device {
    fn as_raw_fd(&self) -> RawFd {
        unsafe { libevdev::get_fd(self.0) }
    }
}

unsafe fn nullable_to_os_str<'a>(x: *const c_char) -> Option<&'a OsStr> {
    if x == ptr::null() {
        None
    } else {
        Some(to_os_str(x))
    }
}

unsafe fn to_os_str<'a>(x: *const c_char) -> &'a OsStr {
    OsStr::from_bytes(slice::from_raw_parts(x as *const u8, ::libc::strlen(x) as usize))
}
