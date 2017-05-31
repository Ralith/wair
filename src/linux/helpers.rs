use std::ffi::CStr;

use super::nix;
use super::libudev as udev;

use DeviceHwId;

pub fn dev_hwid(udev: &udev::Context, ty: udev::DevType, device: nix::sys::stat::dev_t) -> Option<DeviceHwId> {
    let udevice = if let Ok(x) = udev::Device::new_from_devnum(&udev, ty, device) { x } else { return None };
    let usb = unsafe { udevice.find_parent(Some(CStr::from_bytes_with_nul_unchecked(b"usb\0")), Some(CStr::from_bytes_with_nul_unchecked(b"usb_device\0"))) };
    let vendor_id = unsafe { usb.sysattr_value(CStr::from_bytes_with_nul_unchecked(b"idVendor\0")) };
    let vendor_id = if let Some(x) = vendor_id.and_then(|x| x.to_str().ok()).and_then(|x| u16::from_str_radix(x, 16).ok()) { x } else { return None };
    let product_id = unsafe { usb.sysattr_value(CStr::from_bytes_with_nul_unchecked(b"idProduct\0")) };
    let product_id = if let Some(x) = product_id.and_then(|x| x.to_str().ok()).and_then(|x| u16::from_str_radix(x, 16).ok()) { x } else { return None };
    //let serial = unsafe { usb.sysattr_value(CStr::from_bytes_with_nul_unchecked(b"serial\0")) };

    Some(DeviceHwId {
        vendor: vendor_id,
        product: product_id,
    })
}
