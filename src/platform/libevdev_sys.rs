// bindgen libevdev-1.0/libevdev/libevdev.h --no-rust-enums --convert-macros --link=libevdev --match=libevdev.h --match=input.h --ctypes-prefix=libc --remove-prefix=libevdev_

#![allow(dead_code,
         non_camel_case_types,
         non_upper_case_globals,
         non_snake_case)]

pub type __u8 = u8;
pub type __s8 = i8;
pub type __u16 = u16;
pub type __s16 = i16;
pub type __u32 = u32;
pub type __s32 = i32;
pub type __u64 = u64;
pub type __s64 = i64;
pub type va_list = *mut ::std::os::raw::c_void;
use libc::timeval;
use libc::size_t;

pub const EV_VERSION: ::libc::c_uint = 65537;
pub const INPUT_KEYMAP_BY_INDEX: ::libc::c_uchar = 1;
pub const ID_BUS: ::libc::c_uchar = 0;
pub const ID_VENDOR: ::libc::c_uchar = 1;
pub const ID_PRODUCT: ::libc::c_uchar = 2;
pub const ID_VERSION: ::libc::c_uchar = 3;
pub const BUS_PCI: ::libc::c_uchar = 1;
pub const BUS_ISAPNP: ::libc::c_uchar = 2;
pub const BUS_USB: ::libc::c_uchar = 3;
pub const BUS_HIL: ::libc::c_uchar = 4;
pub const BUS_BLUETOOTH: ::libc::c_uchar = 5;
pub const BUS_VIRTUAL: ::libc::c_uchar = 6;
pub const BUS_ISA: ::libc::c_uchar = 16;
pub const BUS_I8042: ::libc::c_uchar = 17;
pub const BUS_XTKBD: ::libc::c_uchar = 18;
pub const BUS_RS232: ::libc::c_uchar = 19;
pub const BUS_GAMEPORT: ::libc::c_uchar = 20;
pub const BUS_PARPORT: ::libc::c_uchar = 21;
pub const BUS_AMIGA: ::libc::c_uchar = 22;
pub const BUS_ADB: ::libc::c_uchar = 23;
pub const BUS_I2C: ::libc::c_uchar = 24;
pub const BUS_HOST: ::libc::c_uchar = 25;
pub const BUS_GSC: ::libc::c_uchar = 26;
pub const BUS_ATARI: ::libc::c_uchar = 27;
pub const BUS_SPI: ::libc::c_uchar = 28;
pub const MT_TOOL_FINGER: ::libc::c_uchar = 0;
pub const MT_TOOL_PEN: ::libc::c_uchar = 1;
pub const MT_TOOL_PALM: ::libc::c_uchar = 2;
pub const MT_TOOL_MAX: ::libc::c_uchar = 2;
pub const FF_STATUS_STOPPED: ::libc::c_uchar = 0;
pub const FF_STATUS_PLAYING: ::libc::c_uchar = 1;
pub const FF_STATUS_MAX: ::libc::c_uchar = 1;
pub const FF_RUMBLE: ::libc::c_uchar = 80;
pub const FF_PERIODIC: ::libc::c_uchar = 81;
pub const FF_CONSTANT: ::libc::c_uchar = 82;
pub const FF_SPRING: ::libc::c_uchar = 83;
pub const FF_FRICTION: ::libc::c_uchar = 84;
pub const FF_DAMPER: ::libc::c_uchar = 85;
pub const FF_INERTIA: ::libc::c_uchar = 86;
pub const FF_RAMP: ::libc::c_uchar = 87;
pub const FF_EFFECT_MIN: ::libc::c_uchar = 80;
pub const FF_EFFECT_MAX: ::libc::c_uchar = 87;
pub const FF_SQUARE: ::libc::c_uchar = 88;
pub const FF_TRIANGLE: ::libc::c_uchar = 89;
pub const FF_SINE: ::libc::c_uchar = 90;
pub const FF_SAW_UP: ::libc::c_uchar = 91;
pub const FF_SAW_DOWN: ::libc::c_uchar = 92;
pub const FF_CUSTOM: ::libc::c_uchar = 93;
pub const FF_WAVEFORM_MIN: ::libc::c_uchar = 88;
pub const FF_WAVEFORM_MAX: ::libc::c_uchar = 93;
pub const FF_GAIN: ::libc::c_uchar = 96;
pub const FF_AUTOCENTER: ::libc::c_uchar = 97;
pub const FF_MAX_EFFECTS: ::libc::c_uchar = 96;
pub const FF_MAX: ::libc::c_uchar = 127;
pub const FF_CNT: ::libc::c_uchar = 128;
#[repr(C)]
#[derive(Copy, Clone)]
//#[derive(Debug)]
pub struct input_event {
    pub time: timeval,
    pub type_: __u16,
    pub code: __u16,
    pub value: __s32,
}
impl ::std::default::Default for input_event {
    fn default() -> Self { unsafe { ::std::mem::zeroed() } }
}
#[repr(C)]
#[derive(Copy, Clone)]
#[derive(Debug)]
pub struct input_id {
    pub bustype: __u16,
    pub vendor: __u16,
    pub product: __u16,
    pub version: __u16,
}
impl ::std::default::Default for input_id {
    fn default() -> Self { unsafe { ::std::mem::zeroed() } }
}
#[repr(C)]
#[derive(Copy, Clone)]
#[derive(Debug)]
pub struct input_absinfo {
    pub value: __s32,
    pub minimum: __s32,
    pub maximum: __s32,
    pub fuzz: __s32,
    pub flat: __s32,
    pub resolution: __s32,
}
impl ::std::default::Default for input_absinfo {
    fn default() -> Self { unsafe { ::std::mem::zeroed() } }
}
#[repr(C)]
#[derive(Copy, Clone)]
#[derive(Debug)]
pub struct input_keymap_entry {
    pub flags: __u8,
    pub len: __u8,
    pub index: __u16,
    pub keycode: __u32,
    pub scancode: [__u8; 32usize],
}
impl ::std::default::Default for input_keymap_entry {
    fn default() -> Self { unsafe { ::std::mem::zeroed() } }
}
#[repr(C)]
#[derive(Copy, Clone)]
#[derive(Debug)]
pub struct input_mask {
    pub type_: __u32,
    pub codes_size: __u32,
    pub codes_ptr: __u64,
}
impl ::std::default::Default for input_mask {
    fn default() -> Self { unsafe { ::std::mem::zeroed() } }
}
#[repr(C)]
#[derive(Copy, Clone)]
#[derive(Debug)]
pub struct ff_replay {
    pub length: __u16,
    pub delay: __u16,
}
impl ::std::default::Default for ff_replay {
    fn default() -> Self { unsafe { ::std::mem::zeroed() } }
}
#[repr(C)]
#[derive(Copy, Clone)]
#[derive(Debug)]
pub struct ff_trigger {
    pub button: __u16,
    pub interval: __u16,
}
impl ::std::default::Default for ff_trigger {
    fn default() -> Self { unsafe { ::std::mem::zeroed() } }
}
#[repr(C)]
#[derive(Copy, Clone)]
#[derive(Debug)]
pub struct ff_envelope {
    pub attack_length: __u16,
    pub attack_level: __u16,
    pub fade_length: __u16,
    pub fade_level: __u16,
}
impl ::std::default::Default for ff_envelope {
    fn default() -> Self { unsafe { ::std::mem::zeroed() } }
}
#[repr(C)]
#[derive(Copy, Clone)]
#[derive(Debug)]
pub struct ff_constant_effect {
    pub level: __s16,
    pub envelope: ff_envelope,
}
impl ::std::default::Default for ff_constant_effect {
    fn default() -> Self { unsafe { ::std::mem::zeroed() } }
}
#[repr(C)]
#[derive(Copy, Clone)]
#[derive(Debug)]
pub struct ff_ramp_effect {
    pub start_level: __s16,
    pub end_level: __s16,
    pub envelope: ff_envelope,
}
impl ::std::default::Default for ff_ramp_effect {
    fn default() -> Self { unsafe { ::std::mem::zeroed() } }
}
#[repr(C)]
#[derive(Copy, Clone)]
#[derive(Debug)]
pub struct ff_condition_effect {
    pub right_saturation: __u16,
    pub left_saturation: __u16,
    pub right_coeff: __s16,
    pub left_coeff: __s16,
    pub deadband: __u16,
    pub center: __s16,
}
impl ::std::default::Default for ff_condition_effect {
    fn default() -> Self { unsafe { ::std::mem::zeroed() } }
}
#[repr(C)]
#[derive(Copy, Clone)]
#[derive(Debug)]
pub struct ff_periodic_effect {
    pub waveform: __u16,
    pub period: __u16,
    pub magnitude: __s16,
    pub offset: __s16,
    pub phase: __u16,
    pub envelope: ff_envelope,
    pub custom_len: __u32,
    pub custom_data: *mut __s16,
}
impl ::std::default::Default for ff_periodic_effect {
    fn default() -> Self { unsafe { ::std::mem::zeroed() } }
}
#[repr(C)]
#[derive(Copy, Clone)]
#[derive(Debug)]
pub struct ff_rumble_effect {
    pub strong_magnitude: __u16,
    pub weak_magnitude: __u16,
}
impl ::std::default::Default for ff_rumble_effect {
    fn default() -> Self { unsafe { ::std::mem::zeroed() } }
}
#[repr(C)]
#[derive(Copy, Clone)]
#[derive(Debug)]
pub struct ff_effect {
    pub type_: __u16,
    pub id: __s16,
    pub direction: __u16,
    pub trigger: ff_trigger,
    pub replay: ff_replay,
    pub u: Union_Unnamed1,
}
impl ::std::default::Default for ff_effect {
    fn default() -> Self { unsafe { ::std::mem::zeroed() } }
}
#[repr(C)]
#[derive(Copy, Clone)]
#[derive(Debug)]
pub struct Union_Unnamed1 {
    pub _bindgen_data_: [u64; 4usize],
}
impl Union_Unnamed1 {
    pub unsafe fn constant(&mut self) -> *mut ff_constant_effect {
        let raw: *mut u8 = ::std::mem::transmute(&self._bindgen_data_);
        ::std::mem::transmute(raw.offset(0))
    }
    pub unsafe fn ramp(&mut self) -> *mut ff_ramp_effect {
        let raw: *mut u8 = ::std::mem::transmute(&self._bindgen_data_);
        ::std::mem::transmute(raw.offset(0))
    }
    pub unsafe fn periodic(&mut self) -> *mut ff_periodic_effect {
        let raw: *mut u8 = ::std::mem::transmute(&self._bindgen_data_);
        ::std::mem::transmute(raw.offset(0))
    }
    pub unsafe fn condition(&mut self) -> *mut [ff_condition_effect; 2usize] {
        let raw: *mut u8 = ::std::mem::transmute(&self._bindgen_data_);
        ::std::mem::transmute(raw.offset(0))
    }
    pub unsafe fn rumble(&mut self) -> *mut ff_rumble_effect {
        let raw: *mut u8 = ::std::mem::transmute(&self._bindgen_data_);
        ::std::mem::transmute(raw.offset(0))
    }
}
impl ::std::default::Default for Union_Unnamed1 {
    fn default() -> Self { unsafe { ::std::mem::zeroed() } }
}
pub enum libevdev { }
pub type read_flag = u32;
pub const READ_FLAG_SYNC: read_flag = 1;
pub const READ_FLAG_NORMAL: read_flag = 2;
pub const READ_FLAG_FORCE_SYNC: read_flag = 4;
pub const READ_FLAG_BLOCKING: read_flag = 8;
pub type log_priority = u32;
pub const LOG_ERROR: log_priority = 10;
pub const LOG_INFO: log_priority = 20;
pub const LOG_DEBUG: log_priority = 30;
pub type log_func_t =
    ::std::option::Option<unsafe extern "C" fn(priority: log_priority,
                                               data: *mut ::libc::c_void,
                                               file: *const ::libc::c_char,
                                               line: ::libc::c_int,
                                               func: *const ::libc::c_char,
                                               format: *const ::libc::c_char,
                                               args: va_list)>;
pub type device_log_func_t =
    ::std::option::Option<unsafe extern "C" fn(dev: *const libevdev,
                                               priority: log_priority,
                                               data: *mut ::libc::c_void,
                                               file: *const ::libc::c_char,
                                               line: ::libc::c_int,
                                               func: *const ::libc::c_char,
                                               format: *const ::libc::c_char,
                                               args: va_list)>;
pub type grab_mode = u32;
pub const GRAB: grab_mode = 3;
pub const UNGRAB: grab_mode = 4;
pub type read_status = u32;
pub const READ_STATUS_SUCCESS: read_status = 0;
pub const READ_STATUS_SYNC: read_status = 1;
pub type led_value = u32;
pub const LED_ON: led_value = 3;
pub const LED_OFF: led_value = 4;
#[link(name = "evdev", kind = "dylib")]
extern "C" {
    #[link_name = "libevdev_new"]
    pub fn new() -> *mut libevdev;
    #[link_name = "libevdev_new_from_fd"]
    pub fn new_from_fd(fd: ::libc::c_int, dev: *mut *mut libevdev)
     -> ::libc::c_int;
    #[link_name = "libevdev_free"]
    pub fn free(dev: *mut libevdev);
    #[link_name = "libevdev_set_log_function"]
    pub fn set_log_function(logfunc: log_func_t, data: *mut ::libc::c_void);
    #[link_name = "libevdev_set_log_priority"]
    pub fn set_log_priority(priority: log_priority);
    #[link_name = "libevdev_get_log_priority"]
    pub fn get_log_priority() -> log_priority;
    #[link_name = "libevdev_set_device_log_function"]
    pub fn set_device_log_function(dev: *mut libevdev,
                                   logfunc: device_log_func_t,
                                   priority: log_priority,
                                   data: *mut ::libc::c_void);
    #[link_name = "libevdev_grab"]
    pub fn grab(dev: *mut libevdev, grab: grab_mode) -> ::libc::c_int;
    #[link_name = "libevdev_set_fd"]
    pub fn set_fd(dev: *mut libevdev, fd: ::libc::c_int) -> ::libc::c_int;
    #[link_name = "libevdev_change_fd"]
    pub fn change_fd(dev: *mut libevdev, fd: ::libc::c_int) -> ::libc::c_int;
    #[link_name = "libevdev_get_fd"]
    pub fn get_fd(dev: *const libevdev) -> ::libc::c_int;
    #[link_name = "libevdev_next_event"]
    pub fn next_event(dev: *mut libevdev, flags: ::libc::c_uint,
                      ev: *mut input_event) -> ::libc::c_int;
    #[link_name = "libevdev_has_event_pending"]
    pub fn has_event_pending(dev: *mut libevdev) -> ::libc::c_int;
    #[link_name = "libevdev_get_name"]
    pub fn get_name(dev: *const libevdev) -> *const ::libc::c_char;
    #[link_name = "libevdev_set_name"]
    pub fn set_name(dev: *mut libevdev, name: *const ::libc::c_char);
    #[link_name = "libevdev_get_phys"]
    pub fn get_phys(dev: *const libevdev) -> *const ::libc::c_char;
    #[link_name = "libevdev_set_phys"]
    pub fn set_phys(dev: *mut libevdev, phys: *const ::libc::c_char);
    #[link_name = "libevdev_get_uniq"]
    pub fn get_uniq(dev: *const libevdev) -> *const ::libc::c_char;
    #[link_name = "libevdev_set_uniq"]
    pub fn set_uniq(dev: *mut libevdev, uniq: *const ::libc::c_char);
    #[link_name = "libevdev_get_id_product"]
    pub fn get_id_product(dev: *const libevdev) -> ::libc::c_int;
    #[link_name = "libevdev_set_id_product"]
    pub fn set_id_product(dev: *mut libevdev, product_id: ::libc::c_int);
    #[link_name = "libevdev_get_id_vendor"]
    pub fn get_id_vendor(dev: *const libevdev) -> ::libc::c_int;
    #[link_name = "libevdev_set_id_vendor"]
    pub fn set_id_vendor(dev: *mut libevdev, vendor_id: ::libc::c_int);
    #[link_name = "libevdev_get_id_bustype"]
    pub fn get_id_bustype(dev: *const libevdev) -> ::libc::c_int;
    #[link_name = "libevdev_set_id_bustype"]
    pub fn set_id_bustype(dev: *mut libevdev, bustype: ::libc::c_int);
    #[link_name = "libevdev_get_id_version"]
    pub fn get_id_version(dev: *const libevdev) -> ::libc::c_int;
    #[link_name = "libevdev_set_id_version"]
    pub fn set_id_version(dev: *mut libevdev, version: ::libc::c_int);
    #[link_name = "libevdev_get_driver_version"]
    pub fn get_driver_version(dev: *const libevdev) -> ::libc::c_int;
    #[link_name = "libevdev_has_property"]
    pub fn has_property(dev: *const libevdev, prop: ::libc::c_uint)
     -> ::libc::c_int;
    #[link_name = "libevdev_enable_property"]
    pub fn enable_property(dev: *mut libevdev, prop: ::libc::c_uint)
     -> ::libc::c_int;
    #[link_name = "libevdev_has_event_type"]
    pub fn has_event_type(dev: *const libevdev, type_: ::libc::c_uint)
     -> ::libc::c_int;
    #[link_name = "libevdev_has_event_code"]
    pub fn has_event_code(dev: *const libevdev, type_: ::libc::c_uint,
                          code: ::libc::c_uint) -> ::libc::c_int;
    #[link_name = "libevdev_get_abs_minimum"]
    pub fn get_abs_minimum(dev: *const libevdev, code: ::libc::c_uint)
     -> ::libc::c_int;
    #[link_name = "libevdev_get_abs_maximum"]
    pub fn get_abs_maximum(dev: *const libevdev, code: ::libc::c_uint)
     -> ::libc::c_int;
    #[link_name = "libevdev_get_abs_fuzz"]
    pub fn get_abs_fuzz(dev: *const libevdev, code: ::libc::c_uint)
     -> ::libc::c_int;
    #[link_name = "libevdev_get_abs_flat"]
    pub fn get_abs_flat(dev: *const libevdev, code: ::libc::c_uint)
     -> ::libc::c_int;
    #[link_name = "libevdev_get_abs_resolution"]
    pub fn get_abs_resolution(dev: *const libevdev, code: ::libc::c_uint)
     -> ::libc::c_int;
    #[link_name = "libevdev_get_abs_info"]
    pub fn get_abs_info(dev: *const libevdev, code: ::libc::c_uint)
     -> *const input_absinfo;
    #[link_name = "libevdev_get_event_value"]
    pub fn get_event_value(dev: *const libevdev, type_: ::libc::c_uint,
                           code: ::libc::c_uint) -> ::libc::c_int;
    #[link_name = "libevdev_set_event_value"]
    pub fn set_event_value(dev: *mut libevdev, type_: ::libc::c_uint,
                           code: ::libc::c_uint, value: ::libc::c_int)
     -> ::libc::c_int;
    #[link_name = "libevdev_fetch_event_value"]
    pub fn fetch_event_value(dev: *const libevdev, type_: ::libc::c_uint,
                             code: ::libc::c_uint, value: *mut ::libc::c_int)
     -> ::libc::c_int;
    #[link_name = "libevdev_get_slot_value"]
    pub fn get_slot_value(dev: *const libevdev, slot: ::libc::c_uint,
                          code: ::libc::c_uint) -> ::libc::c_int;
    #[link_name = "libevdev_set_slot_value"]
    pub fn set_slot_value(dev: *mut libevdev, slot: ::libc::c_uint,
                          code: ::libc::c_uint, value: ::libc::c_int)
     -> ::libc::c_int;
    #[link_name = "libevdev_fetch_slot_value"]
    pub fn fetch_slot_value(dev: *const libevdev, slot: ::libc::c_uint,
                            code: ::libc::c_uint, value: *mut ::libc::c_int)
     -> ::libc::c_int;
    #[link_name = "libevdev_get_num_slots"]
    pub fn get_num_slots(dev: *const libevdev) -> ::libc::c_int;
    #[link_name = "libevdev_get_current_slot"]
    pub fn get_current_slot(dev: *const libevdev) -> ::libc::c_int;
    #[link_name = "libevdev_set_abs_minimum"]
    pub fn set_abs_minimum(dev: *mut libevdev, code: ::libc::c_uint,
                           min: ::libc::c_int);
    #[link_name = "libevdev_set_abs_maximum"]
    pub fn set_abs_maximum(dev: *mut libevdev, code: ::libc::c_uint,
                           max: ::libc::c_int);
    #[link_name = "libevdev_set_abs_fuzz"]
    pub fn set_abs_fuzz(dev: *mut libevdev, code: ::libc::c_uint,
                        fuzz: ::libc::c_int);
    #[link_name = "libevdev_set_abs_flat"]
    pub fn set_abs_flat(dev: *mut libevdev, code: ::libc::c_uint,
                        flat: ::libc::c_int);
    #[link_name = "libevdev_set_abs_resolution"]
    pub fn set_abs_resolution(dev: *mut libevdev, code: ::libc::c_uint,
                              resolution: ::libc::c_int);
    #[link_name = "libevdev_set_abs_info"]
    pub fn set_abs_info(dev: *mut libevdev, code: ::libc::c_uint,
                        abs: *const input_absinfo);
    #[link_name = "libevdev_enable_event_type"]
    pub fn enable_event_type(dev: *mut libevdev, type_: ::libc::c_uint)
     -> ::libc::c_int;
    #[link_name = "libevdev_disable_event_type"]
    pub fn disable_event_type(dev: *mut libevdev, type_: ::libc::c_uint)
     -> ::libc::c_int;
    #[link_name = "libevdev_enable_event_code"]
    pub fn enable_event_code(dev: *mut libevdev, type_: ::libc::c_uint,
                             code: ::libc::c_uint,
                             data: *const ::libc::c_void) -> ::libc::c_int;
    #[link_name = "libevdev_disable_event_code"]
    pub fn disable_event_code(dev: *mut libevdev, type_: ::libc::c_uint,
                              code: ::libc::c_uint) -> ::libc::c_int;
    #[link_name = "libevdev_kernel_set_abs_info"]
    pub fn kernel_set_abs_info(dev: *mut libevdev, code: ::libc::c_uint,
                               abs: *const input_absinfo) -> ::libc::c_int;
    #[link_name = "libevdev_kernel_set_led_value"]
    pub fn kernel_set_led_value(dev: *mut libevdev, code: ::libc::c_uint,
                                value: led_value) -> ::libc::c_int;
    #[link_name = "libevdev_kernel_set_led_values"]
    pub fn kernel_set_led_values(dev: *mut libevdev, ...) -> ::libc::c_int;
    #[link_name = "libevdev_set_clock_id"]
    pub fn set_clock_id(dev: *mut libevdev, clockid: ::libc::c_int)
     -> ::libc::c_int;
    #[link_name = "libevdev_event_is_type"]
    pub fn event_is_type(ev: *const input_event, type_: ::libc::c_uint)
     -> ::libc::c_int;
    #[link_name = "libevdev_event_is_code"]
    pub fn event_is_code(ev: *const input_event, type_: ::libc::c_uint,
                         code: ::libc::c_uint) -> ::libc::c_int;
    #[link_name = "libevdev_event_type_get_name"]
    pub fn event_type_get_name(type_: ::libc::c_uint)
     -> *const ::libc::c_char;
    #[link_name = "libevdev_event_code_get_name"]
    pub fn event_code_get_name(type_: ::libc::c_uint, code: ::libc::c_uint)
     -> *const ::libc::c_char;
    #[link_name = "libevdev_property_get_name"]
    pub fn property_get_name(prop: ::libc::c_uint) -> *const ::libc::c_char;
    #[link_name = "libevdev_event_type_get_max"]
    pub fn event_type_get_max(type_: ::libc::c_uint) -> ::libc::c_int;
    #[link_name = "libevdev_event_type_from_name"]
    pub fn event_type_from_name(name: *const ::libc::c_char) -> ::libc::c_int;
    #[link_name = "libevdev_event_type_from_name_n"]
    pub fn event_type_from_name_n(name: *const ::libc::c_char, len: size_t)
     -> ::libc::c_int;
    #[link_name = "libevdev_event_code_from_name"]
    pub fn event_code_from_name(type_: ::libc::c_uint,
                                name: *const ::libc::c_char) -> ::libc::c_int;
    #[link_name = "libevdev_event_code_from_name_n"]
    pub fn event_code_from_name_n(type_: ::libc::c_uint,
                                  name: *const ::libc::c_char, len: size_t)
     -> ::libc::c_int;
    #[link_name = "libevdev_property_from_name"]
    pub fn property_from_name(name: *const ::libc::c_char) -> ::libc::c_int;
    #[link_name = "libevdev_property_from_name_n"]
    pub fn property_from_name_n(name: *const ::libc::c_char, len: size_t)
     -> ::libc::c_int;
    #[link_name = "libevdev_get_repeat"]
    pub fn get_repeat(dev: *const libevdev, delay: *mut ::libc::c_int,
                      period: *mut ::libc::c_int) -> ::libc::c_int;
}
