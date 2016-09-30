use std::fmt;
use std::hash::Hash;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct AxisID(pub i32);

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct ButtonID(pub i32);

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct ScanCode(pub u32);

impl fmt::Debug for ScanCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ScanCode({:X})", self.0)
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct KeySym(pub u32);

impl fmt::Debug for KeySym {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "KeySym({:X})", self.0)
    }
}

#[derive(Debug, Clone)]
pub enum Event<W : WindowID, D : DeviceID> {
    Map(W),
    Unmap(W),
    Quit(W),
    RawMotion { device: D, axis: AxisID, value: f64 },
    Motion { window: W, device: D, axis: AxisID, value: f64 },
    PointerMotion { window: W, device: D, pos: (f64, f64) },
    RawButtonPress { device: D, button: ButtonID },
    RawButtonRelease { device: D, button: ButtonID },
    ButtonPress { window: W, device: D, button: ButtonID },
    ButtonRelease { window: W, device: D, button: ButtonID },
    RawKeyPress { device: D, scan_code: ScanCode, key_sym: KeySym, text: String },
    RawKeyRelease { device: D, scan_code: ScanCode, key_sym: KeySym },
    KeyPress { window: W, device: D, scan_code: ScanCode, key_sym: KeySym, text: String },
    KeyRelease { window: W, device: D, scan_code: ScanCode, key_sym: KeySym },
    DeviceAdded { device: D },
    DeviceRemoved { device: D },
}

impl<W: WindowID, D: DeviceID> Event<W, D> {
    pub fn map<T: WindowID, U: DeviceID, F: Fn(W) -> T, G: Fn(D) -> U>(self, f: F, g: G) -> Event<T, U> {
        use Event::*;
        match self {
            Map(x) => Map(f(x)),
            Unmap(x) => Unmap(f(x)),
            Quit(x) => Quit(f(x)),
            RawMotion { device: d, axis: a, value: v } => RawMotion { device: g(d), axis: a, value: v },
            Motion { window: w, device: d, axis: a, value: v } => Motion { window: f(w), device: g(d), axis: a, value: v},
            PointerMotion { window: w, device: d, pos: p } => PointerMotion { window: f(w), device: g(d), pos: p},
            RawButtonPress { device: d, button: b } => RawButtonPress { device: g(d), button: b },
            RawButtonRelease { device: d, button: b } => RawButtonRelease { device: g(d), button: b },
            ButtonPress { window: w, device: d, button: b } => ButtonPress { window: f(w), device: g(d), button: b },
            ButtonRelease { window: w, device: d, button: b } => ButtonRelease { window: f(w), device: g(d), button: b },
            RawKeyPress { device: d, key_sym: b, scan_code: c, text: s } => RawKeyPress { device: g(d), key_sym: b, scan_code: c, text: s },
            RawKeyRelease { device: d, key_sym: b, scan_code: c } => RawKeyRelease { device: g(d), key_sym: b, scan_code: c },
            KeyPress { window: w, device: d, key_sym: b, scan_code: c, text: s } => KeyPress { window: f(w), device: g(d), key_sym: b, scan_code: c, text: s },
            KeyRelease { window: w, device: d, key_sym: b, scan_code: c } => KeyRelease { window: f(w), device: g(d), key_sym: b, scan_code: c },
            DeviceAdded { device: d } => DeviceAdded { device: g(d) },
            DeviceRemoved { device: d } => DeviceRemoved { device: g(d) },
        }
    }
}

pub trait WindowID: fmt::Debug + Clone + Hash + Eq {}
pub trait DeviceID: fmt::Debug + Clone + Hash + Eq {}

pub struct WindowBuilder<'a> {
    pub name: &'a str,
    pub position: (i32, i32),
    pub size: (u32, u32),
}

impl<'a> WindowBuilder<'a> {
    pub fn new() -> WindowBuilder<'a> {
        WindowBuilder { name: "wair window", position: (0, 0), size: (256, 256) }
    }

    pub fn name(mut self, name: &'a str) -> WindowBuilder<'a> {
        self.name = name;
        self
    }

    pub fn position(mut self, position: (i32, i32)) -> WindowBuilder<'a> {
        self.position = position;
        self
    }

    pub fn size(mut self, size: (u32, u32)) -> WindowBuilder<'a> {
        self.size = size;
        self
    }
}
