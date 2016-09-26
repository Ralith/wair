use std::fmt;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct AxisID(pub i32);

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct ButtonID(pub i32);

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct ScanCode(pub i32);

impl fmt::Debug for ScanCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ScanCode({:X})", self.0)
    }
}

#[derive(Debug)]
pub enum Event<W : WindowID, D : DeviceID> {
    Map(W),
    Unmap(W),
    Quit(W),
    RawMotion { device: D, axis: AxisID, value: f64 },
    RawButton { device: D, button: ButtonID, pressed: bool },
    RawKey { device: D, scancode: ScanCode, pressed: bool },
}

pub trait WindowID: fmt::Debug + Copy {}
pub trait DeviceID: fmt::Debug + Copy {}

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
