use std::fmt::Debug;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct AxisID(pub i32);

#[derive(Debug)]
pub enum Event<W : WindowID, D : DeviceID> {
    Map(W),
    Unmap(W),
    Quit(W),
    RawMotion{ device: D, axis: AxisID, value: f64 },
}

pub trait WindowID: Debug + Copy {}
pub trait DeviceID: Debug + Copy {}

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
