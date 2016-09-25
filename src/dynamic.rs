use common::*;

pub trait Context {
    fn new_window<'a>(&'a self, WindowBuilder) -> Result<Box<Window + 'a>, String>;
}

pub trait Window {
    fn set_name(&self, name: &str);
    fn map(&self);    
}

#[cfg(feature = "x11")]
pub fn get_context() -> Result<Box<Context>, String> {
    ::x11::Context::new().map(|x| -> Box<Context> { Box::new(x) })
}
