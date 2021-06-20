use crate::code::Instructions;
use crate::object::CompiledFunctionObject;

#[derive(Clone)]
pub struct Frame {
    function: CompiledFunctionObject,
    ip: usize,
}

impl Frame {
    pub fn new(function: CompiledFunctionObject) -> Self {
        Frame { function, ip: 0 }
    }
    pub fn instructions(&self) -> &Instructions {
        self.function.instructions()
    }
    pub fn instructions_mut(&mut self) -> &mut Instructions {
        self.function.instructions_mut()
    }
    pub fn ip(&self) -> usize {
        self.ip
    }
    pub fn update_ip(&mut self, ip: usize) {
        self.ip = ip;
    }
}
