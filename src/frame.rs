use crate::code::Instructions;
use crate::object::CompiledFunctionObject;

#[derive(Clone)]
pub struct Frame {
    function: CompiledFunctionObject,
    ip: usize,
    base_pointer: usize,
}

impl Frame {
    pub fn new(function: CompiledFunctionObject, base_pointer: usize) -> Self {
        Frame {
            function,
            ip: 0,
            base_pointer,
        }
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
    pub fn base_pointer(&self) -> usize {
        self.base_pointer
    }
}
