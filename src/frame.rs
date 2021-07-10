use crate::code::Instructions;
use crate::object::Closure;

#[derive(Clone)]
pub struct Frame {
    closure: Closure,
    ip: usize,
    base_pointer: usize,
}

impl Frame {
    pub fn new(closure: Closure, base_pointer: usize) -> Self {
        Frame {
            closure,
            ip: 0,
            base_pointer,
        }
    }
    pub fn instructions(&self) -> &Instructions {
        self.closure.function().instructions()
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
    pub fn closure(&self) -> &Closure {
        &self.closure
    }
}
