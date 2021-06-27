use crate::code::Instructions;
use crate::object::Closure;
use std::rc::Rc;

#[derive(Clone)]
pub struct Frame {
    closure: Rc<Closure>,
    ip: usize,
    base_pointer: usize,
}

impl Frame {
    pub fn new(closure: Rc<Closure>, base_pointer: usize) -> Self {
        Frame {
            closure,
            ip: 0,
            base_pointer,
        }
    }
    pub fn instructions(&self) -> Rc<Instructions> {
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
    pub fn closure(&self) -> Rc<Closure> {
        Rc::clone(&self.closure)
    }
}
