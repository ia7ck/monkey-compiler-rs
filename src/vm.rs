use crate::code::{read_uint16, Instructions, Opcode};
use crate::compiler::Bytecode;
use crate::object::Object;
use anyhow::{bail, Result};
use std::convert::TryFrom;

const STACK_SIZE: usize = 2048;

pub struct VM<'a> {
    instructions: &'a Instructions,
    constants: &'a [Object],
    stack: Vec<Object>,
    last_popped: Option<Object>,
}

impl<'a> VM<'a> {
    pub fn new(bytecode: &'a Bytecode) -> Self {
        Self {
            instructions: bytecode.instructions,
            constants: bytecode.constants,
            stack: Vec::with_capacity(STACK_SIZE),
            last_popped: None,
        }
    }
    pub fn last_popped_stack_elem(&self) -> Result<Object> {
        match self.last_popped.as_ref() {
            Some(obj) => Ok(obj.clone()),
            None => {
                bail!("there is no last popped stack element");
            }
        }
    }
    fn push(&mut self, obj: Object) -> Result<()> {
        if self.stack.len() >= STACK_SIZE {
            bail!("stack overflow");
        }
        self.stack.push(obj);
        Ok(())
    }
    fn pop(&mut self) -> Result<Object> {
        match self.stack.pop() {
            Some(obj) => {
                self.last_popped = Some(obj.clone());
                Ok(obj)
            }
            None => {
                bail!("stack is empty");
            }
        }
    }
    pub fn run(&mut self) -> Result<()> {
        use Object::*;
        use Opcode::*;
        let mut ip = 0;
        while ip < self.instructions.len() {
            let op = Opcode::try_from(self.instructions[ip]).unwrap();
            match op {
                OpConstant => {
                    let const_index = read_uint16(self.instructions.rest(ip + 1)) as usize;
                    // opcode (1 byte) + operand (2 byte)
                    ip += 3;
                    let obj = self.constants[const_index].clone();
                    self.push(obj)?;
                }
                OpAdd => {
                    ip += 1;
                    let right = self.pop()?;
                    let left = self.pop()?;
                    let result = match (left, right) {
                        (Integer { value: left }, Integer { value: right }) => left + right,
                    };
                    self.push(Integer { value: result })?;
                }
                OpPop => {
                    ip += 1;
                    self.pop()?;
                }
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::Program;
    use crate::compiler::Compiler;
    use crate::lexer::Lexer;
    use crate::object::Object;
    use crate::parser::Parser;
    use crate::vm::VM;
    use anyhow::{bail, Result};

    #[test]
    fn test_integer_arithmetic() {
        use Object::*;
        let tests = vec![
            ("1", Integer { value: 1 }),
            ("2", Integer { value: 2 }),
            ("1 + 2", Integer { value: 3 }),
        ];
        run_vm_tests(tests);
    }

    fn run_vm_tests(tests: Vec<(&'static str, Object)>) {
        for (input, expected) in tests {
            let program = parse(input);
            let mut compiler = Compiler::new();
            compiler
                .compile(program)
                .unwrap_or_else(|err| panic!("compiler error: {:?}", err));
            let bytecode = compiler.bytecode();
            let mut vm = VM::new(&bytecode);
            vm.run().unwrap_or_else(|err| panic!("vm error: {:?}", err));
            let stack_elem = vm
                .last_popped_stack_elem()
                .unwrap_or_else(|err| panic!("{:?}", err));
            test_expected_object(expected, stack_elem);
        }
    }

    fn parse(input: &str) -> Program {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        parser.parse().unwrap()
    }

    fn test_expected_object(expected: Object, actual: Object) {
        match expected {
            Object::Integer { value } => {
                test_integer_object(value, actual)
                    .unwrap_or_else(|err| panic!("test_integer_object failed: {:?}", err));
            }
        }
    }

    impl Object {
        pub fn r#type(&self) -> &'static str {
            match self {
                Object::Integer { .. } => "INTEGER",
            }
        }
    }

    fn test_integer_object(expected: i64, actual: Object) -> Result<()> {
        match actual {
            Object::Integer { value } => {
                if expected != value {
                    bail!("object has wrong value. want={}, got={}", expected, value);
                }
            }
            _ => {
                bail!(
                    "object is not Integer. got={} ({:?})",
                    actual.r#type(),
                    actual
                );
            }
        }
        Ok(())
    }
}
