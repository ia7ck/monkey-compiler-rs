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
}

impl<'a> VM<'a> {
    pub fn new(bytecode: &'a Bytecode) -> Self {
        Self {
            instructions: bytecode.instructions,
            constants: bytecode.constants,
            stack: Vec::with_capacity(STACK_SIZE),
        }
    }
    pub fn stack_top(&self) -> Option<&Object> {
        self.stack.last()
    }
    fn push(&mut self, obj: Object) -> Result<()> {
        if self.stack.len() >= STACK_SIZE {
            bail!("stack overflow");
        }
        self.stack.push(obj);
        Ok(())
    }
    fn pop(&mut self) -> Object {
        self.stack.pop().unwrap()
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
                    let right = self.pop();
                    let left = self.pop();
                    let result = match (left, right) {
                        (Integer { value: left }, Integer { value: right }) => left + right,
                        _ => {
                            unreachable!()
                        }
                    };
                    self.push(Integer { value: result })?;
                }
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Node, Operator};
    use crate::compiler::Compiler;
    use crate::object::Object;
    use crate::vm::VM;
    use anyhow::{bail, Result};

    struct VMTestCase {
        input: &'static str,
        expected: Object,
    }

    #[test]
    fn test_integer_arithmetic() {
        use Object::*;
        let tests = vec![
            VMTestCase {
                input: "1",
                expected: Integer { value: 1 },
            },
            VMTestCase {
                input: "2",
                expected: Integer { value: 2 },
            },
            VMTestCase {
                input: "1 + 2",
                expected: Integer { value: 3 },
            },
        ];
        run_vm_tests(&tests);
    }

    fn run_vm_tests(tests: &[VMTestCase]) {
        for tt in tests {
            let program = parse(tt.input);
            let mut compiler = Compiler::new();
            compiler
                .compile(program)
                .unwrap_or_else(|err| panic!("compiler error: {:?}", err));
            let bytecode = compiler.bytecode();
            let mut vm = VM::new(&bytecode);
            vm.run().unwrap_or_else(|err| panic!("vm error: {:?}", err));
            let stack_elem = vm.stack_top().unwrap_or_else(|| {
                panic!("stack is empty");
            });
            test_expected_object(&tt.expected, stack_elem);
        }
    }

    fn parse(input: &str) -> Node {
        use Node::*;
        if input == "1" {
            return IntegerLiteral { value: 1 };
        }
        if input == "2" {
            return IntegerLiteral { value: 2 };
        }
        if input == "1 + 2" {
            return InfixExpression {
                left: Box::new(IntegerLiteral { value: 1 }),
                operator: Operator::Plus,
                right: Box::new(IntegerLiteral { value: 2 }),
            };
        }
        todo!()
    }

    fn test_expected_object(expected: &Object, actual: &Object) {
        match expected {
            Object::Integer { value } => {
                test_integer_object(value, actual)
                    .unwrap_or_else(|err| panic!("test_integer_object failed: {:?}", err));
            }
        }
    }

    fn test_integer_object(expected: &i64, actual: &Object) -> Result<()> {
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
