use crate::code::{read_uint16, Instructions, Opcode, DEFINITIONS};
use crate::compiler::Bytecode;
use crate::object::Object;
use crate::object::Object::Integer;
use anyhow::{bail, Result};

const STACK_SIZE: usize = 2048;

const TRUE: Object = Object::Boolean { value: true };
const FALSE: Object = Object::Boolean { value: false };
const NULL: Object = Object::Null;

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
        use Opcode::*;
        let mut ip = 0;
        while ip < self.instructions.len() {
            let def = &DEFINITIONS[self.instructions[ip] as usize];
            let op = def.opcode;
            match op {
                OpConstant => {
                    let const_index = read_uint16(self.instructions.rest(ip + 1)) as usize;
                    // opcode (1 byte) + operand (2 byte)
                    let obj = self.constants[const_index].clone();
                    self.push(obj)?;
                    ip += 3;
                }
                OpPop => {
                    self.pop()?;
                    ip += 1;
                }
                OpAdd | OpSub | OpMul | OpDiv => {
                    self.execute_binary_operation(op)?;
                    ip += 1;
                }
                OpTrue => {
                    self.push(TRUE)?;
                    ip += 1;
                }
                OpFalse => {
                    self.push(FALSE)?;
                    ip += 1;
                }
                OpEqual | OpNotEqual | OpGreaterThan => {
                    self.execute_comparison(op)?;
                    ip += 1;
                }
                OpMinus => {
                    self.execute_minus_operator()?;
                    ip += 1;
                }
                OpBang => {
                    self.execute_bang_operator()?;
                    ip += 1;
                }
                OpJumpNotTruthy => {
                    let pos = read_uint16(self.instructions.rest(ip + 1)) as usize;
                    ip += 1 + 2; // opcode (1byte) + operand (2byte)
                    let condition = self.pop()?;
                    if !Self::is_truthy(&condition) {
                        ip = pos;
                    }
                }
                OpJump => {
                    let pos = read_uint16(self.instructions.rest(ip + 1)) as usize;
                    ip = pos;
                }
                OpNull => {
                    self.push(NULL)?;
                    ip += 1;
                }
            }
        }
        Ok(())
    }
    fn execute_binary_operation(&mut self, op: Opcode) -> Result<()> {
        use Object::*;
        let right = self.pop()?;
        let left = self.pop()?;
        match (left, right) {
            (Integer { value: left }, Integer { value: right }) => {
                self.execute_binary_integer_operation(op, left, right)?;
            }
            (left, right) => {
                bail!(
                    "unsupported types for binary operation {} {}",
                    left.r#type(),
                    right.r#type()
                );
            }
        };
        Ok(())
    }
    fn execute_binary_integer_operation(
        &mut self,
        op: Opcode,
        left: i64,
        right: i64,
    ) -> Result<()> {
        use Opcode::*;
        let result = match op {
            OpAdd => left + right,
            OpSub => left - right,
            OpMul => left * right,
            OpDiv => left / right,
            _ => {
                bail!("unknown integer operator: {:?}", op)
            }
        };
        self.push(Integer { value: result })?;
        Ok(())
    }
    fn execute_comparison(&mut self, op: Opcode) -> Result<()> {
        use Object::*;
        use Opcode::*;
        let right = self.pop()?;
        let left = self.pop()?;
        match (op, left, right) {
            (op, Integer { value: left }, Integer { value: right }) => {
                self.execute_integer_comparison(op, left, right)?;
            }
            (op, left, right) if op == OpEqual => {
                self.push(Self::native_bool_to_boolean_object(left == right))?;
            }
            (op, left, right) if op == OpNotEqual => {
                self.push(Self::native_bool_to_boolean_object(left != right))?;
            }
            (op, left, right) => {
                bail!(
                    "unknown operator: {:?} ({} {})",
                    op,
                    left.r#type(),
                    right.r#type()
                );
            }
        }
        Ok(())
    }
    fn execute_integer_comparison(&mut self, op: Opcode, left: i64, right: i64) -> Result<()> {
        use Opcode::*;
        let result = match op {
            OpEqual => left == right,
            OpNotEqual => left != right,
            OpGreaterThan => left > right,
            _ => bail!("unknown operator: {:?}", op),
        };
        self.push(Self::native_bool_to_boolean_object(result))?;
        Ok(())
    }
    fn execute_minus_operator(&mut self) -> Result<()> {
        use Object::*;
        let operand = self.pop()?;
        match operand {
            Integer { value } => {
                self.push(Integer { value: -value })?;
            }
            _ => {
                bail!("unsupported type for negation: {}", operand.r#type());
            }
        }
        Ok(())
    }
    fn execute_bang_operator(&mut self) -> Result<()> {
        use Object::*;
        let operand = self.pop()?;
        match operand {
            Boolean { value } => {
                if value {
                    self.push(FALSE)?;
                } else {
                    self.push(TRUE)?;
                }
            }
            Null => {
                self.push(TRUE)?;
            }
            _ => {
                self.push(FALSE)?;
            }
        }
        Ok(())
    }
    fn native_bool_to_boolean_object(value: bool) -> Object {
        if value {
            TRUE
        } else {
            FALSE
        }
    }
    fn is_truthy(obj: &Object) -> bool {
        match obj {
            Object::Boolean { value } => *value,
            Object::Null => false,
            _ => true,
        }
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
    use anyhow::{bail, ensure, Result};

    #[test]
    fn test_integer_arithmetic() {
        let tests = vec![
            ("1", 1),
            ("2", 2),
            ("1 + 2", 3),
            ("1 * 2", 2),
            ("4 / 2", 2),
            ("50 / 2 * 2 + 10 - 5", 55),
            ("5 * (2 + 10)", 60),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("5 * (2 + 10)", 60),
            ("-5", -5),
            ("-10", -10),
            ("-50 + 100 + -50", 0),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];
        let tests = tests
            .into_iter()
            .map(|(input, value)| (input, Object::Integer { value }))
            .collect();
        run_vm_tests(tests);
    }

    #[test]
    fn test_boolean_expressions() {
        let tests = vec![
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("(1 < 2) != (3 > 4)", true),
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
            ("!(if (false) { 5; })", true),
        ];
        let tests = tests
            .into_iter()
            .map(|(input, value)| (input, Object::Boolean { value }))
            .collect();
        run_vm_tests(tests);
    }

    #[test]
    fn test_conditionals() {
        macro_rules! int {
            ($x: expr) => {
                Object::Integer { value: $x }
            };
        }
        macro_rules! null {
            () => {
                Object::Null
            };
        }
        let tests = vec![
            ("if (true) { 1 }", int!(1)),
            ("if (true) { 1 } else { 2 }", int!(1)),
            ("if (false) { 1 } else { 2 }", int!(2)),
            ("if (1) { 2 }", int!(2)),
            ("if (1 < 2) { 3 }", int!(3)),
            ("if (1 < 2) { 3 } else { 4 }", int!(3)),
            ("if (1 > 2) { 3 } else { 4 }", int!(4)),
            ("if (1 > 2) { 3 }", null!()),
            ("if (false) { 3 }", null!()),
            ("if ((if (false) { 10 })) { 10 } else { 20 }", int!(20)),
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
        use Object::*;
        match expected {
            Integer { value } => {
                test_integer_object(value, actual)
                    .unwrap_or_else(|err| panic!("test_integer_object failed: {:?}", err));
            }
            Boolean { value } => {
                test_boolean_object(value, actual)
                    .unwrap_or_else(|err| panic!("test_boolean_object failed: {:?}", err));
            }
            Null => {
                assert_eq!(Null, actual);
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

    fn test_boolean_object(expected: bool, actual: Object) -> Result<()> {
        match actual {
            Object::Boolean { value } => {
                ensure!(
                    expected == value,
                    "object has wrong value. want={}, got={}",
                    expected,
                    value
                );
            }
            _ => {
                bail!(
                    "object is not Boolean. got={} ({:?})",
                    actual.r#type(),
                    actual
                )
            }
        }
        Ok(())
    }
}
