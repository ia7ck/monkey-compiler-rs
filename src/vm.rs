use crate::code::{read_uint16, read_uint8, Instructions, Opcode, DEFINITIONS};
use crate::compiler::Bytecode;
use crate::frame::Frame;
use crate::object;
use crate::object::Object;
use anyhow::{bail, ensure, Result};
use std::collections::HashMap;
use std::ops::Deref;
use std::rc::Rc;

const STACK_SIZE: usize = 2048;
const GLOBAL_SIZE: usize = 65536;
const MAX_FRAME: usize = 1024;

const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);
const NULL: Object = Object::Null;
const DUMMY: Object = Object::Dummy;

pub struct VM {
    constants: Vec<Rc<Object>>,

    stack: Vec<Rc<Object>>,
    sp: usize,

    globals: Vec<Rc<Object>>,

    frames: Vec<Frame>,
    frames_index: usize,
}

impl VM {
    pub fn new(bytecode: Bytecode) -> Self {
        let constants = bytecode.constants.into_iter().map(Rc::new).collect();
        let mut frames = vec![
            Frame::new(
                Rc::new(object::Closure::new(
                    Rc::new(object::CompiledFunctionObject::new(
                        Rc::new(Instructions::new()),
                        0,
                        0
                    )),
                    vec![]
                )),
                0
            );
            MAX_FRAME
        ];
        let main_closure = object::Closure::new(
            Rc::new(object::CompiledFunctionObject::new(
                Rc::new(bytecode.instructions),
                0,
                0,
            )),
            vec![],
        );
        frames[0] = Frame::new(Rc::new(main_closure), 0);
        Self {
            constants,
            stack: vec![Rc::new(DUMMY); STACK_SIZE],
            sp: 0,
            globals: vec![Rc::new(DUMMY); GLOBAL_SIZE],
            frames,
            frames_index: 1,
        }
    }
    pub fn new_with_global_store(bytecode: Bytecode, globals: &[Object]) -> Self {
        let mut machine = VM::new(bytecode);
        machine.globals = globals.iter().map(|g| Rc::new(g.clone())).collect();
        machine
    }
    pub fn last_popped_stack_elem(&self) -> Rc<Object> {
        Rc::clone(&self.stack[self.sp])
    }
    fn push(&mut self, obj: Rc<Object>) -> Result<()> {
        if self.sp >= STACK_SIZE {
            bail!("stack overflow");
        }
        debug_assert_ne!(obj, Rc::new(DUMMY));
        self.stack[self.sp] = obj;
        self.sp += 1;
        Ok(())
    }
    fn pop(&mut self) -> Rc<Object> {
        let obj = Rc::clone(&self.stack[self.sp - 1]);
        debug_assert_ne!(obj, Rc::new(DUMMY));
        self.sp -= 1;
        obj
    }
    pub fn run(&mut self) -> Result<()> {
        use Opcode::*;
        while self.current_frame().ip() < self.current_frame().instructions().len() {
            let ip = self.current_frame().ip();
            let instructions = self.current_frame().instructions();
            let def = &DEFINITIONS[instructions[ip] as usize];
            let op = def.opcode();
            match op {
                OpConstant => {
                    let const_index = read_uint16(instructions.rest(ip + 1)) as usize;
                    // opcode (1 byte) + operand (2 byte)
                    let obj = Rc::clone(&self.constants[const_index]);
                    self.push(obj)?;
                    self.current_frame_mut().update_ip(ip + 1 + 2);
                }
                OpPop => {
                    self.pop();
                    self.current_frame_mut().update_ip(ip + 1);
                }
                OpAdd | OpSub | OpMul | OpDiv => {
                    self.execute_binary_operation(op)?;
                    self.current_frame_mut().update_ip(ip + 1);
                }
                OpTrue => {
                    self.push(Rc::new(TRUE))?;
                    self.current_frame_mut().update_ip(ip + 1);
                }
                OpFalse => {
                    self.push(Rc::new(FALSE))?;
                    self.current_frame_mut().update_ip(ip + 1);
                }
                OpEqual | OpNotEqual | OpGreaterThan => {
                    self.execute_comparison(op)?;
                    self.current_frame_mut().update_ip(ip + 1);
                }
                OpMinus => {
                    self.execute_minus_operator()?;
                    self.current_frame_mut().update_ip(ip + 1);
                }
                OpBang => {
                    self.execute_bang_operator()?;
                    self.current_frame_mut().update_ip(ip + 1);
                }
                OpJumpNotTruthy => {
                    let pos = read_uint16(instructions.rest(ip + 1)) as usize;

                    self.current_frame_mut().update_ip(ip + 1 + 2);

                    let condition = self.pop();
                    if !Self::is_truthy(&condition) {
                        self.current_frame_mut().update_ip(pos);
                    }
                }
                OpJump => {
                    let pos = read_uint16(instructions.rest(ip + 1)) as usize;
                    self.current_frame_mut().update_ip(pos);
                }
                OpNull => {
                    self.push(Rc::new(NULL))?;
                    self.current_frame_mut().update_ip(ip + 1);
                }
                OpGetGlobal => {
                    let global_index = read_uint16(instructions.rest(ip + 1)) as usize;
                    let obj = Rc::clone(&self.globals[global_index]);
                    self.push(obj)?;
                    self.current_frame_mut().update_ip(ip + 1 + 2);
                }
                OpSetGlobal => {
                    let global_index = read_uint16(instructions.rest(ip + 1)) as usize;
                    let obj = self.pop();
                    self.globals[global_index] = obj;
                    self.current_frame_mut().update_ip(ip + 1 + 2);
                }
                OpArray => {
                    let num_elements = read_uint16(instructions.rest(ip + 1)) as usize;
                    let mut elements = Vec::new();
                    for _ in 0..num_elements {
                        let e = self.pop();
                        elements.push(e);
                    }
                    elements.reverse();
                    self.push(Rc::new(Object::ArrayObject(elements)))?;
                    self.current_frame_mut().update_ip(ip + 1 + 2);
                }
                OpHash => {
                    let num_pairs = read_uint16(instructions.rest(ip + 1)) as usize;
                    let mut hash = HashMap::new();
                    for _ in 0..num_pairs {
                        let value = self.pop();
                        let key = self.pop();
                        let h = key.calculate_hash()?;
                        hash.insert(h, Rc::new(object::HashPair::new(key, value)));
                    }
                    self.push(Rc::new(Object::HashObject(hash)))?;
                    self.current_frame_mut().update_ip(ip + 1 + 2);
                }
                OpIndex => {
                    let index = self.pop();
                    let left = self.pop();
                    self.execute_index_expression(left, index)?;
                    self.current_frame_mut().update_ip(ip + 1);
                }
                OpCall => {
                    let num_arguments = read_uint8(instructions.rest(ip + 1)) as usize;
                    let function_object = Rc::clone(&self.stack[self.sp - num_arguments - 1]);
                    match function_object.deref() {
                        Object::ClosureObject(closure) => {
                            ensure!(
                                closure.function().num_parameters() == num_arguments,
                                "wrong number of arguments: want={}, got={}",
                                closure.function().num_parameters(),
                                num_arguments
                            );
                            self.current_frame_mut().update_ip(ip + 1 + 1);
                            let frame = Frame::new(Rc::clone(closure), self.sp - num_arguments);
                            self.sp = frame.base_pointer() + closure.function().num_locals();
                            self.push_frame(frame);
                        }
                        Object::BuiltinFunction(func) => {
                            self.current_frame_mut().update_ip(ip + 1 + 1);
                            let arguments = &self.stack[self.sp - num_arguments..self.sp];
                            let result = func.call(arguments)?;
                            match result {
                                Some(obj) => {
                                    self.push(obj)?;
                                }
                                None => {
                                    self.push(Rc::new(NULL))?;
                                }
                            }
                        }
                        obj => {
                            bail!("calling non-closure adnd non-builtin: {:?}", obj);
                        }
                    }
                }
                OpReturnValue => {
                    let return_value = self.pop();

                    let base_pointer = self.pop_frame().base_pointer();
                    self.sp = base_pointer - 1;

                    self.push(return_value)?;

                    let ip = self.current_frame().ip();
                    self.current_frame_mut().update_ip(ip);
                }
                OpReturn => {
                    let base_pointer = self.pop_frame().base_pointer();
                    self.sp = base_pointer - 1;

                    self.push(Rc::new(NULL))?;

                    let ip = self.current_frame().ip();
                    self.current_frame_mut().update_ip(ip);
                }
                OpGetLocal => {
                    let local_index = read_uint8(instructions.rest(ip + 1)) as usize;
                    let base_pointer = self.current_frame().base_pointer();
                    self.push(Rc::clone(&self.stack[base_pointer + local_index]))?;
                    self.current_frame_mut().update_ip(ip + 1 + 1);
                }
                OpSetLocal => {
                    let local_index = read_uint8(instructions.rest(ip + 1)) as usize;
                    let base_pointer = self.current_frame().base_pointer();
                    self.stack[base_pointer + local_index] = self.pop();
                    self.current_frame_mut().update_ip(ip + 1 + 1);
                }
                OpGetBuiltin => {
                    let builtin_index = read_uint8(instructions.rest(ip + 1)) as usize;
                    let builtin = object::BUILTINS[builtin_index];
                    self.push(Rc::new(Object::BuiltinFunction(builtin)))?;
                    self.current_frame_mut().update_ip(ip + 1 + 1);
                }
                OpClosure => {
                    let const_index = read_uint16(instructions.rest(ip + 1)) as usize;
                    let num_free = read_uint8(instructions.rest(ip + 3)) as usize;

                    match self.constants[const_index].deref() {
                        Object::CompiledFunctionObject(func) => {
                            let mut free = Vec::new();
                            for i in 0..num_free {
                                free.push(Rc::clone(&self.stack[self.sp - num_free + i]));
                            }
                            self.sp -= num_free;
                            let closure = object::Closure::new(Rc::clone(func), free);
                            self.push(Rc::new(Object::ClosureObject(Rc::new(closure))))?;
                        }
                        obj => {
                            bail!("not a function: {}", obj);
                        }
                    }

                    self.current_frame_mut().update_ip(ip + 1 + (2 + 1));
                }
                OpGetFree => {
                    let free_index = read_uint8(instructions.rest(ip + 1)) as usize;

                    let current_closure = self.current_frame().closure();
                    self.push(Rc::clone(&current_closure.free()[free_index]))?;

                    self.current_frame_mut().update_ip(ip + 1 + 1);
                }
                OpCurrentClosure => {
                    let current_closure = self.current_frame().closure();
                    self.push(Rc::new(Object::ClosureObject(Rc::clone(&current_closure))))?;
                    self.current_frame_mut().update_ip(ip + 1);
                }
            }
        }
        Ok(())
    }
    fn execute_binary_operation(&mut self, op: Opcode) -> Result<()> {
        use Object::*;
        let right = self.pop();
        let left = self.pop();
        match (left.deref(), right.deref()) {
            (Integer(left), Integer(right)) => {
                self.execute_binary_integer_operation(op, *left, *right)?;
            }
            (MonkeyString(left), MonkeyString(right)) => {
                self.execute_binary_string_operation(op, left, right)?;
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
            _ => bail!("unknown integer operator: {:?}", op),
        };
        self.push(Rc::new(Object::Integer(result)))?;
        Ok(())
    }
    fn execute_comparison(&mut self, op: Opcode) -> Result<()> {
        use Object::*;
        use Opcode::*;
        let right = self.pop();
        let left = self.pop();
        match (op, left.deref(), right.deref()) {
            (op, Integer(left), Integer(right)) => {
                self.execute_integer_comparison(op, *left, *right)?;
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
        let operand = self.pop();
        match *operand {
            Integer(value) => {
                self.push(Rc::new(Integer(-value)))?;
            }
            _ => {
                bail!("unsupported type for negation: {}", operand.r#type());
            }
        }
        Ok(())
    }
    fn execute_bang_operator(&mut self) -> Result<()> {
        use Object::*;
        let operand = self.pop();
        match *operand {
            Boolean(value) => {
                if value {
                    self.push(Rc::new(FALSE))?;
                } else {
                    self.push(Rc::new(TRUE))?;
                }
            }
            Null => {
                self.push(Rc::new(TRUE))?;
            }
            _ => {
                self.push(Rc::new(FALSE))?;
            }
        }
        Ok(())
    }
    fn execute_binary_string_operation(
        &mut self,
        op: Opcode,
        left: &str,
        right: &str,
    ) -> Result<()> {
        let result = match op {
            Opcode::OpAdd => format!("{}{}", left, right),
            _ => {
                bail!("unknown string operator: {:?}", op);
            }
        };
        self.push(Rc::new(Object::MonkeyString(result)))?;
        Ok(())
    }
    fn execute_index_expression(&mut self, left: Rc<Object>, index: Rc<Object>) -> Result<()> {
        use Object::{ArrayObject, HashObject, Integer};
        match (left.deref(), index.deref()) {
            (ArrayObject(elements), Integer(index)) => self.execute_array_index(elements, *index),
            (HashObject(hash), index) => self.execute_hash_index(hash, index),
            (left, _) => {
                bail!("index operator not supported: {}", left.r#type());
            }
        }
    }
    fn execute_array_index(&mut self, elements: &[Rc<Object>], index: i64) -> Result<()> {
        if index < 0 {
            self.push(Rc::new(NULL))?;
            return Ok(());
        }
        match elements.get(index as usize) {
            None => self.push(Rc::new(NULL)),
            Some(e) => self.push(Rc::clone(e)),
        }
    }
    fn execute_hash_index(
        &mut self,
        hash: &HashMap<u64, Rc<object::HashPair>>,
        index: &Object,
    ) -> Result<()> {
        let key = index.calculate_hash()?;
        match hash.get(&key) {
            None => self.push(Rc::new(NULL)),
            Some(pair) => self.push(pair.value()),
        }
    }
    fn native_bool_to_boolean_object(value: bool) -> Rc<Object> {
        if value {
            Rc::new(TRUE)
        } else {
            Rc::new(FALSE)
        }
    }
    fn is_truthy(obj: &Object) -> bool {
        match obj {
            Object::Boolean(value) => *value,
            Object::Null => false,
            _ => true,
        }
    }
    pub fn globals(self) -> Vec<Object> {
        self.globals
            .into_iter()
            .map(|g| g.deref().clone())
            .collect()
    }
    fn current_frame(&self) -> &Frame {
        &self.frames[self.frames_index - 1]
    }
    fn current_frame_mut(&mut self) -> &mut Frame {
        &mut self.frames[self.frames_index - 1]
    }
    fn push_frame(&mut self, frame: Frame) {
        self.frames[self.frames_index] = frame;
        self.frames_index += 1;
    }
    fn pop_frame(&mut self) -> &Frame {
        self.frames_index -= 1;
        &self.frames[self.frames_index]
    }
}

#[cfg(test)]
mod tests {
    use crate::compiler::Compiler;
    use crate::lexer::Lexer;
    use crate::object::{HashPair, Object};
    use crate::parser::Parser;
    use crate::vm::VM;
    use anyhow::{bail, ensure, Result};
    use std::collections::HashMap;
    use std::ops::Deref;
    use std::rc::Rc;
    use std::time::Instant;

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
            .map(|(input, value)| (input, Object::Integer(value)))
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
            .map(|(input, value)| (input, Object::Boolean(value)))
            .collect();
        run_vm_tests(tests);
    }

    #[test]
    fn test_conditionals() {
        macro_rules! int {
            ($x: expr) => {
                Object::Integer($x)
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

    #[test]
    fn test_global_let_statements() {
        let tests = vec![
            ("let one = 1; one", 1),
            ("let one = 1; let two = 2; one + two;", 3),
            ("let one = 1; let two = one + one; one + two;", 3),
        ];
        let tests = tests
            .into_iter()
            .map(|(input, value)| (input, Object::Integer(value)))
            .collect();
        run_vm_tests(tests);
    }

    #[test]
    fn test_string_expressions() {
        let tests = vec![
            ("  \"monkey\";  ", "monkey"),
            ("  \"mon\" + \"key\";  ", "monkey"),
            ("  \"mon\" + \"key\" + \"banana\";  ", "monkeybanana"),
        ];
        let tests = tests
            .into_iter()
            .map(|(input, value)| (input, Object::MonkeyString(value.to_string())))
            .collect();
        run_vm_tests(tests);
    }

    #[test]
    fn test_array_literals() {
        let tests = vec![
            ("[]", vec![]),
            ("[1]", vec![1]),
            ("[11, 22 + 33]", vec![11, 55]),
        ];
        let tests = tests
            .into_iter()
            .map(|(input, elements)| {
                let elements = elements
                    .into_iter()
                    .map(|v| Rc::new(Object::Integer(v)))
                    .collect();
                (input, Object::ArrayObject(elements))
            })
            .collect();
        run_vm_tests(tests);
    }

    #[test]
    fn test_hash_literals() {
        let tests = vec![
            ("{}", vec![]),
            ("{12: 3, 4 + 56: 78 * 9}", vec![(12, 3), (4 + 56, 78 * 9)]),
        ];
        let tests = tests
            .into_iter()
            .map(|(input, pairs)| {
                let hash = pairs
                    .into_iter()
                    .map(|(k, v)| {
                        let k = Object::Integer(k);
                        let v = Object::Integer(v);
                        (
                            k.calculate_hash().unwrap(),
                            Rc::new(HashPair::new(Rc::new(k), Rc::new(v))),
                        )
                    })
                    .collect();
                (input, Object::HashObject(hash))
            })
            .collect();
        run_vm_tests(tests);
    }

    #[test]
    fn test_index_expressions() {
        macro_rules! int {
            ($x: expr) => {
                Object::Integer($x)
            };
        }
        macro_rules! null {
            () => {
                Object::Null
            };
        }
        let tests = vec![
            ("[1, 2, 3][1]", int!(2)),
            ("[1, 2, 3][0 + 2]", int!(3)),
            ("[[1, 1, 1]][0][0]", int!(1)),
            ("[][0]", null!()),
            ("[1, 2, 3][99]", null!()),
            ("[1][-1]", null!()),
            ("{1: 1, 2: 2}[1]", int!(1)),
            ("{1: 1, 2: 2}[2]", int!(2)),
            ("{1: 1}[0]", null!()),
            ("{}[0]", null!()),
        ];
        run_vm_tests(tests);
    }

    #[test]
    fn test_calling_functions_without_arguments() {
        macro_rules! int {
            ($x: expr) => {
                Object::Integer($x)
            };
        }
        let tests = vec![
            (
                r#"
                let fivePlusTen = fn() { 10 + 5 };
                fivePlusTen();
                "#,
                int!(15),
            ),
            (
                r#"
                let one = fn() { 1; };
                let two = fn() { 2; };
                one() + two()
                "#,
                int!(3),
            ),
            (
                r#"
                let a = fn() { 1 };
                let b = fn() { a() + 1 };
                let c = fn() { b() + 1 };
                c();
                "#,
                int!(3),
            ),
        ];
        run_vm_tests(tests);
    }

    #[test]
    fn test_functions_with_return_statement() {
        macro_rules! int {
            ($x: expr) => {
                Object::Integer($x)
            };
        }
        let tests = vec![
            (
                r#"
                let earlyExit = fn() { return 99; 100; };
                earlyExit();
                "#,
                int!(99),
            ),
            (
                r#"
                let earlyExit = fn() { return 99; return 100; };
                earlyExit();
                "#,
                int!(99),
            ),
        ];
        run_vm_tests(tests);
    }

    #[test]
    fn test_functions_without_return_value() {
        let tests = vec![
            (
                r#"
                let noReturn = fn() { };
                noReturn();
                "#,
                Object::Null,
            ),
            (
                r#"
                let noReturn = fn() { };
                let noReturnTwo = fn() { noReturn(); };
                noReturn();
                noReturnTwo();
                "#,
                Object::Null,
            ),
        ];
        run_vm_tests(tests);
    }

    #[test]
    fn test_first_class_functions() {
        macro_rules! int {
            ($x: expr) => {
                Object::Integer($x)
            };
        }
        let tests = vec![
            (
                r#"
                let returnsOne = fn() { 1; };
                let returnsOneReturner = fn() { returnsOne; };
                returnsOneReturner()();
                "#,
                int!(1),
            ),
            (
                r#"
                let returnsOneReturner = fn() {
                    let returnsOne = fn() { 1; };
                    returnsOne;
                };
                returnsOneReturner()();
                "#,
                int!(1),
            ),
        ];
        run_vm_tests(tests);
    }

    #[test]
    fn test_calling_functions_with_bindings() {
        macro_rules! int {
            ($x: expr) => {
                Object::Integer($x)
            };
        }
        let tests = vec![
            (
                r#"
                let one = fn() { let one = 1; one };
                one();
                "#,
                int!(1),
            ),
            (
                r#"
                let oneAndTwo = fn() { let one = 1; let two = 2; one + two; };
                oneAndTwo();
                "#,
                int!(3),
            ),
            (
                r#"
                let globalSeed = 50;
                let minusOne = fn() { let num = 1; globalSeed - num };
                minusOne();
                "#,
                int!(49),
            ),
        ];
        run_vm_tests(tests);
    }

    #[test]
    fn test_calling_functions_with_arguments_and_bindings() {
        macro_rules! int {
            ($x: expr) => {
                Object::Integer($x)
            };
        }
        let tests = vec![
            (
                r#"
                let identity = fn(a) { a; };
                identity(42);
                "#,
                int!(42),
            ),
            (
                r#"
                let sum = fn(a, b) { a + b; };
                sum(1, 2);
                "#,
                int!(3),
            ),
            (
                r#"
                let sum = fn(a, b) {
                    let c = a + b;
                    c;
                };
                sum(1, 2) + sum(3, 4);
                "#,
                int!(10),
            ),
            (
                r#"
                let globalNum = 10;

                let sum = fn(a, b) {
                    let c = a + b;
                    c + globalNum;
                };

                let outer = fn() {
                    sum(1, 2) + sum(3, 4) + globalNum;
                };

                outer() + globalNum;
                "#,
                int!(50),
            ),
        ];
        run_vm_tests(tests);
    }

    #[test]
    fn test_calling_functions_with_wrong_arguments() {
        let tests = vec![
            (
                "fn() { 1; }(1);",
                "wrong number of arguments: want=0, got=1",
            ),
            (
                "fn(a) { a; }();",
                "wrong number of arguments: want=1, got=0",
            ),
            (
                "fn(a, b) { a + b; }(1);",
                "wrong number of arguments: want=2, got=1",
            ),
        ];
        for (input, expected) in tests {
            let actual = execute(input).unwrap_err();
            assert_eq!(expected, format!("{}", actual));
        }
    }

    #[test]
    fn test_builtin_functions() {
        macro_rules! int {
            ($x: expr) => {
                Object::Integer($x)
            };
        }
        let tests = vec![
            (r#"len("");"#, int!(0)),
            (r#"len("four");"#, int!(4)),
            ("len([]);", int!(0)),
            ("len([1, 2, 3]);", int!(3)),
            (r#"puts("hello world");"#, Object::Null),
            ("first([1, 2, 3]);", int!(1)),
            ("first([]);", Object::Null),
            ("last([1, 2, 3]);", int!(3)),
            ("last([]);", Object::Null),
            (
                "rest([1, 2, 3]);",
                Object::ArrayObject(vec![Rc::new(int!(2)), Rc::new(int!(3))]),
            ),
            ("rest([]);", Object::Null),
            (
                "push([1, 2], 3);",
                Object::ArrayObject(vec![Rc::new(int!(1)), Rc::new(int!(2)), Rc::new(int!(3))]),
            ),
        ];
        run_vm_tests(tests);
    }

    #[test]
    fn test_builtin_functions_with_wrong_arguments() {
        let tests = vec![
            ("len(42);", "argument to `len` not supported, got INTEGER"),
            (
                r#"len("one", "two");"#,
                "wrong number of arguments. got=2, want=1",
            ),
            (
                "first(42);",
                "argument to `first` must be ARRAY, got INTEGER",
            ),
            (
                r#"last("777");"#,
                "argument to `last` must be ARRAY, got STRING",
            ),
            (
                "push(12, 3);",
                "argument to `push` must be ARRAY, got INTEGER",
            ),
        ];
        for (input, expected) in tests {
            let actual = execute(input).unwrap_err();
            assert_eq!(expected, format!("{}", actual));
        }
    }

    #[test]
    fn test_closures() {
        macro_rules! int {
            ($x: expr) => {
                Object::Integer($x)
            };
        }
        let tests = vec![
            (
                r#"
                let newClosure = fn(a) {
                    fn() { a; };
                };
                let closure = newClosure(42);
                closure();
                "#,
                int!(42),
            ),
            (
                r#"
                let newAdder = fn(a, b) {
                    fn(c) { a + b + c };
                };
                let adder = newAdder(1, 2);
                adder(8);
                "#,
                int!(11),
            ),
            (
                r#"
                let newAdderOuter = fn(a, b) {
                    let c = a + b;
                    fn(d) {
                        let e = d + c;
                        fn(f) { e + f; };
                    };
                };
                let newAdderInner = newAdderOuter(1, 2)
                let adder = newAdderInner(3);
                adder(8);
                "#,
                int!(14),
            ),
            (
                r#"
                let a = 1;
                let newAdderOuter = fn(b) {
                    fn(c) {
                        fn(d) { a + b + c + d };
                    };
                };
                let newAdderInner = newAdderOuter(2)
                let adder = newAdderInner(3);
                adder(8);
                "#,
                int!(14),
            ),
            (
                r#"
                let newClosure = fn(a, b) {
                    let one = fn() { a; };
                    let two = fn() { b; };
                    fn() { one() + two(); };
                };
                let closure = newClosure(9, 90);
                closure();
                "#,
                int!(99),
            ),
        ];
        run_vm_tests(tests);
    }

    #[test]
    fn test_recursive_functions() {
        macro_rules! int {
            ($x: expr) => {
                Object::Integer($x)
            };
        }
        let tests = vec![
            (
                r#"
                let countDown = fn(x) {
                    if (x == 0) {
                        return 0;
                    } else {
                        countDown(x - 1);
                    }
                };
                countDown(3);
                "#,
                int!(0),
            ),
            (
                r#"
                let countDown = fn(x) {
                    if (x == 0) {
                        return 0;
                    } else {
                        countDown(x - 1);
                    }
                };
                let wrapper = fn() {
                    countDown(3);
                }
                wrapper();
                "#,
                int!(0),
            ),
            (
                r#"
                let wrapper = fn() {
                    let countDown = fn(x) {
                        if (x == 0) {
                            return 0;
                        } else {
                            countDown(x - 1);
                        }
                    };
                    countDown(3);
                };
                wrapper();
                "#,
                int!(0),
            ),
        ];
        run_vm_tests(tests);
    }

    fn execute(input: &str) -> Result<Rc<Object>> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse().unwrap();
        let mut compiler = Compiler::new();
        compiler.compile(program).unwrap();
        let bytecode = compiler.bytecode();
        let mut vm = VM::new(bytecode);
        vm.run()?;
        Ok(vm.last_popped_stack_elem())
    }

    fn run_vm_tests(tests: Vec<(&'static str, Object)>) {
        for (input, expected) in tests {
            let actual = execute(input).unwrap_or_else(|err| panic!("{}\ninput:\n{}", err, input));
            test_expected_object(&expected, &actual);
        }
    }

    fn test_expected_object(expected: &Object, actual: &Object) {
        use Object::*;
        match expected {
            Integer(value) => {
                test_integer_object(value, actual)
                    .unwrap_or_else(|err| panic!("test_integer_object failed: {:?}", err));
            }
            MonkeyString(value) => {
                test_string_object(value, actual)
                    .unwrap_or_else(|err| panic!("test_string_object failed: {:?}", err));
            }
            Boolean(value) => {
                test_boolean_object(value, actual)
                    .unwrap_or_else(|err| panic!("test_boolean_object failed: {:?}", err));
            }
            ArrayObject(elements) => {
                test_array_object(elements, actual)
                    .unwrap_or_else(|err| panic!("test_array_object failed: {:?}", err));
            }
            HashObject(hash) => {
                test_hash_object(hash, actual)
                    .unwrap_or_else(|err| panic!("test_hash_object failed: {:?}", err));
            }
            CompiledFunctionObject(..) => unimplemented!(),
            BuiltinFunction(..) => unimplemented!(),
            ClosureObject(..) => unimplemented!(),
            Null => {
                assert_eq!(&Null, actual);
            }
            Dummy => unreachable!(),
        }
    }

    fn test_integer_object(expected: &i64, actual: &Object) -> Result<()> {
        match actual {
            Object::Integer(value) => {
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

    fn test_string_object(expected: &str, actual: &Object) -> Result<()> {
        match actual {
            Object::MonkeyString(value) => {
                if expected != value {
                    bail!("object has wrong value. want={}, got={}", expected, value);
                }
            }
            _ => {
                bail!(
                    "object is not String. got={} ({:?})",
                    actual.r#type(),
                    actual
                );
            }
        }
        Ok(())
    }

    fn test_boolean_object(expected: &bool, actual: &Object) -> Result<()> {
        match actual {
            Object::Boolean(value) => {
                ensure!(
                    expected == value,
                    "object has wrong value. want={}, got={}",
                    expected,
                    value
                );
            }
            _ => bail!(
                "object is not Boolean. got={} ({:?})",
                actual.r#type(),
                actual
            ),
        }
        Ok(())
    }

    fn test_array_object(expected: &Vec<Rc<Object>>, actual: &Object) -> Result<()> {
        match actual {
            Object::ArrayObject(elements) => {
                ensure!(
                    expected.len() == elements.len(),
                    "wrong number of elements. want={}, got={}",
                    expected.len(),
                    elements.len()
                );
                for (expected, actual) in expected.iter().zip(elements.iter()) {
                    test_expected_object(expected, actual);
                }
            }
            _ => {
                bail!(
                    "object is not Array. got={} ({:?})",
                    actual.r#type(),
                    actual
                );
            }
        }
        Ok(())
    }

    fn test_hash_object(expected: &HashMap<u64, Rc<HashPair>>, actual: &Object) -> Result<()> {
        match actual {
            Object::HashObject(hash) => {
                ensure!(
                    expected.len() == hash.len(),
                    "wrong number of pairs. want={}, got={}",
                    expected.len(),
                    hash.len()
                );
                for (expected_key, expected_pair) in expected {
                    match hash.get(expected_key) {
                        None => {
                            bail!("no pair for given key in pairs");
                        }
                        Some(actual_pair) => {
                            test_expected_object(
                                expected_pair.value().deref(),
                                actual_pair.value().deref(),
                            );
                        }
                    }
                }
            }
            _ => bail!("object is not Hash. got={} ({:?})", actual.r#type(), actual),
        }
        Ok(())
    }

    #[test]
    #[ignore]
    fn bench_fibonacci() {
        let input = r#"
    let fib = fn(n) {
        if (n == 0) {
            return 0;
        }
        if (n == 1) {
            return 1;
        }
        return fib(n - 1) + fib(n - 2);
    };
    fib(20);
        "#;
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse().unwrap();
        let mut compiler = Compiler::new();
        compiler.compile(program).unwrap();

        let start = Instant::now();
        let mut vm = VM::new(compiler.bytecode());
        vm.run().unwrap();
        let end = Instant::now();

        println!("Program");
        println!("{}", input);
        println!("took {} seconds", end.duration_since(start).as_secs_f64());
        println!("result: {}", vm.last_popped_stack_elem());
    }
}
