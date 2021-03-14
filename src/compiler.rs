use crate::ast::{Node, Operator};
use crate::code::{make, Instructions, Opcode};
use crate::object::Object;
use anyhow::Result;

pub struct Compiler {
    instructions: Instructions,
    constants: Vec<Object>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            instructions: Instructions::new(),
            constants: vec![],
        }
    }
    pub fn compile(&mut self, node: Node) -> Result<()> {
        use Node::*;
        use Object::*;
        use Opcode::*;
        use Operator::*;
        match node {
            InfixExpression {
                left,
                operator,
                right,
            } => {
                self.compile(*left)?;
                self.compile(*right)?;
                match operator {
                    Plus => {
                        self.emit(OpAdd, &[]);
                    }
                }
            }
            IntegerLiteral { value } => {
                let integer = Integer { value };
                let operands = &[self.add_constant(integer)];
                self.emit(OpConstant, operands);
            }
        }
        Ok(())
    }
    fn add_constant(&mut self, obj: Object) -> usize {
        self.constants.push(obj);
        self.constants.len() - 1
    }
    fn emit(&mut self, op: Opcode, operands: &[usize]) -> usize {
        let instruction = make(op, operands);
        self.add_instruction(instruction)
    }
    fn add_instruction(&mut self, instruction: Instructions) -> usize {
        let pos_new_instruction = self.instructions.len();
        self.instructions.extend(instruction.iter().copied());
        pos_new_instruction
    }
    pub fn bytecode(&self) -> Bytecode {
        Bytecode {
            instructions: &self.instructions,
            constants: &self.constants,
        }
    }
}

pub struct Bytecode<'a> {
    pub instructions: &'a Instructions,
    pub constants: &'a [Object],
}

#[cfg(test)]
mod tests {
    use crate::ast::{Node, Operator};
    use crate::code::Opcode;
    use crate::code::{make, Instructions};
    use crate::compiler::Compiler;
    use crate::object::Object;
    use anyhow::{bail, Result};

    enum Constant {
        Integer(i64),
    }
    struct CompilerTestCase {
        input: &'static str,
        expected_constants: Vec<Constant>,
        expected_instructions: Vec<Instructions>,
    }

    #[test]
    fn test_integer_arithmetic() {
        use Constant::*;
        use Opcode::*;
        let tests = vec![CompilerTestCase {
            input: "1 + 2",
            expected_constants: vec![Integer(1), Integer(2)],
            expected_instructions: vec![
                make(OpConstant, &[0]),
                make(OpConstant, &[1]),
                make(OpAdd, &[]),
            ],
        }];
        run_compiler_tests(&tests);
    }

    fn run_compiler_tests(tests: &[CompilerTestCase]) {
        for tt in tests {
            let program = parse(tt.input);
            let mut compiler = Compiler::new();
            compiler
                .compile(program)
                .unwrap_or_else(|err| panic!("compiler error: {:?}", err));
            let bytecode = compiler.bytecode();
            test_instructions(&tt.expected_instructions, &bytecode.instructions);
            test_constants(&tt.expected_constants, &bytecode.constants);
        }
    }

    fn parse(input: &str) -> Node {
        use Node::*;
        if input == "1 + 2" {
            return InfixExpression {
                left: Box::new(Node::IntegerLiteral { value: 1 }),
                operator: Operator::Plus,
                right: Box::new(Node::IntegerLiteral { value: 2 }),
            };
        }
        todo!()
    }

    fn test_instructions(expected: &[Instructions], actual: &Instructions) {
        let expected = concat_instructions(expected);
        assert_eq!(
            expected.len(),
            actual.len(),
            "wrong instructions length.\nwant={}\ngot ={}",
            expected,
            actual
        );
        for (i, (ex, ac)) in expected.iter().zip(actual.iter()).enumerate() {
            assert_eq!(
                ex, ac,
                "wrong instruction at {}.\nwant={:?}\ngot ={:?}",
                i, expected, actual
            );
        }
    }

    fn concat_instructions(s: &[Instructions]) -> Instructions {
        s.iter()
            .map(|instruction| instruction.iter())
            .flatten()
            .copied()
            .collect()
    }

    fn test_constants(expected: &[Constant], actual: &[Object]) {
        use Constant::*;
        assert_eq!(expected.len(), actual.len(), "wrong number of constants.",);
        for (i, (ex, ac)) in expected.iter().zip(actual.iter()).enumerate() {
            match ex {
                Integer(val) => {
                    test_integer_object(val, ac).unwrap_or_else(|err| {
                        panic!("constant {} - test_integer_object failed: {:?}", i, err)
                    });
                }
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
