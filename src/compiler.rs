use crate::ast::{Expression, InfixOperator, PrefixOperator, Program, Statement};
use crate::code::{make, Instructions, Opcode, DEFINITIONS};
use crate::object::Object;
use anyhow::Result;

pub struct Compiler {
    instructions: Instructions,
    constants: Vec<Object>,
    last_instruction: Option<EmittedInstruction>,
    previous_instruction: Option<EmittedInstruction>,
}

struct EmittedInstruction {
    opcode: Opcode,
    position: usize,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            instructions: Instructions::new(),
            constants: vec![],
            last_instruction: None,
            previous_instruction: None,
        }
    }
    pub fn compile(&mut self, program: Program) -> Result<()> {
        for stmt in program.statements {
            self.compile_statement(stmt)?;
        }
        Ok(())
    }
    fn compile_statement(&mut self, statement: Statement) -> Result<()> {
        use Statement::*;
        match statement {
            ExpressionStatement(exp) => {
                self.compile_expression(exp)?;
                self.emit(Opcode::OpPop, &[]);
            }
            BlockStatement(statements) => {
                for stmt in statements {
                    self.compile_statement(stmt)?;
                }
            }
        }
        Ok(())
    }
    fn compile_expression(&mut self, expression: Expression) -> Result<()> {
        use Expression::*;
        use InfixOperator::*;
        use Object::*;
        use Opcode::*;
        match expression {
            InfixExpression {
                left,
                operator,
                right,
            } => {
                let (left, right) = if operator == LT {
                    // swap
                    (right, left)
                } else {
                    (left, right)
                };
                self.compile_expression(*left)?;
                self.compile_expression(*right)?;
                match operator {
                    PLUS => {
                        self.emit(OpAdd, &[]);
                    }
                    MINUS => {
                        self.emit(OpSub, &[]);
                    }
                    ASTERISK => {
                        self.emit(OpMul, &[]);
                    }
                    SLASH => {
                        self.emit(OpDiv, &[]);
                    }
                    LT | GT => {
                        self.emit(OpGreaterThan, &[]);
                    }
                    EQ => {
                        self.emit(OpEqual, &[]);
                    }
                    NEQ => {
                        self.emit(OpNotEqual, &[]);
                    }
                }
            }
            IntegerLiteral { value } => {
                let integer = Integer { value };
                let operands = &[self.add_constant(integer)];
                self.emit(OpConstant, operands);
            }
            Expression::Boolean { value } => {
                if value {
                    self.emit(OpTrue, &[]);
                } else {
                    self.emit(OpFalse, &[]);
                }
            }
            PrefixExpression { operator, right } => {
                self.compile_expression(*right)?;
                match operator {
                    PrefixOperator::MINUS => {
                        self.emit(OpMinus, &[]);
                    }
                    PrefixOperator::BANG => {
                        self.emit(OpBang, &[]);
                    }
                }
            }
            IfExpression {
                condition,
                consequence,
                alternative,
            } => {
                self.compile_expression(*condition)?;

                let jump_not_truthy_pos = self.emit(OpJumpNotTruthy, &[9999]);

                self.compile_statement(*consequence)?;
                if self.last_instruction_is_pop() {
                    self.remove_last_pop();
                }

                let jump_pos = self.emit(OpJump, &[9999]);

                let after_consequence_pos = self.instructions.len();
                self.change_operand(jump_not_truthy_pos, after_consequence_pos);

                match alternative {
                    None => {
                        self.emit(OpNull, &[]);
                    }
                    Some(alt) => {
                        self.compile_statement(*alt)?;
                        if self.last_instruction_is_pop() {
                            self.remove_last_pop();
                        }
                    }
                }

                let after_alternative_pos = self.instructions.len();
                self.change_operand(jump_pos, after_alternative_pos);
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
        let position = self.add_instruction(instruction);
        self.set_last_instruction(op, position);
        position
    }
    fn add_instruction(&mut self, instruction: Instructions) -> usize {
        let pos_new_instruction = self.instructions.len();
        self.instructions.extend(instruction);
        pos_new_instruction
    }
    fn set_last_instruction(&mut self, op: Opcode, pos: usize) {
        let last = EmittedInstruction {
            opcode: op,
            position: pos,
        };
        let mut last = Some(last);
        // self.prev <- self.last
        // self.last <- last
        std::mem::swap(&mut self.previous_instruction, &mut self.last_instruction);
        std::mem::swap(&mut self.last_instruction, &mut last);
    }
    fn last_instruction_is_pop(&self) -> bool {
        match &self.last_instruction {
            Some(last) => last.opcode == Opcode::OpPop,
            None => false,
        }
    }
    fn remove_last_pop(&mut self) {
        let last = self
            .last_instruction
            .as_ref()
            .unwrap_or_else(|| panic!("not found last instruction"));
        self.instructions.truncate(last.position);
        // self.last <- self.prev
        std::mem::swap(&mut self.last_instruction, &mut self.previous_instruction);
        self.previous_instruction = None;
    }
    fn replace_instruction(&mut self, pos: usize, new_instruction: Instructions) {
        for i in 0..new_instruction.len() {
            self.instructions[pos + i] = new_instruction[i];
        }
    }
    fn change_operand(&mut self, op_pos: usize, operand: usize) {
        let def = &DEFINITIONS[self.instructions[op_pos] as usize];
        let new_instruction = make(def.opcode, &[operand]);
        self.replace_instruction(op_pos, new_instruction);
    }
    pub fn bytecode(self) -> Bytecode {
        Bytecode {
            instructions: self.instructions,
            constants: self.constants,
        }
    }
}

pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}

#[cfg(test)]
mod tests {
    use crate::code::Opcode;
    use crate::code::{make, Instructions};
    use crate::compiler::Compiler;
    use crate::lexer::Lexer;
    use crate::object::Object;
    use crate::parser::Parser;
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
        let tests = vec![
            CompilerTestCase {
                input: "1 + 2",
                expected_constants: vec![Integer(1), Integer(2)],
                expected_instructions: vec![
                    make(OpConstant, &[0]),
                    make(OpConstant, &[1]),
                    make(OpAdd, &[]),
                    make(OpPop, &[]),
                ],
            },
            CompilerTestCase {
                input: "1; 2",
                expected_constants: vec![Integer(1), Integer(2)],
                expected_instructions: vec![
                    make(OpConstant, &[0]),
                    make(OpPop, &[]),
                    make(OpConstant, &[1]),
                    make(OpPop, &[]),
                ],
            },
            CompilerTestCase {
                input: "1 - 2",
                expected_constants: vec![Integer(1), Integer(2)],
                expected_instructions: vec![
                    make(OpConstant, &[0]),
                    make(OpConstant, &[1]),
                    make(OpSub, &[]),
                    make(OpPop, &[]),
                ],
            },
            CompilerTestCase {
                input: "1 * 2",
                expected_constants: vec![Integer(1), Integer(2)],
                expected_instructions: vec![
                    make(OpConstant, &[0]),
                    make(OpConstant, &[1]),
                    make(OpMul, &[]),
                    make(OpPop, &[]),
                ],
            },
            CompilerTestCase {
                input: "2 / 1",
                expected_constants: vec![Integer(2), Integer(1)],
                expected_instructions: vec![
                    make(OpConstant, &[0]),
                    make(OpConstant, &[1]),
                    make(OpDiv, &[]),
                    make(OpPop, &[]),
                ],
            },
            CompilerTestCase {
                input: "-1",
                expected_constants: vec![Integer(1)],
                expected_instructions: vec![
                    make(OpConstant, &[0]),
                    make(OpMinus, &[]),
                    make(OpPop, &[]),
                ],
            },
        ];
        run_compiler_tests(tests);
    }

    #[test]
    fn test_boolean_expressions() {
        use Constant::*;
        use Opcode::*;
        let tests = vec![
            CompilerTestCase {
                input: "true",
                expected_constants: vec![],
                expected_instructions: vec![make(OpTrue, &[]), make(OpPop, &[])],
            },
            CompilerTestCase {
                input: "false",
                expected_constants: vec![],
                expected_instructions: vec![make(OpFalse, &[]), make(OpPop, &[])],
            },
            CompilerTestCase {
                input: "1 > 2",
                expected_constants: vec![Integer(1), Integer(2)],
                expected_instructions: vec![
                    make(OpConstant, &[0]),
                    make(OpConstant, &[1]),
                    make(OpGreaterThan, &[]),
                    make(OpPop, &[]),
                ],
            },
            CompilerTestCase {
                input: "1 < 2",
                expected_constants: vec![Integer(2), Integer(1)], // !!!
                expected_instructions: vec![
                    make(OpConstant, &[0]),
                    make(OpConstant, &[1]),
                    make(OpGreaterThan, &[]),
                    make(OpPop, &[]),
                ],
            },
            CompilerTestCase {
                input: "1 == 2",
                expected_constants: vec![Integer(1), Integer(2)],
                expected_instructions: vec![
                    make(OpConstant, &[0]),
                    make(OpConstant, &[1]),
                    make(OpEqual, &[]),
                    make(OpPop, &[]),
                ],
            },
            CompilerTestCase {
                input: "1 != 2",
                expected_constants: vec![Integer(1), Integer(2)],
                expected_instructions: vec![
                    make(OpConstant, &[0]),
                    make(OpConstant, &[1]),
                    make(OpNotEqual, &[]),
                    make(OpPop, &[]),
                ],
            },
            CompilerTestCase {
                input: "!true",
                expected_constants: vec![],
                expected_instructions: vec![make(OpTrue, &[]), make(OpBang, &[]), make(OpPop, &[])],
            },
        ];
        run_compiler_tests(tests);
    }

    #[test]
    fn test_conditionals() {
        use Constant::*;
        use Opcode::*;
        let tests = vec![CompilerTestCase {
            input: "if (true) { 1 } else { 2 }; 33;",
            expected_constants: vec![Integer(1), Integer(2), Integer(33)],
            expected_instructions: vec![
                // 0000
                make(OpTrue, &[]),
                // 0001
                make(OpJumpNotTruthy, &[10]),
                // 0004
                make(OpConstant, &[0]),
                // 0007
                make(OpJump, &[13]),
                // 0010
                make(OpConstant, &[1]),
                // 0013
                make(OpPop, &[]),
                // 0014
                make(OpConstant, &[2]),
                // 0017
                make(OpPop, &[]),
            ],
        }];
        run_compiler_tests(tests);
    }

    fn run_compiler_tests(tests: Vec<CompilerTestCase>) {
        for tt in tests {
            let lexer = Lexer::new(tt.input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse().unwrap();
            let mut compiler = Compiler::new();
            compiler
                .compile(program)
                .unwrap_or_else(|err| panic!("compiler error: {:?}", err));
            let bytecode = compiler.bytecode();
            test_instructions(tt.expected_instructions, bytecode.instructions);
            test_constants(tt.expected_constants, bytecode.constants);
        }
    }

    fn test_instructions(expected: Vec<Instructions>, actual: Instructions) {
        let expected = concat_instructions(expected);
        assert_eq!(
            expected.len(),
            actual.len(),
            "wrong instructions length.\nwant={}\ngot ={}",
            expected,
            actual
        );
        let expected: Vec<u8> = expected.into_iter().collect();
        let actual: Vec<u8> = actual.into_iter().collect();
        for (i, (ex, ac)) in expected.iter().zip(actual.iter()).enumerate() {
            assert_eq!(
                ex, ac,
                "wrong instruction at {}.\nwant={:?}\ngot ={:?}",
                i, expected, actual
            );
        }
    }

    fn concat_instructions(s: Vec<Instructions>) -> Instructions {
        s.into_iter().flatten().collect()
    }

    fn test_constants(expected: Vec<Constant>, actual: Vec<Object>) {
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
