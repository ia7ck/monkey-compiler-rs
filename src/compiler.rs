use crate::ast::{Expression, InfixOperator, PrefixOperator, Program, Statement};
use crate::code::{make, Instructions, Opcode, DEFINITIONS};
use crate::object;
use crate::object::Object;
use crate::symbol_table::{SymbolScope, SymbolTable};
use anyhow::{bail, Result};
use std::borrow::Borrow;
use std::rc::Rc;

pub struct Compiler {
    constants: Vec<Object>,

    symbol_table: SymbolTable,

    scopes: Vec<CompilationScope>,
    scope_index: usize,
}

#[derive(Copy, Clone)]
struct EmittedInstruction {
    opcode: Opcode,
    position: usize,
}

struct CompilationScope {
    instructions: Instructions,
    last_instruction: Option<EmittedInstruction>,
    previous_instruction: Option<EmittedInstruction>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            constants: vec![],
            symbol_table: SymbolTable::new(),
            scopes: vec![CompilationScope {
                instructions: Instructions::new(),
                last_instruction: None,
                previous_instruction: None,
            }],
            scope_index: 0,
        }
    }
    pub fn new_with_state(symbol_table: &SymbolTable, constants: &[Object]) -> Self {
        let mut compiler = Self::new();
        compiler.symbol_table = symbol_table.clone();
        compiler.constants = constants.to_vec();
        compiler
    }
    pub fn compile(&mut self, program: Program) -> Result<()> {
        for stmt in program.statements() {
            self.compile_statement(stmt)?;
        }
        Ok(())
    }
    fn compile_statement(&mut self, statement: Statement) -> Result<()> {
        use Statement::*;
        match statement {
            LetStatement { name, value } => {
                self.compile_expression(value)?;
                let symbol = self.symbol_table.define(&name);
                match symbol.scope() {
                    SymbolScope::GlobalScope => {
                        self.emit(Opcode::OpSetGlobal, &[symbol.index()]);
                    }
                    SymbolScope::LocalScope => {
                        self.emit(Opcode::OpSetLocal, &[symbol.index()]);
                    }
                }
            }
            ReturnStatement(return_value) => {
                self.compile_expression(return_value)?;
                self.emit(Opcode::OpReturnValue, &[]);
            }
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
            IntegerLiteral(value) => {
                let integer = Integer(value);
                let operands = &[self.add_constant(integer)];
                self.emit(OpConstant, operands);
            }
            Expression::Boolean(value) => {
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
                if self.last_instruction_is(OpPop) {
                    self.remove_last_pop();
                }

                let jump_pos = self.emit(OpJump, &[9999]);

                let after_consequence_pos = self.current_instructions().len();
                self.change_operand(jump_not_truthy_pos, after_consequence_pos);

                match alternative {
                    None => {
                        self.emit(OpNull, &[]);
                    }
                    Some(alt) => {
                        self.compile_statement(*alt)?;
                        if self.last_instruction_is(OpPop) {
                            self.remove_last_pop();
                        }
                    }
                }

                let after_alternative_pos = self.current_instructions().len();
                self.change_operand(jump_pos, after_alternative_pos);
            }
            Identifier(name) => {
                if let Some(symbol) = self.symbol_table.borrow().resolve(&name) {
                    let index = symbol.index();
                    match symbol.scope() {
                        SymbolScope::GlobalScope => {
                            self.emit(Opcode::OpGetGlobal, &[index]);
                        }
                        SymbolScope::LocalScope => {
                            self.emit(Opcode::OpGetLocal, &[index]);
                        }
                    }
                } else {
                    bail!("undefined variable {}", name);
                }
            }
            StringLiteral(value) => {
                let string = MonkeyString(value);
                let operands = &[self.add_constant(string)];
                self.emit(OpConstant, operands);
            }
            ArrayLiteral(elements) => {
                let len = elements.len();
                for e in elements {
                    self.compile_expression(e)?;
                }
                self.emit(OpArray, &[len]);
            }
            HashLiteral(pairs) => {
                let len = pairs.len();
                for (key, value) in pairs {
                    self.compile_expression(key)?;
                    self.compile_expression(value)?;
                }
                self.emit(OpHash, &[len]);
            }
            IndexExpression { left, index } => {
                self.compile_expression(*left)?;
                self.compile_expression(*index)?;
                self.emit(OpIndex, &[]);
            }
            FunctionLiteral { parameters, body } => {
                self.enter_scope();

                let num_parameters = parameters.len();

                for param in parameters {
                    match param {
                        Identifier(ident) => {
                            self.symbol_table.define(&ident);
                        }
                        _ => unreachable!(),
                    }
                }

                self.compile_statement(*body)?;

                if self.last_instruction_is(OpPop) {
                    self.replace_last_pop_with_return();
                }
                if !self.last_instruction_is(OpReturnValue) {
                    self.emit(OpReturn, &[]);
                }

                let num_locals = self.symbol_table.num_definitions();
                let instructions = self.leave_scope();
                let compiled_function = CompiledFunctionObject(Rc::new(
                    object::CompiledFunctionObject::new(instructions, num_locals, num_parameters),
                ));
                let operands = &[self.add_constant(compiled_function)];
                self.emit(OpConstant, operands);
            }
            CallExpression {
                function,
                arguments,
            } => {
                self.compile_expression(*function)?;

                let number_of_arguments = arguments.len();

                for arg in arguments {
                    self.compile_expression(arg)?;
                }

                self.emit(OpCall, &[number_of_arguments]);
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
        let pos_new_instruction = self.current_instructions().len();
        self.current_instructions_mut().extend(instruction);
        pos_new_instruction
    }
    fn set_last_instruction(&mut self, op: Opcode, pos: usize) {
        let last = EmittedInstruction {
            opcode: op,
            position: pos,
        };
        // self.prev <- self.last
        // self.last <- last
        self.scopes[self.scope_index].previous_instruction =
            self.scopes[self.scope_index].last_instruction;
        self.scopes[self.scope_index].last_instruction = Some(last);
    }
    fn last_instruction_is(&self, op: Opcode) -> bool {
        match &self.scopes[self.scope_index].last_instruction {
            Some(last) => last.opcode == op,
            None => false,
        }
    }
    fn remove_last_pop(&mut self) {
        let last_position = self.last_instruction().position;
        self.current_instructions_mut().truncate(last_position);
        // self.last <- self.prev
        self.scopes[self.scope_index].last_instruction =
            self.scopes[self.scope_index].previous_instruction;
        self.scopes[self.scope_index].previous_instruction = None;
    }
    fn replace_instruction(&mut self, pos: usize, new_instruction: Instructions) {
        let instructions = self.current_instructions_mut();
        for i in 0..new_instruction.len() {
            instructions[pos + i] = new_instruction[i];
        }
    }
    fn change_operand(&mut self, op_pos: usize, operand: usize) {
        let def = &DEFINITIONS[self.current_instructions()[op_pos] as usize];
        let new_instruction = make(def.opcode(), &[operand]);
        self.replace_instruction(op_pos, new_instruction);
    }
    fn current_instructions(&self) -> &Instructions {
        &self.scopes[self.scope_index].instructions
    }
    fn current_instructions_mut(&mut self) -> &mut Instructions {
        &mut self.scopes[self.scope_index].instructions
    }
    fn enter_scope(&mut self) {
        let scope = CompilationScope {
            instructions: Instructions::new(),
            last_instruction: None,
            previous_instruction: None,
        };
        self.scopes.push(scope);
        self.scope_index += 1;
        let outer = Box::new(self.symbol_table.clone());
        self.symbol_table = SymbolTable::new_enclosed_symbol_table(outer);
    }
    fn leave_scope(&mut self) -> Instructions {
        let scope = self.scopes.pop();
        self.scope_index -= 1;
        let outer = self.symbol_table.outer().unwrap().clone();
        self.symbol_table = outer;
        scope.unwrap().instructions
    }
    fn replace_last_pop_with_return(&mut self) {
        let last_position = self.last_instruction().position;
        self.replace_instruction(last_position, make(Opcode::OpReturnValue, &[]));
        self.last_instruction_mut().opcode = Opcode::OpReturnValue;
    }
    fn last_instruction(&self) -> &EmittedInstruction {
        self.scopes[self.scope_index]
            .last_instruction
            .as_ref()
            .unwrap_or_else(|| panic!("there is no last emitted instruction"))
    }
    fn last_instruction_mut(&mut self) -> &mut EmittedInstruction {
        self.scopes[self.scope_index]
            .last_instruction
            .as_mut()
            .unwrap_or_else(|| panic!("there is no last emitted instruction"))
    }
    pub fn constants(&self) -> &Vec<Object> {
        &self.constants
    }
    pub fn symbol_table(&self) -> &SymbolTable {
        &self.symbol_table
    }
    pub fn bytecode(self) -> Bytecode {
        Bytecode {
            instructions: self.current_instructions().clone(),
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

    enum Constant {
        Integer(i64),
        MonkeyString(&'static str),
        Function(Vec<Instructions>),
    }
    struct CompilerTestCase {
        input: &'static str,
        expected_constants: Vec<Constant>,
        expected_instructions: Vec<Instructions>,
    }

    #[test]
    fn test_compiler_scopes() {
        use Opcode::*;

        let mut compiler = Compiler::new();
        assert_eq!(compiler.scope_index, 0);

        compiler.emit(OpMul, &[]);

        compiler.enter_scope();
        assert_eq!(compiler.scope_index, 1);

        compiler.emit(OpSub, &[]);
        assert_eq!(compiler.current_instructions().len(), 1);

        let last = compiler.scopes[compiler.scope_index]
            .last_instruction
            .unwrap();
        assert_eq!(last.opcode, OpSub);

        compiler.leave_scope();
        assert_eq!(compiler.scope_index, 0);

        compiler.emit(OpAdd, &[]);
        let last = compiler.scopes[compiler.scope_index]
            .last_instruction
            .unwrap();
        assert_eq!(last.opcode, OpAdd);
        let previous = compiler.scopes[compiler.scope_index]
            .previous_instruction
            .unwrap();
        assert_eq!(previous.opcode, OpMul);
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

    #[test]
    fn test_global_let_statements() {
        use Constant::*;
        use Opcode::*;
        let tests = vec![
            CompilerTestCase {
                input: "let three = 3; let four = 4;",
                expected_constants: vec![Integer(3), Integer(4)],
                expected_instructions: vec![
                    make(OpConstant, &[0]),
                    make(OpSetGlobal, &[0]),
                    make(OpConstant, &[1]),
                    make(OpSetGlobal, &[1]),
                ],
            },
            CompilerTestCase {
                input: "let three = 3; three;",
                expected_constants: vec![Integer(3)],
                expected_instructions: vec![
                    make(OpConstant, &[0]),
                    make(OpSetGlobal, &[0]),
                    make(OpGetGlobal, &[0]),
                    make(OpPop, &[]),
                ],
            },
            CompilerTestCase {
                input: "let three = 3; let t = three; t;",
                expected_constants: vec![Integer(3)],
                expected_instructions: vec![
                    make(OpConstant, &[0]),
                    make(OpSetGlobal, &[0]),
                    make(OpGetGlobal, &[0]),
                    make(OpSetGlobal, &[1]),
                    make(OpGetGlobal, &[1]),
                    make(OpPop, &[]),
                ],
            },
        ];
        run_compiler_tests(tests);
    }

    #[test]
    fn test_string_expressions() {
        use Constant::*;
        use Opcode::*;
        let tests = vec![
            CompilerTestCase {
                input: r#"  "monkey"  "#,
                expected_constants: vec![MonkeyString("monkey")],
                expected_instructions: vec![make(OpConstant, &[0]), make(OpPop, &[])],
            },
            CompilerTestCase {
                input: r#"  "mon" + "key"  "#,
                expected_constants: vec![MonkeyString("mon"), MonkeyString("key")],
                expected_instructions: vec![
                    make(OpConstant, &[0]),
                    make(OpConstant, &[1]),
                    make(OpAdd, &[]),
                    make(OpPop, &[]),
                ],
            },
        ];
        run_compiler_tests(tests);
    }

    #[test]
    fn test_array_literals() {
        use Constant::*;
        use Opcode::*;
        let tests = vec![
            CompilerTestCase {
                input: "[]",
                expected_constants: vec![],
                expected_instructions: vec![make(OpArray, &[0]), make(OpPop, &[])],
            },
            CompilerTestCase {
                input: "[11, 22 + 33]",
                expected_constants: vec![Integer(11), Integer(22), Integer(33)],
                expected_instructions: vec![
                    make(OpConstant, &[0]), // 11
                    make(OpConstant, &[1]), // 22
                    make(OpConstant, &[2]), // 33
                    make(OpAdd, &[]),       // +
                    make(OpArray, &[2]),
                    make(OpPop, &[]),
                ],
            },
        ];
        run_compiler_tests(tests);
    }

    #[test]
    fn test_hash_literals() {
        use Constant::*;
        use Opcode::*;
        let tests = vec![
            CompilerTestCase {
                input: "{}",
                expected_constants: vec![],
                expected_instructions: vec![make(OpHash, &[0]), make(OpPop, &[])],
            },
            CompilerTestCase {
                input: "{12: 3, 4 + 56: 78 * 9}",
                expected_constants: vec![
                    Integer(12),
                    Integer(3),
                    Integer(4),
                    Integer(56),
                    Integer(78),
                    Integer(9),
                ],
                expected_instructions: vec![
                    make(OpConstant, &[0]), // 12
                    make(OpConstant, &[1]), // 3
                    make(OpConstant, &[2]), // 4
                    make(OpConstant, &[3]), // 56
                    make(OpAdd, &[]),       // +
                    make(OpConstant, &[4]), // 78
                    make(OpConstant, &[5]), // 9
                    make(OpMul, &[]),       // *
                    make(OpHash, &[2]),
                    make(OpPop, &[]),
                ],
            },
        ];
        run_compiler_tests(tests);
    }

    #[test]
    fn test_index_expressions() {
        use Constant::*;
        use Opcode::*;
        let tests = vec![CompilerTestCase {
            input: "[11, 22, 33][44 + 55]",
            expected_constants: vec![
                Integer(11),
                Integer(22),
                Integer(33),
                Integer(44),
                Integer(55),
            ],
            expected_instructions: vec![
                make(OpConstant, &[0]), // 11
                make(OpConstant, &[1]), // 22
                make(OpConstant, &[2]), // 33
                make(OpArray, &[3]),
                make(OpConstant, &[3]), // 44
                make(OpConstant, &[4]), // 55
                make(OpAdd, &[]),
                make(OpIndex, &[]),
                make(OpPop, &[]),
            ],
        }];
        run_compiler_tests(tests);
    }

    #[test]
    fn test_functions() {
        use Constant::*;
        use Opcode::*;
        let tests = vec![
            CompilerTestCase {
                input: "fn() { return 5 + 10; }",
                expected_constants: vec![
                    Integer(5),
                    Integer(10),
                    Function(vec![
                        make(OpConstant, &[0]),
                        make(OpConstant, &[1]),
                        make(OpAdd, &[]),
                        make(OpReturnValue, &[]),
                    ]),
                ],
                expected_instructions: vec![
                    make(OpConstant, &[2]), // function
                    make(OpPop, &[]),
                ],
            },
            CompilerTestCase {
                input: "fn() { 5 + 10 }",
                expected_constants: vec![
                    Integer(5),
                    Integer(10),
                    Function(vec![
                        make(OpConstant, &[0]),
                        make(OpConstant, &[1]),
                        make(OpAdd, &[]),
                        make(OpReturnValue, &[]),
                    ]),
                ],
                expected_instructions: vec![make(OpConstant, &[2]), make(OpPop, &[])],
            },
            CompilerTestCase {
                input: "fn() { 11; 22 }",
                expected_constants: vec![
                    Integer(11),
                    Integer(22),
                    Function(vec![
                        make(OpConstant, &[0]),
                        make(OpPop, &[]),
                        make(OpConstant, &[1]),
                        make(OpReturnValue, &[]),
                    ]),
                ],
                expected_instructions: vec![make(OpConstant, &[2]), make(OpPop, &[])],
            },
            CompilerTestCase {
                input: "fn() { }",
                expected_constants: vec![Function(vec![make(OpReturn, &[])])],
                expected_instructions: vec![make(OpConstant, &[0]), make(OpPop, &[])],
            },
        ];
        run_compiler_tests(tests);
    }

    #[test]
    fn test_function_calls() {
        use Constant::*;
        use Opcode::*;
        let tests = vec![
            CompilerTestCase {
                input: "fn() { 42; }();",
                expected_constants: vec![
                    Integer(42),
                    Function(vec![make(OpConstant, &[0]), make(OpReturnValue, &[])]),
                ],
                expected_instructions: vec![
                    make(OpConstant, &[1]),
                    make(OpCall, &[0]),
                    make(OpPop, &[]),
                ],
            },
            CompilerTestCase {
                input: "let answer = fn() { 42; }; answer();",
                expected_constants: vec![
                    Integer(42),
                    Function(vec![make(OpConstant, &[0]), make(OpReturnValue, &[])]),
                ],
                expected_instructions: vec![
                    make(OpConstant, &[1]),
                    make(OpSetGlobal, &[0]),
                    make(OpGetGlobal, &[0]),
                    make(OpCall, &[0]),
                    make(OpPop, &[]),
                ],
            },
            CompilerTestCase {
                input: "let oneArg = fn(a) { a }; oneArg(42);",
                expected_constants: vec![
                    Function(vec![make(OpGetLocal, &[0]), make(OpReturnValue, &[])]),
                    Integer(42),
                ],
                expected_instructions: vec![
                    make(OpConstant, &[0]),
                    make(OpSetGlobal, &[]),
                    make(OpGetGlobal, &[]),
                    make(OpConstant, &[1]),
                    make(OpCall, &[1]),
                    make(OpPop, &[]),
                ],
            },
            CompilerTestCase {
                input: "let manyArg = fn(a, b, c) { a; b; c }; manyArg(11, 22, 33);",
                expected_constants: vec![
                    Function(vec![
                        make(OpGetLocal, &[0]),
                        make(OpPop, &[]),
                        make(OpGetLocal, &[1]),
                        make(OpPop, &[]),
                        make(OpGetLocal, &[2]),
                        make(OpReturnValue, &[]),
                    ]),
                    Integer(11),
                    Integer(22),
                    Integer(33),
                ],
                expected_instructions: vec![
                    make(OpConstant, &[0]),
                    make(OpSetGlobal, &[]),
                    make(OpGetGlobal, &[]),
                    make(OpConstant, &[1]),
                    make(OpConstant, &[2]),
                    make(OpConstant, &[3]),
                    make(OpCall, &[3]), // a, b, c
                    make(OpPop, &[]),
                ],
            },
        ];
        run_compiler_tests(tests);
    }

    #[test]
    fn test_let_statement_scopes() {
        use Constant::*;
        use Opcode::*;
        let tests = vec![
            CompilerTestCase {
                input: "let num = 42; fn() { num; }",
                expected_constants: vec![
                    Integer(42),
                    Function(vec![make(OpGetGlobal, &[0]), make(OpReturnValue, &[])]),
                ],
                expected_instructions: vec![
                    make(OpConstant, &[0]),
                    make(OpSetGlobal, &[0]),
                    make(OpConstant, &[1]),
                    make(OpPop, &[]),
                ],
            },
            CompilerTestCase {
                input: "fn() { let num = 42; num; }",
                expected_constants: vec![
                    Integer(42),
                    Function(vec![
                        make(OpConstant, &[0]), // 42
                        make(OpSetLocal, &[0]),
                        make(OpGetLocal, &[0]),
                        make(OpReturnValue, &[]),
                    ]),
                ],
                expected_instructions: vec![
                    make(OpConstant, &[1]), // fn() { ... }
                    make(OpPop, &[]),
                ],
            },
        ];
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
        assert_eq!(expected.len(), actual.len(), "wrong number of constants.",);
        for (i, (expected, actual)) in expected.into_iter().zip(actual.into_iter()).enumerate() {
            match (expected, actual) {
                (Constant::Integer(v1), Object::Integer(v2)) => {
                    assert_eq!(
                        v1, v2,
                        "constant {} - Integer object has wrong value. want={}, got={}",
                        i, v1, v2
                    );
                }
                (Constant::Integer(..), obj) => {
                    panic!(
                        "constant {} - object is not Integer. got={}",
                        i,
                        obj.r#type()
                    );
                }
                (Constant::MonkeyString(v1), Object::MonkeyString(v2)) => {
                    assert_eq!(
                        v1, v2,
                        "constant {} - String object has wrong value. want={}, got={}",
                        i, v1, v2
                    );
                }
                (Constant::MonkeyString(..), obj) => {
                    panic!(
                        "constant {} - object is not String. got={}",
                        i,
                        obj.r#type()
                    );
                }
                (Constant::Function(ins1), Object::CompiledFunctionObject(func)) => {
                    test_instructions(ins1, func.instructions().clone());
                }
                (Constant::Function(..), obj) => {
                    panic!(
                        "constant {} - object is not Function. got={}",
                        i,
                        obj.r#type()
                    );
                }
            }
        }
    }
}
