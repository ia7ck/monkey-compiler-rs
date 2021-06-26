use crate::ast::Program;
use crate::compiler::Compiler;
use crate::lexer::Lexer;
use crate::object::Object;
use crate::parser::Parser;
use crate::symbol_table::SymbolTable;
use crate::vm::VM;
use anyhow::Result;
use std::io;
use std::io::Write;

const PROMPT: &str = ">> ";

pub fn start() {
    fn parse_program(input: &str) -> Result<Program> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        parser.parse()
    }

    let mut constants = Vec::new();
    let mut globals = vec![Object::Dummy; 65535];
    let mut symbol_table = SymbolTable::new();
    symbol_table.define_builtins();

    loop {
        print!("{}", PROMPT);
        io::stdout().flush().unwrap();
        let mut input = String::new();
        let read = io::stdin().read_line(&mut input).unwrap();
        if read == 0 {
            // EOF
            return;
        }
        let input = input.trim_end();
        let result = parse_program(&input)
            .map_err(|err| format!("parse error:\n {:?}", err))
            .and_then(|program| {
                let mut compiler = Compiler::new_with_state(&symbol_table, &constants);
                let result = compiler.compile(program);
                constants = compiler.constants().clone();
                symbol_table = compiler.symbol_table().clone();
                result
                    .map_err(|err| format!("compilation failed:\n {:?}", err))
                    .map(|()| compiler.bytecode())
            })
            .and_then(|bytecode| {
                let mut machine = VM::new_with_global_store(bytecode, &globals);
                let result = machine
                    .run()
                    .and_then(|()| Ok(machine.last_popped_stack_elem()))
                    .map_err(|err| format!("executing bytecode failed:\n {:?}", err));
                globals = machine.globals();
                result
            });
        match result {
            Ok(stack_top) => {
                println!("{}", stack_top);
            }
            Err(err) => {
                println!("{}", err);
            }
        }
    }
}
