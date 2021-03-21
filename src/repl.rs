use crate::ast::Program;
use crate::compiler::Compiler;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::vm::VM;
use anyhow::Result;
use std::io;
use std::io::Write;

const PROMPT: &'static str = ">> ";

pub fn start() {
    fn parse_program(input: &str) -> Result<Program> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        parser.parse()
    }
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
        let mut compiler = Compiler::new();
        let result = parse_program(&input)
            .or_else(|err| Err(format!("parse error:\n {:?}", err)))
            .and_then(|program| {
                compiler
                    .compile(program)
                    .map(|()| compiler.bytecode())
                    .or_else(|err| Err(format!("compilation failed:\n {:?}", err)))
            })
            .and_then(|bytecode| {
                let mut machine = VM::new(&bytecode);
                machine
                    .run()
                    .map(|()| machine.stack_top().unwrap().clone())
                    .or_else(|err| Err(format!("executing bytecode failed:\n {:?}", err)))
            });
        match result {
            Ok(stack_top) => {
                println!("{:?}", stack_top);
            }
            Err(err) => {
                println!("{}", err);
            }
        }
    }
}
