use crate::ast::Node;
use crate::ast::Operator;
use crate::compiler::Compiler;
use crate::vm::VM;
use anyhow::{bail, Result};
use std::io;
use std::io::Write;

const PROMPT: &'static str = ">> ";

pub fn start() {
    fn parse_program(input: &str) -> Result<Node> {
        use Node::*;
        use Operator::*;
        if input == "1" {
            return Ok(IntegerLiteral { value: 1 });
        }
        if input == "2" {
            return Ok(IntegerLiteral { value: 2 });
        }
        if input == "1 + 2" {
            return Ok(InfixExpression {
                left: Box::new(IntegerLiteral { value: 1 }),
                operator: Plus,
                right: Box::new(IntegerLiteral { value: 2 }),
            });
        }
        bail!("not implemented yet")
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
