use monkey_compiler::compiler::Compiler;
use monkey_compiler::lexer::Lexer;
use monkey_compiler::parser::Parser;
use monkey_compiler::vm::VM;
use std::time::Instant;

fn main() {
    let input = "
        let fib = fn(n) {
            if (n == 0) {
                return 0;
            } else {
                if (n == 1) {
                    return 1;
                } else {
                    return fib(n - 1) + fib(n - 2);
                }
            }
        };
        fib(35);
    ";
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
