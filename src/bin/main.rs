#[cfg(feature="repl")]
extern crate rustyline;

use monkey::lexer::Lexer;
use monkey::parser::Parser;
use monkey::evaluator::env;
use monkey::evaluator::Evaluator;

fn main() {

    let mut rl = rustyline::DefaultEditor::new().expect("should exist");

    let mut evaluator = Evaluator { env: env::Env::new() };

    loop {
        match rl.readline(">> ") {
            Ok(line) => {
                rl.add_history_entry(&line);
                let mut lexer = Lexer::new(&line);
                let mut parser = Parser::new(lexer);
                let program = parser.parse();

                evaluator.eval(&program)

            },
            Err(rustyline::error::ReadlineError::Interrupted) => {
                println!("\n bye~");
                break;
            }
            Err(rustyline::error::ReadlineError::Eof) => {
                println!("");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
            }
        }
    }
}