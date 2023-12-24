#[cfg(feature = "repl")]
extern crate rustyline;

use monkey::evaluator::builtins::new_builtins;
use monkey::evaluator::env;
use monkey::evaluator::Evaluator;
use monkey::lexer::Lexer;
use monkey::parser::Parser;
use std::cell::RefCell;
use std::rc::Rc;

fn main() {
    let mut rl = rustyline::DefaultEditor::new().expect("should exist");

    let mut evaluator = Evaluator {
        env: Rc::new(RefCell::new(env::Env::from(new_builtins()))),
    };

    loop {
        match rl.readline(">> ") {
            Ok(line) => {
                rl.add_history_entry(&line);
                let mut lexer = Lexer::new(&line);
                let mut parser = Parser::new(lexer);
                let mut program = parser.parse();
                let errors = parser.get_errors();

                if errors.len() > 0 {
                    for err in errors {
                        println!("{:?}", err);
                    }
                    continue;
                }

                evaluator.define_macros(&mut program);
                evaluator.expand_macros(&mut program);
                if let Some(evaluated) = evaluator.eval(&program) {
                    println!("{}\n", evaluated);
                }
            }
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
