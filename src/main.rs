mod environment;
mod expression;
mod lex;
mod operations;
mod parser;
mod statement;

use environment::*;
use parser::*;

use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::fs;

const TRACE: bool = true;

pub struct Interpreter {
    pub had_error: bool,
    pub had_runtime_error: bool,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            had_error: false,
            had_runtime_error: false,
        }
    }

    pub fn run(&mut self, global_env: &mut Environment, code: String) {
        let mut scanner = lex::Scanner::new(code);
        let tokens = scanner.tokenize();
        let mut parser = Parser::new(tokens);
        let statements = parser.parse();

        if TRACE {
            statements
                .iter()
                .for_each(|stmt| println!("{}", &stmt.print()));
        }

        for mut stmt in statements {
            let mut result = stmt.execute(global_env);
            match result {
                Ok(_) => {}
                Err(msg) => {
                    self.had_runtime_error = true;
                    eprintln!("{}", &msg.message);
                }
            }
        } // each statement
    }

    fn error(&mut self, line: i32, col: i32, message: String) {
        self.report(line, col, "".to_string(), message)
    }

    pub fn report(&mut self, line: i32, col: i32, location: String, message: String) {
        eprintln!("line {}, col {}, {}, {}", line, col, &location, &message);
        self.had_error = true
    }
} // impl interpreter

pub fn repl() {
    // `()` can be used when no completer is required
    let mut rl = Editor::<()>::new();

    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }

    // The environment for the duration of the REPL session
    let mut global_env = Environment::new();
    let mut interpreter = Interpreter::new();

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());

                interpreter.run(&mut global_env, line);
                interpreter.had_error = false;
                interpreter.had_runtime_error = false;

                //println!("=>  {}", &results);
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    rl.save_history("history.txt").unwrap();
}

fn main() {
    println!("Starting interpreter...");

    let args = std::env::args().collect::<Vec<String>>();
    if args.len() > 2 {
        println!("Usage: rci  [script]");
        std::process::exit(0);
    }

    if args.len() < 2 {
        repl();
    } else {
        let program_file = &args[1];
        let code = fs::read_to_string(program_file)
            .expect(&format!("File at {} unreadable.", program_file));
        let mut global_env = Environment::new();
        let mut interpreter = Interpreter::new();
        interpreter.run(&mut global_env, code);
        if interpreter.had_error {
            std::process::exit(65);
        }
        if interpreter.had_runtime_error {
            std::process::exit(70);
        }
    }
}
