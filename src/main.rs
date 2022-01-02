mod environment;
mod expression;
mod lex;
mod operations;
mod parser;
mod statement;
mod symbol_table;
mod types;

use environment::*;
use parser::*;
use symbol_table::SymbolTable;
use types::*;

use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::fs;

const TRACE: bool = true;

pub struct Interpreter {
    pub had_error: bool,
    pub had_runtime_error: bool,
    pub had_type_error: bool,
	global_symbols: SymbolTable,
	global_env: Environment,
	
}

impl Interpreter {

	pub fn init() ->Interpreter{
		let mut global_symbols = symbol_table::SymbolTable::global();
		let mut global_env = Environment::new();		
		Interpreter::add_standard_library(&mut global_symbols, &mut global_env);
		
		Interpreter {
            had_error: false,
            had_type_error: false,
            had_runtime_error: false,
			global_env,
			global_symbols,
        }
	}
	
    fn add_standard_library(symbols: &mut SymbolTable, envr: &mut Environment)  {
        
        // Add standard library functions
        let clock_func = ReturnValue::CallableValue(Box::new(ClockFunc {}));		
		symbols.add_library_function(&clock_func);
        envr.define_callable(clock_func);
		let sqr_func = ReturnValue::CallableValue(Box::new(SqrFunc {}));		
		symbols.add_library_function(&sqr_func);
        envr.define_callable(sqr_func);
		
		
    }

    pub fn run(&mut self,code: String,) {
        let mut scanner = lex::Scanner::new(code);
        let tokens = scanner.tokenize();
        let mut parser = Parser::new(tokens);
        let statements = parser.parse(&mut self.global_symbols);
        statements
            .iter()
            .for_each(|stmt| match stmt.check_types(&self.global_symbols) {
                Err(type_error) => {
                    self.had_type_error = true;
                    eprintln!("Type error:  {:?}", &type_error.message);
                }
                _ => {}
            });

        if TRACE {
            statements
                .iter()
                .for_each(|stmt| println!("{}", &stmt.print()));
        }

        if self.had_type_error {
            eprintln!("Cannot execute code with type errors.");
            self.had_runtime_error = true;
            return;
        }

        for mut stmt in statements {
            let mut result = stmt.execute(&mut self.global_env);
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
    	
    let mut interpreter = Interpreter::init();

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());

                interpreter.run(line);

                // Clear errors for the next input of the REPL
                interpreter.had_error = false;
                interpreter.had_type_error = false;
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
        

        let mut interpreter = Interpreter::init();
        interpreter.run(code);
		
        if interpreter.had_error {
            std::process::exit(65);
        }
        if interpreter.had_runtime_error {
            std::process::exit(70);
        }
    }
}
