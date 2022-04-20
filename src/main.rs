mod environment;
mod errors;
mod expression;
mod lex;
mod operations;
mod parser;
mod statement;
mod stdlib;
mod symbol_table;
mod types;

use environment::*;
use parser::*;
use stdlib::*;
use symbol_table::SymbolTable;
use types::*;

use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::fs;

const TRACE: bool = false;

pub struct Interpreter {
    pub had_error: bool,
    pub had_runtime_error: bool,
    pub had_type_error: bool,
    global_symbols: SymbolTable,
    global_env: environment::EnvRc,
}

impl Interpreter {
    pub fn init() -> Interpreter {
        let mut global_symbols = symbol_table::SymbolTable::global();
        let mut global_env = environment::new_global();
        Interpreter::add_standard_library(&mut global_symbols, &global_env);

        Interpreter {
            had_error: false,
            had_type_error: false,
            had_runtime_error: false,
            global_env,
            global_symbols,
        }
    }

    fn add_standard_library(symbols: &mut SymbolTable, envr: &environment::EnvRc) {
        // Add standard library functions
        let clock_func = ReturnValue::CallableValue(Box::new(ClockFunc {}));
        symbols.add_library_function(&clock_func);
        envr.define_callable(clock_func);
        let sqr_func = ReturnValue::CallableValue(Box::new(SqrFunc {}));
        symbols.add_library_function(&sqr_func);
        envr.define_callable(sqr_func);
    }

    pub fn run(&mut self, code: String) {
        let mut scanner = lex::Scanner::new(code);
        let tokens = scanner.tokenize();
        let mut parser = Parser::new(tokens);
        let statements = parser.parse(&mut self.global_symbols);
        statements
            .iter()
            .for_each(|stmt| match stmt.check_types(&self.global_symbols) {
                Err(type_error) => {
                    self.had_type_error = true;
                    eprintln!("{}", &type_error.format());
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
            let mut result = stmt.execute(&self.global_env);
            match result {
                Ok(_) => {}
                Err(early_return) => {
                    self.had_runtime_error = true;
                    if let statement::EarlyReturn::Error(msg) = early_return {
                        eprintln!("{}", &msg.format());
                    } else {
                        println!("{:?}", &early_return);
                    }
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


pub fn compile(code: &str) {
	let mut had_type_error = false;
	let mut global_symbols = symbol_table::SymbolTable::global();
	let mut scanner = lex::Scanner::new(code.to_string());
	let tokens = scanner.tokenize();
	let mut parser = Parser::new(tokens);
	let statements = parser.parse(&mut global_symbols);
	statements
		.iter()
		.for_each(|stmt| 
			match stmt.check_types(&global_symbols) {
				Err(type_error) => {
					had_type_error = true;
					eprintln!("{}", &type_error.format());
				}
				_ => {}
		});

	if had_type_error {
		eprintln!("Type errors in source code. Compilation terminated.");
		std::process::exit(3);
	}
	
	let mut had_compiler_error = false;
	
	let mut compiled_statements: Vec<String> = Vec::new();
	for stmt in &statements {
		match stmt.compile(&global_symbols) {
			Ok(code) => compiled_statements.push(code),
			Err(msg) => {
				eprintln!("{}",&msg.format());
				had_compiler_error = true;
			}
		};
	}
	
	let object_code = compiled_statements.join("\n");

    // TODO configure this path and make it a cache for object code. Also, have one sub-directory
    // per program and make the dir hidden with a "." name.
    let tmp_ir_path = "./tmp_ir.c";
    fs::write(tmp_ir_path, object_code).expect("Problem writing intermediate representation code.");
	
	if had_compiler_error {
		eprintln!("Error during compilation. Will not link or run.");
		std::process::exit(4);
	}
	
	// TODO Call tcc or gcc on the output source
    

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
		compile(&code);
		
		std::process::exit(0);
		
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
