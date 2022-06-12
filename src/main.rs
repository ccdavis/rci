mod environment;
mod operations;
mod errors;
mod expression;
mod lex;
mod parser;
mod statement;
mod symbol_table;
mod types;

use parser::*;
use symbol_table::SymbolTable;
use types::*;

use std::fs;

const TRACE: bool = false;

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
	
	println!("Success!");
	
	// TODO Call tcc or gcc on the output source
    

}

fn main() {    
    let args = std::env::args().collect::<Vec<String>>();
    if args.len() != 2 {
        println!("Usage: rci  [source]");
        std::process::exit(0);
    }

	let program_file = &args[1];
	let code = fs::read_to_string(program_file)
		.expect(&format!("File at {} unreadable.", program_file));
	compile(&code);		
}
