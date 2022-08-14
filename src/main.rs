mod errors;
mod expression;
mod lex;
mod parser;
mod statement;
mod symbol_table;
mod types;
use parser::*;
use std::fs;

const TRACE: bool = false;

pub fn compile(code: &str) {
    let mut had_type_error = false;
    let mut global_symbols = symbol_table::SymbolTable::global();
    let mut scanner = lex::Scanner::new(code.to_string());
    let tokens = scanner.tokenize();
    let mut ast = Parser::new(tokens);

    let statements = match ast.parse(&mut global_symbols) {
		Ok(stmts) => stmts,
		Err(parse_errors) => {
			eprintln!("Parse errors in source code. Compilation halted.");
			std::process::exit(3);
		}
	};
	
    statements
        .iter()
        .for_each(|stmt| match stmt.check_types(&global_symbols) {
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
                eprintln!("{}", &msg.format());
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
    let code =
        fs::read_to_string(program_file).expect(&format!("File at {} unreadable.", program_file));
    compile(&code);
}

#[cfg(test)]
mod tests {
	use crate::statement::Stmt;

use super::*;
	
	const SRC_ENUMS:&str = "type Days = Enum(Monday Tuesday Wednesday)\n
							fun check_enum(d: Days): Bool {\n
								if d = Tuesday {\n
									return true\n
								}\n
								var another_day: Days = Wednesday\n
								if another_day == d {\n
									print another_day\n
								}\n
								if d = Monday {\n
									print \"A case of the Mondays\"\n
									return false\n
								}\n
								return false\n 								
							}\n
							";


  pub fn parse(code:&str) -> Result<Vec<Stmt>,Vec<errors::Error>> {
        let mut global_symbols = symbol_table::SymbolTable::global();
		let mut scanner = lex::Scanner::new(code.to_string());
		let tokens = scanner.tokenize();
		let mut ast = Parser::new(tokens);		
		ast.parse(&mut global_symbols)
  }


  pub fn type_check(code:&str) -> Result<(),Vec<errors::Error>> {
    let mut type_errors:Vec<errors::Error> = Vec::new();
    let mut global_symbols = symbol_table::SymbolTable::global();
    let mut scanner = lex::Scanner::new(code.to_string());
    let tokens = scanner.tokenize();
    let mut ast = Parser::new(tokens);		
    let statements = ast.parse(&mut global_symbols)?;
    for s in statements {
        if let Err(type_error) = s.check_types(&global_symbols){
            type_errors.push(type_error);
        }

    }
    if type_errors.is_empty() { return Ok(())}
    Err(type_errors)
}

	#[test]
	fn test_enums() -> Result<(), Vec<errors::Error>> {
        assert!(parse(SRC_ENUMS)?.len()>0);
        assert!(type_check(SRC_ENUMS)? == ());
        Ok(())
	}
}