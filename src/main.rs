mod errors;
mod expression;
mod lex;
mod parser;
mod statement;
mod symbol_table;
mod types;
use parser::*;
use std::fs;
use std::env;
use std::ffi::OsStr;
use std::ffi::OsString;

const TRACE: bool = false;
static mut had_compiler_error: bool = false;
const tmp_compiler_output_directory:&str = "./";
const target_directory: &str = "./";
const tcc_path: &str = "/usr/bin/tcc";
static mut cc_path: &str = "/usr/bin/cc";

pub fn compile(code: &str) -> String {
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

unsafe {    had_compiler_error = false;}

    let mut compiled_statements: Vec<String> = Vec::new();
    for stmt in &statements {
        match stmt.compile(&global_symbols) {
            Ok(code) => compiled_statements.push(code),
            Err(msg) => {
                eprintln!("{}", &msg.format());
                unsafe{had_compiler_error = true;}
            }
        };
    }
	compiled_statements.join("\n")
}

// TODO here is where we might react to configuration information such as where a project resides
// and how to automatically create temporary output directories etc.
fn  prepare_build(object_code: &str, tmp_build_dir: &str, program_name: &str)-> String {
	let tmp_ir_path = format!("{}/{}.c",tmp_build_dir, program_name);
    fs::write(&tmp_ir_path, object_code)
		.expect(&format!("Problem writing intermediate representation code to {}",&tmp_ir_path));
	tmp_ir_path
}


fn build(tmp_build_dir: &str, ir_src: &str, target_dir: &str, program_name: &str) -> bool   {    
    
	// TODO check for existence of target_dir first
	
	let target_bin = if !target_dir.ends_with("/") {
		OsString::from(format!("{}{}",target_dir, program_name))
	} else {
		OsString::from(format!("{}/{}",target_dir, program_name))
	};
		    
	let mut cmd = std::process::Command::new(tcc_path);
	cmd.arg(&ir_src);
	cmd.arg("-o");
	cmd.arg(&target_bin);
	match cmd.status() {
		Ok(status) => {
			if status.success() {
//					println!("{:?}",output);
			 } else {
				eprintln!("BUILD ERROR: {}",status);
				std::process::exit(1);
			 }
		},
		Err(error) => {
			eprintln!("BUILD ERROR: {}", &error);
			std::process::exit(1);
		}			
	}				    	
	true
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
		
	// TODO form a proper program name
	let program_name = "program";
    let object_code = compile(&code);
	let ir_src = prepare_build(&object_code, tmp_compiler_output_directory, program_name);
	if unsafe{had_compiler_error} {
        eprintln!("Error during compilation. Will not link or run.");
        std::process::exit(4);
    }
	if build(tmp_compiler_output_directory, &ir_src, target_directory, program_name) {
		println!("Success!");
		
		// TODO automatically run the binary with supplied args, like 'cargo run'
	} 	
	
}

#[cfg(test)]
mod tests {
	use crate::statement::Stmt;

	use super::*;
	
	const SRC_ENUMS:&str = "type Days = Enum(Monday Tuesday Wednesday)
							fun check_enum(d: Days): Bool {
								if d = Tuesday {
									return true
								}
								var another_day:Days = Wednesday
								if another_day = d {
									print another_day
								}
								if d = Monday {
									print \"A case of the Mondays\"
									return false
								}
								return false
							}
							
							{
								check_enum(Monday)
							}
							";

	const SRC_ENUMS2:&str = "type Days = Enum(Monday Tuesday Wednesday)
				type Weekends = Enum(Saturday Sunday)

				fun test_types(): Bool {
					var d: Days = Wednesday
					var e = Monday
					if e = Monday {
						print \"A case of the Mondays\"
					}
					e := Wednesday
					
					return d = Tuesday 
				}

				{
					test_types()
				}";

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
        assert!(parse(SRC_ENUMS2)?.len()>0);
        assert!(type_check(SRC_ENUMS2)? == ());
        Ok(())
	}
	
	#[test]
	fn test_enums_as_fn_args() -> Result<(), Vec<errors::Error>> {
        assert!(parse(SRC_ENUMS)?.len()>0);
        assert!(type_check(SRC_ENUMS)? == ());
        Ok(())
	}
	
	
}