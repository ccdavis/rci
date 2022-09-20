mod errors;
mod expression;
mod lex;
mod parser;
mod statement;
mod symbol_table;
mod types;
use parser::*;
use std::env;
use std::ffi::OsStr;
use std::ffi::OsString;
use std::fs;
use std::path::PathBuf;

const TRACE: bool = true;

struct Builder {
    had_compiler_error: bool,
    source_directory: std::path::PathBuf,
    tmp_compiler_output_directory: std::path::PathBuf,
    target_directory: std::path::PathBuf,
    tcc_path: std::path::PathBuf,
    cc_path: std::path::PathBuf,
}
// TODO eventually this should read from external config
impl Builder {
    // If in a "project", we assume build_dir is the top level of the project structure
    // Otherwise it's the directory where the source files are.
    fn new(build_dir: &std::path::Path, in_project: bool) -> Self {
        let cache_path = std::path::Path::new(".rci_tmp_cache").to_path_buf();
        if in_project {
            let src_path = std::path::Path::new("src").to_path_buf();
            Builder {
                had_compiler_error: false,
                tmp_compiler_output_directory: build_dir.join(cache_path),
                source_directory: build_dir.join(src_path),
                target_directory: build_dir.join("target"),
                tcc_path: Builder::checked_tcc_path(),
                cc_path: Builder::checked_cc_path(),
            }
        } else {
            Builder {
                had_compiler_error: false,
                tmp_compiler_output_directory: build_dir.join(cache_path),
                source_directory: build_dir.to_path_buf(),
                target_directory: build_dir.to_path_buf(),
                tcc_path: Builder::checked_tcc_path(),
                cc_path: Builder::checked_cc_path(),
            }
        }
    }

    // TODO search out C compilers and prompt for installation if missing
    fn checked_cc_path() -> std::path::PathBuf {
        std::path::Path::new("/usr/bin/cc").to_path_buf()
    }

    fn checked_tcc_path() -> std::path::PathBuf {
        std::path::Path::new("/usr/bin/tcc").to_path_buf()
    }

    pub fn read_source(&self, program_filename: &OsStr) -> String {
        let src_full_path = self.source_directory.join(program_filename);

        let code = match fs::read_to_string(&src_full_path) {
            Ok(file_content) => file_content,
            Err(msg) => {
                eprintln!(
                    "Error reading file {}: {}",
                    &src_full_path.to_string_lossy(),
                    &msg
                );
                std::process::exit(1);
            }
        };

        code
    }

    pub fn compile(&mut self, code: &str) -> String {
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

        self.had_compiler_error = false;

        let mut compiled_statements: Vec<String> = Vec::new();
        for stmt in &statements {
            match stmt.compile(&global_symbols) {
                Ok(code) => compiled_statements.push(code),
                Err(msg) => {
                    eprintln!("{}", &msg.format());
                    self.had_compiler_error = true;
                }
            };
        }
        compiled_statements.join("\n")
    }

    fn prepare_build(&self, object_code: &str, program_name: &str) -> std::path::PathBuf {
        match std::fs::create_dir_all(&self.tmp_compiler_output_directory) {
            _ => (),
            Err(msg) => {
                eprintln!(
                    "Could not create directory '{}' needed to build: {}",
                    self.tmp_compiler_output_directory.to_str().unwrap(),
                    msg
                );
                std::process::exit(1);
            }
        }
        let tmp_output_filename = format!("{}.c", &program_name);
        let tmp_ir_path = self.tmp_compiler_output_directory.join(tmp_output_filename);

        fs::write(&tmp_ir_path, object_code).expect(&format!(
            "Problem writing intermediate representation code to {}",
            &tmp_ir_path.to_str().unwrap()
        ));
        tmp_ir_path
    }

    fn build(&self, ir_src: &std::path::Path, program_name: &str) -> bool {
        let target_bin = self.target_directory.join(program_name);
        let mut cmd = std::process::Command::new(&self.tcc_path);
        cmd.arg(ir_src);
        cmd.arg("-o");
        cmd.arg(&target_bin);
        match cmd.status() {
            Ok(status) => {
                if status.success() {
                    //					println!("{:?}",output);
                    println!("[Compiled] {}", &target_bin.to_string_lossy())
                } else {
                    eprintln!("BUILD ERROR: {}", status);
                    std::process::exit(1);
                }
            }
            Err(error) => {
                eprintln!("BUILD ERROR: {}", &error);
                std::process::exit(1);
            }
        }
        true
    }
}

fn main() {
    let args = std::env::args().collect::<Vec<String>>();
    if args.len() != 2 {
        println!("Usage: rci  [source]");
        std::process::exit(0);
    }

    let program_file = &args[1];
    let src_dir = std::path::Path::new(program_file).parent();
    let program_filename = std::path::Path::new(program_file).file_name();
    let program_name = match program_filename {
        Some(filename) => filename.to_str().unwrap().trim_end_matches(".rci"),
        None => {
            eprintln!("Incomplete path to program file: {}", program_file);
            std::process::exit(1);
        }
    };

    let mut builder = Builder::new(src_dir.unwrap(), false);
    let code = builder.read_source(program_filename.unwrap());
    let object_code = builder.compile(&code);
    let ir_src = builder.prepare_build(&object_code, program_name);
    if builder.had_compiler_error {
        eprintln!("Error during compilation. Will not link or run.");
        std::process::exit(4);
    }
    if builder.build(&ir_src, program_name) {
        println!("Success!");

        // TODO automatically run the binary with supplied args, like 'cargo run'
    }
}

#[cfg(test)]
mod tests {
    use crate::statement::Stmt;

    use super::*;

    // Only test declaring record type
    const SRC_RECORD_DECL: &str = "type Customer = Rec { name: Str, id: Num, discount: Bool }
		{
			print \"Hello\"
		}
		
	";

    // Only test passing record type param, not using it
    const SRC_RECORD_PARAM: &str = "type Customer = Rec{ name: Str, id: Num, discount: Bool}
		
		fun display(cust: Customer): Str {
			return \"customer\"		
		}		
		{
			print display(c)
		}
	";

    const SRC_RECORD_TYPE_INFERENCE: &str ="
        type Customer = Rec{name: Str, balance: Num}
        val cust  = Customer(name: \"abc\", balance: 0)
    {
        var c =  Customer(name: \"Joe Smith\", balance: 25)
    }
";

    const SRC_RECORD_TYPE_EXPLICIT: &str = "
        type Customer=Rec{name: Str, balance: Num}
        val cust: Customer = Customer(name: n, balance: 0)
    {
        var c: Customer =  Customer(name: \"John Smith\", balance: 25)
    }
    ";


    const SRC_RECORD_RETURN: &str = "type Customer = Rec{name: Str, balance: Num}
		fun new_customer(n: Str): Customer {
			val cust = Customer(name: n, balance: 0)
			return cust
		}		
		{
			var c =  new_customer(\"Joe\")
		}
	";

    const SRC_ENUMS: &str = "type Days = Enum {Monday, Tuesday, Wednesday}
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
							}";

    const SRC_ENUMS3: &str = " type Days = Enum {Monday, Tuesday, Wednesday}
        type Weekends = Enum {Saturday, Sunday}

        {
            var d = Monday
            print \"Day: \", d
        }";

    const SRC_ENUMS2: &str = "type Days = Enum {Monday, Tuesday, Wednesday}
				//type Weekends = Enum {Saturday, Sunday}

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

    pub fn parse(code: &str) -> Result<Vec<Stmt>, Vec<errors::Error>> {
        let mut global_symbols = symbol_table::SymbolTable::global();
        let mut scanner = lex::Scanner::new(code.to_string());
        let tokens = scanner.tokenize();
        let mut ast = Parser::new(tokens);
        ast.parse(&mut global_symbols)
    }

    pub fn type_check(code: &str) -> Result<(), Vec<errors::Error>> {
        let mut type_errors: Vec<errors::Error> = Vec::new();
        let mut global_symbols = symbol_table::SymbolTable::global();
        let mut scanner = lex::Scanner::new(code.to_string());
        let tokens = scanner.tokenize();
        let mut ast = Parser::new(tokens);
        let statements = ast.parse(&mut global_symbols)?;
        for s in statements {
            if let Err(type_error) = s.check_types(&global_symbols) {
                type_errors.push(type_error);
            }
        }
        if type_errors.is_empty() {
            return Ok(());
        }
        Err(type_errors)
    }

    #[test]
    fn test_enums() -> Result<(), Vec<errors::Error>> {
        assert!(parse(SRC_ENUMS2)?.len() > 0);
        assert!(type_check(SRC_ENUMS2)? == ());
        Ok(())
    }

    #[test]
    fn test_decl_enums() -> Result<(), Vec<errors::Error>> {
        assert!(parse(SRC_ENUMS3)?.len() > 0);
        assert!(type_check(SRC_ENUMS3)? == ());
        Ok(())
    }

    #[test]
    fn test_enums_as_fn_args() -> Result<(), Vec<errors::Error>> {
        assert!(parse(SRC_ENUMS)?.len() > 0);
        assert!(type_check(SRC_ENUMS)? == ());
        Ok(())
    }

    #[test]
    fn test_record_decl() -> Result<(), Vec<errors::Error>> {
        assert!(parse(SRC_RECORD_DECL)?.len() > 0);
        assert!(type_check(SRC_RECORD_DECL)? == ());
        Ok(())
    }

    #[test]
    fn test_record_param() -> Result<(), Vec<errors::Error>> {
        assert!(parse(SRC_RECORD_PARAM)?.len() > 0);
        assert!(type_check(SRC_RECORD_PARAM)? == ());
        Ok(())
    }

    #[test]
    fn test_record_return() -> Result<(), Vec<errors::Error>> {
        assert!(parse(SRC_RECORD_RETURN)?.len() > 0);
        //assert!(type_check(SRC_RECORD_RETURN)? == ());
        Ok(())
    }

    #[test]
    fn test_record_type_inference() -> Result<(), Vec<errors::Error>> {
        assert!(parse(SRC_RECORD_TYPE_INFERENCE)?.len() > 0);
        assert!(type_check(SRC_RECORD_TYPE_INFERENCE)? == ());
        Ok(())
    }

    #[test]
    fn test_record_type_explicit() -> Result<(), Vec<errors::Error>> {
        assert!(parse(SRC_RECORD_TYPE_EXPLICIT)?.len() > 0);
        assert!(type_check(SRC_RECORD_TYPE_EXPLICIT)? == ());
        Ok(())
    }

}
