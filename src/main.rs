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

use colored::Colorize;

const TRACE: bool = false;

struct Builder {
    had_compiler_error: bool,
    source_directory: std::path::PathBuf,
    tmp_compiler_output_directory: std::path::PathBuf,
    installation_location: std::path::PathBuf,
    standard_lib_source_directory: std::path::PathBuf,
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
        let standard_lib_directory = std::path::Path::new("ir").to_path_buf();
        if in_project {
            let src_path = std::path::Path::new("src").to_path_buf();
            Builder {
                had_compiler_error: false,
                tmp_compiler_output_directory: build_dir.join(cache_path),
                standard_lib_source_directory: standard_lib_directory,
                installation_location: Builder::find_installation(),
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
                standard_lib_source_directory: standard_lib_directory,
                installation_location: Builder::find_installation(),
                target_directory: build_dir.to_path_buf(),
                tcc_path: Builder::checked_tcc_path(),
                cc_path: Builder::checked_cc_path(),
            }
        }
    }

    fn is_windows() -> bool {
        "windows".to_string() == env::consts::OS
    }

    // Search out an env var or compiler on the path or else assume it's a git clone where we run from the root
    // dir and use "cargo run". In that case we use the "." as the installation directory; the "ir" directory is
    // relative to the installation dir. We always add the installation location as an include dir to the C
    // compiler, like "tcc -I." or "gcc -I." or "gcc -I/home/ccd/.rci_installation"
    pub fn find_installation() -> std::path::PathBuf {
        // TODO add installer and search for installation; for now default to expecting the git clone install
        std::path::Path::new(".").to_path_buf()
    }

    fn locate_program(name: &str) -> Option<std::path::PathBuf> {
        let output = std::process::Command::new("which").arg(name).output();
        if let Ok(result) = output {
            if result.status.success() {
                //
                let which_result = std::str::from_utf8(&result.stdout);
                match which_result {
                    Ok(res) => {
                        if res.trim().len() == 0 {
                            None
                        } else {
                            Some(std::path::Path::new(res.trim()).to_path_buf())
                        }
                    }
                    Err(msg) => panic!("Problem with 'which' result: {}", &msg),
                }
            } else {
                None
            }
        } else {
            None
        }
    }

    fn check_program_path(name: &str, default_location: &str) -> std::path::PathBuf {
        if Builder::is_windows() {
            panic!("Windows not supported yet.");
        } else {
        }
        let program_on_path = Builder::locate_program(name);
        let default = std::path::Path::new(default_location).to_path_buf();

        if let Some(on_path) = program_on_path {
            on_path
        } else {
            default
        }
    }

    // TODO Allow for a C compiler to be configured
    fn checked_cc_path() -> std::path::PathBuf {
        let cc_path = Builder::check_program_path("cc", "/usr/bin/cc");
        if cc_path.exists() {
            cc_path
        } else {
            eprintln!("");
            eprintln!("Build configuration error: No C compiler located. Add one to your path or to /usr/bin (default Linux installation.)");
            eprintln!("On Ubuntu or Debian Linux do 'sudo apt install build-essential' or consult your operating system documentation.");
            std::process::exit(1);
        }
    }

    fn checked_tcc_path() -> std::path::PathBuf {
        let tcc_path = Builder::check_program_path("tcc", "/usr/bin/tcc");
        if tcc_path.exists() {
            tcc_path
        } else {
            eprintln!("");
            eprintln!("Build configuration error: No TCC compiler located. Add one to your path or to /usr/bin (default Linux installation.)");
            eprintln!("On Ubuntu or Debian Linux do 'sudo apt install tcc' or consult your operating system documentation.");
            eprintln!("Or you can download TCC directly from http://download.savannah.gnu.org/releases/tinycc/");
            std::process::exit(1);
        }
    }

    pub fn read_source(src_full_path: &OsStr) -> String {
        match fs::read_to_string(src_full_path) {
            Ok(file_content) => file_content,
            Err(msg) => {
                eprintln!(
                    "Error reading file {}: {}",
                    src_full_path.to_string_lossy().bright_white(),
                    &msg
                );
                std::process::exit(1);
            }
        }
    }

    pub fn read_standard_lib_source(&self) -> String {
        let stdlib_src_full_path = self.standard_lib_source_directory.join("standard_lib.rci");
        read_source(&stdlib_src_full_path)
    }

    pub fn read_user_source(&self, program_filename: &OsStr) -> String {
        let src_full_path = self.source_directory.join(program_filename);
        read_source(&src_full_path)
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
                let msg = "Parse errors in source code. Compilation halted.".red();
                eprintln!("\n{}\n", &msg);
                std::process::exit(3);
            }
        };

        statements
            .iter()
            .for_each(|stmt| match stmt.check_types(&global_symbols) {
                Err(type_error) => {
                    had_type_error = true;
                    eprintln!("{}\n", &type_error.format());
                }
                _ => {}
            });

        if had_type_error {
            eprintln!(
                "\n{}\n",
                &"Type errors in source code. Compilation terminated.".red()
            );
            std::process::exit(3);
        }

        self.had_compiler_error = false;

        let mut compiled_statements: Vec<String> = Vec::new();
        for stmt in &statements {
            match stmt.compile(&global_symbols) {
                Ok(code) => compiled_statements.push(code),
                Err(msg) => {
                    eprintln!("{}\n", &msg.format());
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
                    "\nBuild error: Could not create directory '{}' needed to build: {}",
                    self.tmp_compiler_output_directory.to_str().unwrap(),
                    msg
                );
                std::process::exit(1);
            }
        }
        let tmp_output_filename = format!("{}.c", &program_name);
        let tmp_ir_path = self.tmp_compiler_output_directory.join(tmp_output_filename);

        let gc_support = self.standard_lib_source_directory.join("tgc.h");
        let use_gc = format!(
            "\n#include \"{}\" \n static tgc_t gc;\n",
            &gc_support.to_string_lossy()
        );

        let runtime_support = self
            .standard_lib_source_directory
            .join("compiler_support.h");
        let use_runtime = format!("#include \"{}\"\n", &runtime_support.to_string_lossy());

        let standard_lib_support = self.standard_lib_source_directory.join("standard_lib.h");
        let use_standard_lib =
            format!("#include \"{}\"\n", &standard_lib_support.to_string_lossy());

        let code_to_write = use_gc + &use_runtime + &use_standard_lib + &object_code;

        fs::write(&tmp_ir_path, code_to_write).expect(&format!(
            "\nBuild error: Problem writing intermediate representation code to {}",
            &tmp_ir_path.to_str().unwrap()
        ));
        tmp_ir_path
    }

    fn build(&self, ir_src: &std::path::Path, program_name: &str) -> bool {
        let include_path = format!("-I{}", self.installation_location.to_string_lossy());
        let standard_lib_src = self.standard_lib_source_directory.join("tgc.c");
        let target_bin = self.target_directory.join(program_name);
        let mut cmd = std::process::Command::new(&self.tcc_path);
        cmd.arg(include_path);
        cmd.arg(standard_lib_src);
        cmd.arg(ir_src);
        cmd.arg("-o");
        cmd.arg(&target_bin);
        if TRACE {
            println!("try to execute {:?}", &cmd);
        }
        match cmd.status() {
            Ok(status) => {
                if status.success() {
                    //					println!("{:?}",output);
                    let compiled_msg = format!(
                        "\n{} {}",
                        "[Compiled]".blue(),
                        &target_bin.to_string_lossy().bright_blue()
                    );
                    println!("{}", &compiled_msg);
                } else {
                    let failed_msg = format!("\nBUILD ERROR: {}", status).red();
                    eprintln!("{}", &failed_msg);

                    std::process::exit(1);
                }
            }
            Err(error) => {
                let failed_msg = format!("\nBUILD ERROR: {}", &error).red();
                eprintln!("{}", &failed_msg);
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
            eprintln!(
                "\nBuild error: Incomplete path to program file: {}",
                program_file
            );
            std::process::exit(1);
        }
    };

    let mut builder = Builder::new(src_dir.unwrap(), false);
    let user_code = builder.read_user_source(program_filename.unwrap());
    let standard_lib_code = builder.read_stdlib_source();
    let program_code = standard_lib_code + &user_code;

    let object_code = builder.compile(&program_code);
    let ir_src = builder.prepare_build(&object_code, program_name);
    if builder.had_compiler_error {
        eprintln!("\nError during compilation. Will not link or run.");
        std::process::exit(4);
    }

    if builder.build(&ir_src, program_name) {
        let success_msg = "Success!".bright_yellow();
        println!("{}", &success_msg);

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

    const SRC_RECORD_TYPE_INFERENCE: &str = "
        type Customer = Rec{name: Str, balance: Num}
        val cust  = Customer(name: \"abc\", balance: 0)
    {
        var c =  Customer(name: \"Joe Smith\", balance: 25)
    }
";

    const SRC_RECORD_TYPE_EXPLICIT: &str = "
        type Customer=Rec{name: Str, balance: Num}
        val cust: Customer = Customer(name: \"Joe Smith\", balance: 0)
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
    const SRC_RECORD_GETTER: &str = "
        type Address = Rec {street: Str,zip: Num,state: Str}            

        type PizzaOrder = Rec {
            thin_crust: Bool
            size: Num,
            vegetarian: Bool,
            price: Num
        }
        
        {
            var addr: Address = Address(street: \"2121 22 Street\",zip: 11111, state: \"AL\") 
            var street : Str = addr.street
            var zip: Num = addr.zip
            print street, zip

            val addr2 = Address(street: \"1234 Oak\", zip: 543210, state: \"AK\")
            print addr2.street, addr2.state, addr2.zip

            val pizza = PizzaOrder(vegetarian: true, thin_crust: false, price: 15.75, size: 14)
            val pi = 3.14159
            val radius = pizza.size / 2.0            
            val price_per_inch = pizza.price / (radius * pi) * (radius * pi) 
            print \"Price per inch: \", price_per_inch
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
        assert!(type_check(SRC_RECORD_RETURN)? == ());
        Ok(())
    }

    #[test]
    fn test_record_getter() -> Result<(), Vec<errors::Error>> {
        assert!(parse(SRC_RECORD_GETTER)?.len() > 0);
        assert!(type_check(SRC_RECORD_GETTER)? == ());
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
