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
use std::fs;

use colored::Colorize;

const TRACE: bool = true;

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
        "windows" == env::consts::OS
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
                        if res.trim().is_empty() {
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
            eprintln!("\nBuild configuration error: No C compiler located. Add one to your path or to /usr/bin (default Linux installation.)");
            eprintln!("On Ubuntu or Debian Linux do 'sudo apt install build-essential' or consult your operating system documentation.");
            std::process::exit(1);
        }
    }

    fn checked_tcc_path() -> std::path::PathBuf {
        let tcc_path = Builder::check_program_path("tcc", "/usr/bin/tcc");
        if tcc_path.exists() {
            tcc_path
        } else {
            eprintln!("\nBuild configuration error: No TCC compiler located. Add one to your path or to /usr/bin (default Linux installation.)");
            eprintln!("On Ubuntu or Debian Linux do 'sudo apt install tcc' or consult your operating system documentation.");
            eprintln!("Or you can download TCC directly from http://download.savannah.gnu.org/releases/tinycc/");
            std::process::exit(1);
        }
    }

    pub fn read_source(src_full_path: &OsStr) -> String {
        if TRACE {
            println!("Read RCI source code from: {:?}", src_full_path);
        }
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
        Builder::read_source(stdlib_src_full_path.as_os_str())
    }

    pub fn read_user_source(&self, program_filename: &OsStr) -> String {
        let src_full_path = self.source_directory.join(program_filename);
        Builder::read_source(src_full_path.as_os_str())
    }

    //  Emits C, adds to symbol table
    // The main difference is that files that aren't the main program are modules
    // like with Rust., so we have to add a bit of source boiler-plate.
    pub fn compile_module(&mut self, input_file: &std::path::Path, global_symbols: &mut symbol_table::SymbolTable) -> String {
        "".to_string()

    }

    pub fn compile_source_file(&mut self, source_file: &std::path::Path, global_symbols: &mut symbol_table::SymbolTable) -> String {
        let mut had_type_error = false;
        let user_code = self.read_user_source(source_file.as_os_str());

        let mut scanner = lex::Scanner::new(user_code.to_string());
        let tokens = scanner.tokenize();
        let mut ast = Parser::new(tokens, Some(source_file));

        // We can catch errors from parsing and include a line of source code
        let statements = match ast.parse(global_symbols) {
            Ok(stmts) => stmts,
            Err(_parse_errors) => {
                let msg = "Parse errors in source code. Compilation halted.".red();
                eprintln!("\n{}\n", &msg);
                std::process::exit(3);

            }
        };
        
        statements.iter()
            .for_each(|stmt| 
                if let Err(type_error) = stmt.check_types(&global_symbols) {                
                    had_type_error = true;
                    eprintln!("{}\n", &type_error.format());                                
            });

        if had_type_error {
            eprintln!(
                "\n{}\n",
                &"Type errors in source code. Compilation terminated.".red()
            );
            std::process::exit(3);
        }
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

    
    pub fn compile_program(&mut self, main_file: &std::path::Path) -> String {        
        let mut global_symbols = symbol_table::SymbolTable::global();            
        self.had_compiler_error = false;

        let stdlib_src_full_path = self.standard_lib_source_directory.join("standard_lib.rci");
        let compiled_stdlib = self.compile_source_file(&stdlib_src_full_path, &mut global_symbols);
        let compiled_main = self.compile_source_file(main_file, &mut global_symbols);
        compiled_stdlib + &compiled_main



        
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

        let code_to_write = use_gc + &use_runtime + &use_standard_lib + object_code;

        fs::write(&tmp_ir_path, code_to_write).unwrap_or_else(|_| {
            panic!(
                "\nBuild error: Problem writing intermediate representation code to {}",
                &tmp_ir_path.to_str().unwrap()
            )
        });
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
                    //	println!("{:?}",output);
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
    let main_file = program_filename.expect("Couldn't fine file for main.");
    let object_code = builder.compile_program(&std::path::Path::new(main_file));
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
    const SRC_RECORD_DECL: &str = "type Customer = Rec { name: Str, id: Int, discount: Bool }
		{
			print \"Hello\"
		}
		
	";

    // Only test passing record type param, not using it
    const SRC_RECORD_PARAM: &str = "type Customer = Rec{ name: Str, id: Int, discount: Bool}
		
		fun display(cust: Customer): Str {
			return \"customer\"		
		}		
		{
			print display(c)
		}
	";

    const SRC_RECORD_TYPE_INFERENCE: &str = "
        type Customer = Rec{name: Str, balance: Int}
        val cust  = Customer(name: \"abc\", balance: 0)
    {
        var c =  Customer(name: \"Joe Smith\", balance: 25)
    }
";

    const SRC_RECORD_TYPE_EXPLICIT: &str = "
        type Customer=Rec{name: Str, balance: Int}
        val cust: Customer = Customer(name: \"Joe Smith\", balance: 0)
    {
        var c: Customer =  Customer(name: \"John Smith\", balance: 25)
    }
    ";

    const SRC_RECORD_RETURN: &str = "type Customer = Rec{name: Str, balance: Int}
		fun new_customer(n: Str): Customer {
			val cust = Customer(name: n, balance: 0)
			return cust
		}		
		{
			var c =  new_customer(\"Joe\")
		}
	";
    const SRC_RECORD_GETTER: &str = "
        type Address = Rec {street: Str,zip: Int,state: Str}            

        type PizzaOrder = Rec {
            thin_crust: Bool
            size: Flt
            vegetarian: Bool
            price: Flt
        }
        
        {
            var addr: Address = Address(street: \"2121 22 Street\",zip: 11111, state: \"AL\") 
            var street : Str = addr.street
            var zip: Int = addr.zip
            print street, zip

            val addr2 = Address(street: \"1234 Oak\", zip: 543210, state: \"AK\")
            print addr2.street, addr2.state, addr2.zip

            val pizza = PizzaOrder(vegetarian: true, thin_crust: false, price: 15.75, size: 14.0)
            val pi = 3.14159
            val radius = pizza.size / 2.0            
            val price_per_inch = pizza.price / (radius * pi) * (radius * pi) 
            print \"Price per inch: \", price_per_inch
        }        
    ";

    const SRC_ENUMS: &str = "
            type Days = Enum {Monday, Tuesday, Wednesday}
            fun check_enum(d: Days): Bool {
                if d = Tuesday {
                    return true
                }
                var another_day: Days = Wednesday
                if d = another_day {
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

    const SRC_ENUMS2: &str = "
                type Days = Enum {Monday, Tuesday, Wednesday}
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
    const SRC_MODULE: &str = "
            val global_val = 551
            module test {            
                val module_val = 1
                var module_var = \"hello\"
                
                fun module_fun(x: Bool): Bool {
                    return not x
                }
            }

            {
                val x: Int = test@module_val                
                val y: Int =  2 * test@module_val - 3
                print y,\"\\n\"
                val z:Bool = test@module_fun(true)

                val try_this = test@module_var
            }        
    ";

    const SRC_MODULE_TYPES: &str = "            
            module test {            
                type Color = Rec{
                    red: Int
                    green: Int
                    blue: Int
                }

                type Day = Enum {
                    Sun
                    Mon
                    Tue
                    Wed
                    Thu
                    Fri
                    Sat
                }
                
                val module_enum = Mon
                val module_rec = Color(red: 25,blue: 88, green: 115)
                
                fun module_fun(x: Color, d: Day): Bool {
                    val c = x
                    val today = d
                    print c.red,c.green,c.blue
                    if today = Wed {
                        return true
                    } else {
                        return false
                    }                    
                } // fun
            } // module

            {
                print \"hello\\n\"
                val d: test@Day = test@Mon
                if d = test@Tue {
                    print \"It's Tuesday!\\n\"
                }
                val m = test@Fri

            }        
    ";

    // Only the first type error on each code block is returned; the type checker fails after each
    // block failure (to be improved)
    // This code should return three errors: The type mismatch on math operators in main
    // and the function, and the failure to infer the LHS type in the funtion code.
    const SRC_INVALID_NUMERIC_OPERATIONS: &str ="
            val x: Int = 5
            val y: Flt = 8.0

            fun bad_type_combo(t1: Int, t2: Flt): Int {
                val r = t1 / t2
                return r
            }

            {
                val z: Int = x + y
                val a: Int = x * y

                print z,a

            }
    ";

    pub fn parse(code: &str) -> Result<Vec<Stmt>, Vec<errors::Error>> {
        let mut global_symbols = symbol_table::SymbolTable::global();
        let mut scanner = lex::Scanner::new(code.to_string());
        let tokens = scanner.tokenize();
        let mut ast = Parser::new(tokens, None);
        ast.parse(&mut global_symbols)
    }

    pub fn type_check(code: &str) -> Result<(), Vec<errors::Error>> {
        let mut type_errors: Vec<errors::Error> = Vec::new();
        let mut global_symbols = symbol_table::SymbolTable::global();
        let mut scanner = lex::Scanner::new(code.to_string());
        let tokens = scanner.tokenize();
        let mut ast = Parser::new(tokens, None);
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
    fn test_typecheck_incompatible_numeric_ops() {
        let results = type_check(SRC_INVALID_NUMERIC_OPERATIONS);
        assert!(results.is_err());
        
        if let Err(ref errors) = results {
            //println!("{:?}", errors);
            assert_eq!(3, errors.len());
            

        }


    }

    #[test]
    fn test_enums() -> Result<(), Vec<errors::Error>> {
        assert!(!parse(SRC_ENUMS2)?.is_empty());
        assert!(type_check(SRC_ENUMS2).is_ok());
        Ok(())
    }

    #[test]    
    fn test_decl_enums() -> Result<(), Vec<errors::Error>> {
        assert!(!parse(SRC_ENUMS3)?.is_empty());
        assert!(type_check(SRC_ENUMS3).is_ok());
        Ok(())
    }

    #[test]    
    fn test_enums_as_fn_args() -> Result<(), Vec<errors::Error>> {
        assert!(!parse(SRC_ENUMS)?.is_empty());
        assert!(type_check(SRC_ENUMS).is_ok());
        Ok(())
    }

    #[test]    
    fn test_record_decl() -> Result<(), Vec<errors::Error>> {
        assert!(!parse(SRC_RECORD_DECL)?.is_empty());
        assert!(type_check(SRC_RECORD_DECL).is_ok());
        Ok(())
    }

    #[test]    
    fn test_record_param() -> Result<(), Vec<errors::Error>> {
        assert!(!parse(SRC_RECORD_PARAM)?.is_empty());
        assert!(type_check(SRC_RECORD_PARAM).is_ok());
        Ok(())
    }

    #[test]    
    fn test_record_return() -> Result<(), Vec<errors::Error>> {
        assert!(!parse(SRC_RECORD_RETURN)?.is_empty());
        assert!(type_check(SRC_RECORD_RETURN).is_ok());
        Ok(())
    }

    #[test]    
    fn test_record_getter() -> Result<(), Vec<errors::Error>> {
        assert!(!parse(SRC_RECORD_GETTER)?.is_empty());
        assert!(type_check(SRC_RECORD_GETTER).is_ok());
        Ok(())
    }

    #[test]    
    fn test_record_type_inference() -> Result<(), Vec<errors::Error>> {
        assert!(!parse(SRC_RECORD_TYPE_INFERENCE)?.is_empty());
        assert!(type_check(SRC_RECORD_TYPE_INFERENCE).is_ok());
        Ok(())
    }

    #[test]    
    fn test_record_type_explicit() -> Result<(), Vec<errors::Error>> {
        assert!(!parse(SRC_RECORD_TYPE_EXPLICIT)?.is_empty());
        assert!(type_check(SRC_RECORD_TYPE_EXPLICIT).is_ok());
        Ok(())
    }

    #[test]    
    fn test_modules() -> Result<(), Vec<errors::Error>> {
        assert!(!parse(SRC_MODULE)?.is_empty());
        assert!(type_check(SRC_MODULE).is_ok());

        Ok(())
    }

    #[test]    
    fn test_module_types() -> Result<(), Vec<errors::Error>> {
        assert!(!parse(SRC_MODULE_TYPES)?.is_empty());
        assert!(type_check(SRC_MODULE_TYPES).is_ok());

        Ok(())
    }
}
