mod lex;

use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::fs;
use std::process;

fn repl(){
	// `()` can be used when no completer is required
    let mut rl = Editor::<()>::new();
	
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }
	
	// The environment for the duration of the REPL session
	//let mut envr = interpreter::Environment::new();
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());								
				//let results = interpret(line, &mut envr);
				interpret(line);
                //println!("=>  {}", &results);
            },
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break
            },
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break
            },
            Err(err) => {
                println!("Error: {:?}", err);
                break
            }
        }
    }
    rl.save_history("history.txt").unwrap();	
	
}

fn interpret(code:String){
	let mut scanner = lex::Scanner::new(code);
	let tokens = scanner.tokenize();
	for t in &tokens{
		println!("{:?}",&t);
	}		
}

fn main() {
	println!("Starting interpreter...");
	
	let args = std::env::args().collect::<Vec<String>>();
	if args.len() > 2{
		println!("Usage: rci  [script]");
		std::process::exit(0);
	}
	
	if args.len()<2{
		repl();
	}else{
		let program_file = &args[1];
		let code = fs::read_to_string(program_file)
			.expect(&format!("File at {} unreadable.",program_file));
		interpret(code);		
	}
	    
}
