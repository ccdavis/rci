
use core::str::Chars;
#[derive(Clone,Debug)]
pub enum TokenType{	
	  LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE,
	COMMA, DOT, MINUS, PLUS, SEMICOLON, SLASH, STAR,COLON,
	  // One or two character tokens.
	BANG, BANG_EQUAL,
	COLON_EQUAL, EQUAL,
	GREATER, GREATER_EQUAL,
	LESS, LESS_EQUAL,

	// literals
	NUMBER(f64), IDENTIFIER(String)	, STRING(String),
	
	// keywords	  
	AND, CLASS, ELSE, FALSE, FUN, FOR, IF, NIL, OR,
	PRINT, RETURN, SUPER, THIS, TRUE, VAR, WHILE,

	ERROR(String),
	EOF,
}


#[derive(Clone,Debug)]
pub struct Token{
	token_type:TokenType,
	start:usize,
	current : usize,
	line:usize,
	column:usize,
}

impl Token{

	pub fn print(&self)->String{
		format!("{:?}",&self.token_type)
	}
}
#[derive(Clone,Debug)]
pub struct Scanner {		
	text:String,
	finished:bool,
	start: usize,
	current: usize,
	line:usize,
	column:usize,
}

impl Scanner{

	pub fn new(script:String)->Self{	
		Self {text: script, finished:false, start:0, current:0, line:1, column:0}
	}
	
	// place-holder
	fn error(&self, msg:String){
		eprintln!("{}",&msg)
	}
	
	fn is_finished(&self)->bool{
		self.finished
	}
	
	pub fn tokenize(&mut self)->Vec<Token>{
		let mut tokens:Vec<Token> = Vec::new();

		// The chars() iterator borrows the string data, but string is mutable 
		// and so is 'self' here. So we can't do self.text.chars().__rust_force_expr!
		// Also, you simply cannot make an iterator like std::str::chars and the string 
		// it iterates over owned by the same struct
		//
		// We want to hang on to the original text we're parsing because we want
		// to retrieve lines at random to present in error messages.
		//
		// See  https://stackoverflow.com/questions/47193584/is-there-an-owned-version-of-stringchars for
		// some more involved alternative solutions.
		//
		// The clone() here is fine except we double the memory.
		//
		// Another solution would be to only store a Vec<char> in the struct and index through it
		// or iterate as desired. That also takes extra memory compared to a single string.

		let text_to_iterate = self.text.clone();		
		let mut source = text_to_iterate.chars();
				
		while !self.is_finished(){
			self.start = self.current;
			match self.scan_token(&mut source){
				Err(msg) => self.error(msg),
				Ok(token) => tokens.push(token),
			}			
		}
		tokens.push(self.make_token(TokenType::EOF));
		
		tokens
	}		
	
	fn make_token(&self, token_type:TokenType)->Result<Token,String>{
		match token_type {
			TokenType::ERROR(msg) =>Err(msg),
			_ => Ok(Token 
				{ 
					token_type, 		
					start : self.start,
					current : self.current,
					line : self.line, 
					column : self.column,
				}),
		}
	}
	
	fn scan_token(&mut self, source: &mut Chars)-> Result<Token,String>{
		match self.advance(source) {
			Some(current_char) =>{
				let token_type = match current_char {
					'(' => TokenType::LEFT_PAREN,
					')' => TokenType::RIGHT_PAREN,
					'{' => TokenType::LEFT_BRACE,
					'}' => TokenType::RIGHT_BRACE,
					',' => TokenType::COMMA,
					'.' => TokenType::DOT,
					'-' => TokenType::MINUS,
					'*' => TokenType::STAR,
					'+' => TokenType::PLUS,
					';' => TokenType::SEMICOLON,
					_ => TokenType::ERROR(format!("Unrecognized character {} at {}, {}",
								current_char, self.line, self.column)),						
				};
				self.make_token(token_type)						
			},
			None => self.make_token(TokenType::EOF),
		}		
	}
	
	fn advance(&mut self, source:&mut Chars)->Option<char> {
		let c = source.next();			
		self.current += 1;
		self.column += 1;
		match c {
			Some(value) =>{
				if value == '\n' { 
					self.line += 1;
					self.column = 0;
				}
			},
			None =>{},
		}
		c							
	}
	
}
