
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
	finished:bool,
	start: usize,
	current: usize,
	line:usize,
	column:usize,
}

impl Scanner{

	pub fn new()->Self{
		Self {finished:false, start:0, current:0, line:1, column:0}
	}
	
	// place-holder
	fn error(&self, msg:String){
		eprintln!("{}",&msg)
	}
	
	fn is_finished(&self)->bool{
		self.finished
	}
	
	pub fn tokenize(&mut self, input:String)->Vec<Token>{
		let mut tokens:Vec<Token> = Vec::new();
		let mut source = input.chars();
				
		while !self.is_finished(){
			self.start = self.current;
			match self.scan_token(&mut source){
				Err(msg) => self.error(msg),
				Ok(token) => tokens.push(token),
			}			
		}
				
		tokens.push(Token {
			token_type: TokenType::EOF, 
			start : self.start, 
			current : self.current, 
			line:self.line,
			column:self.column} );
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

