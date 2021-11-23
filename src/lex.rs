

#[derive(Clone,Debug)]
pub enum Token{
	LeftParen, RightParen,
	Number(f64), Identifier(String)	,
	EOF,
}

#[derive(Clone,Debug)]
pub struct Scanner{
	source:String,		
}

impl Scanner{

	pub fn new(source:String)->Self{
		Scanner {source}
	}
	
	pub fn tokenize(&mut self)->Vec<Token>{
		let mut tokens:Vec<Token> = Vec::new();
		
		tokens.push(Token::EOF);
		tokens
	}		
}

