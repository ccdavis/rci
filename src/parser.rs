use crate::expression::*;
use crate::lex::Token;
use crate::lex::TokenType;

struct Parser{
	tokens : Vec<Token>,
	current :usize,
	
	
}

impl Parser {

	pub fn new(tokens : Vec<Token>) -> Self{
		Self { tokens, current : 0 }
	}
	
	fn expression(&self) -> Expr {
		equality()
	}
	
	fn equality(&self)-> Expr {
		let expr = comparison();
		// more here
		expr				
	}
	
	fn comparison(&self) -> Expr {
		let mut expr = term();
		use TokenType::*;
		while matches([Less,LessEqual,Greater,GreaterEqual]){
			let operator = previous();
			let right = term();
			expr = Expr::Binary(
				BinaryNode {
					left : Box::new(expr),
					operator : operator,
					right : Box::new(right),
				});			
		}
		expr
	}
	
	fn term(&self) -> Expr {
	}
	
	fn matchs(&self, &[TokenType]) -> bool {
		true
	}
	
	
	
}
