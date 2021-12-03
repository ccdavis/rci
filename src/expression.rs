	
use crate::lex::Token;
use crate::lex::TokenType;


pub trait Node{

	// return the name  of the operation if any and 
	// associated expressions.
	fn print(&self) -> String;
	fn evaluate(&mut self) -> Expr;	
}

#[derive(Clone,Debug)]
struct BinaryNode{
	left : Box<Expr>,
	operator : Token,
	right : Box<Expr>,
}

impl Node for BinaryNode{
	fn print(&self) -> String {
		format!("{} {} {}",&self.operator.print(), self.left.print(), self.right.print())
	}
}

#[derive(Clone,Debug)]
pub struct GroupingNode{
	expr : Box<Expr>
}

impl Node for GroupingNode {
	fn print(&self) -> String {
		format!("Grouping: {}",&self.expr.print())
	}
	
	fn evaluate(&self) -> Box<Expr>{
		self.expr.evaluate();
	}
}

#[derive(Clone,Debug)]
pub struct LiteralNode{
	value : TokenType,
}

impl Node for LiteralNode{
	fn print(&self) -> String {
		format!("{}", &self.value.print())
	}
	
	fn evaluate(&self) -> TokenType {
		self.value
	}
}

#[derive(Clone,Debug)]
pub struct UnaryNode {
	operator : Token,
	expr : Box<Expr>,	
}

impl Node for UnaryNode{
	fn print(&self) ->String{
		format!("{} {}",&self.operator.print(), &self.expr.print())
	}
	
	fn evaluate(&self)-> Expr {
		let right = self.right.evaluate();
		match operator.token_type {
			TokenType::Minus  => {
				match right.literal_value() {
					TokenType::Number(n) => {
						let new_value = TokenType::Number(-n);
						Expr::literal(new_value)
					},
					_ => panic!("Not a number value at {:?}", self.operator),
				}				
			},
			TokenType::Not => {
				right.is_truthy()
			},
			_ => panic!("Invalid unary expression at {:?}",&self.operator),
		}
		
	}
	
	
}

fn parenthesize(inside:String) -> String {
	"(".to_string() + &inside + ")"
}



#[derive(Clone,Debug)]
pub enum Expr{
	Binary(BinaryNode),
	Unary(UnaryNode),
	Grouping(GroupingNode),
	Literal(LiteralNode),
}

impl Expr{
	
	pub fn binary(l:Expr, op:Token, r:Expr) -> Expr{
		let node = BinaryNode 
		{ 	left : Box::new(l),
			operator : op,
			right : Box::new(r)
		};
		Expr::Binary( node)
	}
	
	pub fn unary(op : Token, e: Expr) -> Expr {
		let node = UnaryNode {
			operator : op,
			expr : Box::new(e),
		};
		Expr::Unary(node)
	}
	
	pub fn literal(value:TokenType) -> Expr {
		Expr::Literal(LiteralNode {value} )		
	}
	
	pub fn grouping(e: Expr) -> Expr {
		Expr::Grouping(GroupingNode { expr : Box::new(e) })
	}
	
	pub fn is_literal(&self) -> bool {
		match self {
			Expr::Literal(_) => true,
			_ => false,
		}
	}	
	
	pub fn literal_value(&self) -> TokenType {
		match self {
			Expr::Literal(l) => l.value,
			_ => panic!("Not a literal value: {:?}",self),
		}		
	}
	
	//  True or not Nil
	pub fn is_truthy(&self) -> bool {
		match self.literal_value() {
			TokenType::True => true,
			TokenType::False => false,
			TokenType::Nil => false,
			_ => true,
			
		}
	}
	
	

	
	pub fn print(&self) -> String {
		use Expr::*;
		let inside = match self {
			Binary(n) => n.print(),
			Unary(n) =>n.print(),
			Literal(n) => n.print(),
			Grouping(n) => n.print(),
			
			_ => panic!("Not implemented"),
		};
		parenthesize(inside)
	}
}
