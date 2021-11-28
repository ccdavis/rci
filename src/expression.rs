
use crate::lex::Token;
use crate::lex::TokenType;

#[derive(Clone,Debug)]
struct BinaryNode{
	left : Box<Expr>,
	operator : Token,
	right : Box<Expr>,
}
impl BinaryNode{
	pub fn print(&self) -> String {
		format!("{} {} {}",&self.operator.print(), self.left.print(), self.right.print())
	}
}

#[derive(Clone,Debug)]
pub struct GroupingNode{
	expr : Box<Expr>
}

impl GroupingNode {
	pub fn print(&self) -> String {
		format!("Grouping: {}",&self.expr.print())
	}
}

#[derive(Clone,Debug)]
pub struct LiteralNode{
	value : TokenType,
}

impl LiteralNode{
	pub fn print(&self) -> String {
		format!("{}", &self.value.print())
	}
}

#[derive(Clone,Debug)]
pub struct UnaryNode {
	operator : Token,
	expr : Box<Expr>,	
}

impl UnaryNode{
	pub fn print(&self) ->String{
		format!("{} {}",&self.operator.print(), &self.expr.print())
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

	
	pub fn print(&self) -> String {
		let inside = match self {
			Expr::Binary(n) => n.print(),
			Expr::Unary(n) =>n.print(),
			Expr::Literal(n) => n.print(),
			Expr::Grouping(n) => n.print(),
			
			_ => panic!("Not implemented"),
		};
		parenthesize(inside)
	}
}
