use crate::expression::Expr;
#[derive(Clone,Debug)]
enum Stmt {
	Print(PrintNode),
	ExpressionStmt(ExpressionStmtNode),
	Block(BlockNode),		
	If(IfNode),
	While(WhileNode),
}

#[derive(Clone,Debug)]
struct PrintNode {
	expression : Expr,
}

#[derive(Clone,Debug)]
struct ExpressionStmtNode {
	expression : Expr,
}

#[derive(Clone,Debug)]
struct BlockNode {
	statements : Vec<Stmt>,
}
#[derive(Clone,Debug)]
struct IfNode {
	condition : Expr,
	then_branch : Box<Stmt>,
	else_branch : Box<Stmt>,
}

#[derive(Clone,Debug)]
struct WhileNode {
	condition : Expr,
	body : Box<Stmt>,
}

impl Stmt {
	

	pub fn print_stmt(expression : Expr) -> Stmt {
		Stmt::Print(PrintNode{ expression })
	}
	
	pub fn expression_stmt(expression : Expr) -> Stmt {
		Stmt::ExpressionStmt( ExpressionStmtNode { expression })
	}
	
	pub fn block_stmt(statements:Vec<Stmt>) -> Stmt {
		Stmt::Block(BlockNode { statements })
	}
	
	pub fn if_stmt(
		condition: Expr, 
		then_branch: Stmt, 
		else_branch: Stmt) -> Stmt {
		
		Stmt::If(IfNode { 
			condition, 
			then_branch: Box::new(then_branch), 
			else_branch: Box::new(else_branch)
		})
	}
	
	pub fn while_stmt(condition: Expr, body: Stmt) -> Stmt {
		Stmt::While(WhileNode { condition, body : Box::new(body) } )
	}
	
	
	
}


