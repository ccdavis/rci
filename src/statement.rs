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

