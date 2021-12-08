use crate::expression::Expr;


struct ExecutionError {
	message : String,
}

#[derive(Clone,Debug)]
enum Stmt {
	Print(PrintNode),
	ExpressionStmt(ExpressionStmtNode),
	Block(BlockNode),		
	If(IfNode),
	While(WhileNode),
}


pub trait StmtNode {
	pub fn execute(&self) -> Result<(), ExecutionError>;	
}

#[derive(Clone,Debug)]
struct PrintNode {
	expression : Expr,
}

impl StmtNode for PrintNode {

	fn execute(&self)  -> Result<(), ExecutionError> {
		match self.expr.evaluate(){
			Ok(v) =>println!("{}",&value.print()),
			Err(msg) => {
				let message = format!("Execution error on 'print' because of {}", &msg.message);
				ExecutionError {message })
			}
		}
				
	}
}

#[derive(Clone,Debug)]
struct ExpressionStmtNode {
	expression : Expr,
}

impl StmtNode for ExpressionNode {

	fn execute(&self) -> Result<(), ExecutionError> {
		match self.expression.evaluate() {
			Err(msg) =>{
				let message = format!("Execution error on 'expression-statement' because of {}", &msg.message);
				ExecutionError {message })
			},
			_ => {},
		}
	}
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


