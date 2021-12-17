use crate::environment::Environment;
use crate::expression::Expr;
use crate::lex::Token;
use crate::lex::TokenType;

pub struct ExecutionError {
    pub message: String,
}
pub trait Executable {
    fn execute(&mut self, envr: &mut Environment) -> Result<(), ExecutionError>;
    fn print(&self) -> String;
}

#[derive(Clone, Debug)]
pub enum Stmt {
    Print(PrintStmtNode),
    ExpressionStmt(ExpressionStmtNode),
    Block(BlockStmtNode),
    If(IfStmtNode),
    Var(VarNode),
    While(WhileNode),
	NoOp,
}

impl Stmt {
    pub fn print(&self) -> String {
        use Stmt::*;
        match self {
            Print(stmt) => stmt.print(),
            ExpressionStmt(stmt) => stmt.print(),
            Var(stmt) => stmt.print(),
            Block(stmt) => stmt.print(),
			If(stmt) => stmt.print(),
            _ => format!("{:?}", &self),
        }
    }

    pub fn execute(&mut self, envr: &mut Environment) -> Result<(), ExecutionError> {
        use Stmt::*;
        match self {
            Print(stmt) => stmt.execute(envr),
            ExpressionStmt(stmt) => stmt.execute(envr),
            Var(stmt) => stmt.execute(envr),
            Block(stmt) => stmt.execute(envr),
			If(stmt) => stmt.execute(envr),
            _ => Err(ExecutionError {
                message: " Statement type not implemented.".to_string(),
            }),
        }
    }
}

#[derive(Clone, Debug)]
struct PrintStmtNode {
    expression: Expr,
}

impl Executable for PrintStmtNode {
    fn print(&self) -> String {
        format!("print-stmt {}", &self.expression.print())
    }

    fn execute(&mut self, envr: &mut Environment) -> Result<(), ExecutionError> {
        match self.expression.evaluate(envr) {
            Ok(value) => {
                println!("{}", &value.print());
                Ok(())
            }
            Err(msg) => {
                let message = format!("Execution error on 'print' because of {}", &msg.message);
                Err(ExecutionError { message: message })
            }
        }
    }
}

#[derive(Clone, Debug)]
struct ExpressionStmtNode {
    expression: Expr,
}

impl Executable for ExpressionStmtNode {
    fn print(&self) -> String {
        format!("expr-stmt {}", &self.expression.print())
    }

    fn execute(&mut self, envr: &mut Environment) -> Result<(), ExecutionError> {
        match self.expression.evaluate(envr) {
            Err(msg) => {
                let message = format!(
                    "Execution error on 'expression-statement' because of {}",
                    &msg.message
                );
                Err(ExecutionError { message })
            }
            _ => Ok(()),
        }
    }
}

#[derive(Clone, Debug)]
struct BlockStmtNode {
    statements: Vec<Stmt>,
}

impl Executable for BlockStmtNode {
    fn print(&self) -> String {
        let stmts: String = self
            .statements
            .iter()
            .map(|s| s.print())
            .collect::<Vec<String>>()
            .join(";");

        format!("block-stmt: {}", &stmts)
    }

    fn execute(&mut self, envr: &mut Environment) -> Result<(), ExecutionError> {
        let mut local_envr = envr.extend();
        for stmt in &mut self.statements {
            stmt.execute(&mut local_envr)?
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
struct IfStmtNode {
    condition: Expr,
    then_branch: Box<Stmt>,
	has_else: bool, // avoid Option inside else box
    else_branch: Box<Stmt>,
}

impl Executable for IfStmtNode {
	fn print(&self) -> String {
		let else_stmt = if self.has_else {
			self.else_branch.print()
		} else {
			"None".to_string()
		};
		format!("if-stmt {} then {} else {}", &self.condition.print(), &*self.then_branch.print(), else_stmt)
	}
	
	fn execute(&mut self, envr: &mut Environment) -> Result<(), ExecutionError> {
		let test = self.condition.evaluate(envr);
		match test {
			Ok(result) => {
				let falsey = matches!(result.get(), TokenType::False) || 
					matches!(result.get(), TokenType::Nil);
				
				if !falsey {
					self.then_branch.execute(envr)
				} else {
					if self.has_else {
						self.else_branch.execute(envr)
					} else {
						Ok(())
					}
					
				}				
			},
			Err(err) => Err(ExecutionError { message: err.message } ),
		}
	}
}

#[derive(Clone, Debug)]
struct VarNode {
    name: String,
    index: usize,
    initializer: Box<Expr>,
}

impl Executable for VarNode {

    fn print(&self) -> String {
        format!("var-stmt = {}", &self.initializer.print())
    }

    fn execute(&mut self, envr: &mut Environment) -> Result<(), ExecutionError> {
        // unwrap result of evaluation and
        let evaluated = self.initializer.evaluate(envr);
        match evaluated {
            Ok(value) => {
                // Add value to environment binding to name
                self.index = envr.define(self.name.clone(), value);
                Ok(())
            }
            Err(msg) => {
                let message = format!(
                    "Execution error on variable initialization for '{}' because of {}",
                    &self.name, &msg.message
                );
                Err(ExecutionError { message: message })
            }
        }
    }
}

#[derive(Clone, Debug)]
struct WhileNode {
    condition: Expr,
    body: Box<Stmt>,
}

impl Stmt {
    pub fn print_stmt(expression: Expr) -> Stmt {
        Stmt::Print(PrintStmtNode { expression })
    }

    pub fn expression_stmt(expression: Expr) -> Stmt {
        Stmt::ExpressionStmt(ExpressionStmtNode { expression })
    }

    pub fn block_stmt(statements: Vec<Stmt>) -> Stmt {
        Stmt::Block(BlockStmtNode { statements })
    }

    pub fn if_stmt(condition: Expr, then_branch: Stmt, else_branch: Option<Stmt>) -> Stmt {
		match else_branch {
			None => 
			Stmt::If(IfStmtNode {
				condition,
				then_branch: Box::new(then_branch),
				has_else: false,
				else_branch: Box::new(Stmt::no_op()),
			}),
			Some(unwrapped_else_branch) =>
			Stmt::If(IfStmtNode {
				condition,
				then_branch: Box::new(then_branch),
				has_else: true,
				else_branch: Box::new(unwrapped_else_branch),
			}),			
		}
    }
	
    pub fn var_stmt(name: Token, expr: Expr) -> Stmt {
        match name.token_type {
            TokenType::Identifier(n) => Stmt::Var(VarNode {
                name: n,
                index: 0,
                initializer: Box::new(expr),
            }),
            _ => panic!("Can't add variable at {:?}", &name),
        }
    }
	
	pub fn no_op() -> Stmt {
		Stmt::NoOp
	}

    pub fn while_stmt(condition: Expr, body: Stmt) -> Stmt {
        Stmt::While(WhileNode {
            condition,
            body: Box::new(body),
        })
    }
}
