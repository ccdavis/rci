use crate::environment::Environment;
use crate::expression::Expr;
use crate::expression::TypeError;
use crate::lex::Token;
use crate::lex::TokenType;
use crate::types::DataValue;
use crate::types::DataType;
use crate::types::ReturnValue;
use crate::symbol_table::SymbolTable;
use crate::symbol_table::SymbolTableEntry;



pub struct ExecutionError {
    pub message: String,
}

pub trait Executable {
    fn execute(&mut self, envr: &mut Environment) -> Result<(), ExecutionError>;
    fn print(&self) -> String;
}

pub trait TypeChecking {
	fn check_types(&self, symbols: &SymbolTable) -> Result<(), TypeError> ;	
}
#[derive(Clone, Debug)]
pub enum Stmt {
    Print(PrintStmtNode),
    ExpressionStmt(ExpressionStmtNode),
    Block(BlockStmtNode),
    If(IfStmtNode),
    Var(VarStmtNode),
	Fun(FunStmtNode),
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
			Fun(stmt) => stmt.print(),
            _ => format!("{:?}", &self),
        }
    }
	
	

    pub fn execute(&mut self, envr: &mut Environment) -> Result<(), ExecutionError> {
        use Stmt::*;
        match self {
            Print(stmt) => stmt.execute(envr),
            ExpressionStmt(stmt) => stmt.execute(envr),
            Var(stmt) => stmt.execute(envr),
			Fun(stmt) => stmt.execute(envr),
            Block(stmt) => stmt.execute(envr),
			If(stmt) => stmt.execute(envr),
            _ => Err(ExecutionError {
                message: " Statement type not implemented.".to_string(),
            }),
        }
    }
	
	pub fn check_types(&self, symbols: &SymbolTable) -> Result<(),TypeError> {
		use Stmt::*;
		match self {
			Print(n) => n.check_types(symbols),
			ExpressionStmt(n) => n.check_types(symbols),
			Var(n) => n.check_types(symbols),
			Fun(n) => n.check_types(symbols),
			Block(n) => n.check_types(symbols),
			If(n) => n.check_types(symbols),
			_ => Err(TypeError {
                message: " Statement type not type-checked yet.".to_string(),
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

impl TypeChecking for PrintStmtNode {

	fn check_types(&self, symbols: &SymbolTable) -> Result<(), TypeError> {
		// print can take any type
		let expr_type = self.expression.determine_type(symbols)?;
				
		Ok(())
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

impl TypeChecking for ExpressionStmtNode {

	fn check_types(&self, symbols: &SymbolTable) -> Result<(), TypeError> {
		// Trigger type checks from the expression contained
		let t = self.expression.determine_type(symbols)?;
		Ok(())
	}
}

#[derive(Clone, Debug)]
struct BlockStmtNode {
    statements: Vec<Stmt>,
	symbols: SymbolTable,
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

impl TypeChecking for BlockStmtNode {

	// Ignore the symbols passed in for now; they will be useful when we have
	// lambdas with parameters.
	fn check_types(&self, symbols: &SymbolTable) -> Result<(), TypeError> {
		// TODO: For now just return error on the first 
		// bad statement, but improve this to check them all.
		for stmt in &self.statements {
            stmt.check_types(&self.symbols)?
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
				let falsey = match result.get() {
					DataValue::Bool(b) => !b,					
					_ => false,					
				};
												
				if !falsey {
					self.then_branch.execute(envr)
				} else if self.has_else {
						self.else_branch.execute(envr)
					} else {
						Ok(())
					}
					
				
			},
			Err(err) => Err(ExecutionError { message: err.message } ),
		}
	}
}

impl TypeChecking for IfStmtNode {

	fn check_types(&self, symbols: &SymbolTable) -> Result<(), TypeError> {
		let cond_type = self.condition.determine_type(symbols)?;
		if let DataType::Bool = cond_type {
			Ok(())
		}else {
			let message = format!("Condition in if-statement must be boolean but was {} instead.",cond_type);
			Err( TypeError { message })
		}				
	}
}

#[derive(Clone, Debug)]
struct VarStmtNode {
    name: String,
	data_type: DataType,
    index: usize,
    initializer: Box<Expr>,
}

impl Executable for VarStmtNode {

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


impl TypeChecking for VarStmtNode {

	fn check_types(&self, symbols: &SymbolTable) -> Result<(), TypeError> {
		let init_type = self.initializer.determine_type(symbols)?;
		if self.data_type != init_type {
			let message = format!("Type '{}' specified for variable '{}' declaration doesn't match initializer expression type of '{}'",
				self.data_type, &self.name, init_type);
				
			Err( TypeError { message } )			
		} else {		
			Ok(())
		}		
	}
}

#[derive(Debug,Clone)]
pub struct FunStmtNode {
	name: Token, // name + line, column	
	params: Vec<Box<SymbolTableEntry>>,
	return_type: DataType,
	body: Vec<Stmt>,
	symbols: SymbolTable,
}

impl Executable for FunStmtNode {

	fn print(&self) -> String {
		format!("function: {}",&self.name.identifier_string())
	}
	
	// This adds the function to the interpreter's environment, executing the function declaration.
	// The evaluation of the function happens in the Expression 'Call' node.
	// which then calls back to the implementation of Callable here.
	fn execute(&mut self, envr: &mut Environment) -> Result<(), ExecutionError> {
		Ok(())
	}
}

impl TypeChecking  for  FunStmtNode {

	fn check_types(&self, symbols: &SymbolTable) -> Result<(), TypeError> {
		// TODO: Not a complete type check yet; need to check the return type
		for stmt in &self.body {
            stmt.check_types(&self.symbols)?
        }
		Ok(())		
	}
	
} // impl

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

    pub fn block_stmt(statements: Vec<Stmt>, symbols: SymbolTable) -> Stmt {
        Stmt::Block(BlockStmtNode { statements, symbols })
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
	
    pub fn var_stmt(name: Token, data_type:DataType, expr: Expr) -> Stmt {
        match name.token_type {
            TokenType::Identifier(n) => Stmt::Var(VarStmtNode {
                name: n,
				data_type,
                index: 0,
                initializer: Box::new(expr),
            }),
            _ => panic!("Can't add variable at {:?}", &name),
        }
    }
	
	pub fn fun_stmt(
		name: Token, 
		params: Vec<Box<SymbolTableEntry>>,
		return_type: DataType, 
		body: Vec<Stmt>,
		symbols: SymbolTable) ->  Stmt {
		
			Stmt::Fun( FunStmtNode {
				name,
				params,			
				return_type,
				body,
				symbols,
			})
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
