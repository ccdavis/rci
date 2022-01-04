use crate::environment::Environment;
use crate::expression::Expr;
use crate::expression::TypeError;
use crate::lex::Token;
use crate::lex::TokenType;
use crate::symbol_table::SymbolTable;
use crate::symbol_table::SymbolTableEntry;
use crate::types::Callable;
use crate::types::DataType;
use crate::types::DataValue;
use crate::types::ReturnValue;

#[derive(Clone, Debug)]
pub struct ExecutionError {
    pub message: String,
}

// Use the '?' early return mechanism to propagate results of errors similar to exceptions.
// Also use it to abandon execution in a function body and return values when executing the 
// 'return' statement, or exit a block when executing 'break'.
#[derive(Clone, Debug)]
pub enum EarlyReturn {
	BreakStatement,
    ReturnStatement(ReturnValue),
    Error(ExecutionError),    
}

impl EarlyReturn {
    pub fn print(&self) -> String {
        match self {
            EarlyReturn::Error(ref e) => e.message.to_owned(),
            EarlyReturn::BreakStatement => "break-statement".to_string(),
            EarlyReturn::ReturnStatement(ref retval) => retval.print(),
        }
    }

    pub fn error(msg: String) -> EarlyReturn {
        let exec_error = ExecutionError { message: msg };
        EarlyReturn::Error(exec_error)
    }
}

pub trait Executable {
    fn execute(&mut self, envr: &mut Environment) -> Result<(), EarlyReturn>;
    fn print(&self) -> String;
}

pub trait TypeChecking {
    fn check_types(&self, symbols: &SymbolTable) -> Result<(), TypeError>;
}
#[derive(Clone, Debug)]
pub enum Stmt {
    Print(PrintStmtNode),
    ExpressionStmt(ExpressionStmtNode),
    Block(BlockStmtNode),
    If(IfStmtNode),
    Var(VarStmtNode),
    Fun(FunStmtNode),
    While(WhileStmtNode),
    Return(ReturnStmtNode),
	Break(BreakStmtNode),
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
            While(stmt) => stmt.print(),
            Return(stmt) => stmt.print(),
            _ => format!("{:?}", &self),
        }
    }

    pub fn execute(&mut self, envr: &mut Environment) -> Result<(), EarlyReturn> {
        use Stmt::*;
        match self {
            Print(stmt) => stmt.execute(envr),
            ExpressionStmt(stmt) => stmt.execute(envr),
            Var(stmt) => stmt.execute(envr),
            Fun(stmt) => stmt.execute(envr),
            Block(stmt) => stmt.execute(envr),
            If(stmt) => stmt.execute(envr),
            While(stmt) => stmt.execute(envr),
            Return(stmt) => stmt.execute(envr),
			Break(stmt) => stmt.execute(envr),
            _ => Err(EarlyReturn::Error(ExecutionError {
                message: " Statement type not implemented.".to_string(),
            })),
        }
    }

    pub fn check_types(&self, symbols: &SymbolTable) -> Result<(), TypeError> {
        use Stmt::*;
        match self {
            Print(n) => n.check_types(symbols),
            ExpressionStmt(n) => n.check_types(symbols),
            Var(n) => n.check_types(symbols),
            Fun(n) => n.check_types(symbols),
            Block(n) => n.check_types(symbols),
            If(n) => n.check_types(symbols),
            While(n) => n.check_types(symbols),
            Return(n) => n.check_types(symbols),
			Break(n) => n.check_types(symbols),
            _ => Err(TypeError {
                message: " Statement type not type-checked yet.".to_string(),
            }),
        }
    }
}

#[derive(Clone, Debug)]
pub struct PrintStmtNode {
    expression: Expr,
}

impl Executable for PrintStmtNode {
    fn print(&self) -> String {
        format!("print-stmt {}", &self.expression.print())
    }

    fn execute(&mut self, envr: &mut Environment) -> Result<(), EarlyReturn> {
        match self.expression.evaluate(envr) {
            Ok(value) => {
                println!("{}", &value.print());
                Ok(())
            }
            Err(msg) => {
                let message = format!("Execution error on 'print' because of {}", &msg.message);
                Err(EarlyReturn::error(message))
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
pub struct ExpressionStmtNode {
    expression: Expr,
}

impl Executable for ExpressionStmtNode {
    fn print(&self) -> String {
        format!("expr-stmt {}", &self.expression.print())
    }

    fn execute(&mut self, envr: &mut Environment) -> Result<(), EarlyReturn> {
        match self.expression.evaluate(envr) {
            Err(msg) => {
                let message = format!(
                    "Execution error on 'expression-statement' because of {}",
                    &msg.message
                );
                Err(EarlyReturn::error(message))
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
pub struct BlockStmtNode {
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

    fn execute(&mut self, envr: &mut Environment) -> Result<(), EarlyReturn> {
        envr.extend();
        for stmt in &mut self.statements {
            if let Err(early_return) = stmt.execute(envr) {
                match early_return {
                    EarlyReturn::BreakStatement => break,
                    EarlyReturn::ReturnStatement(_) => {
                        envr.retract();
                        return Err(early_return);
                    }
                    EarlyReturn::Error(_) => return Err(early_return),
                }
            }
        }
        envr.retract();
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
pub struct IfStmtNode {
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
        format!(
            "if-stmt {} then {} else {}",
            &self.condition.print(),
            &*self.then_branch.print(),
            else_stmt
        )
    }

    fn execute(&mut self, envr: &mut Environment) -> Result<(), EarlyReturn> {
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
            }
            Err(err) => Err(EarlyReturn::Error(ExecutionError {
                message: err.message,
            })),
        }
    }
}

impl TypeChecking for IfStmtNode {
    fn check_types(&self, symbols: &SymbolTable) -> Result<(), TypeError> {
        let cond_type = self.condition.determine_type(symbols)?;
        if let DataType::Bool = cond_type {
            Ok(())
        } else {
            let message = format!(
                "Condition in if-statement must be boolean but was {} instead.",
                cond_type
            );
            Err(TypeError { message })
        }
    }
}

#[derive(Clone, Debug)]
pub struct VarStmtNode {
    name: String,
    data_type: DataType,
    index: usize,
    initializer: Box<Expr>,
}

impl Executable for VarStmtNode {
    fn print(&self) -> String {
        format!("var-stmt = {}", &self.initializer.print())
    }

    fn execute(&mut self, envr: &mut Environment) -> Result<(), EarlyReturn> {
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
                Err(EarlyReturn::error(message))
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

            Err(TypeError { message })
        } else {
            Ok(())
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunStmtNode {
    name: Token, // name + line, column
    params: Vec<Box<SymbolTableEntry>>,
    return_type: DataType,
    body: Vec<Stmt>,
    symbols: SymbolTable,
}

impl Executable for FunStmtNode {
    fn print(&self) -> String {
        format!("function: {}", &self.name.identifier_string())
    }

    // This adds the function to the interpreter's environment, executing the function declaration.
    // function happens in the Expression 'Call' node.
    // which then calls back to the implementation of Callable (UserFunction) here.
    fn execute(&mut self, envr: &mut Environment) -> Result<(), EarlyReturn> {
        envr.define(
            self.name.identifier_string(),
            ReturnValue::CallableValue(Box::new(UserFunction::new(self.clone()))),
        );
        // The type-checker should have caught situations where we're redefining a function.
        // No other errors should be possible at this point either.
        Ok(())
    }
}

impl FunStmtNode {
    // Searches the body for a return statement and adds the
    // return type  to any found. If no return type, it's a
    // type error.
    fn check_return_type(&mut self, return_type: &DataType) -> Result<(), TypeError> {
        let mut has_return = false;
        for stmt in &mut self.body {}

        if has_return == false {
            let message = format!(
                "Missing return statement in function {} at {}",
                &self.name.identifier_string(),
                &self.name.print()
            );
            Err(TypeError { message })
        } else {
            Ok(())
        }
    }
}

impl TypeChecking for FunStmtNode {
    fn check_types(&self, symbols: &SymbolTable) -> Result<(), TypeError> {
        for stmt in &self.body {
            stmt.check_types(&self.symbols)?
        }
        Ok(())
    }
} // impl

#[derive(Clone)]
pub struct UserFunction {
    declaration: FunStmtNode,
}

impl UserFunction {
    fn new(declaration: FunStmtNode) -> Self {
        Self { declaration }
    }
}

impl Callable for UserFunction {
    fn name(&self) -> String {
        self.declaration.name.identifier_string()
    }

    fn arity(&self) -> usize {
        self.declaration.params.len()
    }

    fn return_type(&self) -> &DataType {
        &self.declaration.return_type
    }

    fn params(&self) -> Vec<Box<SymbolTableEntry>> {
        self.declaration.params.clone()
    }

    fn call(
        &mut self,
        envr: &mut Environment,
        arguments: Vec<ReturnValue>,
    ) -> Result<ReturnValue, EarlyReturn> {
        envr.extend();
        // Add argument values to the local environment
        for (index, arg_value) in arguments.into_iter().enumerate() {
            let param = &self.declaration.params[index];
            envr.define(self.declaration.params[index].name.to_owned(), arg_value);
        }

        let mut return_value = ReturnValue::Value(DataValue::Unresolved);
        let decl = &mut self.declaration;

        for stmt in &mut decl.body {
            // Catch any early returns from a return statement, otherwise
            // return the error as normal.
            if let Err(early_return) = stmt.execute(envr) {
                match early_return {
                    EarlyReturn::ReturnStatement(retval) => {
                        return_value = retval;
                        break;
                    }
                    EarlyReturn::Error(_) => return Err(early_return),
                    EarlyReturn::BreakStatement => {
                        panic!("A 'break' statement escaped its context!");
                    }
                }
            }
        }

        if matches!(return_value, ReturnValue::Value(DataValue::Unresolved)) {
            let message = format!(
                "No return executed for function {} at {}",
                &self.declaration.name.identifier_string(),
                &self.name()
            );

            return Err(EarlyReturn::error(message));
        }
        envr.retract();
        Ok(return_value)
    }
}

#[derive(Clone, Debug)]
pub struct ReturnStmtNode {
    location: Token,
    expr: Expr,
    return_type: DataType,
}

impl Executable for ReturnStmtNode {
    fn print(&self) -> String {
        format!("return-statement: {}", &self.expr.print())
    }

    fn execute(&mut self, envr: &mut Environment) -> Result<(), EarlyReturn> {
        match self.expr.evaluate(envr) {
            Ok(retval) => Err(EarlyReturn::ReturnStatement(retval)),
            Err(eval_error) => Err(EarlyReturn::error(eval_error.message)),
        }
    }
}

impl TypeChecking for ReturnStmtNode {
    fn check_types(&self, symbols: &SymbolTable) -> Result<(), TypeError> {
        let would_return_type = self.expr.determine_type(symbols)?;
        if would_return_type != self.return_type {
            let message = format!(
                "Type of return statement '{}' doesn't match function return type of '{}' at {:?}",
                &would_return_type, &self.return_type, &self.location
            );
            Err(TypeError { message })
        } else {
            Ok(())
        }
    }
}

#[derive(Clone,Debug)]
struct BreakStmtNode {
	location: Token,
}

impl Executable for BreakStmtNode {

	fn print(&self) ->String {
		"break-statement".to_string()
	}
	
	// The parser should prevent 'break' outside of a block
	fn execute(&mut self, envr:&mut Environment) -> Result<(), EarlyReturn> {
		Err(EarlyReturn::BreakStatement)
	}
}

impl TypeChecking for BreakStmtNode {


	// We could add a special symbol to any block's symbol table and then the
	// break statement could check if it was valid ...
	fn check_types(&self, symbols: &SymbolTable) -> Result<(), TypeError> {
		Ok(())
	}
}

#[derive(Clone, Debug)]
pub struct WhileStmtNode {
    condition: Expr,
    body: Box<Stmt>,
}

impl Executable for WhileStmtNode {
    fn print(&self) -> String {
        format!(
            "while-loop cond: {}, body: {}",
            &self.condition.print(),
            &self.body.print()
        )
    }

    fn execute(&mut self, envr: &mut Environment) -> Result<(), EarlyReturn> {
        loop {
            match self.condition.evaluate(envr) {
                Err(eval_err) => return Err(EarlyReturn::error(eval_err.message)),
                Ok(cond) => {
                    if let DataValue::Bool(b) = cond.get() {
                        if *b {
                            if let Err(early_return) = self.body.execute(envr) {
                                match early_return {
                                    EarlyReturn::BreakStatement => break,
                                    _ => return Err(early_return),
                                }
                            }
                        } else {
                            break;
                        }
                    } else {
                        // Type error in test expression
                        let message = format!(
                            "Expressions in a while statement must evaluate to true or false."
                        );
                        return Err(EarlyReturn::error(message));
                    }
                }
            }
        }

        Ok(())
    }
}

impl TypeChecking for WhileStmtNode {
    fn check_types(&self, symbols: &SymbolTable) -> Result<(), TypeError> {
        let cond_type = self.condition.determine_type(symbols)?;
        if matches!(cond_type, DataType::Bool) {
            self.body.check_types(symbols)
        } else {
            let message = format!(
                "Condition in while-statement must be boolean but was {} instead.",
                cond_type
            );
            Err(TypeError { message })
        }
    }
}

impl Stmt {
    pub fn print_stmt(expression: Expr) -> Stmt {
        Stmt::Print(PrintStmtNode { expression })
    }

    pub fn expression_stmt(expression: Expr) -> Stmt {
        Stmt::ExpressionStmt(ExpressionStmtNode { expression })
    }

    pub fn block_stmt(statements: Vec<Stmt>, symbols: SymbolTable) -> Stmt {
        Stmt::Block(BlockStmtNode {
            statements,
            symbols,
        })
    }

    pub fn if_stmt(condition: Expr, then_branch: Stmt, else_branch: Option<Stmt>) -> Stmt {
        match else_branch {
            None => Stmt::If(IfStmtNode {
                condition,
                then_branch: Box::new(then_branch),
                has_else: false,
                else_branch: Box::new(Stmt::no_op()),
            }),
            Some(unwrapped_else_branch) => Stmt::If(IfStmtNode {
                condition,
                then_branch: Box::new(then_branch),
                has_else: true,
                else_branch: Box::new(unwrapped_else_branch),
            }),
        }
    }

    pub fn var_stmt(name: Token, data_type: DataType, expr: Expr) -> Stmt {
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

    pub fn return_stmt(location: Token, expr: Expr, return_type: DataType) -> Stmt {
        Stmt::Return(ReturnStmtNode {
            location,
            expr,
            return_type,
        })
    }
	
	pub fn break_stmt(location: Token) -> Stmt {
		Stmt::Break(BreakStmtNode { location })
	}

    pub fn fun_stmt(
        name: Token,
        params: Vec<Box<SymbolTableEntry>>,
        return_type: DataType,
        body: Vec<Stmt>,
        symbols: SymbolTable,
    ) -> Stmt {
        Stmt::Fun(FunStmtNode {
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
        Stmt::While(WhileStmtNode {
            condition,
            body: Box::new(body),
        })
    }
}
