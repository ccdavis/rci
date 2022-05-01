#[allow(dead_code)]
use crate::environment;
use crate::environment::EnvNode;
use crate::environment::EnvRc;
use crate::errors;
use crate::errors::*;
use crate::expression::Expr;
use crate::lex::Token;
use crate::lex::TokenType;
use crate::symbol_table::SymbolTable;
use crate::symbol_table::SymbolTableEntry;
use crate::types::DeclarationType;
use crate::types::Callable;
use crate::types::DataType;
use crate::types::DataValue;
use crate::types::ReturnValue;

use std::cell::RefCell;
use std::rc::Rc;

const TRACE: bool = false;

// Use the '?' early return mechanism to propagate results of errors similar to exceptions.
// Also use it to abandon execution in a function body and return values when executing the
// 'return' statement, or exit a block when executing 'break'.
#[derive(Clone, Debug)]
pub enum EarlyReturn {
    BreakStatement,
    ReturnStatement(ReturnValue),
    Error(errors::Error),
}

impl EarlyReturn {
    pub fn print(&self) -> String {
        match self {
            EarlyReturn::Error(ref e) => e.format(),
            EarlyReturn::BreakStatement => "break-statement".to_string(),
            EarlyReturn::ReturnStatement(ref retval) => retval.print(),
        }
    }

    pub fn error(t: &Token, msg: String) -> EarlyReturn {
        let exec_error = Error::new(t, ErrorType::Execution, msg);
        EarlyReturn::Error(exec_error)
    }
}

pub trait Executable {
    fn execute(&mut self, envr: &EnvRc) -> Result<(), EarlyReturn>;
    fn print(&self) -> String;
}

pub trait TypeChecking {
    fn check_types(&self, symbols: &SymbolTable) -> Result<(), errors::Error>;
}

pub trait Compiler {
    fn compile(&self, symbols: &SymbolTable) -> Result<String, errors::Error>;
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
    Program(ProgramNode),
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

    pub fn execute(&mut self, envr: &EnvRc) -> Result<(), EarlyReturn> {
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
            Program(stmt) => stmt.execute(envr),
            _ => panic!("Statement not implemented."),
        }
    }

    pub fn check_types(&self, symbols: &SymbolTable) -> Result<(), errors::Error> {
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
            Program(n) => n.check_types(symbols),
			NoOp => Ok(()),
            _ => panic!("Statement type '{}' not type-checked yet.", &self.print()),
        }
    }

    pub fn compile(&self, symbols: &SymbolTable) -> Result<String, errors::Error> {
        use Stmt::*;
        match self {
            Print(n) => n.compile(symbols),
            ExpressionStmt(n) => n.compile(symbols),
            Var(n) => n.compile(symbols),
            Fun(n) => n.compile(symbols),
            Block(n) => n.compile(symbols),
            If(n) => n.compile(symbols),
            While(n) => n.compile(symbols),
            Return(n) => n.compile(symbols),
            Break(n) => n.compile(symbols),
            Program(n) => n.compile(symbols),
            _ => panic!("Statement type compilation not supported yet."),
        }
    }
}

#[derive(Clone, Debug)]
pub struct PrintStmtNode {
    expressions: Vec<Expr>,
}

impl Executable for PrintStmtNode {
    fn print(&self) -> String {
        format!("print-stmt {:?}", &self.expressions)
    }

    fn execute(&mut self, envr: &EnvRc) -> Result<(), EarlyReturn> {
        for expr in &self.expressions {
            match expr.evaluate(envr) {
                Ok(value) => {
                    print!("{}", &value.print());
                }
                Err(err) => {
                    return Err(EarlyReturn::Error(err));
                }
            }
        }
        println!("");
        Ok(())
    }
}

impl TypeChecking for PrintStmtNode {
    fn check_types(&self, symbols: &SymbolTable) -> Result<(), errors::Error> {
        // print can take any type
        for expr in &self.expressions {
            let expr_type = expr.determine_type(symbols)?;
        }
        Ok(())
    }
}

impl Compiler for PrintStmtNode {
    fn compile(&self, symbols: &SymbolTable) -> Result<String, errors::Error> {
        // Parse each expression and return the code + its data type or an error
        let mut subst_codes = "".to_string();
        let mut values: Vec<String> = Vec::new();
        for expr in &self.expressions {
            let obj_code = expr.compile(symbols)?;
            let subst_code = match obj_code.data_type {
                DataType::Str => "%s",
                DataType::Number => "%f",
                DataType::Bool => "%d",
                _ => "%s",
            };
			
			let printable = match obj_code.data_type {
				DataType::Str =>  format!("rci_value_to_c_str({})",&obj_code.code),
				DataType::Number => format!("rci_value_to_c_double({})",&obj_code.code),
				DataType::Bool => format!("rci_value_to_c_boolean({})",&obj_code.code),
				_ => "** PRINTING NOT SUPPORTED **".to_string(),
			};
			
            subst_codes = subst_codes + subst_code;
            values.push(printable);
        }

        let mut code = format!("printf(\"{}\\n\",{});\n", 
			&subst_codes, &values.join(","));
        Ok(code)
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

    fn execute(&mut self, envr: &EnvRc) -> Result<(), EarlyReturn> {
        match self.expression.evaluate(envr) {
            Err(err) => Err(EarlyReturn::Error(err)),
            _ => Ok(()),
        }
    }
}

impl TypeChecking for ExpressionStmtNode {
    fn check_types(&self, symbols: &SymbolTable) -> Result<(), errors::Error> {
        // Trigger type checks from the expression contained
        let t = self.expression.determine_type(symbols)?;
        Ok(())
    }
}

impl Compiler for ExpressionStmtNode {
    fn compile(&self, symbols: &SymbolTable) -> Result<String, errors::Error> {
            let expr_code = self.expression.compile(symbols)?;
		let code = format!("{};",&expr_code.code);
        Ok(code)
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

    fn execute(&mut self, envr: &EnvRc) -> Result<(), EarlyReturn> {
        let local_envr = environment::extend(envr);
        // To keep the live environment indexes in sync withthe parsed symbol
        // table which inserts IN_BLOCK and RETURNTYPE in blocks and functions
        // for parse-time type checking._
        local_envr.define("IN_BLOCK".to_string(), ReturnValue::None);

        for stmt in &mut self.statements {
            if let Err(early_return) = stmt.execute(&local_envr) {
                match early_return {
                    EarlyReturn::BreakStatement => break,
                    EarlyReturn::ReturnStatement(_) => {
                        return Err(early_return);
                    }
                    EarlyReturn::Error(_) => return Err(early_return),
                }
            }
        }
        Ok(())
    }
}

impl TypeChecking for BlockStmtNode {
    // Ignore the symbols passed in for now; they will be useful when we have
    // lambdas with parameters.
    fn check_types(&self, symbols: &SymbolTable) -> Result<(), errors::Error> {
        // TODO: For now just return error on the first
        // bad statement, but improve this to check them all.
        for stmt in &self.statements {
            stmt.check_types(&self.symbols)?
        }
        Ok(())
    }
}

impl Compiler for BlockStmtNode {
    fn compile(&self, symbols: &SymbolTable) -> Result<String, errors::Error> {
		let mut stmts = Vec::new();
		for stmt in &self.statements {
			let stmt_code = stmt.compile(&self.symbols)?;
			stmts.push(stmt_code);
		}
		let stmts_code = stmts.join("\n");
		
        let code = format!("{{\n\t{}\n}}", &stmts_code);
		Ok(code)
    }
}

#[derive(Clone, Debug)]
pub struct IfStmtNode {
    location: Token,
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

    fn execute(&mut self, envr: &EnvRc) -> Result<(), EarlyReturn> {
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
            Err(err) => Err(EarlyReturn::Error(err)),
        }
    }
}

impl TypeChecking for IfStmtNode {
    fn check_types(&self, symbols: &SymbolTable) -> Result<(), errors::Error> {
        let cond_type = self.condition.determine_type(symbols)?;
        if let DataType::Bool = cond_type {
            Ok(())
        } else {
            let message = format!(
                "Condition in if-statement must be boolean but was {} instead.",
                cond_type
            );
            Err(Error::new(&self.location, ErrorType::Type, message))
        }
    }
}

impl Compiler for IfStmtNode {
    fn compile(&self, symbols: &SymbolTable) -> Result<String, errors::Error> {        
		let cond_code = self.condition.compile(symbols)?;
		let then_branch_code = self.then_branch.compile(symbols)?;
		
		let code = if self.has_else {
			let else_branch_code = self.else_branch.compile(symbols)?;
			format!("if (rci_value_to_c_boolean({}))\n{} else\n{}\n",
				&cond_code.code,&then_branch_code, &else_branch_code)
		} else {
			format!("if (rci_value_to_c_boolean({}))\n{}\n",
				&cond_code.code, &then_branch_code)
		};
		
		Ok(code)
			
    }
}

#[derive(Clone, Debug)]
pub struct VarStmtNode {
    location: Token,
    name: String,
    data_type: DataType,
    index: usize,
    initializer: Box<Expr>,
}

impl Executable for VarStmtNode {
    fn print(&self) -> String {
        format!("var-stmt = {}", &self.initializer.print())
    }

    fn execute(&mut self, envr: &EnvRc) -> Result<(), EarlyReturn> {
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
                let err = errors::Error::new(&self.location, ErrorType::Execution, message);
                Err(EarlyReturn::Error(err))
            }
        }
    }
}

impl TypeChecking for VarStmtNode {
    fn check_types(&self, symbols: &SymbolTable) -> Result<(), errors::Error> {
        let init_type = self.initializer.determine_type(symbols)?;
        if self.data_type != init_type {
            let message = format!("Type '{}' specified for variable '{}' declaration doesn't match initializer expression type of '{}'",
				self.data_type, &self.name, init_type);

            Err(errors::Error::new(&self.location, ErrorType::Type, message))
        } else {
            Ok(())
        }
    }
}

impl Compiler for VarStmtNode {
    fn compile(&self, symbols: &SymbolTable) -> Result<String, errors::Error> {
		let lhs = format!("rci_value {}",self.name);
		let rhs =  self.initializer.compile(symbols)?;
		let code = format!("{} = {};",&lhs, &rhs.code);
		Ok(code)		        
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
    fn execute(&mut self, envr: &EnvRc) -> Result<(), EarlyReturn> {
        if TRACE {
            println!("Define function {}", self.name.identifier_string());
        }
        envr.define(
            self.name.identifier_string(),
            ReturnValue::CallableValue(Box::new(UserFunction::new(self.clone(), Rc::clone(envr)))),
        );
        // The type-checker should have caught situations where we're redefining a function.
        // No other errors should be possible at this point either.
        Ok(())
    }
}

impl TypeChecking for FunStmtNode {
    fn check_types(&self, symbols: &SymbolTable) -> Result<(), errors::Error> {
        for stmt in &self.body {
            stmt.check_types(&self.symbols)?
        }
        Ok(())
    }
} // impl

impl Compiler for FunStmtNode {
    fn compile(&self, symbols: &SymbolTable) -> Result<String, errors::Error> {
            
		let fun_name = self.name.token_type.print_value();		
		let mut params_code:Vec<String> = Vec::new();
		for p in &self.params {
			let param_code = match p.entry_type {
				DeclarationType::Var => {
					format!("rci_value * {}",&p.name) 
				},
				DeclarationType::Val => {
					format!("const rci_value {}",&p.name)
				},
				DeclarationType::Cpy => {
					// NOTE: at the call site a deep copy should have been
					// made by the compiler.
					format!("rci_value {}",&p.name)
				},
				_ => {
				 panic!("Internal compiler error. Only val, var,, cpy allowed as param decl types.")
				}				
			};
			params_code.push(param_code);
		}
		
		let mut stmts_code:Vec<String> = Vec::new();
		for stmt in &self.body {
			let stmt_code = stmt.compile(&self.symbols)?;
			stmts_code.push(stmt_code);
		}

        let decl = format!("rci_value {}({})", 
			&fun_name, &params_code.join(","));

		let body = format!("{{\n{}\n}}", 
			&stmts_code.join("\n"));
		Ok(format!("{}\n{}\n",&decl, &body))		
    }
}

#[derive(Clone)]
pub struct UserFunction {
    declaration: FunStmtNode,
    closure: EnvRc,
}

impl UserFunction {
    fn new(declaration: FunStmtNode, closure: EnvRc) -> Self {
        Self {
            declaration,
            closure,
        }
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

    fn call(&mut self, arguments: Vec<ReturnValue>) -> Result<ReturnValue, EarlyReturn> {
        let local_envr = environment::extend(&self.closure);
        // To keep in sync with the symbol table indexes
        local_envr.define("RETURN_TYPE".to_string(), ReturnValue::None);

        // Add argument values to the local environment
        for (index, arg_value) in arguments.into_iter().enumerate() {
            let param = &self.declaration.params[index];
            local_envr.define(self.declaration.params[index].name.to_owned(), arg_value);
        }

        let mut return_value = ReturnValue::Value(DataValue::Unresolved);
        if TRACE {
            println!("In function {}", self.name());
        }
        let decl = &mut self.declaration;

        for stmt in &mut decl.body {
            // Catch any early returns from a return statement, otherwise
            // return the error as normal.
            if let Err(early_return) = stmt.execute(&local_envr) {
                match early_return {
                    EarlyReturn::ReturnStatement(retval) => {
                        return_value = retval;
                        return Ok(return_value);
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
                "No return executed for function {}.",
                &self.declaration.name.identifier_string(),
            );

            return Err(EarlyReturn::error(&self.declaration.name, message));
        }
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

    fn execute(&mut self, envr: &EnvRc) -> Result<(), EarlyReturn> {
        match self.expr.evaluate(envr) {
            Ok(retval) => Err(EarlyReturn::ReturnStatement(retval)),
            Err(eval_error) => Err(EarlyReturn::error(&self.location, eval_error.message)),
        }
    }
}

impl TypeChecking for ReturnStmtNode {
    fn check_types(&self, symbols: &SymbolTable) -> Result<(), errors::Error> {
        let would_return_type = self.expr.determine_type(symbols)?;
        if would_return_type != self.return_type {
            let message = format!(
                "Type of return statement '{}' doesn't match function return type of '{}'.",
                &would_return_type, &self.return_type
            );
            Err(Error::new(&self.location, ErrorType::Type, message))
        } else {
            Ok(())
        }
    }
}

impl Compiler for ReturnStmtNode {
    fn compile(&self, symbols: &SymbolTable) -> Result<String, errors::Error> {
		let return_value = self.expr.compile(symbols)?;
        Ok(format!("return {};", &return_value.code))
    }
}

#[derive(Clone, Debug)]
pub struct BreakStmtNode {
    location: Token,
}

impl Executable for BreakStmtNode {
    fn print(&self) -> String {
        "break-statement".to_string()
    }

    // The parser should prevent 'break' outside of a block
    fn execute(&mut self, envr: &EnvRc) -> Result<(), EarlyReturn> {
        Err(EarlyReturn::BreakStatement)
    }
}

impl TypeChecking for BreakStmtNode {
    // We could add a special symbol to any block's symbol table and then the
    // break statement could check if it was valid ...
    fn check_types(&self, symbols: &SymbolTable) -> Result<(), errors::Error> {
        Ok(())
    }
}

impl Compiler for BreakStmtNode {
    fn compile(&self, symbols: &SymbolTable) -> Result<String, errors::Error> {
        Ok("break;".to_string())
    }
}

#[derive(Clone, Debug)]
pub struct WhileStmtNode {
    location: Token,
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

    fn execute(&mut self, envr: &EnvRc) -> Result<(), EarlyReturn> {
        loop {
            match self.condition.evaluate(envr) {
                Err(eval_err) => return Err(EarlyReturn::Error(eval_err)),
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
                        return Err(EarlyReturn::error(&self.location, message));
                    }
                }
            }
        }

        Ok(())
    }
}

impl TypeChecking for WhileStmtNode {
    fn check_types(&self, symbols: &SymbolTable) -> Result<(), errors::Error> {
        let cond_type = self.condition.determine_type(symbols)?;
        if matches!(cond_type, DataType::Bool) {
            self.body.check_types(symbols)
        } else {
            let message = format!(
                "Condition in while-statement must be boolean but was {} instead.",
                cond_type
            );
            Err(Error::new(&self.location, ErrorType::Type, message))
        }
    }
}

impl Compiler for WhileStmtNode {
    fn compile(&self, symbols: &SymbolTable) -> Result<String, errors::Error> {
        let cond = self.condition.compile(symbols)?;
		let body_code = self.body.compile(symbols)?;
		Ok( format!("while(rci_value_to_c_boolean({}))\n{}",&cond.code, &body_code))
    }
}
#[derive(Clone, Debug)]
pub struct ProgramNode {
    declarations: Vec<Stmt>,
    imperatives: Box<Stmt>, // this is only allowed to be a block
}

impl Executable for ProgramNode {
    fn print(&self) -> String {
        "Program".to_string()
    }

    fn execute(&mut self, envr: &EnvRc) -> Result<(), EarlyReturn> {        
		for stmt in &mut self.declarations {
			stmt.execute(envr)?;
		}
		
		self.imperatives.execute(envr)
    }
}

impl TypeChecking for ProgramNode {
    fn check_types(&self, symbols: &SymbolTable) -> Result<(), errors::Error> {
        for decl in &self.declarations {
			decl.check_types(symbols)?;
		}
		self.imperatives.check_types(symbols)
    }
}

// This is  where we insert the internal types for supporting function calling
// and minimal standard library functions.
impl Compiler for ProgramNode {
    fn compile(&self, symbols: &SymbolTable) -> Result<String, errors::Error> {
        let  compiler_support = "#include \"./ir/compiler_support.h\"";
		let mut decls: Vec<String> = Vec::new();
		for decl in &self.declarations {
			let decl_code = decl.compile(symbols)?;
			decls.push(decl_code);
		}
		let declarations_code = decls.join("\n\n");
		
		let imperatives_code = self.imperatives.compile(symbols)?;
		
		let main_fn = format!("int main(int argc, const char ** argv) {{\n{}\nreturn 0;\n}}",
			&imperatives_code);
			
		Ok(format!("{}\n{}\n\n{}", 
			compiler_support, 
			&declarations_code,
			&main_fn))				        
    }
}

impl Stmt {
    pub fn print_stmt(expressions: Vec<Expr>) -> Stmt {
        Stmt::Print(PrintStmtNode { expressions })
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

    pub fn if_stmt(
        location: Token,
        condition: Expr,
        then_branch: Stmt,
        else_branch: Option<Stmt>,
    ) -> Stmt {
        match else_branch {
            None => Stmt::If(IfStmtNode {
                location,
                condition,
                then_branch: Box::new(then_branch),
                has_else: false,
                else_branch: Box::new(Stmt::no_op()),
            }),
            Some(unwrapped_else_branch) => Stmt::If(IfStmtNode {
                location,
                condition,
                then_branch: Box::new(then_branch),
                has_else: true,
                else_branch: Box::new(unwrapped_else_branch),
            }),
        }
    }

    pub fn var_stmt(name: Token, data_type: DataType, expr: Expr) -> Stmt {
        let location = name.clone();
        match name.token_type {
            TokenType::Identifier(n) => Stmt::Var(VarStmtNode {
                location,
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

    // A program has declarations for functions, variables and types
    // at the global level, and a single block of statements at the end
    // to execute.
    pub fn program(declarations: Vec<Stmt>, imperatives: Box<Stmt>) -> Stmt {
        Stmt::Program(ProgramNode {
            declarations,
            imperatives,
        })
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

    pub fn while_stmt(location: Token, condition: Expr, body: Stmt) -> Stmt {
        Stmt::While(WhileStmtNode {
            location,
            condition,
            body: Box::new(body),
        })
    }
}
