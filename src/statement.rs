#[allow(dead_code)]
use crate::errors;
use crate::errors::*;
use crate::expression::Expr;
use crate::lex::Token;
use crate::lex::TokenType;
use crate::symbol_table::SymbolTable;
use crate::symbol_table::SymbolTableEntry;
use crate::symbol_table::*;

use crate::types::DataType;
use crate::types::DeclarationType;
use crate::types::GlobalStatementObjectCode;

const TRACE: bool = false;

pub trait TypeChecking {
    fn check_types(&self, symbols: &SymbolTable) -> Result<(), errors::Error>;
}

pub trait Compiler {
    fn compile(&self, symbols: &SymbolTable) -> Result<String, errors::Error>;
    fn compile_global(
        &self,
        symbols: &SymbolTable,
    ) -> Result<GlobalStatementObjectCode, errors::Error> {
        Err(errors::Error::internal(
            ErrorType::Internal,
            "Can't compile this statement type as a global declaration.".to_string(),
        ))
    }
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
    Module(ModuleNode),
    Type(TypeNode),
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
            Module(stmt) => stmt.print(),
            Return(stmt) => stmt.print(),
            _ => format!("{:?}", &self),
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
            Type(n) => n.check_types(symbols),
            Module(n) => n.check_types(symbols),
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
            Type(n) => n.compile(symbols),
            Module(n) => n.compile(symbols),
            NoOp => Ok("".to_string()),
            _ => panic!("Statement type compilation not supported yet."),
        }
    }

    pub fn compile_global(
        &self,
        symbols: &SymbolTable,
    ) -> Result<GlobalStatementObjectCode, errors::Error> {
        use Stmt::*;
        if TRACE {
            println!("Compiling global");
        }
        match self {
            Var(n) => n.compile_global(symbols),
            Fun(n) => n.compile_global(symbols),
            Type(n) => n.compile_global(symbols),
            Module(n) => n.compile_global(symbols),
            NoOp => Ok(GlobalStatementObjectCode::no_op()),
            _ => panic!("Statement type compilation not supported yet."),
        }
    }

    pub fn is_declaration(&self) -> bool {
        use Stmt::*;
        match self {
            Var(_) | Fun(_) | Type(_) | Module(_) => true,
            _ => false,
        }
    }

    pub fn declaration_name(&self) -> String {
        use Stmt::*;
        match self {
            Var(ref n) => n.name.clone(),
            Fun(ref n) => n.name.identifier_string().clone(),
            Type(ref n) => n.name.clone(),
            Module(ref n) => n.name.clone(),
            _ => panic!("Not a declaration statement. This was called in error during parsing or compilation because of a compiler bug."),
        }
    }

    //  qualify the name we're emitting with the  owning module
    // and make it something that can't clash with  other variable names.
    pub fn codegen_symbol(entry: &SymbolTableEntry) -> String {
        let mod_space = entry
            .module
            .iter()
            .map(|m| m.to_uppercase().clone())
            .collect::<Vec<String>>()
            .join("_MOD_");

        // Names in source may have '@' to show module locations
        // The symbol table entry will have the name used in source at the call site.
        //  Also it will have the '@' in the name for exported symbols from a module.
        let base_name = match entry.name.rsplit_once("@") {
            Some(n) => n.1.to_string(),
            None => entry.name.clone(),
        };
        mod_space + "_MOD_" + &base_name
    }
}

#[derive(Clone, Debug)]
pub struct PrintStmtNode {
    expressions: Vec<Expr>,
}

impl PrintStmtNode {
    fn print(&self) -> String {
        format!("print {:?}", &self.expressions)
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
                DataType::Str => format!("rci_value_to_c_str({})", &obj_code.code),
                DataType::Number => format!("rci_value_to_c_double({})", &obj_code.code),
                DataType::Bool => format!("rci_value_to_c_boolean({})", &obj_code.code),
                _ => "** PRINTING NOT SUPPORTED **".to_string(),
            };

            subst_codes = subst_codes + subst_code;
            values.push(printable);
        }

        let mut code = format!("printf(\"{}\",{});\n", &subst_codes, &values.join(","));
        Ok(code)
    }
}

#[derive(Clone, Debug)]
pub struct ExpressionStmtNode {
    expression: Expr,
}

impl ExpressionStmtNode {
    fn print(&self) -> String {
        format!("{}", &self.expression.print())
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
        let code = format!("{};", &expr_code.code);
        Ok(code)
    }
}

#[derive(Clone, Debug)]
pub struct ModuleNode {
    pub name: String, // the name of the module
    pub statements: Vec<Stmt>,
    // all public symbols will be added to the parent symbols table during parsing.
    // All public and private symbols will be in this symbol table as well, but
    // without the module name prefixing the symbols.
    pub symbols: SymbolTable,
}

impl ModuleNode {
    fn print(&self) -> String {
        format!("module {}", &self.name)
    }
}

impl TypeChecking for ModuleNode {
    fn check_types(&self, symbols: &SymbolTable) -> Result<(), errors::Error> {
        // TODO: For now just return error on the first
        // bad statement, but improve this to check them all.
        for stmt in &self.statements {
            stmt.check_types(&self.symbols)?
        }
        Ok(())
    }
}

impl Compiler for ModuleNode {
    fn compile(&self, symbols: &SymbolTable) -> Result<String, errors::Error> {
        let mut stmts = Vec::new();
        for stmt in &self.statements {
            let stmt_code = stmt.compile(&self.symbols)?;
            stmts.push(stmt_code);
        }
        let stmts_code = stmts.join("\n");

        let code = format!(
            "// BEGIN MODULE {}\n\t{}\n//END MODULE {}",
            &self.name, &stmts_code, &self.name
        );
        Ok(code)
    }

    fn compile_global(
        &self,
        symbols: &SymbolTable,
    ) -> Result<GlobalStatementObjectCode, errors::Error> {
        let code = GlobalStatementObjectCode {
            decl_type: DeclarationType::Val,
            base_code: self.compile(&symbols)?,
            decl_name: self.name.clone(),
            init_code: "".to_string(),
            init_name: "".to_string(),
        };
        Ok(code)
    }
}

#[derive(Clone, Debug)]
pub struct BlockStmtNode {
    statements: Vec<Stmt>,
    symbols: SymbolTable,
}

impl BlockStmtNode {
    fn print(&self) -> String {
        let stmts: String = self
            .statements
            .iter()
            .map(|s| s.print())
            .collect::<Vec<String>>()
            .join(";");

        format!("{{\n {}\n}}", &stmts)
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

impl IfStmtNode {
    fn print(&self) -> String {
        let else_stmt = if self.has_else {
            self.else_branch.print()
        } else {
            "".to_string()
        };
        format!(
            "if {} {{\n {}\n}} else{{ \n{}\n}}",
            &self.condition.print(),
            &*self.then_branch.print(),
            else_stmt
        )
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
            format!(
                "if (AS_BOOL({}))\n{} else\n{}\n",
                &cond_code.code, &then_branch_code, &else_branch_code
            )
        } else {
            format!("if (AS_BOOL({}))\n{}\n", &cond_code.code, &then_branch_code)
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

impl VarStmtNode {
    fn print(&self) -> String {
        format!("var-stmt = {}", &self.initializer.print())
    }
}

impl TypeChecking for VarStmtNode {
    fn check_types(&self, symbols: &SymbolTable) -> Result<(), errors::Error> {
        let init_type = self.initializer.determine_type(symbols)?;
        if self.data_type != init_type {
            let message = format!("Type '{}' specified for variable '{}' declaration doesn't match initializer expression type of '{}'",
				self.data_type, &self.name, init_type);

            return Err(errors::Error::new(&self.location, ErrorType::Type, message))
        }        
        Ok(())        
    }
}

impl Compiler for VarStmtNode {
    fn compile(&self, symbols: &SymbolTable) -> Result<String, errors::Error> {
        let entry = symbols
            .lookup(&self.name)
            .expect("Major compiler bug: Symbol should be in symbol table.");

        let generated_symbol = Stmt::codegen_symbol(&entry);
        let lhs = format!("rci_value {}", &generated_symbol);
        let rhs = self.initializer.compile(symbols)?;
        let code = format!("{} = {};", &lhs, &rhs.code);
        Ok(code)
    }

    fn compile_global(
        &self,
        symbols: &SymbolTable,
    ) -> Result<GlobalStatementObjectCode, errors::Error> {
        let ste = symbols
            .lookup(&self.name)
            .expect("Major compiler bug: Symbol should be in symbol table.");
        let generated_symbol = Stmt::codegen_symbol(&ste);
        let lhs = format!("rci_value {};\n", &generated_symbol);

        // make an initializer function to be called from an init_globals() function called first in main()
        let initializer_name = format!("__INITIALIZER_FOR_{}()", &self.name);
        let rhs = self.initializer.compile(symbols)?;
        let init_func = format!(
            "void {} {{\n {} = {}; \n }}\n",
            &initializer_name, &generated_symbol, &rhs.code
        );

        let code = GlobalStatementObjectCode {
            decl_type: ste.entry_type.clone(),
            base_code: lhs,
            decl_name: self.name.clone(),
            init_code: init_func,
            init_name: initializer_name,
        };
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

impl FunStmtNode {
    fn print(&self) -> String {
        format!("fun {}", &self.name.identifier_string())
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
        let local_function_name = self.name.token_type.print_value();
        let full_function_name = match symbols.entries.get(&local_function_name) {
            None => {
                for (k, v) in &symbols.entries {
                    eprintln!("{}  ->  {}", &k, &v.format_debug());
                }
                panic!(
                    "Internal error: can't find function with name {}",
                    &local_function_name
                );
            }
            Some(ref entry) => Stmt::codegen_symbol(entry),
        };

        let mut params_code: Vec<String> = Vec::new();
        for p in &self.params {
            let param_code = match p.entry_type {
                DeclarationType::Var => {
                    format!("rci_value * {}", &p.name)
                }
                DeclarationType::Val => {
                    format!("const rci_value {}", &p.name)
                }
                DeclarationType::Cpy => {
                    // NOTE: at the call site a deep copy should have been
                    // made by the compiler.
                    format!("rci_value {}", &p.name)
                }
                _ => {
                    panic!(
                        "Internal compiler error. Only val, var,, cpy allowed as param decl types."
                    )
                }
            };
            params_code.push(param_code);
        }

        let mut stmts_code: Vec<String> = Vec::new();
        for stmt in &self.body {
            let stmt_code = stmt.compile(&self.symbols)?;
            stmts_code.push(stmt_code);
        }

        let decl = format!(
            "rci_value {}({})",
            &full_function_name,
            &params_code.join(",")
        );

        let body = format!("{{\n{}\n}}", &stmts_code.join("\n"));
        Ok(format!("{}\n{}\n", &decl, &body))
    }

    fn compile_global(
        &self,
        symbols: &SymbolTable,
    ) -> Result<GlobalStatementObjectCode, errors::Error> {
        let function_name = self.name.token_type.print_value();
        let code = GlobalStatementObjectCode {
            decl_type: DeclarationType::Fun,
            base_code: self.compile(&symbols)?,
            decl_name: function_name.clone(),
            init_code: "".to_string(),
            init_name: function_name.clone(),
        };
        Ok(code)
    }
}

#[derive(Clone, Debug)]
pub struct ReturnStmtNode {
    location: Token,
    expr: Expr,
    return_type: DataType,
}

impl ReturnStmtNode {
    fn print(&self) -> String {
        format!("return-statement: {}", &self.expr.print())
    }
}
impl TypeChecking for ReturnStmtNode {
    fn check_types(&self, symbols: &SymbolTable) -> Result<(), errors::Error> {
        if TRACE {
            println!("Try to determine type of return-type expr: {:?}", self.expr);
        }
        let would_return_type = self.expr.determine_type(symbols)?;
        if TRACE {
            println!("resolved return-type expr {:?}", &would_return_type);
        }
        let should_return_type = match self.return_type {
            DataType::User(ref u) => match resolve_user_type(symbols, u.as_ref()) {
                Ok(data_type) => Ok(data_type),
                Err(not_declared) => Err(Error::new(
                    &self.location,
                    ErrorType::Type,
                    not_declared.message,
                )),
            },
            _ => Ok(self.return_type.clone()),
        }?;
        if TRACE {
            println!(
                "resolved return type self.return_type to {:?}",
                &should_return_type
            );
        }

        if would_return_type != should_return_type {
            let message = format!(
                "Type of return statement '{:?}' doesn't match function return type of '{:?}'.",
                &would_return_type, &should_return_type
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

impl BreakStmtNode {
    fn print(&self) -> String {
        "break-statement".to_string()
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

impl WhileStmtNode {
    fn print(&self) -> String {
        format!(
            "while-loop cond: {}, body: {}",
            &self.condition.print(),
            &self.body.print()
        )
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
        Ok(format!("while(AS_BOOL({}))\n{}", &cond.code, &body_code))
    }
}
#[derive(Clone, Debug)]
pub struct ProgramNode {
    declarations: Vec<Stmt>,
    imperatives: Box<Stmt>, // this is only allowed to be a block
}

impl ProgramNode {
    fn print(&self) -> String {
        "Program".to_string()
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
        let mut decls: Vec<GlobalStatementObjectCode> = Vec::new();
        if TRACE {
            println!(
                "Compile Program with {} declarations.",
                &self.declarations.len()
            );
        }

        for decl in &self.declarations {
            let decl_code = decl.compile_global(symbols)?;
            if TRACE {
                println!("Generating code: {}", &decl_code.base_code);
            }
            decls.push(decl_code);
        }

        let declarations_code = decls
            .iter()
            .map(|decl| match decl.decl_type {
                DeclarationType::Val | DeclarationType::Var => {
                    format!("{}\n{}\n", &decl.base_code, &decl.init_code)
                }
                _ => decl.base_code.clone(),
            })
            .collect::<Vec<String>>()
            .join("\n");

        let initializer_stmts: Vec<String> = decls
            .iter()
            .filter(|d| {
                matches!(d.decl_type, DeclarationType::Var)
                    || matches!(d.decl_type, DeclarationType::Val)
            })
            .map(|decl| format!("{};", &decl.init_name))
            .collect();

        let imperatives_code = self.imperatives.compile(symbols)?;
        let init_globals = format!(
            "void init_globals() {{\n {} \n}}\n",
            &initializer_stmts.join("\n")
        );

        let main_fn = format!(
            "int main(int argc, const char ** argv) {{\n\ttgc_start(&gc, &argc);\n{}\n{}\n\ttgc_stop(&gc);\n\treturn 0;\n}}",
            "\tinit_globals();\n", &imperatives_code
        );

        Ok(format!(
            "{}\n\n{}\n{}\n",
            &declarations_code, &init_globals, &main_fn
        ))
    }
}

#[derive(Clone, Debug)]
pub struct TypeNode {
    location: Token,
    name: String,
    definition: DataType,
}

impl TypeChecking for TypeNode {
    fn check_types(&self, symbols: &SymbolTable) -> Result<(), errors::Error> {
        Ok(())
    }
}

impl Compiler for TypeNode {
    fn compile(&self, symbols: &SymbolTable) -> Result<String, errors::Error> {
        let code = match self.definition {
            DataType::Enumeration(ref enumeration_type) => {
                let enumeration_list = enumeration_type
                    .items
                    .iter()
                    .map(|i| format!("{}_{}", &self.name, &i.value))
                    .collect::<Vec<String>>()
                    .join(",");
                let enum_decl = format!("enum {} {{ {} }};\n", &self.name, enumeration_list);

                let string_rep_list = enumeration_type
                    .items
                    .iter()
                    .map(|i| format!("\"{}\"", &i.value))
                    .collect::<Vec<String>>()
                    .join(",");
                let str_conversions = format!(
                    "static const char * {}_strings[] = {{ {} }};\n",
                    &self.name, &string_rep_list
                );
                format!("{}{}", &enum_decl, &str_conversions)
            }
            _ => format!(
                " // Type '{}' requires no IR source for current language features",
                &self.name
            ),
        };
        Ok(code)
    }

    fn compile_global(
        &self,
        symbols: &SymbolTable,
    ) -> Result<GlobalStatementObjectCode, errors::Error> {
        let code = GlobalStatementObjectCode {
            decl_type: DeclarationType::Type,
            base_code: self.compile(symbols)?,
            decl_name: self.name.clone(),
            init_code: "".to_string(),
            init_name: self.name.clone(),
        };
        Ok(code)
    }
}

impl Stmt {
    pub fn type_decl(location: &Token, name: &str, definition: &DataType) -> Stmt {
        Stmt::Type(TypeNode {
            location: location.clone(),
            name: name.to_string(),
            definition: definition.clone(),
        })
    }

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
