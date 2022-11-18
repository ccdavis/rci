use crate::errors;
use crate::errors::compiler_err;
use crate::errors::*;
use crate::lex::Token;
use crate::lex::TokenType;
use crate::symbol_table::*;
use crate::types::DataType;
use crate::types::DataValue;
use crate::types::DeclarationType;
use crate::types::LookupType;
use crate::types::ObjectCode;

const TRACE: bool = false;

pub trait TypeCheck {
    // This is for after the program has been fully parsed and all symbols are known.
    fn determine_type(&self, symbols: &SymbolTable) -> Result<DataType, errors::Error>;
}

pub trait Compiler {
    fn compile(&self, symbols: &SymbolTable) -> Result<ObjectCode, errors::Error>;
}
#[derive(Clone, Debug)]
pub enum Expr {
    Binary(BinaryNode),
    Logical(LogicalNode),
    Call(CallNode),
    Lookup(LookupNode),
    Unary(UnaryNode),
    Grouping(GroupingNode),
    Array(ArrayNode),
    Literal(DataValue),
    UserTypeLiteral(UserTypeLiteralNode),
    Variable(VariableNode),
    Setter(SetterNode), // record instance . field =
    Getter(GetterNode), // record instance. field
    Assignment(AssignmentNode),
}

impl Expr {
    // I don't love this matching. It would be nice to have varients as types (refinement types)
    // Discussion: https://www.reddit.com/r/rust/comments/2rdoxx/enum_variants_as_types/

    pub fn determine_type(&self, symbols: &SymbolTable) -> Result<DataType, errors::Error> {
        match self {
            Expr::Binary(n) => n.determine_type(symbols),
            Expr::Logical(n) => n.determine_type(symbols),
            Expr::Call(n) => n.determine_type(symbols),
            Expr::Lookup(n) => n.determine_type(symbols),
            Expr::Unary(n) => n.determine_type(symbols),
            Expr::Grouping(n) => n.determine_type(symbols),
            Expr::Array(n) => n.determine_type(symbols),
            Expr::Variable(n) => n.determine_type(symbols),
            Expr::Getter(n) => n.determine_type(symbols),
            //Expr::Setter(n) => n.determine_type(symbols),
            Expr::Assignment(n) => n.determine_type(symbols),
            Expr::Literal(value) => Ok(DataType::from_data_value(value)),
            Expr::UserTypeLiteral(n) => n.determine_type(symbols),
            _ => panic!("determine_type not implemented!"),
        }
    }

    pub fn compile(&self, symbols: &SymbolTable) -> Result<ObjectCode, errors::Error> {
        match self {
            Expr::Binary(n) => n.compile(symbols),
            Expr::Logical(n) => n.compile(symbols),
            Expr::Call(n) => n.compile(symbols),
            Expr::Lookup(n) => n.compile(symbols),
            Expr::Unary(n) => n.compile(symbols),
            Expr::Grouping(n) => n.compile(symbols),
            Expr::Array(n) => n.compile(symbols),
            Expr::Variable(n) => n.compile(symbols),
            Expr::Getter(n) => n.compile(symbols),
            Expr::Assignment(n) => n.compile(symbols),
            Expr::UserTypeLiteral(n) => n.compile(symbols),
            Expr::Literal(ref value) => {
                let literal_type = DataType::from_data_value(value);
                let object_code = match value {
                    DataValue::Str(ref v) => format!("(rci_value) string_literal(\"{}\")", &v),
                    DataValue::Number(n) => format!("(rci_value) NUMBER_VAL({})", n),
                    DataValue::Bool(b) => format!("(rci_value)BOOL_VAL({})", b),
                    _ => panic!("Literal compilation not implemented for {:?}", value),
                };

                Ok(ObjectCode {
                    data_type: literal_type,
                    code: object_code,
                })
            }
            _ => panic!("Compile not implemented!"),
        }
    }

    // Callees get apssed around as Expr type values but sometimes we need to know if they
    // are names of variables or names of user types.
    pub fn is_type_name(&self) -> bool {
        match self {
            Expr::Variable(v) => v
                .name
                .identifier_string()
                .chars()
                .next()
                .unwrap()
                .is_uppercase(),
            _ => false,
        }
    }
} // impl expr

#[derive(Clone, Debug)]
pub struct BinaryNode {
    left: Box<Expr>,
    operator: Token,
    right: Box<Expr>,
}

impl TypeCheck for BinaryNode {
    fn determine_type(&self, symbols: &SymbolTable) -> Result<DataType, errors::Error> {
        use TokenType::*;
        let left_type = self.left.determine_type(symbols)?;
        if TRACE {
            println!("In binary type check left: {:?}", &left_type);
        }
        let right_type = self.right.determine_type(symbols)?;
        if TRACE {
            println!("In binary type check right: {:?}", &right_type);
        }

        if self.operator.is_comparison_operator() {
            if matches!(left_type, DataType::Number) && matches!(right_type, DataType::Number) {
                return Ok(DataType::Bool);

                // Allow boolean comparison with = or <>
            } else if matches!(left_type, DataType::Enumeration(_))
                && matches!(right_type, DataType::Enumeration(_))
                && (matches!(self.operator.token_type, Equal)
                    || matches!(self.operator.token_type, LessGreater))
            {
                return Ok(DataType::Bool);
            } else if matches!(left_type, DataType::Bool)
                && matches!(right_type, DataType::Bool)
                && (matches!(self.operator.token_type, Equal)
                    || matches!(self.operator.token_type, LessGreater))
            {
                return Ok(DataType::Bool);

            // TODO: support String comparison
            } else {
                let message = format!(
                    "Invalid operand types for operator {}",
                    &self.operator.print()
                );
                return Err(Error::new(&self.operator, ErrorType::Type, message));
            }
        }

        if self.operator.is_arithmetic_operator() {
            // Allow string concatenation with '+'
            if matches!(self.operator.token_type, Plus) {
                if matches!(left_type, DataType::Str) && matches!(right_type, DataType::Str) {
                    return Ok(DataType::Str);
                }
            } // allow + for strings

            if matches!(left_type, DataType::Number) && matches!(right_type, DataType::Number) {
                return Ok(DataType::Number);
            } else {
                let message = format!(
                    "Invalid operand types for operator {}, expected Numbers.",
                    &self.operator.print()
                );
                return Err(Error::new(&self.operator, ErrorType::Type, message));
            }
        }

        let message = format!(
            "Operator should not be part of a binary expression:{:}",
            &self.operator.print()
        );
        Err(Error::new(&self.operator, ErrorType::Type, message))
    }
} // impl

impl Compiler for BinaryNode {
    fn compile(&self, symbols: &SymbolTable) -> Result<ObjectCode, errors::Error> {
        let data_type = self.determine_type(symbols)?;
        let left = self.left.compile(symbols)?;
        let right = self.right.compile(symbols)?;
        let op = match self.operator.token_type {
            TokenType::Plus => "_ADD_",
            TokenType::Minus => "_SUB_",
            TokenType::Star => "_MUL_",
            TokenType::Slash => "_DIV_",
            TokenType::Less => "_LT_",
            TokenType::Greater => "_GT_",
            TokenType::LessEqual => "<=",
            TokenType::GreaterEqual => ">=",
            TokenType::Equal => "_EQ_",
            TokenType::LessGreater => "_NE_",
            _ => panic!(
                "Compilation error, operator not supported yet: {}",
                &self.operator.token_type.print()
            ),
        };

        if matches!(data_type, DataType::Str) {
            if matches!(self.operator.token_type, TokenType::Plus) {
                return Ok(ObjectCode {
                    data_type,
                    code: format!("string_concat({}, {})", &left.code, &right.code),
                });
            } else {
                let msg = format!("Operator {} not supported for string type.", &op);
                return Err(Error::new(&self.operator, ErrorType::Compiler, msg));
            }
        }
        Ok(ObjectCode {
            data_type,
            code: format!("binary_operation({}, {}, {})", &op, &left.code, &right.code),
        })
    }
}

#[derive(Clone, Debug)]
pub struct LogicalNode {
    left: Box<Expr>,
    operator: Token,
    right: Box<Expr>,
}

impl TypeCheck for LogicalNode {
    fn determine_type(&self, symbols: &SymbolTable) -> Result<DataType, errors::Error> {
        let right_type = self.right.determine_type(symbols)?;
        let left_type = self.left.determine_type(symbols)?;
        if matches!(left_type, DataType::Bool) && matches!(right_type, DataType::Bool) {
            Ok(DataType::Bool)
        } else {
            let message = format!(
                "Operands of a logical operator must be boolean. Got {}, {} instead.",
                &left_type, &right_type
            );
            Err(Error::new(&self.operator, ErrorType::Type, message))
        }
    }
}

impl Compiler for LogicalNode {
    fn compile(&self, symbols: &SymbolTable) -> Result<ObjectCode, errors::Error> {
        let data_type = self.determine_type(symbols)?;
        let left = self.left.compile(symbols)?;
        let right = self.right.compile(symbols)?;
        let op = match self.operator.token_type {
		  // TODO the 'or' operator is control flow, really needs correct implementation!
			TokenType::And => "&&",
			TokenType::Or => "||",
			_ => panic!("Code generation error, operator not a logical operator: '{}'. The compiler never should have reached this point.",
						&self.operator.print()),
		  };

        Ok(ObjectCode {
            data_type,
            code: format!(
                "BOOL_VAL( AS_BOOL({}) {} AS_BOOL({}))",
                &left.code, op, &right.code
            ),
        })
    }
}

// a 'getter' is any of VAR.FIELD, VAR.FIELD[index], VAR.FUNCTION()
#[derive(Clone, Debug)]
pub struct GetterNode {
    pub callee: Box<Expr>,
    pub dot: Token,
    pub getter: Box<Expr>,
}

impl TypeCheck for GetterNode {
    fn determine_type(&self, symbols: &SymbolTable) -> Result<DataType, errors::Error> {
        let callee_type = self.callee.determine_type(symbols)?;
        if let DataType::Record(rec_type) = callee_type {
            match *self.getter {
                Expr::Variable(ref g) => {
                    let field_name = g.get_name();
                    rec_type.type_of_field(&field_name, &g.name)
                }
                _ => self.getter.determine_type(symbols),
            }
        } else {
            panic!("Compiler error, only Rec type can have a '.' getter, this code shouldn't have been parsed.");
        }
    }
}

impl Compiler for GetterNode {
    fn compile(&self, symbols: &SymbolTable) -> Result<ObjectCode, errors::Error> {
        let record_var_name = match *self.callee {
            Expr::Variable(ref v) => v.get_name(),
            _ => panic!("Compiler error. Only Variable nodes can have a getter"),
        };

        match *self.getter {
            Expr::Variable(ref v) => {
                let field_name = v.get_name();
                let callee_type = self.callee.determine_type(symbols)?;

                let field_index = match callee_type {
					DataType::Record(ref rec) =>rec.index_of_field(&field_name).unwrap(),
					_ => panic!("Compiler error. Only record type instances currently supported by compiler."),
				};
                let code = format!(
                    "record_access_member({}, {})",
                    &record_var_name, field_index
                );
                Ok(ObjectCode {
                    data_type: callee_type.clone(),
                    code,
                })
            }
            Expr::Getter(ref g) => {
                /*
                let inner_code = self.getter.compile()?;

                Ok( ObjectCode {
                    data_type: callee_type,
                    code:
                })
                */
                panic!("Compiler error. Chained getters not yet implemented.");
            }
            _ => panic!("A getter must be a variable node or another getter."),
        }
    }
}

#[derive(Clone, Debug)]
pub struct SetterNode {
    pub name: Box<Expr>,
    pub dot: Token,
    pub value: Box<Expr>,
}

#[derive(Clone, Debug)]
pub struct CallNode {
    callee: Box<Expr>,
    paren: Token, // To locate the call in the source
    args: Vec<Expr>,
}

impl TypeCheck for CallNode {
    fn determine_type(&self, symbols: &SymbolTable) -> Result<DataType, errors::Error> {
        let callee_return_type = self.callee.determine_type(symbols)?;
        Ok(callee_return_type)
    }
}

impl Compiler for CallNode {
    fn compile(&self, symbols: &SymbolTable) -> Result<ObjectCode, errors::Error> {
        let callee_return_type = self.callee.determine_type(symbols)?;

        // The 'callee' is an expression but it really just resolves to the
        // name of the function to be called; it could be expanded to be a
        // class-instance.method() call or other things as well.
        let function_name = self.callee.compile(symbols)?;
        let function_ste = symbols.lookup(&function_name.code).unwrap();
        if self.args.len() != function_ste.fields.len() {
            let msg = format!(
                "Symbol table definition of function {} and call site don't match arrity!",
                &function_name.code
            );
            return compiler_err(&self.paren, &msg);
        }

        let mut arguments: Vec<String> = Vec::new();
        for (arg_num, arg_expr) in self.args.iter().enumerate() {
            // For 'var' params, we can pass in only variables, not other expressions, and the
            // variable must have a 'var' declaration type.
            let mut var_name = "".to_string();
            let mut is_var_decl = false;
            let is_variable_expr = if let Expr::Variable(ref var_node) = arg_expr {
                var_name = var_node.name.identifier_string();
                let var_ste = symbols.lookup(&var_name);
                if var_ste.is_err() {
                    return compiler_err(
                        &self.paren,
                        &format!("{} not in symbol table!", &var_name),
                    );
                }
                is_var_decl = matches!(var_ste.unwrap().entry_type, DeclarationType::Var);
                true
            } else {
                false
            };

            let arg = arg_expr.compile(symbols)?;
            let param_definition = function_ste.fields.get(arg_num).unwrap();
            if matches!(param_definition.entry_type, DeclarationType::Var) {
                if !is_var_decl && is_variable_expr {
                    let msg = &format!("parameter '{}' on function '{}' is a 'var' type, but the variable '{}' is not 'var'.",
                        &param_definition.name,&function_ste.name,&var_name);
                    return compiler_err(&self.paren, msg);
                }
                if !is_variable_expr {
                    let msg = &format!("Argument to parameter '{}' on function '{}' must be a variable with a 'var' declaration type.",
						&param_definition.name,&function_ste.name);
                    return compiler_err(&self.paren, msg);
                }
                arguments.push("& ".to_string() + &arg.code);
            } else {
                arguments.push(arg.code);
            }
        }

        let args_list = arguments.join(", ");
        Ok(ObjectCode {
            data_type: callee_return_type,
            code: format!("{}({})", &function_name.code, &args_list),
        })
    }
}

#[derive(Clone, Debug)]
pub struct UserTypeLiteralNode {
    type_name: Box<Expr>,
    location: Token,
    field_names: Vec<String>,
    field_values: Vec<Expr>,
    field_keys: Vec<DataValue>,
}

impl TypeCheck for UserTypeLiteralNode {
    fn determine_type(&self, symbols: &SymbolTable) -> Result<DataType, errors::Error> {
        let name = match *self.type_name {
            Expr::Variable(ref v) => v.name.identifier_string().clone(),
            _ => panic!(
                "Compiler error in type checking (determine_type) for User Type '{:?}'",
                self.type_name
            ),
        };

        let user_type = match symbols.lookup(&name) {
            Ok(ref type_definition_ste) => Ok(type_definition_ste.data_type.clone()),
            Err(msg) => Err(Error::new(
                &self.location,
                ErrorType::Type,
                format!("No type '{}' declared.", &name),
            )),
        };

        // Check field types
        if let Ok(ref this_type) = user_type {
            match this_type {
                DataType::Record(ref record) => {
                    for (index, field_name) in self.field_names.iter().enumerate() {
                        let f = record.fields.iter().find(|field| &field.name == field_name);
                        // The field the user used in the constructor / literal isn't in the type
                        if f.is_none() {
                            return Err(Error::new(
                                &self.location,
                                ErrorType::Type,
                                format!("No field named  '{}' for type {}.", &field_name, &name),
                            ));
                        }

                        // Check that the types match between what the value assigned to the field is and
                        // what the field's defined type is.
                        // It's also possible for determine_type() to fail and a match can't be performed.
                        // In principle this shouldn't happen but for now we have to check.
                        if let Some(field_info) = f {
                            let field_value = self.field_values[index].clone();
                            match field_value.determine_type(symbols) {
                                Ok(ref value_type) => {
                                    if value_type != &field_info.field_type {
                                        return Err(Error::new(&self.location, ErrorType::Type,
                                            format!("Type '{}' value assigned to field '{}' of '{}' didn't match.Expected '{}'",
                                            &value_type, &field_info.name,&name,field_info.field_type)));
                                    }
                                }
                                Err(msg) => {
                                    return Err(Error::new(&self.location, ErrorType::Type,
                                        format!("Type of value assigned to field '{}' of '{}' could not be determined. Problem was '{}'",
                                        &field_name, &name, &msg.message)));
                                }
                            }
                        }
                    }
                }
                _ => panic!("Not implemented for '{:?}'", this_type),
            }
        }
        user_type
    }
}

impl Compiler for UserTypeLiteralNode {
    fn compile(&self, symbols: &SymbolTable) -> Result<ObjectCode, errors::Error> {
        let mut code: String = format!("record_new({}, ", &self.field_values.len());
        let data_type = self.determine_type(symbols)?;
        for (index, name) in self.field_names.iter().enumerate() {
            let field_value = self.field_values[index].clone();
            code = code + &field_value.compile(symbols)?.code;
            if index < self.field_names.len() - 1 {
                code = code + ", ";
            }
        }
        code = code + &format!(")");

        Ok(ObjectCode { data_type, code })
    }
}

#[derive(Clone, Debug)]
pub struct LookupNode {
    callee: Box<Expr>,
    bracket: Token, // To locate the lookup in the source
    index: Box<Expr>,
}

impl TypeCheck for LookupNode {
    fn determine_type(&self, symbols: &SymbolTable) -> Result<DataType, errors::Error> {
        if TRACE {
            println!("In lookup type checker");
        }
        let callee_return_type = self.callee.determine_type(symbols)?;

        if let DataType::Lookup(lookup_variety) = callee_return_type {
            let index_type = self.index.determine_type(symbols)?;
            match *lookup_variety {
                LookupType::DirectMap {
                    index_type: t,
                    contains_type: c,
                    ..
                } => {
                    if index_type != t {
                        let message = format!("Index type doesn't match DirectMap index type");
                        return Err(Error::new(&self.bracket, ErrorType::Type, message));
                    }
                    Ok(c)
                }
                LookupType::HashedMap {
                    index_type: t,
                    contains_type: c,
                    ..
                } => {
                    if index_type != t {
                        let message = format!("Index type doesn't match HashedMap index type");
                        return Err(Error::new(&self.bracket, ErrorType::Type, message));
                    }
                    Ok(c)
                }
                LookupType::Array {
                    index_type: t,
                    contains_type: c,
                    ..
                } => {
                    if index_type != t {
                        let message = format!("Index type doesn't match Vector index type");
                        return Err(Error::new(&self.bracket, ErrorType::Type, message));
                    }
                    Ok(c)
                }
                LookupType::Vector {
                    index_type: t,
                    contains_type: c,
                    ..
                } => {
                    if index_type != t {
                        let message = format!("Index type doesn't match Array index type");
                        return Err(Error::new(&self.bracket, ErrorType::Type, message));
                    }
                    Ok(c)
                }
                _ => {
                    let message = format!("Lookups  type expected.");
                    Err(Error::new(&self.bracket, ErrorType::Type, message))
                }
            }
        } else {
            let message = format!("Lookups  type expected.");
            Err(Error::new(&self.bracket, ErrorType::Type, message))
        }
    }
}

impl Compiler for LookupNode {
    fn compile(&self, symbols: &SymbolTable) -> Result<ObjectCode, errors::Error> {
        Ok(ObjectCode {
            data_type: DataType::Unresolved,
            code: " // unimplemented ".to_string(),
        })
    }
}

#[derive(Clone, Debug)]
pub struct GroupingNode {
    expr: Box<Expr>,
}

impl TypeCheck for GroupingNode {
    fn determine_type(&self, symbols: &SymbolTable) -> Result<DataType, errors::Error> {
        self.expr.determine_type(symbols)
    }
}

impl Compiler for GroupingNode {
    fn compile(&self, symbols: &SymbolTable) -> Result<ObjectCode, errors::Error> {
        let group_type = self.expr.determine_type(symbols)?;
        let group_expr_code = self.expr.compile(symbols)?;
        Ok(ObjectCode {
            data_type: group_type,
            code: format!("( {} )", &group_expr_code.code),
        })
    }
}

#[derive(Clone, Debug)]
pub struct ArrayNode {
    location: Token,
    elements: Vec<Expr>,
}

impl TypeCheck for ArrayNode {
    fn determine_type(&self, symbols: &SymbolTable) -> Result<DataType, errors::Error> {
        if self.elements.len() == 0 {
            return Ok(DataType::Unresolved);
        }

        let first_type = self.elements[0].determine_type(symbols)?;
        let mut last_type = first_type.clone();
        let array_length = self.elements.len();
        let low_ind = DataValue::Number(0.0);
        let high_ind = DataValue::Number(array_length as f64 - 1.0);

        // Use enumerate to give an index to any error messages?
        for (index, t) in self.elements.iter().enumerate() {
            let this_type = t.determine_type(symbols)?;
            if this_type != last_type {
                let message = format!(
                    "Types in array elements differ: {} and {}",
                    index - 1,
                    index
                );
                return Err(Error::new(&self.location, ErrorType::Type, message));
            }
            last_type = this_type;
        }
        Ok(DataType::Lookup(Box::new(LookupType::Array {
            contains_type: first_type.clone(),
            size: Some(array_length),
            index_type: DataType::Number,
            low_index: Some(Box::new(low_ind)),
            high_index: Some(Box::new(high_ind)),
        })))
    }
}

impl Compiler for ArrayNode {
    fn compile(&self, symbols: &SymbolTable) -> Result<ObjectCode, errors::Error> {
        Ok(ObjectCode {
            data_type: DataType::Unresolved,
            code: " // unimplemented ".to_string(),
        })
    }
}

#[derive(Clone, Debug)]
pub struct UnaryNode {
    operator: Token,
    expr: Box<Expr>,
}

impl TypeCheck for UnaryNode {
    fn determine_type(&self, symbols: &SymbolTable) -> Result<DataType, errors::Error> {
        use TokenType::*;
        let right_type = self.expr.determine_type(symbols)?;
        if matches!(right_type, DataType::Number) && matches!(self.operator.token_type, Minus) {
            return Ok(DataType::Number);
        }

        if matches!(right_type, DataType::Bool) && matches!(self.operator.token_type, Not) {
            return Ok(DataType::Bool);
        }

        let message = format!(
            "Type / operator mismatch on unary operator {:?} with operand type {}",
            self.operator, right_type
        );

        Err(Error::new(&self.operator, ErrorType::Type, message))
    }
}

impl Compiler for UnaryNode {
    fn compile(&self, symbols: &SymbolTable) -> Result<ObjectCode, errors::Error> {
        use TokenType::*;
        let compiled_expr = self.expr.compile(symbols)?;

        let code = match self.operator.token_type {
            Not => format!("unary_operation(_NOT_,{})", &compiled_expr.code),
            Minus => format!("unary_operation(_NEGATIVE_,{})", &compiled_expr.code),
            _ => panic!("Compiler error. Only '-' or 'not' should be in a unary expression."),
        };

        Ok(ObjectCode {
            data_type: compiled_expr.data_type,
            code,
        })
    }
}

#[derive(Clone, Debug)]
pub struct VariableNode {
    pub name: Token,
    pub distance: Option<usize>,
    pub index: Option<usize>,
}

pub fn data_type_for_symbol(symbols: &SymbolTable, name: &str) -> Result<DataType, String> {
    match symbols.lookup(name) {
        Ok(ref symbol_table_entry) => {
            if TRACE {
                println!("Found symbol {}", &name);
            }
            if TRACE {
                println!("Type of variable is '{:?}'", symbol_table_entry.data_type);
            }
            if let DataType::User(ref user_type) = symbol_table_entry.data_type {
                if matches!(DataType::Unresolved, user_type) {
                    match symbols.outer {
                        Some(ref outer_scope) => data_type_for_symbol(outer_scope, &user_type.name),
                        None => Err(format!("Type named {} not defined.", &user_type.name)),
                    }
                } else {
                    Ok(*user_type.definition.clone())
                }
            } else {
                Ok(symbol_table_entry.data_type.clone())
            }
        }
        Err(declaration_error) => Err(format!("{}", &declaration_error.message)),
    }
}

impl VariableNode {
    fn get_name(&self) -> String {
        if let TokenType::Identifier(ref variable_name) = self.name.token_type {
            variable_name.clone()
        } else {
            panic!("Compiler error. A variable node must have an identifier token type.");
        }
    }
}

impl TypeCheck for VariableNode {
    // Doing this right requires a symbol table built up from
    // visiting all the variable declarations and using the right
    // lexical scoping rules.

    fn determine_type(&self, symbols: &SymbolTable) -> Result<DataType, errors::Error> {
        if TRACE {
            println!("Check variable node type...");
        }
        let variable_name = self.get_name();
        if TRACE {
            println!("Got name of variable {}", &variable_name);
        }
        match data_type_for_symbol(symbols, &variable_name) {
            Ok(data_type) => Ok(data_type),
            Err(msg) => Err(Error::new(&self.name, ErrorType::Type, msg)),
        }
    }
}

impl Compiler for VariableNode {
    fn compile(&self, symbols: &SymbolTable) -> Result<ObjectCode, errors::Error> {
        let var_type = self.determine_type(symbols)?;
        if TRACE {
            println!("var type for {} is {}", &self.name.print(), &var_type)
        }
        let var_name = self.get_name();
        if TRACE {
            println!("var name is {}", &var_name);
        }
        let ste = match symbols.lookup(&var_name) {
            Ok(ref entry) => entry.clone(),
            Err(error_msg) => {
                return Err(errors::Error::new(
                    &self.name,
                    ErrorType::Compiler,
                    error_msg.message.clone(),
                ));
            }
        };
        if TRACE {
            println!("ste: {:?}", &ste);
        }

        Ok(ObjectCode {
            data_type: var_type,
            code: {
                if ste.is_arg && matches!(ste.entry_type, DeclarationType::Var) {
                    format!("(*{})", &var_name)
                // If the STE has a data_value of EnumerationValue it's an enum literal
                } else if let DataValue::Enumeration(ref enum_value) = ste.data_value {
                    format!(
                        "ENUM_VAL({}_{})",
                        &enum_value.member_of_enum, &enum_value.value
                    )
                } else {
                    format!("{}", &var_name)
                }
            },
        })
    }
}

#[derive(Clone, Debug)]
pub struct AssignmentNode {
    name: Token,
    value: Box<Expr>,
    distance: Option<usize>,
    index: Option<usize>,
}

impl TypeCheck for AssignmentNode {
    // The result of an assignment is an Empty type; this is mostly useful for
    // catching errors.
    //
    // Also, the Emptytype is useful for type checking places where assignment
    // is unintentionally used like an 'if' statement:
    //
    //	if x := 9 { .. do something ... } will not execute.
    //
    // Instead of true or false the Empty in this position  means an error gets thrown,
    // and we can even check at parsing time for this issue.
    // compare assign_type with the type of the variable
    fn determine_type(&self, symbols: &SymbolTable) -> Result<DataType, errors::Error> {
        let assign_type = self.value.determine_type(symbols)?;

        // get variable / assignee name
        let assignee_name = match  self.name.token_type {
			TokenType::Identifier(ref n) => n.clone(),
			_ => panic!("Fatal error during type-checking. Assignment node must have a TokenType::Identifier(name) for the name field."),
		};

        let to_type = match symbols.lookup(&assignee_name) {
            Ok(ref ste) => {
                if matches!(ste.entry_type, DeclarationType::Val) {
                    let message = format!("Can't assign to a 'val'. Only 'var' is mutable.");

                    return Err(Error::new(&self.name, ErrorType::Type, message));
                }
                ste.data_type.clone()
            }
            Err(not_declared) => {
                let message = format!("{}", &not_declared.message);
                return Err(Error::new(&self.name, ErrorType::Type, message));
            }
        };

        if assign_type == to_type {
            Ok(DataType::Empty)
        } else {
            let message = format!(
                "Can't assign {} with type {} to a value of type {}.",
                &assignee_name, &to_type, &assign_type
            );
            Err(Error::new(&self.name, ErrorType::Type, message))
        }
    }
}

impl Compiler for AssignmentNode {
    fn compile(&self, symbols: &SymbolTable) -> Result<ObjectCode, errors::Error> {
        let value_to_store = self.value.compile(symbols)?;
        let assignee_name = match  self.name.token_type {
			TokenType::Identifier(ref n) => n.clone(),
			_ => panic!("Fatal error during type-checking. Assignment node must have a TokenType::Identifier(name) for the name field."),
		};

        let code = match value_to_store.data_type {
            DataType::Str => {
                format!(
                    "{} = string_assign({},{});",
                    &assignee_name, &assignee_name, &value_to_store.code
                )
            }
            _ => format!("{} = {}", &assignee_name, &value_to_store.code),
        };

        Ok(ObjectCode {
            data_type: value_to_store.data_type,
            code,
        })
    }
}

impl Expr {
    pub fn binary(l: Expr, op: Token, r: Expr) -> Expr {
        let node = BinaryNode {
            left: Box::new(l),
            operator: op,
            right: Box::new(r),
        };
        Expr::Binary(node)
    }

    pub fn logical(l: Expr, op: Token, r: Expr) -> Expr {
        Expr::Logical(LogicalNode {
            left: Box::new(l),
            operator: op,
            right: Box::new(r),
        })
    }

    pub fn call(callee: Expr, paren: Token, args: Vec<Expr>) -> Expr {
        let node = CallNode {
            callee: Box::new(callee),
            paren,
            args,
        };
        Expr::Call(node)
    }

    pub fn lookup(callee: Expr, bracket: Token, index: Expr) -> Expr {
        Expr::Lookup(LookupNode {
            callee: Box::new(callee),
            bracket,
            index: Box::new(index),
        })
    }

    pub fn unary(op: Token, e: Expr) -> Expr {
        let node = UnaryNode {
            operator: op,
            expr: Box::new(e),
        };
        Expr::Unary(node)
    }

    pub fn literal(value: Token) -> Expr {
        let data_value = DataValue::from_token_type(&value.token_type);
        Expr::Literal(data_value)
    }

    pub fn record_type_literal(
        type_name: Expr,
        location: Token,
        field_names: Vec<String>,
        field_values: Vec<Expr>,
    ) -> Expr {
        let field_keys: Vec<DataValue> = Vec::new();
        let node = UserTypeLiteralNode {
            type_name: Box::new(type_name),
            location,
            field_names,
            field_values,
            field_keys,
        };

        Expr::UserTypeLiteral(node)
    }

    pub fn array(location: Token, elements: Vec<Expr>) -> Expr {
        Expr::Array(ArrayNode { location, elements })
    }

    pub fn grouping(e: Expr) -> Expr {
        Expr::Grouping(GroupingNode { expr: Box::new(e) })
    }

    pub fn variable(name: Token, distance: Option<usize>, index: Option<usize>) -> Expr {
        Expr::Variable(VariableNode {
            name,
            distance,
            index,
        })
    }

    pub fn assignment(
        name: Token,
        new_value: Expr,
        distance: Option<usize>,
        index: Option<usize>,
    ) -> Expr {
        Expr::Assignment(AssignmentNode {
            name,
            distance,
            index,
            value: Box::new(new_value),
        })
    }

    pub fn is_literal(&self) -> bool {
        matches!(self, Expr::Literal(_))
    }

    fn parenthesize(inside: String) -> String {
        "(".to_string() + &inside + ")"
    }

    pub fn print(&self) -> String {
        use Expr::*;
        let inside = match self {
            Binary(n) => {
                format!(
                    "{} {} {}",
                    &n.left.print(),
                    &n.operator.print(),
                    &n.right.print()
                )
            }
            Unary(n) => {
                format!("{} {}", &n.operator.print(), &n.expr.print())
            }
            Literal(ref n) => {
                format!("")
            }
            Grouping(n) => {
                format!("{}", &n.expr.print())
            }
            Variable(n) => {
                format!("")
            }
            Assignment(n) => {
                format!("")
            }
            Call(n) => {
                format!("")
            }
            Logical(n) => {
                format!("")
            }

            _ => panic!("Not implemented"),
        };
        Expr::parenthesize(inside)
    }
}
