use crate::environment;
use crate::environment::EnvNode;
use crate::environment::EnvRc;
use crate::errors;
use crate::errors::*;
use crate::errors::compiler_err;
use crate::lex::Token;
use crate::lex::TokenType;
use crate::operations;
use crate::symbol_table::*;
use crate::types::DataType;
use crate::types::DataValue;
use crate::types::DeclarationType;
use crate::types::ObjectCode;
use crate::types::ReturnValue;
use crate::types::*;

use std::rc::Rc;

const TRACE: bool = false;

pub trait Evaluation {
    // return the name  of the operation if any and
    // associated expressions.
    fn print(&self) -> String;
    fn evaluate(&self, envr: &EnvRc) -> Result<ReturnValue, errors::Error>;
}

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
    Literal(ReturnValue),
    Variable(VariableNode),
    Assignment(AssignmentNode),
}

impl Expr {
    pub fn evaluate(&self, envr: &EnvRc) -> Result<ReturnValue, errors::Error> {
        match self {
            Expr::Binary(n) => n.evaluate(envr),
            Expr::Logical(n) => n.evaluate(envr),
            Expr::Call(n) => n.evaluate(envr),
            Expr::Lookup(n) => n.evaluate(envr),
            Expr::Unary(n) => n.evaluate(envr),
            Expr::Grouping(n) => n.evaluate(envr),
            Expr::Array(n) => n.evaluate(envr),
            Expr::Variable(n) => n.evaluate(envr),
            Expr::Assignment(n) => n.evaluate(envr),
            Expr::Literal(ref value) => Ok(value.clone_or_increment_count()),
        }
    }

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
            Expr::Assignment(n) => n.determine_type(symbols),
            Expr::Literal(value) => Ok(DataType::from_data_value(value.get())),
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
            Expr::Assignment(n) => n.compile(symbols),
            Expr::Literal(n) => LiteralNode::compile(n, symbols),                
            
        }
    }
} // impl expr

#[derive(Clone, Debug)]
pub struct BinaryNode {
    left: Box<Expr>,
    operator: Token,
    right: Box<Expr>,
}

impl Evaluation for BinaryNode {
    fn print(&self) -> String {
        format!(
            "{} {} {}",
            &self.operator.print(),
            self.left.print(),
            self.right.print()
        )
    }

    fn evaluate(&self, envr: &EnvRc) -> Result<ReturnValue, errors::Error> {
        use TokenType::*;
        let left = self.left.evaluate(envr)?;
        let right = self.right.evaluate(envr)?;
        let left_value = left.get();
        let right_value = right.get();
        let result = match self.operator.token_type {
            Plus => operations::add(left_value, right_value),
            Minus => operations::subtract(left_value, right_value),
            Slash => operations::divide(left_value, right_value),
            Star => operations::multiply(left_value, right_value),
            Greater => operations::compare_gt(left_value, right_value),
            Less => operations::compare_lt(left_value, right_value),
            GreaterEqual => operations::compare_gte(left_value, right_value),
            LessEqual => operations::compare_lte(left_value, right_value),
            LessGreater => operations::not_equal(left_value, right_value),
            Equal => operations::equal(left_value, right_value),

            _ => Err(format!(
                "Operation {} not supported yet!",
                self.operator.token_type.print()
            )),
        }; // match

        // If there's an error, add the source location from the token
        match result {
            Ok(r) => Ok(r),
            Err(message) => {
                let eval_err = format!(
                    "Error performing '{}' operation: {}",
                    &self.operator.token_type.print(),
                    &message
                );
                Err(Error::new(&self.operator, ErrorType::Evaluation, eval_err))
            }
        }
    }
} // impl

impl TypeCheck for BinaryNode {
    fn determine_type(&self, symbols: &SymbolTable) -> Result<DataType, errors::Error> {
        use TokenType::*;

        let left_type = self.left.determine_type(symbols)?;
        let right_type = self.right.determine_type(symbols)?;

        if self.operator.is_comparison_operator() {
            if matches!(left_type, DataType::Number) && matches!(right_type, DataType::Number) {
                return Ok(DataType::Bool);

            // Allow boolean comparison with = or <>
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
	  let mut op = match self.operator.token_type {
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
		_ => panic!("Compilation error, operator not supported yet."),		
	  };
	  
	  if matches!(data_type, DataType::Str) {
		if matches!(self.operator.token_type, TokenType::Plus) {
			return  Ok(ObjectCode {
				data_type,
				code:format!("cat_string({}, {})", &left.code, &right.code),
			});
		 } else {
			let msg = format!("Operator {} not supported for string type.",&op);
			return Err( Error::new(&self.operator, ErrorType::Compiler, msg));
		}
	  }  
	Ok(ObjectCode {
			data_type,
			code:format!("binary_operation({}, {}, {})", &op, &left.code,  &right.code),
		})
    }
}

#[derive(Clone, Debug)]
pub struct LogicalNode {
    left: Box<Expr>,
    operator: Token,
    right: Box<Expr>,
}

impl Evaluation for LogicalNode {
    fn print(&self) -> String {
        format!("logical-operator {}", self.operator.token_type.print())
    }

    fn evaluate(&self, envr: &EnvRc) -> Result<ReturnValue, errors::Error> {
        let left_value = self.left.evaluate(envr)?;
        let left_bool_val = match left_value.get() {
            DataValue::Bool(b) => *b,
            _ => {
                let message = format!("The 'or' operator requires boolean operands.");
                return Err(Error::new(&self.operator, ErrorType::Evaluation, message));
            }
        };

        if matches!(self.operator.token_type, TokenType::Or) {
            if left_bool_val {
                return Ok(left_value);
            }
        } else {
            // an 'and'
            if left_bool_val == false {
                return Ok(left_value);
            }
        }

        self.right.evaluate(envr)
    }
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
			TokenType::And => "&&",
			TokenType::Or => "||",
			_ => panic!("Code generation error, operator not a logical operator: '{}'",
						&self.operator.print()),									
		  };
		  		  
		Ok(ObjectCode {
			data_type,
			code:format!("c_boolean_to_rci_value( rci_value_to_c_boolean({}) {} rci_value_to_c_boolean({}))", 
				&left.code, op, &right.code),
		})
    }				          
}

#[derive(Clone, Debug)]
pub struct CallNode {
    callee: Box<Expr>,
    paren: Token, // To locate the call in the source
    args: Vec<Expr>,
}

impl Evaluation for CallNode {
    fn print(&self) -> String {
        format!("function call: {}", &self.callee.print())
    }

    fn evaluate(&self, envr: &EnvRc) -> Result<ReturnValue, errors::Error> {
        // Look up the 'callee' expr which is currently always going to be an identifier primary
        // expression (which is a 'var' expression.) Evaluating it  returns the entry in the
        // environment associated with the identifier and we expect it to be a callable trait
        // type.
        let mut this_callee = self.callee.evaluate(envr)?;
        if let ReturnValue::CallableValue(ref mut function) = this_callee {
            let mut arguments: Vec<ReturnValue> = Vec::new();
            for arg_expr in &self.args {
                let arg = arg_expr.evaluate(envr)?;
                arguments.push(arg);
            }

            if arguments.len() != function.arity() {
                let message = format!(
                    "Error calling {}, expected {} arguments but got {}.",
                    self.callee.print(),
                    &function.arity(),
                    arguments.len()
                );
                let arity_error = Error::new(&self.paren, ErrorType::Evaluation, message);
                return Err(arity_error);
            }

            // Translate execution errors into evaluation errors
            match function.call(arguments) {
                Err(msg) => Err(Error::new(&self.paren, ErrorType::Evaluation, msg.print())),

                Ok(result) => Ok(result),
            }
        } else {
            let message = format!("Not a callable value {}", &this_callee.print());
            Err(Error::new(&self.paren, ErrorType::Evaluation, message))
        }
    }
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
            let msg = format!("Symbol table definition of function {} and call site don't match arrity!",&function_name.code);
            return  compiler_err(&self.paren, &msg);            
        }
        
            				
		let mut arguments: Vec<String> = Vec::new();
		for (arg_num, arg_expr) in self.args.iter().enumerate() {                
            // For 'var' params, we can pass in only variables, not other expressions, and the
            // variable must have a 'var' declaration type.
			let mut var_name = "".to_string();
            let mut is_var_decl = false;            
            let is_variable_expr = if let  Expr::Variable(ref var_node) = arg_expr{
                var_name =  var_node.name.identifier_string();
                let var_ste =symbols.lookup(&var_name) ;
                if var_ste.is_err() {
                    return compiler_err(&self.paren, &format!("{} not in symbol table!",&var_name));
                }
                is_var_decl = matches!(var_ste.unwrap().entry_type, DeclarationType::Var)                                        ;            
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
            }else{
                arguments.push(arg.code);

            }            			
		}
		
		let args_list = arguments.join(", ");		
        Ok(ObjectCode {
            data_type: callee_return_type,
            code: format!("{}({})",&function_name.code, &args_list),
        })
    }
}

#[derive(Clone, Debug)]
pub struct LookupNode {
    callee: Box<Expr>,
    bracket: Token, // To locate the lookup in the source
    index: Box<Expr>,
}

impl Evaluation for LookupNode {
    fn print(&self) -> String {
        format!("Lookup: {}", &self.callee.print())
    }

    fn evaluate(&self, envr: &EnvRc) -> Result<ReturnValue, errors::Error> {
        // Look up the 'callee' expr which is currently always going to be an identifier primary
        // expression (which is a 'var' expression.) Evaluating it  returns the entry in the
        // environment associated with the identifier and we expect it to be an array or hashtable.
        // type.
        let this_callee = self.callee.evaluate(envr)?;
        if let ReturnValue::Value(DataValue::Array(ref array_data)) = this_callee {
            let index_value = self.index.evaluate(envr)?;
            if let DataValue::Number(ref n) = index_value.get() {
                let int_index: usize = *n as usize;
                if array_data.len() <= int_index {
                    let message = format!(
                        "Index {} out of bounds on array  {}.",
                        int_index,
                        self.callee.print()
                    );
                    Err(Error::new(&self.bracket, ErrorType::Evaluation, message))
                } else {
                    let item = array_data[int_index].clone();
                    Ok(ReturnValue::Value(item))
                }
            } else {
                let message = format!(
                    "Array lookup requires an integer value as an index but was {:?}",
                    index_value
                );

                Err(Error::new(&self.bracket, ErrorType::Evaluation, message))
            }
        } else {
            let message = format!(
                "Only array lookups supported but was {}.",
                &this_callee.print()
            );
            Err(Error::new(&self.bracket, ErrorType::Evaluation, message))
        }
    }
}

impl TypeCheck for LookupNode {
    fn determine_type(&self, symbols: &SymbolTable) -> Result<DataType, errors::Error> {
        if TRACE {
            println!("In lookup type checker");
        }
        let callee_return_type = self.callee.determine_type(symbols)?;
        if let DataType::Array(element_type) = callee_return_type {
            Ok(*element_type)
        } else {
            let message = format!("Only array lookups supported currently.");
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

impl Evaluation for GroupingNode {
    fn print(&self) -> String {
        format!("Grouping: {}", &self.expr.print())
    }

    fn evaluate(&self, envr: &EnvRc) -> Result<ReturnValue, errors::Error> {
        self.expr.evaluate(envr)
    }
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

impl Evaluation for ArrayNode {
    fn print(&self) -> String {
        let element_strs = self
            .elements
            .iter()
            .map(|e| e.print())
            .collect::<Vec<String>>()
            .join(",");
        "array:[".to_string() + &element_strs
    }

    fn evaluate(&self, envr: &EnvRc) -> Result<ReturnValue, errors::Error> {
        // TODO this could be streamlined,
        let mut final_array = Vec::new();
        for e in &self.elements {
            let return_value = e.evaluate(envr)?;
            final_array.push(return_value.get().clone());
        }
        Ok(ReturnValue::Value(DataValue::Array(final_array)))
    }
}

impl TypeCheck for ArrayNode {
    fn determine_type(&self, symbols: &SymbolTable) -> Result<DataType, errors::Error> {
        if self.elements.len() == 0 {
            return Ok(DataType::Unresolved);
        }

        let first_type = self.elements[0].determine_type(symbols)?;
        let mut last_type = first_type.clone();

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
        Ok(DataType::Array(Box::new(first_type.clone())))
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
pub struct LiteralNode {
    value: Rc<DataValue>,
}

impl Evaluation for LiteralNode {
    fn print(&self) -> String {
        self.value.print()
    }

    fn evaluate(&self, envr: &EnvRc) -> Result<ReturnValue, errors::Error> {
        Ok(ReturnValue::Reference(Rc::clone(&self.value)))
    }
}

impl TypeCheck for LiteralNode {
    fn determine_type(&self, symbols: &SymbolTable) -> Result<DataType, errors::Error> {
        Ok(DataType::from_data_value(&*self.value))
    }
}

impl LiteralNode {
    fn compile(data_value: &ReturnValue, symbols: &SymbolTable) -> Result<ObjectCode, errors::Error> {
        let value = data_value.get();
		let literal_type = DataType::from_data_value(value);
		let object_code = match *value {
			DataValue::Str(ref v)=> 
				format!("(rci_value) {{ .data._string = {{.data = \"{}\",.len = {}, .chars = {}, .refs = 0, .encoding = byte_encoded }}, .type = _string_}} ", &v, &v.len(), &v.len()),
			DataValue::Number(n)=>
				format!("(rci_value) {{.data._number={}, .type=_number_}}",n),
			DataValue::Bool(b)=>
				format!("(rci_value) {{.data._boolean={}, .type=_boolean_}}",b),
			_ => panic!("Literal compilation not implemented for {:?}", value),
		};
						
        Ok(ObjectCode {
            data_type: literal_type,
            code: object_code,
        })
    }
}

#[derive(Clone, Debug)]
pub struct UnaryNode {
    operator: Token,
    expr: Box<Expr>,
}

impl Evaluation for UnaryNode {
    fn print(&self) -> String {
        format!("{} {}", &self.operator.print(), &self.expr.print())
    }

    fn evaluate(&self, envr: &EnvRc) -> Result<ReturnValue, errors::Error> {
        let right = &self.expr.evaluate(envr)?;

        match self.operator.token_type {
            TokenType::Minus => match right.get() {
                DataValue::Number(n) => {
                    let new_value = ReturnValue::Value(DataValue::Number(-n));
                    Ok(new_value)
                }
                _ => {
                    let message = format!("Not a number value.");
                    Err(Error::new(&self.operator, ErrorType::Evaluation, message))
                }
            },
            TokenType::Not => {
                if let DataValue::Bool(b) = right.get() {
                    Ok(ReturnValue::Value(DataValue::Bool(!b)))
                } else {
                    let message = format!("The 'not' unary operator only works on boolean values.");
                    Err(Error::new(&self.operator, ErrorType::Evaluation, message))
                }
            }
            _ => {
                let message = format!("Invalid unary expression.");
                Err(Error::new(&self.operator, ErrorType::Evaluation, message))
            }
        } // match
    } // fn
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
		
		let code =match self.operator.token_type {
			Not => format!("unary_operation(_NOT_,{})",&compiled_expr.code),
			Minus => format!("unary_operation(_NEGATIVE_,{})",&compiled_expr.code),
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

impl Evaluation for VariableNode {
    fn print(&self) -> String {
        format!("var: {}", &self.name.print())
    }

    fn evaluate(&self, envr: &EnvRc) -> Result<ReturnValue, errors::Error> {
        match self.name.token_type {
            TokenType::Identifier(ref name) => {
                if let Some(dist) = self.distance {
                    if TRACE {
                        println!(
                            "Get {} with index {} and dist {}",
                            &name,
                            self.index.unwrap(),
                            dist
                        );
                    }
                    if TRACE {
                        envr.dump_content(0);
                    }
                    envr.get_with_index_and_distance(self.index.unwrap(), dist)
                } else {
                    if TRACE {
                        println!("Get {} without distance!", &name);
                    }
                    envr.get(&name)
                }
            }
            _ => Err(Error::new(
                &self.name,
                ErrorType::Evaluation,
                "Can't look up non-identifiers".to_string(),
            )),
        }
    } // fn
} // impl

impl TypeCheck for VariableNode {
    // Doing this right requires a symbol table built up from
    // visiting all the variable declarations and using the right
    // lexical scoping rules.

    fn determine_type(&self, symbols: &SymbolTable) -> Result<DataType, errors::Error> {
        if let TokenType::Identifier(variable_name) = &self.name.token_type {
            match symbols.lookup(&variable_name) {
                Ok(ref symbol_table_entry) => Ok(symbol_table_entry.data_type.clone()),
                Err(declaration_error) => {
                    let message = format!("{}", &declaration_error.message);
                    Err(Error::new(&self.name, ErrorType::Type, message))
                }
            }
        } else {
            panic!("Fatal error during type-checking: A variable expression must have a TokenType::Identifier(name) token type!");
        }
    }
}

impl Compiler for VariableNode {
    fn compile(&self, symbols: &SymbolTable) -> Result<ObjectCode, errors::Error> {
		let var_type = self.determine_type(symbols)?;
		if let TokenType::Identifier(ref var_name) = self.name.token_type {
			let ste = match symbols.lookup(&var_name) {
				Ok(ref entry) => entry.clone(),
				Err(error_msg) => {
					return Err(errors::Error::new(&self.name,ErrorType::Compiler, error_msg.message.clone()));
				}
			};
			
			Ok( ObjectCode {
				data_type: var_type,
				code: {
					if ste.is_arg &&
						matches!(ste.entry_type,DeclarationType::Var) {						
						format!("(*{})",&var_name)
					} else {
						format!("{}",&var_name)
					}
				}
			})
		} else {
			panic!("Compiler error. A variable node must have an identifier token type.");
		}
	}					
}

#[derive(Clone, Debug)]
pub struct AssignmentNode {
    name: Token,
    value: Box<Expr>,
    distance: Option<usize>,
    index: Option<usize>,
}

impl Evaluation for AssignmentNode {
    fn print(&self) -> String {
        format!(
            "assign var: {} to  {}",
            &self.name.print(),
            &self.value.print()
        )
    }

    fn evaluate(&self, envr: &EnvRc) -> Result<ReturnValue, errors::Error> {
        let value_to_store = self.value.evaluate(envr)?;

        let var_name = match self.name.token_type {
            TokenType::Identifier(ref variable_name) => variable_name,
            _ => {
                let message = format!("Only assignment to simple identifiers permitted currently. Assignee token was {:?}", &self.name);
                return Err(Error::new(&self.name, ErrorType::Evaluation, message));
            }
        };
        if let Some(dist) = self.distance {
            if TRACE {
                println!("Assigning {} with distance {}", &var_name, dist);
            }
            envr.assign_with_index_and_distance(self.index.unwrap(), value_to_store, dist)?;
        } else {
            envr.assign(var_name, value_to_store)?;
        }
        Ok(ReturnValue::None)
    }
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
				format!("{} = assign_string({},{});",
					&assignee_name, &assignee_name, &value_to_store.code)
			}
			_ =>format!("{} = {}",&assignee_name, &value_to_store.code),
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
        Expr::Literal(ReturnValue::new_ref(data_value))
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

    pub fn data_type(&self) -> DataType {
        match self {
            Expr::Literal(ReturnValue::Reference(data_value)) => {
                DataType::from_data_value(&data_value)
            }
            Expr::Literal(ReturnValue::Value(data_value)) => DataType::from_data_value(&data_value),
            _ => panic!("Not a literal expression!"),
        }
    }

    // Type check conveniences

    pub fn is_number(&self) -> bool {
        matches!(self.data_type(), DataType::Number)
    }

    pub fn is_boolean(&self) -> bool {
        matches!(self.data_type(), DataType::Bool)
    }

    pub fn is_string(&self) -> bool {
        matches!(self.data_type(), DataType::Str)
    }

    fn parenthesize(inside: String) -> String {
        "(".to_string() + &inside + ")"
    }

    pub fn print(&self) -> String {
        use Expr::*;
        let inside = match self {
            Binary(n) => n.print(),
            Unary(n) => n.print(),
            Literal(ref n) => n.print(),
            Grouping(n) => n.print(),
            Variable(n) => n.print(),
            Assignment(n) => n.print(),
            Call(n) => n.print(),
            Logical(n) => n.print(),

            _ => panic!("Not implemented"),
        };
        Expr::parenthesize(inside)
    }
}
