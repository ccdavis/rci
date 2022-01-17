use crate::environment;
use crate::environment::EnvNode;
use crate::environment::EnvRc;
use crate::lex::Token;
use crate::lex::TokenType;
use crate::operations;
use crate::symbol_table::*;

use crate::types::DataType;
use crate::types::DataValue;
use crate::types::DeclarationType;
use crate::types::ReturnValue;

use std::rc::Rc;

const TRACE: bool = false;

pub struct EvaluationError {
    pub message: String,
}

#[derive(Debug)]
pub struct TypeError {
    pub message: String,
}

pub trait Evaluation {
    // return the name  of the operation if any and
    // associated expressions.
    fn print(&self) -> String;
    fn evaluate(&self, envr: &EnvRc) -> Result<ReturnValue, EvaluationError>;
}

pub trait TypeCheck {
    // This is for use before the symbol table is fully populated
    fn expected_type(&self) -> Result<DataType, TypeError>;

    // This is for after the program has been fully parsed and all symbols are known.
    fn determine_type(&self, symbols: &SymbolTable) -> Result<DataType, TypeError>;
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
    pub fn evaluate(&self, envr: &EnvRc) -> Result<ReturnValue, EvaluationError> {
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

    // Try to figure out what type the expression will yield before evaluation,
    // for use in type-checking during parsing.
    // 'Unresolved' is a valid result.
    pub fn expected_type(&self) -> Result<DataType, TypeError> {
        match self {
            Expr::Binary(n) => n.expected_type(),
            Expr::Logical(n) => n.expected_type(),
            Expr::Unary(n) => n.expected_type(),
            Expr::Grouping(n) => n.expected_type(),
            Expr::Array(n) => n.expected_type(),
            Expr::Variable(n) => n.expected_type(),
            Expr::Assignment(n) => n.expected_type(),
            Expr::Literal(value) => Ok(DataType::from_data_value(value.get())),
            _ => Ok(DataType::Unresolved),
        }
    }

    pub fn determine_type(&self, symbols: &SymbolTable) -> Result<DataType, TypeError> {
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
} // impl expr

#[derive(Clone, Debug)]
struct BinaryNode {
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

    fn evaluate(&self, envr: &EnvRc) -> Result<ReturnValue, EvaluationError> {
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
                    "Error performing '{}' operation at {}, {}: {}",
                    &self.operator.token_type.print(),
                    self.operator.line,
                    self.operator.column,
                    &message
                );
                Err(EvaluationError { message: eval_err })
            }
        }
    }
} // impl

impl TypeCheck for BinaryNode {
    fn expected_type(&self) -> Result<DataType, TypeError> {
        use TokenType::*;

        let left_type = self.left.expected_type()?;
        let right_type = self.right.expected_type()?;

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
                let message = format!("Invalid operand types for operator {:?}", self.operator);
                return Err(TypeError { message });
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
                    "Invalid operand types for operator {:?}, expected Numbers.",
                    self.operator
                );
                return Err(TypeError { message });
            }
        }

        let message = format!(
            "Operator should not be part of a binary expression:{:}",
            &self.operator.print()
        );
        Err(TypeError { message })
    }

    fn determine_type(&self, symbols: &SymbolTable) -> Result<DataType, TypeError> {
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
                let message = format!("Invalid operand types for operator {:?}", self.operator);
                return Err(TypeError { message });
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
                    "Invalid operand types for operator {:?}, expected Numbers.",
                    self.operator
                );
                return Err(TypeError { message });
            }
        }

        let message = format!(
            "Operator should not be part of a binary expression:{:}",
            &self.operator.print()
        );
        Err(TypeError { message })
    }
} // impl

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

    fn evaluate(&self, envr: &EnvRc) -> Result<ReturnValue, EvaluationError> {
        let left_value = self.left.evaluate(envr)?;
        let left_bool_val = match left_value.get() {
            DataValue::Bool(b) => *b,
            _ => {
                let message = format!(
                    "{}: The 'or' operator requires boolean operands.",
                    &self.operator.pos()
                );
                return Err(EvaluationError { message });
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
    fn expected_type(&self) -> Result<DataType, TypeError> {
        Ok(DataType::Unresolved)
    }

    fn determine_type(&self, symbols: &SymbolTable) -> Result<DataType, TypeError> {
        let right_type = self.right.determine_type(symbols)?;
        let left_type = self.left.determine_type(symbols)?;
        if matches!(left_type, DataType::Bool) && matches!(right_type, DataType::Bool) {
            Ok(DataType::Bool)
        } else {
            let message = format!(
                "{} Operands of a logical operator must be boolean. Got {}, {} instead.",
                &self.operator.pos(),
                &left_type,
                &right_type
            );
            Err(TypeError { message })
        }
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

    fn evaluate(&self, envr: &EnvRc) -> Result<ReturnValue, EvaluationError> {
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
                let arity_error = EvaluationError { message };
                return Err(arity_error);
            }

            // Translate execution errors into evaluation errors
            match function.call(arguments) {
                Err(msg) => Err(EvaluationError {
                    message: msg.print(),
                }),
                Ok(result) => Ok(result),
            }
        } else {
            let message = format!("Not a callable value {}", &this_callee.print());
            Err(EvaluationError { message })
        }
    }
}

impl TypeCheck for CallNode {
    fn expected_type(&self) -> Result<DataType, TypeError> {
        Ok(DataType::Unresolved)
    }

    fn determine_type(&self, symbols: &SymbolTable) -> Result<DataType, TypeError> {
        let callee_return_type = self.callee.determine_type(symbols)?;
        Ok(callee_return_type)
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

    fn evaluate(&self, envr: &EnvRc) -> Result<ReturnValue, EvaluationError> {
        // Look up the 'callee' expr which is currently always going to be an identifier primary
        // expression (which is a 'var' expression.) Evaluating it  returns the entry in the
        // environment associated with the identifier and we expect it to be an array or hashtable.
        // type.
        let mut this_callee = self.callee.evaluate(envr)?;
        if let ReturnValue::Value(DataValue::Array(ref array_data)) = this_callee {
            let index_value = self.index.evaluate(envr)?;
            if let ReturnValue::Value(DataValue::Number(n)) = index_value {
                let int_index: usize = n as usize;
                if array_data.len() <= int_index {
                    let message = format!(
                        "Index {} out of bounds on array  {} at {}",
                        int_index,
                        self.callee.print(),
                        &self.bracket.pos()
                    );
                    Err(EvaluationError { message })
                } else {
                    let item = array_data[int_index].clone();
                    Ok(ReturnValue::Value(item))
                }
            } else {
                let message = format!(
                    "Array lookup at {} requires an integer value as an index.",
                    &self.bracket.pos()
                );
                Err(EvaluationError { message })
            }
        } else {
            let message = format!(
                "Only array lookups supported but was {} at {}",
                &this_callee.print(),
                &self.bracket.pos()
            );
            Err(EvaluationError { message })
        }
    }
}

impl TypeCheck for LookupNode {
    fn expected_type(&self) -> Result<DataType, TypeError> {
        Ok(DataType::Unresolved)
    }

    fn determine_type(&self, symbols: &SymbolTable) -> Result<DataType, TypeError> {
        let callee_return_type = self.callee.determine_type(symbols)?;
        Ok(callee_return_type)
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

    fn evaluate(&self, envr: &EnvRc) -> Result<ReturnValue, EvaluationError> {
        self.expr.evaluate(envr)
    }
}

impl TypeCheck for GroupingNode {
    fn expected_type(&self) -> Result<DataType, TypeError> {
        self.expr.expected_type()
    }

    fn determine_type(&self, symbols: &SymbolTable) -> Result<DataType, TypeError> {
        self.expr.determine_type(symbols)
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

    fn evaluate(&self, envr: &EnvRc) -> Result<ReturnValue, EvaluationError> {
        // TODO this could be streamlined,
        let mut final_array = Vec::new();
        for e in &self.elements {
            let return_value = e.evaluate(envr)?;
            final_array.push(return_value.get().clone());
        }
        Ok(ReturnValue::Value(DataValue::Array(final_array)))
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

    fn evaluate(&self, envr: &EnvRc) -> Result<ReturnValue, EvaluationError> {
        Ok(ReturnValue::Reference(Rc::clone(&self.value)))
    }
}

impl TypeCheck for ArrayNode {
    fn expected_type(&self) -> Result<DataType, TypeError> {
        if self.elements.len() == 0 {
            Ok(DataType::Unresolved)
        } else {
            let types: Result<Vec<DataType>, _> =
                self.elements.iter().map(|e| e.expected_type()).collect();

            let first_type: DataType = match types {
                Err(msg) => return Err(msg),
                Ok(ref types) => types[0].clone(),
            };

            if types
                .unwrap()
                .iter()
                .all(|e| e.to_string() == first_type.to_string())
            {
                Ok(first_type)
            } else {
                let message = format!(
                    "Expressions of different types in array, first type was {}",
                    &first_type
                );
                Err(TypeError { message })
            }
        }
    }

    fn determine_type(&self, symbols: &SymbolTable) -> Result<DataType, TypeError> {
        if self.elements.len() == 0 {
            Ok(DataType::Unresolved)
        } else {
            let types: Result<Vec<DataType>, _> = self
                .elements
                .iter()
                .map(|e| e.determine_type(symbols))
                .collect();

            let first_type: DataType = match types {
                Err(msg) => return Err(msg),
                Ok(ref types) => types[0].clone(),
            };

            if types
                .unwrap()
                .iter()
                .all(|e| e.to_string() == first_type.to_string())
            {
                Ok(first_type)
            } else {
                let message = format!(
                    "Expressions of different types in array, first type was {}",
                    &first_type
                );
                Err(TypeError { message })
            }
        }
    }
}

impl TypeCheck for LiteralNode {
    fn expected_type(&self) -> Result<DataType, TypeError> {
        Ok(DataType::from_data_value(&*self.value))
    }

    fn determine_type(&self, symbols: &SymbolTable) -> Result<DataType, TypeError> {
        Ok(DataType::from_data_value(&*self.value))
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

    fn evaluate(&self, envr: &EnvRc) -> Result<ReturnValue, EvaluationError> {
        let right = &self.expr.evaluate(envr)?;

        match self.operator.token_type {
            TokenType::Minus => match right.get() {
                DataValue::Number(n) => {
                    let new_value = ReturnValue::Value(DataValue::Number(-n));
                    Ok(new_value)
                }
                _ => {
                    let message = format!("Not a number value at {:?}", self.operator);
                    Err(EvaluationError { message })
                }
            },
            TokenType::Not => {
                if let DataValue::Bool(b) = right.get() {
                    Ok(ReturnValue::Value(DataValue::Bool(!b)))
                } else {
                    let message = format!(
                        "The 'not' unary operator only works on boolean values. {:?}",
                        self.operator
                    );
                    Err(EvaluationError { message })
                }
            }
            _ => {
                let message = format!("Invalid unary expression at {:?}", &self.operator);
                Err(EvaluationError { message })
            }
        }
    }
}

impl TypeCheck for UnaryNode {
    fn expected_type(&self) -> Result<DataType, TypeError> {
        use TokenType::*;
        let right_type = self.expr.expected_type()?;
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

        Err(TypeError { message })
    }

    fn determine_type(&self, symbols: &SymbolTable) -> Result<DataType, TypeError> {
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

        Err(TypeError { message })
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

    fn evaluate(&self, envr: &EnvRc) -> Result<ReturnValue, EvaluationError> {
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
            _ => Err(EvaluationError {
                message: "Can't look up non-identifiers".to_string(),
            }),
        }
    }
}

impl TypeCheck for VariableNode {
    // Doing this right requires a symbol table built up from
    // visiting all the variable declarations and using the right
    // lexical scoping rules.
    fn expected_type(&self) -> Result<DataType, TypeError> {
        Ok(DataType::Unresolved)
    }

    fn determine_type(&self, symbols: &SymbolTable) -> Result<DataType, TypeError> {
        if let TokenType::Identifier(variable_name) = &self.name.token_type {
            match symbols.lookup(&variable_name) {
                Ok(ref symbol_table_entry) => Ok(symbol_table_entry.data_type.clone()),
                Err(declaration_error) => {
                    let message = format!(
                        "Type Error at {}, {}: {}",
                        self.name.line, self.name.column, &declaration_error.message
                    );
                    Err(TypeError { message })
                }
            }
        } else {
            panic!("Fatal error during type-checking: A variable expression must have a TokenType::Identifier(name) token type!");
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

    fn evaluate(&self, envr: &EnvRc) -> Result<ReturnValue, EvaluationError> {
        let value_to_store = self.value.evaluate(envr)?;

        let var_name = match self.name.token_type {
            TokenType::Identifier(ref variable_name) => variable_name,
            _ => {
                let message = format!("Only assignment to simple identifiers permitted currently. Assignee token was {:?}", &self.name);
                return Err(EvaluationError { message });
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
    fn expected_type(&self) -> Result<DataType, TypeError> {
        // Can't check variable type against assigned value until a
        // pre-runtime symbol table is available.
        let assign_type = self.value.expected_type()?;
        // compare assign_type with the type of the variable
        // TODO use symbol table
        Ok(DataType::Empty)
    }

    // compare assign_type with the type of the variable
    fn determine_type(&self, symbols: &SymbolTable) -> Result<DataType, TypeError> {
        let assign_type = self.value.determine_type(symbols)?;

        // get variable / assignee name
        let assignee_name = match  self.name.token_type {
			TokenType::Identifier(ref n) => n.clone(),
			_ => panic!("Fatal error during type-checking. Assignment node must have a TokenType::Identifier(name) for the name field."),
		};

        let to_type = match symbols.lookup(&assignee_name) {
            Ok(ref ste) => {
                if matches!(ste.entry_type, DeclarationType::Val) {
                    let message = format!(
                        "Type error at {}, {}: Can't assign to a 'val'. Only 'var' is mutable.",
                        self.name.line, self.name.column
                    );

                    return Err(TypeError { message });
                }
                ste.data_type.clone()
            }
            Err(not_declared) => {
                let message = format!(
                    "Type error at {}, {}: {}",
                    self.name.line, self.name.column, &not_declared.message
                );
                return Err(TypeError { message });
            }
        };

        if assign_type == to_type {
            Ok(DataType::Empty)
        } else {
            let message = format!(
                "Type Error at {}, {}: Can't assign {} with type {} to a value of type {}.",
                self.name.line, self.name.column, &assignee_name, &to_type, &assign_type
            );
            Err(TypeError { message })
        }
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
