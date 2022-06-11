use crate::environment::*;
use crate::errors;
use crate::errors::*;
use crate::lex::TokenType;
use crate::statement::EarlyReturn;
use crate::symbol_table::SymbolTableEntry;

use dyn_clonable::*;
use std::fmt;
use std::rc::Rc;

// Types of declarations that can be stored in the symbol table which will
// hold data of the different types

#[derive(PartialEq, Clone, Debug)]
pub enum DeclarationType {
    Var,   // all types
    Val,   // all types
    Cpy,   // all types
    Fun,   // if functions aren't first class, no specific type associated
    Class, // if classes aren't first order no specific types
    Type,  // user defined, the name in the symbol table entry will have the definition
}

#[derive(Clone)]
pub struct CollectionType{
    pub index_type: DataType,
    pub contains_type: DataType,
    pub size: Option<usize>,
}

// Simple data types and values
#[derive(Clone, Debug, PartialEq)]
pub enum DataType {
    Str,
    Number,
    Bool,
    Array(Box<DataType>),
    User(String),
    Empty,      // Like the '()' expressiontype in Rust
    Unresolved, // Incomplete type checker results
}

#[derive(Clone, Debug, PartialEq)]
pub struct ObjectCode {
    pub data_type: DataType,
    pub code: String,
}

#[derive(Clone,Debug)]
pub struct GlobalStatementObjectCode {
    pub decl_type: DeclarationType,
    pub base_code: String,
    pub decl_name: String, // variable name, function name, type name etc
    pub init_code: String, // Compiled init code that must be called elsewhere
    pub init_name: String, // Name of initializer function
}

#[derive(Clone, Debug)]
pub enum DataValue {
    Str(String),
    Number(f64),
    Bool(bool),
    Array(Vec<DataValue>),
    User(String), // just give the name for now
    Unresolved,   // for the type-checker to figure out later
}

impl fmt::Display for DataType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match self {
            DataType::Number => "number".to_string(),
            DataType::Str => "string".to_string(),
            DataType::Bool => "boolean".to_string(),
            DataType::Array(ref dt) => format!("Array[{}]", &dt),
            DataType::Empty => "empty".to_string(),
            DataType::User(u) => u.as_str().to_string(),
            DataType::Unresolved => "UNRESOLVED".to_string(),
        };
        write!(f, "{}", &name)
    }
}

impl DataType {
    pub fn from_token_type(tt: &TokenType) -> Option<DataType> {
        match tt {
            TokenType::NumberType => Some(DataType::Number),
            TokenType::StringType => Some(DataType::Str),
            TokenType::BooleanType => Some(DataType::Bool),
            TokenType::ArrayType => Some(DataType::Array(Box::new(DataType::Unresolved))),
            TokenType::Identifier(n) => Some(DataType::User(n.to_owned())),
            _ => None,
        }
    }

    pub fn from_data_value(dv: &DataValue) -> DataType {
        match dv {
            DataValue::Str(_) => DataType::Str,
            DataValue::Number(_) => DataType::Number,
            DataValue::Bool(_) => DataType::Bool,
            DataValue::Array(ref value) => {
                let element_type = if value.len() == 0 {
                    Box::new(DataType::Unresolved)
                } else {
                    Box::new(DataType::from_data_value(&value[0]))
                };
                DataType::Array(element_type)
            }
            DataValue::User(n) => DataType::User(n.to_owned()),
            DataValue::Unresolved => DataType::Unresolved,
        }
    }
}

impl DataValue {
    pub fn print_value(&self) -> String {
        match self {
            DataValue::Str(s) => s.to_string(),
            DataValue::Number(n) => format!("{}", n),
            DataValue::Bool(b) => format!("{}", b),
            DataValue::Array(ref data) => format!("{:?}", data),
            DataValue::User(u) => format!("{}", u.to_string()),
            DataValue::Unresolved => panic!("Unresolved value. Incomplete parsing or compilation!"),
        }
    }

    pub fn from_token_type(tt: &TokenType) -> DataValue {
        match tt {
            TokenType::Str(s) => DataValue::Str(s.to_owned()),
            TokenType::Number(n) => DataValue::Number(*n),
            TokenType::False => DataValue::Bool(false),
            TokenType::True => DataValue::Bool(true),
            _ => panic!("Not convertable to a ValueType: {}", tt.print()),
        }
    }

    pub fn print(&self) -> String {
        format!("{:?}", self)
    }

    pub fn to_c_literal(&self) -> String {
		let lexeme = self.print_value();
		let c_cast = match self {
			DataValue::Str(_) =>"rci_str",
            DataValue::Number(_) =>"double",
            DataValue::Bool(_) => "",
            DataValue::Array(ref data) => "rci_array",
            DataValue::User(ref u) => u,
            DataValue::Unresolved => panic!("Unresolved value. Incomplete parsing or compilation!"),
		};
		format!("({}) {} ",&c_cast, &lexeme)        
    }
}

#[clonable]
pub trait Callable: Clone {
    fn call(&mut self, arguments: Vec<ReturnValue>) -> Result<ReturnValue, EarlyReturn>;
    fn arity(&self) -> usize;
    fn params(&self) -> Vec<Box<SymbolTableEntry>>;
    fn return_type(&self) -> &DataType;
    fn name(&self) -> String;
}

// We use this both for evaluation of expressions and return values from functions. In the execution of
// statements that are pseudo-expressions ReturnValue is also needed: "return" specifically.
#[derive(Clone)]
pub enum ReturnValue {
    Reference(Rc<DataValue>),
    Value(DataValue),
    CallableValue(Box<dyn Callable>),
    None, // This is for optional return of data, not 'nil' or null
}

impl fmt::Debug for ReturnValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let output = match self {
            ReturnValue::Reference(ref val) => format!("{:?}", val),
            ReturnValue::Value(ref val) => format!("{:?}", val),
            ReturnValue::None => "None".to_string(),
            ReturnValue::CallableValue(_) => "Callable".to_string(),
        };
        f.write_str(&output)
    }
}

impl ReturnValue {
    pub fn new_ref(value: DataValue) -> Self {
        ReturnValue::Reference(Rc::new(value))
    }

    pub fn new_val(value: DataValue) -> Self {
        ReturnValue::Value(value)
    }

    // Helper for internal use in standard library
    pub fn get_as_number(&self) -> Result<f64, errors::Error> {
        match self.get() {
            DataValue::Number(n) => Ok(*n),
            _ => {
                let message = format!(
                    "Incorrect type -- expected number. but was {}",
                    &self.print()
                );
                Err(Error::internal(ErrorType::Execution, message))
            }
        }
    }

    pub fn get(&self) -> &DataValue {
        match self {
            ReturnValue::Reference(v) => &*v,
            ReturnValue::Value(v) => &v,
            ReturnValue::None => {
                panic!("get() is for internal use and should never be called on None varient.")
            }
            _ => panic!("Can't retrieve other types such as Callable."),
        }
    }

    pub fn clone_or_increment_count(&self) -> ReturnValue {
        match self {
            ReturnValue::Reference(v) => ReturnValue::Reference(Rc::clone(&v)),
            ReturnValue::Value(v) => ReturnValue::Value(v.clone()),
            ReturnValue::CallableValue(fun) => ReturnValue::CallableValue(fun.clone()),
            ReturnValue::None => ReturnValue::None,
        }
    }

    pub fn print(&self) -> String {
        self.get().print_value()
    }
}
