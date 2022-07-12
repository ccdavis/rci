use crate::errors;
use crate::errors::*;
use crate::lex::TokenType;
use crate::symbol_table::SymbolTableEntry;

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
pub struct CollectionType {
    pub index_type: DataType,
    pub contains_type: DataType,
    pub size: Option<usize>,
    pub min_index: Option<DataValue>,
    pub max_index: Option<DataValue>,
}

pub enum Lookup {
    pub DirectMap(CollectionType),
    pub HashedMap(CollectionType),
    pub Vector(CollectionType),
}

// Named fields
pub struct FieldType {
	pub name: String,
	pub field_type: DataType,
}

pub struct RecordType {
	pub fields: Vec<FieldType>,
}

pub struct TupleType {
    pub members: Vec<DataType>,
}

pub struct EnumValue {
    pub enum_name: String,
    pub value: String,
}

pub struct EnumerationType {
	pub enum_name: String,
	// Each enum value (location in vec) gets a default name
	// or user-defined name.
	pub items: Vec<String>,
}


pub struct SetType {
	pub member_type: DataType,
	pub members: Vec<DataValue>,
}

pub struct RangeType {
    pub member_type: DataType,
    pub low: DataValue,
    pub high: DataValue,
}

pub struct UserType {
    pub name: String,
    definition: DataType,
}

// Simple data types and values
#[derive(Clone, Debug, PartialEq)]
pub enum DataType {
    Str,
    Number,
    Integer,
    Float,
    Bool,
    Lookup(CollectionType),	
    Tuple(TupleType),
    User(String),
    Empty,      // Like the '()' expressiontype in Rust
    Unresolved, // Incomplete type checker results
}

#[derive(Clone, Debug)]
pub enum DataValue {
    Str(String),
    Number(f64),
    Bool(bool),
    Integer(int64),
    Float(f64),
    Tuple(Vec<DataValue>),
    Lookup(Vec<DataValue>), // not all lookup types can be literals
    User(String), // just give the name for now
    Unresolved,   // for the type-checker to figure out later
}


#[derive(Clone, Debug, PartialEq)]
pub struct ObjectCode {
    pub data_type: DataType,
    pub code: String,
}

#[derive(Clone, Debug)]
pub struct GlobalStatementObjectCode {
    pub decl_type: DeclarationType,
    pub base_code: String,
    pub decl_name: String, // variable name, function name, type name etc
    pub init_code: String, // Compiled init code that must be called elsewhere
    pub init_name: String, // Name of initializer function
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

    pub fn to_c_ir(&self) -> String {
        let lexeme = self.print_value();
        match self {
            DataValue::Str(_) => format!("(rci_value) string_literal({})", &lexeme),
            DataValue::Number(_) => format!("NUMBER_VAL({})", &lexeme),
            DataValue::Bool(_) => format!("BOOL_VAL({})", &lexeme),
            DataValue::Array(ref data) => "//not implemented".to_string(),
            DataValue::User(ref u) => "//not implemented".to_string(),
            DataValue::Unresolved => panic!("Unresolved value. Incomplete parsing or compilation!"),
        }
    }
}
