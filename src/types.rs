use crate::errors;
use crate::errors::*;
use crate::lex::Token;
use crate::lex::TokenType;

use std::fmt;

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

#[derive(Clone, Debug, PartialEq)]
pub enum LookupType {
    Unresolved,
    DirectMap {
        index_type: DataType,
        contains_type: DataType,
        size: usize,
    },
    HashedMap {
        index_type: DataType,
        contains_type: DataType,
    },
    Vector {
        index_type: DataType,
        contains_type: DataType,
    },
    Array {
        index_type: DataType,
        contains_type: DataType,
        size: Option<usize>,
        low_index: Option<Box<DataValue>>,
        high_index: Option<Box<DataValue>>,
    },
}

// Named fields
#[derive(Clone, Debug, PartialEq)]
pub struct FieldType {
    pub name: String,
    pub field_type: DataType,
}

#[derive(Clone, Debug, PartialEq)]
pub struct RecordType {
    pub fields: Vec<FieldType>,
}

impl RecordType {
    pub fn type_of_field(
        &self,
        field_name: &str,
        location: &Token,
    ) -> Result<DataType, errors::Error> {
        let f = self.fields.iter().find(|field| field.name == field_name);
        match f {
            None => Err(Error::new(
                location,
                ErrorType::Type,
                format!("No field named  '{}'", &field_name),
            )),
            Some(field) => Ok(field.field_type.clone()),
        }
    }

    pub fn index_of_field(&self, field_name: &str) -> Option<usize> {
        self.fields
            .iter()
            .position(|field| &field.name == field_name)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TupleType {
    pub members: Vec<DataType>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct EnumerationType {
    pub enum_name: String,
    pub items: Vec<EnumValue>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct SetType {
    pub member_type: DataType,
    pub members: Vec<DataValue>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct RangeType {
    pub member_type: Box<DataType>,
    pub low: Box<DataValue>,
    pub high: Box<DataValue>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct UserType {
    pub name: String,
    pub definition: Box<DataType>,
}

// Simple data types and values
#[derive(Clone, Debug, PartialEq)]
pub enum DataType {
    Str,
    Number,
    Integer,
    Float,
    Bool,
    Lookup(Box<LookupType>),
    Tuple(Box<TupleType>),
    Range(Box<RangeType>),
    Enumeration(EnumerationType),
    Record(RecordType),
    User(Box<UserType>),
    Empty,      // Like the '()' expressiontype in Rust
    Unresolved, // Incomplete type checker results
}

impl DataType {
    pub fn is_numeric(&self) -> bool {
        match self {
            DataType::Integer | DataType::Float | DataType::Number => true,
            _ => false,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct EnumValue {
    pub member_of_enum: String,
    pub value: String,
    pub string_representation: String,
}
#[derive(Clone, PartialEq)]
pub struct FieldValue {
    pub name: String,
    pub value: Box<DataValue>,
}

#[derive(Clone, PartialEq)]
pub struct RecordValue {
    pub fields: Vec<FieldValue>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum DataValue {
    Str(String),
    Number(f64),
    Bool(bool),
    Integer(i64),
    Float(f64),
    Tuple(Vec<DataValue>),
    Lookup {
        varient: LookupType,
        content: Vec<DataValue>,
    }, // not all lookup types can be literals
    Range {
        low: Box<DataValue>,
        high: Box<DataValue>,
    },
    Enumeration(EnumValue),
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

impl GlobalStatementObjectCode {
    pub fn no_op() -> Self {
        Self {
            decl_type: DeclarationType::Val,
            base_code: "".to_string(),
            decl_name: "".to_string(),
            init_code: "".to_string(),
            init_name: "".to_string(),
        }
    }
}

impl fmt::Display for DataType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match self {
            DataType::Number => "num".to_string(),
            DataType::Integer => "int".to_string(),
            DataType::Float => "flt".to_string(),
            DataType::Tuple(_) => "tuple".to_string(),
            DataType::Range(_) => "range".to_string(),
            DataType::Str => "str".to_string(),
            DataType::Bool => "bool".to_string(),
            DataType::Lookup(_) => "Lookup".to_string(),
            DataType::Enumeration(ref enum_type) => format!("Enum({})", &enum_type.enum_name),
            DataType::Record(_) => "Rec".to_string(),
            DataType::Empty => "empty".to_string(),
            DataType::User(u) => u.name.clone(),
            DataType::Unresolved => "UNRESOLVED".to_string(),
        };
        write!(f, "{}", &name)
    }
}

impl DataType {
    pub fn from_token_type(tt: &TokenType) -> Option<DataType> {
        match tt {
            TokenType::NumberType => Some(DataType::Number),
            TokenType::IntegerType => Some(DataType::Integer),
            TokenType::FloatType => Some(DataType::Float),
            TokenType::StringType => Some(DataType::Str),
            TokenType::BooleanType => Some(DataType::Bool),
            TokenType::ArrayType => Some(DataType::Lookup(Box::new(LookupType::Unresolved))),
            TokenType::Identifier(n) => Some(DataType::User(Box::new(UserType {
                name: n.to_owned(),
                definition: Box::new(DataType::Unresolved),
            }))),
            _ => None,
        }
    }

    pub fn from_data_value(dv: &DataValue) -> DataType {
        match dv {
            DataValue::Str(_) => DataType::Str,
            DataValue::Number(_) => DataType::Number,
            DataValue::Integer(_) => DataType::Integer,
            DataValue::Float(_) => DataType::Float,
            DataValue::Range { .. } => {
                panic!("DataValue literal to DataType::Range(.,.) not implemented yet!")
            }
            DataValue::Tuple(_) => {
                panic!("Literal value to type not implemented for Tuple type yet.!")
            }
            DataValue::Enumeration(_enum_value) => panic!("Can't create type from enum value"),
            DataValue::Bool(_) => DataType::Bool,
            DataValue::Lookup {
                varient: _v,
                content: c,
            } => {
                let element_type = if c.is_empty() {
                    DataType::Unresolved
                } else {
                    DataType::from_data_value(&c[0])
                };
                DataType::Lookup(Box::new(LookupType::Array {
                    contains_type: element_type,
                    index_type: DataType::Number,
                    size: Some(c.len()),
                    low_index: None,
                    high_index: None,
                }))
            }
            DataValue::User(n) => DataType::User(Box::new(UserType {
                name: n.to_owned(),
                definition: Box::new(DataType::Unresolved),
            })),
            DataValue::Unresolved => DataType::Unresolved,
            _ => panic!("DataValue to DataType not implemented for this type."),
        }
    }
}

impl DataValue {
    pub fn print_value(&self) -> String {
        match self {
            DataValue::Str(s) => s.to_string(),
            DataValue::Number(n) => format!("{}", n),
            DataValue::Bool(b) => format!("{}", b),
            DataValue::Lookup {
                varient: _v,
                content: data,
            } => format!("{:?}", &data),
            DataValue::User(u) => u.to_string(),
            DataValue::Unresolved => panic!("Unresolved value. Incomplete parsing or compilation!"),
            _ => panic!("Print not implemented for this DataValue varient!"),
        }
    }

    pub fn from_token_type(tt: &TokenType) -> DataValue {
        match tt {
            TokenType::Str(s) => DataValue::Str(s.to_owned()),
            TokenType::Number(n) => DataValue::Number(*n),
            TokenType::Integer(i) => DataValue::Integer(*i),
            TokenType::Float(f) => DataValue::Float(*f),
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
            DataValue::Integer(_) | DataValue::Float(_) | DataValue::Number(_) => {
                format!("NUMBER_VAL({})", &lexeme)
            }
            DataValue::Bool(_) => format!("BOOL_VAL({})", &lexeme),
            DataValue::Lookup { .. } => "//not implemented".to_string(),
            DataValue::User(ref _u) => "//not implemented".to_string(),
            DataValue::Unresolved => panic!("Unresolved value. Incomplete parsing or compilation!"),
            _ => panic!("Not implemented: DataValue can't be translated to object code!"),
        }
    }
}
