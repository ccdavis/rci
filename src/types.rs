use std::rc::Rc;
use crate::lex::TokenType;


// Simple data types and values
pub enum DataType {
	Str,
	Number,
	Bool,
}

impl DataType {
	pub fn from_token_type(tt: &TokenType) -> DataType{
		match tt {
			TokenType::NumberType => DataType::Number,
			TokenType::StringType => DataType::Str,
			TokenType::BooleanType => DataType::Bool,			
			_ => panic!("Not a token type derived from parsing a type name in the code: {}",tt.print()),
		}
	}
}

pub enum DataValue{
	Str(String),
	Number(f64),
	Bool(bool),
		
}

impl DataValue {
	pub fn from_token_type(tt: &TokenType) -> DataValue {
		match tt {
			TokenType::Str(s) => DataValue::Str(s.to_owned()),
			TokenType::Number(n) => DataValue::Number(*n),
			TokenType::False => DataValue::Bool(false),
			TokenType::True => DataValue::Bool(true),
			_ => panic!("Not convertable to a ValueType: {}",tt.print()),
		}
	}
}


#[derive(Clone, Debug)]
pub enum ReturnValue {
    Reference(Rc<TokenType>),
    Value(TokenType),
}

impl ReturnValue {
    pub fn new_ref(value: TokenType) -> Self {
        ReturnValue::Reference(Rc::new(value))
    }

    pub fn new_val(value: TokenType) -> Self {
        ReturnValue::Value(value)
    }

    pub fn get(&self) -> &TokenType {
        match self {
            ReturnValue::Reference(v) => &*v,
            ReturnValue::Value(v) => &v,
        }
    }

    pub fn clone_or_increment_count(&self) -> ReturnValue {
        match self {
            ReturnValue::Reference(v) => ReturnValue::Reference(Rc::clone(&v)),
            ReturnValue::Value(v) => ReturnValue::Value(v.clone()),
        }
    }

    pub fn print(&self) -> String {
        self.get().print_value()
    }
}

