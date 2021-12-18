use std::rc::Rc;
use crate::lex::TokenType;
#[derive(Clone,Debug)]
pub enum DataValue{
	Str(String),
	Number(f64),
	Bool(bool),
		
}


// Simple data types and values
#[derive(Clone,Debug)]
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
	
	pub fn from_data_value(dv: &DataValue) -> DataType {
		match dv {
			DataValue::Str(_) => DataType::Str,
			DataValue::Number(_) => DataType::Number,
			DataValue::Bool(_) => DataType::Bool,
		}
	}
}

impl DataValue {

	pub fn print_value(&self) -> String {		
		match self {
			DataValue::Str(s) => s.to_string(),
			DataValue::Number(n) => format!("{}",n),
			DataValue::Bool(b) => format!("{}",b),
		}
	}

	
	pub fn from_token_type(tt: &TokenType) -> DataValue {
		match tt {
			TokenType::Str(s) => DataValue::Str(s.to_owned()),
			TokenType::Number(n) => DataValue::Number(*n),
			TokenType::False => DataValue::Bool(false),
			TokenType::True => DataValue::Bool(true),
			_ => panic!("Not convertable to a ValueType: {}",tt.print()),
		}
	}
	
	pub fn print(&self) -> String {
		format!("{:?}", self)		
	}
}


#[derive(Clone, Debug)]
pub enum ReturnValue {
    Reference(Rc<DataValue>),
    Value(DataValue),
	None, // This is for optional return of data, not 'nil' or null
}

impl ReturnValue {
    pub fn new_ref(value: DataValue) -> Self {
        ReturnValue::Reference(Rc::new(value))
    }

    pub fn new_val(value: DataValue) -> Self {
        ReturnValue::Value(value)
    }

    pub fn get(&self) -> &DataValue {
        match self {
            ReturnValue::Reference(v) => &*v,
            ReturnValue::Value(v) => &v,
			ReturnValue::None => panic!("get() is for internal use and should never be called on None varient."),
        }
    }

    pub fn clone_or_increment_count(&self) -> ReturnValue {
        match self {
            ReturnValue::Reference(v) => ReturnValue::Reference(Rc::clone(&v)),
            ReturnValue::Value(v) => ReturnValue::Value(v.clone()),
			ReturnValue::None => ReturnValue::None,
        }
    }

    pub fn print(&self) -> String {
        self.get().print_value()
    }
}

