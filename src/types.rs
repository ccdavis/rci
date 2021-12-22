use std::rc::Rc;
use std::fmt;
use crate::lex::TokenType;

// Simple data types and values
#[derive(Clone,Debug,PartialEq)]
pub enum DataType {
	Str,
	Number,
	Bool,
	User(String),
	Empty, // Like the '()' expressiontype in Rust
	Unresolved, // Incomplete type checker results
}

#[derive(Clone,Debug)]
pub enum DataValue{
	Str(String),
	Number(f64),
	Bool(bool),
	User(String), // just give the name for now
		
}




impl fmt::Display for DataType {

	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result{
		let name = match self{
			DataType::Number=>"number",
			DataType::Str => "string",
			DataType::Bool => "boolean",
			DataType::Empty => "empty",
			DataType::User(u) => u.as_str(),
			DataType::Unresolved => "UNRESOLVED",			
		};
		write!(f,"{}",name) 
	}
}


impl DataType {
	pub fn from_token_type(tt: &TokenType) -> Option<DataType>{
		match tt {
			TokenType::NumberType => Some(DataType::Number),
			TokenType::StringType => Some(DataType::Str),
			TokenType::BooleanType => Some(DataType::Bool),			
			TokenType::Identifier(n) =>  Some(DataType::User(n.to_owned())),
			_ => None,
		}
	}
	
	pub fn from_data_value(dv: &DataValue) -> DataType {
		match dv {
			DataValue::Str(_) => DataType::Str,
			DataValue::Number(_) => DataType::Number,
			DataValue::Bool(_) => DataType::Bool,
			DataValue::User(n) =>  DataType::User(n.to_owned()),
		}
	}
}

impl DataValue {

	pub fn print_value(&self) -> String {		
		match self {
			DataValue::Str(s) => s.to_string(),
			DataValue::Number(n) => format!("{}",n),
			DataValue::Bool(b) => format!("{}",b),
			DataValue::User(u) => format!("{}",u.to_string()),
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

