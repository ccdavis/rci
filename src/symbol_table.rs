use std::collections::HashMap;
use crate::types::DataType;
use crate::types::DataValue;
use crate::types::DeclarationType;
use crate::lex::Token;

pub struct SymbolTableEntry {
	pub name: String,
	// Where the symbol was declared in the source
	location: Token, // use the col and line
	data_type: DataType,
	data_value: DataValue, // some will be DataValue::None
	
	size: usize, // compile-time number of items if a collection type
	contains_type: DataType, // type inside of collection if any
	
	// For record / class members
	fields: Vec<Box<SymbolTableEntry>>,
}

impl SymbolTableEntry {

	pub fn new_var(location: &Token, name:&str, data_type: &DataType, data_value:&DataValue) -> Self {
		Self {
			location: location.clone(),
			name: name.to_owned(),
			entry_type: DeclarationType::Var,
			data_type: data_type.clone(),
			data_value: data_value.clone(),
			size:1,
			contains_type: DataType::Empty,
			fields: Vec::new(),									
		}
	}
}


pub struct SymbolTable {
	pub outer: Option<Box<SymbolTable>>,
	pub entries: HashMap<String, SymbolTableEntry>,
}

struct NotDeclaredError {
	message: String,
}

impl SymbolTable {

	pub fn global() -> Self {
		Self {
			entries: HashMap::new(),
			outer: None,
		}
	}
	
	pub fn extend_from(st: SymbolTable) -> SymbolTable {
		SymbolTable {
			entries: HashMap::new(),
			outer: Some(Box::new(st)),
			
		}
	}
	// super simple false if already declared in local scope
	pub fn add(&mut self, st: SymbolTableEntry) ->bool {
		if self.entries.contains_key(&st.name) {
			false
		} else {
			self.entries.insert(st.name, st);
			true
		}
	}
	
	pub fn lookup(&self, name: &str) -> Result<&SymbolTableEntry, NotDeclaredError> {
		match self.entries.get(name) {
			Some(ste) => Ok(ste), 
			None =>{
				if let Some(outer_scope) = &self.outer{
					outer_scope.lookup(name)					
				} else {
					let message = format!("{} not declared.",name);
					Err( NotDeclaredError { message })
				}
				
			}
		}
	} // fn 
	
}


