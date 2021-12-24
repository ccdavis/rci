use std::collections::HashMap;
use crate::types::DataType;
use crate::types::DataValue;
use crate::types::DeclarationType;
use crate::lex::Token;

#[derive(Clone,Debug)]
pub struct SymbolTableEntry {
	pub name: String,
	// Where the symbol was declared in the source
	pub location: Token, // use the col and line
	pub data_type: DataType,
	pub data_value: DataValue, // some will be DataValue::None
	pub entry_type: DeclarationType,
	
	pub size: usize, // compile-time number of items if a collection type
	pub contains_type: DataType, // type inside of collection if any
	
	// For record / class members
	pub fields: Vec<Box<SymbolTableEntry>>,
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

#[derive(Clone,Debug)]
pub struct SymbolTable {
	pub outer: Option<Box<SymbolTable>>,
	pub entries: HashMap<String, SymbolTableEntry>,
}


pub struct NotDeclaredError {
	pub message: String,
}

impl SymbolTable {

	pub fn global() -> Self {
		Self {
			entries: HashMap::new(),
			outer: None,
		}
	}
	
	pub fn extend(&mut self) -> SymbolTable {
		SymbolTable {
			entries: HashMap::new(),
			outer: Some(Box::new(self.clone())),
			
		}
	}
	// super simple false if already declared in local scope
	pub fn add(&mut self, st: SymbolTableEntry) ->bool {
		if self.entries.contains_key(&st.name) {
			false
		} else {
			self.entries.insert(st.name.to_owned(), st);
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


