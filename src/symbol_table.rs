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

	// TODO: Change these init methods to take the Declaration / Statement Node struct types

	pub fn new_var(location: &Token, name:&str, data_type: &DataType, data_value:&DataValue) -> Self {
		Self {
			location: location.clone(),
			name: name.to_owned(),
			entry_type: DeclarationType::Var,
			data_type: data_type.clone(),
			data_value: data_value.clone(),
			size:1,
			contains_type: DataType::Empty,
			fields:Vec::new(),
		}
	}
	
	pub fn new_val(location: &Token, name:&str, data_type: &DataType, data_value:&DataValue) -> Self {
		Self {
			location: location.clone(),
			name: name.to_owned(),
			entry_type: DeclarationType::Val,
			data_type: data_type.clone(),
			data_value: data_value.clone(),
			size:1,
			contains_type: DataType::Empty,
			fields:Vec::new(),
		}
	}
	
	pub fn new_copy(location: &Token, name:&str, data_type: &DataType, data_value:&DataValue) -> Self {
		Self {
			location: location.clone(),
			name: name.to_owned(),
			entry_type: DeclarationType::Copy,
			data_type: data_type.clone(),
			data_value: data_value.clone(),
			size:1,
			contains_type: DataType::Empty,
			fields:Vec::new(),
		}
	}
	
	
	
	
	pub fn new_fun(location: &Token, name:&str, arg_types: Vec<(Token,Token,DataType)>, data_type: &DataType) -> Self {
		// The arg_types are passed in as 'param-type, name, data_type. The param-type is either nothing "", "var" 
		// or "copy". "" is the same as "val", a read-only value; var is a mutable reference; "copy" makes a copy which
		// is internally mutable but doesn't effect the passed in data.
		let mut argument_definitions:Vec<Box<SymbolTableEntry>> = Vec::new();
		for arg_def in &arg_types {
					
			let param_type = &arg_def.0.token_type;			
			let arg_name = &arg_def.1.identifier_string();
			let arg_type = &arg_def.2;
			
			let arg = if matches!(param_type , TokenType::Var) {
				SymbolTableEntry::new_var(location,arg_name,arg_type, &DataValue::Unresolved) 
			} else if matches!(param_type, TokenType::Val) {
				SymbolTableEntry::new_val(location, arg_name, arg_type, &DataValue::Unresolved)
			} else if matches!(param_type,  TokenType::Copy) {
				SymbolTableEntry::new_copy(location, arg_name, arg_type, &DataValue::Unresolved)
			} else {
				// It should never come to this, the parser should catch illegal param types
				panic!("Cannot add to symbol table, param type {} not recognized.",&arg_def.0);										
			};
			argument_definitions.push(Box::new(arg));
		}
							
		Self {
			location: location.clone(),
			name: name.to_owned(),
			entry_type: DeclarationType::Fun,			
			data_type: data_type.clone(),			
			data_value: DataValue::Unresolved,
			size:1,
			contains_type: DataType::Empty,
			fields: argument_definitions,
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


