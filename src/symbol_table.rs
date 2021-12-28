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
			entry_type: DeclarationType::Cpy,
			data_type: data_type.clone(),
			data_value: data_value.clone(),
			size:1,
			contains_type: DataType::Empty,
			fields:Vec::new(),
		}
	}
	
	
	pub fn new_param(decl_type: DeclarationType, name: Token,data_type: DataType) -> SymbolTableEntry {
		let location = name.clone();
		let param_name = name.identifier_string();
		
		match decl_type {
			DeclarationType::Var =>SymbolTableEntry::new_var(&location, &param_name, &data_type, &DataValue::Unresolved),
			DeclarationType::Val =>SymbolTableEntry::new_val(&location, &param_name, &data_type, &DataValue::Unresolved),
			DeclarationType::Cpy => SymbolTableEntry::new_copy(&location, &param_name, &data_type, &DataValue::Unresolved),
			_ => panic!("Can't use decl type '{:?}' in a parameter definition at {}!",
					decl_type, &name.print()),
		}	
	}
		
	
	pub fn new_fun(
		location: &Token, 
		name:&str, 
		params: Vec<Box<SymbolTableEntry>>, 
		data_type: &DataType) -> Self {
		
		Self {
			location: location.clone(),
			name: name.to_owned(),
			entry_type: DeclarationType::Fun,			
			data_type: data_type.clone(),			
			data_value: DataValue::Unresolved,
			size:1,
			contains_type: DataType::Empty,
			fields: params,
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


