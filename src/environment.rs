use crate::expression::ReturnValue;
use crate::expression::EvaluationError;

use std::HashMap;
// Consider using SlotMap here
pub struct Environment {
	storage : Vec<ReturnValue>,
	lookup: HashMap<String,usize>,		
}

impl Environment {

	pub fn new() -> Self {
		Self { storage: Vec::new(), lookup: HashMap::new()}
	}

	pub fn define(&mut self, name: String, value:ReturnValue) -> usize {
		if self.lookup.contains_key(&name) {
			// TODO: Figure out something better
			let index = self.lookup.get(&name).unwrap();
			self.storage[index] = value;
			index
		} else {		
			self.storage.push(value);
			let index = self.storage.len() -1;
			self.lookup.insert(&name, index);
			index		
		}
	}
	
	pub fn get(&self, name:String) -> Result<ReturnValue, EvaluationError> {
		match self.lookup.get(&name) {
			Some(index) => self.storage[index],
			None => Err(EvaluationError { 
				message : format!("{} not defined.",&name) })
		}
			
						
	}
	
	// This is only safe if you had previously gotten the index
	pub fn get_by_index(&self, index:usize) -> ReturnValue {
		self.storage[index]
	}
	
	
}

