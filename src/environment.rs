use std::collections::HashMap;

use crate::expression::EvaluationError;
use crate::types::DataValue;
use crate::types::ReturnValue;
use crate::statement::ExecutionError;

// Consider using SlotMap here or return &ValueType instead
#[derive(Clone)]
pub struct Environment<'a> {
    storage: Vec<DataValue>,
    lookup: HashMap<String, usize>,	
	callable_lookup: HashMap<String,ReturnValue>,
    parent: Option<&'a Environment<'a>>,
}

impl Environment<'_> {
    pub fn new<'a>() -> Environment<'a> {
        Environment {
            storage: Vec::new(),
            lookup: HashMap::new(),
			callable_lookup: HashMap::new(),
            parent: None,
        }
    }

    pub fn extend<'a>(&'a self) -> Environment<'a> {
        Environment {
            parent: Some(self),
            lookup: HashMap::new(),
			callable_lookup: HashMap::new(),
            storage: Vec::new(),
        }
    }
	
	
    pub fn define(&mut self, name: String, value: ReturnValue) -> usize {
		if let ReturnValue::CallableValue(ref callable) = value {
			if self.callable_lookup.contains_key(&name) {
				panic!("Function {} already defined",&name);
			}
			self.callable_lookup.insert(name, value);
			return 1;
		}
		
        if self.lookup.contains_key(&name) {
            // TODO: Figure out something better
            let index: usize = *self.lookup.get(&name).unwrap();
            self.storage[index] = value.get().clone();
            index
        } else {            
            self.storage.push(value.get().clone());
            let index = self.storage.len() - 1;
            self.lookup.insert(name, index);

            index
        }
    }

    pub fn assign(&mut self, name: &str, value: ReturnValue) -> Result<(), EvaluationError> {
        if self.lookup.contains_key(name) {
            // TODO: Figure out something better
            let index: usize = *self.lookup.get(name).unwrap();
            self.storage[index] = value.get().clone();
            Ok(())
        } else {
            let message = format!("Nothing named {} found in current scope.", name);
            Err(EvaluationError { message })
        }
    }
	
	pub fn get_callable(&self, name: &str) -> Result<ReturnValue, EvaluationError> {
		match self.callable_lookup.get(name) {
			Some(return_value) => Ok(return_value.clone()),
			None => {
                if let Some(enclosing) = self.parent {
                    enclosing.get_callable(name)
                } else {
                    Err(EvaluationError {
                        message: format!("{} not defined.", name),
                    })
                }
			}
		}
	}

    pub fn get(&self, name: &str) -> Result<ReturnValue, EvaluationError> {
		// Refactor the storage so this check isn't needed; we should be storing
		// ReturnValue in 'storage' instead of DataValue.
		if self.callable_lookup.contains_key(name) {
			return self.get_callable(name);
		}
		
        match self.lookup.get(name) {
            Some(index) => 
				Ok(ReturnValue::Value(self.storage[*index].clone())),
            None => {
                
                if let Some(enclosing) = self.parent {
                    enclosing.get(name)
                } else {
                    Err(EvaluationError {
                        message: format!("{} not defined.", name),
                    })
                }
            }
        }
    }

    // This is only safe if you had previously gotten the index
    pub fn get_by_index(&self, index: usize) -> ReturnValue {
        ReturnValue::Value(self.storage[index].clone())
    }
}
