use std::collections::HashMap;

use crate::expression::EvaluationError;
use crate::statement::ExecutionError;
use crate::types::DataValue;
use crate::types::ReturnValue;

// Consider using SlotMap here or return &ValueType instead
#[derive(Clone, Debug)]
pub struct Environment{
    storage: Vec<DataValue>,
    lookup: HashMap<String,DataValue >,
    callable_lookup: HashMap<String, ReturnValue>,
    parent: Option<Box<Environment>>,
}

impl Environment{
    pub fn new() -> Environment {
        Environment {
            storage: Vec::new(),
            lookup: HashMap::new(),
            callable_lookup: HashMap::new(),
            parent: None,
        }
    }

    pub fn extend(&mut self) ->Environment {
        Environment {
            parent: Some(Box::new(self.clone())),
            lookup: HashMap::new(),
            callable_lookup: HashMap::new(),
            storage: Vec::new(),
        }
    }

    pub fn define(&mut self, name: String, value: ReturnValue) -> usize {
        if let ReturnValue::CallableValue(ref callable) = value {
            if self.callable_lookup.contains_key(&name) {
                panic!("Function {} already defined", &name);
            }
            self.callable_lookup.insert(name, value);
            return 1;
        }

        if self.lookup.contains_key(&name) {
            // TODO: Figure out something better
			panic!("Can't redefine variables! Use assign instead.");
            
            
            0
        } else {
            self.storage.push(value.get().clone());
            let index = self.storage.len() - 1;
			
            self.lookup.insert(name, value.get().clone());

            index
        }
    }
	
    pub fn assign(&mut self, name: &str, value: ReturnValue) -> Result<(), EvaluationError> {
        if self.lookup.contains_key(name) {            
			println!("assigned  {} to value {:?}",name,&value);
			if let Some(k) = self.lookup.get_mut(name) {
				*k = value.get().clone();
				return Ok(())
			}
		}
		
				
		match &mut self.parent{
			Some(outer) =>{
				println!("Assign to outer scope");
				outer.assign(name, value)
			}
			None => {
				let message = format!("Cannot find '{}' in current scope.",name);
				Err(EvaluationError { message })
			}
		}
    }

    pub fn get_callable(&self, name: &str) -> Result<ReturnValue, EvaluationError> {
        match self.callable_lookup.get(name) {
            Some(return_value) => Ok(return_value.clone()),
            None => {
                if let Some(enclosing) = &self.parent {
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
            Some(data_value) => Ok(ReturnValue::Value(data_value.clone())),
            None => {
                if let Some(enclosing) = &self.parent {
                    enclosing.get(name)
                } else {
                    Err(EvaluationError {
                        message: format!("{} not defined.", name),
                    })
                }
            }
        }
    }
	// For  diagnostics
	pub fn dump_content(&self) {
		println!("Current: {:?}", self.lookup);
		if self.parent.is_none() {
			println!("At main::");
		}else{
			if let Some(enclosing) = &self.parent {
				enclosing.dump_content();
			}
			
		}
		
	}

    // This is only safe if you had previously gotten the index
    pub fn get_by_index(&self, index: usize) -> ReturnValue {
        ReturnValue::Value(self.storage[index].clone())
    }
}
