use std::collections::HashMap;

use crate::expression::EvaluationError;
use crate::expression::ReturnValue;
use crate::statement::ExecutionError;

// Consider using SlotMap here or return &ValueType instead
#[derive(Clone)]
pub struct Environment<'a> {
    storage: Vec<ReturnValue>,
    lookup: HashMap<String, usize>,
    parent: Option<&'a Environment<'a>>,
}

impl Environment<'_> {
    pub fn new<'a>() -> Environment<'a> {
        Environment {
            storage: Vec::new(),
            lookup: HashMap::new(),
            parent: None,
        }
    }

    pub fn extend<'a>(&'a self) -> Environment<'a> {
        Environment {
            parent: Some(self),
            lookup: HashMap::new(),
            storage: Vec::new(),
        }
    }

    pub fn define(&mut self, name: String, value: ReturnValue) -> usize {
        if self.lookup.contains_key(&name) {
            // TODO: Figure out something better
            let index: usize = *self.lookup.get(&name).unwrap();
            self.storage[index] = value.clone();
            index
        } else {
            self.storage.push(value);
            let index = self.storage.len() - 1;
            self.lookup.insert(name, index);
            index
        }
    }

    pub fn assign(&mut self, name: &str, value: ReturnValue) -> Result<(), EvaluationError> {
		if self.lookup.contains_key(name) {
            // TODO: Figure out something better
            let index: usize = *self.lookup.get(name).unwrap();
            self.storage[index] = value.clone();			
			Ok(())
        } else {
			let message = format!("Nothing named {} found in current scope.",name);
			Err( EvaluationError { message })
			
		}                       
    }

    pub fn get(&self, name: &str) -> Result<ReturnValue, EvaluationError> {
        match self.lookup.get(name) {
            Some(index) => Ok(self.storage[*index].clone()),
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
        self.storage[index].clone()
    }
}
