use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

use crate::expression::EvaluationError;


use crate::types::ReturnValue;
const TRACE:bool = true;

#[derive(Clone, Debug)]
pub struct Environment {
    parent: Option<Rc<RefCell<Environment>>>,
	// TODO replace this with an indexable container
	storage: HashMap<String, ReturnValue>,    
}

impl Environment {

    pub fn new() -> Environment {
		Environment {
			storage: HashMap::new(),
			parent: None,
		}        
    }

    pub fn extend(outer: Rc<RefCell<Environment>>) -> Self{
		if TRACE { println!("Extending with enclosing env.");}
		Self{
			parent: Some(outer),
			storage: HashMap::new(),
		}		        
    }

	pub fn new_local(&self) -> Self {
		Self{
				storage: HashMap::new(),
				parent: Some(Rc::new(RefCell::new(self.clone()))),
		}
		
	}
	


    // A minor convenience for adding callable since they respond to 'name()'
    pub fn define_callable(&mut self, value: ReturnValue) -> usize {
        if let ReturnValue::CallableValue(ref callable) = value {
            self.define(callable.name(), value)
        } else {
            panic!(
                "Cannot define a callable value with a non-callable: {}",
                &value.print()
            );
        }
    }

    pub fn define(&mut self, name: String, value: ReturnValue) -> usize {        
            self.storage.insert(name, value);
			// This will be the nth thing define which should
			// match the simple indexing scheme in the works.
            return self.storage.len()
        
    }

    // This should always succeed or panic
    fn set_value(&mut self,name: &str, value: ReturnValue) {        
        if let Some(k) = self.storage.get_mut(name) {
            *k = value;
        } else {
			panic!("{} not in local environment",name);
		}
    }


    pub fn assign(&mut self, name: &str, value: ReturnValue) -> Result<(), EvaluationError> {
		if self.storage.contains_key(name) {
			self.set_value(name, value);
			Ok(())
		 } else {
			match self.parent {
				None => {
					let message = format!("No definition for {}", name);
					Err( EvaluationError { message })
				}					
				Some(ref parent_env) => {
					let mut env = parent_env.borrow_mut();
					env.assign(name, value)								
				}
			}		
		}
	}
	
	pub fn assign_with_distance(&mut self, name: &str, value: ReturnValue, distance: usize) -> Result<(), EvaluationError> {		
		if distance == 0 {
			Ok(self.set_value(name, value))
		} else {
			if let Some(parent_env) = &self.parent{
				let mut p = parent_env.borrow_mut();
				p.assign_with_distance(name,value,distance-1)
			} else {
				panic!("Distance of {} doesn't match current environment.",distance);
			}			
		}		
    }

    pub fn get(&self, name: &str) -> Result<ReturnValue, EvaluationError> {
		match self.storage.get(name) {
			None => {
				if let Some(enclosing) = &self.parent {
					let e = enclosing.borrow();
					e.get(name)
				 } else {
					Err(EvaluationError {
						message: format!("{} not defined.", name)})
						
				 }
			}
			Some(value) => Ok(value.clone()),
		}				
    }
	
	pub fn get_with_distance(&self, name: &str, distance: usize) ->Result<ReturnValue, EvaluationError> {
		if distance == 0 {
			if let Some(value) = self.storage.get(name) {
				Ok(value.clone())
			} else {
				Err(EvaluationError { message: format!("{} not defined.", name)})
			}			
		} else {
			if let Some(enclosing) = &self.parent {
				let e = enclosing.borrow();
				e.get_with_distance(name, distance - 1)
			} else {
				panic!("Environments don't match variable resolution distances!");
			}
		}
				
	}
			    
    pub fn dump_content(&self, dist:usize) {	
        if dist == 0 {println!("== Current environment ----------");}
		println!("{}: {:?}",dist, self.storage);
		println!("");
		if dist == 0 {println!("== Enclosing Environments: ------------");}
		self.dump_content(dist + 1);		
    }
}
