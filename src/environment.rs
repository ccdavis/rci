use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

use crate::expression::EvaluationError;


use crate::types::ReturnValue;
const TRACE:bool = false;


#[derive(Clone, Debug)]
pub struct Environment {
    parent: Option<Env>,
	// TODO replace this with an indexable container
	storage: RefCell<HashMap<String, ReturnValue>>,    
}

struct  Env(Rc<Environment>);

impl Env {
	pub fn global() -> Env {
		let envr = Environment {
			parent: None,
			storage: RefCell::new(HashMap::new()),
		};
		Rc::new(envr)		
    }

    pub fn new(enclosing: Option<Env>) -> Env{
		let envr = Environment {
			parent: enclosing,
			storage: RefCell::new(HashMap::new()),
		};
		Rc::new(envr)		
    }
	
	pub fn extend(&self) -> Env {
		Env::new(Some(Rc::clone(self)))
	}
	
}

impl Environment {


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
					parent_env.assign(name, value)								
				}
			}		
		}
	}
	
	pub fn assign_with_distance(&mut self, name: &str, value: ReturnValue, distance: usize) -> Result<(), EvaluationError> {		
		if distance == 0 {
			Ok(self.set_value(name, value))
		} else {
			if let Some(parent_env) = &self.parent{				
				parent_env.assign_with_distance(name,value,distance-1)
			} else {
				panic!("Distance of {} doesn't match current environment.",distance);
			}			
		}		
    }

    pub fn get(&self, name: &str) -> Result<ReturnValue, EvaluationError> {
		match self.storage.get(name) {
			None => {
				if let Some(enclosing) = &self.parent {					
					enclosing.get(name)
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
				enclosing.get_with_distance(name, distance - 1)
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
		if let Some(ref parent_env) = self.parent {			
			parent_env.dump_content(dist+1);
			
		}
    }
}
