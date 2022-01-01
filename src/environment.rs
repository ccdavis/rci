use std::collections::HashMap;

use crate::expression::EvaluationError;
use crate::statement::ExecutionError;
use crate::types::DataValue;
use crate::types::ReturnValue;

// Consider using SlotMap here or return &ValueType instead
#[derive(Clone, Debug)]
pub struct Environment {
    frames: Vec<HashMap<String, DataValue>>,
    callable_frames: Vec<HashMap<String, ReturnValue>>,
    current_frame: usize,
    //parent: Option<&'a Environment<'a>>,
}

impl Environment {
    pub fn new() -> Environment {
        let mut first_frame = Environment {
            frames: Vec::new(),
            callable_frames: Vec::new(),
            current_frame: 0,
        };
        first_frame.frames.push(HashMap::new());
        first_frame.callable_frames.push(HashMap::new());
        first_frame
    }

    pub fn extend(&mut self) -> usize {
        self.current_frame += 1;
        if self.frames.len() <= self.current_frame {
            self.frames.push(HashMap::new());
            self.callable_frames.push(HashMap::new());
        }
        self.current_frame
    }

    pub fn retract(&mut self) -> usize {
        if self.current_frame == 0 {
            panic!("Can't leave the base environment!");
        }

        self.frames[self.current_frame].clear();
        self.callable_frames[self.current_frame].clear();
        self.current_frame -= 1;
        self.current_frame
    }

    fn frame_contains(&self, frame: usize, name: &str) -> bool {
        self.frames[frame].contains_key(name)
    }

    pub fn current_frame_contains(&self, name: &str) -> bool {
        self.frames[self.current_frame].contains_key(name)
    }

    pub fn current_callable_frame_contains(&self, name: &str) -> bool {
        self.callable_frames[self.current_frame].contains_key(name)
    }

    pub fn define(&mut self, name: String, value: ReturnValue) -> usize {
        if let ReturnValue::CallableValue(ref callable) = value {
            if self.current_callable_frame_contains(&name) {
                panic!("Function {} already defined. The parser and type checker should have caught this.", &name);
            }
            self.callable_frames[self.current_frame].insert(name, value);
            return self.current_frame;
        }

        if self.current_frame_contains(&name) {
            // TODO: Figure out something better
            panic!("Can't redefine variables! Use assign instead. The parser and type checker should have caught this.");
            self.current_frame
        } else {
            self.frames[self.current_frame].insert(name, value.get().clone());
            self.current_frame
        }
    }

    // This should always succeed or panic
    fn set_value(&mut self, frame: usize, name: &str, value: ReturnValue) {
        if frame >= self.frames.len() {
            panic!("No stack frame {}", frame);
        }
        println!("assigned  {} to value {:?}", name, &value);
        if let Some(k) = self.frames[frame].get_mut(name) {
            *k = value.get().clone();
        }
    }

    // Right now callables aren't assignable
    pub fn assign(&mut self, name: &str, value: ReturnValue) -> Result<(), EvaluationError> {
        let mut frame = self.current_frame;
        loop {
            if self.frame_contains(frame, name) {
                self.set_value(frame, name, value);
                return Ok(());
            }

            if 0 == frame {
                let message = format!("Cannot find '{}' in current scope.", name);
                return Err(EvaluationError { message });
            }
            frame -= 1;
        }
    }

    pub fn get(&self, name: &str) -> Result<ReturnValue, EvaluationError> {
        let mut frame = self.current_frame;
        loop {
            if let Some(callable) = self.callable_frames[frame].get(name) {
                return Ok(callable.clone());
            }

            if let Some(data_value) = self.frames[frame].get(name) {
                return Ok(ReturnValue::Value(data_value.clone()));
            }

            if 0 == frame {
                return Err(EvaluationError {
                    message: format!("{} not defined.", name),
                });
            }
            frame -= 1;
        }
    }
    // For  diagnostics
    pub fn dump_content(&self) {
        println!("Current: {:?}", self.current_frame);
        let mut frame = self.current_frame;
        loop {
            let this_frame = &self.frames[frame];
            println!("{}: {:?}", frame, &this_frame);
            let this_callable = &self.callable_frames[frame];
            println!("Callables: {:?}", &this_callable);
            if 0 == frame {
                break;
            }
            frame -= 1;
        }
    }
}
