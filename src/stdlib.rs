use crate::types::*;
use crate::symbol_table::*;
use crate::statement::ExecutionError;
use crate::environment::*;

use dyn_clonable::*;

fn val_prm( name:&str, data_type: DataType) -> Box<SymbolTableEntry> {
	Box::new(
		SymbolTableEntry::new_stdlib_val(				
				name,
				&data_type
			))
}

fn var_prm(name:&str, data_type: DataType) -> Box<SymbolTableEntry> {
	Box::new(
		SymbolTableEntry::new_stdlib_var(				
				name,
				&data_type
			))
}

#[derive(Clone)]
pub struct ClockFunc {}

impl Callable for ClockFunc {

	fn name(&self) -> String {
		"clock".to_string()
	}
    
    fn arity(&self) -> usize {
        0
    }
	
	fn params(&self) -> Vec<Box<SymbolTableEntry>> {
		Vec::new()
	}

    fn return_type(&self) -> &DataType {
        &DataType::Number
    }

    fn call(
        &mut self,
        envr: &mut Environment,
        arguments: Vec<ReturnValue>,
    ) -> Result<ReturnValue, ExecutionError> {
        use std::time::{SystemTime, UNIX_EPOCH};
        let now = SystemTime::now();
        let since_epoch = now
            .duration_since(UNIX_EPOCH)
            .expect("Impossible time difference.");

        Ok(ReturnValue::Value(DataValue::Number(
            since_epoch.as_millis() as f64,
        )))
    }
}


#[derive(Clone)]
pub struct SqrFunc{} 

impl Callable for SqrFunc {

	fn name(&self) -> String {
		"sqr".to_string()
	}
    
    fn arity(&self) -> usize {
        1
    }
	
	fn params(&self) -> Vec<Box<SymbolTableEntry>> {
		vec![ val_prm("sqr", DataType::Number)]
	}

    fn return_type(&self) -> &DataType {
        &DataType::Number
    }

    fn call(
        &mut self,
        envr: &mut Environment,
        arguments: Vec<ReturnValue>,
    ) -> Result<ReturnValue, ExecutionError> {
        
        let base = arguments[0].get_as_number()?;
        Ok(ReturnValue::Value(DataValue::Number(
            base * base
        )))
    }
}



struct SqrRtFunc {}
struct PowerFunc{}
struct AbsFunc {}
struct FloorFunc {}
struct CeilingFunc {}
struct logFunc {}
struct RoundFunc {}
struct RandomFunc {}
struct TostringFunc {}

struct UpcaseFunc {}
struct DowncaseFunc {}
struct LenFunc {}
struct ReplaceRangeFunc {}
struct ReplacePatternFunc{}
struct SubstringFunc {}
struct TrimFunc {}
struct TrimleftFunc{}
struct TrimrightFunc {}
struct LeftjustFunc{}
struct RightjustFunc{}
struct TonumberFunc{}

struct TolinesFunc {}  // Keeps newlines
struct SplitFunc {} // split on any string
struct JoinFunc{} // Join with any string

struct ReadStdinFunc{}
struct WriteStderrFunc{}
struct WriteStdoutFunc {}
struct ReadTextFileFunc{}
struct WriteTextFileFunc{}





