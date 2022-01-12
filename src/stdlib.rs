use crate::environment::*;
use crate::statement::EarlyReturn;
use crate::statement::ExecutionError;
use crate::symbol_table::*;
use crate::types::*;

use dyn_clonable::*;

fn val_prm(entry_num:usize, name: &str, data_type: DataType) -> Box<SymbolTableEntry> {
    Box::new(SymbolTableEntry::new_stdlib_val(entry_num,name, &data_type))
}

fn var_prm(entry_num: usize, name: &str, data_type: DataType) -> Box<SymbolTableEntry> {
    Box::new(SymbolTableEntry::new_stdlib_var(entry_num,name, &data_type))
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

    fn call(&mut self, arguments: Vec<ReturnValue>) -> Result<ReturnValue, EarlyReturn> {
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
pub struct SqrFunc {}

impl Callable for SqrFunc {
    fn name(&self) -> String {
        "sqr".to_string()
    }

    fn arity(&self) -> usize {
        1
    }

    fn params(&self) -> Vec<Box<SymbolTableEntry>> {
        vec![val_prm(0, "sqr", DataType::Number)]
    }

    fn return_type(&self) -> &DataType {
        &DataType::Number
    }

    fn call(&mut self, arguments: Vec<ReturnValue>) -> Result<ReturnValue, EarlyReturn> {
        let base_result = arguments[0].get_as_number();
        match base_result {
            Ok(base) => Ok(ReturnValue::Value(DataValue::Number(base * base))),
            Err(err) => Err(EarlyReturn::Error(err)),
        }
    }
}

struct SqrRtFunc {}
struct PowerFunc {}
struct AbsFunc {}
struct FloorFunc {}
struct CeilingFunc {}
struct LogFunc {}
struct RoundFunc {}
struct RandomFunc {}
struct TostringFunc {}

struct UpcaseFunc {}
struct DowncaseFunc {}
struct LenFunc {}
struct ReplaceRangeFunc {}
struct ReplacePatternFunc {}
struct SubstringFunc {}
struct TrimFunc {}
struct TrimleftFunc {}
struct TrimrightFunc {}
struct LeftjustFunc {}
struct RightjustFunc {}
struct TonumberFunc {}

struct TolinesFunc {} // Keeps newlines
struct SplitFunc {} // split on any string
struct JoinFunc {} // Join with any string

struct ReadStdinFunc {}
struct WriteStderrFunc {}
struct WriteStdoutFunc {}
struct ReadTextFileFunc {}
struct WriteTextFileFunc {}
