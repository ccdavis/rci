use crate::types::DataValue;
use crate::types::ReturnValue;
use crate::types::ObjectCode;
use crate::types::*;

// Takes generic DataValue enums that carry literal values.
// Apply common operations or return error messages.

use DataValue::*;

pub fn add(left: &DataValue, right: &DataValue) -> Result<ReturnValue, String> {
    match (left, right) {
        (Number(l), Number(r)) => Ok(ReturnValue::Value(Number(l + r))),
        (Str(l), Str(r)) => {
            let new_string = "".to_string() + &l;
            Ok(ReturnValue::Value(Str(new_string + &r)))
        }
        _ => Err(format!(
            "Can't apply '+' to types '{}' and '{}'",
            left.print(),
            right.print()
        )),
    }
}

pub fn subtract(left: &DataValue, right: &DataValue) -> Result<ReturnValue, String> {
    match (left, right) {
        (Number(l), Number(r)) => Ok(ReturnValue::Value(Number(l - r))),
        _ => Err(format!(
            "Can't apply '-' to types '{}' and '{}'",
            left.print(),
            right.print()
        )),
    }
}

pub fn multiply(left: &DataValue, right: &DataValue) -> Result<ReturnValue, String> {
    match (left, right) {
        (Number(l), Number(r)) => Ok(ReturnValue::Value(Number(l * r))),
        _ => Err(format!(
            "Can't apply '*' to types '{}' and '{}'",
            left.print(),
            right.print()
        )),
    }
}

pub fn divide(left: &DataValue, right: &DataValue) -> Result<ReturnValue, String> {
    match (left, right) {
        (Number(l), Number(r)) => Ok(ReturnValue::Value(Number(l / r))),
        _ => Err(format!(
            "Can't apply '/' to types '{}' and '{}'",
            left.print(),
            right.print()
        )),
    }
}

fn make_bool(val: bool) -> Result<ReturnValue, String> {
    Ok(ReturnValue::Value(DataValue::Bool(val)))
}

pub fn compare_gt(left: &DataValue, right: &DataValue) -> Result<ReturnValue, String> {
    match (left, right) {
        (Number(l), Number(r)) => make_bool(l > r),
        _ => Err(format!(
            "Can't apply '+' to types '{}' and '{}'",
            left.print(),
            right.print()
        )),
    }
}

pub fn compare_lt(left: &DataValue, right: &DataValue) -> Result<ReturnValue, String> {
    match (left, right) {
        (Number(l), Number(r)) => make_bool(l < r),
        _ => Err(format!(
            "Can't apply '+' to types '{}' and '{}'",
            left.print(),
            right.print()
        )),
    }
}

pub fn compare_lte(left: &DataValue, right: &DataValue) -> Result<ReturnValue, String> {
    match (left, right) {
        (Number(l), Number(r)) => make_bool(l <= r),
        _ => Err(format!(
            "Can't apply '+' to types '{}' and '{}'",
            left.print(),
            right.print()
        )),
    }
}

pub fn compare_gte(left: &DataValue, right: &DataValue) -> Result<ReturnValue, String> {
    match (left, right) {
        (Number(l), Number(r)) => make_bool(l >= r),
        _ => Err(format!(
            "Can't apply '+' to types '{}' and '{}'",
            left.print(),
            right.print()
        )),
    }
}

pub fn not_equal(left: &DataValue, right: &DataValue) -> Result<ReturnValue, String> {
    match (left, right) {
        (Number(l), Number(r)) => make_bool(l != r),
        (Str(l), Str(r)) => make_bool(l != r),
        (Bool(l), Bool(r)) => make_bool(l != r),

        _ => Err(format!(
            "Can't apply '+' to types '{}' and '{}'",
            left.print(),
            right.print()
        )),
    }
}

pub fn equal(left: &DataValue, right: &DataValue) -> Result<ReturnValue, String> {
    match (left, right) {
        (Number(l), Number(r)) => make_bool(l == r),
        (Str(l), Str(r)) => make_bool(l == r),
        (Bool(l), Bool(r)) => make_bool(l == r),

        _ => Err(format!(
            "Can't apply '+' to types '{}' and '{}'",
            left.print(),
            right.print()
        )),
    }
}
