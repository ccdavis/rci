use crate::lex::TokenType;
use crate::expression::ReturnValue;


// Takes generic TokenType enums that carry literal values.
// Apply common operations or return error messages.


use TokenType::*;

pub fn add(left: &TokenType, right: &TokenType) -> Result<ReturnValue, String> {
    match (left,right) {
        (Number(l), Number(r)) => 
			Ok(ReturnValue::Value(Number(l + r))),
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

pub fn subtract(left: &TokenType, right: &TokenType) -> Result<ReturnValue, String> {
    match (left,right) {
        (Number(l), Number(r)) => 
			Ok(ReturnValue::Value(Number(l - r))),     
        _ => Err(format!(
            "Can't apply '+' to types '{}' and '{}'",
            left.print(),
            right.print()
        )),
    }    
}

pub fn multiply(left: &TokenType, right: &TokenType) -> Result<ReturnValue, String> {	
	match (left,right) {
		(Number(l), Number(r)) => 
			Ok(ReturnValue::Value(Number(l * r))),     
        _ => Err(format!(
            "Can't apply '+' to types '{}' and '{}'",
            left.print(),
            right.print()
        )),
    }    
}

pub fn divide(left: &TokenType, right: &TokenType) -> Result<ReturnValue, String> {	
	match (left,right) {
		(Number(l), Number(r)) => 
			Ok(ReturnValue::Value(Number(l / r))),     
        _ => Err(format!(
            "Can't apply '+' to types '{}' and '{}'",
            left.print(),
            right.print()
        )),
    }    
}

fn make_bool(val:bool) -> Result<ReturnValue,String> {
	Ok(if val {
		ReturnValue::Value(True)
	} else {
		ReturnValue::Value(False)
	})
}

pub fn compare_gt(left: &TokenType, right: &TokenType) -> Result<ReturnValue, String> {	
	match (left,right) {
		(Number(l), Number(r)) => make_bool(l > r),			
        _ => Err(format!(
            "Can't apply '+' to types '{}' and '{}'",
            left.print(),
            right.print()
        )),
    }    
}

pub fn compare_lt(left: &TokenType, right: &TokenType) -> Result<ReturnValue, String> {
	match (left,right) {
		(Number(l), Number(r)) => make_bool(l < r),			
        _ => Err(format!(
            "Can't apply '+' to types '{}' and '{}'",
            left.print(),
            right.print()
        )),
    }    
	
}

pub fn compare_lte(left: &TokenType, right: &TokenType) -> Result<ReturnValue, String> {	
	match (left,right) {
		(Number(l), Number(r)) => make_bool(l <= r),			
        _ => Err(format!(
            "Can't apply '+' to types '{}' and '{}'",
            left.print(),
            right.print()
        )),
    }    	
}

pub fn compare_gte(left: &TokenType, right: &TokenType) -> Result<ReturnValue, String> {	
	match (left,right) {
		(Number(l), Number(r)) => make_bool(l >= r),			
        _ => Err(format!(
            "Can't apply '+' to types '{}' and '{}'",
            left.print(),
            right.print()
        )),
    }    	
}

pub fn not_equal(left: &TokenType, right: &TokenType) -> Result<ReturnValue, String> {	
	match (left,right) {
		(Number(l), Number(r)) => make_bool(l != r),			
		(Str(l), Str(r)) => make_bool(l != r),
		(True, True) => Ok(ReturnValue::Value(False)),
		(False, False) => Ok(ReturnValue::Value(False)),
		(True, False) => Ok(ReturnValue::Value(True)),
		(False, True) => Ok(ReturnValue::Value(True)),
        _ => Err(format!(
            "Can't apply '+' to types '{}' and '{}'",
            left.print(),
            right.print()
        )),
    }    	
}

pub fn equal(left: &TokenType, right: &TokenType) -> Result<ReturnValue, String> {
	match (left,right) {
		(Number(l), Number(r)) => make_bool(l == r),			
		(Str(l), Str(r)) => make_bool(l == r),			
		(True, True) => Ok(ReturnValue::Value(True)),
		(False, False) => Ok(ReturnValue::Value(True)),
		(True, False) => Ok(ReturnValue::Value(False)),
		(False, True) => Ok(ReturnValue::Value(False)),
        _ => Err(format!(
            "Can't apply '+' to types '{}' and '{}'",
            left.print(),
            right.print()
        )),
    }    	
	
}
