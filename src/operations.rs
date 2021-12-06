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

pub fn subtract(left: &TokenType, right: &TokenType) -> Result<TokenType, String> {
    Err("not implemented".to_string())
}

pub fn multiply(left: &TokenType, right: &TokenType) -> Result<TokenType, String> {
	Err("not implemented".to_string())
}

pub fn divide(left: &TokenType, right: &TokenType) -> Result<TokenType, String> {
	Err("not implemented".to_string())    
}

pub fn compare_gt(left: &TokenType, right: &TokenType) -> Result<TokenType, String> {
	Err("not implemented".to_string())        
}

pub fn compare_lt(left: &TokenType, right: &TokenType) -> Result<TokenType, String> {
	Err("not implemented".to_string())            
}

pub fn compare_lte(left: &TokenType, right: &TokenType) -> Result<TokenType, String> {
	Err("not implemented".to_string())            
}

pub fn compare_gte(left: &TokenType, right: &TokenType) -> Result<TokenType, String> {
	Err("not implemented".to_string())                
}

pub fn not_equal(left: &TokenType, right: &TokenType) -> Result<TokenType, String> {
	Err("not implemented".to_string())                
}

pub fn equal(left: &TokenType, right: &TokenType) -> Result<TokenType, String> {
	Err("not implemented".to_string())                
}
