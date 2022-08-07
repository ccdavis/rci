
#ifndef RCI_OPERATIONS
#define RCI_OPERATIONS


typedef enum { _NEGATIVE_, _NOT_} rci_unary_operators;

typedef enum { _ADD_, _SUB_, _MUL_, _DIV_, _MOD_, _POW_, _SHL_, _SHR_,  
			_LT_, _GT_, _LTE_, _GTE_, _EQ_, _NE_, _AND_, _OR_ }  
	rci_binary_operators;
	
	
rci_value comparison_binary_operation(rci_binary_operators op, rci_value left, rci_value  right) {
	rci_value result = BOOL_VAL(false);
	if (IS_ENUM(left) && IS_ENUM(right)) {
		switch (op) {
			case _EQ_ : {				
				result = BOOL_VAL(AS_ENUM(left) == AS_ENUM(right));
			}break;
			case _NE_: {
				result = BOOL_VAL(AS_ENUM(left) != AS_ENUM(right));
			}break;
			default:{
				code_gen_error("Comparison operator not implemented for enumeration values.");
			}
		}
		
		return result;			
	}
	
	switch(op) {
		case _LT_ :{
			result = BOOL_VAL(AS_NUMBER(left) < AS_NUMBER(right));
		} break;
		case _EQ_ :{
			result = BOOL_VAL(left.as._number == right.as._number);
		}break;
		case _GT_ : {
			result = BOOL_VAL(left.as._number > right.as._number);
		}break;
		case _NE_ : {
			result = BOOL_VAL(left.as._number != right.as._number);
		}break;
		case _GTE_ : {
			result = BOOL_VAL(left.as._number >= right.as._number);
		}break;
		case _LTE_ : {
			result = BOOL_VAL(left.as._number <= right.as._number);
		}break;
		default:code_gen_error("op for comparison not implemented");					
	}
	return result;	
}

rci_value logical_binary_operation(rci_binary_operators op, rci_value left, rci_value right) {
	rci_value result = BOOL_VAL(false);
	switch(op) {
		case _OR_: {
			result.as._boolean = left.as._boolean || right.as._boolean;
		} break;
		default:code_gen_error("op for comparison not implemented");					
	}
	return result;
}

// Maybe turn this into a macro?
rci_value binary_operation(rci_binary_operators op, rci_value left, rci_value  right) {	
	rci_value result;
	switch(op) {
		case _ADD_: {
			left.as._number += right.as._number;					
			} break;
		case _SUB_: {
			left.as._number -= right.as._number;
		}break;
		case _MUL_: {
			left.as._number = left.as._number * right.as._number;
		}break;
		case _DIV_: {
			left.as._number = left.as._number / right.as._number;
		}break;
		case _MOD_: {
			left.as._number = (long) left.as._number % (long) right.as._number;
		}break;
		case _POW_ : {
			left = rcilib_power(left, right);
		} break;
		case _LT_: 
		case _GT_:
		case _LTE_:
		case _GTE_:
		case _NE_:
		case _EQ_: {
			left = comparison_binary_operation(op,left,right);					
		} break;
		case _AND_:
		case _OR_:{
			left = logical_binary_operation(op, left, right);
		}break;
		default: code_gen_error("op not implemented");
	}
	return left;	
}

rci_value unary_operation(rci_unary_operators op, rci_value value) {
	switch(op) {
		case _NEGATIVE_: {
			value.as._number *= -1;
			return value;
		}break;
		case _NOT_ : {
			if (IS_BOOL(value)) {
				value.as._boolean = false;
			}else{
				value.as._boolean = true;
			}
			return value;
		}break;
		default: {
			code_gen_error("Unary operator not supported.");
		}
	}
}



#endif