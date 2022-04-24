#ifndef COMPILER_SUPPORT
#define COMPILER_SUPPORT

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

typedef unsigned char rci_bool;
#define true 1
#define false 0

typedef long rci_int;
typedef double rci_number;

typedef enum { utf_8_encoded, byte_encoded} 
	char_encoding;
	

typedef enum { _number_, _string_ , _boolean_, _array_ } 
	rci_type;	
	

typedef enum { _NEGATIVE_, _NOT_} rci_unary_operators;
// These all result in number
typedef enum { _ADD_, _SUB_, _MUL_, _DIV_, _MOD_, _POW_, _SHL_, _SHR_,  
			_LT_, _GT_, _LTE_, _GTE_, _EQ_, _NE_, _AND_, _OR_ }  
	rci_binary_operators;

typedef struct {	
	char * data;
	long len;	 // number of bytes
	long chars; // numbers of chars
	long refs;
	char_encoding encoding;
} rci_str;

rci_str rci_str_ascii_literal(char  str[]) {
	rci_str result = {
		.data = str,
		.len = strlen(str),
		.chars = strlen(str),
		.refs = 0,
		.encoding = byte_encoded
	};
	return result;
}

rci_str copy_rci_str(rci_str  value) {
	rci_str result = value;
	char * memory = malloc(value.len + 1);
	strcpy(memory, value.data);
	result.refs = 1;
	return result;	
}

rci_str new_rci_str(char  data[], char_encoding enc) {
	int byte_length = strlen(data);	
	char * memory = malloc(byte_length + 1);
	strcpy(memory, data);
	return (rci_str) {
		.data = memory,
		.len = byte_length,
		.chars = byte_length,
		.refs = 1,
		.encoding = enc				
	};
}

rci_str cat_rci_str(rci_str left, rci_str right) {				
	long new_len = left.len + right.len;	
	char * new_data = malloc(new_len + 1);	
	memcpy(new_data, left.data, left.len);
	memcpy(new_data + left.len, right.data, right.len);
	new_data[new_len] = '\0';
	rci_str new_string= {
		.data = new_data, 
		.len = new_len, 
		.chars = left.chars + right.chars,
		.refs = 1 ,
		.encoding = left.encoding 		
	};	
	return new_string;
}	

rci_str * pass_rci_str(rci_str  *value) {
	value->refs += 1;
	return value;
}

void exit_scope_rci_str(rci_str *value) {
	// If refs == 0 it's a literal and 
	// no memory needs to be managed.
	if (value->refs == 1) {
		free(value->data);
		value->refs = -1;
	} else  if (value->refs > 1) {
		value->refs -= 1;
	}
}

rci_str * return_rci_str(rci_str *value) {
	// If refs == 0 or 1 this is a 'move', the
	// change in scope decrements the count normally
	// but the memory won't be needed in the scope
	// we're exiting.
	if (value->refs == 0 || value->refs == 1) return value;
	if (value->refs > 1) {
		value->refs -= 1;
		return value;
	}
	if (value->refs == -1) {
		printf("Runtime error: String memory deallocated.");
		exit(1);
	}
	
}

void debug_str_to_stdout(rci_str s) {
	printf("string data: '%s', len: %d, chars: %d",s.data,s.len,s.chars);
}


typedef union {
	rci_number _number;
	rci_bool _boolean;
	rci_str _string;			
} rci_data;

// For run-time type information
typedef struct {
	rci_data data;	
	rci_type type;	
}  rci_value;

void debug_value_to_stdout(rci_value value) {
	switch (value.type) {
		case _number_ : {
			printf("Number: %f",value.data._number);
		}break;
		case _boolean_: {
			printf("Boolean: %d",value.data._boolean);
		}break;
		case _string_ : {
			char * encoding = "8-bit";
			if (value.data._string.encoding == utf_8_encoded) {
				encoding = "UTF-8";
			}
			
			printf("String: '%s', bytes: %ld, encoding: %s",
				value.data._string.data, 
				value.data._string.len,
				encoding);
		}break;
		default: {
			printf("Type %ld not handled \n", value.type);
		}
	}
}

typedef struct {
	rci_data * elements;	
	long len;
	rci_type type;	
} rci_array;


void code_gen_error(const char * msg) {
	printf("%s",msg);
	exit(1);
}

rci_value power(rci_value x,rci_value p) {
	rci_value result = {
		.data = (rci_data) {._number = (double)1 }, 
		.type = (rci_type) _number_};
					
	while (p.data._number > 1) {
		p.data._number =p.data._number - 1;
		result.data._number = result.data._number * x.data._number;					
	}
	return result;
}

rci_value comparison_binary_operation(rci_binary_operators op, rci_value left, rci_value  right) {
	rci_value result = {.data._boolean=false,.type=_boolean_};
	switch(op) {
		case _LT_ :{
			result.data._boolean = left.data._number < right.data._number;
		} break;
		default:code_gen_error("op for comparison not implemented");					
	}
	return result;	
}

rci_value logical_binary_operation(rci_binary_operators op, rci_value left, rci_value right) {
	rci_value result = {.data._boolean=false,.type=_boolean_};
	switch(op) {
		case _OR_: {
			result.data._boolean = left.data._boolean || right.data._boolean;
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
			left.data._number += right.data._number;					
			} break;
		case _SUB_: {
			left.data._number -= right.data._number;
		}break;
		case _MUL_: {
			left.data._number = left.data._number * right.data._number;
		}break;
		case _DIV_: {
			left.data._number = left.data._number / right.data._number;
		}break;
		case _MOD_: {
			left.data._number = (long) left.data._number % (long) right.data._number;
		}break;
		case _POW_ : {
			left = power(left, right);
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
			value.data._number *= -1;
			return value;
		}break;
		case _NOT_ : {
			if (value.data._boolean) {
				value.data._boolean = false;
			}else{
				value.data._boolean = true;
			}
			return value;
		}break;
		default: {
			code_gen_error("Unary operator not supported.");
		}
	}
}

unsigned char  rci_value_to_c_boolean(rci_value v) {	
	return v.data._boolean;
}


rci_value to_string(rci_value value) {
	rci_value result;
	result.type = _string_;
	switch(value.type) {
		case _number_: {
			char buffer[50];
			sprintf(buffer, "%f", value.data._number);
			result.data._string = (rci_str) new_rci_str(buffer, byte_encoded);
		}break;
		case _string_ : {
			result.data._string =   copy_rci_str(value.data._string);
		}break;
		case _boolean_ : {
			if (value.data._boolean == true) {
				result.data._string = (rci_str) rci_str_ascii_literal("true");
			} else {
				result.data._string = (rci_str) rci_str_ascii_literal("false");
			}
		}break;
		default: {
			printf("Compilation error, to_string() not supported for type.");
			exit(1);
		}
	}
	return result;
}



#endif