#ifndef COMPILER_SUPPORT
#define COMPILER_SUPPORT

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

// Alias primitive data types
typedef unsigned char rci_bool;
#define true 1
#define false 0
typedef long rci_int;
typedef double rci_number;

typedef enum { utf_8_encoded, byte_encoded} 
	char_encoding;
	

// All the types that can have literals
typedef enum { _number_, _string_ , _boolean_, _array_, _object_ } 
	rci_type;	
	

typedef enum { _NEGATIVE_, _NOT_} rci_unary_operators;

typedef enum { _ADD_, _SUB_, _MUL_, _DIV_, _MOD_, _POW_, _SHL_, _SHR_,  
			_LT_, _GT_, _LTE_, _GTE_, _EQ_, _NE_, _AND_, _OR_ }  
	rci_binary_operators;


typedef enum {
	object_array,
	object_string,
	object_map,
	object_set,
	object_record
} rci_object_type;

struct rci_object {
	rci_object_type type;
	// memory management info goes here
};


typedef union {
	rci_number _number;
	rci_bool _boolean;	
	rci_object * _object;
} rci_data;

// For run-time type information
typedef struct {
	rci_type type;	
	rci_data as;	
}  rci_value;

#define BOOL_VAL(value) ((rci_value) {_boolean_, (rci_data){._boolean = value}})
#define NUMBER_VAL(value) ((rci_value) {_number_, (rci_data){._number = value}})


#define AS_BOOL(value) ((value).as._boolean)
#define AS_NUMBER(value) ((value).as._number)
#define AS_OBJECT(value) ((value).as._object)


#define IS_BOOL(value) ((value).type == _boolean_)
#define IS_NUMBER(value) ((value).type == _number_)
#define IS_OBJECT(value) ((value).type == _object_ )

// String, Array are 'Object' types
#define IS_STRING(value) ((value).type == _string_)
#define IS_ARRAY(value) ((value).type == _array_ )

/* ************************************************************************************************


Implementation of complex types

*/

struct rci_str {		
	long len;	 // number of bytes
	long chars; // numbers of chars
	long refs; // basically is it a literal or a heap value
	char_encoding encoding;
	char * data;
}; 

struct StringObject {
	rci_object obj;
	rci_str string_data;
};




rci_str rci_str_ascii_literal(char  str[]) {
	rci_str result = {		
		.len = strlen(str),
		.chars = strlen(str),
		.refs = 0,
		.encoding = byte_encoded,
		.data = str
	};
	return result;
}

rci_str copy_rci_str(rci_str  original) {
	rci_str result = original;
	char * memory = malloc(original.len + 1);
	strcpy(memory, original.data);
	result.refs = 1;
	return result;	
}

rci_str new_rci_str(char  data[], char_encoding enc) {
	int byte_length = strlen(data);	
	char * memory = malloc(byte_length + 1);
	strcpy(memory, data);
	return (rci_str) {		
		.len = byte_length,
		.chars = byte_length,
		.refs = 1,
		.encoding = enc,				
		.data = memory
	};
}

rci_str cat_rci_str(rci_str left, rci_str right) {				
	long new_len = left.len + right.len;	
	char * new_data = malloc(new_len + 1);	
	memcpy(new_data, left.data, left.len);
	memcpy(new_data + left.len, right.data, right.len);
	new_data[new_len] = '\0';
	rci_str new_string= {		
		.len = new_len, 
		.chars = left.chars + right.chars,
		.refs = 1,
		.encoding = left.encoding,
		.data = new_data 
	};	
	return new_string;
}	

struct rci_array {	
	long len;
	rci_type type;	
	rci_data * elements;	
};

struct ArrayObject {
	rci_obj obj;
	rci_array array_data;
};


rci_value new_array(rci_type element_type,rci_value * initial_data, long initial_len) {
	ArrayObject new_array = malloc(sizeof(ArrayObject));
	new_array->obj.type = object_array;
	new_array->array_data.len = initial_len;
	new_array->array_data.type = element_type;
	new_array->array_data.elements = malloc(initial_len * sizeof(rci_data));
	for (int e=0; e<initial_len; e++) {
		new_array->array_data.elements[e] = initial_data[e].as;		
	}
	return (rci_value) {.as._object = new_array,.type=_array_};
}

// Add a little run-time checking
rci_value array_lookup(rci_array this_array, long index) {
	if (index>= this_array.len) {
		runtime_error("Index out of bounds.");
	}
	return (rci_value) {.as = this_array[index], .type = this_array.type};	
}

void replace_element(rci_array this_array, long index, rci_value new_element) {	
	if (new_element.type != this_array.type) {
		code_gen_error("Wrong element type for array element type.");
	}
	
	if (index >= this_array.len) {
		runtime_error("Index out of bounds.");
	}
	
	this_array[index] = (rci_data) new_element.as;	
}



/* ***************************************************************************

	Debuging support
*/

void debug_str_to_stdout(rci_str s) {
	printf("string data: '%s', len: %d, chars: %d",s.data,s.len,s.chars);
}



int is_type(rci_value v, rci_type t) {
	return v.type == t;
}



void debug_value_to_stdout(rci_value value) {
	switch (value.type) {
		case _number_ : {
			printf("Number: %f",value.as._number);
		}break;
		case _boolean_: {
			printf("Boolean: %d",value.as._boolean);
		}break;
		case _string_ : {
			debug_str_to_stdout(value.as._object->string_data);
		}break;
		default: {
			printf("Type %ld not handled \n", value.type);
		}
	}
}


void code_gen_error(const char * msg) {
	printf("Code generation error: %s",msg);
	exit(1);
}

void runtime_error(const char * msg) {
	printf("Runtime error: %s",msg);
	exit(1);
}



/* *********************************************************
 Compiler support and stdlib

*/

rci_value new_string_object(char  data[], char_encoding enc) {
	StringObject * new_string = malloc(sizeof(StringObject));
	new_string->obj.type = object_string;
	new_string.string_data = new_rci_str(data,enc);
	return (rci_value) {.type =_string_,.as._object = new_string_object};
}


rci_value cat_string(rci_value lhs, rci_value rhs) {
	StringObject *new_string = malloc(sizeof(StringObject));
	new_string->obj.type = object_string;
	new_string->string_data = cat_rci_str(lhs.as._object->string_data,rhs.as._object->string_data);
	
	return (rci_value) { .type = _string_,.as._object = new_string};
}

rci_value assign_string(rci_value lhs, rci_value rhs) {
	if (lhs.as._object->refs > 0) {
		free(lhs.as._object->string_data.data);
	}
	lhs.as._object.string_data = rhs.as._object.string_data;	
	return lhs;
}


rci_value power(rci_value x,rci_value p) {
	rci_value result = NUMBER_VAL(1);						
	while (AS_NUMBER(p) > 1) {
		AS_NUMBER(p) = AS_NUMBER(p) - 1;
		AS_NUMBER(p) = AS_NUMBER(result) * AS_NUMBER(x);
	}
	return result;
}

rci_value comparison_binary_operation(rci_binary_operators op, rci_value left, rci_value  right) {
	rci_value result = BOOL_VAL(false);
	switch(op) {
		case _LT_ :{
			AS_BOOL(result) = AS_NUMBER(left) < AS_NUMBER(right);
		} break;
		case _EQ_ :{
			result.data._boolean = left.data._number == right.data._number;
		}break;
		case _GT_ : {
			result.data._boolean = left.data._number > right.data._number;
		}break;
		case _NE_ : {
			result.data._boolean = left.data._number != right.data._number;
		}break;
		case _GTE_ : {
			result.data._boolean = left.data._number >= right.data._number;
		}break;
		case _LTE_ : {
			result.data._boolean = left.data._number <= right.data._number;
		}break;
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

/*  ****************************************************************

 C object code generation support
 
*/
unsigned char  rci_value_to_c_boolean(rci_value v) {	
	return (unsigned char) v.data._boolean;
}

char * rci_value_to_c_str(rci_value value) {
	return (char*) value.data._string.data;
}

double rci_value_to_c_double(rci_value value) {
	return (double) value.data._number;
}

rci_value c_boolean_to_rci_value(unsigned char b) {
	return (rci_value) {.data= (rci_data) {._boolean = b}, .type = _boolean_};  	
}




rci_value to_string(rci_value value) {
	rci_value result;
	result.type = _string_;
	switch(value.type) {
		case _number_: {
			char buffer[50];
			sprintf(buffer, "%f", value.as._number);
			result.as._object = (StringObject) new_rci_str(buffer, byte_encoded);
		}break;
		case _string_ : {
			result.as._string =   copy_rci_str(value.as._string);
		}break;
		case _boolean_ : {
			if (value.as._boolean == true) {
				result.as._string = (rci_str) rci_str_ascii_literal("true");
			} else {
				result.as._string = (rci_str) rci_str_ascii_literal("false");
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