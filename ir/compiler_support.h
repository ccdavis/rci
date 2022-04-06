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
	
typedef enum { _ADD_, _SUB_, _MUL_, _DIV_ }
	rci_binary_operation;

typedef struct {	
	char * data;
	long len;	 // number of bytes
	long chars; // numbers of chars
	char_encoding encoding;
} rci_str;



typedef union {
	rci_number _number;
	rci_bool _boolean;
	rci_str _string;			
} rci_data;

// For run-time type information
typedef struct  {
	rci_data data;	
	rci_type type;	
} rci_value;

void debug_value_to_stdout(rci_value *value) {
	switch (value->type) {
		case _number_ : {
			printf("Number: %f",value->data._number);
		}break;
		case _boolean_: {
			printf("Boolean: %d",value->data._boolean);
		}break;
		case _string_ : {
			char * encoding = "8-bit";
			if (value->data._string.encoding == utf_8_encoded) {
				encoding = "UTF-8";
			}
			
			printf("String: '%s', bytes: %ld, encoding: %s",
				value->data._string.data, 
				value->data._string.len,
				encoding);
		}break;
		default: {
			printf("Type %ld not handled \n", value->type);
		}
	}
}

typedef struct {
	rci_data * elements;	
	long len;
	rci_type type;	
} rci_array;


rci_str string_concat(rci_str left, rci_str right) {
	return left;
}

void code_gen_error(const char * msg) {
	printf("%s",msg);
	exit(1);
}
// Maybe turn this into a macro?
rci_data binary_operation(rci_binary_operation op, rci_data left, rci_data right) {	
	switch(op) {
		case _ADD_: {
			left._number += right._number;					
			} break;
		case _SUB_: {
			left._number -= right._number;
		}break;
		case _MUL_: {
			left._number = left._number * right._number;
		}break;
		case _DIV_: {
			left._number = left._number / right._number;
		}break;
		default: code_gen_error("op not implemented");
	}
	return left;	
}


#endif