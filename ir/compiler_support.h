
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
	long len;	
	char_encoding encoding;
} rci_str;

typedef union {
	rci_number _number;
	rci_bool _boolean;
	rci_str _string;		
	void * _collection;
} rci_data;

// For run-time type information
typedef struct  {
	rci_data data;	
	rci_type type;	
} rci_value;

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


