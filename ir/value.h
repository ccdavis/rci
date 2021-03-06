#ifndef RCI_VALUE
#define RCI_VALUE

#include "types.h"

typedef union {	
	rci_bool _boolean;	
	rci_number _number;
	rci_object * _object;
} rci_data;

// For run-time type information
typedef struct rci_value{
	rci_type type;	
	rci_data as;	
}  rci_value;


/* ************************************************************************************************


Definitions for complex types

*/

typedef struct rci_str {		
	long len;	 // number of bytes
	long chars; // numbers of chars
	long refs; // basically is it a literal or a heap value
	char_encoding encoding;
	char * data;
} rci_str;

typedef struct StringObject {
	rci_object obj;
	rci_str string_data;
} StringObject;

typedef struct rci_array {
	long len;
	rci_type type;	
	rci_data * elements;	
} rci_array;

typedef struct ArrayObject {
	rci_object obj;
	rci_array array_data;
} ArrayObject;


#define BOOL_VAL(value) ((rci_value) {_boolean_, (rci_data){._boolean = value}})
#define NUMBER_VAL(value) ((rci_value) {.type = _number_, .as = (rci_data){._number = value}})

#define AS_BOOL(value) ((value).as._boolean)
#define AS_NUMBER(value) ((value).as._number)
#define AS_OBJECT(value) ((value).as._object)



#define IS_BOOL(value) ((value).type == _boolean_)
#define IS_NUMBER(value) ((value).type == _number_)
#define IS_OBJECT(value) ((value).type == _object_ )

// String, Array are 'Object' types
#define IS_STRING(value) ((value).type == _string_)
#define IS_ARRAY(value) ((value).type == _array_ )



#endif
