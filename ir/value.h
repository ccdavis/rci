#ifndef RCI_VALUE
#define RCI_VALUE

#include "types.h"

typedef union {	
	rci_bool _boolean;	
	rci_number _number;
	rci_float _float;
	rci_integer _integer;
	rci_enumeration _enumeration;
	rci_object * _object;
} rci_data;

// For run-time type information
typedef struct rci_value{
	rci_type type;	
	rci_data as;	
} rci_value;


/* ************************************************************************************************


Definitions for complex types

*/
typedef struct rci_record {
	rci_value * fields;
	int field_count;
	//const char * type_name;
} rci_record;

typedef struct RecordObject {
	rci_object obj;
	rci_record record_data;
}RecordObject;

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


#define ENUM_VAL(value) ((rci_value) {.type = _enumeration_, .as = (rci_data){._enumeration = value}})

#define BOOL_VAL(value) ((rci_value) {_boolean_, (rci_data){._boolean = value}})
#define NUMBER_VAL(value) ((rci_value) {.type = _number_, .as = (rci_data){._number = value}})
#define INTEGER_VAL(value) ((rci_value) {.type = _integer_, .as = (rci_data){._integer = value}})
#define FLOAT_VAL(value) ((rci_value) {.type = _float_, .as = (rci_data){._float = value}})


#define AS_BOOL(value) ((value).as._boolean)
#define AS_NUMBER(value) ((value).as._number)
#define AS_INTEGER(value) ((value).as._integer)
#define AS_FLOAT(value) ((value).as._float)
#define AS_ENUM(value) ((value).as._enumeration)
#define AS_OBJECT(value) ((value).as._object)

#define IS_BOOL(value) ((value).type == _boolean_)
#define IS_NUMBER(value) ((value).type == _number_)
#define IS_INTEGER(value) ((value).type == _integer_)
#define IS_FLOAT(value) ((value).type == _float_)
#define IS_ENUM(value) ((value).type == _enumeration_)
#define IS_OBJECT(value) ((value).type == _object_ )

// String, Array are 'Object' types
#define IS_STRING(value) ((value).type == _string_)
#define IS_ARRAY(value) ((value).type == _array_ )
#define IS_RECORD(value) ((value).type == _object_ && (value).as._object->type == object_record )

#endif
