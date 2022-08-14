#ifndef COMPILER_SUPPORT
#define COMPILER_SUPPORT
#include "types.h"
#include "memory.h"
#include "value.h"




void code_gen_error(const char * msg) {
	printf("Code generation error: %s",msg);
	exit(1);
}

void runtime_error(const char * msg) {
	printf("Runtime error: %s",msg);
	exit(1);
}



rci_value record_new(rci_value data[], int field_count) {	
	RecordObject * new_record = ALLOCATE_OBJECT(RecordObject, object_record);	
	rci_value * fields = ALLOCATE(rci_value, field_count);
	memcpy(fields,data, sizeof(rci_value) * field_count);	
	rci_record new_record_data = (rci_record) {.fields = fields,.field_count =field_count}; 
	new_record->record_data = new_record_data;
	return (rci_value) { .type = _object_, .as._object = (rci_object*) new_record};	
}


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

rci_value string_literal(char str[]) {
	StringObject * string_object = (StringObject*) ALLOCATE_OBJECT(StringObject, object_string);
	string_object->string_data = rci_str_ascii_literal(str);
	return (rci_value) {.type=_string_, .as._object =  (rci_object*) string_object};
}

rci_str copy_rci_str(rci_str  original) {
	rci_str result = original;
	char * memory = ALLOCATE(char, original.len + 1);
	strcpy(memory, original.data);
	result.refs = 1;
	result.data = memory;
	return result;	
}

rci_value string_copy(rci_value original) {
	StringObject * orig = (StringObject*) original.as._object;
	rci_str copied_string = copy_rci_str(orig->string_data);
	StringObject * new_string  = ALLOCATE_OBJECT(StringObject, object_string);
	new_string->string_data =  copied_string;
	return (rci_value) { .type = _string_, .as._object = (rci_object*) new_string};	
}

rci_str new_rci_str(char  data[], char_encoding enc) {
	int byte_length = strlen(data);	
	char * memory = ALLOCATE(char, byte_length+1);
	strcpy(memory, data);
	return (rci_str) {		
		.len = byte_length,
		.chars = byte_length,
		.refs = 1,
		.encoding = enc,				
		.data = memory
	};
}

rci_value string_new(char data[], char_encoding enc) {	
	StringObject * new_string  = ALLOCATE_OBJECT(StringObject, object_string);
	new_string->string_data = new_rci_str(data, enc);
	return (rci_value) { .type = _string_, .as._object = (rci_object*) new_string};	
}

rci_str cat_rci_str(rci_str left, rci_str right) {				
	long new_len = left.len + right.len;	
	char * new_data = ALLOCATE(char, new_len + 1);	
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

rci_value string_concat(rci_value left, rci_value right) {
	StringObject*  left_string = (StringObject*) left.as._object;
	StringObject * right_string = (StringObject*) right.as._object;	
	StringObject * new_string  = ALLOCATE_OBJECT(StringObject, object_string);
	new_string->string_data = cat_rci_str(left_string->string_data, right_string->string_data);
	return (rci_value) {.type = _string_, .as._object = (rci_object*) new_string};	
}

rci_value string_equal(rci_value left_str, rci_value right_str) {
	StringObject * l = (StringObject*) left_str.as._object;
	StringObject * r = (StringObject*) right_str.as._object;
	if (l->string_data.len != r->string_data.len) 
		return BOOL_VAL(false);
	
	return BOOL_VAL( 0 == strncmp(l->string_data.data, r->string_data.data, l->string_data.len));
}

rci_value new_array(rci_type element_type,rci_value * initial_data, long initial_len) {
	ArrayObject * new_array = ALLOCATE_OBJECT(ArrayObject, object_array);	
	new_array->array_data.len = initial_len;
	new_array->array_data.type = element_type;
	new_array->array_data.elements = ALLOCATE(rci_data, initial_len);
	for (int e=0; e<initial_len; e++) {
		new_array->array_data.elements[e] = initial_data[e].as;		
	}
	return (rci_value) {.type=_array_, .as._object = (rci_object*) new_array};
}

// Add a little run-time checking
rci_value array_lookup(rci_array this_array, long index) {
	if (index>= this_array.len) {
		runtime_error("Index out of bounds.");
	}
	rci_data element = this_array.elements[index];
	return (rci_value) {.type = this_array.type, .as = element};	
}

void replace_element(rci_array this_array, long index, rci_value new_element) {	
	if (new_element.type != this_array.type) {
		code_gen_error("Wrong element type for array element type.");
	}
	
	if (index >= this_array.len) {
		runtime_error("Index out of bounds.");
	}
	
	this_array.elements[index] = (rci_data) new_element.as;	
}


/* ***************************************************************************

	Debugging support
*/

void debug_str_to_stdout(rci_str s) {
	printf("string data: '%s', len: %ld, chars: %ld",s.data,s.len,s.chars);
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
			StringObject * this_string = (StringObject*) value.as._object;
			debug_str_to_stdout(this_string->string_data);
		}break;
		default: {
			printf("Type %d not handled \n", value.type);
		}
	}
}


/* *********************************************************
 Compiler support and stdlib

*/

rci_value string_assign(rci_value lhs, rci_value rhs) {
	StringObject * old_string = (StringObject*) lhs.as._object;
	StringObject * new_string = (StringObject*) rhs.as._object;
	if (old_string->string_data.refs > 0) {				
		FREE(char, old_string->string_data.data);				
		FREE(StringObject, old_string);
	}	
	if (new_string->string_data.refs>0) new_string->string_data.refs += 1;
	lhs.as._object =  new_string;	
	return lhs;
}

/* This isn't particularly fast; it's a demonstration of using the type cast macros to show
how generated code could look. */
rci_value rcilib_power(rci_value x,rci_value p) {
	rci_value result = NUMBER_VAL(1);						
	while (AS_NUMBER(p) > 1) {
		AS_NUMBER(p) = AS_NUMBER(p) - 1;
		AS_NUMBER(p) = AS_NUMBER(result) * AS_NUMBER(x);
	}
	return result;
}

#include "operations.h"

/*  ****************************************************************

 C object code generation support
 
*/
unsigned char  rci_value_to_c_boolean(rci_value v) {	
	return (unsigned char) AS_BOOL(v);
}

char * rci_value_to_c_str(rci_value value) {
	StringObject *str = (StringObject*) AS_OBJECT(value);
	return (char*) str->string_data.data;
}

double rci_value_to_c_double(rci_value value) {
	return (double) value.as._number;
}

rci_value c_boolean_to_rci_value(unsigned char b) {
	return (rci_value) BOOL_VAL(b);
}

rci_value to_string(rci_value value) {
	rci_value result;
	result.type = _string_;
	switch(value.type) {
		case _number_: {
			char buffer[50];
			sprintf(buffer, "%f", value.as._number);
			result = string_new(buffer, byte_encoded);
		}break;
		case _string_ : {
			result = value;
		}break;
		case _boolean_ : {
			if (value.as._boolean == true) {
				result = string_literal("true");
			} else {
				result = string_literal("false");
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