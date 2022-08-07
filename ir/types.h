#ifndef RCI_TYPES
#define RCI_TYPES

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

// Alias primitive data types
typedef unsigned char rci_bool;
#define true 1
#define false 0
typedef long rci_int;
typedef double rci_number;
typedef long rci_integer;
typedef double rci_float;
typedef long rci_enumeration;

typedef enum { utf_8_encoded, byte_encoded} 
	char_encoding;
	

// All the types that can have literals
typedef enum { _integer_,_float_, _enumeration_, _number_, _string_ , _boolean_, _array_, _object_ } 
	rci_type;	


typedef enum {
	object_array,
	object_string,
	object_map,
	object_set,
	object_record
} rci_object_type;

typedef struct rci_object  {
	rci_object_type type;
	unsigned char is_marked;
	void * next;	
	// memory management info goes here
} rci_object; 


	
#endif