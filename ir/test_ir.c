#include "tgc.h"
static tgc_t gc;
//#include "value.h"
//#include "memory.h"
#include <stdarg.h>
#include "compiler_support.h"


static int  failures = 0;
static int total = 0;

void check(unsigned char result, const char * test_name) {	
	total += 1;
	if (result == 0) {
		printf("not OK:  ");
		failures+=1;
	} else {
		printf("OK:  ");
	}	
	printf("%s",test_name);
	printf("\n");	
}



void test_enum_macros() {	
	rci_value  x_type = ENUM_VAL(5);
	rci_value  y_type = ENUM_VAL(5);
	//check(5 == y_type.as._number,"ENUM_VAL(5)");
	check(false == IS_NUMBER(x_type),"IS_NUMBER false on enums");
	check(true == IS_ENUM(y_type),"IS_ENUM true");
	check(y_type.as._enumeration == x_type.as._enumeration, "X and Y enum types are equal");
	check(5 == (rci_enumeration) y_type.as._enumeration,"The enum value should be 5");
}

void test_number_macros() {		
	 rci_value  x = NUMBER_VAL(5);
	rci_value  y = NUMBER_VAL(1.25);
	
	check(1.25 == y.as._number,"NUMBER_VAL(1.25)");
	check(5 == x.as._number, "NUMBER_VAL(5)");		
	check(5 == AS_NUMBER(x), "AS_NUMBER  rci_value 5");
	check(1.25 == AS_NUMBER(y), "AS_NUMBER rci_value 1.25");	
	check(true == IS_NUMBER(x), "IS_NUMBER");	
}

void test_boolean_macros() {
	rci_value b = BOOL_VAL(true);
	rci_value b_f = BOOL_VAL(false);
	rci_value b_0 = BOOL_VAL(0);
	
	check(false == AS_BOOL(b_f), "BOOL_VAL(false)");
	check(false == AS_BOOL(b_0),"BOOL_VAL(0)");
	check(true == AS_BOOL(b),"BOOL_VAL(true)");
	check(true == IS_BOOL(b),"IS_BOOL");
}

// minimal memory allocation check TODO check memory tracking, garbage collection
void test_allocate() {	
	const char * test_value = "abcdefghijkl mnop";
	StringObject * string_object = ALLOCATE_OBJECT(StringObject, _string_);	
	string_object->string_data.data = ALLOCATE(char, 25);
	string_object->string_data.len = 25;
	string_object->string_data.chars = 25;
	strncpy(string_object->string_data.data, test_value, 25);		
	check( 0 == strcmp(string_object->string_data.data, "abcdefghijkl mnop"), "String content should be accessible");
	FREE(char, string_object->string_data.data);
	FREE(StringObject, string_object);
}

void test_to_string() {
	rci_value number = NUMBER_VAL(885);
	rci_value b = BOOL_VAL(true);	
	rci_value s =  string_new("abc", byte_encoded);
	
	rci_value number_as_string = to_string(number);
	char * printed_number = ((StringObject*)number_as_string.as._object)->string_data.data;	
	check( 0 == strcmp(printed_number, "885.000000"), "to_string(number)");
	
	rci_value b_as_string = to_string(b);
	char * printed_boolean  = ((StringObject*) b_as_string.as._object)->string_data.data;
	check( 0 == strcmp(printed_boolean, "true"), "to_string(bool)");
	
	rci_value  string_as_string = to_string(s);
	char * printed_string = ((StringObject*) string_as_string.as._object)->string_data.data;
	check(0 == strcmp(printed_string, "abc"), "to_string( string)");		
}

void test_string_equal() {
	rci_value t1 = string_new("123", byte_encoded);
	rci_value t2 = string_new("123456",byte_encoded);
	rci_value result = string_equal(t1,t2);
	check(false == AS_BOOL(result), "Unequal strings");
	
	rci_value t3 = string_new("1134",byte_encoded);
	rci_value t4 = string_new("1134", byte_encoded);
	
	check(true == AS_BOOL(string_equal(t3,t4)), "strings equal");	
}

void test_record_new() {
	rci_value t1 = string_new("123", byte_encoded);
	rci_value b = BOOL_VAL(true);
	rci_value  y = NUMBER_VAL(1.25);
	rci_value data[3] = {t1, b, y}; 
	
	rci_value rec = record_new(3, t1, b, y);
	RecordObject * r = (RecordObject*) rec.as._object;
	rci_record record_data = r->record_data;
	check(3 == record_data.field_count,"Three fields were set");
	rci_value v1 = record_data.fields[1];
	check(true == AS_BOOL(v1),"'b' set to true");
	rci_value v2 = record_data.fields[2];
	check(1.25 == AS_NUMBER(v2),"should be a number");
	
	rci_value t2 = string_new("123", byte_encoded);
	rci_value v0 =record_data.fields[0];
	check(true == AS_BOOL(string_equal(t2,v0)), "String value set");	
	
	for (int i=0;i<3;i++) {
		rci_value v = record_data.fields[i];
		//debug_value_to_stdout(v);
	}	
}

void test_record_access() {
	rci_value t1 = string_new("123", byte_encoded);
	rci_value b = BOOL_VAL(true);
	rci_value  y = NUMBER_VAL(1.25);
	rci_value data[3] = {t1, b, y}; 
	
	rci_value rec = record_new(3, t1, b, y);
	
	rci_value retrieved_v1 = record_access_member(rec, 1);
	check(true == AS_BOOL(retrieved_v1),"'b' set to true");
	rci_value retrieved_v2 = record_access_member(rec, 2);
	check(1.25 == AS_NUMBER(retrieved_v2),"should be a number");
	rci_value t2 = string_new("123", byte_encoded);
	rci_value retrieved_v0 = record_access_member(rec, 0);
	check(true == AS_BOOL(string_equal(t2,retrieved_v0)), "String value set");	
}


void test_record_set() {
	rci_value t1 = string_new("123", byte_encoded);
	rci_value b = BOOL_VAL(true);
	rci_value  y = NUMBER_VAL(1.25);
		
	rci_value rec = record_new(3, t1, b, y);
	
	rci_value new_t1 = string_new("zyx", byte_encoded);
	rci_value new_b = BOOL_VAL(false);
	rci_value  new_y = NUMBER_VAL(99.55);
	record_set_member(rec, new_t1, 0);
	record_set_member(rec, new_b, 1);
	record_set_member(rec, new_y, 2);
	
	rci_value retrieved_v1 = record_access_member(rec, 1);
	check(false == AS_BOOL(retrieved_v1),"bool record member updated");
	
	rci_value retrieved_v2 = record_access_member(rec, 2);
	check(99.55 == AS_NUMBER(retrieved_v2),"number record member updated");
	
	rci_value retrieved_v0 = record_access_member(rec, 0);
	check(false == AS_BOOL(string_equal(t1,retrieved_v0)), "String value record member set");	
}



int main(int argc, char ** argv) {
	// &argc is the address of the stack, any local variable will do
	tgc_start(&gc, &argc);
		
	test_number_macros();
	test_boolean_macros();
	test_enum_macros();
	test_allocate();
	test_to_string();
	test_string_equal();
	test_record_new();
	test_record_access();
	test_record_set();
	
	printf("\n");
	
	if (failures == 0) {
		printf("All good! All %d tests passed.\n", total);
	} else {
		printf("%d failures out of %d total tests.",failures,total);
	}
	
	tgc_stop(&gc);
	
	return 0;
}