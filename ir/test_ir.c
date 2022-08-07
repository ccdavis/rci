//#include "value.h"
//#include "memory.h"
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

rci_value  x_type = ENUM_VAL(5);
abc
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
	check( 0 == strcmp(string_object->string_data.data, "abcdefghijkl mnop"), "String content accessible");
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

int main() {
	test_number_macros();
	test_boolean_macros();
	test_allocate();
	test_to_string();
	test_string_equal();
	
	printf("\n");
	
	if (failures == 0) {
		printf("All good! All %d tests passed.\n", total);
	} else {
		printf("%d failures out of %d total tests.",failures,total);
	}
	
	return 0;
}