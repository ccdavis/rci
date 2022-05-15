#include "value.h"

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
	printf(test_name);
	printf("\n");	
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



int main() {
	test_number_macros();
	test_boolean_macros();
	printf("\n");
	
	if (failures == 0) {
		printf("All good! All %d tests passed.\n", total);
	} else {
		printf("%d failures out of %d total tests.",failures,total);
	}
	
	return 0;
}