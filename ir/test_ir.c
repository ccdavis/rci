#include "value.h"


void check(unsigned char result, const char * test_name) {	
	if (result == 0) {
		printf(" not OK");
	} else {
		printf(" OK");
	}	
	printf(test_name);
	printf("\n");	
}

void test_macros() {	
	int x = AS_NUMBER(5);
	int y = AS_NUMBER(1.25);
	
	check(1.25 == y.as._number,"NUMBER_VAL(1.25)");
	check(5 == x.as._number, "NUMBER_VAL(5)");	
}

int main() {
	test_macros();
	
	return 0;
}