#ifndef STANDARD_LIB
#define STANDARD_LIB

#include "compiler_support.h"
#include <stdio.h>
#include <time.h>
#include <stdlib.h>

rci_value std_clock() {
	clock_t c = clock();
	return NUMBER_VAL(c);	
}

rci_value std_random() {
	return NUMBER_VAL(rand());
}

rci_value seed(rci_value s) {
	rci_value v = NUMBER_VAL(0);
	srand(AS_NUMBER(s));
	return v;
}



#endif
