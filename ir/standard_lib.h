#ifndef STANDARD_LIB
#define STANDARD_LIB

#include "compiler_support.h"
#include <stdio.h>
#include <time.h>

rci_value std_clock() {
	clock_t c = clock();
	return NUMBER_VAL(c);
	
}




#endif
