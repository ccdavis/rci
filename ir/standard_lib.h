#ifndef STANDARD_LIB
#define STANDARD_LIB

#include "compiler_support.h"
#include <stdio.h>
#include <time.h>
#include <stdlib.h>
#include<unistd.h>

rci_value std_clock() {
	double  c = (double)clock();	
	return NUMBER_VAL(c);	
}

rci_value std_clock_per_second() {
	return  NUMBER_VAL(CLOCKS_PER_SEC);
}


rci_value std_random() {
	return NUMBER_VAL(rand());
}

rci_value std_random_seed(rci_value s) {
	rci_value v = NUMBER_VAL(0);
	srand(AS_NUMBER(s));
	return v;
}

rci_value std_sleep(rci_value secs) {
	return NUMBER_VAL(sleep(AS_NUMBER(secs)));
}




#endif
