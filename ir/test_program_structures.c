#include "compiler_support.h"

rci_value v;

void init_globals() {
	v = (rci_value) NUMBER_VAL(25.0);
}
int main() {
	init_globals();
	debug_value_to_stdout(v);
	return 0;
	
	
}