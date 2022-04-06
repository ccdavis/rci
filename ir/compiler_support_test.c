#include "compiler_support.h"

int main() {
	
	
	rci_data x;
	rci_data y;
	rci_data result;
	
	x._number = 8.0;
	y._number = (double) 25;
		
	result._number =  x._number + y._number;
	
//	binary_operation(_ADD_, x,y);

	rci_value tagged_type = {.data= (rci_data) {._number = (double)32.3 }, .type = _number_};  	
	debug_value_to_stdout(&tagged_type);
	printf("\n");

	rci_data str_result;	
	str_result._string = (rci_str) {.data = "abc",.len=3,  .encoding = byte_encoded }; 
	
	rci_str this_string = str_result._string;
	printf("\n%ld, '%s' \n", this_string.len, this_string.data);
		
	rci_value string_value = {.data = str_result, .type = _string_};

	debug_value_to_stdout(&string_value);
	printf("\n");

	
	
	return 0;
}
