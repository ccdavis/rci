#include "compiler_support.h"

int main() {
	
	rci_data x = (rci_data) 25.0;
	rci_data y = (rci_data) (rci_number)8;
	
	rci_data result;
	result._number =  x._number + y._number;
//	binary_operation(_ADD_, x,y);
	
	printf("%f", result._number);
		
	rci_data str_result;	
	str_result._string = (rci_str) {.len=3, .data = "abc", .encoding = byte_encoded }; 
	
	rci_str  this_string = str_result._string;
	printf("\n%ld, '%s'", this_string.len, this_string.data);
	
	return 0;
}
