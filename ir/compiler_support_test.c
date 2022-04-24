#include "compiler_support.h"




rci_value init_xyz() {
	rci_value x = {.data= (rci_data) {._number = (double)11.3 }, .type = _number_};  	
	rci_value y = {.data= (rci_data) {._number = (double)25.3 }, .type = _number_};  	
	return x;	
}


rci_value test_function(int argc,rci_value *arg1, rci_value * arg2){
	rci_value local = {.data= (rci_data) {._number = (double)25.3 }, .type = _number_};  	
	rci_value result = binary_operation(_ADD_, *arg2, local);
	rci_value new_arg2 = binary_operation(_ADD_, result, local);
	(*arg2).data =  new_arg2.data;
	return result;
	
}

rci_value test2(int argc, rci_value * arg1, rci_value * arg2) {
	return binary_operation(_MUL_, (*arg1), (*arg2));
}

rci_value test3(int argc, rci_value * arg1) {
	rci_value local = {.data= (rci_data) {._number = (double)25.3 }, .type = _number_};  	
	
	rci_value result = test2(2, &local, &(*arg1));
	return result;
	
}


int main() {			
	rci_data x;
	rci_data y;
	rci_data result;
	
	x._number = 8.0;
	y._number = (double) 25;
		
	result._number =  x._number + y._number;
	


	rci_value tagged_type = {.data= (rci_data) {._number = (double)32.3 }, .type = _number_};  	
	debug_value_to_stdout(tagged_type);
	printf("\n");
	
	rci_value r3 = binary_operation(
		_ADD_, 
		tagged_type, 
		(rci_value) {.data= (rci_data) {._number = (double)32.3 }, .type = _number_});
	debug_value_to_stdout(r3);
	printf("\n");
	
	rci_value r4 = binary_operation(_ADD_, 
		 binary_operation(_MUL_, tagged_type, tagged_type),
		 tagged_type);
	debug_value_to_stdout(r4);
	printf("\n");
	
	rci_data str_result;	
	str_result._string = (rci_str) {.data = "abc",.len=3,.chars = 3,  .refs = 1,.encoding = byte_encoded}; 
	
	rci_data str_other_result;	
	str_other_result._string = (rci_str) {.data = "wxyz",.len=4, .chars = 4, .refs=1, .encoding = byte_encoded }; 
		
	rci_str this_string = str_result._string;
	rci_str other_string =  str_other_result._string;
	
	rci_str new_string = cat_rci_str(this_string, other_string);
	printf("new string: ");
	debug_str_to_stdout(new_string);
	
	
	printf("print string values\n");	
	rci_value string_value = {.data = str_result, .type = _string_};
	debug_value_to_stdout(string_value);
	printf("\n");
	rci_value other_string_value = {.data = str_other_result, .type = _string_};
	
	debug_value_to_stdout(other_string_value);
	printf("\n");
	
	
	rci_str left = string_value.data._string;
	rci_str right = other_string_value.data._string;
		
	rci_value new_string_value = {
			.data._string = new_string, 
			.type = _string_};
	
	debug_value_to_stdout(new_string_value);
	printf("\n");
	
	rci_value return_value = test_function(2, 
		(rci_value[2]) {
		new_string_value,
		tagged_type}, &tagged_type);
		
	debug_value_to_stdout(return_value);
	printf("\n");
	debug_value_to_stdout(tagged_type);
	printf("\n");
	
	rci_value result5 = test3(1, &tagged_type);
	debug_value_to_stdout(result5);
	printf("\n");
	
	return 0;
}
