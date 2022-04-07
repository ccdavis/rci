#include "compiler_support.h"




rci_value init_xyz() {
	rci_value x ={.data= (rci_data) {._number = (double)11.3 }, .type = _number_};  	
	rci_value y = {.data= (rci_data) {._number = (double)25.3 }, .type = _number_};  	
	return x;	
}


rci_str cat_test(rci_str *left, rci_str *right) {				
	long new_len = left->len + right->len;	
	char * new_data = malloc(new_len + 1);	
	memcpy(new_data, left->data, left->len);
	memcpy(new_data + left->len, right->data, right->len);
	new_data[new_len] = '\0';
	rci_str new_string= {
		.data = new_data, 
		.len = new_len, 
		.chars = left->chars + right->chars,
		.refs = 1 ,
		.encoding = left->encoding 		
	};	
	return new_string;
}	

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
	str_result._string = (rci_str) {.data = "abc",.len=3,.chars = 3,  .refs = 1,.encoding = byte_encoded}; 
	
	rci_data str_other_result;	
	str_other_result._string = (rci_str) {.data = "wxyz",.len=4, .chars = 4, .refs=1, .encoding = byte_encoded }; 
		
	rci_str this_string = str_result._string;
	rci_str other_string =  str_other_result._string;
	
	rci_str new_string = cat_test(&this_string, &other_string);
	printf("new string: ");
	debug_str_to_stdout(new_string);
	
	
	printf("print string values\n");	
	rci_value string_value = {.data = str_result, .type = _string_};
	debug_value_to_stdout(&string_value);
	printf("\n");
	rci_value other_string_value = {.data = str_other_result, .type = _string_};
	debug_value_to_stdout(&other_string_value);
	printf("\n");
	
	
	rci_str left = string_value.data._string;
	rci_str right = other_string_value.data._string;
	
	
	
	rci_value new_string_value = {
			.data._string = new_string, 
			.type = _string_};
	
	debug_value_to_stdout(&new_string_value);
	printf("\n");

	return 0;
}
