#include<stdlib.h>
#include<stdio.h>
#include<stdint.h>
#include<inttypes.h>

uint64_t binaryToDecimal(uint64_t n){
	uint64_t num = n;
	uint64_t dec_value = 0;
	uint64_t base = 1;
	uint64_t temp = num;
	while (temp) {
		uint64_t last_digit = temp % 10;
		temp = temp / 10;
		dec_value += last_digit * base;
		base = base * 2;
	}
	return dec_value;
}
int expo(int n, int x){
	int res = x;
	for (int i = 1; i < n; i ++){
		res = res * x;
}
	return res;
	}
 int main(void){
	uint64_t reg_o_a = 0;
	uint64_t reg_o_n;
	uint64_t reg__l_2_a = 0;
	uint64_t reg__l_2_n;
	for (uint64_t i = 0; i < 3; i = i + 1){
		printf("o=%ld", o );
		reg_o_a = reg_o_n;
		reg__l_2_a = reg__l_2_n;
} 
 return 0;
 }