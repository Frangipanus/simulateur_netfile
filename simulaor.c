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
	for (uint64_t i = 0; i < 3; i = i + 1){
		uint64_t a;
		printf("Donnder la valeur de a qui a une longeur 1: ");
		scanf("%ld", &a);
 		a = binaryToDecimal(a);
		uint64_t b;
		printf("Donnder la valeur de b qui a une longeur 1: ");
		scanf("%ld", &b);
 		b = binaryToDecimal(b);
		uint64_t c;
		printf("Donnder la valeur de c qui a une longeur 1: ");
		scanf("%ld", &c);
 		c = binaryToDecimal(c);
uint64_t _l_1 = a ^ b;
uint64_t s = _l_1 ^ c;
uint64_t _l_3 = a && b;
uint64_t _l_4 = a ^ b;
uint64_t _l_5 = _l_4 && c;
uint64_t r = _l_3 || _l_5;
		printf("s=%ld\n", s );
		printf("r=%ld\n", r );
} 
 return 0;
 }