#ifdef underscore
#define power _power
#endif

int power(int base, int exp) {
	int i = 0, res = 1;
	while (i < exp) {
		res = res * base; 
		i++;
	}	
	return res;
}
