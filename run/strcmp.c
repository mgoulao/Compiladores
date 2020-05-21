#ifdef underscore
#define strcmp _strcmp
#endif

int strcmp(char* l, char* r) {
	int i = 0;
	while (l[i] == r[i] && r[i] != '\0')
		i++;

	if(r[i] < l[i]) return -1;
	else if (r[i] > l[i]) return 1;
	return 0;
}
