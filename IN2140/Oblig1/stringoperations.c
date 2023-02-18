#include <stdio.h>
#include <string.h>
#include <ctype.h> // toupper()
#include <stdlib.h>

int stringsum(char *s) {
	int sum = 0;
	const int s_len = strlen(s);
	char s_lower[s_len];
	// gjoer ordet til bare store bokstaver for aa gjoere neste del enklere
	for(int i = 0; i < s_len; i++) {
		s_lower[i] = tolower(s[i]);
	}

	for(int i = 0; i < s_len; i++) {
		if(s_lower[i] == 32) continue;
		if(s_lower[i] < 97 || s_lower[i] > 122) return -1;
		sum += s_lower[i] - 96;
	}
	return sum;
}

// neste 2 oppgaver har veldig lik kode saa lager hjelpefunksjon
int *get_substring_interval(char *s, char c) {
	int *interval = malloc(2);
	int first = 0; 
	int last = 0;

	for(int i = 0; i < strlen(s); i++) {
		if(s[i] == c) {
			if(first == 0) {
				first = i;
			} else {
				last = i;
			}
		}
	}

	interval[0] = first;
	interval[1] = last;
	// kan ikke frigjoere siden vi trenger variabelen for returverdi
	return interval;
}

int distance_between(char *s, char c) {
	int *interval = get_substring_interval(s, c);
	if(interval[0] == 0) return -1;
	if(interval[1] == 0) return 0;
	return interval[1] - interval[0];
}
	
char *string_between(char *s, char c) {
	int *interval = get_substring_interval(s, c);
	if(interval[0] == 0) return NULL;
	if(interval[1] == 0) return "";
	
	char *substring = malloc(interval[1] - interval[0]);
	for(int i = 0; i < interval[1] - interval[0] - 1; i++) {
		substring[i] = s[interval[0] + i + 1];
	}
	free(interval); // eneste stedet jeg ser jeg kan bruke free
	return substring; 
}

int stringsum2(char *s, int *res) {
	*res = stringsum(s);
	if(*res > 0) return 0;
	return -1;
}
