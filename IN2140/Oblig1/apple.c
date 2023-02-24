#include <stdio.h>
#include "the_apple.h"

int locateworm(char *apple) {
	for(int i = 0; apple[i] != '\0'; i++) {
		if(apple[i] == 'w') return i;
	}

	return 0;
}

int removeworm(char *apple) {
	int start = 0;
	int end = 0;
	
	for(int i = 0; apple[i] != '\0'; i++) {
		if(apple[i] == 'w' || apple[i] == 'o' || apple[i] =='r' || apple[i] == 'm') {
			if(start == 0) start = i;
			end = i + 1;
			apple[i] = ' ';
		}
	}

	return end - start; // returnerer 0 hvis ingen mark finnes
}

int main(void) {
	printf("Startindeksen til ormen: %d\n", locateworm(apple));
	printf("Orm fjernet, lengde: %d\n", removeworm(apple));
}
