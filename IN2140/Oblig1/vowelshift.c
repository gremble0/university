#include <stdio.h> // printf(), gets(), getchar()
#include <string.h> // strcpy(), strlen()
#include <stdlib.h> // malloc()
#include <ctype.h> // toupper()

char *vowelshift(char str[], char c) {
	char *res = malloc(strlen(str));
	strcpy(res, str);
	for(size_t i = 0; i < strlen(str); i++) {
		if(str[i] == 'a' || str[i] == 'e' || str[i] == 'i' || str[i] == 'o' || str[i] == 'u') {
			res[i] = c;
		}
		if(str[i] == 'A' || str[i] == 'E' || str[i] == 'I' || str[i] == 'O' || str[i] == 'U') {
			res[i] = toupper(c);
		}
	}
	return res;
}

int main(void) {
	char str[100];
	char c;

	printf("%s\n", "Skriv en setning");
	gets(str);
	
	printf("%s\n", "Skriv en vokal");
	c = getchar();	

	if(c != 'a' && c != 'e' && c != 'i' && c != 'o' && c != 'u') {
		printf("%s\n", "Du skrev ikke en vokal");
		return 0;
	}

	printf("%s\n", vowelshift(str, c));
	return 0;
}
