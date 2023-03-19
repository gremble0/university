#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

char *vowelshift(char str[], char c) {
	char *res = malloc(strlen(str) + 1);
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

int main(int argc, char *argv[]) {
	char *str = argv[1];
	char c = argv[2][0];

    printf("%c",c);
	if(c != 'a' && c != 'e' && c != 'i' && c != 'o' && c != 'u') {
		printf("%s\n", "Du skrev ikke en vokal");
		return -1;
	}
    
    char *res = vowelshift(str, c);
	printf("%s\n", res);
    free(res);
	return 0;
}
