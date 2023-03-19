#include <stdio.h>
#include <string.h>
#include <ctype.h>
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
        if(s_lower[i] == ' ') continue;
        if(s_lower[i] < 'a' || s_lower[i] > 'z') return -1;
        sum += s_lower[i] - 96;
    }
    return sum;
}

int distance_between(char *s, char c) {
    int interval[] = {0, 0};

    for(int i = 0; i < strlen(s); i++) {
        if(s[i] == c) {
            if(interval[0]  == 0) {
                interval[0] = i;
            } else {
                interval[1] = i;
            }
        }
    }

    if(interval[0] == 0) return -1;
    if(interval[1] == 0) return 0;

    int res = interval[1] - interval[0];
    return res;
}

char *string_between(char *s, char c) {
    int interval[] = {0, 0};

    for(int i = 0; i < strlen(s); i++) {
        if(s[i] == c) {
            if(interval[0] == 0) {
                interval[0] = i;
            } else {
                interval[1] = i;
            }
        }
    }

    if(interval[0] == 0) return NULL;
    if(interval[1] == 0) return strdup("");

    char *res = malloc((interval[1] - interval[0])*sizeof(char));
    if (res == NULL) return NULL;

    int i = 0;
    for(int j = interval[0] + 1; j < interval[1]; j++) {
        res[i++] = s[j];
    }
    res[i] = '\0';

    return res;
}

int stringsum2(char *s, int *res) {
    *res = stringsum(s);
    if(*res > 0) return 0;
    return -1;
}
