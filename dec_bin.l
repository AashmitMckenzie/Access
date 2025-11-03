%{
#include <stdio.h>
#include <stdlib.h>
void decimal_to_binary(char *str) {
    int num = atoi(str);
    char result[32];
    int pos = 0;
    
    do {
        result[pos++] = (num % 2) + '0';
        num /= 2;
    } while (num > 0);
    
    while (pos > 0) {
    	putchar(result[--pos]);
    }
}
%}

DECIMAL [0-9]+

%%
{DECIMAL}  { decimal_to_binary(yytext); }
%%

int main(int argc, char **argv) {
    if (argc > 1) {
        yyin = fopen(argv[1], "r");
        if (!yyin) {
            perror(argv[1]);
            return 1;
        }
    }
    yylex();
    if (yyin && yyin != stdin) fclose(yyin);
    return 0;
}
int yywrap(void) {
    return 1;
}
