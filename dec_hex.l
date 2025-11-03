%{
#include <stdio.h>
#include <stdlib.h>
void decimal_to_hex(char *str) {
    int decimal = atoi(str);
    int i = 0;
    char hex_digits[] = "0123456789ABCDEF";
    char hex_result[20];
    int hex_index = 0;
    
    do {
        int remainder = decimal % 16;
        hex_result[hex_index] = hex_digits[remainder];
        hex_index++;
        decimal = decimal / 16;
    } while (decimal > 0);

    for (i = hex_index - 1; i >= 0; i--) {
        putchar(hex_result[i]);
    }

}
%}

DECIMAL [0-9]+

%%
{DECIMAL}  { decimal_to_hex(yytext); }
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
