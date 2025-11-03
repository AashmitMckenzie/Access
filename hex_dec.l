%{
#include <stdio.h>
#include <stdlib.h>

void hex_to_decimal(char *str) {
    int num = strtol(str, NULL, 16);
    printf("%d ", num);
}
%}

%%
[0-9A-Fa-f]+   { hex_to_decimal(yytext); }
.              { putchar(yytext[0]); }
\n             { putchar('\n'); }
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
