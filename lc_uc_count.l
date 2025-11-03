%{
#include <stdio.h>
int lowercase = 0;
int uppercase = 0;
%}

LOWER   [a-z]
UPPER   [A-Z]

%%
{LOWER}    { lowercase++; }
{UPPER}    { uppercase++; }
.|\n       {  }
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
    printf("Lowercase letters: %d\n", lowercase);
    printf("Uppercase letters: %d\n", uppercase);
    return 0;
}
int yywrap(void) {
    return 1;
}
