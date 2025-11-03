%{
#include <stdio.h>
%}

UPPER   [A-Z]
LOWER   [a-z]

%%

{UPPER}    { printf("'%c' is an uppercase letter\n", yytext[0]); }
{LOWER}    { printf("'%c' is a lowercase letter\n", yytext[0]); }
.          { printf("'%c' is a non-alphabetic character\n", yytext[0]); }

%%

int main(int argc, char **argv) {
    if (argc > 1) {
        if (!(yyin = fopen(argv[1], "r"))) {
            perror(argv[1]);
            return 1;
        }
    } else {
        printf("Enter a character/text:\n");
        yyin = stdin;
    }

    yylex();
    return 0;
}

int yywrap(void) {
    return 1;
}
