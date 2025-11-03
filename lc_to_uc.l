%{
#include <stdio.h>
%}

LOWER   [a-z]
UPPER   [A-Z]
OTHER   .    /* any character not caught above */

%%
{LOWER}    { putchar( yytext[0] - ('a' - 'A') ); }
{UPPER}    { putchar( yytext[0] + ('a' - 'A') ); }
.    { putchar( yytext[0] ); }                
%%

int main(void) {
    printf("Enter text:\n");
    yyin = stdin;

    yylex();
    return 0;
}
int yywrap(void) {
    return 1;
}
