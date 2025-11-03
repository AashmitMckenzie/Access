%{
#include <stdio.h>
%}

LOWER   [\x61-\x7A]    
UPPER   [\x41-\x5A]    
SPACE   [ \t\n]

%%

{LOWER}    { putchar( yytext[0] - 32 ); }
{UPPER}    { putchar( yytext[0] + 32 ); }
{SPACE}    { putchar( yytext[0] ); }
.          { putchar( yytext[0] ); }

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
