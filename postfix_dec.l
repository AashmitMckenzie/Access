%{
#include "postfix_dec.tab.h"
#include <stdlib.h>
%}

%%
[0-9]+(\.[0-9]+)?    { yylval.fval = atof(yytext); return NUMBER; }
[\n]                 { return '\n'; }
[ \t]                ;   // ignore spaces and tabs
.                    { return yytext[0]; }
%%

int yywrap(void) { 
return 1; 
}

bison -d postfix_dec.y
flex postfix_dec.l
gcc lex.yy.c postfix_dec.tab.c -o postfix_dec -lfl -lm
./postfix_dec < output.txt

3 4 2 * +
10 2 /
5.5 2.5 +
2 3 ^
10 2 5 + /

postfix_dec.y

%{
#include <stdio.h>
#include <math.h>

/* Forward declarations */
int yylex(void);
int yyerror(char *s);
%}

/* Define value type */
%union {
    double fval;
}

%token <fval> NUMBER
%type <fval> expr

%left '+' '-'
%left '*' '/'
%right '^'

%%

input:
      /* empty */
    | input expr '\n'    { printf("Result = %g\n", $2); }
;

expr:
      expr expr '+'      { $$ = $1 + $2; }
    | expr expr '-'      { $$ = $1 - $2; }
    | expr expr '*'      { $$ = $1 * $2; }
    | expr expr '/'      {
                            if ($2 == 0) {
                                printf("Error: Division by zero\n");
                                $$ = 0;
                            } else {
                                $$ = $1 / $2;
                            }
                         }
    | expr expr '^'      { $$ = pow($1, $2); }
    | NUMBER             { $$ = $1; }
;

%%

int yyerror(char *s) {
    fprintf(stderr, "Error: %s\n", s);
    return 0;
}

int main(void) {
    printf("Enter postfix expressions (Ctrl+D to exit):\n");
    yyparse();
    return 0;
}

