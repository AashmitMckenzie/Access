%{
#include "y.tab.h"
extern int yylval;
%}
%%
[0-9]+  { yylval = atoi(yytext); return NUM; }
[a-z]   { yylval = yytext[0]; return ID; }
\n      return 0;
[ \t]   ;
.       return yytext[0];
%%
int yywrap() {
    return 1;
}

/*
flex exp10_arith.l
yacc -d exp10_arith.y
gcc y.tab.c lex.yy.c -o exp10_arith
./exp10_arith
*/

ic_arith.y

%{
#include <stdio.h>
int yylex();
void yyerror(char *s);
int t = 1;
%}
%token NUM ID
%%
S : E { printf("\n"); }
  | ID '=' E { printf("%c = t%d\n\n", $1, t-1); };
E : E '+' E { printf("t%d = t%d + t%d\n", t, t-2, t-1); t++; }
  | E '-' E { printf("t%d = t%d - t%d\n", t, t-2, t-1); t++; }
  | E '*' E { printf("t%d = t%d * t%d\n", t, t-2, t-1); t++; }
  | E '/' E { printf("t%d = t%d / t%d\n", t, t-2, t-1); t++; }
  | NUM     { printf("t%d = %d\n", t++, $1); }
  | ID      { printf("t%d = %c\n", t++, $1); };
%%
void yyerror(char *s) { printf("Error\n"); }
int main() {
    printf("Enter expression: ");
    yyparse();
    return 0;
}
