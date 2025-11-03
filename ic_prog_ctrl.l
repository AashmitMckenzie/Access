%{
#include "y.tab.h"
%}
%%
"if"        return IF;
"while"     return WHILE;
[0-9]+      return NUM;
[a-z]       return ID;
"<"         return LT;
">"         return GT;
[ \t\n]     ;
.           return yytext[0];
%%
int yywrap() {
    return 1;
}

/*
flex exp10_cons.l
yacc -d exp10_cons.y
gcc y.tab.c lex.yy.c -o exp10_cons
./exp10_cons
*/

prog_ctrl.y

%{
#include <stdio.h>
int yylex();
void yyerror(char *s);
int t = 0, lbl = 0;
%}
%token IF WHILE NUM ID LT GT
%%
stmt : assign | if_stmt | while_stmt ;

assign : ID '=' expr ';'
       { printf("%c = t%d\n", $1, t-1); };

if_stmt : IF '(' cond ')' '{' stmt '}'
        { printf("L%d:\n", lbl-1); };

while_stmt : WHILE { printf("L%d:\n", lbl++); }
             '(' cond ')' '{' stmt '}'
             { printf("goto L%d\nL%d:\n", lbl-2, lbl-1); };

cond : expr LT expr
     { 
       printf("if t%d < t%d goto L%d\n", t-2, t-1, lbl);
       printf("goto L%d\nL%d:\n", lbl+1, lbl);
       lbl += 2;
     }
     | expr GT expr
     { 
       printf("if t%d > t%d goto L%d\n", t-2, t-1, lbl);
       printf("goto L%d\nL%d:\n", lbl+1, lbl);
       lbl += 2;
     };

expr : expr '+' expr { printf("t%d = t%d + t%d\n", t, t-2, t-1); t++; }
     | expr '-' expr { printf("t%d = t%d - t%d\n", t, t-2, t-1); t++; }
     | NUM { printf("t%d = %d\n", t++, $1); }
     | ID { printf("t%d = %c\n", t++, $1); };
%%
void yyerror(char *s) { printf("Error\n"); }
int main() {
    printf("Enter code: ");
    yyparse();
    return 0;
}
