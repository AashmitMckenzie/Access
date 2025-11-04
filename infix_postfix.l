%{
#include "infix_postfix.tab.h"
#include <string.h>
#include <stdlib.h>
%}

%%
[0-9]+ {
    yylval.str = strdup(yytext);
    return NUM;
}

[+\-*/^()] { return yytext[0]; }

\n    { return '\n'; }     // return newline to parser
[ \t] ;                   // ignore spaces and tabs
%%

int yywrap() { 
return 1; 
}

bison -d infix_postfix.y
flex infix_postfix.l
gcc lex.yy.c infix_postfix.tab.c -o infix_postfix -lfl
./infix_postfix < output.txt

3+4*2
(1+2)*3
10/(5-3)
7*(8+9)-4
(6+2)*(5-3)/4

infix_postfix.y

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Forward declarations */
int yylex(void);
int yyerror(char *s);
%}

/* Define value type */
%union {
    char *str;
}

%token <str> NUM
%type <str> expr

%left '+' '-'
%left '*' '/'

%%

/* Allow multiple lines of expressions */
input:
      /* empty */
    | input expr '\n' { printf("Postfix: %s\n", $2); free($2); }
;

expr:
      expr '+' expr {
        char *res = malloc(strlen($1) + strlen($3) + 3);
        sprintf(res, "%s %s +", $1, $3);
        free($1); free($3);
        $$ = res;
    }
    | expr '-' expr {
        char *res = malloc(strlen($1) + strlen($3) + 3);
        sprintf(res, "%s %s -", $1, $3);
        free($1); free($3);
        $$ = res;
    }
    | expr '*' expr {
        char *res = malloc(strlen($1) + strlen($3) + 3);
        sprintf(res, "%s %s *", $1, $3);
        free($1); free($3);
        $$ = res;
    }
    | expr '/' expr {
        char *res = malloc(strlen($1) + strlen($3) + 3);
        sprintf(res, "%s %s /", $1, $3);
        free($1); free($3);
        $$ = res;
    }
    | '(' expr ')' { $$ = $2; }
    | NUM { $$ = $1; }
;

%%

int yyerror(char *s) {
    fprintf(stderr, "Error: %s\n", s);
    return 0;
}

int main() {
    yyparse();
    return 0;
}

