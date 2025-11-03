%{ 
#include "y.tab.h" 
%} 

%% 
[0-9]+(\.[0-9]+)?   { 
                        yylval.dval = atof(yytext); 
                        return NUMBER;          
                    } 
\n                  return '\n';           
[ \t]               ;                         
"+"                 return '+'; 
"-"                 return '-'; 
"*"                 return '*'; 
"/"                 return '/'; 
.                   {printf("Invalid character: %s\n",yytext);} 
%% 
 
int yywrap(){ 
    return 1; 
}

/*
"("                 return '('; 
")"                 return ')'; 

yacc -d calc_error.y
lex calc_error.l
cc lex.yy.c y.tab.c -o calc_error -lm
./calc_error

*/

%{ 
#include <stdio.h> 
#include <ctype.h> 
 
void yyerror(const char *s); 
int yylex(void); 
 
%} 
 
%union { 
    double dval; 
} 
 
%token <dval> NUMBER 
%left '+' '-' 
%left '*' '/' 
%right UMINUS 
 
%type <dval> expr 
 
%% 
 
input: 
    /* empty */ 
    | input line 
; 
 
line: 
    '\n' 
    | expr '\n' { printf("Result: %.6g\n", $1); } 
    | error '\n' {  
        yyerror("Invalid expression, please enter again.");  
        yyerrok;   // Recover from error  
    } 
; 
 
expr: 
    NUMBER 
    | expr '+' expr { $$ = $1 + $3; } 
    | expr '-' expr { $$ = $1 - $3; } 
    | expr '*' expr { $$ = $1 * $3; } 
    | expr '/' expr {  
        if ($3 == 0) { 
            yyerror("Division by zero"); 
            $$ = 0; 
        } else { 
            $$ = $1 / $3; 
        } 
      } 
    | '-' expr %prec UMINUS { $$ = -$2; } 
    | '(' expr ')' { $$ = $2; } 
; 
 
%% 
 
void yyerror(const char *s) { 
    fprintf(stderr, "Error: %s\n", s); 
} 
 
int main() { 
    printf("Enter expressions:\n"); 
    yyparse(); 
    return 0; 
}
