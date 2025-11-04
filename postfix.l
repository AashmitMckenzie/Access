%{
#include "postfix.tab.h"
%}

%%
[0-9]+          { yylval = atoi(yytext); return NUMBER; }
[\+\-\*/]         { return yytext[0]; }
[\n]            { return '\n'; }
[ \t]           ;  // Ignore whitespace
%%

int yywrap(void) {
    return 1;
}

bison -d postfix.y
flex postfix.l
gcc lex.yy.c postfix.tab.c -o postfix -lfl
./postfix < output.txt

3 4 + 2 *
5 6 2 / +
10 2 8 * + 3 –

postfix.y

%{
#include <stdio.h>
#include <stdlib.h>

int yylex(void);       // Declare scanner
int yyerror(char *s);

#define STACKSIZE 100
int stack[STACKSIZE];
int top = -1;

void push(int val) {
    if (top >= STACKSIZE - 1) {
        printf("Stack overflow\n");
        exit(1);
    }
    stack[++top] = val;
}

int pop() {
    if (top < 0) {
        printf("Stack underflow\n");
        exit(1);
    }
    return stack[top--];
}
%}

%token NUMBER

%%

input:
    /* empty */
  | input line
  ;

line:
    expr '\n'    { printf("Result: %d\n", pop()); }
  ;

expr:
    expr NUMBER     { push($2); }
  | expr '+'        { int b = pop(); int a = pop(); push(a + b); }
  | expr '-'        { int b = pop(); int a = pop(); push(a - b); }
  | expr '*'        { int b = pop(); int a = pop(); push(a * b); }
  | expr '/'        { int b = pop(); int a = pop(); push(a / b); }
  | NUMBER          { push($1); }
  ;

%%

int main() {
    printf("Enter postfix expression (e.g., 3 4 + 2 *):\n");
    yyparse();
    return 0;
}

int yyerror(char *s) {
    fprintf(stderr, "Error: %s\n", s);
    return 0;
}
