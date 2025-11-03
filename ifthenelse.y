%{
#include <stdio.h>
#include <stdlib.h>

int yylex();
int yyerror(char *s);
%}

%token ID NUM FOR LE GE EQ NE LT GT OR AND NOT
%token ASSIGN PLUS MINUS MUL DIV INCR DECR
%token LPAREN RPAREN LBRACE RBRACE SEMICOLON COMMA
%token WHILE SWITCH CASE DEFAULT IF THEN ELSE
%token INT FLOAT CHAR VOID RETURN BREAK COLON LBRACKET RBRACKET

%left OR
%left AND
%left EQ NE
%left LT GT LE GE
%left PLUS MINUS
%left MUL DIV
%right NOT
%right ASSIGN

%nonassoc THEN
%nonassoc ELSE

%start start

%%

start: if_stmt
     {
         printf("IF-THEN-ELSE syntax is correct!\n");
     }
     ;

if_stmt: IF LPAREN condition RPAREN THEN body ELSE body
       | IF LPAREN condition RPAREN THEN body %prec THEN
       ;

condition: expression
         ;

body: LBRACE statements RBRACE
    | statement
    ;

statements: statements statement
          | statement
          ;

statement: assignment SEMICOLON
         | expression SEMICOLON
         | if_stmt
         | SEMICOLON
         ;

assignment: ID ASSIGN expression
          ;

expression: expression PLUS expression
          | expression MINUS expression
          | expression MUL expression
          | expression DIV expression
          | expression LT expression
          | expression GT expression
          | expression LE expression
          | expression GE expression
          | expression EQ expression
          | expression NE expression
          | expression AND expression
          | expression OR expression
          | NOT expression
          | LPAREN expression RPAREN
          | ID INCR
          | ID DECR
          | INCR ID
          | DECR ID
          | ID
          | NUM
          ;

%%

int yyerror(char *s) {
    printf("Syntax Error: %s\n", s);
    return 0;
}

int main() {
    printf("Enter IF-THEN-ELSE statement:\n");
    yyparse();
    return 0;
}

#include "lex.yy.c"

/*
flex scanner.l
yacc -d exp9_ifthenelse.y
gcc y.tab.c -o exp9_ifthenelse -lfl
./exp9_ifthenelse
*/

scanner.l

%{
#include <stdio.h>
#include <stdlib.h>
#include "y.tab.h"
%}

digit    [0-9]
letter   [a-zA-Z]
id       {letter}({letter}|{digit})*
number   {digit}+

%%

"for"       { return FOR; }
"while"     { return WHILE; }
"switch"    { return SWITCH; }
"case"      { return CASE; }
"default"   { return DEFAULT; }
"if"        { return IF; }
"then"      { return THEN; }
"else"      { return ELSE; }
"int"       { return INT; }
"float"     { return FLOAT; }
"char"      { return CHAR; }
"void"      { return VOID; }
"return"    { return RETURN; }
"break"     { return BREAK; }

{id}        { return ID; }
{number}    { return NUM; }

"<="        { return LE; }
">="        { return GE; }
"=="        { return EQ; }
"!="        { return NE; }
"<"         { return LT; }
">"         { return GT; }

"&&"        { return AND; }
"||"        { return OR; }
"!"         { return NOT; }

"="         { return ASSIGN; }
"+"         { return PLUS; }
"-"         { return MINUS; }
"*"         { return MUL; }
"/"         { return DIV; }
"++"        { return INCR; }
"--"        { return DECR; }

"("         { return LPAREN; }
")"         { return RPAREN; }
"{"         { return LBRACE; }
"}"         { return RBRACE; }
"["         { return LBRACKET; }
"]"         { return RBRACKET; }
";"         { return SEMICOLON; }
","         { return COMMA; }
":"         { return COLON; }

[ \t\n]+    { /* ignore whitespace */ }

 /* Comments */
"/*"([^*]|\*+[^*/])*\*+"/"  { /* ignore comments */ }
"//".*                       { /* ignore single line comments */ }
.           { printf("Unexpected character: %s\n", yytext); }

%%

int yywrap() {
    return 1;
}


