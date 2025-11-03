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

%start start

%%


start: function_definition
     {
         printf("Function definition syntax is correct!\n");
     }
     ;

function_definition: return_type ID LPAREN parameter_list RPAREN function_body
                   | return_type ID LPAREN RPAREN function_body
                   ;

return_type: INT
           | FLOAT
           | CHAR
           | VOID
           ;

parameter_list: parameter_list COMMA parameter
              | parameter
              ;

parameter: return_type ID
         ;

function_body: LBRACE statements RBRACE
             ;

statements: statements statement
          | statement
          | /* empty */
          ;

statement: assignment SEMICOLON
         | expression SEMICOLON
         | return_statement
         | if_stmt
         | while_stmt
         | for_stmt
         | BREAK SEMICOLON
         | SEMICOLON
         ;

return_statement: RETURN expression SEMICOLON
                | RETURN SEMICOLON
                ;

assignment: ID ASSIGN expression
          ;

if_stmt: IF LPAREN expression RPAREN LBRACE statements RBRACE ELSE LBRACE statements RBRACE
       | IF LPAREN expression RPAREN LBRACE statements RBRACE
       ;

while_stmt: WHILE LPAREN expression RPAREN LBRACE statements RBRACE
          ;

for_stmt: FOR LPAREN expression SEMICOLON expression SEMICOLON expression RPAREN LBRACE statements RBRACE
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
          | function_call
          | ID
          | NUM
          ;

function_call: ID LPAREN argument_list RPAREN
             | ID LPAREN RPAREN
             ;

argument_list: argument_list COMMA expression
             | expression
             ;

%%

int yyerror(char *s) {
    printf("Syntax Error: %s\n", s);
    return 0;
}

int main() {
    printf("Enter function definition:\n");
    yyparse();
    return 0;
}

#include "lex.yy.c"

/*
flex scanner.l
yacc -d exp9_function.y
gcc y.tab.c -o exp9_function -lfl
./exp9_function
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


