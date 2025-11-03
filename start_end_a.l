%{
#include <stdio.h>
int a_beginandend=0;
int a_begin=0;
int a_end=0;
int a_total=0;
%}

digit [0-9]
letter [a-zA-Z]
id {letter}({letter}|{digit})*
words {letter}*
spl_char [^a-zA-Z0-9 \t\n]

%%
[aA]{letter}*[aA] {printf("Words that start and end with a: %s\n", yytext); a_beginandend++;}
[aA]{letter}* {printf("Words that start a: %s\n", yytext); a_begin++;}
{letter}*[aA] {printf("Words that end with a: %s\n", yytext); a_end++;}
{letter}+ { /* Other words - ignore */ }
[ \t\n] { /* space and tab - ignore */ }
{spl_char} { /* Special characters - ignore */ }

%%

int main(int argc, char **argv){
	if(argc>1){
		FILE *file=fopen(argv[1],"r");
		if(!file){
			perror("File not found");
			return 1;
		}
		yyin=file;
	}
	yylex();
	a_total=a_begin+a_end+a_beginandend;
	printf("Number words that start and end with a: %d\n", a_beginandend);
	printf("Number words that end with a: %d\n", a_begin);
	printf("Number words that begin with a: %d\n", a_begin);	
	printf("Total number words that begin with a, end with a and that start and end with a: %d\n", a_total);
}
int yywrap(){
return 1;
}
