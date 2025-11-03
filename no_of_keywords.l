%{
#include <stdio.h>
int spl=0, lines=0, spaces=0, tabs=0, ids=0, words=0, ints=0, floats=0;
%}
%%
[0-9]+\.[0-9]+ {printf("Float: %s\n", yytext); floats++;}
[0-9]+ {printf("Integer: %s\n", yytext); ints++;}
[a-zA-Z][0-9]* {printf("Identifier: %s\n", yytext); ids++;}
[a-zA-Z]+ {printf("Word: %s\n", yytext); words++;}
[^a-zA-Z0-9 \t\n] {printf("Special char: %s\n", yytext); spl++;}
\n {lines++;}
\t {tabs++;}
" " {spaces++;}
%%
int main(int argc, char **argv){
    if(argc>1 && !(yyin=fopen(argv[1],"r"))){
        perror("File not found");
        return 1;
    }
    yylex();
    printf("Identifiers: %d\nWords: %d\nIntegers: %d\nFloats: %d\n"
           "Lines: %d\nTabs: %d\nSpaces: %d\nSpecial chars: %d\n"
           "Total tokens: %d\n", 
           ids, words, ints, floats, lines, tabs, spaces, spl,
           ids+words+ints+floats+lines+tabs+spaces+spl);
    if(argc>1) fclose(yyin);
    return 0;
}
int yywrap(){return 1;}
