%{
#include <string.h>
#include <stdio.h>

char target_word[100];
int count = 0;
%}

%%
[a-zA-Z]+ {
    if (strcmp(yytext, target_word) == 0) {
        count++;
    }
}
.|\n   ; 
%%

int main(int argc, char *argv[])
{
    if (argc != 2) {
        printf("Usage: %s <word_to_count>\n", argv[0]);
        return 1;
    }
    
    strcpy(target_word, argv[1]);
    yylex();
    printf("The word exists '%s':%d times\n", target_word, count);
    return 0;
}
int yywrap(void) {
    return 1;
}

/*
lex wordcount.l

gcc lex.yy.c -ll -o wordcount

./wordcount hello < output.txt

*/
