%{ 
#include <stdio.h> 
int valid = 0, invalid = 0; 
%} 
%% 
[a-z0-9._]+@[a-z]+\.(com|in|org|edu\.in|net|gov|co\.in) {printf("Valid: %s\n", yytext);valid++;} 
[a-z0-9._]+@[a-z]+\.[a-z]+ {printf("Invalid domain: %s\n", yytext);invalid++;} 
[^ \t\n]+ {printf("Invalid format: %s\n", yytext);invalid++;} 
[ \t\n]  ; 
%% 
int main(int argc, char *argv[]) { 
    if(argc > 1) { 
        FILE *fp = fopen(argv[1], "r"); 
        if(fp) { 
            printf("Reading from file: %s\n\n", argv[1]); 
            yyin = fp; 
            yylex(); 
            fclose(fp); 
        } else { 
            printf("Cannot open file!\n"); 
            return 1; 
        } 
    } else { 
        printf("Enter emails (Ctrl+D to end):\n"); 
        yylex(); 
    } 
    printf("\n--- Summary ---\n"); 
    printf("Valid emails: %d\n", valid); 
    printf("Invalid emails: %d\n", invalid); 
    return 0; 
} 
int yywrap() {  
    return 1; 
}

/*
flex email.l
gcc lex.yy.c
./a.out output.txt
*/
