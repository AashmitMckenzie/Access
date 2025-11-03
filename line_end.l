%{ 
#include <stdio.h> 
int count_com=0, count_org=0, count_edu=0; 
%} 
 
%% 
 
www\.[a-zA-Z0-9_.-]+\.com   {printf("%s\n",yytext); count_com++;} 
www\.[a-zA-Z0-9_.-]+\.org   {printf("%s\n",yytext); count_org++;} 
www\.[a-zA-Z0-9_.-]+\.edu   {printf("%s\n",yytext); count_edu++;} 
www\.[a-zA-Z0-9_.-]+\.[a-z]+ {printf("%s\n",yytext);} 
 
\n|.  ;    
 
%% 
 
int main(int argc, char **argv){ 
    if(argc>1){ 
        FILE *fp=fopen(argv[1],"r"); 
        if(!fp){ 
            perror("Cannot open file"); 
            return 1; 
        } 
        yyin=fp; 
    } 
    yylex(); 
    printf("\nURLs ending with .com = %d\n",count_com); 
    printf("URLs ending with .org = %d\n",count_org); 
    printf("URLs ending with .edu = %d\n",count_edu); 
    if(argc>1) fclose(yyin);
    return 0; 
} 
 
int yywrap(){
    return 1;
}
