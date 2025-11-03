%{
#include <stdio.h>
int n1, n2, n3, cnt = 0;
char op1, op2;
%}
%%
[0-9]+  { 
            printf("Num: %s\n", yytext);
            if(cnt==0) n1 = atoi(yytext);
            if(cnt==1) n2 = atoi(yytext);
            if(cnt==2) n3 = atoi(yytext);
            cnt++;
        }
"+"     { printf("Op: +\n"); if(!op1) op1='+'; else op2='+'; }
"-"     { printf("Op: -\n"); if(!op1) op1='-'; else op2='-'; }
"*"     { printf("Op: *\n"); if(!op1) op1='*'; else op2='*'; }
"/"     { printf("Op: /\n"); if(!op1) op1='/'; else op2='/'; }
\n      { 
            int res;
            if(op1=='+') res = n1 + n2;
            if(op1=='-') res = n1 - n2;
            if(op1=='*') res = n1 * n2;
            if(op1=='/') res = n1 / n2;
            if(op2=='+') res = res + n3;
            if(op2=='-') res = res - n3;
            if(op2=='*') res = res * n3;
            if(op2=='/') res = res / n3;
            printf("\nResult = %d\n", res);
            return 0;
        }
[ \t]   ;
%%
int main() {
    printf("Enter: ");
    yylex();
    return 0;
}
int yywrap() { return 1; }

/*
flex exp11.l
gcc lex.yy.c -o exp11
./exp11
*/
