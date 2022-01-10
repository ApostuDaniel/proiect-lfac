%{
    #include <stdio.h>
    #include <string.h>
    #include <stdlib.h>
    #include <stdbool.h>
    #include <strings.h>
    #include "restart.h"
    #include "y.tab.h"
    extern YYSTYPE yylval;
%}
%option noyywrap
%%
"global" {printf("global"); return GLOBAL;}
"const" {printf("const"); return CONST;}
"var" {printf("var"); return VAR;}
"takes" {printf("takes"); return TAKES;}
"returns" {printf("returns"); return RETURNS;}
"string" {printf("string");return STRING;}
"int" {printf("int");return INT;}
"char" {printf("char");return CHAR;}
"bool" {printf("bool");return BOOL;}
"float" {printf("float");return FLOAT;}
"null" {printf("null");return VOID;}
"newtype" {printf("newtype");return NEWTYPE;}
"func" {printf("func");return FUNC;}
"start" {printf("start");return START;}
"if" {printf("if");return IF;}
"then" {printf("then");return THEN;}
"else" {printf("else");return ELSE;}
"for" {printf("for");return FOR;}
"while" {printf("while");return WHILE;}
"return" {printf("return");return RETURN;}
"Print" {printf("Print");return PRINT;}
"<-" {printf("<-");return ASSGN;}
"==" {printf("==");return EQ;}
"!=" {printf("!=");return NEQ;}
">=" {printf(">=");return GEQ;}
"<=" {printf("<=");return LEQ;}
"&&" {printf("&&");return AND;}
"||" {printf("||");return OR;}
"\+\+" {printf("++");return PP;}
"\-\-" {printf("--");return MM;}
"true"|"false" {yylval.boolval = yytext[0] == 't' ? true : false; printf("%s", yytext); return BOOLCONST;}
[1-9][0-9]*|0 {yylval.intval=atoi(yytext);printf("%d", yylval.intval); return INTCONST;}
([1-9][0-9]*|0)(\.[0-9]+)? {yylval.floatval=atof(yytext);printf("%f", yylval.floatval ); return FLOATCONST;}
\"[^"]* {yylval.strval = strndup(yytext+1, yyleng-1); input(); printf("%s", yylval.strval); return STRINGCONST;}
\'.\' {yylval.charval = yytext[1];printf("%c", yylval.charval); return CHARCONST;}
[_a-zA-Z][_a-zA-Z0-9]* {yylval.strval = strdup(yytext); printf("%s", yylval.strval); return ID;}
[ \t] {printf("%s", yytext);};
\n {yylineno++;printf("\n");}
. {printf("%c", yytext[0]);return yytext[0];}
<<EOF>> {printf("end_of_file\n"); yyterminate();}
%%