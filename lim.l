%{
#include <stdio.h>
#include "lim.tab.h"
%}
%option noyywrap
alpha [a-zA-Z]
digit [0-9]
unary "++"|"--"
%%
"int" {return INT;}
"float" {return FLOAT;}
"string" {return STRING;}
"char" {return CHAR;}
"bool" {return BOOL;}
"begin_progr" {return BGIN;}
"end_progr" {return END;}
"struct" {return STRUCT; }
"enum" {return ENUM; }
"void" {return VOID;}
"for" { return FOR; }
"if" { return IF; }
"else" { return ELSE; }
"while" { return WHILE;}
"do" {return DO;}
"<=" { return LE; }
">=" { return GE; }
"==" { return EQ; }
"!=" { return NE; }
">"	 { return GT; }
"<"	 { return LT; }
"&&" { return AND; }
"||" { return OR; }
"+" { return ADD; }
"-" { return SUBTRACT; }
"/" { return DIVIDE; }
"*" { return MULTIPLY; }
":=" {return ASSIGN;}
"true" { return TRUE; }
"false" { return FALSE; }
"print" { return PRINT; }
"return" { return RETURN; }
[-]?{digit}+                { return NUMBER; }
[-]?{digit}+\.{digit}{1,6}  { return FLOAT_NUM; }
{alpha}({alpha}|{digit})*   {yylval.id=strdup(yytext); return ID; }
{unary}                     { return UNARY; }
['].['] { return CHARACTER; }
[ \t] ;
\n {yylineno++;}
\r ;
. {return yytext[0];}
