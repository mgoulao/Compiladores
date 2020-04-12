 
%{
#include <stdio.h>
#include <stdlib.h>
#ifndef YYERRCODE
#define YYERRCODE 256
#endif
#define YYDEBUG 1
%}

%union {
    int i;	/* integer value */
    char* s;    /* string value*/
    double r;   /* (real) number value*/
};

%token <i> INTEGER
%token <s> IDENT TEXTSTRING
%token MODULE PROGRAM START END VOID CONST NUMBER ARRAY STRING FUNCTION
%token PUBLIC FORWARD IF THEN ELSE ELIF FI FOR UNTIL STEP DO DONE REPEAT STOP RETURN NIL



%nonassoc IF
%nonassoc ELSE
%right ASSIGN
%left GE LE EQ NE '<' '>'
%left '~' '&' '|'
%left '+' '-'
%left '*' '/' '%' '^'
%left '?'

%%
start:;
%%
int yyerror(char *s) { printf("%s\n",s); return 1; }
char *dupstr(const char*s) { return strdup(s); }
int main(int argc, char *argv[]) {
	extern YYSTYPE yylval;
	int tk;
	while ((tk = yylex()))
		if (tk > YYERRCODE)
			printf("%d:\t%s\n", tk, yyname[tk]);
		else
			printf("%d:\t%c\n", tk, tk);
	return 0;
}
