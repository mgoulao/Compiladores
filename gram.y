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

%token <i> INTEGER CHAR
%token <s> VARIABLE STRING
%token MODULE PROGRAM START END VOID CONST NUMBER ARRAY STRING FUNCTION
%token PUBLIC FORWARD IF THEN ELSE ELIF FI FOR UNTIL STEP DO DONE REPEAT STOP RETURN

%nonassoc IF
%nonassoc ELSE
%right ASSIGN
%left GE LE EQ NE LESS GREATER
%left LNOT LAND LOR
%left PLUS MINUS
%left TIMES DIVISION MOD POW
%left READ

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
