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
};

%token <i> INTEGER CHAR
%token <s> IDENT STRING
%token MODULE PROGRAM START END VOID CONST NUMBER ARRAY STRING TEXTSTRING CHAR FUNCTION
%token PUBLIC FORWARD IF THEN ELSE ELIF FI FOR UNTIL STEP DO DONE REPEAT STOP RETURN

%nonassoc IF
%nonassoc ELSE
%right ASSIGN
%left GE LE EQ '=' NE '<' '>'
%nonassoc '~' '&' '?'
%left '|'
%left '+' '-'
%left '*' '/' '%'
%right '^'
%nonassoc UMINUS
%nonassoc '[' '('

%%


file	: program
	| module 
      	;

program	: PROGRAM decls START body END
	;

module	: MODULE decls END
	;

decl	: function 
	| public var opt_initializer
	| FORWARD CONST var
	| FORWARD var
	| public CONST initializer	
	;

opt_initializer
	:
	| initializer
	;

initializer
	: ARRAY IDENT ASSIGN array_init    
 	| NUMBER IDENT ASSIGN INTEGER      
	| STRING IDENT ASSIGN string

decls 	:
      	| decl 
       	| decls ';' decl
	;

function: FUNCTION FORWARD type IDENT params DONE 
       	| FUNCTION public type IDENT params DO body return
	;

var	: NUMBER IDENT
    	| STRING IDENT
	| ARRAY IDENT
    	| ARRAY IDENT '[' INTEGER ']'
	;

params	:
    	| var
     	| params ';' var
	;

args	: expr
     	| args ',' expr
	;

vars 	: var ';'
	| vars var ';'
	;

lvalue	: IDENT
       	| IDENT '[' expr ']'
	| '*' lvalue
	; 

type	: NUMBER 
	| STRING
	| ARRAY
	;

qualif	:
       	| FORWARD
	| PUBLIC
	;

public  : 
	| PUBLIC 
	;

body	: vars instrs 
	| instrs     
	;
				
literal	: INTEGER
	| CHAR
	;

array_init
	: INTEGER
	| array_init ',' INTEGER

literals: literal
	| string
	;

instr   : IF expr THEN instrs return elifs FI
	| IF expr THEN instrs return elifs ELSE instrs return FI
	| FOR expr UNTIL expr STEP expr DO instrs return DONE 
	| expr ';'
	| expr '!'
	| REPEAT
	| STOP
	| lvalue '#' expr ';'
	| error ';'
	;

return	:
      	| RETURN
       	| RETURN expr
	;

instrs	:
	| instrs instr
	;

elifs	:
	| ELIF THEN instrs return elifs
	;

string	: TEXTSTRING
       	| str_init str_continuation
	;

str_continuation
	:
	| str_continuation str_symbol
	; 

str_init: str_symbol str_symbol
	;

str_symbol
	: CHAR
	| INTEGER
	| TEXTSTRING
	;

expr	: lvalue
	| literal
	| '-' expr %prec UMINUS
	| '?' expr
	| '&' lvalue %prec UMINUS
	| expr '+' expr
	| expr '-' expr
	| expr '*' expr
	| expr '/' expr
	| expr '%' expr
	| expr '<' expr
	| expr '>' expr
	| expr GE expr
	| expr LE expr
	| expr NE expr
	| expr EQ expr
	| expr '=' expr
	| expr '&' expr
	| expr '|' expr
	| '(' expr ')'
	| IDENT '(' args ')'
	;

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
