%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "node.h"
#include "tabid.h"
extern int yylex();
%}

%union {
    int i;	/* integer value */
    char* s;    /* string value*/
    Node* n;	/* Node pointer */
};

%token <i> INTEGER CHAR
%token <s> IDENT TEXTSTRING
%token MODULE PROGRAM START END VOID CONST NUMBER ARRAY STRING CHAR FUNCTION ASSING
%token PUBLIC FORWARD IF THEN ELSE ELIF FI FOR UNTIL STEP DO DONE REPEAT STOP RETURN NIL

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

%type <n> program module decl opt_initializer initializer decls
%type <n> function public var params args vars lvalue type body literal array_init
%type <n> literals  stmt return stmts elifs string str_init str_continuation
%type <n> str_symbol expr

%%


file	: program 	{ printNode($1, 0, (char**)yyname);}
	| module 	{ printNode($1, 0, (char**)yyname);} 
      	;

program	: PROGRAM decls START body END	{ $$ = binNode(PROGRAM, $2, $4); }
	;

module	: MODULE decls END { $$ = uniNode(MODULE, $2); }
	;

decl	: function { $$ = $1; }
	| public var opt_initializer { $$ = triNode(ASSIGN, $1, $2, $3); } 
	| FORWARD CONST var { $$ = uniNode(CONST, $3); }
	| FORWARD var	{ $$ = uniNode(FORWARD, $2); }
	| public CONST initializer { binNode(CONST, $1, $3); }	
	;

opt_initializer
	:		{ $$ = nilNode(NIL); }
	| initializer	{ $$ = $1; }
	;

initializer
	: ARRAY IDENT ASSIGN array_init { $$ = binNode(ASSIGN, $2, uniNode(ARRAY, $4)); }
 	| NUMBER IDENT ASSIGN INTEGER { $$ = binNode(ASSING, $2, intNode(NUMBER, $4)); }
	| STRING IDENT ASSIGN string { $$ = binNode(ASSIGN, $2, strNode(STRING, $4)); } 

decls 	:		{ $$ = nilNode(NIL); }
      	| decl 		{ $$ = uniNode(';', $1); }
       	| decls ';' decl { $$ = binNode(';', $1, $3); }
	;

function: FUNCTION FORWARD type IDENT params DONE { $$ = quadNode(';', nilNode(FORWARD), $3, $4, $5); }
       	| FUNCTION public type IDENT params DO body return { $$ = triNode(FUNCTION, quadNode(';', $2, $3, $4, $5), $7 ,$8); }
	;

var	: NUMBER IDENT	{ $$ = strNode(NUMBER, $2); }
    	| STRING IDENT	{ $$ = strNode(STRING, $2); }
	| ARRAY IDENT	{ $$ = strNode(ARRAY, $2); }
    	| ARRAY IDENT '[' INTEGER ']' { $$ = binNode(ARRAY, $2, $4); }
	;

params	:		{ $$ = nilNode(NIL); }
    	| var		{ $$ = uniNode(';', $1); }
     	| params ';' var { $$ = binNode(';', $1, $3); }
	;

args	: expr		{ $$ = uniNode(',', $1); }
     	| args ',' expr	{ $$ = binNode(',', $1, $3); }
	;

vars 	: var ';'	{ $$ = uniNode(';', $1); }
	| vars var ';'	{ $$ = binNode(';', $1, $2); }
	;

lvalue	: IDENT		{ $$ = strNode(IDENT, $1);}
       	| IDENT '[' expr ']' { $$ = binNode(IDENT, $1, $3); }
	| '*' lvalue	{ $$ = uniNode('*', $2); }
	; 

type	: NUMBER	{ $$ = nilNode(NUMBER); } 
	| STRING	{ $$ = nilNode(STRING); }
	| ARRAY 	{ $$ = nilNode(ARRAY); } 
	;

public  : 		{ $$ = nilNode(NIL); }
	| PUBLIC 	{ $$ = nilNode(PUBLIC); }
	;

body	: vars stmts 	{ $$ = binNode('{', $1, $2); }
	| stmts     	{ $$ = uniNode('{', $1); }  
	;
				
literal	: INTEGER	{ $$ = intNode(NUMBER, $1); }
	| CHAR		{ $$ = intNode('a', (int)$1); }
	;

array_init
	: INTEGER	{ $$ = intNode(NUMBER, $1); }
	| array_init ',' INTEGER { $$ = binNode(',', $1, $3); }
	;

literals: literal	{ $$ = uniNode('.', $1); }
	| string	{ $$ = strNode(STRING, $1); }
	;

stmt  : IF expr THEN stmts return elifs FI { $$ = quadNode(IF, $2, $4, $5, uniNode(ELIF, $6)); }
	| IF expr THEN stmts return elifs ELSE stmts return FI { $$ = quadNode(ELSE, triNode(IF, $2, $4, $5), uniNode(ELIF, $6), $8, $9); }  
	| FOR expr UNTIL expr STEP expr DO stmts return DONE { $$ = binNode(FOR, triNode(',', $2, $4, $6), binNode('{', $8, $9)); } 
	| expr ';'	{ $$ = $1; }
	| expr '!'	{ $$ = uniNode('!', $1); }
	| REPEAT	{ $$ = nilNode(REPEAT); }
	| STOP		{ $$ = nilNode(STOP); }
	| lvalue '#' expr ';' { $$ = binNode('#', $1, $3); }
	| error ';'
	;

return	:		{ $$ = nilNode(NIL); }
      	| RETURN	{ $$ = nilNode(RETURN); }
       	| RETURN expr	{ $$ = uniNode(RETURN, $2); }
	;

stmts
	:		{ $$ = nilNode(NIL); }
	| stmts stmt	{ $$ = binNode('.', $1, $2); }
	;

elifs	:		{ $$ = nilNode(NIL); }
	| ELIF THEN stmts return elifs { $$ = binNode(ELIF, binNode(ELIF, $3, $4), $5); }
	;

string	: TEXTSTRING 	{ $$ = strNode(STRING, $1); }
       	| str_init str_continuation { $$ = binNode(STRING, $1, $2); } 
	;

str_continuation
	:		{ $$ = nilNode(STRING); }
	| str_continuation str_symbol { $$ = binNode(STRING, $1, $2); }
	; 

str_init: str_symbol str_symbol	{ $$ = binNode(STRING, $1, $2); }
	;

str_symbol
	: CHAR		{ $$ = intNode(CHAR, (int)$1); }
	| INTEGER	{ $$ = intNode(INTEGER, $1); }
	| TEXTSTRING	{ $$ = strNode(STRING, $1); }
	;

expr	: lvalue 	{ $$ = uniNode('*', $1); }
	| INTEGER 	{ $$ = intNode(INTEGER, $1); }
	| string 	{ $$ = $1; }
	| '-' expr %prec UMINUS { $$ = uniNode('-', $2); }
	| '?' expr 	{ $$ = uniNode('?', $2); }
	| '&' lvalue %prec UMINUS { $$ = uniNode('&', $2); }
	| expr '+' expr	{ $$ = binNode('+', $1, $3); }
	| expr '-' expr	{ $$ = binNode('-', $1, $3); }
	| expr '*' expr	{ $$ = binNode('*', $1, $3); }
	| expr '/' expr	{ $$ = binNode('/', $1, $3); }
	| expr '%' expr	{ $$ = binNode('%', $1, $3); }
	| expr '<' expr	{ $$ = binNode('<', $1, $3); }
	| expr '>' expr	{ $$ = binNode('>', $1, $3); }
	| expr GE expr	{ $$ = binNode(GE, $1, $3); }
	| expr LE expr	{ $$ = binNode(LE, $1, $3); }
	| expr NE expr	{ $$ = binNode(NE, $1, $3); }
	| expr EQ expr	{ $$ = binNode(EQ, $1, $3); }
	| expr '=' expr	{ $$ = binNode('=', $1, $3); }
	| expr '&' expr	{ $$ = binNode('&', $1, $3); }
	| expr '|' expr { $$ = binNode('|', $1, $3); }
	| '(' expr ')' 	{ $$ = $2; }
	| IDENT '(' args ')' { $$ = binNode('(', strNode(IDENT, $1), $3); }
	;

%%
char **yynames =
#if YYDEBUG > 0
		 (char**)yyname;
#else
		 0;
#endif
int yyerror(const char*);
/*int main(int argc, char *argv[]) {
	extern YYSTYPE yylval;
	int tk;
	while ((tk = yylex()))
		if (tk > YYERRCODE)
			printf("%d:\t%s\n", tk, yyname[tk]);
		else
			printf("%d:\t%c\n", tk, tk);
	return 0;
}*/
