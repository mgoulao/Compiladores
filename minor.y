%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "node.h"
#include "tabid.h"
#ifndef YYERRCODE
#define YYERRCODE 256
#endif
#define YYDEBUG 1
extern int yylex();
%}

%union {
    int i;	/* integer value */
    char* s;    /* string value*/
    Node* n	/* Node pointer */
};

%token <i> INTEGER CHAR
%token <s> IDENT STRING
%token MODULE PROGRAM START END VOID CONST NUMBER ARRAY STRING TEXTSTRING CHAR FUNCTION
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
	| FORWARD var	{ $$ = uniNode(FORWARD, $1); }
	| public CONST initializer { binNode(CONST, $1, $3); }	
	;

opt_initializer
	:		{ $$ = nilNode(NIL); }
	| initializer	{ $$ = $1; }
	;

initializer
	: ARRAY IDENT ASSIGN array_init { $$ = binNode(ASSIGN, $2, uniNode(array_init)); }
 	| NUMBER IDENT ASSIGN INTEGER { $$ = binNode(ASSING, $2, intNode(NUM, $4)); }
	| STRING IDENT ASSIGN string { $$ = binNode(ASSIGN, $2, strNode(STR, $4)); } 

decls 	:
      	| decl 		{ $$ = uniNode(';', $1); }
       	| decls ';' decl { $$ = binNode(';', $1, $3); }
	;

function: FUNCTION FORWARD type IDENT params DONE { $$ = quadNode(';', uniNode(FORWARD), $3, $4, $5); }
       	| FUNCTION public type IDENT params DO body return { $$ = triNode(FUNCTION, quadNode(';', $2, $3, $4, $5), $7 ,$8); }
	;

var	: NUMBER IDENT	{ $$ = strNode(NUMBER, $2); }
    	| STRING IDENT	{ $$ = strNode(STRING, $2); }
	| ARRAY IDENT	{ $$ = strNode(ARRAY, $2); }
    	| ARRAY IDENT '[' INTEGER ']' { $$ = binNode(ARRAY, $2, $4); }
	;

params	:
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

body	: vars stmts 	{ $$ = binNode(BODY, $1, $2); }
	| stmts     	{ $$ = uniNode(BODY_STMTS, $1); }  
	;
				
literal	: INTEGER	{ $$ = intNode(INT, $1); }
	| CHAR		{ $$ = intNode(CHAR, (int)$1); }
	;

array_init
	: INTEGER	{ $$ = intNode(INT, $1); }
	| array_init ',' INTEGER { $$ = binNode(ARRAY_INIT, $1, $3); }
	;

literals: literal	{ $$ = uniNode('.', $1); }
	| string	{ $$ = strNode(STRING, $1); }
	;

stmt  : IF expr THEN stmts return elifs FI { $$ = quadNode(IF, $2, $3, $4, uniNode(ELIFS, $5)); }
	| IF expr THEN stmts return elifs ELSE stmts return FI { $$ = quadNode(ELSE, triNode(IF, $2, $4, $5), uniNode(ELIFS, $6), $8, $9); }  
	| FOR expr UNTIL expr STEP expr DO stmts return DONE { $$ = binNode(FOR, triNode(FOR_PARAMS, $2, $4, $6), binNode(FOR_BODY, $8, $9)); } 
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

elifs	:
	| ELIF THEN stmts return elifs { $$ = triNode(ELIF, binNode(ELIF, $3, $4), $5); }
	;

string	: TEXTSTRING 	{ $$ = strNode(STR, $1); }
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
int yyerror(const char*);
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
