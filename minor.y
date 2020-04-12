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

%token <i> INTEGER
%token <s> IDENT TEXTSTRING
%token MODULE PROGRAM START END VOID CONST NUMBER ARRAY STRING FUNCTION ALOC 
%token PUBLIC FORWARD IF THEN ELSE ELIF FI FOR UNTIL STEP DO DONE REPEAT STOP RETURN NIL

%nonassoc IF
%nonassoc ELSE
%left ELIF
%right ASSIGN
%left GE LE '=' NE '<' '>'
%nonassoc '~' '&' '?'
%left '|'
%left '+' '-'
%left '*' '/' '%'
%right POW
%nonassoc UMINUS
%nonassoc '[' '('

%type <n> program module decl opt_initializer initializer decls
%type <n> function public var params args vars lvalue type body literal array_init
%type <n> literals  stmt return stmts elifs string str_init str_continuation
%type <n> str_symbol expr array_assign number_assign string_assign array_size end

%%


file	: program 	{ printNode($1, 0, (char**)yyname);}
	| module 	{ printNode($1, 0, (char**)yyname);} 
      	;

program	: PROGRAM decls START body END	{ $$ = binNode(PROGRAM, $2, $4); }
	;

module	: MODULE decls END { $$ = uniNode(MODULE, $2); }
	;

decl	: function { $$ = $1; }
	| public opt_initializer { $$ = binNode(ASSIGN, $1, $2); } 
	| FORWARD CONST var { $$ = uniNode(CONST, $3); }
	| FORWARD var	{ $$ = uniNode(FORWARD, $2); }
	| public CONST initializer { binNode(CONST, $1, $3); }	
	;

opt_initializer
	: ARRAY IDENT array_size array_assign { $$ = triNode(ASSIGN, strNode(IDENT, $2), $3, $4); }
        | NUMBER IDENT number_assign { $$ = binNode(ASSIGN, strNode(IDENT, $2), $3); }
        | STRING IDENT string_assign { $$ = binNode(ASSIGN, strNode(IDENT, $2), $3); }
	;

array_size
	:		{ $$ = nilNode(NIL); }
	| '[' INTEGER ']' { $$ = intNode('[', $2); }
	;

array_assign
	:		{ $$ = nilNode(NIL); }
	| ASSIGN array_init { $$ = uniNode(ASSIGN, $2); } 
	;

number_assign
	:		{ $$ = nilNode(NIL); }
	| ASSIGN INTEGER { $$ = intNode(ASSIGN, $2); }
	;

string_assign
	:		{ $$ = nilNode(NIL); }
	| ASSIGN string	{ $$ = uniNode(ASSIGN, $2); }
	; 

initializer
	: ARRAY IDENT ASSIGN array_size array_init { $$ = triNode(ASSIGN, strNode(IDENT, $2), $4, uniNode(ARRAY, $5)); }
 	| NUMBER IDENT ASSIGN INTEGER { $$ = binNode(ASSIGN, strNode(IDENT, $2), intNode(NUMBER, $4)); }
	| STRING IDENT ASSIGN string { $$ = binNode(ASSIGN, strNode(IDENT, $2), uniNode(STRING, $4)); } 

decls 	:		{ $$ = nilNode(NIL); }
      	| decl 		{ $$ = uniNode(';', $1); }
       	| decls ';' decl { $$ = binNode(';', $1, $3); }
	;

function: FUNCTION FORWARD type IDENT params DONE { $$ = quadNode(';', nilNode(FORWARD), $3, strNode(IDENT, $4), $5); }
       	| FUNCTION public type IDENT params DO body return { $$ = triNode(FUNCTION, quadNode(';', $2, $3, strNode(IDENT, $4), $5), $7 ,$8); }
	;

var	: NUMBER IDENT	{ $$ = strNode(NUMBER, $2); }
    	| STRING IDENT	{ $$ = strNode(STRING, $2); }
	| ARRAY IDENT	{ $$ = strNode(ARRAY, $2); }
    	| ARRAY IDENT '[' INTEGER ']' { $$ = binNode(ARRAY, strNode(STRING, $2), intNode(INTEGER, $4)); }
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
       	| IDENT '[' expr ']' { $$ = binNode(IDENT, strNode(IDENT, $1), $3); }
	| '*' lvalue	{ $$ = uniNode('*', $2); }
	; 

type	: NUMBER	{ $$ = nilNode(NUMBER); } 
	| STRING	{ $$ = nilNode(STRING); }
	| ARRAY 	{ $$ = nilNode(ARRAY); } 
	| VOID		{ $$ = nilNode(VOID); }
	;

public  : 		{ $$ = nilNode(NIL); }
	| PUBLIC 	{ $$ = nilNode(PUBLIC); }
	;

body	: vars stmts 	{ $$ = binNode('{', $1, $2); }
	| stmts     	{ $$ = uniNode('{', $1); }  
	;
				
literal	: INTEGER	{ $$ = intNode(NUMBER, $1); }
	;

array_init
	: INTEGER	{ $$ = intNode(NUMBER, $1); }
	| array_init ',' INTEGER { $$ = binNode(',', $1, intNode(INTEGER, $3)); }
	;

literals: literal	{ $$ = uniNode('.', $1); }
	| string	{ $$ = uniNode(STRING, $1); }
	;

stmt  : IF expr THEN stmts end elifs FI { $$ = quadNode(IF, $2, $4, $5, uniNode(ELIF, $6)); }
	| IF expr THEN stmts end elifs ELSE stmts end FI { $$ = quadNode(ELSE, triNode(IF, $2, $4, $5), uniNode(ELIF, $6), $8, $9); }  
	| FOR expr UNTIL expr STEP expr DO stmts end DONE { $$ = binNode(FOR, triNode(',', $2, $4, $6), binNode('{', $8, $9)); } 
	| expr ';'	{ $$ = $1; }
	| expr '!'	{ $$ = uniNode('!', $1); }
	| lvalue ALOC  expr ';' { $$ = binNode('#', $1, $3); }
	| error ';'
	;

end	:		{ $$ = nilNode('}'); }
        | RETURN        { $$ = nilNode(RETURN); }
        | RETURN expr   { $$ = uniNode(RETURN, $2); }
	| REPEAT	{ $$ = nilNode(REPEAT); }
	| STOP		{ $$ = nilNode(STOP); }
	;

return	:		{ $$ = nilNode(RETURN); }
      	| RETURN	{ $$ = nilNode(RETURN); }
       	| RETURN expr	{ $$ = uniNode(RETURN, $2); }
	;

stmts
	:		{ $$ = nilNode(NIL); }
	| stmts stmt	{ $$ = binNode('.', $1, $2); }
	;

elifs	:		{ $$ = nilNode(NIL); }
	| ELIF expr THEN stmts end elifs { $$ = triNode(ELIF, $2, binNode(ELIF, $4, $5), $6); }
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
	: INTEGER	{ $$ = intNode(INTEGER, $1); }
	| TEXTSTRING	{ $$ = strNode(STRING, $1); }
	;

expr	: lvalue 	{ $$ = uniNode('*', $1); }
     	| lvalue ASSIGN expr { $$ = binNode(ASSIGN, $1, $3); }
	| INTEGER 	{ $$ = intNode(INTEGER, $1); }
	| string 	{ $$ = $1; }
	| '-' expr %prec UMINUS { $$ = uniNode('-', $2); }
	| '?'	 	{ $$ = nilNode('?'); }
	| '&' lvalue %prec UMINUS { $$ = uniNode('&', $2); }
	| expr '+' expr	{ $$ = binNode('+', $1, $3); }
	| expr '-' expr	{ $$ = binNode('-', $1, $3); }
	| expr '*' expr	{ $$ = binNode('*', $1, $3); }
	| expr POW expr	{ $$ = binNode('^', $1, $3); }
	| expr '/' expr	{ $$ = binNode('/', $1, $3); }
	| expr '%' expr	{ $$ = binNode('%', $1, $3); }
	| expr '<' expr	{ $$ = binNode('<', $1, $3); }
	| expr '>' expr	{ $$ = binNode('>', $1, $3); }
	| expr GE expr	{ $$ = binNode(GE, $1, $3); }
	| expr LE expr	{ $$ = binNode(LE, $1, $3); }
	| expr NE expr	{ $$ = binNode(NE, $1, $3); }
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
