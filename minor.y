%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "node.h"
#include "tabid.h"
extern int yylex();

#define INFO_ARRAY 0
#define INFO_INT 1 
#define INFO_STR 2
#define INFO_VOID 3
#define INFO_CHAR_LIT 4
#define INFO_CONST_ARRAY 10
#define INFO_CONST_INT 11
#define INFO_CONST_STR 12
#define INFO_FUNC_ARRAY 20
#define INFO_FUNC_INT 21
#define INFO_FUNC_STR 22
#define INFO_FUNC_VOID 23
char errmsg[80];
static int pos = 0;
int dim(long type);
int verifyAssign(Node* lvNode, Node* valNode);
int intOnly(Node* n);
int intExpr(Node* n1, Node* n2);
int strIntExpr(Node* n1, Node*n2);
int intArrayExpr(Node* n1, Node* n2);
int strIntExpr(Node* n1, Node* n2);
int verifyArgs(char* ident, Node* argsNode);
int strOrArrayIndex(Node* lvNode, Node* exprNode);
char* getArrayName(Node* node); 
void formatedErrorMsg(char* format, char* msg);
int castCharToInt(Node* n); 
%}

%union {
    int i;	/* integer value */
    char* s;    /* string value*/
    Node* n;	/* Node pointer */
};

%token <i> INTEGER CHAR
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

%type <n> file program module decl opt_initializer const_initializer decls
%type <n> function public var params args intOrChar vars lvalue type body array_init
%type <n> stmt return stmts elifs string str_init str_continuation
%type <n> str_symbol expr array_assign number_assign string_assign array_size end

%%

start	: file		{  if(!yynerrs) printNode($1, 0, (char**)yyname); }
      	;

file	: program 	{ $$ = uniNode('f', $1); }
	| module 	{ $$ = uniNode('f', $1); } 
      	;

program	: PROGRAM decls START body END	{ $$ = binNode(PROGRAM, $2, $4); }
	;

module	: MODULE decls END { $$ = uniNode(MODULE, $2); }
	;

decl	: function	{ $$ = $1; }
	| public opt_initializer { $$ = binNode(ASSIGN, $1, $2); } 
	| FORWARD CONST var { $$ = uniNode(CONST, $3); int type = LEFT_CHILD($3)->value.i+10; IDnew(type, RIGHT_CHILD($3)->value.s, pos); pos += dim(LEFT_CHILD($3)->value.i); }
	| FORWARD var	{ $$ = uniNode(FORWARD, $2); int type = LEFT_CHILD($2)->value.i; IDnew(type, RIGHT_CHILD($2)->value.s, pos); pos += dim(LEFT_CHILD($2)->value.i); }
	| public const_initializer { binNode(CONST, $1, $2); }	
	;

decls 	:		{ $$ = nilNode(NIL); }
      	| decl 		{ $$ = uniNode(';', $1); }
       	| decls ';' decl { $$ = binNode(';', $1, $3); }
	;

opt_initializer
	: ARRAY IDENT array_size array_assign { $$ = binNode(ASSIGN, strNode(IDENT, $2), binNode('[', $3, $4)); IDnew(INFO_ARRAY, $2, pos); }
        | NUMBER IDENT number_assign { $$ = binNode(ASSIGN, strNode(IDENT, $2), $3); IDnew(INFO_INT, $2, pos); }
        | STRING IDENT string_assign { $$ = binNode(ASSIGN, strNode(IDENT, $2), $3); IDnew(INFO_STR, $2, pos); }
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
	| ASSIGN intOrChar { $$ = $2; }
	;

string_assign
	:		{ $$ = nilNode(NIL); }
	| ASSIGN string	{ $$ = uniNode(ASSIGN, $2); }
	; 

const_initializer
	: CONST ARRAY IDENT ASSIGN array_size array_init { $$ = binNode(ASSIGN, strNode(IDENT, $3), binNode('[', $5, uniNode(ARRAY, $6)));  int type = INFO_CONST_ARRAY; IDnew(type, $3, pos); pos += dim(type); }
 	| CONST NUMBER IDENT ASSIGN intOrChar { $$ = binNode(ASSIGN, strNode(IDENT, $3), $5); int type = INFO_CONST_INT; IDnew(type, $3, pos); pos += dim(type); }
	| CONST STRING IDENT ASSIGN string { $$ = binNode(ASSIGN, strNode(IDENT, $3), uniNode(STRING, $5)); int type = INFO_CONST_STR; IDnew(type, $3, pos); pos += dim(type); } 
	;

function: FUNCTION FORWARD type IDENT { IDnew($3->value.i+20, $4, 0); IDpush(); pos = 0; } params DONE { pos = 0; $$ = uniNode(FUNCTION, binNode(';', binNode(';', nilNode(FORWARD), $3), binNode('(', strNode(IDENT, $4), $6))); IDpop(); }
       	| FUNCTION public type IDENT { IDnew($3->value.i+20, $4, 0); IDpush(); pos = 0; } params DO { pos = 0; } body return { $$ = binNode(FUNCTION, binNode(';', binNode(';', $2, $3), binNode('(', strNode(IDENT, $4), $6)), binNode('{', $8 ,$9)); IDpop(); }
	;

var	: NUMBER IDENT	{ $$ = binNode(NUMBER, intNode('t', INFO_INT), strNode(NUMBER, $2)); }
    	| STRING IDENT	{ $$ =  binNode(STRING, intNode('t', INFO_STR), strNode(STRING, $2)); }
	| ARRAY IDENT	{ $$ =  binNode(ARRAY, intNode('t', INFO_ARRAY), strNode(ARRAY, $2)); }
    	| ARRAY IDENT '[' INTEGER ']' { $$ = binNode(ARRAY, intNode('t', INFO_ARRAY), binNode('[', intNode(INTEGER, $4), strNode(STRING, $2))); }
	;

params	:		{ $$ = nilNode(NIL); }
	| var		{ $$ = uniNode(';', $1); int type = LEFT_CHILD($1)->value.i; IDnew(type, getArrayName(RIGHT_CHILD($1)), pos); pos += dim(LEFT_CHILD($1)->value.i);  }
     	| params ';' var { $$ = binNode(';', $1, $3); 
				int type = LEFT_CHILD($3)->value.i; 
				IDnew(type, getArrayName(RIGHT_CHILD($3)), pos); 
				pos += dim(LEFT_CHILD($3)->value.i); 
			}
	;

args	: expr		{ $$ = uniNode(',', $1); }
     	| args ',' expr	{ $$ = binNode(',', $1, $3); }
	;

vars 	: var ';'	{ $$ = uniNode(';', $1); int type = LEFT_CHILD($1)->value.i; IDnew(type, getArrayName(RIGHT_CHILD($1)), pos); pos += dim(LEFT_CHILD($1)->value.i);  }
	| vars var ';'	{ $$ = binNode(';', $1, $2); 
				int type = LEFT_CHILD($2)->value.i;
				IDnew(type, getArrayName(RIGHT_CHILD($2)), pos); 
				pos += dim(LEFT_CHILD($2)->value.i); }
	;

lvalue	: IDENT		{ $$ = strNode(IDENT, $1); $$->info = IDfind($1,(void**)&pos); }
       	| lvalue '[' expr ']' { $$ = binNode(IDENT, $1, $3); $$->info = strOrArrayIndex($1, $3); }
	; 

type	: NUMBER	{ $$ = intNode(NUMBER, INFO_INT); } 
	| STRING	{ $$ = intNode(STRING, INFO_STR); }
	| ARRAY 	{ $$ = intNode(ARRAY, INFO_ARRAY); } 
	| VOID		{ $$ = intNode(VOID, INFO_VOID); }
	;

public  : 		{ $$ = nilNode(NIL); }
	| PUBLIC 	{ $$ = nilNode(PUBLIC); }
	;

body	: vars stmts 	{ $$ = binNode('{', $1, $2); }
	| stmts     	{ $$ = uniNode('{', $1); }  
	;
				
array_init
	: intOrChar	{ $$ = $1; }
	| array_init ',' intOrChar { $$ = binNode(',', $1, $3); }
	;

intOrChar
	: INTEGER	{ $$ = intNode(NUMBER, $1); }
	| CHAR		{ $$ = intNode(NUMBER, $1); }
	;

stmt  : IF expr THEN stmts end elifs FI { $$ = quadNode(IF, $2, $4, $5, uniNode(ELIF, $6)); }
	| IF expr THEN stmts end elifs ELSE stmts end FI { $$ = quadNode(ELSE, triNode(IF, $2, $4, $5), uniNode(ELIF, $6), $8, $9); }  
	| FOR expr UNTIL expr STEP expr DO stmts end DONE { $$ = binNode(FOR, triNode(',', $2, $4, $6), binNode('{', $8, $9)); } 
	| expr ';'	{ $$ = $1; }
	| expr '!'	{ $$ = uniNode('!', $1); }
	| lvalue ALOC  expr ';' { $$ = binNode('#', $1, $3); }
	| error ';'
	;

end	: return	{ $$ = $1; }
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
	: intOrChar	{ $$ = $1; }
	| TEXTSTRING	{ $$ = strNode(STRING, $1); }
	;

expr	: lvalue 	{ $$ = uniNode('*', $1); $$->info = $1->info; }
     	| lvalue ASSIGN expr { $$ = binNode(ASSIGN, $1, $3); $$->info = verifyAssign($1, $3); }
	| INTEGER 	{ $$ = intNode(INTEGER, $1); $$->info = INFO_INT; }
	| string 	{ $$ = $1; $$->info = INFO_STR; }
	| CHAR	 	{ $$ = intNode(CHAR, $1); $$->info = INFO_CHAR_LIT; }
	| '-' expr %prec UMINUS { $$ = uniNode('-', $2); $$->info = intOnly($2);}
	| '?'	 	{ $$ = nilNode('?'); $$->info = INFO_INT; }
	| '&' lvalue %prec UMINUS { $$ = uniNode('&', $2); $$->info = intOnly($2); }
	| '~' expr	{ $$ = uniNode('~', $2); $$->info = intOnly($2);}
	| expr '+' expr	{ $$ = binNode('+', $1, $3); $$->info = intArrayExpr($1, $3); }
	| expr '-' expr	{ $$ = binNode('-', $1, $3);  $$->info = intArrayExpr($1, $3);}
	| expr '*' expr	{ $$ = binNode('*', $1, $3);  $$->info = intExpr($1, $3);}
	| expr POW expr	{ $$ = binNode('^', $1, $3);  $$->info = intExpr($1, $3);}
	| expr '/' expr	{ $$ = binNode('/', $1, $3);  $$->info = intExpr($1, $3);}
	| expr '%' expr	{ $$ = binNode('%', $1, $3);  $$->info = intExpr($1, $3);}
	| expr '<' expr	{ $$ = binNode('<', $1, $3); $$->info = strIntExpr($1, $3); }
	| expr '>' expr	{ $$ = binNode('>', $1, $3); $$->info = strIntExpr($1, $3);}
	| expr GE expr	{ $$ = binNode(GE, $1, $3); $$->info = strIntExpr($1, $3);}
	| expr LE expr	{ $$ = binNode(LE, $1, $3); $$->info = strIntExpr($1, $3);}
	| expr NE expr	{ $$ = binNode(NE, $1, $3); $$->info = strIntExpr($1, $3);}
	| expr '=' expr	{ $$ = binNode('=', $1, $3); $$->info = strIntExpr($1, $3);}
	| expr '&' expr	{ $$ = binNode('&', $1, $3); $$->info = intExpr($1, $3);}
	| expr '|' expr { $$ = binNode('|', $1, $3); $$->info = intExpr($1, $3);}
	| '(' expr ')' 	{ $$ = $2; $$->info = $2->info; }
	| IDENT '(' args ')' { $$ = binNode('(', strNode(IDENT, $1), $3); $$->info = verifyArgs($1, $3); }
	;

%%
char **yynames =
#if YYDEBUG > 0
		 (char**)yyname;
#else
		 0;
#endif
int yyerror(const char*);

int dim(long type) {
	return 4; // TODO
}

int verifyAssign(Node* lvNode, Node* valNode) {
	valNode->info = castCharToInt(valNode);
	int lvInfo = lvNode->info,
	valInfo = valNode->info;
	/* lvalue is a constant  */
	printf("==== %d, %d  =====\n",lvInfo, valInfo);
	if (lvInfo >= INFO_CONST_ARRAY && lvInfo < 20)
		yyerror("can't make assignment to constant");
	/* type := type or type := const_type or any := 0 or string[index] := char */
	if (!(lvInfo == valInfo || lvInfo + 10 == valInfo || valNode->value.i == 0
		|| (lvInfo%10 == INFO_STR && valInfo == INFO_CHAR_LIT))) 
		yyerror("invalid assignment");
	return lvInfo;
}

int intOnly(Node* n) {
	n->info = castCharToInt(n);
	if(n->info%10 != INFO_INT)
		yyerror("int only");
	return INFO_INT;
}

int intExpr(Node* n1, Node* n2) {
	n1->info = castCharToInt(n1);
	n2->info = castCharToInt(n2);
	if(n1->info != INFO_INT || n2->info != INFO_INT)
		 yyerror("int only expression");
	return INFO_INT;
}

int intArrayExpr(Node* n1, Node* n2) {
	printf("OPERATORs -/+: %d , %d\n ",n1->info, n2->info);
	n1->info = castCharToInt(n1);
	n2->info = castCharToInt(n2);
	if(!(n1->info%10 == INFO_INT || n2->info%10 == INFO_INT
	||n1->info%10 == INFO_ARRAY && n2->info%10 == INFO_ARRAY))
		yyerror("expression only allows int type or pointer arithmetic");
	if(n1->info == n2->info)
		return INFO_INT;
	return INFO_ARRAY; 
}

int strIntExpr(Node* n1, Node*n2) {
	n1->info = castCharToInt(n1);
	n2->info = castCharToInt(n2);
	if(n1->info%10 != n2->info%10)
		yyerror("different types");
	if(n1->info%10 != INFO_INT && n1->info%10 != INFO_STR)
		yyerror("operator only allow string and int types");
	return INFO_INT;
}

int verifyArgs(char* name, Node* argsNode){
	// Add args verifications
	int* defArgs;
	int type = IDfind(name, (void**)&defArgs);
/*	
	if (typ < 20) {
		return 0;
	} else if (defArgs[0] != 0 && argsNode[0] == 0) {		
		char* format = "\"%s\" function requires arguments";
		formatedErrorMsg(format, name);		
	} else if (defArgs[0] == 0 && argsNode[0] != 0) {
		char* format = "\"%s\" function doesn't have arguments";
		formatedErrorMsg(format, name);		
	} else {
		// check type
	}	
*/	
	return type-20;
}

int strOrArrayIndex(Node* lvNode, Node* exprNode) {
	long pos = 0;
	int type = IDfind(lvNode->value.s, (void**)&pos);
	
	if(type < 0) yyerror("couldn't find identifier");

	if(type != INFO_ARRAY && type != INFO_CONST_ARRAY
		&& type != INFO_STR && type != INFO_CONST_STR)
		yyerror("the left value needs to be an array or a string");
	
	if(exprNode->info%10 != INFO_INT)
		yyerror("index value must be a number");	

	// if(type == INFO_ARRAY || type == INFO_CONST_ARRAY)
		return INFO_INT;	
	
	//return INFO_STR;
}

char* getArrayName(Node* node) {
	char* name;
	// array[size] and array declarations have a different structure
	if(node->attrib == '[')
		name = RIGHT_CHILD(node)->value.s;
	else
		name = node->value.s;
	return name;
}

int castCharToInt(Node* n) {
	return n->info == INFO_CHAR_LIT ? INFO_INT : n->info;
}

void formatedErrorMsg(char* format, char* msg) {
	sprintf(errmsg, format, msg);
	yyerror(errmsg);
}

