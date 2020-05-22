%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "node.h"
#include "tabid.h"
#include "postfix.h"
#include "minor.h"

extern int yylex();


char errmsg[80];
static long* funcArgs;
static int loopLvl = 0, funcLvl = 0, currRetType = 1; 
static long fPos = 0;

void publicVariable(char* name, Node* public, int isConst, Node* vectorSize, Node* init);
void forwardVariable(char* name, int isConst, Node* vector, Node* init);
void variable(char* name, int isConst, Node* vectorSize, Node* init);
void forwardFunction(char* name);
void burgFunction(char* name, int enter, Node* public, Node* type, Node* stmt);
void mainFunction(Node* body, int enter);
void externs();
void evaluate();


int dim(long type);
int verifyAssign(Node* lvNode, Node* valNode);
int intOnly(Node* n);
int intExpr(Node* n1, Node* n2);
int intArrayExpr(Node* n1, Node* n2);
int strIntExpr(Node* n1, Node* n2);
void printExpr(Node* n);
int verifyArgs(char* ident, Node* argsNode);
int verifyLVName(char* name);
int strOrArrayIndex(Node* lvNode, Node* exprNode);
char* getArrayName(Node* node); 
void formatedErrorMsg(char* format, char* msg);
int castCharToInt(Node* n); 
void nonVoidExpr(Node* n);
void alocExpr(Node* n1, Node* n2);
void enterFunction(long type, char* name); 
void function(char* name, int forward, Node* public,Node* type, Node* body);
void defineFunction(long type, char* name);
%}

%union {
    int i;	/* integer value */
    char* s;    /* string value*/
    Node* n;	/* Node pointer */
};

%token <i> INTEGER CHAR
%token <s> IDENT TEXTSTRING
%token MODULE PROGRAM START END VOID CONST NUMBER ARRAY STRING FUNCTION ALOC 
%token PUBLIC FORWARD IF THEN ELSE ELIF FI FOR UNTIL STEP DO DONE REPEAT STOP RETURN 

// AST Tokens
%token START_FILE BODY TYPE STMTS NIL STRING_ELEM PARAMS PARAM DECLS ARGS VAR VARS INTS INDEX FOR_EXPRS IF_ELIFS INT_TYPE STR_TYPE ARR_TYPE NEG PRINT ADDR FBODY LOCAL ELIFS FOR_START FOR_COND FOR_END FUNC_PARAMS NO_ARG_CALL

%nonassoc IF
%nonassoc ELSE
%left ELIF
%right ASSIGN
%left '|'
%left '&'
%nonassoc '~' '?'
%left '=' NE
%left GE LE '<' '>'
%left '+' '-'
%left '*' '/' '%'
%right POW
%nonassoc UMINUS
%nonassoc '[' '('

%type <n> file program module decl opt_initializer const_initializer decls
%type <n> function public var params args intOrChar vars lvalue type body array_init
%type <n> stmt return end stmts elifs string str_init str_continuation
%type <n> str_symbol expr array_assign number_assign string_assign array_size

%%

start	: file		{ if(!yynerrs){externs(); } }
      	;

file	: program 	{ $$ = uniNode(START_FILE, $1); }
	| module 	{ $$ = uniNode(START_FILE, $1); } 
      	;

program	: PROGRAM decls START { IDpush(); currRetType = INFO_INT; fPos = 0; } body END { $$ = binNode(PROGRAM, $2, $5); mainFunction($5, -fPos);  IDpop(); }
	;

module	: MODULE decls END { $$ = uniNode(MODULE, $2); }
	;

decl	: function
	| public opt_initializer { $$ = binNode(ASSIGN, $1, $2); publicVariable(LEFT_CHILD($2)->value.s, $1, 0, nilNode(NIL), $2); } 
	| FORWARD CONST var { $$ = uniNode(CONST, $3); int type = LEFT_CHILD($3)->value.i+10; char* name = RIGHT_CHILD($3)->value.s; IDnew(type, name, 0); forwardVariable(name, 1, nilNode(NIL), nilNode(NIL)); }
	| FORWARD var	{ $$ = uniNode(FORWARD, $2); int type = LEFT_CHILD($2)->value.i; char* name = RIGHT_CHILD($2)->value.s;IDnew(type, name, 0); forwardVariable(name, 0, nilNode(NIL), nilNode(NIL)); }
	| public const_initializer { binNode(CONST, $1, $2); publicVariable(LEFT_CHILD($2)->value.s, $1, 1, nilNode(NIL), $2);  }	
	;

decls 	:		{ $$ = nilNode(NIL); }
      	| decl
       	| decls ';' decl { $$ = binNode(DECLS, $1, $3); }
	;

opt_initializer
	: ARRAY IDENT array_size array_assign { $$ = binNode(ASSIGN, strNode(IDENT, $2), binNode('[', $3, $4)); IDnew(INFO_ARRAY, $2, 0); arrayAssign($3, $4); }
        | NUMBER IDENT number_assign { $$ = binNode(ASSIGN, strNode(IDENT, $2), $3); IDnew(INFO_INT, $2, 0); }
        | STRING IDENT string_assign { $$ = binNode(ASSIGN, strNode(IDENT, $2), $3); IDnew(INFO_STR, $2, 0); }
	;

array_size
	:		{ $$ = nilNode(NIL); }
	| '[' INTEGER ']' { $$ = intNode('[', $2);  if($2 <= 0) yyerror("invalid array dimension");}
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
	: CONST ARRAY IDENT ASSIGN array_size array_init { $$ = binNode(ASSIGN, strNode(IDENT, $3), binNode('[', $5, uniNode(ARRAY, $6)));  int type = INFO_CONST_ARRAY; IDnew(type, $3, 0); }
 	| CONST NUMBER IDENT ASSIGN intOrChar { $$ = binNode(ASSIGN, strNode(IDENT, $3), $5); int type = INFO_CONST_INT; IDnew(type, $3, 0); }
	| CONST STRING IDENT ASSIGN string { $$ = binNode(ASSIGN, strNode(IDENT, $3), uniNode(STRING, $5)); int type = INFO_CONST_STR; IDnew(type, $3, 0); } 
	;

function: FUNCTION FORWARD type IDENT { enterFunction($3->value.i+40, $4); } params DONE { $$ = uniNode(FUNCTION, binNode(';', binNode(';', nilNode(FORWARD), $3), binNode('(', strNode(IDENT, $4), $6))); function($4, 1, nilNode(NIL), $3, nilNode(NIL)); }
       	| FUNCTION public type IDENT { enterFunction($3->value.i+20+$2->info, $4); currRetType = $3->value.i; fPos = pfWORD; } params { fPos = 0; } DO body return { $$ = binNode(FUNCTION, binNode(';', binNode(';', $2, $3), binNode('(', strNode(IDENT, $4), $6)), binNode(BODY, $9 ,$10)); function($4, 0, $2, $3, binNode(BODY, $9, $10));  }
	;

var	: NUMBER IDENT	{ $$ = binNode(INT_TYPE, intNode(TYPE, INFO_INT), strNode(IDENT, $2)); }
    	| STRING IDENT	{ $$ =  binNode(STR_TYPE, intNode(TYPE, INFO_STR), strNode(IDENT, $2)); }
	| ARRAY IDENT	{ $$ =  binNode(ARR_TYPE, intNode(TYPE, INFO_ARRAY), strNode(IDENT, $2)); }
    	| ARRAY IDENT '[' INTEGER ']' { $$ = binNode(ARRAY, intNode(TYPE, INFO_ARRAY), binNode('[', intNode(INTEGER, $4), strNode(IDENT, $2))); if($4 <= 0) yyerror("invalid array dimension"); }
	;

params	:		{ $$ = nilNode(NIL); }
	| var		{ $$ = binNode(PARAMS, nilNode(NIL), functionParams($1)); }
     	| params ';' var { $$ = binNode(PARAMS, $1, functionParams($3)); }
	;

args	: expr		{ $$ = binNode(ARGS, $1, nilNode(NIL)); }
     	| args ',' expr	{ $$ = binNode(ARGS, $3, $1); }
	;

vars 	: var ';'	{ $$ = uniNode(VAR, $1); int type = LEFT_CHILD($1)->value.i; fPos -= pfWORD; IDnew(type, getArrayName(RIGHT_CHILD($1)), (void*)fPos); }
	| vars var ';'	{ $$ = binNode(VARS, $1, $2); 
				int type = LEFT_CHILD($2)->value.i;
				fPos -= pfWORD; int pos = fPos;
				IDnew(type, getArrayName(RIGHT_CHILD($2)), (void*)fPos); 
	 }
	;

lvalue	: IDENT		{ $$ = variableName($1); $$->info = verifyLVName($1);  }
       	| lvalue '[' expr ']' { $$ = binNode(INDEX, $1, $3); $$->info = strOrArrayIndex($1, $3); }
	; 

type	: NUMBER	{ $$ = intNode(NUMBER, INFO_INT); } 
	| STRING	{ $$ = intNode(STRING, INFO_STR); }
	| ARRAY 	{ $$ = intNode(ARRAY, INFO_ARRAY); } 
	| VOID		{ $$ = intNode(VOID, INFO_VOID); }
	;

public  : 		{ $$ = nilNode(NIL); $$->info = 0;}
	| PUBLIC 	{ $$ = nilNode(PUBLIC); $$->info = 10; }
	;

body	: vars stmts 	{ $$ = binNode(BODY, $1, $2); }
	| stmts     	{ $$ = binNode(BODY, nilNode(NIL), $1); }  
	;
				
array_init
	: INTEGER	{ $$ = binNode(INTS, nilNode(NIL), intNode(INTEGER, $1)); }
	| '-' INTEGER	{ $$ = binNode(INTS, nilNode(NIL), intNode(INTEGER, -$2)); }
	| array_init ','  INTEGER { $$ = binNode(INTS, $1, intNode(INTEGER, $3)); }
	| array_init ',' '-' INTEGER { $$ = binNode(INTS, $1, intNode(INTEGER, -$4)); }
	;

intOrChar
	: INTEGER	{ $$ = intNode(NUMBER, $1); }
	| CHAR		{ $$ = intNode(NUMBER, $1); }
	;

stmt  : IF expr THEN stmts end elifs FI { $$ = binNode(IF_ELIFS, binNode(IF, $2, binNode(STMTS, $4, $5)), $6); nonVoidExpr($2); }
	| IF expr THEN stmts end elifs ELSE stmts end FI { $$ = binNode(ELSE, binNode(IF_ELIFS, binNode(IF, $2, binNode(STMTS, $4, $5)), $6), binNode(STMTS, $8, $9)); nonVoidExpr($2);}  
	| FOR expr UNTIL expr STEP expr DO {loopLvl++;} stmts end DONE { $$ = binNode(FOR, binNode(FOR_EXPRS, binNode(FOR_EXPRS, $2, nilNode(FOR_START)), binNode(FOR_EXPRS, $4, nilNode(FOR_COND))), binNode(FOR_END, binNode(STMTS, $9, $10), $6)); nonVoidExpr($2); nonVoidExpr($4);nonVoidExpr($6);} 
	| expr ';'
	| expr '!'	{ $$ = uniNode(PRINT, $1); printExpr($1); }
	| lvalue ALOC  expr ';' { $$ = binNode(ALOC, $3, $1); alocExpr($1, $3); }
	| error ';'
	;

end	: return
	| REPEAT	{ $$ = nilNode(REPEAT); if(!loopLvl) yyerror("invalid repeat statement"); }
	| STOP		{ $$ = nilNode(STOP); if(!loopLvl) yyerror("invalid stop statement");}
	;

return	:		{ $$ = nilNode(NIL); }
      	| RETURN	{ $$ = uniNode(RETURN, nilNode(NIL)); if(!IDlevel() || currRetType != INFO_VOID) yyerror("invalid return statement");}
       	| RETURN expr	{ $$ = uniNode(RETURN, $2); if(!IDlevel() || currRetType != $2->info%10 || currRetType%10 == INFO_VOID) yyerror("invalid return statement");}
	;

stmts
	:		{ $$ = nilNode(NIL); }
	| stmts stmt	{ $$ = binNode(STMTS, $1, $2); }
	;

elifs	:		{ $$ = nilNode(NIL); }
	|  ELIF expr THEN stmts end elifs { $$ = binNode(ELIFS, binNode(ELIF, $2, binNode(STMTS, $4, $5)), $6); nonVoidExpr($2);}
	;

string	: TEXTSTRING 	{ $$ = binNode(STRING, strNode(TEXTSTRING, $1), nilNode(NIL)); }
       	| str_init str_continuation { $$ = binNode(STRING, $1, $2); } 
	;

str_continuation
	:		{ $$ = nilNode(NIL); }
	| str_continuation str_symbol { $$ = binNode(STRING_ELEM, $1, $2); }
	; 

str_init: str_symbol str_symbol	{ $$ = binNode(STRING_ELEM, $1, $2); }
	;

str_symbol
	: intOrChar
	| TEXTSTRING	{ $$ = strNode(TEXTSTRING, $1); }
	;

expr	: lvalue 	{ $$ = $1; $$->info = $1->info; }
     	| lvalue ASSIGN expr { $$ = binNode(ASSIGN, $3, $1); $$->info = verifyAssign($1, $3); }
	| INTEGER 	{ $$ = intNode(INTEGER, $1); $$->info = INFO_INT; }
	| string 	{ $$->info = INFO_STR; }
	| CHAR	 	{ $$ = intNode(CHAR, $1); $$->info = INFO_CHAR_LIT; }
	| '-' expr %prec UMINUS { $$ = uniNode(NEG, $2); $$->info = intOnly($2);}
	| '?'	 	{ $$ = nilNode('?'); $$->info = INFO_INT; }
	| '&' lvalue %prec UMINUS { $$ = uniNode(ADDR, $2); $$->info = addrExpr($2); }
	| '~' expr	{ $$ = uniNode('~', $2); $$->info = intOnly($2);}
	| expr '+' expr	{ $$ = binNode('+', $1, $3); $$->info = intArrayPointerExpr($1, $3); }
	| expr '-' expr	{ $$ = binNode('-', $1, $3);  $$->info = intArrayExpr($1, $3);}
	| expr '*' expr	{ $$ = binNode('*', $1, $3);  $$->info = intExpr($1, $3);}
	| expr POW expr	{ $$ = binNode(POW, $3, $1);  $$->info = intExpr($1, $3);}
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

void enterFunction(long type, char* name) {
	funcArgs = malloc(50);
	funcArgs[0] = 0;
	int typeTest = IDfind(name, (void*)IDtest);
	// If function is FORWARD or new
	if(typeTest == -1) {
		IDnew(type, name, funcArgs);
		if(type != 3) fPos = 2*pfWORD;
	}
	else if (typeTest > 40 && typeTest-20 == type) {
		// wait for parameters to verify
	} else yyerror("name already used");

	IDpush(); 
}

void function(char* name, int forward, Node* public,Node* type, Node* body) {
	if(forward == 0) {
		defineFunction(type->value.i+20+public->info, name);
		burgFunction(name, -fPos, public, type, body); 
	} else {
		forwardFunction(name);
	}
	IDpop();
}

void defineFunction(long type, char* name) {
	long* args;
	int typeTest = IDfind(name, (void**)&args);
	if (typeTest > 40 && typeTest-20 == type){
		int err = 0;
		for(int i = 1; i <= funcArgs[0] && i <= args[0]; i++) {
			if (funcArgs[i] != args[i]) err++;
		}	
		if (err || funcArgs[0] != args[0])
			yyerror("invalid function definition");
		else {
			// define a FORWARD function makes it PUBLIC
			free(funcArgs);
			IDreplace(type-10, name, args);
		}
		
	}
}

Node* functionParams(Node* n) {
	int type = LEFT_CHILD(n)->value.i;
	fPos += pfWORD;
	IDnew(type, getArrayName(RIGHT_CHILD(n)), (void*)fPos); 
	funcArgs[++funcArgs[0]] = type;
	return intNode(PARAM, fPos);
}

Node* variableName(char* name) {
	Node* node = strNode(IDENT, name);
	long pos = 0;
	int type = IDfind(name, (void*)&pos);
	if(pos != 0 && type < INFO_FUNC_ARRAY) {
		node = intNode(LOCAL, pos);
	} else if (type >= INFO_FUNC_ARRAY) {
		node = strNode(NO_ARG_CALL, name);
	}
	return node;
}

int verifyAssign(Node* lvNode, Node* valNode) {
	valNode->info = castCharToInt(valNode);
	int lvInfo = lvNode->info,
	valInfo = valNode->info;
	/* lvalue is a constant  */
	if (lvInfo >= INFO_CONST_ARRAY && lvInfo < 20)
		yyerror("can't make assignment to constant");
	/* type := type or type := const_type or string[index] := char  special zero */
	if (!(lvInfo == valInfo || lvInfo + 10 == valInfo
		|| (valNode->value.i == 0 && valNode->attrib == INTEGER)
		|| (lvInfo%10 == INFO_STR && valInfo == INFO_CHAR_LIT))) 
		yyerror("invalid assignment");
	return lvInfo;
}

int intOnly(Node* n) {
	n->info = castCharToInt(n);
	if(n->info%10 != INFO_INT)
		yyerror("int only expression");
	return INFO_INT;
}

int addrExpr(Node* n) {
	if(n->info%10 != INFO_INT)
		yyerror("can not take '&' of non-number");
	return INFO_ARRAY;
}

int intExpr(Node* n1, Node* n2) {
	n1->info = castCharToInt(n1);
	n2->info = castCharToInt(n2);
	if(n1->info != INFO_INT || n2->info != INFO_INT)
		 yyerror("int only expression");
	return INFO_INT;
}

int intArrayExpr(Node* n1, Node* n2) {
	n1->info = castCharToInt(n1);
	n2->info = castCharToInt(n2);
	if(!(n1->info%10 >= INFO_ARRAY && n1->info%10 <= INFO_INT
	&& n2->info%10 >= INFO_ARRAY && n2->info%10 <= INFO_INT))
		yyerror("expression only allows int type or pointer arithmetic");
	if(n1->info == n2->info)
		return INFO_INT;
	return INFO_ARRAY;
}

int intArrayPointerExpr(Node* n1, Node* n2) {
	
	if(n1->info == INFO_ARRAY && n2->info == INFO_ARRAY &&
	((n1->attrib == LOCAL || n1->attrib == IDENT) 
	&& (n2->attrib == LOCAL || n2->attrib == IDENT)))
		yyerror("invalid operator for the types array and array");
	return intArrayExpr(n1, n2);
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

void printExpr(Node* n) {
	if(n->info != INFO_CHAR_LIT && (n->info%10 < INFO_ARRAY || n->info%10 > INFO_STR))
		yyerror("can only print array pointers, strings and intergers");
}

int verifyArgs(char* name, Node* argsNode){
	long* defArgs;
	int type = IDfind(name, (void**)&defArgs);	
	if (type < 20) {
		char* format = "\"%s\" is not a function";
		formatedErrorMsg(format, name);
		return 0;
	} else if (defArgs[0] == 0 && argsNode != 0) {
		char* format = "\"%s\" function doesn't have arguments";
		formatedErrorMsg(format, name);		
	} else {
		int i = defArgs[0];
		while(argsNode->attrib != NIL && i > 0) {
			Node* argNode = LEFT_CHILD(argsNode);
			int defType = defArgs[i];
			if(argNode->info%10 != defType) {
				if (defType == INFO_STR || defType == INFO_ARRAY) {
				printf("%d, %d\n", argNode->info, defType);
					if(!(argNode->info == INFO_INT && argNode->value.i == 0)) {
						char* format = "\"%s\":invalid argument type";
						formatedErrorMsg(format, name);
					}
				} else {
					char* format = "\"%s\":invalid argument type";
					formatedErrorMsg(format, name);
				}
			}
			i--;
			argsNode = RIGHT_CHILD(argsNode);
		}
		if(i != 0) {
			char* format = "\"%s\": wrong number of arguments";
			formatedErrorMsg(format, name);
		}
	}
	return type%10;
}

int verifyLVName(char* name) {
	long* args; 
	int type = IDfind(name, (void**)&args);
	if(type < 0) 
		yyerror("couldn't find identifier");

	if(type >= INFO_FUNC_ARRAY && args[0] > 0) 
		yyerror("function requires arguments");
	return type;
}

int strOrArrayIndex(Node* lvNode, Node* exprNode) {
	int type = lvNode->info;
	if(type != INFO_ARRAY && type != INFO_CONST_ARRAY
		&& type != INFO_STR && type != INFO_CONST_STR)
		yyerror("the left value needs to be an array or a string");
	
	if(exprNode->info%10 != INFO_INT)
		yyerror("index value must be a number");	

	return INFO_INT;		
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

void alocExpr(Node* n1, Node* n2) {
	if((n1->info != INFO_ARRAY && n1->info != INFO_STR)
		|| n2->info%10 != INFO_INT)
		yyerror("can only allocate for strings and arrays");
}

void nonVoidExpr(Node* n) {
	if(n->info%10 == INFO_VOID) yyerror("invalid void expression");
}

void formatedErrorMsg(char* format, char* msg) {
	sprintf(errmsg, format, msg);
	yyerror(errmsg);
}

static void arrayAssign(Node* dimNode, Node* initNode) {
	if (!dimNode->value.i) return; // size not specified
	if (initNode->attrib == NIL) return;
	Node* currNode = initNode->SUB(0);
	int counter, dim = dimNode->value.i;
	
	for (counter = 0; currNode->attrib == INTS; counter++) {
		currNode = currNode->SUB(0);
	}
	if(counter > dim) {
		yyerror("to many initializers for array");
	}
}
