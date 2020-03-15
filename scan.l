%{ 
#include <stdlib.h> 
#include <string.h> 
#include "y.tab.h" 

char *dupstr(const char *s);
extern void yyerror(const char *msg);
%} 

D [0-9]
H [a-fA-F0-9]
O [0-7]
B [0-1]
CHAR_HEX \\[0-9A-Fa-f]{1,2}

%x COM STR CHAR

%% 
"$$".*			; /* operational comment */
"$"			BEGIN COM; /* explanatory comment */ 
<COM>"$"		BEGIN INITIAL;
<COM>.|\n		;

"-"			return MINUS;
"+"			return PLUS;
"*"			return TIMES;
"/"			return DIVISION;
"%"			return MOD;
"^"			return POW;
":="			return ASSIGN;
"<"			return LESS;
">"			return GREATER;
">="			return GE; 
"<="			return LE; 
"~=="			return EQ; 
"<>"			return NE; 
"~"			return LNOT;
"&"			return LAND;
"|"			return LOR;
"?"			return READ;


"program"		return PROGRAM;
"module"		return MODULE;
"start"			return START;
"end" 			return END;
"void"			return VOID;
"const"			return CONST;
"number"		return NUMBER;
"array"			return ARRAY;
"string"		return STRING;
"function"		return FUNCTION;
"public"		return PUBLIC;
"forward"		return FORWARD;
"if"			return IF;
"then"			return THEN;
"else"			return ELSE;
"elif"			return ELIF;
"fi"			return FI;
"for" 			return FOR;
"until"			return UNTIL;
"step"			return STEP;
"do"			return DO;
"done"			return DONE;
"repeat"		return REPEAT;
"stop"			return STOP;
"return"		return RETURN;

\'			BEGIN CHAR;
<CHAR>\'		BEGIN INITIAL; return CHAR
<CHAR>\\		strcat(yylval.s, "\n"			

[a-zA-Z][a-zA-Z0-9_]*	yylval.s = dupstr(yytext); return VARIABLE;


\"			BEGIN STR;
<STR>\"			BEGIN INITIAL; return STR
<STR>\\n		strcat(yylval.s, "\n");
<STR>\\r		strcat(yylval.s, "\r");
<STR>\\t		strcat(yylval.s, "\t");

0{O}+                   yylval.i = strtol(yytext, 0, 8); return INTEGER;
{D}+			yylval.i = strtol(yytext, 0, 10); return INTEGER; 
"0x"{H}+		yylval.i = strtol(yytext, 0, 16); return INTEGER;
"0b"{B}+		yylval.i = strtol(yytext, 0, 2); return INTEGER;


[-+()\[\];:!,.<>=*/%~?|&]	return *yytext; 
 
[ \t\n\r]+		; /* whitespaces */ 
 
.			yyerror("Unknown character"); 
%% 
int yywrap(void) { return 1; } 
char *getyytext() { return yytext; }
