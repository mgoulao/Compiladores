%{ 
#include <stdlib.h> 
#include <string.h> 
#include "y.tab.h" 

char *dupstr(const char *s);
extern void yyerror(const char *msg);
int hasStarted = 0;
%} 

D [0-9]
H [a-fA-F0-9]
O [0-7]
B [0-1]
CHAR_HEX \\[0-9A-Fa-f]{1,2}

%x COM CT CI CC FIN

%% 
	if (!hasStarted) BEGIN COM;

<FIN>.|\n		;

"$$".*			; /* operational comment */
"$"			BEGIN COM; /* explanatory comment */ 
<COM>^"program"		hasStarted = 1; BEGIN INITIAL; return PROGRAM;
<COM>^"module"		hasStarted = 1; BEGIN INITIAL; return MODULE;
<COM>"$"		BEGIN INITIAL;
<COM>.|\n		;

":="			return ASSIGN;
">="			return GE; 
"<="			return LE; 
"~=="			return EQ; 
"<>"			return NE; 


"start"			return START;
"end" 			BEGIN FIN; return FIN;
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


[a-zA-Z][a-zA-Z0-9_]*	yylval.s = dupstr(yytext); return IDENT;



\'			BEGIN CI;
<CI>\'			BEGIN INITIAL; return CHAR;
<CI>\\\'		yylval.i = (int) '\'';
<CI>\\			yylval.i = (int) '\\';
<CI>\\n			yylval.i = (int) '\n';
<CI>\\r			yylval.i = (int) '\r';	
<CI>\\t			yylval.i = (int) '\t';

\"([^*"]|\*.)*\"	yylval.s = malloc(yyleng); *yylval.s = 0; REJECT;
\"			printf("STRING");BEGIN CT;
<CT>\"			BEGIN INITIAL; return TEXTSTRING;
<CT>\\\"		strcat(yylval.s, "\"");
<CT>\\			strcat(yylval.s, "\\");
<CT>\\n			strcat(yylval.s, "\n");
<CT>\\r			strcat(yylval.s, "\r");
<CT>\\t			strcat(yylval.s, "\t");
<CT>\\{CHAR_HEX}	{ char s[2]; s[1] = 0; s[0] = strtol(yytext+1,0,16); strcat(yylval.s, s); }
<CT>\\.			yyerror("invalid escape sequence in string");
<CT>[^"]		strcat(yylval.s, yytext);

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
