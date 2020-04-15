%{ 
#include <stdlib.h> 
#include <string.h> 
#include <errno.h>
#include "node.h" 
#include "y.tab.h" 
char errmsg[80];
char* dupstr(const char *s);
extern void yyerror(const char *msg);
void reservedWordError(char* word);
int hasStarted = 0, charRead = 0;
%} 

D [0-9]
H [a-fA-F0-9]
O [0-7]
B [0-1]
CHAR_HEX \\[0-9A-Fa-f][0-9A-Fa-f]?

%x IGN COM CT CI CC FIN

%% 
	if (!hasStarted) BEGIN IGN;

<FIN>.|\n		;

<IGN>^"program"		hasStarted = 1; BEGIN INITIAL; return PROGRAM;
<IGN>^"module"		hasStarted = 1; BEGIN INITIAL; return MODULE;

"$$".*			; /* operational comment */
"$"			BEGIN COM; /* explanatory comment */ 
<COM>"$"		BEGIN INITIAL;
<COM>.|\n		;

":="			return ASSIGN;
">="			return GE; 
"<="			return LE; 
"~="			return NE; 
"#"			return ALOC;
"^"			return POW;

^"program"		reservedWordError(yytext);
^"module"		reservedWordError(yytext);

"start"			return START;
"end" 			BEGIN FIN; return END;
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


\'			{ BEGIN CI; charRead = 0; } 
<CI>\'			{ if(charRead == 0){ yyerror("empty char"); } else if(charRead > 1) { yyerror("multiple character char"); }BEGIN INITIAL; return CHAR;}
<CI>\\\'		{yylval.i = (int) '\''; charRead++;}
<CI>\\\\		{yylval.i = (int) '\\'; charRead++;}
<CI>\\n			{yylval.i = (int) '\n'; charRead++;}
<CI>\\r			{yylval.i = (int) '\r'; charRead++;}	
<CI>\\t			{yylval.i = (int) '\t'; charRead++;}
<CI>{CHAR_HEX}        { yylval.i = strtol(yytext+1, 0,16); charRead++; }
<CI>[^']		{yylval.i = (int) yytext[0]; charRead++;}

\"([^*"]|\*.)*\"	yylval.s = malloc(yyleng); *yylval.s = 0; REJECT;
\"			BEGIN CT;
<CT>\"			BEGIN INITIAL; return TEXTSTRING;
<CT>\\\"		strcat(yylval.s, "\"");
<CT>\\\\		strcat(yylval.s, "\\");
<CT>\\n			strcat(yylval.s, "\n");
<CT>\\r			strcat(yylval.s, "\r");
<CT>\\t			strcat(yylval.s, "\t");
<CT>{CHAR_HEX}	{ char s[2]; s[1] = 0; s[0] = strtol(yytext+1,0,16); strcat(yylval.s, s); }
<CT>\\.			yyerror("invalid escape sequence in string");
<CT>[^"]		strcat(yylval.s, yytext);

0{O}+                   { errno = 0; int val = strtol(yytext+1, 0, 8); if(errno == ERANGE || val < 0) yyerror("number overflow"); yylval.i = val; return INTEGER; }
{D}+			{ errno = 0; int val = strtol(yytext, 0, 10); if(errno == ERANGE || val < 0) yyerror("number overflow"); yylval.i = val; return INTEGER; } 
"0x"{H}+		{ errno = 0; int val = strtol(yytext+2, 0, 16); if(errno == ERANGE || val < 0) yyerror("number overflow"); yylval.i = val; return INTEGER; }
"0b"{B}+		{ errno = 0; int val = strtol(yytext+2, 0, 2); if(errno == ERANGE || val < 0) yyerror("number overflow"); yylval.i = val; return INTEGER; }



[-+()\[\];:!,.<>=*/%~?|&]	return *yytext; 
 
[ \t\n\r]+		; /* whitespaces */ 
 
.			yyerror("Unknown character"); 
%% 
int yywrap(void) { 
	if (YYSTATE == COM) 
		yyerror("unfinished comment");
	return 1;
} 
char *getyytext() { return yytext; }

void reservedWordError(char* word) {
	const char* format = "\"%s\" is a reserved word" ;
	sprintf(errmsg, format, word);
	yyerror(errmsg);
}

