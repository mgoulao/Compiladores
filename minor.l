%{ 
#include <stdlib.h>
#include <limits.h>
#include <string.h> 
#include "node.h" 
#include "y.tab.h" 
char errmsg[80];
char* dupstr(const char *s);
extern void yyerror(const char *msg);
int integerConvertion(char* s);
void reservedWordError(char* word);
int hasStarted = 0, charRead = 0, strSize = 0;
%} 

D [0-9]
H [a-fA-F0-9]
O [0-7]
B [01]
CHAR_HEX \\[0-9A-Fa-f][0-9A-Fa-f]?

%x IGN COM CT CI CC FIN

%%
	if (!hasStarted) BEGIN IGN;

<FIN>.|\n		return 0;

<IGN>^"program"		hasStarted = 1; BEGIN INITIAL; return PROGRAM;
<IGN>^"module"		hasStarted = 1; BEGIN INITIAL; return MODULE;
<IGN>.|\n
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
^"end" 			BEGIN FIN; return END;
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
"assert"		return ASSERT;
"xor"			return XOR;

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

\"([^"\\]|\\.)*\"	strSize = yyleng; REJECT;
\"			if(strSize) { BEGIN CT;yylval.s = malloc(strSize); }
<CT>\"			BEGIN INITIAL; return TEXTSTRING;
<CT>\\\"		strcat(yylval.s, "\"");
<CT>\\\\		strcat(yylval.s, "\\");
<CT>\\n			strcat(yylval.s, "\n");
<CT>\\r			strcat(yylval.s, "\r");
<CT>\\t			strcat(yylval.s, "\t");
<CT>{CHAR_HEX}	{ char s[2]; s[1] = 0; s[0] = strtol(yytext+1,0,16); strcat(yylval.s, s); }
<CT>\\.			yyerror("invalid escape sequence in string");
<CT>.|\n		strcat(yylval.s, yytext);

0{O}+                   { yylval.i = integerConvertion(yytext); return INTEGER; }
{D}+			{ yylval.i = integerConvertion(yytext); return INTEGER; } 
"0x"{H}+		{ yylval.i = integerConvertion(yytext); return INTEGER; }
"0b"{B}+		{ yylval.i = integerConvertion(yytext); return INTEGER; }



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


int integerConvertion(char* s) {
	// based on teacher's itermediate solution
	char* end;
	unsigned long ret;
	if (strlen(s) > 1 && s[1] == 'b')
		ret = strtoul(s+2, &end, 2);
	else
		ret = strtoul(s, &end, 0);
	if (*end != 0) {
		yyerror("conversion error in literal integer");
		return -1;
	}
	if (ret > INT_MAX) {
		yyerror("overflow in literal integer");
		return -1;
	}
	return ret;
}

void reservedWordError(char* word) {
	const char* format = "\"%s\" is a reserved word" ;
	sprintf(errmsg, format, word);
	yyerror(errmsg);
}

