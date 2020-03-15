
  Compiler for a minor programming language

  (tools: flex, byacc, pburg, nasm)

  [pburg: https://github.com/pedroreissantos/pburg ]

{D}*"."{D}+             { yylval.f = strtof(yytext, 0); retur    n FLOAT}
{D}+"."{D}*             { yylval.f = strtof(yytext, 0); retur    n FLOAT} /* Verificar se existe floats na linguagem */


