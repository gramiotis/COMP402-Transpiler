%{
  #include <stdio.h>
  #include "cgen.h"	
  #include "myanalyzer.tab.h"

  int line_num = 1;
%}

ID          [a-zA-Z][0-9a-zA-Z_]*
DIGIT       [0-9]
NUMBER      {DIGIT}{DIGIT}*
DECIMAL     ("."{NUMBER})|({NUMBER}("."{NUMBER}?)?)
REAL        {DECIMAL}([eE][+-]?{NUMBER})?
CONST_STRINGS \"(\\.|[^"\\])*\"
DELIMS      [;\(\),\[\]:.]
	
%%
void        { return KW_VOID; }
integer     { return KW_INT; }
scalar      { return KW_SCALAR; }
str         { return KW_STR; }
boolean     { return KW_BOOL; }
const       { return KW_CONST; }
if          { return KW_IF; }
else        { return KW_ELSE; }
endif       { return KW_ENDIF; }
for         { return KW_FOR; }
in          { return KW_IN; }
endfor      { return KW_ENDFOR; }
while       { return KW_WHILE; }
endwhile    { return KW_ENDWHILE; }
break       { return KW_BREAK; }
continue    { return KW_CONTINUE; }
not         { return KW_NOT; }
and         { return KW_AND; }
or          { return KW_OR; }
def         { return KW_DEF; }
enddef      { return KW_ENDDEF; }
main        { return KW_MAIN; }
return      { return KW_RETURN; }
comp        { return KW_COMP; }
endcomp     { return KW_ENDCOMP; }

"+"         return '+';
"-"         return '-';
"*"         return '*';
"/"         return '/';
"%"         return '%';
"**"        return KW_EXP;

"="         return '=';
"+="        return KW_PA;
"-="        return KW_MA;
"*="        return KW_MULA;
"/="        return KW_DA;
"%="        return KW_MODA;

"<"         return '<';
">"         return '>';
"<="        return KW_LEQ;
">="        return KW_GEQ;
"!="        return KW_NEQ;
"=="        return KW_EQ;


"("				  return '(';
")"					return ')';
"["					return '[';
"]"					return ']';
";"					return ';';
":"					return ':';
","					return ',';
"#"					return '#';
"."					return '.';
"->"        return KW_TYPE;

"True"      return TK_TRUE;
"False"     return TK_FALSE;

{NUMBER}        { yylval.str = strdup(yytext); return TK_INT; }
{CONST_STRINGS} { yylval.str = strdup(yytext); return TK_CONST_STRINGS; }
{REAL}          { yylval.str = strdup(yytext); return TK_REAL; }
{ID}            { yylval.str = strdup(yytext); return TK_IDENT; }


[ \r\t] /* skip whitespace */
"--".*  /* DO NOTHING */ 
\n 			++line_num;
<<EOF>> 	return EOF;

.           { printf("Unrecognized token %s in line %d:\n", yytext, line_num); }


%%
/*int main() { 
  yylex(); 
}*/
