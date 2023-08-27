%{
	#include <stdio.h>
    #include "cgen.h"
	#include "thetalib.h"
	
	extern int yylex(void);
%}

%union {
	char* str;
}

/***********TOKENS***********/
%token <str> TK_IDENT 
%token <str> TK_INT 
%token <str> TK_REAL 
%token <str> TK_CONST_STRINGS
%token <str> TK_TRUE
%token <str> TK_FALSE

/***********KEYWORDS***********/
%token KW_VOID
%token KW_INT
%token KW_SCALAR
%token KW_BOOL
%token KW_STR
%token KW_CONST 
%token KW_ELSE 
%token KW_OR
%token KW_AND
%token KW_NOT
%token KW_MAIN
%token KW_DEF
%token KW_ENDDEF
%token KW_IF 
%token KW_ENDIF
%token KW_FOR
%token KW_ENDFOR
%token KW_IN
%token KW_WHILE
%token KW_ENDWHILE
%token KW_COMP
%token KW_ENDCOMP
%token KW_BREAK
%token KW_CONTINUE
%token KW_RETURN
%token KW_EXP
%token KW_PA;
%token KW_MA;
%token KW_MULA;
%token KW_DA;
%token KW_MODA;
%token KW_LEQ;
%token KW_GEQ;
%token KW_NEQ;
%token KW_EQ;
%token KW_TYPE;

%start START

%type <str> START PRE_MAIN MAIN STMTS STMT listOfExprs expr assignExpr arrayASSGN STMT_ASSGN declareExpr declareConst FOR_VAR
%type <str> listOfID STMT_DECLARE STMT_IF ELSESTMT STMT_WHILE STMT_FOR BOOL STMT_FUNCCALL STMT_FUNC declareArray type args
%type <str> COMP_FUNC STMT_COMP COMP_STMTS COMP_STMT declareExpr_COMP declareArray_COMP listOfID_COMP assignExpr_COMP arrayASSGN_COMP op_COMP STMT_FUNC_COMP STMT_FUNCCALL_COMP
%type <str> listOfExprs_COMP STMT_IF_COMP ELSESTMT_COMP STMT_FOR_COMP STMT_WHILE_COMP FOR_VAR_COMP STMT_FUNCCALL_expr


%left '.' '(' ')' '[' ']'
%right KW_EXP
%left '+' '-'
%left '*' '/' '%'
%left KW_GEQ KW_LEQ '>' '<'
%left KW_EQ KW_NEQ
%left KW_OR
%left KW_AND
%right KW_NOT
%right '=' KW_PA KW_MA KW_MULA KW_DA

%%

//Functions/Comps Main or main only
START:
	MAIN { if(yyerror_count == 0) {puts(c_prologue); printf("%s\n", $1);} }
	| PRE_MAIN MAIN { if(yyerror_count == 0) {puts(c_prologue); printf("%s\n\n%s\n", $1, $2);} }

PRE_MAIN: //if there is anything before main
	COMP_FUNC { $$ = template("%s", $1);} 
	| PRE_MAIN COMP_FUNC { $$ = template("%s\n\n%s", $1, $2);}
	;

COMP_FUNC: //functions or comps
	STMT_FUNC
	| STMT_COMP
	| STMT_DECLARE
	;

/****STATEMENTS OF COMP****/
STMT_COMP:
	KW_COMP TK_IDENT ':' COMP_STMTS KW_ENDCOMP ';' { $$ = template("typedef struct %s\n{\n%s\n}%s;", $2, $4, $2); }

COMP_STMTS:
	COMP_STMT { $$ = template("%s", $1);}
	| COMP_STMT COMP_STMTS { $$ = template("%s\n%s", $1, $2);}
	;

COMP_STMT: //Statements modified for comp
	declareExpr_COMP
	| assignExpr_COMP
	| STMT_FUNCCALL_COMP
	| STMT_FUNC_COMP	
	| STMT_IF_COMP
	| STMT_FOR_COMP 
	| STMT_WHILE_COMP
	| KW_BREAK ';' { $$ = template("break;"); }
	| KW_CONTINUE ';' {$$ = template("continue;"); }

declareExpr_COMP:
	listOfID_COMP ':' KW_INT ';' { $$ = template("int %s;", $1); }
	| listOfID_COMP ':' KW_SCALAR ';' { $$ = template("double %s;", $1); }
	| listOfID_COMP ':' KW_STR ';' { $$ = template("char *%s;", $1); }
	| listOfID_COMP ':' KW_BOOL ';' { $$ = template("int %s;", $1); }
	| listOfID_COMP ':' TK_IDENT ';' { $$ = template("%s %s;", $3, $1); }
	;

declareArray_COMP:
	'#' TK_IDENT '[' expr ']' { $$ = template("%s[%s];", $2, $4); }
	| '#' TK_IDENT '[' ']' { $$ = template("* %s[];", $2); }

listOfID_COMP:
	'#' TK_IDENT { $$ = $2; }
	| TK_IDENT
	| declareArray_COMP
	| listOfID_COMP ',' '#' TK_IDENT { $$ = template("%s, %s", $1, $4);}
	| listOfID_COMP ',' declareArray_COMP { $$ = template("%s, %s", $1, $3);}

assignExpr_COMP:
	'#' TK_IDENT { $$ = $2; }
	| TK_IDENT
	| arrayASSGN_COMP
	| assignExpr_COMP '=' op_COMP ';' { $$ = template("%s = %s;", $1, $3); }
	| assignExpr_COMP KW_PA op_COMP ';' { $$ = template("%s += %s;", $1, $3); }
	| assignExpr_COMP KW_MA op_COMP ';' { $$ = template("%s -= %s;", $1, $3); }
	| assignExpr_COMP KW_MULA op_COMP ';' { $$ = template("%s *= %s;", $1, $3); }
	| assignExpr_COMP KW_DA op_COMP ';' { $$ = template("%s /= %s;", $1, $3); }
	| assignExpr_COMP KW_MODA op_COMP ';' { $$ = template("%s %= %s;", $1, $3); }
	;

arrayASSGN_COMP:
	'#' TK_IDENT '[' expr ']' { $$ = template("%s[%s]", $2, $4); }

op_COMP:
	TK_INT
	| TK_REAL 
	| TK_IDENT
	| '#' TK_IDENT { $$ = $2; }
	| arrayASSGN_COMP
	| TK_CONST_STRINGS
	| BOOL
	/* ARITHMETIC */
	| '-' op_COMP { $$ = template("-%s", $2); }
	| op_COMP '+' op_COMP { $$ = template("%s + %s", $1, $3); }
	| op_COMP '-' op_COMP { $$ = template("%s - %s", $1, $3); }
	| op_COMP '*' op_COMP { $$ = template("%s * %s", $1, $3); }
	| op_COMP '/' op_COMP { $$ = template("%s / %s", $1, $3); }
	| op_COMP '%' op_COMP { $$ = template("%s % %s", $1, $3); }
	| op_COMP KW_EXP op_COMP { $$ = template("%s ** %s", $1, $3); }
	/* RELATIONS */
	| op_COMP '<' op_COMP { $$ = template("%s < %s", $1, $3); }
  	| op_COMP '>' op_COMP { $$ = template("%s > %s", $1, $3); }
  	| op_COMP KW_LEQ op_COMP { $$ = template("%s <= %s", $1, $3); }
  	| op_COMP KW_GEQ op_COMP { $$ = template("%s >= %s", $1, $3); }
  	| op_COMP KW_NEQ op_COMP { $$ = template("%s != %s", $1, $3); }
  	| op_COMP KW_EQ op_COMP { $$ = template("%s == %s", $1, $3); }
	/* LOGICAL */
	| op_COMP KW_AND op_COMP { $$ = template("%s && %s", $1, $3); }
	| op_COMP KW_OR op_COMP { $$ = template("%s || %s", $1, $3); }
	| KW_NOT op_COMP { $$ = template("!%s", $2); }
	| '(' op_COMP ')' { $$ = template("(%s)", $2); }
	;

STMT_FUNC_COMP: //functions modified for comp
	KW_DEF TK_IDENT '(' args ')' KW_TYPE type ':' COMP_STMTS KW_RETURN op_COMP ';' KW_ENDDEF ';' { $$ = template("%s %s(%s)\n{\n%s\nreturn %s;\n}", $7, $2, $4, $9, $11); }
	| KW_DEF TK_IDENT '(' ')' KW_TYPE type ':' COMP_STMTS KW_RETURN op_COMP ';' KW_ENDDEF ';' { $$ = template("%s %s()\n{\n%s\nreturn %s;\n}", $6, $2, $8, $10); }
	| KW_DEF TK_IDENT '(' args ')' ':' COMP_STMTS KW_ENDDEF ';' { $$ = template("void %s(%s)\n{\n%s\nreturn;\n}", $2, $4, $7); }
	| KW_DEF TK_IDENT '(' ')' ':' COMP_STMTS KW_ENDDEF ';' { $$ = template("void %s()\n{\n%s\nreturn;\n}", $2, $6); }
	| KW_DEF TK_IDENT '(' ')' ':' KW_ENDDEF ';' { $$ = template("void %s()\n{\n\n}", $2); }
	;

STMT_FUNCCALL_COMP:
	TK_IDENT '(' listOfExprs_COMP ')' ';' { $$ = template("%s(%s);", $1, $3);}
	| TK_IDENT '(' ')' ';' { $$ = template("%s();", $1);}
	| '#' TK_IDENT '.' TK_IDENT '(' listOfExprs_COMP ')' ';' { $$ = template("%s.%s(&%s, %s);", $2, $4, $2, $6); }
	| '#' TK_IDENT '.' TK_IDENT '(' ')' ';' { $$ = template("%s.%s(&%s, %s);", $2, $4, $2); }
	| arrayASSGN_COMP '.' TK_IDENT '(' listOfExprs_COMP ')' ';' { $$ = template("%s.%s(&%s, %s);", $1, $3, $1, $5); }
	| arrayASSGN_COMP '.' TK_IDENT '(' ')' ';' { $$ = template("%s.%s(&%s, %s);", $1, $3, $1); }
	;

listOfExprs_COMP: 
	op_COMP
	| listOfExprs_COMP ',' op_COMP { $$ = template("%s,%s", $1, $3);}
	;

/****IF STATEMENT****/
STMT_IF_COMP: 
	KW_IF '(' op_COMP ')' ':' COMP_STMTS KW_ENDIF ';' { $$ = template("if(%s)\n{\n%s\n}", $3, $6);}
	| KW_IF '(' op_COMP ')' ':' COMP_STMTS ELSESTMT_COMP KW_ENDIF ';' { $$ = template("if(%s)\n{\n%s\n}\nelse\n{\n%s\n}", $3, $6, $7);}
	;

ELSESTMT_COMP: 
	KW_ELSE ':' COMP_STMTS { $$ = template("%s", $3); }
	;

/****WHILE STATEMENT****/
STMT_WHILE_COMP:
	KW_WHILE '(' op_COMP ')' ':' COMP_STMTS KW_ENDWHILE ';' { $$ = template("while(%s)\n{\n%s\n}", $3, $6);}
	;


/****FOR STATEMENT****/
STMT_FOR_COMP: 
	KW_FOR TK_IDENT KW_IN '[' FOR_VAR_COMP ':' FOR_VAR_COMP ':' FOR_VAR_COMP ']' ':' COMP_STMTS KW_ENDFOR ';' { $$ = template("for(int %s = %s; %s <= %s; %s += %s)\n{\n%s\n}", $2, $5, $2, $7, $2, $9, $12); }
 	| KW_FOR TK_IDENT KW_IN '[' FOR_VAR_COMP ':' FOR_VAR_COMP ']' ':' COMP_STMTS KW_ENDFOR ';' { $$ = template("for(int %s = %s; %s <= %s; %s++)\n{\n%s\n}", $2, $5, $2, $7, $2, $10); }
	;

FOR_VAR_COMP:
	op_COMP
	;
/****END COMP***/






/****MAIN****/
MAIN:
	KW_DEF KW_MAIN '(' ')' ':' STMTS KW_ENDDEF ';' { $$ = template("void main(){\n%s\nreturn;\n}", $6); }
	;

STMTS:
	STMT { $$ = template("%s", $1);}
	| STMT STMTS { $$ = template("%s\n%s", $1, $2);}
	;

/****All types of statements****/
STMT: 
	STMT_DECLARE
	| STMT_ASSGN  
	| STMT_FUNCCALL
	| STMT_IF
	| STMT_WHILE
	| STMT_FOR
	| KW_BREAK ';' { $$ = template("break;"); }
	| KW_CONTINUE ';' {$$ = template("continue;"); }
	;

/****EXPRESSIONS****/
listOfExprs: 
	expr 
	| listOfExprs ',' expr { $$ = template("%s, %s", $1, $3);}
	;

expr:
	TK_INT
	| TK_REAL 
	| TK_IDENT
	| arrayASSGN
	| STMT_FUNCCALL_expr
	| TK_CONST_STRINGS
	| BOOL
	/* ARITHMETIC */
	| '-' expr { $$ = template("-%s", $2); }
	| expr '+' expr { $$ = template("%s + %s", $1, $3); }
	| expr '-' expr { $$ = template("%s - %s", $1, $3); }
	| expr '*' expr { $$ = template("%s * %s", $1, $3); }
	| expr '/' expr { $$ = template("%s / %s", $1, $3); }
	| expr '%' expr { $$ = template("%s %% %s", $1, $3); }
	| expr KW_EXP expr { $$ = template("pow(%s,%s)", $1, $3); }
	/* RELATIONS */
	| expr '<' expr { $$ = template("%s < %s", $1, $3); }
  	| expr '>' expr { $$ = template("%s > %s", $1, $3); }
  	| expr KW_LEQ expr { $$ = template("%s <= %s", $1, $3); }
  	| expr KW_GEQ expr { $$ = template("%s >= %s", $1, $3); }
  	| expr KW_NEQ expr { $$ = template("%s != %s", $1, $3); }
  	| expr KW_EQ expr { $$ = template("%s == %s", $1, $3); }
	/* LOGICAL */
	| expr KW_AND expr { $$ = template("%s && %s", $1, $3); }
	| expr KW_OR expr { $$ = template("%s || %s", $1, $3); }
	| KW_NOT expr { $$ = template("!%s", $2); }
	| '(' expr ')' { $$ = template("(%s)", $2); }
	;

STMT_FUNCCALL_expr:
	TK_IDENT '(' listOfExprs ')' { $$ = template("%s(%s)", $1, $3); }
	| TK_IDENT '(' ')' { $$ = template("%s()", $1); }
	| TK_IDENT '.' TK_IDENT '(' ')' { $$ = template("%s.%s(&%s)", $1, $3, $1); }
	| TK_IDENT '.' TK_IDENT '(' listOfExprs ')' ';' { $$ = template("%s.%s(&%s, %s);", $1, $3, $1, $5); }

/****ASSIGN VARIABLES****/
assignExpr:
	TK_IDENT
	| arrayASSGN
	| assignExpr '=' expr ';' { $$ = template("%s = %s;", $1, $3); }
	| assignExpr KW_PA expr ';' { $$ = template("%s += %s;", $1, $3); }
	| assignExpr KW_MA expr ';' { $$ = template("%s -= %s;", $1, $3); }
	| assignExpr KW_MULA expr ';' { $$ = template("%s *= %s;", $1, $3); }
	| assignExpr KW_DA expr ';' { $$ = template("%s /= %s;", $1, $3); }
	| assignExpr KW_MODA expr ';' { $$ = template("%s %= %s;", $1, $3); }
	;

arrayASSGN:
	TK_IDENT '[' expr ']' { $$ = template("%s[%s]", $1, $3); }

STMT_ASSGN: 
	assignExpr
	;


/****DECLARE VARIABLES****/
declareExpr:
	listOfID ':' KW_INT ';' { $$ = template("int %s;", $1); }
	| listOfID ':' KW_SCALAR ';' { $$ = template("double %s;", $1); }
	| listOfID ':' KW_STR ';' { $$ = template("char *%s;", $1); }
	| listOfID ':' KW_BOOL ';' { $$ = template("int %s;", $1); }
	| listOfID ':' TK_IDENT ';' { $$ = template("%s %s;", $3, $1); }
	;

declareArray:
	TK_IDENT '[' expr ']' { $$ = template("%s[%s]", $1, $3); }
	| TK_IDENT '[' ']' { $$ = template("* %s[];", $1); }

declareConst:
	KW_CONST TK_IDENT '=' TK_INT ':' KW_INT ';' { $$ = template("const int %s = %s;", $2, $4); }
	| KW_CONST TK_IDENT '=' '-' TK_INT ':' KW_INT ';' { $$ = template("const int %s = -%s;", $2, $5); }
	| KW_CONST TK_IDENT '=' TK_REAL ':' KW_SCALAR ';' { $$ = template("const double %s = %s;", $2, $4); }
	| KW_CONST TK_IDENT '=' '-' TK_REAL ':' KW_SCALAR ';' { $$ = template("const double %s = -%s;", $2, $5); }
	| KW_CONST TK_IDENT '=' TK_CONST_STRINGS ':' KW_STR ';' { $$ = template("const char* %s = %s;", $2, $4); }
	| KW_CONST TK_IDENT '=' BOOL ':' KW_BOOL ';' { $$ = template("const int %s = %s;", $2, $4); }
	;

listOfID:
	TK_IDENT
	| declareArray
	| listOfID ',' TK_IDENT { $$ = template("%s, %s", $1, $3);}
	| listOfID ',' declareArray { $$ = template("%s, %s", $1, $3);}
	;

STMT_DECLARE: 
	declareExpr
	| declareConst
	;

BOOL:
	TK_FALSE { $$ = template("0"); }
	| TK_TRUE { $$ = template("1"); }
	;



/****IF STATEMENT****/
STMT_IF: 
	KW_IF '(' expr ')' ':' STMTS KW_ENDIF ';' { $$ = template("if(%s)\n{\n%s\n}", $3, $6);}
	| KW_IF '(' expr ')' ':' STMTS ELSESTMT KW_ENDIF ';' { $$ = template("if(%s)\n{\n%s\n}\nelse\n{\n%s\n}", $3, $6, $7);}
	;

ELSESTMT: 
	KW_ELSE ':' STMTS { $$ = template("%s", $3); }
	;

/****WHILE STATEMENT****/
STMT_WHILE:
	KW_WHILE '(' expr ')' ':' STMTS KW_ENDWHILE ';' { $$ = template("while(%s)\n{\n%s\n}", $3, $6);}
	;


/****FOR STATEMENT****/
STMT_FOR: 
	KW_FOR TK_IDENT KW_IN '[' FOR_VAR ':' FOR_VAR ':' FOR_VAR ']' ':' STMTS KW_ENDFOR ';' { $$ = template("for(int %s = %s; %s <= %s; %s += %s)\n{\n%s\n}", $2, $5, $2, $7, $2, $9, $12); }
 	| KW_FOR TK_IDENT KW_IN '[' FOR_VAR ':' FOR_VAR ']' ':' STMTS KW_ENDFOR ';' { $$ = template("for(int %s = %s; %s <= %s; %s++)\n{\n%s\n}", $2, $5, $2, $7, $2, $10); }
	;

FOR_VAR:
	expr 
	;

/****FUNCTION STATEMENT****/
STMT_FUNC: 
	KW_DEF TK_IDENT '(' args ')' KW_TYPE type ':' STMTS KW_RETURN expr ';' KW_ENDDEF ';' { $$ = template("%s %s(%s)\n{\n%s\nreturn %s;\n}", $7, $2, $4, $9, $11); }
	| KW_DEF TK_IDENT '(' ')' KW_TYPE type ':' STMTS KW_RETURN expr ';' KW_ENDDEF ';' { $$ = template("%s %s()\n{\n%s\nreturn %s;\n}", $6, $2, $8, $10); }
	| KW_DEF TK_IDENT '(' args ')' ':' STMTS KW_ENDDEF ';' { $$ = template("void %s(%s)\n{\n%s\nreturn;\n}", $2, $4, $7); }
	| KW_DEF TK_IDENT '(' ')' ':' STMTS KW_ENDDEF ';' { $$ = template("void %s()\n{\n%s\nreturn;\n}", $2, $6); }
	| KW_DEF TK_IDENT '(' args ')' KW_TYPE KW_VOID ':' STMTS KW_ENDDEF ';' { $$ = template("void %s(%s)\n{\n%s\nreturn;\n}", $2, $4, $9); }
	| KW_DEF TK_IDENT '(' ')' KW_TYPE KW_VOID ':' STMTS KW_ENDDEF ';' { $$ = template("void %s()\n{\n%s\nreturn;\n}", $2, $8); }
	| KW_DEF TK_IDENT '(' args ')' KW_TYPE type ':' KW_RETURN expr ';' KW_ENDDEF ';' { $$ = template("%s %s(%s)\n{\nreturn %s;\n}", $7, $2, $4, $10); }
	| KW_DEF TK_IDENT '(' ')' KW_TYPE type ':' KW_RETURN expr ';' KW_ENDDEF ';' { $$ = template("%s %s()\n{\nreturn %s;\n}", $6, $2, $9); }
	| KW_DEF TK_IDENT '(' ')' ':' KW_ENDDEF ';' { $$ = template("void %s()\n{\n\n}", $2); }
	;

type:
	KW_INT { $$ = template("int"); }
	| KW_SCALAR { $$ = template("double"); }
	| KW_STR { $$ = template("char*"); }
	| KW_BOOL { $$ = template("int"); }
	| TK_IDENT { $$ = template("%s", $1); }
	;

args:
	TK_IDENT ':' KW_INT { $$ = template("int %s", $1);}
	| TK_IDENT ':' KW_SCALAR { $$ = template("double %s", $1);}
	| TK_IDENT ':' KW_STR { $$ = template("char* %s", $1);}
	| TK_IDENT ':' KW_BOOL { $$ = template("int %s", $1);}
	| TK_IDENT ':' TK_IDENT { $$ = template("%s %s", $3, $1);}
	| TK_IDENT '[' ']' ':' KW_STR { $$ = template("char** %s", $1);}
	| TK_IDENT '[' ']' ':' KW_INT { $$ = template("int* %s", $1);}
	| args ',' TK_IDENT ':' KW_INT { $$ = template("%s, int %s", $1, $3);}
	| args ',' TK_IDENT ':' KW_SCALAR { $$ = template("%s, double %s", $1, $3);}
	| args ',' TK_IDENT ':' KW_STR { $$ = template("%s, char* %s", $1, $3);}
	| args ',' TK_IDENT ':' KW_BOOL { $$ = template("%s, int %s", $1, $3);}
	| args ',' TK_IDENT ':' TK_IDENT { $$ = template("%s, %s %s", $1, $5, $3);}
	| args ',' TK_IDENT '[' ']' ':' KW_STR { $$ = template("%s, char** %s", $1, $3);}
	| args ',' TK_IDENT '[' ']' ':' KW_INT { $$ = template("%s, int* %s", $1, $3);}
	;

/****FUNCTION CALL****/
STMT_FUNCCALL:
	TK_IDENT '.' TK_IDENT '(' listOfExprs ')' ';' { $$ = template("%s.%s(&%s, %s);", $1, $3, $1, $5); }
	| TK_IDENT '.' TK_IDENT '(' ')' ';' { $$ = template("%s.%s(&%s);", $1, $3, $1); }
	| TK_IDENT '(' listOfExprs ')' ';' { $$ = template("%s(%s);", $1, $3); }
	| TK_IDENT '(' ')' ';' { $$ = template("%s();", $1);}
	
%%
int main ()
{
   if ( yyparse() == 0 )
		return 0; //printf("Your program is syntactically correct!\n");
	else
		return 1; //printf("Rejected!\n");
}

