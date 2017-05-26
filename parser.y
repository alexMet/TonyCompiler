%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <unistd.h>

#include "error.h"
#include "general.h"
#include "symbol.h"

Type getType(SymbolEntry *e);

%}

%union{
	const char	*name;
	struct SymbolEntry_tag *place;
	struct {
		struct SymbolEntry_tag *place;
		bool can_assign;
	} lvalue;
	struct Type_tag *type;
}

/* Token declarations */

%token T_end 	"end"
%token T_def 	"def"
%token T_ref 	"ref"
%token T_int 	"int"
%token T_bool	"bool"
%token T_char	"char"
%token T_list	"list"
%token T_decl	"decl"
%token T_exit	"exit"
%token T_for	"for"
%token T_return	"return"
%token T_skip	"skip"
%token T_true	"true"
%token T_false 	"false"
%token T_not 	"not"
%token T_and 	"and"
%token T_or 	"or"
%token T_isnil 	"nil?"
%token T_nil 	"nil"
%token T_head 	"head"
%token T_tail 	"tail"
%token T_new 	"new"
%token T_uneq	"<>"
%token T_greq	">="
%token T_leeq	"<="
%token T_anath	":="
%token T_mod
%token T_if		"if"
%token T_then 	"then"
%token T_else 	"else"
%token T_elsif	"elsif"

%token T_id
%token T_const_string
%token T_const_int
%token T_const_char

/* Precedence declarations */

%left 		T_or
%left 		T_and
%right 		T_not
%nonassoc 	'=' '<' '>' T_uneq T_greq T_leeq
%right 		'#'
%left 		'+' '-'
%left 		'*' '/' T_mod
%left 		NEG POS

/* Terminal and non-terminal symbols and their types */

%type<name>		T_id
%type<name>		T_const_int

%type<place>	expr
%type<lvalue>	atom
%type<name>		var_star

%%


program
:	
	{
		initSymbolTable(SYMBOL_TABLE_SIZE);
		printSymbolTable();
	
		openScope();
		printSymbolTable();
	}
	func_def
	{
		closeScope();
		printSymbolTable();

		destroySymbolTable();
	}
;

func_def
:	"def" header ':' def_star stmt_plus "end"
;

def_star
:	/* nothing */
|	func_def def_star
|	func_decl def_star
|	var_def def_star
;

stmt_plus
:	stmt stmt_plus
|	stmt
;

header
:	type T_id '(' ')'
|	T_id '(' ')'
|	type T_id '(' formal formal_star ')'
|	T_id '(' formal formal_star ')'
;

formal_star
:	/* nothing */
|	';' formal formal_star	
;

formal
:	"ref" type T_id var_star
|	type T_id var_star
;

var_star
:	/* nothing */
|	{printf("%s\n", $<name>$);} ',' T_id var_star
;

type
:	"int"
|	"bool"
|	"char"
|	type '[' ']'
|	"list" '[' type ']'
;

func_decl
:	"decl" header
;

var_def
:	type T_id { printf("Lelouas %s\n", $2); } var_star
;

stmt
:	simple
|	"exit"
|	"return" expr
|	"if" expr ':' stmt_plus else_star "end"
|	"if" expr ':' stmt_plus else_star "else" ':' stmt_plus "end"
|	"for" simple_list ';' expr ';' simple_list ':' stmt_plus "end"
;

else_star
:	/* nothing */
|	"elsif" expr ':' stmt_plus else_star
;

simple
:	"skip"
|	atom ":=" expr
|	call
;

simple_list
:	simple simple_star
;

simple_star
:	/* nothing */	
|	',' simple simple_star
;

call
:	T_id '(' ')'
|	T_id '(' expr expr_star ')'
;

expr_star
:	/* nothing */	
|	',' expr expr_star
;

atom
:	T_id
	{
		SymbolEntry *p = lookupEntry($1, LOOKUP_ALL_SCOPES, true);
		
		if (p == NULL)
			p = newVariable($1, typeInteger);
			
		$$.place = p;
		$$.can_assign = true;
	}
|	T_const_string
|	atom '[' expr ']'
|	call
;

expr
:	atom { $$ = $1.place; }
|	T_const_int
	{
		$$ = newConstant($1, typeInteger);
	}
|	T_const_char
|	'(' expr ')' { $$ = $2; }
|	'+' expr %prec POS
	{ 
		if (getType($2) != typeInteger)
			error("operator and operand don\'t agree: + t1 -> + int");
		
		$$ = $2; 
	}
|	'-' expr %prec NEG
	{
		if (getType($2) != typeInteger)
			error("operator and operand don\'t agree: - t1 -> - int");
			
		$$ = newTemporary(typeInteger);
		printf("00: -, %s, %s, %s\n", $2->id, "-", $$->id);
	}
|	expr '+' expr
	{
		if (getType($1) != typeInteger || getType($3) != typeInteger)
			error("operator and operand don\'t agree: t1 + t2 -> int + int");
			
		$$ = newTemporary(typeInteger);
		printf("00: +, %s, %s, %s\n", $1->id, $3->id, $$->id);
	}
|	expr '-' expr
	{
		if (getType($1) != typeInteger || getType($3) != typeInteger)
			error("operator and operand don\'t agree: t1 - t2 -> int - int");
			
		$$ = newTemporary(typeInteger);
		printf("00: -, %s, %s, %s\n", $1->id, $3->id, $$->id);
	}
|	expr '*' expr 
	{
		if (getType($1) != typeInteger || getType($3) != typeInteger)
			error("operator and operand don\'t agree: t1 * t2 -> int * int");
			
		$$ = newTemporary(typeInteger);
		printf("00: *, %s, %s, %s\n", $1->id, $3->id, $$->id);
	}
|	expr '/' expr
	{
		if (getType($1) != typeInteger || getType($3) != typeInteger)
			error("operator and operand don\'t agree: t1 / t2 -> int / int");
			
		$$ = newTemporary(typeInteger);
		printf("00: /, %s, %s, %s\n", $1->id, $3->id, $$->id);
	}
|	expr T_mod expr
	{
		if (getType($1) != typeInteger || getType($3) != typeInteger)
			error("operator and operand don\'t agree: t1 mod t2 -> int mod int");
			
		$$ = newTemporary(typeInteger);
		printf("00: mod, %s, %s, %s\n", $1->id, $3->id, $$->id);
	}
|	expr '=' expr
|	expr '<' expr
|	expr '>' expr
|	expr "<>" expr
|	expr "<=" expr
|	expr ">=" expr
|	"true"	
|	"false"
|	"not" expr
|	expr "and" expr
|	expr "or" expr
|	expr '#' expr
|	"new" type '[' expr ']'
|	"nil"
|	"nil?" '(' expr ')'
|	"head" '(' expr ')'
|	"tail" '(' expr ')'
;

		
%%

//Type getType(Type t) {

//}

Type getType(SymbolEntry *e) {
	switch (e->entryType) {
		case ENTRY_FUNCTION:
		    return e->u.eFunction.resultType;
		case ENTRY_VARIABLE:
		    return e->u.eVariable.type;
		case ENTRY_PARAMETER:
		    return e->u.eParameter.type;
		case ENTRY_TEMPORARY:
		    return e->u.eTemporary.type;
		case ENTRY_CONSTANT:
			return e->u.eConstant.type;
		default:
			internal("No such enty type.");
			return NULL;
	}
}

void yyerror(const char *msg) {
	fprintf(stderr, "syntax error: %s, line: %d\n", msg, linecount);
	exit(1);
}

int main(int argc, char **argv) {
	FILE *fd = fopen(argv[1], "r");
	filename = argv[1];
	dup2(fileno(fd), fileno(stdin));
	
	return yyparse();
}
