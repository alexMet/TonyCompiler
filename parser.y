%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <unistd.h>

#include "general.h"
#include "symbol.h"
#include "error.h"

struct func_node {
	bool hasRet;
	struct Type_tag *typ;
	struct func_node *prev, *next;
};

struct call_node {
	int argu;
	SymbolEntry *cur;
	SymbolEntry *cpo;
	struct call_node *prev, *next;
};

struct func_node *start_in_func, *end_in_func;
struct call_node *start_check_args, *end_check_args;
SymbolEntry *p;
SymbolEntry *cpo;
int atomo;
bool forw, refer, variable;
struct Type_tag *decla;


int yylex(void);
void yyerror (const char msg []);

void enqueue_in_func(struct Type_tag *ty) {
	struct func_node *new;

	if ((new = malloc(sizeof(struct func_node))) == NULL)
		internal("No memory");

	new->next = NULL;
	new->typ = ty;
	new->hasRet = false;

	if (start_in_func == NULL) {	
		new->prev = NULL;
		start_in_func = end_in_func = new;
	}
	else {
		new->prev = end_in_func;
		end_in_func = new;
	}
}

void dequeue_in_func(void) {
	struct func_node *cur;

	if (end_in_func == NULL)
		internal("Remove from empty func queue");
	
	if (end_in_func == start_in_func) {
		free(start_in_func);
		end_in_func = start_in_func = NULL;
		return;
	}
	
	cur = end_in_func;
	cur->prev->next = NULL;
	end_in_func = cur->prev;
	free(cur);
}

void enqueue_check_args(SymbolEntry *cal) {
	struct call_node *new;

	if ((new = malloc(sizeof(struct call_node))) == NULL)
		internal("No memory");

	new->next = NULL;
	new->cpo = cpo;
	new->cur = cpo->u.eFunction.firstArgument->u.eParameter.next;
	new->argu = 2;

	if (start_check_args == NULL) {	
		new->prev = NULL;
		start_check_args = end_check_args = new;
	}
	else {
		new->prev = end_check_args;
		end_check_args = new;
	}
}

void dequeue_check_args(void) {
	struct call_node *cur;

	if (end_check_args == NULL)
		internal("Remove from empty func queue");
	
	if (end_check_args == start_check_args) {
		free(start_check_args);
		end_check_args = start_check_args = NULL;
		return;
	}
	
	cur = end_check_args;
	cur->prev->next = NULL;
	end_check_args = cur->prev;
	free(cur);
}

%}

%union{
	int number;
	char character;
	char *string;
	struct Type_tag * typ;
	struct kefali {
		bool from_string;
		struct Type_tag * typ;
	} atomic;
}

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
%token T_if 	"if"
%token T_return	"return"
%token T_skip	"skip"
%token T_true	"true"
%token T_false 	"false"
%token T_not 	"not"
%token T_and 	"and"
%token T_or 	"or"
%token T_nil 	"nil"
%token T_isnil 	"nil?"
%token T_head 	"head"
%token T_tail 	"tail"
%token T_new 	"new"
%token T_uneq	"<>"
%token T_greq	">="
%token T_leeq	"<="
%token T_anath	":="
%token T_mod
%token T_then   "then"
%token T_else	"else"
%token T_elsif	"elsif"

%token T_const
%token T_var
%token T_string
%token T_chara

%left 		T_or
%left 		T_and
%right 		T_not
%nonassoc 	'=' '<' '>' T_uneq T_greq T_leeq
%right 		'#'
%left 		'+' '-'
%left 		'*' '/' T_mod
%left 		NEG POS

%type<typ> 		expr
%type<typ> 		simple
%type<atomic> 	atom
%type<typ> 		type
%type<typ> 		call
%type<number> 	T_const
%type<string> 	var
%type<string> 	cvar
%type<string> 	fvar
%type<string> 	T_var
%type<string> 	T_string
%type<character> T_chara

%%


program
:	func_def 
;

func_def	
:	"def"  header ':' def_star stmt_plus "end"
	{
		if (end_in_func->typ != typeVoid && !(end_in_func->hasRet)) {
			error("Function ends without returning a value");
			return 1;
		}
		dequeue_in_func();
		closeScope();
	}
;

def_star
:	func_def def_star
|	func_decl def_star
|	var_def def_star
|	/* nothing */
;

stmt_plus
:	stmt stmt_plus
|	stmt
;

header
:	type fvar '(' formal formal_star ')'
	{
		if (!forw) enqueue_in_func($1);
		endFunctionHeader(p, $1);
		printSymbolTable();
	}

|	fvar '(' formal formal_star ')'
	{
		if (!forw) enqueue_in_func(typeVoid);
		endFunctionHeader(p, typeVoid);
		printSymbolTable();
	}

|	fvar '(' ')' 
	{	
		if (!forw) enqueue_in_func(typeVoid);	
		endFunctionHeader(p, typeVoid);
		printSymbolTable();
	}

|	type fvar '(' ')'
	{ 	   
		if (!forw) enqueue_in_func($1);
		endFunctionHeader(p, $1);
		printSymbolTable();
	}
;

fvar
: T_var 
	{	
		$$ = yylval.string; 
		p = newFunction(yylval.string);
		if (p == NULL) return 1;
		if (forw) forwardFunction(p);
		openScope();
		printSymbolTable();
	}
;

var	
:	T_var 
	{	$$ = yylval.string; }
;

formal_star
:	';' formal formal_star
|	/* nothing */
;

formal
:
	"ref" { refer = true; } type var var_star 
	{
		refer = false;
		SymbolEntry * fail = newParameter($4, $3, PASS_BY_REFERENCE, p);
		printSymbolTable(); 
		if (fail == NULL) return 1;		
	}

|	type var var_star 
	{ 
		SymbolEntry * fail = newParameter($2, $1, PASS_BY_VALUE, p);
		printSymbolTable(); 
		if (fail == NULL) return 1;
	}
;

var_star
:	',' var var_star
	{
		if (variable) {
			SymbolEntry * fail = newVariable($2, decla);
			printSymbolTable();
			if (fail == NULL) return 1;	
		}
		else {
			PassMode mode;
			if (refer) mode = PASS_BY_REFERENCE;
			else mode = PASS_BY_VALUE;
			SymbolEntry * fail = newParameter($2, decla, mode, p);
			printSymbolTable();
			if (fail == NULL) return 1;	
		}		
	}

|	/* nothing */
;

type
:	"int" 
	{	$$ = decla = typeInteger; }

|	"bool"
	{	$$ = decla = typeBoolean; }

|	"char"
	{	$$ = decla = typeChar; }

|	type '[' ']'
	{	$$ = decla = typeIArray(decla); } //create array recursive

|	"list" '[' type ']'
	{	$$ = decla =  typePointer(decla); } //new type for list
;

func_decl
:	{ 	forw = true; } 
	"decl" header 
	{ 	forw = false; closeScope(); }
;

var_def
:	
	{ 	variable = true; }
	type var var_star
	{
		variable = false;
		SymbolEntry * fail = newVariable($3, $2);
		if (fail == NULL) return 1;		
		printSymbolTable();
	}
;

stmt
:	simple
|	"exit"
	{
		if (end_in_func->typ != typeVoid) {
			error("Exit in function with return value");
			return 1;
		}
	}

|	"return" expr
	{
		if (!equalType(end_in_func->typ, $2)) { //TODO elengxos gia lista kai nil
			error("Wrong 'return' value diffent from function's type");
			return 1;
		}
		end_in_func->hasRet = true;
	}

|	"if" expr ':' stmt_plus else_star "end" 
	{	
		if ($2 != typeBoolean) {
			error("If (boolean expr) : ...");
			return 1;
		}
	}

|	"if" expr ':' stmt_plus else_star "else" ':' stmt_plus "end"
	{	
		if ($2 != typeBoolean) {
			error("If (boolean expr) : ...");
			return 1;
		}
	}

|	"for" simple_list ';' expr ';' simple_list ':' stmt_plus "end"
	{
		if ($4 != typeBoolean) {
			error("For ... ; (boolean expr) ; ...");
			return 1;
		}
	}
;

else_star
:	"elsif" expr ':' stmt_plus else_star
	{
		if ($2 != typeBoolean) {
			error("Elsif (boolean expr) : ...");
			return 1;
		}
	}

|	/* nothing */	
;

simple
:	"skip"
	{ $$ = NULL; }

|	atom ":=" expr 
	{	
		if (equalType($1.typ, $3)) // TODO elengxos gia lista kai nil
			$$ = $1.typ;
		else {
			error("Wrong assignment");
			return 1;
		}
	}

|	call
	{ $$ = $1; }
;

simple_list
:	simple simple_star
;

simple_star
:	',' simple simple_star
|	/* nothing */
;

call
:	cvar '(' ')' 
	{	
		if (cpo->u.eFunction.firstArgument != NULL) {
			error("Too few arguments in function %s", cpo->id);
			return 1;
		}
		$$ = cpo->u.eFunction.resultType;
	}

|	cvar '(' expr 
	{
		if (cpo->u.eFunction.firstArgument == NULL) {
			error("Too many arguments in function %s", cpo->id);
			return 1;
		}
		if (!equalType($3, cpo->u.eFunction.firstArgument->u.eParameter.type)) { //TODO lista kai nil
			error("Type mismatch in parameter 1");
			return 1;
		}
		enqueue_check_args(cpo);
	}
	expr_star ')'
	{
		$$ = end_check_args->cpo->u.eFunction.resultType;
		dequeue_check_args();
	}
;

cvar	
:	T_var
	{
		cpo = lookupEntry(yylval.string, LOOKUP_ALL_SCOPES, true); 
		if (cpo == NULL) return 1;
		else 
			if (cpo->entryType != ENTRY_FUNCTION) {
				error("Wrong function calling name, %s is not a function", cpo->id);
				return 1;	
			}
			else $$ = yylval.string;
	}
;

expr_star
:	',' expr 
	{
		if (end_check_args->cur == NULL) {
			error("Too many arguments in function %s", cpo->id);
			return 1;
		}
		if (!equalType($2, end_check_args->cur->u.eParameter.type)) { //TODO list and nil
			error("Type mismatch in parameter %d", end_check_args->argu);
			return 1;
		}
		if (end_check_args->cur == end_check_args->cpo->u.eFunction.lastArgument) {
			end_check_args->cur = NULL;
		}
		else {
			end_check_args->argu++;
			end_check_args->cur = end_check_args->cur->u.eParameter.next;
		}
	}
	expr_star

|	/* nothing */
	{
		if (end_check_args->cur != NULL) {
			error("Too few arguments in function %s", end_check_args->cpo->id);
			return 1;
		}
	}
;

atom
:	var	
	{ 
		p = lookupEntry($1, LOOKUP_ALL_SCOPES, true); 
		if (p == NULL)
			return 1;
		else 
			if (p->entryType == ENTRY_VARIABLE || p->entryType == ENTRY_PARAMETER) 
				$$.typ = p->u.eVariable.type;
			else {
				error("Wrong left name in assignment");
				return 1;	
			}
		$$.from_string = false;
	}

|	T_string
	{
		$$.typ = typeArray((RepInteger) (strlen(yylval.string) - 1), typeChar);
		$$.from_string = true;
	}

|	atom '[' expr ']'
	{
		if ($3 != typeInteger) {
			error("Array [ (integer expression) ]");
			return 1;
		}
		if ($1.from_string) {
			error("string [ expr ]");
			return 1;
		}
		if ($1.typ == NULL || $1.typ->refType == NULL) {
			error("Accessing more dimentions than array has");
			return 1;
		}
		$$.typ = $1.typ->refType;
		$$.from_string = false;
	}

|	call 
	{ 
		$$.typ = $1;
		$$.from_string = false;	
	}
;

expr
:	atom
	{	$$ = $1.typ; }

|	T_const
	{	$$ = typeInteger; }

|	T_chara
	{	$$ = typeChar; }

|	'(' expr ')'
	{	$$ = $2; }

|	'+' expr		
	{
		if ($2 == typeInteger) $$ = $2; 
		else { 
			error("Type mismatch: + t1 -> + int");
			return 1;
		}
	}

|	'-' expr
	{ 
		if ($2 == typeInteger) $$ = $2; 
		else {
			error("Type mismatch: - t1 -> - int");
			return 1;
		}
	}

|	expr '+' expr 	
	{
		if ($1 == typeInteger && equalType($1, $3)) $$ = $1; 
		else {
			error("Type mismatch: t1 + t2 -> int + int");
			return 1;
		}
	}

|	expr '-' expr 	
	{
		if ($1 == typeInteger && equalType($1, $3)) $$ = $1;
		else {
			error("Type mismatch: t1 - t2 -> int - int");
			return 1;
		}
	}

|	expr '*' expr 	
	{ 
		if ($1 == typeInteger && equalType($1, $3)) $$ = $1;
		else {
			error("Type mismatch: t1 * t2 -> int * int");
			return 1;
		}
	}

|	expr '/' expr 	
	{ 
		if ($1 == typeInteger && equalType($1, $3)) $$ = $1; 
		else { 
			error("Type mismatch: t1 / t2 -> int / int"); 
			return 1; 
		}
	}

|	expr T_mod expr	
	{ 
		if ($1 == typeInteger && equalType($1, $3)) $$ = $1;
		else {
			error("Type mismatch: t1 mod t2 -> int mod int"); 
			return 1; 
		} 
	}

|	expr '=' expr	
	{ 
		if (equalType($1, $3)) $$ = typeBoolean; 
		else { 
			error("Type mismatch: t1 = t2");
		 	return 1; 
		} 
	}

|	expr '<' expr 	
	{ 
		if (equalType($1, $3)) $$ = typeBoolean; 
		else { 
			error("Type mismatch t1 < t2"); 
			return 1; 
		}
	}

|	expr '>' expr 	
	{
		if (equalType($1, $3)) $$ = typeBoolean;
		else {
			error("Type mismatch: t1 > t2");
			return 1;
		}
	}

|	expr "<>" expr	
	{
		if (equalType($1, $3)) $$ = typeBoolean;
		else {
			error("Type mismatch: t1 <> t2");
			return 1;
		}
	}

|	expr "<=" expr	
	{ 
		if (equalType($1, $3)) $$ = typeBoolean;
		else {
			error("Type mismatch: t1 <= t2");
			return 1;
		}
	}

|	expr ">=" expr 	
	{
		if (equalType($1, $3)) $$ = typeBoolean;
		else { 
			error("Type mismatch: t1 >= t2");
			return 1;
		}
	}

|	expr '#' expr
	{
		if ($1 == $3) {
			$$ = NULL; 
		}
		else {
			error("Type mismatch: t1 # t2 -> t1 # list [t1]");
			return 1;
		}
	}

|	"true"			
	{	$$ = typeBoolean; }

|	"false"	
	{	$$ = typeBoolean; }

|	"not" expr		
	{ 
		if ($2 == typeBoolean) $$ = $2; 
		else {
			error("Type mismatch: not t1 -> not bool"); 
			return 1; 
		} 
	}

|	expr "and" expr	
	{ 
		if ($1 == typeBoolean && equalType($1, $3)) $$ = $1; 
		else {
			error("Type mismatch: t1 and t2 -> bool and bool");
			return 1;
		}
	}

|	expr "or" expr	
	{
		if ($1 == typeBoolean && equalType($1, $3)) $$ = $1;
		else {
			error("Type mismatch: t1 or t2 -> bool or bool");
			return 1;
		}
	}

|	"new" type '[' expr ']'
	{
		if ($4 != typeInteger) {
			error("Type mismatch: new (type) [ t1 ] -> new (type) [ int ]");
			return 1;	
		}
		$$ = $2;
	}

|	"nil"	
	{	$$ = NULL; } //TODO list that equals to all types

|	"nil?" '(' expr ')'	
	{	
		if ($3->kind != TYPE_POINTER) {
			error("Type mismatch: nil? ( t1 ) -> nil? (list [ t1 ])");
			return 1;
		}
		$$ = typeBoolean;
	}

|	"head" '(' expr ')'	
	{	
		if ($3->kind != TYPE_POINTER) {
			error("Type mismatch: head ( t1 ) -> head (list [ t1 ])");
			return 1;
		}
		$$ = $3->refType; 
	}

|	"tail" '(' expr ')'
	{	
		if ($3->kind != TYPE_POINTER) {
			error("Type mismatch: tail ( t1 ) -> tail (list [ t1 ])");
			return 1;
		}
		$$ = typePointer($3->refType);
	}
;

		
%%


void yyerror(const char * msg) {
	fprintf(stderr, "Syntax error: %s, line: %d\n", msg, linecount);//ERROR(msg);
	exit(1);
}

// -O veltistopoihsh
// -i diavasma apo stdin kai output endiamesou kwdika sto stdout
// -f diavasma apo stdin kai output telikou kwdika sto stdout

// to arxeio prepei na teleiwnei se *.tony kai prepei na dhmiourghsw
// an to arxeio legetai fileName.tony -> fileName.imm kai fileName.asm

// Sto telos mporw na valw to synolo twn errors kai twn warnings, ton
// synoliko xrono kai thn synolikh mnhmh

int main(int argc, char **argv) {
	int ret;
	FILE *fd = fopen(argv[1], "r");

	atomo = 0;
	forw = refer = variable = false;
	start_in_func = end_in_func = NULL;
	start_check_args = end_check_args = NULL;
	filename = argv[1];

	dup2(fileno(fd), fileno(stdin));
	
	initSymbolTable(997);
	printSymbolTable();
	
	openScope();
	printSymbolTable();

	ret = yyparse();
	printSymbolTable();

	closeScope();
	printSymbolTable();

	destroySymbolTable();
	fclose(fd);

	printf("Return type = %d\n", ret);
	return ret;
}
