%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <unistd.h>

#include "symbol.h"
#include "error.h"
#include "general.h"

SymbolEntry *tmpPlace;           // Temporary symbol entry place
LabelList   *tmpL1;
LabelList   *tmpL2;
Type         tmpType;            // Temporary type
bool         fromFormal;         // Flag indicating variable decaration
bool         fromExpr;

%}

%union {
    const char  *name;
    Type         type;
    SymbolEntry *place;
    LabelList   *NEXT;
    
    struct {
        bool         can_assign;
        SymbolEntry *place;
    } lvalue;
    
    struct {
        Type         type;
        SymbolEntry *place;
        LabelList   *TRUE;
        LabelList   *FALSE;
    } cond;
}

/* Token declarations */

%token T_end     "end"
%token T_def     "def"
%token T_ref     "ref"
%token T_int     "int"
%token T_bool    "bool"
%token T_char    "char"
%token T_list    "list"
%token T_decl    "decl"
%token T_exit    "exit"
%token T_for     "for"
%token T_return  "return"
%token T_skip    "skip"
%token T_true    "true"
%token T_false   "false"
%token T_not     "not"
%token T_and     "and"
%token T_or      "or"
%token T_isnil   "nil?"
%token T_nil     "nil"
%token T_head    "head"
%token T_tail    "tail"
%token T_new     "new"
%token T_ne      "<>"
%token T_ge      ">="
%token T_le      "<="
%token T_assign  ":="
%token T_mod     "mod"
%token T_if      "if"
%token T_then    "then"
%token T_else    "else"
%token T_elsif   "elsif"

%token T_id
%token T_const_string
%token T_const_int
%token T_const_char

/* Precedence declarations */

%left         T_or
%left         T_and
%right        T_not
%nonassoc     '=' '<' '>' T_ne T_ge T_le
%right        '#'
%left         '+' '-'
%left         '*' '/' T_mod
%left         NEG POS

/* Terminal and non-terminal symbols and their types */

%type<name>   T_id T_const_string T_const_int T_const_char

%type<NEXT>   stmt stmt_plus
%type<type>   type
%type<cond>   expr_cond cond
%type<place>  expr
%type<lvalue> atom
//%type<name> var_star

%%


program
:   {
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
        
        fprintf(stderr, "\nFound %d errors.\n", errors);
        printQuads();
    }
;

func_def
:   "def" header ':' def_star 
    {
        genQuad("unit", "main", "-", "-");
    }
    stmt_plus "end"
    {
        genQuad("endu", "main", "-", "-");
    }
;

def_star
:   /* nothing */
|   func_def def_star
|   func_decl def_star
|   var_def { printSymbolTable(); } def_star
;

stmt_plus
:   stmt stmt_plus
|   stmt
;

header
:   type T_id '(' ')'
|   T_id '(' ')'
|   type T_id '(' formal formal_star ')'
|   T_id '(' formal formal_star ')'
;

formal_star
:   /* nothing */
|   ';' formal formal_star    
;

formal
:   "ref" type T_id 
    {
        // fromFormal = true;
        // passMode = PASS_BY_REFERENCE;
    }
    var_star
|   type T_id var_star
;

var_star
:   /* nothing */
|   ',' T_id 
    {
        if (fromFormal) {
        
        }
        else {
            if (!lookupEntry($2, LOOKUP_CURRENT_SCOPE, false))
                newVariable($2, tmpType);
            else
                error("Duplicate identifier: %s", $2);
        }
    }
    var_star
;

type
:   "int"                  { $$ = tmpType = typeInteger; }
|   "bool"                 { $$ = tmpType = typeBoolean; }
|   "char"                 { $$ = tmpType = typeChar; }
|   type '[' ']'           { $$ = tmpType = typeIArray($1); }
|   "list" '[' type ']'    { /* TODO typeList */ }
;

func_decl
:   "decl" header
;

var_def
:   type T_id 
    {
        fromFormal = false;
        
        if (!lookupEntry($2, LOOKUP_ALL_SCOPES, false)) {
            newVariable($2, $1);
            destroyType($1);
        }
        else
            error("Duplicate identifier: %s", $2);
    }
    var_star
;

stmt
:   simple
|   "exit"
|   "return" expr
|   "return" cond
|   "if" expr_cond 
    {
        if (!equalType($2.type, typeBoolean))
            error("type mismatch: expression after 'if' should be boolean");
            
        if (fromExpr)
            exprToCond($2.place, &($2.TRUE), &($2.FALSE));
        
        backpatch($2.TRUE, quadNext);
    }
    ':' stmt_plus elseif_star "end"
|   "for" simple_list ';' expr_cond ';' simple_list ':' stmt_plus "end"
;

elseif_star
:   else
|   "elsif" 
    {
        // TODO tha mporousa na kanw merge ola ta endiamesa jump kai sto telos backpatch
        $<NEXT>$ = makeList(quadNext);
        genQuad("jump", "-", "-", "*");
        backpatch($<cond>-4.FALSE, quadNext);
    }
    expr_cond ':'
    {        
        if (!equalType($3.type, typeBoolean))
            error("type mismatch: expression after 'elsif' should be boolean");
            
        if (fromExpr)
            exprToCond($3.place, &($3.TRUE), &($3.FALSE));
            
        backpatch($3.TRUE, quadNext);
    } 
    stmt_plus { backpatch($<NEXT>2, quadNext); } elseif_star
;

else
:   /* nothing */ { backpatch($<cond>-4.FALSE, quadNext); }
|   "else" ':'
    {
        // TODO tha mporousa na kanw merge ola ta endiamesa jump kai sto telos backpatch
        $<NEXT>$ = makeList(quadNext);
        genQuad("jump", "-", "-", "*");
        backpatch($<cond>-4.FALSE, quadNext);
    }
    stmt_plus { backpatch($<NEXT>3, quadNext); }
;

simple
:   "skip"
|   atom ":=" expr_cond
    {
        if (!$1.can_assign)
            error("cannot assign to atom");
                        
        if (!equalType(getType($1.place), $3.type))
            error("assignment should be between equal types");
            
        if (fromExpr)
            genQuad(":=", $3.place->id, "-", $1.place->id);
        else {
            backpatch($3.TRUE, quadNext);
            genQuad(":=", "true", "-", $1.place->id);
            int q = quadNext + 2;
            genQuad("jump", "-", "-", intToString(q));
            backpatch($3.FALSE, quadNext);
            genQuad(":=", "false", "-", $1.place->id);
        }
    }
|   call
;

simple_list
:   simple simple_star
;

simple_star
:   /* nothing */    
|   ',' simple simple_star
;

call
:   T_id '(' ')'
|   T_id '(' expr expr_star ')'
|   T_id '(' cond expr_star ')'
;

expr_star
:   /* nothing */    
|   ',' expr expr_star
|   ',' cond expr_star
;

atom
:   T_id
    {
        if (!(tmpPlace = lookupEntry($1, LOOKUP_ALL_SCOPES, true)))
            tmpPlace = newVariable($1, typeInteger);
            
        $$.place = tmpPlace;
        $$.can_assign = true;
    }
|   T_const_string
|   atom '[' expr ']'
|   call
;

expr_cond
:   expr
    {
        $$.place = $1;
        $$.type  = getType($1);
        fromExpr = true;
    }
|   cond
    {
        $$.place = $1.place;
        $$.type  = $1.type;
        $$.TRUE  = $1.TRUE;
        $$.FALSE = $1.FALSE;
        fromExpr = false;
    }
;

cond
:   '(' cond ')' 
    { 
        $$.place = $2.place;
        $$.type  = $2.type;
        $$.TRUE  = $2.TRUE;
        $$.FALSE = $2.FALSE;
    }
|   expr_cond '=' { condToExpr(&($1.place), $1.TRUE, $1.FALSE); } expr_cond
    {
        if (!(equalType($1.type, $4.type) && ($1.type == typeInteger || $1.type == typeBoolean || $1.type == typeChar)))
            error("type mismatch: t1 = t2. Comparison should be between same basic types");

        condToExpr(&($4.place), $4.TRUE, $4.FALSE);
        
        $$.place = NULL;
        $$.type  = typeBoolean;
        $$.TRUE  = makeList(quadNext);
        genQuad("=", $1.place->id, $4.place->id, "*");
        $$.FALSE = makeList(quadNext);
        genQuad("jump", "-", "-", "*");
    }
|   expr_cond '<' { condToExpr(&($1.place), $1.TRUE, $1.FALSE); } expr_cond
    {        
        if (!(equalType($1.type, $4.type) && ($1.type == typeInteger || $1.type == typeBoolean || $1.type == typeChar)))
            error("type mismatch: t1 < t2. Comparison should be between same basic types");
        
        condToExpr(&($4.place), $4.TRUE, $4.FALSE);
        
        $$.place = NULL;
        $$.type  = typeBoolean;
        $$.TRUE  = makeList(quadNext);
        genQuad("<", $1.place->id, $4.place->id, "*");
        $$.FALSE = makeList(quadNext);
        genQuad("jump", "-", "-", "*");
    }
|   expr_cond '>' { condToExpr(&($1.place), $1.TRUE, $1.FALSE); } expr_cond
    {
        if (!(equalType($1.type, $4.type) && ($1.type == typeInteger || $1.type == typeBoolean || $1.type == typeChar)))
            error("type mismatch: t1 > t2. Comparison should be between same basic types");
        
        condToExpr(&($4.place), $4.TRUE, $4.FALSE);
        
        $$.place = NULL;
        $$.type  = typeBoolean;
        $$.TRUE  = makeList(quadNext);
        genQuad(">", $1.place->id, $4.place->id, "*");
        $$.FALSE = makeList(quadNext);
        genQuad("jump", "-", "-", "*");
    }
|   expr_cond "<>" { condToExpr(&($1.place), $1.TRUE, $1.FALSE); } expr_cond
    {
        if (!(equalType($1.type, $4.type) && ($1.type == typeInteger || $1.type == typeBoolean || $1.type == typeChar)))
            error("type mismatch: t1 <> t2. Comparison should be between same basic types");
        
        condToExpr(&($4.place), $4.TRUE, $4.FALSE);
        
        $$.place = NULL;
        $$.type  = typeBoolean;
        $$.TRUE  = makeList(quadNext);
        genQuad("<>", $1.place->id, $4.place->id, "*");
        $$.FALSE = makeList(quadNext);
        genQuad("jump", "-", "-", "*");
    }
|   expr_cond "<=" { condToExpr(&($1.place), $1.TRUE, $1.FALSE); } expr_cond
    {
        if (!(equalType($1.type, $4.type) && ($1.type == typeInteger || $1.type == typeBoolean || $1.type == typeChar)))
            error("type mismatch: t1 <= t2. Comparison should be between same basic types");
        
        condToExpr(&($4.place), $4.TRUE, $4.FALSE);
        
        $$.place = NULL;
        $$.type  = typeBoolean;
        $$.TRUE  = makeList(quadNext);
        genQuad("<=", $1.place->id, $4.place->id, "*");
        $$.FALSE = makeList(quadNext);
        genQuad("jump", "-", "-", "*");
    }
|   expr_cond ">=" { condToExpr(&($1.place), $1.TRUE, $1.FALSE); } expr_cond
    {
        if (!(equalType($1.type, $4.type) && ($1.type == typeInteger || $1.type == typeBoolean || $1.type == typeChar)))
            error("type mismatch: t1 >= t2. Comparison should be between same basic types");
        
        condToExpr(&($4.place), $4.TRUE, $4.FALSE);
        
        $$.place = NULL;
        $$.type  = typeBoolean;
        $$.TRUE  = makeList(quadNext);
        genQuad(">=", $1.place->id, $4.place->id, "*");
        $$.FALSE = makeList(quadNext);
        genQuad("jump", "-", "-", "*");
    }
|   "not" expr_cond
    {
        if ($2.type != typeBoolean)
            error("operator and operand don't agree: not t1 -> not bool");
        
        if (fromExpr) exprToCond($2.place, &($2.TRUE), &($2.FALSE));
        
        $$.place = $2.place;
        $$.type  = typeBoolean;
        $$.TRUE  = $2.FALSE;
        $$.FALSE = $2.TRUE;
    }
|   expr_cond "and" 
    {
        if ($1.type != typeBoolean)
            error("operator and operand don't agree: t1 and t2 -> bool and bool");

        if (fromExpr) exprToCond($1.place, &($1.TRUE), &($1.FALSE));
        backpatch($1.TRUE, quadNext);
    }
    expr_cond
    {
        if ($4.type != typeBoolean)
            error("operator and operand don't agree: t1 and t2 -> bool and bool");
            
        if (fromExpr) exprToCond($4.place, &($4.TRUE), &($4.FALSE));
        
        $$.type  = typeBoolean;
        $$.TRUE  = $4.TRUE;
        $$.FALSE = merge($1.FALSE, $4.FALSE);
    }
|   expr_cond "or" 
    {
        if ($1.type != typeBoolean)
            error("operator and operand don't agree: t1 or t2 -> bool or bool");

        if (fromExpr) exprToCond($1.place, &($1.TRUE), &($1.FALSE));
        backpatch($1.FALSE, quadNext);
    }
    expr_cond
    {
        if ($4.type != typeBoolean)
            error("operator and operand don't agree: t1 or t2 -> bool or bool");

        if (fromExpr) exprToCond($4.place, &($4.TRUE), &($4.FALSE));
        
        $$.type  = typeBoolean;
        $$.TRUE  = merge($1.TRUE, $4.TRUE);
        $$.FALSE = $4.FALSE;
    }
;

expr
:   atom            { $$ = $1.place; }
|   T_const_int     { $$ = newConstant($1, typeInteger); }
|   T_const_char    { $$ = newConstant($1, typeChar); }
|   '(' expr ')'    { $$ = $2; }
|   "true"          { $$ = newConstant("true", typeBoolean); }
|   "false"         { $$ = newConstant("false", typeBoolean); }
|   '+' expr %prec POS
    { 
        if (getType($2) != typeInteger)
            error("operator and operand don't agree: + t1 -> + int");
        
        $$ = $2; 
    }
|   '-' expr %prec NEG
    {
        if (getType($2) != typeInteger)
            error("operator and operand don't agree: - t1 -> - int");
            
        $$ = newTemporary(typeInteger);
        genQuad("-", $2->id, "-", $$->id);
    }
|   expr '+' expr
    {
        if (getType($1) != typeInteger || getType($3) != typeInteger)
            error("operator and operand don't agree: t1 + t2 -> int + int");
            
        $$ = newTemporary(typeInteger);
        genQuad("+", $1->id, $3->id, $$->id);
    }
|   expr '-' expr
    {
        if (getType($1) != typeInteger || getType($3) != typeInteger)
            error("operator and operand don't agree: t1 - t2 -> int - int");
            
        $$ = newTemporary(typeInteger);
        genQuad("-", $1->id, $3->id, $$->id);
    }
|   expr '*' expr 
    {
        if (getType($1) != typeInteger || getType($3) != typeInteger)
            error("operator and operand don't agree: t1 * t2 -> int * int");
            
        $$ = newTemporary(typeInteger);
        genQuad("*", $1->id, $3->id, $$->id);
    }
|   expr '/' expr
    {
        if (getType($1) != typeInteger || getType($3) != typeInteger)
            error("operator and operand don't agree: t1 / t2 -> int / int");
            
        $$ = newTemporary(typeInteger);
        genQuad("/", $1->id, $3->id, $$->id);
    }
|   expr "mod" expr
    {
        if (getType($1) != typeInteger || getType($3) != typeInteger)
            error("operator and operand don't agree: t1 mod t2 -> int mod int");
            
        $$ = newTemporary(typeInteger);
        genQuad("mod", $1->id, $3->id, $$->id);
    }
|   expr '#' expr
|   "new" type '[' expr ']'
|   "nil"
|   "nil?" '(' expr ')'
|   "head" '(' expr ')'
|   "tail" '(' expr ')'
;

        
%%


void yyerror(const char *msg) {
    fprintf(stderr, "syntax error: %s, line: %d\n", msg, linecount);
    exit(1);
}

int main(int argc, char **argv) {
    FILE *fd = fopen(argv[1], "r");
    filename = argv[1];
    dup2(fileno(fd), fileno(stdin));
    
    printf("Size of symbolEntry * = %ld\n", sizeof(SymbolEntry *));
    printf("Size of int           = %ld\n", sizeof(int));
    printf("Size of char          = %ld\n", sizeof(char));
    printf("Size of const char *  = %ld\n\n\n\n", sizeof(const char *));
        
    return yyparse();
}
