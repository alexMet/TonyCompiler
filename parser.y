%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#include "symbol.h"
#include "error.h"
#include "general.h"

SymbolEntry *tmpPlace;           // Temporary symbol entry place
Type         tmpType;            // Temporary type
bool         fromFormal;         // Flag indicating variable decaration
bool         fromExpr;
PassMode     tmpPassMode;

%}

%union {
    const char      *name;
    Type             type;
    SymbolEntry     *place;
    LabelList       *NEXT;
    unsigned int     tmpQuadNext;
    
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

%type<type>   type
%type<cond>   expr_cond cond
%type<place>  expr header call
%type<lvalue> atom
//%type<name> var_star

%%


program
:   {
        initSymbolTable(SYMBOL_TABLE_SIZE);
        printSymbolTable();
    
        openScope(NULL);
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
        genQuad("unit", $2->id, "-", "-");
    }
    stmt_plus "end"
    {
        genQuad("endu", $2->id, "-", "-");
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
    {
        tmpPlace = newFunction($2);
        openScope($1);
        printSymbolTable();
        endFunctionHeader(tmpPlace, $1);
        $$ = tmpPlace;
    }
|   T_id '(' ')'
    {
        tmpPlace = newFunction($1);
        openScope(typeVoid);
        printSymbolTable();
        endFunctionHeader(tmpPlace, typeVoid);
        $$ = tmpPlace;
    }
|   type T_id '('
    {
        tmpPlace = newFunction($2);
        openScope($1);
        printSymbolTable();
    }
    formal formal_star ')'
    {
        printSymbolTable();
        endFunctionHeader(tmpPlace, $1);
        $$ = tmpPlace;
    }
|   T_id '('
    {       
        tmpPlace = newFunction($1);
        openScope(typeVoid);
        printSymbolTable();
    }
    formal formal_star ')'
    {
        printSymbolTable();
        endFunctionHeader(tmpPlace, typeVoid);
        $$ = tmpPlace;
    }
;

formal_star
:   /* nothing */
|   ';' formal formal_star    
;

formal
:   "ref" type T_id
    {
        fromFormal = true;
        tmpPassMode = PASS_BY_REFERENCE;
        
        newParameter($3, $2, PASS_BY_REFERENCE, tmpPlace);
    }
    var_star
|   type T_id
    {
        fromFormal = true;
        tmpPassMode = PASS_BY_VALUE;
        
        newParameter($2, $1, PASS_BY_VALUE, tmpPlace);
    }
    var_star
;

var_star
:   /* nothing */
|   ',' T_id 
    {
        if (fromFormal)
            newParameter($2, tmpType, tmpPassMode, tmpPlace);
        else
            newVariable($2, tmpType);
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
    {
        forwardFunction($2);
        closeScope();
    }
;

var_def
:   type T_id
    {
        fromFormal = false;
        
        newVariable($2, $1);
    }
    var_star
;

stmt
:   simple
|   "exit" 
    {
        if (currentScope->returnType != typeVoid)
            error("cannot call 'exit' from a non void function");
            
        genQuad("ret", "-", "-", "-");
    }
|   "return" expr_cond
    {
        if (!equalType(currentScope->returnType, $2.type))
            error("type mismatch: between 'return' expression type and function return type");
        
        if (fromExpr)
            genQuad("retv", $2.place->id, "-", "-");
        else {
            backpatch($2.TRUE, quadNext);
            genQuad("retv", "true", "-", "-");
            int q = quadNext + 2;
            genQuad("jump", "-", "-", intToString(q));
            backpatch($2.FALSE, quadNext);
            genQuad("retv", "false", "-", "-");
        }
    }
|   "if" expr_cond ':'
    {
        if (!equalType($2.type, typeBoolean))
            error("type mismatch: expression after 'if' should be boolean");
            
        if (fromExpr)
            exprToCond($2.place, &($2.TRUE), &($2.FALSE));
        
        backpatch($2.TRUE, quadNext);
    }
    stmt_plus { /* empty in order for expr_cond to be at place -4 */ } elseif_star "end"
|   "for" simple_list ';' { $<tmpQuadNext>$ = quadNext; } expr_cond ';'
    {
        if (!equalType($5.type, typeBoolean))
            error("type mismatch: expression after 'if' should be boolean");
            
        if (fromExpr)
            exprToCond($5.place, &($5.TRUE), &($5.FALSE));
        
        backpatch($5.TRUE, quadNext);
        $<tmpQuadNext>$ = quadNext;
    }
    simple_list ':'
    {
        genQuad("jump", "-", "-", intToString($<tmpQuadNext>4));
        backpatch($5.TRUE, quadNext);
    } 
    stmt_plus 
    {
        genQuad("jump", "-", "-", intToString($<tmpQuadNext>7));
        backpatch($5.FALSE, quadNext);
    }
    "end"
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
    {
        // TODO get the function's place
        if (getType($1) != typeVoid)
            warning("ignoring a non void function's return value");
    }
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
    {
        if (!($$ = lookupEntry($1, LOOKUP_ALL_SCOPES, true)))
            $$ = newVariable($1, typeInteger); // TODO den yparxei h synarthsh
            
        if ($$->entryType != ENTRY_FUNCTION)
            error("***");
            
        if (getType($$) != typeVoid) {
            $$ = newTemporary(getType($$));
            genQuad("par", $$->id, "RET", "-");
        }
            
        genQuad("call", "-", "-", $1);
        
        // TODO check function parameters
    }
|   T_id '('
    {
        // TODO fix this because a call can contain a call and it will overide the tmpPlace
        
        if (!(tmpPlace = lookupEntry($1, LOOKUP_ALL_SCOPES, true)))
            tmpPlace = newVariable($1, typeInteger); // TODO den yparxei h synarthsh
            
        if (tmpPlace->entryType != ENTRY_FUNCTION)
            error("***");
            
        // TODO check function parameters
    }
    expr_cond
    {
        if (fromExpr)
            genQuad("par", $4.place->id, "?", "-");
        else {
            // TODO if pass by ref then maybe error?
            backpatch($4.TRUE, quadNext);
            genQuad("par", "true", "?", "-");
            int q = quadNext + 2;
            genQuad("jump", "-", "-", intToString(q));
            backpatch($4.FALSE, quadNext);
            genQuad("par", "false", "?", "-");
        }
    }
    expr_star ')'
    {
        if (getType(tmpPlace) != typeVoid) {
            $$ = newTemporary(getType(tmpPlace));
            genQuad("par", $$->id, "RET", "-");
        }
            
        genQuad("call", "-", "-", $1);
    }
;

expr_star
:   /* nothing */ // TODO check if exausted all function arguments
|   ',' expr_cond
    {
        // TODO check types between function argument and expr
        if (fromExpr)
            genQuad("par", $2.place->id, "?", "-");
        else {
            // TODO if pass by ref then maybe error?
            backpatch($2.TRUE, quadNext);
            genQuad("par", "true", "?", "-");
            int q = quadNext + 2;
            genQuad("jump", "-", "-", intToString(q));
            backpatch($2.FALSE, quadNext);
            genQuad("par", "false", "?", "-");
        }
    }
    expr_star
;

atom
:   T_id
    {
        if (!($$.place = lookupEntry($1, LOOKUP_ALL_SCOPES, true)))
            $$.place = newVariable($1, typeInteger); // TODO den yparxei to id
            
        $$.can_assign = true;
    }
|   T_const_string
|   atom '[' expr ']'
|   call
    {
        $$.place      = $1;
        $$.can_assign = false;
    }
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
    error("syntax error %s", msg);        
    exit(1);
}

int main(int argc, char **argv) {
    initFiles(argc, argv);
        
    printf("Size of symbolEntry * = %ld\n", sizeof(SymbolEntry *));
    printf("Size of int           = %ld\n", sizeof(int));
    printf("Size of char          = %ld\n", sizeof(char));
    printf("Size of const char *  = %ld\n\n", sizeof(const char *));
        
    return yyparse();
}
