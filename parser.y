%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <signal.h>

#include "symbol.h"
#include "error.h"
#include "general.h"
#include "quad.h"

// TODO Το κύριο πρόγραμμα είναι μία δομική μονάδα που δεν επιστρέφει αποτέλεσμα και δε δέχεται παραμέτρους.
// TODO Αν η τυπική παράμετρος είναι τύπου t και περνά κατ’ αξία, τότε η αντίστοιχη πραγματική παράμετρος πρέπει να είναι έκφραση τύπου t.
// TODO Αν η τυπική παράμετρος είναι τύπου t και περνά κατ’ αναφορά, τότε η αντίστοιχη πραγματική παράμετρος πρέπει να είναι l-value τύπου t.
// TODO Η εντολή return πρέπει να εμφανίζεται στο σώμα μίας δομικής μονάδας.
// TODO Library functions: Είναι ορατές σε κάθε δομική μονάδα, εκτός αν επισκιάζονται από μεταβλητές, παραμέτρους ή άλλες δομικές μονάδες με το ίδιο όνομα.
// TODO add my_exit() so i can remove the .imm and .asm files
// TODO -O optimizations on imm code (e.x. copy probagation, algebraic transformation...)
// TODO Sto telos mporw na valw ton synoliko xrono kai thn synolikh mnhmh


SymbolEntry *tmpPlace;          // Temporary symbol entry place
Type         tmpType;           // Temporary type
bool         fromFormal;        // Flag indicating variable decaration
bool         fromExpr;          // Flag indicating that expr_cond is an expression or not
PassMode     tmpPassMode;       // Temporary pass mode for function's formal parameter 

Type         typeIArrayAny;     // Type checking for any type of array
Type         typeListAny;       // Type checking for any type of list

%}

%union {
    const char      *name;
    Type             type;
    SymbolEntry     *place;
    LabelList       *NEXT;
    unsigned int     quadNext;
        
    struct {
        bool         can_assign;
        SymbolEntry *place;
    } lvalue;
    
    struct {
        int          argCnt;
        const char  *fname;
        SymbolEntry *curArg;
    } call;
    
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


%%


program
:   {
        typeIArrayAny = typeIArray(typeAny);
        typeListAny   = typeList(typeAny);
        initSymbolTable(SYMBOL_TABLE_SIZE);
        printSymbolTable();
        openScope(NULL);
        initLibFuns();
        printSymbolTable();
    }
    func_def
    {
        closeScope();
        destroySymbolTable();
        destroyType(typeIArrayAny);
        destroyType(typeListAny);
    }
;

func_def
:   "def" header ':' def_star 
    {
        genQuad(QUNIT, newSymbolOperand($2), opNull, opNull);
    }
    stmt_plus "end"
    {
        genQuad(QENDU, newSymbolOperand($2), opNull, opNull);
        
        printQuads();
        
        closeScope();
    }
;

def_star
:   /* nothing */
|   func_def def_star
|   func_decl def_star
|   var_def def_star
;

stmt_plus
:   stmt stmt_plus
|   stmt
;

header
:   type T_id '(' ')'
    {
        $$ = tmpPlace = newFunction($2);
        openScope($1);
        endFunctionHeader(tmpPlace, $1);
    }
|   T_id '(' ')'
    {
        $$ = tmpPlace = newFunction($1);
        openScope(typeVoid);
        endFunctionHeader(tmpPlace, typeVoid);
    }
|   type T_id '('
    {
        tmpPlace = newFunction($2);
        openScope($1);
    }
    formal formal_star ')'
    {
        endFunctionHeader(tmpPlace, $1);
        $$ = tmpPlace;
    }
|   T_id '('
    {       
        tmpPlace = newFunction($1);
        openScope(typeVoid);
    }
    formal formal_star ')'
    {
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
        newParameter($3, $2, tmpPassMode, tmpPlace);
    }
    var_star
|   type T_id
    {
        fromFormal = true;
        tmpPassMode = (equalType($1, typeIArrayAny) || equalType($1, typeListAny)) ? PASS_BY_REFERENCE : PASS_BY_VALUE;
        newParameter($2, $1, tmpPassMode, tmpPlace);
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
|   "list" '[' type ']'    { $$ = tmpType = typeList($3); }
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
            
        genQuad(QRET, opNull, opNull, opNull);
    }
|   "return" expr_cond
    {
        if (currentScope->returnType == typeVoid)
            error("trying to return a value from a void function");
            
        if (!equalType(currentScope->returnType, $2.type))
            error("type mismatch: between 'return' expression type and function return type");
        
        if (fromExpr)
            genQuad(QRETV, newSymbolOperand($2.place), opNull, opNull);
        else {
            backpatch($2.TRUE, quadNext);
            genQuad(QRETV, newConstantOperand(typeBoolean, "true"), opNull, opNull);
            genQuad(QJMP, opNull, opNull, newLabelOperand(quadNext + 2));
            backpatch($2.FALSE, quadNext);
            genQuad(QRETV, newConstantOperand(typeBoolean, "false"), opNull, opNull);
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
|   "for" simple_list ';' { $<quadNext>$ = quadNext; } expr_cond ';'
    {
        if (!equalType($5.type, typeBoolean))
            error("type mismatch: expression after 'if' should be boolean");
            
        if (fromExpr)
            exprToCond($5.place, &($5.TRUE), &($5.FALSE));
        
        $<quadNext>$ = quadNext;
    }
    simple_list ':'
    {
        genQuad(QJMP, opNull, opNull, newLabelOperand($<quadNext>4));
        backpatch($5.TRUE, quadNext);
    } 
    stmt_plus 
    {
        genQuad(QJMP, opNull, opNull, newLabelOperand($<quadNext>7));
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
        genQuad(QJMP, opNull, opNull, opStar);
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
        genQuad(QJMP, opNull, opNull, opStar);
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
            genQuad(QASSIGN, newSymbolOperand($3.place), opNull, newSymbolOperand($1.place));
        else {
            backpatch($3.TRUE, quadNext);
            genQuad(QASSIGN, newConstantOperand(typeBoolean, "true"), opNull, newSymbolOperand($1.place));
            genQuad(QJMP, opNull, opNull, newLabelOperand(quadNext + 2));
            backpatch($3.FALSE, quadNext);
            genQuad(QASSIGN, newConstantOperand(typeBoolean, "false"), opNull, newSymbolOperand($1.place));
        }
    }
|   call
    {
        if (getType($1) != typeAny)
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
        if (!($$ = lookupEntry($1, LOOKUP_ALL_SCOPES, true))) {
            $$ = newFunction($1); // TODO den yparxei h synarthsh
            endFunctionHeader($$, typeAny);
        }
            
        if ($$->entryType == ENTRY_FUNCTION) {
            if ($$->u.eFunction.firstArgument)
                error("passed fewer arguments than expected to function '%s'", $$->id);
                
            if (getType($$) != typeVoid) {
                $$ = newTemporary(getType($$));
                genQuad(QPAR, newSymbolOperand($$), opRet, opNull);
            }
            else
                $$ = newTemporary(typeAny);
                
            genQuad(QCALL, opNull, opNull, newSymbolOperand($$));
        }
        else
            error("***");
    }
|   T_id '('
    {
        if (!($<place>$ = lookupEntry($1, LOOKUP_ALL_SCOPES, true))) {
            $<place>$ = newFunction($1); // TODO den yparxei h synarthsh
            endFunctionHeader($<place>$, typeAny);
        }
        
        if ($<place>$->entryType != ENTRY_FUNCTION)
            error("***");
    }
    expr_cond
    {
        if ($<place>3->entryType == ENTRY_FUNCTION) {
            SymbolEntry *c = $<place>3->u.eFunction.firstArgument;
            if (c) {
                if (!equalType(getType(c), $4.type))
                    error("type mismatch: in argument %d of function '%s'", 1, $<place>3->id);
            }
            else
                error("passed more arguments than expected to function '%s'", $<place>3->id);
                            
            $<call>$.argCnt = 2;
            $<call>$.fname  = $<place>3->id;
            $<call>$.curArg = (c) ? c->u.eParameter.next : NULL;
            
            if (fromExpr)
                genQuad(QPAR, newSymbolOperand($4.place), passModeToOperand(c), opNull);
            else {
                // TODO if pass by ref then maybe error?
                backpatch($4.TRUE, quadNext);
                genQuad(QPAR, newConstantOperand(typeBoolean, "true"), passModeToOperand(c), opNull);
                genQuad(QJMP, opNull, opNull, newLabelOperand(quadNext + 2));
                backpatch($4.FALSE, quadNext);
                genQuad(QPAR, newConstantOperand(typeBoolean, "false"), passModeToOperand(c), opNull);
            }
        }
    }
    expr_star ')'
    {
        if (getType($<place>3) != typeVoid) {
            $$ = newTemporary(getType($<place>3));
            genQuad(QPAR, newSymbolOperand($$), opRet, opNull);
        }
        else
            $$ = newTemporary(typeAny);
            
        genQuad(QCALL, opNull, opNull, newSymbolOperand($<place>3));
    }
;

expr_star
:   /* nothing */ {
        if ($<call>0.fname != NULL && $<call>0.curArg)
            error("passed fewer arguments than expected to function '%s'", $<call>0.fname);
    }   
|   ',' expr_cond
    {
        if ($<call>0.fname) {
            SymbolEntry *c = $<call>0.curArg;
            if (c) {
                if (!equalType(getType(c), $2.type))
                    error("type mismatch: in argument %d of function '%s'", $<call>0.argCnt, $<call>0.fname);
            }
            else
                error("passed more arguments than expected to function '%s'", $<call>0.fname);

            $<call>$.argCnt = ++$<call>0.argCnt;
            $<call>$.fname  = $<call>0.fname;
            $<call>$.curArg = (c) ? c->u.eParameter.next : NULL;
            
            if (fromExpr)
                genQuad(QPAR, newSymbolOperand($2.place), passModeToOperand(c), opNull);
            else {
                // TODO if pass by ref then maybe error?
                backpatch($2.TRUE, quadNext);
                genQuad(QPAR, newConstantOperand(typeBoolean, "true"), passModeToOperand(c), opNull);
                genQuad(QJMP, opNull, opNull, newLabelOperand(quadNext + 2));
                backpatch($2.FALSE, quadNext);
                genQuad(QPAR, newConstantOperand(typeBoolean, "false"), passModeToOperand(c), opNull);
            }
        }
    }
    expr_star
;

atom
:   T_id
    {
        if (!($$.place = lookupEntry($1, LOOKUP_ALL_SCOPES, true)))
            $$.place = newVariable($1, typeAny); // TODO den yparxei to id
            
        $$.can_assign = true;
    }
|   T_const_string
    {
        $$.place = newConstant(typeIArray(typeChar), $1);
        $$.can_assign = false;
    }
|   atom '[' expr_cond ']'
    {
        // TODO constant
        if (!equalType($3.type, typeInteger))
            error("type mismatch: expression in brackets should be int");
        
        if (!equalType(getType($1.place), typeIArrayAny))
            error("type mismatch: atom isn't an array t[expr] -> array[expr]");
            
        SymbolEntry *e = newTemporary(getType($1.place)->refType);
        genQuad(QARRAY, newSymbolOperand($1.place), newSymbolOperand($3.place), newSymbolOperand(e));
        
        char buffer[10];
        sprintf(buffer, "[$%d]", e->u.eTemporary.number);
        delete((char *) e->id);
        e->id = strdup(buffer);
        
        $$.place      = e;
        $$.can_assign = true;
    }
|   call
    {
        if (getType($1) == typeAny)
            error("trying to get return value from a void function");
            
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
        if (!(equalType($1.type, $4.type) && isBasicType($1.type)))
            error("type mismatch: t1 = t2. Comparison should be between same basic types");

        condToExpr(&($4.place), $4.TRUE, $4.FALSE);
        
        $$.place = NULL;
        $$.type  = typeBoolean;
        $$.TRUE  = makeList(quadNext);
        genQuad(QEQ, newSymbolOperand($1.place), newSymbolOperand($4.place), opStar);
        $$.FALSE = makeList(quadNext);
        genQuad(QJMP, opNull, opNull, opStar);
    }
|   expr_cond '<' { condToExpr(&($1.place), $1.TRUE, $1.FALSE); } expr_cond
    {        
        if (!(equalType($1.type, $4.type) && isBasicType($1.type)))
            error("type mismatch: t1 < t2. Comparison should be between same basic types");
        
        condToExpr(&($4.place), $4.TRUE, $4.FALSE);
        
        $$.place = NULL;
        $$.type  = typeBoolean;
        $$.TRUE  = makeList(quadNext);
        genQuad(QLT, newSymbolOperand($1.place), newSymbolOperand($4.place), opStar);
        $$.FALSE = makeList(quadNext);
        genQuad(QJMP, opNull, opNull, opStar);
    }
|   expr_cond '>' { condToExpr(&($1.place), $1.TRUE, $1.FALSE); } expr_cond
    {
        if (!(equalType($1.type, $4.type) && isBasicType($1.type)))
            error("type mismatch: t1 > t2. Comparison should be between same basic types");
        
        condToExpr(&($4.place), $4.TRUE, $4.FALSE);
        
        $$.place = NULL;
        $$.type  = typeBoolean;
        $$.TRUE  = makeList(quadNext);
        genQuad(QGT, newSymbolOperand($1.place), newSymbolOperand($4.place), opStar);
        $$.FALSE = makeList(quadNext);
        genQuad(QJMP, opNull, opNull, opStar);
    }
|   expr_cond "<>" { condToExpr(&($1.place), $1.TRUE, $1.FALSE); } expr_cond
    {
        if (!(equalType($1.type, $4.type) && isBasicType($1.type)))
            error("type mismatch: t1 <> t2. Comparison should be between same basic types");
        
        condToExpr(&($4.place), $4.TRUE, $4.FALSE);
        
        $$.place = NULL;
        $$.type  = typeBoolean;
        $$.TRUE  = makeList(quadNext);
        genQuad(QNE, newSymbolOperand($1.place), newSymbolOperand($4.place), opStar);
        $$.FALSE = makeList(quadNext);
        genQuad(QJMP, opNull, opNull, opStar);
    }
|   expr_cond "<=" { condToExpr(&($1.place), $1.TRUE, $1.FALSE); } expr_cond
    {
        if (!(equalType($1.type, $4.type) && isBasicType($1.type)))
            error("type mismatch: t1 <= t2. Comparison should be between same basic types");
        
        condToExpr(&($4.place), $4.TRUE, $4.FALSE);
        
        $$.place = NULL;
        $$.type  = typeBoolean;
        $$.TRUE  = makeList(quadNext);
        genQuad(QLE, newSymbolOperand($1.place), newSymbolOperand($4.place), opStar);
        $$.FALSE = makeList(quadNext);
        genQuad(QJMP, opNull, opNull, opStar);
    }
|   expr_cond ">=" { condToExpr(&($1.place), $1.TRUE, $1.FALSE); } expr_cond
    {
        if (!(equalType($1.type, $4.type) && isBasicType($1.type)))
            error("type mismatch: t1 >= t2. Comparison should be between same basic types");
        
        condToExpr(&($4.place), $4.TRUE, $4.FALSE);
        
        $$.place = NULL;
        $$.type  = typeBoolean;
        $$.TRUE  = makeList(quadNext);
        genQuad(QGE, newSymbolOperand($1.place), newSymbolOperand($4.place), opStar);
        $$.FALSE = makeList(quadNext);
        genQuad(QJMP, opNull, opNull, opStar);
    }
|   "not" expr_cond
    {
        if (!equalType($2.type, typeBoolean))
            error("operator and operand don't agree: not t1 -> not bool");
        
        if (fromExpr) exprToCond($2.place, &($2.TRUE), &($2.FALSE));
        
        $$.place = $2.place;
        $$.type  = typeBoolean;
        $$.TRUE  = $2.FALSE;
        $$.FALSE = $2.TRUE;
    }
|   expr_cond "and" 
    {
        if (!equalType($1.type, typeBoolean))
            error("operator and operand don't agree: t1 and t2 -> bool and bool");

        if (fromExpr) exprToCond($1.place, &($1.TRUE), &($1.FALSE));
        backpatch($1.TRUE, quadNext);
    }
    expr_cond
    {
        if (!equalType($4.type, typeBoolean))
            error("operator and operand don't agree: t1 and t2 -> bool and bool");
            
        if (fromExpr) exprToCond($4.place, &($4.TRUE), &($4.FALSE));
        
        $$.type  = typeBoolean;
        $$.TRUE  = $4.TRUE;
        $$.FALSE = merge($1.FALSE, $4.FALSE);
    }
|   expr_cond "or" 
    {
        if (!equalType($1.type, typeBoolean))
            error("operator and operand don't agree: t1 or t2 -> bool or bool");

        if (fromExpr) exprToCond($1.place, &($1.TRUE), &($1.FALSE));
        backpatch($1.FALSE, quadNext);
    }
    expr_cond
    {
        if (!equalType($4.type, typeBoolean))
            error("operator and operand don't agree: t1 or t2 -> bool or bool");

        if (fromExpr) exprToCond($4.place, &($4.TRUE), &($4.FALSE));
        
        $$.type  = typeBoolean;
        $$.TRUE  = merge($1.TRUE, $4.TRUE);
        $$.FALSE = $4.FALSE;
    }
|   "nil?" '(' expr_cond ')'
    {
        if (!equalType($3.type, typeListAny))
            error("operator and operand don't agree: nil?(t1) -> nil?(list)");
            
        $$.place = NULL;
        $$.type  = typeBoolean;
        $$.TRUE  = makeList(quadNext);
        genQuad(QEQ, newSymbolOperand($3.place), newConstantOperand(typeListAny, "nil"), opStar);
        $$.FALSE = makeList(quadNext);
        genQuad(QJMP, opNull, opNull, opStar);
    }
;

expr
:   atom            { $$ = $1.place; }
|   T_const_int     { $$ = newConstant(typeInteger, $1); }
|   T_const_char    { $$ = newConstant(typeChar, $1); }
|   '(' expr ')'    { $$ = $2; }
|   "true"          { $$ = newConstant(typeBoolean, "true"); }
|   "false"         { $$ = newConstant(typeBoolean, "false"); }
|   '+' expr %prec POS
    { 
        if (!equalType(getType($2), typeInteger))
            error("operator and operand don't agree: + t1 -> + int");
        
        $$ = $2; 
    }
|   '-' expr %prec NEG
    {
        if (!equalType(getType($2), typeInteger))
            error("operator and operand don't agree: - t1 -> - int");
            
        $$ = newTemporary(typeInteger);
        genQuad(QMINUS, newSymbolOperand($2), opNull, newSymbolOperand($$));
    }
|   expr '+' expr
    {
        if (!equalType(getType($1), typeInteger) || !equalType(getType($3), typeInteger))
            error("operator and operand don't agree: t1 + t2 -> int + int");
            
        $$ = newTemporary(typeInteger);
        genQuad(QPLUS, newSymbolOperand($1), newSymbolOperand($3), newSymbolOperand($$));
    }
|   expr '-' expr
    {
        if (!equalType(getType($1), typeInteger) || !equalType(getType($3), typeInteger))
            error("operator and operand don't agree: t1 - t2 -> int - int");
            
        $$ = newTemporary(typeInteger);
        genQuad(QMINUS, newSymbolOperand($1), newSymbolOperand($3), newSymbolOperand($$));
    }
|   expr '*' expr 
    {
        if (!equalType(getType($1), typeInteger) || !equalType(getType($3), typeInteger))
            error("operator and operand don't agree: t1 * t2 -> int * int");
            
        $$ = newTemporary(typeInteger);
        genQuad(QMULT, newSymbolOperand($1), newSymbolOperand($3), newSymbolOperand($$));
    }
|   expr '/' expr
    {
        if (!equalType(getType($1), typeInteger) || !equalType(getType($3), typeInteger))
            error("operator and operand don't agree: t1 / t2 -> int / int");
            
        $$ = newTemporary(typeInteger);
        genQuad(QDIV, newSymbolOperand($1), newSymbolOperand($3), newSymbolOperand($$));
    }
|   expr "mod" expr
    {
        if (!equalType(getType($1), typeInteger) || !equalType(getType($3), typeInteger))
            error("operator and operand don't agree: t1 mod t2 -> int mod int");
            
        $$ = newTemporary(typeInteger);
        genQuad(QMOD, newSymbolOperand($1), newSymbolOperand($3), newSymbolOperand($$));
    }
|   expr '#' expr
    {
        if (!equalType(getType($3), typeListAny) && !equalType(getType($1), getType($3)->refType))
            error("type mismatch in list construction: t # list[t]");

        // TODO check if consp or consv is redifined
        // TODO if not a list then create one?
        $$ = newTemporary(typeList(getType($1)));
        bool isConsp = equalType(getType($1), typeIArrayAny) || equalType(getType($1), typeListAny);
        SymbolEntry *f = lookupEntry((isConsp) ? "consp" : "consv", LOOKUP_ALL_SCOPES, false);

        genQuad(QPAR, newSymbolOperand($1), (isConsp) ? opRef : opVal, opNull);
        genQuad(QPAR, newSymbolOperand($3), (isConsp) ? opRef : opVal, opNull);
        genQuad(QPAR, newSymbolOperand($$), opRet, opNull);
        genQuad(QCALL, opNull, opNull, newSymbolOperand(f));
    }
|   "new" type '[' expr ']'
    {
        if (!equalType(getType($4), typeInteger))
            error("type mismatch: new t [ t1 ] -> new t [ int ]");

        // TODO check if newarrp or newarrv is redifined
        SymbolEntry *t = newTemporary(typeInteger);
        $$ = newTemporary(typeIArray($2));
        
        bool isNewarrp = (equalType($2, typeIArrayAny) || equalType($2, typeListAny));
        SymbolEntry *f = lookupEntry((isNewarrp)? "newarrp" : "newarrv", LOOKUP_ALL_SCOPES, false);
        
        char buffer[5];
        sprintf(buffer, "%u", sizeOfType($2));

        genQuad(QMULT, newSymbolOperand($4), newConstantOperand(typeInteger, buffer), newSymbolOperand(t));
        genQuad(QPAR, newSymbolOperand(t), opVal, opNull);
        genQuad(QPAR, newSymbolOperand($$), opRet, opNull);
        genQuad(QCALL, opNull, opNull, newSymbolOperand(f));
    }
|   "nil"   { $$ = newConstant(typeList(typeAny), "nil"); }
|   "head" '(' expr ')'
    {
        if (!equalType(getType($3), typeListAny))
            error("type mismatch: head(t1) -> head(list[t])");

        // TODO check if head is redifined
        // TODO if not a list then create one?
        $$ = newTemporary((getType($3)->refType) ? getType($3)->refType : typeAny);
        SymbolEntry *f = lookupEntry("head", LOOKUP_ALL_SCOPES, false);

        genQuad(QPAR, newSymbolOperand($3), opVal, opNull);
        genQuad(QPAR, newSymbolOperand($$), opRet, opNull);
        genQuad(QCALL, opNull, opNull, newSymbolOperand(f));
    }
|   "tail" '(' expr ')'
    {
        if (!equalType(getType($3), typeListAny))
            error("type mismatch: tail(t1) -> tail(list[t])");

        // TODO check if tail is redifined
        // TODO if not a list then create one?
        $$ = newTemporary(getType($3));
        SymbolEntry *f = lookupEntry("tail", LOOKUP_ALL_SCOPES, false);

        genQuad(QPAR, newSymbolOperand($3), opVal, opNull);
        genQuad(QPAR, newSymbolOperand($$), opRet, opNull);
        genQuad(QCALL, opNull, opNull, newSymbolOperand(f));
    }
;


%%

void sigsegv_hndler(int signum) {
    printf("eskase sthn %d tou %s\n", linecount, filename);
    exit(1);
}

void yyerror(const char *msg) {
    error("syntax error %s", msg);        
    exit(1);
}

int main(int argc, char **argv) {
    int ret;
        
    signal(SIGSEGV, sigsegv_hndler);
    initFiles(argc, argv);
    
    ret = yyparse();
    
    fprintf(stderr, "\nFound %d error%s and %d warning%s.\n", 
        errors, (errors == 1) ? "" : "s", warnings, (warnings == 1) ? "" : "s");
        
    return ret;
}
