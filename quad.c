#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "symbol.h"
#include "error.h"
#include "general.h"
#include "quad.h"

/* --- Quad code production functions. --- */

static Quad quads[QUAD_ARRAY_SIZE];

static struct Operand_tag operandConst [] = {
    { OP_NULL,  { NULL } },
    { OP_STAR,  { NULL } },
    { OP_RET,   { NULL } },
    { OP_VAL,   { NULL } },
    { OP_REF,   { NULL } }
};

const Operand opNull = &(operandConst[0]);
const Operand opStar = &(operandConst[1]);
const Operand opRet  = &(operandConst[2]);
const Operand opVal  = &(operandConst[3]);
const Operand opRef  = &(operandConst[4]);

/*void initQuadArray() {*/
/*    curQuadArraySize = QUAD_ARRAY_SIZE;*/
/*    quads = (Quad **) new(QUAD_ARRAY_SIZE * sizeof(Quad *));*/
/*}*/

/* Generate a new quad. If we reached the max number of quads then we have an internal error. */
void genQuad(Operator op, Operand x, Operand y, Operand z) {
    // Quad *newQuad = (Quad *) new(sizeof(Quad));
    int q = quadNext - quadOffset;

    quads[q].op = op;
    quads[q].x  = x;
    quads[q].y  = y;
    quads[q].z  = z;

    ++quadNext;
    
    if (q + 1 == QUAD_ARRAY_SIZE) {
        internal("Too many quads generated");
        // TODO resize array
        // curQuadArraySize = 2 * curQuadArraySize;
    }
}

/* We pass the function parameter's symbol entry and we return it's pass mode as an operand. */
Operand passModeToOperand(SymbolEntry *e) {
    if (e == NULL)
        return opNull;
        
    if (e->entryType != ENTRY_PARAMETER)
        internal("Getting pass mode for something that isn't a parameter.");
        
    return (e->u.eParameter.mode == PASS_BY_REFERENCE) ? opRef : opVal;
}

/* We create a new operand that is either a symbol entry, or it is a constant value. */
Operand newSymbolOperand(SymbolEntry *e) {
    Operand o;
            
    if (e->entryType == ENTRY_CONSTANT) {
        o = newConstantOperand(e->u.eVariable.type, e->id);
        
        destroyType(e->u.eVariable.type);
        delete((char *) (e->id));
        delete(e); 
    
        return o;
    }
        
    o = (Operand) new(sizeof(struct Operand_tag));
    o->operandType   = OP_SYMBOLENTRY;
    o->u.symbolEntry = e;
        
    return o;
}

/* We create a new operand that stores a label. */
Operand newLabelOperand(unsigned int l) {
    Operand o = (Operand) new(sizeof(struct Operand_tag));
    
    o->operandType = OP_LABEL;
    o->u.label     = l;
    
    return o;
}

/* We create a new operand that stores a constant. */
Operand newConstantOperand(Type type, const char *value) {
    Operand o = (Operand) new(sizeof(struct Operand_tag));
    o->operandType = OP_CONSTANT;
    
    switch (type->kind) {
        case TYPE_INTEGER:
            o->u.oConstant.type = CONST_INTEGER;
            o->u.oConstant.value.vInteger = atoi(value);
            break;
        case TYPE_BOOLEAN:
            o->u.oConstant.type = CONST_BOOLEAN;
            o->u.oConstant.value.vBoolean = (strcmp(value, "true") == 0) ? true : false;
            break;
        case TYPE_CHAR:
            o->u.oConstant.type = CONST_CHAR;
            o->u.oConstant.value.vChar = atoi(value);
            break;
        case TYPE_IARRAY:
            if (equalType(type->refType, typeChar)) {
                o->u.oConstant.type = CONST_IARRAY;
                o->u.oConstant.value.vString = (const char *) new(strlen(value) + 1);
                strcpy((char *) (o->u.oConstant.value.vString), value);
                break;
            }
        case TYPE_LIST:
        case TYPE_POINTER:
        case TYPE_VOID:
            break;
        default:
            internal("Invalid type for constant");
    }
    
    return o;
}

/* Create a symbol entry for a new constant but without putting it in the symbol table. */
SymbolEntry *newConstant(Type type, const char *name) {
    SymbolEntry *e = (SymbolEntry *) new(sizeof(SymbolEntry));
    
    e->id = (const char *) new(strlen(name) + 1);
    strcpy((char *) (e->id), name);
    
    e->entryType = ENTRY_CONSTANT;
    e->u.eVariable.type = type;
    type->refCount++;
        
    return e;
}

/* --- Quad code printing functions. --- */

/* It takes an operator and prints it as a string to the immStream. */
void printOperator(Operator op) {
    const char *outOp;
    
    switch (op) {
        case QPLUS:     outOp = "+";     break;
        case QMINUS:    outOp = "-";     break;
        case QMULT:     outOp = "*";     break;
        case QDIV:      outOp = "/";     break;
        case QMOD:      outOp = "mod";   break;
        case QASSIGN:   outOp = ":=";    break;
        case QEQ:       outOp = "=";     break;
        case QNE:       outOp = "<>";    break;
        case QGT:       outOp = ">";     break;
        case QLT:       outOp = "<";     break;
        case QGE:       outOp = ">=";    break;
        case QLE:       outOp = "<=";    break;
        case QIFB:      outOp = "ifb";   break;
        case QJMP:      outOp = "jump";  break;
        case QPAR:      outOp = "par";   break;
        case QRET:      outOp = "ret";   break;
        case QRETV:     outOp = "retv";  break;
        case QCALL:     outOp = "call";  break;
        case QARRAY:    outOp = "array"; break;
        case QUNIT:     outOp = "unit";  break;
        case QENDU:     outOp = "endu";  break;
        default:        internal("uknown quad op");
    }
    
    printf("%s", outOp);
}

/* It takes an operand and prints its value to the immStream. */
void printOperand(Operand o) {
    switch (o->operandType) {
        case OP_SYMBOLENTRY:
            printf("%s", o->u.symbolEntry->id);
            break;
        case OP_CONSTANT:
            switch (o->u.oConstant.type) {
                case CONST_INTEGER:
                    printf("%d", o->u.oConstant.value.vInteger);
                    break;
                case CONST_BOOLEAN:
                    printf("%s", (o->u.oConstant.value.vBoolean) ? "true" : "false");
                    break;
                case CONST_CHAR:
                    printf("%c", o->u.oConstant.value.vChar);
                    break;
                case CONST_IARRAY:
                    printf("%s", o->u.oConstant.value.vString);
                    break;
            }
            break;
        case OP_LABEL:
            printf("%u", o->u.label);
            break;
        case OP_NULL:
            printf("-");
            break;
        case OP_STAR:
            printf("*");
            break;
        case OP_VAL:
            printf("V");
            break;
        case OP_REF:
            printf("R");
            break;
        case OP_RET:
            printf("RET");
            break;
    }
}

/* A function to print all of the intermediate code to the immStream. */
void printQuads() {
    unsigned int i, q;
    
    for (i = quadOffset; i < quadNext; i++) {
        q = i - quadOffset;
        printf("%d: ", i);
        printOperator(quads[q].op);
        printf(", ");
        printOperand(quads[q].x);
        printf(", ");
        printOperand(quads[q].y);
        printf(", ");
        printOperand(quads[q].z);
        printf("\n");
    }
    
    quadOffset = quadNext;
}

void exprToCond(SymbolEntry *e, LabelList **TRUE, LabelList **FALSE) {
    *TRUE = makeList(quadNext);
    genQuad(QIFB, newSymbolOperand(e), opNull, opStar);
    *FALSE = makeList(quadNext);
    genQuad(QJMP, opNull, opNull, opStar);
}

void condToExpr(SymbolEntry **e, LabelList *TRUE, LabelList *FALSE) {
    if (*e == NULL) {
        *e = newTemporary(typeBoolean);
        backpatch(TRUE, quadNext);
        genQuad(QASSIGN, newConstantOperand(typeBoolean, "true"), opNull,  newSymbolOperand(*e));
        genQuad(QJMP, opNull, opNull, newLabelOperand(quadNext + 2));
        backpatch(FALSE, quadNext);
        genQuad(QASSIGN, newConstantOperand(typeBoolean, "false"), opNull,  newSymbolOperand(*e));
    }
}

/* Patch all the unknown labels (*) of a label list into a valid label. */
void backpatch(LabelList *l, unsigned int label) {
    LabelList *cur;
    
    while (l != NULL) {
        quads[l->label - quadOffset].z = newLabelOperand(label);
        cur = l->next;
        delete(l);
        l = cur;
    }
}

/* Create a new list with a single label. */
LabelList *makeList(unsigned int label) {
    LabelList *l = (LabelList *) new(sizeof(LabelList));
    
    l->label = label;
    l->next  = NULL;
    
    return l;
}

/* Merge two label lists into one. */
LabelList *merge(LabelList *l1, LabelList *l2) {
    LabelList *l;
    
    if (l1 == NULL) return l2;
    
    for (l = l1; l->next != NULL; l = l->next) ;
        
    l->next = l2;
    return l;
}

/* Print the values of a label list. */
void printLabelList(LabelList *l) {
    LabelList *c;
    printf("[");
    
    for (c = l; c != NULL; c = c->next)
        printf("%d, ", c->label);
    
    printf("]\n");
}
