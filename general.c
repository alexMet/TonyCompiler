#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "symbol.h"
#include "error.h"
#include "general.h"

Quad        quads[QUAD_ARRAY_SIZE];

const char *filename;           // File name to be compiled
int         linecount   = 1;    // Line number
int         errors      = 0;    // Error counter

const char *intToString(unsigned int n) {
    char *str = (char *) new(5 * sizeof(char));
    sprintf(str, "%u", n);
    
    return (const char *) str;
}

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
            internal("No such entry type.");
            return NULL;
    }
}
        
/*void initQuadArray() {*/
/*    curQuadArraySize = QUAD_ARRAY_SIZE;*/
/*    quads = (Quad **) new(QUAD_ARRAY_SIZE * sizeof(Quad *));*/
/*}*/

void genQuad(const char *op, const char *op1, const char *op2, const char *dest) {
    // Quad *newQuad = (Quad *) new(sizeof(Quad));
    
    quads[quadNext].op   = strdup(op);
    quads[quadNext].op1  = strdup(op1);
    quads[quadNext].op2  = strdup(op2);
    quads[quadNext].dest = strdup(dest);
    
    quadNext++;
    
    if (quadNext == QUAD_ARRAY_SIZE) {
        internal("Too many quads generated");
        // TODO resize array
        // curQuadArraySize = 2 * curQuadArraySize;
    }
}

void printQuads() {
    int i;
    
    for (i = 1; i < quadNext; i++)
        printf("%d: %s, %s, %s, %s\n", i, quads[i].op, quads[i].op1, quads[i].op2, quads[i].dest);
}

void exprToCond(SymbolEntry *p, LabelList **TRUE, LabelList **FALSE) {
    *TRUE = makeList(quadNext);
    genQuad("ifb", p->id, "-", "*");
    *FALSE = makeList(quadNext);
    genQuad("jump", "-", "-", "*");
}

void condToExpr(SymbolEntry **p, LabelList *TRUE, LabelList *FALSE) {
    if (*p == NULL) {
        *p = newTemporary(typeBoolean);
        backpatch(TRUE, quadNext);
        genQuad(":=", "true", "-",  (*p)->id);
        int q = quadNext + 2;
        genQuad("jump", "-", "-",  intToString(q));
        backpatch(FALSE, quadNext);
        genQuad(":=", "false", "-",  (*p)->id);
    }
}
 
void backpatch(LabelList *l, unsigned int label) {
    LabelList *cur;
    
    for (cur = l; cur != NULL; cur = cur->next)
        quads[cur->label].dest = strdup(intToString(label));
}

LabelList *makeList(unsigned int label) {
    LabelList *l = (LabelList *) new(sizeof(LabelList));
    
    l->label = label;
    l->next  = NULL;
    
    return l;
}

LabelList *merge(LabelList *l1, LabelList *l2) {
    LabelList *l;
    
    if (l1 == NULL) return l2;
    
    for (l = l1; l->next != NULL; l = l->next) ;
        
    l->next = l2;
    return l;
}

void printLabelList(LabelList *l) {
    LabelList *c;
    printf("[");
    
    for (c = l; c != NULL; c = c->next)
	    printf("%d, ", c->label);
	    
	printf("]\n");
}

/* --- Memory handler functions implementation. --- */

void *new(size_t size) {
   void *result = malloc(size);
   
   if (result == NULL)
      fatal("Out of memory");
      
   return result;
}

void delete(void *p) {
   if (p != NULL)
      free(p);
}
