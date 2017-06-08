#ifndef __GENERAL_H__
#define __GENERAL_H__

#include "symbol.h"

#define SYMBOL_TABLE_SIZE 997
#define QUAD_ARRAY_SIZE   1000

typedef struct quad_item Quad;

struct quad_item {
    const char *op, *op1, *op2, *dest;
};

typedef struct label_list LabelList;

struct label_list {
    unsigned int label;
    struct label_list *next;
};

/* --- Compiler's global variables. --- */

extern Quad          quads[];

extern const char   *filename;
extern int           linecount;
extern int           errors;

/* --- Quad generator functions prototype. --- */

const char *intToString      (unsigned int n);
Type        getType          (SymbolEntry *e);

void        genQuad          (const char *op, const char *op1, const char *op2, const char *dest);
void        printQuads       (void);

void        exprToCond       (SymbolEntry *p, LabelList **TRUE, LabelList **FALSE);
void        condToExpr       (SymbolEntry **p, LabelList *TRUE, LabelList *FALSE);

void        backpatch        (LabelList *l, unsigned int label);
LabelList  *makeList         (unsigned int label);
LabelList  *merge            (LabelList *l1, LabelList *l2);

void        printLabelList   (LabelList *l);

/* --- Memory handler functions prototype. --- */
 
void       *new     (size_t);
void        delete  (void *);

extern int  yylex   (void);
extern void yyerror (const char msg []);

#endif
