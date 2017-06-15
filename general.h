#ifndef __GENERAL_H__
#define __GENERAL_H__

#include "symbol.h"

#define SYMBOL_TABLE_SIZE 997
#define QUAD_ARRAY_SIZE   1000

typedef enum {
	QPLUS, QMINUS, QMULT, QDIV, QMOD,
	QEQ, QNE, QGT, QLT, QGE, QLE,
	QASSIGN, QIFB, QJMP, QRET, QRETV,
	QUNIT, QENDU, QPAR, QCALL
} QuadOp;

typedef struct quad_item Quad;

struct quad_item {
    QuadOp      op;
    const char *op1, *op2, *dest;
};

typedef struct label_list LabelList;

struct label_list {
    unsigned int label;
    struct label_list *next;
};

/* --- Compiler's global variables. --- */

extern Quad          quads[];

extern bool          OPTIMIZE;
extern FILE         *immStream;
extern FILE         *finalStream;

extern const char   *filename;
extern int           linecount;
extern int           errors;
extern int           warnings;

extern FILE         *yyin;
extern char         *yytext;

extern int  yylex            (void);
extern void yyerror          (const char msg[]);

/* --- Quad generator functions prototype. --- */

const char *intToString      (unsigned int n);

bool        isBasicType      (Type t);
Type        getType          (SymbolEntry *e);

void        genQuad          (QuadOp op, const char *op1, const char *op2, const char *dest);
void        printQuads       (void);

void        exprToCond       (SymbolEntry *p, LabelList **TRUE, LabelList **FALSE);
void        condToExpr       (SymbolEntry **p, LabelList *TRUE, LabelList *FALSE);

void        backpatch        (LabelList *l, unsigned int label);
LabelList  *makeList         (unsigned int label);
LabelList  *merge            (LabelList *l1, LabelList *l2);

void        printLabelList   (LabelList *l);

/* --- Compiler initialization functions prototype. --- */

void        usage            (char *compilerName);
void        initFiles        (int argc, char **argv);
void        initLibFuns      (void);

/* --- Memory handler functions prototype. --- */
 
void       *new              (size_t);
void        delete           (void *);

#endif
