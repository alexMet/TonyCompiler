#ifndef __QUAD_H__
#define __QUAD_H__

/* Τύποι δεδομένων για την υλοποίηση των σταθερών */

typedef int                RepInteger;         /* Integers    */
typedef unsigned char      RepBoolean;         /* Booleans    */
typedef char               RepChar;            /* Characters  */
typedef const char        *RepString;          /* Strings     */

/* Quad operator type */

typedef enum {
    QPLUS, QMINUS, QMULT, QDIV, QMOD,
    QEQ, QNE, QGT, QLT, QGE, QLE,
    QASSIGN, QIFB, QJMP, QRET, QRETV,
    QARRAY, QUNIT, QENDU, QPAR, QCALL
} Operator;

/* Quad operand type */

typedef enum {
    OP_SYMBOLENTRY,
    OP_CONSTANT,
    OP_LABEL,
    OP_NULL, OP_STAR, OP_RET, OP_VAL, OP_REF
} OperandType;

/* Quad operand */

typedef struct Operand_tag *Operand;

struct Operand_tag {
    OperandType operandType;

    union {
        SymbolEntry     *symbolEntry;
        unsigned int     label;

        struct {
            enum {
                CONST_INTEGER,
                CONST_BOOLEAN,
                CONST_CHAR,
                CONST_IARRAY
            } type;
            union {
                RepInteger vInteger;
                RepBoolean vBoolean;
                RepChar    vChar;
                RepString  vString;
            } value;
        } oConstant;

    } u;
};

/* Quad type */

typedef struct Quad_tag Quad;

struct Quad_tag {
    Operator    op;
    Operand     x, y, z;
};

/* Boolean expression label list type */

typedef struct label_list LabelList;

struct label_list {
    unsigned int label;
    struct label_list *next;
};

/* --- Quad generator global variables. --- */

extern const Operand opNull;
extern const Operand opStar;
extern const Operand opRet;
extern const Operand opVal;
extern const Operand opRef;

/* --- Quad generator functions prototype. --- */

Operand         passModeToOperand   (SymbolEntry *e);
Operand         newSymbolOperand    (SymbolEntry *e);
Operand         newLabelOperand     (unsigned int l);
Operand         newConstantOperand  (Type type, const char *value);
SymbolEntry    *newConstant         (Type type, const char *name);

void            genQuad             (Operator op, Operand x, Operand y, Operand z);
void            printQuads          (void);

void            exprToCond          (SymbolEntry *e, LabelList **TRUE, LabelList **FALSE);
void            condToExpr          (SymbolEntry **e, LabelList *TRUE, LabelList *FALSE);

void            backpatch           (LabelList *l, unsigned int label);
LabelList      *makeList            (unsigned int label);
LabelList      *merge               (LabelList *l1, LabelList *l2);

void            printLabelList      (LabelList *l);

#endif
