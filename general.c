#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "symbol.h"
#include "error.h"
#include "general.h"

Quad        quads[QUAD_ARRAY_SIZE];

bool        OPTIMIZE    = false;
FILE       *immStream;
FILE       *finalStream;

const char *filename;           // File name to be compiled
int         linecount   = 1;    // Line number
int         errors      = 0;    // Error counter
int         warnings    = 0;    // Warning counter

const char *intToString(unsigned int n) {
    char *str = (char *) new(5 * sizeof(char));
    sprintf(str, "%u", n);
    
    return (const char *) str;
}

bool isBasicType(Type t) {
    return equalType(t, typeInteger) || equalType(t, typeBoolean) || equalType(t, typeChar);
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

/* --- Quad code production functions. --- */

/*void initQuadArray() {*/
/*    curQuadArraySize = QUAD_ARRAY_SIZE;*/
/*    quads = (Quad **) new(QUAD_ARRAY_SIZE * sizeof(Quad *));*/
/*}*/

void genQuad(QuadOp op, const char *op1, const char *op2, const char *dest) {
    // Quad *newQuad = (Quad *) new(sizeof(Quad));
    
    quads[quadNext].op   = op;
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

const char *quadToStr(QuadOp op) {
    switch (op) {
        case QPLUS:     return "+";
        case QMINUS:    return "-";
        case QMULT:     return "*";
        case QDIV:      return "/";
        case QMOD:      return "mod";
        case QIFB:      return "ifb";
        case QJMP:      return "jump";
        case QASSIGN:   return ":=";
        case QEQ:       return "=";
        case QNE:       return "<>";
        case QGT:       return ">";
        case QLT:       return "<";
        case QGE:       return ">=";
        case QLE:       return "<=";
        case QUNIT:     return "unit";
        case QENDU:     return "endu";
        case QPAR:      return "par";
        case QCALL:     return "call";
        case QRET:      return "ret";
        case QRETV:     return "retv";
        default:
            internal("uknown quad op");
            exit(1);
        }
}

void printQuads() {
    int i;
    
    for (i = 1; i < quadNext; i++)
        printf("%d: %s, %s, %s, %s\n", i, quadToStr(quads[i].op), quads[i].op1, quads[i].op2, quads[i].dest);
}

void exprToCond(SymbolEntry *p, LabelList **TRUE, LabelList **FALSE) {
    *TRUE = makeList(quadNext);
    genQuad(QIFB, p->id, "-", "*");
    *FALSE = makeList(quadNext);
    genQuad(QJMP, "-", "-", "*");
}

void condToExpr(SymbolEntry **p, LabelList *TRUE, LabelList *FALSE) {
    if (*p == NULL) {
        *p = newTemporary(typeBoolean);
        backpatch(TRUE, quadNext);
        genQuad(QASSIGN, "true", "-",  (*p)->id);
        int q = quadNext + 2;
        genQuad(QJMP, "-", "-",  intToString(q));
        backpatch(FALSE, quadNext);
        genQuad(QASSIGN, "false", "-",  (*p)->id);
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

/* --- Compiler initialization functions implementation. --- */

void usage(char *compilerName) {
    fprintf(stderr, 
        "Usage: %s [-oit] [file.tony]\n\n"
        "Options:\n"
        "\t-o:\t\tturns on the optimization flag\n"
        "\t-i:\t\toutput the intermediate code to stdout\n"
        "\t-f:\t\toutput the assembly code to stdout\n\n"
        "If -i or -f options are given, the compiler reads from stdin.\n"
        "Otherwise, it reads from file.tony and outputs the intermidiate\n"
        "code to file.imm and the final code to file.asm\n", compilerName);
    exit(1);
}

void initFiles(int argc, char **argv) {
    int opt;
    bool opti = false, optf = false;
    
    while ((opt = getopt(argc, argv, "ofi")) != -1) {
        switch (opt) {
            case 'o':
                OPTIMIZE = true;
                break;
            case 'i':
                opti = true;
                break;
            case 'f':
                optf = true;
                break;
            default:
                usage(argv[0]);
        }
    }

    if (opti && optf)
        usage(argv[0]);
    else if (opti) {
        if (optind != argc)
            usage(argv[0]);
        
        filename    = strdup("stdin");
        yyin        = stdin;
        immStream   = stdout;
        finalStream = NULL;
    }
    else if (optf) {
        if (optind != argc)
            usage(argv[0]);
            
        filename    = strdup("stdin");
        yyin        = stdin;
        immStream   = NULL;
        finalStream = stdout;
    }
    else {
        if (optind != argc - 1)
            usage(argv[0]);
            
        filename = strdup(argv[optind]);
        char *e  = strrchr(filename, '.');
        
        if (e == NULL || strcmp(e, ".tony")) {
            fprintf(stderr, "The input filename's extension must be .tony\n");
            exit(1);
        }
        
        if (!(yyin = fopen(filename, "r"))) {
            fprintf(stderr, "No such file '%s'\n", filename);
            exit(1);
        }
        
        strcpy(e, ".imm");
        if (!(immStream = fopen(filename, "w"))) {
            fprintf(stderr, "Cannot open output file '%s'\n", filename);
            exit(1);
        }

        strcpy(e, ".asm");
        if (!(finalStream = fopen(filename, "w"))) {
            fprintf(stderr, "Cannot open output file '%s'\n", filename);
            exit(1);
        }
        
        strcpy(e, ".tony");
    }
}

/* --- Tony library functions declarations. --- */

typedef struct LibFunParams_tag LibFunParams;

struct LibFunParams_tag {
    char    *name;
    Type     type;
    PassMode passMode;
};
    
typedef struct LibFun_tag LibFun;

struct LibFun_tag {
    char         *name;
    Type          returnType;
    int           paramsNum;
    LibFunParams *params;
};

void initLibFuns() {
    int i, j;
    
    LibFun functions[] = {
        {"puti", typeVoid, 1, (LibFunParams[]) {{"n", typeInteger,          PASS_BY_VALUE}}},
        {"putb", typeVoid, 1, (LibFunParams[]) {{"b", typeBoolean,          PASS_BY_VALUE}}},
        {"putc", typeVoid, 1, (LibFunParams[]) {{"c", typeChar,             PASS_BY_VALUE}}},
        {"puts", typeVoid, 1, (LibFunParams[]) {{"s", typeIArray(typeChar), PASS_BY_VALUE}}},
        
        {"geti", typeInteger, 0, NULL},
        {"getb", typeBoolean, 0, NULL},
        {"getc", typeChar,    0, NULL},
        {"gets", typeVoid,    2, (LibFunParams[]) {
            {"n", typeInteger,          PASS_BY_VALUE},
            {"s", typeIArray(typeChar), PASS_BY_VALUE}}},
            
        {"abs", typeInteger, 1, (LibFunParams[]) {{"n", typeInteger, PASS_BY_VALUE}}},
        {"ord", typeInteger, 1, (LibFunParams[]) {{"c", typeChar,    PASS_BY_VALUE}}},
        {"chr", typeChar,    1, (LibFunParams[]) {{"c", typeInteger, PASS_BY_VALUE}}},
        
        {"strlen", typeInteger, 1, (LibFunParams[]) {{"s", typeIArray(typeChar), PASS_BY_VALUE}}},
        {"strcmp", typeInteger, 2, (LibFunParams[]) {
            {"s1", typeIArray(typeChar), PASS_BY_VALUE},
            {"s2", typeIArray(typeChar), PASS_BY_VALUE}}},
        {"strcpy", typeVoid, 2, (LibFunParams[]) {
            {"trg", typeIArray(typeChar), PASS_BY_VALUE},
            {"src", typeIArray(typeChar), PASS_BY_VALUE}}},
        {"strcat", typeVoid, 2, (LibFunParams[]) {
            {"trg", typeIArray(typeChar), PASS_BY_VALUE},
            {"src", typeIArray(typeChar), PASS_BY_VALUE}}}
    };
    
    int functionNum = (int) sizeof(functions) / sizeof(LibFun);
    for (i = 0; i < functionNum; i++) {
        LibFun f = functions[i];
        SymbolEntry *s = newFunction(f.name);
        forwardFunction(s);
        openScope(NULL);
    
        for (j = 0; j < f.paramsNum; j++)
            newParameter(f.params[j].name, f.params[j].type, f.params[j].passMode, s);
    
        endFunctionHeader(s, f.returnType);
        closeScope();
    }
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
