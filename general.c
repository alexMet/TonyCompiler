#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "symbol.h"
#include "error.h"
#include "general.h"

/* --- Compiler's global variables. --- */

FILE       *immStream;
FILE       *finalStream;
bool        OPTIMIZE    = false;

const char *filename;           // File name to be compiled
int         linecount   = 1;    // Line number
int         errors      = 0;    // Error counter
int         warnings    = 0;    // Warning counter


bool isBasicType(Type t) {
    return equalType(t, typeInteger) || equalType(t, typeBoolean) || equalType(t, typeChar);
}

Type getType(SymbolEntry *e) {
    switch (e->entryType) {
        case ENTRY_FUNCTION:
            return e->u.eFunction.resultType;
        case ENTRY_VARIABLE:
        case ENTRY_CONSTANT:
            return e->u.eVariable.type;
        case ENTRY_PARAMETER:
            return e->u.eParameter.type;
        case ENTRY_TEMPORARY:
            return e->u.eTemporary.type;
        default:
            internal("No such entry type.");
            return NULL;
    }
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
        {"puts", typeVoid, 1, (LibFunParams[]) {{"s", typeIArray(typeChar), PASS_BY_REFERENCE}}},
        
        {"geti", typeInteger, 0, NULL},
        {"getb", typeBoolean, 0, NULL},
        {"getc", typeChar,    0, NULL},
        {"gets", typeVoid,    2, (LibFunParams[]) {
            {"n", typeInteger,          PASS_BY_VALUE},
            {"s", typeIArray(typeChar), PASS_BY_REFERENCE}}},
            
        {"abs", typeInteger, 1, (LibFunParams[]) {{"n", typeInteger, PASS_BY_VALUE}}},
        {"ord", typeInteger, 1, (LibFunParams[]) {{"c", typeChar,    PASS_BY_VALUE}}},
        {"chr", typeChar,    1, (LibFunParams[]) {{"c", typeInteger, PASS_BY_VALUE}}},
        
        {"strlen", typeInteger, 1, (LibFunParams[]) {{"s", typeIArray(typeChar), PASS_BY_REFERENCE}}},
        {"strcmp", typeInteger, 2, (LibFunParams[]) {
            {"s1", typeIArray(typeChar), PASS_BY_REFERENCE},
            {"s2", typeIArray(typeChar), PASS_BY_REFERENCE}}},
        {"strcpy", typeVoid, 2, (LibFunParams[]) {
            {"trg", typeIArray(typeChar), PASS_BY_REFERENCE},
            {"src", typeIArray(typeChar), PASS_BY_REFERENCE}}},
        {"strcat", typeVoid, 2, (LibFunParams[]) {
            {"trg", typeIArray(typeChar), PASS_BY_REFERENCE},
            {"src", typeIArray(typeChar), PASS_BY_REFERENCE}}},
            
        {"head",  typeAny,           1, (LibFunParams[]) {{"l", typeList(typeAny), PASS_BY_REFERENCE}}},
        {"tail",  typeList(typeAny), 1, (LibFunParams[]) {{"l", typeList(typeAny), PASS_BY_REFERENCE}}},
        {"consp", typeList(typeAny), 2, (LibFunParams[]) {
            {"p", typeAny, PASS_BY_REFERENCE},
            {"l", typeAny, PASS_BY_REFERENCE}}},
        {"consv", typeList(typeAny), 2, (LibFunParams[]) {
            {"v", typeAny,           PASS_BY_REFERENCE},
            {"l", typeList(typeAny), PASS_BY_REFERENCE}}},
        {"newarrp", typeIArray(typeAny), 1, (LibFunParams[]) {{"v", typeInteger, PASS_BY_VALUE}}},
        {"newarrv", typeIArray(typeAny), 1, (LibFunParams[]) {{"v", typeInteger, PASS_BY_VALUE}}}
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
