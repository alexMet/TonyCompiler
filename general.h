#ifndef __GENERAL_H__
#define __GENERAL_H__

#include "symbol.h"
#include "quad.h"

#define SYMBOL_TABLE_SIZE 997
#define QUAD_ARRAY_SIZE   1000
#define INTTOSTR_BUF_SIZE 5

/* --- Compiler's global variables. --- */

extern char          tmpBuf[];

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

bool        isBasicType      (Type t);
Type        getType          (SymbolEntry *e);

/* --- Compiler initialization functions prototype. --- */

void        usage            (char *compilerName);
void        initFiles        (int argc, char **argv);
void        initLibFuns      (void);

/* --- Memory handler functions prototype. --- */
 
void       *new              (size_t);
void        delete           (void *);

#endif
