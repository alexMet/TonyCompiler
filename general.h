#ifndef __GENERAL_H__
#define __GENERAL_H__
 
/*
 *	Memory handler functions prototype.
 */
 
void       *new     (size_t);
void        delete  (void *);

extern int  yylex   (void);
extern void yyerror (const char msg []);

/*
 *	Compiler's global variables.
 */

#define SYMBOL_TABLE_SIZE 997

extern const char  *filename;
extern int 			linecount;
extern int 			errors;

#endif
