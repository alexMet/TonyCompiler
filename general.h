#ifndef __GENERAL_H__
#define __GENERAL_H__

/*
 *	Memory handler functions prototype.
 */
 
void * new    (size_t);
void   delete (void *);

/*
 *	Compiler's global variables.
 */

extern const char * filename;
extern int linecount;
extern int errors;

#endif
