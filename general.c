#include <stdlib.h>

#include "error.h"
#include "general.h"

/* 
 *	Memory handler functions implementation.
 */

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

/* ---------------------------------------------------------------------
   ------- Αρχείο εισόδου του μεταγλωττιστή και αριθμός γραμμής --------
   --------------------------------------------------------------------- */


/*
 *	File name to be compiled
 *	Line number
 *	Flag that counts errors.
 */

const char *filename;
int linecount = 1;
int errors = 0;
