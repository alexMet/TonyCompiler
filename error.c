#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#include "general.h"
#include "error.h"

/*
 *	Error handler functions implementations. 
 */

void internal(const char *fmt, ...) {
   va_list ap;

   va_start(ap, fmt);
   fprintf(stderr, "%s:%d: ", filename, linecount);
   fprintf(stderr, "internal error: ");
   vfprintf(stderr, fmt, ap);
   fprintf(stderr, "\n");
   va_end(ap);
   exit(1);
}

void fatal(const char *fmt, ...) {
   va_list ap;

   va_start(ap, fmt);
   fprintf(stderr, "%s:%d: ", filename, linecount);
   fprintf(stderr, "fatal error: ");
   vfprintf(stderr, fmt, ap);
   fprintf(stderr, "\n");
   va_end(ap);
   exit(1);
}

void error(const char *fmt, ...) {
   va_list ap;

   va_start(ap, fmt);
   fprintf(stderr, "%s:%d: ", filename, linecount);
   fprintf(stderr, "error: ");
   vfprintf(stderr, fmt, ap);
   fprintf(stderr, "\n");
   va_end(ap);
}

void warning(const char *fmt, ...) {
   va_list ap;

   va_start(ap, fmt);
   fprintf(stderr, "%s:%d: ", filename, linecount);
   fprintf(stderr, "warning: ");
   vfprintf(stderr, fmt, ap);
   fprintf(stderr, "\n");
   va_end(ap);
}
