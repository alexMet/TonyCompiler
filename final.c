#include <stdio.h>

void initFinalCode(void) {
	fprintf(finalStream, 
	    "xseg\tsegment\tbyte public 'code'"
	    "\tassume\tcs:xseg, ds:xseg, ss:xseg"
	    "\torg\t100h\n"
	    "start:"
	    "\tcall\tnear ptr %s\n"
	    "\tmov\tax,4c00h"
	    "\tint\t21h\n", name("main"));
}

void endFinalCode(void) {    
	fprintf(finalStream, 
	    "xseg\tends"
	    "\tend\tmain");
}
