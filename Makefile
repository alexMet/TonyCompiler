.PHONY: clean distclean count default

# OS type: Linux/Win DJGPP
ifdef OS
   EXE=.exe
else
   EXE=
endif

CC	= gcc
CFLAGS	= -Wall -g

CFILES   = error.c general.c symbol.c quad.c
HFILES   = error.h general.h symbol.h quad.h
OBJFILES = parser.o lexer.o $(patsubst %.c, %.o, $(CFILES))
EXEFILES = tony$(EXE)
SRCFILES = $(HFILES) $(CFILES) lexer.l parser.y

#default: tony clean

%.o : %.c
	$(CC) $(CFLAGS) -c $<
	
$(EXEFILES): $(OBJFILES)
	$(CC) $(CFLAGS) -o $@ $^ -lfl

lexer.c: lexer.l
	flex -s -o $@ $<

parser.c parser.h: parser.y
	bison -v -d -o $@ $<
	
parser.o	: parser.c error.h general.h symbol.h quad.h
lexer.o		: lexer.c error.h general.h parser.h
error.o		: error.c error.h general.h
general.o	: general.c error.h general.h symbol.h
symbol.o	: symbol.c error.h general.h symbol.h
quad.o	    : quad.c error.h general.h symbol.h quad.h

clean:
	$(RM) $(OBJFILES) lexer.c parser.c parser.h parser.output *~

distclean: clean
	$(RM) $(EXEFILES)
	
count:
	wc -l -c Makefile $(SRCFILES)

bonus.zip: distclean
	zip bonus.zip Makefile $(SRCFILES)

