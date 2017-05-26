.PHONY: clean distclean count

# OS type: Linux/Win DJGPP
ifdef OS
   EXE=.exe
else
   EXE=
endif

CC	= gcc
CFLAGS	= -Wall -g

CFILES   = error.c general.c symbol.c
HFILES   = error.h general.h symbol.h
OBJFILES = parser.o lexer.o $(patsubst %.c, %.o, $(CFILES))
EXEFILES = tony$(EXE)
SRCFILES = $(HFILES) $(CFILES) lexer.l parser.y

%.o : %.c
	$(CC) $(CFLAGS) -c $<
	
$(EXEFILES): $(OBJFILES)
	$(CC) $(CFLAGS) -o $@ $^ -lfl

lexer.c: lexer.l
	flex -s -o $@ $<

parser.c parser.h: parser.y
	bison -v -d -o $@ $<
	
parser.o	: parser.c general.h symbol.h error.h	
lexer.o		: lexer.c error.h parser.h general.h
error.o		: error.c general.h error.h
general.o	: general.c general.h error.h
symbol.o	: symbol.c symbol.h general.h error.h

clean:
	$(RM) $(OBJFILES) lexer.c parser.c parser.h parser.output *~

distclean: clean
	$(RM) $(EXEFILES)
	
count:
	wc -l -c Makefile $(SRCFILES)

bonus.zip: distclean
	zip bonus.zip Makefile $(SRCFILES)

