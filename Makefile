LANG=minor
EXT=min# file extension: .$(EXT)
LIB=lib# compiler library directory
UTIL=util# compiler library: lib$(LIB).a
RUN=run# runtime directory
EXS=exs# examples directory
CC=gcc
CFLAGS=-g -DYYDEBUG
LDLIBS=run/lib$(LANG).a
AS=nasm -felf32 -l
LD=ld -m elf_i386

.SUFFIXES: .asm $(EXT)

$(LANG): $(LANG).y $(LANG).l $(LANG).brg $(LANG).h 
	make -C $(LIB)
	byacc -dv $(LANG).y
	flex -dl $(LANG).l
	pburg -T $(LANG).brg
	$(LINK.c) -o $(LANG) $(ARCH) -I$(LIB) lex.yy.c y.tab.c yyselect.c -L$(LIB) -l$(UTIL)
	make -C $(RUN)
	-cp $(RUN)/lib$(LANG).a .

examples:: $(LANG)
	make -C $(EXS)

run:: $(LANG)
	make -C $(EXS) run

%: %.asm
	$(AS) $*.asm
	$(LD) -o $@ $*.o $(LDLIBS)

clean::
	make -C $(LIB) clean
	make -C $(RUN) clean
	rm -f a.out *.o $(LANG) lib$(LANG).a lex.yy.c y.tab.c y.tab.h y.output yyselect.c *.asm *~
