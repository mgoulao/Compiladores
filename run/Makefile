.SUFFIXES: .asm
LIB=minor
CC=gcc -m32 # produce elf32 code in 64-bit machines
CFLAGS=-g -Dunderscore -fno-stack-protector # when __stack_chk_fail in Ubuntu
RUNTIME=lib$(LIB).a # stand-alone runtime
AS=nasm -felf32 # assembler command
OS=dbl.o linux.o lib.o sys.o


all: $(RUNTIME)

$(RUNTIME): power.o $(OS)
	ar crl $@ $?

power.o: power.c
	$(CC) $(CFLAGS) -DPOSTFIX -c $<

.asm.o:
	$(AS) $< -o $@

install:: $(RUNTIME)
	cp $(RUNTIME) ..

clean::
	rm -f $(RUNTIME) *.o a.out core
