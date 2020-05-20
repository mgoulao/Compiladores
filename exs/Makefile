COMP=../minor
EXT=.min
LDLIBS=../libminor.a
.SUFFIXES: $(EXT) .brg .asm
NASM=nasm -felf32
#NASM=as -o $@
LD=ld -m elf_i386 # 32-bit binaries in a 64-bit system
#LD=ld

SRC= ack.min fib.min hello.min isbn10.min div.min find.min \
     histograma.min test.min gcd.min \
     fact.min hanoi.min hondt.min

EXE=$(SRC:%.min=%)

all: $(EXE)

linux32:
	$(MAKE) $(MFLAGS) NASM="nasm -felf32"

ack: ack.o
	$(LD) -o $@ $^ $(LDLIBS)
div: div.o
	$(LD) -o $@ $^ $(LDLIBS)
fact: fact.o iter.o
	$(LD) -o $@ $^ $(LDLIBS)
fib: fib.o
	$(LD) -o $@ $^ $(LDLIBS)
find: find.o
	$(LD) -o $@ $^ $(LDLIBS)
gcd: gcd.o
	$(LD) -o $@ $^ $(LDLIBS)
hanoi: hanoi.o
	$(LD) -o $@ $^ $(LDLIBS)
hello: hello.o
	$(LD) -o $@ $^ $(LDLIBS)
histograma: histograma.o
	$(LD) -o $@ $^ $(LDLIBS)
hondt: hondt.o
	$(LD) -o $@ $^ $(LDLIBS)
isbn10: isbn10.o
	$(LD) -o $@ $^ $(LDLIBS)
test: test.o
	$(LD) -o $@ $^ $(LDLIBS)



%: %.o
	$(LD) -o $@ $< $(LDLIBS)

.asm.o:
	$(NASM) $<

.min.asm:
	$(COMP) $<

clean:
	rm -f a.out *~ *.o *.asm *.mine.out $(EXE)
