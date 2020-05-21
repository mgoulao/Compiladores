COMP=../minor
EXT=.min
LDLIBS=../libminor.a
.SUFFIXES: $(EXT) .brg .asm
NASM=nasm -felf32
#NASM=as -o $@
LD=ld -m elf_i386 # 32-bit binaries in a 64-bit system
#LD=ld

ALL= 01ok.o 02ok.o 03ok.o 04ok.o 05ok 06ok 07ok 08ok 09ok \
     10ok 11ok 12ok 13ok 14ok 16ok 17ok 18ok 19ok 20ok \
     21ok 22ok 23ok 24ok 25ok 26ok 27ok 28ok 29ok 30ok 31ok \
     32ok 33ok 34ok 35ok 36ok 37ok 38ok \
     39ok 40ok 41ok 42ok 45ok 44ok 46ok 49ok \
     51ok ack fib \
     hello isbn10 div find  \
     histograma test gcd \
     fact hanoi hondt \



all::
	for i in $(ALL); do $(MAKE) $(MFLAGS) $$i; done

fact: fact.o iter.o
	$(LD) -o $@ $^ $(LDLIBS)

linux32:
	$(MAKE) $(MFLAGS) NASM="nasm -felf32"

arm::
	$(MAKE) $(MFLAGS) AS=as LD=ld

%: %.o
	$(LD) -o $@ $< $(LDLIBS)

.asm.o:
	$(NASM) $<

.min.asm:
	$(COMP) $<

clean:
	rm -f a.out *~ *.o *.asm *.mine.out $(ALL)
