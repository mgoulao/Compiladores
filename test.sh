#!/bin/bash

make
./minor ./exs/test.min
nasm -felf32 exs/test.asm
ld -m elf_i386 exs/test.o -L/home/mgoulao/Projects/Compiladores/minor -lminor


