#!/bin/bash

make
./minor ./exs/test.min
./minor ./exs/module.min
nasm -felf32 exs/test.asm
nasm -felf32 exs/module.asm
ld -m elf_i386 exs/test.o -L/home/mgoulao/Projects/Compiladores/minor -lminor


