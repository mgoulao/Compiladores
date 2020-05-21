#!/bin/bash

passed="\e[32mPASSED\e[0m"
failed="\e[31mFAILED\e[0m"

make clean
make

for arg in $(find . -name "*.min")
do
	if test -f ${arg%.*}; then
  		${arg%.*} < "${arg%.*}".in > "${arg%.*}".mine.out
		if cmp -s "${arg%.*}".out "${arg%.*}".mine.out; then
			echo -e $passed $arg
		else
			echo -e $failed $arg
		fi
	fi
done
