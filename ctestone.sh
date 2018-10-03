#!/bin/bash
set -e
CFLAGS="-Wall -std=gnu99 -g -I. -no-pie"
testfile=$1
make
make test/util/util.o
./8cc < $testfile > /tmp/a.s
gcc $CFLAGS -o /tmp/a.out /tmp/a.s test/util/util.o
echo '---------'
/tmp/a.out
rm /tmp/a.out

