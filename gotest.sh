#!/bin/bash
export MODE=go
rm -f 8gg.*
./test.sh
make -f MakefileGo nqueen
