#!/bin/bash
set -e
export MODE=go
rm -f 8gg.*
make -f MakefileGo nqueen
./test.sh
