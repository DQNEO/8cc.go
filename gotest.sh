#!/bin/bash
set -e
export MODE=go
rm -f 8gg.* 8cc
make -f MakefileGo nqueen test

