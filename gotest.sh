#!/bin/bash
set -e
export MODE=go
make -f MakefileGo nqueen test

