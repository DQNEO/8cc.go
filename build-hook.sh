#!/bin/bash
set -e

if [[ $MODE == 'c' ]]; then
    gcc -o 8cc 8cc.c
else
    cp 8gg.linux 8cc
fi
