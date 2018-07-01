#!/bin/bash
cfiles=$(ls test/*.c)

for c in $cfiles
do
    ./8cc < $c > ${c%.*}.s
    ./asrun < ${c%.*}.s
done
