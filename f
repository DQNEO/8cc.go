#!/bin/bash

read line
echo "int main(){printf(\"%d\", f());} int f(){$line}"
