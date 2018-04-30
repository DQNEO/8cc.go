#/bin/bash

make clean
docker run --rm -v `pwd`:/mnt/ dqneo/ubuntu-build-essential:go  bash -c 'cd /mnt/; export PATH="/usr/lib/go-1.10/bin:$PATH"; ./test.sh'
