#/bin/bash

set -e
make clean
make 8gg
docker run --rm -v `pwd`:/mnt/ mugen/ubuntu-build-essential bash -c 'cd /mnt/; ./test.sh'

