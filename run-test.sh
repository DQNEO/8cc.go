#/bin/bash

set -e
make clean
make 8gg
docker run --rm -v `pwd`:/mnt/ sublimino/debian-build-essential  bash -c 'cd /mnt/; ./test.sh'

