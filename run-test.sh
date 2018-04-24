#/bin/bash

docker run --rm -v `pwd`:/mnt/ mugen/ubuntu-build-essential bash -c 'cd /mnt/; ./test.sh'

