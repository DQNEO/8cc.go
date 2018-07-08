# 8cc.go C Compiler

8cc.go is a C Compiler written in Go. It's a port of 8cc (https://github.com/rui314/8cc)

# Author

DQNEO

( Originally from Rui Ueyama rui314@gmail.com )

# How to run

The output assembly code can only be run on Linux.

If you use other platforms, I would recomment Docker to run it.

```
$ docker run -it --rm -v `pwd`:/mnt  dqneo/ubuntu-build-essential:go bash
root@62b0d706a586:/# cd /mnt/
root@62b0d706a586:/mnt# ./gotest.sh
```


