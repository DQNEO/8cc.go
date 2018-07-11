# 8cc.go C Compiler

8cc.go is a C Compiler written in Go. It's a port of 8cc (https://github.com/rui314/8cc)

# Author

DQNEO

( Originally from Rui Ueyama rui314@gmail.com )

# Usage

The output assembly code can only be run on Linux.

If you use other platforms, I would recomment Docker to run it.


```
$ docker run -it --rm -v `pwd`:/mnt  dqneo/ubuntu-build-essential:go bash
root@62b0d706a586:/mnt# echo 'int main(){printf("%s\n","hello world");}' |./gorun|./asrun
hello world
```
# What kind of syntax does it support ?

* if/else
* for
* binary operators (+-/* & |)
* logical operators (&&, ||)
* ternary operator (?:)
* declaration of functions
* function call
* assign to local variables
* assign to global variables
* primitiv data types (int, char)
* composite data types (array, struct, union, pointer)
* arithmetic of pointer
* dereference of pointer

# Test

```
$ docker run -it --rm -v `pwd`:/mnt  dqneo/ubuntu-build-essential:go bash
root@62b0d706a586:/# cd /mnt/
root@62b0d706a586:/mnt# ./gotest.sh
```

# LICENSE
MIT License
