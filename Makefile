CFLAGS=-Wall -std=gnu99 -g
OBJS=lex.o string.o util.o


$(OBJS) unittest.o main.o: 8cc.h

#8cc: 8cc.h main.o $(OBJS)
#	$(CC) $(CFLAGS) -o $@ main.o $(OBJS)
8cc: 8gg
	./build-hook.sh

unittest: 8cc.h unittest.o $(OBJS)
	$(CC) $(CFLAGS) -o $@ unittest.o $(OBJS)

test: unittest
	./unittest
	./test.sh

clean:
	rm -f 8cc *.o tmp.*
	rm -f 8gg.*

8gg: main.go adapter.go
	GOOS=linux  GOARCH=amd64 go build -o 8gg.linux main.go lex.go adapter.go
	GOOS=darwin GOARCH=amd64 go build -o 8gg.mac   main.go lex.go adapter.go

