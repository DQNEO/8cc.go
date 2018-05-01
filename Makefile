CFLAGS=-Wall -std=gnu99 -g
OBJS=main.o lex.o string.o

#8cc: $(OBJS) 8gg
#	$(CC) $(CFLAGS) -o $@ $(OBJS)
8cc: 8gg
	./build-hook.sh

clean:
	rm -f 8cc *.o tmp.*
	rm -f 8gg.*

8gg: main.go adapter.go
	GOOS=linux  GOARCH=amd64 go build -o 8gg.linux main.go lex.go adapter.go
	GOOS=darwin GOARCH=amd64 go build -o 8gg.mac   main.go lex.go adapter.go

