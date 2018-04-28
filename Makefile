CFLAGS=-Wall -std=gnu99

8gg: 8cc.go adapter.go
	GOOS=linux GOARCH=amd64  go build -o 8gg.linux 8cc.go adapter.go
	GOOS=darwin GOARCH=amd64 go build -o 8gg.mac   8cc.go adapter.go

8cc: 8gg
	cp 8gg.linux 8cc

_8cc: 8cc.o

clean:
	rm -f 8cc *.o tmp.*
	rm -f 8gg
