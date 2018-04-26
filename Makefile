CFLAGS=-Wall

8gg: 8cc.go
	GOOS=linux GOARCH=amd64 go build -o 8gg 8cc.go
	go build -o 8gg.local 8cc.go

8cc: 8gg
	cp 8gg 8cc

#8cc: 8cc.o

clean:
	rm -f 8cc *.o tmp.*
	rm -f 8gg
