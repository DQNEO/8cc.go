package main

// emulates some C functions

import (
	"os"
	"fmt"
)


type pseudoStdin struct {
	buf []byte
	i int
}


func newStdin() *pseudoStdin {
	s := &pseudoStdin{}
	s.buf = make([]byte, 1024*1024)
	os.Stdin.Read(s.buf)
	return s
}

func getc(stdin *pseudoStdin) (byte, error) {
	b := stdin.buf[stdin.i]
	if b == byte(0) {
		return b, fmt.Errorf("EOL")
	}
	stdin.i++
	return b,nil
}

func ungetc(c byte, stdin *pseudoStdin) {
	stdin.i--
	return
}



