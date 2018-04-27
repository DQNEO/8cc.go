package main

// emulates some C functions

import (
	"os"
	"fmt"
	"unicode"
)


type pseudoStdin struct {
	buf []byte
	i int
}

var stdin *pseudoStdin

func initStdin() {
	stdin = newStdin()
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

func isspace(c byte) bool {
	return unicode.IsSpace(rune(c))
}

func isdigit(c byte) bool {
	return unicode.IsDigit(rune(c))
}

func printf(format string, args... interface{}) {
	fmt.Printf(format, args...)
}
