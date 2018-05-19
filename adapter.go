package main

// emulates some C functions

import (
	"fmt"
	"os"
	"unicode"
)

type pseudoStdin struct {
	buf []byte
	i   int
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
	return b, nil
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

func isalpha(c byte) bool {
	return (byte('a') <= c && c <= byte('z')) || (byte('A') <= c && c <= byte('Z'))
}

func isalnum(c byte) bool {
	return isalpha(c) || byte('0') <= c && c <= byte('9')
}

func strcmp(a Cstring, b Cstring) int {
	for i := 0; i < len(a); i++ {
		if a[i] != b[i] {
			return -1
		}
		if a[i] == 0 {
			break
		}
	}
	return 0
}

func strlen(str Cstring) int {
	var i int
	for i = 0; str[i] != 0; i++ {

	}
	return i
}

type Cstring []byte
func (b Cstring) String() string {
	i := 0
	for {
		if b[i] == byte(0) {
			break
		}
		i++
	}
	subb := b[0:i]
	return string(subb)
}

func printf(format string, args ...interface{}) {
	fmt.Printf(format, args...)
}
