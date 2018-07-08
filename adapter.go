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

func strcmp(a string, b string) int {
	if string(a) == string(b) {
		return 0
	}
	return 1
}

func strlen(str string) int {
	return len(str)
}

func appendNullByte(b []byte) []byte {
	return b
}

// "abc" => string("abc\x00")
func NewCstringFromLiteral(s string) string {
	return string(s)
}

func printf(format string, args ...interface{}) {
	fmt.Printf(format, args...)
}
