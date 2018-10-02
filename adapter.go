package main

// emulates some C functions

import (
	"fmt"
	"os"
	"unicode"
)

const UINT_MAX = 4294967295

type stream struct {
	buf []byte
	i   int
}

var file *stream

func initStdin() {
	file = newStdin()
}

func newStdin() *stream {
	s := &stream{}
	s.buf = make([]byte, 1024*1024)
	os.Stdin.Read(s.buf)
	return s
}

func (stdin *stream) getc() (byte, error) {
	b := stdin.buf[stdin.i]
	if b == byte(0) {
		return b, fmt.Errorf("EOL")
	}
	stdin.i++
	return b, nil
}

func getc(stdin *stream) (byte, error) {
	return stdin.getc()
}

func (stdin *stream) ungetc(c byte) {
	stdin.i--
	return
}

func ungetc(c byte, stdin *stream) {
	stdin.ungetc(c)
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

func printf(format string, args ...interface{}) {
	fmt.Printf(format, args...)
}
