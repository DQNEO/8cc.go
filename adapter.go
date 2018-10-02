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
	fp *os.File
}

var stdin *stream

func initStdin() {
	stdin = newStream(os.Stdin)
}

func newStream(fp *os.File) *stream {
	s := &stream{}
	s.buf = make([]byte, 1024*1024) // @TODO. use real file size
	fp.Read(s.buf)
	s.fp = fp
	return s
}

func (s *stream) getc() (byte, error) {
	b := s.buf[s.i]
	if b == byte(0) {
		return b, fmt.Errorf("EOL")
	}
	s.i++
	return b, nil
}

func (s *stream) ungetc(c byte) {
	s.i--
	return
}

func (s *stream) close() {
	s.fp.Close()
}

func getc(s *stream) (byte, error) {
	return s.getc()
}

func ungetc(c byte, s *stream) {
	s.ungetc(c)
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
