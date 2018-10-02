package main

import (
	"fmt"
	"os"
)

func errorf(format string, args ...interface{}) {
	fmt.Fprintf(os.Stderr, "%s ", input_position())
	panic(fmt.Sprintf(format, args...))
	os.Exit(1)
}

func assert(expr bool) {
	if !expr {
		errorf("Assertion failed.s")
	}
}

func warn(format string, args ...interface{}) {
	fmt.Fprint(os.Stderr, "warning: ")
	fmt.Fprintf(os.Stderr, format, args...)
}

func quote_cstring(sval string) string {
	var s string
	for _, c := range []byte(sval) {
		if c == byte(0) {
			break
		}
		if c == '"' || c == '\\' {
			s += fmt.Sprintf("\\%c", c)
		} else if c == '\n' {
			s += "\\n"
		} else {
			s += fmt.Sprintf("%c", c)
		}
	}
	return s
}

func quote_char(c byte) string {
	s := ""
	if c == '\\' {
		s = "'\\\\"
	} else if c == '\'' {
		s = "'\\''"
	} else {
		s = fmt.Sprintf("'%c'", c)
	}
	return s
}
