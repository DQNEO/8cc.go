package main

import (
	"fmt"
	"os"
	"unicode"
)

type pseudoStdin struct {
	buf []byte
	i int
}

var stdin *pseudoStdin

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

const BUFLEN = 256

func _error(format string, args ...interface{}) {
	fmt.Printf(format, args...)
	os.Exit(1)
}

func skip_space() {
	for {
		c, err := getc(stdin)
		if err != nil {
			break
		}
		if unicode.IsSpace(rune(c)) {
			continue
		}
		ungetc(c, stdin)
		return
	}
}
func read_number(n int) int {
	for {
		c, err := getc(stdin)
		if err != nil {
			break
		}
		if !unicode.IsDigit(rune(c)) {
			ungetc(c, stdin)
			return n
		}
		n = n * 10 + int(c - byte('0'))
	}
	return n
}

func compile_expr2() {
	for {
		skip_space()
		c, err := getc(stdin)
		if err != nil {
			fmt.Printf("ret\n")
			os.Exit(0)
		}
		var op string
		if c == '+' {
			op = "add"
		} else if c == '-' {
			op = "sub"
		} else {
			_error("Operator expected, but got '%c'", c)
		}
		skip_space()
		c, err = getc(stdin)
		if !unicode.IsDigit(rune(c)) {
			_error("Number expected, but got '%c", c)
		}
		fmt.Printf("%s $%d, %%rax\n\t", op, read_number(int(c - byte('0'))))
	}
}

/*
 */
func compile_expr(n int) {
	n = read_number(n)
	fmt.Printf(".text\n\t"+
		".global intfn\n"+
		"intfn:\n\t"+
		"mov $%d, %%rax\n\t", n)
	compile_expr2()
}

func compile_string() {
	var buf [BUFLEN]byte
	var i = 0
	for {
		c, err := getc(stdin)
		if err != nil {
			_error("Unterminated string")
		}
		if c == '"' {
			break
		}
		if c == '\\' {
			c, err = getc(stdin)
			if err != nil {
				_error("Unterminated \\")
			}
		}

		buf[i] = c
		i++
		if i == BUFLEN - 1 {
			_error("String too long");
		}
	}
	fmt.Printf("\t.data\n"+
		".mydata:\n\t"+
		".string \"%s\"\n\t"+
		".text\n\t"+
		".global stringfn\n"+
		"stringfn:\n\t"+
		"lea .mydata(%%rip), %%rax\n\t"+
		"ret\n", buf)
	os.Exit(0)
}

func compile() {
	c,_ := getc(stdin)
	if unicode.IsDigit(rune(c)) {
		compile_expr(int(c - byte('0')))
		return
	} else if c == '"' {
		compile_string()
	} else {
		_error("Don't know how to handle '%c'", c)
	}
}

func main() {
	stdin = newStdin()
	compile()
	return
}
