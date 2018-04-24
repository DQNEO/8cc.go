package main

import (
	"fmt"
	"os"
	"unicode"
)

const BUFLEN = 256

func _error(format string, args ...interface{}) {
	fmt.Printf(format, args...)
	os.Exit(1)
}

func getc(f *os.File) (byte, error) {
	b := make([]byte, 1)
	_, err := f.Read(b)
	return b[0], err
}


func compile_number(n int) {
	for {
		c, err := getc(os.Stdin)
		if err != nil {
			break
		}

		if unicode.IsSpace(rune(c)) {
			break
		}
		if !unicode.IsDigit(rune(c)) {
			_error("Invalid character in number: '%c'", c);
		}
		n = n * 10 + int(c) - int('0')
	}
	fmt.Printf(".text\n\t"+
		".global intfn\n"+
		"intfn:\n\t"+
		"mov $%d, %%rax\n\t"+
		"ret\n", n)
}

func compile_string() {
	var buf [BUFLEN]byte
	var i = 0
	for {
		c, err := getc(os.Stdin)
		if err != nil {
			_error("Unterminated string")
		}
		if c == '"' {
			break
		}
		if c == '\\' {
			c, err = getc(os.Stdin)
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
}

func compile() {
	c,_ := getc(os.Stdin)
	if unicode.IsDigit(rune(c)) {
		compile_number(int(c) - int('0'))
		return
	} else if c == '"' {
		compile_string()
		return
	}
	_error("Don't know how to handle '%c'", c)
}

func main() {
	compile()
	return
}
