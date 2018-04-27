package main

import (
	"fmt"
	"os"
	"unicode"
)

var stdin *pseudoStdin

const BUFLEN = 256

const (
	AST_INT byte = iota
	AST_STR
)

type Ast struct {
	typ byte
	ival int
	sval []byte
	left *Ast
	right *Ast
}

func _error(format string, args ...interface{}) {
	fmt.Printf(format, args...)
	os.Exit(1)
}

func make_ast_op(typ byte, left *Ast, right *Ast) *Ast {
	r := &Ast{}
	r.typ = typ
	r.left = left
	r.right = right
	return r
}

func make_ast_int(val int) *Ast {
	r := &Ast{}
	r.typ = AST_INT
	r.ival = val
	return r
}

func make_ast_str(str []byte) *Ast {
	r := &Ast{}
	r.typ = AST_STR
	r.sval = str
	return r
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

func priority(op byte) int {
	switch op {
	case '+':
		return 1
	case '-' :
		return 1
	case '*':
		return 2
	case '/':
		return 2
	default:
		_error("Operator expected, but got '%c", op)
		return 0
	}
}

func read_number(n int) *Ast {
	for {
		c, _ := getc(stdin)
		if !unicode.IsDigit(rune(c)) {
			ungetc(c, stdin)
			return make_ast_int(n)
		}
		n = n * 10 + int(c - byte('0'))
	}
}



func read_prim() *Ast {
	c, err := getc(stdin)
	if unicode.IsDigit(rune(c)) {
		return read_number(int(c - '0'))
	} else if c == '"' {
		return read_string()
	} else if err != nil {
		_error("Unexpected EOF")
	}
	_error("Don't know how to handle '%c", c)
	return nil
}

func read_expr2(prec int) *Ast {
	ast := read_prim()
	for {
	skip_space()
	op, err := getc(stdin)
	if err != nil {
		return ast
	}
	prec2 := priority(op)
	if prec2 < prec {
		ungetc(op, stdin)
		return ast
	}
	skip_space()
	ast = make_ast_op(op, ast, read_expr2(prec2+1))
	}
	return ast
}

func read_string() *Ast {
	buf := make([]byte, BUFLEN)
	i := 0
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
			_error("String too long")
		}
	}
	buf[i] = 0
	return make_ast_str(buf)
}

func read_expr() *Ast {
	return read_expr2(0)
}

func print_quote(sval []byte) {
	for _, c := range sval {
		if c == byte(0) {
			break
		}
		if c == '"' || c == '\\' {
			fmt.Printf("\\")
		}
		fmt.Printf("%c", c)
	}
}

func emit_string(ast *Ast) {
	fmt.Printf("\t.data\n"+
		".mydata:\n\t"+
		".string \"")
	print_quote(ast.sval)
	fmt.Printf("\"\n\t"+
		".text\n\t"+
		".global stringfn\n"+
		"stringfn:\n\t"+
		"lea .mydata(%%rip), %%rax\n\t"+
		"ret\n")
}

func emit_binop(ast *Ast) {
	var op string
	switch ast.typ {
	case '+':
		op = "add"
	case '-':
		op = "sub"
	case '*':
		op = "imul"
	case '/':
		break
	default:
		_error("invalid operator '%c", ast.typ)
	}

	emit_intexpr(ast.left)
	fmt.Printf("push %%rax\n\t")
	emit_intexpr(ast.right)
	if ast.typ == '/' {
		fmt.Printf("mov %%eax, %%ebx\n\t")
		fmt.Printf("pop %%rax\n\t")
		fmt.Printf("mov $0, %%edx\n\t")
		fmt.Printf("idiv %%ebx\n\t")
	} else {
		fmt.Printf("pop %%rbx\n\t")
		fmt.Printf("%s %%ebx, %%eax\n\t", op)
	}
}

func ensure_intexpr(ast *Ast) {
	switch ast.typ {
	case '+' :
		return
	case '-' :
		return
	case '*' :
		return
	case '/' :
		return
	case AST_INT:
		return
	default:
		_error("integer or binary operator expected")
	}
}

func emit_intexpr(ast *Ast) {
	ensure_intexpr(ast)
	if ast.typ == AST_INT {
		fmt.Printf("mov $%d, %%eax\n\t", ast.ival)
	} else {
		emit_binop(ast)
	}
}

func print_ast(ast *Ast) {
	switch ast.typ {
	case AST_INT:
		fmt.Printf("%d", ast.ival)
	case AST_STR:
		print_quote(ast.sval)
	default:
		fmt.Printf("(%c ", ast.typ)
		print_ast(ast.left)
		fmt.Printf(" ")
		print_ast(ast.right)
		fmt.Printf(")")
	}
}

func compile(ast *Ast) {
	if ast.typ == AST_STR {
		emit_string(ast)
	} else {
		fmt.Printf(".text\n\t"+
			".global intfn\n"+
				"intfn:\n\t")
		emit_intexpr(ast)
		fmt.Printf("ret\n")
	}
}

func main() {
	stdin = newStdin()
	ast := read_expr()
	if len(os.Args) > 1 && os.Args[1] == "-a" {
		print_ast(ast)
	} else {
		compile(ast)
	}
	return
}
