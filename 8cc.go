package main

import (
	"os"
)

const BUFLEN = 256

const (
	AST_INT byte = iota
	AST_SYM
)

type Var struct {
	name string
	pos int
	next *Var
}

type Ast struct {
	typ byte
	ival int
	sval []byte
	variable *Var
	left *Ast
	right *Ast
}

var vars *Var

func _error(format string, args ...interface{}) {
	printf(format, args...)
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

func make_ast_sym(v *Var) *Ast {
	r := &Ast{}
	r.typ = AST_SYM
	r.variable = v
	return r
}

func find_var(name string) *Var {
	v := vars
	for ;v != nil; v = v.next {
		if v.name == name {
			return v
		}
	}
	return nil
}

func make_var(name string) *Var {
	v := &Var{}
	v.name = name
	if vars == nil {
		v.pos = 1
	} else {
		v.pos = vars.pos + 1
	}
	v.next = vars
	vars = v
	return v
}

func skip_space() {
	for {
		c, err := getc(stdin)
		if err != nil {
			break
		}
		if isspace(c) {
			continue
		}
		ungetc(c, stdin)
		return
	}
}

func priority(op byte) int {
	switch op {
	case '=':
		return 1
	case '+':
		return 2
	case '-' :
		return 2
	case '*':
		return 3
	case '/':
		return 3
	default:
		return -1
	}
}

func read_number(n int) *Ast {
	for {
		c, _ := getc(stdin)
		if !isdigit(c) {
			ungetc(c, stdin)
			return make_ast_int(n)
		}
		n = n * 10 + int(c - '0')
	}
}

func isalpha(c byte) bool {
	return  byte('a') <= c && c <= byte('z')
}

func read_symbol(c byte) *Ast {
	buf := make([]byte, BUFLEN)
	buf[0] = c
	i := 1
	for {
		c, _ := getc(stdin)
		if (!isalpha(c)) {
			ungetc(c, stdin)
			break
		}
		buf[i] = c
		i++
		if i == (BUFLEN -1) {
			_error("Symbol too long")
		}
	}
	buf[i] = 0;
	v := find_var(string(buf))
	if v == nil {
		v = make_var(string(buf))
	}
	return make_ast_sym(v)
}

func read_prim() *Ast {
	c, err := getc(stdin)
	if isdigit(c) {
		return read_number(int(c - '0'))
	} else if isalpha(c) {
		return read_symbol(c)
	} else if err != nil {
		return nil
	}
	_error("Don't know how to handle '%c'", c)
	return nil
}

func read_expr2(prec int) *Ast {
	skip_space()
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

func read_expr() *Ast {
	r := read_expr2(0)
	if r == nil {
		return nil
	}
	skip_space()
	c, _ := getc(stdin)
	if c != ';' {
		_error("Unterminated expression")
	}
	return r
}

func print_quote(sval []byte) {
	for _, c := range sval {
		if c == byte(0) {
			break
		}
		if c == '"' || c == '\\' {
			printf("\\")
		}
		printf("%c", c)
	}
}

func emit_string(ast *Ast) {
	printf("\t.data\n"+
		".mydata:\n\t"+
		".string \"")
	print_quote(ast.sval)
	printf("\"\n\t"+
		".text\n\t"+
		".global stringfn\n"+
		"stringfn:\n\t"+
		"lea .mydata(%%rip), %%rax\n\t"+
		"ret\n")
}

func emit_binop(ast *Ast) {
	if ast.typ == '=' {
		emit_expr(ast.right)
		if ast.left.typ != AST_SYM {
			_error("Symbol expected")
		}
		printf("mov %%eax, -%d(%%rbp)\n\t", ast.left.variable.pos*4)
		return
	}

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

	emit_expr(ast.left)
	printf("push %%rax\n\t")
	emit_expr(ast.right)
	if ast.typ == '/' {
		printf("mov %%eax, %%ebx\n\t")
		printf("pop %%rax\n\t")
		printf("mov $0, %%edx\n\t")
		printf("idiv %%ebx\n\t")
	} else {
		printf("pop %%rbx\n\t")
		printf("%s %%ebx, %%eax\n\t", op)
	}
}

func emit_expr(ast *Ast) {
	switch ast.typ {
	case AST_INT:
		printf("mov $%d, %%eax\n\t", ast.ival)
	case AST_SYM:
		printf("mov -%d(%%rbp), %%eax\n\t", ast.variable.pos*4)
	default:
		emit_binop(ast)
	}
}

func print_ast(ast *Ast) {
	switch ast.typ {
	case AST_INT:
		printf("%d", ast.ival)
	case AST_SYM:
		printf("%s", ast.variable.name)
	default:
		printf("(%c ", ast.typ)
		print_ast(ast.left)
		printf(" ")
		print_ast(ast.right)
		printf(")")
	}
}

func main() {
	initStdin()
	wantast := (len(os.Args) > 1 && os.Args[1] == "-a")
	if !wantast {
		printf(".text\n\t"+
			".global mymain\n"+
			"mymain:\n\t")
	}
	for {
		ast := read_expr()
		if ast == nil {
			break
		}
		if wantast {
			print_ast(ast)
		} else {
 			emit_expr(ast)
		}
	}

	if !wantast {
		printf("ret\n")
	}
	return
}
