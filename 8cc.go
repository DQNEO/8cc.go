package main

import (
	"os"
	"fmt"
)

const BUFLEN = 256
const MAX_ARGS = 6

const (
	AST_INT byte = iota
	AST_SYM
	AST_FUNCALL
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
	op struct {
		left *Ast
		right *Ast
	}
	funcall struct {
		fname string
		nargs int
		args []*Ast
	}
}

var vars *Var
var REGS = []string{"rdi","rsi","rdx", "rcx", "r8", "r9"}

func _error(format string, args ...interface{}) {
	panic(fmt.Sprintf(format, args...))
	os.Exit(1)
}

func make_ast_op(typ byte, left *Ast, right *Ast) *Ast {
	r := &Ast{}
	r.typ = typ
	r.op.left = left
	r.op.right = right
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

func make_ast_funcall(fname string , nargs int, args []*Ast) *Ast {
	r := &Ast{}
	r.typ = AST_FUNCALL
	r.funcall.fname = fname
	r.funcall.nargs = nargs
	r.funcall.args = args
	return r
}

func find_var(name string) *Var {
	for v := vars;v != nil; v = v.next {
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

func read_ident(c byte) []byte {
	buf := make([]byte, BUFLEN)
	buf[0] = c
	i := 1
	for {
		c, _ := getc(stdin)
		if (!isalnum(c)) {
			ungetc(c, stdin)
			break
		}
		buf[i] = c
		i++
		if i == (BUFLEN -1) {
			_error("Identifier too long")
		}
	}
	//buf[i] = 0;
	return buf
}

func read_func_args(fname []byte) *Ast {
	args := make([]*Ast, MAX_ARGS+1)
	i := 0
	nargs := 0
	for ;i< MAX_ARGS; i++ {
		skip_space()
		c, _ := getc(stdin)
		if c == ')' {
			break
		}
		ungetc(c, stdin)
		args[i] = read_expr2(0)
		nargs++
		c, _ = getc(stdin)
		if c == ')' {
			break
		}
		if c == ',' {
			skip_space()
		} else {
			_error("Unexpected character: '%c", c)
		}
	}
	if i == MAX_ARGS {
		_error("Too many arguments: %s", fname)
	}
	return make_ast_funcall(string(fname), nargs, args)
}

func read_ident_or_func(c byte) *Ast {
	name := read_ident(c)
	skip_space()
	c, _ = getc(stdin)
	if c == '(' {
		return read_func_args(name)
	}
	ungetc(c, stdin)

	v := find_var(string(name))
	if v == nil {
		v = make_var(string(name))
	}
	return make_ast_sym(v)
}

func read_prim() *Ast {
	c, err := getc(stdin)
	if isdigit(c) {
		return read_number(int(c - '0'))
	} else if isalpha(c) {
		return read_ident_or_func(c)
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
		_error("Unterminated expression [%c]", c)
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
		emit_expr(ast.op.right)
		if ast.op.left.typ != AST_SYM {
			_error("Symbol expected")
		}
		printf("mov %%eax, -%d(%%rbp)\n\t", ast.op.left.variable.pos*4)
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

	emit_expr(ast.op.left)
	printf("push %%rax\n\t")
	emit_expr(ast.op.right)
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
	case AST_FUNCALL:
		for i := 0; i < ast.funcall.nargs; i++ {
			printf("push %%%s\n\t" , REGS[i])
		}
		for i := 0; i < ast.funcall.nargs; i++ {
			emit_expr(ast.funcall.args[i])
			printf("push %%rax\n\t")
		}
		for i := ast.funcall.nargs -1;i >= 0;i-- {
			printf("pop %%%s\n\t", REGS[i])
		}
		printf("mov $0, %%eax\n\t")
		printf("call %s\n\t", ast.funcall.fname)
		for i := ast.funcall.nargs -1;i >= 0;i-- {
			printf("pop %%%s\n\t", REGS[i])
		}
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
	case AST_FUNCALL:
		printf("%s(", ast.funcall.fname)
		for i:=0; ast.funcall.args[i] != nil;i++ {
			print_ast(ast.funcall.args[i])
			if ast.funcall.args[i+1] != nil{
				printf(",")
			}
		}
		printf(")")
	default:
		printf("(%c ", ast.typ)
		print_ast(ast.op.left)
		printf(" ")
		print_ast(ast.op.right)
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
