package main

import (
	"os"
	"fmt"
)

const BUFLEN = 256
const EXPR_LEN = 100
const MAX_ARGS = 6

const (
	AST_INT byte = iota
	AST_CHAR
	AST_VAR
	AST_STR
	AST_FUNCALL
)

const (
	TTYPE_IDENT int = iota
	TTYPE_PUNCT
	TTYPE_INT
	TTYPE_CHAR
	TTYPE_STRING
	)

type Ast struct {
	typ byte
	// want to be "union"
	// Integer
	ival int
	// Char
	c byte
	// String
	str struct {
		val  []byte
		id   int
		next *Ast
	}
	// Variable
	variable struct {
		name []byte
		pos int
		next *Ast
	}
	// Binary operator
	op struct {
		left *Ast
		right *Ast
	}
	// Function call
	funcall struct {
		fname []byte
		nargs int
		args []*Ast
	}
}

var vars *Ast
var strings *Ast
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

func make_ast_var(vname []byte) *Ast {
	r := &Ast{}
	r.typ = AST_VAR
	r.variable.name = vname
	if vars == nil {
		r.variable.pos = 1
	} else {
		r.variable.pos = vars.variable.pos + 1
	}
	r.variable.next = vars
	vars = r
	return r
}

func make_ast_char(c byte) *Ast {
	r := &Ast{}
	r.typ = AST_CHAR
	r.c = c
	return r
}

func make_ast_str(str []byte) *Ast{
	r := &Ast{}
	r.typ = AST_STR
	r.str.val = str

	if strings == nil {
		r.str.id = 0
		r.str.next = nil
	} else {
		r.str.id = strings.str.id + 1
		r.str.next = strings
	}

	strings = r
	return r
}

func make_ast_funcall(fname []byte , nargs int, args []*Ast) *Ast {
	r := &Ast{}
	r.typ = AST_FUNCALL
	r.funcall.fname = fname
	r.funcall.nargs = nargs
	r.funcall.args = args
	return r
}

func find_var(name []byte) *Ast {
	for v := vars;v != nil; v = v.variable.next {
		if strcmp(name, v.variable.name) == 0 {
			return v
		}
	}
	return nil
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


func read_func_args(fname []byte) *Ast {
	args := make([]*Ast, MAX_ARGS+1)
	i := 0
	nargs := 0
	for ;i< MAX_ARGS; i++ {
		ch := skip_space_read_ch()
		if is_punct(ch, ')') {
			break
		}
		unget_ch(ch)
		args[i] = read_expr2(0)
		nargs++
		ch = skip_space_read_ch()
		if is_punct(ch, ')') {
			break
		}
		if is_punct(ch, ',') {
			skip_space()
		} else {
			_error("Unexpected character: '%c", ch.c)
		}
	}
	if i == MAX_ARGS {
		_error("Too many arguments: %s", fname)
	}
	return make_ast_funcall(fname, nargs, args)
}

func read_ident_or_func(c byte) *Ast {
	name := read_ident(c)
	ch := skip_space_read_ch()
	if is_punct(ch, '(') {
		return read_func_args(name)
	}
	unget_ch(ch)

	v := find_var(name)
	if v != nil {
		return v
	} else {
		return make_ast_var(name)
	}
}

func read_prim() *Ast {
	ch := skip_space_read_ch()
	if ch == nil {
		return nil
	}
	switch ch.typ {
	case TTYPE_INT:
		return read_number(int(ch.c - '0'))
	case TTYPE_CHAR:
		return read_char()
	case TTYPE_STRING:
		return read_string()
	case TTYPE_IDENT:
		return read_ident_or_func(ch.c)
	case TTYPE_PUNCT:
		_error("unexpected character: '%c'", ch.c)
	default:
		_error("Don't know how to handle '%c'", ch.c)
	}
	return nil
}

func read_expr2(prec int) *Ast {
	ast := read_prim()
	for {
	op := skip_space_read_ch()
	if op == nil {
		return ast
	}
	prec2 := priority(op.c)
	if prec2 < prec {
		unget_ch(op)
		return ast
	}
	skip_space()
	ast = make_ast_op(op.c, ast, read_expr2(prec2+1))
	}
	return ast
}

func read_expr() *Ast {
	r := read_expr2(0)
	if r == nil {
		return nil
	}
	ch := skip_space_read_ch()
	if !is_punct(ch, ';') {
		_error("Unterminated expression [%c]", ch.c)
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

func emit_binop(ast *Ast) {
	if ast.typ == '=' {
		emit_expr(ast.op.right)
		if ast.op.left.typ != AST_VAR {
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
	case AST_VAR:
		printf("mov -%d(%%rbp), %%eax\n\t", ast.variable.pos*4)
	case AST_STR:
		printf("lea .s%d(%%rip), %%rax\n\t", ast.str.id)
	case AST_CHAR:
		printf("mov $%d, %%eax\n\t", ast.c)
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
		printf("call %s\n\t", bytes2string(ast.funcall.fname))
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
	case AST_VAR:
		printf("%s", bytes2string(ast.variable.name))
	case AST_CHAR:
		printf("'%c'", ast.c)
	case AST_STR:
		printf("\"")
		print_quote(ast.str.val)
		printf("\"")
	case AST_FUNCALL:
		printf("%s(", bytes2string(ast.funcall.fname))
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


func emit_data_section() {
	if strings == nil {
		return
	}
	printf("\t.data\n")
	for p := strings; p != nil; p = p.str.next {
		printf(".s%d:\n\t", p.str.id)
		printf(".string \"")
		print_quote(p.str.val)
		printf("\"\n")
	}
	printf("\t")

}
func main() {
	initStdin()
	wantast := (len(os.Args) > 1 && os.Args[1] == "-a")
	var exprs [EXPR_LEN]*Ast
	var i int
	for i = 0; i < EXPR_LEN; i++ {
		t := read_expr()
		if t == nil {
			break
		}
		exprs[i] = t
	}
	nexpr := i
	if !wantast {
		emit_data_section()
		printf(".text\n\t"+
			".global mymain\n"+
			"mymain:\n\t")
	}
	for i = 0; i < nexpr; i++ {
		if wantast {
			print_ast(exprs[i])
		} else {
 			emit_expr(exprs[i])
		}
	}

	if !wantast {
		printf("ret\n")
	}
	return
}
