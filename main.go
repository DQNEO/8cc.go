package main

import (
	"fmt"
	"os"
)

const EXPR_LEN = 100
const MAX_ARGS = 6

const (
	AST_INT byte = iota
	AST_CHAR
	AST_VAR
	AST_STR
	AST_FUNCALL
	AST_DECL
)

const (
	CTYPE_VOID int = iota
	CTYPE_INT
	CTYPE_CHAR
	CTYPE_STR
)

type Ast struct {
	typ byte
	ctype int
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
		pos  int
		next *Ast
	}
	// Binary operator
	op struct {
		left  *Ast
		right *Ast
	}
	// Function call
	funcall struct {
		fname []byte
		nargs int
		args  []*Ast
	}
	// Declaration
	decl struct {
		decl_var  *Ast
		decl_init *Ast
	}
}

var vars *Ast
var strings *Ast
var REGS = []string{"rdi", "rsi", "rdx", "rcx", "r8", "r9"}

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

func make_ast_var(ctype int, vname []byte) *Ast {
	r := &Ast{}
	r.typ = AST_VAR
	r.ctype = ctype
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

func make_ast_string(str []byte) *Ast {
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

func make_ast_funcall(fname []byte, nargs int, args []*Ast) *Ast {
	r := &Ast{}
	r.typ = AST_FUNCALL
	r.funcall.fname = fname
	r.funcall.nargs = nargs
	r.funcall.args = args
	return r
}

func make_ast_decl(variable *Ast, init *Ast) *Ast {
	r := &Ast{}
	r.typ = AST_DECL
	r.decl.decl_var = variable
	r.decl.decl_init = init
	return r
}

func find_var(name []byte) *Ast {
	for v := vars; v != nil; v = v.variable.next {
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
	case '-':
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
	for ; i < MAX_ARGS; i++ {
		tok := read_token()
		if is_punct(tok, ')') {
			break
		}
		unget_token(tok)
		args[i] = read_expr(0)
		nargs++
		tok = read_token()
		if is_punct(tok, ')') {
			break
		}
		if !is_punct(tok, ',') {
			_error("Unexpected token: '%s'", token_to_string(tok))
		}
	}
	if i == MAX_ARGS {
		_error("Too many arguments: %s", fname)
	}
	return make_ast_funcall(fname, nargs, args)
}

func read_ident_or_func(name []byte) *Ast {
	ch := read_token()
	if is_punct(ch, '(') {
		return read_func_args(name)
	}
	unget_token(ch)

	v := find_var(name)
	if v == nil {
		_error("Undefined varaible: %s", name);
	}
	return v
}

func read_prim() *Ast {
	tk := read_token()
	if tk == nil {
		return nil
	}
	switch tk.typ {
	case TTYPE_IDENT:
		return read_ident_or_func(tk.v.sval)
	case TTYPE_INT:
		return make_ast_int(tk.v.ival)
	case TTYPE_CHAR:
		return make_ast_char(tk.v.c)
	case TTYPE_STRING:
		return make_ast_string(tk.v.sval)
	case TTYPE_PUNCT:
		_error("unexpected character: '%c'", tk.v.c)
	default:
		_error("Don't know how to handle '%d'", tk.typ)
	}

	return nil
}

func ensure_lvalue(ast *Ast) {
	if ast.typ != AST_VAR {
		_error("variable expected")
	}
}

func read_expr(prec int) *Ast {
	ast := read_prim()
	if ast == nil {
		return nil
	}
	for {
		tok := read_token()
		if tok.typ != TTYPE_PUNCT {
			unget_token(tok)
			return ast
		}
		prec2 := priority(tok.v.punct)
		if prec2 < 0 || prec2 < prec {
			unget_token(tok)
			return ast
		}

		if (is_punct(tok, '=')) {
			ensure_lvalue(ast)
		}
		ast = make_ast_op(tok.v.punct, ast, read_expr(prec2+1))
	}
	return ast
}

func get_ctype(tok *Token) int {
	if tok.typ != TTYPE_IDENT {
		return -1
	}
	if strcmp(tok.v.sval, []byte{'i','n','t',0}) == 0 {
		return CTYPE_INT
	}
	if strcmp(tok.v.sval, []byte{'c','h','a','r', 0}) == 0 {
		return CTYPE_CHAR
	}
	if strcmp(tok.v.sval, []byte{'s','t','r','i','n','g',0}) == 0 {
		return CTYPE_STR
	}
	return -1
}

func is_type_keyword(tok *Token) bool {
	return get_ctype(tok) != -1
}

func expect(punct byte) {
	tok := read_token()
	if !is_punct(tok, punct) {
		_error("%punct expected but got %s", punct, token_to_string(tok))
	}
}


func read_decl() *Ast {
	ctype := get_ctype(read_token())
	name := read_token()
	if name.typ != TTYPE_IDENT {
		_error("Identifier expected, but got %s", token_to_string(name))
	}
	variable := make_ast_var(ctype, name.v.sval)
	expect('=')
	init := read_expr(0)
	return make_ast_decl(variable, init)
}

func read_decl_or_stmt() *Ast {
	tok := peek_token()
	if tok == nil {
		return nil
	}
	var r *Ast
	if is_type_keyword(tok) {
		r = read_decl()
	} else {
		r = read_expr(0)
	}
	// should use expect(';')
	tok2 := read_token()
	if !is_punct(tok2, ';') {
		_error("Unterminated expression %s", token_to_string(tok2))
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

func emit_assign(variable *Ast, value *Ast) {
	emit_expr(value)
	printf("mov %%eax, -%d(%%rbp)\n\t", variable.variable.pos * 4)
}

func emit_binop(ast *Ast) {
	if ast.typ == '=' {
		emit_assign(ast.op.left, ast.op.right)
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
			printf("push %%%s\n\t", REGS[i])
		}
		for i := 0; i < ast.funcall.nargs; i++ {
			emit_expr(ast.funcall.args[i])
			printf("push %%rax\n\t")
		}
		for i := ast.funcall.nargs - 1; i >= 0; i-- {
			printf("pop %%%s\n\t", REGS[i])
		}
		printf("mov $0, %%eax\n\t")
		printf("call %s\n\t", bytes2string(ast.funcall.fname))
		for i := ast.funcall.nargs - 1; i >= 0; i-- {
			printf("pop %%%s\n\t", REGS[i])
		}
	case AST_DECL:
		emit_assign(ast.decl.decl_var, ast.decl.decl_init)
	default:
		emit_binop(ast)
	}
}

func ctype_to_string(ctype int) string {
	switch ctype {
	case CTYPE_VOID:
		return "void"
	case CTYPE_INT:
		return "int"
	case CTYPE_CHAR:
		return "char"
	case CTYPE_STR:
		return "string"
	default:
		_error("Unknown ctype: %d", ctype)
	}

	return ""
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
		for i := 0; ast.funcall.args[i] != nil; i++ {
			print_ast(ast.funcall.args[i])
			if ast.funcall.args[i+1] != nil {
				printf(",")
			}
		}
		printf(")")
	case AST_DECL:
		printf("(decl %s %s ",
			ctype_to_string(ast.decl.decl_var.ctype),
			bytes2string(ast.decl.decl_var.variable.name))
		print_ast(ast.decl.decl_init)
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
		t := read_decl_or_stmt()
		if t == nil {
			break
		}
		exprs[i] = t
	}
	nexpr := i
	if !wantast {
		emit_data_section()
		printf(".text\n\t" +
			".global mymain\n" +
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
