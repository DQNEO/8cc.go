package main

import (
	"errors"
	"fmt"
	"os"
)

const EXPR_LEN = 100
const MAX_ARGS = 6

const (
	AST_LITERAL byte = iota
	AST_VAR
	AST_FUNCALL
	AST_DECL
	AST_ADDR
	AST_DEREF
)

const (
	CTYPE_VOID int = iota
	CTYPE_INT
	CTYPE_CHAR
	CTYPE_STR
	CTYPE_PTR
)

type Ctype struct {
	typ int
	ptr *Ctype
}

type Ast struct {
	typ   byte
	ctype *Ctype
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
	binop struct {
		left  *Ast
		right *Ast
	}
	// Unary operator
	unary struct {
		operand *Ast
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

var ctype_int = &Ctype{CTYPE_INT, nil}
var ctype_char = &Ctype{CTYPE_CHAR, nil}
var ctype_str = &Ctype{CTYPE_STR, nil}

func make_ast_uop(typ byte, ctype *Ctype, operand *Ast) *Ast {
	r := &Ast{}
	r.typ = typ
	r.ctype = ctype
	r.unary.operand = operand
	return r
}

func make_ast_binop(typ byte, ctype *Ctype, left *Ast, right *Ast) *Ast {
	r := &Ast{}
	r.typ = typ
	r.ctype = ctype
	r.binop.left = left
	r.binop.right = right
	return r
}

func make_ast_int(val int) *Ast {
	r := &Ast{}
	r.typ = AST_LITERAL
	r.ctype = ctype_int
	r.ival = val
	return r
}

func make_ast_char(c byte) *Ast {
	r := &Ast{}
	r.typ = AST_LITERAL
	r.ctype = ctype_char
	r.c = c
	return r
}

func make_ast_var(ctype *Ctype, vname []byte) *Ast {
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

func make_ast_string(str []byte) *Ast {
	r := &Ast{}
	r.typ = AST_LITERAL
	r.ctype = ctype_str
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
	r.ctype = ctype_int // WHY??
	r.funcall.fname = fname
	r.funcall.nargs = nargs
	r.funcall.args = args
	return r
}

func make_ast_decl(variable *Ast, init *Ast) *Ast {
	r := &Ast{}
	r.typ = AST_DECL
	r.ctype = nil
	r.decl.decl_var = variable
	r.decl.decl_init = init
	return r
}

func make_ptr_type(ctype *Ctype) *Ctype {
	r := &Ctype{}
	r.typ = CTYPE_PTR
	r.ptr = ctype
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

func is_right_assoc(op byte) bool {
	return op == '='
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
		_error("Undefined varaible: %s", name)
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
		_error("unexpected character: '%c'", tk.v.punct)
	default:
		_error("Don't know how to handle '%d'", tk.typ)
	}

	return nil
}

func result_type_int(op byte, a *Ctype, b *Ctype) (*Ctype, error) {
	if a.typ > b.typ {
		b, a = a, b
	}

	default_err := errors.New("")
	var err error
	if b.typ == CTYPE_PTR {
		if a.typ != CTYPE_PTR {
			return nil, default_err
		}
		r := &Ctype{}
		r.typ = CTYPE_PTR
		r.ptr, err = result_type_int(op, a.ptr, b.ptr)
		if err != nil {
			return nil, err
		}
		return r, nil
	}

	switch a.typ {
	case CTYPE_VOID:
		return nil, default_err
	case CTYPE_INT:
		switch b.typ {
		case CTYPE_INT:
			return ctype_int, nil
		case CTYPE_CHAR:
			return ctype_int, nil
		case CTYPE_STR:
			return nil, default_err
		}
		_error("internal error")
	case CTYPE_CHAR:
		switch b.typ {
		case CTYPE_CHAR:
			return ctype_int, nil
		case CTYPE_STR:
			return nil, default_err
		}
		_error("internal error")
	case CTYPE_STR:
		return nil, default_err
	default:
		_error("internal error")
	}

	return nil, default_err
}

func result_type(op byte, a *Ast, b *Ast) *Ctype {
	ret, err := result_type_int(op, a.ctype, b.ctype)
	if err != nil {
		_error("incompatible operands: %c: <%s> and <%s>",
			op, ast_to_string(a), ast_to_string(b))
	}
	return ret
}

func ensure_lvalue(ast *Ast) {
	if ast.typ != AST_VAR {
		_error("variable expected")
	}
}

func read_unary_expr() *Ast {
	tok := read_token()
	if is_punct(tok, '&') {
		operand := read_unary_expr()
		ensure_lvalue(operand)
		return make_ast_uop(AST_ADDR, make_ptr_type(operand.ctype), operand)
	}
	if is_punct(tok, '*') {
		operand := read_unary_expr()
		if operand.ctype.typ != CTYPE_PTR {
			_error("pointer type expected, but got %", ast_to_string(operand))
		}
		return make_ast_uop(AST_DEREF, operand.ctype.ptr, operand)
	}
	unget_token(tok)
	return read_prim()
}

func read_expr(prec int) *Ast {
	ast := read_unary_expr()
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

		if is_punct(tok, '=') {
			ensure_lvalue(ast)
		}

		var prec_incr int
		if is_right_assoc(tok.v.punct) {
			prec_incr = 0
		} else {
			prec_incr = 1
		}
		rest := read_expr(prec2 + prec_incr)
		ctype := result_type(tok.v.punct, ast, rest)
		ast = make_ast_binop(tok.v.punct, ctype, ast, rest)
	}
	return ast
}

func get_ctype(tok *Token) *Ctype {
	if tok.typ != TTYPE_IDENT {
		return nil
	}
	//@TODO use string literal
	if strcmp(tok.v.sval, []byte("int\x00")) == 0 {
		return ctype_int
	}
	if strcmp(tok.v.sval, []byte("char\x00")) == 0 {
		return ctype_char
	}
	if strcmp(tok.v.sval, []byte("string\x00")) == 0 {
		return ctype_str
	}
	return nil
}

func is_type_keyword(tok *Token) bool {
	return get_ctype(tok) != nil
}

func expect(punct byte) {
	tok := read_token()
	if !is_punct(tok, punct) {
		_error("'%c' expected but got %s", punct, token_to_string(tok))
	}
}

func read_decl() *Ast {
	ctype := get_ctype(read_token())
	var tok *Token
	for {
		tok = read_token()
		if !is_punct(tok, '*') {
			break
		}
		// pointer
		ctype = make_ptr_type(ctype)
	}

	if tok.typ != TTYPE_IDENT {
		_error("Identifier expected, but got %s", token_to_string(tok))
	}
	variable := make_ast_var(ctype, tok.v.sval)
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

func emit_assign(variable *Ast, value *Ast) {
	emit_expr(value)
	printf("mov %%rax, -%d(%%rbp)\n\t", variable.variable.pos*8)
}

func emit_binop(ast *Ast) {
	if ast.typ == '=' {
		emit_assign(ast.binop.left, ast.binop.right)
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

	emit_expr(ast.binop.left)
	printf("push %%rax\n\t")
	emit_expr(ast.binop.right)
	if ast.typ == '/' {
		printf("mov %%rax, %%rbx\n\t")
		printf("pop %%rax\n\t")
		printf("mov $0, %%edx\n\t")
		printf("idiv %%rbx\n\t")
	} else {
		printf("pop %%rbx\n\t")
		printf("%s %%rbx, %%rax\n\t", op)
	}
}

func emit_expr(ast *Ast) {
	switch ast.typ {
	case AST_LITERAL:
		switch ast.ctype.typ {
		case CTYPE_INT:
			printf("mov $%d, %%rax\n\t", ast.ival)
		case CTYPE_CHAR:
			printf("mov $%d, %%rax\n\t", ast.c)
		case CTYPE_STR:
			printf("lea .s%d(%%rip), %%rax\n\t", ast.str.id)
		default:
			_error("internal error")
		}
	case AST_VAR:
		printf("mov -%d(%%rbp), %%rax\n\t", ast.variable.pos*8)
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
		printf("mov $0, %%rax\n\t")
		printf("call %s\n\t", bytes2string(ast.funcall.fname))
		for i := ast.funcall.nargs - 1; i >= 0; i-- {
			printf("pop %%%s\n\t", REGS[i])
		}
	case AST_DECL:
		emit_assign(ast.decl.decl_var, ast.decl.decl_init)
	case AST_ADDR:
		assert(ast.unary.operand.typ == AST_VAR)
		printf("lea -%d(%%rbp), %%rax\n\t", ast.unary.operand.variable.pos*8)
	case AST_DEREF:
		assert(ast.unary.operand.ctype.typ == CTYPE_PTR)
		emit_expr(ast.unary.operand)
		printf("mov (%%rax), %%rax\n\t")
	default:
		emit_binop(ast)
	}
}

func quote(sval []byte) string {
	var s string
	for _, c := range sval {
		if c == byte(0) {
			break
		}
		if c == '"' || c == '\\' {
			s += "\\"
		}
		s += fmt.Sprintf("%c", c)
	}
	return s
}

func ctype_to_string(ctype *Ctype) string {
	switch ctype.typ {
	case CTYPE_VOID:
		return "void"
	case CTYPE_INT:
		return "int"
	case CTYPE_CHAR:
		return "char"
	case CTYPE_STR:
		return "string"
	case CTYPE_PTR:
		return fmt.Sprintf("%s*", ctype_to_string(ctype.ptr))
	default:
		_error("Unknown ctype: %d", ctype)
	}

	return ""
}

func ast_to_string_int(ast *Ast) string {
	switch ast.typ {
	case AST_LITERAL:
		switch ast.ctype.typ {
		case CTYPE_INT:
			return fmt.Sprintf("%d", ast.ival)
		case CTYPE_CHAR:
			return fmt.Sprintf("'%c'", ast.c)
		case CTYPE_STR:
			return fmt.Sprintf("\"%s\"", quote(ast.str.val))
		default:
			_error("internal error")
			return ""
		}
	case AST_VAR:
		return fmt.Sprintf("%s", bytes2string(ast.variable.name))
	case AST_FUNCALL:
		s := fmt.Sprintf("%s(", bytes2string(ast.funcall.fname))
		for i := 0; ast.funcall.args[i] != nil; i++ {
			s += ast_to_string_int(ast.funcall.args[i])
			if ast.funcall.args[i+1] != nil {
				s += ","
			}
		}
		s += ")"
		return s
	case AST_DECL:
		return fmt.Sprintf("(decl %s %s %s)",
			ctype_to_string(ast.decl.decl_var.ctype),
			bytes2string(ast.decl.decl_var.variable.name),
			ast_to_string_int(ast.decl.decl_init))
	case AST_ADDR:
		return fmt.Sprintf("(& %s)", ast_to_string(ast.unary.operand))
	case AST_DEREF:
		return fmt.Sprintf("(* %s)", ast_to_string(ast.unary.operand))
	default:
		left := ast_to_string_int(ast.binop.left)
		right := ast_to_string_int(ast.binop.right)
		return fmt.Sprintf("(%c %s %s)", ast.typ, left, right)
	}
}

func ast_to_string(ast *Ast) string {
	return ast_to_string_int(ast)
}

func emit_data_section() {
	if strings == nil {
		return
	}
	printf("\t.data\n")
	for p := strings; p != nil; p = p.str.next {
		printf(".s%d:\n\t", p.str.id)
		printf(".string \"%s\"\n", quote(p.str.val))
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
			"mymain:\n\t" +
			"push %%rbp\n\t" +
			"mov %%rsp, %%rbp\n\t")
		if vars != nil {
			printf("sub $%d, %%rsp\n\t", vars.variable.pos*8)
		}
	}
	for i = 0; i < nexpr; i++ {
		if wantast {
			printf("%s", ast_to_string(exprs[i]))
		} else {
			emit_expr(exprs[i])
		}
	}

	if !wantast {
		printf("leave\n\t" +
			"ret\n")
	}
	return
}
