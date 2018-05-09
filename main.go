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
	AST_STRING
	AST_LVAR
	AST_LREF
	AST_FUNCALL
	AST_DECL
	AST_ARRAY_INIT
	AST_ADDR
	AST_DEREF
)

const (
	CTYPE_VOID int = iota
	CTYPE_INT
	CTYPE_CHAR
	CTYPE_ARRAY
	CTYPE_PTR
)

type Ctype struct {
	typ int
	ptr *Ctype
	size int
}

type Ast struct {
	typ   byte
	ctype *Ctype
	next *Ast
	// want to be "union"
	// Integer
	ival int
	// Char
	c byte
	// String
	str struct {
		val  []byte
		slabel   string
	}
	// Local variable
	variable struct {
		lname []byte
		loff  int
	}
	// Local reference
	lref struct {
		ref *Ast
		off int
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
		declvar  *Ast
		declinit *Ast
	}
	array_initializer struct {
		size int
		array_init []*Ast
	}
}

var globals *Ast
var locals *Ast

var labelseq = 0;
var REGS = []string{"rdi", "rsi", "rdx", "rcx", "r8", "r9"}

var ctype_int = &Ctype{CTYPE_INT, nil, 0}
var ctype_char = &Ctype{CTYPE_CHAR, nil, 0}

func ast_uop(typ byte, ctype *Ctype, operand *Ast) *Ast {
	r := &Ast{}
	r.typ = typ
	r.ctype = ctype
	r.unary.operand = operand
	return r
}

func ast_binop(typ byte, ctype *Ctype, left *Ast, right *Ast) *Ast {
	r := &Ast{}
	r.typ = typ
	r.ctype = ctype
	r.binop.left = left
	r.binop.right = right
	return r
}

func ast_int(val int) *Ast {
	r := &Ast{}
	r.typ = AST_LITERAL
	r.ctype = ctype_int
	r.ival = val
	return r
}

func ast_char(c byte) *Ast {
	r := &Ast{}
	r.typ = AST_LITERAL
	r.ctype = ctype_char
	r.c = c
	return r
}

func make_next_label() string {
	ret := labelseq
	labelseq++
	return fmt.Sprintf(".L%d", ret)
}

func ast_lvar(ctype *Ctype, name []byte) *Ast {
	r := &Ast{}
	r.typ = AST_LVAR
	r.ctype = ctype
	r.variable.lname = name

	r.next = nil
	if locals != nil {
		var p *Ast
		for p = locals; p.next != nil; p = p.next {
		}
		p.next = r
	} else {
		locals = r
	}
	return r
}

func ast_lref(ctype *Ctype, lvar *Ast, off int) *Ast {
	r := &Ast{}
	r.typ = AST_LREF
	r.ctype = ctype
	r.lref.ref = lvar
	r.lref.off = off
	return r
}

func ast_string(str []byte) *Ast {
	r := &Ast{}
	r.typ = AST_STRING
	r.ctype = make_array_type(ctype_char, strlen(str) + 1)
	r.str.val = str
	r.str.slabel = make_next_label()
	r.next = globals

	globals = r
	return r
}

func ast_funcall(fname []byte, nargs int, args []*Ast) *Ast {
	r := &Ast{}
	r.typ = AST_FUNCALL
	r.ctype = ctype_int // WHY??
	r.funcall.fname = fname
	r.funcall.nargs = nargs
	r.funcall.args = args
	return r
}

func ast_decl(variable *Ast, init *Ast) *Ast {
	r := &Ast{}
	r.typ = AST_DECL
	r.ctype = nil
	r.decl.declvar = variable
	r.decl.declinit = init
	return r
}

func ast_array_init(size int, array_init []*Ast) *Ast {
	r := &Ast{}
	r.typ = AST_ARRAY_INIT
	r.ctype = nil
	r.array_initializer.size = size
	r.array_initializer.array_init = array_init
	return r
}

func make_ptr_type(ctype *Ctype) *Ctype {
	r := &Ctype{}
	r.typ = CTYPE_PTR
	r.ptr = ctype
	return r
}

func make_array_type(ctype *Ctype, size int) *Ctype {
	r := &Ctype{}
	r.typ = CTYPE_ARRAY
	r.ptr = ctype
	r.size = size
	return r
}

func find_var(name []byte) *Ast {
	for v := locals; v != nil; v = v.next {
		if strcmp(name, v.variable.lname) == 0 {
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
	return ast_funcall(fname, nargs, args)
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
		return ast_int(tk.v.ival)
	case TTYPE_CHAR:
		return ast_char(tk.v.c)
	case TTYPE_STRING:
		return ast_string(tk.v.sval)
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
	if b.typ == CTYPE_PTR {
		if op != '+' && op != '-' {
			return nil, default_err
		}
		if a.typ != CTYPE_INT {
			return nil, default_err
		}
		return b, nil
	}

	switch a.typ {
	case CTYPE_VOID:
		return nil, default_err
	case CTYPE_INT:
		fallthrough
	case CTYPE_CHAR:
		switch b.typ {
		case CTYPE_INT:
			fallthrough
		case CTYPE_CHAR:
			return ctype_int, nil
		case CTYPE_ARRAY:
			fallthrough
		case CTYPE_PTR:
			return b, nil
		}
		_error("internal error")
	case CTYPE_ARRAY:
		return nil, default_err
	default:
		_error("internal error")
	}

	return nil, default_err
}

func result_type(op byte, a *Ctype, b *Ctype) *Ctype {
	ret, err := result_type_int(op, a, b)
	if err != nil {
		_error("incompatible operands: %c: <%s> and <%s>",
			op, ctype_to_string(a), ctype_to_string(b))
	}
	return ret
}

func ensure_lvalue(ast *Ast) {
	switch ast.typ {
	case AST_LVAR, AST_LREF:
		return
	}
	_error("lvalue expected, but got %s", ast_to_string(ast))
	return
}

func read_unary_expr() *Ast {
	tok := read_token()
	if is_punct(tok, '&') {
		operand := read_unary_expr()
		ensure_lvalue(operand)
		return ast_uop(AST_ADDR, make_ptr_type(operand.ctype), operand)
	}
	if is_punct(tok, '*') {
		operand := read_unary_expr()
		if operand.ctype.typ != CTYPE_PTR {
			_error("pointer type expected, but got %", ast_to_string(operand))
		}
		return ast_uop(AST_DEREF, operand.ctype.ptr, operand)
	}
	unget_token(tok)
	return read_prim()
}

func convert_array(ast *Ast) *Ast {
	if ast.typ == AST_STRING {
		return ast
	}
	if ast.ctype.typ != CTYPE_ARRAY {
		return ast
	}
	if ast.typ == AST_LVAR {
		return ast_lref(make_ptr_type(ast.ctype.ptr), ast, 0)
	}
	return ast
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
		} else {
			ast = convert_array(ast)
		}

		var prec_incr int
		if is_right_assoc(tok.v.punct) {
			prec_incr = 0
		} else {
			prec_incr = 1
		}
		rest := read_expr(prec2 + prec_incr)
		rest = convert_array(rest)
		ctype := result_type(tok.v.punct, ast.ctype, rest.ctype)
		if !is_punct(tok, '=') && ast.ctype.typ != CTYPE_PTR &&
			rest.ctype.typ == CTYPE_PTR {
				ast,rest = rest,ast
		}
		ast = ast_binop(tok.v.punct, ctype, ast, rest)
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

func read_decl_array_initializer(ctype *Ctype) *Ast {
	tok := read_token()
	if ctype.ptr.typ == CTYPE_CHAR && tok.typ == TTYPE_STRING {
		return ast_string(tok.v.sval)
	}

	if !is_punct(tok, '{') {
		_error("Expected an initializer list, but got %s", token_to_string(tok))
	}
	init := make([]*Ast, ctype.size)
	for i := 0; i < ctype.size; i++ {
		init[i] = read_expr(0)
		result_type('=', init[i].ctype, ctype.ptr)
		tok = read_token()
		if is_punct(tok, '}') && i == ctype.size - 1  {
			break
		}
		if !is_punct(tok, ',') {
			_error("comma expected, but got %s", token_to_string(tok))
		}
		if i == ctype.size - 1 {
			tok = read_token()
			if !is_punct(tok, '}') {
				_error("'}' expected, but got %s", token_to_string(tok))
			}
			//break // we don't need to break
		}
	}

	return ast_array_init(ctype.size, init)
}

func read_declinitializer(ctype *Ctype) *Ast {
	if ctype.typ == CTYPE_ARRAY {
		return read_decl_array_initializer(ctype)
	}
	return read_expr(0)
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
	varname := tok
	for { // we need to loop?
		tok = read_token()
		if is_punct(tok, '[') {
			size := read_expr(0)
			//                            wny not compare to size.ctype != ctype_int ?
			if size.typ != AST_LITERAL || size.ctype.typ != CTYPE_INT {
				_error("Integer expected, but got %s", ast_to_string(size))
			}
			expect(']')
			ctype = make_array_type(ctype, size.ival)
		} else {
			unget_token(tok)
			break
		}
	}
	variable := ast_lvar(ctype, varname.v.sval)
	expect('=')
	init := read_declinitializer(ctype)
	return ast_decl(variable, init)
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

func ctype_size(ctype *Ctype) int {
	switch ctype.typ {
	case CTYPE_CHAR:
		return 1
	case CTYPE_INT:
		return 4
	case CTYPE_PTR:
		return 8
	case CTYPE_ARRAY:
		return ctype_size(ctype.ptr) * ctype.size
	default:
		_error("internal error")
	}
	return -1
}

func emit_lload(v *Ast, off int) {
	if v.ctype.typ == CTYPE_ARRAY {
		printf("lea -%d(%%rbp), %%rax\n\t", v.variable.loff)
		return;
	}
	size := ctype_size(v.ctype)
	switch size {
	case 1:
		printf("mov $0, %%eax\n\t")
		printf("mov -%d(%%rbp), %%al\n\t", v.variable.loff)
	case 4:
		printf("mov -%d(%%rbp), %%eax\n\t", v.variable.loff)
	case 8:
		printf("mov -%d(%%rbp), %%rax\n\t", v.variable.loff)
	default:
		_error("Unknown data size: %s: %d", ast_to_string(v), size)
	}
	if off > 0 {
		printf("add $%d, %%rax\n\t", size, off * size)
	}
}

func emit_lsave(ctype *Ctype, loff int, off int) {
	var reg string
	size := ctype_size(ctype)
	switch size {
	case 1:
		reg = "al"
	case 4:
		reg = "eax"
	case 8:
		reg = "rax"
	}
	printf("mov %%%s, -%d(%%rbp)\n\t", reg, loff + off * size)
}

func emit_pointer_arith(op byte, left *Ast, right *Ast) {
	assert(left.ctype.typ == CTYPE_PTR)
	emit_expr(left)
	printf("push %%rax\n\t")
	emit_expr(right)
	size := ctype_size(left.ctype.ptr)
	if size > 1 {
		printf("imul $%d, %%rax\n\t", size)
	}
	printf("mov %%rax, %%rbx\n\t"+
		"pop %%rax\n\t"+
		"add %%rbx, %%rax\n\t")
}

func emit_assign(variable *Ast, value *Ast) {
	emit_expr(value)
	switch variable.typ {
	case AST_LVAR:
		emit_lsave(variable.ctype, variable.variable.loff, 0)
	case AST_LREF:
		emit_lsave(variable.lref.ref.ctype, variable.lref.ref.variable.loff, variable.lref.off)
	default:
		_error("internal error")
	}
	printf("mov %%rax, -%d(%%rbp)\n\t", variable.variable.loff)
}

func emit_binop(ast *Ast) {
	if ast.typ == '=' {
		emit_assign(ast.binop.left, ast.binop.right)
		return
	}

	if ast.ctype.typ == CTYPE_PTR {
		emit_pointer_arith(ast.typ, ast.binop.left, ast.binop.right)
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
			printf("mov $%d, %%eax\n\t", ast.ival)
		case CTYPE_CHAR:
			printf("mov $%d, %%rax\n\t", ast.c)
		default:
			_error("internal error")
		}
	case AST_STRING:
		printf("lea %s(%%rip), %%rax\n\t", ast.str.slabel)
	case AST_LVAR:
		emit_lload(ast, 0)
	case AST_LREF:
		assert(ast.lref.ref.typ == AST_LVAR)
		emit_lload(ast.lref.ref, ast.lref.off)
	case AST_FUNCALL:
		for i := 1; i < ast.funcall.nargs; i++ {
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
		if ast.decl.declinit.typ == AST_ARRAY_INIT {
			for i := 0; i < ast.decl.declinit.array_initializer.size; i++ {
				emit_expr(ast.decl.declinit.array_initializer.array_init[i])
				emit_lsave(ast.decl.declvar.ctype.ptr, ast.decl.declvar.variable.loff, -i)
			}
		} else if ast.decl.declvar.ctype.typ == CTYPE_ARRAY {
			assert(ast.decl.declinit.typ == AST_STRING)
			var i int
			for i = 0; ast.decl.declinit.str.val[i] != 0; i++ {
				printf("movb $%d, -%d(%%rbp)\n\t", ast.decl.declinit.str.val[i], ast.decl.declvar.variable.loff-i)
			}
			printf("movb $0, -%d(%%rbp)\n\t", ast.decl.declvar.variable.loff-i)
		} else if ast.decl.declinit.typ == AST_STRING {
			printf("lea %s(%%rip), %%rax\n\t", ast.decl.declinit.str.slabel); //emit_gload
			emit_lsave(ast.decl.declvar.ctype, ast.decl.declvar.variable.loff, 0)
		} else {
			emit_expr(ast.decl.declinit)
			emit_lsave(ast.decl.declvar.ctype, ast.decl.declvar.variable.loff,0)
		}
	case AST_ADDR:
		assert(ast.unary.operand.typ == AST_LVAR)
		printf("lea -%d(%%rbp), %%rax\n\t", ast.unary.operand.variable.loff)
	case AST_DEREF:
		assert(ast.unary.operand.ctype.typ == CTYPE_PTR)
		emit_expr(ast.unary.operand)
		var reg string
		switch ctype_size(ast.ctype) {
		case 1:
			reg = "%bl"
		case 4:
			reg = "%ebx"
		case 8:
			reg = "%rbx"
		default:
			_error("internal error")
		}
		printf("mov $0, %%ebx\n\t")
		printf("mov (%%rax), %s\n\t", reg)
		printf("mov %%rbx, %%rax\n\t")
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
	case CTYPE_PTR:
		return fmt.Sprintf("%s*", ctype_to_string(ctype.ptr))
	case CTYPE_ARRAY:
		return fmt.Sprintf("%s[%d]", ctype_to_string(ctype.ptr), ctype.size)
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
		default:
			_error("internal error")
			return ""
		}
	case AST_STRING:
		return fmt.Sprintf("\"%s\"", quote(ast.str.val))
	case AST_LVAR:
		return fmt.Sprintf("%s", bytes2string(ast.variable.lname))
	case AST_LREF:
		return fmt.Sprintf("%s[%d]", ast_to_string(ast.lref.ref), ast.lref.off)
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
			ctype_to_string(ast.decl.declvar.ctype),
			bytes2string(ast.decl.declvar.variable.lname),
			ast_to_string_int(ast.decl.declinit))
	case AST_ARRAY_INIT:
		s := "{"
		for i:= 0; i < ast.array_initializer.size; i++ {
			s += ast_to_string_int(ast.array_initializer.array_init[i])
			if i != ast.array_initializer.size - 1 {
				s += ","
			}
		}
		s += "}"
		return s
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
	if globals == nil {
		return
	}
	printf("\t.data\n")
	for p := globals; p != nil; p = p.next {
		assert(p.typ == AST_STRING)
		printf("%s:\n\t", p.str.slabel)
		printf(".string \"%s\"\n", quote(p.str.val))
	}
	printf("\t")

}

func ceil8(n int) int {
	rem := n % 8
	if rem == 0 {
		return n
	} else {
		return n - rem + 8
	}
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
		off := 0
		for p := locals; p != nil; p = p.next {
			off += ceil8(ctype_size(p.ctype))
			p.variable.loff = off
		}
		emit_data_section()
		printf(".text\n\t" +
			".global mymain\n" +
			"mymain:\n\t" +
			"push %%rbp\n\t" +
			"mov %%rsp, %%rbp\n\t")
		if locals != nil {
			printf("sub $%d, %%rsp\n\t", off)
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
