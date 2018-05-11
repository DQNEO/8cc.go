package main

import (
	"errors"
	"fmt"
)

const MAX_ARGS = 6

var globals *Ast
var locals *Ast

var labelseq = 0

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

func make_next_label() []byte {
	seq := labelseq
	labelseq++
	s := fmt.Sprintf(".L%d", seq)
	return []byte(s + "\x00")
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

func ast_gvar(ctype *Ctype, name []byte, filelocal bool) *Ast {
	r := &Ast{}
	r.typ = AST_GVAR
	r.ctype = ctype
	r.gvar.gname = name
	if filelocal {
		r.gvar.glabel = make_next_label()
	} else {
		r.gvar.glabel = name
	}
	r.next = nil
	if globals != nil {
		var p *Ast
		for p = locals; p.next != nil; p = p.next {
		}
		p.next = r
	} else {
		globals = r
	}
	return r
}

func ast_gref(ctype *Ctype, gvar *Ast, off int) *Ast {
	r := &Ast{}
	r.typ = AST_GREF
	r.ctype = ctype
	r.gref.ref = gvar
	r.gref.off = off
	return r
}

func ast_string(str []byte) *Ast {
	r := &Ast{}
	r.typ = AST_STRING
	r.ctype = make_array_type(ctype_char, strlen(str)+1)
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

	for v := globals; v != nil; v = v.next {
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
	case AST_LVAR, AST_LREF,
		AST_GVAR, AST_GREF:
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
		return ast_gref(make_ptr_type(ctype_char), ast, 0)
	}
	if ast.ctype.typ != CTYPE_ARRAY {
		return ast
	}
	if ast.typ == AST_LVAR {
		return ast_lref(make_ptr_type(ast.ctype.ptr), ast, 0)
	}

	if ast.typ != AST_GVAR {
		_error("Internal error: Gvar expected, but got %s", ast_to_string(ast))
	}
	return ast_gref(make_ptr_type(ast.ctype.ptr), ast, 0)
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
			ast, rest = rest, ast
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
		if is_punct(tok, '}') && i == ctype.size-1 {
			break
		}
		if !is_punct(tok, ',') {
			_error("comma expected, but got %s", token_to_string(tok))
		}
		if i == ctype.size-1 {
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
		return fmt.Sprintf("\"%s\"", quote_cstring(ast.str.val))
	case AST_LVAR:
		return fmt.Sprintf("%s", bytes2string(ast.variable.lname))
	case AST_GVAR:
		return fmt.Sprintf("%s", bytes2string(ast.gvar.gname))
	case AST_LREF:
		return fmt.Sprintf("%s[%d]", ast_to_string(ast.lref.ref), ast.lref.off)
	case AST_GREF:
		return fmt.Sprintf("%s[%d]", ast_to_string(ast.gref.ref), ast.gref.off)
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
		for i := 0; i < ast.array_initializer.size; i++ {
			s += ast_to_string_int(ast.array_initializer.array_init[i])
			if i != ast.array_initializer.size-1 {
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

func read_block() []*Ast {
	var block []*Ast
	block = make([]*Ast, EXPR_LEN)
	var i int
	for i = 0; i < EXPR_LEN; i++ {
		t := read_decl_or_stmt()
		if t == nil {
			break
		}
		block[i] = t
	}
	block[i] = nil;
	return block
}

func ast_to_string(ast *Ast) string {
	return ast_to_string_int(ast)
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

func block_to_string(block []*Ast) string {
	s := "{"
	for i := 0; block[i] != nil; i++ {
		s += ast_to_string(block[i])
		s += ";"
	}
	s += "}"
	return s
}
