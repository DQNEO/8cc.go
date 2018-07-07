package main

import (
	"errors"
	"fmt"
)

const MAX_ARGS = 6
const MAX_OP_PRIO = 16
const MAX_ALIGN = 16

var globalenv = &EMPTY_ENV
var struct_defs []*Ctype
var localenv *Env
var localvars []*Ast
var labelseq = 0

var ctype_int = &Ctype{CTYPE_INT, nil, 0, nil, nil, nil, 0}
var ctype_char = &Ctype{CTYPE_CHAR, nil, 0, nil, nil, nil, 0}

func make_env(next *Env) *Env {
	r := &Env{}
	r.next = next
	r.vars = make([]*Ast, 0)
	return r
}

func env_append(env *Env, v *Ast) {
	assert(v.typ == AST_LVAR || v.typ == AST_GVAR || v.typ == AST_STRING)
	env.vars = append(env.vars, v)
}

func ast_uop(typ int, ctype *Ctype, operand *Ast) *Ast {
	r := &Ast{}
	r.typ = typ
	r.ctype = ctype
	r.unary.operand = operand
	return r
}

func ast_binop(typ int, left *Ast, right *Ast) *Ast {
	r := &Ast{}
	r.typ = typ
	r.ctype = result_type(byte(typ), left.ctype, right.ctype)
	if typ != '=' && convert_array(left.ctype).typ != CTYPE_PTR &&
		convert_array(right.ctype).typ == CTYPE_PTR {
		r.binop.left = right
		r.binop.right = left
	} else {
		r.binop.left = left
		r.binop.right = right
	}
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

func make_label() Cstring {
	seq := labelseq
	labelseq++
	s := fmt.Sprintf(".L%d", seq)
	return NewCstringFromLiteral(s)
}

func ast_lvar(ctype *Ctype, name Cstring) *Ast {
	r := &Ast{}
	r.typ = AST_LVAR
	r.ctype = ctype
	r.variable.varname = name
	env_append(localenv, r)
	if localvars != nil {
		localvars = append(localvars, r)
	}
	return r
}

func ast_gvar(ctype *Ctype, name Cstring, filelocal bool) *Ast {
	r := &Ast{}
	r.typ = AST_GVAR
	r.ctype = ctype
	r.variable.varname = name
	if filelocal {
		r.variable.glabel = make_label()
	} else {
		r.variable.glabel = name
	}
	globalenv.vars = append(globalenv.vars, r)
	env_append(globalenv, r)
	return r
}

func ast_string(str Cstring) *Ast {
	r := &Ast{}
	r.typ = AST_STRING
	r.ctype = make_array_type(ctype_char, strlen(str)+1)
	r.str.val = str
	r.str.slabel = make_label()
	return r
}

func ast_funcall(ctype *Ctype, fname Cstring, args []*Ast) *Ast {
	r := &Ast{}
	r.typ = AST_FUNCALL
	r.ctype = ctype
	r.fnc.fname = fname
	r.fnc.args = args
	return r
}

func ast_func(rettype *Ctype, fname Cstring, params []*Ast, localvars []*Ast, body *Ast) *Ast {
	r := &Ast{}
	r.typ = AST_FUNC
	r.ctype = rettype
	r.fnc.fname = fname
	r.fnc.params = params
	r.fnc.localvars = localvars
	r.fnc.body = body
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

func ast_array_init(arrayinit []*Ast) *Ast {
	r := &Ast{}
	r.typ = AST_ARRAY_INIT
	r.ctype = nil
	r.array_initializer.arrayinit = arrayinit
	return r
}

func ast_if(cond *Ast, then *Ast, els *Ast) *Ast {
	r := &Ast{}
	r.typ = AST_IF
	r.ctype = nil
	r._if.cond = cond
	r._if.then = then
	r._if.els = els
	return r
}

func ast_ternary(ctype *Ctype, cond *Ast, then *Ast, els *Ast) *Ast {
	r := &Ast{}
	r.typ = AST_TERNARY
	r.ctype = ctype
	r._if.cond = cond
	r._if.then = then
	r._if.els = els
	return r
}

func ast_for(init *Ast, cond *Ast, step *Ast, body *Ast) *Ast {
	r := &Ast{}
	r.typ = AST_FOR
	r.ctype = nil
	r._for.init = init
	r._for.cond = cond
	r._for.step = step
	r._for.body = body
	return r
}

func ast_return(retval *Ast) *Ast {
	r := &Ast{}
	r.typ = AST_RETURN
	r.ctype = nil
	r._return.retval = retval
	return r
}

func ast_compound_stmt(stmts []*Ast) *Ast {
	r := &Ast{}
	r.typ = AST_COMPOUND_STMT
	r.ctype = nil
	r.compound.stmts = stmts
	return r
}

func ast_struct_ref(struc *Ast, field *Ctype) *Ast {
	r := &Ast{}
	r.typ = AST_STRUCT_REF
	r.ctype = field
	r.structref.struc = struc
	r.structref.field = field
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

func make_struct_field_type(ctype *Ctype, name Cstring, offset int) *Ctype {
	copy := *ctype
	r := &copy
	r.name = name
	r.offset = offset
	return r
}

func make_struct_type(ctypes []*Ctype, tag Cstring) *Ctype {
	r := &Ctype{}
	r.typ = CTYPE_STRUCT
	r.fields = ctypes
	r.tag = tag
	return r
}

func find_var(name Cstring) *Ast {
	for p := localenv; p != nil; p = p.next {
		for _, v := range p.vars {
			if strcmp(name, v.variable.varname) == 0 {
				return v
			}
		}
	}
	return nil
}

func ensure_lvalue(ast *Ast) {
	switch ast.typ {
	case AST_LVAR, AST_GVAR, AST_DEREF, AST_STRUCT_REF:
		return
	}
	_error("lvalue expected, but got %s", ast)
	return
}

func is_ident(tok *Token, s string) bool {
	return tok.typ == TTYPE_IDENT && strcmp(tok.v.sval, NewCstringFromLiteral(s)) == 0
}

func is_right_assoc(tok *Token) bool {
	return tok.v.punct == '='
}

func priority(tok *Token) int {
	switch tok.v.punct {
	case '.':
		return 1
	case PUNCT_INC, PUNCT_DEC:
		return 2
	case '*', '/':
		return 3
	case '+', '-':
		return 4
	case '<', '>':
		return 6
	case '&':
		return 8
	case '|':
		return 9
	case PUNCT_EQ:
		return 7
	case PUNCT_LOGAND:
		return 11
	case PUNCT_LOGOR:
		return 12
	case '?':
		return 13
	case '=':
		return 14
	default:
		return -1
	}
}

func read_func_args(fname Cstring) *Ast {
	var args []*Ast
	for {
		tok := read_token()
		if is_punct(tok, ')') {
			break
		}
		unget_token(tok)
		args = append(args, read_expr())
		tok = read_token()
		if is_punct(tok, ')') {
			break
		}
		if !is_punct(tok, ',') {
			_error("Unexpected token: '%s'", tok)
		}
	}
	if MAX_ARGS < len(args) {
		_error("Too many arguments: %s", fname)
	}
	return ast_funcall(ctype_int, fname, args)
}

func read_ident_or_func(name Cstring) *Ast {
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
		r := ast_string(tk.v.sval)
		env_append(globalenv, r)
		return r
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
		if op == '=' {
			return a, nil
		}
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
		if b.typ != CTYPE_ARRAY {
			return nil, default_err
		}

		return result_type_int(op, a.ptr, b.ptr)
	default:
		_error("internal error: %s %s", a, b)
	}

	return nil, default_err
}

func read_subscript_expr(ast *Ast) *Ast {
	sub := read_expr()
	expect(']')
	t := ast_binop('+', ast, sub)
	return ast_uop(AST_DEREF, t.ctype.ptr, t)
}

func read_postfix_expr() *Ast {
	r := read_prim()
	for {
		tok := read_token()
		if tok == nil {
			return r
		}
		if is_punct(tok, '[') {
			r = read_subscript_expr(r)
		} else if is_punct(tok, PUNCT_INC) || is_punct(tok, PUNCT_DEC) {
			ensure_lvalue(r)
			r = ast_uop(tok.v.punct, r.ctype, r)
		} else {
			unget_token(tok)
			return r
		}
	}
}

func convert_array(ctype *Ctype) *Ctype {
	if ctype.typ != CTYPE_ARRAY {
		return ctype
	}
	return make_ptr_type(ctype.ptr)
}

func result_type(op byte, a *Ctype, b *Ctype) *Ctype {
	ret, err := result_type_int(op, convert_array(a), convert_array(b))
	if err != nil {
		_error("incompatible operands: %c: <%s> and <%s>",
			op, a, b)
	}
	return ret
}

func read_unary_expr() *Ast {
	tok := read_token()
	if tok.typ != TTYPE_PUNCT {
		unget_token(tok)
		return read_postfix_expr()
	}
	if is_punct(tok, '(') {
		r := read_expr()
		expect(')')
		return r
	}
	if is_punct(tok, '&') {
		operand := read_unary_expr()
		ensure_lvalue(operand)
		return ast_uop(AST_ADDR, make_ptr_type(operand.ctype), operand)
	}
	if is_punct(tok, '*') {
		operand := read_unary_expr()
		ctype := convert_array(operand.ctype) // looks no need to call convert_array.
		if ctype.typ != CTYPE_PTR {
			_error("pointer type expected, but got %", ctype)
		}
		return ast_uop(AST_DEREF, operand.ctype.ptr, operand)
	}
	if is_punct(tok, '!') {
		operand := read_unary_expr()
		return ast_uop(int('!'), ctype_int, operand)
	}
	unget_token(tok)
	return read_prim()
}

func read_cond_expr(cond *Ast) *Ast {
	then := read_unary_expr()
	expect(':')
	els := read_unary_expr()
	return ast_ternary(then.ctype, cond, then, els)
}

func find_struct_field(struc *Ast, name Cstring) *Ctype {
	for _,f := range struc.ctype.fields {
		if strcmp(f.name, name) == 0 {
			return f
		}
	}
	return nil
}

func read_struct_field(struc *Ast) *Ast {
	if struc.ctype.typ != CTYPE_STRUCT {
		_error("struct expected, but got %s", struc);
	}
	name := read_token()
	if name.typ != TTYPE_IDENT {
		_error("field name expected, but got %s", name)
	}
	field := find_struct_field(struc, name.v.sval)
	return ast_struct_ref(struc, field)
}

func read_expr_int(prec int) *Ast {
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
		prec2 := priority(tok)
		if prec2 < 0 || prec <= prec2 {
			unget_token(tok)
			return ast
		}

		if is_punct(tok, '?') {
			ast = read_cond_expr(ast)
			continue
		}
		if is_punct(tok, '.') {
			ast = read_struct_field(ast)
			continue
		}
		if is_punct(tok, '=') {
			ensure_lvalue(ast)
		}
		var prec_incr int
		if is_right_assoc(tok) {
			prec_incr = 1
		} else {
			prec_incr = 0
		}
		rest := read_expr_int(prec2 + prec_incr)
		ast = ast_binop(tok.v.punct, ast, rest)

	}
	return ast
}

func read_expr() *Ast {
	return read_expr_int(MAX_OP_PRIO)
}

func get_ctype(tok *Token) *Ctype {
	if tok == nil {
		return nil
	}
	if tok.typ != TTYPE_IDENT {
		return nil
	}

	if strcmp(tok.v.sval, NewCstringFromLiteral("int")) == 0 {
		return ctype_int
	}
	if strcmp(tok.v.sval, NewCstringFromLiteral("char")) == 0 {
		return ctype_char
	}

	return nil
}

func is_type_keyword(tok *Token) bool {
	return get_ctype(tok) != nil || is_ident(tok, "struct")
}

func expect(punct byte) {
	tok := read_token()
	if !is_punct(tok, int(punct)) {
		_error("'%c' expected but got %s", punct, tok)
	}
}

func read_decl_array_init_int(ctype *Ctype) *Ast {
	tok := read_token()
	if ctype.ptr.typ == CTYPE_CHAR && tok.typ == TTYPE_STRING {
		return ast_string(tok.v.sval)
	}

	if !is_punct(tok, '{') {
		_error("Expected an initializer list, but got %s", tok)
	}
	var initlist []*Ast
	for {
		tok := read_token()
		if is_punct(tok, '}') {
			break
		}
		unget_token(tok)
		init := read_expr()
		initlist = append(initlist, init)
		result_type('=', init.ctype, ctype.ptr)
		tok = read_token()
		if !is_punct(tok, ',') {
			unget_token(tok)
		}
	}

	return ast_array_init(initlist)
}

func find_struct_def(name Cstring) *Ctype {
	for _, t := range struct_defs {
		if len(t.tag) > 0 && strcmp(t.tag, name) == 0 {
			return t
		}
	}
	return nil
}

func read_struct_def() *Ctype {
	tok := read_token()
	var tag Cstring
	if tok.typ == TTYPE_IDENT {
		tag = tok.v.sval
	} else {
		unget_token(tok)
	}
	ctype := find_struct_def(tag)
	var fields []*Ctype
	if ctype != nil {
		return ctype
	}
	expect('{')
	offset := 0
	for {
		if !is_type_keyword(peek_token()) {
			break
		}
		fieldtype := read_decl_spec()
		name := read_token()
		if name.typ != TTYPE_IDENT {
			_error("Identifier expected, but got %s",name);
		}
		fieldtype = read_array_dimensions(fieldtype)

		size := ctype_size(fieldtype)
		if size < MAX_ALIGN {

		} else {
			size = MAX_ALIGN
		}
		if offset % size != 0 {
			offset += size - offset % size
		}
		fields = append(fields, make_struct_field_type(fieldtype, name.v.sval, offset))
		offset += size
		expect(';')
	}
	expect('}')
	r := make_struct_type(fields, tag)
	struct_defs = append(struct_defs, r)
	return r
}

func read_decl_spec() *Ctype {
	tok := read_token()
	var ctype *Ctype
	if is_ident(tok, "struct") {
		ctype = read_struct_def()
	}  else {
		ctype = get_ctype(tok)
	}
	if ctype == nil {
		_error("Type expected, but got %s", tok)
	}
	for {
		tok = read_token()
		if !is_punct(tok, '*') {
			unget_token(tok)
			return ctype
		}
		// pointer
		ctype = make_ptr_type(ctype)
	}
	return ctype
}

func read_decl_init_val(v *Ast) *Ast {
	if v.ctype.typ == CTYPE_ARRAY {
		init := read_decl_array_init_int(v.ctype)
		var length int
		if init.typ == AST_STRING {
			length = strlen(init.str.val) + 1
		} else {
			length = len(init.array_initializer.arrayinit)
		}
		if v.ctype.size == -1 {
			v.ctype.size = length
		} else if v.ctype.size != length {
			_error("Invalid array initializer: expected %d items but got %d",
				v.ctype.size, length)
		}
		expect(';')
		return ast_decl(v, init)
	}
	init := read_expr()
	expect(';')
	if v.typ == AST_GVAR {
		check_intexp(init)
	}
	return ast_decl(v, init)
}

func check_intexp(ast *Ast) {
	if ast.typ != AST_LITERAL || ast.ctype.typ != CTYPE_INT {
		_error("Integer expected, but got %s", ast)
	}
}

func read_array_dimensions_int() *Ctype {
	tok := read_token()
	if !is_punct(tok, '[') {
		unget_token(tok)
		return nil
	}
	dim := -1
	tok = peek_token()
	if !is_punct(tok, ']') {
		size := read_expr()
		check_intexp(size)
		dim = size.ival
	}
	expect(']')
	sub := read_array_dimensions_int()
	if sub != nil {
		if sub.size == -1 && dim == -1 {
			_error("Array size is not specified")
		}
		return make_array_type(sub, dim)
	}

	return make_array_type(nil, dim)
}

func read_array_dimensions(basetype *Ctype) *Ctype {
	ctype := read_array_dimensions_int()
	if ctype == nil {
		return basetype
	}
	p := ctype
	for ; p.ptr != nil; p = p.ptr {
	}
	p.ptr = basetype
	return ctype
}

func read_decl_init(variable *Ast) *Ast {
	tok := read_token()
	if is_punct(tok, '=') {
		return read_decl_init_val(variable)
	}
	unget_token(tok)
	expect(';')
	return ast_decl(variable, nil)
}

func read_decl() *Ast {
	ctype := read_decl_spec()
	varname := read_token()
	if varname.typ != TTYPE_IDENT {
		_error("Identifier expected, but got %s", varname)
	}
	ctype = read_array_dimensions(ctype)
	variable := ast_lvar(ctype, varname.v.sval)
	return read_decl_init(variable)
}

func read_if_stmt() *Ast {
	expect('(')
	cond := read_expr()
	expect(')')
	then := read_stmt()
	tok := read_token()
	if tok == nil || tok.typ != TTYPE_IDENT || strcmp(tok.v.sval, NewCstringFromLiteral("else")) != 0 {
		unget_token(tok)
		return ast_if(cond, then, nil)
	}
	els := read_stmt()
	return ast_if(cond, then, els)
}

func read_opt_decl_or_stmt() *Ast {
	tok := read_token()
	if is_punct(tok, ';') {
		return nil
	}
	unget_token(tok)
	return read_decl_or_stmt()
}

func read_opt_expr() *Ast {
	tok := read_token()
	if is_punct(tok, ';') {
		return nil
	}
	unget_token(tok)
	r := read_expr()
	expect(';')
	return r
}

func read_for_stmt() *Ast {
	expect('(')
	localenv = make_env(localenv)
	init := read_opt_decl_or_stmt()
	cond := read_opt_expr()
	var step *Ast
	if is_punct(peek_token(), ')') {
		step = nil
	} else {
		step = read_expr()
	}
	expect(')')
	body := read_stmt()
	localenv = localenv.next
	return ast_for(init, cond, step, body)
}

func read_return_stmt() *Ast {
	retval := read_expr()
	expect(';')
	return ast_return(retval)
}

func read_stmt() *Ast {
	tok := read_token()
	if is_ident(tok, "if") {
		return read_if_stmt()
	}
	if is_ident(tok, "for") {
		return read_for_stmt()
	}
	if is_ident(tok, "return") {
		return read_return_stmt()
	}
	if is_punct(tok, '{') {
		return read_compound_stmt()
	}
	unget_token(tok)
	r := read_expr()
	expect(';')
	return r
}

func read_decl_or_stmt() *Ast {
	tok := peek_token()
	if tok == nil {
		return nil
	}

	if is_type_keyword(tok) {
		return read_decl()
	} else {
		return read_stmt()
	}
}

func read_compound_stmt() *Ast {
	localenv = make_env(localenv)
	var list []*Ast

	for {
		stmt := read_decl_or_stmt()
		if stmt != nil {
			list = append(list, stmt)
		}
		if stmt == nil {
			break
		}
		tok := read_token()
		if is_punct(tok, '}') {
			break
		}
		unget_token(tok)
	}
	localenv = localenv.next
	return ast_compound_stmt(list)
}

func read_params() []*Ast {
	var params []*Ast
	pt := read_token()
	if is_punct(pt, ')') {
		return nil
	}
	unget_token(pt)
	for {
		ctype := read_decl_spec()
		pname := read_token()
		if pname.typ != TTYPE_IDENT {
			_error("Identifier expected, but got %s", pname)
		}
		ctype = read_array_dimensions(ctype)
		if ctype.typ == CTYPE_ARRAY {
			ctype = make_ptr_type(ctype.ptr)
		}
		params = append(params, ast_lvar(ctype, pname.v.sval))
		tok := read_token()
		if is_punct(tok, ')') {
			return params
		}
		if !is_punct(tok, ',') {
			_error("Comma expected, but got %s", tok)
		}
	}
	return params // this is never reached
}

func read_func_def(rettype *Ctype, fname []byte) *Ast {
	expect('(')
	localenv = make_env(globalenv)
	params := read_params()
	expect('{')
	localenv = make_env(localenv)
	localvars = make([]*Ast, 0)
	body := read_compound_stmt()
	r := ast_func(rettype, fname, params, localvars, body)
	localvars = nil
	return r
}

func read_decl_or_func_def() *Ast {
	tok := peek_token()
	if tok == nil {
		return nil
	}
	ctype := read_decl_spec()
	name := read_token()
	if name.typ != TTYPE_IDENT {
		_error("Identifier name expected, but got %s", name)
	}
	ctype = read_array_dimensions(ctype)
	tok = peek_token()
	if is_punct(tok, '=') || ctype.typ == CTYPE_ARRAY {
		gvar := ast_gvar(ctype, name.v.sval, false)
		return read_decl_init(gvar)
	}
	if is_punct(tok, '(') {
		return read_func_def(ctype, name.v.sval)
	}
	if is_punct(tok, ';') {
		read_token()
		gvar := ast_gvar(ctype, name.v.sval, false)
		return ast_decl(gvar, nil)
	}
	_error("Don't know how to handle %s", tok)
	return nil
}

func read_toplevels() []*Ast {
	var func_list []*Ast

	for {
		ast := read_decl_or_func_def()
		if ast == nil {
			return func_list
		}
		func_list = append(func_list, ast)
	}

	return nil
}
