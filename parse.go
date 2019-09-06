package main

import (
	"errors"
	"strconv"
	"strings"
)

const MAX_ARGS = 6
const MAX_OP_PRIO = 16
const MAX_ALIGN = 16

var gstrings []*Ast
var flonums []*Ast
var globalenv = &Dict{}
var localenv *Dict
var struct_defs Dict
var union_defs Dict
var typedefs Dict
var localvars []*Ast
var current_func_rettype *Ctype
var labelseq = 0

var ctype_void = &Ctype{typ: CTYPE_VOID, size: 0, sign: true,}
var ctype_char = &Ctype{typ: CTYPE_CHAR, size: 1, sign: true,}
var ctype_short = &Ctype{typ: CTYPE_SHORT, size: 2, sign: true}
var ctype_int = &Ctype{typ: CTYPE_INT, size: 4, sign: true,}
var ctype_long = &Ctype{typ: CTYPE_LONG, size: 8, sign: true,}
var ctype_float = &Ctype{typ: CTYPE_FLOAT, size: 4, sign: true,}
var ctype_double = &Ctype{typ: CTYPE_DOUBLE, size: 8, sign: true,}

var ctype_uchar = &Ctype{typ: CTYPE_CHAR, size: 1, sign: false,}
var ctype_ushort = &Ctype{typ: CTYPE_SHORT, size: 2, sign: false}
var ctype_uint = &Ctype{typ: CTYPE_INT, size: 4, sign: false,}
var ctype_ulong = &Ctype{typ: CTYPE_LONG, size: 8, sign: false,}

func ast_uop(typ int, ctype *Ctype, operand *Ast) *Ast {
	r := &Ast{}
	r.typ = typ
	r.ctype = ctype
	r.operand = operand
	return r
}

func ast_binop(typ int, left *Ast, right *Ast) *Ast {
	r := &Ast{}
	r.typ = typ
	r.ctype = result_type(byte(typ), left.ctype, right.ctype)
	if typ != '=' && convert_array(left.ctype).typ != CTYPE_PTR &&
		convert_array(right.ctype).typ == CTYPE_PTR {
		r.left = right
		r.right = left
	} else {
		r.left = left
		r.right = right
	}
	return r
}

func ast_inttype(ctype *Ctype, val int) *Ast {
	r := &Ast{}
	r.typ = AST_LITERAL
	r.ctype = ctype
	r.ival = val
	return r
}

func ast_double(val float64) *Ast {
	r := &Ast{}
	r.typ = AST_LITERAL
	r.ctype = ctype_double
	r.fval = val
	flonums = append(flonums, r)
	return r
}

func make_label() string {
	s := format(".L%d", labelseq)
	labelseq++
	return s
}

func ast_lvar(ctype *Ctype, name string) *Ast {
	r := &Ast{}
	r.typ = AST_LVAR
	r.ctype = ctype
	r.varname = name
	localenv.PutAst(name, r)
	if localvars != nil {
		localvars = append(localvars, r)
	}

	return r
}

func ast_gvar(ctype *Ctype, name string) *Ast {
	r := &Ast{}
	r.typ = AST_GVAR
	r.ctype = ctype
	r.varname = name
	r.glabel = name
	globalenv.PutAst(name, r)
	return r
}

func ast_string(str string) *Ast {
	r := &Ast{}
	r.typ = AST_STRING
	r.ctype = make_array_type(ctype_char, len(str)+1)
	r.val = str
	r.slabel = make_label()
	return r
}

func ast_funcall(ctype *Ctype, fname string, args []*Ast, paramtypes []*Ctype) *Ast {
	r := &Ast{}
	r.typ = AST_FUNCALL
	r.ctype = ctype
	r.fname = fname
	r.args = args
	r.paramtypes = paramtypes
	return r
}

func ast_func(rettype *Ctype, fname string, params []*Ast, localvars []*Ast, body *Ast) *Ast {
	r := &Ast{}
	r.typ = AST_FUNC
	r.ctype = rettype
	r.fname = fname
	r.params = params
	r.localvars = localvars
	r.body = body
	return r
}

func ast_decl(variable *Ast, init *Ast) *Ast {
	r := &Ast{}
	r.typ = AST_DECL
	r.ctype = nil
	r.declvar = variable
	r.declinit = init
	return r
}

func ast_array_init(arrayinit []*Ast) *Ast {
	r := &Ast{}
	r.typ = AST_ARRAY_INIT
	r.ctype = nil
	r.arrayinit = arrayinit
	return r
}

func ast_if(cond *Ast, then *Ast, els *Ast) *Ast {
	r := &Ast{}
	r.typ = AST_IF
	r.ctype = nil
	r.cond = cond
	r.then = then
	r.els = els
	return r
}

func ast_ternary(ctype *Ctype, cond *Ast, then *Ast, els *Ast) *Ast {
	r := &Ast{}
	r.typ = AST_TERNARY
	r.ctype = ctype
	r.cond = cond
	r.then = then
	r.els = els
	return r
}

func ast_for(init *Ast, cond *Ast, step *Ast, body *Ast) *Ast {
	r := &Ast{}
	r.typ = AST_FOR
	r.ctype = nil
	r.init = init
	r.cond = cond
	r.step = step
	r.body = body
	return r
}

func ast_return(rettype *Ctype, retval *Ast) *Ast {
	r := &Ast{}
	r.typ = AST_RETURN
	r.ctype = rettype
	r.retval = retval
	return r
}

func ast_compound_stmt(stmts []*Ast) *Ast {
	r := &Ast{}
	r.typ = AST_COMPOUND_STMT
	r.ctype = nil
	r.stmts = stmts
	return r
}

func ast_struct_ref(ctype *Ctype, struc *Ast, name string) *Ast {
	r := &Ast{}
	r.typ = AST_STRUCT_REF
	r.ctype = ctype
	r.struc = struc
	r.field = name
	return r
}

func make_ptr_type(ctype *Ctype) *Ctype {
	r := &Ctype{}
	r.typ = CTYPE_PTR
	r.ptr = ctype
	r.size = 8
	return r
}

func make_array_type(ctype *Ctype, len int) *Ctype {
	r := &Ctype{}
	r.typ = CTYPE_ARRAY
	r.ptr = ctype
	if len < 0 {
		r.size = -1
	} else {
		r.size = r.ptr.size * len
	}
	r.len = len
	return r
}

func make_struct_field_type(ctype *Ctype, offset int) *Ctype {
	copy := *ctype
	r := &copy
	//r.name = name
	r.offset = offset
	return r
}

func make_struct_type(fields *Dict, size int) *Ctype {
	r := &Ctype{}
	r.typ = CTYPE_STRUCT
	r.fields = fields
	r.size = size
	return r
}

func make_func_type(rettype *Ctype, paramtypes []*Ctype) *Ctype {
	r := &Ctype{}
	r.typ = CTYPE_FUNC
	r.rettype = rettype
	r.params = paramtypes
	return r
}

func is_inttype(ctype *Ctype) bool {
	return ctype.typ == CTYPE_CHAR || ctype.typ == CTYPE_INT || ctype.typ == CTYPE_LONG
}

func is_flotype(ctype *Ctype) bool {
	return ctype.typ == CTYPE_FLOAT || ctype.typ == CTYPE_DOUBLE
}

func ensure_lvalue(ast *Ast) {
	switch ast.typ {
	case AST_LVAR, AST_GVAR, AST_DEREF, AST_STRUCT_REF:
		return
	}
	errorf("lvalue expected, but got %s", ast)
	return
}

func expect(punct byte) {
	tok := read_token()
	if !tok.is_punct(int(punct)) {
		errorf("'%c' expected but got %s", punct, tok)
	}
}

func (tok *Token) is_ident(s string) bool {
	return tok.is_ident_type() && tok.sval == s
}

func is_right_assoc(tok *Token) bool {
	return tok.punct == '='
}

func eval_intexpr(ast *Ast) int {
	switch ast.typ {
	case AST_LITERAL:
		if is_inttype(ast.ctype) {
			return ast.ival
		}
		errorf("Integer expression expected, but got %s", ast)
	case '+':
		return eval_intexpr(ast.left) + eval_intexpr(ast.right)
	case '-':
		return eval_intexpr(ast.left) - eval_intexpr(ast.right)
	case '*':
		return eval_intexpr(ast.left) * eval_intexpr(ast.right)
	case '/':
		return eval_intexpr(ast.left) / eval_intexpr(ast.right)
	case '<':
		return bool2int(eval_intexpr(ast.left) < eval_intexpr(ast.right))
	case '>':
		return bool2int(eval_intexpr(ast.left) > eval_intexpr(ast.right))
	case '!':
		return bool2int(!int2bool(eval_intexpr(ast.operand)))
	case AST_TERNARY:
		if int2bool(eval_intexpr(ast.cond)) {
			return eval_intexpr(ast.then)
		} else {
			return eval_intexpr(ast.els)
		}
	case OP_EQ:
		return bool2int(eval_intexpr(ast.left) == eval_intexpr(ast.right))
	case OP_GE:
		return bool2int(eval_intexpr(ast.left) >= eval_intexpr(ast.right))
	case OP_LE:
		return bool2int(eval_intexpr(ast.left) <= eval_intexpr(ast.right))
	case OP_LOGAND:
		return eval_intexpr(ast.left) * eval_intexpr(ast.right)
	case OP_LOGOR:
		return bool2int(int2bool(eval_intexpr(ast.left)) || int2bool(eval_intexpr(ast.right)))
	default:
		errorf("Integer expression expected, but got %s", ast)
	}
	return -1
}

func priority(tok *Token) int {
	switch tok.punct {
	case '[', '.', OP_ARROW:
		return 1
	case OP_INC, OP_DEC:
		return 2
	case '*', '/':
		return 3
	case '+', '-':
		return 4
	case '<', '>', OP_LE, OP_GE:
		return 6
	case '&':
		return 8
	case '|':
		return 9
	case OP_EQ:
		return 7
	case OP_LOGAND:
		return 11
	case OP_LOGOR:
		return 12
	case '?':
		return 13
	case '=':
		return 14
	default:
		return -1
	}
}

func param_types(params []*Ast) []*Ctype {
	var r []*Ctype
	for _, ast := range params {
		r = append(r, ast.ctype)
	}
	return r
}

func function_type_check(fname string, params []*Ctype, args []*Ctype) {
	if len(args) < len(params) {
		errorf("Too few arguments: %s", fname)
	}
	for i, arg := range args {
		if i < len(params) {
			param := params[i]
			result_type('=', param, arg)
		} else {
			result_type('=', arg, ctype_int)
		}
	}
}

func read_func_args(fname string) *Ast {
	var args []*Ast
	for {
		tok := read_token()
		if tok.is_punct(')') {
			break
		}
		unget_token(tok)
		args = append(args, read_expr())
		tok = read_token()
		if tok.is_punct(')') {
			break
		}
		if !tok.is_punct(',') {
			errorf("Unexpected token: '%s'", tok)
		}
	}
	if MAX_ARGS < len(args) {
		errorf("Too many arguments: %s", fname)
	}
	decl := localenv.GetCtype(fname)
	if decl != nil {
		if decl.typ != CTYPE_FUNC {
			errorf("%s is not a function, but %s", fname, decl)
		}
		function_type_check(fname, decl.params, param_types(args))
		return ast_funcall(decl.rettype, fname, args, decl.params)
	}
	return ast_funcall(ctype_int, fname, args, nil)
}

func read_ident_or_func(name string) *Ast {
	ch := read_token()
	if ch.is_punct('(') {
		return read_func_args(name)
	}
	unget_token(ch)

	v := localenv.GetAst(name)
	if v == nil {
		errorf("Undefined varaible: %s", name)
	}
	return v
}

func is_long_token(s string) bool {
	for i, c := range []byte(s) {
		if !isdigit(c) {
			return (c == 'L' || c == 'l') && (i == len(s)-1)
		}
	}
	return false
}

func atol(sval string) int {
	s := strings.TrimSuffix(sval, "L")
	i, _ := strconv.Atoi(s)
	return i
}

func read_number_ast(sval string) *Ast {
	assert(sval[0] > 0)
	index := 0
	base := 10
	if sval[0] == '0' {
		index++
		if index < len(sval) && (sval[index] == 'x' || sval[index] == 'X') {
			base = 16
			index++
		} else if index < len(sval) && isdigit(sval[index]) {
			base = 8
		}
	}
	start := index
	for index < len(sval) && isdigit(sval[index]) {
		index++
	}
	if index < len(sval) && sval[index] == '.' {
		if base != 10 {
			errorf("malformed number: %s", sval)
		}
		index++
		for index < len(sval) && isdigit(sval[index]) {
			index++
		}
		if index < len(sval) && sval[index] != byte(0) {
			errorf("malformed number: %s", sval)
		}
		end := index - 1
		assert(start != end)
		fval, _ := strconv.ParseFloat(sval, 64)
		return ast_double(fval)
	}
	if index < len(sval) && (sval[index] == 'l' || sval[index] == 'L') {
		ival := atol(sval)
		return ast_inttype(ctype_long, ival)
	} else if  index < len(sval) && (sval[index:index+1] == "ul" || sval[index:index+1] == "ul") {
		val, _ := strconv.ParseInt(sval, base, 0)
		return ast_inttype(ctype_long, int(val))
	} else {
		if index < len(sval) && sval[index] != byte(0) {
			errorf("malformed number: %s", sval)
		}
		val, _ := strconv.ParseInt(sval, 0, 64)
		if val >= UINT_MAX {
			return ast_inttype(ctype_long, int(val))
		}
		return ast_inttype(ctype_int, int(val))
	}
}

func read_prim() *Ast {
	tok := read_token()
	if tok == nil {
		return nil
	}
	switch tok.typ {
	case TTYPE_IDENT:
		return read_ident_or_func(tok.sval)
	case TTYPE_NUMBER:
		return read_number_ast(tok.sval)
	case TTYPE_CHAR:
		return ast_inttype(ctype_char, int(tok.c))
	case TTYPE_STRING:
		r := ast_string(tok.sval)
		gstrings = append(gstrings, r)
		return r
	case TTYPE_PUNCT:
		unget_token(tok)
		return nil
	default:
		errorf("Don't know how to handle '%d'", tok.typ)
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
		if !is_inttype(a) {
			return nil, default_err
		}
		return b, nil
	}

	switch a.typ {
	case CTYPE_VOID:
		return nil, default_err
	case CTYPE_CHAR, CTYPE_SHORT, CTYPE_INT:
		switch b.typ {
		case CTYPE_CHAR, CTYPE_SHORT, CTYPE_INT:
			return ctype_int, nil
		case CTYPE_LONG:
			return ctype_long, nil
		case CTYPE_FLOAT, CTYPE_DOUBLE:
			return ctype_double, nil
		case CTYPE_ARRAY, CTYPE_PTR:
			return b, nil
		}
		errorf("internal error")
	case CTYPE_LONG:
		switch b.typ {
		case CTYPE_LONG:
			return ctype_long, nil
		case CTYPE_FLOAT, CTYPE_DOUBLE:
			return ctype_double, nil
		case CTYPE_ARRAY, CTYPE_PTR:
			return b, nil
		}
		errorf("internal error")
	case CTYPE_FLOAT:
		if b.typ == CTYPE_FLOAT || b.typ == CTYPE_DOUBLE {
			return ctype_double, nil
		}
		return nil, default_err
	case CTYPE_DOUBLE:
		if b.typ == CTYPE_DOUBLE {
			return ctype_double, nil
		}
	case CTYPE_ARRAY:
		if b.typ != CTYPE_ARRAY {
			return nil, default_err
		}

		return result_type_int(op, a.ptr, b.ptr)
	default:
		errorf("internal error: %s %s", a, b)
	}

	return nil, default_err
}

func read_subscript_expr(ast *Ast) *Ast {
	sub := read_expr()
	expect(']')
	t := ast_binop('+', ast, sub)
	return ast_uop(AST_DEREF, t.ctype.ptr, t)
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
		errorf("incompatible operands: %c: <%s> and <%s>",
			op, a, b)
	}
	return ret
}

func get_sizeof_size(allow_typename bool) *Ast {
	tok := read_token()
	if allow_typename && is_type_keyword(tok) {
		unget_token(tok)
		_, ctype := read_decl_int()
		assert(ctype != nil)
		return ast_inttype(ctype_long, ctype.size)
	}
	if tok.is_punct('(') {
		r := get_sizeof_size(true)
		expect(')')
		return r
	}
	unget_token(tok)
	expr := read_unary_expr()
	if expr.ctype.size == 0 {
		errorf("invalid operand for sizeof(): %s", expr)
	}
	return ast_inttype(ctype_long, expr.ctype.size)
}

func read_unary_expr() *Ast {
	tok := read_token()
	if tok == nil {
		errorf("premature end of input")
	}
	if tok.is_ident("sizeof") {
		return get_sizeof_size(false)
	}
	if tok.typ != TTYPE_PUNCT {
		unget_token(tok)
		return read_prim()
	}
	if tok.is_punct('(') {
		r := read_expr()
		expect(')')
		return r
	}
	if tok.is_punct('&') {
		operand := read_unary_expr()
		ensure_lvalue(operand)
		return ast_uop(AST_ADDR, make_ptr_type(operand.ctype), operand)
	}
	if tok.is_punct('*') {
		operand := read_unary_expr()
		ctype := convert_array(operand.ctype) // looks no need to call convert_array.
		if ctype.typ != CTYPE_PTR {
			errorf("pointer type expected, but got %", ctype)
		}
		return ast_uop(AST_DEREF, operand.ctype.ptr, operand)
	}
	if tok.is_punct('!') {
		operand := read_unary_expr()
		return ast_uop(int('!'), ctype_int, operand)
	}
	unget_token(tok)
	return read_prim()
}

func read_cond_expr(cond *Ast) *Ast {
	then := read_expr()
	expect(':')
	els := read_expr()
	return ast_ternary(then.ctype, cond, then, els)
}

func read_struct_field(struc *Ast) *Ast {
	if struc.ctype.typ != CTYPE_STRUCT {
		errorf("struct expected, but got %s", struc)
	}
	name := read_token()
	if !name.is_ident_type() {
		errorf("field name expected, but got %s", name)
	}
	field := struc.ctype.fields.GetCtype(name.sval)
	return ast_struct_ref(field, struc, name.sval)
}

func read_expr_int(prec int) *Ast {
	ast := read_unary_expr()
	if ast == nil {
		return nil
	}
	for {
		tok := read_token()
		if tok == nil {
			return ast
		}
		if tok.typ != TTYPE_PUNCT {
			unget_token(tok)
			return ast
		}
		prec2 := priority(tok)
		if prec2 < 0 || prec <= prec2 {
			unget_token(tok)
			return ast
		}

		if tok.is_punct('?') {
			ast = read_cond_expr(ast)
			continue
		}
		if tok.is_punct('.') {
			ast = read_struct_field(ast)
			continue
		}
		if tok.is_punct(OP_ARROW) {
			if ast.ctype.typ != CTYPE_PTR {
				errorf("pointer type expected, but got %s %s",
					ast.ctype, ast)
			}
			ast = ast_uop(AST_DEREF, ast.ctype.ptr, ast)
			ast = read_struct_field(ast)
			continue
		}
		if tok.is_punct('[') {
			ast = read_subscript_expr(ast)
			continue
		}
		if tok.is_punct(OP_INC) || tok.is_punct(OP_DEC) {
			ensure_lvalue(ast)
			ast = ast_uop(tok.punct, ast.ctype, ast)
			continue
		}
		if tok.is_punct('=') {
			ensure_lvalue(ast)
		}
		var prec_incr int
		if is_right_assoc(tok) {
			prec_incr = 1
		} else {
			prec_incr = 0
		}
		rest := read_expr_int(prec2 + prec_incr)
		if rest == nil {
			errorf("second operand missing")
		}
		ast = ast_binop(tok.punct, ast, rest)

	}
	return ast
}

func read_expr() *Ast {
	return read_expr_int(MAX_OP_PRIO)
}

func read_ctype(tok *Token) *Ctype {
	assert(tok != nil && tok.is_ident_type())
	r := typedefs.GetCtype(tok.sval)
	if r != nil {
		return r
	}

	const unspec = 0

	type sign int
	const (
		ssign  = sign(iota + 1)
		sunsign
	)
	var si sign

	type ttype int
	const (
		tchar = ttype(iota + 1)
		tshort
		tint
		tlong
		tllong
	)
	var ti ttype
	for {
		s := tok.sval
		if s == "signed" {
			if si != unspec {
				dupspec(tok)
			}
			si = ssign
		} else if s == "unsigned" {
			if si != unspec {
				dupspec(tok)
			}
			si = sunsign
		} else if s == "char" {
			if ti != unspec {
				duptype(tok)
			}
			ti = tchar
		} else if s == "short" {
			if ti != unspec {
				duptype(tok)
			}
			ti = tshort
		} else if s == "int" {
			if ti == unspec {
				ti = tint
			} else if ti == tchar {
				duptype(tok)
			}
		} else if s == "long" {
			if ti == unspec {
				ti = tlong
			} else if ti == tlong {
				ti = tllong
			} else {
				duptype(tok)
			}
		} else if s == "float" {
			if si != unspec {
				invspec(tok)
			}
			if ti != unspec {
				duptype(tok)
			}
			return ctype_float
		} else if s == "double" {
			if si != unspec {
				invspec(tok)
			}
			if ti != unspec {
				duptype(tok)
			}
			return ctype_double
		} else if s == "void" {
			if si != unspec {
				invspec(tok)
			}
			if ti != unspec {
				duptype(tok)
			}
			return ctype_void
		} else {
			unget_token(tok)
			break
		}
		tok = read_token()
		if !tok.is_ident_type() {
			unget_token(tok)
			break
		}
	}

	if ti == unspec && si == unspec {
		errorf("Type expected, but got '%s'", tok)
	}
	switch ti {
	case tchar:
		if si == sunsign {
			return ctype_uchar
		} else {
			return ctype_char
		}
	case tshort:
		if si == sunsign {
			return ctype_ushort
		} else {
			return ctype_short
		}
	case tint:
		if si == sunsign {
			return ctype_uint
		} else {
			return ctype_int
		}
	case tlong, tllong:
		if si == sunsign {
			return ctype_ulong
		} else {
			return ctype_long
		}
	}
	errorf("internal error")
	return nil
}

func dupspec(tok *Token) {
	errorf("duplicate specifier: %s", tok)
}

func duptype(tok *Token) {
	errorf("duplicate type specifier: %s", tok)
}

func invspec(tok *Token){
	errorf("cannot combine signed/unsigned with %s", tok)
}


func is_type_keyword(tok *Token) bool {
	if !tok.is_ident_type() {
		return false
	}

	keyword := []string{
		"char", "short", "int", "long", "float", "double", "struct",
		"union", "signed", "unsigned", "enum", "void",
	}
	for _, k := range keyword {
		if k == tok.sval {
			return true
		}
	}

	return typedefs.GetCtype(tok.sval) != nil
}

func read_decl_array_init_int(ctype *Ctype) *Ast {
	tok := read_token()
	if ctype.ptr.typ == CTYPE_CHAR && tok.typ == TTYPE_STRING {
		return ast_string(tok.sval)
	}

	if !tok.is_punct('{') {
		errorf("Expected an initializer list, but got %s", tok)
	}
	var initlist []*Ast
	for {
		tok := read_token()
		if tok.is_punct('}') {
			break
		}
		unget_token(tok)
		init := read_expr()
		initlist = append(initlist, init)
		result_type('=', init.ctype, ctype.ptr)
		tok = read_token()
		if !tok.is_punct(',') {
			unget_token(tok)
		}
	}

	return ast_array_init(initlist)
}

func read_struct_union_tag() string {
	tok := read_token()
	if tok.is_ident_type() {
		return tok.sval
	} else {
		unget_token(tok)
		return ""
	}
}

func read_struct_union_fields() *Dict {
	tok := read_token()
	if !tok.is_punct('{') {
		unget_token(tok)
		return nil
	}
	r := MakeDict(nil)
	for {
		if !is_type_keyword(peek_token()) {
			break
		}
		name, fieldtype  := read_decl_int()
		r.PutCtype(name.sval, make_struct_field_type(fieldtype, 0))
		expect(';')
	}
	expect('}')
	return r
}

func compute_union_size(fields *Dict) int {
	maxsize := 0
	for _, v := range fields.Values() {
		fieldtype := v.ctype
		if maxsize < fieldtype.size {
			maxsize = fieldtype.size
		}
	}
	return maxsize
}

func compute_struct_size(fields *Dict) int {
	offset := 0
	for _, v := range fields.Values() {
		fieldtype := v.ctype
		var align int
		if fieldtype.size < MAX_ALIGN {
			align = fieldtype.size
		} else {
			align = MAX_ALIGN
		}
		if offset%align != 0 {
			offset += align - offset%align
		}
		fieldtype.offset = offset
		offset += fieldtype.size
	}
	return offset
}

func read_struct_union_def(env *Dict, compute_size func(*Dict)int) *Ctype {
	tag := read_struct_union_tag()
	var prev *Ctype
	if tag != "" {
		prev = env.GetCtype(tag)
	} else {
		prev = nil
	}
	if prev != nil {
		return prev
	}
	fields := read_struct_union_fields()
	var r *Ctype
	if fields != nil {
		r = make_struct_type(fields, compute_size(fields))
	} else {
		r = make_struct_type(nil, 0)
	}
	if tag != "" {
		env.PutCtype(tag, r)
	}
	return r
}

func read_struct_def() *Ctype {
	return read_struct_union_def(&struct_defs, compute_struct_size)
}

func read_union_def() *Ctype {
	return read_struct_union_def(&union_defs, compute_union_size)
}

func read_enum_def() *Ctype {
	tok := read_token()
	if tok.is_ident_type() {
		tok = read_token()
	}
	if !tok.is_punct('{') {
		unget_token(tok)
		return ctype_int
	}
	val := 0
	for {
		tok = read_token()
		if tok.is_punct('}') {
			break
		}
		if !tok.is_ident_type() {
			errorf("Identifier expected, but got %s", tok)
		}
		constval := ast_inttype(ctype_int, val)
		val++
		if localenv != nil {
			localenv.PutAst(tok.sval, constval)
		} else {
			globalenv.PutAst(tok.sval, constval)
		}
		tok = read_token()
		if tok.is_punct(',') {
			continue
		}
		if tok.is_punct('}') {
			break
		}
		errorf("',' or '} expected, but got %s", tok)
	}
	return ctype_int
}

func read_decl_spec() *Ctype {
	tok := read_token()
	if tok == nil {
		return nil
	}
	var ctype *Ctype
	if tok.is_ident("struct") {
		ctype = read_struct_def()
	} else if tok.is_ident("union") {
		ctype = read_union_def()
	} else if tok.is_ident("enum") {
		ctype = read_enum_def()
	} else {
		ctype = read_ctype(tok)
	}

	assert(ctype != nil)
	for {
		tok = read_token()
		if !tok.is_punct('*') {
			unget_token(tok)
			return ctype
		}
		// pointer
		ctype = make_ptr_type(ctype)
	}
	return ctype
}

func read_decl_int() (*Token, *Ctype) {
	ctype := read_decl_spec()
	tok := read_token()
	var name *Token
	if tok.is_punct(';') {
		unget_token(tok)
		name = nil
		return name, ctype
	}
	if !tok.is_ident_type() {
		unget_token(tok)
		name = nil
	} else {
		name = tok
	}
	ctype = read_array_dimensions(ctype)
	return name, ctype
}

func read_decl_init_val(v *Ast) *Ast {
	if v.ctype.typ == CTYPE_ARRAY {
		init := read_decl_array_init_int(v.ctype)
		var length int
		if init.typ == AST_STRING {
			length = len(init.val) + 1
		} else {
			length = len(init.arrayinit)
		}
		if v.ctype.len == -1 {
			v.ctype.len = length
			v.ctype.size = length * v.ctype.ptr.size
		} else if v.ctype.len != length {
			errorf("Invalid array initializer: expected %d items but got %d",
				v.ctype.len, length)
		}
		expect(';')
		return ast_decl(v, init)
	}
	init := read_expr()
	expect(';')
	if v.typ == AST_GVAR {
		init = ast_inttype(ctype_int, eval_intexpr(init))
	}
	return ast_decl(v, init)
}

func read_array_dimensions_int(basetype *Ctype) *Ctype {
	tok := read_token()
	if !tok.is_punct('[') {
		unget_token(tok)
		return nil
	}
	dim := -1
	if !peek_token().is_punct(']') {
		size := read_expr()
		dim = eval_intexpr(size)
	}
	expect(']')
	sub := read_array_dimensions_int(basetype)
	if sub != nil {
		if sub.len == -1 && dim == -1 {
			errorf("Array len is not specified")
		}
		return make_array_type(sub, dim)
	}

	return make_array_type(basetype, dim)
}

func read_array_dimensions(basetype *Ctype) *Ctype {
	ctype := read_array_dimensions_int(basetype)
	if ctype == nil {
		return basetype
	}
	return ctype
}

func read_decl_init(variable *Ast) *Ast {
	tok := read_token()
	if tok.is_punct('=') {
		return read_decl_init_val(variable)
	}
	if variable.ctype.len == -1 {
		errorf("Missing array initializer")
	}
	unget_token(tok)
	expect(';')
	return ast_decl(variable, nil)
}

func read_decl() *Ast {
	varname, ctype := read_decl_int()
	if varname == nil {
		expect(';')
		return nil
	}
	variable := ast_lvar(ctype, varname.sval)
	return read_decl_init(variable)
}

func read_typedef() {
	name, ctype := read_decl_int()
	if name == nil {
		errorf("Typedef name missing")
	}
	typedefs.PutCtype(name.sval, ctype)
	expect(';')
}

func read_if_stmt() *Ast {
	expect('(')
	cond := read_expr()
	expect(')')
	then := read_stmt()
	tok := read_token()
	if tok == nil || !tok.is_ident_type() || tok.sval != "else" {
		unget_token(tok)
		return ast_if(cond, then, nil)
	}
	els := read_stmt()
	return ast_if(cond, then, els)
}

func read_opt_decl_or_stmt() *Ast {
	tok := read_token()
	if tok.is_punct(';') {
		return nil
	}
	unget_token(tok)
	return read_decl_or_stmt()
}

func read_opt_expr() *Ast {
	tok := read_token()
	if tok.is_punct(';') {
		return nil
	}
	unget_token(tok)
	r := read_expr()
	expect(';')
	return r
}

func read_for_stmt() *Ast {
	expect('(')
	localenv = MakeDict(localenv)
	init := read_opt_decl_or_stmt()
	cond := read_opt_expr()
	var step *Ast
	if peek_token().is_punct(')') {
		step = nil
	} else {
		step = read_expr()
	}
	expect(')')
	body := read_stmt()
	localenv = localenv.Parent()
	return ast_for(init, cond, step, body)
}

func read_return_stmt() *Ast {
	retval := read_expr()
	expect(';')
	return ast_return(current_func_rettype, retval)
}

func read_stmt() *Ast {
	tok := read_token()
	if tok.is_ident("if") {
		return read_if_stmt()
	}
	if tok.is_ident("for") {
		return read_for_stmt()
	}
	if tok.is_ident("return") {
		return read_return_stmt()
	}
	if tok.is_punct('{') {
		return read_compound_stmt()
	}
	unget_token(tok)
	r := read_expr()
	expect(';')
	return r
}

func read_decl_or_stmt() *Ast {
	tok := read_token()
	if tok == nil {
		return nil
	}
	if tok.is_ident("typedef") {
		read_typedef()
		return read_decl_or_stmt()
	}
	unget_token(tok)
	if is_type_keyword(tok) {
		return read_decl()
	} else {
		return read_stmt()
	}
}

func read_compound_stmt() *Ast {
	localenv = MakeDict(localenv)
	var list []*Ast

	for {
		stmt := read_decl_or_stmt()
		if stmt != nil {
			list = append(list, stmt)
		}
		if stmt == nil {
			continue
		}
		tok := read_token()
		if tok.is_punct('}') {
			break
		}
		unget_token(tok)
	}
	localenv = localenv.Parent()
	return ast_compound_stmt(list)
}

func read_params() []*Ast {
	var params []*Ast
	pt := read_token()
	if pt.is_punct(')') {
		return nil
	}
	unget_token(pt)
	for {
		ctype := read_decl_spec()
		pname := read_token()
		if !pname.is_ident_type() {
			errorf("Identifier expected, but got %s", pname)
		}
		ctype = read_array_dimensions(ctype)
		if ctype.typ == CTYPE_ARRAY {
			ctype = make_ptr_type(ctype.ptr)
		}
		params = append(params, ast_lvar(ctype, pname.sval))
		tok := read_token()
		if tok.is_punct(')') {
			return params
		}
		if !tok.is_punct(',') {
			errorf("Comma expected, but got %s", tok)
		}
	}
	return params // this is never reached
}

func read_func_def(rettype *Ctype, fname string, params []*Ast) *Ast {
	localenv = MakeDict(localenv)
	localvars = make([]*Ast, 0)
	current_func_rettype = rettype
	body := read_compound_stmt()
	typ := make_func_type(rettype, param_types(params))
	r := ast_func(typ, fname, params, localvars, body)
	globalenv.PutCtype(fname, typ)
	current_func_rettype = nil
	localenv = nil
	localvars = nil
	return r
}

func read_func_decl_or_def(rettype *Ctype, fname string) *Ast {
	expect('(')
	localenv = MakeDict(globalenv)
	params := read_params()
	tok := read_token()
	if tok.is_punct('{') {
		return read_func_def(rettype, fname, params)
	}
	typ := make_func_type(rettype, param_types(params))
	globalenv.PutCtype(fname, typ)
	return read_toplevel()
}

func read_toplevel() *Ast {
	for {
		tok := read_token()
		if tok == nil {
			return nil
		}
		if tok.is_ident("typedef") {
			read_typedef()
			continue
		}
		unget_token(tok)
		ctype := read_decl_spec()
		name := read_token()
		if name.is_punct(';') {
			continue
		}
		if !name.is_ident_type() {
			errorf("Identifier name expected, but got %s", name)
		}
		ctype = read_array_dimensions(ctype)
		tok = peek_token()
		if tok.is_punct('=') || ctype.typ == CTYPE_ARRAY {
			gvar := ast_gvar(ctype, name.sval)
			return read_decl_init(gvar)
		}
		if tok.is_punct('(') {
			return read_func_decl_or_def(ctype, name.sval)
		}
		if tok.is_punct(';') {
			read_token()
			gvar := ast_gvar(ctype, name.sval)
			return ast_decl(gvar, nil)
		}
		errorf("Don't know how to handle %s", tok)
		return nil
	}
}

func read_toplevels() []*Ast {
	var r []*Ast

	for {
		ast := read_toplevel()
		if ast == nil {
			return r
		}
		r = append(r, ast)
	}

	return r
}
