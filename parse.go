package main

import (
	"errors"
	"fmt"
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
var localvars []*Ast
var labelseq = 0

var ctype_int = &Ctype{typ: CTYPE_INT, size: 4}
var ctype_long = &Ctype{typ: CTYPE_LONG, size: 8}
var ctype_char = &Ctype{typ: CTYPE_CHAR, size: 1}
var ctype_float = &Ctype{typ: CTYPE_FLOAT, size: 4}
var ctype_double = &Ctype{typ: CTYPE_DOUBLE, size: 8}

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
	seq := labelseq
	labelseq++
	s := fmt.Sprintf(".L%d", seq)
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

func ast_gvar(ctype *Ctype, name string, filelocal bool) *Ast {
	r := &Ast{}
	r.typ = AST_GVAR
	r.ctype = ctype
	r.varname = name
	if filelocal {
		r.glabel = make_label()
	} else {
		r.glabel = name
	}
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

func ast_funcall(ctype *Ctype, fname string, args []*Ast) *Ast {
	r := &Ast{}
	r.typ = AST_FUNCALL
	r.ctype = ctype
	r.fname = fname
	r.args = args
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

func ast_return(retval *Ast) *Ast {
	r := &Ast{}
	r.typ = AST_RETURN
	r.ctype = nil
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
	if !is_punct(tok, int(punct)) {
		errorf("'%c' expected but got %s", punct, tok)
	}
}

func is_ident(tok *Token, s string) bool {
	return tok.typ == TTYPE_IDENT && tok.sval == s
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
	default:
		errorf("Integer expression expected, but got %s", ast)
	}
	return -1
}

func priority(tok *Token) int {
	switch tok.punct {
	case '[', '.', PUNCT_ARROW:
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

func read_func_args(fname string) *Ast {
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
			errorf("Unexpected token: '%s'", tok)
		}
	}
	if MAX_ARGS < len(args) {
		errorf("Too many arguments: %s", fname)
	}
	return ast_funcall(ctype_int, fname, args)
}

func read_ident_or_func(name string) *Ast {
	ch := read_token()
	if is_punct(ch, '(') {
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

func is_int_token(s string) bool {
	for _, c := range []byte(s) {
		if !isdigit(c) {
			return false
		}
	}
	return true
}

func is_float_token(s string) bool {
	var c byte
	var i int
	var b = []byte(s)
	for i, c = range b {
		if !isdigit(c) {
			break
		}
	}
	if c != '.' {
		return false
	}
	i++
	for j := i; j < len(b); j++ {
		if !isdigit(b[j]) {
			return false
		}
	}
	return true
}

func atol(sval string) int {
	s := strings.TrimSuffix(sval, "L")
	i, _ := strconv.Atoi(s)
	return i
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
		if is_long_token(tok.sval) {
			ival := atol(tok.sval)
			return ast_inttype(ctype_long, ival)
		}
		if is_int_token(tok.sval) {
			val, _ := strconv.Atoi(tok.sval)
			if val >= UINT_MAX {
				return ast_inttype(ctype_long, val)
			}
			return ast_inttype(ctype_int, val)
		}
		if is_float_token(tok.sval) {
			fval, _ := strconv.ParseFloat(tok.sval, 64)
			return ast_double(float64(fval))
		}
		errorf("Malformed number: %s", tok)
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
	case CTYPE_CHAR, CTYPE_INT:
		switch b.typ {
		case CTYPE_CHAR, CTYPE_INT:
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

func read_unary_expr() *Ast {
	tok := read_token()
	if tok.typ != TTYPE_PUNCT {
		unget_token(tok)
		return read_prim()
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
			errorf("pointer type expected, but got %", ctype)
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
	if name.typ != TTYPE_IDENT {
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
		if is_punct(tok, PUNCT_ARROW) {
			if ast.ctype.typ != CTYPE_PTR {
				errorf("pointer type expected, but got %s %s",
					ast.ctype, ast)
			}
			ast = ast_uop(AST_DEREF, ast.ctype.ptr, ast)
			ast = read_struct_field(ast)
			continue
		}
		if is_punct(tok, '[') {
			ast = read_subscript_expr(ast)
			continue
		}
		if is_punct(tok, PUNCT_INC) || is_punct(tok, PUNCT_DEC) {
			ensure_lvalue(ast)
			ast = ast_uop(tok.punct, ast.ctype, ast)
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

func get_ctype(tok *Token) *Ctype {
	if tok == nil {
		return nil
	}
	if tok.typ != TTYPE_IDENT {
		return nil
	}

	if tok.sval == "int" {
		return ctype_int
	}
	if tok.sval == "long" {
		return ctype_long
	}
	if tok.sval == "char" {
		return ctype_char
	}
	if tok.sval == "float" {
		return ctype_float
	}
	if tok.sval == "double" {
		return ctype_double
	}

	return nil
}

func is_type_keyword(tok *Token) bool {
	return get_ctype(tok) != nil || is_ident(tok, "struct") || is_ident(tok, "union")
}

func read_decl_array_init_int(ctype *Ctype) *Ast {
	tok := read_token()
	if ctype.ptr.typ == CTYPE_CHAR && tok.typ == TTYPE_STRING {
		return ast_string(tok.sval)
	}

	if !is_punct(tok, '{') {
		errorf("Expected an initializer list, but got %s", tok)
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

func read_struct_union_tag() string {
	tok := read_token()
	if tok.typ == TTYPE_IDENT {
		return tok.sval
	} else {
		unget_token(tok)
		return ""
	}
}

func read_struct_union_fields() *Dict {
	r := NewDict()
	expect('{')
	for {
		if !is_type_keyword(peek_token()) {
			break
		}
		fieldtype, name := read_decl_int()
		r.PutCtype(name.sval, make_struct_field_type(fieldtype, 0))
		expect(';')
	}
	expect('}')
	return r
}

func read_union_def() *Ctype {
	tag := read_struct_union_tag()
	ctype := union_defs.GetCtype(tag)
	if ctype != nil {
		return ctype
	}
	fields := read_struct_union_fields()
	maxsize := 0
	for _, v := range fields.Values() {
		fieldtype := v.ctype
		if maxsize < fieldtype.size {
			maxsize = fieldtype.size
		}
	}
	r := make_struct_type(fields, maxsize)
	if tag != "" {
		union_defs.PutCtype(tag, r)
	}
	return r
}

func read_struct_def() *Ctype {
	tag := read_struct_union_tag()
	ctype := struct_defs.GetCtype(tag)
	if ctype != nil {
		return ctype
	}
	fields := read_struct_union_fields()
	offset := 0
	for _, v := range fields.Values() {
		fieldtype := v.ctype
		var size int
		if fieldtype.size < MAX_ALIGN {
			size = fieldtype.size
		} else {
			size = MAX_ALIGN
		}
		if offset%size != 0 {
			offset += size - offset%size
		}
		fieldtype.offset = offset
		offset += fieldtype.size
	}
	r := make_struct_type(fields, offset)
	if tag != "" {
		struct_defs.PutCtype(tag, r)
	}
	return r
}

func read_decl_spec() *Ctype {
	tok := read_token()
	var ctype *Ctype
	if is_ident(tok, "struct") {
		ctype = read_struct_def()
	} else if is_ident(tok, "union") {
		ctype = read_union_def()
	} else {
		ctype = get_ctype(tok)
	}

	if ctype == nil {
		errorf("Type expected, but got %s", tok)
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

func read_decl_int() (*Ctype, *Token) {
	ctype := read_decl_spec()
	name := read_token()
	if name.typ != TTYPE_IDENT {
		errorf("Identifier expected, but got %s", name)
	}
	ctype = read_array_dimensions(ctype)
	return ctype, name
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
	if !is_punct(tok, '[') {
		unget_token(tok)
		return nil
	}
	dim := -1
	if !is_punct(peek_token(), ']') {
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
	if is_punct(tok, '=') {
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
	ctype, varname := read_decl_int()
	variable := ast_lvar(ctype, varname.sval)
	return read_decl_init(variable)
}

func read_if_stmt() *Ast {
	expect('(')
	cond := read_expr()
	expect(')')
	then := read_stmt()
	tok := read_token()
	if tok == nil || tok.typ != TTYPE_IDENT || tok.sval != "else" {
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
	localenv = localenv.MakeDict()
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
	localenv = localenv.Parent()
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
	localenv = localenv.MakeDict()
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
	localenv = localenv.Parent()
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
			errorf("Identifier expected, but got %s", pname)
		}
		ctype = read_array_dimensions(ctype)
		if ctype.typ == CTYPE_ARRAY {
			ctype = make_ptr_type(ctype.ptr)
		}
		params = append(params, ast_lvar(ctype, pname.sval))
		tok := read_token()
		if is_punct(tok, ')') {
			return params
		}
		if !is_punct(tok, ',') {
			errorf("Comma expected, but got %s", tok)
		}
	}
	return params // this is never reached
}

func read_func_def(rettype *Ctype, fname string, params []*Ast) *Ast {
	localenv = localenv.MakeDict()
	localvars = make([]*Ast, 0)
	body := read_compound_stmt()
	r := ast_func(rettype, fname, params, localvars, body)
	localenv = nil
	localvars = nil
	return r
}

func read_func_decl_or_def(rettype *Ctype, fname string) *Ast {
	expect('(')
	localenv = globalenv.MakeDict()
	params := read_params()
	tok := read_token()
	if is_punct(tok, '{') {
		return read_func_def(rettype, fname, params)
	}
	return read_toplevel();
}

func read_toplevel() *Ast {
	tok := peek_token()
	if tok == nil {
		return nil
	}
	ctype := read_decl_spec()
	name := read_token()
	if name.typ != TTYPE_IDENT {
		errorf("Identifier name expected, but got %s", name)
	}
	ctype = read_array_dimensions(ctype)
	tok = peek_token()
	if is_punct(tok, '=') || ctype.typ == CTYPE_ARRAY {
		gvar := ast_gvar(ctype, name.sval, false)
		return read_decl_init(gvar)
	}
	if is_punct(tok, '(') {
		return read_func_decl_or_def(ctype, name.sval)
	}
	if is_punct(tok, ';') {
		read_token()
		gvar := ast_gvar(ctype, name.sval, false)
		return ast_decl(gvar, nil)
	}
	errorf("Don't know how to handle %s", tok)
	return nil
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
