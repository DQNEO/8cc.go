package main

import (
	"fmt"
	"os"
)

var macros = make(map[string]*Macro)
var cond_incl_stack = make([]*CondIncl, 0)
var std_include_path []string
var cpp_token_zero = &Token{typ: TTYPE_NUMBER, sval: "0"}
var cpp_token_one = &Token{typ: TTYPE_NUMBER, sval: "1"}

type CondInclCtx int

const (
	IN_THEN CondInclCtx = 0
	IN_ELSE CondInclCtx = 1
)

type CondIncl struct {
	ctx     CondInclCtx
	wastrue bool
}

type MacroType int

const (
	MACRO_OBJ  MacroType = 0
	MACRO_FUNC MacroType = 1
)

type Macro struct {
	typ     MacroType
	nargs   int
	body    TokenList
	is_varg bool
}

func eval(buf string){
	bytes := append([]byte(buf), byte(0)) // add EOF
	fp := &stream{
		buf:  bytes,
		i:0,
	}
	set_input_file("(eval)", fp)
	toplevels := read_toplevels()
	for _, ast := range toplevels {
		printf("# ast=%s\n", ast)
		emit_toplevel(ast)
	}
	set_input_file("(stdin)", stdin)
}

func initCpp() {
	std_include_path = []string{
		"/usr/local/include",
		"/usr/include/x86_64-linux-gnu",
		"/usr/include/x86_64-linux-gnu/7/include",
		"/usr/include/linux",
		"/usr/include",
		".",
	}

	macros["__x86_64__"] = &Macro{}
	macros["__8cc__"] = &Macro{}
	eval("typedef int __builtin_va_list[1];")
}

func make_cond_incl(ctx CondInclCtx, wastrue bool) *CondIncl {
	r := &CondIncl{
		ctx:     ctx,
		wastrue: wastrue,
	}
	return r
}

func make_obj_marco(body TokenList) *Macro {
	r := &Macro{
		typ:     MACRO_OBJ,
		body:    body,
		is_varg: false,
	}
	return r
}

func make_func_macro(body TokenList, nargs int, is_varg bool) *Macro {
	r := &Macro{
		typ:     MACRO_FUNC,
		nargs:   nargs,
		body:    body,
		is_varg: is_varg,
	}
	return r
}

func make_macro_token(position int) *Token {
	r := &Token{
		typ:      TTYPE_MACRO_PARAM,
		hideset:  MakeDict(nil),
		position: position,
		space:    false,
		bol: false,
	}
	return r
}

func copy_token(tok *Token) *Token {
	r := *tok
	return &r
}

func expect2(punct int) {
	tok := read_cpp_token()
	if tok == nil || !tok.is_punct(punct) {
		errorf("%c expected, but got %s", punct, tok)
	}
}

func read_ident2() *Token {
	r := read_cpp_token()
	if !r.is_ident_type() {
		errorf("identifier expected, but got %s", r)
	}
	return r
}

func unget_all(tokens TokenList) {
	for _, tok := range list_reverse(tokens) {
		unget_token(tok)
	}
}

func read_expand() *Token {
	tok := read_cpp_token()
	if tok == nil {
		return nil
	}
	if tok.is_newline() {
		return read_expand()
	}
	if !tok.is_ident_type() {
		return tok
	}
	name := tok.sval
	macro, ok := macros[name]
	if !ok || tok.hideset.Get(name) != nil {
		return tok
	}

	switch macro.typ {
	case MACRO_OBJ:
		hideset := dict_append(tok.hideset, name)
		tokens := subst(macro, make([]TokenList, 0), hideset)
		unget_all(tokens)
		return read_expand()
	case MACRO_FUNC:
		args := read_args(macro)
		rparen := read_cpp_token()
		assert(rparen.is_punct(')'))
		hideset := dict_append(dict_intersection(tok.hideset, rparen.hideset), name)
		tokens := subst(macro, args, hideset)
		unget_all(tokens)
		return read_expand()
	default:
		errorf("internal error")
	}
	return nil
}

func read_funclike_macro_args(param *Dict) bool {
	pos := 0
	for {
		tok := read_cpp_token()
		if tok.is_punct(')') {
			return false
		}
		if pos > 0 {
			if !tok.is_punct(',') {
				errorf("',' expected, but got '%s'", tok)
			}
			tok = read_cpp_token()
		}
		if tok == nil || tok.typ == TTYPE_NEWLINE {
			errorf("missing ')' in macro parameter list")
		}
		if tok.is_ident("...") {
			param.PutToken("__VA_ARGS__", make_macro_token(pos))
			pos++
			expect(')')
			return true
		}
		if !tok.is_ident_type() {
			errorf("identifier expected, but got '%s'", tok)
		}
		param.PutToken(tok.sval, make_macro_token(pos))
		pos++
	}
}

func read_funclike_macro_body(param *Dict) TokenList {
	r := make(TokenList, 0)
	for {
		tok := read_cpp_token()
		if tok == nil || tok.is_newline() {
			return r
		}
		if tok.is_ident_type() {
			subst := param.GetToken(tok.sval)
			if subst != nil {
				r = append(r, subst)
				continue
			}
		}
		r = append(r, tok)
	}
	return r
}

func read_funclike_macro(name string) {
	param := MakeDict(nil)
	varg := read_funclike_macro_args(param)
	body := read_funclike_macro_body(param)
	macro := make_func_macro(body, len(param.Keys()), varg)
	macros[name] = macro
}

func expect_newline() {
	tok := read_cpp_token()
	if tok == nil || !tok.is_newline() {
		errorf("Newline expected, but got %s", tok)
	}
}

func read_args_int(macro *Macro) []TokenList {
	tok := read_cpp_token()
	if tok == nil || !tok.is_punct('(') {
		unget_token(tok)
		return nil
	}
	r := make([]TokenList, 0)
	arg := make_list()
	depth := 0
	for {
		tok = read_cpp_token()
		if tok == nil {
			errorf("unterminated macro argument list")
		}
		if tok.is_newline() {
			continue
		}
		if depth > 0 {
			if tok.is_punct(')') {
				depth--
			}
			arg = append(arg, tok)
			continue
		}
		if tok.is_punct('(') {
			depth++
		}
		if tok.is_punct(')') {
			unget_token(tok)
			if len(r) != 0 || len(arg) != 0 {
				r = append(r, arg)
			}
			return r
		}
		in_threedots := macro.is_varg && (len(r)+1 == macro.nargs)
		if tok.is_punct(',') && !in_threedots {
			r = append(r, arg)
			arg = make_list()
			continue
		}
		arg = append(arg, tok)
	}
}

func read_args(macro *Macro) []TokenList {
	args := read_args_int(macro)
	if args == nil {
		return nil
	}
	if (macro.is_varg && len(args) < macro.nargs) ||
		(!macro.is_varg && len(args) != macro.nargs) {
		errorf("Macro argument number does not match")
	}
	return args
}

func dict_union(a *Dict, b *Dict) *Dict {
	r := MakeDict(nil)
	for _, key := range a.Keys() {
		r.Put(key, a.Get(key))
	}
	for _, key := range b.Keys() {
		r.Put(key, b.Get(key))
	}
	return r
}

func dict_intersection(a *Dict, b *Dict) *Dict {
	r := MakeDict(nil)
	for _, key := range a.Keys() {
		if b.Get(key) != nil {
			r.Put(key, &DictValue{})
		}
	}
	return r
}
func dict_append(dict *Dict, s string) *Dict {
	r := MakeDict(dict)
	r.Put(s, &DictValue{})
	return r
}

func add_hide_set(tokens TokenList, hideset *Dict) TokenList {
	r := make(TokenList, 0)
	for _, tok := range tokens {
		t := copy_token(tok)
		t.hideset = dict_union(t.hideset, hideset)
		r = append(r, t)
	}
	return r
}

func paste(s string, tok *Token) string {
	switch tok.typ {
	case TTYPE_IDENT, TTYPE_NUMBER:
		return s + tok.sval
	case TTYPE_PUNCT:
		return s + fmt.Sprintf("%c", tok.punct)
	default:
		errorf("can't paste: %s", tok)
	}
	return ""
}

func glue_tokens(t0 *Token, t1 *Token) *Token {
	s := ""
	s = paste(s, t0)
	s = paste(s, t1)
	r := copy_token(t0)
	if isdigit(s[0]) {
		r.typ = TTYPE_NUMBER
	} else {
		r.typ = TTYPE_IDENT
	}
	r.sval = s
	return r
}

func glue_push(tokens TokenList, tok *Token) TokenList {
	assert(len(tokens) > 0)
	last := tokens[len(tokens)-1]
	tokens = tokens[:len(tokens)-1]
	return append(tokens, glue_tokens(last, tok))
}

func join_tokens(args TokenList) string {
	s := ""
	for _, tok := range args {
		if len(s) > 0 && tok.space {
			s += " "
		}
		switch tok.typ {
		case TTYPE_IDENT, TTYPE_NUMBER:
			s += tok.sval
		case TTYPE_PUNCT:
			s += fmt.Sprintf("%c", tok.punct)
		case TTYPE_CHAR:
			s += quote_char(tok.c)
		case TTYPE_STRING:
			s += fmt.Sprintf("\"%s\"", tok.sval)
		default:
			errorf("internal error")
		}
	}
	return s
}

func stringize(args TokenList) *Token {
	r := &Token{
		typ:  TTYPE_STRING,
		sval: join_tokens(args),
	}
	return r
}
func expand_all(tokens TokenList) TokenList {
	r := make_list()
	orig := get_input_buffer()
	set_input_buffer(tokens)
	tok := read_expand()
	for ; tok != nil; tok = read_expand() {
		r = append(r, tok)
	}
	set_input_buffer(orig)
	return r
}

func subst(macro *Macro, args []TokenList, hideset *Dict) TokenList {
	r := make_list()
	for i := 0; i < len(macro.body); i++ {
		islast := i == (len(macro.body) - 1)
		t0 := macro.body[i]
		var t1 *Token
		if islast {
			t1 = nil
		} else {
			t1 = macro.body[i+1]
		}
		t0_param := (t0.typ == TTYPE_MACRO_PARAM)
		t1_param := (!islast && t1.typ == TTYPE_MACRO_PARAM)
		if t0.is_punct('#') && t1_param {
			r = append(r, stringize(args[t1.position]))
			i++
			continue
		}
		if t0.is_ident("##") && t1_param {
			arg := args[t1.position]
			if len(arg) > 0 {
				r = glue_push(r, arg[0])
				var tmp TokenList
				tmp = arg[1:]
				r = list_append(r, expand_all(tmp))
			}
			i++
			continue
		}
		if t0.is_ident("##") && !islast {
			hideset = t1.hideset
			r = glue_push(r, t1)
			i++
			continue
		}
		if t0_param && !islast && t1.is_ident("##") {
			hideset = t1.hideset
			arg := args[t0.position]
			if len(arg) == 0 {
				i++
			} else {
				r = list_append(r, arg)
			}
			continue
		}

		if t0_param {
			arg := args[t0.position]
			r = list_append(r, expand_all(arg))
			continue
		}
		r = append(r, t0)
	}
	return add_hide_set(r, hideset)
}

func read_undef() {
	name := read_ident2()
	expect_newline()
	delete(macros, name.sval)
}

func read_obj_macro(name string) {
	body := make(TokenList, 0)
	for {
		tok := read_cpp_token()
		if tok == nil || tok.is_newline() {
			break
		}
		body = append(body, tok)
	}
	macros[name] = make_obj_marco(body)
}

func read_define() {
	name := read_ident2()
	tok := read_cpp_token()
	if tok != nil && tok.is_punct('(') && !tok.space {
		read_funclike_macro(name.sval)
		return
	}
	unget_token(tok)
	read_obj_macro(name.sval)
}

func read_defined_operator() *Token {
	tok := read_cpp_token()
	if tok.is_punct('(') {
		tok = read_cpp_token()
		expect2(')')
	}
	if !tok.is_ident_type() {
		errorf("Identifier expected, but got %s", tok)
	}
	if _, ok := macros[tok.sval]; ok {
		return cpp_token_one
	} else {
		return cpp_token_zero
	}
}

func read_intexpr_line() TokenList {
	var r TokenList
	for {
		tok := read_token_int2(true)
		if tok == nil {
			return r
		}
		if tok.is_ident("defined") {
			r = append(r, read_defined_operator())
		} else if tok.is_ident_type() {
			r = append(r, cpp_token_one)
		} else {
			r = append(r, tok)
		}
	}
}

func read_constexpr() bool {
	orig := get_input_buffer()
	set_input_buffer(read_intexpr_line())
	expr := read_expr()
	buf := get_input_buffer()
	if len(buf) > 0 {
		errorf("Stray token: %v", buf)
	}
	set_input_buffer(orig)
	return eval_intexpr(expr) != 0
}

func read_if_generic(cond bool) {
	cond_incl_stack = append(cond_incl_stack, make_cond_incl(IN_THEN, cond))
	if !cond {
		skip_cond_incl()
	}
}

func read_if() {
	read_if_generic(read_constexpr())
}

func read_ifdef_generic(is_ifdef bool) {
	tok := read_cpp_token()
	if tok == nil || ! tok.is_ident_type() {
		errorf("identifier expected, but got %s", tok)
	}
	_, cond := macros[tok.sval]
	expect_newline()
	var cond2 bool
	if is_ifdef {
		cond2 = cond
	} else {
		cond2 = !cond
	}
	read_if_generic(cond2)
}

func read_ifdef() {
	read_ifdef_generic(true)
}

func read_ifndef() {
	read_ifdef_generic(false)
}

func read_else() {
	if len(cond_incl_stack) == 0 {
		errorf("stray #else")
	}
	ci := cond_incl_stack[len(cond_incl_stack)-1]
	if ci.ctx == IN_ELSE {
		errorf("#else appears in #else")
	}
	expect_newline()
	if ci.wastrue {
		skip_cond_incl()
	}
}

func read_elif() {
	if len(cond_incl_stack) == 0 {
		errorf("stray #elif")
	}
	ci := cond_incl_stack[len(cond_incl_stack)-1]
	if ci.ctx == IN_ELSE {
		errorf("#elif after #else")
	}
	if ci.wastrue {
		skip_cond_incl()
		return
	} else {
		cond := read_constexpr()
		ci.wastrue = cond
		if !cond {
			skip_cond_incl()
		}
	}
}

func read_endif() {
	if len(cond_incl_stack) == 0 {
		errorf("stray #endif")
	}
	cond_incl_stack = cond_incl_stack[:len(cond_incl_stack)-1]
	expect_newline()
}

func read_cpp_header_name() (string, bool) {
	if get_input_buffer() == nil {
		if name, std, found := read_header_file_name(); found {
			return name, std
		}
	}
	return "", false
}

func construct_path(path1 string, path2 string) string {
	if path1 == "" {
		return path2
	}
	return format("%s/%s", path1, path2)
}

func read_include() {
	name,std := read_cpp_header_name()
	expect_newline()
	var paths []string
	if std {
		paths = std_include_path
	} else {
		paths = []string{""}
	}

	for _, directory := range paths {
		path := construct_path(directory, name)
		fp, _ := os.Open(path)
		if fp != nil {
			push_input_file(path, fp)
			return
		}
	}
	errorf("Cannot file header file: %s", name)
}

func read_directive() {
	tok := read_cpp_token()
	if tok.is_ident("define") {
		read_define()
	} else if tok.is_ident("undef") {
		read_undef()
	} else if tok.is_ident("if") {
		read_if()
	} else if tok.is_ident("ifdef") {
		read_ifdef()
	} else if tok.is_ident("ifndef") {
		read_ifndef()
	} else if tok.is_ident("else") {
		read_else()
	} else if tok.is_ident("elif") {
		read_elif()
	} else if tok.is_ident("endif") {
		read_endif()
	} else if tok.is_ident("include") {
		read_include()
	} else {
		errorf("unsupported preprocessor directive: %s", tok)
	}
}

func unget_token(tok *Token) {
	unget_cpp_token(tok)
}

func peek_token() *Token {
	tok := read_token()
	unget_token(tok)
	return tok
}

func read_token_int2(return_at_eol bool) *Token {
	for {
		tok := read_cpp_token()
		if tok == nil {
			return nil
		}
		if tok != nil && tok.is_newline() {
			if return_at_eol {
				return nil
			}
			continue
		}
		if tok.bol && tok.is_punct('#') {
			read_directive()
			continue
		}
		unget_token(tok)
		r := read_expand()
		if r != nil && r.bol && r.is_punct('#') && r.hideset.Empty() {
			read_directive()
			continue
		}
		return r
	}
}

func read_token() *Token {
	r := read_token_int2(false)
	if r == nil {
		return nil
	}
	assert(!r.is_newline())
	assert(r.typ != TTYPE_SPACE)
	assert(r.typ != TTYPE_MACRO_PARAM)
	return r
}
