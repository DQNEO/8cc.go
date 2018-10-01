package main

var macros = make(map[string]*Macro)
var buffer = make(TokenList, 0)
var altbuffer TokenList = nil
var cond_incl_stack = make([]*CondIncl, 0)
var bol = true

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
	typ   MacroType
	nargs int
	body  TokenList
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
		typ:  MACRO_OBJ,
		body: body,
	}
	return r
}

func make_func_macro(body TokenList, nargs int) *Macro {
	r := &Macro{
		typ:   MACRO_FUNC,
		nargs: nargs,
		body:  body,
	}
	return r
}

func make_macro_token(position int) *Token {
	r := &Token{
		typ:      TTYPE_MACRO_PARAM,
		hideset:  MakeDict(nil),
		position: position,
		space :   false,
	}
	return r
}

func copy_token(tok *Token) *Token {
	r := *tok
	return &r
}

func expect2(punct int) {
	tok := get_token()
	if tok == nil || !tok.is_punct(punct) {
		errorf("%c expected, but got %s", punct, tok)
	}
}

func read_ident2() *Token {
	r := get_token()
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
	tok := get_token()
	if tok == nil {
		return nil
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
		tokens := subst(macro, make([]TokenList , 0), hideset)
		unget_all(tokens)
		return read_expand()
	case MACRO_FUNC:
		args := read_args(macro)
		rparen := get_token()
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

func read_funclike_macro_args(param *Dict) {
	pos := 0
	for {
		tok := get_token()
		if tok.is_punct(')') {
			return
		}
		if pos > 0 {
			if !tok.is_punct(',') {
				errorf("',' expected, but got '%s'", tok)
			}
			tok = get_token()
		}
		if tok == nil || tok.typ == TTYPE_NEWLINE {
			errorf("missing ')' in macro parameter list")
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
		tok := get_token()
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
	read_funclike_macro_args(param)
	body := read_funclike_macro_body(param)
	macro := make_func_macro(body, len(param.Keys()))
	macros[name] = macro
}

func expect_newline() {
	tok := get_token()
	if tok == nil || !tok.is_newline() {
		errorf("Newline expected, but got %s", tok)
	}
}

func read_args_int() []TokenList {
	tok := get_token()
	if tok == nil || !tok.is_punct('(') {
		unget_token(tok)
		return nil
	}
	r := make([]TokenList, 0)
	arg := make_list()
	depth := 0
	for {
		tok = get_token()
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
		if tok.is_punct(',') {
			r = append(r, arg)
			arg = make_list()
			continue
		}
		arg = append(arg, tok)
	}
}

func read_args(macro *Macro) []TokenList {
	args := read_args_int()
	if args == nil {
		return nil
	}
	if len(args) != macro.nargs {
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

func dict_intersection(a *Dict , b *Dict) *Dict {
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

func join_tokens(args TokenList) string {
	s := ""
	for _, tok := range args {
		switch tok.typ {
		case TTYPE_IDENT, TTYPE_NUMBER:
			s += tok.sval
		default:
			errorf("internal error")
		}
	}
	return s
}

func stringize(args TokenList) *Token {
	r := &Token {
		typ : TTYPE_STRING,
		sval : join_tokens(args),
	}
	return r
}
func expand_all(tokens TokenList) TokenList {
	r := make_list()
	orig := altbuffer
	altbuffer = list_reverse(tokens)
	tok := read_expand()
	for ; tok != nil ; tok = read_expand() {
		r = append(r, tok)
	}
	altbuffer  = orig
	return r
}

func subst(macro *Macro, args []TokenList, hideset *Dict) TokenList {
	r := make_list()
	for i := 0; i < len(macro.body); i++ {
		islast := i == (len(macro.body) -1)
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
			arg := args[t1.position]
			r = append(r, stringize(arg))
			i++
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
		tok := get_token()
		if tok == nil || tok.is_newline() {
			break
		}
		body = append(body, tok)
	}
	macros[name] = make_obj_marco(body)
}

func read_define() {
	name := read_ident2()
	tok := get_token()
	if tok != nil && tok.is_punct('(') && !tok.space {
		read_funclike_macro(name.sval)
		return
	}
	unget_token(tok)
	read_obj_macro(name.sval)
}

func read_defined_operator() *Token {
	tok := get_token()
	if tok.is_punct('(') {
		tok = get_token()
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
	altbuffer = list_reverse(read_intexpr_line())
	expr := read_expr()
	if len(altbuffer) > 0 {
		errorf("Stray token: %v", altbuffer)
	}
	altbuffer = nil
	return eval_intexpr(expr) != 0
}

func read_if() {
	cond := read_constexpr()
	cond_incl_stack = append(cond_incl_stack, make_cond_incl(IN_THEN, cond))
	if !cond {
		skip_cond_incl()
	}
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

func read_directive() {
	tok := get_token()
	if tok.is_ident("define") {
		read_define()
	} else if tok.is_ident("undef") {
		read_undef()
	} else if tok.is_ident("if") {
		read_if()
	} else if tok.is_ident("else") {
		read_else()
	} else if tok.is_ident("elif") {
		read_elif()
	} else if tok.is_ident("endif") {
		read_endif()
	} else {
		errorf("unsupported preprocessor directive: %s", tok)
	}
}

func unget_token(tok *Token) {
	if altbuffer != nil {
		altbuffer = append(altbuffer, tok)
	} else {
		buffer = append(buffer, tok)
	}
}

func peek_token() *Token {
	tok := read_token()
	unget_token(tok)
	return tok
}

func get_token() *Token {
	var tok *Token
	if altbuffer != nil {
		altbuffer, tok = list_pop(altbuffer)
		return tok
	}

	if len(buffer) > 0 {
		buffer, tok = list_pop(buffer)
	} else {
		tok = read_cpp_token()
	}

	return tok
}

func read_token_int2(return_at_eol bool) *Token {
	for {
		tok := get_token()
		if tok == nil {
			return nil
		}
		if tok != nil && tok.is_newline() {
			bol = true
			if return_at_eol {
				return nil
			}
			continue
		}
		if bol && tok.is_punct('#') {
			read_directive()
			bol = true
			continue
		}
		bol = false
		unget_token(tok)
		return read_expand()
	}
}

func read_token() *Token {
	return read_token_int2(false)
}
