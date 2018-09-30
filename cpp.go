package main

var macros = make(map[string]TokenList)
var buffer = make(TokenList, 0)
var altbuffer TokenList = nil
var bol = true
var wastrue bool = true

type CondIncl struct {
	wastrue bool
}
var ci *CondIncl

func make_cond_incl(wastrue bool) *CondIncl {
	r := &CondIncl{
		wastrue: wastrue,
	}
	return r
}

func read_ident2() *Token {
	r := read_cpp_token()
	if !r.is_ident_type() {
		errorf("identifier expected, but got %s", r)
	}
	return r
}

func expand(hideset *Dict, tok *Token) *Token {
	if !tok.is_ident_type() {
		return tok
	}
	if hideset.Get(tok.sval) != nil {
		return tok
	}
	body, ok := macros[tok.sval]
	if !ok {
		return tok
	}
	hideset.Put(tok.sval, &DictValue{})
	buffer = list_append(buffer, body)
	return read_token_int2(hideset, false)
}

func expect_newine() {
	tok := read_cpp_token()
	if tok == nil || !tok.is_newline() {
		errorf("Newline expected, but got %s", tok)
	}
}

func read_undef() {
	name := read_ident2()
	expect_newine()
	delete(macros, name.sval)
}

func read_define() {
	name := read_ident2()
	body := make(TokenList, 0)
	for {
		tok := read_cpp_token()
		if tok == nil || tok.is_newline() {
			break
		}
		body = append(body, tok)
	}
	macros[name.sval] = body
}

func read_line() TokenList {
	var r TokenList
	for {
		tok := read_token_int2(NewDict(), true)
		if tok == nil {
			return r
		}
		r = append(r, tok)
	}
}

func read_constexpr() bool {
	altbuffer = list_reverse(read_line())
	expr := read_expr()
	if len(altbuffer) > 0 {
		  errorf("Stray token: %v", altbuffer);
	}
	altbuffer = nil
	return eval_intexpr(expr) != 0
}

func read_if() {
	cond := read_constexpr()
	ci = make_cond_incl(cond)
	if !cond {
		skip_cond_incl()
	}
}

func read_else() {
	expect_newine()
	if ci.wastrue {
		skip_cond_incl()
	}
}

func read_elif() {
	if ci.wastrue {
		skip_cond_incl()
		return
	} else{
		cond := read_constexpr()
		ci.wastrue = cond
		if !cond {
			skip_cond_incl()
		}
	}
}

func read_endif() {
	expect_newine()
}

func read_directive() {
	tok := read_cpp_token()
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

func read_token_int2(hideset *Dict, return_at_eol bool) *Token {
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
		return expand(hideset, tok)
	}
}

func read_token() *Token {
	return read_token_int2(NewDict(), false)
}
