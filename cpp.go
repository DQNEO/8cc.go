package main

var macros = make(map[string]TokenList)
var buffer = make(TokenList, 0)
var bol = true

func list_append(a TokenList, b TokenList) TokenList{
	r := a
	for _, tok := range b {
		r = append(r, tok)
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
	return read_token_int2(hideset)
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

func read_directive() {
	tok := read_cpp_token()
	if tok.is_ident("define") {
		read_define()
	} else if tok.is_ident("undef") {
		read_undef()
	} else {
		errorf("unsupported preprocessor directive: %s", tok)
	}
}

func unget_token(tok *Token) {
	buffer = append(buffer, tok)
}

func peek_token() *Token {
	tok := read_token()
	unget_token(tok)
	return tok
}

func read_token_int2(hideset *Dict) *Token {
	for {
		var tok *Token
		if len(buffer) > 0 {
			// list_pop
			tok = buffer[len(buffer)-1]
			buffer = buffer[:len(buffer)-1]
		} else {
			tok = read_cpp_token()
		}
		if tok == nil {
			return nil
		}
		if tok != nil && tok.is_newline() {
			bol = true
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
	return read_token_int2(NewDict())
}
