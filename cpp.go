package main

var macros = make(map[string][]*Token)
var buffer = make([]*Token, 0)
var bol = true

func list_append(a []*Token, b []*Token) []*Token {
	r := a
	for _, tok := range b {
		r = append(r, tok)
	}
	return r
}

func expand(tok *Token) *Token {
	if tok.typ != TTYPE_IDENT {
		return tok
	}
	body, ok := macros[tok.sval]
	if !ok {
		return tok
	}
	buffer = list_append(buffer, body)
	return read_token()
}

func read_define() {
	name := read_cpp_token()
	if name.typ != TTYPE_IDENT {
		errorf("macro name must be an identifier, but got %s", name)
	}
	body := make([]*Token, 0)
	for {
		tok := read_cpp_token()
		if tok == nil || tok.typ == TTYPE_NEWLINE {
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

func read_token() *Token {
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
		if tok != nil && tok.typ == TTYPE_NEWLINE {
			bol = true
			continue
		}
		if bol && tok.is_punct('#') {
			read_directive()
			bol = true
			continue
		}
		bol = false
		return expand(tok)
	}
}
