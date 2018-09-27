package main

var macros = make(map[string][]*Token)
var buffer = make([]*Token, 0)
var bol bool = true

func read_define() {
	name := read_cpp_token()
	if name.typ != TTYPE_IDENT {
		errorf("macro name must be an identifier, but got %s", name)
	}
	body := make([]*Token, 0)
	for {
		tok := read_cpp_token()
		if tok != nil && tok.typ == TTYPE_NEWLINE {
			break
		}
		body = append(body, tok)
	}
	macros[name.sval] = body
}

func read_directive() {
	tok := read_cpp_token()
	if is_ident(tok, "define") {
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
			tok = buffer[len(buffer) - 1]
			buffer = buffer[:len(buffer) - 1]
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
		if bol && is_punct(tok, '#')  {
			read_directive()
			bol = true
			continue
		}
		bol = false
		return tok
	}
}
