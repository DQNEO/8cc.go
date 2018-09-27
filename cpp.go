package main

var buffer = make([]*Token, 0)

func unget_token(tok *Token) {
	buffer = append(buffer, tok)
}

func peek_token() *Token {
	tok := read_token()
	unget_token(tok)
	return tok
}

func read_token() *Token {
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
	if tok.typ == TTYPE_NEWLINE {
		return read_token()
	}
	return tok
}
