package main

var buffer = make([]*Token, 0)
var bol bool = true

func read_directive() {
	for {
		tok := read_cpp_token()
		printf("# debug token:%s\n", tok)
		if tok == nil {
			return
		}
		if tok.typ == TTYPE_NEWLINE {
			printf("# newline\n")
			return
		}

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
			continue
		}
		bol = false
		return tok
	}
}
