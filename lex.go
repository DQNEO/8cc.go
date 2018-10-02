package main

import "fmt"

const BUFLEN = 256

var buffer = make(TokenList, 0)
var altbuffer TokenList = nil

var newline_token = &Token{typ: TTYPE_NEWLINE}
var space_token = &Token{typ: TTYPE_SPACE}

func make_ident(s string) *Token {
	r := &Token{}
	r.typ = TTYPE_IDENT
	r.hideset = MakeDict(nil)
	r.sval = s
	return r
}

func make_strtok(s string) *Token {
	r := &Token{}
	r.typ = TTYPE_STRING
	r.hideset = MakeDict(nil)
	r.sval = s
	return r
}

func make_punct(punct int) *Token {
	r := &Token{}
	r.typ = TTYPE_PUNCT
	r.hideset = MakeDict(nil)
	r.punct = punct
	return r
}

func make_number(s string) *Token {
	r := &Token{}
	r.typ = TTYPE_NUMBER
	r.hideset = MakeDict(nil)
	r.sval = s
	return r
}

func make_char(c byte) *Token {
	r := &Token{}
	r.typ = TTYPE_CHAR
	r.hideset = MakeDict(nil)
	r.c = c
	return r
}

func make_string_ident(s string) *Token {
	return make_ident(s)
}

func getc_nonspace() (byte, error) {
	var c byte
	var err error
	for {
		c, err = getc(file)
		if err != nil {
			break
		}
		if c == byte(' ') || c == byte('\t') {
			continue
		}
		return c, nil
	}
	return 0, err
}

func skip_line() {
	for {
		c, err := getc(file)
		if err != nil || c == '\n' {
			return
		}
	}
}

func skip_cond_incl() {
	nest := 0
	for {
		c, err := getc_nonspace()
		if err != nil {
			return
		}
		if c != '#' {
			skip_line()
			continue
		}
		tok := read_cpp_token()
		if tok.is_newline() {
			continue
		}
		if !tok.is_ident_type() {
			skip_line()
		} else if tok.is_ident("if") || tok.is_ident("ifdef") || tok.is_ident("ifndef") {
			nest++
		} else if nest > 0 && tok.is_ident("endif") {
			nest--
		} else if nest <= 0 && tok.is_ident("else") || tok.is_ident("elif") || tok.is_ident("endif") {
			unget_cpp_token(tok)
			unget_cpp_token(make_punct('#'))
			return
		} else {
			skip_line()
		}
	}
}

func read_number(c byte) *Token {
	var b []byte
	b = append(b, c)
	for {
		c, _ := getc(file)
		if !isdigit(c) && !isalpha(c) && c != '.' {
			ungetc(c, file)
			return make_number(string(b))
		}
		b = append(b, c)
	}
}

func read_char() *Token {
	c, err := getc(file)
	if err != nil {
		errorf("Unterminated char")
	}
	if c == '\\' {
		c, err = getc(file)
		if err != nil {
			errorf("Unterminated char")
		}
	}

	c2, err := getc(file)
	if err != nil {
		errorf("Unterminated char")
	}
	if c2 != '\'' {
		errorf("Malformed char constant")
	}

	return make_char(c)
}

func read_string() *Token {
	buf := make([]byte, 0, BUFLEN)
	for {
		c, err := getc(file)
		if err != nil {
			errorf("Unterminated string")
		}
		if c == '"' {
			break
		}
		if c == '\\' {
			c, err = getc(file)
			if err != nil {
				errorf("Unterminated \\")
			}
			switch c {
			case '"':
				break
			case '\\':
				c = '\\'
				break
			case 'n':
				c = '\n'
			default:
				errorf("Unknown quote: %c", c)
			}
		}
		buf = append(buf, c)
		if len(buf) == BUFLEN-1 {
			errorf("String too long")
		}
	}
	return make_strtok(string(buf))
}

func read_ident(c byte) *Token {
	buf := make([]byte, 0, BUFLEN)
	buf = append(buf, c)
	for {
		c2, _ := getc(file)
		if isalnum(c2) || c2 == '_' {
			buf = append(buf, c2)
		} else {
			ungetc(c2, file)
			return make_ident(string(buf))
		}
	}
}

func skip_line_comment() {
	for {
		c, err := getc(file)
		if c == '\n' || err != nil {
			return
		}
	}
}

func skip_space() {
	for {
		c, _ := getc(file)
		if c == ' ' || c == '\t' {
			continue
		}
		ungetc(c, file)
		return
	}
}

func skip_block_comment() {
	const (
		in_comment    = 1
		asterisk_read = 2
	)
	state := in_comment
	for {
		c, _ := getc(file)
		if state == in_comment {
			if c == '*' {
				state = asterisk_read
			}
		} else if c == '/' {
			return
		}
	}
}

func read_rep(expect int, t1 int, t2 int) *Token {
	c, _ := getc(file)
	if c == byte(expect) {
		return make_punct(t2)
	}
	ungetc(c, file)
	return make_punct(t1)
}

func read_token_int() *Token {
	c, err := getc(file)
	if err != nil {
		// EOF
		return nil
	}

	switch {
	case c == ' ' || c == '\t':
		skip_space()
		return space_token
	case c == '\n':
		return newline_token
	case '0' <= c && c <= '9':
		return read_number(c)
	case ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c == '_':
		return read_ident(c)
	case c == '/':
		c, _ = getc(file)
		if c == '/' {
			skip_line_comment()
			return read_token_int()
		}
		if c == '*' {
			skip_block_comment()
			return read_token_int()
		}
		ungetc(c, file)
		return make_punct('/')
	case c == '.':
		c, _ = getc(file)
		if c == '.' {
			c, _ = getc(file)
			s := fmt.Sprintf("..%c", c)
			return make_ident(s)
		}
		ungetc(c, file)
		return make_punct('.')

	case c == '*' || c == '(' ||
		c == ')' || c == ',' || c == ';' ||
		c == '[' || c == ']' || c == '{' || c == '}' ||
		c == '<' || c == '>' || c == '!' ||
		c == '?' || c == ':':
		return make_punct(int(c))
	case c == '#':
		c, _ = getc(file)
		if c == '#' {
			return make_string_ident("##")
		}
		ungetc(c, file)
		return make_punct('#')
	case c == '-':
		c, _ = getc(file)
		if c == '-' {
			return make_punct(PUNCT_DEC)
		}
		if c == '>' {
			return make_punct(PUNCT_ARROW)
		}
		ungetc(c, file)
		return make_punct('-')
	case c == '=':
		return read_rep(int('='), int('='), PUNCT_EQ)
	case c == '+':
		return read_rep(int('+'), int('+'), PUNCT_INC)
	case c == '&':
		return read_rep(int('&'), int('&'), PUNCT_LOGAND)
	case c == '|':
		return read_rep(int('|'), int('|'), PUNCT_LOGOR)
	case c == '"':
		return read_string()
	case c == '\'':
		return read_char()
	default:
		errorf("Don't know how to handle '%c'", c)
	}

	return nil
}

func (tok *Token) is_punct(c int) bool {
	return tok != nil && (tok.typ == TTYPE_PUNCT && tok.punct == c)
}

func set_input_buffer(tokens TokenList) {
	if tokens == nil {
		altbuffer = nil
	} else {
		altbuffer = list_reverse(tokens)
	}
}

func get_input_buffer() TokenList {
	return altbuffer
}

func unget_cpp_token(tok *Token) {
	if tok == nil {
		return
	}
	if altbuffer != nil {
		altbuffer = append(altbuffer, tok)
		return
	}
	buffer = append(buffer, tok)
}

func peek_cpp_token() *Token {
	tok := read_token()
	unget_cpp_token(tok)
	return tok
}

func read_cpp_token() *Token {
	var tok *Token
	if altbuffer != nil {
		altbuffer, tok = list_pop(altbuffer)
		return tok
	}
	if len(buffer) > 0 {
		buffer, tok = list_pop(buffer)
		return tok
	}

	tok = read_token_int()
	for tok != nil && tok.typ == TTYPE_SPACE {
		tok = read_token_int()
		if tok != nil {
			tok.space = true
		}
	}
	return tok
}

func (tok *Token) is_ident_type() bool {
	return tok.typ == TTYPE_IDENT
}

func (tok *Token) is_newline() bool {
	return tok.typ == TTYPE_NEWLINE
}
