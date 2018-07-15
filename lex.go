package main

const BUFLEN = 256

var ungotten *Token

func make_ident(s string) *Token {
	r := &Token{}
	r.typ = TTYPE_IDENT
	r.sval = s
	return r
}

func make_strtok(s string) *Token {
	r := &Token{}
	r.typ = TTYPE_STRING
	r.sval = s
	return r
}

func make_punct(punct int) *Token {
	r := &Token{}
	r.typ = TTYPE_PUNCT
	r.punct = punct
	return r
}

func make_number(s string) *Token {
	r := &Token{}
	r.typ = TTYPE_NUMBER
	r.sval = s
	return r
}

func make_char(c byte) *Token {
	r := &Token{}
	r.typ = TTYPE_CHAR
	r.c = c
	return r
}

func getc_nonspace() (byte, error) {
	var c byte
	var err error
	for {
		c, err = getc(stdin)
		if err != nil {
			break
		}
		if isspace(c) || c == byte('\n') || c == byte('\r') {
			continue
		}
		return c, nil
	}
	return 0, err
}

func read_number(c byte) *Token {
	var b []byte
	b = append(b, c)
	for {
		c, _ := getc(stdin)
		if !isdigit(c) && c != '.' {
			ungetc(c, stdin)
			return make_number(string(b))
		}
		b = append(b, c)
	}
}

func read_char() *Token {
	c, err := getc(stdin)
	if err != nil {
		errorf("Unterminated char")
	}
	if c == '\\' {
		c, err = getc(stdin)
		if err != nil {
			errorf("Unterminated char")
		}
	}

	c2, err := getc(stdin)
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
		c, err := getc(stdin)
		if err != nil {
			errorf("Unterminated string")
		}
		if c == '"' {
			break
		}
		if c == '\\' {
			c, err = getc(stdin)
			if err != nil {
				errorf("Unterminated \\")
			}
			switch c {
			case '"':
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
		c2, _ := getc(stdin)
		if isalnum(c2) || c2 == '_' {
			buf = append(buf, c2)
		} else {
			ungetc(c2, stdin)
			return make_ident(string(buf))
		}
	}
}

func skip_line_comment() byte {
	for {
		c,err := getc(stdin)
		if c == '\n' || err != nil {
			return c
		}
	}
}

func read_rep(expect int, t1 int, t2 int) *Token {
	c, _ := getc(stdin)
	if c == byte(expect) {
		return make_punct(t2)
	}
	ungetc(c, stdin)
	return make_punct(t1)
}

func read_token_init() *Token {
	c, err := getc_nonspace()
	if err != nil {
		// EOF
		return nil
	}

	switch {
	case '0' <= c && c <= '9':
		return read_number(c)
	case ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c == '_':
		return read_ident(c)
	case c == '*' || c == '(' ||
		c == ')' || c == ',' || c == ';' || c == '.' ||
		c == '[' || c == ']' || c == '{' || c == '}' ||
		c == '<' || c == '>' || c == '!' ||
		c == '?' || c == ':':
		return make_punct(int(c))
	case c == '/':
		c, _ = getc(stdin)
		if c == '/' {
			skip_line_comment()
			return read_token_init()
		} else {
			ungetc(c, stdin)
			return make_punct('/')
		}
		return make_punct(int(c))
	case c == '-':
		c, _ = getc(stdin)
		if c == '-' {
			return make_punct(PUNCT_DEC)
		}
		if c == '>' {
			return make_punct(PUNCT_ARROW)
		}
		ungetc(c, stdin)
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

func is_punct(tok *Token, c int) bool {
	return tok != nil && (tok.typ == TTYPE_PUNCT && tok.punct == c)
}

func unget_token(tok *Token) {
	if tok == nil {
		return
	}
	if ungotten != nil {
		errorf("Push back buffer is already full")
	}
	ungotten = tok
}

func peek_token() *Token {
	tok := read_token()
	unget_token(tok)
	return tok
}

func read_token() *Token {
	if ungotten != nil {
		tok := ungotten
		ungotten = nil
		return tok
	}

	return read_token_init()
}
