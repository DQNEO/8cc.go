package main

type char struct {
	c byte
	typ int
}

var ungotten *char

func skip_space_read_ch() *char {
	if ungotten != nil {
		ret := ungotten
		ungotten = nil
		return ret
	}
	skip_space()
	return read_ch()
}

func _read_token() *Token {
	var tk *Token
	ch := skip_space_read_ch()
	if ch == nil {
		return nil
	}
	switch ch.typ {
	case TTYPE_IDENT:
		tk = read_ident(ch.c)
	case TTYPE_INT:
		tk = read_number(int(ch.c - '0'))
	case TTYPE_CHAR:
		tk = read_char()
	case TTYPE_STRING:
		tk = read_string()
	case TTYPE_PUNCT:
		tk = make_punct(ch.c)
	default:
		_error("Don't know how to handle '%c'", ch.c)
	}
	return tk
}

func read_ch() *char {
	c,err := getc(stdin)
	if err != nil {
		return nil
	}
	ch := &char{c:c}
	// TODO use switch syntax
	if '0' <= c && c <= '9' {
		ch.typ = TTYPE_INT
	}
	if c == '"' {
		ch.typ = TTYPE_STRING
	}
	if c == '\'' {
		ch.typ = TTYPE_CHAR
	}
	if ('a'<= c && c <= 'z') || ('A' <= c && c <= 'Z') || c == '_' {
		ch.typ = TTYPE_IDENT
	}
	if c == '/' || c == '=' || c == '*' ||
		c == '+' || c == '-' || c == '(' ||
			c == ')' || c == ',' || c == ';' {
		ch.typ = TTYPE_PUNCT
	}

	return ch
}

func unget_ch(c *char) {
	if ungotten != nil {
		_error("Push back buffer is already full");
	}
	ungotten = c
}

func skip_space() {
	for {
		c, err := getc(stdin)
		if err != nil {
			break
		}
		if isspace(c) {
			continue
		}
		ungetc(c, stdin)
		return
	}
}

func is_punctchar(ch *char, c byte) bool {
	if ch == nil {
		_error("Token is null")
	}
	return ch.c == c
}

func make_int(n int) *Token {
	r := &Token{}
	r.typ = TTYPE_INT
	r.v.ival = n
	return r
}

func make_char(c byte) *Token {
	r := &Token{}
	r.typ = TTYPE_CHAR
	r.v.c = c
	return r
}


func make_punct(c byte) *Token {
	r := &Token{}
	r.typ = TTYPE_PUNCT
	r.v.c = c
	return r
}

func make_string(s []byte) *Token {
	r := &Token{}
	r.typ = TTYPE_STRING
	r.v.sval = s
	return r
}

func make_ident(s []byte) *Token {
	r := &Token{}
	r.typ = TTYPE_IDENT
	r.v.sval = s
	return r
}
func read_number(n int) *Token {
	for {
		ch := skip_space_read_ch()
		if !isdigit(ch.c) {
			unget_ch(ch)
			return make_int(n)
		}
		n = n * 10 + int(ch.c - '0')
	}
}

func read_ident(c byte) *Token {
	buf := make([]byte, BUFLEN)
	buf[0] = c
	i := 1
	for {
		ch := skip_space_read_ch()
		if (!isalnum(ch.c)) {
			unget_ch(ch)
			break
		}
		buf[i] = ch.c
		i++
		if i == (BUFLEN -1) {
			_error("Identifier too long")
		}
	}
	buf[i] = 0;
	return make_ident(buf)
}


func read_char() *Token {
	ch := read_ch()
	if ch == nil {
		_error("Unterminated char")
	}
	if ch.c == '\\' {
		ch = read_ch()
		if ch == nil {
			_error("Unterminated char")
		}
	}

	ch2 := read_ch()
	if ch2 == nil {
		_error("Unterminated char")
	}
	if ch2.c != '\'' {
		_error("Malformed char constant")
	}

	return make_char(ch.c)
}

func read_string() *Token {
	buf := make([]byte, BUFLEN)
	i := 0
	for {
		ch := read_ch()
		if ch == nil {
			_error("Unterminated string")
		}
		if ch.c == '"' {
			break
		}
		if ch.c == '\\' {
			ch = read_ch()
			if ch == nil {
				_error("Unterminated \\")
			}
		}
		buf[i] = ch.c
		i++
		if i == BUFLEN - 1 {
			_error("String too long")
		}
	}
	buf[i] = 0
	return make_string(buf)
}
