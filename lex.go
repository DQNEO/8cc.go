package main

type char struct {
	c byte
}

func read_ch() *char {
	c,err := getc(stdin)
	if err != nil {
		return nil
	}
	return &char{c:c}
}

func unget_ch(c *char) {
	ungetc(c.c, stdin)
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

func is_punct(ch *char, c byte) bool {
	if ch == nil {
		_error("Token is null")
	}
	return ch.c == c
}

func read_number(n int) *Ast {
	for {
		ch := read_ch()
		if !isdigit(ch.c) {
			unget_ch(ch)
			return make_ast_int(n)
		}
		n = n * 10 + int(ch.c - '0')
	}
}

func read_ident(c byte) []byte {
	buf := make([]byte, BUFLEN)
	buf[0] = c
	i := 1
	for {
		ch := read_ch()
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
	return buf
}


func read_char() *Ast {
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

	return make_ast_char(ch.c)
}

func read_string() *Ast {
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
	return make_ast_str(buf)
}
