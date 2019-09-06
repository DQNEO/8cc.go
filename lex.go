package main

import (
	"fmt"
	"os"
)

const BUFLEN = 256

var at_bol = true

type File struct {
	name string
	line int
	column int
	fp *stream
}

var buffer = make(TokenList, 0)
var altbuffer TokenList = nil
var file_stack []*File
var file *File
var ungotten byte = 0 // this maybe fragile

var newline_token = &Token{typ: TTYPE_NEWLINE}
var space_token = &Token{typ: TTYPE_SPACE}

func make_file(name string, s *stream) *File {
	return &File{
		name: name,
		line: 1,
		column:1,
		fp : s,
	}
}

func initLex() {
	file = make_file("(stdin)", stdin)
}

func make_token(typ int) *Token {
	r := &Token{}
	r.typ = typ
	r.hideset = MakeDict(nil)
	r.space = false
	r.bol = false
	r.file = file.name
	r.line = file.line
	r.column = file.column
	return r
}

func make_ident(p string) *Token {
	r := make_token(TTYPE_IDENT)
	r.sval = p
	return r
}

func make_strtok(s string) *Token {
	r := make_token(TTYPE_STRING)
	r.sval = s
	return r
}

func make_punct(punct int) *Token {
	r := make_token(TTYPE_PUNCT)
	r.punct = punct
	return r
}

func make_number(s string) *Token {
	r := make_token(TTYPE_NUMBER)
	r.sval = s
	return r
}

func make_char(c byte) *Token {
	r := make_token(TTYPE_CHAR)
	r.c = c
	return r
}

func push_input_file(filename string, input *os.File) {
	file_stack = append(file_stack, file)
	file = make_file(filename, newStream(input))
	at_bol = true
}

func set_input_file(filename string, fp *stream) {
	file = make_file(filename, fp)
	at_bol = true
}

func input_position() string {
	return format("%s:%d:%d", file.name, file.line, file.column)
}

func unget(c byte) {
	if c == '\n' {
		file.line--
	}
	if int(ungotten) > 0 {
		ungetc(ungotten, file.fp)
	}
	ungetc(c, file.fp)
	file.column--
}

func get() (byte,error) {
	var c byte
	var err error
	if ungotten != 0 {
		c = ungotten
	} else {
		c, err = getc(file.fp)
	}
	file.column++
	ungotten = 0
	if c == '\\' {
		c, err = getc(file.fp)
		file.column++
		if c == '\n' {
			file.line++
			file.column = 1
			return get()
		}
		unget(c)
		at_bol = false
		return '\\', nil
	}
	if c == '\n' {
		file.line++
		file.column = 1
		at_bol = true
	} else {
		at_bol = false
	}
	return c, err
}

func skip_line() {
	for {
		c, err := get()
		if err != nil || c == '\n' {
			return
		}
	}
}

func skip_space() {
	for {
		c, err := get()
		if err != nil {
			return
		}
		if c == ' ' || c == '\t' {
			continue
		}
		if c == '/' {
			c ,_ = get()
			if c == '*' {
				skip_block_comment()
				continue
			} else if c == '/' {
				skip_line()
				continue
			}
			unget(c)
			unget('/')
			return
		}
		unget(c)
		return
	}
}

func skip_cond_incl() {
	nest := 0
	for {
		skip_space()
		c, err := get()
		if err != nil {
			return
		}
		if c == '\n' {
			continue
		}
		if c != '#' {
			skip_line()
			continue
		}
		skip_space()
		tok := read_cpp_token()
		if tok.is_newline() {
			continue
		}
		if !tok.is_ident_type() {
			skip_line()
			continue
		}
		if nest == 0 && (tok.is_ident("else") || tok.is_ident("elif") || tok.is_ident("endif")) {
			unget_cpp_token(tok)
			sharp := make_punct('#')
			sharp.bol = true
			unget_cpp_token(sharp)
			return
		}
		if tok.is_ident("if") || tok.is_ident("ifdef") || tok.is_ident("ifndef") {
			nest++
		} else if nest > 0 && tok.is_ident("endif") {
			nest--
		}
		skip_line()
	}
}

func read_number(c byte) *Token {
	var b []byte
	b = append(b, c)
	for {
		c, _ := get()
		if !isdigit(c) && !isalpha(c) && c != '.' {
			unget(c)
			return make_number(string(b))
		}
		b = append(b, c)
	}
}

func read_char() *Token {
	c, err := get()
	if err != nil {
		errorf("Unterminated char")
	}
	if c == '\\' {
		c, err = get()
		if err != nil {
			errorf("Unterminated char")
		}
	}

	c2, err := get()
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
		c, err := get()
		if err != nil {
			errorf("Unterminated string")
		}
		if c == '"' {
			break
		}
		if c == '\\' {
			c, err = get()
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
		c2, _ := get()
		if isalnum(c2) || c2 == '_' {
			buf = append(buf, c2)
		} else {
			unget(c2)
			return make_ident(string(buf))
		}
	}
}

func skip_block_comment() {
	const (
		in_comment    = 1
		asterisk_read = 2
	)
	state := in_comment
	for {
		c, err := get()
		if err != nil {
			errorf("premature end of block comment")
		}
		if c == '*' {
			state = asterisk_read
		} else if state == asterisk_read && c == '/' {
			return
		} else {
			state = in_comment
		}
	}
}

func read_rep(expect byte, t1 int, t2 int) *Token {
	c, _ := get()
	if c == expect {
		return make_punct(t1)
	}
	unget(c)
	return make_punct(t2)
}

func read_token_int() *Token {
	c, err := get()
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
		c, _ = get()
		if c == '/' {
			skip_line()
			return space_token
		}
		if c == '*' {
			skip_block_comment()
			return space_token
		}
		unget(c)
		return make_punct('/')
	case c == '.':
		c, _ = get()
		if c == '.' {
			c, _ = get()
			return make_ident(format("..%c", c))
		}
		unget(c)
		return make_punct('.')

	case c == '*' || c == '(' || c == ')' || c == ',' || c == ';' || c == '[' || c == ']' ||
		c == '{' || c == '}' || c == '?' || c == ':':
		return make_punct(int(c))
	case c == '#':
		c, _ = get()
		if c == '#' {
			return make_ident("##")
		}
		unget(c)
		return make_punct('#')
	case c == '-':
		c, _ = get()
		if c == '-' {
			return make_punct(OP_DEC)
		}
		if c == '>' {
			return make_punct(OP_ARROW)
		}
		unget(c)
		return make_punct('-')
	case c == '<': return read_rep('=', OP_LE, '<')
	case c == '>': return read_rep('=', OP_GE, '>')
	case c == '=':
		return read_rep('=', OP_EQ, int('='))
	case c == '!':
		return read_rep('=', OP_EQ, int('!'))
	case c == '+':
		return read_rep('+', OP_INC, int('+'))
	case c == '&':
		return read_rep('&', OP_LOGAND, int('&'))
	case c == '|':
		return read_rep('|', OP_LOGOR, int('|'))
	case c == '"':
		return read_string()
	case c == '\'':
		return read_char()
	default:
		errorf("Don't know how to handle '%c'", c)
	}

	return nil
}

func read_header_file_name() (string , bool, bool) {
	var std bool
	skip_space()
	var close byte
	c, _ := get()
	if c == '"' {
		std = false
		close = '"'
	} else if c == '<' {
		std = true
		close = '>'
	} else {
		unget(c)
		return "", std, false
	}
	s := ""
	for {
		c, err := get()
		if err != nil || c == '\n' {
			errorf("premature end of header name")
		}
		if c == close {
			break
		}
		s += fmt.Sprintf("%c", c)
	}
	if s == "" {
		errorf("header name shoudl not be empty")
	}

	name := s
	return name, std, true
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

func read_cpp_token_int() *Token {
	var tok *Token
	if altbuffer != nil {
		altbuffer, tok = list_pop(altbuffer)
		return tok
	}
	if len(buffer) > 0 {
		buffer, tok = list_pop(buffer)
		return tok
	}
	bol := at_bol
	tok = read_token_int()
	for tok != nil && tok.typ == TTYPE_SPACE {
		tok = read_token_int()
		if tok != nil {
			tok.space = true
		}
	}

	if tok == nil && len(file_stack) > 0 {
		file.fp.close()
		file = file_stack[len(file_stack) -1]
		at_bol = true
		file_stack = file_stack[:len(file_stack) -1]
		return newline_token
	}
	if tok != nil {
		tok.bol = bol
	}
	return tok
}

func read_cpp_token() *Token {
	return read_cpp_token_int()
}
func (tok *Token) is_ident_type() bool {
	return tok.typ == TTYPE_IDENT
}

func (tok *Token) is_newline() bool {
	return tok.typ == TTYPE_NEWLINE
}
