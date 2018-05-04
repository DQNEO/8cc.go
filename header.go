package main

const (
	TTYPE_IDENT int = iota
	TTYPE_PUNCT
	TTYPE_INT
	TTYPE_CHAR
	TTYPE_STRING
)

type Token struct {
	typ int
	v   struct { // wanna be Union
		ival  int
		sval  []byte
		punct byte
		c     byte
	}
}

func assert(expr bool) {
	if !expr {
		_error("Assertion failed.s")
	}
}
