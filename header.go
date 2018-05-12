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

const (
	AST_LITERAL byte = iota
	AST_STRING
	AST_LVAR
	AST_LREF
	AST_GVAR
	AST_GREF
	AST_FUNCALL
	AST_DECL
	AST_ARRAY_INIT
	AST_ADDR
	AST_DEREF
	AST_IF
)

const (
	CTYPE_VOID int = iota
	CTYPE_INT
	CTYPE_CHAR
	CTYPE_ARRAY
	CTYPE_PTR
)

type Ctype struct {
	typ  int
	ptr  *Ctype
	size int
}

type Ast struct {
	typ   byte
	ctype *Ctype
	next  *Ast
	// want to be "union"
	// Integer
	ival int
	// Char
	c byte
	// String
	str struct {
		val    []byte
		slabel []byte
	}
	// Local variable
	variable struct {
		lname []byte
		loff  int
	}
	// Global variable
	gvar struct {
		gname  []byte
		glabel []byte
	}
	// Local reference
	lref struct {
		ref *Ast
		off int
	}
	// Global reference
	gref struct {
		ref *Ast
		off int
	}
	// Binary operator
	binop struct {
		left  *Ast
		right *Ast
	}
	// Unary operator
	unary struct {
		operand *Ast
	}
	// Function call
	funcall struct {
		fname []byte
		nargs int
		args  []*Ast
	}
	// Declaration
	decl struct {
		declvar  *Ast
		declinit *Ast
	}
	// Array initializer
	array_initializer struct {
		size       int
		array_init []*Ast
	}
	// If statement
	_if struct {
		cond *Ast
		then []*Ast
		els  []*Ast
	}
}

func assert(expr bool) {
	if !expr {
		_error("Assertion failed.s")
	}
}
