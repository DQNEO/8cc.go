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
		sval  Cstring
		punct int
		c     byte
	}
}

const (
	AST_LITERAL int = iota
	AST_STRING
	AST_LVAR
	AST_GVAR
	AST_FUNCALL
	AST_FUNC
	AST_DECL
	AST_ARRAY_INIT
	AST_ADDR
	AST_DEREF
	AST_IF
	AST_FOR
	AST_RETURN
	AST_COMPOUND_STMT
	PUNCT_EQ
	PUNCT_INC
	PUNCT_DEC
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
	typ   int
	ctype *Ctype
	// want to be "union"
	// Integer
	ival int
	// Char
	c byte
	// String
	str struct {
		val    Cstring
		slabel Cstring
	}
	// Local variable
	variable struct {
		lname Cstring
		loff  int
	}
	// Global variable
	gvar struct {
		gname  Cstring
		glabel Cstring
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
	// Function call or function declaration
	fnc struct {
		fname  Cstring
		args   []*Ast
		params []*Ast
		locals []*Ast
		body   *Ast
	}
	// Declaration
	decl struct {
		declvar  *Ast
		declinit *Ast
	}
	// Array initializer
	array_initializer struct {
		arrayinit []*Ast
	}
	// If statement
	_if struct {
		cond *Ast
		then *Ast
		els  *Ast
	}
	// For statement
	_for struct {
		init *Ast
		cond *Ast
		step *Ast
		body *Ast
	}
	_return  struct {
		retval *Ast
	}
	compound struct {
		stmts []*Ast
	}
}
