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

	// intends union
	ival  int
	sval  string
	punct int
	c     byte
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
	AST_TERNARY
	AST_FOR
	AST_RETURN
	AST_COMPOUND_STMT
	AST_STRUCT_REF
	PUNCT_EQ
	PUNCT_INC
	PUNCT_DEC
	PUNCT_LOGAND
	PUNCT_LOGOR
	PUNCT_ARROW
)

const (
	CTYPE_VOID int = iota
	CTYPE_INT
	CTYPE_CHAR
	CTYPE_ARRAY
	CTYPE_PTR
	CTYPE_STRUCT
)

type Ctype struct {
	typ    int
	ptr    *Ctype // pointer or array
	size   int
	len    int    // array
	name   string // struct field
	tag    string // struct
	fields []*Ctype
	offset int // struct
}

type Ast struct {
	typ   int
	ctype *Ctype
	// want to be "union"
	// Integer
	ival int
	// Char
	c byte

	// pseudo Union
	// String
	val    string
	slabel string
	// Local/Global variable
	varname string
	loff    int
	glabel  string
	// Binary operator
	left  *Ast
	right *Ast
	// Unary operator
	operand *Ast
	// Function call or function declaration
	fname     string
	args      []*Ast
	params    []*Ast
	localvars []*Ast
	body      *Ast
	// Declaration
	declvar  *Ast
	declinit *Ast
	// Array initializer
	arrayinit []*Ast
	// If statement or ternary operator
	cond *Ast
	then *Ast
	els  *Ast
	// For statement
	init *Ast
//	cond *Ast
	step *Ast
//	body *Ast
	// return
	retval *Ast
	// compound
	stmts []*Ast
	// StructRef
	struc *Ast
	field *Ctype
}

type Env struct {
	vars    []*Ast
	next    *Env
	structs []*Ast
}

var EMPTY_ENV = Env{}
