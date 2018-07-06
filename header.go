package main

type float float32

const FLOAT_SIZE = 32
const (
	TTYPE_IDENT = iota
	TTYPE_PUNCT
	TTYPE_NUMBER
	TTYPE_CHAR
	TTYPE_STRING
)

type Token struct {
	typ int

	// intends union
	sval  string
	punct int
	c     byte
}

const (
	AST_LITERAL = iota + 256
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
	CTYPE_CHAR
	CTYPE_INT
	CTYPE_LONG
	CTYPE_FLOAT
	CTYPE_DOUBLE
	CTYPE_ARRAY
	CTYPE_PTR
	CTYPE_STRUCT
)

type Ctype struct {
	typ    int
	size   int
	ptr    *Ctype // pointer or array
	// array length
	len    int
	// struct
	tag    string
	fields *DictCtype
	offset int
}

type Ast struct {
	typ   int
	ctype *Ctype
	// want to be "union"
	// Char, int, or long
	ival int
	// Float or double
	fval   float64
	flabel string

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
	field string
}

type Env struct {
	vars    []*Ast
	next    *Env
	structs []*Ast
}

var EMPTY_ENV = Env{}
