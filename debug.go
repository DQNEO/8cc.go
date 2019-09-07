package main

import "fmt"

func (ctype *Ctype) String() string {
	if ctype == nil {
		return "(nil)"
	}
	switch ctype.typ {
	case CTYPE_VOID:
		return "void"
	case CTYPE_CHAR:
		return "char"
	case CTYPE_SHORT:
		return "short"
	case CTYPE_INT:
		return "int"
	case CTYPE_LONG:
		return "long"
	case CTYPE_FLOAT:
		return "float"
	case CTYPE_DOUBLE:
		return "double"
	case CTYPE_PTR:
		return format("*%s", ctype.ptr)
	case CTYPE_ARRAY:
		return format("[%d]%s", ctype.len, ctype.ptr)
	case CTYPE_STRUCT:
		s := "(struct"
		for _, v := range ctype.fields.Values() {
			field := v
			s += format(" (%s)", field.ctype)
		}
		s += ")"
		return s
	case CTYPE_FUNC:
		s := format("%s(", ctype.rettype)
		params := ctype.params
		for i, t := range params {
			s += format("%s", t)
			if i != len(params)-1 {
				s += ","
			}
		}
		s += ")"
		return s
	default:
		fmt.Sprintf("Unknown ctype: %d", ctype.typ)
	}

	return ""
}

type Block []*Ast

func uop_to_string(op string, ast *Ast) string {
	return format("(%s %s)", op, ast.operand)
}

func binop_to_string(op string, ast *Ast) string {
	return format("(%s %s %s)", op, ast.left, ast.right)
}

func (ast *Ast) String() string {
	if ast == nil {
		return "(nil)"
	}
	switch ast.typ {
	case AST_LITERAL:
		switch ast.ctype.typ {
		case CTYPE_CHAR:
			if ast.ival == '\n' {
				return "'\n'"
			} else if ast.ival == '\\' {
				return "'\\\\'"
			} else {
				return format("'%c'", ast.ival)
			}
		case CTYPE_INT:
			return format("%d", ast.ival)
		case CTYPE_LONG:
			return format("%dL", ast.ival)
		case CTYPE_FLOAT, CTYPE_DOUBLE:
			return format("%f", ast.fval)
		default:
			errorf("internal error")
			return ""
		}
	case AST_STRING:
		return format("\"%s\"", quote_cstring(ast.val))
	case AST_LVAR:
		return format("%s", ast.varname)
	case AST_GVAR:
		return format("%s", ast.varname)
	case AST_FUNCALL:
		s := format("(%s)%s(", ast.ctype, ast.fname)
		for i, v := range ast.args {
			s += v.String()
			if i < len(ast.args)-1 {
				s += ","
			}
		}
		s += ")"
		return s
	case AST_FUNC:
		s := format("(%s)%s(",
			ast.ctype,
			ast.fname)
		for i, p := range ast.params {
			s += format("%s %s", p.ctype, p)
			if i < (len(ast.params) - 1) {
				s += ","
			}
		}
		s += ")"
		s += ast.body.String()
		return s
	case AST_DECL:
		var vname string
		if ast.declvar.typ == AST_GVAR {
			vname = ast.declvar.varname
		} else {
			vname = ast.declvar.varname
		}
		s := format("(decl %s %s",
			ast.declvar.ctype,
			vname)
		if ast.declinit != nil {
			s += format(" %s", ast.declinit)
		}
		s += ")"
		return s
	case AST_ARRAY_INIT:
		s := "{"
		for i, v := range ast.arrayinit {
			s += v.String()
			if i != len(ast.arrayinit)-1 {
				s += ","
			}
		}
		s += "}"
		return s
	case AST_IF:
		s := format("(if %s %s",
			ast.cond,
			ast.then)
		if ast.els != nil {
			s += format(" %s", ast.els)
		}
		s += ")"
		return s
	case AST_TERNARY:
		return format("(? %s %s %s)",
			ast.cond,
			ast.then,
			ast.els)
	case AST_FOR:
		s := format("(for %s %s %s %s)",
			ast.init,
			ast.cond,
			ast.step,
			ast.body)
		return s
	case AST_RETURN:
		return format("(return %s)", ast.retval)
	case AST_COMPOUND_STMT:
		s := "{"
		for _, v := range ast.stmts {
			s += v.String()
			s += ";"
		}
		s += "}"
		return s
	case AST_STRUCT_REF:
		s := ast.struc.String()
		s += "."
		s += ast.field
		return s
	case AST_ADDR:
		return uop_to_string("addr", ast)
	case AST_DEREF:
		return uop_to_string("deref", ast)
	case OP_GE:
		return binop_to_string( ">=", ast)
	case OP_LE:
		return binop_to_string("<=", ast)
	case OP_NE:
		return binop_to_string("!=", ast)
	case OP_INC:
		return uop_to_string("++", ast)
	case OP_DEC:
		return uop_to_string("--", ast)
	case OP_LOGAND:
		return binop_to_string("and", ast)
	case OP_LOGOR:
		return binop_to_string("or", ast)
	case '!':
		return uop_to_string("!", ast)
	case '&':
		return binop_to_string("&", ast)
	case '|':
		return binop_to_string("|", ast)
	default:
		left := ast.left
		right := ast.right
		var s string
		if ast.typ == OP_EQ {
			s += "(== "
		} else {
			s += format("(%c ", ast.typ)
		}
		s += format("%s %s)", left, right)
		return s
	}
}

func (tok *Token) String() string {
	if tok == nil {
		return "(null)"
	}
	switch tok.typ {
	case TTYPE_IDENT:
		return tok.sval
	case TTYPE_PUNCT:
		if tok.is_punct(OP_EQ) {
			return "=="
		} else if tok.is_punct(OP_NE) {
			return "!="
		}
		return format("%c", tok.punct)
	case TTYPE_CHAR:
		return format("%c", tok.c)
	case TTYPE_NUMBER:
		return tok.sval
	case TTYPE_STRING:
		return format("\"%s\"", tok.sval)
	case TTYPE_NEWLINE:
		return "(newline)"
	case TTYPE_SPACE:
		return "(space)"
	case TTYPE_MACRO_PARAM:
		return "(macro-param)"
	}
	errorf("internal error: unknown token type: %d", tok.typ)
	return ""
}

func (tokens TokenList) String() string {
	r := ""
	for _, token := range tokens {
		r = r + token.String() + " "
	}
	return r
}
