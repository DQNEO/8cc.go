package main

import (
	"fmt"
)

func (ctype *Ctype) String() string {
	if ctype == nil {
		return "(nil)"
	}
	switch ctype.typ {
	case CTYPE_VOID:
		return "void"
	case CTYPE_INT:
		return "int"
	case CTYPE_CHAR:
		return "char"
	case CTYPE_FLOAT:
		return "float"
	case CTYPE_DOUBLE:
		return "double"
	case CTYPE_PTR:
		return fmt.Sprintf("*%s", ctype.ptr)
	case CTYPE_ARRAY:
		return fmt.Sprintf("[%d]%s", ctype.len, ctype.ptr)
	case CTYPE_STRUCT:
		s := "(struct"
		if len(ctype.tag) > 0 {
			s += fmt.Sprintf(" %s", ctype.tag)
		}
		for _, field := range ctype.fields {
			s += fmt.Sprintf(" (%s)", field)
		}
		s += ")"
		return s
	default:
		errorf("Unknown ctype: %d", ctype)
	}

	return ""
}

type Block []*Ast

func uop_to_string(op string, ast *Ast) string {
	return fmt.Sprintf("(%s %s)", op, ast.operand)
}

func binop_to_string(op string, ast *Ast) string {
	return fmt.Sprintf("(%s %s %s)", op, ast.left, ast.right)
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
				return fmt.Sprintf("'%c'", ast.ival)
			}
		case CTYPE_INT:
			return fmt.Sprintf("%d", ast.ival)
		case CTYPE_FLOAT, CTYPE_DOUBLE:
			return fmt.Sprintf("%f", ast.fval)
		default:
			errorf("internal error")
			return ""
		}
	case AST_STRING:
		return fmt.Sprintf("\"%s\"", quote_cstring(ast.val))
	case AST_LVAR:
		return fmt.Sprintf("%s", ast.varname)
	case AST_GVAR:
		return fmt.Sprintf("%s", ast.varname)
	case AST_FUNCALL:
		s := fmt.Sprintf("(%s)%s(", ast.ctype, ast.fname)
		for i, v := range ast.args {
			s += v.String()
			if i < len(ast.args)-1 {
				s += ","
			}
		}
		s += ")"
		return s
	case AST_FUNC:
		s := fmt.Sprintf("(%s)%s(",
			ast.ctype,
			ast.fname)
		for i, p := range ast.params {
			s += fmt.Sprintf("%s %s", p.ctype, p)
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
		s := fmt.Sprintf("(decl %s %s",
			ast.declvar.ctype,
			vname)
		if ast.declinit != nil {
			s += fmt.Sprintf(" %s", ast.declinit)
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
		s := fmt.Sprintf("(if %s %s",
			ast.cond,
			ast.then)
		if ast.els != nil {
			s += fmt.Sprintf(" %s", ast.els)
		}
		s += ")"
		return s
	case AST_TERNARY:
		return fmt.Sprintf("(? %s %s %s)",
			ast.cond,
			ast.then,
			ast.els)
	case AST_FOR:
		s := fmt.Sprintf("(for %s %s %s %s)",
			ast.init,
			ast.cond,
			ast.step,
			ast.body)
		return s
	case AST_RETURN:
		return fmt.Sprintf("(return %s)", ast.retval)
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
		s += ast.field.name
		return s
	case AST_ADDR:
		return uop_to_string("addr", ast)
	case AST_DEREF:
		return uop_to_string("deref", ast)
	case PUNCT_INC:
		return uop_to_string("++", ast)
	case PUNCT_DEC:
		return uop_to_string("--", ast)
	case PUNCT_LOGAND:
		return binop_to_string("and", ast)
	case PUNCT_LOGOR:
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
		if ast.typ == PUNCT_EQ {
			s += "(== "
		} else {
			s += fmt.Sprintf("(%c ", ast.typ)
		}
		s += fmt.Sprintf("%s %s)", left, right)
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
		if is_punct(tok, PUNCT_EQ) {
			return "=="
		} else {

			return string([]byte{byte(tok.punct)})
		}
	case TTYPE_CHAR:
		return string([]byte{tok.c})
	case TTYPE_NUMBER:
		return tok.sval
	case TTYPE_STRING:
		return tok.sval
	}
	errorf("internal error: unknown token type: %d", tok.typ)
	return ""
}
