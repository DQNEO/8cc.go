package main

import (
	"fmt"
	"strconv"
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
	case CTYPE_PTR:
		return fmt.Sprintf("*%s", ctype.ptr)
	case CTYPE_ARRAY:
		return fmt.Sprintf("[%d]%s", ctype.size, ctype.ptr)
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
		_error("Unknown ctype: %d", ctype)
	}

	return ""
}

type Block []*Ast

func uop_to_string(op string, ast *Ast) string {
	return fmt.Sprintf("(%s %s)", op, ast.unary.operand)
}

func binop_to_string(op string, ast *Ast) string {
	return fmt.Sprintf("(%s %s %s)", op, ast.binop.left, ast.binop.right)
}

func (ast *Ast) String() string {
	if ast == nil {
		return "(nil)"
	}
	switch ast.typ {
	case AST_LITERAL:
		switch ast.ctype.typ {
		case CTYPE_INT:
			return fmt.Sprintf("%d", ast.ival)
		case CTYPE_CHAR:
			return fmt.Sprintf("'%c'", ast.c)
		default:
			_error("internal error")
			return ""
		}
	case AST_STRING:
		return fmt.Sprintf("\"%s\"", quote_cstring(ast.str.val))
	case AST_LVAR:
		return fmt.Sprintf("%s", ast.variable.varname)
	case AST_GVAR:
		return fmt.Sprintf("%s", ast.variable.varname)
	case AST_FUNCALL:
		s := fmt.Sprintf("(%s)%s(", ast.ctype, ast.fnc.fname)
		for i, v := range ast.fnc.args {
			s += v.String()
			if i < len(ast.fnc.args)-1 {
				s += ","
			}
		}
		s += ")"
		return s
	case AST_FUNC:
		s := fmt.Sprintf("(%s)%s(",
			ast.ctype,
			ast.fnc.fname)
		for i, p := range ast.fnc.params {
			s += fmt.Sprintf("%s %s", p.ctype, p)
			if i < (len(ast.fnc.params) - 1) {
				s += ","
			}
		}
		s += ")"
		s += ast.fnc.body.String()
		return s
	case AST_DECL:
		var vname Cstring
		if ast.decl.declvar.typ == AST_GVAR {
			vname = ast.decl.declvar.variable.varname
		} else {
			vname = ast.decl.declvar.variable.varname
		}
		s := fmt.Sprintf("(decl %s %s",
			ast.decl.declvar.ctype,
			vname)
		if ast.decl.declinit != nil {
			s += fmt.Sprintf(" %s", ast.decl.declinit)
		}
		s += ")"
		return s
	case AST_ARRAY_INIT:
		s := "{"
		for i, v := range ast.array_initializer.arrayinit {
			s += v.String()
			if i != len(ast.array_initializer.arrayinit)-1 {
				s += ","
			}
		}
		s += "}"
		return s
	case AST_IF:
		s := fmt.Sprintf("(if %s %s",
			ast._if.cond,
			ast._if.then)
		if ast._if.els != nil {
			s += fmt.Sprintf(" %s", ast._if.els)
		}
		s += ")"
		return s
	case AST_TERNARY:
		return fmt.Sprintf("(? %s %s %s)",
			ast._if.cond,
			ast._if.then,
			ast._if.els)
	case AST_FOR:
		s := fmt.Sprintf("(for %s %s %s %s)",
			ast._for.init,
			ast._for.cond,
			ast._for.step,
			ast._for.body)
		return s
	case AST_RETURN:
		return fmt.Sprintf("(return %s)", ast._return.retval)
	case AST_COMPOUND_STMT:
		s := "{"
		for _, v := range ast.compound.stmts {
			s += v.String()
			s += ";"
		}
		s += "}"
		return s
	case AST_STRUCT_REF:
		s := ast.structref.struc.String()
		s += "."
		s += ast.structref.field.name.String()
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
		left := ast.binop.left
		right := ast.binop.right
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
	return tok.ToCtring().String()
}

func (tok *Token) ToCtring() Cstring {
	if tok == nil {
		return NewCstringFromLiteral("(null)")
	}
	switch tok.typ {
	case TTYPE_IDENT:
		return tok.v.sval
	case TTYPE_PUNCT:
		if is_punct(tok, PUNCT_EQ) {
			return Cstring("==")
		} else {
			return Cstring{byte(tok.v.punct), 0}
		}
	case TTYPE_CHAR:
		return Cstring{tok.v.c, 0}
	case TTYPE_INT:
		return NewCstringFromLiteral(strconv.Itoa(tok.v.ival))
	case TTYPE_STRING:
		return tok.v.sval
	}
	_error("internal error: unknown token type: %d", tok.typ)
	return nil
}

