package main

var REGS = []string{"rdi", "rsi", "rdx", "rcx", "r8", "r9"}

func emit(format string, args ...interface{}) {
	printf(format+"\n\t", args...)
}

func ctype_size(ctype *Ctype) int {
	switch ctype.typ {
	case CTYPE_CHAR:
		return 1
	case CTYPE_INT:
		return 4
	case CTYPE_PTR:
		return 8
	case CTYPE_ARRAY:
		return ctype_size(ctype.ptr) * ctype.size
	case CTYPE_STRUCT:
		last := ctype.fields[len(ctype.fields) - 1]
		return last.offset + ctype_size(last)
	default:
		_error("internal error")
	}
	return -1
}

func emit_gload(ctype *Ctype, label Cstring) {
	if ctype.typ == CTYPE_ARRAY {
		emit("lea %s(%%rip), %%rax", label)
		return
	}
	var reg string
	size := ctype_size(ctype)
	switch size {
	case 1:
		reg = "al"
		emit("mov $0, %%eax")
	case 4:
		reg = "eax"
	case 8:
		reg = "rax"
	default:
		_error("Unknown data size: %s: %d", ctype, size)
	}

	emit("mov %s(%%rip), %%%s", label, reg)
}

func emit_lload(ctype *Ctype, off int) {
	if ctype.typ == CTYPE_ARRAY {
		emit("lea %d(%%rbp), %%rax", -off)
		return
	}
	size := ctype_size(ctype)
	switch size {
	case 1:
		emit("mov $0, %%eax")
		emit("mov %d(%%rbp), %%al", -off)
	case 4:
		emit("mov %d(%%rbp), %%eax", -off)
	case 8:
		emit("mov %d(%%rbp), %%rax", -off)
	default:
		_error("Unknown data size: %s: %d", ctype, size)
	}
}

func emit_gsave(v *Ast) {
	assert(v.ctype.typ != CTYPE_ARRAY)
	var reg string

	size := ctype_size(v.ctype)
	switch size {
	case 1:
		reg = "al"
	case 4:
		reg = "eax"
	case 8:
		reg = "rax"
	default:
		_error("Unknown data size: %s: %d", v, size)
	}
	emit("mov %%%s, %s(%%rip)", reg, v.variable.varname)
}

func emit_lsave(ctype *Ctype, loff int, off int) {
	var reg string
	size := ctype_size(ctype)
	switch size {
	case 1:
		reg = "al"
	case 4:
		reg = "eax"
	case 8:
		reg = "rax"
	}
	emit("mov %%%s, %d(%%rbp)", reg, -(loff + off*size))
}

func emit_assign_deref(variable *Ast) {
	emit("push %%rax")
	emit_expr(variable.unary.operand)
	emit("pop %%rcx")
	var reg string
	size := ctype_size(variable.unary.operand.ctype)
	switch size {
	case 1:
		reg = "cl"
	case 4:
		reg = "ecx"
	case 8:
		reg = "rcx"
	}

	emit("mov %%%s, (%%rax)", reg)

}

func emit_pointer_arith(_ byte, left *Ast, right *Ast) {
	emit_expr(left)
	emit("push %%rax")
	emit_expr(right)
	size := ctype_size(left.ctype.ptr)
	if size > 1 {
		emit("imul $%d, %%rax", size)
	}
	emit("mov %%rax, %%rcx")
	emit("pop %%rax")
	emit("add %%rcx, %%rax")
}

func emit_assign_struct_ref(struc *Ast, field *Ctype, off int) {
	switch struc.typ {
	case AST_LVAR:
		emit_lsave(field, struc.variable.loff - field.offset - off, 0)
	case AST_STRUCT_REF:
		emit_assign_struct_ref(struc.structref.struc, struc.structref.field, 0)
	default:
		_error("internal error: %s", struc)
	}
}

func emit_load_struct_ref(struc *Ast, field *Ctype, off int) {
	emit_lload(field, struc.variable.loff - field.offset - off)
}

func emit_assign(variable *Ast) {
	if variable.typ == AST_DEREF {
		emit_assign_deref(variable)
		return
	}
	switch variable.typ {
	case AST_LVAR:
		emit_lsave(variable.ctype, variable.variable.loff, 0)
	case AST_GVAR:
		emit_gsave(variable)
	case AST_STRUCT_REF:
		emit_assign_struct_ref(variable.structref.struc, variable.structref.field, 0)
	default:
		_error("internal error")
	}
}

func emit_comp(inst string, a *Ast, b *Ast) {
	emit_expr(a)
	emit("push %%rax")
	emit_expr(b)
	emit("pop %%rcx")
	emit("cmp %%rax, %%rcx")
	emit("%s %%al", inst)
	emit("movzb %%al, %%eax")
}

func emit_binop(ast *Ast) {
	if ast.typ == '=' {
		emit_expr(ast.binop.right)
		emit_assign(ast.binop.left)
		return
	}
	if ast.typ == PUNCT_EQ {
		emit_comp("sete", ast.binop.left, ast.binop.right)
		return
	}
	if ast.ctype.typ == CTYPE_PTR {
		emit_pointer_arith(byte(ast.typ), ast.binop.left, ast.binop.right)
		return
	}
	var op string
	switch ast.typ {
	case '<':
		emit_comp("setl", ast.binop.left, ast.binop.right)
		return
	case '>':
		emit_comp("setg", ast.binop.left, ast.binop.right)
		return
	case '+':
		op = "add"
	case '-':
		op = "sub"
	case '*':
		op = "imul"
	case '/':
		break
	default:
		_error("invalid operator '%d", ast.typ)
	}

	emit_expr(ast.binop.right)
	emit("push %%rax")
	emit_expr(ast.binop.left)
	if ast.typ == '/' {
		emit("pop %%rcx")
		emit("mov $0, %%edx")
		emit("idiv %%rcx")
	} else {
		emit("pop %%rcx")
		emit("%s %%rcx, %%rax", op)
	}
}

func emit_inc_dec(ast *Ast, op string) {
	emit_expr(ast.unary.operand)
	emit("push %%rax")
	emit("%s $1, %%rax", op)
	emit_assign(ast.unary.operand)
	emit("pop %%rax")
}


func emit_load_deref(result_type *Ctype, operand_type *Ctype, off int) {
	var reg string
	switch ctype_size(result_type) {
	case 1:
		reg = "%cl"
		emit("mov $0, %%ecx")
	case 4:
		reg = "%ecx"
	default:
		reg = "%rcx"
	}
	if operand_type.ptr.typ != CTYPE_ARRAY {
		emit("mov (%%rax), %s", reg)
		emit("mov %%rcx, %%rax")
	}
}

func emit_expr(ast *Ast) {
	switch ast.typ {
	case AST_LITERAL:
		switch ast.ctype.typ {
		case CTYPE_INT:
			emit("mov $%d, %%eax", ast.ival)
		case CTYPE_CHAR:
			emit("mov $%d, %%rax", ast.c)
		default:
			_error("internal error")
		}
	case AST_STRING:
		emit("lea %s(%%rip), %%rax", ast.str.slabel)
	case AST_LVAR:
		emit_lload(ast.ctype, ast.variable.loff)
	case AST_GVAR:
		emit_gload(ast.ctype, ast.variable.glabel)
	case AST_FUNCALL:
		for i := 1; i < len(ast.fnc.args); i++ {
			emit("push %%%s", REGS[i])
		}
		for _, v := range ast.fnc.args {
			emit_expr(v)
			emit("push %%rax")
		}
		for i := len(ast.fnc.args) - 1; i >= 0; i-- {
			emit("pop %%%s", REGS[i])
		}
		emit("mov $0, %%eax")
		emit("call %s", ast.fnc.fname)
		for i := len(ast.fnc.args) - 1; i > 0; i-- {
			emit("pop %%%s", REGS[i])
		}
	case AST_DECL:
		if ast.decl.declinit == nil {
			return
		}
		if ast.decl.declinit.typ == AST_ARRAY_INIT {
			for i, v := range ast.decl.declinit.array_initializer.arrayinit {
				emit_expr(v)
				emit_lsave(ast.decl.declvar.ctype.ptr, ast.decl.declvar.variable.loff, -i)
			}
		} else if ast.decl.declvar.ctype.typ == CTYPE_ARRAY {
			assert(ast.decl.declinit.typ == AST_STRING)
			var i int
			for i = 0; ast.decl.declinit.str.val[i] != 0; i++ {
				emit("movb $%d, %d(%%rbp)", ast.decl.declinit.str.val[i], -(ast.decl.declvar.variable.loff - i))
			}
			emit("movb $0, %d(%%rbp)", -(ast.decl.declvar.variable.loff - i))
		} else if ast.decl.declinit.typ == AST_STRING {
			emit_gload(ast.decl.declinit.ctype, ast.decl.declinit.str.slabel)
			emit_lsave(ast.decl.declvar.ctype, ast.decl.declvar.variable.loff, 0)
		} else {
			emit_expr(ast.decl.declinit)
			emit_lsave(ast.decl.declvar.ctype, ast.decl.declvar.variable.loff, 0)
		}
	case AST_ADDR:
		assert(ast.unary.operand.typ == AST_LVAR)
		emit("lea %d(%%rbp), %%rax", -ast.unary.operand.variable.loff)
	case AST_DEREF:
		emit_expr(ast.unary.operand)
		emit_load_deref(ast.ctype, ast.unary.operand.ctype, 0)
	case AST_IF, AST_TERNARY:
		emit_expr(ast._if.cond)
		ne := make_label()
		emit("test %%rax, %%rax")
		emit("je %s", ne)
		emit_expr(ast._if.then)
		if ast._if.els != nil {
			end := make_label()
			emit("jmp %s", end)
			emit("%s:", ne)
			emit_expr(ast._if.els)
			emit("%s:", end)
		} else {
			emit("%s:", ne)
		}
	case AST_FOR:
		if ast._for.init != nil {
			emit_expr(ast._for.init)
		}
		begin := make_label()
		end := make_label()
		emit("%s:", begin)
		if ast._for.cond != nil {
			emit_expr(ast._for.cond)
			emit("test %%rax, %%rax")
			emit("je %s", end)
		}
		emit_expr(ast._for.body)
		if ast._for.step != nil {
			emit_expr(ast._for.step)
		}
		emit("jmp %s", begin)
		emit("%s:", end)
	case AST_RETURN:
		emit_expr(ast._return.retval)
		emit("leave")
		emit("ret")
		break
	case AST_COMPOUND_STMT:
		for _, v := range ast.compound.stmts {
			emit_expr(v)
		}
	case AST_STRUCT_REF:
		emit_load_struct_ref(ast.structref.struc, ast.structref.field, 0)
	case PUNCT_INC:
		emit_inc_dec(ast, "add")
	case PUNCT_DEC:
		emit_inc_dec(ast, "sub")
	case '!':
		emit_expr(ast.unary.operand)
		emit("cmp $0, %%rax")
		emit("sete %%al")
		emit("movzb %%al, %%eax")
	case '&':
		emit_expr(ast.binop.left)
		emit("push %%rax")
		emit_expr(ast.binop.right)
		emit("pop %%rcx")
		emit("and %%rcx, %%rax")
	case '|':
		emit_expr(ast.binop.left)
		emit("push %%rax")
		emit_expr(ast.binop.right)
		emit("pop %%rcx")
		emit("or %%rcx, %%rax")
	case PUNCT_LOGAND:
		end := make_label()
		emit_expr(ast.binop.left)
		emit("test %%rax, %%rax")
		emit("mov $0, %%rax")
		emit("je %s", end)
		emit_expr(ast.binop.right)
		emit("test %%rax, %%rax")
		emit("mov $0, %%rax")
		emit("je %s", end)
		emit("mov $1, %%rax")
		emit("%s:", end)
	case PUNCT_LOGOR:
		end := make_label()
		emit_expr(ast.binop.left)
		emit("test %%rax, %%rax")
		emit("mov $1, %%rax")
		emit("jne %s", end)
		emit_expr(ast.binop.right)
		emit("test %%rax, %%rax")
		emit("mov $1, %%rax")
		emit("jne %s", end)
		emit("mov $0, %%rax")
		emit("%s:", end)
	default:
		emit_binop(ast)
	}
}

func emit_data_section() {
	if len(globalenv.vars) == 0 {
		return
	}
	emit(".data")
	for _, v := range globalenv.vars {
		if v.typ == AST_STRING {
			emit("%s:", v.str.slabel)
			emit(".string \"%s\"", quote_cstring(v.str.val))
		} else if v.typ != AST_GVAR {
			_error("internal error: %s", v)
		}
	}
}

func ceil8(n int) int {
	rem := n % 8
	if rem == 0 {
		return n
	} else {
		return n - rem + 8
	}
}

func emit_func_prologue(fn *Ast) {
	if len(fn.fnc.params) > len(REGS) {
		_error("Parameter list too long: %s", fn.fnc.fname)
	}
	emit(".text")
	emit(".global %s\n", fn.fnc.fname)
	emit("%s:", fn.fnc.fname)
	emit("push %%rbp")
	emit("mov %%rsp, %%rbp")
	off := 0
	ri := 0
	for _, v := range fn.fnc.params {
		emit("push %%%s", REGS[ri])
		ri++
		off += ceil8(ctype_size(v.ctype))
		v.variable.loff = off
	}
	for _, v := range fn.fnc.localvars {
		off += ceil8(ctype_size(v.ctype))
		v.variable.loff = off
	}
	if off > 0 {
		emit("sub $%d, %%rsp", off)
	}
}

func emit_func_epilogue() {
	emit("leave")
	emit("ret")
}

func emit_label(fmt string, args ...interface{}) {
	emit(fmt, args...)
}

func emit_data_int(data *Ast) {
	assert(data.ctype.typ != CTYPE_ARRAY)
	switch ctype_size(data.ctype) {
	case 1:
		emit(".byte %d", data.ival)
	case 4:
		emit(".long %d", data.ival)
	case 8:
		emit(".quad %d", data.ival)
	default:
		_error("internal error")
	}
}

func emit_data(v *Ast) {
	emit_label(".global %s", v.decl.declvar.variable.varname)
	emit_label("%s:", v.decl.declvar.variable.varname)
	if v.decl.declinit.typ == AST_ARRAY_INIT {
		for _, v := range v.decl.declinit.array_initializer.arrayinit {
			emit_data_int(v)
		}
		return
	}
	assert(v.decl.declinit.typ == AST_LITERAL && v.decl.declinit.ctype.typ == CTYPE_INT)
	emit_data_int(v.decl.declinit)
}

func emit_bss(v *Ast) {
	emit(".lcomm %s, %d", v.decl.declvar.variable.varname, ctype_size(v.decl.declvar.ctype))
}

func emit_global_var(v *Ast) {
	if v.decl.declinit != nil {
		emit_data(v)
	} else {
		emit_bss(v)
	}
}

func emit_toplevel(v *Ast) {
	if v.typ == AST_FUNC {
		emit_func_prologue(v)
		emit_expr(v.fnc.body)
		emit_func_epilogue()
	} else if v.typ == AST_DECL {
		emit_global_var(v)
	} else {
		_error("internal error")
	}
}
