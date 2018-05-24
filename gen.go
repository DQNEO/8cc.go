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
	default:
		_error("internal error")
	}
	return -1
}

func emit_gload(ctype *Ctype, label Cstring, off int) {
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
	if off > 0 {
		emit("add $%d, %%rax", off*size)
	}
	emit("mov (%%rax), %%%s", reg)
}

func emit_lload(v *Ast, off int) {
	if v.ctype.typ == CTYPE_ARRAY {
		emit("lea %d(%%rbp), %%rax", -v.variable.loff)
		return
	}
	size := ctype_size(v.ctype)
	switch size {
	case 1:
		emit("mov $0, %%eax")
		emit("mov %d(%%rbp), %%al", -v.variable.loff)
	case 4:
		emit("mov %d(%%rbp), %%eax", -v.variable.loff)
	case 8:
		emit("mov %d(%%rbp), %%rax", -v.variable.loff)
	default:
		_error("Unknown data size: %s: %d", v, size)
	}
	if off > 0 {
		emit("add $%d, %%rax", size, off*size)
	}
}

func emit_gsave(v *Ast, off int) {
	assert(v.ctype.typ != CTYPE_ARRAY)
	var reg string
	emit("push %%rcx")
	emit("mov %s(%%rip) %%rcx", v.gvar.glabel)

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
	emit("mov %s, %d(%%rbp)", reg, off*size)
	emit("pop %%rcx")
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

func emit_deref(variable *Ast, value *Ast) {
	emit_expr(variable.unary.operand)
	emit("push %%rax")
	emit_expr(value)
	emit("pop %%rcx")
	var reg string
	size := ctype_size(variable.unary.operand.ctype)
	switch size {
	case 1:
		reg = "al"
	case 4:
		reg = "eax"
	case 8:
		reg = "rax"
	}

	emit("mov %%%s, (%%rcx)", reg)

}

func emit_pointer_arith(_ byte, left *Ast, right *Ast) {
	assert(left.ctype.typ == CTYPE_PTR)
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

func emit_assign(variable *Ast, value *Ast) {
	emit_expr(value)
	switch variable.typ {
	case AST_LVAR:
		emit_lsave(variable.ctype, variable.variable.loff, 0)
	case AST_LREF:
		emit_lsave(variable.lref.ref.ctype, variable.lref.ref.variable.loff, variable.lref.off)
	case AST_GVAR:
		emit_gsave(variable, 0)
	case AST_DEREF:
		emit_deref(variable, value)
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
		emit_assign(ast.binop.left, ast.binop.right)
		return
	}
	if ast.typ == '@' {
		emit_comp("sete", ast.binop.left, ast.binop.right)
		return
	}
	if ast.ctype.typ == CTYPE_PTR {
		emit_pointer_arith(ast.typ, ast.binop.left, ast.binop.right)
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
		_error("invalid operator '%c", ast.typ)
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
		emit_lload(ast, 0)
	case AST_LREF:
		assert(ast.lref.ref.typ == AST_LVAR)
		emit_lload(ast.lref.ref, ast.lref.off)
	case AST_GVAR:
		emit_gload(ast.ctype, ast.gvar.glabel, 0)
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
			emit_gload(ast.decl.declinit.ctype, ast.decl.declinit.str.slabel, 0)
			emit_lsave(ast.decl.declvar.ctype, ast.decl.declvar.variable.loff, 0)
		} else {
			emit_expr(ast.decl.declinit)
			emit_lsave(ast.decl.declvar.ctype, ast.decl.declvar.variable.loff, 0)
		}
	case AST_ADDR:
		assert(ast.unary.operand.typ == AST_LVAR)
		emit("lea %d(%%rbp), %%rax", -ast.unary.operand.variable.loff)
	case AST_DEREF:
		assert(ast.unary.operand.ctype.typ == CTYPE_PTR)
		emit_expr(ast.unary.operand)
		var reg string
		switch ctype_size(ast.ctype) {
		case 1:
			reg = "%cl"
		case 4:
			reg = "%ecx"
		case 8:
			reg = "%rcx"
		default:
			_error("internal error")
		}
		emit("mov $0, %%ecx")
		emit("mov (%%rax), %s", reg)
		emit("mov %%rcx, %%rax")
	case AST_IF:
		emit_expr(ast._if.cond)
		ne := make_label()
		emit("test %%rax, %%rax")
		emit("je %s", ne)
		emit_block(ast._if.then)
		if ast._if.els != nil {
			end := make_label()
			emit("jmp %s", end)
			emit("%s:", ne)
			emit_block(ast._if.els)
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
		emit_block(ast._for.body)
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
	default:
		emit_binop(ast)
	}
}

func emit_data_section() {
	if globals == nil {
		return
	}
	emit(".data")
	for _, v := range globals {
		assert(v.typ == AST_STRING)
		emit("%s:", v.str.slabel)
		emit(".string \"%s\"", quote_cstring(v.str.val))
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
	for _, v := range fn.fnc.locals {
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

func emit_block(block Block) {
	for _, v := range block {
		emit_expr(v)
	}

}

func emit_func(fnc *Ast) {
	assert(fnc.typ == AST_FUNC)
	emit_func_prologue(fnc)
	emit_block(fnc.fnc.body)
	emit_func_epilogue()
}
