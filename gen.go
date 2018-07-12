package main

import "unsafe"
var REGS = []string{"rdi", "rsi", "rdx", "rcx", "r8", "r9"}

func emit(format string, args ...interface{}) {
	printf(format+"\n\t", args...)
}

func get_int_reg(ctype *Ctype, r byte) string {
	assert(r == 'a' || r == 'c')
	switch ctype.size {
	case 1:
		if r == 'a' {
			return "al"
		} else {
			return "cl"
		}
	case 4:
		if r == 'a' {
			return "eax"
		} else {
			return "ecx"
		}
	case 8:
		if r == 'a' {
			return "rax"
		} else {
			return "rcx"
		}
	default:
		errorf("Unknown data size: %s: %d", ctype, ctype.size);
	}
	return ""
}

func emit_gload(ctype *Ctype, label string, off int) {
	if ctype.typ == CTYPE_ARRAY {
		if off != 0 {
			emit("lea %s+%d(%%rip), %%rax", label, off)
		} else {
			emit("lea %s(%%rip), %%rax", label)
		}
		return
	}
	reg := get_int_reg(ctype, 'a')
	if ctype.size == 1 {
		emit("mov $0, %%eax")
	}
	if off != 0 {
		emit("mov %s+%d(%%rip), %%%s", label, off, reg)
	} else {
		emit("mov %s(%%rip), %%%s", label, reg)
	}
}

func emit_toint(ctype *Ctype) {
	if ctype.typ != CTYPE_FLOAT {
		return
	}
	emit("cvttss2si %%xmm0, %%eax")
}

func emit_tofloat(ctype *Ctype) {
	if ctype.typ == CTYPE_FLOAT {
		return
	}
	emit("cvtsi2ss %%eax, %%xmm0")
}

func emit_lload(ctype *Ctype, off int) {
	if ctype.typ == CTYPE_ARRAY {
		emit("lea %d(%%rbp), %%rax", off)
		return
	}
	if ctype.typ == CTYPE_FLOAT {
		emit("movss %d(%%rbp), %%xmm0", off)
		return
	}
	reg := get_int_reg(ctype, 'a')
	if ctype.size == 1 {
		emit("mov $0, %%eax")
	}
	emit("mov %d(%%rbp), %%%s", off, reg)

}

func emit_gsave(varname string, ctype *Ctype, off int) {
	assert(ctype.typ != CTYPE_ARRAY)
	reg := get_int_reg(ctype, 'a')
	if off != 0 {
		emit("mov %%%s, %s+%d(%%rip)", reg, varname, off)
	} else {
		emit("mov %%%s, %s(%%rip)", reg, varname)
	}
}

func emit_lsave(ctype *Ctype, off int) {
	if ctype.typ == CTYPE_FLOAT {
		emit("movss %%xmm0, %d(%%rbp)", off)
	} else {
		reg := get_int_reg(ctype, 'a')
		emit("mov %%%s, %d(%%rbp)", reg, off)
	}
}

func emit_assign_deref_int(ctype *Ctype, off int) {
	emit("mov (%%rsp), %%rcx")
	reg := get_int_reg(ctype, 'c')
	if off != 0 {
		emit("mov %%%s, %d(%%rax)", reg, off)
	} else {
		emit("mov %%%s, (%%rax)", reg)
	}
	emit("pop %%rax")
}

func emit_assign_deref(variable *Ast) {
	emit("push %%rax")
	emit_expr(variable.operand)
	emit_assign_deref_int(variable.operand.ctype.ptr, 0)
}

func emit_pointer_arith(_ byte, left *Ast, right *Ast) {
	emit_expr(left)
	emit("push %%rax")
	emit_expr(right)
	size := left.ctype.ptr.size
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
		emit_lsave(field, struc.loff+field.offset+off)
	case AST_GVAR:
		emit_gsave(struc.varname, field, field.offset+off)
	case AST_STRUCT_REF:
		emit_assign_struct_ref(struc.struc, field, off+struc.field.offset)
	case AST_DEREF:
		v := struc
		emit("push %%rax")
		emit_expr(v.operand)
		emit_assign_deref_int(field, field.offset+off)
	default:
		errorf("internal error: %s", struc)
	}
}

func emit_load_struct_ref(struc *Ast, field *Ctype, off int) {
	switch struc.typ {
	case AST_LVAR:
		emit_lload(field, struc.loff+field.offset+off)
	case AST_GVAR:
		emit_gload(field, struc.glabel, field.offset+off)
	case AST_STRUCT_REF:
		emit_load_struct_ref(struc.struc, field, struc.field.offset+off)
	case AST_DEREF:
		emit_expr(struc.operand)
		emit_load_deref(struc.ctype, field, field.offset+off)
	default:
		errorf("internal error: %s", struc)
	}
}

func emit_assign(variable *Ast) {
	if variable.typ == AST_DEREF {
		emit_assign_deref(variable)
		return
	}
	switch variable.typ {
	case AST_DEREF:
		emit_assign_deref(variable)
	case AST_STRUCT_REF:
		emit_assign_struct_ref(variable.struc, variable.field, 0)
	case AST_LVAR:
		emit_lsave(variable.ctype, variable.loff)
	case AST_GVAR:
		emit_gsave(variable.varname, variable.ctype, 0)
	default:
		errorf("internal error")
	}
}

func emit_comp(inst string, ast *Ast) {
	if ast.ctype.typ == CTYPE_FLOAT {

	} else {
		emit_expr(ast.left)
		emit("push %%rax")
		emit_expr(ast.right)
		emit("pop %%rcx")
		emit("cmp %%rax, %%rcx")
		emit("%s %%al", inst)
		emit("movzb %%al, %%eax")
	}
}

func emit_bion_int_arith(ast *Ast) {
	var op string
	switch ast.typ {
	case '+':
		op = "add"
	case '-':
		op = "sub"
	case '*':
		op = "imul"
	case '/':
		break
	default:
		errorf("invalid operator '%d", ast.typ)
	}

	emit_expr(ast.left)
	emit_toint(ast.left.ctype)
	emit("push %%rax")
	emit_expr(ast.right)
	emit_toint(ast.right.ctype)
	emit("mov %%rax, %%rcx")
	emit("pop %%rax")
	if ast.typ == '/' {
		emit("mov $0, %%edx")
		emit("idiv %%rcx")
	} else {
		emit("%s %%rcx, %%rax", op)
	}
}

func emit_push_xmm(reg int) {
	emit("sub $8, %%rsp")
	emit("movss %%xmm%d, (%%rsp)", reg)
}

func emit_pop_xmm(reg int) {
	emit("movss (%%rsp), %%xmm%d", reg)
	emit("add $8, %%rsp")
}

func emit_binop_float_arith(ast *Ast) {
	var op string
	switch ast.typ {
	case '+':
		op = "addss"
	case '-':
		op = "subss"
	case '*':
	op = "muss"
	case '/':
		op = "divss"
	default :
		errorf("invalid operator '%d'", ast.typ)
	}
	 emit_expr(ast.left)
	 emit_tofloat(ast.left.ctype)
	 emit_push_xmm(0)
	 emit_expr(ast.right)
	 emit_tofloat(ast.right.ctype)
	 emit("movsd %%xmm0, %%xmm1")
	 emit_pop_xmm(0)
	 emit("%s %%xmm1, %%xmm0", op)
}

func emit_binop(ast *Ast) {
	if ast.typ == '=' {
		emit_expr(ast.right)
		if ast.ctype.typ == CTYPE_FLOAT {
			emit_tofloat(ast.right.ctype)
		} else {
			emit_toint(ast.right.ctype)
		}

		emit_assign(ast.left)
		return
	}
	if ast.typ == PUNCT_EQ {
		emit_comp("sete", ast)
		return
	}
	if ast.ctype.typ == CTYPE_PTR {
		emit_pointer_arith(byte(ast.typ), ast.left, ast.right)
		return
	}
	switch ast.typ {
	case '<':
		emit_comp("setl", ast)
		return
	case '>':
		emit_comp("setg", ast)
		return
	}

	if ast.ctype.typ == CTYPE_INT {
		emit_bion_int_arith(ast)
	} else if ast.ctype.typ == CTYPE_FLOAT {
		emit_binop_float_arith(ast)
	} else {
		errorf("internal error")
	}
}

func emit_inc_dec(ast *Ast, op string) {
	emit_expr(ast.operand)
	emit("push %%rax")
	emit("%s $1, %%rax", op)
	emit_assign(ast.operand)
	emit("pop %%rax")
}

func emit_load_deref(result_type *Ctype, operand_type *Ctype, off int) {
	if operand_type.typ == CTYPE_PTR &&
		operand_type.ptr.typ == CTYPE_ARRAY {
		return
	}
	if result_type.size == 1 {
		emit("mov $0, %%ecx")
	}
	reg := get_int_reg(result_type, 'c')
	if off != 0 {
		emit("mov %d(%%rax), %%%s", off, reg)
	} else {
		emit("mov (%%rax), %%%s", reg)
	}
	emit("mov %%rcx, %%rax")

}

func emit_expr(ast *Ast) {
	switch ast.typ {
	case AST_LITERAL:
		switch ast.ctype.typ {
		case CTYPE_INT:
			emit("mov $%d, %%eax", ast.ival)
		case CTYPE_CHAR:
			emit("mov $%d, %%rax", ast.c)
		case CTYPE_FLOAT:
			emit("movss %s(%%rip), %%xmm0", ast.flabel)
		default:
			errorf("internal error")
		}
	case AST_STRING:
		emit("lea %s(%%rip), %%rax", ast.slabel)
	case AST_LVAR:
		emit_lload(ast.ctype, ast.loff)
	case AST_GVAR:
		emit_gload(ast.ctype, ast.glabel, 0)
	case AST_FUNCALL:
		ireg := 0
		xreg := 0
		for _, v := range ast.args {
			if v.ctype.typ == CTYPE_FLOAT {
				emit_push_xmm(xreg)
				xreg++
			} else {
				emit("push %%%s", REGS[ireg])
				ireg++
			}
		}
		for _, v := range ast.args {
			emit_expr(v)
			if v.ctype.typ == CTYPE_FLOAT {
				emit_push_xmm(0)
			} else {
				emit("push %%rax")
			}
		}
		ir := ireg
		xr := xreg
		var reversed_args []*Ast
		for i:= len(ast.args) -1 ; i >= 0 ; i-- {
			reversed_args = append(reversed_args, ast.args[i])
		}
		for _,v := range reversed_args {
			if v.ctype.typ == CTYPE_FLOAT {
				xr--
				emit_pop_xmm(xr)
				emit("cvtps2pd %%xmm%d, %%xmm%d", xr, xr)
			} else {
				ir--
				emit("pop %%%s", REGS[ir])
			}
		}
		emit("mov $%d, %%eax", xreg)
		emit("call %s", ast.fname)
		for _,v := range reversed_args {
			if v.ctype.typ == CTYPE_FLOAT {
				xreg--
				emit_pop_xmm(xreg)
			} else {
				ireg--
				emit("pop %%%s", REGS[ireg])
			}
		}
	case AST_DECL:
		if ast.declinit == nil {
			return
		}
		if ast.declinit.typ == AST_ARRAY_INIT {
			off := 0
			for _, v := range ast.declinit.arrayinit {
				emit_expr(v)
				emit_lsave(ast.declvar.ctype.ptr, ast.declvar.loff+off)
				off += ast.declvar.ctype.ptr.size
			}
		} else if ast.declvar.ctype.typ == CTYPE_ARRAY {
			assert(ast.declinit.typ == AST_STRING)
			var i int
			for i, char := range ast.declinit.val {
				emit("movb $%d, %d(%%rbp)", char, ast.declvar.loff+i)
			}
			emit("movb $0, %d(%%rbp)", ast.declvar.loff+i)
		} else if ast.declinit.typ == AST_STRING {
			emit_gload(ast.declinit.ctype, ast.declinit.slabel, 0)
			emit_lsave(ast.declvar.ctype, ast.declvar.loff)
		} else {
			emit_expr(ast.declinit)
			emit_lsave(ast.declvar.ctype, ast.declvar.loff)
		}
	case AST_ADDR:
		switch ast.operand.typ {
		case AST_LVAR:
			emit("lea %d(%%rbp), %%rax", ast.operand.loff)
		case AST_GVAR:
			emit("lea %s(%%rip), %%rax", ast.operand.glabel)
		default:
			errorf("internal error")
		}
	case AST_DEREF:
		emit_expr(ast.operand)
		emit_load_deref(ast.ctype, ast.operand.ctype, 0)
	case AST_IF, AST_TERNARY:
		emit_expr(ast.cond)
		ne := make_label()
		emit("test %%rax, %%rax")
		emit("je %s", ne)
		emit_expr(ast.then)
		if ast.els != nil {
			end := make_label()
			emit("jmp %s", end)
			emit("%s:", ne)
			emit_expr(ast.els)
			emit("%s:", end)
		} else {
			emit("%s:", ne)
		}
	case AST_FOR:
		if ast.init != nil {
			emit_expr(ast.init)
		}
		begin := make_label()
		end := make_label()
		emit("%s:", begin)
		if ast.cond != nil {
			emit_expr(ast.cond)
			emit("test %%rax, %%rax")
			emit("je %s", end)
		}
		emit_expr(ast.body)
		if ast.step != nil {
			emit_expr(ast.step)
		}
		emit("jmp %s", begin)
		emit("%s:", end)
	case AST_RETURN:
		emit_expr(ast.retval)
		emit("leave")
		emit("ret")
		break
	case AST_COMPOUND_STMT:
		for _, v := range ast.stmts {
			emit_expr(v)
		}
	case AST_STRUCT_REF:
		emit_load_struct_ref(ast.struc, ast.field, 0)
	case PUNCT_INC:
		emit_inc_dec(ast, "add")
	case PUNCT_DEC:
		emit_inc_dec(ast, "sub")
	case '!':
		emit_expr(ast.operand)
		emit("cmp $0, %%rax")
		emit("sete %%al")
		emit("movzb %%al, %%eax")
	case '&':
		emit_expr(ast.left)
		emit("push %%rax")
		emit_expr(ast.right)
		emit("pop %%rcx")
		emit("and %%rcx, %%rax")
	case '|':
		emit_expr(ast.left)
		emit("push %%rax")
		emit_expr(ast.right)
		emit("pop %%rcx")
		emit("or %%rcx, %%rax")
	case PUNCT_LOGAND:
		end := make_label()
		emit_expr(ast.left)
		emit("test %%rax, %%rax")
		emit("mov $0, %%rax")
		emit("je %s", end)
		emit_expr(ast.right)
		emit("test %%rax, %%rax")
		emit("mov $0, %%rax")
		emit("je %s", end)
		emit("mov $1, %%rax")
		emit("%s:", end)
	case PUNCT_LOGOR:
		end := make_label()
		emit_expr(ast.left)
		emit("test %%rax, %%rax")
		emit("mov $1, %%rax")
		emit("jne %s", end)
		emit_expr(ast.right)
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
			emit("%s:", v.slabel)
			emit(".string \"%s\"", quote_cstring(v.val))
		} else if v.typ != AST_GVAR {
			errorf("internal error: %s", v)
		}
	}
	for _,v := range floats {
		label := make_label()
		v.flabel = label
		emit_label("%s:", label)
		long := *(* int)(unsafe.Pointer(&v.fval))
		emit(".long %d", long)
	}
}

func ceil(n int, m int) int {
	rem := n % m
	if rem == 0 {
		return n
	} else {
		return n - rem + m
	}
}

func emit_func_prologue(fn *Ast) {
	if len(fn.params) > len(REGS) {
		errorf("Parameter list too long: %s", fn.fname)
	}
	emit(".text")
	emit(".global %s\n", fn.fname)
	emit("%s:", fn.fname)
	emit("push %%rbp")
	emit("mov %%rsp, %%rbp")
	off := 0
	ri := 0
	for _, v := range fn.params {
		emit("push %%%s", REGS[ri])
		ri++
		off -= ceil(v.ctype.size, 8)
		v.loff = off
	}
	for _, v := range fn.localvars {
		off -= ceil(v.ctype.size, 8)
		v.loff = off
	}
	if off != 0 {
		emit("sub $%d, %%rsp", ceil(-off, 16))
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
	switch data.ctype.size {
	case 1:
		emit(".byte %d", data.ival)
	case 4:
		emit(".long %d", data.ival)
	case 8:
		emit(".quad %d", data.ival)
	default:
		errorf("internal error")
	}
}

func emit_data(v *Ast) {
	emit_label(".global %s", v.declvar.varname)
	emit_label("%s:", v.declvar.varname)
	if v.declinit.typ == AST_ARRAY_INIT {
		for _, v := range v.declinit.arrayinit {
			emit_data_int(v)
		}
		return
	}
	assert(v.declinit.typ == AST_LITERAL && v.declinit.ctype.typ == CTYPE_INT)
	emit_data_int(v.declinit)
}

func emit_bss(v *Ast) {
	emit(".lcomm %s, %d", v.declvar.varname, v.declvar.ctype.size)
}

func emit_global_var(v *Ast) {
	if v.declinit != nil {
		emit_data(v)
	} else {
		emit_bss(v)
	}
}

func emit_toplevel(v *Ast) {
	if v.typ == AST_FUNC {
		emit_func_prologue(v)
		emit_expr(v.body)
		emit_func_epilogue()
	} else if v.typ == AST_DECL {
		emit_global_var(v)
	} else {
		errorf("internal error")
	}
}
