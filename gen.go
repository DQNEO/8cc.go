package main

import "unsafe"
import "runtime"
import "fmt"
import "strings"

var REGS = []string{"rdi", "rsi", "rdx", "rcx", "r8", "r9"}

var stackpos int

func emit(format string, args ...interface{}) {
	emit_int("\t"+format, args...)
}
func emit_label(fmt string, args ...interface{}) {
	emit_int(fmt, args...)
}

func emit_int(format string, args ...interface{}) {
	code := fmt.Sprintf(format, args...)
	pc, _, no, ok := runtime.Caller(3)
	if !ok {
		errorf("Unable to get caller")
	}
	details := runtime.FuncForPC(pc)
	callerName := (strings.Split(details.Name(), "."))[1]
	caller := fmt.Sprintf(" %s %d", callerName, no)
	numSpaces := 27 - len(code)
	printf("%s %*c %s\n", code, numSpaces, '#', caller)
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
	case 2:
		if r == 'a' {
			return "ax"
		} else {
			return "cx"
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
		errorf("Unknown data size: %s: %d", ctype, ctype.size)
	}
	return ""
}

func push_xmm(reg int) {
	emit("sub $8, %%rsp")
	emit("movsd %%xmm%d, (%%rsp)", reg)
	stackpos += 8
}

func pop_xmm(reg int) {
	emit("movsd (%%rsp), %%xmm%d", reg)
	emit("add $8, %%rsp")
	stackpos -= 8
	assert(stackpos >= 8)
}

func push(reg string) {
	emit("push %%%s", reg)
	stackpos += 8
}

func pop(reg string) {
	emit("pop %%%s", reg)
	stackpos -= 8
	assert(stackpos >= 8)
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
	if ctype.size < 4 {
		emit("mov $0, %%eax")
	}
	if off != 0 {
		emit("mov %s+%d(%%rip), %%%s", label, off, reg)
	} else {
		emit("mov %s(%%rip), %%%s", label, reg)
	}
}

func emit_toint(ctype *Ctype) {
	if !is_flotype(ctype) {
		return
	}
	emit("cvttsd2si %%xmm0, %%eax")
}

func emit_todouble(ctype *Ctype) {
	if is_flotype(ctype) {
		return
	}
	emit("cvtsi2sd %%eax, %%xmm0")
}

func emit_lload(ctype *Ctype, off int) {
	if ctype.typ == CTYPE_ARRAY {
		emit("lea %d(%%rbp), %%rax", off)
	} else if ctype.typ == CTYPE_FLOAT {
		emit("cvtps2pd %d(%%rbp), %%xmm0", off)
	} else if ctype.typ == CTYPE_DOUBLE || ctype.typ == CTYPE_LDOUBLE {
		emit("movsd %d(%%rbp), %%xmm0", off)
	} else {
		reg := get_int_reg(ctype, 'a')
		if ctype.size < 4 {
			emit("mov $0, %%eax")
		}
		emit("mov %d(%%rbp), %%%s", off, reg)
	}
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
		push_xmm(0)
		emit("cvtpd2ps %%xmm0, %%xmm0")
		emit("movss %%xmm0, %d(%%rbp)", off)
		pop_xmm(0)
	} else if ctype.typ == CTYPE_DOUBLE || ctype.typ == CTYPE_LDOUBLE {
		emit("movsd %%xmm0, %d(%%rbp)", off)
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
	pop("rax")
}

func emit_assign_deref(variable *Ast) {
	push("rax")
	emit_expr(variable.operand)
	emit_assign_deref_int(variable.operand.ctype.ptr, 0)
}

func emit_pointer_arith(_ byte, left *Ast, right *Ast) {
	emit_expr(left)
	push("rax")
	emit_expr(right)
	size := left.ctype.ptr.size
	if size > 1 {
		emit("imul $%d, %%rax", size)
	}
	emit("mov %%rax, %%rcx")
	pop("rax")
	emit("add %%rcx, %%rax")
}

func emit_assign_struct_ref(struc *Ast, field *Ctype, off int) {
	switch struc.typ {
	case AST_LVAR:
		emit_lsave(field, struc.loff+field.offset+off)
	case AST_GVAR:
		emit_gsave(struc.varname, field, field.offset+off)
	case AST_STRUCT_REF:
		emit_assign_struct_ref(struc.struc, field, off+struc.ctype.offset)
	case AST_DEREF:
		v := struc
		push("rax")
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
		emit_load_struct_ref(struc.struc, field, struc.ctype.offset+off)
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
		emit_assign_struct_ref(variable.struc, variable.ctype, 0)
	case AST_LVAR:
		emit_lsave(variable.ctype, variable.loff)
	case AST_GVAR:
		emit_gsave(variable.varname, variable.ctype, 0)
	default:
		errorf("internal error")
	}
}

func emit_comp(inst string, ast *Ast) {
	if is_flotype(ast.ctype) {
		emit_expr(ast.left)
		emit_todouble(ast.left.ctype)
		push_xmm(0)
		emit_expr(ast.right)
		emit_todouble(ast.right.ctype)
		pop_xmm(1)
		emit("ucomisd %%xmm0, %%xmm1")
	} else {
		emit_expr(ast.left)
		emit_toint(ast.left.ctype)
		push("rax")
		emit_expr(ast.right)
		emit_toint(ast.right.ctype)
		pop("rcx")
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
	push("rax")
	emit_expr(ast.right)
	emit_toint(ast.right.ctype)
	emit("mov %%rax, %%rcx")
	pop("rax")
	if ast.typ == '/' {
		emit("mov $0, %%edx")
		emit("idiv %%rcx")
	} else {
		emit("%s %%rcx, %%rax", op)
	}
}

func emit_binop_float_arith(ast *Ast) {
	var op string
	switch ast.typ {
	case '+':
		op = "addsd"
	case '-':
		op = "subsd"
	case '*':
		op = "mulsd"
	case '/':
		op = "divsd"
	default:
		errorf("invalid operator '%d'", ast.typ)
	}
	emit_expr(ast.left)
	emit_todouble(ast.left.ctype)
	push_xmm(0)
	emit_expr(ast.right)
	emit_todouble(ast.right.ctype)
	emit("movsd %%xmm0, %%xmm1")
	pop_xmm(0)
	emit("%s %%xmm1, %%xmm0", op)
}

func emit_load_convert(to *Ctype, from *Ctype) {
	if is_flotype(to) {
		emit_todouble(from)
	} else {
		emit_toint(from)
	}
}

func emit_save_convert(to *Ctype, from *Ctype) {
	if is_inttype(from) && to.typ == CTYPE_FLOAT {
		emit("cvtsi2ss %%eax, %%xmm0")
	} else if is_flotype(from) && to.typ == CTYPE_FLOAT {
		emit("cvtpd2ps %%xmm0, %%xmm0")
	} else if is_inttype(from) && (to.typ == CTYPE_DOUBLE || to.typ == CTYPE_LDOUBLE) {
		emit("cvtsi2sd %%eax, %%xmm0")
	} else if !(is_flotype(from) && (to.typ == CTYPE_DOUBLE || to.typ == CTYPE_LDOUBLE)) {
		emit_load_convert(to, from)
	}
}

func emit_binop(ast *Ast) {
	if ast.typ == '=' {
		emit_expr(ast.right)
		emit_load_convert(ast.ctype, ast.right.ctype)
		emit_assign(ast.left)
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
	case OP_EQ:
		emit_comp("sete", ast)
		return
	case OP_GE:
		emit_comp("setge", ast)
		return
	case OP_LE:
		emit_comp("setle", ast)
		return
	case OP_NE:
		emit_comp("setne", ast)
		return
	}

	if is_inttype(ast.ctype) {
		emit_bion_int_arith(ast)
	} else if is_flotype(ast.ctype) {
		emit_binop_float_arith(ast)
	} else {
		errorf("internal error")
	}
}

func emit_inc_dec(ast *Ast, op string) {
	emit_expr(ast.operand)
	push("rax")
	emit("%s $1, %%rax", op)
	emit_assign(ast.operand)
	pop("rax")
}

func emit_load_deref(result_type *Ctype, operand_type *Ctype, off int) {
	if operand_type.typ == CTYPE_PTR &&
		operand_type.ptr.typ == CTYPE_ARRAY {
		return
	}
	if result_type.size < 4 {
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

func get_arg_types(ast *Ast) []*Ctype {
	var r []*Ctype
	for i, v := range ast.args {
		var ptype *Ctype
		if len(ast.paramtypes) > i {
			ptype = ast.paramtypes[i]
		} else {
			ptype = result_type('=', v.ctype, ctype_int)
		}
		r = append(r, ptype)
	}
	return r
}
func emit_expr(ast *Ast) {
	switch ast.typ {
	case AST_LITERAL:
		switch ast.ctype.typ {
		case CTYPE_CHAR:
			emit("mov $%d, %%rax", ast.ival)
		case CTYPE_INT:
			emit("mov $%d, %%eax", ast.ival)
		case CTYPE_LONG:
			emit("mov $%d, %%rax", ast.ival)
		case CTYPE_FLOAT, CTYPE_DOUBLE, CTYPE_LDOUBLE:
			emit("movsd %s(%%rip), %%xmm0", ast.flabel)
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
		argtypes := get_arg_types(ast)
		for _, v := range argtypes {
			if is_flotype(v) {
				if xreg > 0 {
					push_xmm(xreg)
				}
				xreg++
			} else {
				push(REGS[ireg])
				ireg++
			}
		}
		for i, v := range ast.args {
			emit_expr(v)
			ptype := argtypes[i]
			emit_save_convert(ptype, v.ctype)
			if is_flotype(ptype) {
				push_xmm(0)
			} else {
				push("rax")
			}
		}
		ir := ireg
		xr := xreg
		var reversed_args []*Ctype
		for i := len(argtypes) - 1; i >= 0; i-- {
			reversed_args = append(reversed_args, argtypes[i])
		}
		for _, v := range reversed_args {
			if is_flotype(v) {
				xr--
				pop_xmm(xr)
			} else {
				ir--
				pop(REGS[ir])
			}
		}
		emit("mov $%d, %%eax", xreg)
		if stackpos%16 != 0 {
			emit("sub $8, %%rsp")
		}
		emit("call %s", ast.fname)
		if stackpos%16 != 0 {
			emit("add $8, %%rsp")
		}
		for _, v := range reversed_args {
			if is_flotype(v) {
				if xreg != 1 {
					xreg--
					pop_xmm(xreg)
				}
			} else {
				ireg--
				pop(REGS[ireg])
			}
		}
		if ast.ctype.typ == CTYPE_FLOAT {
			emit("cvtps2pd %%xmm0, %%xmm0")
		}
	case AST_DECL:
		if ast.declinit == nil {
			return
		}
		if ast.declinit.typ == AST_INIT_LIST {
			off := 0
			for _, v := range ast.declinit.initlist {
				elem := v
				emit_expr(elem)
				emit_lsave(elem.totype, ast.declvar.loff+off)
				off += elem.totype.size
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
		if ast.retval != nil {
			emit_expr(ast.retval)
			emit_save_convert(ast.ctype, ast.retval.ctype)
		}
		emit("leave")
		emit("ret")
		break
	case AST_COMPOUND_STMT:
		for _, v := range ast.stmts {
			emit_expr(v)
		}
	case AST_STRUCT_REF:
		emit_load_struct_ref(ast.struc, ast.ctype, 0)
	case OP_INC:
		emit_inc_dec(ast, "add")
	case OP_DEC:
		emit_inc_dec(ast, "sub")
	case '!':
		emit_expr(ast.operand)
		emit("cmp $0, %%rax")
		emit("sete %%al")
		emit("movzb %%al, %%eax")
	case '&':
		emit_expr(ast.left)
		push("rax")
		emit_expr(ast.right)
		pop("rcx")
		emit("and %%rcx, %%rax")
	case '|':
		emit_expr(ast.left)
		push("rax")
		emit_expr(ast.right)
		pop("rcx")
		emit("or %%rcx, %%rax")
	case OP_LOGAND:
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
	case OP_LOGOR:
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
	emit(".data")
	for _, v := range gstrings {
		emit_label("%s:", v.slabel)
		emit(".string \"%s\"", quote_cstring(v.val))
	}
	for _, v := range flonums {
		label := make_label()
		v.flabel = label
		emit_label("%s:", label)

		up1 := unsafe.Pointer(&v.fval)
		up2 := unsafe.Pointer(uintptr(up1) + 4) // 4 means the size of int32
		i1 := *(*int32)(up1)
		i2 := *(*int32)(up2)
		emit(".long %d", i1)
		emit(".long %d", i2)
	}
}

func align(n int, m int) int {
	rem := n % m
	if rem == 0 {
		return n
	} else {
		return n - rem + m
	}
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
	if v.declinit.typ == AST_INIT_LIST {
		for _, v := range v.declinit.initlist {
			emit_data_int(v)
		}
		return
	}
	assert(v.declinit.typ == AST_LITERAL && is_inttype(v.declinit.ctype))
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

func emit_func_prologue(fn *Ast) {
	emit(".text")
	emit_label(".global %s\n", fn.fname)
	emit_label("%s:", fn.fname)
	push("rbp")
	emit("mov %%rsp, %%rbp")
	off := 0
	ireg := 0
	xreg := 0
	for _, v := range fn.params {
		if is_flotype(v.ctype) {
			push_xmm(xreg)
			xreg++
		} else if v.ctype.typ == CTYPE_DOUBLE || v.ctype.typ == CTYPE_LDOUBLE {
			push_xmm(xreg)
			xreg++
		} else {
			push(REGS[ireg])
			ireg++
		}

		off -= align(v.ctype.size, 8)
		v.loff = off
	}
	localarea := 0
	for _, v := range fn.localvars {
		a := align(v.ctype.size, 8)
		off -= a
		v.loff = off
		localarea -= a
	}
	if localarea != 0 {
		emit("sub $%d, %%rsp", -localarea)
	}
	stackpos += -(off - 8)
}

func emit_func_epilogue() {
	emit("leave")
	emit("ret")
}

func emit_toplevel(v *Ast) {
	stackpos = 0
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
