package main

var REGS = []string{"rdi", "rsi", "rdx", "rcx", "r8", "r9"}

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

func emit_gload(ctype *Ctype, label []byte, off int) {
	if ctype.typ == CTYPE_ARRAY {
		printf("lea %s(%%rip), %%rax\n\t", bytes2string(label))
		return
	}
	var reg string
	size := ctype_size(ctype)
	switch size {
	case 1:
		reg = "al"
		printf("mov $0, %%eax\n\t")
	case 4:
		reg = "eax"
	case 8:
		reg = "rax"
	default:
		_error("Unknown data size: %s: %d", ctype_to_string(ctype), size)
	}

	printf("mov %s(%%rip), %%%s\n\t", bytes2string(label), reg)
	if off > 0 {
		printf("add $%d, %%rax\n\t", off*size)
	}
	printf("mov (%%rax), %%%s\n\t", reg)
}

func emit_lload(v *Ast, off int) {
	if v.ctype.typ == CTYPE_ARRAY {
		printf("lea %d(%%rbp), %%rax\n\t", -v.variable.loff)
		return
	}
	size := ctype_size(v.ctype)
	switch size {
	case 1:
		printf("mov $0, %%eax\n\t")
		printf("mov %d(%%rbp), %%al\n\t", -v.variable.loff)
	case 4:
		printf("mov %d(%%rbp), %%eax\n\t", -v.variable.loff)
	case 8:
		printf("mov %d(%%rbp), %%rax\n\t", -v.variable.loff)
	default:
		_error("Unknown data size: %s: %d", ast_to_string(v), size)
	}
	if off > 0 {
		printf("add $%d, %%rax\n\t", size, off*size)
	}
}

func emit_gsave(v *Ast, off int) {
	assert(v.ctype.typ != CTYPE_ARRAY)
	var reg string
	printf("push %%rcx\n\t")
	printf("mov %s(%%rip) %%rcx\n\t", v.gvar.glabel)

	size := ctype_size(v.ctype)
	switch size {
	case 1:
		reg = "al"
	case 4:
		reg = "eax"
	case 8:
		reg = "rax"
	default:
		_error("Unknown data size: %s: %d", ast_to_string(v), size)
	}
	printf("mov %s, %d(%%rbp)\n\t", reg, off*size)
	printf("pop %%rcx\n\t")
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
	printf("mov %%%s, %d(%%rbp)\n\t", reg, -(loff+off*size))
}

func emit_pointer_arith(_ byte, left *Ast, right *Ast) {
	assert(left.ctype.typ == CTYPE_PTR)
	emit_expr(left)
	printf("push %%rax\n\t")
	emit_expr(right)
	size := ctype_size(left.ctype.ptr)
	if size > 1 {
		printf("imul $%d, %%rax\n\t", size)
	}
	printf("mov %%rax, %%rcx\n\t" +
		"pop %%rax\n\t" +
		"add %%rcx, %%rax\n\t")
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
	case AST_GREF:
		emit_gsave(variable.gref.ref, variable.gref.off)
	default:
		_error("internal error")
	}
	printf("mov %%rax, %d(%%rbp)\n\t", -variable.variable.loff)
}

func emit_binop(ast *Ast) {
	if ast.typ == '=' {
		emit_assign(ast.binop.left, ast.binop.right)
		return
	}

	if ast.ctype.typ == CTYPE_PTR {
		emit_pointer_arith(ast.typ, ast.binop.left, ast.binop.right)
		return
	}
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
		_error("invalid operator '%c", ast.typ)
	}

	emit_expr(ast.binop.left)
	printf("push %%rax\n\t")
	emit_expr(ast.binop.right)
	if ast.typ == '/' {
		printf("mov %%rax, %%rcx\n\t")
		printf("pop %%rax\n\t")
		printf("mov $0, %%edx\n\t")
		printf("idiv %%rcx\n\t")
	} else {
		printf("pop %%rcx\n\t")
		printf("%s %%rcx, %%rax\n\t", op)
	}
}

func emit_expr(ast *Ast) {
	switch ast.typ {
	case AST_LITERAL:
		switch ast.ctype.typ {
		case CTYPE_INT:
			printf("mov $%d, %%eax\n\t", ast.ival)
		case CTYPE_CHAR:
			printf("mov $%d, %%rax\n\t", ast.c)
		default:
			_error("internal error")
		}
	case AST_STRING:
		printf("lea %s(%%rip), %%rax\n\t", bytes2string(ast.str.slabel))
	case AST_LVAR:
		emit_lload(ast, 0)
	case AST_LREF:
		assert(ast.lref.ref.typ == AST_LVAR)
		emit_lload(ast.lref.ref, ast.lref.off)
	case AST_GVAR:
		emit_gload(ast.ctype, ast.gvar.glabel, 0)
	case AST_GREF:
		if ast.gref.ref.typ == AST_STRING {
			printf("lea %s(%%rip), %%rax\n\t", bytes2string(ast.gref.ref.str.slabel))
		} else {
			assert(ast.gref.ref.typ == AST_GVAR)
			emit_gload(ast.gref.ref.ctype, ast.gref.ref.gvar.glabel, ast.gref.off)
		}
	case AST_FUNCALL:
		for i := 1; i < len(ast.funcall.args); i++ {
			printf("push %%%s\n\t", REGS[i])
		}
		for _, v := range ast.funcall.args {
			emit_expr(v)
			printf("push %%rax\n\t")
		}
		for i := len(ast.funcall.args) - 1; i >= 0; i-- {
			printf("pop %%%s\n\t", REGS[i])
		}
		printf("mov $0, %%eax\n\t")
		printf("call %s\n\t", bytes2string(ast.funcall.fname))
		for i := len(ast.funcall.args); i >= 0; i-- {
			printf("pop %%%s\n\t", REGS[i])
		}
	case AST_DECL:
		if ast.decl.declinit.typ == AST_ARRAY_INIT {
			for i,v := range  ast.decl.declinit.array_initializer.arrayinit {
				emit_expr(v)
				emit_lsave(ast.decl.declvar.ctype.ptr, ast.decl.declvar.variable.loff, -i)
			}
		} else if ast.decl.declvar.ctype.typ == CTYPE_ARRAY {
			assert(ast.decl.declinit.typ == AST_STRING)
			var i int
			for i = 0; ast.decl.declinit.str.val[i] != 0; i++ {
				printf("movb $%d, %d(%%rbp)\n\t", ast.decl.declinit.str.val[i], -(ast.decl.declvar.variable.loff-i))
			}
			printf("movb $0, %d(%%rbp)\n\t", -(ast.decl.declvar.variable.loff-i))
		} else if ast.decl.declinit.typ == AST_STRING {
			emit_gload(ast.decl.declinit.ctype, ast.decl.declinit.str.slabel, 0)
			emit_lsave(ast.decl.declvar.ctype, ast.decl.declvar.variable.loff, 0)
		} else {
			emit_expr(ast.decl.declinit)
			emit_lsave(ast.decl.declvar.ctype, ast.decl.declvar.variable.loff, 0)
		}
	case AST_ADDR:
		assert(ast.unary.operand.typ == AST_LVAR)
		printf("lea %d(%%rbp), %%rax\n\t", -ast.unary.operand.variable.loff)
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
		printf("mov $0, %%ecx\n\t")
		printf("mov (%%rax), %s\n\t", reg)
		printf("mov %%rcx, %%rax\n\t")
	case AST_IF:
		emit_expr(ast._if.cond)
		l1 := make_label()
		printf("test %%rax, %%rax\n\t")
		printf("je %s\n\t", bytes2string(l1))
		emit_block(ast._if.then)
		if ast._if.els != nil {
			l2 := make_label()
			printf("jmp %s\n\t", bytes2string(l2))
			printf("%s:\n\t", bytes2string(l1))
			emit_block(ast._if.els)
			printf("%s:\n\t", bytes2string(l2))
		} else {
			printf("%s:\n\t", bytes2string(l1))
		}
	default:
		emit_binop(ast)
	}
}

func emit_data_section() {
	if globals == nil {
		return
	}
	printf("\t.data\n")
	for _,v := range globals {
		assert(v.typ == AST_STRING)
		printf("%s:\n\t", bytes2string(v.str.slabel))
		printf(".string \"%s\"\n", quote_cstring(v.str.val))
	}
	printf("\t")

}

func ceil8(n int) int {
	rem := n % 8
	if rem == 0 {
		return n
	} else {
		return n - rem + 8
	}
}

func print_asm_header() {
	off := 0
	for _, v := range locals {
		off += ceil8(ctype_size(v.ctype))
		v.variable.loff = off
	}
	emit_data_section()
	printf(".text\n\t" +
		".global mymain\n" +
		"mymain:\n\t" +
		"push %%rbp\n\t" +
		"mov %%rsp, %%rbp\n\t")
	if off > 0 {
		printf("sub $%d, %%rsp\n\t", off)
	}
}

func emit_block(block []*Ast) {
	for _,v := range block {
		emit_expr(v)
	}

	printf("leave\n\t" +
		"ret\n")
}
