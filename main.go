package main

import (
	"os"
)

const EXPR_LEN = 50

func main() {
	initStdin()
	wantast := (len(os.Args) > 1 && os.Args[1] == "-a")
	var exprs []*Ast
	exprs = make([]*Ast, EXPR_LEN)
	var i int
	for i = 0; i < EXPR_LEN; i++ {
		t := read_decl_or_stmt()
		if t == nil {
			break
		}
		exprs[i] = t
	}
	exprs[i] = nil;
	if wantast {
		printf("{")
		for i = 0; exprs[i] != nil; i++ {
			printf("%s", ast_to_string(exprs[i]))
			printf(";")
		}
		printf("}")
	} else {
		print_asm_header()
		for i = 0; exprs[i] != nil; i++ {
			emit_expr(exprs[i])
		}
		printf("leave\n\t" +
			"ret\n")
	}

	return
}
