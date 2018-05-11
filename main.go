package main

import (
	"os"
)

const EXPR_LEN = 100

func main() {
	initStdin()
	wantast := (len(os.Args) > 1 && os.Args[1] == "-a")
	var exprs [EXPR_LEN]*Ast
	var i int
	for i = 0; i < EXPR_LEN; i++ {
		t := read_decl_or_stmt()
		if t == nil {
			break
		}
		exprs[i] = t
	}
	nexpr := i
	if wantast {
		printf("{")
		for i = 0; i < nexpr; i++ {
			printf("%s", ast_to_string(exprs[i]))
			printf(";")
		}
		printf("}")
	} else {
		print_asm_header()
		for i = 0; i < nexpr; i++ {
			emit_expr(exprs[i])
		}
		printf("leave\n\t" +
			"ret\n")
	}

	return
}
