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
	if !wantast {
		print_asm_header()
	}
	for i = 0; i < nexpr; i++ {
		if wantast {
			printf("%s", ast_to_string(exprs[i]))
		} else {
			emit_expr(exprs[i])
		}
	}

	if !wantast {
		printf("leave\n\t" +
			"ret\n")
	}
	return
}
