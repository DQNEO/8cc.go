package main

import (
	"os"
)

const EXPR_LEN = 50

func read_block() []*Ast {
	var block []*Ast
	block = make([]*Ast, EXPR_LEN)
	var i int
	for i = 0; i < EXPR_LEN; i++ {
		t := read_decl_or_stmt()
		if t == nil {
			break
		}
		block[i] = t
	}
	block[i] = nil;
	return block
}

func block_to_string(block []*Ast) string {
	s := "{"
	for i := 0; block[i] != nil; i++ {
		s += ast_to_string(block[i])
		s += ";"
	}
	s += "}"
	return s
}

func emit_block(block []*Ast) {
	for i := 0; block[i] != nil; i++ {
		emit_expr(block[i])
	}
}

func main() {
	initStdin()
	wantast := (len(os.Args) > 1 && os.Args[1] == "-a")
	block := read_block()
	if wantast {
		printf("%s", block_to_string(block))
	} else {
		print_asm_header()
		emit_block(block)
		printf("leave\n\t" +
			"ret\n")
	}

	return
}
