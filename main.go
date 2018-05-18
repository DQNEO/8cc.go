package main

import (
	"os"
)

func ast_func(body []*Ast) *Ast {
	r := &Ast{}
	r.funcall.fname = []byte("mymain\x00")
	r.funcall.locals = locals
	r.funcall.body = body
	return r
}

func main() {
	initStdin()
	wantast := len(os.Args) > 1 && os.Args[1] == "-a"
	block := read_block()

	r := ast_func(block)

	if wantast {
		printf("%s", block_to_string(r.funcall.body))
	} else {
		emit_data_section()
		emit_func(r)
	}

	return
}
