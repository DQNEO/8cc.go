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

func read_func_list() []*Ast {
	block := read_block()
	r := ast_func(block)
	func_list := []*Ast{}
	func_list = append(func_list, r)
	return func_list
}

func main() {
	initStdin()
	wantast := len(os.Args) > 1 && os.Args[1] == "-a"
	func_list := read_func_list()

	if !wantast {
		emit_data_section()
	}

	for _, fnc := range func_list {
		if wantast {
			printf("%s", block_to_string(fnc.funcall.body))
		} else {
			emit_func(fnc)
		}
	}

	return
}
