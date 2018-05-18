package main

import (
	"os"
)

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
