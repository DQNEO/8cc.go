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
			printf("%s", ast_to_string(fnc))
		} else {
			emit_func(fnc)
		}
	}

	return
}
