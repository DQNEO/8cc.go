package main

import (
	"os"
)

func main() {
	initStdin()
	wantast := len(os.Args) > 1 && os.Args[1] == "-a"
	funcs := read_func_list()

	if !wantast {
		emit_data_section()
	}

	for _, fnc := range funcs {
		if wantast {
			printf("%s", fnc)
		} else {
			emit_func(fnc)
		}
	}

	return
}
