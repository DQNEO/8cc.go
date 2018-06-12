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

	for _, v := range funcs {
		if wantast {
			printf("%s", v)
		} else {
			emit_toplevel(v)
		}
	}

	return
}
