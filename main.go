package main

import (
	"os"
)

func main() {
	initStdin()
	initLex()
	wantast := len(os.Args) > 1 && os.Args[1] == "-a"
	toplevels := read_toplevels()

	if !wantast {
		emit_data_section()
	}

	for _, v := range toplevels {
		if wantast {
			printf("%s", v)
		} else {
			emit_toplevel(v)
		}
	}

	return
}
