package main

import (
	"os"
)

const EXPR_LEN = 50




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
