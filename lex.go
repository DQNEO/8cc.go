package main

func read_c() (byte, error) {
	return getc(stdin)
}

func unget_c(c byte) {
	ungetc(c, stdin)
}

func skip_space() {
	for {
		c, err := getc(stdin)
		if err != nil {
			break
		}
		if isspace(c) {
			continue
		}
		ungetc(c, stdin)
		return
	}
}
