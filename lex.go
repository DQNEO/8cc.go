package main

type char struct {
	c byte
}

func read_ch() *char {
	c,err := getc(stdin)
	if err != nil {
		return nil
	}
	return &char{c:c}
}

func unget_ch(c *char) {
	ungetc(c.c, stdin)
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
