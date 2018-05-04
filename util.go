package main

import (
	"fmt"
	"os"
)

func _error(format string, args ...interface{}) {
	panic(fmt.Sprintf(format, args...))
	os.Exit(1)
}

