package main

import (
	"fmt"
	"os"
)

func _error(format string, args ...interface{}) {
	panic(fmt.Sprintf(format, args...))
	os.Exit(1)
}

func warn(format string, args ...interface{}) {
	fmt.Fprint(os.Stderr, "warning: ")
	fmt.Fprintf(os.Stderr, format,args...)
}
