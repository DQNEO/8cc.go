package main

import "fmt"

func format(format string, a ... interface{}) string {
	return fmt.Sprintf(format, a...)
}
