package main

func assert(expr bool) {
	if !expr {
		_error("Assertion failed.s")
	}
}
