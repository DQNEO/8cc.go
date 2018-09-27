package main

func unget_token(tok *Token) {
	unget_cpp_token(tok);
}

func peek_token() *Token {
	return peek_cpp_token()
}

func read_token() *Token {
	return read_cpp_token()
}
