#include "8cc.h"

void unget_token(Token *tok) {
    unget_cpp_token(tok);
}

Token *peek_token(void) {
    return peek_cpp_token();
}

Token *read_token(void) {
    return read_cpp_token();
}
