#include "8cc.h"

void unget_token(Token *tok) {
    unget_cpp_token(tok);
}

Token *peek_token(void) {
    Token *r = read_token();
    unget_token(r);
    return r;
}

Token *read_token(void) {
    Token *tok = read_cpp_token();
    return tok;
}
