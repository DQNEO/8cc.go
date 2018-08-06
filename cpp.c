#include "8cc.h"

static List *buffer = &EMPTY_LIST;

void unget_token(Token *tok) {
    list_push(buffer, tok);
}

Token *peek_token(void) {
    Token *r = read_token();
    unget_token(r);
    return r;
}

Token *read_token(void) {
    Token *tok = (list_len(buffer) > 0) ? list_pop(buffer) : read_cpp_token();
    return tok;
}
