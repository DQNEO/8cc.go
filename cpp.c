#include "8cc.h"

static List *buffer = &EMPTY_LIST;
static bool bol = true;


static void read_define(void) {
    for (;;) {
        Token *tok = read_cpp_token();
        if (tok->type == TTYPE_NEWLINE)
            return;
    }
}

static void read_directive(void) {
    Token *tok = read_cpp_token();
    if (is_ident(tok, "define"))
        read_define();
    else
        error("unsupported preprocessor directive: %s", token_to_string(tok));
}

void unget_token(Token *tok) {
    list_push(buffer, tok);
}

Token *peek_token(void) {
    Token *r = read_token();
    unget_token(r);
    return r;
}

Token *read_token(void) {
    for (;;) {
        Token *tok = (list_len(buffer) > 0) ? list_pop(buffer) : read_cpp_token();
        if (!tok)
            return NULL;
        if (tok && tok->type == TTYPE_NEWLINE) {
            bol = true;
            continue;
        }
        if (bol && is_punct(tok, '#')) {
            read_directive();
            bol = true;
            continue;
        }
        bol = false;
        return tok;
    }

}
