#include "8cc.h"

static Dict *macros = &EMPTY_DICT;
static List *buffer = &EMPTY_LIST;
static List *altbuffer = NULL;
static bool bol = true;
static bool wastrue = false;

static Token *read_token_int(Dict *hideset, bool return_at_eol);

static Token *read_ident(void) {
    Token *r = read_cpp_token();
    if (r->type != TTYPE_IDENT)
        error("identifier expected, but got %s", token_to_string(r));
    return r;
}

static void expect_newline(void) {
    Token *tok = read_cpp_token();
    if (!tok || tok->type != TTYPE_NEWLINE)
        error("Newline expected, but got %s", token_to_string(tok));
}

static Token *expand(Dict *hideset, Token *tok) {
    if (tok->type != TTYPE_IDENT)
        return tok;
    if (dict_get(hideset, tok->sval))
        return tok;
    List *body = dict_get(macros, tok->sval);
    if (!body)
        return tok;
    dict_put(hideset, tok->sval, (void *)1);
    list_append(buffer, body);
    return read_token_int(hideset, false);
}

static void read_define(void) {
    Token *name = read_ident();
    List *body = make_list();
    for (;;) {
        Token *tok = read_cpp_token();
        if (!tok || tok->type == TTYPE_NEWLINE)
            break;
        list_push(body, tok);
    }
    dict_put(macros, name->sval, body);
}

static void read_undef(void) {
    Token *name = read_ident();
    expect_newline();
    dict_remove(macros, name->sval);
}

static List *read_line(void) {
    List *r = make_list();
    for (;;) {
        Token *tok = read_token_int(&EMPTY_DICT, true);
        if (!tok) return r;
        list_push(r, tok);
    }
}

static bool read_constexpr(void) {
    altbuffer = list_reverse(read_line());
    Ast *expr = read_expr();
    if (list_len(altbuffer) > 0)
        error("Stray token: %s", token_to_string(list_shift(altbuffer)));
    altbuffer = NULL;
    return eval_intexpr(expr);
}

static void read_if(void) {
    bool cond = read_constexpr();
    wastrue = cond;
    if (!cond)
        skip_cond_incl();
}

static void read_else(void) {
    expect_newline();
    if (wastrue)
        skip_cond_incl();
}

static void read_elif(void) {
    if (wastrue) {
        skip_cond_incl();
        return;
    }
    bool cond = read_constexpr();
    wastrue = cond;
    if (!cond)
        skip_cond_incl();
}

static void read_endif(void) {
    expect_newline();
}

static void read_directive(void) {
    Token *tok = read_cpp_token();
    if (is_ident(tok, "define"))     read_define();
    else if (is_ident(tok, "undef")) read_undef();
    else if (is_ident(tok, "if"))    read_if();
    else if (is_ident(tok, "else"))  read_else();
    else if (is_ident(tok, "elif"))  read_elif();
    else if (is_ident(tok, "endif")) read_endif();
    else
        error("unsupported preprocessor directive: %s", token_to_string(tok));
}

void unget_token(Token *tok) {
    list_push(altbuffer ? altbuffer : buffer, tok);
}

Token *peek_token(void) {
    Token *r = read_token();
    unget_token(r);
    return r;
}

static Token *get_token(void) {
    if (altbuffer)
        return list_pop(altbuffer);
    return (list_len(buffer) > 0) ? list_pop(buffer) : read_cpp_token();
}

static Token *read_token_int(Dict *hideset, bool return_at_eol) {
    for (;;) {
        Token *tok = get_token();
        if (!tok)
            return NULL;
        if (tok && tok->type == TTYPE_NEWLINE) {
            bol = true;
            if (return_at_eol)
                return NULL;
            continue;
        }
        if (bol && is_punct(tok, '#')) {
            read_directive();
            bol = true;
            continue;
        }
        bol = false;
        return expand(hideset, tok);
    }
}

Token *read_token(void) {
    return read_token_int(&EMPTY_DICT, false);
}
