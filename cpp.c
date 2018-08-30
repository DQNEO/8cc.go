#include <stdlib.h>
#include "8cc.h"

static Dict *macros = &EMPTY_DICT;
static List *buffer = &EMPTY_LIST;
static List *altbuffer = NULL;
static List *cond_incl_stack = &EMPTY_LIST;
static bool bol = true;

typedef enum { IN_THEN, IN_ELSE } CondInclCtx;

typedef struct {
    CondInclCtx ctx;
    bool wastrue;
} CondIncl;

typedef enum { MACRO_OBJ, MACRO_FUNC } MacroType;

typedef struct {
    MacroType type;
    int nargs;
    List *body;
} Macro;

static Token *read_token_int(Dict *hideset, bool return_at_eol);
static Token *get_token(void);

static CondIncl *make_cond_incl(CondInclCtx ctx, bool wastrue) {
    CondIncl *r = malloc(sizeof(CondIncl));
    r->ctx = ctx;
    r->wastrue = wastrue;
    return r;
}

static Macro *make_obj_macro(List *body) {
    Macro *r = malloc(sizeof(Macro));
    r->type = MACRO_OBJ;
    r->body = body;
    return r;
}

static Macro *make_func_macro(List *body, int nargs) {
    Macro *r = malloc(sizeof(Macro));
    r->type = MACRO_FUNC;
    r->nargs = nargs;
    r->body = body;
    return r;
}

static Token *make_macro_token(int position) {
    Token *r = malloc(sizeof(Token));
    r->type = TTYPE_MACRO_PARAM;
    r->position = position;
    r->space = false;
    return r;
}

static void expect(char punct) {
    Token *tok = get_token();
    if (!tok || !is_punct(tok, punct))
        error("%c expected, but got %s", punct, t2s(tok));
}

static Token *read_ident(void) {
    Token *r = get_token();
    if (r->type != TTYPE_IDENT)
        error("identifier expected, but got %s", t2s(r));
    return r;
}

static void expect_newline(void) {
    Token *tok = get_token();
    if (!tok || tok->type != TTYPE_NEWLINE)
        error("Newline expected, but got %s", t2s(tok));
}

static void unget_all(List *tokens) {
    for (Iter *i = list_iter(list_reverse(tokens)); !iter_end(i);)
        unget_token(iter_next(i));
}

static Token *read_expand(Dict *hideset) {
    Token *tok = get_token();
    if (!tok) return NULL;
    if (tok->type != TTYPE_IDENT)
        return tok;
    char *name = tok->sval;
    Macro *macro = dict_get(macros, name);
    if (!macro || dict_get(hideset, name))
        return tok;

    switch (macro->type) {
    case MACRO_OBJ: {
        dict_put(hideset, name, (void *)1);
        List *tokens = macro->body;
        unget_all(tokens);
        return read_expand(hideset);
    }
    case MACRO_FUNC: {
        error("TBD: func macro %s", name);
    }
    default:
        error("internal error");
    }
}

static void read_funclike_macro_args(Dict *param) {
    int pos = 0;
    for (;;) {
        Token *tok = get_token();
        if (is_punct(tok, ')'))
            return;
        if (pos) {
            if (!is_punct(tok, ','))
                error("',' expected, but got '%s'", t2s(tok));
            tok = get_token();
        }
        if (!tok || tok->type == TTYPE_NEWLINE)
            error("missing ')' in macro parameter list");
        if (tok->type != TTYPE_IDENT)
            error("identifier expected, but got '%s'", t2s(tok));
        dict_put(param, tok->sval, make_macro_token(pos++));
    }
}

static List *read_funclike_macro_body(Dict *param) {
    List *r = make_list();
    for (;;) {
        Token *tok = get_token();
        if (!tok || tok->type == TTYPE_NEWLINE)
            return r;
        if (tok->type == TTYPE_IDENT) {
            Token *subst = dict_get(param, tok->sval);
            if (subst) {
                list_push(r, subst);
                continue;
            }

        }
        list_push(r, tok);
    }
    return r;
}

static void read_funclike_macro(char *name) {
    Dict *param = make_dict(NULL);
    read_funclike_macro_args(param);
    List *body = read_funclike_macro_body(param);
    Macro *macro = make_func_macro(body, list_len(dict_keys(param)));
    dict_put(macros, name, macro);
}

static void read_obj_macro(char *name) {
    List *body = make_list();
    for (;;) {
        Token *tok = get_token();
        if (!tok || tok->type == TTYPE_NEWLINE)
            break;
        printf("# pushing token %s to obj macro body\n", t2s(tok));
        list_push(body, tok);
    }
    dict_put(macros, name, make_obj_macro(body));
}

static void read_define(void) {
    Token *name = read_ident();
    Token *tok = get_token();
    if (tok && is_punct(tok, '(') && !tok->space) {
        error("funclike macro found");
        read_funclike_macro(name->sval);
        return;
    }
    unget_token(tok);
    read_obj_macro(name->sval);
}

static void read_undef(void) {
    Token *name = read_ident();
    expect_newline();
    dict_remove(macros, name->sval);
}

static Token *read_defined_operator(void) {
    Token *tok = get_token();
    if (is_punct(tok, '(')) {
        tok = get_token();
        expect(')');
    }
    if (tok->type != TTYPE_IDENT)
        error("Identifier expected, but got %s", t2s(tok));
    return dict_get(macros, tok->sval) ?
        cpp_token_one : cpp_token_zero;
}

static List *read_intexpr_line(void) {
    List *r = make_list();
    for (;;) {
        Token *tok = read_token_int(&EMPTY_DICT, true);
        if (!tok) return r;
        if (is_ident(tok, "defined"))
            list_push(r, read_defined_operator());
        else if (tok->type == TTYPE_IDENT)
            list_push(r, cpp_token_one);
        else
            list_push(r, tok);
    }
}

static bool read_constexpr(void) {
    altbuffer = list_reverse(read_intexpr_line());
    Ast *expr = read_expr();
    if (list_len(altbuffer) > 0)
        error("Stray token: %s", t2s(list_shift(altbuffer)));
    altbuffer = NULL;
    return eval_intexpr(expr);
}

static void read_if(void) {
    bool cond = read_constexpr();
    list_push(cond_incl_stack, make_cond_incl(IN_THEN, cond));
    if (!cond)
        skip_cond_incl();
}

static void read_else(void) {
    if (list_len(cond_incl_stack) == 0)
        error("stray #else");
    CondIncl *ci = list_tail(cond_incl_stack);
    if (ci->ctx == IN_ELSE)
        error("#else appears in #else");
    expect_newline();
    if (ci->wastrue)
        skip_cond_incl();
}

static void read_elif(void) {
    if (list_len(cond_incl_stack) == 0)
        error("stray #elif");
    CondIncl *ci = list_tail(cond_incl_stack);
    if (ci->ctx == IN_ELSE)
        error("#elif after #else");
    if (ci->wastrue)
        skip_cond_incl();
    else {
        bool cond = read_constexpr();
        ci->wastrue = cond;
        if (!cond)
            skip_cond_incl();
    }
}

static void read_endif(void) {
    if (list_len(cond_incl_stack) == 0)
        error("stray #endif");
    list_pop(cond_incl_stack);
    expect_newline();
}

static void read_directive(void) {
    Token *tok = get_token();
    if (is_ident(tok, "define"))     read_define();
    else if (is_ident(tok, "undef")) read_undef();
    else if (is_ident(tok, "if"))    read_if();
    else if (is_ident(tok, "else"))  read_else();
    else if (is_ident(tok, "elif"))  read_elif();
    else if (is_ident(tok, "endif")) read_endif();
    else
        error("unsupported preprocessor directive: %s", t2s(tok));
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
        unget_token(tok);
        return read_expand(hideset);
    }
}

Token *read_token(void) {
    return read_token_int(&EMPTY_DICT, false);
}
