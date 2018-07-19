#ifndef EIGHTCC_H
#define EIGHTCC_H

#include <stdbool.h>
#include "util.h"
#include "list.h"
#include "dict.h"

enum {
    TTYPE_IDENT,
    TTYPE_PUNCT,
    TTYPE_NUMBER,
    TTYPE_CHAR,
    TTYPE_STRING,
};

typedef struct {
    int type;
    union {
        char *sval;
        int punct;
        char c;
    };
} Token;

typedef struct {
    char *body;
    int nalloc;
    int len;
} String;

enum {
    AST_LITERAL = 256,
    AST_STRING,
    AST_LVAR,
    AST_GVAR,
    AST_FUNCALL,
    AST_FUNC,
    AST_DECL,
    AST_ARRAY_INIT,
    AST_ADDR,
    AST_DEREF,
    AST_IF,
    AST_TERNARY,
    AST_FOR,
    AST_RETURN,
    AST_COMPOUND_STMT,
    AST_STRUCT_REF,
    PUNCT_EQ,
    PUNCT_INC,
    PUNCT_DEC,
    PUNCT_LOGAND,
    PUNCT_LOGOR,
    PUNCT_ARROW,
};

enum {
    CTYPE_VOID,
    CTYPE_CHAR,
    CTYPE_INT,
    CTYPE_LONG,
    CTYPE_FLOAT,
    CTYPE_DOUBLE,
    CTYPE_ARRAY,
    CTYPE_PTR,
    CTYPE_STRUCT,
};

typedef struct Ctype {
    int type;
    int size;
    struct Ctype *ptr; // pointer or array
    // array length
    int len;
    // struct
    char *tag;
    Dict *fields;
    int offset;
} Ctype;

typedef struct Ast {
    int type;
    Ctype *ctype;
    union {
        // Char, int, or long
        long ival;
        // Float or double
        struct {
            double fval;
            char *flabel;
        };
        // String
        struct {
            char *sval;
            char *slabel;
        };
        // Local/global variable
        struct {
            char *varname;
            struct {
                int loff;
                char *glabel;
            };
        };
        // Binary operator
        struct {
            struct Ast *left;
            struct Ast *right;
        };
        // Unary operator
        struct {
            struct Ast *operand;
        };
        // Function call or function declaration
        struct {
            char *fname;
            struct {
                struct List *args;
                struct {
                    struct List *params;
                    struct List *localvars;
                    struct Ast *body;
                };
            };
        };
        // Declaration
        struct {
            struct Ast *declvar;
            struct Ast *declinit;
        };
        // Array initializer
        struct List *arrayinit;
        // If statement or ternary operator
        struct {
            struct Ast *cond;
            struct Ast *then;
            struct Ast *els;
        };
        // For statement
        struct {
            struct Ast *forinit;
            struct Ast *forcond;
            struct Ast *forstep;
            struct Ast *forbody;
        };
        // Return statement
        struct Ast *retval;
        // Compound statement
        struct List *stmts;
        // Struct reference
        struct {
            struct Ast *struc;
            char *field; // only for ast_to_string
            //Ctype *fieldtype;
        };
    };
} Ast;

typedef struct Env {
    List *vars;
    struct Env *next;
    struct List *structs;
} Env;

#define EMPTY_ENV                                       \
    (((Env){ .vars = &EMPTY_LIST, .next = NULL }))

extern String *make_string(void);
extern char *get_cstring(String *s);
extern void string_append(String *s, char c);
extern void string_appendf(String *s, char *fmt, ...);

extern char *token_to_string(Token *tok);
extern bool is_punct(Token *tok, int c);
extern void unget_token(Token *tok);
extern Token *peek_token(void);
extern Token *read_token(void);
extern char *ast_to_string(Ast *ast);
extern char *ctype_to_string(Ctype *ctype);
extern void print_asm_header(void);
extern char *make_label(void);
extern List *read_toplevels(void);
extern bool is_inttype(Ctype *ctype);
extern bool is_flotype(Ctype *ctype);

extern void emit_data_section(void);
extern void emit_toplevel(Ast *v);

extern Env *globalenv;
extern List *flonums;

#endif /* EIGHTCC_H */
