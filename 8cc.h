#ifndef EIGHTCC_H
#define EIGHTCC_H

#include <stdbool.h>
#include <stdio.h>
#include "dict.h"
#include "list.h"
#include "util.h"

typedef struct {
    char *body;
    int nalloc;
    int len;
} String;

extern String *make_string(void);
extern char *format(char *fmt, ...);
extern char *get_cstring(String *s);
extern int string_len(String *s);
extern void string_append(String *s, char c);
extern void string_appendf(String *s, char *fmt, ...);

enum {
    TTYPE_IDENT,
    TTYPE_PUNCT,
    TTYPE_NUMBER,
    TTYPE_CHAR,
    TTYPE_STRING,
    // Only in CPP
    TTYPE_NEWLINE,
    TTYPE_SPACE,
    TTYPE_MACRO_PARAM,
};

typedef struct {
    int type;
    bool space;
    bool bol;
    char *file;
    int line;
    int column;
    Dict *hideset;
    union {
        char *sval;
        int punct;
        char c;
        union {
            int position;
        };
    };
} Token;

enum {
    AST_LITERAL = 256,
    AST_STRING,
    AST_LVAR,
    AST_GVAR,
    AST_FUNCALL,
    AST_FUNC,
    AST_DECL,
    AST_INIT_LIST,
    AST_ADDR,
    AST_DEREF,
    AST_IF,
    AST_TERNARY,
    AST_FOR,
    AST_RETURN,
    AST_COMPOUND_STMT,
    AST_STRUCT_REF,
    OP_EQ,
    OP_NE,
    OP_LE,
    OP_GE,
    OP_INC,
    OP_DEC,
    OP_LOGAND,
    OP_LOGOR,
    OP_ARROW,
};

enum {
    CTYPE_VOID,
    CTYPE_CHAR,
    CTYPE_SHORT,
    CTYPE_INT,
    CTYPE_LONG,
    CTYPE_LLONG,
    CTYPE_FLOAT,
    CTYPE_DOUBLE,
    CTYPE_LDOUBLE,
    CTYPE_ARRAY,
    CTYPE_PTR,
    CTYPE_STRUCT,
    CTYPE_FUNC,
    // used only in parser
    CTYPE_STUB,
};

typedef struct Ctype {
    int type;
    int size;
    // true if signed
    bool sig;
    // pointer or array
    struct Ctype *ptr;
    // array length
    int len;
    // struct
    Dict *fields;
    int offset;
    // function
    struct Ctype *rettype;
    List *params;
    bool hasva;
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
            int loff;
            char *glabel;
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
            // Function call
            struct List *args;
            struct List *paramtypes;
            // Function declaration
            struct List *params;
            struct List *localvars;
            struct Ast *body;
        };
        // Declaration
        struct {
            struct Ast *declvar;
            struct Ast *declinit;
        };
        // array or struct initializer
        struct {
            struct List *initlist;
            Ctype *totype;
        };
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
            char *field;  // only for ast_to_string
        };
    };
} Ast;

extern Ctype *ctype_char;
extern Ctype *ctype_short;
extern Ctype *ctype_int;
extern Ctype *ctype_long;
extern Ctype *ctype_float;
extern Ctype *ctype_double;

extern void unget_cpp_token(Token *tok);
extern Token *peek_cpp_token(void);
extern Token *read_cpp_token(void);
extern void set_input_buffer(List *tokens);
extern List *get_input_buffer(void);
extern void skip_cond_incl(void);
extern bool read_header_file_name(char **name, bool *std);
extern void push_input_file(char *filename, FILE *input);
extern void set_input_file(char *filename, FILE *input);
extern char *input_position(void);

extern void unget_token(Token *tok);
extern Token *peek_token(void);
extern Token *read_token(void);

extern char *t2s(Token *tok);
extern bool is_punct(Token *tok, int c);
extern bool is_ident(Token *tok, char *s);
extern char *a2s(Ast *ast);
extern char *c2s(Ctype *ctype);
extern void print_asm_header(void);
extern char *make_label(void);
extern List *read_toplevels(void);
extern Ast *read_expr(void);
extern int eval_intexpr(Ast *ast);
extern bool is_inttype(Ctype *ctype);
extern bool is_flotype(Ctype *ctype);
extern Ctype *result_type(char op, Ctype *a, Ctype *b);

extern void emit_data_section(void);
extern void emit_toplevel(Ast *v);

extern List *strings;
extern List *flonums;

#endif /* EIGHTCC_H */
