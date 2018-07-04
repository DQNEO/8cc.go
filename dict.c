#include <stdlib.h>
#include <string.h>
#include "dict.h"

typedef struct DictEntry {
    char *key;
    void *val;
} DictEntry;


Dict *make_dict(void *p) {
    Dict *r = malloc(sizeof(Dict));
    r->list = make_list();
    return r;
}

Dict *dict_parent(Dict *d) {
    return NULL;
}

void *dict_get(Dict *dict, char *key) {
    for (Iter *i = list_iter(dict->list); !iter_end(i);) {
        DictEntry *e = iter_next(i);
        if (!strcmp(key, e->key))
            return e->val;
    }
    return NULL;
}

void dict_put(Dict *dict, char *key, void *val) {
    DictEntry *e = malloc(sizeof(DictEntry));
    e->key = key;
    e->val = val;
    list_push(dict->list, (void *)e);
}
