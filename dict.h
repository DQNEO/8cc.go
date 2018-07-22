#ifndef EIGHTCC_DICT_H
#define EIGHTCC_DICT_H

#include "list.h"

typedef struct Dict {
    List *list;
    struct Dict *parent;
} Dict;

#define EMPTY_DICT                              \
    ((Dict){ &EMPTY_LIST, NULL })

void *make_dict(void *parent);
void *dict_get(Dict *dict, char *key);
void dict_put(Dict *dict, char *key, void *val);
List *dict_keys(Dict *dict);
List *dict_values(Dict *dict);
void *dict_parent(Dict *dict);

#endif /* EIGHTCC_DICT_H */
