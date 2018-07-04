#ifndef EIGHTCC_DICT_H
#define EIGHTCC_DICT_H

#include "list.h"

typedef struct Dict {
    List *list;
    struct Dict *parent;
} Dict;

Dict *make_dict(void *p);
Dict *dict_parent(Dict *d);
void *dict_get(Dict *d, char *key);
void dict_put(Dict *d, char *key, void *val);

#endif /* EIGHTCC_DICT_H */
