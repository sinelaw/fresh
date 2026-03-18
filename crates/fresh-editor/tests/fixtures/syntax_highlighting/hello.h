/* C header syntax highlighting test */
#ifndef HELLO_H
#define HELLO_H

#include <stddef.h>

#define MAX_NAME_LEN 256
#define VERSION_MAJOR 1

typedef struct {
    char name[MAX_NAME_LEN];
    int count;
    double value;
} Config;

void greet(const char* name);
int init_config(Config* config, const char* name);

#endif /* HELLO_H */
