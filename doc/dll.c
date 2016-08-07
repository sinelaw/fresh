api DLL {
    type DLL t;

    contract Item with t {
        t *get_next(t *);
        t *get_prev(t *);
        Void set_next(t *, t *);
        Void set_prev(t *, t *);
    };

    Void init(DLL t *);
    Void push_head(DLL t *, t *) where t of Item;
    Void push_tail(DLL t *, t *) where t of Item;
    t *remove(SLL t *, t *) where t of Item;
};

# Questions:
# 1. How do comments look?
# 2. Inside 'api', namespacing is clear. However, outside of 'api', do we need to add 'DLL.' to everything?
#    If not, what iss the namespacing?

struct DLL t {
    t *head;
    t *tail;
    uint32_t count;
};

Void init(DLL t *dll)
{
    dll->head = NULL;
    dll->tail = NULL;
    dll->count = 0;
}

Void push_head(DLL t *dll, t *item) where t of Item
{
    set_next(item, dll->head);
    set_prev(item , NULL);
    dll->head = item;
    if (!dll->tail) {
        dll->tail = dll->head;
    }
    dll->count++;
}

Void push_tail(DLL t *dll, t *item) where t of Item
{
    set_next(item, NULL);
    set_prev(item , dll->tail);
    dll->tail = item;
    if (!dll->head) {
        dll->head = dll->tail;
    }
    dll->count++;
}

Void remove(DLL t *dll, t *item) where t of Item
{
    let next = get_next(item);
    let prev = get_prev(item);

    if (next) {
        set_prev(next, prev);
    }
    if (prev) {
        set_next(prev, next);
    }

    if (item == dll->head) {
        dll->head = next;
    }
    if (item == dll->tail) {
        dll->tail = prev;
    }
}
