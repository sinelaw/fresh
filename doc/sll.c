api SLL {
    type SLL t;

    // impl SLLItem by Foo

    contract Item with t {
        t *get_next(t *);
        void set_next(t *, t*);
    };

    void init(SLL t *);
    void push(SLL t *, t *) where t of SLLItem;
    t *pop(SLL t *) where t of SLLItem;


    // contract Monoid with t {
    //     t empty;
    //     t *append(t *, t*);
    // }
};

// impl SLL.Item by Foo {
// }

typedef (SLL Int) Bah;

struct SLL t {
    t *head;
    uint32_t count;
};

void sll_init(SLL t *sll)
{
    sll->head = NULL;
    sll->count = 0;
}

// 1. anonymous record with sll item field:
// void sll_push(SLL<T> *sll, { t *next , .. } *item) { }

// 2. any type t that as in instance of SLLItem class:
// class SLLItem t where ...methods...
// typeclass SLLItem of (t, x, y) {

void sll_push(SLL t *sll, t *item) where t of SLLItem
{
    sll_set_next(t, sll->head);
    sll->head = item;
    sll->count++;
}

t *sll_pop(SLL t *sll) where t of SLLItem
{
    t *const res = sll->head;
    sll->head = res == NULL ? NULL : sll_get_next(res);
    if (res) sll->count--;
    return res;
}

// iso recursive
typedef struct _Foo {
    struct _Foo *next;
    uint64_t data;
} Foo;

void foo_set_next(Foo *item, Foo *next)
{
    item->next = next;
}

impl SLLItem by Foo {
    Foo *sll_get_next(Foo *item) {
        return item->next;
    }
    sll_set_next = foo_set_next;
};

// in header file:
// impl SLLItem by Foo;
