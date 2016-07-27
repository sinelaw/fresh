typedef struct _SLL<t> {
    t *head;
    uint32_t count;
} SLL;

void sll_init(SLL<t> *sll)
{
    sll->head = NULL;
    sll->count = 0;
}

// 1. anonymous record with sll item field:
// void sll_push(SLL<T> *sll, { t *next , .. } *item) { }

// 2. any type t that as in instance of SLLItem class:
// class SLLItem t where ...methods...
// typeclass SLLItem of (t, x, y) {
typeclass SLLItem of t {
    t *sll_get_next(t *);
    void sll_set_next(t *, t*);
}
// where t implements SLLItem
//impl SLLItem by (Foo, Moo, Koko) {
impl SLLItem by Foo {
    sll_get_next = foo_get_next;
    sll_set_next = foo_set_next;
};

// Foo Moo Koko as SLLItem  { .. }

// implementation SLLItem of Foo Moo Koko  { .. }
// implementation SLLItem of Foo Moo Koko  { .. }


void sll_push(SLL t x *sll, t *item) where (SLLItem t,...)
{
    sll_set_next(t, sll->head);
    sll->head = item;
    sll->count++;
}

SLLItem<T> *sll_pop(SLL<T> *sll) where SLLItem t
{
    SLLItem<T> *const res = sll->head;
    sll->head = res == NULL ? NULL : sll_get_next(res);
    if (res) sll->count--;
    return res;
}


int x; // x <- newSTRef
// can read x??
if (..) {
    x = 0;
} else {
    x = 123;
}

x..

// iso recursive
typedef struct _Foo {
    struct _Foo *next1; @SLLItemNext // will generate SLLItemOfStructNamed!(Foo, "next1")
    struct _Foo *next2; @SLLItemNext
    uint64_t data;
} Foo;

// generate newtypes:
FooNext1
FooNext2

SLLItemOfStructNamed!(Foo, "next1"); // will generate also a newtype Foo1

// built-in type class
interface Field of t fieldName fieldType { }

//codegen: how to implement some type class for a given type
instance SLLItemOfStruct(t) {
    X *sll_get_next(X *x)
    {
        return x->next;
    }
    void sll_set_next(Foo *x, Foo *y)
    {
        x->next = y;
    }
}

// SLLNamed t "name" => instance SLLItem t {
// }


struct Foo1 {
    Foo unFoo1;
};

switch (foo1 /* has type Foo1 */) {
    case Foo1 { x }: /* x is of type Foo */
}

x.unFoo1;

// get(x, "unFoo1")
// set(x, "unfoo", bla)

Foo *unFoo1(Foo1 *) {
}

// equi recursive
X = struct {
    X *next;
    ...
};
