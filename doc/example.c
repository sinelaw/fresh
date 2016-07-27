
// fromMaybe :: forall t. Maybe t -> t -> t

// C++-style        : t fromMaybe<t>(Maybe<t> m, t _default) { }
// Without <t>      : t fromMaybe(Maybe<t> m, t _default) { }
// ocaml-style      : fromMaybe(m, _default) = {
// ocaml-style      : Maybe<Tuple<Maybe<t>, u>> fromMaybe(Maybe<t> m, t _default, u second) = {
// typescript-style : fromMaybe(m : Maybe(t), _default : t, second : u) : Maybe(Tuple(Maybe(t), u)) = {

// Preferred type syntax:
// Maybe(Tuple(Maybe(t), u)) fromMaybe(Maybe(t) m, t _default, u second) { .. }

/* variables/values = lowercase - snake_case */
/* data constructors = camel case (Just, Nothing) <-- or maybe lower case? */
/* types = capitalize (camel case) UInt16 */
/* type variables = lowercase */

/* Questions:
1. Const vs. non-const vs. pointers
2. for loops / foreach
3. arrays as memory area (e.g. in end of struct)
4. modules for systems programming (memory allocation, clock, etc.)
5. module = signature + implementation = compilation unit
   generating code that uses two different implementations of the same signature (e.g. linked list)
6. dynamic loading replacing internal module
7. namespace syntax : vs .
8. nullable pointers / pointer arithmetic if they are not nullable?
9. mutable vars vs. let bindings: var, val, let, mut, ?
10. uncrapify 'static'
11. shortcuts for defining interfaces using existing function names
12. typeclass vs. contract (or other name)
13. mempty of Monoid = by pointer or by value?

Answers:

1. x y z * is syntax sugar for: Ptr (x y z)

2. There is D-style code generation, used for generating instances of typeclasses for common
   types (e.g. structs with next*). Attributes used as shortcuts for code generation.

*/

clock.fresh-sig

module foo {
    import clock; // signature only

    Void func(..) {
        clock.get_nanos();
    }
}

import foo (with clock = ... )

linkage defaults {
    clock = real_time_clock;
    dynamic foo;
}


import foo as foo1 (with clock = load_module("clock"));
foo1::bla()

import foo as foo2 (with clock = clock2);

foo2::bla()

    /* Pattern matching: */
Maybe(Tuple(Maybe(t), u)) fromMaybe(Maybe(t) m, t _default, u second) {
    switch (m) {
    case [just(x)]
    case just: return x;
    case Nothing: return _default;
    }
}
Union { Just(t), Nothing }

Maybe(t) Just(t) { .. }

res : Union { Success(t), NotLeader, ..r }

error_handler: union (..r) -> ...

switch (res) {
case Success: ...
case NotLeader: ...
default x: error_handler(x);
}


struct { arg0: CtxSwitch, arg1: type, ..r } -> Void

Void shutdown(CtxSwitch ctx_switch, ...), ctx: { ..r}

forall r. { shutdown: { ctx_switch : CtxSwitch, ..r }, ctx: r }
do_shutdown(Void shutdown(CtxSwitch ctx_switch, ..r), { r, .. } ctx)
do_shutdown(shutdown, ctx)

shutdown(CTX_SWITCH, ctx)

data DrainResult = DrainDone | DrainPerformedWork

union DrainResult {
    Done,
    PerformedWork,
};

interface Service(s) {
    DrainResult drain(MutPtr(s));
    Bool equals(s*, s*);
    Bool kookoo(s); // pass by value - copied from caller
};

t read(Ref(t));
Void store(MutRef(t), t);

DrainResult drain(MutRef(KokoService) k) {
    k->field = 3;
    (*k)
}

DrainResult drain(items) {
    mut drain_results;
    val x = 3 * 4;
    mut y; // mutref(int)
    foreach (index, item in items) {
        if (Service.drain(item) == DrainResult.PerformedWork) {
            drain_results[index] = DrainResult.PerformedWork;
        }
    }
    return DrainResult.Done;
}

a[] = Array(a)

Alloc(a) malloc();
Void free(Alloc(a));

// deref(int *x) {
//     return *x;
// }

// funct() {
//     forall s1.
//     // type var: s1
//    int x;
//    y = &x; // Auto(s1, int)


//    derefS1 :: (Auto(s1, t) -> t)
// }

sig Clock {
    get_nanos : () -> Int;
}

interface Clock(c) {
    get_nanos : c -> Int;
}

sig Memory {
    type t;
    malloc : () -> t a;
    free : t a -> ();
}

Alloc(Alloc(int)[3]) nums = malloc();
int* nums = malloc(num);

interface Enumerable(e) {
    Void for_(e(a) items, Void func(a*));
}

forall a. instance Enumerable [a] {
    // size?
    Void for_([a] items, Void func(a*)) {
        for (int i = 0; i < size?; i++) {
            func(&items[i]);
        }
    }
}

/* Maybe<t> */

