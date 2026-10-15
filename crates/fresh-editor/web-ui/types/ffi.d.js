// The typed surface of js/00-ffi.js, the one part of the web UI the inty
// checker does not read (web-ui/typecheck.sh leaves it out and loads this
// file instead). Each declaration says what the checked code may assume;
// keep the list short and each implementation a line or two.

// `target.addEventListener(type, fn, options)`. Member functions in
// types/dom.d.js take no options argument (see the note on `El` there).
/** const listen: <T>(target: T, type: String, fn: (UiEvent) => Undefined, options: Dict<Boolean>) => Undefined */
const listen;

// A value decoded from the server's JSON, given the type its use needs.
// The checker can't see which type a frame's changed path carries (the path
// is a string), so applyFrame says it; nothing checks it at run time, just
// as nothing checks what `JSON.parse` returns.
/** const jsonAs: <A, B>(A) => B */
const jsonAs;

// `window.fresh = hook`: the test surface web-ui/test/drive.mjs reads.
/** const exposeTestHook: <T>(T) => Undefined */
const exposeTestHook;

// `n.toFixed(digits)`: inty's stdlib has no methods on numbers.
/** const toFixed: (Number, Number) => String */
const toFixed;
