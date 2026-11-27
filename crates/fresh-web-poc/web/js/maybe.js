// A value that may be missing, as one type (`T | Null`): for a function
// that answers one or the other, so both answers are typed alike.

/** function some<T>(x: T) => T | Null */
export function some(x) { return x; }
/** function none<T>() => T | Null */
export function none() { return null; }
// A value or none as a list of at most one (to act on it if it is there).
/** function listOf<T>(x: T | Null) => T[] */
export function listOf(x) { return x === null ? [] : [x]; }
// A value that may be undefined (a Map lookup, a find) as a list of at
// most one.
/** function found<T>(x: T | Undefined) => T[] */
export function found(x) { return x === undefined ? [] : [x]; }
