// A value that may be missing, as one type (`T | Null`): for a function
// that answers one or the other, so both answers are typed alike.

/** function some<T>(x: T) => T | Null */
export function some(x) { return x; }
/** function none<T>() => T | Null */
export function none() { return null; }
