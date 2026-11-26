// The command table's entries.

// A command's action as the command table holds it: run with its one
// optional argument, it comes to a promise of nothing (its failure reaches
// whoever awaits it).
/** function asCommand<R>(f: () => R) => (CommandArg) => Promise<Undefined> */
export function asCommand(f) { return (_arg) => Promise.resolve(f()).then((_r) => undefined); }
// A command whose action takes the argument (a palette prefix, a theme's
// key).
/** function asArgCommand<R>(f: (CommandArg) => R) => (CommandArg) => Promise<Undefined> */
export function asArgCommand(f) { return (arg) => Promise.resolve(f(arg)).then((_r) => undefined); }
