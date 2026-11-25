// The command table's entries.

// A command's action as the command table holds it: run with its one
// optional argument, it comes to a promise of nothing (its failure reaches
// whoever awaits it).
export const asCommand = (f) => (arg) => Promise.resolve(f(arg)).then(() => undefined);
