// Words as the UI writes them.

// `n` and `word`, plural unless there is one: "1 file", "3 files".
/** function plural(n: Number, word: String) => String */
export function plural(n, word) { return `${n} ${word}${n === 1 ? "" : "s"}`; }
