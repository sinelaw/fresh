// Markdown structure for `lines_changed`: each line's role in a fenced code
// block (`open` / `body` / `close`) and in a table (`header` / `delimiter` /
// `row`, with `first_row` and `last`). In the editor these come from the
// grammar; here the client classifies its own text. Computed over the whole
// document, because a bare ``` opens or closes depending on every fence above
// it — so only for a buffer held whole. For a windowed (huge) buffer the
// fields are left out, which consumers read as "unknown".

const FENCE = /^(\s*)(`{3,}|~{3,})(.*)$/;
const DELIM = /^\s*\|?\s*:?-+:?\s*(\|\s*:?-+:?\s*)*\|?\s*$/;

export function classify(lines) {
  const region = new Array(lines.length).fill(undefined);
  const table = new Array(lines.length).fill(undefined);
  let fence = null; // {ch, n}
  for (let i = 0; i < lines.length; i++) {
    const t = lines[i];
    const m = FENCE.exec(t);
    if (fence) {
      if (m && m[2][0] === fence.ch && m[2].length >= fence.n && m[3].trim() === "") {
        region[i] = "close";
        fence = null;
      } else region[i] = "body";
      continue;
    }
    if (m && !(m[2][0] === "`" && m[3].includes("`"))) {
      region[i] = "open";
      fence = { ch: m[2][0], n: m[2].length };
      continue;
    }
    // A table: a header row with a pipe, then a delimiter row.
    if (t.includes("|") && i + 1 < lines.length && DELIM.test(lines[i + 1]) && lines[i + 1].includes("-")) {
      table[i] = { role: "header", first_row: false, last: false };
      table[i + 1] = { role: "delimiter", first_row: false, last: false };
      let j = i + 2;
      let first = true;
      while (j < lines.length && lines[j].includes("|") && lines[j].trim() !== "" && !FENCE.test(lines[j])) {
        table[j] = { role: "row", first_row: first, last: false };
        first = false;
        j++;
      }
      table[j - 1].last = true;
      i = j - 1;
    }
  }
  return { region, table };
}
