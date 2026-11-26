// Markdown structure for `lines_changed`: each line's role in a fenced code
// block (`open` / `body` / `close`) and in a table (`header` / `delimiter` /
// `row`, with `first_row` and `last`). In the editor these come from the
// grammar; here the client classifies its own text. Computed over the whole
// document, because a bare ``` opens or closes depending on every fence above
// it — so only for a buffer held whole. For a windowed (huge) buffer the
// fields are left out, which consumers read as "unknown".

const FENCE = /^(\s*)(`{3,}|~{3,})(.*)$/;
const DELIM = /^\s*\|?\s*:?-+:?\s*(\|\s*:?-+:?\s*)*\|?\s*$/;

/** type FenceRole = "open" | "body" | "close" */
/** type TableRow = {role: "header" | "delimiter" | "row", first_row: Boolean, last: Boolean} */

// A line in no fence, and in no table.
/** const NO_ROLE: FenceRole | Undefined */
const NO_ROLE = undefined;
/** const NO_ROW: TableRow | Undefined */
const NO_ROW = undefined;
// A fence's lines.
/** function role(r: FenceRole) => FenceRole | Undefined */
function role(r) { return r; }
// A table's line: its header (`k` 0), its delimiter (1) or a row.
/** function tableRow(k: Number, firstRow: Boolean, last: Boolean) => TableRow | Undefined */
function tableRow(k, firstRow, last) { return { role: k === 0 ? "header" : k === 1 ? "delimiter" : "row", first_row: firstRow, last }; }

/** function classify(lines: String[]) => {
 *   region: (FenceRole | Undefined)[],
 *   table: (TableRow | Undefined)[],
 * } */
export function classify(lines) {
  // (Filled by `push`: a `map` to the constant would widen its type.)
  /** const region: (FenceRole | Undefined)[] */
  const region = [];
  /** const table: (TableRow | Undefined)[] */
  const table = [];
  for (const _ of lines) { region.push(NO_ROLE); table.push(NO_ROW); }
  /** let fence: {ch: String, n: Number} | Null */
  let fence = null;
  for (let i = 0; i < lines.length; i++) {
    const t = lines[i];
    const m = FENCE.exec(t);
    const open = fence;
    if (open) {
      if (m && m[2][0] === open.ch && m[2].length >= open.n && m[3].trim() === "") {
        region[i] = role("close");
        fence = null;
      } else region[i] = role("body");
      continue;
    }
    if (m && !(m[2][0] === "`" && m[3].includes("`"))) {
      region[i] = role("open");
      fence = { ch: m[2][0], n: m[2].length };
      continue;
    }
    // A table: a header row with a pipe, then a delimiter row.
    if (t.includes("|") && i + 1 < lines.length && DELIM.test(lines[i + 1]) && lines[i + 1].includes("-")) {
      let j = i + 2;
      while (j < lines.length && lines[j].includes("|") && lines[j].trim() !== "" && !FENCE.test(lines[j])) j++;
      for (let k = i; k < j; k++) {
        table[k] = tableRow(k - i, k === i + 2, k === j - 1);
      }
      i = j - 1;
    }
  }
  return { region, table };
}
