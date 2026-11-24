// Visual layout of a buffer for one view: source lines and decorations to
// rows of cells, at the view's own width.
//
//  * A conceal draws its text instead of [start, end); one that covers a
//    newline joins the next source line onto the same row (how compose mode
//    joins a paragraph's lines). With `reveal`, it is inactive while a cursor
//    is inside that range, so the source shows where the user is editing.
//  * Inline text is drawn before or after the character at its position, or
//    with `edge` at the right edge of the row it falls in.
//  * Lines are whole rows above or below their position's line; a `rule`
//    {left, fill, right} spans the view's width.
//  * With wrapping on, a row breaks at the last space that fits (else at the
//    width); continuation rows take the hanging indent and prefix of the
//    block decoration covering the line.
//  * A view's folds hide whole lines after a fold's first line.
// Decorations with `when: "compose"` apply only to a view in compose mode.

import { byteLen } from "./ot.js";

export function charWidth(cp) {
  if (cp === 0) return 0;
  if (cp < 0x1100) return 1;
  if (
    (cp >= 0x1100 && cp <= 0x115f) || (cp >= 0x2e80 && cp <= 0xa4cf && cp !== 0x303f) ||
    (cp >= 0xac00 && cp <= 0xd7a3) || (cp >= 0xf900 && cp <= 0xfaff) || (cp >= 0xfe30 && cp <= 0xfe4f) ||
    (cp >= 0xff00 && cp <= 0xff60) || (cp >= 0xffe0 && cp <= 0xffe6) ||
    (cp >= 0x1f300 && cp <= 0x1f64f) || (cp >= 0x1f900 && cp <= 0x1f9ff) || (cp >= 0x20000 && cp <= 0x3fffd)
  ) return 2;
  if ((cp >= 0x300 && cp <= 0x36f) || cp === 0x200b || (cp >= 0xfe00 && cp <= 0xfe0f)) return 0;
  return 1;
}
export function stringWidth(s) {
  let w = 0;
  for (const ch of s) w += charWidth(ch.codePointAt(0));
  return w;
}

const applies = (d, compose) => !d.when || (d.when === "compose" && compose);
const cursorIn = (cursors, r) => cursors.some((c) => c >= r.start && c < Math.max(r.end, r.start + 1));

function idxOf(s, b) {
  let n = 0, i = 0;
  while (i < s.length && n < b) {
    const c = s.charCodeAt(i);
    if (c < 0x80) n += 1;
    else if (c < 0x800) n += 2;
    else if (c >= 0xd800 && c < 0xdc00) { n += 4; i++; }
    else n += 3;
    i++;
  }
  return i;
}

// o: {compose, wrap, width, tab, cursors, folds:[{start,end}] (line indices),
//     styleAt(b, li, idx), whitespace}
export function layout(buf, top, maxRows, o) {
  const L = buf.text;
  const d = buf.decor;
  const rows = [];
  const consumed = [];
  if (!L || top < 0) return { rows, consumed, next: top };
  const width = Math.max(8, o.width);

  // Index the active decorations by position.
  const conceals = new Map();
  for (const c of d.conceals) {
    if (c.end <= c.start || !applies(c, o.compose)) continue;
    if (c.reveal && cursorIn(o.cursors, c.reveal)) continue;
    const prev = conceals.get(c.start);
    if (!prev || prev.end < c.end) conceals.set(c.start, c);
  }
  const inlines = new Map();
  for (const v of d.inlines) {
    if (!applies(v, o.compose)) continue;
    if (!inlines.has(v.pos)) inlines.set(v.pos, []);
    inlines.get(v.pos).push(v);
  }
  const vlines = new Map();
  for (const v of d.lines) {
    if (!applies(v, o.compose)) continue;
    const i = L.indexOf(v.pos);
    if (i < 0) continue;
    if (!vlines.has(i)) vlines.set(i, []);
    vlines.get(i).push(v);
  }
  for (const list of vlines.values()) list.sort((a, b) => a.order - b.order);
  const blockAt = (b) => {
    let best = null;
    for (const k of d.blocks) if (applies(k, o.compose) && b >= k.start && b < k.end) best = k;
    return best;
  };
  const foldAt = new Map((o.folds || []).map((f) => [f.start, f]));

  const newRow = (lineIdx, cont) => ({ kind: "text", cells: [], col: 0, lineIdx, cont, eol: null, fill: null, edge: [] });
  let row;
  const put = (ch, src, style, w) => { row.cells.push({ ch, src, style, w }); row.col += w; };
  const putText = (text, src, style, spans) => {
    let bi = 0;
    for (const ch of text) {
      let st = style || {};
      if (spans) for (const r of spans) if (bi >= r.start && bi < r.end) st = { ...st, ...r.style };
      put(ch, src, st, charWidth(ch.codePointAt(0)));
      bi += byteLen(ch);
    }
  };
  // Edge text waits for the end of its row, then sits against the width.
  const finishRow = () => {
    for (const v of row.edge) {
      const pad = width - row.col - stringWidth(v.text);
      if (pad > 0) putText(" ".repeat(pad), null, {});
      putText(v.text, null, v.style);
    }
    row.edge = [];
    rows.push(row);
  };
  const inlineAt = (b, side) => {
    for (const v of inlines.get(b) || []) {
      if (v.side !== side) continue;
      if (v.edge) row.edge.push(v);
      else putText(v.text, null, v.style);
    }
  };
  const virtualRow = (v) => {
    const r = { kind: "virtual", cells: [], col: 0, lineIdx: null, eol: null, fill: null, edge: [] };
    const prev = row;
    row = r;
    if (v.rule) {
      const l = v.rule.left || "", rt = v.rule.right || "", f = v.rule.fill || " ";
      putText(l + f.repeat(Math.max(0, width - stringWidth(l) - stringWidth(rt))) + rt, null, v.style);
    } else putText(v.text || "", null, v.style, v.spans);
    rows.push(r);
    row = prev;
  };
  // Continue on a new row: the hanging indent and prefix of the block.
  const wrapRow = (cur, b) => {
    const blk = blockAt(b);
    const carry = row.edge;
    row.edge = [];
    rows.push(row);
    row = newRow(cur, true);
    row.edge = carry;
    if (blk) {
      if (blk.prefix) putText(blk.prefix.text, null, blk.prefix.style);
      while (row.col < blk.indent) put(" ", null, {}, 1);
    }
  };

  let li = top;
  while (rows.length < maxRows && li < L.count) {
    for (const v of vlines.get(li) || []) if (v.above) virtualRow(v);
    row = newRow(li, false);
    consumed.push(li);
    let cur = li, text = L.lines[cur], i = 0, b = L.starts[cur], last = li;
    // Where the row may break: the cell index after the last space.
    let breakAt = -1;
    const fold = foldAt.get(li);
    for (;;) {
      if (i >= text.length) {
        // End of a source line; b is its newline (or the end of the text).
        inlineAt(b, "before");
        const c = conceals.get(b);
        if (c && L.hasNewline(cur)) {
          putText(c.text, b, { ...o.styleAt(b), ...(c.style || {}) }, c.spans);
          const j = L.indexOf(c.end);
          if (j < 0) { row.eol = b; break; }
          for (let k = last + 1; k <= j; k++) consumed.push(k);
          last = cur = j;
          text = L.lines[cur];
          i = idxOf(text, c.end - L.starts[cur]);
          b = c.end;
          breakAt = row.cells.length;
          continue;
        }
        if (fold) putText(" ⋯", null, { fg: "#808080" });
        inlineAt(b, "after");
        row.eol = b;
        if (o.extendAt) row.fill = o.extendAt(b);
        break;
      }
      inlineAt(b, "before");
      const c = conceals.get(b);
      if (c) {
        putText(c.text, b, { ...o.styleAt(b, cur, i), ...(c.style || {}) }, c.spans);
        const j = L.indexOf(c.end);
        if (j < 0) { row.eol = b; break; }
        if (j !== cur) {
          for (let k = last + 1; k <= j; k++) consumed.push(k);
          last = cur = j;
          text = L.lines[cur];
        }
        i = idxOf(text, c.end - L.starts[cur]);
        b = c.end;
        continue;
      }
      const cp = text.codePointAt(i);
      const ch = String.fromCodePoint(cp);
      const w = cp === 9 ? o.tab - (row.col % o.tab) : charWidth(cp);
      if (o.wrap && row.col + w > width && row.cells.some((x) => x.src != null)) {
        if (breakAt > 0 && breakAt < row.cells.length) {
          // Move the word that does not fit onto the next row.
          const tail = row.cells.splice(breakAt);
          row.col = row.cells.reduce((n, x) => n + x.w, 0);
          wrapRow(cur, b);
          for (const x of tail) put(x.ch, x.src, x.style, x.w);
        } else wrapRow(cur, b);
        breakAt = -1;
      }
      const st = o.styleAt(b, cur, i);
      if (cp === 9) {
        put(o.whitespace ? "→" : " ", b, o.whitespace ? { ...st, fg: "#505050" } : st, 1);
        for (let k = 1; k < w; k++) put(" ", null, st, 1);
      } else if (cp === 32 && o.whitespace) put("·", b, { ...st, fg: "#505050" }, 1);
      else put(ch, b, st, w);
      inlineAt(b, "after");
      if (cp === 32 || cp === 9) breakAt = row.cells.length;
      i += ch.length;
      b += byteLen(ch);
    }
    finishRow();
    // A fold hides the lines after its first.
    if (fold) last = Math.max(last, fold.end);
    for (let k = li; k <= last; k++) for (const v of vlines.get(k) || []) if (!v.above && !(fold && k > li)) virtualRow(v);
    li = last + 1;
  }
  return { rows, consumed, next: li };
}
