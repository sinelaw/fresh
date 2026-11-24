// Visual layout of a buffer for one view: source lines plus decorations to
// rows of cells, at the view's own width.
//
// Decoration semantics follow the editor's:
//  * a conceal hides [start, end) and draws its replacement instead; one that
//    covers a newline joins the next source line onto the same row (how
//    compose reflows a paragraph). `unless-cursor-in` / `if-cursor-in` make it
//    depend on whether a cursor is inside its scope; `composeOnly` limits it
//    to a view in compose mode;
//  * a soft break ends the row before its byte and starts the continuation
//    `indent` columns in, with an optional prefix drawn inside the indent;
//  * inline virtual text is drawn before (or after) the character at its
//    position, optionally padded to end at `padToColumn`;
//  * virtual lines are whole rows above or below their anchor's line;
//  * overlays style the source bytes they cover.
// With wrapping on, a row that reaches the width breaks there.

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

function cursorIn(cursors, s, e) {
  return cursors.some((c) => c >= s && c < e);
}

export function active(d, compose, cursors) {
  if (d.composeOnly && !compose) return false;
  if (d.activation === "unless-cursor-in") return !cursorIn(cursors, d.scopeStart, d.scopeEnd);
  if (d.activation === "if-cursor-in") return cursorIn(cursors, d.scopeStart, d.scopeEnd);
  return true;
}

// Group decorations by the window line their position falls in.
function byLine(L, items, posOf) {
  const m = new Map();
  for (const it of items) {
    const i = L.indexOf(posOf(it));
    if (i < 0) continue;
    if (!m.has(i)) m.set(i, []);
    m.get(i).push(it);
  }
  return m;
}

// Lay out from window line `top` until `maxRows` rows exist.
// `o`: {compose, wrap, width, tab, cursors:[byte], styleAt(b) -> style,
//       syntax(lineIdx) -> [{s,e,style}] (UTF-16 indices)}
export function layout(buf, top, maxRows, o) {
  const L = buf.text;
  const d = buf.decor;
  const rows = [];
  const consumed = [];
  if (!L || top < 0) return { rows, consumed };
  const width = Math.max(4, o.width);

  const conceals = new Map();
  for (const c of d.conceals) {
    if (c.end <= c.start || !active(c, o.compose, o.cursors)) continue;
    if (!conceals.has(c.start) || conceals.get(c.start).end < c.end) conceals.set(c.start, c);
  }
  const breaks = new Map();
  for (const b of d.softBreaks) if (active(b, o.compose, o.cursors)) breaks.set(b.pos, b);
  const vtexts = new Map();
  for (const v of d.vtexts.values()) {
    if (v.composeOnly && !o.compose) continue;
    if (!vtexts.has(v.pos)) vtexts.set(v.pos, []);
    vtexts.get(v.pos).push(v);
  }
  const vl = byLine(L, d.vlines.filter((v) => !v.composeOnly || o.compose), (v) => v.pos);
  for (const list of vl.values()) list.sort((a, b) => (a.priority || 0) - (b.priority || 0));

  const newRow = (lineIdx, cont) => ({ kind: "text", cells: [], col: 0, lineIdx, cont, eol: null, fill: null, rowEnd: [] });
  let row;
  const put = (ch, src, style, w) => {
    row.cells.push({ ch, src, style, w });
    row.col += w;
  };
  const putText = (text, src, style, overlays) => {
    let bi = 0;
    for (const ch of text) {
      const cp = ch.codePointAt(0);
      let st = style;
      if (overlays) for (const r of overlays) if (bi >= r.start && bi < r.end) st = { ...st, ...r };
      put(ch, src, st, charWidth(cp));
      bi += byteLen(ch);
    }
  };
  // Hints padded to a column wait for the end of their row (a newline or a
  // soft break, not a wrap), then end at that column.
  const flushRowEnd = () => {
    for (const v of row.rowEnd) {
      const pad = v.padToColumn - row.col - stringWidth(v.text);
      putText(" ".repeat(Math.max(0, pad)) + v.text, null, v.style || {});
    }
    row.rowEnd = [];
  };
  const pushRow = () => { flushRowEnd(); rows.push(row); };
  // Inline virtual text at byte b, padded as the editor pads inlay hints:
  // before a character "text ", before a newline " text " (just "text" on an
  // empty line), after a character " text".
  const emitVtexts = (b, before, atNewline, emptyLine) => {
    const list = vtexts.get(b);
    if (!list) return;
    for (const v of list) {
      if (!!v.before !== before) continue;
      if (v.padToColumn != null) { row.rowEnd.push(v); continue; }
      const t = before ? (atNewline ? (emptyLine ? v.text : ` ${v.text} `) : `${v.text} `) : ` ${v.text}`;
      putText(t, null, v.style || {});
    }
  };
  const virtualRow = (v) => {
    const r = { kind: "virtual", cells: [], col: 0, lineIdx: null, eol: null, fill: v.style?.bg ?? null, rowEnd: [] };
    const prev = row;
    row = r;
    const ov = (v.style?.textOverlays || []).map((t) => ({ start: t.start, end: t.end, fg: t.fg, bg: t.bg, bold: t.bold, italic: t.italic, underline: t.underline }));
    putText(v.text, null, { fg: v.style?.fg, bg: v.style?.bg }, ov);
    rows.push(r);
    row = prev;
  };
  // A soft break at b ends the row (when it has started) and indents the
  // continuation, prefix first.
  const softBreak = (b, cur) => {
    const sb = breaks.get(b);
    if (!sb) return false;
    pushRow();
    row = newRow(cur, true);
    const indent = Math.max(sb.indent || 0, sb.prefix?.text ? stringWidth(sb.prefix.text) : 0);
    if (sb.prefix?.text) putText(sb.prefix.text, null, { fg: sb.prefix.fg, bg: sb.prefix.bg, bold: sb.prefix.bold, italic: sb.prefix.italic });
    while (row.col < indent) put(" ", null, {}, 1);
    return true;
  };

  let li = top;
  while (rows.length < maxRows && li < L.count) {
    for (const v of vl.get(li) || []) if (v.above) virtualRow(v);
    row = newRow(li, false);
    consumed.push(li);
    let cur = li;
    let text = L.lines[cur];
    let i = 0;
    let b = L.starts[cur];
    let lastConsumed = li;
    let lineHasText = text.length > 0;
    for (;;) {
      if (i >= text.length) {
        // End of a source line: b is its newline (or the end of the text).
        if (L.hasNewline(cur)) softBreak(b, cur);
        emitVtexts(b, true, true, !lineHasText);
        const c = conceals.get(b);
        if (c && L.hasNewline(cur)) {
          putText(c.replacement || "", b, { ...o.styleAt(b), ...(c.style || {}) }, c.replacementOverlays);
          emitVtexts(b, false);
          const j = L.indexOf(c.end);
          if (j < 0) { row.eol = b; break; }
          cur = j; text = L.lines[cur];
          i = Math.min(text.length, idxOf(text, c.end - L.starts[cur]));
          b = c.end;
          lineHasText = true;
          for (let k = lastConsumed + 1; k <= cur; k++) consumed.push(k);
          lastConsumed = cur;
          continue;
        }
        emitVtexts(b, false);
        row.eol = b;
        const ext = o.extendAt ? o.extendAt(b) : null;
        if (ext) row.fill = ext;
        break;
      }
      const cp = text.codePointAt(i);
      if (softBreak(b, cur) && cp === 32) {
        // The space is the break: the row end consumes it.
        i += 1;
        b += 1;
        continue;
      }
      emitVtexts(b, true, false, false);
      const c = conceals.get(b);
      if (c) {
        putText(c.replacement || "", b, { ...o.styleAt(b), ...(c.style || {}) }, c.replacementOverlays);
        const j = L.indexOf(c.end);
        if (j < 0) { row.eol = b; break; }
        if (j !== cur) {
          for (let k = lastConsumed + 1; k <= j; k++) consumed.push(k);
          lastConsumed = j;
          cur = j; text = L.lines[cur];
        }
        i = idxOf(text, c.end - L.starts[cur]);
        b = c.end;
        continue;
      }
      const ch = String.fromCodePoint(cp);
      let w = cp === 9 ? o.tab - (row.col % o.tab) : charWidth(cp);
      if (o.wrap && row.col + w > width && row.cells.length) {
        rows.push(row);
        const carry = row.rowEnd;
        row = newRow(cur, true);
        row.rowEnd = carry;
      }
      const st = o.styleAt(b, cur, i);
      if (cp === 9) { put(" ", b, st, 1); for (let k = 1; k < w; k++) put(" ", null, st, 1); }
      else put(ch, b, st, w);
      emitVtexts(b, false);
      i += ch.length;
      b += byteLen(ch);
    }
    pushRow();
    for (let k = li; k <= lastConsumed; k++) for (const v of vl.get(k) || []) if (!v.above) virtualRow(v);
    li = lastConsumed + 1;
  }
  return { rows, consumed, next: li };
}

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
