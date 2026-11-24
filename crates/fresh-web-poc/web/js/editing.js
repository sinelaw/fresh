// Editing commands over every cursor of a view.
//
// Every edit is a set of replacements, one per cursor, applied as one batch
// (one undo step) from the last position to the first so earlier positions
// stay valid; each caret is then placed where its replacement says.

import { byteLen } from "./ot.js";
import { byteToIdx } from "./buffer.js";
import { sel } from "./view.js";

const PAIRS = { "(": ")", "[": "]", "{": "}", '"': '"', "'": "'", "`": "`" };
const CLOSERS = new Set([")", "]", "}"]);
const COMMENT = {
  rust: "//", javascript: "//", typescript: "//", go: "//", c: "//", cpp: "//", java: "//", css: "/*",
  python: "#", shell: "#", toml: "#", yaml: "#", ruby: "#", markdown: "<!--", html: "<!--",
};
const INDENT = "    ";

export class Editing {
  constructor(app) { this.app = app; }

  // ---------------------------------------------------------------- text queries

  lineAt(b, p) {
    const L = b.text;
    const i = L.indexOf(p);
    if (i < 0) return null;
    return { i, start: L.starts[i], end: L.lineEnd(i), text: L.lines[i], next: i + 1 < L.count ? L.starts[i + 1] : null };
  }
  charBefore(b, p) {
    const ln = this.lineAt(b, p);
    if (!ln || p <= 0) return null;
    const idx = byteToIdx(ln.text, p - ln.start);
    if (idx === 0) return ln.i > 0 || b.text.start < ln.start ? "\n" : null;
    const c = ln.text.charCodeAt(idx - 1);
    return idx >= 2 && c >= 0xdc00 && c < 0xe000 ? ln.text.slice(idx - 2, idx) : ln.text[idx - 1];
  }
  charAt(b, p) {
    if (p >= b.len) return null;
    const ln = this.lineAt(b, p);
    if (!ln) return null;
    const idx = byteToIdx(ln.text, p - ln.start);
    return idx >= ln.text.length ? "\n" : String.fromCodePoint(ln.text.codePointAt(idx));
  }
  isWord(c) { return c != null && /[\p{L}\p{N}_]/u.test(c); }
  wordEdge(b, p, dir) {
    const step = (x) => (dir < 0 ? this.charBefore(b, x) : this.charAt(b, x));
    let q = p;
    while (step(q) != null && !this.isWord(step(q)) && step(q) !== "\n") q += dir * byteLen(step(q));
    while (this.isWord(step(q))) q += dir * byteLen(step(q));
    if (q === p && step(q) != null) q += dir * byteLen(step(q));
    return q;
  }
  wordAt(b, p) {
    let s = p, e = p;
    while (this.isWord(this.charBefore(b, s))) s -= byteLen(this.charBefore(b, s));
    while (this.isWord(this.charAt(b, e))) e += byteLen(this.charAt(b, e));
    return s < e ? { start: s, end: e } : null;
  }
  lang(b) { return b.language || (/\.(md|markdown)$/.test(b.path || "") ? "markdown" : null); }

  // ---------------------------------------------------------------- the core

  // `fn(cursor)` returns {start, end, text, caret?, anchor?} or null; caret
  // and anchor are offsets from `start` in the result (caret defaults to the
  // end of the text). One batch, one undo step (`typing`: see buffer.js).
  replace(pane, fn, typing = null) {
    const v = pane.view;
    const b = v.buf;
    if (b.readOnly) { this.app.setStatus("Read-only buffer"); return false; }
    v.normalize();
    const reps = [];
    for (const c of v.cursors) {
      const r = fn(c);
      if (r) reps.push({ c, ...r });
    }
    if (!reps.length) return false;
    reps.sort((x, y) => y.start - x.start);
    const ops = [];
    for (const r of reps) {
      if (r.end > r.start) ops.push({ t: "del", pos: r.start, len: r.end - r.start });
      if (r.text) ops.push({ t: "ins", pos: r.start, text: r.text });
    }
    const old = v.primary.pos;
    for (const c of v.cursors) c.anchor = null;
    if (ops.length) this.app.applyEdit(b, ops, typing);
    // Final place of each replacement: shifted by every one before it.
    const asc = [...reps].sort((x, y) => x.start - y.start);
    let delta = 0;
    for (const r of asc) {
      const s = r.start + delta;
      const len = byteLen(r.text || "");
      r.c.pos = s + (r.caret ?? len);
      r.c.anchor = r.anchor != null ? s + r.anchor : null;
      r.c.goalX = null;
      delta += len - (r.end - r.start);
    }
    v.normalize();
    pane.reveal();
    if (old !== v.primary.pos) this.app.cursorMoved(v, old);
    pane.scheduleRender();
    return true;
  }

  // Move every cursor with `fn(cursor) -> position`.
  move(pane, fn, extend) {
    const v = pane.view;
    const old = v.primary.pos;
    for (const c of v.cursors) {
      const p = Math.max(0, Math.min(v.buf.len, fn(c)));
      if (extend) { if (c.anchor == null) c.anchor = c.pos; }
      else c.anchor = null;
      c.pos = p;
      c.goalX = null;
    }
    v.normalize();
    pane.reveal();
    if (old !== v.primary.pos) this.app.cursorMoved(v, old);
    pane.scheduleRender();
  }

  // Put the primary cursor at `p` (one cursor), extending its selection.
  place(pane, p, extend) {
    const v = pane.view;
    const c = v.primary;
    const old = c.pos;
    v.cursors = [c];
    if (extend) { if (c.anchor == null) c.anchor = old; }
    else c.anchor = null;
    c.pos = Math.max(0, Math.min(v.buf.len, p));
    c.goalX = null;
    pane.reveal();
    if (old !== c.pos) this.app.cursorMoved(v, old);
    pane.scheduleRender();
  }

  selectWord(pane) {
    const v = pane.view;
    const w = this.wordAt(v.buf, v.primary.pos);
    if (w) { v.primary.anchor = w.start; v.primary.pos = w.end; }
    pane.scheduleRender();
  }

  // ---------------------------------------------------------------- folding

  // The block that starts at window line `li`: the language server's range
  // when it gave one, else the lines indented deeper than it.
  foldRangeAt(b, li) {
    const L = b.text;
    const first = L.firstLine ?? 0;
    const lsp = (b.foldRanges || []).find((r) => r.start === li + first && r.end > r.start);
    if (lsp) return { start: li, end: Math.min(L.count - 1, lsp.end - first) };
    const ind = (t) => /^\s*/.exec(t)[0].length;
    if (L.lines[li].trim() === "") return null;
    const base = ind(L.lines[li]);
    let end = li;
    for (let k = li + 1; k < L.count; k++) {
      if (L.lines[k].trim() === "") continue;
      if (ind(L.lines[k]) <= base) break;
      end = k;
    }
    return end > li ? { start: li, end } : null;
  }

  toggleFoldAt(pane, li) {
    const v = pane.view;
    const i = v.folds.findIndex((f) => f.start === li);
    if (i >= 0) v.folds.splice(i, 1);
    else {
      const r = this.foldRangeAt(v.buf, li);
      if (!r) return;
      v.folds = v.folds.filter((f) => f.start < r.start || f.start > r.end);
      v.folds.push(r);
      // A cursor inside the fold moves to its first line.
      const L = v.buf.text;
      for (const c of v.cursors) { const ci = L.indexOf(c.pos); if (ci > r.start && ci <= r.end) { c.pos = L.lineEnd(r.start); c.anchor = null; } }
    }
    pane.scheduleRender();
  }

  toggleFold(pane) {
    const v = pane.view;
    const L = v.buf.text;
    const li = L.indexOf(v.primary.pos);
    // Unfold the fold the cursor's line heads, else fold the innermost
    // block holding the cursor.
    if (v.folds.some((f) => f.start === li)) return this.toggleFoldAt(pane, li);
    for (let k = li; k >= 0; k--) {
      const r = this.foldRangeAt(v.buf, k);
      if (r && r.end >= li) return this.toggleFoldAt(pane, k);
    }
  }

  foldAll(pane) {
    const v = pane.view;
    const L = v.buf.text;
    v.folds = [];
    for (let k = 0; k < L.count; k++) {
      const r = this.foldRangeAt(v.buf, k);
      if (r && !v.folds.some((f) => k > f.start && k <= f.end)) v.folds.push(r);
    }
    pane.scheduleRender();
  }

  unfoldAll(pane) { pane.view.folds = []; pane.scheduleRender(); }

  // ---------------------------------------------------------------- motion

  left(pane, extend, word) {
    const b = pane.view.buf;
    this.move(pane, (c) => {
      const s = sel(c);
      if (!extend && !word && s) return s.start;
      return word ? this.wordEdge(b, c.pos, -1) : c.pos - byteLen(this.charBefore(b, c.pos) || "");
    }, extend);
  }
  right(pane, extend, word) {
    const b = pane.view.buf;
    this.move(pane, (c) => {
      const s = sel(c);
      if (!extend && !word && s) return s.end;
      return word ? this.wordEdge(b, c.pos, 1) : c.pos + byteLen(this.charAt(b, c.pos) || "");
    }, extend);
  }
  home(pane, extend) {
    const b = pane.view.buf;
    this.move(pane, (c) => {
      const ln = this.lineAt(b, c.pos);
      const indent = ln.start + byteLen(/^\s*/.exec(ln.text)[0]);
      return c.pos === indent ? ln.start : indent;
    }, extend);
  }
  end(pane, extend) {
    const b = pane.view.buf;
    this.move(pane, (c) => this.lineAt(b, c.pos).end, extend);
  }
  vertical(pane, dir, extend) {
    const v = pane.view;
    if (v.cursors.length === 1) return pane.moveRows(dir, extend);
    // Several cursors: each moves a source line, keeping its column.
    const b = v.buf;
    this.move(pane, (c) => {
      const ln = this.lineAt(b, c.pos);
      const L = b.text;
      const j = ln.i + dir;
      if (j < 0 || j >= L.count) return c.pos;
      const col = byteToIdx(ln.text, c.pos - ln.start);
      return L.starts[j] + byteLen(L.lines[j].slice(0, col));
    }, extend);
  }
  paragraph(pane, dir, extend) {
    const b = pane.view.buf;
    const L = b.text;
    this.move(pane, (c) => {
      let i = L.indexOf(c.pos) + dir;
      while (i > 0 && i < L.count - 1 && L.lines[i].trim() !== "") i += dir;
      i = Math.max(0, Math.min(L.count - 1, i));
      return L.starts[i];
    }, extend);
  }
  matchingBracket(pane) {
    const b = pane.view.buf;
    const L = b.text;
    const OPEN = { "(": ")", "[": "]", "{": "}" }, CLOSE = { ")": "(", "]": "[", "}": "{" };
    this.move(pane, (c) => {
      let p = c.pos, ch = this.charAt(b, p);
      if (!(ch in OPEN) && !(ch in CLOSE)) { ch = this.charBefore(b, p); p -= 1; }
      if (!ch || (!(ch in OPEN) && !(ch in CLOSE))) return c.pos;
      const all = L.lines.join("\n");
      let i = byteToIdx(all, p - L.start);
      const fwd = ch in OPEN, want = fwd ? OPEN[ch] : CLOSE[ch];
      let depth = 0;
      for (; i >= 0 && i < all.length; i += fwd ? 1 : -1) {
        if (all[i] === ch) depth++;
        else if (all[i] === want && --depth === 0) return L.start + byteLen(all.slice(0, i));
      }
      return c.pos;
    }, false);
  }

  // ---------------------------------------------------------------- typing

  // Insert `text` at every cursor. `typed`: it was typed (not pasted or
  // inserted by a command), so a run of it is one undo step: the run goes
  // on while each insert starts at the carets the last one left.
  type(pane, text, typed = false) {
    const v = pane.view;
    const b = v.buf;
    const lang = this.lang(b);
    const pairsOn = this.app.settings.auto_close && lang !== "markdown";
    const carets = () => v.cursors.map((c) => c.pos).join();
    const run = typed && !text.includes("\n");
    const typing = !run ? null : this.typed && this.typed.v === v && this.typed.carets === carets() ? "continue" : "start";
    const done = () => { this.typed = run ? { v, carets: carets() } : null; };
    if (text.length === 1) {
      const closer = PAIRS[text];
      // Overtype a closer that is already there.
      if ((CLOSERS.has(text) || text === '"' || text === "'" || text === "`") && v.cursors.every((c) => !sel(c) && this.charAt(b, c.pos) === text) && pairsOn) {
        this.move(pane, (c) => c.pos + 1, false);
        return done();
      }
      if (closer && pairsOn) {
        this.replace(pane, (c) => {
          const s = sel(c);
          if (s) {
            // Surround the selection.
            const inner = b.textRange(s.start, s.end) ?? "";
            return { start: s.start, end: s.end, text: text + inner + closer, caret: 1 + byteLen(inner), anchor: 1 };
          }
          const next = this.charAt(b, c.pos);
          const prev = this.charBefore(b, c.pos);
          const quote = text === closer;
          const ok = (next == null || /\s/.test(next) || CLOSERS.has(next) || next === ",") && !(quote && this.isWord(prev));
          return ok ? { start: c.pos, end: c.pos, text: text + closer, caret: 1 } : { start: c.pos, end: c.pos, text };
        }, typing);
        return done();
      }
    }
    this.replace(pane, (c) => {
      const s = sel(c);
      return s ? { start: s.start, end: s.end, text } : { start: c.pos, end: c.pos, text };
    }, typing);
    done();
  }

  newline(pane) {
    const b = pane.view.buf;
    const lang = this.lang(b);
    this.replace(pane, (c) => {
      const s = sel(c) || { start: c.pos, end: c.pos };
      const ln = this.lineAt(b, s.start);
      const before = ln.text.slice(0, byteToIdx(ln.text, s.start - ln.start));
      let indent = /^[ \t]*/.exec(ln.text)[0];
      if (lang === "markdown") {
        // Continue a list item.
        const m = /^(\s*)([-*+]|\d+[.)])\s+(\[[ x]\]\s+)?/.exec(before);
        if (m) {
          if (before.trim() === m[0].trim()) return { start: ln.start, end: s.end, text: "" };
          const bullet = /\d/.test(m[2]) ? `${parseInt(m[2], 10) + 1}${m[2].slice(-1)}` : m[2];
          return { start: s.start, end: s.end, text: `\n${m[1]}${bullet} ${m[3] ? "[ ] " : ""}` };
        }
        return { start: s.start, end: s.end, text: "\n" + indent };
      }
      const t = before.trimEnd();
      const opens = /[{([]$/.test(t) || (lang === "python" && t.endsWith(":"));
      const next = this.charAt(b, s.end);
      if (opens && next != null && CLOSERS.has(next) && PAIRS[t.slice(-1)] === next) {
        const inner = "\n" + indent + INDENT;
        return { start: s.start, end: s.end, text: inner + "\n" + indent, caret: byteLen(inner) };
      }
      return { start: s.start, end: s.end, text: "\n" + indent + (opens ? INDENT : "") };
    });
  }

  backspace(pane, word) {
    const b = pane.view.buf;
    this.replace(pane, (c) => {
      const s = sel(c);
      if (s) return { start: s.start, end: s.end, text: "" };
      if (c.pos === 0) return null;
      if (word) return { start: this.wordEdge(b, c.pos, -1), end: c.pos, text: "" };
      const ln = this.lineAt(b, c.pos);
      const before = ln.text.slice(0, byteToIdx(ln.text, c.pos - ln.start));
      // In leading whitespace, remove one indent level.
      if (before.length && /^ +$/.test(before)) {
        const n = before.length % 4 || 4;
        return { start: c.pos - n, end: c.pos, text: "" };
      }
      const prev = this.charBefore(b, c.pos), next = this.charAt(b, c.pos);
      if (prev && PAIRS[prev] === next && this.app.settings.auto_close) return { start: c.pos - 1, end: c.pos + 1, text: "" };
      return { start: c.pos - byteLen(prev), end: c.pos, text: "" };
    });
  }

  del(pane, word) {
    const b = pane.view.buf;
    this.replace(pane, (c) => {
      const s = sel(c);
      if (s) return { start: s.start, end: s.end, text: "" };
      const next = this.charAt(b, c.pos);
      if (next == null) return null;
      return { start: c.pos, end: word ? this.wordEdge(b, c.pos, 1) : c.pos + byteLen(next), text: "", caret: 0 };
    });
  }

  // Lines touched by a cursor, its selection included: [first, last] indices.
  lineSpan(b, c) {
    const s = sel(c) || { start: c.pos, end: c.pos };
    const L = b.text;
    let last = L.indexOf(s.end);
    if (s.end > s.start && s.end === L.starts[last] && last > L.indexOf(s.start)) last--;
    return [L.indexOf(s.start), last];
  }

  indent(pane, dir) {
    const v = pane.view;
    const b = v.buf;
    const L = b.text;
    if (dir > 0 && v.cursors.every((c) => !sel(c))) return this.type(pane, INDENT);
    const sels = v.cursors.map((c) => ({ c, s: sel(c), span: this.lineSpan(b, c) }));
    const lines = new Set();
    for (const x of sels) for (let i = x.span[0]; i <= x.span[1]; i++) lines.add(i);
    const ops = [];
    for (const i of [...lines].sort((a, z) => z - a)) {
      if (dir > 0) ops.push({ t: "ins", pos: L.starts[i], text: INDENT });
      else {
        const n = /^ {0,4}/.exec(L.lines[i])[0].length || (L.lines[i].startsWith("\t") ? 1 : 0);
        if (n) ops.push({ t: "del", pos: L.starts[i], len: n });
      }
    }
    if (ops.length) this.app.applyEdit(b, ops);
    pane.scheduleRender();
  }

  // ---------------------------------------------------------------- line commands

  selectLine(pane) {
    const b = pane.view.buf;
    const L = b.text;
    for (const c of pane.view.cursors) {
      const [a, z] = this.lineSpan(b, c);
      c.anchor = L.starts[a];
      c.pos = z + 1 < L.count ? L.starts[z + 1] : L.lineEnd(z);
    }
    pane.scheduleRender();
  }

  deleteLine(pane) {
    const b = pane.view.buf;
    const L = b.text;
    this.replace(pane, (c) => {
      const [a, z] = this.lineSpan(b, c);
      const end = z + 1 < L.count ? L.starts[z + 1] : L.lineEnd(z);
      const start = z + 1 < L.count || a === 0 ? L.starts[a] : L.starts[a] - 1;
      return { start, end, text: "", caret: 0 };
    });
  }

  duplicateLine(pane) {
    const b = pane.view.buf;
    const L = b.text;
    this.replace(pane, (c) => {
      const [a, z] = this.lineSpan(b, c);
      const text = L.lines.slice(a, z + 1).join("\n");
      const col = c.pos - L.starts[a];
      return { start: L.lineEnd(z), end: L.lineEnd(z), text: "\n" + text, caret: 1 + col };
    });
  }

  moveLines(pane, dir) {
    const v = pane.view;
    const b = v.buf;
    const L = b.text;
    if (v.cursors.length > 1) return;
    const c = v.cursors[0];
    const [a, z] = this.lineSpan(b, c);
    if ((dir < 0 && a === 0) || (dir > 0 && z >= L.count - 1)) return;
    const block = L.lines.slice(a, z + 1).join("\n");
    const off = c.pos - L.starts[a];
    const anchorOff = c.anchor != null ? c.anchor - L.starts[a] : null;
    let start, end, text, newStart;
    if (dir < 0) {
      const prev = L.lines[a - 1];
      start = L.starts[a - 1]; end = L.lineEnd(z);
      text = block + "\n" + prev;
      newStart = start;
    } else {
      const next = L.lines[z + 1];
      start = L.starts[a]; end = L.lineEnd(z + 1);
      text = next + "\n" + block;
      newStart = start + byteLen(next) + 1;
    }
    this.app.applyEdit(b, [{ t: "del", pos: start, len: end - start }, { t: "ins", pos: start, text }]);
    c.pos = newStart + off;
    c.anchor = anchorOff != null ? newStart + anchorOff : null;
    pane.reveal();
    pane.scheduleRender();
  }

  toggleComment(pane) {
    const v = pane.view;
    const b = v.buf;
    const L = b.text;
    const prefix = COMMENT[this.lang(b)] || "//";
    const close = prefix === "<!--" ? " -->" : prefix === "/*" ? " */" : "";
    const lines = new Set();
    for (const c of v.cursors) { const [a, z] = this.lineSpan(b, c); for (let i = a; i <= z; i++) lines.add(i); }
    const idx = [...lines].filter((i) => L.lines[i].trim() !== "");
    if (!idx.length) return;
    const commented = idx.every((i) => L.lines[i].trimStart().startsWith(prefix));
    const minIndent = Math.min(...idx.map((i) => /^\s*/.exec(L.lines[i])[0].length));
    const ops = [];
    for (const i of idx.sort((a, z) => z - a)) {
      const t = L.lines[i];
      if (commented) {
        if (close && t.trimEnd().endsWith(close.trim())) {
          const e = byteLen(t.trimEnd()) - byteLen(close.trim());
          const sp = t[byteToIdx(t, e) - 1] === " " ? 1 : 0;
          ops.push({ t: "del", pos: L.starts[i] + e - sp, len: byteLen(close.trim()) + sp });
        }
        const at = t.indexOf(prefix);
        const n = prefix.length + (t[at + prefix.length] === " " ? 1 : 0);
        ops.push({ t: "del", pos: L.starts[i] + byteLen(t.slice(0, at)), len: n });
      } else {
        if (close) ops.push({ t: "ins", pos: L.lineEnd(i), text: close });
        ops.push({ t: "ins", pos: L.starts[i] + byteLen(t.slice(0, minIndent)), text: prefix + " " });
      }
    }
    // Ops above are per line, last line first, so each stays valid.
    this.app.applyEdit(b, ops);
    pane.scheduleRender();
  }

  // ---------------------------------------------------------------- multiple cursors

  addCursorVertical(pane, dir) {
    const v = pane.view;
    const b = v.buf;
    const L = b.text;
    const edge = dir < 0 ? v.cursors.reduce((m, c) => (c.pos < m.pos ? c : m)) : v.cursors.reduce((m, c) => (c.pos > m.pos ? c : m));
    const ln = this.lineAt(b, edge.pos);
    const j = ln.i + dir;
    if (j < 0 || j >= L.count) return;
    const col = byteToIdx(ln.text, edge.pos - ln.start);
    v.cursors.push({ pos: L.starts[j] + byteLen(L.lines[j].slice(0, Math.min(col, L.lines[j].length))), anchor: null, goalX: null });
    v.normalize();
    pane.scheduleRender();
  }

  selectNextOccurrence(pane) {
    const v = pane.view;
    const b = v.buf;
    const prim = v.cursors[v.cursors.length - 1];
    const s = sel(v.cursors[0]);
    if (!s) {
      const w = this.wordAt(b, v.primary.pos);
      if (w) { v.primary.anchor = w.start; v.primary.pos = w.end; }
      pane.scheduleRender();
      return;
    }
    const needle = b.textRange(s.start, s.end);
    if (!needle) return;
    const L = b.text;
    const all = L.lines.join("\n");
    const lastSel = sel(prim) || s;
    let i = all.indexOf(needle, byteToIdx(all, lastSel.end - L.start));
    if (i < 0) i = all.indexOf(needle);
    if (i < 0) return;
    const start = L.start + byteLen(all.slice(0, i));
    if (v.cursors.some((c) => Math.min(c.pos, c.anchor ?? c.pos) === start)) { this.app.setStatus("All occurrences selected"); return; }
    v.cursors.push({ pos: start + byteLen(needle), anchor: start, goalX: null });
    v.normalize();
    pane.reveal();
    pane.scheduleRender();
  }

  selectAllOccurrences(pane) {
    const v = pane.view;
    const b = v.buf;
    let s = sel(v.cursors[0]);
    if (!s) { const w = this.wordAt(b, v.primary.pos); if (!w) return; s = w; }
    const needle = b.textRange(s.start, s.end);
    const L = b.text;
    const all = L.lines.join("\n");
    const out = [];
    for (let i = all.indexOf(needle); i >= 0; i = all.indexOf(needle, i + needle.length)) {
      const start = L.start + byteLen(all.slice(0, i));
      out.push({ pos: start + byteLen(needle), anchor: start, goalX: null });
    }
    if (!out.length) return;
    const first = out.findIndex((c) => c.anchor === s.start);
    if (first > 0) out.unshift(out.splice(first, 1)[0]);
    v.cursors = out;
    this.app.setStatus(`${out.length} occurrences selected`);
    pane.scheduleRender();
  }
}

