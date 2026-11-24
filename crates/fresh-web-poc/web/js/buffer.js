// Buffers as this client holds them.
//
// A file buffer is a replica of a server document: the whole text for an
// ordinary file, a window of lines (refetched as the view moves) for a huge
// one. Edits are applied here at once and sent to the server against the
// last log version seen; the server orders them, and every client applies
// the log in that order, transforming its own unconfirmed edits past
// whatever arrived first. Undo is this client's own: the inverse of its own
// edits, carried through everyone else's.
//
// A virtual buffer is local to this client (a plugin's panel): its text and
// per-entry properties live here and nowhere else.

import { rpc } from "./rpc.js";
import { byteLen, transformSeqs, shiftPos } from "./ot.js";
import { Decor } from "./decor.js";

// A window: this many lines before and after the line asked for. A file
// shorter than that arrives whole in its first window; nothing else differs.
const WINDOW_BEFORE = 2000;
const WINDOW_AFTER = 6000;

let nextBufferId = 1;

// Typing a run of characters is one undo step. The editor marks typing
// edits with `typing`: "start" for the first of a run, "continue" for one
// that goes on where the last left the carets. A continuing edit joins the
// entry before it when that one was typing too, within a second.
function pushUndo(buf, inverse, typing) {
  const now = Date.now();
  const last = buf.undo[buf.undo.length - 1];
  if (typing === "continue" && buf.typingRun && last && now - buf.lastUndoAt < 1000) last.unshift(...inverse);
  else buf.undo.push(inverse);
  buf.typingRun = !!typing;
  buf.lastUndoAt = now;
}

export function nl(s) {
  let n = 0;
  for (let i = 0; i < s.length; i++) if (s.charCodeAt(i) === 10) n++;
  return n;
}

// UTF-16 index of byte offset `b` within `s`.
export function byteToIdx(s, b) {
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
export function idxToByte(s, idx) { return byteLen(s.slice(0, idx)); }

// The byte start of each line, for lines of byte lengths `lens` starting
// at byte `start`.
function lineStarts(start, lens) {
  const starts = [];
  let b = start;
  for (const n of lens) {
    starts.push(b);
    b += n + 1;
  }
  return starts;
}

// The text of a buffer as lines, with each line's byte length cached and
// its byte start kept. An edit re-measures only the lines it touches; the
// starts after them move by a sum, not a re-measure.
class Lines {
  constructor(start, text, bof, eof, firstLine) {
    this.start = start;
    this.bof = bof;
    this.eof = eof;
    this.firstLine = firstLine; // null when the document has no line index
    const lines = text.split("\n");
    // A trailing empty piece after the last newline is a real (empty) line
    // only at the end of the document; otherwise the next line is unloaded.
    if (!eof && lines.length > 1 && lines[lines.length - 1] === "") lines.pop();
    this.lines = lines;
    this.lens = lines.map(byteLen);
    this.starts = lineStarts(start, this.lens);
  }
  recount(from) {
    let b = from === 0 ? this.start : this.starts[from - 1] + this.lens[from - 1] + 1;
    const { starts, lens } = this;
    starts.length = lens.length;
    for (let i = from; i < lens.length; i++) {
      starts[i] = b;
      b += lens[i] + 1;
    }
  }
  // Every start moves by `d` (an edit before the window).
  move(d) {
    this.start += d;
    const { starts } = this;
    for (let i = 0; i < starts.length; i++) starts[i] += d;
  }
  get count() { return this.lines.length; }
  // End of the covered bytes: the end of the document when eof.
  end(docLen) {
    if (this.eof) return docLen;
    const k = this.lines.length - 1;
    return this.starts[k] + this.lens[k] + 1;
  }
  // Index of the line holding byte `b`, or -1 outside.
  indexOf(b) {
    const { starts, lens } = this;
    let hi = starts.length - 1;
    if (b < this.start || b > starts[hi] + lens[hi] + (this.eof ? 0 : 1)) return -1;
    let lo = 0;
    while (lo < hi) {
      const mid = (lo + hi + 1) >> 1;
      if (starts[mid] <= b) lo = mid; else hi = mid - 1;
    }
    return lo;
  }
  lineEnd(i) { return this.starts[i] + this.lens[i]; }
  hasNewline(i) { return i < this.lines.length - 1 || !this.eof; }
  text(s, e) {
    const a = this.indexOf(s), b = this.indexOf(e);
    if (a < 0 || b < 0) return null;
    if (a === b) { const l = this.lines[a]; return l.slice(byteToIdx(l, s - this.starts[a]), byteToIdx(l, e - this.starts[a])); }
    const seg = this.lines.slice(a, b + 1).join("\n");
    const off = this.starts[a];
    return seg.slice(byteToIdx(seg, s - off), byteToIdx(seg, e - off));
  }
  // Splice a primitive that lies within the window.
  splice(op) {
    const s = op.pos, e = op.t === "del" ? op.pos + op.len : op.pos;
    const a = this.indexOf(s), b = this.indexOf(e);
    const seg = a === b ? this.lines[a] : this.lines.slice(a, b + 1).join("\n");
    const off = this.starts[a];
    const si = byteToIdx(seg, s - off), ei = byteToIdx(seg, e - off);
    const out = (seg.slice(0, si) + (op.t === "ins" ? op.text : "") + seg.slice(ei)).split("\n");
    this.lines.splice(a, b - a + 1, ...out);
    this.lens.splice(a, b - a + 1, ...out.map(byteLen));
    this.recount(a);
  }
}

export class Buffer {
  constructor(kind) {
    this.id = nextBufferId++;
    this.kind = kind;
    this.decor = new Decor();
    this.views = new Set();
    this.undo = [];
    this.redo = [];
    this.props = new Map();
    this.listeners = new Set();
  }
  emit(what, data) { for (const f of this.listeners) f(what, data); }
  get isVirtual() { return this.kind === "virtual"; }
  lineCount() { return this.text ? (this.text.firstLine != null && this.text.eof && this.text.bof ? this.text.count : this.totalLines) : null; }

  // Move everything anchored in this buffer through `op`: text, decorations,
  // cursors in every view, and undo entries.
  moveThrough(op) {
    this.decor.shift(op);
    for (const v of this.views) v.shift(op);
  }

  // Apply a primitive to the held text. Returns the deleted text when the
  // window held it.
  applyText(op) {
    this.textGen = (this.textGen || 0) + 1;
    const L = this.text;
    const n = op.t === "ins" ? byteLen(op.text) : op.len;
    let gone = null;
    if (L) {
      const ws = L.start, we = L.end(this.len);
      const end = op.t === "del" ? op.pos + op.len : op.pos;
      const lines = op.t === "ins" ? nl(op.text) : op.nl ?? 0;
      if (op.t === "ins" ? op.pos < ws : end <= ws) {
        // Wholly before the window.
        L.move(op.t === "ins" ? n : -n);
        if (L.firstLine != null) L.firstLine += op.t === "ins" ? lines : -lines;
      } else if (op.pos > we || (op.pos === we && !L.eof)) {
        // Wholly after it.
      } else if (op.pos >= ws && (end < we || (end <= we && L.eof))) {
        if (op.t === "del") gone = L.text(op.pos, end);
        L.splice(op);
      } else {
        this.text = null; // straddles the edge: fetch again
      }
    }
    this.len += op.t === "ins" ? n : -n;
    if (this.totalLines != null) {
      this.totalLines += op.t === "ins" ? nl(op.text) : -(gone != null ? nl(gone) : op.nl ?? 0);
    }
    return gone;
  }

  textRange(s, e) { return this.text ? this.text.text(s, Math.min(e, this.len)) : null; }
  get readOnly() { return !!this._readOnly; }
}

// ---------------------------------------------------------------- file

export class FileBuffer extends Buffer {
  constructor(info) {
    super("file");
    this.doc = info.doc;
    this.path = info.path;
    this.name = info.path.split("/").pop();
    this.len = info.len;
    this.version = info.version;
    this.large = info.large;
    this.language = info.language;
    this.modified = info.modified;
    this.encoding = info.encoding;
    this.lineEnding = info.line_ending;
    this._readOnly = !!info.readonly;
    this.totalLines = info.lines;
    this.text = null;
    this.inflight = null;
    this.pending = [];
    this.recent = []; // [{version, ops}] as sequenced, for windows fetched late
    this.waiting = null;
    this.fetching = null;
  }

  // The server's word on the document's file (after a rename or a delete):
  // its path, name, language and whether it is unsaved.
  setInfo(info) {
    this.path = info.path;
    this.name = info.path.split("/").pop();
    this.language = info.language;
    this.modified = info.modified;
    this.encoding = info.encoding;
    this.lineEnding = info.line_ending;
  }

  // The server replaced the document (a huge file reloaded from disk): a
  // new log from `info.version`. Edits not yet sequenced are dropped (the
  // server refuses them as based on the old log), undo starts over, and
  // the window is fetched again.
  reloaded(info) {
    this.setInfo(info);
    this.version = info.version;
    this.len = info.len;
    this.large = info.large;
    this.totalLines = info.lines;
    this.inflight = null;
    this.pending = [];
    this.recent = [];
    this.undo = [];
    this.redo = [];
    this.text = null;
    for (const v of this.views) {
      for (const c of v.cursors) { c.pos = Math.min(c.pos, this.len); c.anchor = null; }
      v.setTop(0);
    }
    const v = [...this.views][0];
    return this.ensure(v ? v.primary.pos : 0);
  }

  // The held text covers the whole document.
  get full() { const L = this.text; return !!L && L.bof && L.eof; }

  // Make sure the window holds the line at `pos` and `after` lines past it.
  // Resolves when it does (immediately when it already does).
  ensure(pos, after = 200) {
    const L = this.text;
    if (L) {
      const i = L.indexOf(pos);
      if (i >= 0 && (L.eof || L.count - i > after) && (L.bof || i > 50)) return Promise.resolve();
    }
    if (this.fetching) return this.fetching;
    const args = { doc: this.doc, around: pos, before: WINDOW_BEFORE, after: Math.max(WINDOW_AFTER, after * 2) };
    this.fetching = (async () => {
      for (;;) {
        // A window is the server's text; with edits of ours not yet in it,
        // wait for them to be confirmed.
        while (this.inflight || this.pending.length) await new Promise((r) => setTimeout(r, 20));
        const w = await rpc("doc.window", args);
        if (this.inflight || this.pending.length) continue;
        while (w.version > this.version) await new Promise((r) => setTimeout(r, 10));
        const L2 = new Lines(w.start, w.text, w.bof, w.eof, w.first_line);
        // Replay what we applied since the version the window was read at.
        const savedLen = this.len;
        this.len = w.len;
        for (const e of this.recent) {
          if (e.version <= w.version) continue;
          for (const op of e.ops) {
            const tmp = this.text;
            this.text = L2;
            const tl = this.totalLines;
            this.applyText(op);
            this.totalLines = tl;
            this.text = tmp;
          }
        }
        this.len = savedLen;
        this.text = L2;
        this.textGen = (this.textGen || 0) + 1;
        break;
      }
      this.fetching = null;
      this.emit("text");
    })();
    return this.fetching;
  }

  // A local edit: apply now, send when nothing is in flight.
  edit(ops, { undoable = true, typing = null } = {}) {
    const inverse = [];
    this.modified = true; // before the edit events, which redraw the tabs
    for (const op of ops) {
      const gone = op.t === "del" ? this.textRange(op.pos, op.pos + op.len) : null;
      if (op.t === "del") op.nl = gone != null ? nl(gone) : 0;
      this.applyText(op);
      this.moveThrough(op);
      inverse.unshift(op.t === "ins" ? { t: "del", pos: op.pos, len: byteLen(op.text) } : { t: "ins", pos: op.pos, text: gone ?? "" });
      this.emit("edit", { op, gone, local: true });
    }
    if (undoable) { pushUndo(this, inverse, typing); this.redo = []; }
    this.lastEdit = Date.now();
    this.pending.push(...ops.map((o) => ({ ...o })));
    this.flush();
    return inverse;
  }

  flush() {
    if (this.inflight || !this.pending.length) return;
    this.inflight = this.pending;
    this.pending = [];
    const ops = this.inflight.map(({ t, pos, text, len }) => (t === "ins" ? { t, pos, text } : { t, pos, len }));
    rpc("doc.edit", { doc: this.doc, base: this.version, ops }).catch((e) => {
      console.error(e);
      this.emit("error", e);
    });
  }

  // An event from the log, in order.
  onOps(ev, me) {
    if (ev.client === me) {
      // Our own edit, sequenced: the acknowledgement.
      this.version = ev.version;
      this.recent.push({ version: ev.version, ops: ev.ops });
      this.inflight = null;
      this.modified = ev.modified;
      this.flush();
    } else {
      let remote = ev.ops;
      // Ours are sequenced after it: theirs win ties.
      if (this.inflight) [this.inflight, remote] = transformSeqs(this.inflight, remote, false);
      if (this.pending.length) [this.pending, remote] = transformSeqs(this.pending, remote, false);
      for (const op of remote) {
        const gone = this.applyText(op);
        this.moveThrough(op);
        this.undo = this.undo.map((u) => transformSeqs(u, [op], false)[0]);
        this.redo = this.redo.map((u) => transformSeqs(u, [op], false)[0]);
        this.emit("edit", { op, gone, local: false });
      }
      this.version = ev.version;
      this.recent.push({ version: ev.version, ops: ev.ops });
      this.modified = ev.modified;
    }
    if (this.recent.length > 2000) this.recent.splice(0, this.recent.length - 2000);
  }

  doUndo(stack = this.undo, other = this.redo) {
    const ops = stack.pop();
    if (!ops) return false;
    this.typingRun = false;
    const inv = this.edit(ops.map((o) => ({ ...o })), { undoable: false });
    other.push(inv);
    return ops;
  }
  doRedo() { return this.doUndo(this.redo, this.undo); }

  // `force`: overwrite a file that changed on disk since it was loaded.
  async save({ force = false } = {}) {
    while (this.inflight || this.pending.length) await new Promise((r) => setTimeout(r, 20));
    const info = await rpc("doc.save", { doc: this.doc, force });
    this.modified = info.modified;
    return info;
  }

  // Text for a plugin's getBufferText: local when held, else the server's.
  async textAsync(s, e) {
    const t = this.textRange(s, e);
    if (t != null) return t;
    return (await rpc("doc.text", { doc: this.doc, start: s, end: e })).text;
  }
}

// ---------------------------------------------------------------- virtual

export class VirtualBuffer extends Buffer {
  constructor(name, opts = {}) {
    super("virtual");
    this.name = name;
    this.path = "";
    this.mode = opts.mode || null;
    this._readOnly = !!opts.readOnly;
    this.len = 0;
    this.entries = [];
    this.setEntries([]);
    this.modified = false;
    this.totalLines = 1;
  }
  setEntries(entries) {
    this.entries = entries.map((e) => ({ ...e }));
    let text = "";
    this.ranges = [];
    for (const e of this.entries) {
      const s = byteLen(text);
      text += e.text || "";
      this.ranges.push({ start: s, end: byteLen(text), props: e.props || {}, style: e.style, inline: e.inlineOverlays });
    }
    this.len = byteLen(text);
    this.text = new Lines(0, text, true, true, 0);
    this.textGen = (this.textGen || 0) + 1;
    this.offered = new Set();
    this.totalLines = this.text.count;
    this.decor.clearNamespace("entry-style");
    for (const r of this.ranges) {
      if (r.style) this.decor.add("overlays", { ns: "entry-style", start: r.start, end: r.end, style: r.style, when: null });
    }
    this.emit("text");
  }
  propsAt(pos) {
    return this.ranges.filter((r) => pos >= r.start && (pos < r.end || (pos === r.end && r.end === this.len))).map((r) => r.props);
  }
  get full() { return true; }
  ensure() { return Promise.resolve(); }
  edit(ops, { undoable = true, typing = null } = {}) {
    const inverse = [];
    for (const op of ops) {
      const gone = op.t === "del" ? this.textRange(op.pos, op.pos + op.len) : null;
      this.applyText(op);
      this.moveThrough(op);
      inverse.unshift(op.t === "ins" ? { t: "del", pos: op.pos, len: byteLen(op.text) } : { t: "ins", pos: op.pos, text: gone ?? "" });
      this.emit("edit", { op, gone, local: true });
    }
    if (undoable) { pushUndo(this, inverse, typing); this.redo = []; }
    return inverse;
  }
  doUndo(stack = this.undo, other = this.redo) {
    const ops = stack.pop();
    if (!ops) return false;
    this.typingRun = false;
    other.push(this.edit(ops, { undoable: false }));
    return ops;
  }
  doRedo() { return this.doUndo(this.redo, this.undo); }
  async textAsync(s, e) { return this.textRange(s, e) ?? ""; }
  async save() { return {}; }
}

export { shiftPos };
