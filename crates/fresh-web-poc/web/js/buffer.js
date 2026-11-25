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
import { some, none } from "./maybe.js";
import { byteLen, transformSeqs } from "./ot.js";
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
  if (typing === "continue" && buf.typingRun && last !== undefined && now - buf.lastUndoAt < 1000) buf.undo = [...buf.undo.slice(0, -1), [...inverse, ...last]];
  else buf.undo = [...buf.undo, inverse];
  buf.typingRun = typing !== "none";
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
/** function lineStarts(start: Number, lens: Number[]) => Number[] */
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
// A window of a document's text: it starts at byte `start`, at the
// document's start (`bof`) or end (`eof`) or not, at document line
// `firstLine` (null: the document has no line index).
/** type Window = {start: Number, text: String, bof: Boolean, eof: Boolean, firstLine: Number | Null} */
/** function windowOf(w: Window) => Window */
function windowOf(w) { return w; }
/** function asOp(o: Op) => Op */
function asOp(o) { return o; }
// No text there (outside the window).
/** const NO_TEXT: String | Null */
const NO_TEXT = null;

class Lines {
  constructor(window) {
    const w = windowOf(window);
    this.start = w.start;
    this.bof = w.bof;
    this.eof = w.eof;
    this.firstLine = w.firstLine;
    const lines = w.text.split("\n");
    // A trailing empty piece after the last newline is a real (empty) line
    // only at the end of the document; otherwise the next line is unloaded.
    if (!w.eof && lines.length > 1 && lines[lines.length - 1] === "") lines.pop();
    this.lines = lines;
    this.lens = lines.map((t) => byteLen(t));
    this.starts = lineStarts(w.start, this.lens);
  }
  recount(from) {
    const b = from === 0 ? this.start : this.starts[from - 1] + this.lens[from - 1] + 1;
    this.starts = [...this.starts.slice(0, from), ...lineStarts(b, this.lens.slice(from))];
  }
  // Every start moves by `d` (an edit before the window).
  move(d) {
    this.start += d;
    const { starts } = this;
    for (let i = 0; i < starts.length; i++) starts[i] += d;
  }
  get count() { return this.lines.length; }
  // End of the covered bytes, short of the document's end (past the last
  // held line's newline).
  heldEnd() {
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
    if (a < 0 || b < 0) return NO_TEXT;
    if (a === b) { const l = this.lines[a]; return some(l.slice(byteToIdx(l, s - this.starts[a]), byteToIdx(l, e - this.starts[a]))); }
    const seg = this.lines.slice(a, b + 1).join("\n");
    const off = this.starts[a];
    return some(seg.slice(byteToIdx(seg, s - off), byteToIdx(seg, e - off)));
  }
  // Splice a primitive that lies within the window.
  splice(o) {
    const op = asOp(o);
    const s = op.pos, e = op.t === "del" ? op.pos + op.len : op.pos;
    const a = this.indexOf(s), b = this.indexOf(e);
    const seg = a === b ? this.lines[a] : this.lines.slice(a, b + 1).join("\n");
    const off = this.starts[a];
    const si = byteToIdx(seg, s - off), ei = byteToIdx(seg, e - off);
    const out = (seg.slice(0, si) + (op.t === "ins" ? op.text : "") + seg.slice(ei)).split("\n");
    this.lines = [...this.lines.slice(0, a), ...out, ...this.lines.slice(b + 1)];
    this.lens = [...this.lens.slice(0, a), ...out.map((t) => byteLen(t)), ...this.lens.slice(b + 1)];
    this.recount(a);
  }
}

// A buffer: its text (all of it, or a window of it), decorations, views,
// undo and listeners, whatever its kind. What differs by kind is its
// source: a `FileSource` (a server document) or a `VirtualSource` (a
// plugin's entries). Make one with `fileBuffer` or `virtualBuffer`.
// The newlines a deletion took: in its text when it was held, else as
// the server counted them (`counted`).
/** function linesGone(gone: String | Null, counted: Number) => Number */
function linesGone(gone, counted) { return gone === null ? counted : nl(gone); }

// An insertion, a deletion; an op as the server takes it (no more).
/** function insOp(pos: Number, text: String) => Op */
function insOp(pos, text) { return { t: "ins", pos, text }; }
/** function delOp(pos: Number, len: Number) => Op */
function delOp(pos, len) { return { t: "del", pos, len }; }
// What an op does: whether it inserts, the range it covers in the text
// before it, the bytes it adds (negative: removes) and the lines
// (`deleted`: a deletion's newlines, counted elsewhere).
/** type Reach = {ins: Boolean, pos: Number, end: Number, bytes: Number, lines: Number} */
/** function reachOf(op: Op, deleted: Number) => Reach */
function reachOf(op, deleted) {
  return op.t === "ins"
    ? { ins: true, pos: op.pos, end: op.pos, bytes: byteLen(op.text), lines: nl(op.text) }
    : { ins: false, pos: op.pos, end: op.pos + op.len, bytes: -op.len, lines: -deleted };
}
// A document log's entries as this client saw them sequenced.
/** type LogEntry = {version: Number, ops: LoggedOp[]} */
/** function noLog() => LogEntry[] */
function noLog() { return []; }

// The range a deletion removes (a list of at most one), and the op that
// undoes `op` (`gone`: the text it deleted).
/** function deletedRanges(op: Op) => Range[] */
function deletedRanges(op) { return op.t === "del" ? [{ start: op.pos, end: op.pos + op.len }] : []; }
/** function inverseOf(op: Op, gone: String | Null) => Op */
function inverseOf(op, gone) { return op.t === "ins" ? delOp(op.pos, byteLen(op.text)) : insOp(op.pos, gone ?? ""); }
/** function plainOp(o: LoggedOp) => Op */
function plainOp(o) { return o.t === "ins" ? insOp(o.pos, o.text) : delOp(o.pos, o.len); }
// What a buffer tells its listeners: its held text changed (fetched or
// replaced), an edit (here or another client's) and what it deleted
// where the text was held, or a failure.
/** type BufferNews = {kind: "text"} | {kind: "edit", op: Op, gone: String | Null, local: Boolean} | {kind: "error", message: String} */
/** const TEXT_NEWS: BufferNews */
const TEXT_NEWS = { kind: "text" };
/** function editNews(op: Op, gone: String | Null, local: Boolean) => BufferNews */
function editNews(op, gone, local) { return { kind: "edit", op, gone, local }; }
/** function errorNews(message: String) => BufferNews */
function errorNews(message) { return { kind: "error", message }; }
/** function wireOp(o: Op) => Op */
function wireOp(o) { return o.t === "ins" ? insOp(o.pos, o.text) : delOp(o.pos, o.len); }

// A count, if known, moved by `d`.
/** function movedBy(n: Number | Null, d: Number) => Number | Null */
function movedBy(n, d) { return n === null ? null : n + d; }

// The held text as a list of at most one.
/** function heldList<A>(L: A | Null) => A[] */
function heldList(L) { return L === null ? [] : [L]; }

export class Buffer {
  constructor(source) {
    this.id = nextBufferId++;
    this.source = source;
    this.name = "";
    this.path = "";
    this.untitled = none(); // an untitled document's number (Untitled-N)
    // What the server says of a file (`setInfo`); a virtual buffer's
    // language is its creator's.
    this.language = none();
    this.encoding = none();
    this.lineEnding = none();
    this.large = false; // read from disk as it is viewed, see FileSource
    this.modified = false;
    this.readOnly = false;
    this.mode = none(); // the plugin mode whose keys apply (a virtual buffer's)
    this.text = none(); // the held text (none until fetched, and again)
    this.textGen = 0; // bumped on every change to the held text
    this.len = 0;
    this.totalLines = none(); // none while a huge file has no line index
    this.decor = new Decor();
    this.views = new Set();
    this.undo = [];
    this.redo = [];
    this.typingRun = false;
    this.lastUndoAt = 0;
    this.lastEdit = 0;
    this.listeners = new Set();
    // Kept by the app and plugins.
    this.keep = false; // stays open while no view shows it
    this.discard = false; // closing gives up its unsaved changes
    this.keepMine = false; // the next save overwrites a change on disk
    this.foldRanges = none(); // a language server's, as line ranges
  }
  emit(news) { for (const f of this.listeners) f(news); }
  // Not a document: a plugin's text, or a terminal.
  get isVirtual() { return this.source.kind !== "file"; }
  // The terminal it is the tab of: a list of at most one id.
  get terminal() { return this.source.kind === "terminal" ? [this.source.term] : []; }
  // The server document's id; null for a virtual buffer.
  get doc() { return this.source.doc; }
  // The held text covers the whole document.
  get full() { return heldList(this.text).some((L) => L.bof && L.eof); }

  // The server's word on the document's file (after an open, a save, a
  // Save As, a rename or a delete): its path (none while untitled), name,
  // language, format and whether it is unsaved.
  setInfo(info) {
    const path = info.path ?? "";
    this.path = path;
    this.untitled = info.untitled;
    this.name = path !== "" ? path.slice(path.lastIndexOf("/") + 1) : `Untitled-${info.untitled ?? ""}`;
    this.language = info.language;
    this.modified = info.modified;
    this.encoding = some(info.encoding);
    this.lineEnding = some(info.line_ending);
  }

  // Move everything anchored in this buffer through `op`: text, decorations,
  // cursors in every view, and undo entries.
  moveThrough(op) {
    this.decor.shift(op);
    for (const v of this.views) v.shift(op);
  }

  // Apply a primitive to the held text (`deleted`: the newlines it deleted,
  // for a deletion outside the window). Returns the deleted text when the
  // window held it.
  applyText(op, deleted) {
    this.textGen++;
    const r = reachOf(op, deleted);
    let gone = NO_TEXT;
    for (const L of heldList(this.text)) {
      const ws = L.start, we = L.eof ? this.len : L.heldEnd();
      if (r.ins ? r.pos < ws : r.end <= ws) {
        // Wholly before the window.
        L.move(r.bytes);
        L.firstLine = movedBy(L.firstLine, r.lines);
      } else if (r.pos > we || (r.pos === we && !L.eof)) {
        // Wholly after it.
      } else if (r.pos >= ws && (r.end < we || (r.end <= we && L.eof))) {
        if (!r.ins) gone = L.text(r.pos, r.end);
        L.splice(op);
      } else {
        this.text = none(); // straddles the edge: fetch again
      }
    }
    this.len += r.bytes;
    this.totalLines = movedBy(this.totalLines, r.ins ? r.lines : -linesGone(gone, deleted));
    return gone;
  }

  textRange(s, e) { return heldList(this.text).map((L) => L.text(s, Math.min(e, this.len))).at(0) ?? NO_TEXT; }
  // Text for a plugin's getBufferText: local when held, else the source's.
  async textAsync(s, e) {
    const t = this.textRange(s, e);
    if (t != null) return t;
    return this.source.fetchText(s, e);
  }

  // Make sure the held text has the line at `pos` and `after` lines past
  // it. Resolves when it does (immediately when it already does).
  ensure(pos, after = 200) { return this.source.ensure(this, pos, after); }

  // A local edit: applied now, and passed to the source (a file's goes to
  // the server). `undoable`: an undo step (typing joins the last when
  // `typing` is "continue"; "start" begins a run; "none": not typing).
  edit(ops, undoable, typing) {
    const inverse = [];
    if (this.doc) this.modified = true; // before the edit events, which redraw the tabs
    for (const op of ops) {
      const gone = deletedRanges(op).map((r) => this.textRange(r.start, r.end)).at(0) ?? NO_TEXT;
      this.applyText(op, linesGone(gone, 0));
      this.moveThrough(op);
      inverse.unshift(inverseOf(op, gone));
      this.emit(editNews(op, gone, true));
    }
    if (undoable) { pushUndo(this, inverse, typing); this.redo = []; }
    this.lastEdit = Date.now();
    this.source.send(this, ops);
    return inverse;
  }

  // Undo the last step (redo: from the redo stack): the ops it undid (none
  // when there was nothing to undo).
  doUndo(stack = this.undo, other = this.redo) {
    const ops = stack.pop();
    if (ops === undefined) return [];
    this.typingRun = false;
    const inv = this.edit(ops.map(wireOp), false, "none");
    other.push(inv);
    return ops;
  }
  doRedo() { return this.doUndo(this.redo, this.undo); }

  // The properties of the entries at `pos` (a virtual buffer's; a file
  // has none).
  propsAt(pos) { return this.source.propsAt(pos, this.len); }

  // ---- a virtual buffer's

  // Replace the text with `entries`: [{text, props, style}].
  setEntries(entries) { this.source.setEntries(this, entries); }

  // ---- a file buffer's

  // The document log version this buffer has seen.
  get version() { return this.source.version; }
  // Every local edit has been sequenced by the server.
  get synced() { return this.source.synced; }
  // An event from the document's log, in order.
  onOps(ev, me) { this.source.onOps(this, ev, me); }
  // The edits applied here since log version `version`, in order: those
  // sequenced after it, then this client's not yet sequenced.
  opsSince(version) { return this.source.opsSince(version); }
  // `force`: overwrite a file that changed on disk since it was loaded.
  save(force) { return this.source.save(this, force); }

  // The server replaced the document (a huge file reloaded from disk): a
  // new log from `info.version`. Edits not yet sequenced are dropped (the
  // server refuses them as based on the old log), undo starts over, and
  // the window is fetched again.
  reloaded(info) {
    this.setInfo(info);
    this.source.restart(info.version);
    this.len = info.len;
    this.large = info.large;
    this.totalLines = info.lines;
    this.undo = [];
    this.redo = [];
    this.text = none();
    for (const v of this.views) {
      for (const c of v.cursors) { c.pos = Math.min(c.pos, this.len); c.anchor = null; }
      v.setTop(0);
    }
    const v = [...this.views][0];
    return this.ensure(v ? v.primary.pos : 0);
  }
}

// ---------------------------------------------------------------- file

// A buffer for a server document (`doc.open`'s answer).
export function fileBuffer(info) {
  const b = new Buffer(new FileSource(info.doc, info.version));
  b.setInfo(info);
  b.len = info.len;
  b.large = info.large;
  b.totalLines = info.lines;
  b.readOnly = !!info.readonly;
  return b;
}

// A replica of a server document: windows of its text fetched as the view
// moves, edits sent against the last log version seen, and the log applied
// in the server's order.
class FileSource {
  constructor(doc, version) {
    this.kind = "file";
    this.doc = doc;
    this.version = version;
    this.inflight = []; // the batch sent, not yet sequenced: a list of at most one
    this.pending = [];
    this.recent = noLog(); // as sequenced, for windows fetched late
    this.fetching = []; // the window being fetched: a list of at most one
  }
  get synced() { return !this.inflight.length && !this.pending.length; }

  // A new log from `version` (the document was reloaded).
  restart(version) {
    this.version = version;
    this.inflight = [];
    this.pending = [];
    this.recent = noLog();
  }

  ensure(buf, pos, after) {
    for (const L of heldList(buf.text)) {
      const i = L.indexOf(pos);
      if (i >= 0 && (L.eof || L.count - i > after) && (L.bof || i > 50)) return Promise.resolve(undefined);
    }
    for (const f of this.fetching) return f;
    const args = { doc: this.doc, around: pos, before: WINDOW_BEFORE, after: Math.max(WINDOW_AFTER, after * 2) };
    const fetching = (async () => {
      for (;;) {
        // A window is the server's text; with edits of ours not yet in it,
        // wait for them to be confirmed.
        while (!this.synced) await new Promise((r) => { setTimeout(() => { r(undefined); }, 20); });
        const w = await rpc("doc.window", args);
        if (!this.synced) continue;
        while (w.version > this.version) await new Promise((r) => { setTimeout(() => { r(undefined); }, 10); });
        const L2 = some(new Lines({ start: w.start, text: w.text, bof: w.bof, eof: w.eof, firstLine: w.first_line }));
        // Replay what we applied since the version the window was read at.
        const savedLen = buf.len;
        buf.len = w.len;
        for (const e of this.recent) {
          if (e.version <= w.version) continue;
          for (const op of e.ops) {
            const tmp = buf.text;
            buf.text = L2;
            const tl = buf.totalLines;
            buf.applyText(plainOp(op), op.nl);
            buf.totalLines = tl;
            buf.text = tmp;
          }
        }
        buf.len = savedLen;
        buf.text = L2;
        buf.textGen++;
        break;
      }
      this.fetching = [];
      buf.emit(TEXT_NEWS);
    })();
    this.fetching = [fetching];
    return fetching;
  }

  // Local edits: sent when nothing is in flight.
  send(buf, ops) {
    this.pending.push(...ops.map((o) => ({ ...o })));
    this.flush(buf);
  }

  flush(buf) {
    if (this.inflight.length || !this.pending.length) return;
    const batch = this.pending;
    this.inflight = [batch];
    this.pending = [];
    const ops = batch.map(wireOp);
    rpc("doc.edit", { doc: this.doc, base: this.version, ops }).catch((e) => {
      console.error(e);
      buf.emit(errorNews(String(e)));
    });
  }

  onOps(buf, ev, me) {
    if (ev.client === me) {
      // Our own edit, sequenced: the acknowledgement.
      this.version = ev.version;
      this.recent.push({ version: ev.version, ops: ev.ops });
      this.inflight = [];
      buf.modified = ev.modified;
      this.flush(buf);
    } else {
      const logged = ev.ops;
      let remote = logged.map(plainOp);
      // Ours are sequenced after it: theirs win ties.
      this.inflight = this.inflight.map((batch) => {
        const [ours, theirs] = transformSeqs(batch, remote, false);
        remote = theirs;
        return ours;
      });
      if (this.pending.length) {
        const [ours, theirs] = transformSeqs(this.pending, remote, false);
        this.pending = ours;
        remote = theirs;
      }
      // The newlines each deleted, as the log says (moved through ours,
      // a deletion split by our insertion is counted from its text alone).
      const deleted = remote.length === logged.length ? logged.map((o) => (o.t === "del" ? o.nl : 0)) : remote.map(() => 0);
      remote.forEach((op, i) => {
        const gone = buf.applyText(op, deleted[i]);
        buf.moveThrough(op);
        buf.undo = buf.undo.map((u) => transformSeqs(u, [op], false)[0]);
        buf.redo = buf.redo.map((u) => transformSeqs(u, [op], false)[0]);
        buf.emit(editNews(op, gone, false));
      });
      this.version = ev.version;
      this.recent.push({ version: ev.version, ops: ev.ops });
      buf.modified = ev.modified;
    }
    if (this.recent.length > 2000) this.recent = this.recent.slice(-2000);
  }

  opsSince(version) {
    return [...this.recent.filter((e) => e.version > version).flatMap((e) => e.ops.map(plainOp)), ...this.inflight.flatMap((b) => b), ...this.pending];
  }

  async save(buf, force) {
    while (!this.synced) await new Promise((r) => { setTimeout(() => { r(undefined); }, 20); });
    const info = await rpc("doc.save", { doc: this.doc, force });
    buf.modified = info.modified;
    return info;
  }

  // Text not held: the server's.
  async fetchText(s, e) { return (await rpc("doc.text", { doc: this.doc, start: s, end: e })).text; }

  propsAt() { return []; }
}

// ---------------------------------------------------------------- terminal

// A terminal (terminal.js) shown as a tab: it has no text, and its views
// draw the terminal instead.
class TerminalSource {
  constructor(term) {
    this.kind = "terminal";
    this.doc = null;
    this.term = term;
  }
  ensure() { return Promise.resolve(undefined); }
  send() {}
  async fetchText() { return ""; }
  propsAt() { return []; }
}

// The tab of terminal `term`, named `name`.
export function terminalBuffer(term, name) {
  const b = new Buffer(new TerminalSource(term));
  b.name = name;
  b.readOnly = true;
  const L = new Lines({ start: 0, text: "", bof: true, eof: true, firstLine: 0 });
  b.text = some(L);
  b.totalLines = some(L.count);
  return b;
}

// ---------------------------------------------------------------- virtual

// A buffer local to this client, of `entries` (Entry[], see `setEntries`):
// `mode` is the plugin mode whose keys apply in it (or null).
export function virtualBuffer(name, entries, mode, readOnly) {
  const b = new Buffer(new VirtualSource());
  b.name = name;
  b.mode = mode;
  b.readOnly = readOnly;
  b.setEntries(entries);
  return b;
}

// A plugin's text: entries, each with its properties and style, held
// whole and nowhere else.
class VirtualSource {
  constructor() {
    this.kind = "virtual";
    this.doc = null;
    this.ranges = []; // [{start, end, props, style}], an entry's bytes
  }

  setEntries(buf, entries) {
    let text = "";
    this.ranges = [];
    for (const e of entries) {
      const s = byteLen(text);
      text += e.text;
      this.ranges.push({ start: s, end: byteLen(text), props: e.props, style: e.style });
    }
    buf.len = byteLen(text);
    const L = new Lines({ start: 0, text, bof: true, eof: true, firstLine: 0 });
    buf.text = some(L);
    buf.textGen++;
    buf.totalLines = some(L.count);
    buf.decor.clearNamespace("entry-style");
    for (const r of this.ranges) {
      if (r.style) buf.decor.addOverlay({ ns: "entry-style", start: r.start, end: r.end, style: r.style, when: "always", mark: false, above: false });
    }
    buf.emit(TEXT_NEWS);
  }

  propsAt(pos, len) {
    return this.ranges.filter((r) => pos >= r.start && (pos < r.end || (pos === r.end && r.end === len))).map((r) => r.props);
  }

  // Held whole: nothing to fetch, nothing to send.
  ensure() { return Promise.resolve(undefined); }
  send() {}
  async fetchText() { return ""; }
}
