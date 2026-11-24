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

// A buffer: its text (all of it, or a window of it), decorations, views,
// undo and listeners, whatever its kind. What differs by kind is its
// source: a `FileSource` (a server document) or a `VirtualSource` (a
// plugin's entries). Make one with `fileBuffer` or `virtualBuffer`.
export class Buffer {
  constructor(source) {
    this.id = nextBufferId++;
    this.source = source;
    this.name = "";
    this.path = "";
    this.untitled = null; // an untitled document's number (Untitled-N)
    // What the server says of a file (`setInfo`); a virtual buffer's
    // language is its creator's.
    this.language = null;
    this.encoding = null;
    this.lineEnding = null;
    this.large = false; // read from disk as it is viewed, see FileSource
    this.modified = false;
    this.readOnly = false;
    this.mode = null; // the plugin mode whose keys apply (a virtual buffer's)
    this.text = null; // Lines, or null until (again) fetched
    this.textGen = 0; // bumped on every change to the held text
    this.len = 0;
    this.totalLines = null; // null while a huge file has no line index
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
    this.foldRanges = null; // a language server's, as line ranges
    this.struct = null; // Markdown structure of the held lines, at `structGen`
    this.structGen = -1;
  }
  emit(what, data) { for (const f of this.listeners) f(what, data); }
  get isVirtual() { return this.source.kind === "virtual"; }
  // The server document's id; null for a virtual buffer.
  get doc() { return this.source.doc; }
  // The held text covers the whole document.
  get full() { const L = this.text; return !!L && L.bof && L.eof; }

  // The server's word on the document's file (after an open, a save, a
  // Save As, a rename or a delete): its path (none while untitled), name,
  // language, format and whether it is unsaved.
  setInfo(info) {
    this.path = info.path || "";
    this.untitled = info.untitled ?? null;
    this.name = info.path ? info.path.split("/").pop() : `Untitled-${info.untitled}`;
    this.language = info.language;
    this.modified = info.modified;
    this.encoding = info.encoding;
    this.lineEnding = info.line_ending;
  }

  // Move everything anchored in this buffer through `op`: text, decorations,
  // cursors in every view, and undo entries.
  moveThrough(op) {
    this.decor.shift(op);
    for (const v of this.views) v.shift(op);
  }

  // Apply a primitive to the held text. Returns the deleted text when the
  // window held it.
  applyText(op) {
    this.textGen++;
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
  // the server).
  edit(ops, { undoable = true, typing = null } = {}) {
    const inverse = [];
    if (this.doc) this.modified = true; // before the edit events, which redraw the tabs
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
    this.source.send(this, ops);
    return inverse;
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
  save({ force = false } = {}) { return this.source.save(this, force); }

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
    this.text = null;
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
    this.inflight = null;
    this.pending = [];
    this.recent = []; // [{version, ops}] as sequenced, for windows fetched late
    this.fetching = null;
  }
  get synced() { return !this.inflight && !this.pending.length; }

  // A new log from `version` (the document was reloaded).
  restart(version) {
    this.version = version;
    this.inflight = null;
    this.pending = [];
    this.recent = [];
  }

  ensure(buf, pos, after) {
    const L = buf.text;
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
        while (!this.synced) await new Promise((r) => setTimeout(r, 20));
        const w = await rpc("doc.window", args);
        if (!this.synced) continue;
        while (w.version > this.version) await new Promise((r) => setTimeout(r, 10));
        const L2 = new Lines(w.start, w.text, w.bof, w.eof, w.first_line);
        // Replay what we applied since the version the window was read at.
        const savedLen = buf.len;
        buf.len = w.len;
        for (const e of this.recent) {
          if (e.version <= w.version) continue;
          for (const op of e.ops) {
            const tmp = buf.text;
            buf.text = L2;
            const tl = buf.totalLines;
            buf.applyText(op);
            buf.totalLines = tl;
            buf.text = tmp;
          }
        }
        buf.len = savedLen;
        buf.text = L2;
        buf.textGen++;
        break;
      }
      this.fetching = null;
      buf.emit("text");
    })();
    return this.fetching;
  }

  // Local edits: sent when nothing is in flight.
  send(buf, ops) {
    this.pending.push(...ops.map((o) => ({ ...o })));
    this.flush(buf);
  }

  flush(buf) {
    if (this.inflight || !this.pending.length) return;
    this.inflight = this.pending;
    this.pending = [];
    const ops = this.inflight.map(({ t, pos, text, len }) => (t === "ins" ? { t, pos, text } : { t, pos, len }));
    rpc("doc.edit", { doc: this.doc, base: this.version, ops }).catch((e) => {
      console.error(e);
      buf.emit("error", e);
    });
  }

  onOps(buf, ev, me) {
    if (ev.client === me) {
      // Our own edit, sequenced: the acknowledgement.
      this.version = ev.version;
      this.recent.push({ version: ev.version, ops: ev.ops });
      this.inflight = null;
      buf.modified = ev.modified;
      this.flush(buf);
    } else {
      let remote = ev.ops;
      // Ours are sequenced after it: theirs win ties.
      if (this.inflight) {
        const [ours, theirs] = transformSeqs(this.inflight, remote, false);
        this.inflight = ours;
        remote = theirs;
      }
      if (this.pending.length) {
        const [ours, theirs] = transformSeqs(this.pending, remote, false);
        this.pending = ours;
        remote = theirs;
      }
      for (const op of remote) {
        const gone = buf.applyText(op);
        buf.moveThrough(op);
        buf.undo = buf.undo.map((u) => transformSeqs(u, [op], false)[0]);
        buf.redo = buf.redo.map((u) => transformSeqs(u, [op], false)[0]);
        buf.emit("edit", { op, gone, local: false });
      }
      this.version = ev.version;
      this.recent.push({ version: ev.version, ops: ev.ops });
      buf.modified = ev.modified;
    }
    if (this.recent.length > 2000) this.recent.splice(0, this.recent.length - 2000);
  }

  opsSince(version) {
    const out = [];
    for (const e of this.recent) if (e.version > version) out.push(...e.ops);
    if (this.inflight) out.push(...this.inflight);
    out.push(...this.pending);
    return out;
  }

  async save(buf, force) {
    while (!this.synced) await new Promise((r) => setTimeout(r, 20));
    const info = await rpc("doc.save", { doc: this.doc, force });
    buf.modified = info.modified;
    return info;
  }

  // Text not held: the server's.
  async fetchText(s, e) { return (await rpc("doc.text", { doc: this.doc, start: s, end: e })).text; }

  propsAt() { return []; }
}

// ---------------------------------------------------------------- virtual

// A buffer local to this client, of `entries` (see `setEntries`).
export function virtualBuffer(name, entries, { mode = null, readOnly = false } = {}) {
  const b = new Buffer(new VirtualSource());
  b.name = name;
  b.mode = mode;
  b.readOnly = !!readOnly;
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
      text += e.text || "";
      this.ranges.push({ start: s, end: byteLen(text), props: e.props || {}, style: e.style });
    }
    buf.len = byteLen(text);
    buf.text = new Lines(0, text, true, true, 0);
    buf.textGen++;
    buf.totalLines = buf.text.count;
    buf.decor.clearNamespace("entry-style");
    for (const r of this.ranges) {
      if (r.style) buf.decor.add("overlays", { ns: "entry-style", start: r.start, end: r.end, style: r.style, when: null });
    }
    buf.emit("text");
  }

  propsAt(pos, len) {
    return this.ranges.filter((r) => pos >= r.start && (pos < r.end || (pos === r.end && r.end === len))).map((r) => r.props);
  }

  // Held whole: nothing to fetch, nothing to send.
  ensure() { return Promise.resolve(); }
  send() {}
  async fetchText() { return ""; }
}
