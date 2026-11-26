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

import { rpc, attempt, failure } from "./rpc.js";
import { some, none } from "./maybe.js";
import { transformSeqs, insOp, delOp } from "./ot.js";
import { byteLen, byteToIdx } from "./utf8.js";
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
/** type Window = {
 *   start: Number,
 *   text: String,
 *   bof: Boolean,
 *   eof: Boolean,
 *   firstLine: Number | Null,
 * } */
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
// source (a Source, types/fresh.d.js): a server document's replica, a
// plugin's entries, or a terminal. Make one with `fileBuffer`,
// `virtualBuffer` or `terminalBuffer`.
// The newlines a deletion took: in its text when it was held, else as
// the server counted them (`counted`).
/** function linesGone(gone: String | Null, counted: Number) => Number */
function linesGone(gone, counted) { return gone === null ? counted : nl(gone); }

// An op as the server takes it (no more).
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
/** function noLog() => LogEntry[] */
function noLog() { return []; }

// The range a deletion removes (a list of at most one), and the op that
// undoes `op` (`gone`: the text it deleted).
/** function deletedRanges(op: Op) => Range[] */
function deletedRanges(op) { return op.t === "del" ? [{ start: op.pos, end: op.pos + op.len }] : []; }
// (none: a deletion whose text was not held, which nothing can undo).
/** function inverseOf(op: Op, gone: String | Null) => Op[] */
function inverseOf(op, gone) {
  if (op.t === "ins") return [delOp(op.pos, byteLen(op.text))];
  return gone === null ? [] : [insOp(op.pos, gone)];
}
// The newlines a logged op deleted.
/** function deletedLines(o: LoggedOp) => Number */
function deletedLines(o) { return o.t === "del" ? o.nl : 0; }
// An op as such: a copy (the pending ops are moved through others' in
// place), and what is sent; a logged op without its newline count.
/** function copyOp(o: Op) => Op */
function copyOp(o) { return o.t === "ins" ? insOp(o.pos, o.text) : delOp(o.pos, o.len); }
/** function plainOp(o: LoggedOp) => Op */
function plainOp(o) { return o.t === "ins" ? insOp(o.pos, o.text) : delOp(o.pos, o.len); }
// What a buffer tells its listeners: its held text changed (fetched or
// replaced), an edit (here or another client's) and what it deleted
// where the text was held, a failure, or an edit the server refused (the
// document as the server has it, to start again from).
/** type BufferNews =
 *   | {kind: "text"}
 *   | {kind: "edit", op: Op, gone: String | Null, local: Boolean}
 *   | {kind: "error", message: String}
 *   | {kind: "refused", message: String, info: DocInfo}
 */
/** const TEXT_NEWS: BufferNews */
const TEXT_NEWS = { kind: "text" };
/** function editNews(op: Op, gone: String | Null, local: Boolean) => BufferNews */
function editNews(op, gone, local) { return { kind: "edit", op, gone, local }; }
/** function errorNews(message: String) => BufferNews */
function errorNews(message) { return { kind: "error", message }; }
/** function refusedNews(message: String, info: DocInfo) => BufferNews */
function refusedNews(message, info) { return { kind: "refused", message, info }; }

// A primitive applied to held lines `L` of a document `len` bytes long
// (before it; `deleted`: a deletion's newlines, for one before the
// window): what it deleted there (none: not held), or the window lost (the
// op straddles its edge).
/** type Spliced = {kind: "held", gone: String | Null} | {kind: "lost"} */
/** const LOST: Spliced */
const LOST = { kind: "lost" };
/** function heldGone(gone: String | Null) => Spliced */
function heldGone(gone) { return { kind: "held", gone }; }
function spliceHeld(L, op, deleted, len) {
  const r = reachOf(op, deleted);
  const ws = L.start, we = L.eof ? len : L.heldEnd();
  if (r.ins ? r.pos < ws : r.end <= ws) {
    // Wholly before the window.
    L.move(r.bytes);
    L.firstLine = movedBy(L.firstLine, r.lines);
    return heldGone(NO_TEXT);
  }
  if (r.pos > we || (r.pos === we && !L.eof)) return heldGone(NO_TEXT); // wholly after it
  if (r.pos >= ws && (r.end < we || (r.end <= we && L.eof))) {
    const gone = r.ins ? NO_TEXT : L.text(r.pos, r.end);
    L.splice(op);
    return heldGone(gone);
  }
  return LOST;
}
// Log entries applied to held lines `L` of a document `len` bytes long at
// the version they follow (a list of at most one: none to apply when not
// kept): whether the lines still hold their window.
function replayOnto(L, len, since) {
  let n = len;
  for (const log of since) {
    for (const e of log) {
      for (const o of e.ops) {
        const op = plainOp(o);
        if (spliceHeld(L, op, o.nl, n).kind === "lost") return false;
        n += reachOf(op, o.nl).bytes;
      }
    }
    return true;
  }
  return false;
}

// A count, if known, moved by `d`.
/** function movedBy(n: Number | Null, d: Number) => Number | Null */
function movedBy(n, d) { return n === null ? null : n + d; }

// The held text as a list of at most one.
/** function heldList<A>(L: A | Null) => A[] */
function heldList(L) { return L === null ? [] : [L]; }

// How much text past a position is fetched when nothing else says.
export const ENSURE_AFTER = 200;

/** function noFoldRanges() => LspFold[] */
function noFoldRanges() { return []; }

/** function noFollowers() => Set<(Op) => Undefined> */
function noFollowers() { return new Set(); }

export class Buffer {
  // `source`: where its text comes from (a Source).
  constructor(source) {
    this.source = source;
    this.id = nextBufferId++;
    this.name = "";
    this.path = "";
    this.untitled = none(); // an untitled document's number (Untitled-N)
    // What the server says of a file (`setInfo`); a virtual buffer's
    // language is its creator's.
    this.language = none();
    this.encoding = none();
    this.lineEnding = none();
    this.large = false; // read from disk as it is viewed, a window at a time (fetchWindow)
    this.modified = false;
    this.readOnly = false;
    this.mode = none(); // the plugin mode whose keys apply (a virtual buffer's)
    this.text = none(); // the held text (none until fetched, and again)
    this.textGen = 0; // bumped on every change to the held text
    this.len = 0;
    this.totalLines = none(); // none while a huge file has no line index
    this.decor = new Decor();
    this.followers = noFollowers(); // what moves through its edits (views' cursors and scroll)
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
    this.foldRanges = noFoldRanges(); // a language server's, as line ranges (none known: empty)
  }
  emit(news) { for (const f of this.listeners) f(news); }
  // Not a document: a plugin's text, or a terminal.
  get isVirtual() { return fileOf(this.source).length === 0; }
  // Its server document's replica (a list of at most one: a file's).
  get file() { return fileOf(this.source); }
  // The server document's id; null for a virtual buffer.
  get doc() { return docOf(this.source); }
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
    for (const f of this.followers) f(op);
  }

  // Apply a primitive to the held text (`deleted`: the newlines it deleted,
  // for a deletion outside the window). Returns the deleted text when the
  // window held it.
  applyText(op, deleted) {
    this.textGen++;
    const r = reachOf(op, deleted);
    let gone = NO_TEXT;
    for (const L of heldList(this.text)) {
      const s = spliceHeld(L, op, deleted, this.len);
      if (s.kind === "lost") this.text = none(); // straddles the edge: fetch again
      else gone = s.gone;
    }
    this.len += r.bytes;
    this.totalLines = movedBy(this.totalLines, r.ins ? r.lines : -linesGone(gone, deleted));
    return gone;
  }

  // The held text, where the caller knows it is held (a view drawn, a
  // cursor moved in it): it throws when it is not.
  get held() {
    for (const L of heldList(this.text)) return L;
    throw new Error(`${this.name}: its text is not held`);
  }
  // The document's line index is built (`lines` long): the window is
  // fetched again, with its line numbers, at `top`.
  indexed(lines, top) {
    this.totalLines = some(lines);
    this.text = none();
    return this.ensure(top, ENSURE_AFTER);
  }
  // Follow its edits: `shift(op)` for each primitive applied (a view's
  // cursors and scroll move through it). Returns the unfollow.
  follow(shift) {
    this.followers.add(shift);
    return () => { this.followers.delete(shift); };
  }
  textRange(s, e) { return heldList(this.text).map((L) => L.text(s, Math.min(e, this.len))).at(0) ?? NO_TEXT; }
  // Text for a plugin's getBufferText: local when held, else the source's.
  async textAsync(s, e) {
    const t = this.textRange(s, e);
    if (t != null) return t;
    for (const f of fileOf(this.source)) return (await rpc("doc.text", { doc: f.doc, start: s, end: e })).text;
    return "";
  }

  // Make sure the held text has the line at `pos` and `after` lines past
  // it. Resolves when it does (immediately when it already does).
  // Hold the text from `pos` for at least `after` bytes.
  ensure(pos, after) {
    for (const f of fileOf(this.source)) return this.fetchWindow(f, pos, after);
    return Promise.resolve(undefined); // held whole, or no text at all
  }

  // A local edit: applied now, and passed to the source (a file's goes to
  // the server). `undoable`: an undo step (typing joins the last when
  // `typing` is "continue"; "start" begins a run; "none": not typing).
  // Returns the ops that undo it: none when it deleted text not held here,
  // which cannot be undone (the undo history then ends before it).
  edit(ops, undoable, typing) {
    /** const inverse: Op[] */
    const inverse = [];
    let invertible = true;
    if (this.doc) this.modified = true; // before the edit events, which redraw the tabs
    for (const op of ops) {
      const gone = deletedRanges(op).map((r) => this.textRange(r.start, r.end)).at(0) ?? NO_TEXT;
      this.applyText(op, linesGone(gone, 0));
      this.moveThrough(op);
      const inv = inverseOf(op, gone);
      if (inv.length === 0) invertible = false;
      inverse.unshift(...inv);
      this.emit(editNews(op, gone, true));
    }
    if (undoable) {
      if (invertible) pushUndo(this, inverse, typing);
      else this.forgetUndo();
      this.redo = [];
    }
    this.lastEdit = Date.now();
    for (const f of fileOf(this.source)) { f.pending.push(...ops.map(copyOp)); this.flush(f); }
    return invertible ? [inverse] : [];
  }
  // The undo history ends here: an edit removed text not held here.
  forgetUndo() {
    this.undo = [];
    this.emit(errorNews(`${this.name}: an edit removed text not loaded here, so it cannot be undone (undo history cleared)`));
  }

  // Undo the last step (redo: from the redo stack): the ops it undid (none
  // when there was nothing to undo).
  doUndo() { return this.undoFrom(this.undo, this.redo); }
  doRedo() { return this.undoFrom(this.redo, this.undo); }
  // Undo the last edit on `stack`, its inverse going on `other`; the ops
  // undone (none: nothing to undo).
  undoFrom(stack, other) {
    for (const ops of stack.splice(-1)) {
      this.typingRun = false;
      const inverse = this.edit(ops.map(copyOp), false, "none");
      for (const inv of inverse) other.push(inv);
      if (inverse.length === 0) other.splice(0);
      return ops;
    }
    return [];
  }

  // The properties of the entries at `pos` (a virtual buffer's; a file
  // has none).
  propsAt(pos) {
    const len = this.len;
    return rangesOf(this.source).flatMap((ranges) => ranges.filter((r) => pos >= r.start && (pos < r.end || (pos === r.end && r.end === len))).map((r) => r.props));
  }

  // ---- a virtual buffer's

  // Replace the text with `entries`: [{text, props, style}].
  setEntries(entries) {
    const s = this.source;
    if (rangesOf(s).length === 0) throw new Error(`${this.name} is not a plugin's buffer`);
    let text = "";
    const ranges = [];
    for (const e of entries) {
      const start = byteLen(text);
      text += e.text;
      ranges.push(entryRange(start, byteLen(text), e.props, e.style));
    }
    this.source = virtualSource(ranges);
    this.len = byteLen(text);
    const L = new Lines({ start: 0, text, bof: true, eof: true, firstLine: 0 });
    this.text = some(L);
    this.textGen++;
    this.totalLines = some(L.count);
    this.decor.clearNamespace("entry-style");
    for (const r of ranges) {
      const style = r.style;
      if (style !== null) this.decor.addOverlay({ ns: "entry-style", start: r.start, end: r.end, style, when: "always", mark: false, above: false });
    }
    this.emit(TEXT_NEWS);
  }


  // ---- a file buffer's (its document's replica: `file`)

  // The window of text around `pos` (at least `after` bytes past it)
  // fetched, unless it is held. A window is the server's text: with edits
  // of ours not yet in it, it waits for them to be confirmed, then replays
  // what was applied here since the version it was read at.
  fetchWindow(f, pos, after) {
    for (const L of heldList(this.text)) {
      const i = L.indexOf(pos);
      if (i >= 0 && (L.eof || L.count - i > after) && (L.bof || i > 50)) return Promise.resolve(undefined);
    }
    for (const p of f.fetching) return p;
    const args = { doc: f.doc, around: pos, before: WINDOW_BEFORE, after: Math.max(WINDOW_AFTER, after * 2) };
    const fetching = (async () => {
      for (;;) {
        await until(f, () => synced(f));
        const w = await rpc("doc.window", args);
        // The log entries after the window's version arrive (they may come
        // after the answer); edits made here meanwhile: ask again.
        await until(f, () => f.version >= w.version);
        if (!synced(f)) continue;
        const L2 = new Lines({ start: w.start, text: w.text, bof: w.bof, eof: w.eof, firstLine: w.first_line });
        // Bring it to this replica's version: the entries sequenced since
        // it was read (none known that far back, or one crossing its edge:
        // ask again).
        if (!replayOnto(L2, w.len, logSince(f, w.version))) continue;
        this.text = some(L2);
        this.textGen++;
        break;
      }
      f.fetching = [];
      this.emit(TEXT_NEWS);
    })();
    f.fetching = [fetching];
    return fetching;
  }

  // Local edits are sent when nothing is in flight.
  flush(f) {
    if (f.inflight.length > 0 || f.pending.length === 0) return;
    const batch = f.pending;
    f.inflight = [batch];
    f.pending = [];
    rpc("doc.edit", { doc: f.doc, base: f.version, ops: batch.map(copyOp) }).catch(async (e) => {
      // Refused: this replica has edits the document never will. It starts
      // again from the document as the server has it.
      const message = `${this.name}: the server refused an edit (${failure(e)}); reloaded`;
      try { this.emit(refusedNews(message, await rpc("doc.info", { doc: f.doc }))); }
      catch (e2) { this.emit(errorNews(`${message}; and could not reload it: ${failure(e2)}`)); }
    });
  }

  // An event from the document's log, in order: our own edit sequenced
  // (the acknowledgement), or another's, moved through ours not yet
  // sequenced (theirs win ties) and applied.
  onOps(ev /*: DocOps */, me /*: Number */) {
    for (const f of fileOf(this.source)) {
      // An entry from before this replica's version (it started again
      // from a later one) is in it already.
      if (ev.version <= f.version) continue;
      if (ev.client === me) {
        f.version = ev.version;
        f.recent.push({ version: ev.version, ops: ev.ops });
        f.inflight = [];
        this.modified = ev.modified;
        this.flush(f);
      } else {
        const logged = ev.ops;
        let remote = logged.map(plainOp);
        f.inflight = f.inflight.map((batch) => {
          const [ours, theirs] = transformSeqs(batch, remote, false);
          remote = theirs;
          return ours;
        });
        if (f.pending.length) {
          const [ours, theirs] = transformSeqs(f.pending, remote, false);
          f.pending = ours;
          remote = theirs;
        }
        // The newlines each deleted, as the log says (moved through ours,
        // a deletion split by our insertion is counted from its text alone).
        const deleted = remote.length === logged.length ? logged.map(deletedLines) : remote.map(() => 0);
        remote.forEach((op, i) => {
          const gone = this.applyText(op, deleted[i]);
          this.moveThrough(op);
          this.undo = this.undo.map((u) => transformSeqs(u, [op], false)[0]);
          this.redo = this.redo.map((u) => transformSeqs(u, [op], false)[0]);
          this.emit(editNews(op, gone, false));
        });
        f.version = ev.version;
        f.recent.push({ version: ev.version, ops: ev.ops });
        this.modified = ev.modified;
      }
      // With nothing of ours unsequenced, the text is the log's at this
      // version: its line count is the server's (a deletion counted here
      // without its text is set right).
      if (synced(f)) this.totalLines = ev.lines;
      prune(f);
      wake(f);
    }
  }

  // Save the document, once every edit is sequenced: the server's refusal,
  // if it refused (a list of at most one). `force`: overwrite a file that
  // changed on disk since it was loaded.
  async save(force) {
    for (const f of fileOf(this.source)) {
      await whenSynced(f);
      const o = await attempt("doc.save", { doc: f.doc, force });
      if (o.kind === "refused") return [o];
      this.modified = o.value.modified;
    }
    return [];
  }

  // The server replaced the document (a huge file reloaded from disk): a
  // new log from `info.version`. Edits not yet sequenced are dropped (the
  // server refuses them as based on the old log), undo starts over, and
  // the window is fetched again.
  reloaded(info) {
    this.setInfo(info);
    for (const f of fileOf(this.source)) restart(f, info.version);
    this.len = info.len;
    this.large = info.large;
    this.totalLines = info.lines;
    this.undo = [];
    this.redo = [];
    this.text = none();
  }
}

// ---------------------------------------------------------------- file

// A buffer for a server document (`doc.open`'s answer).
export function fileBuffer(info) {
  const b = new Buffer(fileSource(replicaAt(info.doc, info.version)));
  b.setInfo(info);
  b.len = info.len;
  b.large = info.large;
  b.totalLines = info.lines;
  b.readOnly = !!info.readonly;
  return b;
}

// A new replica of document `doc` at log version `version`.
/** function replicaAt(doc: Number, version: Number) => Replica */
function replicaAt(doc, version) { return { doc, version, inflight: [], pending: [], from: version, recent: noLog(), fetching: [], waiting: noWaiting() }; }
/** function noWaiting() => (() => Boolean)[] */
function noWaiting() { return []; }
// Every local edit has been sequenced by the server.
/** function synced(f: Replica) => Boolean */
export function synced(f) { return f.inflight.length === 0 && f.pending.length === 0; }
// Resolves once `ready()` holds of replica `f` (checked again each time its
// log moves).
/** function until(f: Replica, ready: () => Boolean) => Promise<Undefined> */
function until(f, ready) {
  if (ready()) return Promise.resolve(undefined);
  return new Promise((resolve) => { f.waiting.push(() => { if (!ready()) return false; resolve(undefined); return true; }); });
}
// Resolves once every local edit is sequenced.
/** function whenSynced(f: Replica) => Promise<Undefined> */
export function whenSynced(f) { return until(f, () => synced(f)); }
// The replica's log moved: the waits it satisfies end.
/** function wake(f: Replica) => Undefined */
function wake(f) { f.waiting = f.waiting.filter((w) => !w()); }
// The log entries kept: those a window or diagnostics read at a version
// this client may still be sent could need (the last LOG_KEPT); `from` is
// the version the first kept one follows.
const LOG_KEPT = 2000;
/** function prune(f: Replica) => Undefined */
function prune(f) {
  for (const e of f.recent.slice(0, Math.max(0, f.recent.length - LOG_KEPT))) f.from = e.version;
  f.recent = f.recent.slice(-LOG_KEPT);
}
// The entries sequenced after log version `version` (a list of at most
// one: none when they are no longer kept).
/** function logSince(f: Replica, version: Number) => LogEntry[][] */
function logSince(f, version) { return version >= f.from ? [f.recent.filter((e) => e.version > version)] : []; }
// A new log from `version` (the document was reloaded): edits not yet
// sequenced are dropped (the server refuses them as based on the old log).
/** function restart(f: Replica, version: Number) => Undefined */
function restart(f, version) {
  f.version = version;
  f.from = version;
  f.inflight = [];
  f.pending = [];
  f.recent = noLog();
  wake(f);
}
// The edits applied here since log version `version`, in order: those
// sequenced after it, then this client's not yet sequenced (a list of at
// most one: none when the entries after it are no longer kept).
/** function opsSince(f: Replica, version: Number) => Op[][] */
export function opsSince(f, version) {
  return logSince(f, version).map((log) => [...log.flatMap((e) => e.ops.map(plainOp)), ...f.inflight.flatMap((b) => b), ...f.pending]);
}

// The three kinds of Source, and a source's parts by kind (each a list of
// at most one).
/** function fileSource(replica: Replica) => Source */
function fileSource(replica) { return { kind: "file", replica }; }
/** function virtualSource(ranges: EntryRange[]) => Source */
function virtualSource(ranges) { return { kind: "virtual", ranges }; }
/** function terminalSource(term: Number) => Source */
function terminalSource(term) { return { kind: "terminal", term }; }
/** function fileOf(s: Source) => Replica[] */
function fileOf(s) { return s.kind === "file" ? [s.replica] : []; }
/** function docOf(s: Source) => Number | Null */
function docOf(s) { return s.kind === "file" ? s.replica.doc : null; }
/** function rangesOf(s: Source) => EntryRange[][] */
function rangesOf(s) { return s.kind === "virtual" ? [s.ranges] : []; }
// The terminal a buffer is the tab of (its source): a list of at most one
// id.
/** function terminalOf(s: Source) => Number[] */
export function terminalOf(s) { return s.kind === "terminal" ? [s.term] : []; }
/** function entryRange(
 *   start: Number,
 *   end: Number,
 *   props: Dict<String>,
 *   style: Style | Null,
 * ) => EntryRange */
function entryRange(start, end, props, style) { return { start, end, props, style }; }

// ---------------------------------------------------------------- terminal

// The tab of terminal `term`, named `name`: it has no text, and its views
// draw the terminal instead.
export function terminalBuffer(term, name) {
  const b = new Buffer(terminalSource(term));
  b.name = name;
  b.readOnly = true;
  const L = new Lines({ start: 0, text: "", bof: true, eof: true, firstLine: 0 });
  b.text = some(L);
  b.totalLines = some(L.count);
  return b;
}

// ---------------------------------------------------------------- virtual

// A buffer local to this client, of a plugin's `entries` (Entry[]), held
// whole and nowhere else: `mode` is the plugin mode whose keys apply in it
// (or null).
export function virtualBuffer(name, entries, mode, readOnly) {
  const b = new Buffer(virtualSource([]));
  b.name = name;
  b.mode = mode;
  b.readOnly = readOnly;
  b.setEntries(entries);
  return b;
}
