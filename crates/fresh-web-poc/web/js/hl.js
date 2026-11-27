// Syntax highlighting: fresh-editor's grammar engine (syntect, the same
// grammars and the same scope-to-category table as the terminal editor),
// compiled to WebAssembly (crates/fresh-highlight-wasm).
//
//  * The engine keeps its own copy of each buffer's held text, fed only by
//    the buffer's change stream (Buffer.followText) through a channel. The
//    channel is in-process and delivers on the next slice; moving the
//    engine into a Web Worker swaps the channel and nothing else.
//  * It parses in slices of a fixed number of bytes: the lines a view asked
//    for first, then on to the end of each buffer.
//  * Its answers are category runs per line, for the version of the text
//    it had. Each buffer's Colours keeps the runs on the page and carries
//    them forward through changes the engine has not answered yet: text
//    typed since takes the category of the place it was typed into.
//  * With its runs, an exact line says its roles: its part in an embedded
//    region (a Markdown fence) and in a table, or that they are not known
//    yet. A line an edit touched has unknown roles until the engine answers.
//  * A category is a theme key ("syntax.keyword", "editor.diff_add_bg"), so
//    a theme change is a CSS change.
//  * A plugin's buffer (a diff, a log) can be coloured in declared regions
//    instead of a grammar (setSyntaxRegions), and plugins add grammars
//    (registerGrammar, grammars.js). Both travel in the change stream, in
//    order with the edits, so a declaration never races an edit.

import { listOf, found } from "./maybe.js";
import { baseName } from "./paths.js";
import { byteLen } from "./utf8.js";
import { encodeFields, wordField, textField } from "./wasm.js";
import { THEME } from "./highlight.js";
import { bracketsIn } from "./brackets.js";
import { errorNews, filePath } from "./buffer.js";
import { failure } from "./rpc.js";

// The categories brackets inside do not count.
const LITERAL = ["syntax.string", "syntax.comment"];
// Bytes parsed per slice.
const SLICE_BYTES = 32 * 1024;

// A run of one category on a line: byte offsets [s, e) from the line's start.
// `b`: the category whose background it sits on, when not its own (a
// source token in a diff's added or removed row).
/** type Run = {s: Number, e: Number, c: Number, b: Number | Null} */
// What the page knows of a line's colours: exact (parsed from the true
// state), provisional (parsed from a guessed state, until the background
// pass reaches it), or carried (moved through an edit not yet answered).
// A parsed line has the roles the engine gave it with its runs; a carried
// line's are unknown.
/** type LineColours =
 *   | {state: "exact", runs: Run[], roles: LineRoles}
 *   | {state: "provisional", runs: Run[], roles: LineRoles}
 *   | {state: "carried", runs: Run[]}
 *   | {state: "unknown"} */
/** const UNKNOWN: LineColours */
const UNKNOWN = { state: "unknown" };
/** function carried(runs: Run[]) => LineColours */
function carried(runs) { return { state: "carried", runs }; }
/** function parsed(exact: Boolean, runs: Run[], roles: LineRoles) => LineColours */
function parsed(exact, runs, roles) { return exact ? { state: "exact", runs, roles } : { state: "provisional", runs, roles }; }

// A line's roles (LineRoles, types/fresh.d.js).
/** const UNKNOWN_ROLES: LineRoles */
const UNKNOWN_ROLES = { kind: "unknown" };
/** function knownRoles(region: RegionPart, table: TableLine | Null) => LineRoles */
function knownRoles(region, table) { return { kind: "known", region, table }; }
/** function tableLine(role: TableRole, firstRow: Boolean, last: Boolean) => TableLine */
function tableLine(role, firstRow, last) { return { role, firstRow, last }; }

// `hl_take`'s line flags (crates/fresh-highlight-wasm: LINE_*).
const LINE_EXACT = 1 << 0;
const LINE_ROLES_KNOWN = 1 << 1;
const LINE_REGION_OPEN = 1 << 2;
const LINE_REGION_BODY = 1 << 3;
const LINE_REGION_CLOSE = 1 << 4;
const LINE_TABLE_HEADER = 1 << 5;
const LINE_TABLE_DELIMITER = 1 << 6;
const LINE_TABLE_ROW = 1 << 7;
const LINE_TABLE_FIRST_ROW = 1 << 8;
const LINE_TABLE_LAST = 1 << 9;
// A line's roles from its flags.
/** function rolesOfFlags(flags: Number) => LineRoles */
function rolesOfFlags(flags) {
  if ((flags & LINE_ROLES_KNOWN) === 0) return UNKNOWN_ROLES;
  /** const region: RegionPart */
  const region = flags & LINE_REGION_OPEN ? "open" : flags & LINE_REGION_BODY ? "body" : flags & LINE_REGION_CLOSE ? "close" : "none";
  /** const role: TableRole[] */
  const role = flags & LINE_TABLE_HEADER ? ["header"] : flags & LINE_TABLE_DELIMITER ? ["delimiter"] : flags & LINE_TABLE_ROW ? ["row"] : [];
  /** const table: TableLine | Null */
  const table = role.map((r) => tableLine(r, (flags & LINE_TABLE_FIRST_ROW) !== 0, (flags & LINE_TABLE_LAST) !== 0)).at(0) ?? null;
  return knownRoles(region, table);
}
// What became of a declared region (`hl_regions`' statuses: REGION_*).
/** const REGION_STATUSES: RegionStatus[] */
const REGION_STATUSES = ["kept", "unknown-language", "overlaps", "empty"];
/** function regionStatus(word: Number) => RegionStatus */
function regionStatus(word) {
  for (const s of found(REGION_STATUSES[word])) return s;
  throw new Error(`the engine answered region status ${word}, which this page does not know`);
}
// Regions as `hl_regions` reads them.
/** function regionFields(regions: SyntaxRegion[]) => Field[] */
function regionFields(regions) {
  return [wordField(regions.length), ...regions.flatMap((r) => [
    wordField(r.start), wordField(r.end), wordField(r.prefix),
    wordField(r.streams.length), ...r.streams.map(wordField),
    textField(r.language),
  ])];
}
// The category at a position, as a feature on the keystroke reads it.
/** type CategoryAt =
 *   | {kind: "category", key: String, exact: Boolean}
 *   | {kind: "plain", exact: Boolean}
 *   | {kind: "unknown"} */
/** function categoryOf(key: String, exact: Boolean) => CategoryAt */
function categoryOf(key, exact) { return { kind: "category", key, exact }; }
/** function plainAt(exact: Boolean) => CategoryAt */
function plainAt(exact) { return { kind: "plain", exact }; }

// ---------------------------------------------------------------- the module

const enc = new TextEncoder();

// A line of `hl_take`'s answer.
/** type TakenLine = {line: Number, exact: Boolean, roles: LineRoles, runs: Run[]} */

// The syntax engine's calls in the module (wasm.js; the `hl_*` ABI).
class Engine {
  constructor(wasm) {
    /** this.w: Wasm */
    this.w = wasm;
    this.x = wasm.x;
  }
  init(packdump) { this.w.withBytes(packdump, (p, n) => this.w.check(this.x.hl_init(p, n))); }
  categories() { return JSON.parse(this.w.str(this.x.hl_categories() >>> 0)); }
  open(doc, syntax, text) {
    this.w.withBytes(enc.encode(syntax), (sp, sn) => this.w.withBytes(enc.encode(text), (tp, tn) => this.w.check(this.x.hl_open(doc, sp, sn, tp, tn))));
  }
  // Open `doc` plain, coloured only in the regions declared over it.
  openDeclared(doc, text) { this.w.withBytes(enc.encode(text), (p, n) => this.w.check(this.x.hl_open_declared(doc, p, n))); }
  // Declare `doc`'s regions: what became of each, in order.
  /** method regions(doc: Number, regions: SyntaxRegion[]) => RegionStatus[] */
  regions(doc, regions) {
    const words = this.w.withBytes(encodeFields(regionFields(regions)), (p, n) => this.w.words(this.x.hl_regions(doc, p, n)));
    return [...words].map(regionStatus);
  }
  close(doc) { this.w.check(this.x.hl_close(doc)); }
  edit(doc, at, removed, text) { this.w.withBytes(enc.encode(text), (p, n) => this.w.check(this.x.hl_edit(doc, at, removed, p, n))); }
  want(doc, from, to) { this.w.check(this.x.hl_want(doc, from, to)); }
  // Parse up to `budget` bytes: whether work remains. A grammar that failed
  // closes its document (take then fails for it).
  work(budget) { return this.x.hl_work(budget) === 1; }
  // The lines whose runs or roles changed since the last take.
  take(doc) {
    const ptr = this.x.hl_take(doc) >>> 0;
    if (ptr === 0) throw this.w.failed();
    const w = (k) => this.w.u32(ptr + 4 * k);
    /** const out: TakenLine[] */
    const out = [];
    let i = 2;
    for (let k = 0; k < w(1); k++) {
      const line = w(i), flags = w(i + 1), n = w(i + 2);
      i += 3;
      /** const runs: Run[] */
      const runs = [];
      for (let r = 0; r < n; r++, i += 4) runs.push({ s: w(i), e: w(i) + w(i + 1), c: w(i + 2), b: backgroundOf(w(i + 3)) });
      out.push({ line, exact: (flags & LINE_EXACT) !== 0, roles: rolesOfFlags(flags), runs });
    }
    return out;
  }
}

// ---------------------------------------------------------------- the channel

// What travels to the engine: a buffer's text change, its syntax or
// declared regions, the lines a view wants, or a new grammar set. A
// message that is answered carries a ticket its answer names.
/** type ToEngine =
 *   | {kind: "open", doc: Number, version: Number, syntax: String, text: String}
 *   | {kind: "declared", doc: Number, version: Number, text: String}
 *   | {kind: "regions", doc: Number, ticket: Number, regions: SyntaxRegion[]}
 *   | {kind: "edit", doc: Number, version: Number, at: Number, removed: Number, text: String}
 *   | {kind: "close", doc: Number, version: Number}
 *   | {kind: "want", doc: Number, from: Number, to: Number}
 *   | {kind: "grammars", ticket: Number, packdump: Uint8Array} */
/** function openMsg(doc: Number, version: Number, syntax: String, text: String) => ToEngine */
function openMsg(doc, version, syntax, text) { return { kind: "open", doc, version, syntax, text }; }
/** function declaredMsg(doc: Number, version: Number, text: String) => ToEngine */
function declaredMsg(doc, version, text) { return { kind: "declared", doc, version, text }; }
/** function regionsMsg(doc: Number, ticket: Number, regions: SyntaxRegion[]) => ToEngine */
function regionsMsg(doc, ticket, regions) { return { kind: "regions", doc, ticket, regions }; }
/** function editMsg(
 *   doc: Number,
 *   version: Number,
 *   at: Number,
 *   removed: Number,
 *   text: String,
 * ) => ToEngine */
function editMsg(doc, version, at, removed, text) { return { kind: "edit", doc, version, at, removed, text }; }
/** function closeMsg(doc: Number, version: Number) => ToEngine */
function closeMsg(doc, version) { return { kind: "close", doc, version }; }
/** function wantMsg(doc: Number, from: Number, to: Number) => ToEngine */
function wantMsg(doc, from, to) { return { kind: "want", doc, from, to }; }
/** function grammarsMsg(ticket: Number, packdump: Uint8Array) => ToEngine */
function grammarsMsg(ticket, packdump) { return { kind: "grammars", ticket, packdump }; }

// The engine's answer to a ticketed message: the regions' statuses, the
// grammar set loaded, or why it failed.
/** type FromEngine =
 *   | {kind: "regions", ticket: Number, statuses: RegionStatus[]}
 *   | {kind: "grammars", ticket: Number}
 *   | {kind: "failed", ticket: Number, why: String} */
/** function regionsAnswer(ticket: Number, statuses: RegionStatus[]) => FromEngine */
function regionsAnswer(ticket, statuses) { return { kind: "regions", ticket, statuses }; }
/** function grammarsAnswer(ticket: Number) => FromEngine */
function grammarsAnswer(ticket) { return { kind: "grammars", ticket }; }
/** function failedAnswer(ticket: Number, why: String) => FromEngine */
function failedAnswer(ticket, why) { return { kind: "failed", ticket, why }; }

// How a buffer is coloured: by its grammar (its syntax, or a registered
// grammar its language or extension matches), or in the regions its plugin
// declared (as the page has moved them through edits since).
/** type DocHost = {kind: "grammar"} | {kind: "declared", regions: SyntaxRegion[]} */
/** const BY_GRAMMAR: DocHost */
const BY_GRAMMAR = { kind: "grammar" };
/** function declaredHost(regions: SyntaxRegion[]) => DocHost */
function declaredHost(regions) { return { kind: "declared", regions }; }

// A change seen on the page, by line, for carrying late answers forward.
/** type LineChange =
 *   | {kind: "lines", version: Number, line: Number, removed: Number, added: Number}
 *   | {kind: "reset", version: Number} */
/** function resetChange(version: Number) => LineChange */
function resetChange(version) { return { kind: "reset", version }; }
/** function linesChange(
 *   version: Number,
 *   line: Number,
 *   removed: Number,
 *   added: Number,
 * ) => LineChange */
function linesChange(version, line, removed, added) { return { kind: "lines", version, line, removed, added }; }

// ---------------------------------------------------------------- a buffer's colours

// The colours the page has for one buffer's held lines.
export class Colours {
  constructor(categories) {
    this.keys = categories; // theme key by category index
    /** this.lines: LineColours[] */
    this.lines = [];
    /** this.lens: Number[] */
    this.lens = [];
    this.version = 0;
    /** this.pending: LineChange[] */
    this.pending = [];
    this.gen = 0; // bumped on every change to what is known
    this.spanCache = new WeakMap();
    this.bracketCache = new WeakMap();
  }

  // The held text replaced (none: not held): nothing is known of it yet.
  reset(version, L) {
    this.lines = L === null ? [] : L.lens.map(() => UNKNOWN);
    this.lens = L === null ? [] : [...L.lens];
    this.version = version;
    this.gen++;
    this.pending.push(resetChange(version));
  }

  // An edit to the held text (`L`, as it is after it): carry the colours
  // through it.
  edit(c, L) {
    const line = L.indexOf(L.start + c.at);
    const col = c.at - (L.starts[line] - L.start);
    // The old lines the removed bytes reach, and where in the last one.
    let k = 0, endCol = col + c.removed;
    while (line + k < this.lens.length && endCol > this.lens[line + k]) { endCol -= this.lens[line + k] + 1; k++; }
    const segs = c.text.split("\n");
    const added = segs.length - 1;
    const first = this.lines[line] ?? UNKNOWN, last = this.lines[line + k] ?? UNKNOWN;
    const cat = categoryBefore(first, col);
    const typed = (s, len) => (cat === null || len === 0 ? [] : [{ s, e: s + len, c: cat, b: null }]);
    const head = runsOf(first).flatMap((r) => (r.s < col ? [{ s: r.s, e: Math.min(r.e, col), c: r.c, b: r.b }] : []));
    const tail = (shift) => runsOf(last).flatMap((r) => (r.e > endCol ? [{ s: Math.max(r.s, endCol) + shift, e: r.e + shift, c: r.c, b: r.b }] : []));
    const known = first.state !== "unknown" && last.state !== "unknown";
    /** const made: LineColours[] */
    const made = [];
    if (added === 0) {
      const len = byteLen(c.text);
      made.push(known ? carried([...head, ...typed(col, len), ...tail(col + len - endCol)]) : UNKNOWN);
    } else {
      made.push(known ? carried([...head, ...typed(col, byteLen(segs[0]))]) : UNKNOWN);
      for (const s of segs.slice(1, -1)) made.push(known ? carried(typed(0, byteLen(s))) : UNKNOWN);
      const lastLen = byteLen(segs[added]);
      made.push(known ? carried([...typed(0, lastLen), ...tail(lastLen - endCol)]) : UNKNOWN);
    }
    this.lines.splice(line, k + 1, ...made);
    this.lens.splice(line, k + 1, ...L.lens.slice(line, line + added + 1));
    this.version = c.version;
    this.gen++;
    this.pending.push(linesChange(c.version, line, k, added));
  }

  // The engine's answer for `lines` at `version`: each line moved through
  // the changes made since (dropped when one of them touched it: the
  // engine answers it again).
  apply(version, lines) {
    const after = this.pending.filter((p) => p.version > version);
    this.pending = after;
    for (const r of lines) {
      for (const li of moved(r.line, after)) this.lines[li] = parsed(r.exact, r.runs, r.roles);
    }
    this.gen++;
  }

  // What the buffer is coloured by changed (its declared regions): every
  // line keeps its runs until the engine answers again, as carried.
  restyle() {
    this.lines = this.lines.map((lc) => (lc.state === "unknown" ? UNKNOWN : carried(lc.runs)));
    this.gen++;
  }

  // The roles of lines [from, to) (as many of them as there are).
  /** method roles(from: Number, to: Number) => LineRoles[] */
  roles(from, to) { return this.lines.slice(from, to).map(rolesOf); }

  // Line `li` of held lines `L`: its syntax spans (UTF-16 indices into its
  // text) with their style.
  spans(li, L) {
    const lc = this.lines[li] ?? UNKNOWN;
    if (lc.state === "unknown") return [];
    const text = L.lines[li];
    const hit = this.spanCache.get(lc);
    if (hit !== undefined && hit.text === text) return hit.spans;
    const spans = utf16Runs(lc.runs, L, li).map((r) => ({ s: r.s, e: r.e, style: styleOf(this.keys[r.c], listOf(r.b).map((b) => this.keys[b]).at(0) ?? null) }));
    this.spanCache.set(lc, { text, spans });
    return spans;
  }
  // Line `li`'s ranges (UTF-16) of the categories `keys` (e.g. strings and
  // comments).
  rangesOf(li, L, keys) {
    const lc = this.lines[li] ?? UNKNOWN;
    if (lc.state === "unknown") return [];
    return utf16Runs(lc.runs.filter((r) => keys.includes(this.keys[r.c])), L, li).map((r) => ({ s: r.s, e: r.e }));
  }
  // Line `li`'s brackets outside its strings and comments, in its
  // coloured part (past the cut, the text is plain: no bracket counts).
  brackets(li, L) {
    const lc = this.lines[li] ?? UNKNOWN;
    const text = L.lines[li];
    const hit = this.bracketCache.get(lc);
    if (hit !== undefined && hit.text === text) return hit.brackets;
    const brackets = bracketsIn(text, this.rangesOf(li, L, LITERAL), colouredUnits(L, li));
    this.bracketCache.set(lc, { text, brackets });
    return brackets;
  }
  // The category at byte `b` of line `li`.
  categoryAt(li, b) {
    const lc = this.lines[li] ?? UNKNOWN;
    if (lc.state === "unknown") return UNKNOWN_AT;
    const exact = lc.state === "exact";
    for (const x of lc.runs) if (b >= x.s && b < x.e) return categoryOf(this.keys[x.c], exact);
    return plainAt(exact);
  }
  // Whether every line in [from, to) is exact.
  exact(from, to) { return this.lines.slice(from, to).every((l) => l.state === "exact"); }
}
/** const UNKNOWN_AT: CategoryAt */
const UNKNOWN_AT = { kind: "unknown" };

// A line's roles: a parsed line's, else unknown.
/** function rolesOf(lc: LineColours) => LineRoles */
function rolesOf(lc) {
  if (lc.state === "exact") return lc.roles;
  if (lc.state === "provisional") return lc.roles;
  return UNKNOWN_ROLES;
}
// A line's runs (none while unknown).
/** function runsOf(lc: LineColours) => Run[] */
function runsOf(lc) { return lc.state === "unknown" ? [] : lc.runs; }
// The category typed text at byte `col` takes: the character's before it
// (or at it, at a line's start). None for plain text or an unknown line.
/** function categoryBefore(lc: LineColours, col: Number) => Number | Null */
function categoryBefore(lc, col) {
  const at = col > 0 ? col - 1 : col;
  return runsOf(lc).filter((r) => at >= r.s && at < r.e).map((r) => r.c).at(0) ?? null;
}
// Line `li` at an earlier version moved through `changes` (none: one of
// them touched it, or replaced the text).
/** function moved(li: Number, changes: LineChange[]) => Number[] */
function moved(li, changes) {
  let at = li;
  for (const p of changes) {
    if (p.kind === "reset") return [];
    if (at < p.line) continue;
    if (at <= p.line + p.removed) return [];
    at += p.added - p.removed;
  }
  return [at];
}
// Line `li`'s runs in bytes as UTF-16 index ranges into its text (held
// lines `L` convert them), in order.
/** function utf16Runs(runs: Run[], L: Lines, li: Number) => Run[] */
function utf16Runs(runs, L, li) {
  return [...runs].sort((x, y) => x.s - y.s).map((r) => ({ s: L.idxAt(li, r.s), e: L.idxAt(li, r.e), c: r.c, b: r.b }));
}
// The bytes of a line the engine colours (fresh-highlight's
// MAX_LINE_BYTES); past them a line is plain text.
const COLOURED_BYTES = 20000;
// How much of line `li` is coloured, in UTF-16 units: all of it, or up to
// the last character boundary within COLOURED_BYTES.
/** function colouredUnits(L: Lines, li: Number) => Number */
function colouredUnits(L, li) { return L.idxBefore(li, COLOURED_BYTES); }
// A run's background word from the module: `1 << 31 | category`, or 0.
/** function backgroundOf(word: Number) => Number | Null */
function backgroundOf(word) { return word >= 0x80000000 ? word - 0x80000000 : null; }
// The style a run draws with, from its category's theme key and the key of
// the category it sits on (none: its own): a syntax key colours the text;
// a diff key fills the row behind it.
/** const styles: Map<String, Drawn> */
const styles = new Map();
/** function styleOf(key: String, under: String | Null) => Drawn */
function styleOf(key, under) {
  const id = `${key} ${under}`;
  const hit = styles.get(id);
  if (hit !== undefined) return hit;
  const fill = (k) => k.startsWith("editor.");
  /** const fg: String */
  const fg = fill(key) ? "" : THEME[key] ?? "";
  /** const bgKey: String[] */
  const bgKey = [...listOf(under), key].filter(fill);
  const bg = bgKey.map((k) => THEME[k] ?? "").at(0) ?? "";
  const st = Object.freeze({ fg, bg, bold: false, italic: false, underline: false, strike: false, mono: false, markup: false, wavy: "" });
  styles.set(id, st);
  return st;
}

// What the editor's features read of a buffer's syntax, over its held
// lines `L`: its colours (a list of at most one), or plain text without.
export class LineSyntax {
  constructor(colours, L) { this.colours = colours; this.L = L; }
  // Bumped whenever what it answers may change.
  get gen() { return this.colours.map((c) => c.gen).at(0) ?? 0; }
  // Line `li`'s spans: its colours' own (not a copy: a layout asks for
  // them at every cell).
  spans(li) {
    for (const c of this.colours) return c.spans(li, this.L);
    return [];
  }
  brackets(li) {
    for (const c of this.colours) return c.brackets(li, this.L);
    return bracketsIn(this.L.lines[li], [], colouredUnits(this.L, li));
  }
  // Line `li`'s UTF-16 ranges coloured as one of `keys`.
  ranges(li, keys) { return this.colours.flatMap((c) => c.rangesOf(li, this.L, keys)); }
  // The roles of held lines [from, to): what is known now (unknown for
  // every line of a buffer with no colours).
  /** method roles(from: Number, to: Number) => LineRoles[] */
  roles(from, to) {
    for (const c of this.colours) return c.roles(from, to);
    return this.L.lines.slice(from, to).map(() => UNKNOWN_ROLES);
  }
  // The category at byte `p`.
  categoryAt(p) {
    const li = this.L.indexOf(p);
    for (const c of this.colours) if (li >= 0) return c.categoryAt(li, p - this.L.starts[li]);
    return this.colours.length === 0 ? plainAt(true) : UNKNOWN_AT;
  }
}

// The runs of bytes [start, end) of buffer `b` in byte offsets, with their
// theme keys, and the version they are for.
function runsIn(b, colours, start, end) {
  const L = b.held;
  const out = [];
  for (let li = L.indexOf(start); li >= 0 && li < L.count && L.starts[li] < end; li++) {
    const lc = colours.lines[li] ?? UNKNOWN;
    for (const r of runsOf(lc)) {
      const s = Math.max(start, L.starts[li] + r.s), e = Math.min(end, L.starts[li] + r.e);
      if (s < e) out.push({ start: s, end: e, key: colours.keys[r.c] });
    }
  }
  return { version: b.textGen, runs: out };
}

// ---------------------------------------------------------------- the host

// Region `r` moved through an edit (`removed` bytes at `at` replaced by
// `inserted`) as the engine moves it: as a span whose ends both have right
// gravity (text inserted at its start goes before it, at its end into it).
/** function movedRegion(
 *   r: SyntaxRegion,
 *   at: Number,
 *   removed: Number,
 *   inserted: Number,
 * ) => SyntaxRegion */
function movedRegion(r, at, removed, inserted) {
  const to = (p) => (p < at ? p : p < at + removed ? at + inserted : p - removed + inserted);
  return { start: to(r.start), end: to(r.end), language: r.language, prefix: r.prefix, streams: r.streams };
}
// A buffer's declared regions (a list of at most one: none when it is
// coloured by a grammar).
/** function regionsOf(host: DocHost) => SyntaxRegion[][] */
function regionsOf(host) { return host.kind === "declared" ? [host.regions] : []; }
// What is wrong with region `r` (number `i`) of a text `len` bytes long
// (none: nothing).
/** function regionProblems(r: SyntaxRegion, i: Number, len: Number) => String[] */
function regionProblems(r, i, len) {
  const whole = (n) => Number.isInteger(n) && n >= 0;
  return [
    ...(whole(r.start) && whole(r.end) && whole(r.prefix) && r.streams.every(whole) ? [] : [`region ${i}: start, end, prefix and streams are whole numbers`]),
    ...(Math.max(r.start, r.end) > len ? [`region ${i} (bytes ${r.start}..${r.end}) is past the text's end (${len} bytes)`] : []),
  ];
}
// A file's extension (a list of at most one: none without one).
/** function extensionOf(path: String) => String[] */
function extensionOf(path) {
  const name = baseName(path);
  const dot = name.lastIndexOf(".");
  return dot > 0 ? [name.slice(dot + 1)] : [];
}

// The engine and the buffers it colours. `onColours(buffer)` is told when a
// buffer's colours changed; `log(text)` notes what the engine did not take.
export class Highlighting {
  constructor(engine, packdump, onColours, log) {
    this.engine = engine;
    this.keys = engine.categories();
    // The server's grammar set: plugin grammars are built onto it.
    this.packdump = packdump;
    // The plugins' grammars in the set the engine has, in order (grammars.js).
    /** this.grammars: Grammar[] */
    this.grammars = [];
    this.onColours = onColours;
    this.log = log;
    /** this.queue: ToEngine[] */
    this.queue = [];
    // The engine's answers to ticketed messages, handed out after the slice
    // that made them, and who waits on each ticket.
    /** this.answers: FromEngine[] */
    this.answers = [];
    /** this.tickets: Map<Number, (FromEngine) => Undefined> */
    this.tickets = new Map();
    this.nextTicket = 1;
    this.docs = new Map(); // buffer id → {buf, colours, applied, open, host, unfollow}
    // Who waits for a range to be exact: {doc, from, to, resolve}.
    this.waiting = [];
    this.scheduled = false;
    // Slices run as separate tasks, so input and drawing go between them.
    this.channel = new MessageChannel();
  }
  tick() { this.channel.port2.postMessage(null); }

  // Start the engine in module `wasm` (wasm.js) with the grammars the
  // server serves.
  static async load(wasm, onColours, log) {
    const r = await fetch("/hl/syntaxes.packdump");
    if (!r.ok) throw new Error(`highlighting: the server did not serve the grammars (${r.status})`);
    const packdump = new Uint8Array(await r.arrayBuffer());
    const engine = new Engine(wasm);
    engine.init(packdump);
    const h = new Highlighting(engine, packdump, onColours, log);
    h.channel.port1.onmessage = () => { h.slice(); };
    return h;
  }

  // Colour buffer `b` (its `syntax`, none: plain text) from now on. Returns
  // its colours.
  attach(b) {
    const colours = new Colours(this.keys);
    const d = { buf: b, colours, applied: 0, open: false, host: BY_GRAMMAR, unfollow: () => {} };
    this.docs.set(b.id, d);
    this.reopen(d);
    d.unfollow = b.followText((c) => {
      if (c.kind === "edit") {
        for (const L of listOf(b.text)) colours.edit(c, L);
        for (const regions of regionsOf(d.host)) d.host = declaredHost(regions.map((r) => movedRegion(r, c.at, c.removed, byteLen(c.text))));
        if (d.open) this.post(editMsg(b.id, c.version, c.at, c.removed, c.text));
      } else {
        // The text replaced: the regions declared over the old one go.
        for (const _ of regionsOf(d.host)) d.host = declaredHost([]);
        this.reopen(d);
      }
    });
    return colours;
  }
  // Buffer `b`'s syntax changed: parse it again.
  resyntax(b) { for (const d of found(this.docs.get(b.id))) this.reopen(d); }
  // Start `d`'s buffer afresh: its held text, in its grammar (plain text
  // without one) or in the regions declared over it.
  reopen(d) {
    this.restart(d);
    for (const regions of regionsOf(d.host)) {
      if (!d.open || regions.length === 0) continue;
      this.sendRegions(d, regions).catch((e) => { this.log(`${d.buf.name}: its syntax regions were not declared again (${failure(e)})`); });
    }
  }
  // Open `d`'s buffer afresh in the engine, without its regions.
  restart(d) {
    const b = d.buf;
    d.colours.reset(b.textGen, b.text);
    this.close(d);
    for (const L of listOf(b.text)) {
      const text = L.lines.join("\n");
      if (d.host.kind === "declared") {
        this.post(declaredMsg(b.id, b.textGen, text));
        d.open = true;
      } else {
        for (const syntax of listOf(this.grammarOf(b))) {
          this.post(openMsg(b.id, b.textGen, syntax, text));
          d.open = true;
        }
      }
    }
  }
  close(d) {
    if (d.open) this.post(closeMsg(d.buf.id, d.buf.textGen));
    d.open = false;
  }
  // The grammar buffer `b` is parsed with: the last plugin grammar its
  // language or file extension matches, else its own syntax (none: plain
  // text).
  /** method grammarOf(b: Buffer) => String | Null */
  grammarOf(b) {
    const ext = filePath(b.origin).flatMap(extensionOf);
    const matching = this.grammars.filter((g) => g.language === b.language || ext.some((e) => g.extensions.includes(e)));
    return matching.map((g) => g.syntax).at(-1) ?? b.syntax;
  }
  detach(b) {
    for (const d of found(this.docs.get(b.id))) {
      d.unfollow();
      this.docs.delete(b.id);
      this.close(d);
    }
  }
  colours(b) { return found(this.docs.get(b.id)).map((d) => d.colours); }
  // Lines [from, to) of `b` are about to be drawn.
  want(b, from, to) { for (const d of found(this.docs.get(b.id))) if (d.open) this.post(wantMsg(b.id, from, to)); }

  // Colour plugin buffer `b` in `regions` (SyntaxRegion, types/fresh.d.js)
  // instead of by a grammar, from now on, until they are declared again or
  // its text is replaced. Resolves with what became of each region, in
  // order, once the engine has them; rejects when `b` is not coloured here
  // or a region does not fit its text.
  /** method declare(b: Buffer, regions: SyntaxRegion[]) => Promise<RegionStatus[]> */
  declare(b, regions) {
    const d = this.docs.get(b.id);
    if (d === undefined) return Promise.reject(new Error(`${b.name} has no syntax colours`));
    if (b.text === null) return Promise.reject(new Error(`${b.name}: its text is not loaded`));
    const problems = regions.flatMap((r, i) => regionProblems(r, i, b.len));
    if (problems.length > 0) return Promise.reject(new Error(`${b.name}: ${problems.join("; ")}`));
    const declared = regions.map((r) => ({ start: r.start, end: r.end, language: r.language, prefix: r.prefix, streams: [...r.streams] }));
    const opened = d.open && d.host.kind === "declared";
    d.host = declaredHost(declared);
    if (opened) d.colours.restyle();
    else this.restart(d);
    return this.sendRegions(d, declared);
  }
  /** method sendRegions(d: {buf: Buffer}, regions: SyntaxRegion[]) => Promise<RegionStatus[]> */
  sendRegions(d, regions) {
    return new Promise((resolve, reject) => {
      const ticket = this.ticket((a) => {
        if (a.kind === "regions") resolve(a.statuses);
        else if (a.kind === "failed") reject(new Error(a.why));
        else reject(new Error(`the engine answered regions with ${a.kind}`));
      });
      this.post(regionsMsg(d.buf.id, ticket, regions));
    });
  }

  // Load grammar set `packdump` (the server's with the plugins'
  // `grammars`, built by grammars.js) in place of the one the engine has.
  // Loading closes every document, so each buffer is closed before it and
  // reopened after it (in the old set when the new one does not load),
  // through `reopen`: versions and colours stay coherent.
  /** method swap(packdump: Uint8Array, grammars: Grammar[]) => Promise<Undefined> */
  swap(packdump, grammars) {
    for (const d of this.docs.values()) this.close(d);
    return new Promise((resolve, reject) => {
      const ticket = this.ticket((a) => {
        if (a.kind === "grammars") this.grammars = grammars;
        for (const d of this.docs.values()) this.reopen(d);
        if (a.kind === "failed") reject(new Error(a.why));
        else resolve(undefined);
      });
      this.post(grammarsMsg(ticket, packdump));
    });
  }

  // A ticket for a message whose answer `f` takes.
  /** method ticket(f: (FromEngine) => Undefined) => Number */
  ticket(f) {
    const t = this.nextTicket++;
    this.tickets.set(t, f);
    return t;
  }

  // The colours of bytes [start, end) of `b`'s held text once they are
  // exact for its text as it is then: {version, runs: [{start, end, key}]}
  // in byte offsets. Rejects when `b` is not coloured or not held there.
  exactly(b, start, end) {
    const d = this.docs.get(b.id);
    if (d === undefined || !d.open) return Promise.reject(new Error(`${b.name} has no syntax colours`));
    const L = b.held;
    const from = L.indexOf(start), to = L.indexOf(Math.max(start, end - 1));
    if (from < 0 || to < 0) return Promise.reject(new Error(`${b.name}: bytes ${start}..${end} are not loaded`));
    return new Promise((resolve) => {
      this.waiting.push({ doc: b.id, from, to: to + 1, resolve: () => { resolve(runsIn(b, d.colours, start, end)); } });
      this.want(b, from, to + 1);
    });
  }
  // Answer whoever waits on a range that is now exact.
  answer() {
    const still = [];
    for (const w of this.waiting) {
      const d = this.docs.get(w.doc);
      if (d !== undefined && d.colours.pending.length === 0 && d.colours.exact(w.from, w.to)) w.resolve();
      else if (d !== undefined) still.push(w);
    }
    this.waiting = still;
  }

  post(m) {
    this.queue.push(m);
    if (!this.scheduled) { this.scheduled = true; this.tick(); }
  }

  // One slice: deliver what was posted, parse SLICE_BYTES, hand out answers.
  slice() {
    this.scheduled = false;
    const queue = this.queue;
    this.queue = [];
    for (const m of queue) this.deliver(m);
    const more = this.engine.work(SLICE_BYTES);
    for (const d of [...this.docs.values()].filter((x) => x.open)) {
      try {
        const lines = this.engine.take(d.buf.id);
        if (lines.length === 0) continue;
        d.colours.apply(d.applied, lines);
        this.onColours(d.buf);
      } catch (e) { this.refused(d, e); }
    }
    this.answer();
    const answers = this.answers;
    this.answers = [];
    for (const a of answers) for (const f of found(this.tickets.get(a.ticket))) { this.tickets.delete(a.ticket); f(a); }
    if ((more || this.queue.length > 0) && !this.scheduled) { this.scheduled = true; this.tick(); }
  }
  /** method deliver(m: ToEngine) => Undefined */
  deliver(m) {
    if (m.kind === "grammars") {
      try {
        this.engine.init(m.packdump);
        this.answers.push(grammarsAnswer(m.ticket));
      } catch (e) { this.answers.push(failedAnswer(m.ticket, failure(e))); }
      return;
    }
    if (m.kind === "regions") {
      try { this.answers.push(regionsAnswer(m.ticket, this.engine.regions(m.doc, m.regions))); }
      catch (e) { this.answers.push(failedAnswer(m.ticket, failure(e))); }
      return;
    }
    const d = this.docs.get(m.doc);
    try {
      if (m.kind === "open") { this.engine.open(m.doc, m.syntax, m.text); for (const x of found(d)) x.applied = m.version; }
      else if (m.kind === "declared") { this.engine.openDeclared(m.doc, m.text); for (const x of found(d)) x.applied = m.version; }
      else if (m.kind === "edit") { this.engine.edit(m.doc, m.at, m.removed, m.text); for (const x of found(d)) x.applied = m.version; }
      else if (m.kind === "close") { this.engine.close(m.doc); for (const x of found(d)) x.applied = m.version; }
      else this.engine.want(m.doc, m.from, m.to);
    } catch (e) { for (const x of found(d)) this.refused(x, e); }
  }
  // The engine refused `d`'s buffer (an unknown syntax, an edit it cannot
  // place, a grammar that failed): it shows plain text from now on, and
  // says why.
  refused(d, e) {
    d.open = false;
    this.detach(d.buf);
    d.buf.emit(errorNews(`${d.buf.name}: no syntax colours (${failure(e)})`));
    this.onColours(d.buf);
  }
}
