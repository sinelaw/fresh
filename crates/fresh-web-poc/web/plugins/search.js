// Search and replace.
//
// In the buffer: a bar above the editor with match case, whole word,
// regex and in-selection toggles, a match count, previous/next, replace
// and replace all. Every match on screen is marked; the current one more
// strongly. A buffer held whole is searched here; a huge one is searched by
// the server. As in the editor: Find in Selection (and Find or Replace
// opened over a selection of several lines) searches only that range,
// shaded while it is searched; Find Selection Next / Previous (Ctrl+F3,
// Alt+N / Ctrl+Shift+F3, Alt+P) search for the selection or the word at
// the cursor without the bar; Query Replace (Ctrl+Alt+R) asks at each
// match: y replace, n skip, a or ! replace the rest, q or c stop.
//
// In the workspace: a panel in the bottom dock listing every match by file
// (the server walks the workspace, gitignore-aware); Enter opens a match,
// Replace All rewrites them through the documents, so open editors, other
// clients and undo all see the change.

import * as fresh from "../js/api.js";
import { byteLen, listOf, insOp, delOp } from "../js/api.js";
import * as ui from "../js/ui.js";

// The searched range, the matches and the current match.
/** const SCOPE: Style */
const SCOPE = { fg: null, bg: "search.scope_bg", bold: false, italic: false, underline: false, strike: false, mono: false, extendToLineEnd: true };
/** const MATCH: Style */
const MATCH = { fg: "search.match_fg", bg: "search.match_bg", bold: false, italic: false, underline: false, strike: false, mono: false, extendToLineEnd: false };
/** const CURRENT: Style */
const CURRENT = { fg: "search.current_fg", bg: "search.current_bg", bold: true, italic: false, underline: false, strike: false, mono: false, extendToLineEnd: false };

// The word (letters, digits, _) at byte `pos` of the held lines `L`, if
// there is one.
/** function wordIn(L: HeldLines, pos: Number) => Range | Null */
function wordIn(L, pos) {
  for (const i of fresh.heldLineAt(L, pos)) {
    const line = L.lines[i];
    let col = 0, bytes = 0;
    while (col < line.length && bytes < pos - L.starts[i]) { bytes += byteLen(line[col]); col++; }
    let s = col, e = col;
    while (s > 0 && /[\p{L}\p{N}_]/u.test(line[s - 1])) s--;
    while (e < line.length && /[\p{L}\p{N}_]/u.test(line[e])) e++;
    if (s === e) return null;
    return { start: L.starts[i] + byteLen(line.slice(0, s)), end: L.starts[i] + byteLen(line.slice(0, e)) };
  }
  return null;
}

// A match: its bytes, and its text (null until held, for a match the
// server found in a huge file).
/** type Match = {pos: Number, len: Number, text: String | Null} */
// A query replace's question in a buffer: where it started, whether it
// went round the end, how many were replaced, of how many asked.
/** type Question = {start: Number, wrapped: Boolean, done: Number, of: Number} */
/** const match: (Number, Number, String | Null) => Match */
const match = (pos, len, text) => ({ pos, len, text });

// The most matches looked for in a buffer.
const MATCH_LIMIT = 20000;

export function activate(app) {
  const opts = { case: false, word: false, regex: false };
  let query = "";
  let replacement = "";
  /** let matches: Match[] */
  let matches = []; // in the active buffer
  let current = -1;
  let showReplace = false;
  let error = "";
  // Query replace: whether a replacement asks at each match, and the
  // question being asked in a buffer ({start, wrapped, done, of}), by
  // buffer id.
  let confirmEach = false;
  /** const asking: Map<Number, Question> */
  const asking = new Map();
  /** const questionIn: (BufferHandle | Null) => Question | Undefined */
  const questionIn = (b) => (b === null ? undefined : asking.get(b.id));
  // The find bar, if it is on screen.
  const findBar = () => fresh.shown(app, "find");

  // Whether the toggles ask for plain text, case and all (what a huge
  // file's search on the server finds).
  const plainOnly = () => opts.case && !opts.word && !opts.regex;
  const toRegex = (q, global = true) => {
    let src = opts.regex ? q : q.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
    if (opts.word) src = `\\b(?:${src})\\b`;
    return new RegExp(src, (global ? "g" : "") + (opts.case ? "" : "i") + "u");
  };

  const buffer = () => fresh.activeBuffer(app);
  const deco = (b) => b.decorations("search");
  // The range searched (Find in Selection): an overlay, so edits move it.
  const scopeDeco = (b) => b.decorations("search-scope");
  const scopeOf = (b) => {
    if (b == null) return null;
    const ranges = scopeDeco(b).overlays();
    return ranges.length ? ranges[0] : null;
  };
  const setScope = (b, s) => {
    scopeDeco(b).clear();
    scopeDeco(b).overlay(s.start, s.end, SCOPE, { when: "always", mark: false, above: false });
    b.render();
  };
  const clearScope = (b) => {
    scopeDeco(b).clear();
    b.render();
  };

  // All matches in a buffer held whole, as byte ranges: the first
  // MATCH_LIMIT (more are not looked for; the bar says so).
  /** const findAll: (BufferHandle) => Match[] */
  const findAll = (b) => {
    const L = b.lines();
    if (L === null || !b.whole() || !query) return [];
    let re;
    try { re = toRegex(query, true); error = ""; } catch (e) { error = "invalid regex"; return []; }
    /** const out: Match[] */
    const out = [];
    const scope = scopeOf(b);
    L.lines.forEach((line, i) => {
      if (scope != null && (L.starts[i] + fresh.byteLen(line) < scope.start || L.starts[i] > scope.end)) return;
      re.lastIndex = 0;
      for (;;) {
        const m = re.exec(line);
        if (!m) break;
        const found = m[0];
        if (found === "") { re.lastIndex++; continue; }
        // A global regex's lastIndex is the end of the match it found.
        const index = re.lastIndex - found.length;
        const pos = L.starts[i] + fresh.byteLen(line.slice(0, index));
        const len = fresh.byteLen(found);
        if (scope != null && (pos < scope.start || pos + len > scope.end)) continue;
        out.push(match(pos, len, found));
        if (out.length >= MATCH_LIMIT) return;
      }
    });
    return out;
  };

  const mark = () => {
    const b = buffer();
    if (!b) return;
    const d = deco(b);
    d.clear();
    matches.forEach((m, i) => d.overlay(m.pos, m.pos + m.len, i === current ? CURRENT : MATCH, { when: "always", mark: true, above: true }));
    b.render();
  };

  const update = () => {
    const b = buffer();
    if (!b) return;
    matches = findAll(b);
    const v = fresh.activeView(app);
    current = matches.findIndex((m) => m.pos >= (v ? v.cursor() : 0));
    if (current < 0 && matches.length) current = 0;
    mark();
    renderBar();
  };

  const go = async (dir) => {
    const b = buffer();
    const v = fresh.activeView(app);
    if (!b || !v || !query) return;
    if (!b.whole()) {
      // A huge file: the server finds the next match, as plain text with
      // case: the toggles would be ignored, so it isn't asked.
      if (!plainOnly()) { fresh.message(app, "In a huge file, find looks for plain text, case and all: turn off Aa, ab and .*"); return; }
      const sel = v.selection();
      const start = dir > 0 ? (sel ? sel.end : v.cursor()) : (sel ? sel.start : v.cursor());
      const res = await fresh.searchDocument(app, b, query, start, dir < 0);
      if (res == null) { fresh.message(app, `Not found: ${query}`); return; }
      matches = [match(res, fresh.byteLen(query), null)];
      current = 0;
    } else {
      if (!matches.length) { fresh.message(app, `Not found: ${query}`); renderBar(); return; }
      const at = v.cursor();
      if (dir > 0) {
        const sel = v.selection();
        const cur = matches[current];
        const start = sel != null && cur != null && sel.start === cur.pos ? sel.end : at;
        current = matches.findIndex((m) => m.pos >= start && !(sel != null && m.pos === sel.start && sel.end === m.pos + m.len));
        if (current < 0) current = 0;
      } else {
        const sel = v.selection();
        const start = sel ? sel.start : at;
        let k = -1;
        for (let i = 0; i < matches.length; i++) if (matches[i].pos < start) k = i;
        current = k < 0 ? matches.length - 1 : k;
      }
    }
    const m = matches[current];
    await v.select(m.pos, m.pos + m.len, { center: true });
    if (m.text == null) m.text = b.text(m.pos, m.pos + m.len); // found by the server
    mark();
    renderBar();
  };

  // The replacement for match `m`.
  const replacementFor = (m) => {
    const text = m.text;
    return opts.regex && text != null ? text.replace(toRegex(query, false), replacement) : replacement;
  };

  // Query replace: ask about the match at or after the cursor (wrapping
  // once, back to where it started), and the answer acts on it.
  const ask = async (from) => {
    const b = buffer();
    const v = fresh.activeView(app);
    const q = questionIn(b);
    if (!q) return;
    update();
    let i = matches.findIndex((m) => m.pos >= from);
    if (i >= 0 && q.wrapped && matches[i].pos >= q.start) i = -1;
    if (i < 0 && !q.wrapped) { q.wrapped = true; i = matches.length && matches[0].pos < q.start ? 0 : -1; }
    if (i < 0 || !v) { finishAsking(); return; }
    current = i;
    const m = matches[i];
    await v.select(m.pos, m.pos + m.len, { center: true });
    mark();
    renderBar();
  };
  const startAsking = async () => {
    const b = buffer();
    const v = fresh.activeView(app);
    if (!b || !v || !query) return;
    if (!b.whole()) { fresh.message(app, "A huge file is searched by the server: Query Replace needs a file held whole"); return; }
    const sel = v.selection();
    const q = { start: sel ? sel.start : v.cursor(), wrapped: false, done: 0, of: 0 };
    asking.set(b.id, q);
    await ask(q.start);
    const bar = findBar();
    if (bar) bar.focus("replace");
  };
  const finishAsking = () => {
    const b = buffer();
    const q = questionIn(b);
    if (!b || !q) return;
    const { done, of } = q;
    asking.delete(b.id);
    renderBar();
    fresh.message(app, of ? `Replaced ${done} of ${of} match${of === 1 ? "" : "es"}` : `Not found: ${query}`);
  };
  // An answer to the question: y, n, a (or !), q (or c).
  const answer = async (k) => {
    const b = buffer();
    const q = questionIn(b);
    const m = matches[current];
    if (!b || !q || !m) { finishAsking(); return; }
    if (k === "y") {
      const text = replacementFor(m);
      b.replace(m.pos, m.pos + m.len, text);
      q.done++; q.of++;
      await ask(m.pos + fresh.byteLen(text));
    } else if (k === "n") {
      q.of++;
      await ask(m.pos + Math.max(1, m.len));
    } else if (k === "a" || k === "!") {
      // This match and every one after it, back round to where it started.
      const rest = matches.filter((x) => (q.wrapped ? x.pos >= m.pos && x.pos < q.start : x.pos >= m.pos || x.pos < q.start));
      /** const ops: Op[] */
      const ops = [];
      for (const x of [...rest].sort((p, q) => q.pos - p.pos)) {
        ops.push(delOp(x.pos, x.len));
        const text = replacementFor(x);
        if (text) ops.push(insOp(x.pos, text));
      }
      if (ops.length) b.edit(ops);
      q.done += rest.length; q.of += rest.length;
      finishAsking();
      update();
    } else finishAsking();
  };

  // Replace the current match (marked in the buffer) and go to the next.
  const replaceOne = async () => {
    const b = buffer();
    const v = fresh.activeView(app);
    const m = matches[current];
    if (!b || !v || !m) { await go(1); return; }
    const text = replacementFor(m);
    b.replace(m.pos, m.pos + m.len, text);
    await v.setCursor(m.pos + fresh.byteLen(text), { center: false });
    update();
    go(1);
  };

  const replaceAll = () => {
    const b = buffer();
    if (!b || !matches.length) return;
    const re = toRegex(query, false);
    // One edit, last match first so earlier offsets stay valid.
    /** const ops: Op[] */
    const ops = [];
    for (const m of [...matches].reverse()) {
      ops.push(delOp(m.pos, m.len));
      const found = m.text;
      const text = opts.regex && found != null ? found.replace(re, replacement) : replacement;
      if (text) ops.push(insOp(m.pos, text));
    }
    const n = matches.length;
    b.edit(ops);
    update();
    const more = n >= MATCH_LIMIT ? " (the first ones found: Replace All again for more)" : "";
    fresh.message(app, `Replaced ${n} occurrence${n === 1 ? "" : "s"}${more}`);
  };

  // The match case / whole word / regex toggles; `after` runs on a change.
  const toggles = (after) => [
    ui.toggle({ ...ui.TOGGLE, key: "case", on: opts.case, title: "Match case", onChange: (on) => { opts.case = on; after(); } }, "Aa"),
    ui.toggle({ ...ui.TOGGLE, key: "word", on: opts.word, title: "Whole word", onChange: (on) => { opts.word = on; after(); } }, "ab"),
    ui.toggle({ ...ui.TOGGLE, key: "regex", on: opts.regex, title: "Regular expression", onChange: (on) => { opts.regex = on; after(); } }, ".*"),
  ];
  // Search the selection only (on: the selection now becomes the range).
  const scopeToggle = () => {
    const b = buffer();
    return ui.toggle({
      ...ui.TOGGLE,
      key: "scope",
      on: !!scopeOf(b),
      title: "Find in selection",
      onChange: (on) => {
        if (!b) return;
        const av = fresh.activeView(app);
        const sel = av === null ? null : av.selection();
        if (on && !sel) { fresh.message(app, "Select the text to search in first"); return; }
        if (on && sel != null) setScope(b, sel);
        else clearScope(b);
        update();
      },
    }, "⊂");
  };
  // While asking: the replace field takes y, n, a, !, q, c and Escape as
  // answers (and types nothing).
  const askKey = (e) => {
    if (!questionIn(buffer())) return false;
    const k = e.key === "Escape" ? "q" : e.key.toLowerCase();
    if (["y", "n", "a", "!", "q", "c"].includes(k)) answer(k);
    return e.key.length === 1 || e.key === "Escape" || e.key === "Enter";
  };

  const renderBar = () => {
    const bar = findBar();
    if (!bar) return;
    const b = buffer();
    const q = questionIn(b);
    const huge = b != null && !b.whole();
    const count = !query ? "" : error || (huge ? (plainOnly() ? "huge file: Enter searches" : "huge file: plain text only (turn off Aa, ab, .*)") : matches.length ? `${current + 1} of ${matches.length}${matches.length >= MATCH_LIMIT ? "+" : ""}` : "no results");
    const cur = matches[current];
    const shown = q != null && cur != null ? replacementFor(cur) : "";
    bar.render(ui.col(ui.BOX, [
      ui.row(ui.BOX, [
        ui.input({
          ...ui.INPUT,
          key: "find",
          value: query,
          placeholder: "Find",
          grow: true,
          onInput: (q) => { query = q; update(); },
          onEnter: (_q, e) => { go(e.shiftKey ? -1 : 1); },
          onKey: (e) => { if (e.key !== "Escape") return false; close(); return true; },
        }),
        ...toggles(update),
        scopeToggle(),
        ui.text({ ...ui.TEXT, dim: true }, count),
        ui.button({ ...ui.BUTTON, title: "Previous (Shift+Enter)", onClick: () => { go(-1); } }, "↑"),
        ui.button({ ...ui.BUTTON, title: "Next (Enter)", onClick: () => { go(1); } }, "↓"),
        ui.button({ ...ui.BUTTON, title: "Toggle replace", onClick: () => { showReplace = !showReplace; renderBar(); } }, "⇄"),
        ui.button({ ...ui.BUTTON, title: "Close (Esc)", onClick: close }, "×"),
      ]),
      ...(showReplace ? [ui.row(ui.BOX, [
        ui.input({
          ...ui.INPUT,
          key: "replace",
          value: replacement,
          placeholder: q ? "y n a q" : "Replace",
          grow: true,
          onInput: (r) => { if (!q) replacement = r; },
          onEnter: () => { if (q) return; if (confirmEach) startAsking(); else replaceOne(); },
          onKey: (e) => { if (askKey(e)) return true; if (e.key !== "Escape") return false; close(); return true; },
        }),
        ui.toggle({ ...ui.TOGGLE, key: "ask", on: confirmEach, title: "Ask at each match (Query Replace): y, n, a, q", onChange: (on) => { confirmEach = on; renderBar(); } }, "?"),
        ui.button({ ...ui.BUTTON, onClick: () => (confirmEach ? startAsking() : replaceOne()), disabled: !!q }, confirmEach ? "Replace…" : "Replace"),
        ui.button({ ...ui.BUTTON, onClick: replaceAll, disabled: !matches.length || !!q }, "Replace All"),
      ])] : []),
      ...(q ? [ui.row({ ...ui.BOX, class: "search-ask" }, [
        ui.text(ui.TEXT, `Replace with “${shown}”?`),
        ui.button({ ...ui.BUTTON, title: "Replace this one (y)", onClick: () => answer("y") }, "Yes  y"),
        ui.button({ ...ui.BUTTON, title: "Skip this one (n)", onClick: () => answer("n") }, "No  n"),
        ui.button({ ...ui.BUTTON, title: "Replace this and the rest (a, !)", onClick: () => answer("a") }, "All  a"),
        ui.button({ ...ui.BUTTON, title: "Stop (q, c, Esc)", onClick: () => answer("q") }, "Quit  q"),
        ui.text({ ...ui.TEXT, dim: true }, `${q.done} replaced`),
      ])] : []),
    ]));
  };

  // The bar: a selection of one line is the query; one of several lines
  // (or `inSelection`) is the range searched. `ask`: a replacement asks at
  // each match.
  const open = (replace, { inSelection, ask }) => {
    const v = fresh.activeView(app);
    const b = buffer();
    const sel = v === null ? null : v.selection();
    const t = v !== null && sel !== null && v.buffer.whole() ? v.buffer.text(sel.start, sel.end) : null;
    if (b) {
      if (sel != null && (inSelection || (t != null && t.includes("\n")))) setScope(b, sel);
      else clearScope(b);
    }
    if (t != null && !t.includes("\n") && scopeOf(b) == null) query = t;
    showReplace = replace;
    confirmEach = ask;
    asking.clear();
    const bar = fresh.panel(app, { id: "find", title: "Find", place: "bar", onClose: () => { asking.clear(); const ab = buffer(); if (ab) { deco(ab).clear(); clearScope(ab); } } });
    update();
    bar.focus(replace && query ? "replace" : "find");
  };
  const close = () => { const bar = findBar(); if (bar) bar.close(); };

  // Find Selection Next / Previous: the selection (or the word at the
  // cursor) is the query, searched as it is written; its matches are
  // marked until Escape.
  const findSelection = async (dir) => {
    const v = fresh.activeView(app);
    const b = buffer();
    if (!v || !b) return;
    const sel = v.selection();
    const at = matches[current];
    const onMatch = sel != null && at != null && sel.start === at.pos && sel.end === at.pos + at.len && b.text(sel.start, sel.end) === at.text;
    if (!onMatch) {
      const L = b.lines();
      const word = sel !== null ? sel : L === null ? null : wordIn(L, v.cursor());
      const t = word === null ? null : b.text(word.start, word.end);
      if (word === null || t === null || t.includes("\n")) { fresh.message(app, "Select the text to find first"); return; }
      query = opts.regex ? t.replace(/[.*+?^${}()|[\]\\]/g, "\\$&") : t;
      if (findBar()) clearScope(b);
      if (sel == null) await v.select(word.start, word.end, { center: false });
      update();
    }
    await go(dir);
  };
  fresh.register(app, "search.find", { title: "Find…", hidden: false, run: () => open(false, { inSelection: false, ask: false }) });
  fresh.register(app, "search.replace", { title: "Replace…", hidden: false, run: () => open(true, { inSelection: false, ask: false }) });
  fresh.register(app, "search.findInSelection", { title: "Find in Selection…", hidden: false, run: () => open(false, { inSelection: true, ask: false }) });
  fresh.register(app, "search.queryReplace", { title: "Query Replace…", hidden: false, run: () => open(true, { inSelection: false, ask: true }) });
  fresh.register(app, "search.findNext", { title: "Find Next", hidden: false, run: () => go(1) });
  fresh.register(app, "search.findPrevious", { title: "Find Previous", hidden: false, run: () => go(-1) });
  fresh.register(app, "search.findSelectionNext", { title: "Find Selection Next", hidden: false, run: () => findSelection(1) });
  fresh.register(app, "search.findSelectionPrevious", { title: "Find Selection Previous", hidden: false, run: () => findSelection(-1) });
  fresh.register(app, "search.clear", { title: "Clear Search Highlights", hidden: false, run: () => clearMarks() });
  fresh.bindKey(app, "C-M-r", "search.queryReplace");
  fresh.bindKey(app, "C-F3", "search.findSelectionNext");
  fresh.bindKey(app, "M-n", "search.findSelectionNext");
  fresh.bindKey(app, "C-S-F3", "search.findSelectionPrevious");
  fresh.bindKey(app, "M-p", "search.findSelectionPrevious");
  // The marks of a search made without the bar go with Escape.
  const clearMarks = () => { const b = buffer(); matches = []; current = -1; if (b) { deco(b).clear(); b.render(); } };
  fresh.bindKey(app, "C-f", "search.find");
  fresh.bindKey(app, "C-h", "search.replace");
  fresh.bindKey(app, "C-r", "search.replace");
  fresh.bindKey(app, "F3", "search.findNext");
  fresh.bindKey(app, "S-F3", "search.findPrevious");
  fresh.onBuffer(app, "buffer.changed", (b) => { const a = buffer(); if (findBar() != null && a != null && b.id === a.id) update(); });
  fresh.onBuffer(app, "buffer.activated", (_b) => { if (findBar()) update(); });
  fresh.on(app, "escape", () => { if (findBar()) close(); else clearMarks(); });

  // ---------------------------------------------------------------- the workspace

  // (`unsearched`: files that could not be searched, with why.)
  const pq = { query: "", replace: "", results: [], files: 0, truncated: false, unsearched: [], busy: false, error: "" };
  // The project search runs a moment after typing stops.
  const later = fresh.debouncer();
  const runSearch = async () => {
    if (!pq.query) { pq.results = []; renderPanel(); return; }
    pq.busy = true;
    renderPanel();
    try {
      const r = await fresh.searchWorkspace(app, pq.query, { regex: opts.regex, case: opts.case, whole: opts.word, max: 5000 });
      pq.results = r.matches;
      pq.files = r.files;
      pq.truncated = r.truncated;
      pq.unsearched = r.unsearched;
      pq.error = "";
    } catch (e) {
      pq.results = [];
      pq.error = String(e).replace(/^search.project: /, "");
    }
    pq.busy = false;
    renderPanel();
  };
  const rows = () => {
    const out = [];
    let last = null;
    for (const m of pq.results) {
      if (m.path !== last) { out.push({ header: true, path: m.path, line: 0, col: 0, len: 0, text: "" }); last = m.path; }
      out.push({ header: false, path: m.path, line: m.line, col: m.col, len: m.len, text: m.text });
    }
    return out;
  };
  // A match selected (a click, the arrows) shows in the preview tab, the
  // keyboard staying in the list; Enter or a double-click opens it in a
  // tab of its own.
  const openMatch = async (m, preview) => {
    if (!m || m.header) return;
    for (const b of listOf(await fresh.open(app, m.path, { line: m.line + 1, column: 1, place: "tab", preview, focus: !preview }))) {
      for (const v of listOf(fresh.activeView(app))) {
        for (const L of listOf(b.lines())) {
          for (const i of fresh.heldIndex(L, m.line)) {
            const start = L.starts[i] + m.col;
            await v.select(start, start + m.len, { center: true });
          }
        }
      }
    }
  };
  const replaceEverywhere = async () => {
    const byFile = new Map();
    for (const m of pq.results) {
      const list = byFile.get(m.path);
      if (list) list.push(m);
      else byFile.set(m.path, [m]);
    }
    let n = 0, unheld = 0;
    const re = toRegex(pq.query, false);
    for (const [path, list] of byFile) {
      const b = await fresh.load(app, path);
      const L = b.lines();
      if (L === null) continue;
      /** const ops: Op[] */
      const ops = [];
      for (const m of [...list].reverse()) {
        // A match in a part of a huge file not held here is left alone.
        const found = fresh.heldIndex(L, m.line).map((i) => L.starts[i] + m.col)
          .flatMap((start) => listOf(b.text(start, start + m.len)).map((old) => ({ start, old })));
        if (found.length === 0) unheld++;
        for (const { start, old } of found) {
          ops.push(delOp(start, m.len));
          const text = opts.regex ? old.replace(re, pq.replace) : pq.replace;
          if (text) ops.push(insOp(start, text));
          n++;
        }
      }
      b.edit(ops);
    }
    const left = unheld > 0 ? `; ${unheld} in parts of huge files not loaded were left` : "";
    fresh.message(app, `Replaced ${n} match${n === 1 ? "" : "es"} in ${byFile.size} file(s)${left}; not saved yet (Save All)`);
    runSearch();
  };
  const renderPanel = () => {
    const panel = fresh.shown(app, "project-search");
    if (!panel) return;
    const unsearched = pq.unsearched.length === 0 ? "" : ` · ${pq.unsearched.length} file(s) not searched (${pq.unsearched[0].path}: ${pq.unsearched[0].why})`;
    const status = pq.busy ? "searching…" : pq.error ? pq.error : pq.query ? `${pq.results.length}${pq.truncated ? "+" : ""} matches in ${pq.files} files${unsearched}` : "";
    panel.render(ui.col(ui.BOX, [
      ui.row(ui.BOX, [
        ui.input({
          ...ui.INPUT,
          key: "q",
          value: pq.query,
          placeholder: "Search the workspace",
          grow: true,
          list: ["results"],
          onInput: (q) => { pq.query = q; later("search", 250, () => { runSearch(); }); },
          onEnter: () => { runSearch(); },
        }),
        ...toggles(() => { update(); runSearch(); }),
        ui.input({ ...ui.INPUT, key: "r", value: pq.replace, placeholder: "Replace with", grow: true, onInput: (r) => { pq.replace = r; } }),
        ui.button({ ...ui.BUTTON, onClick: replaceEverywhere, disabled: !pq.results.length }, "Replace All"),
        ui.text({ ...ui.TEXT, dim: true }, status),
      ]),
      ui.list({
        ...ui.listProps("results", rows(), (m) => m.header
          ? ui.text({ ...ui.TEXT, bold: true }, m.path)
          : ui.row(ui.BOX, [ui.text({ ...ui.TEXT, dim: true }, `  ${m.line + 1}:${m.col + 1}`), ui.text({ ...ui.TEXT, mono: true }, m.text.trim())])),
        onSelect: (m) => openMatch(m, true),
        onActivate: (m) => openMatch(m, false),
      }),
    ]));
  };
  const openPanel = () => {
    const v = fresh.activeView(app);
    const sel = v === null ? null : v.selection();
    if (v !== null && sel !== null && v.buffer.whole()) {
      const t = v.buffer.text(sel.start, sel.end);
      if (t != null && !t.includes("\n")) pq.query = t;
    }
    const panel = fresh.panel(app, { id: "project-search", title: "Search", place: "bottom", onClose: null });
    renderPanel();
    panel.focus("q");
    if (pq.query) runSearch();
  };
  fresh.register(app, "search.project", { title: "Search in Workspace…", hidden: false, run: openPanel });
  fresh.bindKey(app, "C-S-f", "search.project");
}
