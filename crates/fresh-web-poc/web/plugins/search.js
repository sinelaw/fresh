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

export function activate(fresh) {
  const { h } = fresh;
  const opts = { case: false, word: false, regex: false };
  let query = "";
  let replacement = "";
  let matches = []; // [{pos, len}] in the active buffer
  let current = -1;
  let bar = null;
  let showReplace = false;
  let error = "";
  // Query replace: whether a replacement asks at each match, and the
  // question being asked ({start, wrapped, done, of}) or null.
  let confirmEach = false;
  let asking = null;

  const toRegex = (q, global = true) => {
    let src = opts.regex ? q : q.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
    if (opts.word) src = `\\b(?:${src})\\b`;
    return new RegExp(src, (global ? "g" : "") + (opts.case ? "" : "i") + "u");
  };

  const buffer = () => fresh.buffers.active();
  const deco = (b) => b.decorations("search");
  // The range searched (Find in Selection): an overlay, so edits move it.
  const scopeDeco = (b) => b.decorations("search-scope");
  const scopeOf = (b) => (b ? scopeDeco(b).overlays()[0] || null : null);
  const setScope = (b, s) => {
    scopeDeco(b).clear();
    if (s) scopeDeco(b).overlay(s.start, s.end, { bg: "search.scope_bg", extendToLineEnd: true });
    b.render();
  };

  // All matches in a buffer held whole, as byte ranges.
  const findAll = (b) => {
    const L = b.lines();
    if (!L || !b.whole || !query) return [];
    let re;
    try { re = toRegex(query); error = ""; } catch (e) { error = "invalid regex"; return []; }
    const out = [];
    const scope = scopeOf(b);
    L.lines.forEach((line, i) => {
      if (scope && (L.starts[i] + fresh.text.byteLen(line) < scope.start || L.starts[i] > scope.end)) return;
      re.lastIndex = 0;
      for (let m = re.exec(line); m; m = re.exec(line)) {
        if (m[0] === "") { re.lastIndex++; continue; }
        const pos = L.starts[i] + fresh.text.byteLen(line.slice(0, m.index));
        const len = fresh.text.byteLen(m[0]);
        if (scope && (pos < scope.start || pos + len > scope.end)) continue;
        out.push({ pos, len, text: m[0] });
        if (out.length > 20000) return;
      }
    });
    return out;
  };

  const mark = () => {
    const b = buffer();
    if (!b) return;
    const d = deco(b);
    d.clear();
    matches.forEach((m, i) => d.overlay(m.pos, m.pos + m.len, i === current ? { bg: "search.current_bg", fg: "search.current_fg", bold: true } : { bg: "search.match_bg", fg: "search.match_fg" }, { mark: true, above: true }));
    b.render();
  };

  const update = () => {
    const b = buffer();
    if (!b) return;
    matches = findAll(b);
    const v = fresh.views.active();
    current = matches.findIndex((m) => m.pos >= (v ? v.cursor : 0));
    if (current < 0 && matches.length) current = 0;
    mark();
    renderBar();
  };

  const go = async (dir) => {
    const b = buffer();
    const v = fresh.views.active();
    if (!b || !v || !query) return;
    if (!b.whole) {
      // A huge file: the server finds the next match (plain text).
      const sel = v.selection;
      const start = dir > 0 ? (sel ? sel.end : v.cursor) : (sel ? sel.start : v.cursor);
      const res = await fresh.host.searchDocument(b, query, start, dir < 0);
      if (res == null) { fresh.ui.message(`Not found: ${query}`); return; }
      matches = [{ pos: res, len: fresh.text.byteLen(query) }];
      current = 0;
    } else {
      if (!matches.length) { fresh.ui.message(`Not found: ${query}`); renderBar(); return; }
      const at = v.cursor;
      if (dir > 0) {
        const sel = v.selection;
        const start = sel && matches[current] && sel.start === matches[current].pos ? sel.end : at;
        current = matches.findIndex((m) => m.pos >= start && !(sel && m.pos === sel.start && sel.end === m.pos + m.len));
        if (current < 0) current = 0;
      } else {
        const sel = v.selection;
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
  const replacementFor = (m) => (opts.regex ? m.text.replace(toRegex(query, false), replacement) : replacement);

  // Query replace: ask about the match at or after the cursor (wrapping
  // once, back to where it started), and the answer acts on it.
  const ask = async (from) => {
    const b = buffer();
    const v = fresh.views.active();
    update();
    let i = matches.findIndex((m) => m.pos >= from);
    if (i >= 0 && asking.wrapped && matches[i].pos >= asking.start) i = -1;
    if (i < 0 && !asking.wrapped) { asking.wrapped = true; i = matches.length && matches[0].pos < asking.start ? 0 : -1; }
    if (i < 0 || !b || !v) return finishAsking();
    current = i;
    const m = matches[i];
    await v.select(m.pos, m.pos + m.len, { center: true });
    mark();
    renderBar();
  };
  const startAsking = async () => {
    const b = buffer();
    const v = fresh.views.active();
    if (!b || !v || !query) return;
    if (!b.whole) { fresh.ui.message("A huge file is searched by the server: Query Replace needs a file held whole"); return; }
    const sel = v.selection;
    asking = { start: sel ? sel.start : v.cursor, wrapped: false, done: 0, of: 0 };
    await ask(asking.start);
    bar?.focus("replace");
  };
  const finishAsking = () => {
    if (!asking) return;
    const { done, of } = asking;
    asking = null;
    renderBar();
    fresh.ui.message(of ? `Replaced ${done} of ${of} match${of === 1 ? "" : "es"}` : `Not found: ${query}`);
  };
  // An answer to the question: y, n, a (or !), q (or c).
  const answer = async (k) => {
    const b = buffer();
    const m = matches[current];
    if (!asking || !b || !m) return finishAsking();
    if (k === "y") {
      const text = replacementFor(m);
      b.replace(m.pos, m.pos + m.len, text);
      asking.done++; asking.of++;
      await ask(m.pos + fresh.text.byteLen(text));
    } else if (k === "n") {
      asking.of++;
      await ask(m.pos + Math.max(1, m.len));
    } else if (k === "a" || k === "!") {
      // This match and every one after it, back round to where it started.
      const rest = matches.filter((x) => (asking.wrapped ? x.pos >= m.pos && x.pos < asking.start : x.pos >= m.pos || x.pos < asking.start));
      const ops = [];
      for (const x of [...rest].sort((p, q) => q.pos - p.pos)) {
        ops.push({ t: "del", pos: x.pos, len: x.len });
        const text = replacementFor(x);
        if (text) ops.push({ t: "ins", pos: x.pos, text });
      }
      if (ops.length) b.edit(ops);
      asking.done += rest.length; asking.of += rest.length;
      finishAsking();
      update();
    } else finishAsking();
  };

  // Replace the current match (marked in the buffer) and go to the next.
  const replaceOne = async () => {
    const b = buffer();
    const v = fresh.views.active();
    const m = matches[current];
    if (!b || !v || !m) return go(1);
    const text = replacementFor(m);
    b.replace(m.pos, m.pos + m.len, text);
    await v.setCursor(m.pos + fresh.text.byteLen(text));
    update();
    go(1);
  };

  const replaceAll = () => {
    const b = buffer();
    if (!b || !matches.length) return;
    const re = toRegex(query, false);
    // One edit, last match first so earlier offsets stay valid.
    const ops = [];
    for (const m of [...matches].reverse()) {
      ops.push({ t: "del", pos: m.pos, len: m.len });
      const text = opts.regex ? m.text.replace(re, replacement) : replacement;
      if (text) ops.push({ t: "ins", pos: m.pos, text });
    }
    const n = matches.length;
    b.edit(ops);
    update();
    fresh.ui.message(`Replaced ${n} occurrence${n === 1 ? "" : "s"}`);
  };

  // The match case / whole word / regex toggles; `after` runs on a change.
  const toggles = (after) => [
    h("toggle", { key: "case", on: opts.case, title: "Match case", onChange: (on) => { opts.case = on; after(); } }, ["Aa"]),
    h("toggle", { key: "word", on: opts.word, title: "Whole word", onChange: (on) => { opts.word = on; after(); } }, ["ab"]),
    h("toggle", { key: "regex", on: opts.regex, title: "Regular expression", onChange: (on) => { opts.regex = on; after(); } }, [".*"]),
  ];
  // Search the selection only (on: the selection now becomes the range).
  const scopeToggle = () => {
    const b = buffer();
    return h("toggle", {
      key: "scope", on: !!scopeOf(b), title: "Find in selection",
      onChange: (on) => {
        if (!b) return;
        const sel = fresh.views.active()?.selection;
        if (on && !sel) { fresh.ui.message("Select the text to search in first"); return; }
        setScope(b, on ? sel : null);
        update();
      },
    }, ["⊂"]);
  };
  // While asking: the replace field takes y, n, a, !, q, c and Escape as
  // answers (and types nothing).
  const askKey = (e) => {
    if (!asking) return false;
    const k = e.key === "Escape" ? "q" : e.key.toLowerCase();
    if (["y", "n", "a", "!", "q", "c"].includes(k)) answer(k);
    return e.key.length === 1 || e.key === "Escape" || e.key === "Enter";
  };

  const renderBar = () => {
    if (!bar || !bar.open) return;
    const b = buffer();
    const count = !query ? "" : error || (b && !b.whole ? "huge file: Enter searches" : matches.length ? `${current + 1} of ${matches.length}` : "no results");
    const shown = asking ? replacementFor(matches[current] || { text: "" }) : "";
    bar.render(h("col", {}, [
      h("row", {}, [
        h("input", {
          key: "find", value: query, placeholder: "Find", grow: true,
          onInput: (q) => { query = q; update(); },
          onEnter: (_q, e) => go(e.shiftKey ? -1 : 1),
          onKey: (e) => { if (e.key === "Escape") { close(); return true; } },
        }),
        ...toggles(update),
        scopeToggle(),
        h("text", { dim: true }, [count]),
        h("button", { title: "Previous (Shift+Enter)", onClick: () => go(-1) }, ["↑"]),
        h("button", { title: "Next (Enter)", onClick: () => go(1) }, ["↓"]),
        h("button", { title: "Toggle replace", onClick: () => { showReplace = !showReplace; renderBar(); } }, ["⇄"]),
        h("button", { title: "Close (Esc)", onClick: close }, ["×"]),
      ]),
      showReplace ? h("row", {}, [
        h("input", {
          key: "replace", value: replacement, placeholder: asking ? "y n a q" : "Replace", grow: true,
          onInput: (r) => { if (!asking) replacement = r; },
          onEnter: () => { if (asking) return; if (confirmEach) startAsking(); else replaceOne(); },
          onKey: (e) => { if (askKey(e)) return true; if (e.key === "Escape") { close(); return true; } },
        }),
        h("toggle", { key: "ask", on: confirmEach, title: "Ask at each match (Query Replace): y, n, a, q", onChange: (on) => { confirmEach = on; renderBar(); } }, ["?"]),
        h("button", { onClick: () => (confirmEach ? startAsking() : replaceOne()), disabled: !!asking }, [confirmEach ? "Replace…" : "Replace"]),
        h("button", { onClick: replaceAll, disabled: !matches.length || !!asking }, ["Replace All"]),
      ]) : null,
      asking ? h("row", { class: "search-ask" }, [
        h("text", {}, [`Replace with “${shown}”?`]),
        h("button", { title: "Replace this one (y)", onClick: () => answer("y") }, ["Yes  y"]),
        h("button", { title: "Skip this one (n)", onClick: () => answer("n") }, ["No  n"]),
        h("button", { title: "Replace this and the rest (a, !)", onClick: () => answer("a") }, ["All  a"]),
        h("button", { title: "Stop (q, c, Esc)", onClick: () => answer("q") }, ["Quit  q"]),
        h("text", { dim: true }, [`${asking.done} replaced`]),
      ]) : null,
    ]));
  };

  // The bar: a selection of one line is the query; one of several lines
  // (or `inSelection`) is the range searched.
  const open = (replace, { inSelection = false, ask = false } = {}) => {
    const v = fresh.views.active();
    const b = buffer();
    const sel = v?.selection;
    const t = sel && v.buffer.whole ? v.buffer.text(sel.start, sel.end) : null;
    if (b) setScope(b, sel && (inSelection || (t && t.includes("\n"))) ? sel : null);
    if (t && !t.includes("\n") && !scopeOf(b)) query = t;
    showReplace = replace;
    confirmEach = ask;
    asking = null;
    bar = fresh.ui.panel({ id: "find", title: "Find", place: "bar", onClose: () => { asking = null; const ab = buffer(); if (ab) { deco(ab).clear(); setScope(ab, null); } } });
    update();
    bar.focus(replace && query ? "replace" : "find");
  };
  const close = () => { bar?.close(); bar = null; };

  // Find Selection Next / Previous: the selection (or the word at the
  // cursor) is the query, searched as it is written; its matches are
  // marked until Escape.
  const findSelection = async (dir) => {
    const v = fresh.views.active();
    const b = buffer();
    if (!v || !b) return;
    const sel = v.selection;
    const at = matches[current];
    const onMatch = sel && at && sel.start === at.pos && sel.end === at.pos + at.len && b.text(sel.start, sel.end) === at.text;
    if (!onMatch) {
      const word = sel || wordAt(b, v.cursor);
      const t = word ? b.text(word.start, word.end) : null;
      if (!t || t.includes("\n")) { fresh.ui.message("Select the text to find first"); return; }
      query = opts.regex ? t.replace(/[.*+?^${}()|[\]\\]/g, "\\$&") : t;
      if (bar && bar.open) setScope(b, null);
      if (!sel) await v.select(word.start, word.end);
      update();
    }
    await go(dir);
  };
  const wordAt = (b, pos) => {
    const L = b.lines();
    if (!L) return null;
    const i = L.starts.findIndex((s, k) => pos >= s && (k === L.starts.length - 1 || pos < L.starts[k + 1]));
    if (i < 0) return null;
    const line = L.lines[i];
    let col = 0, bytes = 0;
    while (col < line.length && bytes < pos - L.starts[i]) { bytes += fresh.text.byteLen(line[col]); col++; }
    let s = col, e = col;
    while (s > 0 && /[\p{L}\p{N}_]/u.test(line[s - 1])) s--;
    while (e < line.length && /[\p{L}\p{N}_]/u.test(line[e])) e++;
    if (s === e) return null;
    return { start: L.starts[i] + fresh.text.byteLen(line.slice(0, s)), end: L.starts[i] + fresh.text.byteLen(line.slice(0, e)) };
  };

  fresh.commands.register("search.find", { title: "Find…", run: () => open(false) });
  fresh.commands.register("search.replace", { title: "Replace…", run: () => open(true) });
  fresh.commands.register("search.findInSelection", { title: "Find in Selection…", run: () => open(false, { inSelection: true }) });
  fresh.commands.register("search.queryReplace", { title: "Query Replace…", run: () => open(true, { ask: true }) });
  fresh.commands.register("search.findNext", { title: "Find Next", run: () => go(1) });
  fresh.commands.register("search.findPrevious", { title: "Find Previous", run: () => go(-1) });
  fresh.commands.register("search.findSelectionNext", { title: "Find Selection Next", run: () => findSelection(1) });
  fresh.commands.register("search.findSelectionPrevious", { title: "Find Selection Previous", run: () => findSelection(-1) });
  fresh.commands.register("search.clear", { title: "Clear Search Highlights", run: () => clearMarks() });
  fresh.keys.bind("C-M-r", "search.queryReplace");
  fresh.keys.bind("C-F3", "search.findSelectionNext");
  fresh.keys.bind("M-n", "search.findSelectionNext");
  fresh.keys.bind("C-S-F3", "search.findSelectionPrevious");
  fresh.keys.bind("M-p", "search.findSelectionPrevious");
  // The marks of a search made without the bar go with Escape.
  const clearMarks = () => { const b = buffer(); matches = []; current = -1; if (b) { deco(b).clear(); b.render(); } };
  fresh.keys.bind("C-f", "search.find");
  fresh.keys.bind("C-h", "search.replace");
  fresh.keys.bind("C-r", "search.replace");
  fresh.keys.bind("F3", "search.findNext");
  fresh.keys.bind("S-F3", "search.findPrevious");
  fresh.events.on("buffer.changed", ({ buffer: b }) => { if (bar && bar.open && b.id === buffer()?.id) update(); });
  fresh.events.on("buffer.activated", () => { if (bar && bar.open) update(); });
  fresh.events.on("escape", () => { if (bar && bar.open) close(); else clearMarks(); });

  // ---------------------------------------------------------------- the workspace

  const pq = { query: "", replace: "", results: [], files: 0, truncated: false, busy: false, error: "" };
  let panel = null;
  const runSearch = async () => {
    if (!pq.query) { pq.results = []; renderPanel(); return; }
    pq.busy = true;
    renderPanel();
    try {
      const r = await fresh.host.search(pq.query, { regex: opts.regex, case: opts.case, whole: opts.word, max: 5000 });
      pq.results = r.matches;
      pq.files = r.files;
      pq.truncated = r.truncated;
      pq.error = "";
    } catch (e) {
      pq.results = [];
      pq.error = String(e.message || e).replace(/^search.project: /, "");
    }
    pq.busy = false;
    renderPanel();
  };
  let timer = null;
  const rows = () => {
    const out = [];
    let last = null;
    for (const m of pq.results) {
      if (m.path !== last) { out.push({ header: true, path: m.path }); last = m.path; }
      out.push(m);
    }
    return out;
  };
  // A match selected (a click, the arrows) shows in the preview tab, the
  // keyboard staying in the list; Enter or a double-click opens it in a
  // tab of its own.
  const openMatch = async (m, preview) => {
    if (!m || m.header) return;
    const b = await fresh.buffers.open(m.path, preview ? { preview: true, focus: false } : { preview: false });
    const v = fresh.views.active();
    const L = b?.lines();
    if (!L) return;
    const i = m.line - (L.firstLine ?? 0);
    const start = L.starts[i] + m.col;
    await v.select(start, start + m.len, { center: true });
  };
  const replaceEverywhere = async () => {
    const byFile = new Map();
    for (const m of pq.results) { if (!byFile.has(m.path)) byFile.set(m.path, []); byFile.get(m.path).push(m); }
    let n = 0;
    const re = toRegex(pq.query, false);
    for (const [path, list] of byFile) {
      const b = await fresh.buffers.load(path);
      const L = b.lines();
      const ops = [];
      for (const m of [...list].reverse()) {
        const start = L.starts[m.line - (L.firstLine ?? 0)] + m.col;
        const old = b.text(start, start + m.len);
        if (old == null) continue;
        ops.push({ t: "del", pos: start, len: m.len });
        const text = opts.regex ? old.replace(re, pq.replace) : pq.replace;
        if (text) ops.push({ t: "ins", pos: start, text });
        n++;
      }
      b.edit(ops);
    }
    fresh.ui.message(`Replaced ${n} match${n === 1 ? "" : "es"} in ${byFile.size} file(s); not saved yet (Save All)`);
    runSearch();
  };
  const renderPanel = () => {
    if (!panel || !panel.open) return;
    const status = pq.busy ? "searching…" : pq.error ? pq.error : pq.query ? `${pq.results.length}${pq.truncated ? "+" : ""} matches in ${pq.files} files` : "";
    panel.render(h("col", {}, [
      h("row", {}, [
        h("input", {
          key: "q", value: pq.query, placeholder: "Search the workspace", grow: true, list: "results",
          onInput: (q) => { pq.query = q; clearTimeout(timer); timer = setTimeout(runSearch, 250); },
          onEnter: runSearch,
        }),
        ...toggles(() => { update(); runSearch(); }),
        h("input", { key: "r", value: pq.replace, placeholder: "Replace with", grow: true, onInput: (r) => { pq.replace = r; } }),
        h("button", { onClick: replaceEverywhere, disabled: !pq.results.length }, ["Replace All"]),
        h("text", { dim: true }, [status]),
      ]),
      h("list", {
        key: "results",
        items: rows(),
        render: (m) => m.header
          ? h("text", { bold: true }, [m.path])
          : h("row", {}, [h("text", { dim: true }, [`  ${m.line + 1}:${m.col + 1}`]), h("text", { mono: true }, [m.text.trim()])]),
        onSelect: (m) => openMatch(m, true),
        onActivate: (m) => openMatch(m, false),
      }),
    ]));
  };
  const openPanel = () => {
    const v = fresh.views.active();
    const sel = v?.selection;
    if (sel && v.buffer.whole) {
      const t = v.buffer.text(sel.start, sel.end);
      if (t && !t.includes("\n")) pq.query = t;
    }
    panel = fresh.ui.panel({ id: "project-search", title: "Search", place: "bottom" });
    renderPanel();
    panel.focus("q");
    if (pq.query) runSearch();
  };
  fresh.commands.register("search.project", { title: "Search in Workspace…", run: openPanel });
  fresh.keys.bind("C-S-f", "search.project");
}
