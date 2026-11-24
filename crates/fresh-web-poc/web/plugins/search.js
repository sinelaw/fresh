// Search and replace.
//
// In the buffer: a bar above the editor with match case, whole word and
// regex toggles, a match count, previous/next, replace and replace all.
// Every match on screen is marked; the current one more strongly. A buffer
// held whole is searched here; a huge one is searched by the server.
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

  const toRegex = (q, global = true) => {
    let src = opts.regex ? q : q.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
    if (opts.word) src = `\\b(?:${src})\\b`;
    return new RegExp(src, (global ? "g" : "") + (opts.case ? "" : "i") + "u");
  };

  const buffer = () => fresh.buffers.active();
  const deco = (b) => b.decorations("search");

  // All matches in a buffer held whole, as byte ranges.
  const findAll = (b) => {
    const L = b.lines();
    if (!L || !b.whole || !query) return [];
    let re;
    try { re = toRegex(query); error = ""; } catch (e) { error = "invalid regex"; return []; }
    const out = [];
    L.lines.forEach((line, i) => {
      re.lastIndex = 0;
      for (let m = re.exec(line); m; m = re.exec(line)) {
        if (m[0] === "") { re.lastIndex++; continue; }
        const pos = L.starts[i] + fresh.text.byteLen(line.slice(0, m.index));
        out.push({ pos, len: fresh.text.byteLen(m[0]), text: m[0] });
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
    matches.forEach((m, i) => d.overlay(m.pos, m.pos + m.len, i === current ? { bg: "search.current_bg", fg: "search.current_fg" } : { bg: "search.match_bg", fg: "search.match_fg" }));
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
      const from = dir > 0 ? (sel ? sel.end : v.cursor) : (sel ? sel.start : v.cursor);
      const res = await fresh.host.searchDocument(b, query, from, dir < 0);
      if (res == null) { fresh.ui.message(`Not found: ${query}`); return; }
      matches = [{ pos: res, len: fresh.text.byteLen(query) }];
      current = 0;
    } else {
      if (!matches.length) { fresh.ui.message(`Not found: ${query}`); renderBar(); return; }
      const at = v.cursor;
      if (dir > 0) {
        const sel = v.selection;
        const from = sel && matches[current] && sel.start === matches[current].pos ? sel.end : at;
        current = matches.findIndex((m) => m.pos >= from && !(sel && m.pos === sel.start && sel.end === m.pos + m.len));
        if (current < 0) current = 0;
      } else {
        const sel = v.selection;
        const from = sel ? sel.start : at;
        let k = -1;
        for (let i = 0; i < matches.length; i++) if (matches[i].pos < from) k = i;
        current = k < 0 ? matches.length - 1 : k;
      }
    }
    const m = matches[current];
    await v.select(m.pos, m.pos + m.len, { center: true });
    if (m.text == null) m.text = b.text(m.pos, m.pos + m.len); // found by the server
    mark();
    renderBar();
  };

  // Replace the current match (marked in the buffer) and go to the next.
  const replaceOne = async () => {
    const b = buffer();
    const v = fresh.views.active();
    const m = matches[current];
    if (!b || !v || !m) return go(1);
    const text = opts.regex ? m.text.replace(toRegex(query, false), replacement) : replacement;
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
    h("toggle", { key: "case", on: opts.case, title: "Match case", onChange: (on) => { opts.case = on; after(); } }, "Aa"),
    h("toggle", { key: "word", on: opts.word, title: "Whole word", onChange: (on) => { opts.word = on; after(); } }, "ab"),
    h("toggle", { key: "regex", on: opts.regex, title: "Regular expression", onChange: (on) => { opts.regex = on; after(); } }, ".*"),
  ];

  const renderBar = () => {
    if (!bar || !bar.open) return;
    const b = buffer();
    const count = !query ? "" : error || (b && !b.whole ? "huge file: Enter searches" : matches.length ? `${current + 1} of ${matches.length}` : "no results");
    bar.render(h("col", {},
      h("row", {},
        h("input", {
          key: "find", value: query, placeholder: "Find", grow: true,
          onInput: (q) => { query = q; update(); },
          onEnter: (_q, e) => go(e.shiftKey ? -1 : 1),
          onKey: (e) => { if (e.key === "Escape") { close(); return true; } },
        }),
        ...toggles(update),
        h("text", { dim: true }, count),
        h("button", { title: "Previous (Shift+Enter)", onClick: () => go(-1) }, "↑"),
        h("button", { title: "Next (Enter)", onClick: () => go(1) }, "↓"),
        h("button", { title: "Toggle replace", onClick: () => { showReplace = !showReplace; renderBar(); } }, "⇄"),
        h("button", { title: "Close (Esc)", onClick: close }, "×")),
      showReplace ? h("row", {},
        h("input", {
          key: "replace", value: replacement, placeholder: "Replace", grow: true,
          onInput: (r) => { replacement = r; },
          onEnter: replaceOne,
          onKey: (e) => { if (e.key === "Escape") { close(); return true; } },
        }),
        h("button", { onClick: replaceOne }, "Replace"),
        h("button", { onClick: replaceAll, disabled: !matches.length }, "Replace All")) : null));
  };

  const open = (replace) => {
    const v = fresh.views.active();
    const sel = v?.selection;
    if (sel && v.buffer.whole) {
      const t = v.buffer.text(sel.start, sel.end);
      if (t && !t.includes("\n")) query = t;
    }
    showReplace = replace;
    bar = fresh.ui.panel({ id: "find", title: "Find", place: "bar", onClose: () => { const b = buffer(); if (b) { deco(b).clear(); b.render(); } } });
    update();
    bar.focus(replace && query ? "replace" : "find");
  };
  const close = () => { bar?.close(); bar = null; };

  fresh.commands.register("search.find", { title: "Find…", run: () => open(false) });
  fresh.commands.register("search.replace", { title: "Replace…", run: () => open(true) });
  fresh.commands.register("search.findNext", { title: "Find Next", run: () => go(1) });
  fresh.commands.register("search.findPrevious", { title: "Find Previous", run: () => go(-1) });
  fresh.keys.bind("C-f", "search.find");
  fresh.keys.bind("C-h", "search.replace");
  fresh.keys.bind("C-r", "search.replace");
  fresh.keys.bind("F3", "search.findNext");
  fresh.keys.bind("S-F3", "search.findPrevious");
  fresh.events.on("buffer.changed", ({ buffer: b }) => { if (bar && bar.open && b.id === buffer()?.id) update(); });
  fresh.events.on("buffer.activated", () => { if (bar && bar.open) update(); });
  fresh.events.on("escape", close);

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
  const openMatch = async (m) => {
    if (!m || m.header) return;
    const b = await fresh.buffers.open(m.path);
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
    panel.render(h("col", {},
      h("row", {},
        h("input", {
          key: "q", value: pq.query, placeholder: "Search the workspace", grow: true, list: "results",
          onInput: (q) => { pq.query = q; clearTimeout(timer); timer = setTimeout(runSearch, 250); },
          onEnter: runSearch,
        }),
        ...toggles(() => { update(); runSearch(); }),
        h("input", { key: "r", value: pq.replace, placeholder: "Replace with", grow: true, onInput: (r) => { pq.replace = r; } }),
        h("button", { onClick: replaceEverywhere, disabled: !pq.results.length }, "Replace All"),
        h("text", { dim: true }, status)),
      h("list", {
        key: "results",
        items: rows(),
        render: (m) => m.header
          ? h("text", { bold: true }, m.path)
          : h("row", {}, h("text", { dim: true }, `  ${m.line + 1}:${m.col + 1}`), h("text", { mono: true }, m.text.trim())),
        onActivate: openMatch,
      })));
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
