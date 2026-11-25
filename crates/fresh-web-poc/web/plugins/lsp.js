// Language-server features. The server holds one connection per language
// and keeps each document in sync from its edit log; this plugin asks it
// questions about the active document and shows the answers.
//
//   F12 / Ctrl+click   go to definition        Shift+F12  find references
//   Ctrl+Space         completion (also after ".")
//   Ctrl+K / hover     hover information       F2         rename symbol
//   Ctrl+.             code actions            Shift+Alt+F format document
//   Ctrl+Shift+O       document symbols        Ctrl+Shift+M diagnostics panel
//   ( and ,            signature help
//
// The palette's `@` lists the file's symbols, its `%` the workspace's
// (`workspace/symbol`, asked of every running server). Inlay hints
// (`textDocument/inlayHint`) are drawn as inline decorations, asked again
// after edits; LSP: Toggle Inlay Hints. LSP: Restart Server and Stop
// Server act on the active file's language (Show Status offers them for
// each).

import * as fresh from "../js/api.js";
import { h } from "../js/ui.js";

// An inlay hint.
/** const HINT: Style */
const HINT = { fg: "editor.line_number_fg", bg: null, bold: false, italic: true, underline: false, strike: false, mono: false, extendToLineEnd: false };

// A palette row that only says something.
/** function note(label: String) => PaletteItem */
function note(label) { return { label, detail: null, run: null, preview: null }; }

export function activate(app) {
  const SEV = ["", "error", "warning", "info", "hint"];
  const status = fresh.statusItem(app, "lsp");

  const active = () => {
    const v = fresh.activeView(app);
    return v && fresh.lspAvailable(app, v.buffer) ? v : null;
  };
  // A request about the cursor's position. A refused one throws; each
  // command shows the server's reason (see `reg`).
  const ask = (v, method, params) => fresh.lspRequest(app, v.buffer, method, params, v.cursor);
  const rel = (p) => (p.startsWith(fresh.root(app) + "/") ? p.slice(fresh.root(app).length + 1) : p);

  // Open a location: `preview` as for OpenOptions (a list's selection
  // previews it, the keyboard staying in the list).
  /** const goto: (String, LspRange, Boolean | Null) => Promise<Undefined> */
  const goto = async (path, range, preview) => {
    await fresh.open(app, path, { line: range.start.line + 1, column: range.start.character + 1, place: "tab", preview, focus: preview !== true });
  };

  // ---------------------------------------------------------------- navigation

  const definition = async () => {
    const v = active();
    if (!v) return;
    /** const r: LspLocation[] */
    const r = await ask(v, "textDocument/definition", {});
    if (!r.length) return fresh.message(app, "No definition found");
    if (r.length === 1) { await goto(r[0].path, r[0].range, null); return; }
    fresh.pick(app, { title: "Definitions", items: r.map((l) => ({ label: `${rel(l.path)}:${l.range.start.line + 1}`, value: l })), onAccept: (it) => { goto(it.value.path, it.value.range, null); } });
  };

  const references = async () => {
    const v = active();
    if (!v) return;
    /** const r: LspLocation[] */
    const r = await ask(v, "textDocument/references", { context: { includeDeclaration: true } });
    if (!r.length) return fresh.message(app, "No references found");
    // Show each reference's line: read it from the file through the server.
    /** const texts: Map<String, String[]> */
    const texts = new Map();
    for (const l of r) {
      if (!texts.has(l.path)) {
        try { texts.set(l.path, (await fresh.read(app, l.path)).split("\n")); } catch { texts.set(l.path, []); }
      }
    }
    const lineOf = (l) => {
      const lines = texts.get(l.path) ?? [];
      const i = l.range.start.line;
      return i < lines.length ? lines[i].trim() : "";
    };
    // A file's header, then its references: one row shape for both.
    const items = r.flatMap((l, i) => {
      const row = { header: false, path: l.path, range: l.range, text: lineOf(l) };
      return i === 0 || r[i - 1].path !== l.path ? [{ header: true, path: l.path, range: l.range, text: "" }, row] : [row];
    });
    const refsPanel = fresh.panel(app, { id: "references", title: `References (${r.length})`, place: "bottom", onClose: null });
    refsPanel.render(h("list", {
      key: "refs",
      items,
      render: (it) => it.header ? h("text", { bold: true }, [rel(it.path)]) : h("row", {}, [h("text", { dim: true }, [`  ${it.range.start.line + 1}:${it.range.start.character + 1}`]), h("text", { mono: true }, [it.text])]),
      // Selected, a reference shows in the preview tab (the keyboard stays
      // in the list); Enter or a double-click opens it for good.
      onSelect: (it) => { if (!it.header) goto(it.path, it.range, true); },
      onActivate: (it) => { if (!it.header) goto(it.path, it.range, false); },
    }));
    refsPanel.focus("refs");
  };

  // The palette's `@`: the active file's symbols, filtered by what is
  // typed, each previewed as it is selected. Asked once per opening.
  // The last file's symbols, by `id:length`.
  /** const symCache: Map<String, LspSymbol[]> */
  const symCache = new Map();
  const KINDS = new Map([[5, "class"], [6, "method"], [12, "function"], [13, "variable"], [14, "constant"], [23, "struct"], [10, "enum"], [11, "interface"], [8, "field"], [2, "module"]]);
  const kindName = (k) => KINDS.get(k) || "";
  fresh.paletteMode(app, "@", {
    hint: "symbols",
    items: async (q) => {
      const v = active();
      if (!v) return [note("No language server for this file")];
      const key = `${v.buffer.id}:${v.buffer.length}`;
      const cached = symCache.get(key);
      /** let list: LspSymbol[] */
      let list = [];
      if (cached) list = cached;
      else {
        try { list = await ask(v, "textDocument/documentSymbol", {}); } catch (e) { return [note(fresh.errorText(e))]; }
        symCache.clear();
        symCache.set(key, list);
      }
      /** const out: PaletteItem[] */
      const out = [];
      // `path`: the symbols it is inside, dotted (null at the top).
      /** const walk: (LspSymbol[], String | Null) => Undefined */
      const walk = (symbols, path) => {
        for (const s of symbols) {
          const range = s.range;
          const go = () => { const p = fresh.lspOffset(app, v.buffer, range.start.line, range.start.character); if (p != null) v.setCursor(p, { center: true }); };
          const inside = path ?? s.container;
          if (!q || s.name.toLowerCase().includes(q.toLowerCase())) out.push({ label: s.name, detail: [kindName(s.kind), inside ?? ""].filter(Boolean).join(" · "), run: go, preview: go });
          walk(s.children, path === null ? s.name : `${path}.${s.name}`);
        }
      };
      walk(list, null);
      return out.length ? out : [note(q ? "No symbol matches" : "No symbols")];
    },
  });
  const symbols = () => fresh.run(app, "palette.open", "@");

  // The palette's `%`: the workspace's symbols matching what is typed,
  // from every running server.
  // Each language's server: running (true) or stopped (false), from the
  // server; a language not in it has none started.
  /** let running: Map<String, Boolean> */
  let running = new Map();
  /** const statusOf: (Dict<Boolean>) => Map<String, Boolean> */
  const statusOf = (d) => new Map(Object.entries(d));
  const runningLanguages = async () => {
    try { running = statusOf(await fresh.lspStatus(app)); } catch { running = new Map(); }
    return [...running].filter(([, on]) => on).map(([l]) => l).sort();
  };
  fresh.paletteMode(app, "%", {
    hint: "workspace symbols",
    items: async (q) => {
      if (!q) return [note("Type a symbol's name")];
      const langs = await runningLanguages();
      if (!langs.length) return [note("No language server is running (open a file of its language)")];
      /** const out: PaletteItem[] */
      const out = [];
      for (const l of langs) {
        /** let found: LspWorkspaceSymbol[] */
        let found = [];
        try { found = await fresh.lspWorkspaceRequest(app, l, "workspace/symbol", { query: q }); } catch { continue; }
        for (const s of found) {
          const go = () => { goto(s.path, s.range, false); };
          out.push({
            label: s.name,
            detail: [kindName(s.kind), s.container ?? "", `${rel(s.path)}:${s.range.start.line + 1}`].filter(Boolean).join(" · "),
            run: go,
            preview: null,
          });
        }
      }
      return out.length ? out : [note("No symbol matches")];
    },
  });
  const workspaceSymbols = () => fresh.run(app, "palette.open", "%");

  // ---------------------------------------------------------------- edits

  // Apply a WorkspaceEdit through the documents (open or not), so every
  // client and every undo sees it.
  /** const applyWorkspaceEdit: (LspFileEdit[]) => Promise<Number> */
  const applyWorkspaceEdit = async (files) => {
    let n = 0;
    for (const f of files) {
      const b = await fresh.load(app, f.path);
      if (b) n += applyTextEdits(b, f.edits);
    }
    return n;
  };
  const applyTextEdits = (b, edits) => {
    /** const ops: Op[] */
    const ops = [];
    const resolved = edits
      .map((e) => ({ s: fresh.lspOffset(app, b, e.range.start.line, e.range.start.character), e: fresh.lspOffset(app, b, e.range.end.line, e.range.end.character), text: e.newText }))
      .filter((e) => e.s != null && e.e != null)
      .sort((x, y) => y.s - x.s);
    for (const e of resolved) {
      if (e.e > e.s) ops.push({ t: "del", pos: e.s, len: e.e - e.s });
      if (e.text) ops.push({ t: "ins", pos: e.s, text: e.text });
    }
    if (ops.length) b.edit(ops);
    return resolved.length;
  };

  // The identifier characters that end `s`, and that start it.
  const wordEnd = (s) => s.slice(s.search(/[\w$]*$/));
  const wordStart = (s) => s.slice(0, s.search(/[^\w$]|$/));
  // The identifier around the cursor.
  const currentWord = (v) => {
    const b = v.buffer;
    const left = b.text(Math.max(0, v.cursor - 100), v.cursor) || "";
    const right = b.text(v.cursor, Math.min(b.length, v.cursor + 100)) || "";
    return wordEnd(left) + wordStart(right);
  };

  const rename = async () => {
    const v = active();
    if (!v) return;
    const name = await fresh.input(app, { title: "Rename to", value: currentWord(v) });
    if (!name) return;
    /** const r: LspFileEdit[] */
    const r = await ask(v, "textDocument/rename", { newName: name });
    if (!r.length) return fresh.message(app, "Nothing to rename here");
    fresh.message(app, `Renamed: ${await applyWorkspaceEdit(r)} edit(s)`);
  };

  const codeActions = async () => {
    const v = active();
    if (!v) return;
    const sel = v.selection || { start: v.cursor, end: v.cursor };
    const toPos = (p) => fresh.lspPosition(app, v.buffer, p);
    const diags = fresh.diagnostics(app, v.buffer).filter((d) => d.start <= sel.end && d.end >= sel.start);
    /** const r: LspAction[] */
    const r = await ask(v, "textDocument/codeAction", {
      range: { start: toPos(sel.start), end: toPos(sel.end) },
      context: { diagnostics: diags.map((d) => ({ message: d.message, severity: d.severity, range: { start: toPos(d.start), end: toPos(d.end) } })) },
    });
    if (!r.length) return fresh.message(app, "No code actions here");
    fresh.pick(app, {
      title: "Code actions",
      items: r.map((a) => ({ label: a.title, detail: a.kind ?? "", value: a })),
      onAccept: async ({ value: a }) => {
        const edit = a.edit, cmd = a.command;
        if (edit) await applyWorkspaceEdit(edit);
        if (cmd) await ask(v, "workspace/executeCommand", { command: cmd.command, arguments: JSON.parse(cmd.arguments) });
      },
    });
  };

  const format = async () => {
    const v = active();
    if (!v) return;
    /** const r: LspTextEdit[] */
    const r = await ask(v, "textDocument/formatting", { options: { tabSize: 4, insertSpaces: true } });
    if (!r.length) return fresh.message(app, "Already formatted (or the server does not format)");
    fresh.message(app, `Formatted: ${applyTextEdits(v.buffer, r)} edit(s)`);
  };

  // ---------------------------------------------------------------- completion, hover, signatures

  const completion = async () => {
    const v = active();
    if (!v) return;
    /** const items: LspCompletion[] */
    const items = await ask(v, "textDocument/completion", {});
    if (!items.length) return fresh.message(app, "No completions");
    const word = wordEnd(v.buffer.text(Math.max(0, v.cursor - 100), v.cursor) || "");
    fresh.pick(app, {
      title: "Complete",
      value: word,
      items: items.slice(0, 500).map((i) => ({ label: i.label, detail: i.detail ?? "", value: i })),
      onAccept: ({ value: i }) => {
        const vv = fresh.activeView(app);
        if (!vv) return;
        const b = vv.buffer;
        const left = b.text(Math.max(0, vv.cursor - 100), vv.cursor) || "";
        const w = wordEnd(left);
        const text = i.insert;
        const start = vv.cursor - fresh.byteLen(w);
        b.replace(start, vv.cursor, text);
        vv.setCursor(start + fresh.byteLen(text), { center: false });
      },
    });
  };

  // Hover contents as text: markdown's code fences dropped (the popup is
  // monospace already) and its rules shortened.
  /** const hoverText: (String | Null) => String | Null */
  const hoverText = (md) => {
    if (md === null) return null;
    const text = md.split("\n").filter((l) => !/^\s*```/.test(l)).map((l) => (/^\s*(-{3,}|\*{3,}|_{3,})\s*$/.test(l) ? "───" : l)).join("\n").trim();
    return text === "" ? null : text;
  };
  fresh.hoverProvider(app, async (b, pos) => {
    const diags = fresh.diagnostics(app, b).filter((d) => pos >= d.start && pos <= Math.max(d.end, d.start + 1));
    const problems = diags.map((d) => `${SEV[d.severity] || "error"}: ${d.message}`);
    /** let about: String | Null */
    let about = null;
    if (fresh.lspAvailable(app, b)) {
      try { about = hoverText(await fresh.lspRequest(app, b, "textDocument/hover", {}, pos)); } catch {}
    }
    const parts = about === null ? problems : [...problems, about];
    return parts.length ? parts.join("\n\n") : null;
  });
  const hover = async () => {
    const v = active();
    if (!v) return;
    const t = hoverText(await ask(v, "textDocument/hover", {}));
    if (t) fresh.popup(app, t);
  };

  const signatureHelp = async () => {
    const v = active();
    if (!v) return;
    /** const sig: LspSignature | Null */
    const sig = await ask(v, "textDocument/signatureHelp", {});
    if (!sig) return;
    const doc = sig.documentation;
    fresh.popup(app, doc ? `${sig.label}\n\n${doc}` : sig.label);
  };

  fresh.onBuffer(app, "buffer.changed", (buffer, { ops, local }) => {
    if (!local || !fresh.lspAvailable(app, buffer)) return;
    const op = ops[0];
    if (op?.t !== "ins") return;
    // The inserted text's first character: typing "(" inserts "()".
    const ch = op.text[0];
    if (ch === "(" || ch === ",") setTimeout(() => { signatureHelp(); }, 50);
    else if (ch === "." && op.text.length === 1) setTimeout(() => { completion(); }, 50);
  });

  // ---------------------------------------------------------------- diagnostics

  // The line number (1-based, as text) of byte `pos` in `b`, or "?".
  const lineLabel = (b, pos) => {
    const L = b.lines();
    if (!L) return "?";
    const i = L.starts.findIndex((s, k) => pos >= s && (k + 1 >= L.starts.length || pos < L.starts[k + 1]));
    return i >= 0 ? String((L.firstLine ?? 0) + i + 1) : "?";
  };
  const renderDiagnostics = () => {
    const diagPanel = fresh.shown(app, "diagnostics");
    if (!diagPanel) return;
    // Each file with problems: a header row, then a row per problem.
    const items = fresh.buffers(app).flatMap((b) => {
      const ds = fresh.diagnostics(app, b).sort((x, y) => x.start - y.start);
      const rows = ds.map((d) => ({ header: false, b, d, line: lineLabel(b, d.start) }));
      return ds.length ? [{ header: true, b, d: ds[0], line: "" }, ...rows] : [];
    });
    diagPanel.render(h("list", {
      key: "diags",
      items,
      render: (it) => it.header
        ? h("text", { bold: true }, [rel(it.b.path)])
        : h("row", {}, [h("text", { color: it.d.severity === 1 ? "diagnostic.error" : "diagnostic.warning" }, [it.d.severity === 1 ? "  ●" : "  ▲"]), h("text", { dim: true }, [it.line]), h("text", { mono: true }, [it.d.message.split("\n")[0]])]),
      onActivate: async (it) => { if (it.header) return; await fresh.open(app, it.b.path, { line: null, column: 1, place: "tab", preview: null, focus: true }); fresh.activeView(app).setCursor(it.d.start, { center: true }); },
    }));
  };
  const diagnosticsPanel = () => {
    const diagPanel = fresh.panel(app, { id: "diagnostics", title: "Diagnostics", place: "bottom", onClose: null });
    renderDiagnostics();
    diagPanel.focus("diags");
  };
  fresh.onBuffer(app, "diagnostics.changed", renderDiagnostics);

  // ---------------------------------------------------------------- folding and status

  const refreshFolds = async (b) => {
    if (!fresh.lspAvailable(app, b)) return;
    try {
      /** const folds: LspFold[] */
      const folds = await fresh.lspRequest(app, b, "textDocument/foldingRange", {});
      b.setFoldRanges(folds);
    } catch {}
  };
  // The status bar: the active file's server, or `LSP (off)` when it was
  // stopped.
  const showStatus = (b) => {
    const installed = b != null && b.language != null && fresh.lspLanguages(app).includes(b.language) && !b.isVirtual;
    const text = !installed ? "" : fresh.lspAvailable(app, b) ? `LSP ${b.language}` : running.get(b.language) === false ? "LSP (off)" : "";
    status.set(text, { title: "Language servers: Show Status", onClick: () => fresh.run(app, "lsp.status") });
  };
  fresh.onBuffer(app, "buffer.activated", (buffer) => { showStatus(buffer); refreshFolds(buffer); refreshHints(buffer); });
  // Its language changed: another server, or none.
  fresh.onBuffer(app, "buffer.info", (buffer) => { if (buffer.id === fresh.activeBuffer(app)?.id) showStatus(buffer); });
  // Each language's server: running, available (started with its first
  // file), or none found on the host.
  // Picking one offers to restart or stop it.
  const lspStatus = async () => {
    await runningLanguages();
    const langs = [...new Set([...fresh.lspLanguages(app), ...running.keys()])].sort();
    const state = (l) => { const on = running.get(l); return on === true ? "running" : on === false ? "stopped" : fresh.lspLanguages(app).includes(l) ? "available (starts with a file)" : "not started"; };
    fresh.pick(app, {
      title: fresh.trusted(app) ? "Language servers" : "Language servers (the workspace is restricted: trust it to start them)",
      items: langs.length ? langs.map((l) => ({ label: l, detail: state(l), value: l })) : [{ label: "No language server found on the host", detail: "", value: "" }],
      onAccept: async (it) => {
        if (!it.value || !fresh.trusted(app)) return;
        const what = await fresh.choose(app, `${it.value} language server (${state(it.value)})`, [
          { label: running.get(it.value) ? "Restart" : "Start", value: "restart" },
          ...(running.get(it.value) ? [{ label: "Stop", value: "stop" }] : []),
        ]);
        if (what) await serverOp(what, it.value);
      },
    });
  };
  // The active file's language, when a server is found for it.
  const activeLanguage = () => {
    const b = fresh.activeBuffer(app);
    const l = b === null ? null : b.language;
    return l !== null && fresh.lspLanguages(app).includes(l) ? l : null;
  };
  // Restart or stop the active file's server (the list of them when it has
  // none).
  const activeServerOp = (what) => {
    const l = activeLanguage();
    if (l === null) lspStatus();
    else serverOp(what, l);
  };
  // Restart or stop language `l`'s server.
  const serverOp = async (what, l) => {
    try {
      running = statusOf(await (what === "stop" ? fresh.lspStop(app, l) : fresh.lspRestart(app, l)));
      fresh.message(app, what === "stop" ? `Stopped the ${l} language server` : `Restarted the ${l} language server`);
    } catch (e) { fresh.showError(app, e); }
  };
  fresh.onLspStatus(app, (s) => {
    running = statusOf(s);
    const b = fresh.activeBuffer(app);
    showStatus(b);
    for (const x of fresh.buffers(app)) refreshHints(x);
  });

  // ---------------------------------------------------------------- inlay hints

  // A document's hints, drawn before the character they are at (in the
  // line numbers' colour), asked for the whole document once its text has
  // settled after an edit. An answer to an older text is dropped.
  const hintTimers = new Map();
  const refreshHints = async (b) => {
    const deco = b.decorations("inlay");
    if (!fresh.settings(app).inlay_hints || !fresh.lspAvailable(app, b) || !b.whole) { deco.clear(); b.render(); return; }
    const L = b.lines();
    if (!L) return;
    const len = b.length, text0 = b.text(0, Math.min(len, 64));
    /** let hints: LspHint[] */
    let hints = [];
    try {
      hints = await fresh.lspRequest(app, b, "textDocument/inlayHint", { range: { start: { line: 0, character: 0 }, end: { line: L.lines.length, character: 0 } } });
    } catch { return; }
    if (b.length !== len || b.text(0, Math.min(len, 64)) !== text0) return;
    deco.clear();
    for (const hnt of hints) {
      const pos = fresh.lspOffset(app, b, hnt.position.line, hnt.position.character);
      if (pos == null) continue;
      const text = (hnt.paddingLeft ? " " : "") + hnt.label + (hnt.paddingRight ? " " : "");
      deco.inline(pos, text, { style: HINT, side: "before", edge: false, when: "always" });
    }
    b.render();
  };
  const hintsSoon = (b) => {
    const pending = hintTimers.get(b.id);
    if (pending !== undefined) clearTimeout(pending);
    hintTimers.set(b.id, setTimeout(() => { hintTimers.delete(b.id); refreshHints(b); }, 500));
  };
  fresh.onBuffer(app, "buffer.changed", (buffer) => { if (fresh.settings(app).inlay_hints && fresh.lspAvailable(app, buffer)) hintsSoon(buffer); });
  const toggleHints = () => {
    fresh.settings(app).inlay_hints = !fresh.settings(app).inlay_hints;
    fresh.saveSettings(app);
    for (const b of fresh.buffers(app)) refreshHints(b);
    fresh.message(app, `Inlay hints: ${fresh.settings(app).inlay_hints ? "on" : "off"}`);
  };
  // Trusting the workspace starts its language servers.
  fresh.on(app, "workspace.trusted", () => { const b = fresh.activeBuffer(app); showStatus(b); if (b) { refreshFolds(b); refreshHints(b); } });
  fresh.onBuffer(app, "buffer.saved", (buffer) => refreshFolds(buffer));

  // A command, and its key ("" for none).
  // A failure (a refused request) shows the server's reason.
  const reg = (id, title, run, key) => {
    fresh.register(app, id, { title, hidden: false, run: () => { Promise.resolve(run()).catch((e) => fresh.showError(app, e)); } });
    if (key) fresh.bindKey(app, key, id);
  };
  reg("lsp.definition", "LSP: Go to Definition", definition, "F12");
  reg("lsp.references", "LSP: Find References", references, "S-F12");
  reg("lsp.completion", "LSP: Completion", completion, "C-Space");
  reg("lsp.hover", "LSP: Hover", hover, "C-k");
  reg("lsp.rename", "LSP: Rename Symbol", rename, "F2");
  reg("lsp.codeActions", "LSP: Code Actions", codeActions, "C-.");
  reg("lsp.format", "LSP: Format Document", format, "M-S-f");
  reg("lsp.symbols", "Go to Symbol in File…", symbols, "C-S-o");
  reg("lsp.signatureHelp", "LSP: Signature Help", signatureHelp, "C-S-Space");
  reg("lsp.diagnostics", "Diagnostics: Show Panel", diagnosticsPanel, "C-S-m");
  // The next (previous) diagnostic after (before) the cursor, round the
  // end of the file; its message in the status bar.
  const stepDiagnostic = async (dir) => {
    const v = fresh.activeView(app);
    if (!v) return;
    const ds = fresh.diagnostics(app, v.buffer).sort((a, b) => a.start - b.start || a.severity - b.severity);
    if (!ds.length) { fresh.message(app, "No problems in this file"); return; }
    const at = v.cursor;
    const next = dir > 0 ? ds.find((x) => x.start > at) : [...ds].reverse().find((x) => x.start < at);
    const d = next !== undefined ? next : dir > 0 ? ds[0] : ds[ds.length - 1];
    await v.setCursor(d.start, { center: true });
    fresh.message(app, `${SEV[d.severity] || "error"}: ${d.message.split("\n")[0]}`);
  };
  reg("lsp.nextDiagnostic", "Go to Next Problem", () => stepDiagnostic(1), "F8");
  reg("lsp.prevDiagnostic", "Go to Previous Problem", () => stepDiagnostic(-1), "S-F8");
  reg("lsp.status", "LSP: Show Status", lspStatus, "");
  reg("lsp.workspaceSymbols", "Go to Symbol in Workspace…", workspaceSymbols, "");
  reg("lsp.restart", "LSP: Start/Restart Server", () => activeServerOp("restart"), "");
  reg("lsp.stop", "LSP: Stop Server", () => activeServerOp("stop"), "");
  reg("lsp.toggleInlayHints", "LSP: Toggle Inlay Hints", toggleHints, "");
}
