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

export function activate(fresh) {
  const { h } = fresh;
  const SEV = ["", "error", "warning", "info", "hint"];
  const status = fresh.ui.status("lsp");

  const active = () => {
    const v = fresh.views.active();
    return v && fresh.lsp.available(v.buffer) ? v : null;
  };
  const ask = async (v, method, params = {}) => {
    try {
      return await fresh.lsp.request(v.buffer, method, params, v.cursor);
    } catch (e) {
      // A refused request carries the server's message as `detail`.
      fresh.ui.message(e.detail ?? String(e.message || e));
      return null;
    }
  };
  const pathOf = (uri) => decodeURIComponent(uri.replace(/^file:\/\//, ""));
  const rel = (p) => (p.startsWith(fresh.workspace.root + "/") ? p.slice(fresh.workspace.root.length + 1) : p);
  const asList = (r) => (r == null ? [] : Array.isArray(r) ? r : [r]);
  const loc = (l) => ({ path: pathOf(l.uri || l.targetUri), range: l.range || l.targetSelectionRange });

  // `how` as for `fresh.buffers.open` ({preview, focus}).
  const goto = (path, range, how = {}) => fresh.buffers.open(path, { line: range.start.line + 1, column: range.start.character + 1, ...how });

  // ---------------------------------------------------------------- navigation

  const definition = async () => {
    const v = active();
    if (!v) return;
    const r = asList(await ask(v, "textDocument/definition")).map(loc);
    if (!r.length) return fresh.ui.message("No definition found");
    if (r.length === 1) return goto(r[0].path, r[0].range);
    fresh.ui.pick({ title: "Definitions", items: r.map((l) => ({ label: `${rel(l.path)}:${l.range.start.line + 1}`, value: l })), onAccept: (it) => goto(it.value.path, it.value.range) });
  };

  let refsPanel = null;
  const references = async () => {
    const v = active();
    if (!v) return;
    const r = asList(await ask(v, "textDocument/references", { context: { includeDeclaration: true } })).map(loc);
    if (!r.length) return fresh.ui.message("No references found");
    // Show each reference's line: read it from the file through the server.
    const texts = new Map();
    for (const l of r) {
      if (!texts.has(l.path)) {
        try { texts.set(l.path, (await fresh.host.read(l.path)).split("\n")); } catch { texts.set(l.path, []); }
      }
    }
    const items = [];
    let last = null;
    for (const l of r) {
      if (l.path !== last) { items.push({ header: true, path: l.path }); last = l.path; }
      items.push({ ...l, text: (texts.get(l.path)[l.range.start.line] || "").trim() });
    }
    refsPanel = fresh.ui.panel({ id: "references", title: `References (${r.length})`, place: "bottom" });
    refsPanel.render(h("list", {
      key: "refs",
      items,
      render: (it) => it.header ? h("text", { bold: true }, [rel(it.path)]) : h("row", {}, [h("text", { dim: true }, [`  ${it.range.start.line + 1}:${it.range.start.character + 1}`]), h("text", { mono: true }, [it.text])]),
      // Selected, a reference shows in the preview tab (the keyboard stays
      // in the list); Enter or a double-click opens it for good.
      onSelect: (it) => !it.header && goto(it.path, it.range, { preview: true, focus: false }),
      onActivate: (it) => !it.header && goto(it.path, it.range, { preview: false }),
    }));
    refsPanel.focus("refs");
  };

  // The palette's `@`: the active file's symbols, filtered by what is
  // typed, each previewed as it is selected. Asked once per opening.
  let symCache = null;
  const KINDS = { 5: "class", 6: "method", 12: "function", 13: "variable", 14: "constant", 23: "struct", 10: "enum", 11: "interface", 8: "field", 2: "module" };
  fresh.palette.provide("@", {
    hint: "symbols",
    items: async (q) => {
      const v = active();
      if (!v) return [{ label: "No language server for this file" }];
      const key = `${v.buffer.id}:${v.buffer.length}`;
      if (!symCache || symCache.key !== key) symCache = { key, list: asList(await ask(v, "textDocument/documentSymbol")) };
      const out = [];
      const walk = (list, path) => {
        for (const s of list) {
          const range = s.selectionRange || s.location?.range || s.range;
          const go = (o) => { const p = fresh.lsp.offset(v.buffer, range.start.line, range.start.character); if (p != null) v.setCursor(p, { center: true, ...o }); };
          if (!q || s.name.toLowerCase().includes(q.toLowerCase())) out.push({ label: s.name, detail: [KINDS[s.kind], path || s.containerName].filter(Boolean).join(" · "), run: () => go({}), preview: () => go({}) });
          if (s.children) walk(s.children, path ? `${path}.${s.name}` : s.name);
        }
      };
      walk(symCache.list, "");
      return out.length ? out : [{ label: q ? "No symbol matches" : "No symbols" }];
    },
  });
  const symbols = () => fresh.commands.run("palette.open", "@");

  // The palette's `%`: the workspace's symbols matching what is typed,
  // from every running server.
  let running = {}; // {language: running}, from the server
  const runningLanguages = async () => {
    try { running = await fresh.lsp.status(); } catch { running = {}; }
    return Object.keys(running).filter((l) => running[l]).sort();
  };
  fresh.palette.provide("%", {
    hint: "workspace symbols",
    items: async (q) => {
      if (!q) return [{ label: "Type a symbol's name" }];
      const langs = await runningLanguages();
      if (!langs.length) return [{ label: "No language server is running (open a file of its language)" }];
      const out = [];
      for (const l of langs) {
        let r = null;
        try { r = await fresh.lsp.requestWorkspace(l, "workspace/symbol", { query: q }); } catch { continue; }
        for (const s of asList(r)) {
          const loc = s.location;
          if (!loc || !loc.uri || !loc.uri.startsWith("file://")) continue;
          const path = pathOf(loc.uri), range = loc.range || { start: { line: 0, character: 0 } };
          out.push({
            label: s.name,
            detail: [KINDS[s.kind], s.containerName, `${rel(path)}:${range.start.line + 1}`].filter(Boolean).join(" · "),
            run: () => goto(path, range, { preview: false }),
          });
        }
      }
      return out.length ? out : [{ label: "No symbol matches" }];
    },
  });
  const workspaceSymbols = () => fresh.commands.run("palette.open", "%");

  // ---------------------------------------------------------------- edits

  // Apply a WorkspaceEdit through the documents (open or not), so every
  // client and every undo sees it.
  const applyWorkspaceEdit = async (edit) => {
    const byUri = new Map();
    for (const [uri, edits] of Object.entries(edit.changes || {})) byUri.set(uri, edits);
    for (const dc of edit.documentChanges || []) if (dc.textDocument) byUri.set(dc.textDocument.uri, dc.edits);
    let n = 0;
    for (const [uri, edits] of byUri) {
      const b = await fresh.buffers.load(pathOf(uri));
      n += applyTextEdits(b, edits);
    }
    return n;
  };
  const applyTextEdits = (b, edits) => {
    const ops = [];
    const resolved = edits
      .map((e) => ({ s: fresh.lsp.offset(b, e.range.start.line, e.range.start.character), e: fresh.lsp.offset(b, e.range.end.line, e.range.end.character), text: e.newText }))
      .filter((e) => e.s != null && e.e != null)
      .sort((x, y) => y.s - x.s);
    for (const e of resolved) {
      if (e.e > e.s) ops.push({ t: "del", pos: e.s, len: e.e - e.s });
      if (e.text) ops.push({ t: "ins", pos: e.s, text: e.text });
    }
    if (ops.length) b.edit(ops);
    return resolved.length;
  };

  // The identifier around the cursor.
  const currentWord = (v) => {
    const b = v.buffer;
    const left = b.text(Math.max(0, v.cursor - 100), v.cursor) || "";
    const right = b.text(v.cursor, Math.min(b.length, v.cursor + 100)) || "";
    return /[\w$]*$/.exec(left)[0] + /^[\w$]*/.exec(right)[0];
  };

  const rename = async () => {
    const v = active();
    if (!v) return;
    const name = await fresh.ui.input({ title: "Rename to", value: currentWord(v) });
    if (!name) return;
    const r = await ask(v, "textDocument/rename", { newName: name });
    if (!r) return fresh.ui.message("Nothing to rename here");
    fresh.ui.message(`Renamed: ${await applyWorkspaceEdit(r)} edit(s)`);
  };

  const codeActions = async () => {
    const v = active();
    if (!v) return;
    const sel = v.selection || { start: v.cursor, end: v.cursor };
    const toPos = (p) => fresh.lsp.position(v.buffer, p);
    const diags = fresh.lsp.diagnostics(v.buffer).filter((d) => d.start <= sel.end && d.end >= sel.start);
    const r = asList(await ask(v, "textDocument/codeAction", {
      range: { start: toPos(sel.start), end: toPos(sel.end) },
      context: { diagnostics: diags.map((d) => ({ message: d.message, severity: d.severity, range: { start: toPos(d.start), end: toPos(d.end) } })) },
    }));
    if (!r.length) return fresh.ui.message("No code actions here");
    fresh.ui.pick({
      title: "Code actions",
      items: r.map((a) => ({ label: a.title, detail: a.kind || "", value: a })),
      onAccept: async ({ value: a }) => {
        if (a.edit) await applyWorkspaceEdit(a.edit);
        const cmd = a.command && typeof a.command === "object" ? a.command : a.command ? a : null;
        if (cmd && cmd.command) await ask(v, "workspace/executeCommand", { command: cmd.command, arguments: cmd.arguments || [] });
      },
    });
  };

  const format = async () => {
    const v = active();
    if (!v) return;
    const r = asList(await ask(v, "textDocument/formatting", { options: { tabSize: 4, insertSpaces: true } }));
    if (!r.length) return fresh.ui.message("Already formatted (or the server does not format)");
    fresh.ui.message(`Formatted: ${applyTextEdits(v.buffer, r)} edit(s)`);
  };

  // ---------------------------------------------------------------- completion, hover, signatures

  const completion = async () => {
    const v = active();
    if (!v) return;
    const r = await ask(v, "textDocument/completion");
    const items = asList(r && r.items ? r.items : r);
    if (!items.length) return fresh.ui.message("No completions");
    const word = /[\w$]*$/.exec(v.buffer.text(Math.max(0, v.cursor - 100), v.cursor) || "")[0];
    fresh.ui.pick({
      title: "Complete",
      value: word,
      items: items.slice(0, 500).map((i) => ({ label: i.label, detail: i.detail || "", value: i })),
      onAccept: ({ value: i }) => {
        const vv = fresh.views.active();
        const b = vv.buffer;
        const left = b.text(Math.max(0, vv.cursor - 100), vv.cursor) || "";
        const w = /[\w$]*$/.exec(left)[0];
        const text = i.textEdit?.newText ?? i.insertText ?? i.label;
        const start = vv.cursor - fresh.text.byteLen(w);
        b.replace(start, vv.cursor, text);
        vv.setCursor(start + fresh.text.byteLen(text));
      },
    });
  };

  // Hover contents as text: markdown's code fences dropped (the popup is
  // monospace already) and its rules shortened.
  const hoverText = (r) => {
    const c = r?.contents;
    if (!c) return null;
    const md = typeof c === "string" ? c : Array.isArray(c) ? c.map((x) => (typeof x === "string" ? x : x.value)).join("\n\n") : c.value;
    if (!md) return null;
    return md.split("\n").filter((l) => !/^\s*```/.test(l)).map((l) => (/^\s*(-{3,}|\*{3,}|_{3,})\s*$/.test(l) ? "───" : l)).join("\n").trim() || null;
  };
  fresh.hover.provide(async (b, pos) => {
    const diags = fresh.lsp.diagnostics(b).filter((d) => pos >= d.start && pos <= Math.max(d.end, d.start + 1));
    let text = diags.map((d) => `${SEV[d.severity] || "error"}: ${d.message}`).join("\n");
    if (fresh.lsp.available(b)) {
      try { const t = hoverText(await fresh.lsp.request(b, "textDocument/hover", {}, pos)); if (t) text = text ? `${text}\n\n${t}` : t; } catch {}
    }
    return text || null;
  });
  const hover = async () => {
    const v = active();
    if (!v) return;
    const t = hoverText(await ask(v, "textDocument/hover"));
    if (t) fresh.ui.popup(t);
  };

  const signatureHelp = async () => {
    const v = active();
    if (!v) return;
    const r = await ask(v, "textDocument/signatureHelp");
    const sig = r?.signatures?.[r.activeSignature ?? 0];
    if (sig) fresh.ui.popup(sig.label + (sig.documentation ? `\n\n${typeof sig.documentation === "string" ? sig.documentation : sig.documentation.value}` : ""));
  };

  fresh.events.on("buffer.changed", ({ buffer, ops, local }) => {
    if (!local || !fresh.lsp.available(buffer)) return;
    const op = ops[0];
    if (op?.t !== "ins") return;
    // The inserted text's first character: typing "(" inserts "()".
    const ch = op.text[0];
    if (ch === "(" || ch === ",") setTimeout(signatureHelp, 50);
    else if (ch === "." && op.text.length === 1) setTimeout(completion, 50);
  });

  // ---------------------------------------------------------------- diagnostics

  let diagPanel = null;
  const renderDiagnostics = () => {
    if (!diagPanel || !diagPanel.open) return;
    const items = [];
    for (const b of fresh.buffers.all()) {
      const ds = fresh.lsp.diagnostics(b);
      if (!ds.length) continue;
      items.push({ header: true, b });
      const L = b.lines();
      for (const d of ds.sort((x, y) => x.start - y.start)) {
        const i = L ? L.starts.findIndex((s, k) => d.start >= s && (k + 1 >= L.starts.length || d.start < L.starts[k + 1])) : -1;
        items.push({ b, d, line: i >= 0 ? (L.firstLine ?? 0) + i + 1 : "?" });
      }
    }
    diagPanel.render(h("list", {
      key: "diags",
      items,
      render: (it) => it.header
        ? h("text", { bold: true }, [rel(it.b.path)])
        : h("row", {}, [h("text", { color: it.d.severity === 1 ? "diagnostic.error" : "diagnostic.warning" }, [it.d.severity === 1 ? "  ●" : "  ▲"]), h("text", { dim: true }, [String(it.line)]), h("text", { mono: true }, [it.d.message.split("\n")[0]])]),
      onActivate: async (it) => { if (it.header) return; await fresh.buffers.open(it.b.path); fresh.views.active().setCursor(it.d.start, { center: true }); },
    }));
  };
  const diagnosticsPanel = () => {
    diagPanel = fresh.ui.panel({ id: "diagnostics", title: "Diagnostics", place: "bottom" });
    renderDiagnostics();
    diagPanel.focus("diags");
  };
  fresh.events.on("diagnostics.changed", renderDiagnostics);

  // ---------------------------------------------------------------- folding and status

  const refreshFolds = async (b) => {
    if (!fresh.lsp.available(b)) return;
    try {
      const r = await fresh.lsp.request(b, "textDocument/foldingRange", {});
      b.setFoldRanges(asList(r).map((f) => ({ start: f.startLine, end: f.endLine })));
    } catch {}
  };
  // The status bar: the active file's server, or `LSP (off)` when it was
  // stopped.
  const showStatus = (b) => {
    const installed = b && b.language && fresh.lsp.languages().includes(b.language) && !b.isVirtual;
    const text = !installed ? "" : fresh.lsp.available(b) ? `LSP ${b.language}` : running[b.language] === false ? "LSP (off)" : "";
    status.set(text, { title: "Language servers: Show Status", onClick: () => fresh.commands.run("lsp.status") });
  };
  fresh.events.on("buffer.activated", ({ buffer }) => { showStatus(buffer); refreshFolds(buffer); refreshHints(buffer); });
  // Its language changed: another server, or none.
  fresh.events.on("buffer.info", ({ buffer }) => { if (buffer.id === fresh.buffers.active()?.id) showStatus(buffer); });
  // Each language's server: running, available (started with its first
  // file), or none found on the host.
  // Picking one offers to restart or stop it.
  const lspStatus = async () => {
    await runningLanguages();
    const langs = [...new Set([...fresh.lsp.languages(), ...Object.keys(running)])].sort();
    const state = (l) => (running[l] ? "running" : running[l] === false ? "stopped" : fresh.lsp.languages().includes(l) ? "available (starts with a file)" : "not started");
    fresh.ui.pick({
      title: fresh.workspace.trusted ? "Language servers" : "Language servers (the workspace is restricted: trust it to start them)",
      items: langs.length ? langs.map((l) => ({ label: l, detail: state(l), value: l })) : [{ label: "No language server found on the host", value: null }],
      onAccept: async (it) => {
        if (!it.value || !fresh.workspace.trusted) return;
        const what = await fresh.ui.choose(`${it.value} language server (${state(it.value)})`, [
          { label: running[it.value] ? "Restart" : "Start", value: "restart" },
          ...(running[it.value] ? [{ label: "Stop", value: "stop" }] : []),
        ]);
        if (what) await serverOp(what, it.value);
      },
    });
  };
  // Restart or stop `language`'s server (the active file's when not given).
  const serverOp = async (what, language) => {
    const b = fresh.buffers.active();
    const l = language || (b && fresh.lsp.languages().includes(b.language) ? b.language : null);
    if (!l) { lspStatus(); return; }
    try {
      running = await (what === "stop" ? fresh.lsp.stop(l) : fresh.lsp.restart(l));
      fresh.ui.message(what === "stop" ? `Stopped the ${l} language server` : `Restarted the ${l} language server`);
    } catch (e) { fresh.ui.message(e.detail ?? String(e.message || e)); }
  };
  fresh.events.on("lsp.status", ({ status: s }) => {
    running = s;
    const b = fresh.buffers.active();
    showStatus(b);
    for (const x of fresh.buffers.all()) refreshHints(x);
  });

  // ---------------------------------------------------------------- inlay hints

  // A document's hints, drawn before the character they are at (in the
  // line numbers' colour), asked for the whole document once its text has
  // settled after an edit. An answer to an older text is dropped.
  const hintTimers = new Map();
  const refreshHints = async (b) => {
    const deco = b.decorations("inlay");
    if (!fresh.settings.inlay_hints || !fresh.lsp.available(b) || !b.whole) { deco.clear(); b.render(); return; }
    const L = b.lines();
    if (!L) return;
    const len = b.length, text0 = b.text(0, Math.min(len, 64));
    let r = null;
    try {
      r = await fresh.lsp.request(b, "textDocument/inlayHint", { range: { start: { line: 0, character: 0 }, end: { line: L.lines.length, character: 0 } } });
    } catch { return; }
    if (b.length !== len || b.text(0, Math.min(len, 64)) !== text0) return;
    deco.clear();
    for (const hnt of asList(r)) {
      const pos = fresh.lsp.offset(b, hnt.position.line, hnt.position.character);
      if (pos == null) continue;
      const label = typeof hnt.label === "string" ? hnt.label : asList(hnt.label).map((p) => p.value).join("");
      const text = (hnt.paddingLeft ? " " : "") + label + (hnt.paddingRight ? " " : "");
      deco.inline(pos, text, { side: "before", style: { fg: "editor.line_number_fg", italic: true } });
    }
    b.render();
  };
  const hintsSoon = (b) => {
    clearTimeout(hintTimers.get(b.id));
    hintTimers.set(b.id, setTimeout(() => { hintTimers.delete(b.id); refreshHints(b); }, 500));
  };
  fresh.events.on("buffer.changed", ({ buffer }) => { if (fresh.settings.inlay_hints && fresh.lsp.available(buffer)) hintsSoon(buffer); });
  const toggleHints = () => {
    fresh.settings.inlay_hints = !fresh.settings.inlay_hints;
    fresh.saveSettings();
    for (const b of fresh.buffers.all()) refreshHints(b);
    fresh.ui.message(`Inlay hints: ${fresh.settings.inlay_hints ? "on" : "off"}`);
  };
  // Trusting the workspace starts its language servers.
  fresh.events.on("workspace.trusted", () => { const b = fresh.buffers.active(); showStatus(b); if (b) { refreshFolds(b); refreshHints(b); } });
  fresh.events.on("buffer.saved", ({ buffer }) => refreshFolds(buffer));

  const reg = (id, title, run, key) => { fresh.commands.register(id, { title, run }); if (key) fresh.keys.bind(key, id); };
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
    const v = fresh.views.active();
    if (!v) return;
    const ds = fresh.lsp.diagnostics(v.buffer).sort((a, b) => a.start - b.start || a.severity - b.severity);
    if (!ds.length) { fresh.ui.message("No problems in this file"); return; }
    const at = v.cursor;
    const d = dir > 0 ? ds.find((x) => x.start > at) || ds[0] : [...ds].reverse().find((x) => x.start < at) || ds[ds.length - 1];
    await v.setCursor(d.start, { center: true });
    fresh.ui.message(`${SEV[d.severity] || "error"}: ${d.message.split("\n")[0]}`);
  };
  reg("lsp.nextDiagnostic", "Go to Next Problem", () => stepDiagnostic(1), "F8");
  reg("lsp.prevDiagnostic", "Go to Previous Problem", () => stepDiagnostic(-1), "S-F8");
  reg("lsp.status", "LSP: Show Status", lspStatus);
  reg("lsp.workspaceSymbols", "Go to Symbol in Workspace…", workspaceSymbols);
  reg("lsp.restart", "LSP: Start/Restart Server", () => serverOp("restart"));
  reg("lsp.stop", "LSP: Stop Server", () => serverOp("stop"));
  reg("lsp.toggleInlayHints", "LSP: Toggle Inlay Hints", toggleHints);
}
