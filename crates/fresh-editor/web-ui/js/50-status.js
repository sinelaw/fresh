// The status bar: the core's facts (cursor, language, diagnostics, LSP,
// messages, plugin items), arranged by the client.

function renderStatus() {
  const s = core.status || {};
  const host = document.getElementById("statusbar");
  const b = bufferById(s.buffer);
  // `anchor` names the item a popup can stand on (the core's
  // `status:<name>` placement).
  const item = (text, cls, title, onClick, anchor) => {
    if (text === undefined || text === null || text === "") return null;
    const e = el(onClick ? "button" : "span", "sitem " + (cls || ""), text);
    if (title) e.title = title;
    if (anchor) e.dataset.anchor = anchor;
    if (onClick) e.addEventListener("click", onClick);
    return e;
  };
  const action = (name) => () => api.send("action", { name });
  const cmd = (name) => () => { api.send("command.run", { name }); focusKeyboard(); };
  const d = s.diagnostics || {};
  const left = [
    item(b ? baseName(b.name) + (b.modified ? " ●" : "") : "", "sname", b && b.path),
    s.readOnly ? item("read-only", "sro", "Read-only", null, "readOnly") : null,
    s.line ? item(`Ln ${s.line}, Col ${s.col}`, "", "Go to line", cmd("Go to Line")) : null,
    s.cursors > 1 ? item(`${s.cursors} cursors`) : null,
    (d.errors || d.warnings) ? item(`⨯ ${d.errors || 0}  ⚠ ${d.warnings || 0}`, d.errors ? "serr" : "swarn", "Diagnostics", cmd("Show Diagnostics")) : null,
    item([s.message, s.pluginMessage].filter(Boolean).join(" | "), "smsg"),
  ];
  const right = [
    ...(s.items || []).map((i) => item(i.text, "splugin", i.key)),
    item(s.remote, "sremote", "Remote", null, "remote"),
    s.lsp ? item(s.lsp.trim(), "slsp lsp-" + String(s.lspState || "").toLowerCase(), "Language servers", action("show_lsp_status"), "lsp") : null,
    item(s.languageName, "", "Language", cmd("Set Language"), "language"),
    item(s.encoding, "", "Encoding", cmd("Set Encoding"), "encoding"),
    item(s.lineEnding, "", "Line ending", cmd("Set Line Ending"), "lineEnding"),
    s.paletteKey ? item(`Palette ${keyLabel(s.paletteKey)}`, "spalette", "Command palette",
      () => { api.send("action", { name: "quick_open" }); }) : null,
  ];
  const l = div("sleft"), r = div("sright");
  l.append(...left.filter(Boolean));
  r.append(...right.filter(Boolean));
  host.replaceChildren(l, r);
}

api.on("status", renderStatus);
api.on("buffers", renderStatus);
