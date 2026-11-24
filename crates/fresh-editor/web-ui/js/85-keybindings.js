// Keyboard shortcuts: the client's own screen over the core's keybinding
// model (`keys.*`). The core resolves what a key means, checks conflicts and
// writes the user's config on save; the client lists, filters and edits.

const keysUi = { open: false, filter: "", editing: null, error: "" };

async function openKeybindings() {
  closeMenus();
  try { await api.call("keys.open"); } catch (e) { showError(e); return; }
  keysUi.open = true;
  keysUi.filter = "";
  keysUi.editing = null;
  keysUi.error = "";
  renderKeybindings();
}

function closeKeybindings(save) {
  api.call(save ? "keys.save" : "keys.close").catch(showError);
  keysUi.open = false;
  overlay("keys", null);
  focusKeyboard();
}

/** A captured key as the API takes it, from a browser keydown. */
function capturedKey(e) {
  return { key: e.key, code: e.code, ctrl: e.ctrlKey, alt: e.altKey, shift: e.shiftKey, meta: e.metaKey };
}

/** The inline row that adds a binding or changes one. */
function editorRow(kb, b) {
  const ed = keysUi.editing;
  const row = div("kb-edit");
  const capture = el("input", "ui-field kb-capture");
  capture.readOnly = true;
  capture.placeholder = "Press a key…";
  capture.value = ed.display || "";
  const conflicts = el("div", "kb-conflicts", ed.conflicts && ed.conflicts.length ? "Also bound: " + ed.conflicts.join("; ") : "");
  capture.addEventListener("keydown", async (e) => {
    if (["Shift", "Control", "Alt", "Meta"].includes(e.key)) return;
    e.preventDefault();
    e.stopPropagation();
    ed.key = capturedKey(e);
    try {
      const r = await api.call("keys.conflicts", { key: ed.key, context: ed.context });
      ed.display = r.display;
      ed.conflicts = (r.conflicts || []).filter((c) => !b || !c.startsWith(b.label + " ("));
    } catch (err) { ed.display = ""; ed.conflicts = []; keysUi.error = err.message; }
    renderKeybindings();
  });
  const action = el("input", "ui-field kb-action");
  action.value = ed.action || "";
  action.placeholder = "Action";
  action.setAttribute("list", "kb-actions");
  action.addEventListener("input", () => { ed.action = action.value; });
  const list = el("datalist");
  list.id = "kb-actions";
  for (const a of kb.actions) { const o = el("option"); o.value = a; list.append(o); }
  const ctx = el("select", "ui-select");
  for (const c of kb.contexts) {
    const o = el("option", "", c);
    o.value = c;
    o.selected = c === ed.context;
    ctx.append(o);
  }
  ctx.addEventListener("change", () => { ed.context = ctx.value; });
  const apply = el("button", "btn primary", b ? "Apply" : "Add");
  apply.disabled = !ed.key;
  apply.addEventListener("click", async () => {
    try {
      await api.call("keys.set", { ...(b ? { index: b.index } : {}), key: ed.key, action: ed.action, context: ed.context });
      keysUi.editing = null;
      keysUi.error = "";
    } catch (e) { keysUi.error = e.message; }
    renderKeybindings();
  });
  const cancel = el("button", "btn", "Cancel");
  cancel.addEventListener("click", () => { keysUi.editing = null; keysUi.error = ""; renderKeybindings(); });
  row.append(capture, action, list, ctx, apply, cancel, conflicts);
  setTimeout(() => { if (!ed.key) capture.focus(); }, 0);
  return row;
}

function renderKeybindings() {
  const kb = core.keybindings;
  if (!keysUi.open || !kb) return;
  const keepSearch = document.activeElement && document.activeElement.classList.contains("kb-search");
  const box = div("settings keys");
  box.setAttribute("role", "dialog");
  box.setAttribute("aria-label", "Keyboard shortcuts");
  const head = div("settings-head");
  const search = el("input", "ui-field settings-search kb-search");
  search.placeholder = "Search by key, command or context";
  search.value = keysUi.filter;
  search.addEventListener("input", () => { keysUi.filter = search.value; renderKeybindings(); });
  const add = el("button", "btn", "Add…");
  add.addEventListener("click", () => {
    keysUi.editing = { index: null, key: null, display: "", action: "", context: "normal", conflicts: [] };
    renderKeybindings();
  });
  const cancel = el("button", "btn", "Cancel");
  cancel.addEventListener("click", () => closeKeybindings(false));
  const save = el("button", "btn primary", "Save");
  save.disabled = !kb.changed;
  save.addEventListener("click", () => closeKeybindings(true));
  head.append(el("span", "settings-title", "Keyboard Shortcuts"), search, add, cancel, save);
  const body = div("kb-body");
  if (keysUi.error) body.append(el("div", "kb-error", keysUi.error));
  if (keysUi.editing && keysUi.editing.index === null) body.append(editorRow(kb, null));
  const table = el("table", "kb-table");
  const thead = el("thead");
  const hr = el("tr");
  for (const h of ["Keys", "Command", "Context", "Source", ""]) hr.append(el("th", "", h));
  thead.append(hr);
  const tbody = el("tbody");
  const f = keysUi.filter.trim().toLowerCase();
  const rows = kb.bindings.filter((b) => !f || [b.keys, b.label, b.action, b.context].some((x) => String(x).toLowerCase().includes(f)));
  for (const b of rows.slice(0, 800)) {
    if (keysUi.editing && keysUi.editing.index === b.index) {
      const tr = el("tr", "editing");
      const td = el("td");
      td.colSpan = 5;
      td.append(editorRow(kb, b));
      tr.append(td);
      tbody.append(tr);
      continue;
    }
    const tr = el("tr", "src-" + b.source);
    tr.append(el("td", "kb-keys", b.keys || "—"), el("td", "", b.label), el("td", "kb-ctx", b.context), el("td", "kb-src", b.source));
    const actions = el("td", "kb-actions");
    const edit = el("button", "kb-btn", b.source === "unbound" ? "Bind" : "Edit");
    edit.addEventListener("click", () => {
      keysUi.editing = { index: b.index, key: null, display: b.keys, action: b.action, context: b.context === "global" || kb.contexts.includes(b.context) ? b.context : "normal", conflicts: [] };
      renderKeybindings();
    });
    actions.append(edit);
    if (b.source !== "unbound") {
      const del = el("button", "kb-btn", "Remove");
      del.title = "Remove this binding; the key falls through to anything else bound to it";
      del.addEventListener("click", () => api.call("keys.delete", { index: b.index }).catch(showError));
      const off = el("button", "kb-btn", "Disable");
      off.title = "Make this key do nothing here";
      off.addEventListener("click", () => api.call("keys.disable", { index: b.index }).catch(showError));
      actions.append(del, off);
    }
    tr.append(actions);
    tbody.append(tr);
  }
  table.append(thead, tbody);
  body.append(table);
  if (rows.length > 800) body.append(el("div", "kb-more", `${rows.length - 800} more — refine the search`));
  box.append(head, body);
  box.addEventListener("keydown", (e) => { if (e.key === "Escape" && !keysUi.editing) { e.preventDefault(); closeKeybindings(false); } });
  overlay("keys", scrim(box));
  if (keepSearch || !keysUi.editing) search.focus();
  if (keepSearch) search.setSelectionRange(search.value.length, search.value.length);
}

api.on("keybindings", (v) => {
  core.keybindings = v.keybindings;
  if (!v.keybindings && keysUi.open) { keysUi.open = false; overlay("keys", null); return; }
  renderKeybindings();
});
