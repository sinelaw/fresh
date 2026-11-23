// Questions. The core asks through `ask` — a prompt (palette, quick open, go
// to line, find, …) with suggestions, or a confirmation with choices — and
// the client draws and answers it. The client's own questions (a name for a
// new file, "delete this?") use the same look, answered locally.

const askUi = { box: null, input: null, list: null, sent: [], kind: null };

function renderAsk() {
  const a = core.ask;
  if (!a) {
    if (askUi.box) { overlay("ask", null); askUi.box = null; askUi.kind = null; focusKeyboard(); }
    for (const v of views.values()) v.placeCaret();
    return;
  }
  if (a.kind === "confirm") return renderConfirm(a);
  if (a.kind === "trust") return renderTrust(a);
  if (askUi.kind !== "prompt") {
    askUi.kind = "prompt";
    const box = div("ask palette");
    const head = div("ask-head");
    const label = el("span", "ask-msg");
    const input = el("input", "ask-input");
    input.spellcheck = false;
    input.autocomplete = "off";
    head.append(label, input);
    const list = div("ask-list");
    list.setAttribute("role", "listbox");
    box.append(head, list);
    input.addEventListener("input", () => {
      askUi.sent.push(input.value);
      if (askUi.sent.length > 64) askUi.sent.shift();
      api.send("prompt.set", { text: input.value });
    });
    input.addEventListener("keydown", (e) => {
      const p = core.ask;
      if (!p || p.kind !== "prompt") return;
      // A file browser's keys are the core's: it moves the selection,
      // enters directories and goes up a level.
      if (p.browser && ["ArrowDown", "ArrowUp", "PageDown", "PageUp", "Enter"].includes(e.key)) {
        e.preventDefault();
        api.send("key", { key: e.key });
        return;
      }
      if (p.browser && e.key === "Backspace" && input.value === "") {
        e.preventDefault();
        api.send("key", { key: "Backspace" });
        return;
      }
      const n = p.suggestions.length;
      const sel = p.selected ?? -1;
      const pick = (i) => { e.preventDefault(); api.send("prompt.select", { index: i }); };
      if (e.key === "ArrowDown" && n) return pick((sel + 1) % n);
      if (e.key === "ArrowUp" && n) return pick((sel - 1 + n) % n);
      if (e.key === "PageDown" && n) return pick(Math.min(n - 1, sel + 10));
      if (e.key === "PageUp" && n) return pick(Math.max(0, sel - 10));
      if (e.key === "Enter") { e.preventDefault(); api.send("prompt.confirm"); return; }
      if (e.key === "Escape") { e.preventDefault(); api.send("prompt.cancel"); return; }
      if (e.key === "Tab") { e.preventDefault(); api.send("key", { key: "Tab", shift: e.shiftKey }); return; }
    });
    askUi.box = box; askUi.input = input; askUi.list = list; askUi.label = label;
    askUi.sent = [];
    // Dimmed like any modal; a press outside it cancels, as Escape does.
    overlay("ask", scrim(box, () => api.send("prompt.cancel")));
    input.focus();
  }
  askUi.label.textContent = a.message || "";
  askUi.label.hidden = !a.message;
  // The core echoes each text we sent, late; only a text we did not send
  // (a completion, a history recall) replaces what is being typed.
  if (a.query === askUi.input.value) askUi.sent = [];
  else if (!askUi.sent.includes(a.query)) askUi.input.value = a.query;
  const rows = a.suggestions.map((s, i) => {
    const r = div("ask-row" + (i === a.selected ? " selected" : "") + (s.disabled ? " disabled" : ""));
    r.setAttribute("role", "option");
    r.setAttribute("aria-selected", String(i === a.selected));
    const main = div("ask-main");
    main.append(el("span", "ask-text", s.text));
    if (s.description) main.append(el("span", "ask-desc", s.description));
    r.append(main);
    if (s.keys) r.append(el("kbd", "ask-keys", keyLabel(s.keys)));
    r.addEventListener("mousedown", (e) => e.preventDefault());
    r.addEventListener("click", () => {
      if (s.disabled) return;
      api.send("prompt.select", { index: i });
      api.send("prompt.confirm");
    });
    return r;
  });
  askUi.list.replaceChildren(...rows);
  askUi.list.hidden = rows.length === 0;
  renderBrowser(a);
  const sel = askUi.list.children[a.selected];
  if (sel) sel.scrollIntoView({ block: "nearest" });
  if (document.activeElement !== askUi.input) askUi.input.focus();
  for (const v of views.values()) v.placeCaret();
}

/** The directory a file prompt is browsing, under its input. */
function renderBrowser(a) {
  let host = askUi.box.querySelector(".browser");
  const b = a.browser;
  if (!b) { if (host) host.remove(); return; }
  if (!host) { host = div("browser"); askUi.box.appendChild(host); }
  const head = div("browser-head");
  head.append(el("span", "browser-dir", b.dir));
  const hidden = el("label", "browser-hidden");
  const box = el("input");
  box.type = "checkbox";
  box.checked = !!b.showHidden;
  box.addEventListener("change", () => api.send("browser.hidden"));
  hidden.append(box, document.createTextNode(" Hidden files"));
  head.append(hidden);
  const shortcuts = div("browser-shortcuts");
  for (const s of b.shortcuts) {
    const c = el("button", "chip", s.label);
    c.title = s.description || s.path;
    c.addEventListener("mousedown", (e) => e.preventDefault());
    c.addEventListener("click", () => api.send("browser.cd", { path: s.path }));
    shortcuts.append(c);
  }
  const list = div("browser-list");
  list.setAttribute("role", "listbox");
  if (b.loading) list.append(el("div", "browser-note", "Loading…"));
  if (b.error) list.append(el("div", "browser-note error", b.error));
  b.entries.forEach((e, i) => {
    const index = b.indices[i];
    const r = div("ask-row" + (index === b.selected ? " selected" : ""));
    r.setAttribute("role", "option");
    const main = div("ask-main");
    main.append(el("span", "browser-icon", e.dir ? "▸" : " "), el("span", "ask-text", e.name + (e.dir && e.name !== ".." ? "/" : "")));
    r.append(main);
    if (!e.dir && e.size !== null && e.size !== undefined) r.append(el("span", "ask-desc", humanSize(e.size)));
    r.addEventListener("mousedown", (ev) => ev.preventDefault());
    r.addEventListener("click", (ev) => {
      api.send("browser.select", { index });
      if (e.dir || ev.detail >= 2) api.send("key", { key: "Enter" });
    });
    list.append(r);
  });
  host.replaceChildren(head, shortcuts, list);
  const s = list.querySelector(".selected");
  if (s) s.scrollIntoView({ block: "nearest" });
}

function humanSize(n) {
  const u = ["B", "KB", "MB", "GB", "TB"];
  let i = 0;
  while (n >= 1024 && i < u.length - 1) { n /= 1024; i++; }
  return (i ? n.toFixed(1) : n) + " " + u[i];
}

function renderConfirm(a) {
  askUi.kind = "confirm";
  const box = div("ask dialog");
  box.setAttribute("role", "alertdialog");
  box.append(el("div", "dlg-title", a.title));
  if (a.body) box.append(el("div", "dlg-body", a.body));
  if (a.detail) box.append(el("div", "dlg-detail", a.detail));
  const row = div("dlg-buttons");
  a.choices.forEach((c, i) => {
    const b = el("button", "btn" + (i === a.selected ? " primary" : "") + (c.destructive ? " danger" : ""), c.label);
    b.addEventListener("click", () => api.send("confirm.choose", { index: i }));
    row.append(b);
  });
  box.append(row);
  box.tabIndex = -1;
  // The question's keys are the core's: Enter, Escape, the arrows and each
  // choice's mnemonic letter answer as they do in the terminal.
  box.addEventListener("keydown", (e) => {
    if (["Shift", "Control", "Alt", "Meta"].includes(e.key)) return;
    e.preventDefault();
    api.send("key", { key: e.key, code: e.code, ctrl: e.ctrlKey, alt: e.altKey, shift: e.shiftKey, meta: e.metaKey });
  });
  overlay("ask", scrim(box));
  askUi.box = box;
  box.focus();
}

/** The workspace-trust question: pick a level, then commit it. */
function renderTrust(a) {
  askUi.kind = "trust";
  const box = div("ask dialog trust");
  box.setAttribute("role", "alertdialog");
  box.append(el("div", "dlg-title", a.title));
  box.append(el("div", "dlg-detail", a.path));
  if (a.triggers && a.triggers.length) box.append(el("div", "dlg-body", a.triggers.join(", ")));
  const opts = div("trust-options");
  opts.setAttribute("role", "radiogroup");
  a.options.forEach((o, i) => {
    const r = el("label", "trust-option" + (i === a.selected ? " selected" : ""));
    const input = el("input");
    input.type = "radio";
    input.name = "trust";
    input.checked = i === a.selected;
    input.addEventListener("change", () => api.send("trust.select", { index: i }));
    const text = div("trust-text");
    text.append(el("div", "trust-label", o.label), el("div", "dlg-detail", o.description));
    r.append(input, text);
    opts.append(r);
  });
  box.append(opts);
  const row = div("dlg-buttons");
  const no = el("button", "btn", a.declineLabel);
  no.addEventListener("click", () => api.send("trust.decline"));
  const ok = el("button", "btn primary", a.okLabel);
  ok.addEventListener("click", () => api.send("trust.confirm"));
  row.append(no, ok);
  box.append(row);
  box.tabIndex = -1;
  box.addEventListener("keydown", (e) => {
    if (e.key === "Enter") { e.preventDefault(); api.send("trust.confirm"); }
    if (e.key === "ArrowDown" || e.key === "ArrowUp") {
      e.preventDefault();
      const n = a.options.length;
      api.send("trust.select", { index: (a.selected + (e.key === "ArrowDown" ? 1 : n - 1)) % n });
    }
  });
  overlay("ask", scrim(box));
  askUi.box = box;
  box.focus();
}

/** A dimmed backdrop around a modal box; `outside`, if given, runs on a
 *  press on the backdrop itself. */
function scrim(box, outside) {
  const s = div("scrim");
  s.appendChild(box);
  if (outside) s.addEventListener("mousedown", (e) => { if (e.target === s) { e.preventDefault(); outside(); } });
  return s;
}

api.on("ask", renderAsk);

// ---- the client's own questions --------------------------------------------

function askText(title, initial) {
  return new Promise((resolve) => {
    const box = div("ask dialog");
    box.append(el("div", "dlg-title", title));
    const input = el("input", "ask-input");
    input.value = initial || "";
    box.append(input);
    const done = (v) => { overlay("local", null); resolve(v); focusKeyboard(); };
    input.addEventListener("keydown", (e) => {
      if (e.key === "Enter") { e.preventDefault(); done(input.value.trim()); }
      if (e.key === "Escape") { e.preventDefault(); done(null); }
    });
    const s = scrim(box);
    s.addEventListener("mousedown", (e) => { if (e.target === s) done(null); });
    overlay("local", s);
    input.focus();
    input.select();
  });
}

function askConfirm(title, body, yes) {
  return new Promise((resolve) => {
    const box = div("ask dialog");
    box.append(el("div", "dlg-title", title));
    if (body) box.append(el("div", "dlg-body", body));
    const row = div("dlg-buttons");
    const done = (v) => { overlay("local", null); resolve(v); focusKeyboard(); };
    const no = el("button", "btn", "Cancel");
    no.addEventListener("click", () => done(false));
    const ok = el("button", "btn primary danger", yes);
    ok.addEventListener("click", () => done(true));
    row.append(no, ok);
    box.append(row);
    box.tabIndex = -1;
    box.addEventListener("keydown", (e) => {
      if (e.key === "Escape") done(false);
      if (e.key === "Enter") done(true);
    });
    overlay("local", scrim(box));
    box.focus();
  });
}

function showError(e) {
  const t = el("div", "toast", String(e && e.message ? e.message : e));
  document.getElementById("overlays").appendChild(t);
  setTimeout(() => t.remove(), 4000);
}
