// Settings: the client's own screen over the core's schema and values. A
// change is sent as a JSON pointer and a value; the core validates, applies
// and saves it to the user's config, and the new values come back as the
// `settings` event.

const settingsUi = { schema: null, sources: {}, open: false, category: null, filter: "" };

async function openSettings() {
  // Asked on every open: the lists a setting chooses from (the installed
  // themes) change while the editor runs.
  try {
    const r = await api.call("settings.schema");
    settingsUi.schema = r.schema;
    settingsUi.sources = r.sources || {};
  } catch (e) { showError(e); return; }
  settingsUi.open = true;
  renderSettings();
}

function closeSettings() {
  settingsUi.open = false;
  overlay("settings", null);
  focusKeyboard();
}

function resolveRef(s) {
  const root = settingsUi.schema;
  let guard = 0;
  while (s && s.$ref && guard++ < 10) {
    const path = s.$ref.replace(/^#\//, "").split("/");
    let t = root;
    for (const p of path) t = t && t[p];
    s = { ...t, ...s, $ref: undefined };
  }
  return s || {};
}

function prettyName(key) {
  return key.replace(/_/g, " ").replace(/^./, (c) => c.toUpperCase());
}

/** The categories: one per object-valued top-level setting, plus General. */
function settingsCategories() {
  const props = settingsUi.schema.properties || {};
  const cats = new Map([["General", []]]);
  for (const [k, raw] of Object.entries(props)) {
    const s = resolveRef(raw);
    const cat = raw["x-category"] || (s.type === "object" && s.properties ? prettyName(k) : "General");
    if (!cats.has(cat)) cats.set(cat, []);
    if (s.type === "object" && s.properties && !raw["x-category"]) {
      for (const [ck, cs] of Object.entries(s.properties)) cats.get(cat).push({ ptr: `/${k}/${ck}`, key: ck, schema: cs });
    } else {
      cats.get(cat).push({ ptr: `/${k}`, key: k, schema: raw });
    }
  }
  return cats;
}

function valueAt(ptr) {
  let v = core.settings && core.settings.values;
  for (const p of ptr.split("/").slice(1)) v = v == null ? undefined : v[p];
  return v;
}

function setSetting(ptr, value) {
  api.call("settings.set", { changes: { [ptr]: value } }).catch(showError);
}

/** A setting's choices as [{label, value}], or null for a free value: an
 *  enum, a set of constants, or a live list (`x-enum-from`, sent with the
 *  schema). */
function choicesOf(s) {
  const plain = (vs) => vs.map((v) => ({ label: v === null ? "(automatic)" : String(v), value: v }));
  if (typeof s["x-enum-from"] === "string" && Array.isArray(settingsUi.sources[s["x-enum-from"]])) {
    return settingsUi.sources[s["x-enum-from"]].map((o) => ({ label: o.name, value: o.value }));
  }
  if (Array.isArray(s.enum)) return plain(s.enum);
  const alts = s.oneOf || s.anyOf;
  if (Array.isArray(alts) && alts.every((a) => a.const !== undefined || Array.isArray(resolveRef(a).enum))) {
    return plain(alts.flatMap((a) => (a.const !== undefined ? [a.const] : resolveRef(a).enum)));
  }
  return null;
}

function controlFor(item) {
  const s = resolveRef(item.schema);
  const v = valueAt(item.ptr);
  let choices = choicesOf(s);
  if (s.type === "boolean") {
    const c = el("input", "ui-check-input");
    c.type = "checkbox";
    c.checked = !!v;
    c.addEventListener("change", () => setSetting(item.ptr, c.checked));
    return c;
  }
  if (choices) {
    const sel = el("select", "ui-select");
    // The current value is offered even when the list lacks it (a theme
    // named in config.json that is not installed here).
    if (v !== undefined && v !== null && !choices.some((c) => JSON.stringify(c.value) === JSON.stringify(v))) {
      choices = [{ label: String(v), value: v }, ...choices];
    }
    for (const ch of choices) {
      const o = el("option", "", ch.label);
      o.value = JSON.stringify(ch.value);
      if (JSON.stringify(ch.value) === JSON.stringify(v ?? null)) o.selected = true;
      sel.append(o);
    }
    sel.addEventListener("change", () => setSetting(item.ptr, JSON.parse(sel.value)));
    return sel;
  }
  const types = [].concat(s.type || []);
  if (types.includes("integer") || types.includes("number")) {
    const n = el("input", "ui-field");
    n.type = "number";
    if (s.minimum !== undefined) n.min = s.minimum;
    if (s.maximum !== undefined) n.max = s.maximum;
    n.value = v ?? "";
    n.addEventListener("change", () => {
      if (n.value === "" && types.includes("null")) return setSetting(item.ptr, null);
      setSetting(item.ptr, types.includes("integer") ? parseInt(n.value, 10) : parseFloat(n.value));
    });
    return n;
  }
  if (types.includes("string") && types.length <= 2) {
    const t = el("input", "ui-field");
    t.value = v ?? "";
    t.addEventListener("change", () => setSetting(item.ptr, t.value === "" && types.includes("null") ? null : t.value));
    return t;
  }
  // Lists, maps and structured values: edited as JSON.
  const ta = el("textarea", "ui-field json");
  ta.spellcheck = false;
  ta.value = JSON.stringify(v ?? null, null, 2);
  ta.rows = Math.min(12, ta.value.split("\n").length);
  ta.addEventListener("change", () => {
    try { setSetting(item.ptr, JSON.parse(ta.value)); ta.classList.remove("invalid"); }
    catch (e) { ta.classList.add("invalid"); }
  });
  return ta;
}

function renderSettings() {
  if (!settingsUi.open || !settingsUi.schema) return;
  const cats = settingsCategories();
  if (!settingsUi.category || !cats.has(settingsUi.category)) settingsUi.category = cats.keys().next().value;
  const box = div("settings");
  box.setAttribute("role", "dialog");
  box.setAttribute("aria-label", "Settings");
  const head = div("settings-head");
  const search = el("input", "ui-field settings-search");
  search.placeholder = "Search settings";
  search.value = settingsUi.filter;
  search.addEventListener("input", () => { settingsUi.filter = search.value; renderSettings(); });
  const close = el("button", "btn", "Done");
  close.addEventListener("click", closeSettings);
  head.append(el("span", "settings-title", "Settings"), search, close);
  const nav = div("settings-nav");
  const shortcuts = el("button", "settings-cat link", "Keyboard Shortcuts…");
  shortcuts.addEventListener("click", () => { closeSettings(); openKeybindings(); });
  for (const name of cats.keys()) {
    const b = el("button", "settings-cat" + (name === settingsUi.category && !settingsUi.filter ? " active" : ""), name);
    b.addEventListener("click", () => { settingsUi.category = name; settingsUi.filter = ""; renderSettings(); });
    nav.append(b);
  }
  nav.append(shortcuts);
  const list = div("settings-list");
  const f = settingsUi.filter.trim().toLowerCase();
  const items = f
    ? [...cats.values()].flat().filter((i) => (i.key + " " + (resolveRef(i.schema).description || "")).toLowerCase().includes(f))
    : cats.get(settingsUi.category);
  let section = null;
  for (const item of items) {
    const s = item.schema;
    const sec = s["x-section"];
    if (sec && sec !== section) { section = sec; list.append(el("div", "settings-section", sec)); }
    const row = div("setting");
    const text = div("setting-text");
    text.append(el("div", "setting-name", prettyName(item.key)));
    const desc = resolveRef(s).description;
    if (desc) text.append(el("div", "setting-desc", desc));
    row.append(text, controlFor(item));
    list.append(row);
  }
  const bodyEl = div("settings-body");
  bodyEl.append(nav, list);
  box.append(head, bodyEl);
  const keepFocus = document.activeElement === document.querySelector(".settings-search");
  overlay("settings", scrim(box));
  box.addEventListener("keydown", (e) => { if (e.key === "Escape") { e.preventDefault(); closeSettings(); } });
  if (keepFocus || !f) search.focus();
}

api.on("settings", () => {
  // Redraw only when nothing is being typed into a field.
  const a = document.activeElement;
  if (settingsUi.open && !(a && a.closest && a.closest(".settings-list"))) renderSettings();
});
