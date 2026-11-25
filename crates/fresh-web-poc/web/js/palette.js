// The palette: one input over a list, in four modes picked by prefix, as in
// the editor —
//   (none) files     fuzzy file finder (server-side), `path:line:col` jumps
//   >      commands  every registered command, with its key
//   #      buffers   open buffers by name
//   :      line      go to a line, previewed as you type
//   @      symbols   (and any prefix a plugin provides: `provide`) the
//                    selected item previewed, Escape puts the view back
// — and the general pick-from-a-list / ask-for-text prompts plugins use.

import { rpc } from "./rpc.js";

import { $ } from "./dom.js";

// A palette row that runs `run` when chosen (none: a note).
/** function paletteItem(label: String, detail: String | Null, run: (() => Undefined) | Null) => PaletteItem */
function paletteItem(label, detail, run) { return { label, detail, run, preview: null }; }

// Run a palette row's action, or its preview: whether it has one.
/** function ran(it: PaletteItem | Undefined) => Boolean */
function ran(it) {
  const run = it === undefined ? null : it.run;
  if (run === null) return false;
  run();
  return true;
}
/** function previewed(it: PaletteItem | Undefined) => Boolean */
function previewed(it) {
  const preview = it === undefined ? null : it.preview;
  if (preview === null) return false;
  preview();
  return true;
}

export class Palette {
  constructor(app) {
    this.app = app;
    this.el = $("#prompt");
    this.label = this.el.querySelector(".label");
    this.input = this.el.querySelector("input");
    this.list = this.el.querySelector(".list");
    this.hint = this.el.querySelector(".hint");
    this.items = [];
    this.sel = 0;
    this.spec = null;
    this.seq = 0;
    this.providers = new Map(); // prefix -> {hint, items(query)}
  }

  // A mode for prefix `prefix` (one character): `items(query)` resolves
  // with [{label, detail, run, preview?}]; `preview()` runs as the item is
  // selected (Escape puts the view back where it was).
  provide(prefix, o) { this.providers.set(prefix, o); }

  // Take the input's events (once, after `new`).
  listen() {
    this.input.addEventListener("input", () => this.changed());
    this.input.addEventListener("keydown", (e) => this.key(e));
    this.input.addEventListener("blur", () => setTimeout(() => { if (this.spec && document.activeElement !== this.input) this.cancel(); }, 150));
  }

  get isOpen() { return !!this.spec; }

  // The palette, in the mode `prefix` selects.
  palette(prefix) {
    const v = this.app().view();
    const origin = v ? { v, pos: v.primary.pos, top: v.top } : null;
    let moved = false;
    this.show({
      label: "",
      value: prefix,
      hint: ["files · > commands · # buffers · : go to line", ...[...this.providers].map(([k, o]) => `${k} ${o.hint}`)].join(" · "),
      source: (q) => this.paletteItems(q),
      onChange: (q) => {
        // Preview a `:N` jump; Escape puts the view back.
        const m = /^:(\d+)/.exec(q);
        if (m && origin) this.app().gotoLine(parseInt(m[1], 10) - 1, 0, { history: false });
      },
      onAccept: (it, q) => {
        // The jump is remembered from where the palette opened, not from
        // the preview.
        if (/^:\d/.test(q) && origin) this.app().pushHistory(origin.v, origin.pos);
        if (!ran(it)) this.openTyped(q);
      },
      // A provider's item previews where it goes.
      onSelect: (it) => { if (previewed(it)) moved = true; },
      onCancel: () => { if (origin && (moved || /^:\d/.test(this.input.value))) { origin.v.setTop(origin.top); this.app().editing.place(origin.v.pane, origin.pos, false); } },
    });
  }

  async paletteItems(q) {
    const app = this.app();
    if (q.startsWith(">")) {
      const terms = q.slice(1).toLowerCase().split(/\s+/).filter(Boolean);
      return [...app.commands]
        .filter(([, c]) => !c.hidden && terms.every((t) => c.title.toLowerCase().includes(t)))
        .map(([id, c]) => paletteItem(c.title, [app.keyFor(id), c.source !== "core" ? c.source : ""].filter(Boolean).join(" · "), () => { app.run(id); }))
        .sort((a, b) => a.label.localeCompare(b.label));
    }
    if (q.startsWith("#")) {
      const t = q.slice(1).toLowerCase();
      return [...app.buffers.values()]
        .filter((b) => (b.path || b.name).toLowerCase().includes(t))
        .map((b) => paletteItem(`${b.modified ? "● " : ""}${b.name}`, b.path ? b.path.replace(app.root + "/", "") : b.doc ? "unsaved" : "virtual", () => { app.showBuffer(b, "tab", { preview: null, focus: true }); }));
    }
    const pr = this.providers.get(q[0]);
    if (pr) return pr.items(q.slice(1).trim());
    if (q.startsWith(":")) {
      const n = parseInt(q.slice(1), 10);
      return [n > 0 ? paletteItem(`Go to line ${n}`, null, () => { app.gotoLine(n - 1, 0, { history: false }); }) : paletteItem("Type a line number", null, null)];
    }
    const m = /^(.*?)(?::(\d+)(?::(\d+))?)?$/.exec(q);
    // Nothing typed: the files opened lately.
    if (!q.trim()) {
      const rel = (p) => (p.startsWith(app.root + "/") ? p.slice(app.root.length + 1) : p);
      return app.recentFiles().map((p) => paletteItem(rel(p), "recent", () => { app.openFile(p, { line: null, column: 1, place: "tab", preview: null, focus: true }); }));
    }
    if (!m) return [];
    const files = (await rpc("fs.find", { query: m[1] })).files;
    return files.map((f) => paletteItem(f, null, () => {
      app.openFile(f, { line: m[2] ? Number(m[2]) : null, column: m[3] ? Number(m[3]) : 1, place: "tab", preview: null, focus: true });
    }));
  }

  openTyped(q) {
    const m = /^(.+?)(?::(\d+)(?::(\d+))?)?$/.exec(q.trim());
    if (m && !/^[>#:]/.test(q) && !this.providers.has(q[0])) this.app().openFile(m[1], { line: m[2] ? Number(m[2]) : null, column: m[3] ? Number(m[3]) : 1, place: "tab", preview: null, focus: true });
  }

  // Pick from a list: {title, items: [{label, detail, value}], onAccept(item), onChange(item)}
  open(o) {
    this.show({
      label: o.title || "",
      value: o.value || "",
      // Items holding every typed word; those whose label starts with the
      // first word come first, each group in the given order.
      source: (q) => {
        const t = q.toLowerCase().split(/\s+/).filter(Boolean);
        const hits = (o.items || []).filter((it) => t.every((x) => `${it.label} ${it.detail || ""}`.toLowerCase().includes(x)));
        if (!t.length) return hits;
        const lead = (it) => (it.label.toLowerCase().startsWith(t[0]) ? 0 : 1);
        return hits.map((it, i) => ({ lead: lead(it), i, it })).sort((a, b) => a.lead - b.lead || a.i - b.i).map((x) => x.it);
      },
      onSelect: o.onChange,
      // Enter with nothing left to pick is a cancel.
      onAccept: (it) => (it ? o.onAccept?.(it) : o.onCancel?.()),
      onCancel: o.onCancel,
    });
  }

  // Ask for text: resolves with it, or null when cancelled.
  ask(o) {
    return new Promise((resolve) => {
      this.show({
        label: o.title || "",
        value: o.value || "",
        hint: o.hint || "",
        source: null,
        onAccept: (_it, q) => resolve(q),
        onCancel: () => resolve(null),
      });
      if (o.select !== false) this.input.select();
    });
  }

  show(spec) {
    if (this.spec) this.cancel();
    this.spec = spec;
    this.label.textContent = spec.label;
    this.hint.textContent = spec.hint || "";
    this.input.value = spec.value;
    this.items = [];
    this.sel = 0;
    this.el.classList.remove("hidden");
    this.input.focus();
    this.changed();
  }

  async changed() {
    const s = this.spec;
    if (!s) return;
    const q = this.input.value;
    s.onChange?.(q);
    if (!s.source) { this.items = []; this.render(); return; }
    const seq = ++this.seq;
    const r = await s.source(q);
    if (this.spec !== s || seq !== this.seq) return;
    this.items = r || [];
    this.sel = 0;
    this.render();
    if (this.items[0]) s.onSelect?.(this.items[0]);
  }

  render() {
    this.list.innerHTML = "";
    this.items.slice(0, 300).forEach((it, i) => {
      const d = document.createElement("div");
      d.className = "item" + (i === this.sel ? " sel" : "");
      d.innerHTML = `<span class="t"></span><span class="d"></span>`;
      d.querySelector(".t").textContent = it.label;
      d.querySelector(".d").textContent = it.detail || "";
      d.onmousedown = (e) => { e.preventDefault(); this.sel = i; this.accept(); };
      this.list.appendChild(d);
    });
    this.list.querySelector(".sel")?.scrollIntoView({ block: "nearest" });
  }

  key(e) {
    const move = (d) => {
      this.sel = Math.max(0, Math.min(this.items.length - 1, this.sel + d));
      this.render();
      this.spec?.onSelect?.(this.items[this.sel]);
    };
    if (e.key === "Escape") { e.preventDefault(); this.cancel(); }
    else if (e.key === "Enter") { e.preventDefault(); this.accept(); }
    else if (e.key === "ArrowDown") { e.preventDefault(); move(1); }
    else if (e.key === "ArrowUp") { e.preventDefault(); move(-1); }
    else if (e.key === "PageDown") { e.preventDefault(); move(10); }
    else if (e.key === "PageUp") { e.preventDefault(); move(-10); }
    else if (e.key === "Tab" && this.items[this.sel]) {
      // Complete to the top suggestion (a path a folder at a time).
      e.preventDefault();
      const it = this.items[this.sel];
      const q = this.input.value;
      if (!/^[>#:]/.test(q) && it.label) {
        const common = commonPrefix(this.items.map((x) => x.label));
        this.input.value = common.length > q.length ? common : it.label;
        this.changed();
      }
    }
  }

  close() {
    this.spec = null;
    this.el.classList.add("hidden");
    this.app().focusPane(this.app().activePane);
  }

  accept() {
    const s = this.spec;
    if (!s) return;
    const item = this.items[this.sel];
    const q = this.input.value;
    this.close();
    s.onAccept?.(item, q);
  }

  cancel() {
    const s = this.spec;
    this.close();
    s?.onCancel?.();
  }
}

function commonPrefix(list) {
  if (!list.length) return "";
  let p = list[0];
  for (const s of list) while (!s.startsWith(p)) p = p.slice(0, -1);
  return p;
}
