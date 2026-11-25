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

import { $, escapeHtml } from "./dom.js";
import { some, none } from "./maybe.js";

// A palette row that runs `run` when chosen (none: a note).
/** function paletteItem(label: String, detail: String | Null, run: (() => Undefined) | Null) => PaletteItem */
function paletteItem(label, detail, run) { return { label, detail, run, preview: null }; }

// What the prompt shows and does: its label, first text and hint; where
// its rows come from as the text changes (none: it asks for text); and
// what runs as the text changes, as a row is selected, on Enter (the
// row, if any, and the text) and when it is given up. A prompt asking for
// text has no rows.
/** type PromptSpec = {
 *   label: String,
 *   value: String,
 *   hint: String,
 *   source: (String) => Promise<PaletteItem[]>,
 *   onChange: (String) => Undefined,
 *   onSelect: (PaletteItem | Undefined) => Undefined,
 *   onAccept: (PaletteItem | Undefined, String) => Undefined,
 *   onCancel: () => Undefined,
 * } */
/** function promptSpec(label: String, value: String, hint: String, source: (String) => Promise<PaletteItem[]>, onChange: (String) => Undefined, onSelect: (PaletteItem | Undefined) => Undefined, onAccept: (PaletteItem | Undefined, String) => Undefined, onCancel: () => Undefined) => PromptSpec */
function promptSpec(label, value, hint, source, onChange, onSelect, onAccept, onCancel) {
  return { label, value, hint, source, onChange, onSelect, onAccept, onCancel };
}
/** function noSpec() => PromptSpec[] */
function noSpec() { return []; }
/** function noItems() => PaletteItem[] */
function noItems() { return []; }

// A palette mode (`fresh.paletteMode`): its hint, and its rows for the
// text after its prefix.
/** type PaletteMode = {hint: String, items: (String) => Promise<PaletteItem[]>} */
/** function modeList(m: PaletteMode | Undefined) => PaletteMode[] */
function modeList(m) { return m === undefined ? [] : [m]; }
/** function noModes() => Map<String, PaletteMode> */
function noModes() { return new Map(); }

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

// Items holding every word of `q`; those whose label starts with the
// first word come first, each group in the given order.
/** function matching(items: PaletteItem[], q: String) => PaletteItem[] */
function matching(items, q) {
  const t = q.toLowerCase().split(/\s+/).filter(Boolean);
  const hits = items.filter((it) => t.every((x) => `${it.label} ${it.detail ?? ""}`.toLowerCase().includes(x)));
  if (!t.length) return hits;
  const lead = (it) => (it.label.toLowerCase().startsWith(t[0]) ? 0 : 1);
  return hits.map((it, i) => ({ lead: lead(it), i, it })).sort((a, b) => a.lead - b.lead || a.i - b.i).map((x) => x.it);
}

// `path` from `root` (as it is if not under it).
/** function relative(root: String, path: String) => String */
function relative(root, path) { return path.startsWith(root + "/") ? path.slice(root.length + 1) : path; }

/** function commonPrefix(list: String[]) => String */
function commonPrefix(list) {
  let p = list.at(0) ?? "";
  for (const s of list) while (!s.startsWith(p)) p = p.slice(0, -1);
  return p;
}

export class Palette {
  // `editor` (main.js): where the active view is (to put it back), and the
  // commands, buffers, recent files and files the palette lists.
  constructor(editor) {
    this.editor = editor;
    this.el = $("#prompt");
    this.label = $("#prompt .label");
    this.input = $("#prompt input");
    this.list = $("#prompt .list");
    this.hint = $("#prompt .hint");
    this.items = noItems();
    this.sel = 0;
    this.specs = noSpec(); // the prompt open: a list of at most one
    this.seq = 0;
    this.providers = noModes(); // a palette mode by its prefix
  }

  // A mode for prefix `prefix` (one character): `items(query)` resolves
  // with PaletteItems; an item's `preview()` runs as it is selected
  // (Escape puts the view back where it was).
  provide(prefix, o) { this.providers.set(prefix, o); }

  // Take the input's events (once, after `new`).
  listen() {
    this.input.addEventListener("input", () => { this.changed(); });
    this.input.addEventListener("keydown", (e) => { this.key(e); });
    this.input.addEventListener("blur", () => { setTimeout(() => { if (this.isOpen && document.activeElement !== this.input) this.cancel(); }, 150); });
  }

  get isOpen() { return this.specs.length > 0; }

  // The palette, in the mode `prefix` selects.
  palette(prefix) {
    const origin = this.editor.here();
    let moved = false;
    /** const modes: String[] */
    const modes = [];
    this.providers.forEach((o, k) => { modes.push(`${k} ${o.hint}`); });
    const hint = ["files · > commands · # buffers · : go to line", ...modes].join(" · ");
    this.show(promptSpec("", prefix, hint, (q) => this.paletteItems(q),
      (q) => {
        // Preview a `:N` jump; Escape puts the view back.
        const m = /^:(\d+)/.exec(q);
        if (m !== null) this.editor.goto(parseInt(m[1], 10) - 1);
      },
      // A provider's item previews where it goes.
      (it) => { if (previewed(it)) moved = true; },
      (it, q) => {
        // The jump is remembered from where the palette opened, not from
        // the preview.
        if (/^:\d/.test(q)) for (const o of origin) o.remember();
        if (!ran(it)) this.openTyped(q);
      },
      () => {
        if (!moved && !/^:\d/.test(this.input.value)) return;
        for (const o of origin) o.restore();
      }));
  }

  async paletteItems(q) {
    const ed = this.editor;
    if (q.startsWith(">")) {
      const terms = q.slice(1).toLowerCase().split(/\s+/).filter(Boolean);
      return ed.commands()
        .filter((c) => terms.every((t) => c.title.toLowerCase().includes(t)))
        .map((c) => paletteItem(c.title, [c.key, c.source !== "core" ? c.source : ""].filter(Boolean).join(" · "), c.run))
        .sort((a, b) => a.label.localeCompare(b.label));
    }
    if (q.startsWith("#")) {
      const t = q.slice(1).toLowerCase();
      return ed.buffers()
        .filter((b) => (b.path || b.name).toLowerCase().includes(t))
        .map((b) => paletteItem(`${b.modified ? "● " : ""}${b.name}`, b.path ? relative(ed.root(), b.path) : b.stored ? "unsaved" : "virtual", b.show));
    }
    const pr = this.providers.get(q[0]);
    for (const m of modeList(pr)) return await m.items(q.slice(1).trim());
    if (q.startsWith(":")) {
      const n = parseInt(q.slice(1), 10);
      return [n > 0 ? paletteItem(`Go to line ${n}`, null, () => { ed.goto(n - 1); }) : paletteItem("Type a line number", null, null)];
    }
    const m = /^(.*?)(?::(\d+)(?::(\d+))?)?$/.exec(q);
    // Nothing typed: the files opened lately.
    if (!q.trim()) {
      return ed.recent().map((p) => paletteItem(relative(ed.root(), p), "recent", () => { ed.open(p); }));
    }
    if (!m) return [];
    const files = (await rpc("fs.find", { query: m[1] })).files;
    return files.map((f) => paletteItem(f, null, () => { this.openAt(f, m[2], m[3]); }));
  }

  openTyped(q) {
    const m = /^(.+?)(?::(\d+)(?::(\d+))?)?$/.exec(q.trim());
    if (m && !/^[>#:]/.test(q) && !this.providers.has(q[0])) this.openAt(m[1], m[2], m[3]);
  }
  // Open `path` at `line` and `column` as typed after it (`path:line:col`;
  // either may be missing).
  openAt(path, line, column) {
    if (line) this.editor.openAt(path, Number(line), column ? Number(column) : 1);
    else this.editor.open(path);
  }

  // Pick from a list (`o`: a PickOptions, types/fresh.d.js). Enter with
  // nothing left to pick is a cancel.
  open(o) {
    const rows = o.items.map((it) => ({ label: it.label, detail: it.detail, run: () => { o.onAccept(it); }, preview: () => { o.onChange(it); } }));
    this.show(promptSpec(o.title, o.value, "", async (q) => matching(rows, q), (_q) => {},
      (it) => { previewed(it); },
      (it, _q) => { if (!ran(it)) o.onCancel(); },
      () => { o.onCancel(); }));
  }

  // Ask for text (`o`: an AskOptions): resolves with it, or null when
  // cancelled.
  ask(o) {
    return new Promise((resolve) => {
      this.show(promptSpec(o.title, o.value, o.hint, async (_q) => noItems(), (_q) => {}, (_it) => {}, (_it, q) => { resolve(some(q)); }, () => { resolve(none()); }));
      if (o.select) this.input.select();
    });
  }

  show(spec) {
    if (this.isOpen) this.cancel();
    this.specs = [spec];
    this.label.textContent = spec.label;
    this.hint.textContent = spec.hint;
    this.input.value = spec.value;
    this.items = [];
    this.sel = 0;
    this.el.classList.remove("hidden");
    this.input.focus();
    this.changed();
  }

  async changed() {
    for (const s of this.specs) {
      const q = this.input.value;
      s.onChange(q);
      const seq = ++this.seq;
      const r = await s.source(q);
      if (!this.specs.includes(s) || seq !== this.seq) return;
      this.items = r;
      this.sel = 0;
      this.render();
      if (r.length) s.onSelect(r[0]);
    }
  }

  render() {
    this.list.innerHTML = "";
    this.items.slice(0, 300).forEach((it, i) => {
      const d = document.createElement("div");
      d.className = "item" + (i === this.sel ? " sel" : "");
      d.innerHTML = `<span class="t">${escapeHtml(it.label)}</span><span class="d">${escapeHtml(it.detail ?? "")}</span>`;
      d.onmousedown = (e) => { e.preventDefault(); this.sel = i; this.accept(); };
      this.list.appendChild(d);
    });
    const sel = this.list.querySelector(".sel");
    if (sel !== null) sel.scrollIntoView({ block: "nearest" });
  }

  key(e) {
    const move = (d) => {
      this.sel = Math.max(0, Math.min(this.items.length - 1, this.sel + d));
      this.render();
      for (const s of this.specs) s.onSelect(this.items[this.sel]);
    };
    if (e.key === "Escape") { e.preventDefault(); this.cancel(); }
    else if (e.key === "Enter") { e.preventDefault(); this.accept(); }
    else if (e.key === "ArrowDown") { e.preventDefault(); move(1); }
    else if (e.key === "ArrowUp") { e.preventDefault(); move(-1); }
    else if (e.key === "PageDown") { e.preventDefault(); move(10); }
    else if (e.key === "PageUp") { e.preventDefault(); move(-10); }
    else if (e.key === "Tab" && this.sel < this.items.length) {
      // Complete to the top suggestion (a path a folder at a time).
      e.preventDefault();
      const it = this.items[this.sel];
      const q = this.input.value;
      if (!/^[>#:]/.test(q) && it.label !== "") {
        const common = commonPrefix(this.items.map((x) => x.label));
        this.input.value = common.length > q.length ? common : it.label;
        this.changed();
      }
    }
  }

  close() {
    this.specs = [];
    this.el.classList.add("hidden");
    this.editor.focus();
  }

  accept() {
    const was = this.specs;
    const item = this.items.at(this.sel);
    const q = this.input.value;
    this.close();
    for (const s of was) s.onAccept(item, q);
  }

  cancel() {
    const was = this.specs;
    this.close();
    for (const s of was) s.onCancel();
  }
}
