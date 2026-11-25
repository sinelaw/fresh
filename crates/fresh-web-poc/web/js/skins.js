// Skins: how the whole client looks — the frame around the editor, the
// chrome's layout and density, and (through an editor theme) the buffers.
//
// A skin is a folder of plain web files the server serves at
// `skins/<id>/` (a built-in one) or `workspaces/<ws>/skins/<id>/` (one from
// the workspace's host; `src/skins.rs`): a `skin.css` whose header names it, and
// optionally a `furniture.html` of decorative markup and a `theme.json`.
// The client never runs anything a skin brings. Applying one:
//
//   - `<html>` carries `skin-<id>` for the skin and each skin it extends,
//     and `frame-full` or `frame-bezel`;
//   - each skin's stylesheet is linked after the page's own, base first, so
//     its relative `url()`s resolve inside its folder;
//   - its furniture is rebuilt inside `#device` from inert elements;
//   - `--ui-scale` is its density, and the editor's cell is measured again;
//   - when the skin is picked (not on every load), its editor theme becomes
//     the editor's theme, through the theme picker's own command.
//
// The choice is per browser (localStorage); `?skin=<id>` overrides it, to
// recover from a skin that hides the way back.

import { rpc } from "./rpc.js";
import { $ } from "./dom.js";

const KEY = "fresh-poc:skin";

// The skin chosen last in this browser.
/** function savedSkin() => String | Null */
function savedSkin() {
  try { return localStorage.getItem(KEY); } catch { return null; }
}
// What furniture may be built from: decoration, nothing interactive.
const TAGS = new Set(["div", "span", "i", "b", "em", "strong", "small", "p"]);
// The live values furniture can show, and how often they are refreshed.
const VALUES = ["cpu", "mem", "memText", "load", "uptime", "errors", "warnings", "buffers", "time"];
const HOST_VALUES = new Set(["cpu", "mem", "memText", "load", "uptime"]);
const REFRESH_MS = 3000;

// The skin `id` among `all`, if listed.
/** function skinIn(all: SkinInfo[], id: String) => SkinInfo | Null */
function skinIn(all, id) { return all.find((s) => s.id === id) ?? null; }
// Skin `id` and what it extends, the base first (a loop, or a chain past
// eight, is cut).
/** function chainOf(all: SkinInfo[], id: String, depth: Number) => SkinInfo[] */
function chainOf(all, id, depth) {
  const s = skinIn(all, id);
  if (s === null || depth >= 8) return [];
  const base = s.extends;
  const below = base === null ? [] : chainOf(all, base, depth + 1);
  return below.some((x) => x.id === s.id) ? below : [...below, s];
}
// What a chain of skins comes to: the last to set each wins (`base(id)`:
// where skin `id`'s files are served).
/** type ResolvedSkin = {id: String, name: String, chain: String[], frame: String, scale: Number, furniture: String | Null, theme: String | Null} */
/** function resolvedSkin(id: String, chain: SkinInfo[], base: (String) => String) => ResolvedSkin */
function resolvedSkin(id, chain, base) {
  const scales = chain.flatMap((s) => { const x = s.scale; return x === null ? [] : [x]; });
  const furniture = chain.flatMap((s) => { const x = s.furniture; return x === null ? [] : [`${base(s.id)}${x}`]; });
  const themes = chain.flatMap((s) => { const x = s.theme; return x === null ? [] : [x]; });
  return {
    id,
    name: chain.at(-1)?.name ?? id,
    chain: chain.map((s) => s.id),
    frame: chain.at(-1)?.frame ?? "full",
    scale: scales.at(-1) ?? 1,
    furniture: furniture.at(-1) ?? null,
    theme: themes.at(-1) ?? null,
  };
}
/** function noSkins() => SkinInfo[] */
function noSkins() { return []; }

export class Skins {
  constructor(app) {
    this.app = app;
    this.all = noSkins();
    this.current = null; // the resolved skin on screen
    this.timer = 0;
    this.applying = Promise.resolve(undefined); // the skins being put on, in order
  }

  // List the skins and put the chosen one on: before the first paint, so
  // the editor's cell is measured in the skin's font.
  async load() {
    $("#skin-btn").addEventListener("click", () => { this.pick(); });
    try { this.all = (await rpc("skin.list", {})).skins; } catch (e) { this.app().report(`Skins: ${String(e)}`); }
    const id = new URLSearchParams(location.search).get("skin") ?? savedSkin();
    await this.apply(id !== null && this.byId(id) !== null ? id : "default");
  }

  byId(id) { return skinIn(this.all, id); }

  // Where a skin's files are served: a built-in skin's beside the client's
  // own files, a user skin's through the workspace whose host has it.
  base(id) {
    const s = skinIn(this.all, id);
    return s !== null && s.source === "user" ? `workspaces/${this.app().workspace.id}/skins/${id}/` : `skins/${id}/`;
  }

  // A skin with what it extends folded in: the chain base first; the last
  // skin to set a frame, scale, furniture or theme wins.
  resolve(id) {
    return resolvedSkin(id, chainOf(this.all, id, 0), (x) => this.base(x));
  }

  // Put a skin on screen. One at a time, in order: a quick run through the
  // picker never ends with an earlier skin's furniture over a later one.
  apply(id) {
    this.applying = this.applying.then(async () => { await this.put(id); });
    return this.applying;
  }

  async put(id) {
    const sk = this.resolve(id);
    const html = document.documentElement;
    for (const c of [...html.classList]) if (c.startsWith("skin-") || c.startsWith("frame-")) html.classList.remove(c);
    for (const c of sk.chain) html.classList.add(`skin-${c}`);
    html.classList.add(`frame-${sk.frame}`);
    html.style.setProperty("--ui-scale", String(sk.scale));
    // The skin's stylesheets replace the last skin's once they have loaded,
    // so the page never shows between two skins.
    const old = [...document.querySelectorAll("link[data-skin]")];
    const anchor = old[0] || null;
    await Promise.all(sk.chain.map((c) => new Promise((resolve) => {
      const l = document.createElement("link");
      l.rel = "stylesheet";
      l.href = `${this.base(c)}skin.css`;
      l.dataset.skin = c;
      l.onload = l.onerror = resolve;
      document.head.insertBefore(l, anchor);
    })));
    for (const l of old) l.remove();
    await this.furnish(sk.furniture);
    this.current = sk;
    const btn = document.getElementById("skin-btn");
    if (btn) btn.textContent = sk.name;
    // Fonts the skin brings change the cell only once they are in.
    await document.fonts.ready;
    this.app().remeasure?.();
    this.refresh();
  }

  // Build the furniture inside #device from inert elements: only TAGS,
  // their class, data-bind / data-bind-var, and text.
  async furnish(url) {
    const host = document.getElementById("device");
    host.textContent = "";
    if (!url) return;
    let text = "";
    try { const r = await fetch(url); if (r.ok) text = await r.text(); } catch {}
    const doc = new DOMParser().parseFromString(`<body>${text}</body>`, "text/html");
    let budget = 400;
    const copy = (nodes, parent) => {
      for (const n of nodes) {
        if (budget-- <= 0) return;
        if (n.nodeType === Node.TEXT_NODE) {
          if (n.textContent.trim()) parent.appendChild(document.createTextNode(n.textContent));
        } else if (n.nodeType === Node.ELEMENT_NODE && TAGS.has(n.tagName.toLowerCase())) {
          const e = document.createElement(n.tagName.toLowerCase());
          const cls = n.getAttribute("class");
          if (cls) e.className = cls;
          const bind = n.getAttribute("data-bind");
          if (VALUES.includes(bind)) e.dataset.bind = bind;
          const bindVar = /^(--[a-z0-9-]+):([A-Za-z]+)$/.exec(n.getAttribute("data-bind-var") || "");
          if (bindVar && VALUES.includes(bindVar[2])) e.dataset.bindVar = bindVar[0];
          copy(n.childNodes, e);
          parent.appendChild(e);
        }
      }
    };
    copy(doc.body.childNodes, host);
  }

  // Live values for the furniture: the client's own counts, and the host's
  // load while some furniture shows it.
  async refresh() {
    clearTimeout(this.timer);
    const bound = [...document.querySelectorAll("#device [data-bind], #device [data-bind-var]")];
    if (!bound.length) return;
    const names = bound.map((e) => e.dataset.bind || e.dataset.bindVar.split(":")[1]);
    const v = this.localValues();
    if (names.some((n) => HOST_VALUES.has(n))) {
      try { Object.assign(v, await rpc("host.stats", {})); } catch {}
    }
    for (const e of bound) {
      if (e.dataset.bind && v[e.dataset.bind] != null) e.textContent = String(v[e.dataset.bind]);
      if (e.dataset.bindVar) {
        const [prop, name] = e.dataset.bindVar.split(":");
        if (v[name] != null) e.style.setProperty(prop, String(v[name]));
      }
    }
    this.timer = setTimeout(() => this.refresh(), REFRESH_MS);
  }

  localValues() {
    const bufs = [...this.app().buffers.values()];
    const diags = bufs.flatMap((b) => b.decor?.diags || []);
    const d = new Date();
    return {
      buffers: bufs.filter((b) => !b.isVirtual).length,
      errors: diags.filter((x) => x.severity === 1).length,
      warnings: diags.filter((x) => x.severity === 2).length,
      time: `${String(d.getHours()).padStart(2, "0")}:${String(d.getMinutes()).padStart(2, "0")}`,
    };
  }

  // Put a skin on and remember it; its editor theme comes with it.
  async choose(id) {
    if (!this.byId(id)) return;
    try { localStorage.setItem(KEY, id); } catch {}
    await this.apply(id);
    if (this.current.theme && this.app().commands.has("theme.use")) await this.app().run("theme.use", this.current.theme);
    this.app().setStatus(`Skin: ${this.current.name}`);
  }

  // The picker: moving through the list tries each skin on; Escape puts
  // the one before back. The list is read again, so a skin just added to
  // the skins folder is there.
  async pick() {
    try { this.all = (await rpc("skin.list", {})).skins; } catch (e) { this.app().report(`Skins: ${e.message}`); }
    const before = this.current?.id || "default";
    this.app().prompt.open({
      title: "Skin",
      value: "",
      items: this.all.map((s) => ({
        label: s.name,
        detail: [s.id === before ? "current" : "", s.description, s.source === "user" ? "skins folder" : ""].filter(Boolean).join(" · "),
        value: s.id,
      })),
      onChange: (it) => { this.apply(it.value); },
      onAccept: (it) => { this.choose(it.value); },
      onCancel: () => { this.apply(before); },
    });
  }

  // The next (or previous) skin in the list.
  cycle(dir = 1) {
    if (!this.all.length) return;
    const i = this.all.findIndex((s) => s.id === this.current?.id);
    this.choose(this.all[(i + dir + this.all.length) % this.all.length].id);
  }
}
