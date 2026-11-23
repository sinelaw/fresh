// Skins: the client's look, chosen per browser (docs/features/web-ui-skins.md).
//
// A skin is a folder of ordinary web files:
//   skin.css        a stylesheet that starts with a `==FreshSkin==` header
//                   (@id, @name, @extends, @frame, @scale, @furniture,
//                   @editor-theme, @explorer-side). Colours are custom
//                   properties on `:root.theme-<id>`; anything else is CSS
//                   scoped under `.theme-<id>`. Images and fonts by url(…).
//   furniture.html  optional decorative markup, built inside #device.
//   theme.json      optional editor theme in Fresh's own format.
// The client lays itself out, so a skin moves and sizes anything with plain
// CSS. The core takes part in two places only: it serves the skins folder,
// and it installs a skin's editor theme (`skin.theme`), because the editor
// theme is the editor's shared, saved setting. It also sends the live values
// furniture binds to (`skinValues`).
//
// Skins come from three places, read the same way:
//   built-in  web-ui/skins/NN-id/, bundled by build.rs;
//   folder    the server's skins directory, at /skins/…, polled — saving a
//             file re-applies it (the skin author's loop);
//   browser   installed from the picker (URL, folder, files), kept in
//             localStorage.
// No skin runs script: its CSS is set as text and its furniture is rebuilt
// from a few inert elements.

const SKIN_ID_RE = /^[a-z0-9]([a-z0-9-]{0,46}[a-z0-9])?$/;
const SKIN_TAGS = ["div", "span", "i", "b", "em", "strong", "small", "p"];
const SKIN_MAX_CSS = 2000000;
const SKIN_MAX_ASSET = 2000000;
const SKIN_STORE_KEY = "fresh.skins";
const SKIN_FOLDER_CACHE_KEY = "fresh.skins.folders";
const SKIN_PICK_KEY = "fresh.webtheme";
const SKIN_KEEP_THEME_KEY = "fresh.skins.keepEditorTheme";
const SKIN_BIND_RE = /^[A-Za-z0-9_.-]+:[A-Za-z0-9_.-]+$/;
const SKIN_BIND_VAR_RE = /^--[A-Za-z0-9_-]+:[A-Za-z0-9_.-]+:[A-Za-z0-9_.-]+$/;
const MOBILE = window.matchMedia("(max-width: 760px)");

/** The built-in skin folders, spliced in by build.rs. */
const BUILTIN_SKIN_DIRS = /*@SKINS@*/[];

// ---- reading a skin -----------------------------------------------------------

/** The `/* ==FreshSkin== … ==/FreshSkin== *\/` header: `@key value` per line. */
function skinHeader(css) {
  const m = /\/\*\s*==FreshSkin==([\s\S]*?)==\/FreshSkin==\s*\*\//.exec(css);
  if (!m) return null;
  const meta = {};
  for (const line of m[1].split("\n")) {
    const kv = /^\s*@([a-z-]+)\s+(.*?)\s*$/.exec(line);
    if (kv) meta[kv[1]] = kv[2];
  }
  return meta;
}

/** A plain relative path inside a skin folder ("furniture.html", "img/a.png"). */
function plainSkinPath(p) {
  return p.split("/").every((s) => /^[A-Za-z0-9._-]+$/.test(s) && s !== "." && s !== "..");
}

/** The file a skin's editor theme is in: `@editor-theme <file>.json`, else
 *  `theme.json`; "" when the header names an installed theme instead. */
function skinThemeFile(meta) {
  const v = meta ? (meta["editor-theme"] || "").trim() : "";
  if (!v) return "theme.json";
  return /\.json$/i.test(v) && plainSkinPath(v) ? v : "";
}

/** {css, furniture, theme} texts → {skin} or {error}. */
function parseSkin(files, fallbackId, origin) {
  const fail = (error) => ({ skin: null, error });
  const css = files.css;
  if (css.length > SKIN_MAX_CSS) return fail("skin.css is larger than 2 MB");
  const meta = skinHeader(css);
  if (!meta) return fail("skin.css must start with a /* ==FreshSkin== … ==/FreshSkin== */ header");
  const get = (k) => (meta[k] || "").slice(0, 200);
  const id = get("id") || fallbackId;
  if (!SKIN_ID_RE.test(id)) return fail(`"${id}" is not a usable skin id — set @id to 1–48 characters of a–z, 0–9 and "-"`);
  const name = get("name");
  if (!name) return fail("the header needs an @name");
  const parent = get("extends");
  if (parent && (!SKIN_ID_RE.test(parent) || parent === id)) return fail("@extends must name another skin's id");
  const frame = get("frame");
  if (frame && frame !== "bezel" && frame !== "full") return fail('@frame must be "bezel" or "full"');
  const scale = get("scale") ? parseFloat(get("scale")) : 0;
  if (scale !== 0 && !(scale >= 0.5 && scale <= 1.5)) return fail("@scale must be a number from 0.5 to 1.5");
  const furnitureFile = get("furniture");
  if (furnitureFile && !plainSkinPath(furnitureFile)) return fail("@furniture must be a relative path inside the skin folder");
  const explorerSide = get("explorer-side");
  if (explorerSide && explorerSide !== "left" && explorerSide !== "right") return fail('@explorer-side must be "left" or "right"');
  const keys = get("keys");
  if (keys && keys !== "words" && keys !== "symbols") return fail('@keys must be "words" or "symbols"');
  const themeRef = get("editor-theme");
  const editorThemeName = themeRef && !/\.json$/i.test(themeRef) ? themeRef : "";
  const editorTheme = files.theme.trim();
  if (editorTheme) {
    if (editorTheme.length > 512000) return fail("the editor theme is larger than 500 KB");
    let v;
    try { v = JSON.parse(editorTheme); } catch (e) { return fail(`the editor theme is not valid JSON (${String(e).replace(/^SyntaxError: /, "")})`); }
    if (!v || typeof v !== "object" || Array.isArray(v)) return fail("the editor theme must be a JSON object (Fresh's theme format)");
  }
  return { skin: {
    id, name, description: get("description"), author: get("author"), version: get("version"),
    parent, frame, scale, furnitureFile, explorerSide, keys, css, furniture: files.furniture.slice(0, 200000),
    editorTheme, editorThemeName, origin, source: "", stamp: "",
  }, error: "" };
}

/** Rewrite every relative `url(…)` and `@import "…"` of a stylesheet with
 *  `map` (null leaves it). Absolute, root, fragment and data: URLs are not relative. */
function rewriteCssUrls(css, map) {
  let out = "", i = 0;
  const isRel = (u) => !!u && !/^([A-Za-z][A-Za-z0-9+.-]*:|\/|#)/.test(u);
  for (;;) {
    const a = css.indexOf("url(", i);
    const b = css.indexOf("@import", i);
    if (a < 0 && b < 0) break;
    if (a >= 0 && (b < 0 || a < b)) {
      const close = css.indexOf(")", a);
      if (close < 0) break;
      let u = css.slice(a + 4, close).trim();
      if (u.startsWith('"') || u.startsWith("'")) u = u.slice(1, -1);
      const to = isRel(u) ? map(u) : null;
      out += css.slice(i, a) + (to === null ? css.slice(a, close + 1) : `url("${to.split('"').join("%22")}")`);
      i = close + 1;
    } else {
      const m = /^@import\s+(["'])([^"']*)\1/.exec(css.slice(b, b + 2048));
      if (!m) { out += css.slice(i, b + 7); i = b + 7; continue; }
      const to = isRel(m[2]) ? map(m[2]) : null;
      out += css.slice(i, b) + (to === null ? m[0] : `@import "${to.split('"').join("%22")}"`);
      i = b + m[0].length;
    }
  }
  return out + css.slice(i);
}
function cssAgainst(css, base) {
  return rewriteCssUrls(css, (u) => { try { return new URL(u, base).href; } catch (_) { return null; } });
}

// ---- the registry ---------------------------------------------------------------

const skinState = {
  all: [],          // every known skin, built-in first
  folder: [],       // the server's skins folder, as last seen
  rev: 0,           // bumped when the registry changes
  current: "cosmos",
  wanted: "cosmos", // the remembered pick, until the folder that has it is read
  picked: false,    // the user picked since the page loaded
  resolved: null,
  menuOpen: false, urlOpen: false, urlDraft: "", pendingRemove: "", busy: false,
  note: { text: "", error: false },
};

function storedSkins(key) {
  const out = [];
  try {
    const text = localStorage.getItem(key);
    if (text === null) return out;
    for (const e of JSON.parse(text).skins || []) {
      const r = parseSkin({ css: e.css || "", furniture: e.furniture || "", theme: e.theme || "" },
        e.id || "", key === SKIN_STORE_KEY ? "browser" : "folder");
      if (!r.skin) { console.warn(`fresh: a stored skin was skipped: ${r.error}`); continue; }
      r.skin.source = e.source || "";
      r.skin.stamp = e.stamp || "";
      out.push(r.skin);
    }
  } catch (_) { console.warn("fresh: stored skins could not be read"); }
  return out;
}
function storeSkins(key, list) {
  const rows = list.map((s) => ({ id: s.id, css: s.css, furniture: s.furniture, theme: s.editorTheme, source: s.source, stamp: s.stamp }));
  try { localStorage.setItem(key, JSON.stringify({ v: 1, skins: rows })); return ""; }
  catch (_) { return "the browser refused to store the skin (storage full or disabled; browser storage holds about 5 MB)"; }
}

function rebuildSkins() {
  const out = [];
  for (const d of BUILTIN_SKIN_DIRS) {
    const css = d.files["skin.css"] || "";
    const tf = skinThemeFile(skinHeader(css));
    const r = parseSkin({ css, furniture: "", theme: tf ? (d.files[tf] || "") : "" }, d.dir.replace(/^[0-9]+-/, ""), "builtin");
    if (!r.skin) { console.error(`fresh: built-in skin ${d.dir} rejected: ${r.error}`); continue; }
    if (r.skin.furnitureFile) r.skin.furniture = d.files[r.skin.furnitureFile] || "";
    out.push(r.skin);
  }
  for (const sk of skinState.folder.concat(storedSkins(SKIN_STORE_KEY))) {
    if (out.some((o) => o.id === sk.id)) { console.warn(`fresh: skin "${sk.id}" is shadowed by another with the same id`); continue; }
    out.push(sk);
  }
  skinState.all = out;
  skinState.rev++;
  skinState.resolved = null;
}
function skinById(id) { return skinState.all.find((s) => s.id === id) || null; }

/** A skin with everything it extends folded in, base first. A missing parent
 *  or a cycle ends the chain there. */
function resolveSkin(id) {
  const chain = [];
  let cur = skinById(id);
  while (cur && chain.length < 8 && !chain.includes(cur)) {
    chain.unshift(cur);
    cur = cur.parent ? skinById(cur.parent) : null;
  }
  const res = { id, chain: [], frame: "full", scale: 1, keys: "words", furniture: "", css: "",
    themeOwner: "", editorTheme: "", editorThemeName: "", explorerSide: "", imports: "" };
  for (const s of chain) {
    res.chain.push(s.id);
    if (s.frame) res.frame = s.frame;
    if (s.scale) res.scale = s.scale;
    if (s.keys) res.keys = s.keys;
    if (s.furniture) res.furniture = s.furniture;
    if (s.explorerSide) res.explorerSide = s.explorerSide;
    if (s.editorTheme || s.editorThemeName) {
      res.themeOwner = s.id; res.editorTheme = s.editorTheme; res.editorThemeName = s.editorThemeName;
    }
    const { imports, rest } = leadingImports(s.css);
    res.imports += imports;
    res.css += `\n/* ==== skin: ${s.id} ==== */\n${rest}\n`;
  }
  // A browser keeps an @import (a web font, say) only ahead of every rule,
  // so each skin's go first, base first.
  res.css = res.imports + res.css;
  return res;
}

/** A stylesheet's leading @charset/@import statements, and the rest. */
function leadingImports(css) {
  let i = 0, imports = "";
  for (;;) {
    const ws = /^\s*/.exec(css.slice(i))[0].length;
    i += ws;
    if (css.startsWith("/*", i)) {
      const end = css.indexOf("*/", i + 2);
      if (end < 0) break;
      i = end + 2;
      continue;
    }
    const m = /^@(?:import|charset)\b(?:[^;"']|"[^"]*"|'[^']*')*;/.exec(css.slice(i));
    if (!m) break;
    if (m[0].startsWith("@import")) imports += m[0] + "\n";
    // The statement is taken out; the comments before it stay with the rest.
    css = css.slice(0, i) + css.slice(i + m[0].length);
  }
  return { imports, rest: css };
}
function activeSkin() {
  if (!skinState.resolved || skinState.resolved.id !== skinState.current) {
    skinState.resolved = resolveSkin(skinState.current);
  }
  return skinState.resolved;
}

// ---- applying the active skin ---------------------------------------------------

let appliedSkinClasses = [];
let appliedSkinKey = "";
let appliedKeysFor = "";

/** A key as the skin writes it: the core's own words ("Ctrl+Shift+P"), or,
 *  for a skin with `@keys symbols`, the Mac's symbols ("⌃⇧P"). */
const KEY_SYMBOLS = { ctrl: "⌃", control: "⌃", alt: "⌥", option: "⌥", shift: "⇧", super: "⌘", cmd: "⌘", meta: "⌘",
  enter: "↩", return: "↩", backspace: "⌫", delete: "⌦", escape: "⎋", esc: "⎋", tab: "⇥",
  up: "↑", down: "↓", left: "←", right: "→", pageup: "⇞", pagedown: "⇟", home: "↖", end: "↘" };
function keyLabel(keys) {
  if (!keys || activeSkin().keys !== "symbols") return keys;
  // Chords are separated by spaces; a key's parts by "+" (a "+" key itself
  // is the empty part after one).
  return keys.split(" ").map((chord) => chord.split(/\+(?!$)/)
    .map((p) => KEY_SYMBOLS[p.toLowerCase()] || (p.length === 1 ? p.toUpperCase() : p)).join("")).join(" ");
}

function applySkin() {
  const sk = activeSkin();
  const html = document.documentElement, b = document.body;
  const classes = sk.chain.map((id) => `theme-${id}`);
  for (const c of appliedSkinClasses) if (!classes.includes(c)) { b.classList.remove(c); html.classList.remove(c); }
  for (const c of classes) { b.classList.add(c); html.classList.add(c); }
  appliedSkinClasses = classes;
  const mobile = MOBILE.matches;
  b.classList.toggle("mobile", mobile);
  b.classList.toggle("frame-bezel", sk.frame === "bezel" && !mobile);
  b.classList.toggle("frame-full", sk.frame !== "bezel" || mobile);
  const key = `${sk.id}#${skinState.rev}`;
  if (key !== appliedSkinKey) {
    appliedSkinKey = key;
    document.getElementById("skin-css").textContent = sk.css;
    renderFurniture(sk);
  }
  html.style.setProperty("--ui-scale", String(sk.scale));
  // Key hints are written by the menus, the palette and the status bar.
  if (key !== appliedKeysFor) { appliedKeysFor = key; renderMenubar(); renderStatus(); }
  skinFollowTheme();
  // The cell grid follows the skin's density and fonts.
  remeasure();
  publishRegions();
  renderThemeSwitch();
}

/** Re-measure the editor font's cell and refit every view to it. */
function remeasure() {
  const w = CW, h = CH;
  measureCell();
  if (w === CW && h === CH) return;
  for (const v of views.values()) { v.fit(); v.repaint(); }
}

/** Which way the editor's ground leans, and the readable ink on each fill the
 *  skin replaced without saying what goes on it. */
function skinFollowTheme() {
  const html = document.documentElement;
  const bg = hexRgb(themeVars.theme["--bg"] || "");
  const ink = bg ? onColor(bg) : null;
  html.classList.toggle("editor-dark", ink === "#ffffff");
  html.classList.toggle("editor-light", ink !== null && ink !== "#ffffff");
  const css = activeSkin().css;
  const declares = (p) => new RegExp(`${p}\\s*:`).test(css);
  themeVars.ink = {};
  writeThemeRule();
  const ink2 = {};
  for (const [fill, on] of [["--accent", "--on-accent"], ["--ui-accent", "--on-ui-accent"], ["--menuhi", "--on-menuhi"]]) {
    if (!declares(fill) || declares(on)) continue;
    const c = resolvedColor(fill);
    if (c) ink2[on] = onColor(c);
  }
  themeVars.ink = ink2;
  writeThemeRule();
}

/** The skin's furniture inside #device, rebuilt from inert elements: only
 *  SKIN_TAGS, their class, data-bind / data-bind-var, and text. */
function renderFurniture(sk) {
  const host = document.getElementById("device");
  host.textContent = "";
  let budget = 400;
  const copy = (nodes, parent) => {
    for (const n of nodes) {
      if (budget-- <= 0) return;
      if (n.nodeType === 3) {
        if (n.textContent.trim()) parent.appendChild(document.createTextNode(n.textContent));
      } else if (n.nodeType === 1) {
        const tag = n.tagName.toLowerCase();
        if (!SKIN_TAGS.includes(tag)) continue;
        const e = document.createElement(tag);
        const cls = n.getAttribute("class");
        if (cls) e.className = cls;
        const bind = n.getAttribute("data-bind") || "";
        if (SKIN_BIND_RE.test(bind)) { e.dataset.bind = bind; e.dataset.dflt = n.textContent; }
        const bindVar = n.getAttribute("data-bind-var") || "";
        if (SKIN_BIND_VAR_RE.test(bindVar)) e.dataset.bindVar = bindVar;
        copy(n.childNodes, e);
        parent.appendChild(e);
      }
    }
  };
  if (sk.furniture) {
    const doc = new DOMParser().parseFromString(`<body>${sk.furniture}</body>`, "text/html");
    copy(doc.body.childNodes, host);
  }
  bindSkinValues();
}

// ---- live values in furniture ---------------------------------------------------
// The core sends named values (`skinValues`: the built-in `skin_data:*` set,
// and whatever plugins publish with `editor.setSkinValue`). Furniture shows
// them with `data-bind="plugin:name"` (as text) and
// `data-bind-var="--prop:plugin:name"` (as a custom property, unitless when a
// number). Text only: nothing a value says is ever markup.
core.skinValues = {};
api.on("skinValues", (v) => { core.skinValues = v.skinValues || {}; bindSkinValues(); });

function bindSkinValues() {
  const host = document.getElementById("device");
  const vals = core.skinValues;
  for (const e of host.querySelectorAll("[data-bind]")) {
    const v = vals[e.dataset.bind];
    const text = v !== undefined ? String(v) : (e.dataset.dflt || "");
    if (e.textContent !== text) e.textContent = text;
  }
  for (const e of host.querySelectorAll("[data-bind-var]")) {
    const spec = e.dataset.bindVar;
    const i = spec.indexOf(":");
    const prop = spec.slice(0, i), v = vals[spec.slice(i + 1)];
    if (v === undefined) { e.style.removeProperty(prop); continue; }
    const s = String(v).trim();
    const out = /^-?[0-9.]+$/.test(s) ? String(parseFloat(s)) : s;
    if (e.style.getPropertyValue(prop) !== out) e.style.setProperty(prop, out);
  }
}

// ---- where everything is ----------------------------------------------------------
// For furniture that sits by a part of the client: `--<id>-x/y/w/h` on :root
// (px from #app's top-left; w/h 0 while it is hidden), `--app-x/y/w/h` (the
// client itself, in the window: modals are placed in it), and on <body>
// `has-<id>` while it is shown. Ids: menubar, sidebar, editors, dock,
// statusbar. Also `sidebar-left` / `sidebar-right`, the explorer's side.
const REGION_IDS = ["menubar", "sidebar", "editors", "dock", "statusbar"];

function publishRegions() {
  const app = document.getElementById("app").getBoundingClientRect();
  const st = document.documentElement.style, b = document.body;
  for (const id of REGION_IDS) {
    const r = document.getElementById(id).getBoundingClientRect();
    const open = r.width > 0 && r.height > 0;
    st.setProperty(`--${id}-x`, `${open ? r.left - app.left : 0}px`);
    st.setProperty(`--${id}-y`, `${open ? r.top - app.top : 0}px`);
    st.setProperty(`--${id}-w`, `${open ? r.width : 0}px`);
    st.setProperty(`--${id}-h`, `${open ? r.height : 0}px`);
    b.classList.toggle(`has-${id}`, open);
  }
  st.setProperty("--app-x", `${app.left}px`);
  st.setProperty("--app-y", `${app.top}px`);
  st.setProperty("--app-w", `${app.width}px`);
  st.setProperty("--app-h", `${app.height}px`);
}
{
  const ro = new ResizeObserver(() => publishRegions());
  for (const id of ["app", ...REGION_IDS]) ro.observe(document.getElementById(id));
}

/** The explorer's side is an editor setting; the client follows it. */
api.on("settings", () => {
  const v = core.settings && core.settings.values;
  const right = !!v && !!v.file_explorer && v.file_explorer.side === "right";
  document.body.classList.toggle("sidebar-right", right);
  document.body.classList.toggle("sidebar-left", !right);
  publishRegions();
});

// ---- choosing ---------------------------------------------------------------------

function setSkin(id) {
  if (!skinById(id) || id === skinState.current) { renderThemeSwitch(); return; }
  skinState.current = id;
  try { localStorage.setItem(SKIN_PICK_KEY, id); } catch (_) {}
  applySkin();
  applySkinEditorSettings();
}

function cycleSkin(dir) {
  const ids = skinState.all.map((s) => s.id);
  const i = ids.indexOf(skinState.current);
  skinState.picked = true;
  setSkin(ids[(i + dir + ids.length) % ids.length]);
}

function skinThemeOptOuts() {
  try { return JSON.parse(localStorage.getItem(SKIN_KEEP_THEME_KEY) || "[]").filter((v) => typeof v === "string"); }
  catch (_) { return []; }
}
function setSkinThemeOptOut(id, out) {
  const list = skinThemeOptOuts().filter((v) => v !== id);
  if (out) list.push(id);
  try { localStorage.setItem(SKIN_KEEP_THEME_KEY, JSON.stringify(list)); } catch (_) {}
}
function skinEditorThemeName(sk) {
  if (sk.editorThemeName) return sk.editorThemeName.replace(/^builtin:\/\//, "");
  if (!sk.editorTheme) return "";
  try { return JSON.parse(sk.editorTheme).name || "its theme"; } catch (_) { return "its theme"; }
}
/** What picking the skin changes in the editor, for the picker's toggle. */
function skinEditorChanges(sk) {
  const parts = [];
  const t = skinEditorThemeName(sk);
  if (t) parts.push(["its editor theme ", t]);
  if (sk.explorerSide) parts.push(["the explorer on the ", sk.explorerSide]);
  return parts;
}

/** Picking a skin applies the editor settings it carries (its editor theme,
 *  the explorer's side), unless the user opted out for that skin. Only on a
 *  pick: they are the editor's shared, saved settings. */
function applySkinEditorSettings() {
  const sk = activeSkin();
  if (skinThemeOptOuts().includes(sk.id)) return;
  if (sk.explorerSide) {
    api.call("settings.set", { changes: { "/file_explorer/side": sk.explorerSide } }).catch(showError);
  }
  if (!sk.themeOwner) return;
  const args = sk.editorTheme
    ? { skin: sk.themeOwner, theme: JSON.parse(sk.editorTheme) }
    : { name: sk.editorThemeName };
  api.call("skin.theme", args).catch(showError);
}

// ---- the server's skins folder ------------------------------------------------------

async function fetchText(url) {
  let resp;
  try { resp = await fetch(url, { cache: "no-store" }); }
  catch (_) {
    throw new Error(`could not fetch ${url} (it may not exist, or its server does not allow cross-origin requests — the browser cannot tell which). Download the skin and use “Install from folder…”`);
  }
  if (!resp.ok) throw new Error(`${url} answered HTTP ${resp.status}`);
  return resp.text();
}

let skinPollTimer = 0, skinPolling = false;
/** Read /skins/index.json; reload each folder whose stamp changed; drop the
 *  ones that are gone. Re-applies when the active skin was touched. */
async function pollSkinFolders() {
  if (skinPolling) return;
  skinPolling = true;
  try {
    const index = JSON.parse(await fetchText("/skins/index.json"));
    const next = [];
    let changed = false;
    for (const e of index) {
      const id = e.id, stamp = String(e.stamp);
      const base = `/skins/${id}/`;
      const old = skinState.folder.find((s) => s.source === base);
      if (old && old.stamp === stamp) { next.push(old); continue; }
      changed = true;
      try {
        const css = cssAgainst(await fetchText(`${base}skin.css`), new URL(base, location.href).href);
        const meta = skinHeader(css);
        const ff = meta ? (meta.furniture || "") : "";
        const furniture = ff && plainSkinPath(ff) ? await fetchText(base + ff).catch(() => "") : "";
        // The index says whether the conventional theme.json is there.
        const tf = skinThemeFile(meta);
        const hasTheme = tf !== "" && (tf !== "theme.json" || e.theme === "theme.json");
        const theme = hasTheme ? await fetchText(base + tf).catch(() => "") : "";
        const r = parseSkin({ css, furniture, theme }, id, "folder");
        if (r.skin) { r.skin.source = base; r.skin.stamp = stamp; next.push(r.skin); }
        else console.warn(`fresh: skins folder "${id}": ${r.error}`);
      } catch (err) { console.warn(`fresh: skins folder "${id}": ${String(err)}`); }
    }
    if (next.length !== skinState.folder.length) changed = true;
    if (changed) {
      skinState.folder = next;
      storeSkins(SKIN_FOLDER_CACHE_KEY, next);
      const before = activeSkin();
      rebuildSkins();
      if (!skinState.picked && skinById(skinState.wanted)) skinState.current = skinState.wanted;
      if (!skinById(skinState.current)) skinState.current = skinState.all.length ? skinState.all[0].id : "cosmos";
      const after = activeSkin();
      applySkin();
      // An author saving theme.json sees the buffer follow, like the chrome.
      if (after.id === before.id && after.editorTheme !== before.editorTheme) applySkinEditorSettings();
    }
  } catch (_) {
    // Offline: keep the folders last seen.
  } finally {
    skinPolling = false;
  }
}
/** Fast while a folder skin is on screen (its author is editing it), slow
 *  otherwise (to notice a new folder). */
function scheduleSkinPoll() {
  clearTimeout(skinPollTimer);
  const editing = activeSkin().chain.some((id) => { const s = skinById(id); return !!s && s.origin === "folder"; });
  skinPollTimer = setTimeout(() => pollSkinFolders().then(scheduleSkinPoll), editing ? 700 : 5000);
}

// ---- installing into the browser -------------------------------------------------------

function installSkin(files, fallbackId, source) {
  const fail = (error) => ({ ok: false, id: "", error });
  const r = parseSkin(files, fallbackId, "browser");
  const sk = r.skin;
  if (!sk) return fail(r.error);
  sk.source = source;
  const clash = skinById(sk.id);
  if (clash && clash.origin !== "browser") {
    return fail(`"${sk.id}" is already the id of a ${clash.origin === "builtin" ? "built-in" : "skins-folder"} skin — give yours its own @id`);
  }
  if (sk.parent && !skinById(sk.parent)) return fail(`it @extends "${sk.parent}", which is not installed`);
  let p = sk.parent;
  for (let n = 0; p && n < 16; n++) {
    if (p === sk.id) return fail(`@extends makes a cycle through "${sk.id}"`);
    const ps = skinById(p);
    p = ps ? ps.parent : "";
  }
  const list = storedSkins(SKIN_STORE_KEY).filter((s) => s.id !== sk.id);
  list.push(sk);
  const err = storeSkins(SKIN_STORE_KEY, list);
  if (err) return fail(err);
  rebuildSkins();
  return { ok: true, id: sk.id, error: "" };
}

function removeSkin(id) {
  const sk = skinById(id);
  if (!sk || sk.origin !== "browser") return;
  storeSkins(SKIN_STORE_KEY, storedSkins(SKIN_STORE_KEY).filter((s) => s.id !== id));
  rebuildSkins();
  if (skinState.current === id) {
    skinState.current = sk.parent && skinById(sk.parent) ? sk.parent : (skinState.all.length ? skinState.all[0].id : "cosmos");
    try { localStorage.setItem(SKIN_PICK_KEY, skinState.current); } catch (_) {}
  }
  applySkin();
}

/** A GitHub page URL → the raw file (`…/blob/…`) or raw folder (`…/tree/…`). */
function rawSkinUrl(url) {
  const m = /^https:\/\/github\.com\/([^/]+)\/([^/]+)\/(blob|tree)\/(.+?)\/?$/.exec(url.trim());
  if (!m) return url.trim();
  return `https://raw.githubusercontent.com/${m[1]}/${m[2]}/${m[4]}${m[3] === "tree" ? "/" : ""}`;
}

/** Install from the URL of a skin.css, or of its folder (ending in "/"). */
async function installSkinFromUrl(input) {
  let url = rawSkinUrl(input);
  if (!/^https?:\/\//.test(url) && !url.startsWith("/")) return { ok: false, id: "", error: "enter an http(s):// URL" };
  try {
    url = new URL(url, location.href).href;
    if (url.endsWith("/")) url += "skin.css";
    const css = cssAgainst(await fetchText(url), url);
    const meta = skinHeader(css);
    const ff = meta ? (meta.furniture || "") : "";
    const furniture = ff && plainSkinPath(ff) ? await fetchText(new URL(ff, url).href) : "";
    // A URL has no listing: only a declared theme file is fetched.
    const tf = meta && meta["editor-theme"] ? skinThemeFile(meta) : "";
    const theme = tf ? await fetchText(new URL(tf, url).href) : "";
    const segs = new URL(url).pathname.split("/").filter((s) => s !== "");
    const fallback = segs.length >= 2 ? segs[segs.length - 2].toLowerCase() : "";
    return installSkin({ css, furniture, theme }, fallback, url);
  } catch (e) {
    return { ok: false, id: "", error: String(e).replace(/^Error: /, "") };
  }
}

function fileDataUrl(f) {
  return new Promise((resolve, reject) => {
    const r = new FileReader();
    r.onload = () => resolve(String(r.result));
    r.onerror = () => reject(r.error);
    r.readAsDataURL(f);
  });
}

/** Install from picked files: a skin folder, or skin.css with the files it
 *  uses. Assets become data: URLs, so the skin is whole in browser storage. */
async function installSkinFromFiles(list) {
  const fail = (error) => ({ ok: false, id: "", error });
  const path = (f) => f.webkitRelativePath || f.name;
  const isSkinCss = (f) => /(^|\/)skin\.css$/i.test(path(f));
  const sheets = list.filter((f) => /\.css$/i.test(path(f)))
    .sort((a, b) => path(a).split("/").length - path(b).split("/").length || isSkinCss(b) - isSkinCss(a));
  if (!sheets.length) return fail("pick a skin folder (or its skin.css)");
  const main = sheets[0];
  const dir = path(main).slice(0, path(main).length - main.name.length);
  const rel = {};
  for (const f of list) if (path(f).startsWith(dir)) rel[path(f).slice(dir.length)] = f;
  const assets = {};
  for (const [p, f] of Object.entries(rel)) {
    if (f === main || /\.(css|html?|json)$/i.test(p)) continue;
    if (f.size > SKIN_MAX_ASSET) return fail(`${p} is larger than 2 MB`);
    assets[p] = await fileDataUrl(f);
  }
  const css = rewriteCssUrls(await main.text(), (u) => assets[u.replace(/^\.\//, "").split(/[?#]/)[0]] || null);
  const meta = skinHeader(css);
  const ff = meta ? (meta.furniture || "") : "";
  if (ff && !rel[ff]) return fail(`the header names @furniture ${ff}, but that file was not picked`);
  const furniture = ff ? await rel[ff].text() : "";
  const tf = skinThemeFile(meta);
  if (tf && tf !== "theme.json" && !rel[tf]) return fail(`the header names @editor-theme ${tf}, but that file was not picked`);
  const theme = tf && rel[tf] ? await rel[tf].text() : "";
  const folder = dir.split("/").filter((s) => s !== "").pop() || main.name.replace(/\.css$/i, "");
  return installSkin({ css, furniture, theme }, folder.toLowerCase(), "");
}

// ---- the picker: a pill at the end of the menu bar ------------------------------------

function reportInstall(r) {
  skinState.busy = false;
  if (r.ok) {
    const sk = skinById(r.id);
    skinState.note = { text: `Installed “${sk ? sk.name : r.id}”.`, error: false };
    skinState.urlOpen = false;
    skinState.urlDraft = "";
    skinState.picked = true;
    if (skinState.current === r.id) { applySkin(); applySkinEditorSettings(); } else setSkin(r.id);
  } else {
    skinState.note = { text: `Couldn’t install: ${r.error}`, error: true };
    renderThemeSwitch();
  }
}

function closeThemeMenu() {
  Object.assign(skinState, { menuOpen: false, urlOpen: false, pendingRemove: "", note: { text: "", error: false } });
}

function tsButton(cls, label, fn) {
  const a = el("button", cls, label);
  a.type = "button";
  a.addEventListener("click", (e) => { e.stopPropagation(); fn(); });
  return a;
}

function filePicker(folder) {
  const picker = el("input", "ts-file");
  picker.type = "file";
  picker.multiple = true;
  if (folder) picker.setAttribute("webkitdirectory", "");
  else picker.accept = ".css,.html,.htm,.json,image/*,font/*,.woff,.woff2,.ttf,.otf";
  picker.addEventListener("change", () => {
    const files = Array.from(picker.files || []);
    if (!files.length) return;
    skinState.busy = true;
    skinState.note = { text: "Installing…", error: false };
    renderThemeSwitch();
    installSkinFromFiles(files).then(reportInstall);
  });
  return picker;
}

function renderThemeSwitch() {
  const cur = skinById(skinState.current);
  const btn = document.getElementById("themebtn");
  btn.replaceChildren(el("span", "ts-dot"), el("span", "ts-name", cur ? cur.name : skinState.current));
  btn.setAttribute("aria-expanded", String(skinState.menuOpen));
  const menu = document.getElementById("thememenu");
  menu.hidden = !skinState.menuOpen;
  menu.replaceChildren();
  if (!skinState.menuOpen) return;
  menu.appendChild(el("div", "ts-head", "Web skin"));
  let lastOrigin = "builtin";
  for (const sk of skinState.all) {
    if (sk.origin !== lastOrigin) {
      lastOrigin = sk.origin;
      menu.appendChild(el("div", "ts-head ts-sub", sk.origin === "folder" ? "Skins folder" : "Installed in this browser"));
    }
    const on = sk.id === skinState.current;
    const row = div("ts-row" + (on ? " on" : ""));
    row.setAttribute("role", "menuitemradio");
    row.setAttribute("aria-checked", String(on));
    const desc = sk.description
      || (sk.origin === "folder" ? `${sk.source} — reloads when you save` : sk.source ? `From ${sk.source}` : "Installed from files");
    const text = el("span", "ts-rows");
    text.append(el("span", "ts-label", sk.name), el("span", "ts-desc", desc));
    row.append(el("span", "ts-check", on ? "✓" : ""), text);
    row.addEventListener("click", (e) => {
      e.stopPropagation();
      closeThemeMenu();
      skinState.picked = true;
      setSkin(sk.id);
      scheduleSkinPoll();
    });
    if (sk.origin === "browser") {
      const tools = el("span", "ts-tools");
      if (sk.source) {
        const up = tsButton("ts-tool", "↻", () => {
          skinState.busy = true;
          skinState.note = { text: `Updating “${sk.name}”…`, error: false };
          renderThemeSwitch();
          installSkinFromUrl(sk.source).then(reportInstall);
        });
        up.title = "Fetch it again from its URL";
        tools.appendChild(up);
      }
      const confirm = skinState.pendingRemove === sk.id;
      const rm = tsButton("ts-tool" + (confirm ? " ts-danger" : ""), confirm ? "Remove" : "✕", () => {
        if (!confirm) { skinState.pendingRemove = sk.id; renderThemeSwitch(); return; }
        skinState.pendingRemove = "";
        skinState.note = { text: `Removed “${sk.name}”.`, error: false };
        removeSkin(sk.id);
      });
      rm.title = confirm ? "Click again to remove" : "Remove this skin";
      tools.appendChild(rm);
      row.appendChild(tools);
    }
    menu.appendChild(row);
    // The editor settings the active skin carries: applied on a pick unless
    // switched off here.
    const changes = on ? skinEditorChanges(activeSkin()) : [];
    if (changes.length) {
      const keep = skinThemeOptOuts().includes(sk.id);
      const opt = div("ts-opt");
      opt.setAttribute("role", "menuitemcheckbox");
      opt.setAttribute("aria-checked", String(!keep));
      const label = el("span", null, "Also apply ");
      changes.forEach(([what, name], i) => {
        if (i) label.append(" and ");
        label.append(what, el("b", null, name));
      });
      opt.append(el("span", "ts-box", keep ? "" : "✓"), label);
      opt.title = keep ? "Apply these editor settings now, and whenever you pick this skin"
        : "Leave the editor's theme and settings alone when picking this skin";
      opt.addEventListener("click", (e) => {
        e.stopPropagation();
        setSkinThemeOptOut(sk.id, !keep);
        if (keep) applySkinEditorSettings();
        skinState.note = { text: keep ? "Applied the skin's editor settings." : "The editor's theme and settings now stay as they are when you pick this skin.", error: false };
        renderThemeSwitch();
      });
      menu.appendChild(opt);
    }
  }
  menu.appendChild(div("ts-sep"));
  const folderPick = filePicker(true), filesPick = filePicker(false);
  menu.append(folderPick, filesPick,
    tsButton("ts-act", "Install from folder…", () => folderPick.click()),
    tsButton("ts-act", "Install from files…", () => filesPick.click()),
    tsButton("ts-act", "Install from URL…", () => { skinState.urlOpen = !skinState.urlOpen; renderThemeSwitch(); }));
  if (skinState.urlOpen) {
    const box = div("ts-url");
    const input = el("input");
    input.type = "url";
    input.placeholder = "https://…/my-skin/skin.css";
    input.value = skinState.urlDraft;
    input.setAttribute("aria-label", "Skin URL");
    const go = () => {
      if (skinState.busy || !skinState.urlDraft.trim()) return;
      skinState.busy = true;
      skinState.note = { text: "Fetching…", error: false };
      renderThemeSwitch();
      installSkinFromUrl(skinState.urlDraft).then(reportInstall);
    };
    input.addEventListener("input", () => { skinState.urlDraft = input.value; });
    input.addEventListener("keydown", (e) => {
      e.stopPropagation();
      if (e.key === "Enter") { e.preventDefault(); go(); }
      else if (e.key === "Escape") { e.preventDefault(); skinState.urlOpen = false; renderThemeSwitch(); }
    });
    box.append(input, tsButton("ts-go", "Install", go));
    menu.appendChild(box);
    // At once, not on a timer: keys typed straight after the click land in
    // the field, not in the editor behind the picker.
    if (document.activeElement !== input) input.focus({ preventScroll: true });
  }
  if (skinState.note.text) {
    const note = div("ts-note" + (skinState.note.error ? " err" : ""));
    note.setAttribute("role", "status");
    note.textContent = skinState.note.text;
    menu.appendChild(note);
  }
}

// ---- start ------------------------------------------------------------------------------

skinState.folder = storedSkins(SKIN_FOLDER_CACHE_KEY);
rebuildSkins();
try {
  // `?skin=<id>` picks (and remembers) a skin — also the way out of one whose
  // CSS hid the picker.
  const forced = new URL(location.href).searchParams.get("skin");
  const t = forced !== null ? forced : localStorage.getItem(SKIN_PICK_KEY);
  if (t !== null) skinState.wanted = t;
  if (forced !== null) localStorage.setItem(SKIN_PICK_KEY, forced);
} catch (_) {}
// A folder skin not read yet starts on the first built-in; the folder poll
// switches to it when the server has it.
skinState.current = skinById(skinState.wanted) ? skinState.wanted : (skinState.all.length ? skinState.all[0].id : "cosmos");

{
  const btn = document.getElementById("themebtn");
  btn.addEventListener("click", (e) => {
    e.stopPropagation();
    if (skinState.menuOpen) closeThemeMenu();
    else { skinState.menuOpen = true; pollSkinFolders(); }
    renderThemeSwitch();
  });
  document.addEventListener("mousedown", (e) => {
    if (skinState.menuOpen && !document.getElementById("themeswitch").contains(e.target)) {
      closeThemeMenu();
      renderThemeSwitch();
    }
  }, true);
  // Ctrl/Cmd+Alt+T cycles skins, wherever the keyboard is; Escape closes the
  // picker, as it does any menu, and is not the editor's.
  window.addEventListener("keydown", (e) => {
    if (e.key === "Escape" && skinState.menuOpen && !skinState.urlOpen) {
      e.preventDefault();
      e.stopPropagation();
      closeThemeMenu();
      renderThemeSwitch();
      focusKeyboard();
    } else if ((e.ctrlKey || e.metaKey) && e.altKey && e.code === "KeyT") {
      e.preventDefault();
      e.stopPropagation();
      cycleSkin(e.shiftKey ? -1 : 1);
    }
  }, true);
  MOBILE.addEventListener("change", applySkin);
}

/** The active document's name, in the window title and in the title-bar host
 *  a skin may show (#mactitle). */
function syncTitle() {
  const s = core.status || {};
  const b = bufferById(s.buffer);
  const label = b ? `${baseName(b.name)} — Fresh` : "Fresh";
  if (document.title !== label) document.title = label;
  const name = document.querySelector("#mactitle .mt-name");
  if (name.textContent !== label) name.textContent = label;
}
api.on("status", syncTitle);
api.on("buffers", syncTitle);
