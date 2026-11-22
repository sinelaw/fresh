// Web-UI SKINS — a FRONTEND-owned look for the *chrome*, chosen by the user and
// independent of the editor's TUI colour theme (which still owns every buffer
// cell). Same class as zoom / paletteCentered / altselect: a pure view
// preference persisted in localStorage, never editor state.
//
// (web-ui/js — concatenated in filename order into the page's single <script>
// by crates/fresh-editor/build.rs; all files share one scope. This file sorts
// after 10-core so it can use CW/scene etc., and before everything that renders
// chrome. `webTheme` is read at runtime by layoutShell()/render(), never at
// parse time, so there is no cross-file TDZ.)
//
// A skin is a FOLDER of ordinary web files (docs/features/web-ui-skins.md):
//   skin.css        a stylesheet that starts with a `==FreshSkin==` header
//                   comment (@id, @name, @extends, @frame, @scale, @menu,
//                   @furniture, …). Colours are custom properties on
//                   `:root.theme-<id>`; structure is any CSS scoped under
//                   `.theme-<id>`. Images and fonts by relative url(…).
//   furniture.html  optional decorative markup for the #device bezel host.
//   theme.json      optional EDITOR theme, in Fresh's ordinary theme format
//                   (or `@editor-theme <name>` for an installed one). Picking
//                   the skin applies it too — opt-out per skin in the picker.
//                   It goes through the editor's own theme switch (a
//                   `skinTheme` message, src/webui/skins.rs), because the
//                   editor theme is shared, saved state: every client and the
//                   terminal see it, and config.json records it.
// Skins come from three places, read the same way:
//   • built-in — web-ui/skins/NN-id/, bundled by build.rs;
//   • folder   — the server's skins directory (`$FRESH_WEB_SKINS`, else
//                `<config>/web-skins/`), at /skins/…; polled, so a saved edit
//                shows at once — the skin author's dev loop;
//   • browser  — installed in the picker from a URL or a picked folder/files,
//                kept in localStorage with relative URLs made absolute (URL)
//                or inlined as data: URLs (files).
// Applying one: its CSS (and every skin it @extends, base first) goes into
// <style id=skin-css>, between the base CSS and css-late/; <html> and <body>
// carry `theme-<id>` for the whole chain; `editor-dark` / `editor-light` on
// <html> say which way the editor background leans (for skins that follow it
// with `color-mix(in srgb, var(--bg), …)`). The editor theme's own colours
// sit in a plain `:root{}` rule below the skin (shell.html #editor-theme), so
// the skin wins by the ordinary cascade. No skin runs script: the CSS is set
// as text and the furniture is rebuilt from a few whitelisted tags.

/** type Skin = {
    id: String, name: String, description: String, author: String, version: String,
    parent: String, frame: String, scale: Number, menu: String, furnitureFile: String,
    css: String, furniture: String, editorTheme: String, editorThemeName: String,
    origin: String, source: String, stamp: String
} */
/** type ResolvedSkin = {
    id: String, chain: String[], frame: String, scale: Number, menu: String, furniture: String, css: String,
    themeOwner: String, editorTheme: String, editorThemeName: String
} */
/** type SkinFiles = {css: String, furniture: String, theme: String} */
/** type InstallResult = {ok: Boolean, id: String, error: String} */

const SKIN_ID_RE = /^[a-z0-9]([a-z0-9-]{0,46}[a-z0-9])?$/;
const SKIN_TAGS = ["div", "span", "i", "b", "em", "strong", "small", "p"];
const SKIN_MAX_CSS = 2000000;
const SKIN_MAX_ASSET = 2000000;
const SKIN_STORE_KEY = "fresh.skins";
const SKIN_FOLDER_CACHE_KEY = "fresh.skins.folders";

// The built-in skin folders, spliced in by build.rs (the `[]` is what an
// unassembled source reads as).
/** const BUILTIN_SKIN_DIRS: {dir: String, files: Dict<String>}[] */
const BUILTIN_SKIN_DIRS = jsonAs(/*@SKINS@*/[]);

// ---- reading a skin ---------------------------------------------------------

// The `/* ==FreshSkin== … ==/FreshSkin== */` header: `@key value` per line.
/** function skinHeader(String) => Dict<String> | Null */
function skinHeader(css) {
  const m = /\/\*\s*==FreshSkin==([\s\S]*?)==\/FreshSkin==\s*\*\//.exec(css);
  if (!m) return null;
  /** const meta: Dict<String> */
  const meta = {};
  for (const line of m[1].split("\n")) {
    const kv = /^\s*@([a-z-]+)\s+(.*?)\s*$/.exec(line);
    if (kv) meta[kv[1]] = kv[2];
  }
  return meta;
}
// A plain relative path inside a skin folder ("furniture.html", "img/a.png").
/** function plainSkinPath(String) => Boolean */
function plainSkinPath(p) {
  return p.split("/").every(s => /^[A-Za-z0-9._-]+$/.test(s) && s !== "." && s !== "..");
}
// The file a skin's editor theme is in: `@editor-theme <file>.json`, else the
// conventional `theme.json` (optional — "" when the header names an
// installed theme instead).
/** function skinThemeFile(Dict<String> | Null) => String */
function skinThemeFile(meta) {
  const v = meta ? (meta["editor-theme"] ?? "").trim() : "";
  if (!v) return "theme.json";
  return /\.json$/i.test(v) && plainSkinPath(v) ? v : "";
}
/** function parseSkin(SkinFiles, String, String) => {skin: Skin | Null, error: String} */
function parseSkin(files, fallbackId, origin) {
  /** const fail: (String) => {skin: Skin | Null, error: String} */
  const fail = error => ({ skin: null, error });
  const css = files.css;
  if (css.length > SKIN_MAX_CSS) return fail("skin.css is larger than 2 MB");
  const header = skinHeader(css);
  if (!header) return fail("skin.css must start with a /* ==FreshSkin== … ==/FreshSkin== */ header");
  /** const meta: Dict<String> */
  const meta = header;
  const get = (/** String */ k) => (meta[k] ?? "").slice(0, 200);
  const id = get("id") || fallbackId;
  if (!SKIN_ID_RE.test(id)) return fail(`"${id}" is not a usable skin id — set @id to 1–48 characters of a–z, 0–9 and "-"`);
  const name = get("name");
  if (!name) return fail("the header needs an @name");
  const parent = get("extends");
  if (parent && (!SKIN_ID_RE.test(parent) || parent === id)) return fail("@extends must name another skin's id");
  const frame = get("frame");
  if (frame && frame !== "bezel" && frame !== "full") return fail('@frame must be "bezel" or "full"');
  const menu = get("menu");
  if (menu && menu !== "cells" && menu !== "flow") return fail('@menu must be "cells" or "flow"');
  const scale = get("scale") ? parseFloat(get("scale")) : 0;
  if (scale !== 0 && !(scale >= 0.5 && scale <= 1.5)) return fail("@scale must be a number from 0.5 to 1.5");
  const furnitureFile = get("furniture");
  if (furnitureFile && !plainSkinPath(furnitureFile)) return fail("@furniture must be a relative path inside the skin folder");
  const themeRef = get("editor-theme");
  const editorThemeName = themeRef && !/\.json$/i.test(themeRef) ? themeRef : "";
  const editorTheme = files.theme.trim();
  if (editorTheme) {
    if (editorTheme.length > 512000) return fail("the editor theme is larger than 500 KB");
    let kind = "";
    try { kind = jsonKind(jsonParse(editorTheme)); } catch (e) { return fail(`the editor theme is not valid JSON (${String(e).replace(/^SyntaxError: /, "")})`); }
    if (kind !== "object") return fail("the editor theme must be a JSON object (Fresh's theme format)");
  }
  return { skin: {
    id, name, description: get("description"), author: get("author"), version: get("version"),
    parent, frame, scale, menu, furnitureFile, css, furniture: files.furniture.slice(0, 200000),
    editorTheme, editorThemeName, origin, source: "", stamp: "",
  }, error: "" };
}

// Rewrite every relative `url(…)` and `@import "…"` target of a stylesheet
// with `map` (null leaves it as written). Absolute URLs, root paths,
// fragments and data: URLs are never relative.
/** function rewriteCssUrls(String, (String) => String | Null) => String */
function rewriteCssUrls(css, map) {
  let out = "";
  let i = 0;
  const isRel = (/** String */ u) => !!u && !/^([A-Za-z][A-Za-z0-9+.-]*:|\/|#)/.test(u);
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
/** function cssAgainst(String, String) => String */
function cssAgainst(css, base) {
  return rewriteCssUrls(css, u => { try { return new URL(u, base).href; } catch (_) { return null; } });
}

// ---- the registry -----------------------------------------------------------

/** let skins: Skin[] */
let skins = [];
let skinRev = 0;
/** let folderSkins: Skin[] */
let folderSkins = [];
/** function storedSkins(String) => Skin[] */
function storedSkins(key) {
  /** const out: Skin[] */
  const out = [];
  try {
    const text = localStorage.getItem(key);
    if (text === null) return out;
    for (const e of jsonItems(jsonGet(jsonParse(text), "skins"))) {
      const r = parseSkin({ css: jsonStr(jsonGet(e, "css")) ?? "", furniture: jsonStr(jsonGet(e, "furniture")) ?? "",
        theme: jsonStr(jsonGet(e, "theme")) ?? "" },
        jsonStr(jsonGet(e, "id")) ?? "", key === SKIN_STORE_KEY ? "browser" : "folder");
      const sk = r.skin;
      if (!sk) { console.warn(`fresh: a stored skin was skipped: ${r.error}`); continue; }
      sk.source = jsonStr(jsonGet(e, "source")) ?? "";
      sk.stamp = jsonStr(jsonGet(e, "stamp")) ?? "";
      out.push(sk);
    }
  } catch (_) { console.warn("fresh: stored skins could not be read"); }
  return out;
}
/** function storeSkins(String, Skin[]) => String */
function storeSkins(key, list) {
  const rows = list.map(s => ({ id: s.id, css: s.css, furniture: s.furniture, theme: s.editorTheme, source: s.source, stamp: s.stamp }));
  try { localStorage.setItem(key, JSON.stringify({ v: 1, skins: rows })); return ""; }
  catch (_) { return "the browser refused to store the skin (storage full or disabled; browser storage holds about 5 MB)"; }
}
function rebuildSkins() {
  /** const out: Skin[] */
  const out = [];
  for (const d of BUILTIN_SKIN_DIRS) {
    const bcss = d.files["skin.css"] ?? "";
    const tf = skinThemeFile(skinHeader(bcss));
    const r = parseSkin({ css: bcss, furniture: "", theme: tf ? (d.files[tf] ?? "") : "" }, d.dir.replace(/^[0-9]+-/, ""), "builtin");
    const sk = r.skin;
    if (!sk) { console.error(`fresh: built-in skin ${d.dir} rejected: ${r.error}`); continue; }
    if (sk.furnitureFile) sk.furniture = d.files[sk.furnitureFile] ?? "";
    out.push(sk);
  }
  for (const sk of folderSkins.concat(storedSkins(SKIN_STORE_KEY))) {
    if (out.some(o => o.id === sk.id)) { console.warn(`fresh: skin "${sk.id}" is shadowed by another with the same id`); continue; }
    out.push(sk);
  }
  skins = out;
  skinRev++;
  resolvedSkin = null;
}
/** function skinById(String) => Skin | Null */
function skinById(id) { return skins.find(s => s.id === id) ?? null; }
function skinIds() { return skins.map(s => s.id); }
/** function skinName(String) => String */
function skinName(id) { const s = skinById(id); return s ? s.name : id; }

// A skin with everything it extends folded in (base first). A missing parent
// or a cycle ends the chain there rather than failing the page.
/** function resolveSkin(String) => ResolvedSkin */
function resolveSkin(id) {
  /** const chain: Skin[] */
  const chain = [];
  let cur = skinById(id);
  while (cur && chain.length < 8) {
    const c = cur;
    if (chain.some(s => s.id === c.id)) break;
    chain.unshift(c);
    cur = c.parent ? skinById(c.parent) : null;
  }
  /** const res: ResolvedSkin */
  const res = { id, chain: [], frame: "full", scale: 1, menu: "cells", furniture: "", css: "",
    themeOwner: "", editorTheme: "", editorThemeName: "" };
  for (const s of chain) {
    res.chain.push(s.id);
    if (s.frame) res.frame = s.frame;
    if (s.scale) res.scale = s.scale;
    if (s.menu) res.menu = s.menu;
    if (s.furniture) res.furniture = s.furniture;
    if (s.editorTheme || s.editorThemeName) {
      res.themeOwner = s.id; res.editorTheme = s.editorTheme; res.editorThemeName = s.editorThemeName;
    }
    res.css += `\n/* ==== skin: ${s.id} ==== */\n${s.css}\n`;
  }
  return res;
}
/** let resolvedSkin: ResolvedSkin | Null */
let resolvedSkin = null;
/** function activeSkin() => ResolvedSkin */
function activeSkin() {
  const r = resolvedSkin;
  if (r && r.id === webTheme) return r;
  const fresh = resolveSkin(webTheme);
  resolvedSkin = fresh;
  return fresh;
}
function shellTheme() { return activeSkin().frame === "bezel"; }

folderSkins = storedSkins(SKIN_FOLDER_CACHE_KEY);   // last seen; refreshed from the server below
rebuildSkins();
let webTheme = "cosmos";
try {
  // `?skin=<id>` picks (and remembers) a skin — also the way out of one whose
  // CSS hid the picker.
  const forced = new URL(window.location.href).searchParams.get("skin");
  const t = forced ?? localStorage.getItem("fresh.webtheme");
  if (t !== null) webTheme = t;
  if (forced !== null) localStorage.setItem("fresh.webtheme", forced);
} catch (_) {}
// An unknown id (a folder skin not seen yet) falls back for now; the folder
// poll switches to it if the server has it.
const wantedSkin = webTheme;
if (!skinById(webTheme) && skins.length) webTheme = skins[0].id;

// ---- applying the active skin ---------------------------------------------

/** let appliedSkinClasses: String[] */
let appliedSkinClasses = [];

// Rebuild the skin's furniture inside #device from its HTML, keeping only
// whitelisted elements, their class attribute and their text. Not per frame:
// only when the skin or the registry changes.
function renderFurniture(/** ResolvedSkin */ sk) {
  const host = document.getElementById("device");
  if (!host) return;
  const key = `${sk.id}#${skinRev}`;
  if (host.dataset.theme === key) return;
  host.dataset.theme = key;
  host.textContent = "";
  let budget = 400;
  /** const copy: (El[], El) => Undefined */
  const copy = (nodes, parent) => {
    for (const n of nodes) {
      if (budget-- <= 0) return;
      if (n.nodeType === 3) {
        if (n.textContent.trim()) parent.appendChild(document.createTextNode(n.textContent));
      } else if (n.nodeType === 1) {
        const tag = n.tagName.toLowerCase();
        if (!SKIN_TAGS.includes(tag)) continue;
        const el = document.createElement(tag);
        const cls = n.getAttribute("class");
        if (cls) el.className = cls;
        // Live values (bindSkinValues): which value, by "plugin:name" — the
        // only attributes kept besides class, and only in their plain form.
        const bind = n.getAttribute("data-bind") ?? "";
        if (SKIN_BIND_RE.test(bind)) { el.dataset.bind = bind; el.dataset.dflt = n.textContent; }
        const bindVar = n.getAttribute("data-bind-var") ?? "";
        if (SKIN_BIND_VAR_RE.test(bindVar)) el.dataset.bindVar = bindVar;
        copy(n.childNodes, el);
        parent.appendChild(el);
      }
    }
  };
  if (sk.furniture) copy(parseInertHtml(sk.furniture), host);
  bindSkinValues();
}

// ---- live values in furniture -------------------------------------------------
// Named values for skins: the built-in `skin_data` plugin publishes a fixed set
// (system load, memory, diagnostics, … — docs/features/web-ui-skins.md), and
// any installed plugin may add its own (`editor.setSkinValue(name, value)`);
// the scene carries them as `regions.skinValues`, keyed "plugin:name". A skin
// itself never brings code. Furniture shows them with
//   data-bind="plugin:name"            the value as the element's text (its own
//                                      text is the placeholder until one comes);
//   data-bind-var="--prop:plugin:name" the value as a custom property on the
//                                      element, unitless when it is a number —
//                                      for gauges and bars in CSS.
// Text only, through textContent; nothing a value says is ever markup.
const SKIN_BIND_RE = /^[A-Za-z0-9_.-]+:[A-Za-z0-9_.-]+$/;
const SKIN_BIND_VAR_RE = /^--[A-Za-z0-9_-]+:[A-Za-z0-9_.-]+:[A-Za-z0-9_.-]+$/;
function bindSkinValues() {
  const host = document.getElementById("device");
  if (!host) return;
  const sc = scene;
  /** const vals: Dict<String> */
  const vals = sc && sc.regions.skinValues ? sc.regions.skinValues : {};
  for (const el of host.querySelectorAll("[data-bind]")) {
    const key = el.dataset.bind ?? "";
    const v = vals[key];
    const text = v !== undefined ? v : (el.dataset.dflt ?? "");
    if (el.textContent !== text) el.textContent = text;
  }
  for (const el of host.querySelectorAll("[data-bind-var]")) {
    const spec = el.dataset.bindVar ?? "";
    const i = spec.indexOf(":");
    const prop = spec.slice(0, i), key = spec.slice(i + 1);
    const v = vals[key];
    if (v === undefined) { el.style.removeProperty(prop); continue; }
    const n = parseFloat(v);
    const out = String(v) === String(n) || /^-?[0-9.]+$/.test(v.trim()) ? String(n) : v;
    if (el.style.getPropertyValue(prop) !== out) el.style.setProperty(prop, out);
  }
}

// A colour custom property as `#rrggbb`, whatever it is written as
// (`color-mix(…)`, `rgb(…)`): the browser resolves it on a probe.
/** function resolveColor(String) => String | Null */
function resolveColor(prop) {
  const probe = div("skin-probe");
  probe.style.color = `var(${prop})`;
  document.body.appendChild(probe);
  const c = getComputedStyle(probe).getPropertyValue("color");
  probe.remove();
  const m = /^(?:rgba?\(|color\(srgb\s+)([0-9.]+)[ ,]+([0-9.]+)[ ,]+([0-9.]+)/.exec(c.trim());
  if (!m) return null;
  const unit = c.trim().startsWith("color(") ? 255 : 1;
  return "#" + [m[1], m[2], m[3]].map(v => hex2(Math.round(Math.min(255, parseFloat(v) * unit)))).join("");
}

// Apply the active skin. Called from render() right after applyTheme(). Cheap
// when nothing changed: the CSS and furniture are rewritten only on a skin
// (or registry) change.
function applyWebTheme() {
  const sk = activeSkin();
  const html = document.documentElement;
  const b = document.body;
  const classes = sk.chain.map(id => `theme-${id}`);
  for (const c of appliedSkinClasses) if (!classes.includes(c)) { b.classList.remove(c); html.classList.remove(c); }
  for (const c of classes) { b.classList.add(c); html.classList.add(c); }
  appliedSkinClasses = classes;
  b.classList.toggle("frame-full", sk.frame === "full");
  b.classList.toggle("frame-bezel", sk.frame === "bezel");
  // Menu titles flowed with even spacing rather than at their cell slots.
  b.classList.toggle("menu-flow", sk.menu === "flow");
  // Which way the editor background leans: dark exactly when white text reads
  // better on it (the rule onColor() applies everywhere else). Neither on a
  // terminal-reset ground, so a skin's static palette stands.
  const base = editorThemeDecl();
  const bg = base.getPropertyValue("--bg").trim();
  const ink = hexRgb(bg) ? onColor(bg) : null;
  html.classList.toggle("editor-dark", ink === "#ffffff");
  html.classList.toggle("editor-light", ink !== null && ink !== "#ffffff");
  const style = document.getElementById("skin-css");
  const key = `${sk.id}#${skinRev}`;
  if (style && style.dataset.theme !== key) { style.dataset.theme = key; style.textContent = sk.css; }
  renderFurniture(sk);
  // A skin may replace the fills themselves (Winamp's navy highlight, macOS's
  // blue), and the ink that goes ON a fill is computed from it — so recompute
  // when the skin's fill differs from the theme's, or a row keeps the ink
  // picked for the editor theme's fill (gruvbox's amber ink on Winamp's navy
  // measured 1.9:1). Not when the skin states the ink itself (macOS Dark
  // names `--on-accent`): its answer, and otherwise the THEME's declared ink
  // (`menu_highlight_fg`) for a fill the skin left alone, beat any pick.
  const eff = getComputedStyle(html);
  for (const [fill, onFill] of [["--menuhi", "--on-menuhi"], ["--accent", "--on-accent"]]) {
    if (eff.getPropertyValue(fill).trim() === base.getPropertyValue(fill).trim()) continue;
    if (eff.getPropertyValue(onFill).trim() !== base.getPropertyValue(onFill).trim()) continue;
    const c = resolveColor(fill);
    const on = c ? onColor(c) : null;
    if (on) base.setProperty(onFill, on);
  }
  // Density: re-measure only when the multiplier actually changed (this runs on
  // every full render). The caller (render/hello) re-fits the grid afterwards.
  if (sk.scale !== webThemeScale) { webThemeScale = sk.scale; measureMetrics(); }
}

// User-facing switch: persist, re-measure the grid (a skin may rescale cells),
// re-render (re-seeds the classes and tokens), and re-fit the editor to the
// skin's geometry (full-bleed vs the bezel-inset grid).
function setWebTheme(name) {
  if (!skinById(name) || name === webTheme) { renderThemeSwitch(); return; }
  webTheme = name;
  try { localStorage.setItem("fresh.webtheme", name); } catch (_) {}
  refreshSkin();
  applySkinEditorTheme();
}

// ---- a skin's editor theme ---------------------------------------------------
// Picking a skin that carries an editor theme applies that theme too, unless
// the user opted out for that skin (the toggle under it in the picker). Only
// on a pick — never at page load, when the editor already has whatever theme
// was last chosen (it is saved in config.json).
const SKIN_KEEP_THEME_KEY = "fresh.skins.keepEditorTheme";
/** function skinThemeOptOuts() => String[] */
function skinThemeOptOuts() {
  try {
    const t = localStorage.getItem(SKIN_KEEP_THEME_KEY);
    return t === null ? [] : jsonItems(jsonParse(t)).map(v => jsonStr(v) ?? "").filter(v => v !== "");
  } catch (_) { return []; }
}
/** function setSkinThemeOptOut(String, Boolean) => Undefined */
function setSkinThemeOptOut(id, out) {
  const list = skinThemeOptOuts().filter(v => v !== id);
  if (out) list.push(id);
  try { localStorage.setItem(SKIN_KEEP_THEME_KEY, JSON.stringify(list)); } catch (_) {}
}
// The display name of the active skin's editor theme ("" when it has none).
/** function skinEditorThemeName(ResolvedSkin) => String */
function skinEditorThemeName(sk) {
  if (sk.editorThemeName) return sk.editorThemeName.replace(/^builtin:\/\//, "");
  if (!sk.editorTheme) return "";
  try { return jsonStr(jsonGet(jsonParse(sk.editorTheme), "name")) ?? "its theme"; } catch (_) { return "its theme"; }
}
function applySkinEditorTheme() {
  const sk = activeSkin();
  if (!sk.themeOwner || skinThemeOptOuts().includes(sk.id)) return;
  if (sk.editorTheme) {
    try { wsSend({ type: "skinTheme", skin: sk.themeOwner, theme: jsonParse(sk.editorTheme), name: undefined }); } catch (_) {}
  } else {
    wsSend({ type: "skinTheme", skin: sk.themeOwner, theme: undefined, name: sk.editorThemeName });
  }
}
function refreshSkin() {
  resolvedSkin = null;
  webThemeScale = activeSkin().scale;
  measureMetrics();
  if (scene) render();       // re-place at the new metrics + re-seed theme tokens
  else applyWebTheme();
  resize();                  // editor re-fits cols/rows to the new geometry
  renderThemeSwitch();
}
function cycleWebTheme(dir) {
  const ids = skinIds();
  const i = ids.indexOf(webTheme);
  setWebTheme(ids[(i + (dir || 1) + ids.length) % ids.length]);
}

// Keep the title-bar host's document name (#mactitle .mt-name, shown by skins
// that draw a title bar) in step with the active tab. Cheap and called from
// every frame apply; reads the same tab projection the mobile header uses.
function syncMacTitle() {
  const el = document.getElementById("mactitle");
  if (!el) return;
  const name = el.querySelector(".mt-name");
  if (!name) return;
  let label = "Fresh";
  const sc = scene;
  const tabs = sc && sc.regions.panes.length ? sc.regions.panes[0].tabs : [];
  const at = tabs.find(t => t.active) ?? (tabs.length ? tabs[0] : null);
  if (at && at.label) label = `${at.label.split("/").pop()}  —  Fresh`;
  if (name.textContent !== label) name.textContent = label;
}

// ---- the server's skins folder: listed, and polled for edits ---------------

/** function fetchText(String) => Promise<String> */
async function fetchText(url) {
  let ok = false, status = 0, text = "";
  try {
    const resp = await fetch(url);
    ok = resp.ok; status = resp.status;
    if (ok) text = await resp.text();
  } catch (_) {
    throw new Error(`could not fetch ${url} (it may not exist, or its server does not allow cross-origin requests — the browser cannot tell which). Download the skin and use “Install from folder…”`);
  }
  if (!ok) throw new Error(`${url} answered HTTP ${status}`);
  return text;
}
let skinPollTimer = 0;
let skinPolling = false;
// Fetch /skins/index.json; (re)load every folder whose stamp changed; drop the
// ones that are gone. Re-applies when the active skin's chain was touched.
async function pollSkinFolders() {
  if (skinPolling) return;
  skinPolling = true;
  try {
    const index = jsonParse(await fetchText("/skins/index.json"));
    /** const next: Skin[] */
    const next = [];
    let changed = false;
    for (const e of jsonItems(index)) {
      const id = jsonStr(jsonGet(e, "id")) ?? "";
      const stamp = jsonStr(jsonGet(e, "stamp")) ?? "";
      const old = folderSkins.find(s => s.id === id || s.source === `/skins/${id}/`);
      if (old && old.stamp === stamp) { next.push(old); continue; }
      changed = true;
      const base = `/skins/${id}/`;
      try {
        const css = cssAgainst(await fetchText(`${base}skin.css`), new URL(base, window.location.href).href);
        const meta = skinHeader(css);
        const ff = meta ? (meta.furniture ?? "") : "";
        const furniture = ff && plainSkinPath(ff) ? await fetchText(base + ff).catch(() => "") : "";
        // The index says whether the conventional theme.json is there, so an
        // absent one costs no failed request.
        const tf = skinThemeFile(meta);
        const hasTheme = tf !== "" && (tf !== "theme.json" || jsonStr(jsonGet(e, "theme")) === "theme.json");
        const theme = hasTheme ? await fetchText(base + tf).catch(() => "") : "";
        const r = parseSkin({ css, furniture, theme }, id, "folder");
        const sk = r.skin;
        if (sk) { sk.source = base; sk.stamp = stamp; next.push(sk); }
        else console.warn(`fresh: skins folder "${id}": ${r.error}`);
      } catch (err) { console.warn(`fresh: skins folder "${id}": ${String(err)}`); }
    }
    if (next.length !== folderSkins.length) changed = true;
    if (changed) {
      folderSkins = next;
      storeSkins(SKIN_FOLDER_CACHE_KEY, next);
      const before = activeSkin().css;
      const beforeTheme = activeSkin().editorTheme;
      rebuildSkins();
      if (!skinById(webTheme)) webTheme = skins.length ? skins[0].id : "cosmos";
      if (webTheme !== wantedSkin && skinById(wantedSkin) && !skinPickedSinceBoot) webTheme = wantedSkin;
      if (activeSkin().css !== before || resolvedSkin === null) refreshSkin(); else renderThemeSwitch();
      // An author saving theme.json sees the buffer follow, like the chrome.
      if (activeSkin().editorTheme !== beforeTheme) applySkinEditorTheme();
    }
  } catch (_) {
    // No server route (an older bridge) or offline: keep the cached folders.
  } finally {
    skinPolling = false;
  }
}
let skinPickedSinceBoot = false;
// Fast while a folder skin is on screen (the author is editing it), slow
// otherwise (just to notice a new folder).
function scheduleSkinPoll() {
  clearTimeout(skinPollTimer);
  const editing = activeSkin().chain.some(id => { const s = skinById(id); return !!s && s.origin === "folder"; });
  skinPollTimer = setTimeout(() => { pollSkinFolders().then(() => { scheduleSkinPoll(); }); }, editing ? 700 : 5000);
}

// ---- installing into the browser --------------------------------------------

/** function installSkin(SkinFiles, String, String) => InstallResult */
function installSkin(files, fallbackId, source) {
  const fail = (/** String */ error) => ({ ok: false, id: "", error });
  const r = parseSkin(files, fallbackId, "browser");
  const sk = r.skin;
  if (!sk) return fail(r.error);
  sk.source = source;
  const clash = skinById(sk.id);
  if (clash && clash.origin !== "browser")
    return fail(`"${sk.id}" is already the id of a ${clash.origin === "builtin" ? "built-in" : "skins-folder"} skin — give yours its own @id`);
  if (sk.parent && !skinById(sk.parent)) return fail(`it @extends "${sk.parent}", which is not installed`);
  let p = sk.parent;
  for (let n = 0; p && n < 16; n++) {
    if (p === sk.id) return fail(`@extends makes a cycle through "${sk.id}"`);
    const ps = skinById(p);
    p = ps ? ps.parent : "";
  }
  const list = storedSkins(SKIN_STORE_KEY).filter(s => s.id !== sk.id);
  list.push(sk);
  const err = storeSkins(SKIN_STORE_KEY, list);
  if (err) return fail(err);
  rebuildSkins();
  return { ok: true, id: sk.id, error: "" };
}
/** function removeSkin(String) => Boolean */
function removeSkin(id) {
  const sk = skinById(id);
  if (!sk || sk.origin !== "browser") return false;
  storeSkins(SKIN_STORE_KEY, storedSkins(SKIN_STORE_KEY).filter(s => s.id !== id));
  rebuildSkins();
  if (webTheme === id) {
    const fallback = sk.parent && skinById(sk.parent) ? sk.parent : (skins.length ? skins[0].id : "cosmos");
    webTheme = fallback;
    try { localStorage.setItem("fresh.webtheme", fallback); } catch (_) {}
  }
  refreshSkin();
  return true;
}

// A GitHub page URL → the raw file (`…/blob/…`), or the raw folder (`…/tree/…`).
/** function rawSkinUrl(String) => String */
function rawSkinUrl(url) {
  const m = /^https:\/\/github\.com\/([^/]+)\/([^/]+)\/(blob|tree)\/(.+?)\/?$/.exec(url.trim());
  if (!m) return url.trim();
  return `https://raw.githubusercontent.com/${m[1]}/${m[2]}/${m[4]}${m[3] === "tree" ? "/" : ""}`;
}
// Install from the URL of a skin.css, or of its folder (ending in "/").
/** function installSkinFromUrl(String) => Promise<InstallResult> */
async function installSkinFromUrl(input) {
  let url = rawSkinUrl(input);
  if (!/^https?:\/\//.test(url) && !url.startsWith("/")) return { ok: false, id: "", error: "enter an http(s):// URL" };
  try {
    url = new URL(url, window.location.href).href;
    if (url.endsWith("/")) url += "skin.css";
    const css = cssAgainst(await fetchText(url), url);
    const meta = skinHeader(css);
    const ff = meta ? (meta.furniture ?? "") : "";
    const furniture = ff && plainSkinPath(ff) ? await fetchText(new URL(ff, url).href) : "";
    // A URL has no directory listing, so only a DECLARED theme file is
    // fetched (`@editor-theme theme.json`), and it must load. Guessing at a
    // theme.json would cost every theme-less skin a failed request.
    const declared = !!meta && !!(meta["editor-theme"] ?? "");
    const tf = declared ? skinThemeFile(meta) : "";
    const theme = tf ? await fetchText(new URL(tf, url).href) : "";
    const segs = new URL(url).pathname.split("/").filter(s => s !== "");
    const fallback = segs.length >= 2 ? segs[segs.length - 2].toLowerCase() : "";
    return installSkin({ css, furniture, theme }, fallback, url);
  } catch (e) {
    return { ok: false, id: "", error: String(e).replace(/^Error: /, "") };
  }
}
// Install from picked files: a whole skin folder (webkitdirectory), or
// skin.css with the files it references. Assets become data: URLs, so the
// skin is self-contained in browser storage.
/** function installSkinFromFiles(File[]) => Promise<InstallResult> */
async function installSkinFromFiles(list) {
  const fail = (/** String */ error) => ({ ok: false, id: "", error });
  const path = (/** File */ f) => f.webkitRelativePath || f.name;
  const sheets = list.filter(f => /(^|\/)[^/]*\.css$/i.test(path(f)))
    .sort((a, b) => path(a).split("/").length - path(b).split("/").length
      || (/(^|\/)skin\.css$/i.test(path(b)) ? 1 : 0) - (/(^|\/)skin\.css$/i.test(path(a)) ? 1 : 0));
  if (!sheets.length) return fail("pick a skin folder (or its skin.css)");
  const main = sheets[0];
  const dir = path(main).slice(0, path(main).length - main.name.length);
  /** const rel: Dict<File> */
  const rel = {};
  for (const f of list) if (path(f).startsWith(dir)) rel[path(f).slice(dir.length)] = f;
  let css = await main.text();
  /** const assets: Dict<String> */
  const assets = {};
  for (const [p, f] of Object.entries(rel)) {
    if (f === main || /\.(css|html?|json)$/i.test(p)) continue;
    if (f.size > SKIN_MAX_ASSET) return fail(`${p} is larger than 2 MB`);
    assets[p] = await fileDataUrl(f);
  }
  css = rewriteCssUrls(css, u => assets[u.replace(/^\.\//, "").split(/[?#]/)[0]] ?? null);
  const meta = skinHeader(css);
  const ff = meta ? (meta.furniture ?? "") : "";
  const ffile = ff ? rel[ff] : undefined;
  if (ff && !ffile) return fail(`the header names @furniture ${ff}, but that file was not picked`);
  const furniture = ffile ? await ffile.text() : "";
  const tf = skinThemeFile(meta);
  const tfile = tf ? rel[tf] : undefined;
  if (tf && tf !== "theme.json" && !tfile) return fail(`the header names @editor-theme ${tf}, but that file was not picked`);
  const theme = tfile ? await tfile.text() : "";
  const folder = dir.split("/").filter(s => s !== "").pop() ?? main.name.replace(/\.css$/i, "");
  return installSkin({ css, furniture, theme }, folder.toLowerCase(), "");
}

// ---- the desktop skin picker (a frontend-owned floating control) -----------
// A small pill in the top-right wallpaper corner; clicking it drops a menu of
// the skins plus install / remove. Frontend-owned and always present (like the
// reconnect / natsel pills), so the switch never depends on the server-driven
// chrome. Its URL field keeps its own keys (75-app.js skips key events inside
// #themeswitch).
let themeMenuOpen = false;
let skinUrlOpen = false;
let skinUrlDraft = "";
let skinPendingRemove = "";
let skinBusy = false;
/** let skinNote: {text: String, error: Boolean} */
let skinNote = { text: "", error: false };
/** function reportInstall(InstallResult) => Undefined */
function reportInstall(r) {
  skinBusy = false;
  if (r.ok) {
    const sk = skinById(r.id);
    skinNote = { text: `Installed “${sk ? sk.name : r.id}”.`, error: false };
    skinUrlOpen = false; skinUrlDraft = "";
    skinPickedSinceBoot = true;
    if (webTheme === r.id) { refreshSkin(); applySkinEditorTheme(); } else setWebTheme(r.id);
  } else {
    skinNote = { text: `Couldn’t install: ${r.error}`, error: true };
    renderThemeSwitch();
  }
}
function closeThemeMenu() {
  themeMenuOpen = false; skinUrlOpen = false; skinPendingRemove = ""; skinNote = { text: "", error: false };
}
function initThemeSwitch() {
  const root = document.getElementById("themeswitch");
  if (!root) return;
  const btn = byId("themebtn");
  btn.onclick = e => {
    e.stopPropagation();
    if (themeMenuOpen) closeThemeMenu(); else { themeMenuOpen = true; pollSkinFolders(); }
    renderThemeSwitch();
  };
  // Outside-click closes the menu.
  listen(document, "mousedown", e => {
    if (themeMenuOpen && !root.contains(e.target)) { closeThemeMenu(); renderThemeSwitch(); }
  }, {capture: true});
  renderThemeSwitch();
  pollSkinFolders().then(() => { scheduleSkinPoll(); });
}
/** function tsButton(String, String, () => Undefined) => El */
function tsButton(cls, label, fn) {
  const a = document.createElement("button");
  a.type = "button";
  a.className = cls;
  a.textContent = label;
  a.onclick = e => { e.stopPropagation(); fn(); };
  return a;
}
/** function filePicker(Boolean) => El */
function filePicker(folder) {
  const picker = document.createElement("input");
  picker.type = "file";
  picker.className = "ts-file";
  picker.setAttribute("multiple", "");
  if (folder) picker.setAttribute("webkitdirectory", "");
  picker.addEventListener("change", () => {
    const fl = inputFiles(picker);
    if (!fl.length) return;
    skinBusy = true; skinNote = { text: "Installing…", error: false }; renderThemeSwitch();
    installSkinFromFiles(fl).then(reportInstall);
  });
  return picker;
}
const SKIN_ORIGIN_HEAD = { builtin: "", folder: "Skins folder", browser: "Installed in this browser" };
function renderThemeSwitch() {
  const root = document.getElementById("themeswitch");
  if (!root) return;
  root.classList.toggle("mobile-hidden", isMobile());
  const cur = skinById(webTheme);
  const btn = document.getElementById("themebtn");
  if (btn) btn.innerHTML = '<span class="ts-dot"></span><span class="ts-name">'
    + esc(cur ? cur.name : webTheme) + "</span>";
  const menu = document.getElementById("thememenu");
  if (!menu) return;
  menu.classList.toggle("open", themeMenuOpen);
  menu.innerHTML = "";
  if (!themeMenuOpen) return;
  const head = div("ts-head"); head.textContent = "Web skin"; menu.appendChild(head);
  let lastOrigin = "builtin";
  for (const sk of skins) {
    if (sk.origin !== lastOrigin) {
      lastOrigin = sk.origin;
      const h = div("ts-head ts-sub");
      h.textContent = sk.origin === "folder" ? SKIN_ORIGIN_HEAD.folder : SKIN_ORIGIN_HEAD.browser;
      menu.appendChild(h);
    }
    const on = sk.id === webTheme;
    const row = div("ts-row" + (on ? " on" : ""));
    row.setAttribute("role", "menuitemradio");
    row.setAttribute("aria-checked", on ? "true" : "false");
    const desc = sk.description
      || (sk.origin === "folder" ? `${sk.source} — reloads when you save` : sk.source ? `From ${sk.source}` : "Installed from files");
    row.innerHTML = '<span class="ts-check">' + (on ? "✓" : "") + "</span>"
      + '<span class="ts-rows"><span class="ts-label">' + esc(sk.name) + "</span>"
      + '<span class="ts-desc">' + esc(desc) + "</span></span>";
    row.onclick = e => { e.stopPropagation(); closeThemeMenu(); skinPickedSinceBoot = true; setWebTheme(sk.id); scheduleSkinPoll(); };
    if (sk.origin === "browser") {
      const tools = document.createElement("span");
      tools.className = "ts-tools";
      if (sk.source) {
        const up = tsButton("ts-tool", "↻", () => {
          skinBusy = true; skinNote = { text: `Updating “${sk.name}”…`, error: false }; renderThemeSwitch();
          installSkinFromUrl(sk.source).then(reportInstall);
        });
        up.title = "Fetch it again from its URL";
        tools.appendChild(up);
      }
      const confirm = skinPendingRemove === sk.id;
      const rm = tsButton("ts-tool" + (confirm ? " ts-danger" : ""), confirm ? "Remove" : "✕", () => {
        if (!confirm) { skinPendingRemove = sk.id; renderThemeSwitch(); return; }
        skinPendingRemove = "";
        removeSkin(sk.id);
        skinNote = { text: `Removed “${sk.name}”.`, error: false };
        renderThemeSwitch();
      });
      rm.title = confirm ? "Click again to remove" : "Remove this skin";
      tools.appendChild(rm);
      row.appendChild(tools);
    }
    menu.appendChild(row);
    // The active skin's editor theme: applied on a pick unless switched off here.
    const themeName = on ? skinEditorThemeName(activeSkin()) : "";
    if (themeName) {
      const keep = skinThemeOptOuts().includes(sk.id);
      const opt = div("ts-opt");
      opt.setAttribute("role", "menuitemcheckbox");
      opt.setAttribute("aria-checked", keep ? "false" : "true");
      opt.innerHTML = '<span class="ts-box">' + (keep ? "" : "✓") + '</span><span>Also use its editor theme <b>'
        + esc(themeName) + "</b></span>";
      opt.title = keep ? "Apply this skin's editor theme now, and whenever you pick the skin"
        : "Leave the editor theme alone when picking this skin (the current theme stays)";
      opt.onclick = e => {
        e.stopPropagation();
        setSkinThemeOptOut(sk.id, !keep);
        if (keep) applySkinEditorTheme();
        skinNote = { text: keep ? `Editor theme set to “${themeName}”.` : "The editor theme now stays as it is when you pick this skin.", error: false };
        renderThemeSwitch();
      };
      menu.appendChild(opt);
    }
  }
  menu.appendChild(div("ts-sep"));
  const folderPick = filePicker(true);
  const filesPick = filePicker(false);
  filesPick.setAttribute("accept", ".css,.html,.htm,image/*,font/*,.woff,.woff2,.ttf,.otf");
  menu.appendChild(folderPick);
  menu.appendChild(filesPick);
  menu.appendChild(tsButton("ts-act", "Install from folder…", () => folderPick.click()));
  menu.appendChild(tsButton("ts-act", "Install from files…", () => filesPick.click()));
  menu.appendChild(tsButton("ts-act", "Install from URL…", () => { skinUrlOpen = !skinUrlOpen; renderThemeSwitch(); }));
  if (skinUrlOpen) {
    const box = div("ts-url");
    const input = document.createElement("input");
    input.type = "url";
    input.placeholder = "https://…/my-skin/skin.css";
    input.value = skinUrlDraft;
    input.setAttribute("aria-label", "Skin URL");
    const go = () => {
      if (skinBusy || !skinUrlDraft.trim()) return;
      skinBusy = true; skinNote = { text: "Fetching…", error: false }; renderThemeSwitch();
      installSkinFromUrl(skinUrlDraft).then(reportInstall);
    };
    input.addEventListener("input", () => { skinUrlDraft = input.value; });
    input.addEventListener("keydown", e => {
      // The field owns its keys. Stop here rather than rely on 75-app.js's
      // #themeswitch check: go() re-renders the menu, detaching this input
      // before the event would bubble to the document.
      e.stopPropagation();
      if (e.key === "Enter") { e.preventDefault(); go(); }
      else if (e.key === "Escape") { e.preventDefault(); skinUrlOpen = false; renderThemeSwitch(); focusSink(); }
    });
    box.appendChild(input);
    box.appendChild(tsButton("ts-go", "Install", go));
    menu.appendChild(box);
    setTimeout(() => input.focus({ preventScroll: true }), 0);
  }
  if (skinNote.text) {
    const note = div("ts-note" + (skinNote.error ? " err" : ""));
    note.setAttribute("role", "status");
    note.textContent = skinNote.text;
    menu.appendChild(note);
  }
}
