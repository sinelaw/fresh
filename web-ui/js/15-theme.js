// Web-UI theme system — a FRONTEND-owned look for the *chrome*, chosen by the
// user and independent of the editor's TUI colour theme (which still owns every
// buffer cell). Same class as zoom / paletteCentered / altselect: a pure view
// preference persisted in localStorage, never editor state.
//
// (web-ui/js — concatenated in filename order into the page's single <script>
// by crates/fresh-editor/build.rs; all files share one scope. This file sorts
// after 10-core so it can use CW/scene etc., and before everything that renders
// chrome. `webTheme` is read at runtime by layoutShell()/render(), never at
// parse time, so there is no cross-file TDZ.)
//
// HOW IT LAYERS. Two mechanisms, by concern:
//   • Colour tokens — applyWebTheme() writes the active theme's chrome palette
//     as INLINE custom properties on :root, right after applyTheme() has piped
//     the TUI theme in. Inline wins, so macOS/Compact own their chrome colours
//     on ANY editor theme; Cosmos writes nothing and the chrome keeps following
//     the TUI palette exactly as it always has. `--bg` is deliberately NEVER
//     overridden — the syntax-highlighted cells are painted for the TUI bg, so
//     the buffer surface must stay the editor's.
//   • Structure / decoration — a `theme-<name>` class on <body>. The stylesheet
//     (css/92-theme-macos.css, css/94-theme-compact.css) keys geometry, fonts,
//     window chrome and the wallpaper/bezel opt-out off that class. Cosmos needs
//     no rules of its own: it IS the base look, so its class only marks the
//     switcher's active row and gates the hardware bezel in layoutShell().
const WEB_THEMES = ["cosmos", "macos", "macos-dark", "compact"];
const WEB_THEME_LABELS = { cosmos: "Cosmos", macos: "macOS Light", "macos-dark": "macOS Dark", compact: "Compact" };
const WEB_THEME_DESC = {
  cosmos: "Wallpaper, glass & hardware bezel",
  macos: "Native macOS — light & vibrant",
  "macos-dark": "Native macOS — dark & vibrant",
  compact: "Dense, chrome-light IDE",
};
// The macOS variants share one structural stylesheet (title bar, traffic
// lights, system font, control shapes); only their colour tokens differ.
const MACOS_THEMES = ["macos", "macos-dark"];
// The inline custom properties applyTheme() (js/20-cells.js) owns. applyWebTheme
// must NOT clear these when a theme leaves them unset — applyTheme just re-wrote
// them from the live TUI theme, and that is exactly what Cosmos wants.
const THEME_KEYS = ["--bg", "--fg", "--accent", "--muted", "--bg2", "--bg3",
  "--menuhi", "--border", "--status-bg", "--status-fg", "--on-accent", "--on-sel", "--shell"];
// Density multiplier per theme (layered under user zoom in measureMetrics).
const WEB_THEME_SCALE = { cosmos: 1, macos: 1, "macos-dark": 1, compact: 0.92 };

// Per-theme chrome palettes. Cosmos = {} (identity — inherit the TUI theme).
// The macOS variants are fixed "System" palettes (light / dark) built from the
// real macOS system colours; Compact a flat, quiet dark. Any key here is
// applied inline (winning over applyTheme + the :root defaults); any key a
// theme omits that is NOT a THEME_KEYS member is reset to its stylesheet
// default when that theme is active, so nothing leaks between themes. The
// --mac-* keys are consumed only by css/92-theme-macos.css.
const WEB_THEME_VARS = {
  cosmos: {},
  // macOS Light — systemGray6 window, labelColor text, controlAccent blue,
  // selectedContentBackground for list selection.
  macos: {
    "--fg": "#1d1d1f", "--muted": "#86868b",
    "--bg2": "#ffffff", "--bg3": "#f0f0f3",
    "--menuhi": "#0a66ff", "--border": "#d6d6db",
    "--status-bg": "#ececef", "--status-fg": "#6e6e73",
    "--shell": "#ececef",
    "--accent": "#0a66ff", "--ui-accent": "#0a66ff",
    "--on-ui-accent": "#ffffff", "--on-accent": "#ffffff", "--on-sel": "#ffffff",
    "--ok": "#34c759",
    "--surface": "#f6f6f8", "--surface-2": "#ececef",
    "--hairline": "rgba(0,0,0,.09)", "--hairline-strong": "rgba(0,0,0,.14)",
    "--hover": "rgba(0,0,0,.05)",
    "--sel": "#0a66ff", "--sel-ring": "none",
    "--shadow": "0 12px 40px rgba(0,0,0,.18), 0 2px 8px rgba(0,0,0,.12)",
    "--r-sm": "6px", "--r-md": "8px", "--r-lg": "10px",
    "--mac-titlebar": "linear-gradient(180deg,#f6f6f8,#e9e9ec)",
    "--mac-title-fg": "#4b4b51", "--mac-panel": "rgba(246,246,248,.78)",
    "--mac-btn": "linear-gradient(180deg,#ffffff,#f4f4f6)", "--mac-btn-fg": "#1d1d1f",
    "--mac-sidebar": "#eeeef1",
  },
  // macOS Dark — windowBackground ~#28282a, dark controlAccent #0a84ff.
  "macos-dark": {
    "--fg": "#e4e4e7", "--muted": "#8d8d93",
    "--bg2": "#2c2c2e", "--bg3": "#232325",
    "--menuhi": "#0a84ff", "--border": "#3a3a3d",
    "--status-bg": "#232325", "--status-fg": "#98989d",
    "--shell": "#28282a",
    "--accent": "#0a84ff", "--ui-accent": "#0a84ff",
    "--on-ui-accent": "#ffffff", "--on-accent": "#ffffff", "--on-sel": "#ffffff",
    "--ok": "#30d158",
    "--surface": "#2c2c2e", "--surface-2": "#242426",
    "--hairline": "rgba(255,255,255,.08)", "--hairline-strong": "rgba(255,255,255,.15)",
    "--hover": "rgba(255,255,255,.06)",
    "--sel": "#0a84ff", "--sel-ring": "none",
    "--shadow": "0 16px 48px rgba(0,0,0,.55), 0 2px 10px rgba(0,0,0,.45)",
    "--r-sm": "6px", "--r-md": "8px", "--r-lg": "10px",
    "--mac-titlebar": "linear-gradient(180deg,#303032,#28282a)",
    "--mac-title-fg": "#c7c7cc", "--mac-panel": "rgba(42,42,45,.72)",
    "--mac-btn": "linear-gradient(180deg,#3a3a3d,#333336)", "--mac-btn-fg": "#e4e4e7",
    "--mac-sidebar": "#1f1f22",
  },
  compact: {
    "--fg": "#c9d1d9", "--muted": "#7d8590",
    "--bg2": "#1b1e24", "--bg3": "#16181d",
    "--menuhi": "#213a54", "--border": "#2b2f37",
    "--status-bg": "#16181d", "--status-fg": "#8b949e",
    "--shell": "#16181d",
    "--accent": "#4a9eff", "--ui-accent": "#4a9eff",
    "--on-ui-accent": "#04121f", "--on-accent": "#04121f", "--on-sel": "#e6edf3",
    "--surface": "#1b1e24", "--surface-2": "#20242b",
    "--hairline": "rgba(255,255,255,.07)", "--hairline-strong": "rgba(255,255,255,.14)",
    "--hover": "rgba(255,255,255,.06)",
    "--sel": "color-mix(in srgb, var(--ui-accent) 22%, transparent)",
    "--sel-ring": "inset 0 0 0 1px color-mix(in srgb, var(--ui-accent) 42%, transparent)",
    "--shadow": "0 10px 28px rgba(0,0,0,.5)",
    "--r-sm": "3px", "--r-md": "4px", "--r-lg": "6px",
  },
};
// Union of every override key, for stale-clearing on theme switch.
const WEB_THEME_ALL_KEYS = (() => {
  const s = new Set();
  for (const t of WEB_THEMES) for (const k of Object.keys(WEB_THEME_VARS[t])) s.add(k);
  return [...s];
})();

let webTheme = "cosmos";
try { const t = localStorage.getItem("fresh.webtheme"); if (WEB_THEMES.includes(t)) webTheme = t; } catch (_) {}

// Apply the active web theme. Called from render() right after applyTheme():
//   1. swap the body theme-<name> class (drives all structural CSS + the bezel);
//   2. layer the theme's chrome tokens inline, clearing any a prior theme left.
// Runs every full render, so the inline overrides survive re-seeding by
// applyTheme (which runs immediately before it on each render()).
function applyWebTheme() {
  const b = document.body;
  for (const n of WEB_THEMES) b.classList.toggle("theme-" + n, n === webTheme);
  // Family marker so the two macOS variants share one structural stylesheet.
  b.classList.toggle("macfam", MACOS_THEMES.includes(webTheme));
  const r = document.documentElement.style;
  const vars = WEB_THEME_VARS[webTheme] || {};
  for (const k of WEB_THEME_ALL_KEYS) {
    if (k in vars) r.setProperty(k, vars[k]);
    else if (!THEME_KEYS.includes(k)) r.removeProperty(k);   // back to the :root default
    // keys applyTheme owns are left as it just set them (Cosmos wants the TUI value)
  }
  // Density: re-measure only when the multiplier actually changed (this runs on
  // every full render). The caller (render/hello) re-fits the grid afterwards.
  const scale = WEB_THEME_SCALE[webTheme] || 1;
  if (scale !== webThemeScale) { webThemeScale = scale; measureMetrics(); }
}

// User-facing switch: persist, re-measure the grid (Compact rescales cells),
// re-render (re-seeds chrome tokens + body class), and re-fit the editor to the
// theme's geometry (full-bleed vs the bezel-inset Cosmos grid).
function setWebTheme(name) {
  if (!WEB_THEMES.includes(name) || name === webTheme) { renderThemeSwitch(); return; }
  webTheme = name;
  try { localStorage.setItem("fresh.webtheme", name); } catch (_) {}
  webThemeScale = WEB_THEME_SCALE[webTheme] || 1;
  measureMetrics();
  if (scene) render();       // re-place at the new metrics + re-seed theme tokens
  resize();                  // editor re-fits cols/rows to the new geometry
  renderThemeSwitch();
}
function cycleWebTheme(dir) {
  const i = WEB_THEMES.indexOf(webTheme);
  setWebTheme(WEB_THEMES[(i + (dir || 1) + WEB_THEMES.length) % WEB_THEMES.length]);
}

// Keep the macOS title bar's document name in step with the active tab. Cheap
// and called from every frame apply (a no-op unless the macOS theme is on);
// reads the same tab projection the mobile header uses.
function syncMacTitle() {
  const el = document.getElementById("mactitle");
  if (!el) return;
  const name = el.querySelector(".mt-name");
  if (!name || !MACOS_THEMES.includes(webTheme)) return;
  let label = "Fresh";
  try {
    const tabs = (scene && scene.regions && scene.regions.panes[0] && scene.regions.panes[0].tabs) || [];
    const at = tabs.find(t => t.active) || tabs[0];
    if (at && at.label) label = at.label.split("/").pop() + "  —  Fresh";
  } catch (_) {}
  if (name.textContent !== label) name.textContent = label;
}

// ---- the desktop theme switcher (a frontend-owned floating control) ---------
// A small pill in the top-right wallpaper corner; clicking it drops a menu of
// the three themes. Frontend-owned and always present (like the reconnect /
// natsel pills), so the switch never depends on the server-driven chrome.
let themeMenuOpen = false;
function initThemeSwitch() {
  const root = document.getElementById("themeswitch");
  if (!root) return;
  const btn = document.getElementById("themebtn");
  btn.onclick = e => { e.stopPropagation(); themeMenuOpen = !themeMenuOpen; renderThemeSwitch(); };
  // Outside-click / Escape close the menu.
  document.addEventListener("mousedown", e => {
    if (themeMenuOpen && !root.contains(e.target)) { themeMenuOpen = false; renderThemeSwitch(); }
  }, true);
  renderThemeSwitch();
}
function renderThemeSwitch() {
  const root = document.getElementById("themeswitch");
  if (!root) return;
  root.classList.toggle("mobile-hidden", isMobile());
  const btn = document.getElementById("themebtn");
  if (btn) btn.innerHTML = '<span class="ts-dot"></span><span class="ts-name">'
    + esc(WEB_THEME_LABELS[webTheme]) + "</span>";
  const menu = document.getElementById("thememenu");
  if (!menu) return;
  menu.classList.toggle("open", themeMenuOpen);
  if (!themeMenuOpen) { menu.innerHTML = ""; return; }
  menu.innerHTML = "";
  const head = div("ts-head"); head.textContent = "Web theme"; menu.appendChild(head);
  for (const name of WEB_THEMES) {
    const row = div("ts-row" + (name === webTheme ? " on" : ""));
    row.innerHTML = '<span class="ts-check">' + (name === webTheme ? "✓" : "") + "</span>"
      + '<span class="ts-rows"><span class="ts-label">' + esc(WEB_THEME_LABELS[name]) + "</span>"
      + '<span class="ts-desc">' + esc(WEB_THEME_DESC[name]) + "</span></span>";
    row.onclick = e => { e.stopPropagation(); themeMenuOpen = false; setWebTheme(name); };
    menu.appendChild(row);
  }
}
