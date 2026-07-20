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
  // macOS Light — systemGray6 window (#ececec), labelColor text (black 85%),
  // controlAccent / selectedContentBackground blue (#0064e1). Values follow the
  // documented macOS system colours (developer.apple.com HIG + NSColor dumps).
  macos: {
    "--fg": "#1d1d1f", "--muted": "#8a8a8e",
    "--bg2": "#ffffff", "--bg3": "#f2f2f4",
    "--menuhi": "#0064e1", "--border": "#d3d3d8",
    "--status-bg": "#ececec", "--status-fg": "#71717a",
    "--shell": "#ececec",
    "--accent": "#0064e1", "--ui-accent": "#0064e1",
    "--on-ui-accent": "#ffffff", "--on-accent": "#ffffff", "--on-sel": "#ffffff",
    "--ok": "#34c759",
    "--surface": "#f6f6f6", "--surface-2": "#ececec",
    "--hairline": "rgba(0,0,0,.085)", "--hairline-strong": "rgba(0,0,0,.13)",
    "--hover": "rgba(0,0,0,.05)",
    "--sel": "#0064e1", "--sel-ring": "none",
    "--shadow": "0 11px 34px rgba(0,0,0,.17), 0 2px 7px rgba(0,0,0,.11)",
    "--r-sm": "6px", "--r-md": "8px", "--r-lg": "10px",
    "--mac-titlebar": "linear-gradient(180deg,#f5f5f5,#e9e9e9)",
    "--mac-title-fg": "#4a4a4e", "--mac-panel": "rgba(245,245,245,.80)",
    "--mac-btn": "linear-gradient(180deg,#ffffff,#f3f3f3)", "--mac-btn-fg": "#1d1d1f",
    "--mac-sidebar": "#ececec",
  },
  // macOS Dark — windowBackground ~#2b2b2d, dark controlAccent #0a84ff,
  // labelColor white 85%. Surfaces sit a touch above the (dark) editor canvas.
  "macos-dark": {
    "--fg": "#e3e3e6", "--muted": "#909096",
    "--bg2": "#2e2e30", "--bg3": "#252527",
    "--menuhi": "#0a84ff", "--border": "#48484b",
    "--status-bg": "#252527", "--status-fg": "#98989d",
    "--shell": "#2b2b2d",
    "--accent": "#0a84ff", "--ui-accent": "#0a84ff",
    "--on-ui-accent": "#ffffff", "--on-accent": "#ffffff", "--on-sel": "#ffffff",
    "--ok": "#30d158",
    "--surface": "#2e2e30", "--surface-2": "#262628",
    "--hairline": "rgba(255,255,255,.09)", "--hairline-strong": "rgba(255,255,255,.16)",
    "--hover": "rgba(255,255,255,.07)",
    "--sel": "#0a84ff", "--sel-ring": "none",
    "--shadow": "0 16px 48px rgba(0,0,0,.55), 0 2px 10px rgba(0,0,0,.45)",
    "--r-sm": "6px", "--r-md": "8px", "--r-lg": "10px",
    "--mac-titlebar": "linear-gradient(180deg,#333335,#2a2a2c)",
    "--mac-title-fg": "#c9c9ce", "--mac-panel": "rgba(44,44,47,.74)",
    "--mac-btn": "linear-gradient(180deg,#3c3c3f,#353538)", "--mac-btn-fg": "#e3e3e6",
    "--mac-sidebar": "#252527",
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
