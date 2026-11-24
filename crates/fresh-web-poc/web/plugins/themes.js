// Themes: the editor's colours (syntax, UI, diagnostics) and the page's
// CSS variables, switched live. The choice is remembered in the browser.

import { THEME } from "../js/highlight.js";

const THEMES = {
  dark: {},
  light: {
    "--bg": "#ffffff", "--fg": "#1f1f1f", "--panel": "#f3f3f3", "--border": "#d4d4d4", "--accent": "#005fb8", "--muted": "#6e6e6e",
    "editor.fg": "#1f1f1f", "editor.line_number_fg": "#8a8a8a", "editor.selection_bg": "#add6ff", "editor.occurrence_bg": "#e4e4e4",
    "syntax.keyword": "#0000ff", "syntax.string": "#a31515", "syntax.comment": "#008000", "syntax.number": "#098658", "syntax.constant": "#0070c1",
    "syntax.type": "#267f99", "syntax.function": "#795e26", "syntax.variable": "#001080", "syntax.link": "#006ab1", "ui.split_separator_fg": "#bbbbbb",
    "search.match_bg": "#f8c9a1", "search.current_bg": "#f2a96b",
  },
  "high-contrast": {
    "--bg": "#000000", "--fg": "#ffffff", "--panel": "#0b0b0b", "--border": "#6fc3df", "--accent": "#f38518", "--muted": "#c0c0c0",
    "editor.fg": "#ffffff", "editor.line_number_fg": "#c0c0c0", "editor.selection_bg": "#264f78", "editor.occurrence_bg": "#333333",
    "syntax.keyword": "#569cd6", "syntax.string": "#ce9178", "syntax.comment": "#7ca668", "syntax.number": "#b5cea8", "syntax.constant": "#4fc1ff",
    "syntax.type": "#4ec9b0", "syntax.function": "#dcdcaa", "syntax.variable": "#9cdcfe", "syntax.link": "#3794ff",
  },
  solarized: {
    "--bg": "#002b36", "--fg": "#93a1a1", "--panel": "#073642", "--border": "#0b4452", "--accent": "#268bd2", "--muted": "#657b83",
    "editor.fg": "#93a1a1", "editor.line_number_fg": "#586e75", "editor.selection_bg": "#0b4f61", "editor.occurrence_bg": "#0a4150",
    "syntax.keyword": "#859900", "syntax.string": "#2aa198", "syntax.comment": "#586e75", "syntax.number": "#d33682", "syntax.constant": "#cb4b16",
    "syntax.type": "#b58900", "syntax.function": "#268bd2", "syntax.variable": "#6c71c4", "syntax.link": "#268bd2",
  },
};

export function activate(fresh) {
  // The dark theme is the page's own colours and the built-in editor
  // colours, captured before any theme changes them.
  const DARK = { ...THEME };
  const base = {};
  const css = getComputedStyle(document.documentElement);
  for (const t of Object.values(THEMES)) for (const k of Object.keys(t)) if (k.startsWith("--")) base[k] = css.getPropertyValue(k).trim();
  const apply = (name) => {
    const t = THEMES[name] || {};
    const keys = new Set(Object.values(THEMES).flatMap(Object.keys));
    const full = {};
    for (const k of keys) full[k] = k in t ? t[k] : k.startsWith("--") ? base[k] : DARK[k];
    fresh.ui.theme(full);
    fresh.settings.theme = name;
    try { localStorage.setItem("fresh-poc:theme", name); } catch {}
  };
  fresh.events.on("app.ready", () => {
    let saved = null;
    try { saved = localStorage.getItem("fresh-poc:theme"); } catch {}
    if (saved && saved !== "dark") apply(saved);
  });
  fresh.commands.register("theme.select", {
    title: "Select Theme…",
    run: () => fresh.ui.pick({ title: "Theme", items: Object.keys(THEMES).map((n) => ({ label: n, value: n })), onChange: (it) => it && apply(it.value), onAccept: (it) => apply(it.value) }),
  });
}
