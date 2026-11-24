// Themes: the editor's own themes (served by the server, `theme.list` /
// `theme.get`, fully resolved) mapped onto the page's colour variables.
// Every theme sets every variable (`VARS`), so a theme styles the whole UI
// and switching back restores exactly what the previous one replaced. The
// picker previews live; the choice is remembered in the browser.

import { NAMED } from "../js/highlight.js";

// A theme colour ([r, g, b], a colour name, "#rrggbb", or {color, modifier})
// as [r, g, b], or null for "Default" (the terminal's own colour).
function rgb(def) {
  if (def && !Array.isArray(def) && typeof def === "object") def = def.color;
  if (Array.isArray(def)) return def;
  if (typeof def !== "string") return null;
  const hex = def.startsWith("#") ? def : NAMED[def.toLowerCase()];
  if (!hex) return null;
  const n = hex.length === 4 ? [...hex.slice(1)].map((c) => parseInt(c + c, 16)) : [1, 3, 5].map((i) => parseInt(hex.slice(i, i + 2), 16));
  return n.some(Number.isNaN) ? null : n;
}
const mix = (a, b, t) => a.map((x, i) => Math.round(x * (1 - t) + b[i] * t));
const hex = (c) => "#" + c.map((x) => x.toString(16).padStart(2, "0")).join("");
const luminance = ([r, g, b]) => (0.2126 * r + 0.7152 * g + 0.0722 * b) / 255;

// Each variable from the theme's keys. A "Default" colour (the terminal's
// own) is the editor's background or foreground; the few the editor has no
// key for are mixed from the ones it has.
const VARS = {
  // The page
  "--bg": (k) => k("editor.bg"),
  "--fg": (k) => k("editor.fg"),
  "--muted": (k, c) => mix(c.fg, c.bg, 0.4),
  "--faint": (k) => k("editor.line_number_fg", "fg"),
  "--border": (k, c) => mix(c.fg, c.bg, 0.82),
  "--panel": (k, c) => mix(k("ui.file_explorer_bg", "bg"), c.fg, 0.04),
  "--accent": (k) => k("ui.menu_highlight_bg", "fg"),
  "--on-accent": (k) => k("ui.menu_highlight_fg", "bg"),
  "--emphasis": (k) => k("ui.help_key_fg", "fg"),
  "--link": (k) => k("diagnostic.info_fg", "fg"),
  "--status-bg": (k) => k("ui.status_bar_bg", "bg"),
  "--status-fg": (k) => k("ui.status_bar_fg", "fg"),
  "--control-bg": (k) => k("ui.menu_bg", "bg"),
  "--control-fg": (k) => k("ui.menu_fg", "fg"),
  "--hover-bg": (k) => k("ui.menu_hover_bg", "bg"),
  "--sel-bg": (k) => k("ui.popup_selection_bg", "bg"),
  "--sel-fg": (k) => k("ui.popup_selection_fg", "fg"),
  "--sel-inactive-bg": (k, c) => mix(k("ui.popup_selection_bg", "bg"), c.bg, 0.5),
  "--popup-bg": (k) => k("ui.popup_bg", "bg"),
  "--popup-fg": (k) => k("ui.popup_text_fg", "fg"),
  "--popup-border": (k) => k("ui.popup_border_fg", "fg"),
  "--shadow": (k, c) => (luminance(c.bg) > 0.5 ? "rgba(0,0,0,.18)" : "rgba(0,0,0,.5)"),
  "--terminal-bg": (k) => k("ui.terminal_bg", "bg"),
  "--terminal-fg": (k) => k("ui.terminal_fg", "fg"),
  // The text
  "--caret": (k) => k("editor.cursor", "fg"),
  "--selection": (k) => k("editor.selection_bg", "bg"),
  "--current-line": (k) => k("editor.current_line_bg", "bg"),
  "--occurrence": (k) => k("ui.semantic_highlight_bg", "bg"),
  "--whitespace": (k) => k("editor.whitespace_indicator_fg", "fg"),
  "--ruler": (k) => k("editor.ruler_bg", "bg"),
  "--match-bg": (k) => k("search.match_bg", "bg"),
  "--match-fg": (k) => k("search.match_fg", "fg"),
  "--match-current-bg": (k) => k("search.current_match_bg", "bg"),
  "--match-current-fg": (k) => k("search.current_match_fg", "fg"),
  "--code-bg": (k) => k("ui.inline_code_bg", "bg"),
  "--tour-step-bg": (k) => k("ui.tour_step_bg", "bg"),
  "--diff-add-bg": (k) => k("editor.diff_add_bg", "bg"),
  "--diff-remove-bg": (k) => k("editor.diff_remove_bg", "bg"),
  "--git-added": (k) => k("ui.file_status_added_fg", "fg"),
  "--git-modified": (k) => k("ui.file_status_modified_fg", "fg"),
  "--git-deleted": (k) => k("ui.file_status_deleted_fg", "fg"),
  // Syntax
  "--syntax-keyword": (k) => k("syntax.keyword", "fg"),
  "--syntax-string": (k) => k("syntax.string", "fg"),
  "--syntax-comment": (k) => k("syntax.comment", "fg"),
  "--syntax-function": (k) => k("syntax.function", "fg"),
  "--syntax-type": (k) => k("syntax.type", "fg"),
  "--syntax-variable": (k) => k("syntax.variable", "fg"),
  "--syntax-constant": (k) => k("syntax.constant", "fg"),
  "--syntax-number": (k) => k("syntax.number", "fg"),
  "--syntax-operator": (k) => k("syntax.operator", "fg"),
  "--syntax-punctuation": (k) => k("syntax.punctuation_delimiter", "fg"),
  // Diagnostics
  "--error": (k) => k("diagnostic.error_fg", "fg"),
  "--warning": (k) => k("diagnostic.warning_fg", "fg"),
  "--info": (k) => k("diagnostic.info_fg", "fg"),
  "--hint": (k) => k("diagnostic.hint_fg", "fg"),
};

// A theme file (as `theme.get` serves it) as {"--var": css} for every
// variable.
export function themeVars(theme) {
  const at = (key) => key.split(".").reduce((o, p) => o?.[p], theme);
  // The page's own colours first: "Default" for those is the terminal's,
  // which xterm draws as white on black.
  const c = { bg: rgb(at("editor.bg")) || [0, 0, 0], fg: rgb(at("editor.fg")) || rgb("White") };
  const k = (key, fallback) => rgb(at(key)) || c[fallback];
  const out = {};
  for (const [name, f] of Object.entries(VARS)) {
    const v = f(k, c);
    out[name] = Array.isArray(v) ? hex(v) : v;
  }
  return out;
}

const STORE = "fresh-poc:theme";

export function activate(fresh) {
  const cache = new Map();
  const load = (key) => {
    if (!cache.has(key)) cache.set(key, fresh.host.theme(key).then(themeVars));
    return cache.get(key);
  };
  let current = "dark";
  // Applying in order: a slow fetch for an earlier preview never lands
  // after a later one.
  let shown = Promise.resolve();
  const show = (key) => (shown = shown.then(() => load(key)).then((vars) => fresh.ui.theme(vars)).catch((e) => fresh.ui.message(`Theme ${key}: ${e.message || e}`)));
  const choose = (key) => {
    current = key;
    fresh.settings.theme = key;
    try { localStorage.setItem(STORE, key); } catch {}
    return show(key);
  };
  fresh.events.on("app.ready", () => {
    let saved = null;
    try { saved = localStorage.getItem(STORE); } catch {}
    choose(saved || "dark");
  });
  fresh.commands.register("theme.select", {
    title: "Select Theme…",
    run: async () => {
      const themes = await fresh.host.themes();
      const before = current;
      fresh.ui.pick({
        title: "Theme",
        items: themes.map((t) => ({ label: t.name, detail: t.key === current ? "current" : "", value: t.key })),
        onChange: (it) => it && show(it.value),
        onAccept: (it) => choose(it.value),
        onCancel: () => show(before),
      });
    },
  });
}
