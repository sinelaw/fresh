// Colours.
//
// Every colour is a CSS variable (`style.css` holds the dark defaults; a
// theme sets them all, `plugins/themes.js`). THEME maps the theme keys
// plugins pass as `fg`/`bg` strings ("syntax.keyword", "editor.fg", ...) to
// those variables, so whatever is drawn with a key follows a theme change
// without being drawn again. Syntax colours come from the grammar engine
// (hl.js) as these keys.

const v = (name) => `var(--${name})`;
/** const THEME_KEYS: Dict<String> */
const THEME_KEYS = {
  "editor.fg": v("fg"),
  "editor.bg": v("bg"),
  "editor.line_number_fg": v("faint"),
  "editor.selection_bg": v("selection"),
  "editor.current_line_bg": v("current-line"),
  "editor.occurrence_bg": v("occurrence"),
  "editor.bracket_match_fg": v("bracket-match"),
  "editor.bracket_rainbow_1": v("bracket-rainbow-1"),
  "editor.bracket_rainbow_2": v("bracket-rainbow-2"),
  "editor.bracket_rainbow_3": v("bracket-rainbow-3"),
  "editor.bracket_rainbow_4": v("bracket-rainbow-4"),
  "editor.bracket_rainbow_5": v("bracket-rainbow-5"),
  "editor.bracket_rainbow_6": v("bracket-rainbow-6"),
  "editor.whitespace_indicator_fg": v("whitespace"),
  "editor.diff_add_bg": v("diff-add-bg"),
  "editor.diff_remove_bg": v("diff-remove-bg"),
  "editor.diff_modify_bg": v("diff-modify-bg"),
  "ui.split_separator_fg": v("border"),
  "ui.status_bar_bg": v("status-bg"),
  "ui.status_bar_fg": v("status-fg"),
  "ui.tour_step_bg": v("tour-step-bg"),
  "ui.terminal_bg": v("terminal-bg"),
  "ui.terminal_fg": v("terminal-fg"),
  "ui.accent_fg": v("emphasis"),
  "syntax.keyword": v("syntax-keyword"),
  "syntax.string": v("syntax-string"),
  "syntax.comment": v("syntax-comment"),
  "syntax.number": v("syntax-number"),
  "syntax.constant": v("syntax-constant"),
  "syntax.type": v("syntax-type"),
  "syntax.function": v("syntax-function"),
  "syntax.variable": v("syntax-variable"),
  "syntax.operator": v("syntax-operator"),
  "syntax.punctuation": v("syntax-punctuation"),
  "syntax.punctuation_bracket": v("syntax-punctuation-bracket"),
  "syntax.punctuation_delimiter": v("syntax-punctuation-delimiter"),
  "syntax.property": v("syntax-property"),
  "syntax.attribute": v("syntax-attribute"),
  "syntax.variable_builtin": v("syntax-variable-builtin"),
  "syntax.link": v("link"),
  "diagnostic.error": v("error"),
  "diagnostic.warning": v("warning"),
  "diagnostic.info": v("info"),
  "diagnostic.hint": v("hint"),
  "search.match_bg": v("match-bg"),
  "search.match_fg": v("match-fg"),
  "search.current_bg": v("match-current-bg"),
  "search.current_fg": v("match-current-fg"),
  "search.scope_bg": v("search-scope-bg"),
  "git.added": v("git-added"),
  "git.modified": v("git-modified"),
  "git.deleted": v("git-deleted"),
};
export const THEME = Object.freeze(THEME_KEYS);
// Colour names (as themes and plugins write them) in the terminal's
// palette, so a named colour looks the same in the editor and the terminal.
/** const NAMED: Dict<String> */
export const NAMED = {
  black: "#000000", red: "#cd3131", green: "#0dbc79", yellow: "#e5e510", blue: "#2472c8", magenta: "#bc3fbc",
  cyan: "#11a8cd", gray: "#808080", grey: "#808080", darkgray: "#666666", lightred: "#f14c4c",
  lightgreen: "#23d18b", lightyellow: "#f5f543", lightblue: "#3b8eea", lightmagenta: "#d670d6",
  lightcyan: "#29b8db", white: "#e5e5e5",
};

// ANSI 256-colour palette entry as CSS.
/** function ansi(i: Number) => String */
export function ansi(i) {
  const base = ["#000000", "#cd3131", "#0dbc79", "#e5e510", "#2472c8", "#bc3fbc", "#11a8cd", "#e5e5e5",
    "#666666", "#f14c4c", "#23d18b", "#f5f543", "#3b8eea", "#d670d6", "#29b8db", "#ffffff"];
  if (i < 16) return base[i];
  if (i < 232) {
    const n = i - 16, r = Math.floor(n / 36), g = Math.floor(n / 6) % 6, b = n % 6;
    const v = (x) => (x ? 55 + x * 40 : 0);
    return `rgb(${v(r)},${v(g)},${v(b)})`;
  }
  const g = 8 + (i - 232) * 10;
  return `rgb(${g},${g},${g})`;
}

// An overlay colour spec — [r,g,b], a theme key, a colour name or
// "Indexed:N" — as CSS, or null.
/** function color(spec: Color | Null | Undefined) => String | Null */
export function color(spec) {
  if (spec == null) return null;
  if (typeof spec !== "string") return `rgb(${spec[0]},${spec[1]},${spec[2]})`;
  const t = THEME[spec];
  if (t) return t;
  if (spec.startsWith("#") || spec.startsWith("var(")) return spec;
  if (spec.startsWith("Indexed:")) return ansi(Number(spec.slice(8)));
  const n = NAMED[spec.toLowerCase()];
  if (n) return n;
  return null;
}
