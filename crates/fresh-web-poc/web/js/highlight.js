// Colours and a small line-local syntax highlighter.
//
// Every colour is a CSS variable (`style.css` holds the dark defaults; a
// theme sets them all, `plugins/themes.js`). THEME maps the theme keys
// plugins pass as `fg`/`bg` strings ("syntax.keyword", "editor.fg", ...) to
// those variables, so whatever is drawn with a key follows a theme change
// without being drawn again. The highlighter is a stand-in for the editor's
// grammars: a regex lexer per language that colours keywords, strings,
// comments, numbers and markdown markup, line by line (a block comment's
// continuation lines are not tracked).

const v = (name) => `var(--${name})`;
export const THEME = Object.freeze({
  "editor.fg": v("fg"),
  "editor.bg": v("bg"),
  "editor.line_number_fg": v("faint"),
  "editor.selection_bg": v("selection"),
  "editor.current_line_bg": v("current-line"),
  "editor.occurrence_bg": v("occurrence"),
  "editor.whitespace_indicator_fg": v("whitespace"),
  "editor.diff_add_bg": v("diff-add-bg"),
  "editor.diff_remove_bg": v("diff-remove-bg"),
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
  "syntax.link": v("link"),
  "diagnostic.error": v("error"),
  "diagnostic.warning": v("warning"),
  "diagnostic.info": v("info"),
  "diagnostic.hint": v("hint"),
  "search.match_bg": v("match-bg"),
  "search.match_fg": v("match-fg"),
  "search.current_bg": v("match-current-bg"),
  "search.current_fg": v("match-current-fg"),
  "git.added": v("git-added"),
  "git.modified": v("git-modified"),
  "git.deleted": v("git-deleted"),
});
// Colour names (as themes and plugins write them) in the terminal's
// palette, so a named colour looks the same in the editor and the terminal.
export const NAMED = {
  black: "#000000", red: "#cd3131", green: "#0dbc79", yellow: "#e5e510", blue: "#2472c8", magenta: "#bc3fbc",
  cyan: "#11a8cd", gray: "#808080", grey: "#808080", darkgray: "#666666", lightred: "#f14c4c",
  lightgreen: "#23d18b", lightyellow: "#f5f543", lightblue: "#3b8eea", lightmagenta: "#d670d6",
  lightcyan: "#29b8db", white: "#e5e5e5",
};

// ANSI 256-colour palette entry as CSS.
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
export function color(spec) {
  if (spec == null) return null;
  if (Array.isArray(spec)) return `rgb(${spec[0]},${spec[1]},${spec[2]})`;
  if (typeof spec !== "string") return null;
  if (spec in THEME) return THEME[spec];
  if (spec.startsWith("#") || spec.startsWith("var(")) return spec;
  if (spec.startsWith("Indexed:")) return ansi(+spec.slice(8));
  const n = NAMED[spec.toLowerCase()];
  if (n) return n;
  // An unknown theme key in a known family falls back to the family's colour.
  if (spec.startsWith("syntax.")) return THEME["syntax.keyword"];
  return null;
}

const KW = {
  rust: "as async await break const continue crate dyn else enum extern false fn for if impl in let loop match mod move mut pub ref return self Self static struct super trait true type unsafe use where while",
  javascript: "async await break case catch class const continue default delete do else export extends false finally for from function if import in instanceof let new null of return static super switch this throw true try typeof undefined var void while yield",
  python: "and as assert async await break class continue def del elif else except False finally for from global if import in is lambda None nonlocal not or pass raise return True try while with yield",
  go: "break case chan const continue default defer else fallthrough for func go goto if import interface map package range return select struct switch type var nil true false",
  c: "auto break case char const continue default do double else enum extern float for goto if inline int long register return short signed sizeof static struct switch typedef union unsigned void volatile while bool true false nullptr class public private protected namespace template typename using virtual",
  shell: "if then else elif fi for while do done case esac in function return local export echo",
  toml: "true false",
  json: "true false null",
};
KW.typescript = KW.javascript + " interface type enum implements declare readonly as keyof any number string boolean never unknown";
KW.cpp = KW.c;
const kwSets = Object.fromEntries(Object.entries(KW).map(([k, v]) => [k, new Set(v.split(" "))]));

const COMMENT = { rust: "//", javascript: "//", typescript: "//", go: "//", c: "//", cpp: "//", python: "#", shell: "#", toml: "#", yaml: "#" };

// Spans [{s, e, k}] in UTF-16 indices; k is a theme key.
export function highlightLine(lang, text) {
  const out = [];
  if (!lang) return out;
  if (lang === "markdown") return markdown(text);
  if (lang === "diff") {
    const k = /^(\+\+\+|---)/.test(text) ? "syntax.keyword" : text[0] === "+" ? "git.added" : text[0] === "-" ? "git.deleted" : text.startsWith("@@") ? "syntax.type" : /^(commit|diff --git|index )/.test(text) ? "syntax.constant" : null;
    return k ? [{ s: 0, e: text.length, k }] : [];
  }
  const kws = kwSets[lang];
  const cm = COMMENT[lang];
  let i = 0;
  while (i < text.length) {
    const c = text[i];
    if (cm && text.startsWith(cm, i)) { out.push({ s: i, e: text.length, k: "syntax.comment" }); break; }
    if ((lang !== "python" && lang !== "shell") && text.startsWith("/*", i)) {
      const j = text.indexOf("*/", i + 2);
      const e = j < 0 ? text.length : j + 2;
      out.push({ s: i, e, k: "syntax.comment" });
      i = e;
      continue;
    }
    if (c === '"' || c === "'" || c === "`") {
      if (lang === "rust" && c === "'" && /^'[a-z_]+\b[^']/.test(text.slice(i, i + 20))) { i++; continue; }
      let j = i + 1;
      while (j < text.length && text[j] !== c) j += text[j] === "\\" ? 2 : 1;
      out.push({ s: i, e: Math.min(text.length, j + 1), k: "syntax.string" });
      i = j + 1;
      continue;
    }
    if (/[0-9]/.test(c) && (i === 0 || !/[\w]/.test(text[i - 1]))) {
      const m = /^[0-9][0-9a-fA-FxX_.]*/.exec(text.slice(i));
      out.push({ s: i, e: i + m[0].length, k: "syntax.number" });
      i += m[0].length;
      continue;
    }
    if (/[A-Za-z_]/.test(c)) {
      const m = /^[A-Za-z_][\w]*/.exec(text.slice(i));
      const w = m[0];
      const next = text[i + w.length];
      if (kws && kws.has(w)) out.push({ s: i, e: i + w.length, k: "syntax.keyword" });
      else if (next === "(" || (next === "!" && lang === "rust")) out.push({ s: i, e: i + w.length, k: "syntax.function" });
      else if (/^[A-Z]/.test(w) && lang !== "json") out.push({ s: i, e: i + w.length, k: "syntax.type" });
      i += w.length;
      continue;
    }
    i++;
  }
  return out;
}

function markdown(text) {
  const out = [];
  let m;
  if ((m = /^(\s*#{1,6}\s)(.*)$/.exec(text))) {
    out.push({ s: 0, e: text.length, k: "syntax.keyword", bold: true });
    return out;
  }
  if (/^\s*(```|~~~)/.test(text)) { out.push({ s: 0, e: text.length, k: "syntax.comment" }); return out; }
  if ((m = /^(\s*)([-*+]|\d+[.)])\s/.exec(text))) out.push({ s: m[1].length, e: m[0].length - 1, k: "syntax.constant" });
  if ((m = /^(\s*>)/.exec(text))) out.push({ s: 0, e: m[1].length, k: "syntax.comment" });
  const re = /(`[^`]+`)|(\*\*[^*]+\*\*)|(\[[^\]]*\]\([^)]*\))|(\*[^*\s][^*]*\*|_[^_\s][^_]*_)/g;
  while ((m = re.exec(text))) {
    const k = m[1] ? "syntax.string" : m[2] ? "syntax.type" : m[3] ? "syntax.link" : "syntax.variable";
    out.push({ s: m.index, e: m.index + m[0].length, k, bold: !!m[2], italic: !!m[4] });
  }
  return out;
}
