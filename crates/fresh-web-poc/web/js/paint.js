// Styles as HTML: what the source rows (view.js) and the compose document
// (doc.js) share — syntax spans, a style's key and its inline CSS.

import { color, highlightLine, THEME } from "./highlight.js";

// A style as drawn: CSS colours ("" for none) and attributes. `mono` is
// inline code in a document; `markup` revealed Markdown source, drawn dim;
// `wavy` a diagnostic's underline colour ("" for none).
/** type Drawn = {
 *   fg: String,
 *   bg: String,
 *   bold: Boolean,
 *   italic: Boolean,
 *   underline: Boolean,
 *   strike: Boolean,
 *   mono: Boolean,
 *   markup: Boolean,
 *   wavy: String,
 * } */

/** const EMPTY: Drawn */
export const EMPTY = Object.freeze({ fg: "", bg: "", bold: false, italic: false, underline: false, strike: false, mono: false, markup: false, wavy: "" });
export const NO_SPANS = [];

// Syntax spans by language and line text: a line's colours depend only on
// its text (the highlighter is line-local), so scrolling and editing reuse
// them. Each span carries its finished style.
const spanCache = new Map();
const cssCache = new Map();
export function spansFor(lang, text) {
  if (!lang) return NO_SPANS;
  const key = lang + "\u0000" + text;
  let v = spanCache.get(key);
  if (!v) {
    v = highlightLine(lang, text).map((x) => ({ s: x.s, e: x.e, style: Object.freeze({ fg: THEME[x.k] ?? "", bg: "", bold: x.bold, italic: x.italic, underline: false, strike: false, mono: false, markup: false, wavy: "" }) }));
    if (spanCache.size > 50000) spanCache.clear();
    spanCache.set(key, v);
  }
  return v;
}
// The style of UTF-16 index `idx` among a line's syntax spans.
export function synAt(spans, idx) {
  for (const x of spans) if (idx >= x.s && idx < x.e) return x.style;
  return EMPTY;
}

// A fenced code block's info string to the highlighter's language.
/** const FENCE: Dict<String> */
const FENCE = { bash: "shell", sh: "shell", shell: "shell", console: "shell", rust: "rust", rs: "rust", js: "javascript", javascript: "javascript", ts: "typescript", typescript: "typescript", python: "python", py: "python", json: "json", toml: "toml", go: "go", c: "c", cpp: "cpp" };
export const fenceLanguage = (info) => FENCE[(info || "").toLowerCase()] || null;

/** const ESC: Dict<String> */
const ESC = { "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" };
export const esc = (t) => t.replace(/[&<>"]/g, (c) => ESC[c] ?? c);

// Theme keys and names resolve through the theme; CSS colours pass.
/** const css: (Color | Null) => String | Null */
export const css = (x) => (typeof x === "string" && /^(#|rgb|var\()/.test(x) ? x : color(x));

// A decoration's Style (types/fresh.d.js) over a drawn style: its colours
// where it has them, resolved, and the attributes it turns on.
/** function restyler(y: Style) => (Drawn) => Drawn */
export function restyler(y) {
  const fg = color(y.fg), bg = color(y.bg);
  return (st) => ({
    fg: fg || st.fg, bg: bg || st.bg,
    bold: y.bold || st.bold, italic: y.italic || st.italic, underline: y.underline || st.underline, strike: y.strike || st.strike, mono: y.mono || st.mono,
    markup: st.markup, wavy: st.wavy,
  });
}
// A decoration's Style drawn on its own (inline text, a virtual line, a
// conceal's text), its colours resolved.
/** function drawnOf(s: Style) => Drawn */
export function drawnOf(s) {
  return {
    fg: css(s.fg) || "", bg: css(s.bg) || "",
    bold: s.bold, italic: s.italic, underline: s.underline, strike: s.strike, mono: s.mono,
    markup: false, wavy: "",
  };
}
// A drawn style with a diagnostic's wavy underline; as revealed markup.
/** function withWavy(st: Drawn, wavy: String) => Drawn */
export function withWavy(st, wavy) {
  return { fg: st.fg, bg: st.bg, bold: st.bold, italic: st.italic, underline: st.underline, strike: st.strike, mono: st.mono, markup: st.markup, wavy };
}
/** function asMarkup(st: Drawn) => Drawn */
export function asMarkup(st) {
  return { fg: st.fg, bg: st.bg, bold: st.bold, italic: st.italic, underline: st.underline, strike: st.strike, mono: st.mono, markup: true, wavy: st.wavy };
}

export function styleKey(s) {
  return `${s.fg || ""}|${s.bg || ""}|${s.bold ? 1 : 0}${s.italic ? 1 : 0}${s.underline ? 1 : 0}${s.strike ? 1 : 0}${s.mono ? 1 : 0}${s.markup ? 1 : 0}|${s.wavy || ""}`;
}

// The inline CSS for a style, cached by its key. `mono` is inline code in a
// document; `markup` is revealed Markdown source, drawn dim.
export function cssFor(st, key = styleKey(st)) {
  let c = cssCache.get(key);
  if (c == null) {
    c = "";
    if (st.markup) c += "color:var(--faint);";
    else if (st.fg) c += `color:${css(st.fg)};`;
    if (st.mono) c += "font-family:var(--mono);font-size:.88em;background:var(--code-bg);border-radius:3px;";
    if (st.bg) c += `background:${css(st.bg)};`;
    if (st.bold) c += "font-weight:bold;";
    if (st.italic) c += "font-style:italic;";
    if (st.wavy) c += `text-decoration:underline wavy ${st.wavy};`;
    else if (st.underline || st.strike) c += `text-decoration:${[st.underline && "underline", st.strike && "line-through"].filter(Boolean).join(" ")};`;
    cssCache.set(key, c);
  }
  return c;
}
