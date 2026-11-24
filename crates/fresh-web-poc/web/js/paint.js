// Styles as HTML: what the source rows (view.js) and the compose document
// (doc.js) share — syntax spans, a style's key and its inline CSS.

import { color, highlightLine, THEME } from "./highlight.js";

export const EMPTY = Object.freeze({});
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
    v = highlightLine(lang, text).map((x) => ({ s: x.s, e: x.e, style: Object.freeze({ fg: THEME[x.k], bold: x.bold, italic: x.italic }) }));
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
const FENCE = { bash: "shell", sh: "shell", shell: "shell", console: "shell", rust: "rust", rs: "rust", js: "javascript", javascript: "javascript", ts: "typescript", typescript: "typescript", python: "python", py: "python", json: "json", toml: "toml", go: "go", c: "c", cpp: "cpp" };
export const fenceLanguage = (info) => FENCE[(info || "").toLowerCase()] || null;

const ESC = { "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" };
export const esc = (t) => t.replace(/[&<>"]/g, (c) => ESC[c]);

// Theme keys and names resolve through the theme; CSS colours pass.
export const css = (x) => (typeof x === "string" && /^(#|rgb|var\()/.test(x) ? x : color(x));

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
