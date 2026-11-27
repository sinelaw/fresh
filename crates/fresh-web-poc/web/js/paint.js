// Styles as HTML: what the source rows (view.js) and the compose document
// (doc.js) share: a syntax span's style at an index, a style's key and its
// inline CSS.

import { color } from "./highlight.js";

/** const EMPTY: Drawn */
export const EMPTY = Object.freeze({ fg: "", bg: "", bold: false, italic: false, underline: false, strike: false, mono: false, markup: false, wavy: "" });
// The style of UTF-16 index `idx` among a line's syntax spans.
/** function synAt(spans: Span[], idx: Number) => Drawn */
export function synAt(spans, idx) {
  for (const x of spans) if (idx >= x.s && idx < x.e) return x.style;
  return EMPTY;
}


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
/** const cssCache: Map<String, String> */
const cssCache = new Map();
// The inline CSS for a style.
export function cssOf(st) { return cssFor(st, styleKey(st)); }
export function cssFor(st, key) {
  const hit = cssCache.get(key);
  if (hit !== undefined) return hit;
  const lines = [st.underline ? "underline" : "", st.strike ? "line-through" : ""].filter((x) => x !== "");
  const c = [
    st.markup ? "color:var(--faint);" : st.fg ? `color:${css(st.fg)};` : "",
    st.mono ? "font-family:var(--mono);font-size:.88em;background:var(--code-bg);border-radius:3px;" : "",
    st.bg ? `background:${css(st.bg)};` : "",
    st.bold ? "font-weight:bold;" : "",
    st.italic ? "font-style:italic;" : "",
    st.wavy ? `text-decoration:underline wavy ${st.wavy};` : lines.length ? `text-decoration:${lines.join(" ")};` : "",
  ].join("");
  cssCache.set(key, c);
  return c;
}
