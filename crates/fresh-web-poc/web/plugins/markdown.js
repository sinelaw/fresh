// Markdown compose: a Markdown buffer drawn as a document in a view in
// compose mode, while the text stays Markdown.
//
// The plugin declares structure; the view draws it. Each block is a node
// (heading, paragraph, list item, quote, code, table with rows and cells,
// rule) and inline spans are overlays (bold, italic, strike, links, `mono`
// for code). Markup (`# `, list markers, `> `, fences, `**`, backticks, link
// targets, table pipes) is concealed with `reveal` on its line, so the
// source shows, dimmed inside the typography, where the cursor is. All of
// it applies only in compose views (`when: "compose"`): a source view of the
// same buffer shows the text as it is.

import { classify } from "../js/mdstruct.js";
import * as fresh from "../js/api.js";
import * as ui from "../js/ui.js";

/** const isMarkdown: (BufferHandle) => Boolean */
const isMarkdown = (b) => b.language() === "markdown";

// Styles: the markup itself is not restyled; inline spans.
/** const PLAIN: Style */
const PLAIN = { fg: null, bg: null, bold: false, italic: false, underline: false, strike: false, mono: false, extendToLineEnd: false };
/** const CODE: Style */
const CODE = { fg: null, bg: null, bold: false, italic: false, underline: false, strike: false, mono: true, extendToLineEnd: false };
/** const BOLD: Style */
const BOLD = { fg: null, bg: null, bold: true, italic: false, underline: false, strike: false, mono: false, extendToLineEnd: false };
/** const ITALIC: Style */
const ITALIC = { fg: null, bg: null, bold: false, italic: true, underline: false, strike: false, mono: false, extendToLineEnd: false };
/** const STRIKE: Style */
const STRIKE = { fg: null, bg: null, bold: false, italic: false, underline: false, strike: true, mono: false, extendToLineEnd: false };
/** const LINK: Style */
const LINK = { fg: "syntax.link", bg: null, bold: false, italic: false, underline: true, strike: false, mono: false, extendToLineEnd: false };
// Every overlay applies in compose views only.
/** const COMPOSE: OverlayOptions */
const COMPOSE = { when: "compose", mark: false, above: false };

// How long a match is (0: none).
/** function matchLength(m: String[] | Null) => Number */
function matchLength(m) { return m ? m[0].length : 0; }

export function activate(app) {
  const B = fresh.byteLen;
  const W = fresh.textWidth;

  // ---------------------------------------------------------------- inline markup

  // Conceal the markup of one line's inline spans: **bold**, *italic*,
  // `code`, ~~strike~~, [link](url), ![image](src). `at(i)` is the byte
  // offset of UTF-16 index i in the line.
  /** const inline: (DecorationSet, String, (Number) => Number, Range | Null) => Undefined */
  const inline = (d, text, at, reveal) => {
    const re = /(`+)([^`]+?)\1|\*\*([^*]+)\*\*|__([^_]+)__|(?<![\w*])\*([^*\s][^*]*?)\*(?!\w)|(?<![\w_])_([^_\s][^_]*?)_(?![\w])|~~([^~]+)~~|!\[([^\]]*)\]\(([^)]*)\)|\[([^\]]+)\]\(([^)]*)\)/g;
    for (;;) {
      const m = re.exec(text);
      if (!m) break;
      const e = re.lastIndex, s = e - m[0].length;
      const show = (a, b, text) => { if (b > a) d.conceal(at(a), at(b), { text, style: PLAIN, reveal, when: "compose" }); };
      const hide = (a, b) => show(a, b, "");
      if (m[1]) { // code
        hide(s, s + m[1].length); hide(e - m[1].length, e);
        d.overlay(at(s + m[1].length), at(e - m[1].length), CODE, COMPOSE);
      } else if (m[3] || m[4]) { // bold
        hide(s, s + 2); hide(e - 2, e);
        d.overlay(at(s), at(e), BOLD, COMPOSE);
      } else if (m[5] || m[6]) { // italic
        hide(s, s + 1); hide(e - 1, e);
        d.overlay(at(s), at(e), ITALIC, COMPOSE);
      } else if (m[7]) { // strike
        hide(s, s + 2); hide(e - 2, e);
        d.overlay(at(s), at(e), STRIKE, COMPOSE);
      } else if (m[8] != null) { // image
        show(s, e, `🖼 ${m[8] || m[9]}`);
      } else if (m[10]) { // link: the text, underlined
        hide(s, s + 1);
        hide(s + 1 + m[10].length, e);
        d.overlay(at(s + 1), at(s + 1 + m[10].length), LINK, COMPOSE);
      }
    }
  };

  // ---------------------------------------------------------------- blocks

  const LIST = /^(\s*)([-*+]|\d+[.)])(\s+)(\[[ xX]\]\s+)?/;
  const QUOTE = /^(\s*>\s?)+/;
  const HEADING = /^(#{1,6})(\s+)/;
  const RULE = /^\s{0,3}([-*_])(\s*\1){2,}\s*$/;
  const HARD = /( {2,}|\\)$/;
  const quoteDepth = (t) => { const m = QUOTE.exec(t); return m ? (m[0].match(/>/g) || []).length : 0; };
  const alignOf = (c) => (c.startsWith(":") && c.endsWith(":") ? "center" : c.endsWith(":") ? "right" : c.startsWith(":") ? "left" : null);

  /** const decorate: (BufferHandle) => Undefined */
  const decorate = (b) => {
    const d = b.decorations("compose");
    d.clear();
    const L = b.lines();
    if (!L || !b.whole()) { b.render(); return; }
    const { lines, starts } = L;
    const { region, table } = classify(lines);
    const lineEnd = (i) => starts[i] + B(lines[i]);
    /** const reveal: (Number) => Range | Null */
    const reveal = (i) => ({ start: starts[i], end: lineEnd(i) + 1 });
    const atOf = (i) => {
      const t = lines[i];
      return (k) => starts[i] + B(t.slice(0, k));
    };
    /** const hide: (Number, Number, Range | Null) => Undefined */
    const hide = (s, e, rv) => { if (e > s) d.conceal(s, e, { text: "", style: PLAIN, reveal: rv, when: "compose" }); };
    // Lines i..j as a block (a Block: every field, those its type doesn't use
    // at their defaults).
    const node = (i, j, block) => d.node(starts[i], lineEnd(j), block, "compose");
    const blank = (i) => lines[i].trim() === "";
    // Indentations of the open list items, outermost first.
    let listStack = [];

    for (let i = 0; i < lines.length; i++) {
      const t = lines[i];
      const at = atOf(i);
      if (!LIST.test(t) && !blank(i)) listStack = [];

      // Fenced code: the fence lines hidden, the body kept line by line.
      if (region[i] === "open") {
        let j = i + 1;
        while (j < lines.length && region[j] === "body") j++;
        const last = j < lines.length && region[j] === "close" ? j : j - 1;
        const info = (/^\s*(?:`{3,}|~{3,})\s*([^\s`{]*)/.exec(t) || [])[1] || "";
        node(i, last, { type: "code", level: 0, depth: 0, marker: "", lang: info.toLowerCase(), align: [], header: false, delimiter: false });
        hide(starts[i], lineEnd(i), reveal(i));
        if (region[last] === "close") hide(starts[last], lineEnd(last), reveal(last));
        i = last;
        continue;
      }

      // Tables: a row per line, cells split at the pipes.
      if (table[i]?.role === "header") {
        let j = i;
        while (j < lines.length && table[j]) j++;
        const align = lines[i + 1].split("|").map((c) => c.trim()).filter(Boolean).map(alignOf);
        node(i, j - 1, { type: "table", level: 0, depth: 0, marker: "", lang: "", align, header: false, delimiter: false });
        for (let k = i; k < j; k++) {
          const s = lines[k];
          const rv = reveal(k);
          const row = table[k];
          const role = row === undefined ? "row" : row.role;
          node(k, k, { type: "row", level: 0, depth: 0, marker: "", lang: "", align: [], header: role === "header", delimiter: role === "delimiter" });
          if (role === "delimiter") { hide(starts[k], lineEnd(k), rv); continue; }
          const at2 = atOf(k);
          const pipes = [];
          for (let x = 0; x < s.length; x++) if (s[x] === "|" && s[x - 1] !== "\\") pipes.push(x);
          // A cell starts at the line start (when no pipe leads) and at
          // every pipe with text after it; a closing pipe is markup.
          const cuts = !pipes.length || s.slice(0, pipes[0]).trim() !== "" ? [0, ...pipes] : pipes;
          const closed = cuts.length > 1 && s.slice(cuts[cuts.length - 1] + 1).trim() === "";
          const close = closed ? cuts[cuts.length - 1] : s.length;
          const bounds = closed ? cuts.slice(0, -1) : cuts;
          hide(at2(close), lineEnd(k), rv);
          bounds.forEach((a, p) => {
            const last = p + 1 === bounds.length;
            const z = last ? close : bounds[p + 1];
            const raw = s.slice(a, z);
            const lead = matchLength(/^\|?\s*/.exec(raw));
            const trail = raw.length - raw.trimEnd().length;
            d.node(at2(a), last ? lineEnd(k) : at2(z), { type: "cell", level: 0, depth: 0, marker: "", lang: "", align: [], header: false, delimiter: false }, "compose");
            hide(at2(a), at2(a + lead), rv);
            if (z - trail > a + lead) hide(at2(z - trail), at2(z), rv);
          });
          inline(d, s, at2, rv);
        }
        i = j - 1;
        continue;
      }

      if (blank(i)) continue;

      if (RULE.test(t)) {
        node(i, i, { type: "rule", level: 0, depth: 0, marker: "", lang: "", align: [], header: false, delimiter: false });
        hide(starts[i], lineEnd(i), reveal(i));
        continue;
      }

      const hm = HEADING.exec(t);
      if (hm) {
        node(i, i, { type: "heading", level: hm[1].length, depth: 0, marker: "", lang: "", align: [], header: false, delimiter: false });
        hide(starts[i], at(hm[0].length), reveal(i));
        inline(d, t, at, reveal(i));
        continue;
      }

      // A paragraph, list item or quote: its lines, up to a blank line, a
      // new item or a different kind of line, are joined.
      const li = LIST.exec(t);
      const qd = li ? 0 : quoteDepth(t);
      let block;
      if (li) {
        const lead = W(li[1].replace(/\t/g, "    "));
        while (listStack.length && listStack[listStack.length - 1] > lead) listStack.pop();
        if (!listStack.length || listStack[listStack.length - 1] < lead) listStack.push(lead);
        const done = li[4] && /x/i.test(li[4]);
        const marker = li[4] ? (done ? "☑" : "☐") : /\d/.test(li[2]) ? li[2] : "•";
        block = { type: "item", level: 0, depth: listStack.length - 1, marker, lang: "", align: [], header: false, delimiter: false };
        hide(starts[i], at(li[0].length), reveal(i));
      } else if (qd) {
        block = { type: "quote", level: 0, depth: qd, marker: "", lang: "", align: [], header: false, delimiter: false };
        hide(starts[i], at(matchLength(QUOTE.exec(t))), reveal(i));
      } else block = { type: "paragraph", level: 0, depth: 0, marker: "", lang: "", align: [], header: false, delimiter: false };
      inline(d, t, at, reveal(i));
      let j = i;
      while (j + 1 < lines.length) {
        const n = lines[j + 1];
        if (blank(j + 1) || region[j + 1] || table[j + 1] || LIST.test(n) || HEADING.test(n) || RULE.test(n) || quoteDepth(n) !== qd) break;
        // A line ending in two spaces or a backslash breaks there; other
        // trailing blanks go, and the line break draws as a space.
        const prev = lines[j], pe = lineEnd(j);
        const hard = HARD.exec(prev);
        const trail = hard ? hard[0].length : prev.length - prev.trimEnd().length;
        if (trail) hide(pe - B(prev.slice(prev.length - trail)), pe, reveal(j));
        if (hard) d.conceal(pe, pe + 1, { text: "\n", style: PLAIN, reveal: null, when: "compose" });
        j++;
        // The next line's indent (or quote marker) is markup too.
        const lead = qd ? matchLength(QUOTE.exec(n)) : n.length - n.trimStart().length;
        hide(starts[j], atOf(j)(lead), reveal(j));
        inline(d, n, atOf(j), reveal(j));
      }
      node(i, j, block);
      i = j;
    }
    b.render();
  };

  const later = fresh.debouncer();
  const composing = (b) => b.views().some((v) => v.mode() === "compose");
  const schedule = (b, delay) => {
    if (!isMarkdown(b) || !composing(b)) return;
    later(String(b.id), delay, () => { decorate(b); });
  };
  fresh.onBuffer(app, "buffer.changed", (buffer) => schedule(buffer, 30));
  fresh.onBuffer(app, "buffer.activated", (buffer) => schedule(buffer, 0));

  // ---------------------------------------------------------------- commands

  const toggle = () => {
    const v = fresh.activeView(app);
    if (!v || !isMarkdown(v.buffer)) return fresh.message(app, "Not a Markdown file");
    const on = v.mode() !== "compose";
    v.setMode(on ? "compose" : "source");
    if (on) decorate(v.buffer);
    fresh.message(app, `Markdown compose: ${on ? "on" : "off"}`);
  };
  const setWidth = async () => {
    const v = fresh.activeView(app);
    if (!v) return;
    const t = await fresh.input(app, { title: "Compose width (columns, empty for the setting)", value: String(v.composeWidth() ?? fresh.settings(app).page_width), hint: "", select: true });
    if (t == null) return;
    const n = parseInt(t, 10);
    v.setComposeWidth(n > 20 ? n : null);
  };
  const dual = () => {
    const v = fresh.activeView(app);
    if (!v || !isMarkdown(v.buffer)) return fresh.message(app, "Not a Markdown file");
    v.setMode("source");
    fresh.run(app, "view.splitRight");
    const right = fresh.activeView(app);
    if (right === null) return;
    right.setMode("compose");
    decorate(right.buffer);
  };

  // The contents: headings, in the sidebar; click to go there.
  const renderContents = () => {
    const contents = fresh.shown(app, "md-contents");
    if (!contents || !contents.open) return;
    const b = fresh.activeBuffer(app);
    const L = b && isMarkdown(b) ? b.lines() : null;
    const items = [];
    if (L) {
      const { region } = classify(L.lines);
      L.lines.forEach((t, i) => {
        const m = region[i] ? null : HEADING.exec(t);
        if (m) items.push({ level: m[1].length, text: t.slice(m[0].length).trim(), pos: L.starts[i] });
      });
    }
    contents.render(items.length
      ? ui.list({
        ...ui.listProps("toc", items, (it) => ui.text({ ...ui.TEXT, bold: it.level === 1 }, "  ".repeat(it.level - 1) + it.text)),
        onActivate: (it) => fresh.activeView(app)?.setCursor(it.pos, { center: true }),
        activateOnClick: true,
      })
      : ui.text({ ...ui.TEXT, dim: true }, "No headings"));
  };
  const showContents = () => {
    fresh.panel(app, { id: "md-contents", title: "Contents", place: "sidebar", onClose: null });
    renderContents();
  };
  fresh.onBuffer(app, "buffer.activated", (_b) => { renderContents(); });
  fresh.onBuffer(app, "buffer.changed", (_b) => { later("toc", 300, renderContents); });

  fresh.register(app, "markdown.toggleCompose", { title: "Markdown: Toggle Compose", hidden: false, run: toggle });
  fresh.register(app, "markdown.composeWidth", { title: "Markdown: Set Compose Width…", hidden: false, run: setWidth });
  fresh.register(app, "markdown.dual", { title: "Markdown: Source and Compose Side by Side", hidden: false, run: dual });
  fresh.register(app, "markdown.contents", { title: "Markdown: Contents", hidden: false, run: showContents });
}
