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

const isMarkdown = (b) => /\.(md|markdown)$/i.test(b.path || b.name || "");

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

export function activate(fresh) {
  const { h } = fresh;
  const B = fresh.text.byteLen;
  const W = fresh.text.width;

  // ---------------------------------------------------------------- inline markup

  // Conceal the markup of one line's inline spans: **bold**, *italic*,
  // `code`, ~~strike~~, [link](url), ![image](src). `at(i)` is the byte
  // offset of UTF-16 index i in the line.
  const inline = (d, text, at, reveal) => {
    const re = /(`+)([^`]+?)\1|\*\*([^*]+)\*\*|__([^_]+)__|(?<![\w*])\*([^*\s][^*]*?)\*(?!\w)|(?<![\w_])_([^_\s][^_]*?)_(?![\w])|~~([^~]+)~~|!\[([^\]]*)\]\(([^)]*)\)|\[([^\]]+)\]\(([^)]*)\)/g;
    for (let m; (m = re.exec(text)); ) {
      const s = m.index, e = s + m[0].length;
      const show = (a, b, text) => b > a && d.conceal(at(a), at(b), { text, style: PLAIN, reveal, when: "compose" });
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

  const decorate = (b) => {
    const d = b.decorations("compose");
    d.clear();
    const L = b.lines();
    if (!L || !b.whole) { b.render(); return; }
    const { lines, starts } = L;
    const { region, table } = classify(lines);
    const lineEnd = (i) => starts[i] + B(lines[i]);
    const reveal = (i) => ({ start: starts[i], end: lineEnd(i) + 1 });
    const atOf = (i) => {
      const t = lines[i];
      return (k) => starts[i] + B(t.slice(0, k));
    };
    const hide = (s, e, rv) => e > s && d.conceal(s, e, { text: "", style: PLAIN, reveal: rv, when: "compose" });
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
          const role = table[k].role;
          node(k, k, { type: "row", level: 0, depth: 0, marker: "", lang: "", align: [], header: role === "header", delimiter: role === "delimiter" });
          if (role === "delimiter") { hide(starts[k], lineEnd(k), rv); continue; }
          const at2 = atOf(k);
          const pipes = [];
          for (let x = 0; x < s.length; x++) if (s[x] === "|" && s[x - 1] !== "\\") pipes.push(x);
          // A cell starts at the line start (when no pipe leads) and at
          // every pipe with text after it; a closing pipe is markup.
          const bounds = !pipes.length || s.slice(0, pipes[0]).trim() ? [0, ...pipes] : pipes;
          const close = bounds.length > 1 && !s.slice(bounds[bounds.length - 1] + 1).trim() ? bounds.pop() : s.length;
          hide(at2(close), lineEnd(k), rv);
          bounds.forEach((a, p) => {
            const last = p + 1 === bounds.length;
            const z = last ? close : bounds[p + 1];
            const raw = s.slice(a, z);
            const lead = /^\|?\s*/.exec(raw)[0].length;
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
        hide(starts[i], at(QUOTE.exec(t)[0].length), reveal(i));
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
        const lead = qd ? QUOTE.exec(n)[0].length : n.length - n.trimStart().length;
        hide(starts[j], atOf(j)(lead), reveal(j));
        inline(d, n, atOf(j), reveal(j));
      }
      node(i, j, block);
      i = j;
    }
    b.render();
  };

  const timers = new Map();
  const composing = (b) => b.views().some((v) => v.mode === "compose");
  const schedule = (b, delay = 30) => {
    if (!isMarkdown(b) || !composing(b)) return;
    clearTimeout(timers.get(b.id));
    timers.set(b.id, setTimeout(() => decorate(b), delay));
  };
  fresh.events.on("buffer.changed", ({ buffer }) => schedule(buffer));
  fresh.events.on("buffer.activated", ({ buffer }) => schedule(buffer, 0));

  // ---------------------------------------------------------------- commands

  const toggle = () => {
    const v = fresh.views.active();
    if (!v || !isMarkdown(v.buffer)) return fresh.ui.message("Not a Markdown file");
    const on = v.mode !== "compose";
    v.setOptions({ mode: on ? "compose" : "source" });
    if (on) decorate(v.buffer);
    fresh.ui.message(`Markdown compose: ${on ? "on" : "off"}`);
  };
  const setWidth = async () => {
    const v = fresh.views.active();
    if (!v) return;
    const t = await fresh.ui.input({ title: "Compose width (columns, empty for the setting)", value: String(v.options.composeWidth || fresh.settings.page_width) });
    if (t == null) return;
    const n = parseInt(t, 10);
    v.setOptions({ composeWidth: n > 20 ? n : null });
  };
  const dual = () => {
    const v = fresh.views.active();
    if (!v || !isMarkdown(v.buffer)) return fresh.ui.message("Not a Markdown file");
    v.setOptions({ mode: "source" });
    fresh.commands.run("view.splitRight");
    const right = fresh.views.active();
    right.setOptions({ mode: "compose" });
    decorate(right.buffer);
  };

  // The contents: headings, in the sidebar; click to go there.
  let contents = null;
  const renderContents = () => {
    if (!contents || !contents.open) return;
    const b = fresh.buffers.active();
    const L = b && isMarkdown(b) ? b.lines() : null;
    const items = [];
    if (L) {
      const { region } = classify(L.lines);
      L.lines.forEach((t, i) => {
        const m = !region[i] && HEADING.exec(t);
        if (m) items.push({ level: m[1].length, text: t.slice(m[0].length).trim(), pos: L.starts[i] });
      });
    }
    contents.render(items.length
      ? h("list", {
        key: "toc",
        items,
        render: (it) => h("text", { bold: it.level === 1 }, ["  ".repeat(it.level - 1) + it.text]),
        onActivate: (it) => fresh.views.active()?.setCursor(it.pos, { center: true }),
        activateOnClick: true,
      })
      : h("text", { dim: true }, ["No headings"]));
  };
  const showContents = () => {
    contents = fresh.ui.panel({ id: "md-contents", title: "Contents", place: "sidebar", onClose: null });
    renderContents();
  };
  fresh.events.on("buffer.activated", renderContents);
  fresh.events.on("buffer.changed", () => { clearTimeout(timers.get("toc")); timers.set("toc", setTimeout(renderContents, 300)); });

  fresh.commands.register("markdown.toggleCompose", { title: "Markdown: Toggle Compose", hidden: false, run: toggle });
  fresh.commands.register("markdown.composeWidth", { title: "Markdown: Set Compose Width…", hidden: false, run: setWidth });
  fresh.commands.register("markdown.dual", { title: "Markdown: Source and Compose Side by Side", hidden: false, run: dual });
  fresh.commands.register("markdown.contents", { title: "Markdown: Contents", hidden: false, run: showContents });
}
