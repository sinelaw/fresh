// Markdown compose: a Markdown buffer drawn as a document in a view in
// compose mode, while the text stays Markdown.
//
// Everything here is width-free structure, declared as decorations; the
// layout lays it out at each view's own width. So a paragraph's lines are
// *joined* (the line breaks concealed as spaces) and the layout wraps the
// result; a list item says its hanging indent; a quote its `▌` prefix; a
// code block its rails and rules, drawn edge to edge. Markup is concealed
// except on the line the cursor is on, where the source shows for editing.
// All of it applies only in compose views (`when: "compose"`): a source
// view of the same buffer shows the text as it is.

import { classify } from "../js/mdstruct.js";

const isMarkdown = (b) => /\.(md|markdown)$/i.test(b.path || b.name || "");

export function activate(fresh) {
  const { h } = fresh;
  const W = fresh.text.width;
  const B = fresh.text.byteLen;
  const C = { when: "compose" };
  const style = {
    h1: { fg: "syntax.function", bold: true },
    h: { fg: "syntax.keyword", bold: true },
    marker: { fg: "syntax.constant" },
    code: { fg: "syntax.string", bg: "#2b2b2b" },
    link: { fg: "syntax.link", underline: true },
    quote: { fg: "syntax.comment" },
    frame: { fg: "ui.split_separator_fg" },
    dim: { fg: "editor.line_number_fg" },
  };

  // ---------------------------------------------------------------- inline markup

  // Conceal the markup of one line's inline spans: **bold**, *italic*,
  // `code`, ~~strike~~, [link](url), ![image](src). `at(i)` is the byte
  // offset of UTF-16 index i in the line.
  const inline = (d, text, at, reveal) => {
    const re = /(`+)([^`]+?)\1|\*\*([^*]+)\*\*|__([^_]+)__|(?<![\w*])\*([^*\s][^*]*?)\*(?!\w)|(?<![\w_])_([^_\s][^_]*?)_(?![\w])|~~([^~]+)~~|!\[([^\]]*)\]\(([^)]*)\)|\[([^\]]+)\]\(([^)]*)\)/g;
    for (let m; (m = re.exec(text)); ) {
      const s = m.index, e = s + m[0].length;
      const hide = (a, b, repl = "") => b > a && d.conceal(at(a), at(b), { text: repl, reveal, ...C });
      if (m[1]) { // code
        hide(s, s + m[1].length); hide(e - m[1].length, e);
        d.overlay(at(s), at(e), style.code, C);
      } else if (m[3] || m[4]) { // bold
        hide(s, s + 2); hide(e - 2, e);
        d.overlay(at(s), at(e), { bold: true }, C);
      } else if (m[5] || m[6]) { // italic
        hide(s, s + 1); hide(e - 1, e);
        d.overlay(at(s), at(e), { italic: true }, C);
      } else if (m[7]) { // strike
        hide(s, s + 2); hide(e - 2, e);
        d.overlay(at(s), at(e), { strike: true }, C);
      } else if (m[8] != null) { // image
        hide(s, e, `🖼 ${m[8] || m[9]}`);
      } else if (m[10]) { // link: the text, underlined
        hide(s, s + 1);
        hide(s + 1 + m[10].length, e);
        d.overlay(at(s), at(e), style.link, C);
      }
    }
  };

  // ---------------------------------------------------------------- blocks

  const LIST = /^(\s*)([-*+]|\d+[.)])(\s+)(\[[ xX]\]\s+)?/;
  const QUOTE = /^(\s*>\s?)+/;
  const HEADING = /^(#{1,6})(\s+)/;
  const RULE = /^\s{0,3}([-*_])(\s*\1){2,}\s*$/;

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
    const blank = (i) => lines[i].trim() === "";

    for (let i = 0; i < lines.length; i++) {
      const t = lines[i];
      const at = atOf(i);

      // Fenced code: framed, the fences hidden.
      if (region[i] === "open") {
        let j = i + 1;
        while (j < lines.length && region[j] === "body") j++;
        const close = j < lines.length && region[j] === "close" ? j : null;
        const first = i + 1, last = (close ?? j) - 1;
        // The opening fence goes, line break and all; the frame's top edge
        // sits above the first body line (or above the fence, if empty).
        const topAt = first <= last ? starts[first] : starts[i];
        if (first <= last) d.conceal(starts[i], starts[first], { text: "", reveal: reveal(i), ...C });
        d.line(topAt, { above: true, rule: { left: "┌", fill: "─", right: "┐" }, style: style.frame, ...C });
        for (let k = first; k <= last; k++) {
          d.inline(starts[k], "│ ", { style: style.frame, ...C });
          d.inline(lineEnd(k), "│", { edge: true, style: style.frame, ...C });
          d.block(starts[k], lineEnd(k) + 1, { indent: 0, prefix: { text: "│ ", style: style.frame }, ...C });
        }
        if (close != null && first <= last) {
          // The closing fence goes with the line break before it.
          d.conceal(lineEnd(last), lineEnd(close), { text: "", reveal: { start: starts[close], end: lineEnd(close) + 1 }, ...C });
        }
        d.line(first <= last ? starts[last] : starts[i], { above: false, rule: { left: "└", fill: "─", right: "┘" }, style: style.frame, ...C });
        i = close ?? j - 1;
        continue;
      }

      // Tables: borders, pipes as box lines, cells padded to their column.
      if (table[i]?.role === "header") {
        let j = i;
        const rows = [];
        while (j < lines.length && table[j]) rows.push(j++);
        const cells = (k) => {
          const s = lines[k];
          const pipes = [];
          for (let x = 0; x < s.length; x++) if (s[x] === "|" && s[x - 1] !== "\\") pipes.push(x);
          if (!pipes.length || s.slice(0, pipes[0]).trim()) pipes.unshift(-1);
          if (s.slice(pipes[pipes.length - 1] + 1).trim()) pipes.push(s.length);
          const out = [];
          for (let p = 0; p + 1 < pipes.length; p++) {
            const a = pipes[p] + 1, z = pipes[p + 1];
            const raw = s.slice(a, z);
            const lead = raw.length - raw.trimStart().length;
            const text = raw.trim();
            out.push({ pipe: pipes[p], a: a + lead, z: a + lead + text.length, end: z, text });
          }
          return { pipes, out };
        };
        const parsed = rows.map(cells);
        const ncol = Math.max(...parsed.map((p) => p.out.length));
        const widths = new Array(ncol).fill(1);
        parsed.forEach((p, r) => { if (table[rows[r]].role !== "delimiter") p.out.forEach((c, k) => { widths[k] = Math.max(widths[k], W(c.text)); }); });
        const rule = (l, m, r) => l + widths.map((w) => "─".repeat(w + 2)).join(m) + r;
        d.line(starts[rows[0]], { above: true, text: rule("┌", "┬", "┐"), style: style.frame, ...C });
        parsed.forEach((p, r) => {
          const k = rows[r];
          const at2 = atOf(k);
          const rv = reveal(k);
          if (table[k].role === "delimiter") {
            d.conceal(starts[k], lineEnd(k), { text: rule("├", "┼", "┤"), style: style.frame, reveal: rv, ...C });
            return;
          }
          p.out.forEach((c, n) => {
            // From this cell's pipe to its text: "│ ".
            d.conceal(c.pipe < 0 ? at2(0) : at2(c.pipe), at2(c.a), { text: "│ ", style: style.frame, reveal: rv, ...C });
            if (c.pipe < 0 && c.a === 0) d.inline(at2(0), "│ ", { style: style.frame, ...C });
            // From the end of its text to the next pipe: padding.
            const pad = " ".repeat(widths[n] - W(c.text) + 1);
            if (c.end > c.z) d.conceal(at2(c.z), at2(c.end), { text: pad, reveal: rv, ...C });
            else d.inline(at2(c.z), pad, { side: "before", ...C });
            if (table[k].role === "header") d.overlay(at2(c.a), at2(c.z), { bold: true }, C);
            inline(d, lines[k], at2, rv);
          });
          // The last pipe.
          const lastPipe = p.pipes[p.pipes.length - 1];
          if (lastPipe < lines[k].length) d.conceal(at2(lastPipe), lineEnd(k), { text: "│", style: style.frame, reveal: rv, ...C });
          else d.inline(lineEnd(k), "│", { style: style.frame, ...C });
        });
        d.line(starts[rows[rows.length - 1]], { above: false, text: rule("└", "┴", "┘"), style: style.frame, ...C });
        i = j - 1;
        continue;
      }

      if (blank(i)) continue;

      // A horizontal rule.
      if (RULE.test(t)) {
        d.conceal(starts[i], lineEnd(i), { text: "", reveal: reveal(i), ...C });
        d.line(starts[i], { above: true, rule: { fill: "─" }, style: style.dim, ...C });
        continue;
      }

      // Headings.
      const hm = HEADING.exec(t);
      if (hm) {
        d.conceal(starts[i], at(hm[0].length), { text: "", reveal: reveal(i), ...C });
        d.overlay(starts[i], lineEnd(i), hm[1].length === 1 ? style.h1 : style.h, C);
        if (hm[1].length <= 2) d.line(starts[i], { above: false, rule: { fill: hm[1].length === 1 ? "═" : "─" }, style: style.dim, ...C });
        inline(d, t, at, reveal(i));
        continue;
      }

      // A paragraph, list item or quote: its block (a run of lines up to a
      // blank line, a new item or a different kind of line) is joined into
      // one, the layout wraps it, and continuation rows hang under the text.
      const li = LIST.exec(t);
      const qm = !li && QUOTE.exec(t);
      let indent = 0, prefix = null;
      if (li) {
        const bullet = li[4] ? (/x/i.test(li[4]) ? "☑ " : "☐ ") : /\d/.test(li[2]) ? li[2] + " " : "• ";
        const lead = li[1].replace(/\t/g, "    ");
        d.conceal(at(li[1].length), at(li[0].length), { text: bullet, style: style.marker, reveal: reveal(i), ...C });
        indent = W(lead) + W(bullet);
      } else if (qm) {
        const depth = (qm[0].match(/>/g) || []).length;
        prefix = { text: "▌ ".repeat(depth), style: style.quote };
        d.conceal(starts[i], at(qm[0].length), { text: prefix.text, style: style.quote, reveal: reveal(i), ...C });
        d.overlay(at(qm[0].length), lineEnd(i), style.quote, C);
        indent = W(prefix.text);
      } else if (/^\s+/.test(t)) {
        indent = W(/^\s+/.exec(t)[0].replace(/\t/g, "    "));
      }
      inline(d, t, at, reveal(i));
      // Join the lines that continue this block.
      let j = i;
      while (j + 1 < lines.length) {
        const n = lines[j + 1];
        if (blank(j + 1) || region[j + 1] || table[j + 1] || LIST.test(n) || HEADING.test(n) || RULE.test(n) || (qm ? !QUOTE.test(n) : QUOTE.test(n))) break;
        if (/(  |\\)$/.test(lines[j])) break; // a hard line break
        j++;
        const lead = qm ? QUOTE.exec(n)[0].length : n.length - n.trimStart().length;
        // The break and the next line's indent (or quote marker) become a space.
        const trail = lines[j - 1].length - lines[j - 1].trimEnd().length;
        d.conceal(lineEnd(j - 1) - trail, atOf(j)(lead), { text: " ", ...C });
        if (qm) d.overlay(atOf(j)(lead), lineEnd(j), style.quote, C);
        inline(d, n, atOf(j), reveal(j));
      }
      if (indent || prefix) d.block(starts[i], lineEnd(j) + 1, { indent, prefix, ...C });
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
        render: (it) => h("text", { bold: it.level === 1 }, "  ".repeat(it.level - 1) + it.text),
        onActivate: (it) => fresh.views.active()?.setCursor(it.pos, { center: true }),
        activateOnClick: true,
      })
      : h("text", { dim: true }, "No headings"));
  };
  const showContents = () => {
    contents = fresh.ui.panel({ id: "md-contents", title: "Contents", place: "sidebar" });
    renderContents();
  };
  fresh.events.on("buffer.activated", renderContents);
  fresh.events.on("buffer.changed", () => { clearTimeout(timers.get("toc")); timers.set("toc", setTimeout(renderContents, 300)); });

  fresh.commands.register("markdown.toggleCompose", { title: "Markdown: Toggle Compose", run: toggle });
  fresh.commands.register("markdown.composeWidth", { title: "Markdown: Set Compose Width…", run: setWidth });
  fresh.commands.register("markdown.dual", { title: "Markdown: Source and Compose Side by Side", run: dual });
  fresh.commands.register("markdown.contents", { title: "Markdown: Contents", run: showContents });
}
