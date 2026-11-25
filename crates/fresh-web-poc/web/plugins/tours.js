// Code tours: a walkthrough of a codebase, step by step, in the VS Code
// CodeTour format (`.tours/*.tour`: {title, steps: [{file, line,
// description, title?}]}). A panel beside the editor shows the steps; each
// step opens its file at its line and marks it.

import * as fresh from "../js/api.js";
import { h } from "../js/ui.js";

// The step's line.
/** const STEP: Style */
const STEP = { fg: null, bg: "ui.tour_step_bg", bold: false, italic: false, underline: false, strike: false, mono: false, extendToLineEnd: true };

export function activate(app) {
  const root = fresh.root(app);
  let tour = null, step = 0, panel = null;
  let marked = null; // the buffer holding the current step's mark
  const unmark = () => { if (marked) { marked.decorations("tour").clear(); marked.render(); marked = null; } };
  const label = (s) => s.title || (s.file ? `${s.file}${s.line ? `:${s.line}` : ""}` : s.directory ? `${s.directory}/` : "");

  const go = async (i) => {
    if (!tour) return;
    step = Math.max(0, Math.min(tour.steps.length - 1, i));
    const s = tour.steps[step];
    render();
    unmark();
    if (!s.file) return;
    const b = await fresh.open(app, `${root}/${s.file}`, { line: s.line || null, column: 1, place: "tab", preview: null, focus: true });
    if (b && s.line) {
      const L = b.lines();
      const li = s.line - 1 - (L.firstLine ?? 0);
      const d = b.decorations("tour");
      marked = b;
      if (li >= 0 && li < L.starts.length) {
        d.overlay(L.starts[li], L.starts[li] + fresh.byteLen(L.lines[li]) + 1, STEP, { when: "always", mark: false, above: false });
        d.gutter(L.starts[li], { glyph: "▶", color: "syntax.function", title: s.title || `Step ${step + 1}` });
      }
      b.render();
    }
  };
  const render = () => {
    if (!panel || !panel.open || !tour) return;
    const s = tour.steps[step];
    panel.render(h("col", {}, [
      h("text", { bold: true }, [tour.title || "Tour"]),
      h("row", {}, [
        h("button", { onClick: () => go(step - 1), disabled: step === 0 }, ["◀ Previous"]),
        h("text", { dim: true }, [`${step + 1} / ${tour.steps.length}`]),
        h("button", { primary: true, onClick: () => go(step + 1), disabled: step >= tour.steps.length - 1 }, ["Next ▶"]),
      ]),
      h("text", { bold: true }, [label(s)]),
      h("markdown", { text: s.description || "" }),
      h("list", {
        key: "steps",
        items: tour.steps.map((x, i) => ({ ...x, i })),
        selected: step,
        render: (x) => h("text", { mono: true }, [`${x.i + 1}. ${label(x)}`]),
        onActivate: (x) => go(x.i),
        activateOnClick: true,
      }),
    ]));
  };
  const open = async () => {
    let files = [];
    try { files = (await fresh.list(app, `${root}/.tours`)).filter((e) => e.name.endsWith(".tour")); } catch {}
    if (!files.length) return fresh.message(app, "No tours: add .tours/*.tour (CodeTour format)");
    fresh.pick(app, {
      title: "Code tour",
      items: files.map((f) => ({ label: f.name.replace(/\.tour$/, ""), value: f.path })),
      onAccept: async (it) => {
        tour = JSON.parse(await fresh.read(app, it.value));
        panel = fresh.panel(app, { id: "tour", title: "Tour", place: "right", onClose: unmark });
        go(0);
      },
    });
  };
  fresh.register(app, "tour.open", { title: "Code Tour: Open…", hidden: false, run: open });
  fresh.register(app, "tour.next", { title: "Code Tour: Next Step", hidden: false, run: () => go(step + 1) });
  fresh.register(app, "tour.prev", { title: "Code Tour: Previous Step", hidden: false, run: () => go(step - 1) });
}
