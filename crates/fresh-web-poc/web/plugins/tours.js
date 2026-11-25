// Code tours: a walkthrough of a codebase, step by step, in the VS Code
// CodeTour format (`.tours/*.tour`: {title, steps: [{file, line,
// description, title?}]}). A panel beside the editor shows the steps; each
// step opens its file at its line and marks it.

import * as fresh from "../js/api.js";
import { h } from "../js/ui.js";

// The step's line.
/** const STEP: Style */
const STEP = { fg: null, bg: "ui.tour_step_bg", bold: false, italic: false, underline: false, strike: false, mono: false, extendToLineEnd: true };

// A .tour file (CodeTour): its steps, each at a file's line, a folder,
// or neither (words only).
/** type TourStep = {
 *   file: String | Undefined,
 *   line: Number | Undefined,
 *   directory: String | Undefined,
 *   title: String | Undefined,
 *   description: String | Undefined,
 * } */
/** type Tour = {title: String | Undefined, steps: TourStep[]} */
// The tour being taken, and the step it is at.
/** type Taking = {tour: Tour, step: Number} */

/** function label(s: TourStep) => String */
function label(s) {
  const where = s.file ? `${s.file}${s.line ? `:${s.line}` : ""}` : s.directory ? `${s.directory}/` : "";
  return s.title || where;
}

export function activate(app) {
  const root = fresh.root(app);
  /** let taking: Taking | Null */
  let taking = null;
  // The file holding the current step's mark.
  /** let marked: String | Null */
  let marked = null;
  const unmark = () => {
    const path = marked;
    marked = null;
    const b = path ? fresh.bufferByPath(app, path) : null;
    if (b) { b.decorations("tour").clear(); b.render(); }
  };

  const go = async (i) => {
    const now = taking;
    if (!now) return;
    const step = Math.max(0, Math.min(now.tour.steps.length - 1, i));
    taking = { tour: now.tour, step };
    const s = now.tour.steps[step];
    render();
    unmark();
    const file = s.file, line = s.line;
    if (!file) return;
    const path = `${root}/${file}`;
    const b = await fresh.open(app, path, { line: line || null, column: 1, place: "tab", preview: null, focus: true });
    const L = b ? b.lines() : null;
    if (!b || !L || !line) return;
    marked = path;
    const li = line - 1 - (L.firstLine ?? 0);
    if (li >= 0 && li < L.starts.length) {
      const d = b.decorations("tour");
      d.overlay(L.starts[li], L.starts[li] + fresh.byteLen(L.lines[li]) + 1, STEP, { when: "always", mark: false, above: false });
      d.gutter(L.starts[li], { glyph: "▶", color: "syntax.function", title: s.title || `Step ${step + 1}` });
    }
    b.render();
  };
  const render = () => {
    const now = taking;
    const panel = fresh.shown(app, "tour");
    if (!now || !panel || !panel.open) return;
    const { tour, step } = now;
    const s = tour.steps[step];
    panel.render(h("col", {}, [
      h("text", { bold: true }, [tour.title || "Tour"]),
      h("row", {}, [
        h("button", { onClick: () => { go(step - 1); }, disabled: step === 0 }, ["◀ Previous"]),
        h("text", { dim: true }, [`${step + 1} / ${tour.steps.length}`]),
        h("button", { primary: true, onClick: () => { go(step + 1); }, disabled: step >= tour.steps.length - 1 }, ["Next ▶"]),
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
      value: "",
      items: files.map((f) => ({ label: f.name.replace(/\.tour$/, ""), detail: "", value: f.path })),
      onAccept: async (it) => {
        /** const tour: Tour */
        const tour = JSON.parse(await fresh.read(app, it.value));
        taking = { tour, step: 0 };
        fresh.panel(app, { id: "tour", title: "Tour", place: "right", onClose: unmark });
        go(0);
      },
      onChange: (_it) => {},
      onCancel: () => {},
    });
  };
  fresh.register(app, "tour.open", { title: "Code Tour: Open…", hidden: false, run: open });
  // A step `d` on from the current one, when a tour is being taken.
  const move = (d) => { const now = taking; if (now) go(now.step + d); };
  fresh.register(app, "tour.next", { title: "Code Tour: Next Step", hidden: false, run: () => move(1) });
  fresh.register(app, "tour.prev", { title: "Code Tour: Previous Step", hidden: false, run: () => move(-1) });
}
