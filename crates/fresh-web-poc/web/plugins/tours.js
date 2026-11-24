// Code tours: a walkthrough of a codebase, step by step, in the VS Code
// CodeTour format (`.tours/*.tour`: {title, steps: [{file, line,
// description, title?}]}). A panel beside the editor shows the steps; each
// step opens its file at its line and marks it.

export function activate(fresh) {
  const { h } = fresh;
  const root = fresh.workspace.root;
  let tour = null, step = 0, panel = null;

  const go = async (i) => {
    if (!tour) return;
    step = Math.max(0, Math.min(tour.steps.length - 1, i));
    const s = tour.steps[step];
    render();
    if (!s.file) return;
    const b = await fresh.buffers.open(`${root}/${s.file}`, s.line ? { line: s.line } : undefined);
    if (b && s.line) {
      const L = b.lines();
      const li = s.line - 1 - (L.firstLine ?? 0);
      const d = b.decorations("tour");
      d.clear();
      if (li >= 0 && li < L.starts.length) {
        d.overlay(L.starts[li], L.starts[li] + fresh.text.byteLen(L.lines[li]) + 1, { bg: "#264f78", extendToLineEnd: true });
        d.gutter(L.starts[li], { glyph: "▶", color: "syntax.function", title: s.title || `Step ${step + 1}` });
      }
      b.render();
    }
  };
  const render = () => {
    if (!panel || !panel.open || !tour) return;
    const s = tour.steps[step];
    panel.render(h("col", {},
      h("text", { bold: true }, tour.title || "Tour"),
      h("row", {},
        h("button", { onClick: () => go(step - 1), disabled: step === 0 }, "◀ Previous"),
        h("text", { dim: true }, `${step + 1} / ${tour.steps.length}`),
        h("button", { primary: true, onClick: () => go(step + 1), disabled: step >= tour.steps.length - 1 }, "Next ▶")),
      h("text", { bold: true }, s.title || (s.file ? `${s.file}${s.line ? `:${s.line}` : ""}` : "")),
      h("box", { class: "ui-markdown" }, s.description || ""),
      h("list", {
        key: "steps",
        items: tour.steps.map((x, i) => ({ ...x, i })),
        selected: step,
        render: (x) => h("text", { mono: true }, `${x.i + 1}. ${x.title || (x.file ? `${x.file}:${x.line || 1}` : "")}`),
        onActivate: (x) => go(x.i),
        activateOnClick: true,
      })));
  };
  const open = async () => {
    let files = [];
    try { files = (await fresh.host.list(`${root}/.tours`)).filter((e) => e.name.endsWith(".tour")); } catch {}
    if (!files.length) return fresh.ui.message("No tours: add .tours/*.tour (CodeTour format)");
    fresh.ui.pick({
      title: "Code tour",
      items: files.map((f) => ({ label: f.name.replace(/\.tour$/, ""), value: f.path })),
      onAccept: async (it) => {
        tour = JSON.parse(await fresh.host.read(it.value));
        panel = fresh.ui.panel({ id: "tour", title: "Tour", place: "right" });
        go(0);
      },
    });
  };
  fresh.commands.register("tour.open", { title: "Code Tour: Open…", run: open });
  fresh.commands.register("tour.next", { title: "Code Tour: Next Step", run: () => go(step + 1) });
  fresh.commands.register("tour.prev", { title: "Code Tour: Previous Step", run: () => go(step - 1) });
}
