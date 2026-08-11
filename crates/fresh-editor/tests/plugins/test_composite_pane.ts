// Mounts a `pane` widget pointed at a **composite** buffer.
//
// A composite (the side-by-side diff shape) holds no text of its own — its
// content is the source buffers it names, laid into columns. A pane that
// sends it through the ordinary per-leaf renderer paints one empty leaf, so
// this plugin's whole job is to put a real composite behind a pane and let
// the test read what comes out.

/// <reference path="./lib/fresh.d.ts" />
import { col, FloatingWidgetPanel, labeledSection, pane } from "./lib/widgets.ts";

let panel: FloatingWidgetPanel | null = null;

registerHandler("composite_pane_open", async function () {
  // `entries` takes span objects, not a string — nothing inserts the
  // newlines for you.
  const left = await editor.createVirtualBuffer({
    name: "*LEFT*",
    entries: [{ text: "alpha\n" }, { text: "bravo\n" }],
  });
  const right = await editor.createVirtualBuffer({
    name: "*RIGHT*",
    entries: [{ text: "alpha\n" }, { text: "charlie\n" }],
  });
  const leftId = typeof left === "number" ? left : left.bufferId;
  const rightId = typeof right === "number" ? right : right.bufferId;

  const compositeId = await editor.createCompositeBuffer({
    name: "*Composite Under Test*",
    mode: "diff-view",
    layout: { type: "side-by-side", ratios: [0.5, 0.5], showSeparator: true },
    sources: [
      { bufferId: leftId, label: "LEFTHDR", editable: false },
      { bufferId: rightId, label: "RIGHTHDR", editable: false },
    ],
    // Without hunks the composite has no aligned rows and renders only its
    // headers. One hunk covering both files: line 0 is context, line 1
    // differs (bravo -> charlie).
    hunks: [{ oldStart: 0, oldCount: 2, newStart: 0, newCount: 2, ops: " -+" }],
  });

  const env = await editor.describeEnvironment();
  const windowId = env.activeWindowId;
  if (!panel) panel = new FloatingWidgetPanel();
  panel.mount(
    col(labeledSection({
      label: "composite",
      child: pane({ windowId, bufferId: compositeId, rows: 10, key: "c" }),
    })),
    { widthPct: 90, heightPct: 60 },
  );
});

editor.registerCommand(
  "Composite Pane Open",
  "mount a pane over a composite buffer",
  "composite_pane_open",
  null,
);
