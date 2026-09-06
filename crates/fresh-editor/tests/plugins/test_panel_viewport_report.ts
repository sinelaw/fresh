/// <reference path="../../plugins/lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Test plugin for the geometry a buffer group's panels report.
 *
 * A panel that starts hidden is laid out for the first time when it is
 * *shown*, and that first layout is the only moment its size is news: a
 * side panel never resizes on its own afterwards. A plugin that lays its
 * own rows out to the panel width (the review diff's FILES sidebar) is
 * stuck with a guess until it hears that first `viewport_changed`.
 *
 * The plugin opens a group whose `side` panel starts hidden, records every
 * `viewport_changed` it receives per panel, and reports the recorded width
 * on demand: `TestVP: side=<width> main=<width>`, with `-1` for a panel
 * the host has said nothing about.
 */

interface State {
  groupId: number | null;
  panels: Record<string, number>;
  widths: Record<string, number>;
}

const state: State = { groupId: null, panels: {}, widths: {} };

editor.on("viewport_changed", (data: { buffer_id: number; width: number }) => {
  for (const name of Object.keys(state.panels)) {
    if (state.panels[name] === data.buffer_id) state.widths[name] = data.width;
  }
});

function tvp_create(): void {
  if (state.groupId !== null) {
    editor.setStatus("TestVP: already open");
    return;
  }
  // The review diff's shape: a fixed strip across the top, then a side
  // panel beside the main one. Hiding `side` has to leave the group with
  // more than one leaf, or the whole inner tree collapses to a single
  // pane mid-frame — a different (host-side) rough edge than the one
  // under test.
  const layout = JSON.stringify({
    type: "split",
    direction: "v",
    ratio: 0.05,
    first: { type: "fixed", id: "strip", height: 1 },
    second: {
      type: "split",
      direction: "h",
      ratio: 0.25,
      first: { type: "scrollable", id: "side", scrollable: false },
      second: {
        type: "split",
        direction: "h",
        ratio: 0.8,
        first: { type: "scrollable", id: "main" },
        second: { type: "scrollable", id: "rail", scrollable: false },
      },
    },
  });
  editor.createBufferGroup("*TestVP*", "test-vp", layout).then((result) => {
    state.groupId = result.groupId;
    state.panels = result.panels;
    editor.setVirtualBufferContent(state.panels["side"], [
      { text: "SIDE-PANEL-MARKER\n", properties: {} },
    ]);
    editor.setVirtualBufferContent(state.panels["main"], [
      { text: "MAIN-PANEL-MARKER\n", properties: {} },
    ]);
    editor.setVirtualBufferContent(state.panels["strip"], [
      { text: "STRIP-MARKER\n", properties: {} },
    ]);
    // The panel is hidden before the group is ever drawn — the shape the
    // review diff opens with.
    editor.setBufferGroupPanelVisible(result.groupId, "side", false);
    editor.setBufferGroupPanelVisible(result.groupId, "rail", false);
    // The review does this too: the keys go to the main pane, not to a
    // panel that is not on screen.
    editor.focusBufferGroupPanel(result.groupId, "main");
    editor.setStatus("TestVP: opened");
  });
}
registerHandler("tvp_create", tvp_create);

function tvp_show(): void {
  if (state.groupId === null) return;
  editor.setBufferGroupPanelVisible(state.groupId, "side", true);
  editor.setStatus("TestVP: shown");
}
registerHandler("tvp_show", tvp_show);

/** Report the widths recorded so far. `seq` distinguishes a fresh answer
 *  from the one left on the status line by the previous call. */
let reportSeq = 0;
function tvp_report(): void {
  reportSeq++;
  const side = state.widths["side"] ?? -1;
  const main = state.widths["main"] ?? -1;
  editor.setStatus(`TestVP: side=${side} main=${main} seq=${reportSeq}`);
}
registerHandler("tvp_report", tvp_report);

editor.registerCommand("TestVP: Create", "Open a group whose side panel starts hidden", "tvp_create", null);
editor.registerCommand("TestVP: Show", "Show the group's hidden side panel", "tvp_show", null);
editor.registerCommand("TestVP: Report", "Report the panel widths the host reported", "tvp_report", null);
