/// <reference path="../../plugins/lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Reproducer for issue #2035: the Orchestrator-style preview pane
 * (`windowEmbed` inside a floating widget) failed to render virtual
 * buffer GROUPS (the layout primitive that user-visible plugins like
 * `git_log` rely on). The host's `render_session_preview_into_rect`
 * passed an empty `grouped_subtrees` map to the split renderer, so
 * the group's panel layout couldn't be resolved and the embed fell
 * through to rendering the split's underlying (pre-group) buffer.
 *
 * This plugin gives the e2e test a deterministic surface:
 *   1. `TestPrev: Setup` — opens a 2-panel buffer group with
 *      distinctive markers (mirrors `test_buffer_groups.ts`).
 *   2. `TestPrev: Mount` — mounts a near-fullscreen floating widget
 *      whose only contents is a `windowEmbed` pointing at the
 *      current window. With the bug, the embed renders the
 *      underlying file buffer; with the fix, it renders the buffer
 *      group's panel content (markers visible).
 *
 * The float is mounted at 100% width / 100% height so the underlying
 * editor area is fully cleared inside the float — only what the
 * embed paints is visible there. That makes the test assertion
 * unambiguous: marker present ↔ the embed correctly resolved the
 * group.
 */

interface State {
  groupId: number | null;
  panels: Record<string, number>;
}

const state: State = { groupId: null, panels: {} };
const PANEL_ID = 991919; // arbitrary stable id for the float

async function prev_setup(): Promise<void> {
  if (state.groupId !== null) {
    editor.setStatus("TestPrev: already set up");
    return;
  }
  const layout = JSON.stringify({
    type: "split",
    direction: "h",
    ratio: 0.5,
    first: { type: "scrollable", id: "left" },
    second: { type: "scrollable", id: "right" },
  });
  const result = await editor.createBufferGroup(
    "*TestPrev*",
    "test-prev",
    layout,
  );
  state.groupId = result.groupId;
  state.panels = result.panels;
  editor.setVirtualBufferContent(state.panels["left"], [
    { text: "ISSUE2035-LEFT-MARKER\n", properties: {} },
    { text: "left line 2\n", properties: {} },
  ]);
  editor.setVirtualBufferContent(state.panels["right"], [
    { text: "ISSUE2035-RIGHT-MARKER\n", properties: {} },
    { text: "right line 2\n", properties: {} },
  ]);
  editor.setStatus("TestPrev: SETUP_DONE");
}
registerHandler("prev_setup", prev_setup);

function prev_mount(): void {
  // windowEmbed pointing back at the current window. The host's
  // floating-widget-with-embed path bypasses the
  // `PreviewWindowInRect` same-window guard (see `render.rs` near
  // the `preview_window_id` swap), so this renders the active
  // window into the embed even when it's the only window.
  const winId = editor.activeWindow();
  const spec = {
    kind: "col",
    children: [
      {
        kind: "windowEmbed",
        windowId: winId,
        rows: 20,
        key: "issue-2035-embed",
      },
    ],
  };
  editor.mountFloatingWidget(PANEL_ID, spec, 100, 100);
  editor.setStatus("TestPrev: MOUNTED");
}
registerHandler("prev_mount", prev_mount);

function prev_unmount(): void {
  editor.unmountFloatingWidget(PANEL_ID);
  editor.setStatus("TestPrev: UNMOUNTED");
}
registerHandler("prev_unmount", prev_unmount);

editor.registerCommand(
  "TestPrev: Setup",
  "Open a 2-panel buffer group with markers",
  "prev_setup",
  null,
);
editor.registerCommand(
  "TestPrev: Mount",
  "Mount a floating windowEmbed of the current window",
  "prev_mount",
  null,
);
editor.registerCommand(
  "TestPrev: Unmount",
  "Tear down the floating widget",
  "prev_unmount",
  null,
);
