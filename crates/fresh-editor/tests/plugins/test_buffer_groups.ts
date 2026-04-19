/// <reference path="../../plugins/lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Test plugin for buffer groups — specifically for verifying that keyboard
 * input and mouse clicks route to the correct inner-panel buffer when a
 * group tab is active.
 *
 * The plugin creates a buffer group with two scrollable panels (LEFT and
 * RIGHT), populates each with a distinctive marker string, and registers
 * commands that report which buffer currently holds the focus via the
 * status message. A test can then:
 *
 *   1. Run "TestBG: Create" to open the group.
 *   2. Run "TestBG: Which" (or press the bound key) to check which panel
 *      is focused — the status message shows "FOCUS=LEFT" / "FOCUS=RIGHT".
 *   3. Click on one of the panels, then repeat step 2 to verify the click
 *      routed focus correctly.
 */

interface State {
  groupId: number | null;
  panels: Record<string, number>;
}

const state: State = { groupId: null, panels: {} };

function tbg_create(): void {
  if (state.groupId !== null) {
    editor.setStatus("TestBG: already open");
    return;
  }
  const layout = JSON.stringify({
    type: "split",
    direction: "h", // horizontal split = side-by-side
    ratio: 0.5,
    first: { type: "scrollable", id: "left" },
    second: { type: "scrollable", id: "right" },
  });
  editor.createBufferGroup("*TestBG*", "test-bg", layout).then((result) => {
    state.groupId = result.groupId;
    state.panels = result.panels;
    // Populate each panel with a distinctive marker.
    editor.setVirtualBufferContent(state.panels["left"], [
      { text: "LEFT-PANEL-MARKER\n", properties: {} },
      { text: "left line 2\n", properties: {} },
      { text: "left line 3\n", properties: {} },
    ]);
    editor.setVirtualBufferContent(state.panels["right"], [
      { text: "RIGHT-PANEL-MARKER\n", properties: {} },
      { text: "right line 2\n", properties: {} },
      { text: "right line 3\n", properties: {} },
    ]);
    editor.setStatus("TestBG: opened");
  });
}
registerHandler("tbg_create", tbg_create);

/**
 * Report which panel's buffer is the currently active buffer via the status
 * message. The test reads the status bar to assert focus routing.
 *
 * Each call includes a monotonic `seq=N` suffix so tests can distinguish a
 * freshly-computed result from a stale one left over from a previous call;
 * without this, `wait_until(contains "TestBG: FOCUS=")` would return
 * immediately on the old status line before the new invocation has run.
 */
let whichSeq = 0;
function tbg_which(): void {
  whichSeq++;
  const activeId = editor.getActiveBufferId();
  let focus: string;
  if (activeId === state.panels["left"]) {
    focus = "LEFT";
  } else if (activeId === state.panels["right"]) {
    focus = "RIGHT";
  } else {
    focus = `OTHER(${activeId})`;
  }
  editor.setStatus(`TestBG: FOCUS=${focus} seq=${whichSeq}`);
}
registerHandler("tbg_which", tbg_which);

/**
 * Close the group.
 */
function tbg_close(): void {
  if (state.groupId !== null) {
    editor.closeBufferGroup(state.groupId);
    state.groupId = null;
    state.panels = {};
    editor.setStatus("TestBG: closed");
  }
}
registerHandler("tbg_close", tbg_close);

// Use raw strings (not translation keys) to avoid needing an i18n file.
editor.registerCommand("TestBG: Create", "Open a 2-panel test buffer group", "tbg_create", null);
editor.registerCommand("TestBG: Which", "Report which panel's buffer is active", "tbg_which", null);
editor.registerCommand("TestBG: Close", "Close the test buffer group", "tbg_close", null);
