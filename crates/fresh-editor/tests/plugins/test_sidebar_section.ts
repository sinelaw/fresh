/// <reference path="../../plugins/lib/fresh.d.ts" />
const editor = getEditor();

/**
 * The e2e fixture for `sidebar_sections.rs`: a plugin that mounts one
 * small `List` panel as a sidebar section under the file explorer.
 *
 * `SidebarTest: Mount` calls `mountSidebarSection` with four requested
 * rows and `startBlurred` so the explorer keeps the keyboard; the test
 * then drives the section's header with the mouse and asserts on the
 * rendered column.
 */

const PANEL_ID = 7;

// deno-lint-ignore no-explicit-any
function spec(): any {
  return {
    kind: "list",
    items: [
      { text: "alpha", properties: {} },
      { text: "beta", properties: {} },
      { text: "gamma", properties: {} },
    ],
    item_keys: ["alpha", "beta", "gamma"],
    selected_index: 0,
    focusable: true,
    key: "outline",
  };
}

function sidebar_test_mount(): void {
  editor.mountSidebarSection(PANEL_ID, spec(), "Outline", 4, {
    startBlurred: true,
  });
}
registerHandler("sidebar_test_mount", sidebar_test_mount);

editor.registerCommand(
  "SidebarTest: Mount",
  "Mount the test sidebar section under the file explorer",
  "sidebar_test_mount",
  null,
);
