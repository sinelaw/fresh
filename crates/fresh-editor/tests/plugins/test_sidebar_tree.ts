/// <reference path="../../plugins/lib/fresh.d.ts" />
const editor = getEditor();

/**
 * The e2e fixture for `sidebar_focus_cycle.rs`: a plugin that mounts one
 * small `Tree` panel as a sidebar section under the file explorer.
 *
 * A tree rather than a list because, in the sidebar, a tree's selected
 * row wears the explorer's `▌` (design §5.1) whether or not the tree has
 * the keyboard, and the host moves the selection on Down / Up only when
 * it does — which is what the test probes.
 */

const PANEL_ID = 8;

// deno-lint-ignore no-explicit-any
function spec(): any {
  return {
    kind: "tree",
    key: "outline",
    nodes: [
      { text: { text: "alpha" }, depth: 0, hasChildren: false },
      { text: { text: "beta" }, depth: 0, hasChildren: false },
      { text: { text: "gamma" }, depth: 0, hasChildren: false },
    ],
    itemKeys: ["alpha", "beta", "gamma"],
    selectedIndex: 0,
    expandedKeys: [],
    checkable: false,
    itemHeight: 1,
    cardBorders: false,
    indentCols: 1,
  };
}

function sidebar_tree_mount(): void {
  editor.mountSidebarSection(PANEL_ID, spec(), "Outline", 4, {
    startBlurred: true,
  });
}
registerHandler("sidebar_tree_mount", sidebar_tree_mount);

editor.registerCommand(
  "SidebarTree: Mount",
  "Mount the test sidebar tree under the file explorer",
  "sidebar_tree_mount",
  null,
);
