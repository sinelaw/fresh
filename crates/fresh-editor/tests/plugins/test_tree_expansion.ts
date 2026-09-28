/// <reference path="../../plugins/lib/fresh.d.ts" />
const editor = getEditor();

/**
 * The e2e fixture for `tree_expansion_owner.rs`: a floating panel holding
 * two `Tree`s — a plain one and a `cardBorders` one — whose spec is sent
 * once, at mount, and never again.
 *
 * That is the point. A tree's `expandedKeys` is a seed; after the first
 * frame the host owns expansion, and a plugin changes it with
 * `setExpandedKeys` rather than by re-sending the spec. So nothing here
 * re-sends: Enter on a row answers with `setExpandedKeys` alone (the way
 * the Markdown table of contents answers its `expand` event), and an
 * `expand` from a disclosure click or →/← is only reported. Every answer
 * ends in a status line, which is what the test waits on before it reads
 * the panel.
 */

const PANEL_ID = 734105;

// deno-lint-ignore no-explicit-any
function tree(key: string, prefix: string, cardBorders: boolean): any {
  return {
    kind: "tree",
    key,
    nodes: [
      { text: { text: `${prefix}-root` }, depth: 0, hasChildren: true },
      { text: { text: `${prefix}-child` }, depth: 1, hasChildren: false },
      { text: { text: `${prefix}-sibling` }, depth: 0, hasChildren: false },
    ],
    itemKeys: [`${prefix}-r`, `${prefix}-c`, `${prefix}-s`],
    selectedIndex: 0,
    visibleRows: cardBorders ? 9 : 3,
    // Collapsed, and never re-sent: every later change is the host's.
    expandedKeys: [],
    checkable: false,
    itemHeight: 1,
    cardBorders,
    indentCols: 2,
  };
}

// deno-lint-ignore no-explicit-any
function spec(): any {
  return {
    kind: "col",
    children: [
      tree("plain", "plain", false),
      tree("cards", "card", true),
    ],
  };
}

/** What the plugin believes is open, per tree — mirrored from `expand`. */
const opened: Record<string, Set<string>> = { plain: new Set(), cards: new Set() };

function tree_expansion_mount(): void {
  opened.plain.clear();
  opened.cards.clear();
  editor.mountFloatingWidget(PANEL_ID, spec(), 60, 60);
  editor.widgetMutate(PANEL_ID, { kind: "setFocusKey", widgetKey: "plain" });
  editor.setStatus("TreeExp: MOUNTED");
}
registerHandler("tree_expansion_mount", tree_expansion_mount);

editor.on("widget_event", (e) => {
  if (e.panel_id !== PANEL_ID) return;
  const widget = e.widget_key;
  if (widget !== "plain" && widget !== "cards") return;
  const payload = (e.payload ?? {}) as Record<string, unknown>;
  const key = typeof payload.key === "string" ? payload.key : "";
  if (e.event_type === "expand") {
    if (payload.expanded === true) opened[widget].add(key);
    else opened[widget].delete(key);
    editor.setStatus(`TreeExp: EXPAND ${key} ${payload.expanded === true ? "open" : "shut"}`);
    return;
  }
  if (e.event_type === "activate") {
    // Enter flips the row through the mutator, never a spec re-send.
    if (opened[widget].has(key)) opened[widget].delete(key);
    else opened[widget].add(key);
    editor.widgetMutate(PANEL_ID, {
      kind: "setExpandedKeys",
      widgetKey: widget,
      keys: Array.from(opened[widget]),
    });
    editor.setStatus(`TreeExp: SET ${key} ${opened[widget].has(key) ? "open" : "shut"}`);
  }
});

editor.registerCommand(
  "TreeExp: Mount",
  "Mount a plain and a card tree whose spec is never re-sent",
  "tree_expansion_mount",
  null,
);
