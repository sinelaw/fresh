/// <reference path="../../plugins/lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Regression surface for the List keyboard-nav "clamped move still fires
 * a select event" bug.
 *
 * The host's `handle_widget_select_move_for_key` used to fire a
 * `widget_event` "select" on EVERY Up/Down — including a move clamped at
 * the list's top/bottom edge, where the selection doesn't actually move.
 * Holding ↓ against the bottom (or ↑ against the top) therefore spammed
 * the plugin with same-index selections (in the Orchestrator dock each
 * one scheduled a redundant live-switch). The Tree handler already
 * guarded this; the List handler did not.
 *
 * This plugin mounts a 3-item focusable List plus a `SELECTS=<n>` counter
 * line that ticks once per received `select` event. The e2e test arrows
 * to the bottom, keeps pressing ↓ into the boundary, and asserts the
 * counter stops climbing — a screen-observable proxy for "no spurious
 * select event fired" (CONTRIBUTING §2).
 */

const PANEL_ID = 882277; // arbitrary stable id for the float
const LIST_KEY = "clamp-list";

interface State {
  mounted: boolean;
  selects: number;
  idx: number;
}
const state: State = { mounted: false, selects: 0, idx: 0 };

// deno-lint-ignore no-explicit-any
function spec(): any {
  return {
    kind: "col",
    children: [
      {
        kind: "raw",
        entries: [{ text: `SELECTS=${state.selects}\n`, properties: {} }],
      },
      {
        kind: "list",
        items: [
          { text: "ITEM-A", properties: {} },
          { text: "ITEM-B", properties: {} },
          { text: "ITEM-C", properties: {} },
        ],
        itemSpecs: [],
        itemKeys: ["a", "b", "c"],
        selectedIndex: state.idx,
        visibleRows: 5,
        focusable: true,
        key: LIST_KEY,
      },
    ],
  };
}

function sel_mount(): void {
  state.mounted = true;
  state.selects = 0;
  state.idx = 0;
  editor.mountFloatingWidget(PANEL_ID, spec(), 60, 40);
  editor.widgetMutate(PANEL_ID, { kind: "setFocusKey", widgetKey: LIST_KEY });
  editor.setStatus("TestSel: MOUNTED");
}
registerHandler("sel_mount", sel_mount);

editor.on("widget_event", (e) => {
  if (!state.mounted || e.panel_id !== PANEL_ID) return;
  if (e.event_type !== "select") return;
  const payload = (e.payload ?? {}) as Record<string, unknown>;
  const idx = payload.index;
  if (typeof idx === "number") state.idx = idx;
  state.selects += 1;
  editor.updateFloatingWidget(PANEL_ID, spec());
});

editor.registerCommand(
  "TestSel: Mount",
  "Mount a focusable List with a select-event counter",
  "sel_mount",
  null,
);
