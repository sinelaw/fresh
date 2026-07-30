/// <reference path="../../plugins/lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Coverage surface for plugin-visible widget hover events.
 *
 * A button with `hoverable: true` gets `widget_event { event_type:
 * "hover", payload: { hovered } }` as the pointer enters and leaves it,
 * so a plugin can drive its own affordance (a tooltip, a reveal-on-hover
 * action) rather than being limited to the host's built-in `bare`-button
 * highlight. Opting in is per widget because each transition is a plugin
 * round-trip — which is the other half of what this plugin proves: the
 * panel's second button does NOT set `hoverable`, so pointing at it must
 * deliver nothing.
 *
 * The panel renders both counters and the live hover state into a `raw`
 * row, so the e2e test can drive the mouse and assert purely on rendered
 * output (CONTRIBUTING §2).
 */

const PANEL_ID = 913377;
const WATCHED_KEY = "watched-btn";
const QUIET_KEY = "quiet-btn";

interface State {
  mounted: boolean;
  /** `hover` events received for the opted-in button. */
  enters: number;
  leaves: number;
  /** `hover` events received for any widget that did NOT opt in. */
  stray: number;
}
const state: State = { mounted: false, enters: 0, leaves: 0, stray: 0 };

// deno-lint-ignore no-explicit-any
function spec(): any {
  return {
    kind: "col",
    children: [
      {
        kind: "raw",
        entries: [{
          text:
            `ENTERS=${state.enters} LEAVES=${state.leaves} STRAY=${state.stray}\n`,
          properties: {},
        }],
      },
      {
        kind: "button",
        label: "WATCHED",
        focused: false,
        intent: "normal",
        disabled: false,
        focusable: true,
        bare: false,
        hoverable: true,
        key: WATCHED_KEY,
      },
      {
        kind: "button",
        label: "QUIET",
        focused: false,
        intent: "normal",
        disabled: false,
        focusable: true,
        bare: false,
        hoverable: false,
        key: QUIET_KEY,
      },
    ],
  };
}

function hov_mount(): void {
  state.mounted = true;
  state.enters = 0;
  state.leaves = 0;
  state.stray = 0;
  editor.mountFloatingWidget(PANEL_ID, spec(), 60, 40);
  editor.setStatus("TestHover: MOUNTED");
}
registerHandler("hov_mount", hov_mount);

editor.on("widget_event", (e) => {
  if (!state.mounted || e.panel_id !== PANEL_ID) return;
  if (e.event_type !== "hover") return;
  const payload = (e.payload ?? {}) as Record<string, unknown>;
  const hovered = payload.hovered === true;
  if (e.widget_key !== WATCHED_KEY) {
    // A hover event for a widget that never opted in — the bug this
    // plugin is here to catch.
    state.stray += 1;
  } else if (hovered) {
    state.enters += 1;
  } else {
    state.leaves += 1;
  }
  editor.updateFloatingWidget(PANEL_ID, spec());
});

editor.registerCommand(
  "TestHover: Mount",
  "Mount a panel with one hoverable and one non-hoverable button",
  "hov_mount",
  null,
);
