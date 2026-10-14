/// <reference path="../../plugins/lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Regression surface for sinelaw/fresh#3386: a floating panel mounted
 * without a `mode` must not take the window's editor mode as its keymap.
 *
 * The plugin keeps a window mode (`test-window-mode`, the way vi_mode keeps
 * "vi-normal") that binds Esc, then mounts a centred panel whose only
 * control is a button and which names no mode. The button passes Esc; the
 * panel has no keymap, so its default runs: the panel is cancelled
 * (`PANEL-CANCELLED`). When the host fell back to the window's mode, the
 * window's Esc ran instead (`WINDOW-MODE-TOOK-ESC`) and the panel stayed up.
 */

const PANEL_ID = 3386;
const WINDOW_MODE = "test-window-mode";
let mounted = false;

editor.defineMode(WINDOW_MODE, [["Esc", "no_mode_window_esc"]], false, false);

function no_mode_window_esc(): void {
  editor.setStatus("WINDOW-MODE-TOOK-ESC");
}
registerHandler("no_mode_window_esc", no_mode_window_esc);

function no_mode_mount(): void {
  mounted = true;
  editor.setEditorMode(WINDOW_MODE);
  editor.mountFloatingWidget(
    PANEL_ID,
    {
      kind: "col",
      children: [
        {
          kind: "button",
          label: "NoModeButton",
          focused: false,
          intent: "normal",
          key: "no-mode-button",
          disabled: false,
        },
      ],
    },
    40,
    30,
  );
  editor.widgetMutate(PANEL_ID, { kind: "setFocusKey", widgetKey: "no-mode-button" });
}
registerHandler("no_mode_mount", no_mode_mount);

editor.on("widget_event", (e) => {
  if (!mounted || e.panel_id !== PANEL_ID) return;
  if (e.event_type !== "cancel") return;
  mounted = false;
  editor.setStatus("PANEL-CANCELLED");
});

editor.registerCommand(
  "TestNoMode: Mount",
  "Mount a mode-less floating panel over a window mode that binds Esc",
  "no_mode_mount",
  null,
);
