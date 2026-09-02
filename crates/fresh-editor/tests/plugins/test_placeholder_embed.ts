/// <reference path="../../plugins/lib/fresh.d.ts" />
import { col, raw, windowEmbed } from "./lib/widgets.ts";

const editor = getEditor();

/**
 * Regression surface for sinelaw/fresh#1971: a `windowEmbed` naming a
 * workspace that has no window yet froze the panel it lived in.
 *
 * The host reads `windowId` as a `u32`. A plugin that embeds a
 * placeholder row — the orchestrator gives one a synthetic negative id
 * until its workspace exists — sent a value that could not deserialise,
 * and the failure was for the *whole spec*: `updateFloatingWidget`
 * logged `invalid spec` and dropped it, so the panel kept painting
 * whatever it last showed. In the picker that was the "Archiving… /
 * Waiting for git…" card, still up long after the archive had finished.
 *
 * The plugin mounts a panel showing V1 next to a placeholder embed, then
 * updates it to V2. With the bug neither spec survives the boundary and
 * the screen never shows either marker; with the fix the placeholder
 * renders as the blank rows it is meant to be and V2 replaces V1.
 */

const PANEL_ID = 771971;
// The shape of an orchestrator placeholder id: below every real window.
const PLACEHOLDER_WINDOW_ID = -1_000_000;

function spec(marker: string) {
  return col(
    raw([{ text: marker + "\n", properties: {} }]),
    windowEmbed({ windowId: PLACEHOLDER_WINDOW_ID, rows: 5, key: "embed" }),
  );
}

function mount(): void {
  // `startBlurred` — the panel is here to be looked at, not typed into, and
  // a focused one swallows the Ctrl+P the test needs to reach the palette a
  // second time to run the update.
  editor.mountFloatingWidget(
    PANEL_ID,
    spec("PLACEHOLDER-EMBED-V1"),
    90,
    60,
    false,
    false,
    "",
    false,
    true,
  );
  editor.setStatus("TestEmbed: MOUNTED");
}
registerHandler("placeholder_mount", mount);

function update(): void {
  editor.updateFloatingWidget(PANEL_ID, spec("PLACEHOLDER-EMBED-V2"));
  editor.setStatus("TestEmbed: UPDATED");
}
registerHandler("placeholder_update", update);

editor.registerCommand(
  "TestEmbed: Mount",
  "Mount a floating panel embedding a window that does not exist",
  "placeholder_mount",
  null,
);
editor.registerCommand(
  "TestEmbed: Update",
  "Replace that panel's contents",
  "placeholder_update",
  null,
);
