/// <reference path="../../plugins/lib/fresh.d.ts" />
const editor = getEditor();

/**
 * The e2e fixture for `focus_announcer.rs`: records every focus hook the
 * host fires, in order, to `<project>/focus_log.txt`, one line per hook.
 *
 *   buffer_activated <id>
 *   buffer_deactivated <id>
 *   buffer_closed <id>
 *   active_window_changed <prev>-><next>
 *   window_closed <id>
 *
 * The file is rewritten whole on every event, so a reader always sees a
 * consistent prefix of the sequence.
 */

const lines: string[] = [];
const LOG = `${editor.getCwd()}/focus_log.txt`;

function record(line: string): void {
  lines.push(line);
  editor.writeFile(LOG, lines.join("\n") + "\n");
}

editor.on("buffer_activated", (a) => record(`buffer_activated ${a.buffer_id}`));
editor.on("buffer_deactivated", (a) => record(`buffer_deactivated ${a.buffer_id}`));
editor.on("buffer_closed", (a) => record(`buffer_closed ${a.buffer_id}`));
editor.on("active_window_changed", (a) =>
  record(`active_window_changed ${a.previous_id}->${a.active_id}`),
);
editor.on("window_closed", (a) => record(`window_closed ${a.id}`));
