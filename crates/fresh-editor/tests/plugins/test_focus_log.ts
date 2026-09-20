/// <reference path="../../plugins/lib/fresh.d.ts" />
const editor = getEditor();

/**
 * The e2e fixture for `focus_announcer.rs`: records every focus hook the
 * host fires, in order, to `<project>/focus_log.txt`, one line per hook.
 *
 *   buffer_activated <buffer>@<window>
 *   buffer_deactivated <buffer>@<window>
 *   buffer_closed <buffer>@<window>
 *   active_window_changed <prev>-><next>
 *   active_buffer_changed <buffer>@<window> from <buffer>@<window>|none <reason>
 *   window_closed <id>
 *
 * The file is rewritten whole on every event, so a reader always sees a
 * consistent prefix of the sequence.
 */

const lines: string[] = [];
const LOG = `${editor.getCwd()}/focus_log.txt`;

function record(line: string): void {
  lines.push(line);
  // `replaceFile`, not `writeFile`: the log is rewritten whole on every
  // event, and `writeFile` refuses a path that already exists.
  editor.replaceFile(LOG, lines.join("\n") + "\n");
}

editor.on("buffer_activated", (a) => record(`buffer_activated ${a.buffer_id}@${a.window_id}`));
editor.on("buffer_deactivated", (a) =>
  record(`buffer_deactivated ${a.buffer_id}@${a.window_id}`),
);
editor.on("buffer_closed", (a) => record(`buffer_closed ${a.buffer_id}@${a.window_id}`));
editor.on("active_window_changed", (a) =>
  record(`active_window_changed ${a.previous_id}->${a.active_id}`),
);
editor.on("active_buffer_changed", (a) => {
  const prev = a.previous ? `${a.previous.buffer_id}@${a.previous.window_id}` : "none";
  record(`active_buffer_changed ${a.buffer_id}@${a.window_id} from ${prev} ${a.reason}`);
});
editor.on("window_closed", (a) => record(`window_closed ${a.id}`));
