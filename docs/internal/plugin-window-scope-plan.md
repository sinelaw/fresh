# Window scope for plugin state and chrome

Design note for sinelaw/fresh#3326. What was wrong, the model that replaced
it, and the rules the host now keeps. The code carries the detail:
`app::focus_announcer`, `app::sidebar` (`SectionScope`), `app::plugin_buffer_guard`.

## What was found

- A sidebar section was editor-global, like the dock. A Markdown outline
  mounted in one window sat in every other window's sidebar, and a row
  clicked there opened a second, independent buffer of the other window's
  file.
- Ten hand-placed `buffer_activated` sites covered every focus path but the
  window switch, so a dive fired only `active_window_changed` and twelve
  bundled plugins kept the previous window's buffer.
- Buffer ids are unique across the editor, but a buffer belongs to one
  window and nothing in the plugin API said which. A plugin's natural cache
  was a naked id that silently stopped resolving after a switch.
- A restored placeholder no plugin claimed showed "Panel unavailable" for
  the life of the workspace.
- No keymap bound the sidebar focus cycle; the outline could not tell
  whether the pane or the explorer had the keyboard; the explorer said
  nothing when it could not reveal the current file.

## The model

- **One announcer.** Every path that changes what the user is looking at
  calls `announce_focus`, which diffs the active `(window, split, buffer)`
  against the last announcement and fires exactly the hooks the difference
  calls for. A per-frame safety net catches a path nobody wired.
- **The window travels with the id.** `BufferInfo.window_id`, and
  `window_id` on every buffer-bearing hook; `active_buffer_changed` is the
  composed "what the user is looking at changed" hook, with the reason.
- **Chrome has a scope.** A section is about the editor, a window, or a
  buffer; nothing declared is the window of the mount, so editor-wide has
  to be asked for. The host parks sections whose scope does not match the
  active `(window, buffer)`, drops them with their buffer or window, and
  never lets a restore for a non-active window touch the live column.
  Buffer-scoped sections are never persisted; a restored placeholder
  expires after a few frames' grace.
- **Chrome focus is a signal.** `chrome_focus_changed` names the region
  holding the keyboard, so a plugin does not infer it from its own events.

## Invariants the host keeps

- One section per `(plugin, panel_id)` across the column and the parking
  list; the widget registry holds one panel per key.
- The column, the focus cycle and the hit-test only ever see sections that
  belong on screen.
- A buffer-addressed plugin command that names another window's buffer is
  logged with the command's name, and panics under `FRESH_STRICT_PLUGIN_IDS`.

## The one behaviour change for plugins

A sidebar section mounted without a scope is per-window, not editor-wide.
Everything else is additive.
