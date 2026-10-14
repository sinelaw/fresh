<!-- Generated from the plugin API source. Do not edit: change the doc comments in the Rust source, then run `cargo test -p fresh-plugin-runtime write_fresh_dts_file -- --ignored`. -->

# Events & Hooks

Run code when something happens in the editor. Subscribe with `editor.on(event, handler)`; the handler receives the event's payload, listed under [Events](#events) below.

::: v-pre

## Event Handlers

### `on`

Subscribe to an editor event.

The handler is a function, or the name of a function on `globalThis`.
Multiple handlers can be registered for the same event. Events include
"after_file_save", "cursor_moved", "buffer_modified", and others.

```ts
globalThis.onSave = (data) => {
  editor.setStatus(`Saved: ${data.path}`);
};
editor.on("after_file_save", "onSave");
```

```typescript
on(eventName: string, handlerName: string): void;
on<K extends keyof HookEventMap>(eventName: K, handler: (args: HookEventMap[K]) => boolean | void | Promise<boolean | void>): void;
on<K extends keyof HookEventMap>(eventName: K, handlerName: string): void;
```

| Parameter | Description |
|-----------|-------------|
| `eventName` | Event to subscribe to |
| `handlerName` | Name of globalThis function to call with event data |

### `off`

Unsubscribe from an event

```typescript
off(eventName: string, handlerName: string): void;
off<K extends keyof HookEventMap>(eventName: K, handler: (args: HookEventMap[K]) => boolean | void | Promise<boolean | void>): void;
off<K extends keyof HookEventMap>(eventName: K, handlerName: string): void;
```

| Parameter | Description |
|-----------|-------------|
| `handlerName` | Name of the handler to remove |

### `getHandlers`

Get registered event handlers for an event

Returns the handler names.

```typescript
getHandlers(eventName: string): string[];
```

| Parameter | Description |
|-----------|-------------|
| `eventName` | Name of the event |

## Events

### Lifecycle

#### `editor_initialized`

```typescript
editor_initialized: Record<string, never>;
```

#### `plugins_loaded`

```typescript
plugins_loaded: Record<string, never>;
```

#### `ready`

```typescript
ready: Record<string, never>;
```

#### `focus_gained`

```typescript
focus_gained: Record<string, never>;
```

#### `authority_changed`

```typescript
authority_changed: {
  label: string;
};
```

#### `trust_changed`

```typescript
trust_changed: {
  level: "trusted" | "restricted" | "blocked";
};
```

#### `config_changed`

The effective config changed — the user saved from the Settings UI,
or the config was reloaded from disk. Payload-free by design:
re-read what you care about with `editor.getPluginConfig()` /
`editor.getConfig()`, both of which already reflect the new values
when the handler runs.

Any plugin that caches a `defineConfigX` value (rather than reading
it at point of use) should subscribe, or its setting will appear to
do nothing until the editor restarts. Does not fire for the
plugin's own `editor.setSetting(...)` writes.

```typescript
config_changed: Record<string, never>;
```

#### `input_mode_changed`

The editor-wide input mode (`editor.setInputMode`) changed — vi, or
another modal-editing plugin, was turned on or off or switched between
its sub-modes. `mode` is the new value (`null` when cleared). A plugin
whose window-scoped editor mode should step aside while an input mode
is on re-checks here, since a window's editor mode outranks it.

```typescript
input_mode_changed: {
  mode: string | null;
};
```

### Buffer lifecycle

#### `buffer_activated`

```typescript
buffer_activated: {
  buffer_id: number;
  window_id: number;
};
```

#### `buffer_deactivated`

```typescript
buffer_deactivated: {
  buffer_id: number;
  window_id: number;
};
```

#### `buffer_closed`

```typescript
buffer_closed: {
  buffer_id: number;
  window_id: number;
};
```

### File I/O

#### `before_file_open`

```typescript
before_file_open: {
  path: string;
};
```

#### `after_file_open`

```typescript
after_file_open: {
  path: string;
  buffer_id: number;
  window_id: number;
};
```

#### `before_file_save`

```typescript
before_file_save: {
  path: string;
  buffer_id: number;
  window_id: number;
};
```

#### `after_file_save`

```typescript
after_file_save: {
  path: string;
  buffer_id: number;
  window_id: number;
};
```

#### `after_file_revert`

Fired after a buffer is reloaded from disk: auto-revert picked up an
external change (e.g. `git checkout <ref> -- <file>` in another
terminal), or the user ran an explicit revert. Reloads don't fire
`after_file_save`, so plugins that surface disk-derived state
(git gutter, etc.) should subscribe to this too or their decorations
go stale on every external reset.

```typescript
after_file_revert: {
  path: string;
  buffer_id: number;
};
```

#### `after_file_explorer_change`

Fired by the file explorer after a paste/duplicate/etc. mutates
the filesystem without going through a buffer save. Plugins that
surface FS-derived state (git status badges, etc.) should
subscribe in addition to `after_file_save` to refresh on
explorer-driven changes too.

```typescript
after_file_explorer_change: {
  path: string;
};
```

### Text edits

#### `before_insert`

```typescript
before_insert: {
  buffer_id: number;
  window_id: number;
  position: number;
  text: string;
};
```

#### `after_insert`

```typescript
after_insert: {
  buffer_id: number;
  window_id: number;
  position: number;
  text: string;
  affected_start: number;
  affected_end: number;
  start_line: number;
  end_line: number;
  lines_added: number;
};
```

#### `before_delete`

```typescript
before_delete: {
  buffer_id: number;
  window_id: number;
  start: number;
  end: number;
};
```

#### `after_delete`

```typescript
after_delete: {
  buffer_id: number;
  window_id: number;
  start: number;
  end: number;
  deleted_text: string;
  affected_start: number;
  deleted_len: number;
  start_line: number;
  end_line: number;
  lines_removed: number;
};
```

#### `buffer_modified`

Fired after any edit changes a buffer's content — including the bulk
edits (multi-cursor typing, a whole-buffer replace, and the undo or redo
of either) that fire no `after_insert` / `after_delete`. Carries no
positions: it says the buffer changed, so re-read it.

```typescript
buffer_modified: {
  buffer_id: number;
  window_id: number;
};
```

### Cursor & viewport

#### `cursor_moved`

```typescript
cursor_moved: {
  buffer_id: number;
  window_id: number;
  cursor_id: number;
  old_position: number;
  new_position: number;
  line: number;
  text_properties: Record<string, unknown>[];
};
```

#### `viewport_changed`

```typescript
viewport_changed: {
  split_id: number;
  buffer_id: number;
  window_id: number;
  top_byte: number;
  top_line: number | null;
  width: number;
  height: number;
};
```

### Rendering

#### `render_start`

```typescript
render_start: {
  buffer_id: number;
};
```

#### `render_line`

```typescript
render_line: {
  buffer_id: number;
  line_number: number;
  byte_start: number;
  byte_end: number;
  content: string;
};
```

#### `lines_changed`

```typescript
lines_changed: {
  buffer_id: number;
  lines: {
    line_number: number;
    byte_start: number;
    byte_end: number;
    content: string;
    /** This line's role in an embedded-language region — a Markdown fenced
    * code block, a Vue `<script>`/`<style>` block — as the highlighting
    * engine classifies it while parsing. `"open"` and `"close"` are the
    * delimiter lines; `"body"` is content strictly inside.
    *
    * Absent for ordinary lines AND when the region state could not be
    * resolved (a >1MiB buffer whose viewport has no parse checkpoint before
    * it yet). Treat absence as *unknown*, never as "outside a region": the
    * point of this field is that a bare ``` opens or closes depending on
    * every fence above it, so there is nothing to fall back on. */
    region?: "open" | "body" | "close";
    /** Where this line sits in a table the buffer's grammar recognizes.
    * `role` is the line's kind (`"header"` is the column-name row,
    * `"delimiter"` the `|---|---|` row, `"row"` a data row); `first_row`
    * marks the data row directly below the delimiter; `last` marks the
    * table's final line.
    *
    * Companion to `region`, and recoverable where that is not: "is this a
    * table row" *is* derivable from a line's own text, so a consumer may
    * fall back to its own rule when this is absent. What it cannot derive
    * is where the table starts and ends — that needs the neighbouring
    * lines, and an edit-sized batch does not contain them.
    *
    * `last` is false rather than unknown when the engine could not see the
    * line below the table, so a consumer drawing a closing edge from it
    * draws none instead of one in the wrong place. */
    table?: {
      role: "header" | "delimiter" | "row";
      first_row: boolean;
      last: boolean;
    };
  }[];
  /** Buffer version these byte ranges were captured at. Pass back to
  * coordinate-mapping APIs to repair stale offsets from this batch. */
  epoch: number;
  /** Whether any split shows this buffer in compose/preview mode, read from
  * the live view states as this batch was built.
  *
  * Gate decoration work on this, not on
  * `getBufferInfo(buffer_id).is_composing_in_any_split`. The editor marks
  * these lines as seen the moment it sends the batch, so the batch is the
  * only offer they get, while `getBufferInfo` reads the editor's cached
  * state, which the editor refreshes on its own schedule — early in a mode
  * change it still reports the mode the buffer just left. Gating on
  * `getBufferInfo` therefore drops the first decoration pass at random, leaving the
  * document undecorated until an edit or a scroll produces another
  * batch. */
  is_composing_in_any_split: boolean;
};
```

### Commands

#### `pre_command`

```typescript
pre_command: {
  action: string | Record<string, unknown>;
};
```

#### `post_command`

```typescript
post_command: {
  action: string | Record<string, unknown>;
};
```

#### `idle`

NOT EMITTED. Declared here historically, but nothing in the editor ever
fires it: `editor.on("idle", ...)` registers successfully and the handler
is never called. Listed so that its absence is documented rather than
discovered — do not build on it. For "run something later", drive it from
an event that does fire (`cursor_moved`, `buffer_changed`) or from your
own `spawnProcess` timer.

```typescript
idle: {
  milliseconds: number;
};
```

#### `resize`

```typescript
resize: {
  width: number;
  height: number;
};
```

### Prompts

#### `prompt_changed`

The text in a prompt opened with `editor.startPrompt` changed. Fires on
every keystroke, so a plugin can refilter its suggestions.

```typescript
prompt_changed: {
  prompt_type: string;
  input: string;
};
```

#### `prompt_confirmed`

The user pressed Enter in a prompt opened with `editor.startPrompt`.
`input` is the chosen suggestion's `value`, or the typed text when no
suggestion is chosen.

```typescript
prompt_confirmed: {
  prompt_type: string;
  input: string;
  selected_index: number | null;
};
```

#### `prompt_cancelled`

The user pressed Esc in a prompt opened with `editor.startPrompt`.

```typescript
prompt_cancelled: {
  prompt_type: string;
  input: string;
};
```

#### `prompt_selection_changed`

The highlighted suggestion in a prompt changed.

```typescript
prompt_selection_changed: {
  prompt_type: string;
  selected_index: number;
};
```

### Mouse

#### `mouse_click`

```typescript
mouse_click: MouseClickHookArgs;
```

#### `mouse_move`

```typescript
mouse_move: {
  column: number;
  row: number;
  content_x: number;
  content_y: number;
};
```

#### `mouse_scroll`

```typescript
mouse_scroll: {
  buffer_id: number;
  delta: number;
  col: number;
  row: number;
};
```

### LSP

#### `diagnostics_updated`

```typescript
diagnostics_updated: {
  uri: string;
  count: number;
};
```

#### `lsp_references`

```typescript
lsp_references: {
  symbol: string;
  locations: {
    file: string;
    line: number;
    column: number;
  }[];
};
```

#### `lsp_implementation`

```typescript
lsp_implementation: {
  symbol: string;
  locations: {
    file: string;
    line: number;
    column: number;
  }[];
};
```

#### `lsp_server_request`

A language server sent a request (server to client) with a method the
editor does not handle itself. `params` is a JSON string, or `null`.
The editor answers the server with `null`.

```typescript
lsp_server_request: {
  language: string;
  method: string;
  server_command: string;
  params: string | null;
};
```

#### `lsp/custom_notification`

A server -> client notification whose method the editor does not handle
itself (e.g. clangd's `textDocument/clangd.fileStatus`, `$/memoryUsage`).
Unlike `lsp_server_request`, `params` is the parsed JSON value, not a
string. `server_name` tells apart several servers for one language.

```ts
editor.on("lsp/custom_notification", (e) => {
  if (e.method === "textDocument/clangd.fileStatus" && e.params) {
    editor.setStatus(`clangd: ${(e.params as { status: string }).status}`);
  }
});
```

```typescript
"lsp/custom_notification": {
  language: string;
  server_name: string;
  method: string;
  /** JSON-RPC params: an object or array, or `null` when omitted */
  params: Record<string, unknown> | unknown[] | null;
};
```

#### `lsp_server_error`

```typescript
lsp_server_error: {
  language: string;
  server_command: string;
  error_type: string;
  message: string;
};
```

#### `lsp_status_clicked`

```typescript
lsp_status_clicked: {
  language: string;
  has_error: boolean;
  missing_servers: string[];
  user_dismissed: boolean;
};
```

### UI events

#### `action_popup_result`

The user chose an action in a popup opened with `editor.showActionPopup`.

```typescript
action_popup_result: {
  popup_id: string;
  action_id: string;
};
```

#### `status_bar_token_clicked`

User clicked a plugin-registered status-bar token. Subscribers
filter by `plugin_name` + `token_name`. Use this to re-open a
deferred prompt or surface the relevant settings UI for whatever
the token represents (e.g. trust chip → trust-elevation popup).

```typescript
status_bar_token_clicked: {
  plugin_name: string;
  token_name: string;
};
```

#### `process_output`

```typescript
process_output: {
  process_id: number;
  data: string;
};
```

#### `language_changed`

```typescript
language_changed: {
  buffer_id: number;
  language: string;
};
```

#### `theme_inspect_key`

```typescript
theme_inspect_key: {
  theme_name: string;
  key: string;
};
```

#### `keyboard_shortcuts`

```typescript
keyboard_shortcuts: {
  bindings: {
    key: string;
    action: string;
  }[];
};
```

### Terminals

#### `terminal_output`

A terminal produced output. Fires on every read from the terminal, so
in-place redraws and progress bars count, not only new lines.
`window_id` is the window that owns the terminal, so a plugin can tell
which session the output belongs to.

```typescript
terminal_output: {
  terminal_id: number;
  window_id: number;
  last_line: string;
};
```

#### `terminal_exit`

A terminal's process exited. `exit_code` is `null` when a signal ended it.

```typescript
terminal_exit: {
  terminal_id: number;
  window_id: number;
  exit_code: number | null;
};
```

### File watching

#### `path_changed`

A path watched with `editor.watchPath` changed.

```typescript
path_changed: {
  handle: number;
  path: string;
  /** "modify" | "create" | "delete" | "rename" | "other" */
  kind: string;
};
```

### Windows

#### `window_created`

A window (an Orchestrator session) was created, by `editor.createWindow`
or when sessions are restored at startup.

```typescript
window_created: {
  id: number;
  label: string;
  root: string;
};
```

#### `window_closed`

A window was closed.

```typescript
window_closed: {
  id: number;
};
```

#### `active_window_changed`

The active window changed, once the switch has finished.

```typescript
active_window_changed: {
  previous_id: number | null;
  active_id: number;
};
```

#### `active_buffer_changed`

What the user is looking at changed: the active buffer of the active
window is a different `(window, buffer)` than before. The one hook to
subscribe to for "the active buffer" — it fires for a tab switch, a
split focus, an open, a window dive and a workspace restore alike,
after `active_window_changed` / `buffer_activated` for the same change.
`reason` is `"window"` (a window switch), `"buffer"` (a different
buffer in the same window) or `"open"` (the same buffer re-pointed at
another file in place).

```typescript
active_buffer_changed: {
  window_id: number;
  buffer_id: number;
  previous: {
    window_id: number;
    buffer_id: number;
  } | null;
  reason: string;
};
```

#### `chrome_focus_changed`

Which chrome region holds the keyboard changed: `"editor"` (a pane),
`"explorer"` (the file tree), `"dock"`, or `"section"` (a sidebar
section, named by `plugin` and `panel_id`). Fires once per change, so
a plugin can answer "does the pane have the keyboard?" without
inferring it from its own focus events.

```typescript
chrome_focus_changed: {
  window_id: number;
  region: string;
  plugin: string | null;
  panel_id: number | null;
};
```

### Widget runtime

#### `widget_event`

A widget mounted via `editor.mountWidgetPanel` emitted a
semantic event. Fired when the host's hit-test routes a mouse
click to a `Toggle` / `Button` widget node within a mounted
widget panel. See `docs/internal/plugin-widget-library-design.md`.

Panel ids are plugin-local: the host keys panels by
(plugin, id) and delivers each event only to the plugin that
owns the panel, so ids never need to be globally unique.
Routing is by `panel_id` (matches the id the plugin allocated
at mount time) plus `widget_key` (the stable `key` set on the
widget spec node, or empty when the spec did not assign one).

`event_type` and `payload` shapes:
  * Text field: `event_type = "change"`, `payload = { value, cursorByte }`.
  * Toggle: `event_type = "toggle"`, `payload = { checked: <new> }`.
  * Button: `event_type = "activate"`, `payload = {}`.
  * Esc, a click outside, or the `[×]` of a closable panel:
    `event_type = "cancel"`. The host has already unmounted the panel.

```typescript
widget_event: {
  window_id: number;
  panel_id: number;
  widget_key: string;
  event_type: string;
  payload: Record<string, unknown>;
  /** The widget that holds the panel's focus now, after the event
  *  (`""` for none) — the host's fact; see `getPanelFocusKey`. */
  focus_key: string;
};
```

:::
