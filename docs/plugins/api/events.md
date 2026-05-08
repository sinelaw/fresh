# Events and Hooks API

## Event/Hook Operations

### `on`

Subscribe to an editor event
Handler must be a global function name (not a closure).
Multiple handlers can be registered for the same event.
Events: "buffer_save", "cursor_moved", "buffer_modified", etc.

```typescript
on(event_name: string, handler_name: string): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `event_name` | `string` | Event to subscribe to |
| `handler_name` | `string` | Name of globalThis function to call with event data |

**Example:**

```typescript
globalThis.onSave = (data) => {
editor.setStatus(`Saved: ${data.path}`);
};
editor.on("buffer_save", "onSave");
```

#### `off`

Unregister an event handler

```typescript
off(event_name: string, handler_name: string): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `event_name` | `string` | Name of the event |
| `handler_name` | `string` | Name of the handler to remove |

#### `getHandlers`

Get list of registered handlers for an event

```typescript
getHandlers(event_name: string): string[]
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `event_name` | `string` | Name of the event |

## Window / Conductor events

These events fire as the editor's window set changes. (The
editor internally calls these "windows", modelled on VS Code
windows; Conductor's UX presents them as "sessions" — they're
the same thing.) See `docs/internal/conductor-sessions-design.md`
for the full window model.

| Event | Payload | When it fires |
|-------|---------|---------------|
| `window_created` | `{ id, label, root }` | A new session was created (via `editor.createWindow` or session restore on startup) |
| `window_closed` | `{ id }` | A session was closed |
| `active_window_changed` | `{ previous, current }` | The active session changed (warm-swap completed) |
| `terminal_output` | `{ terminal_id, data }` | A terminal produced output. Useful for plugins watching agent activity in background sessions. |
| `terminal_exit` | `{ terminal_id, exit_code }` | A terminal's child process exited. `exit_code` may be `null` on signal-terminated processes. |
| `path_changed` | `{ handle, path, kind }` | A path watched via `editor.watchPath(...)` changed. `kind` is `"create"`, `"modify"`, or `"remove"`. |
