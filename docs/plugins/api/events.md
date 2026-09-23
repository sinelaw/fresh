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

## Window / Orchestrator events

These events fire as the editor's window set changes. (The
editor internally calls these "windows", modelled on VS Code
windows; Orchestrator's UX presents them as "sessions" — they're
the same thing.) See `docs/internal/orchestrator-sessions-design.md`
for the full window model.

| Event | Payload | When it fires |
|-------|---------|---------------|
| `window_created` | `{ id, label, root }` | A new session was created (via `editor.createWindow` or session restore on startup) |
| `window_closed` | `{ id }` | A session was closed |
| `active_window_changed` | `{ previous, current }` | The active session changed (warm-swap completed) |
| `terminal_output` | `{ terminal_id, data }` | A terminal produced output. Useful for plugins watching agent activity in background sessions. |
| `terminal_exit` | `{ terminal_id, exit_code }` | A terminal's child process exited. `exit_code` may be `null` on signal-terminated processes. |
| `path_changed` | `{ handle, path, kind }` | A path watched via `editor.watchPath(...)` changed. `kind` is `"create"`, `"modify"`, or `"remove"`. |

## LSP events

| Event | Payload | When it fires |
|-------|---------|---------------|
| `lsp_server_request` | `{ language, method, server_command, params }` | A language server sent a request (server -> client) with a method the editor does not handle itself. `params` is a JSON **string** (or `null`); the editor answers the server with `null`. |
| `lsp/custom_notification` | `{ language, server_name, method, params }` | A language server sent a notification with a method the editor does not handle itself, e.g. clangd's `textDocument/clangd.fileStatus` or `$/memoryUsage`. `params` is the parsed JSON value (object, array, or `null`). `server_name` distinguishes several servers configured for one language. |

```typescript
globalThis.onLspNotification = (e) => {
  if (e.method === "textDocument/clangd.fileStatus" && e.params) {
    editor.setStatus(`clangd: ${e.params.status}`);
  }
};
editor.on("lsp/custom_notification", "onLspNotification");
```
