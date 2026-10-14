<!-- Generated from the plugin API source. Do not edit: change the doc comments in the Rust source, then run `cargo test -p fresh-plugin-runtime write_fresh_dts_file -- --ignored`. -->

# Status, Logging & Translation

Show messages in the status bar, write to the log, and translate your plugin's text.

::: v-pre

## Status Bar

### `setStatus`

Display a transient message in the editor's status bar.
The message stays until the next status update replaces it. An empty
message clears it. Use for feedback on completed operations (e.g. "File
saved", "2 matches found").

```typescript
setStatus(msg: string): void;
```

| Parameter | Description |
|-----------|-------------|
| `msg` | Text to display; keep short (status bar has limited width) |

### `registerStatusBarElement`

Register a custom statusbar token.
Token will be named "plugin_name:token_name" where plugin_name is the current plugin.
Returns true if registration succeeded, false if invalid or already registered.

```typescript
registerStatusBarElement(tokenName: string, title: string): boolean;
```

### `setStatusBarValue`

Set the value of a status-bar token for a specific buffer.
The full token key sent to the editor is "plugin_name:token_name".

```typescript
setStatusBarValue(bufferId: number, tokenName: string, value: string): boolean;
```

## Logging

### `debug`

Log a debug message from a plugin.
The message goes to the editor's log file at debug level, prefixed with
"Plugin:". It is written only when the log filter (RUST_LOG) allows
that level. Useful for plugin development and troubleshooting.

```typescript
debug(msg: string): void;
```

| Parameter | Description |
|-----------|-------------|
| `msg` | Debug message; include context like function name and relevant values |

### `info`

Log an info message from a plugin.
The message goes to the editor's log file at info level, prefixed with
"Plugin:". It is written only when the log filter (RUST_LOG) allows
that level. Use for important operational messages.

```typescript
info(msg: string): void;
```

### `warn`

Log a warning message from a plugin.
The message goes to the editor's log file at warn level, prefixed with
"Plugin:". It is written only when the log filter (RUST_LOG) allows
that level. Use for warnings that don't prevent operation but
indicate issues.

```typescript
warn(msg: string): void;
```

### `error`

Log an error message from a plugin.
The message goes to the editor's log file at error level, prefixed with
"Plugin:". It is written only when the log filter (RUST_LOG) allows
that level. Use for critical errors that need attention.

```typescript
error(msg: string): void;
```

## Translation

### `t`

Translate a string - reads plugin name from __pluginName__ global
Args is optional - can be omitted, undefined, null, or an object

```typescript
t(key: string, ...args: unknown[]): string;
```

### `pluginTranslate`

Translate a key for a specific plugin

```typescript
pluginTranslate(pluginName: string, key: string, args?: Record<string, unknown>): string;
```

### `getCurrentLocale`

Get the current locale

```typescript
getCurrentLocale(): string;
```

:::
