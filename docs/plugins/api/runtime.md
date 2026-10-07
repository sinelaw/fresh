<!-- Generated from the plugin API source. Do not edit: change the doc comments in the Rust source, then run `cargo test -p fresh-plugin-runtime write_fresh_dts_file -- --ignored`. -->

# Plugin Runtime

The globals every plugin uses, plugin information, sharing an API with other plugins, loading plugins, and small utilities.

::: v-pre

## Globals

### `getEditor`

Get the editor API instance.
Plugins must call this at the top of their file to get a scoped editor object.

```typescript
declare function getEditor(): EditorAPI;
```

### `registerHandler`

Register a function as a named handler on the global scope.

Handler functions registered this way can be referenced by name in
`editor.registerCommand()`, `editor.on()`, and mode keybindings.

The `fn` parameter is typed as `Function` because the runtime passes
different argument shapes depending on the caller: command handlers
receive no arguments, event handlers receive an event-specific data
object (e.g. `{ buffer_id: number }`), and prompt handlers receive
`{ prompt_type: string, input: string }`. Type-annotate your handler
parameters to match the event you are handling.

```typescript
declare function registerHandler(name: string, fn: Function): void;
```

| Parameter | Description |
|-----------|-------------|
| `name` | Handler name (referenced by registerCommand, on, etc.) |
| `fn` | The handler function |

## Plugin Info

### `apiVersion`

Get the plugin API version. Plugins can check this to verify
the editor supports the features they need.

```typescript
apiVersion(): number;
```

### `pluginName`

The name of the plugin this `editor` handle belongs to. Used by the
M3 plugin-API plane (`exportPluginApi` tags the exporter). Plugin
authors generally don't call this directly.

```typescript
pluginName(): string;
```

## Plugin APIs

### `exportPluginApi`

Publish a typed API surface under `name`. Another plugin (typically
`init.ts`) can reach it later via `getPluginApi(name)`. Calling
again with the same `name` replaces the previous registration
(idempotent — reload works). Exports are auto-dropped when the
calling plugin is unloaded.

Returns `true` on success. Rejects with a TypeError if `name` is
empty or `api` is not an object (functions and primitives are not
valid API surfaces — only objects).

```typescript
exportPluginApi(name: string, api: unknown): boolean;
```

### `getPluginApi`

Look up a plugin API previously published via `exportPluginApi`.
Returns the api object (restored into the caller's context) or
`null` if no plugin exports under that name.

```typescript
getPluginApi(name: string): unknown | null;
getPluginApi<K extends keyof FreshPluginRegistry>(name: K): FreshPluginRegistry[K] | null;
```

## Plugin Management

### `loadPlugin`

Load a plugin from a file path (async)

```typescript
loadPlugin(path: string): Promise<boolean>;
```

### `unloadPlugin`

Unload a plugin by name (async)

```typescript
unloadPlugin(name: string): Promise<boolean>;
```

### `reloadPlugin`

Reload a plugin by name (async)

```typescript
reloadPlugin(name: string): Promise<boolean>;
```

### `listPlugins`

List all loaded plugins (async)
Returns array of &#123; name: string, path: string, enabled: boolean }

```typescript
listPlugins(): Promise<Array<{
  name: string;
  path: string;
  enabled: boolean;
}>>;
```

### `reloadInit`

Re-read `~/.config/fresh/init.ts` and run it — the scriptable form of
the "init: Reload" palette command, and the same thing
`fresh --cmd init reload` sends.

Use this rather than `reloadPlugin("init.ts")`: init.ts is not loaded
from a path (its plugin path is the sentinel `<buffer:init.ts>`), so
the by-name plugin reload cannot find it.

Reloading drops the previous init.ts's commands, handlers, event
subscriptions and settings before the new source runs, so the
author → reload → test loop needs no editor restart. Resolves `true`
once the new source has run; rejects with the parse error if the file
does not compile (the old init.ts stays live in that case).

Calling this *from* init.ts re-enters the reload; guard it if you do.

```typescript
reloadInit(): Promise<boolean>;
```

## Utilities

### `utf8ByteLength`

Get the UTF-8 byte length of a JavaScript string.

JS strings are UTF-16 internally, so `str.length` returns the number of
UTF-16 code units, not the number of bytes in a UTF-8 encoding.  The
editor API uses byte offsets for all buffer positions (overlays, cursor,
getBufferText ranges, etc.).  This helper lets plugins convert JS string
lengths / regex match indices to the byte offsets the editor expects.

```typescript
utf8ByteLength(text: string): number;
```

### `computeLineDiff`

Line-level diff of two texts (native patience diff; see
`fresh_core::diff`). Returns hunks of differing line ranges in
increasing order; equal regions are not reported. Lines are
0-indexed `\n`-terminated segments (a final unterminated segment
counts as a line), matching the `text.split("\n")`-and-drop-
trailing-empty convention plugins already use for line arrays.

Never refuses an input: pathological chunks degrade to coarser
hunks instead of failing, so callers don't need a "diff too
large" path. Runs synchronously on the plugin thread — cost is
near-linear in input size, far below the JS it replaces.

```typescript
computeLineDiff(oldText: string, newText: string): LineDiffHunk[];
```

### `parseJsonc`

Parse a JSONC (JSON with comments) string into a JS value.

Accepts the JSONC superset: line and block comments, trailing
commas, single-quoted strings, and unquoted object keys — matching
devcontainer.json / tsconfig.json / VS Code settings.json.

Throws a JS error (catchable with try/catch) when the input is not
valid JSONC, like `JSON.parse` does for invalid JSON.

```typescript
parseJsonc(text: string): unknown;
```

:::
