<!-- Generated from the plugin API source. Do not edit: change the doc comments in the Rust source, then run `cargo test -p fresh-plugin-runtime write_fresh_dts_file -- --ignored`. -->

# Config, Themes & Languages

Plugin settings, editor config, directories, themes, grammars and language servers.

::: v-pre

## Config

### `getConfig`

Get current config as JS object.

This is the merged configuration (user config file plus compiled-in
defaults) that the editor is actually using, including all default values
for LSP servers, languages, keybindings and so on. Use `getUserConfig` for
the user's config file alone.

The snapshot holds an `Arc<serde_json::Value>` that was serialized
on the editor side the last time the underlying `Arc<Config>`
changed. Cloning the Arc inside the read lock is a refcount bump;
the actual walk into the JS runtime happens outside the lock.

```typescript
getConfig(): unknown;
```

### `getUserConfig`

Get user config as JS object. Same Arc-clone pattern as `get_config`.

Returns only the values explicitly set in the config file, not defaults.
The file read is the first that exists: a `config.json` in the working
directory, otherwise the user's config file. Fields not present here use
their default values. Use this with `getConfig()` to tell which values are
defaults.

```typescript
getUserConfig(): unknown;
```

### `defineConfigBoolean`

Declare a boolean config field for the calling plugin.

Validates `options` synchronously: the JS call throws if any
unknown key is present or if `default` isn't a boolean. The
Settings UI grows a "Plugin Settings → &lt;plugin>" sub-category
containing a toggle for this field. Returns the current value
(user-set if present, otherwise the declared `default`).

```typescript
defineConfigBoolean(name: string, options: {
  default: boolean;
  description?: string;
}): boolean;
```

### `defineConfigInteger`

Declare an integer config field for the calling plugin. Throws on
invalid options or if the default falls outside `minimum/maximum`.

```typescript
defineConfigInteger(name: string, options: {
  default: number;
  description?: string;
  minimum?: number;
  maximum?: number;
}): number;
```

### `defineConfigNumber`

Declare a floating-point number config field. Throws on bad
options or default outside `minimum/maximum`.

```typescript
defineConfigNumber(name: string, options: {
  default: number;
  description?: string;
  minimum?: number;
  maximum?: number;
}): number;
```

### `defineConfigString`

Declare a free-form string config field.

```typescript
defineConfigString(name: string, options: {
  default: string;
  description?: string;
}): string;
```

### `defineConfigStringArray`

Declare an array-of-strings config field (e.g. a list of
patterns). The Settings UI renders this as a list editor.

```typescript
defineConfigStringArray(name: string, options: {
  default: string[];
  description?: string;
}): string[];
```

### `getPluginConfig`

Get the calling plugin's settings as a JS object.

Returns the merged value at `config.plugins.<plugin_name>.settings`.
The shape comes from whatever the plugin declared via
`editor.definePluginConfig(...)` (defaults pre-populated by the
host, user overrides on top from the Settings UI). Returns `null`
if the plugin hasn't declared a schema and has no user-set value.

```typescript
getPluginConfig(): unknown;
getPluginConfig<T = unknown>(): T;
```

### `reloadConfig`

Reload configuration from file

After a plugin saves config changes to the config file, call this to reload
the editor's in-memory configuration. This keeps the editor and plugins in
sync with the saved config.

```typescript
reloadConfig(): void;
```

### `setSetting`

Set a single config setting in the runtime layer for this session.

`path` is dot-separated (e.g. `"editor.tab_size"`). `value` is any JSON
value in the shape the setting expects. The write lives in an
in-memory layer scoped to the calling plugin — it does not modify
`config.json`, and unloading the plugin (or reloading init.ts) drops
it. Intended use is `init.ts` running a conditional:
`if (editor.getEnv("SSH_TTY")) editor.setSetting("terminal.mouse", false);`

Returns `true` if the write was queued. The actual update is
asynchronous; a subsequent `getConfig()` will reflect it after the
editor processes the command.

```typescript
setSetting(path: string, value: unknown): boolean;
```

### `saveSetting`

Persist a single core config setting to the user's config file.

The durable counterpart to `setSetting`: `setSetting` patches the
running editor and is gone at exit, this writes `config.json` the way
the Settings UI does (same layer resolution, same comment-preserving
rewrite) *and* applies the value immediately, so a checkbox a plugin
draws can own a real setting.

`path` is dot-separated (e.g. `"orchestrator_mode"`,
`"editor.tab_size"`). The host refuses a path that is not a real
config setting rather than writing a key that would be silently
dropped on the next load, and says so in the status bar.

Returns `true` if the write was queued; it is applied asynchronously,
so a following `getConfig()` reflects it only after the editor
processes the command.

```typescript
saveSetting(path: string, value: unknown): boolean;
```

### `defineConfigEnum`

```typescript
defineConfigEnum<E extends string>(name: string, options: {
  values: readonly E[];
  default: NoInfer<E>;
  description?: string;
}): E;
```

## Directories

### `getTempDir`

Get the OS temporary directory path.

```typescript
getTempDir(): string;
```

### `getPluginDir`

Get the directory where this plugin's files are stored.
For package plugins this is `<plugins_dir>/packages/<plugin_name>/`.

```typescript
getPluginDir(): string;
```

### `getConfigDir`

Get config directory path

Returns the absolute path to the user config directory (e.g.
`~/.config/fresh/` on Linux).

```typescript
getConfigDir(): string;
```

### `getDataDir`

Get the persistent data directory path (DirectoryContext::data_dir).
Intended for plugin state that should outlive a single session — e.g.
review-diff comments keyed off git state.

```typescript
getDataDir(): string;
```

### `getHomeDir`

The user's home directory as the editor resolved it, or `""` when it
has none. A plugin reading a dotfile asks here rather than reading
`$HOME`, which no test can redirect per-editor.

```typescript
getHomeDir(): string;
```

### `getTerminalDir`

Directory holding terminal scrollback backing files for the current
working directory. Each project root / worktree has its own subdir, so
Universal Search's terminal scope can stay scoped to the active
project rather than spanning every project's terminals.

```typescript
getTerminalDir(): string;
```

### `getWorkingDataDir`

Per-working-directory data root for plugin state scoped to the current
project root / worktree (`<data_dir>/workdirs/<encoded-cwd>/`). Use
instead of `getDataDir()` for state that should not be shared across
worktrees. The directory is not created here — callers create what
they need under it.

```typescript
getWorkingDataDir(): string;
```

### `getThemesDir`

Get themes directory path

Returns the absolute path to the directory where user themes are stored
(e.g. `~/.config/fresh/themes/`).

```typescript
getThemesDir(): string;
```

## Themes

### `reloadThemes`

Reload theme registry from disk
Call this after installing theme packages or saving new themes

```typescript
reloadThemes(): void;
```

### `reloadAndApplyTheme`

Reload theme registry and apply a theme atomically

```typescript
reloadAndApplyTheme(themeName: string): void;
```

### `applyTheme`

Apply a theme by name

Loads and applies the theme immediately. The theme can be a built-in theme
name or a custom theme from the themes directory.

```typescript
applyTheme(themeName: string): boolean;
```

| Parameter | Description |
|-----------|-------------|
| `themeName` | Name of the theme to apply (e.g. "dark", "light", "my-custom-theme") |

### `overrideThemeColors`

Override theme colors in-memory for the running session. `overrides`
is a JS object mapping `"section.field"` keys (same namespace as
`getThemeSchema`) to `[r, g, b]` triplets (0–255 each).

Unknown keys are dropped silently; out-of-range values are clamped
to `0..=255`. Overrides survive until the next `applyTheme` call
(which replaces the whole `Theme`). Intended for fast animation
loops from `init.ts` — no disk I/O, no theme-registry rescan.

```typescript
overrideThemeColors(overrides: unknown): boolean;
```

### `getThemeSchema`

Get theme schema as JS object

Returns the raw JSON Schema that schemars generates for `ThemeFile`, for use
by the theme editor. The schema uses standard JSON Schema format with `$ref`
for type references. Plugins must parse the schema and resolve `$ref`
references themselves.

```typescript
getThemeSchema(): unknown;
```

### `getBuiltinThemes`

Get list of builtin themes as JS object

```typescript
getBuiltinThemes(): unknown;
```

### `getAllThemes`

Full theme registry (builtins + user themes + packages + bundles).
Keyed by canonical registry key; each value carries `_key` / `_pack`.

```typescript
getAllThemes(): unknown;
```

### `deleteTheme`

Delete a custom theme (alias for deleteThemeSync)

Only deletes files from the user's themes directory, so a plugin cannot
delete arbitrary files.

```typescript
deleteTheme(name: string): boolean;
```

| Parameter | Description |
|-----------|-------------|
| `name` | Theme name (without the .json extension) |

### `getThemeData`

Get theme data (JSON) by name from the in-memory cache

```typescript
getThemeData(name: string): unknown;
```

### `saveThemeFile`

Save a theme file to the user themes directory, returns the saved path

```typescript
saveThemeFile(name: string, content: string): string;
```

### `themeFileExists`

Check if a user theme file exists

```typescript
themeFileExists(name: string): boolean;
```

## Languages & LSP

### `listGrammars`

List all available grammars with source info - returns array of GrammarInfo objects
Grammars come from all sources: built-in, user-installed, language packs,
bundles and plugin-registered.
Each entry has `name` (use it in the config `grammar` field), `source`
(where the grammar is from, e.g. "built-in" or "plugin (myplugin)") and
`file_extensions` (the file extensions associated with the grammar).

```typescript
listGrammars(): GrammarInfoSnapshot[];
```

### `registerGrammar`

Register a TextMate grammar file for a language
The grammar will be pending until reload_grammars() is called

```typescript
registerGrammar(language: string, grammarPath: string, extensions: string[]): boolean;
```

### `registerLanguageConfig`

Register language configuration (comment prefix, indentation, formatter)

```typescript
registerLanguageConfig(language: string, config: LanguagePackConfig): boolean;
```

### `registerLspServer`

Register an LSP server for a language

```typescript
registerLspServer(language: string, config: LspServerPackConfig): boolean;
```

### `reloadGrammars`

Reload the grammar registry to apply registered grammars (async)
Call this after registering one or more grammars.
Returns a Promise that resolves when the grammar rebuild completes.

```typescript
reloadGrammars(): Promise<void>;
```

### `setBufferLanguage`

Choose the grammar a virtual buffer is highlighted with.

Panel buffers are named `*<panel id>*`, which resolves to no
grammar; a plugin composing a known text shape into one calls this
so the host highlights it instead of the plugin painting overlays.

```typescript
setBufferLanguage(bufferId: number, name: string): boolean;
```

### `setLspMenuContributions`

Contribute (or replace, or clear) menu rows for the LSP-Servers
popup. Pass an empty `items` to clear this plugin's slice for
the given language. See `PluginCommand::SetLspMenuContributions`.

Each item's `id` is its row's key and must be unique among the
items; a repeated one throws.

```typescript
setLspMenuContributions(pluginId: string, language: string, items: TsLspMenuItem[]): boolean;
```

### `disableLspForLanguage`

Disable LSP for a specific language

Stops the language's server and persists the change to the config.
LSP helper plugins use this to let users disable LSP for languages
where the server is not available or not working.

```typescript
disableLspForLanguage(language: string): boolean;
```

| Parameter | Description |
|-----------|-------------|
| `language` | The language to disable LSP for (e.g., "python", "rust") |

### `restartLspForLanguage`

Restart LSP server for a specific language

```typescript
restartLspForLanguage(language: string): boolean;
```

### `registerLspUriScheme`

Claim an LSP URI scheme (e.g. "slang-synth"). LSP navigations that
resolve to a non-file URI with this scheme are routed to the
`lsp_open_external_uri` hook instead of the core's fallback message.

```typescript
registerLspUriScheme(scheme: string): boolean;
```

### `setLspRootUri`

Set the workspace root URI for a specific language's LSP server
This allows plugins to specify project roots (e.g., directory containing .csproj)

```typescript
setLspRootUri(language: string, uri: string): boolean;
```

### `getAllDiagnostics`

Get all diagnostics from LSP

Returns the diagnostics for all files.

```typescript
getAllDiagnostics(): JsDiagnostic[];
```

### `sendLspRequest`

Send LSP request (async, returns request_id)

Sends an arbitrary LSP request and resolves with the raw JSON response.

```typescript
sendLspRequest(language: string, method: string, params: Record<string, unknown> | null): Promise<unknown>;
```

| Parameter | Description |
|-----------|-------------|
| `language` | Language ID (e.g., "cpp") |
| `method` | Full LSP method (e.g., "textDocument/switchSourceHeader") |
| `params` | Request payload, or null for none |

:::
