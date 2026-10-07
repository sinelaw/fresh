# Fresh Editor Plugin API

Plugins talk to the editor through the `editor` object. This page explains the
main ideas. The reference pages list every call, generated from the API's
source so they always match it:

- [Plugin Runtime](./runtime): globals, plugin info, sharing APIs, utilities
- [Status, Logging & Translation](./status-logging)
- [Events & Hooks](./events): `editor.on` and every event payload
- [Buffers & Editing](./buffer)
- [Commands, Prompts & Dialogs](./ui)
- [Decorations](./overlays): overlays, virtual text, folds, gutter marks
- [Virtual Buffers & Panels](./virtual-buffers)
- [Windows, Splits & Terminals](./windows)
- [Files, Paths & Environment](./filesystem)
- [Processes, Timers & Network](./processes)
- [Config, Themes & Languages](./config)
- [Types](./types): every type the API takes or returns

## Core Concepts

### Buffers

A buffer holds text content and may or may not be associated with a file. Each buffer has a unique numeric ID that persists for the editor session. Buffers track their content, modification state, cursor positions, and path. All text operations (insert, delete, read) use byte offsets, not character indices.

### Splits

A split is a viewport pane that displays a buffer. The editor can have multiple splits arranged in a tree layout. Each split shows exactly one buffer, but the same buffer can be displayed in multiple splits. Use split IDs to control which pane displays which buffer.

### Virtual Buffers

Special buffers created by plugins to display structured data like search results, diagnostics, or git logs. Virtual buffers support text properties (metadata attached to text ranges) that plugins can query when the user selects a line. Unlike normal buffers, virtual buffers are typically read-only and not backed by files.

### Text Properties

Metadata attached to text ranges in virtual buffers. Each entry has text content and a properties object with arbitrary key-value pairs. Use `getTextPropertiesAtCursor` to retrieve properties at the cursor position (e.g., to get file/line info for "go to").

### Overlays

Visual decorations applied to buffer text without modifying content. Overlays can change text color and add underlines. Use overlay IDs to manage them; prefix IDs enable batch removal (e.g., "lint:" prefix for all linter highlights).

### Plugin Directory

Plugins can call `getPluginDir()` to locate their own package directory, useful for finding bundled scripts or local dependencies.

### Authority

"Authority" is the editor's slot for where filesystem, process spawning, and LSP routing are targeted. The same abstraction powers the host, SSH remotes, and devcontainers — the rest of the editor doesn't need to know which one is active. Plugins that want to target the host even while the editor is attached elsewhere can use `editor.setAuthority(...)` / `editor.clearAuthority()`, and `editor.spawnHostProcess(...)` to run a process on the host regardless of the current authority. Both authority calls are queued and land on the window showing the current project — nothing is torn down and no plugin is reloaded, so follow-up work belongs in an `authority_changed` handler rather than after the call returns. `spawnHostProcess` returns a handle with a `kill()` method (also callable from the `KillHostProcess` palette command) so long-running agents can be cancelled cleanly. LSP spawns and `command_exists` probes also go through the current authority, with `ProcessLimits` respected end-to-end.

### Remote Indicator

Plugins that mediate a remote authority (the built-in SSH and devcontainer plugins do this) can drive the status-bar `{remote}` indicator: `editor.setRemoteIndicatorState({ state: "Connecting" | "Connected" | "FailedAttach", … })` to set the label, context menu, and any action rows shown on click, and `editor.clearRemoteIndicatorState()` when the authority detaches.

### Theme Overrides

`editor.overrideThemeColors({...})` applies an in-memory tweak to the active theme without touching disk — useful for animations (e.g. fading in on startup) or environment-driven highlights from `init.ts`. Call `editor.applyTheme(name)` to drop the overrides and settle back on the saved theme.

### JSONC Parsing

`editor.parseJsonc(text)` parses JSON with comments using the host's parser, so plugin code doesn't need to bundle a JSONC library just to read a user config file.

### Ephemeral Terminals

Terminals created by a plugin now follow the lifetime of the action that spawned them — when the action finishes, the terminal closes cleanly on its own. This is what you want for one-shot commands (a test run, a formatter) where you don't want the tab to linger.

### Typed Plugin APIs

`editor.exportPluginApi("name", api)` makes an object available to other plugins and `init.ts` via `editor.getPluginApi("name")`. The return type is inferred automatically if the publishing plugin augments the shared `FreshPluginRegistry` interface:

```ts
// In my_plugin.ts
export type MyPluginApi = { doThing(): void };
declare global {
  interface FreshPluginRegistry {
    "my-plugin": MyPluginApi;
  }
}
editor.exportPluginApi("my-plugin", { doThing() { /* ... */ } });
```

Consumers then get a typed surface with no cast:

```ts
const api = editor.getPluginApi("my-plugin"); // MyPluginApi | null
```

Each loaded plugin's augmentation is emitted to `<config_dir>/types/plugins.d.ts` at startup (via oxc's isolated-declarations), so `init.ts` sees every registry entry automatically. Plugins that don't augment the registry still work — the untyped `getPluginApi<T = unknown>(name): T | null` overload takes over.

### Modes

Keybinding contexts that determine how keypresses are interpreted. Each buffer has a mode (e.g., "normal", "insert", "special"). Custom modes can inherit from parents and define buffer-local keybindings. Virtual buffers typically use custom modes.
