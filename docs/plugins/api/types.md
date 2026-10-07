<!-- Generated from the plugin API source. Do not edit: change the doc comments in the Rust source, then run `cargo test -p fresh-plugin-runtime write_fresh_dts_file -- --ignored`. -->

# Types

The data types the API takes and returns, in alphabetical order.

::: v-pre

### `ActionPopupOptions`

Options for showActionPopup

```typescript
type ActionPopupOptions = {
  /**
  * Unique identifier for the popup (used in ActionPopupResult)
  */
  id: string;
  /**
  * Title text for the popup
  */
  title: string;
  /**
  * Body message (supports basic formatting)
  */
  message: string;
  /**
  * Action buttons to display
  */
  actions: Array<TsActionPopupAction>;
  /**
  * Optional buffer to scope the popup to. When set, the popup only
  * renders while that buffer is active (and is dismissed when the buffer
  * closes), rather than floating over every buffer. Omit for global
  * notifications like install help raised from a status-bar click.
  */
  buffer_id?: number;
};
```

### `ActionSpec`

Specification for an action to execute, with optional repeat count

```typescript
type ActionSpec = {
  /**
  * Action name (e.g., "move_word_right", "delete_line")
  */
  action: string;
  /**
  * Number of times to repeat the action (default 1)
  */
  count: number;
  /**
  * Action payload arguments for actions that carry data, e.g.
  * `{ "char": "x" }` for `insert_char` or `{ "text": "hello" }` for
  * `prompt_confirm_with_text`. Empty/absent for the common no-arg
  * actions (motions, edits, commands). This is what lets a recorded
  * macro — which contains `InsertChar` and other payload actions —
  * round-trip losslessly through `executeActions`.
  */
  args: Record<string, unknown>;
};
```

### `AddMenuItemOptions`

Options for `editor.addMenuItem` — one plugin-contributed row in an
existing menu bar menu.

Every string here is matched or displayed by the host, so the plugin
never reaches into menu internals: it names the *target* menu and,
optionally, the neighbour to sit next to. Both lookups accept a stable
identifier (a menu `id` like `"View"`, an item's `action` like
`"toggle_file_explorer"`) as well as a display label, so a plugin can
place its row without knowing the user's locale.

```typescript
type AddMenuItemOptions = {
  /**
  * Target menu, matched against each menu's stable `id` ("View",
  * "File", …) first and its display label second. A menu that matches
  * neither is left alone and the call is a no-op.
  */
  menu: string;
  /**
  * Row label, already localised by the plugin (`editor.t(…)`).
  */
  label: string;
  /**
  * Action dispatched when the row is chosen. A name the editor doesn't
  * know is routed to the plugin action of the same name — i.e. the
  * handler registered with `registerHandler`.
  */
  action: string;
  /**
  * Menu-context key whose boolean value renders the row's checkmark
  * (e.g. `"dock"`). Omit for a plain action row.
  */
  checkbox?: string;
  /**
  * Menu-context key gating whether the row is enabled. Omit for a row
  * that is always available.
  */
  when?: string;
  /**
  * Insert directly after the existing row whose action or label this
  * names. Ignored when nothing matches (the row is appended instead).
  */
  after?: string;
  /**
  * Insert directly before the existing row whose action or label this
  * names. Ignored when `after` is set, or when nothing matches.
  */
  before?: string;
};
```

### `AnimationRect`

A rectangular region, in cells. Used by the animation plugin API so
callers can target arbitrary screen regions without going through a
virtual buffer.

```typescript
type AnimationRect = {
  x: number;
  y: number;
  width: number;
  height: number;
};
```

### `AuthorityFilesystem`

```typescript
type AuthorityFilesystem = {
  kind: "local";
};
```

### `AuthorityPath`

```typescript
type AuthorityPath = {
  kind: "authority";
  value: string;
};
```

### `AuthorityPayload`

```typescript
type AuthorityPayload = {
  filesystem: AuthorityFilesystem;
  spawner: AuthoritySpawner;
  terminal_wrapper: AuthorityTerminalWrapper;
  display_label?: string;
  /**
  * Optional host↔remote workspace path mapping. The dev-container
  * authority sets both roots (editor.getCwd() on host;
  * remoteWorkspaceFolder on container) so LSP URIs translate at the
  * host/container boundary. Local and SSH authorities omit it.
  */
  path_translation?: PathTranslationSpec;
};
```

### `AuthoritySpawner`

```typescript
type AuthoritySpawner = {
  kind: "local";
} | {
  kind: "docker-exec";
  container_id: string;
  user?: string | null;
  workspace?: string | null;
  env?: [string, string][];
};
```

### `AuthorityTerminalWrapper`

```typescript
type AuthorityTerminalWrapper = {
  kind: "host-shell";
} | {
  kind: "explicit";
  command: string;
  args: string[];
  manages_cwd?: boolean;
};
```

### `BackgroundProcessResult`

Result from spawning a background process

```typescript
type BackgroundProcessResult = {
  /**
  * Unique process ID for later reference, e.g. with `killProcess` or
  * `isProcessRunning`
  */
  process_id: number;
  /**
  * Process exit code (0 usually means success, -1 if killed)
  * Only present when the process has exited
  */
  exit_code: number;
};
```

### `BufferId`

Buffer identifier

```typescript
type BufferId = number;
```

### `BufferInfo`

Information about a buffer

```typescript
type BufferInfo = {
  /**
  * Buffer ID
  */
  id: number;
  /**
  * The window this buffer belongs to. A buffer lives in exactly one
  * window — the same file open in two windows is two buffers with two
  * ids — so a plugin that keeps a buffer id keeps this with it, and
  * checks it against the window it is acting in.
  */
  window_id: number;
  /**
  * File path (if any)
  */
  path: string;
  /**
  * The buffer's display name — what the tab shows.
  *
  * For a file buffer this is the filename (or project-relative path). For
  * a **virtual buffer it is the `name` you passed to
  * `createVirtualBuffer`**, which is how a plugin finds its own panel
  * again: `listBuffers().find(b => b.is_virtual && b.name === "…")`.
  * Before this field existed the only handle was
  * `is_virtual && path === ""`, which cannot tell two plugins' panels
  * apart — or two panels of your own.
  */
  name: string;
  /**
  * Whether the buffer has unsaved changes
  */
  modified: boolean;
  /**
  * Length of buffer in bytes
  */
  length: number;
  /**
  * Number of lines, when the buffer has been indexed. `null` for a very
  * large file whose line index hasn't been built yet — the one case
  * where the count genuinely isn't known.
  *
  * Worth reading after writing content: a buffer built from spans that
  * forgot their newlines reports a plausible `length` and one line,
  * which is otherwise only visible by looking at the screen.
  */
  line_count: number | null;
  /**
  * Whether this is a virtual buffer (not backed by a file)
  */
  is_virtual: boolean;
  /**
  * Whether this buffer is a live terminal (a PTY, not text). Terminal
  * buffers are also `is_virtual`; this distinguishes "a shell is
  * running here" from a plugin-owned scratch buffer, which is what
  * `describeWorkspace()` reports as the pane's `kind`.
  */
  is_terminal: boolean;
  /**
  * Whether editing is disabled for this buffer.
  */
  editing_disabled: boolean;
  /**
  * Current view mode of the active split: "source" or "compose"
  */
  view_mode: string;
  /**
  * True if any split showing this buffer has compose mode enabled.
  * Plugins should use this (not `view_mode`) to decide whether to maintain
  * decorations, since decorations live on the buffer and are filtered
  * per-split at render time.
  */
  is_composing_in_any_split: boolean;
  /**
  * Compose width (if set), from the active split's view state
  */
  compose_width: number | null;
  /**
  * The detected language for this buffer (e.g., "rust", "markdown", "text")
  */
  language: string;
  /**
  * Whether this tab was opened in "preview" (ephemeral) mode — true when
  * opened via single-click in the file explorer and not yet committed
  * (no edit, no double-click, no tab-click, no layout change). Plugins
  * that react to buffer lifecycle events should generally treat preview
  * buffers as transient; e.g. a diagnostics panel may want to skip
  * refreshing itself for a preview tab.
  */
  is_preview: boolean;
  /**
  * Split ids that currently hold this buffer (empty when the buffer is
  * open but not visible in any split — e.g. background-opened tabs
  * that haven't been focused). Lets plugins implement "focus existing
  * buffer if visible, else open new" without having to track split
  * ids across editor restarts (which reassign them). The list is a
  * snapshot at the last `update_plugin_state_snapshot` tick.
  */
  splits: number[];
};
```

### `BufferSavedDiff`

Diff between current buffer content and last saved snapshot

```typescript
type BufferSavedDiff = {
  equal: boolean;
  byte_ranges: Array<[number, number]>;
};
```

### `ButtonKind`

Visual role for a `Button`. Maps to theme keys at render time —
plugins describe intent, not colors. See §7 of the design doc.

```typescript
type ButtonKind = "normal" | "primary" | "danger";
```

### `CommandResult`

A non-zero `code` resolves rather than rejecting.

```typescript
interface CommandResult {
  code: number;
  stdout: string;
  stderr: string;
}
```

### `CreatePreparingWindowOptions`

Options for `createPreparingWindow` — a workspace opened before its
contents exist, so the user lands in it immediately instead of waiting
on the work that fills it.

```typescript
type CreatePreparingWindowOptions = {
  /**
  * Absolute path the placeholder window roots at. It must exist — use
  * the project directory when the workspace's own directory is what is
  * still being created; the adopt step re-roots the window onto the
  * final directory.
  */
  root: string;
  /**
  * Human-readable label. Empty defaults to the basename of `root`.
  */
  label: string;
  /**
  * Progress line shown on the placeholder page.
  */
  message: string;
  /**
  * Focus the new window immediately. `false` (the default) builds it in
  * the background and leaves the user where they are.
  */
  activate?: boolean;
};
```

### `CreateTerminalOptions`

Options for createTerminal

```typescript
type CreateTerminalOptions = {
  /**
  * Working directory for the terminal (defaults to editor cwd)
  */
  cwd?: string;
  /**
  * Split direction: `"horizontal"` or `"vertical"` (default:
  * `"vertical"`).
  *
  * The name describes the **divider**, not the arrangement:
  * `"vertical"` puts the panes side by side, `"horizontal"` stacks them.
  */
  direction?: string;
  /**
  * Split ratio 0.0-1.0 (default: 0.5)
  */
  ratio?: number;
  /**
  * Whether to focus the new terminal split (default: true)
  */
  focus?: boolean;
  /**
  * Whether this terminal is part of the user's persisted workspace.
  * Defaults to `false` for plugin-created terminals — they are typically
  * one-off tool UIs (rebuilds, exec shells, build output) and should
  * start with empty scrollback on each invocation. Set to `true` only
  * when the plugin owns a terminal that the user should see restored
  * across editor restarts.
  */
  persistent?: boolean;
  /**
  * Optional session id to attach the new terminal buffer to.
  * Defaults to the active session at creation time. Setting this
  * lets Orchestrator and similar plugins spawn a terminal *into* an
  * inactive session (e.g. an agent in a worktree the user hasn't
  * dived into yet). The terminal's split is created in that
  * session's stashed split tree; the buffer is attached to the
  * target session's membership set rather than the active one's.
  */
  windowId?: WindowId;
  /**
  * Argv to spawn directly inside the PTY instead of the host's
  * configured shell. Omit it (the default) to keep the historical
  * behaviour: spawn the user's shell and let the caller type into
  * it via `sendTerminalInput`. Set to `[cmd, ...args]`, it runs that
  * exact command as the PTY child — no shell middleman, so the
  * process exits cleanly when the agent does and the
  * terminal-buffer's `terminal_exit` plugin hook reflects the
  * agent's real exit status. Used by Orchestrator so a session
  * with agent `python3` is just python3 in the PTY rather than
  * bash-running-python3-as-a-subshell-command.
  */
  command?: Array<string>;
  /**
  * Tab title for the terminal buffer. Defaults to `command[0]`
  * (when `command` is set) or `"Terminal N"` (the historical
  * auto-numbered title). If another terminal in the same window
  * already uses the requested title, the host appends `" (k)"`
  * to disambiguate. An empty string is treated the same as omitting it.
  */
  title?: string;
  /**
  * Extra environment variables to set in the spawned terminal's
  * child process, on top of the inherited/activated env. Mirrors
  * `CreateWindowWithTerminalOptions.env`. Omitted (the default), it
  * adds nothing, so existing callers behave exactly as before.
  */
  env?: { [key in string] : string };
  /**
  * Argv to run when this terminal is *restored* or *restarted*,
  * instead of re-running `command`. The exact counterpart of
  * `CreateWindowWithTerminalOptions.resume`, so an agent launched
  * into an existing window rejoins its conversation on restart the
  * same way one born in its own window does — a session started with
  * `claude --session-id <id>` sets `resume` to
  * `["claude", "--resume", "<id>"]`. Omit it to keep `command` as the
  * restore argv. The id is a plain argv element — never interpolated
  * into a shell string.
  *
  * Setting `command` (with or without `resume`) also marks the
  * terminal as a restorable *session* terminal, so it survives a
  * workspace save even when `persistent` is false — the same
  * exception `createWindowWithTerminal` relies on.
  */
  resume?: Array<string>;
  /**
  * When set, the host mints an unforgeable capability token bound
  * to the TARGET window (the active window, or `windowId` when
  * set) and injects it into the spawned terminal as
  * `FRESH_CMD_TOKEN` (alongside `FRESH_SESSION`). This lets an
  * agent spawned into an *existing* window drive it by submitting
  * scripts — the same capability a `createWindowWithTerminal`
  * agent gets. `false` (the default) mints no token and injects
  * nothing.
  *
  * The grant is all-or-nothing on purpose: a script can call
  * anything the plugin API exposes, so a narrower list would
  * describe a boundary that isn't there.
  */
  allowScript?: boolean;
};
```

### `CreateVirtualBufferInExistingSplitOptions`

Options for createVirtualBufferInExistingSplit

```typescript
type CreateVirtualBufferInExistingSplitOptions = {
  /**
  * Buffer name (displayed in tabs/title), e.g. `"*Commit Details*"`
  */
  name: string;
  /**
  * ID of the existing split to show the buffer in (required)
  */
  splitId: number;
  /**
  * Mode for keybindings (e.g., "git-log", "search-results")
  */
  mode?: string;
  /**
  * Whether buffer is read-only (default: false)
  */
  readOnly?: boolean;
  /**
  * Show line numbers in gutter (default: true)
  */
  showLineNumbers?: boolean;
  /**
  * Show cursor (default: true)
  */
  showCursors?: boolean;
  /**
  * Disable text editing (default: false)
  */
  editingDisabled?: boolean;
  /**
  * Enable line wrapping
  */
  lineWrap?: boolean;
  /**
  * Initial content as **spans, concatenated verbatim** — a span is a run
  * of text with optional styling, not a line. Nothing inserts newlines
  * for you, so `[{text:"a"},{text:"b"}]` is the single line `ab`. Include
  * `\n` yourself — `[{text:"a\n"},{text:"b\n"}]` is two lines.
  *
  * If you are an agent putting text in front of a human, prefer writing a
  * file and opening it (`splitWindow({ file })` /
  * `openFileInSplit(splitId, path)`): you get syntax highlighting, search,
  * save, and ANSI escape codes rendered as colour, none of which a virtual
  * buffer gives you. Virtual buffers are for plugin-owned panels —
  * ephemeral, styled per span, driven by a mode's keybindings.
  */
  entries?: Array<TextPropertyEntry>;
  /**
  * Initial cursor line (0-indexed). Applied to the new buffer *before*
  * it becomes the active buffer; see the matching field on
  * `CreateVirtualBufferOptions` for the rationale.
  */
  initialCursorLine?: number;
};
```

### `CreateVirtualBufferInSplitOptions`

Options for createVirtualBufferInSplit

```typescript
type CreateVirtualBufferInSplitOptions = {
  /**
  * Buffer name (displayed in tabs/title). By convention it is wrapped
  * in asterisks, e.g. `"*Diagnostics*"`
  */
  name: string;
  /**
  * Mode for keybindings (e.g., "git-log", "search-results"); define
  * it with `defineMode` first
  */
  mode?: string;
  /**
  * Whether buffer is read-only (default: false)
  */
  readOnly?: boolean;
  /**
  * Split ratio 0.0-1.0 (default: 0.5): the share of the first pane,
  * which is the existing content unless `before` is set
  */
  ratio?: number;
  /**
  * Split direction: `"horizontal"` or `"vertical"`.
  *
  * The name describes the **divider**, not the arrangement:
  * `"vertical"` puts the panes side by side (a vertical divider between
  * them), `"horizontal"` stacks them. Same convention as `splitWindow`.
  * Default: `"horizontal"`.
  */
  direction?: string;
  /**
  * Panel ID to split from
  */
  panelId?: string;
  /**
  * Show line numbers in gutter (default: true)
  */
  showLineNumbers?: boolean;
  /**
  * Show cursor (default: true)
  */
  showCursors?: boolean;
  /**
  * Disable text editing (default: false): typing, deletion, cut,
  * paste, undo and redo are refused, while navigation, selection and
  * copy still work
  */
  editingDisabled?: boolean;
  /**
  * Enable line wrapping (default: follow the editor's line-wrap
  * setting)
  */
  lineWrap?: boolean;
  /**
  * Place the new buffer before (left/top of) the existing content (default: false)
  */
  before?: boolean;
  /**
  * Initial content as **spans, concatenated verbatim** — a span is a run
  * of text with optional styling, not a line. Nothing inserts newlines
  * for you, so `[{text:"a"},{text:"b"}]` is the single line `ab`. Include
  * `\n` yourself — `[{text:"a\n"},{text:"b\n"}]` is two lines.
  *
  * If you are an agent putting text in front of a human, prefer writing a
  * file and opening it (`splitWindow({ file })` /
  * `openFileInSplit(splitId, path)`): you get syntax highlighting, search,
  * save, and ANSI escape codes rendered as colour, none of which a virtual
  * buffer gives you. Virtual buffers are for plugin-owned panels —
  * ephemeral, styled per span, driven by a mode's keybindings.
  */
  entries?: Array<TextPropertyEntry>;
  /**
  * Split role tag. When set to `"utility_dock"`, the dispatcher
  * routes this buffer to the existing dock leaf if one exists,
  * instead of creating a new split. See
  * `docs/internal/tui-editor-layout-design.md` Section 2.
  */
  role?: string;
  /**
  * Whether the buffer is user-scrollable (default: true). Set to
  * `false` for self-managing widget panels (those whose List/Tree
  * owns its own scroll window): it suppresses the buffer scrollbar
  * and pins the viewport so a drag can't push the panel chrome
  * off-screen and reveal empty space below.
  */
  scrollable?: boolean;
};
```

### `CreateVirtualBufferOptions`

Options for createVirtualBuffer

```typescript
type CreateVirtualBufferOptions = {
  /**
  * Buffer name (displayed in tabs/title). By convention it is wrapped
  * in asterisks, e.g. `"*Diagnostics*"`
  */
  name: string;
  /**
  * Mode for keybindings (e.g., "git-log", "search-results"); define
  * it with `defineMode` first
  */
  mode?: string;
  /**
  * Whether buffer is read-only (default: false)
  */
  readOnly?: boolean;
  /**
  * Show line numbers in gutter (default: false)
  */
  showLineNumbers?: boolean;
  /**
  * Show cursor (default: true)
  */
  showCursors?: boolean;
  /**
  * Disable text editing (default: false): typing, deletion, cut,
  * paste, undo and redo are refused, while navigation, selection and
  * copy still work
  */
  editingDisabled?: boolean;
  /**
  * Hide from tab bar (default: false)
  */
  hiddenFromTabs?: boolean;
  /**
  * Open as a tab without taking the view (default: false).
  *
  * Creating a virtual buffer otherwise makes it the active buffer, and
  * there is no quiet way back: switching away afterwards is a second
  * visible switch, and the layout the panel composed while it briefly
  * held the pane is not the one it gets later. Set this when the buffer
  * is one the editor offers rather than one the reader asked for — a
  * startup page beside a restored session — and it appears in the tab
  * bar with the current buffer left alone.
  *
  * Ignored together with `hiddenFromTabs`, which has no tab bar to be
  * background in.
  */
  background?: boolean;
  /**
  * Current-line highlight for this buffer (default: follow the editor
  * setting). Pass `false` for a page whose rows are laid out by a widget
  * panel — the caret's line means nothing to the reader there, and a
  * lit band across a centred wordmark is noise.
  */
  highlightCurrentLine?: boolean;
  /**
  * Whether the buffer is user-scrollable (default: true). `false` for a
  * buffer a widget panel is mounted into: the panel is described in the
  * tree and its widgets — or, for a `page` panel, its one viewport —
  * own the scrolling, and the buffer under them never moves.
  */
  scrollable?: boolean;
  /**
  * Initial content as **spans, concatenated verbatim** — a span is a run
  * of text with optional styling, not a line. Nothing inserts newlines
  * for you, so `[{text:"a"},{text:"b"}]` is the single line `ab`. Include
  * `\n` yourself — `[{text:"a\n"},{text:"b\n"}]` is two lines.
  *
  * If you are an agent putting text in front of a human, prefer writing a
  * file and opening it (`splitWindow({ file })` /
  * `openFileInSplit(splitId, path)`): you get syntax highlighting, search,
  * save, and ANSI escape codes rendered as colour, none of which a virtual
  * buffer gives you. Virtual buffers are for plugin-owned panels —
  * ephemeral, styled per span, driven by a mode's keybindings.
  */
  entries?: Array<TextPropertyEntry>;
  /**
  * Show the new buffer in this existing pane instead of taking over the
  * focused one.
  *
  * Without it the buffer becomes active in whichever pane has focus —
  * which is what you want for a panel the user just asked for, and
  * emphatically not what you want when arranging a layout, where it
  * silently replaces whatever the user was looking at.
  */
  splitId?: number;
  /**
  * Initial cursor line (0-indexed). Applied to the new buffer *before*
  * it becomes the active buffer, so plugins that want to land the
  * cursor on a specific line don't have to chase a race against user
  * input between "buffer becomes active" and a follow-up
  * `setBufferCursor`. Using a line index (rather than a byte offset)
  * keeps the byte-math on the host side where the buffer content is
  * already in UTF-8 bytes, avoiding the UTF-16-vs-UTF-8 mismatch a
  * plugin would otherwise have to navigate.
  */
  initialCursorLine?: number;
  /**
  * Override indentation-guide visibility for this buffer (default: follows
  * the global setting, but virtual buffers show none). Set `true` when the
  * buffer displays real source — e.g. a file opened at a past commit.
  */
  indentationGuide?: boolean;
};
```

### `CreateWindowWithTerminalOptions`

Options for `createWindowWithTerminal` — the atomic
"spawn a new editor session that hosts an agent terminal"
entry point used by Orchestrator. Bundles window creation,
dive, and terminal spawn so the new window is born with the
terminal as its seed buffer (no transient `[No Name]` tab,
no race between create-window and create-terminal completing).

```typescript
type CreateWindowWithTerminalOptions = {
  /**
  * Absolute path to the new session's worktree / project
  * root. Relative paths are rejected (logged, no window
  * created).
  */
  root: string;
  /**
  * Human-readable label for the new session. When empty,
  * defaults to the basename of `root`.
  */
  label: string;
  /**
  * Working directory for the spawned terminal. Defaults to
  * `root` when omitted.
  */
  cwd?: string;
  /**
  * Argv to spawn directly inside the PTY. Omit it to keep the
  * shell-and-type behaviour; set to `[cmd, ...args]`, it runs the
  * command as the PTY child (used by Orchestrator so the
  * agent process is the PTY's direct child).
  */
  command?: Array<string>;
  /**
  * Tab title override. Defaults to `command[0]`'s basename
  * when `command` is set, or "Terminal N" otherwise.
  */
  title?: string;
  /**
  * Argv to run on *restore* instead of re-running `command`, when
  * the session is reopened after an editor restart. Used by
  * Orchestrator agent-resume: a session launched with
  * `claude --session-id <id>` sets `resume` to
  * `["claude", "--resume", "<id>"]` (or `["claude", "--continue"]`),
  * so a restored session rejoins its conversation rather than starting
  * a fresh agent. Omit it to keep `command` as the restore command. The id
  * is a plain argv element — never interpolated into a shell string.
  */
  resume?: Array<string>;
  /**
  * Extra environment variables to set in the spawned
  * terminal's child process, on top of the inherited/activated
  * env. Applied after the editor's control vars (`TERM`,
  * `FRESH_SESSION`), so a plugin's entry wins over those only
  * when it names the same key. Omitted (the default), it adds
  * nothing — old callers behave exactly as before.
  */
  env?: { [key in string] : string };
  /**
  * When set, the host mints an unforgeable capability token bound
  * to the NEW window and injects it into the spawned terminal as
  * `FRESH_CMD_TOKEN`. A client presenting that token over the
  * control socket may drive this window by submitting scripts.
  * `false` (the default) mints no token and injects nothing.
  *
  * The grant is all-or-nothing on purpose: a script can call
  * anything the plugin API exposes, so a narrower list would
  * describe a boundary that isn't there.
  */
  allowScript?: boolean;
  /**
  * Seed the terminal into this **existing** window — one created by
  * `createPreparingWindow` — instead of opening a new one. The window
  * keeps its id, its durable `stableId`, and everything keyed off them
  * (a manual rename, its folder, its dock position), so a workspace the
  * user has been looking at (and organising) since they asked for it
  * becomes the live session rather than being replaced by one.
  *
  * `root` still applies: a workspace opens as a placeholder before its
  * worktree exists, so adopting it re-roots the window at the directory
  * that was finally created. Ignored — and the call falls back to
  * creating a fresh window — when the id names no preparing window.
  */
  adoptWindow?: number;
};
```

### `CursorInfo`

Information about a cursor in the editor: its position, its line, and
its selection if it has one

```typescript
type CursorInfo = {
  /**
  * Byte position of the cursor
  */
  position: number;
  /**
  * Selection range (if any)
  */
  selection: {
    start: number;
    end: number;
  } | null;
  /**
  * 0-indexed line number of the cursor. `null` when the line index is
  * unavailable — e.g. a huge file whose line scan hasn't completed, where
  * the editor positions purely by byte offset. Plugins must treat `null`
  * as "unknown", never as line 0.
  */
  line: number | null;
};
```

### `DiffBaselineResult`

Result of a host-side baseline diff (`diffAgainstBaseline` /
`diffBaselinePair`).

```typescript
type DiffBaselineResult = {
  /**
  * The buffer content version the hunks were computed against (0 for
  * baseline-pair diffs, which involve no live buffer). A plugin that
  * renders decorations re-checks this against the buffer's current
  * version instead of copying buffer text around for coherence.
  */
  revision: bigint;
  /**
  * "exact": content-accurate line hunks. "byteCoarse": the buffer's
  * line index isn't available yet (large file before its line-feed
  * scan), so no line hunks could be produced; callers fall back to
  * their own coarse rendering.
  */
  fidelity: "exact" | "byteCoarse";
  /**
  * Line hunks, same contract as `computeLineDiff`. Empty means the
  * sides are equal (when `fidelity` is "exact").
  */
  hunks: Array<LineDiffHunk>;
};
```

### `DirEntry`

Directory entry returned by readDir

```typescript
type DirEntry = {
  /**
  * File/directory name only, not the full path: join it with the
  * directory that was read to get the entry's path
  */
  name: string;
  /**
  * True if this is a file
  */
  is_file: boolean;
  /**
  * True if this is a directory. A symlink reports the type of its
  * target, so a link to a directory is a directory here
  */
  is_dir: boolean;
};
```

### `Elide`

How text that does not fit the width layout gave it gives up the cells.

**The cut is the run's to mark, for the same reason the width is the
box's**: only measurement knows whether the text fit, so a plugin that
appended its own ellipsis had to be told a width first — which is the
duplication this removes. Mirrors `fresh_ui::desc::Elide`; ignored by
wrapping text, which has no overflow to mark.

```typescript
type Elide = "none" | "tail" | "head";
```

### `FileExplorerDecoration`

Decoration metadata for a file explorer entry, provided by a plugin
through `setFileExplorerDecorations`.

```typescript
type FileExplorerDecoration = {
  /**
  * File path to decorate: absolute, or relative to the file explorer's
  * root. Paths outside the root are ignored.
  */
  path: string;
  /**
  * Symbol to display (e.g., "●", "M", "A"). Only its first character is
  * shown, so use a single character.
  */
  symbol: string;
  /**
  * Color as RGB array or theme key string (e.g., "ui.file_status_added_fg")
  */
  color: OverlayColorSpec;
  /**
  * Priority for display when multiple decorations exist (higher wins)
  */
  priority: number;
};
```

### `FileExplorerLeadingSlot`

Leading-slot content for a file explorer row.

```typescript
type FileExplorerLeadingSlot = {
  /**
  * Text shown in the leading slot (for example, an icon glyph).
  */
  text: string;
  /**
  * Foreground colour for the leading slot.
  */
  color: OverlayColorSpec;
  /**
  * Minimum display width reserved for the leading slot.
  */
  minWidth: number;
};
```

### `FileExplorerSlotEntry`

Additive slot override for a file explorer entry.

Any field left `null` falls back to the editor's compatibility providers,
so plugins can override just the piece they care about.

```typescript
type FileExplorerSlotEntry = {
  /**
  * File or directory path to override.
  */
  path: string;
  /**
  * Optional leading-slot override.
  */
  leading: FileExplorerLeadingSlot | null;
  /**
  * Explicitly suppress the compatibility leading slot for this path.
  */
  suppressLeading: boolean;
  /**
  * Optional trailing-slot override.
  */
  trailing: FileExplorerTrailingSlot | null;
  /**
  * Explicitly suppress the compatibility trailing slot for this path.
  */
  suppressTrailing: boolean;
  /**
  * Optional filename colour override.
  */
  nameColor: OverlayColorSpec | null;
  /**
  * Explicitly suppress compatibility filename colouring for this path.
  */
  suppressNameColor: boolean;
  /**
  * Priority for display when multiple overrides exist (higher wins).
  */
  priority: number;
};
```

### `FileExplorerTooltip`

Tooltip content shown when hovering a trailing file-explorer slot.

```typescript
type FileExplorerTooltip = {
  /**
  * Tooltip title shown in the popup border.
  */
  title: string;
  /**
  * Body lines shown inside the popup.
  */
  lines: Array<string>;
};
```

### `FileExplorerTrailingSlot`

Trailing-slot content for a file explorer row.

```typescript
type FileExplorerTrailingSlot = {
  /**
  * Text shown in the trailing slot (for example, a badge glyph).
  */
  text: string;
  /**
  * Foreground colour for the trailing slot.
  */
  color: OverlayColorSpec;
  /**
  * Optional tooltip shown when hovering the trailing slot.
  */
  tooltip: FileExplorerTooltip | null;
};
```

### `FilePrefix`

One result from `readFilePrefixes`: `text` on success, else `error`.

```typescript
interface FilePrefix {
  path: string;
  text?: string;
  error?: string;
}
```

### `FormatterPackConfig`

Formatter configuration for language packs

```typescript
type FormatterPackConfig = {
  /**
  * Command to run (e.g., "prettier", "rustfmt")
  */
  command: string;
  /**
  * Arguments to pass to the formatter
  */
  args: Array<string>;
};
```

### `FreshConfig`

The editor's configuration, as returned by `editor.getConfig()` and `editor.getUserConfig()`. Generated from the config's JSON Schema.

Every property is optional: `getUserConfig()` returns only the values the user set, and `getConfig()` the full merged config.

```typescript
interface FreshConfig {
  /**
  * Config format version, used for migration. Missing means version 0.
  *
  * Default: `0`
  */
  version?: number;
  /**
  * Color theme name
  *
  * Default: `"high-contrast"`
  */
  theme?: FreshConfig.ThemeOptions;
  /**
  * UI language. If not set, detected from LC_ALL, LC_MESSAGES or LANG.
  */
  locale?: FreshConfig.LocaleOptions;
  /**
  * Check for new versions on startup (default: true).
  * Also sends basic anonymous telemetry (version, OS, terminal type).
  *
  * Default: `true`
  */
  check_for_updates?: boolean;
  /**
  * Offer to update from inside the editor when a new version is found (default: true).
  * When off, the status-bar indicator only tells you an update exists.
  * Needs `check_for_updates` and an install method that can self-update.
  *
  * Default: `true`
  */
  self_update?: boolean;
  /**
  * When on (default), `fresh` with no arguments reattaches to a single background
  * editor with the workspace dock and your last workspace. When off, it opens a
  * plain editor in the current directory; runs with files or flags are unaffected.
  *
  * Default: `true`
  */
  orchestrator_mode?: boolean;
  /**
  * Editor behavior settings (indentation, line numbers, wrapping, etc.)
  */
  editor?: FreshConfig.EditorConfig;
  /**
  * File explorer panel settings
  */
  file_explorer?: FreshConfig.FileExplorerConfig;
  /**
  * Sidebar settings (the column that holds the file explorer).
  *
  * Default: `{"accordion":"free"}`
  */
  sidebar?: FreshConfig.SidebarConfig;
  /**
  * File browser settings (Open File dialog)
  *
  * Default: `{"show_hidden":false}`
  */
  file_browser?: FreshConfig.FileBrowserConfig;
  /**
  * Clipboard settings (which clipboard methods to use)
  *
  * Default: `{"use_osc52":true,"use_system_clipboard":true}`
  */
  clipboard?: FreshConfig.ClipboardConfig;
  /**
  * Terminal settings
  */
  terminal?: FreshConfig.TerminalConfig;
  /**
  * Custom keybindings (overrides for the active map)
  */
  keybindings?: FreshConfig.Keybinding[];
  /**
  * Custom named keybinding maps. A map can inherit from another map.
  */
  keybinding_maps?: Record<string, FreshConfig.KeymapConfig>;
  /**
  * Active keybinding map name
  *
  * Default: `"default"`
  */
  active_keybinding_map?: FreshConfig.KeybindingMapOptions;
  /**
  * Per-language configuration overrides (tab size, formatters, etc.)
  */
  languages?: Record<string, FreshConfig.LanguageConfig>;
  /**
  * Language used for files whose type can't be detected.
  * Must be a key in `languages` (e.g. "bash"); its full settings apply.
  */
  default_language?: string | null;
  /**
  * Master switch for language servers. When off, no server starts automatically;
  * you can still start one with "Start/Restart LSP Server".
  *
  * Default: `true`
  */
  lsp_enabled?: boolean;
  /**
  * Language servers per language. Each language takes one server or a list.
  */
  lsp?: Record<string, FreshConfig.LspLanguageConfig>;
  /**
  * Language servers for all languages, run alongside those in `lsp`.
  * Keyed by a unique server name (e.g. "quicklsp").
  */
  universal_lsp?: Record<string, FreshConfig.LspLanguageConfig>;
  /**
  * Warning notification settings
  *
  * Default: `{"show_status_indicator":true}`
  */
  warnings?: FreshConfig.WarningsConfig;
  /**
  * Per-plugin settings, by plugin name. Use it to turn plugins on or off.
  */
  plugins?: Record<string, FreshConfig.PluginConfig>;
  /**
  * Package manager settings for plugin/theme installation
  */
  packages?: FreshConfig.PackagesConfig;
  /**
  * Auto-activation of project environments (venv, direnv, mise, …).
  */
  env?: FreshConfig.EnvConfig;
}
```

### `FreshConfig.ClipboardConfig`

Which clipboard methods copy/paste uses, tried in order: OSC 52 (modern
terminals like Kitty, WezTerm), the system clipboard (X11/Wayland), then an
internal clipboard. Disable a method if it hangs (e.g. PuTTY, some SSH setups).

```typescript
interface ClipboardConfig {
    /**
    * Use the terminal's OSC 52 clipboard (default: true). Turn off if unsupported
    * or it hangs.
    *
    * Default: `true`
    */
    use_osc52?: boolean;
    /**
    * Use the X11/Wayland system clipboard (default: true). Turn off if there's
    * no display server or it causes problems.
    *
    * Default: `true`
    */
    use_system_clipboard?: boolean;
  }
```

### `FreshConfig.CursorStyle`

Terminal cursor style

```typescript
type CursorStyle = "default" | "blinking_block" | "steady_block" | "blinking_bar" | "steady_bar" | "blinking_underline" | "steady_underline";
```

### `FreshConfig.EditorConfig`

Editor behavior configuration

```typescript
interface EditorConfig {
    /**
    * Enable UI animations (tab-switch slides, dashboard, plugin effects).
    * Turn off for a fully static UI, e.g. on slow terminals or over SSH.
    *
    * Default: `true`
    */
    animations?: boolean;
    /**
    * Show a trail animation when the cursor jumps far (search, go-to-definition,
    * pane switch). Needs `animations` on.
    *
    * Default: `true`
    */
    cursor_jump_animation?: boolean;
    /**
    * Fade out the top and bottom two rows of each pane when there is more
    * text beyond that edge. The edge the cursor is on is never faded.
    *
    * Default: `true`
    */
    viewport_edge_fade?: boolean;
    /**
    * Show line numbers in the gutter (default for new buffers).
    *
    * Default: `true`
    */
    line_numbers?: boolean;
    /**
    * Show line numbers relative to the cursor line.
    *
    * Default: `false`
    */
    relative_line_numbers?: boolean;
    /**
    * Highlight the line containing the cursor
    *
    * Default: `true`
    */
    highlight_current_line?: boolean;
    /**
    * Highlight all occurrences of the word under the cursor
    *
    * Default: `true`
    */
    highlight_occurrences?: boolean;
    /**
    * Hide the current-line highlight while text is selected. Default: false
    *
    * Default: `false`
    */
    hide_current_line_on_selection?: boolean;
    /**
    * Highlight the column containing the cursor
    *
    * Default: `false`
    */
    highlight_current_column?: boolean;
    /**
    * Wrap long lines to fit the window width (default for new views)
    *
    * Default: `true`
    */
    line_wrap?: boolean;
    /**
    * Indent wrapped lines to match the original line's indentation.
    *
    * Default: `true`
    */
    wrap_indent?: boolean;
    /**
    * Number of text columns to wrap lines at (e.g. `80`), not counting the
    * line-number gutter. Never wider than the window.
    * `null` or `0` (default) wraps at the window edge.
    */
    wrap_column?: number | null;
    /**
    * Text width in columns in page view, centered with margins. Default: 80.
    * `null` or `0` uses the full window width.
    *
    * Default: `80`
    */
    page_width?: number | null;
    /**
    * Enable syntax highlighting for code files.
    *
    * Default: `true`
    */
    syntax_highlighting?: boolean;
    /**
    * Show the menu bar (File, Edit, View, …) at the top. Can be toggled at any time.
    * Default: true
    *
    * Default: `true`
    */
    show_menu_bar?: boolean;
    /**
    * Show a wave-animation screensaver after `screensaver_idle_minutes` with no
    * input. Any key or mouse move ends it. Default: false
    *
    * Default: `false`
    */
    screensaver_enabled?: boolean;
    /**
    * Idle minutes before the screensaver starts. `0` disables it. Default: 5
    *
    * Default: `5`
    */
    screensaver_idle_minutes?: number;
    /**
    * Alt+letter opens menus (Alt+F for File, Alt+E for Edit, …). Turn off to free
    * Alt+letter keys for other bindings. Default: true
    *
    * Default: `true`
    */
    menu_bar_mnemonics?: boolean;
    /**
    * Show the tab bar of open files in each pane. Can be toggled at any time.
    * Default: true
    *
    * Default: `true`
    */
    show_tab_bar?: boolean;
    /**
    * Show the status bar at the bottom. Can be toggled at any time.
    * Default: true
    *
    * Default: `true`
    */
    show_status_bar?: boolean;
    /**
    * Which elements appear in the status bar and in what order.
    */
    status_bar?: StatusBarConfig;
    /**
    * Starting state of the Case / Word / Regex search toggles. Toggles you
    * change are remembered per workspace and take priority.
    */
    search?: SearchConfig;
    /**
    * Always keep the bottom prompt line (search, open file, …) visible.
    * When off (default), it only appears while a prompt is open.
    *
    * Default: `false`
    */
    show_prompt_line?: boolean;
    /**
    * Show a vertical scrollbar in each pane. Can be toggled at any time.
    * Default: true
    *
    * Default: `true`
    */
    show_vertical_scrollbar?: boolean;
    /**
    * Show a horizontal scrollbar in each pane when line wrap is off and lines
    * are wider than the window. Can be toggled at any time. Default: false
    *
    * Default: `false`
    */
    show_horizontal_scrollbar?: boolean;
    /**
    * Show vim-style `~` markers on empty lines past the end of the file.
    * Default: true
    *
    * Default: `true`
    */
    show_tilde?: boolean;
    /**
    * Use Nerd Font icons in the UI (e.g. settings category icons). Only turn on
    * if your terminal uses a Nerd Font, or icons show as `?` or boxes. Default: false
    *
    * Default: `false`
    */
    nerd_font_icons?: boolean;
    /**
    * Use the terminal's background instead of the theme's, so terminal
    * transparency or custom backgrounds show through. Default: false
    *
    * Default: `false`
    */
    use_terminal_bg?: boolean;
    /**
    * Set the terminal window title to "<file> — Fresh" for the active buffer.
    * Default: true
    *
    * Default: `true`
    */
    set_window_title?: boolean;
    /**
    * Name terminal tabs after the running program (e.g. `python3`) and its title,
    * instead of the fixed `*Terminal N*`. Default: true
    *
    * Default: `true`
    */
    terminal_auto_title?: boolean;
    /**
    * Cursor shape: block, bar or underline, blinking or steady.
    * Default: the terminal's own style.
    *
    * Default: `"default"`
    */
    cursor_style?: CursorStyle;
    /**
    * Draw vertical lines at these columns, e.g. [80, 120]. Default: none.
    * Columns count screen cells from 1, so tabs and wide characters (CJK, emoji)
    * can differ from the status bar's column.
    */
    rulers?: number[];
    /**
    * Vertical lines at each indent level (based on tab size). Display only.
    * `none` (default): off. `all`: every level. `active`: only the cursor's block.
    *
    * Default: `"none"`
    */
    indentation_guide?: IndentationGuideMode;
    /**
    * Character for indentation guides; use a single-width one. Blank resets
    * to the default. Default: ▏
    *
    * Default: `"▏"`
    */
    indentation_guide_glyph?: string;
    /**
    * Color indentation guides by level, using the theme's `indent_rainbow_1`–`6`
    * colors. Default: false
    *
    * Default: `false`
    */
    rainbow_indentation?: boolean;
    /**
    * Master switch for whitespace markers (·, →). When off, none are shown,
    * whatever the settings below say. Default: true
    *
    * Default: `true`
    */
    whitespace_show?: boolean;
    /**
    * Show · for spaces used as indentation. Default: false
    *
    * Default: `false`
    */
    whitespace_spaces_leading?: boolean;
    /**
    * Show · for spaces between words. Default: false
    *
    * Default: `false`
    */
    whitespace_spaces_inner?: boolean;
    /**
    * Show · for spaces at the end of a line. Default: false
    *
    * Default: `false`
    */
    whitespace_spaces_trailing?: boolean;
    /**
    * Show → for tabs used as indentation. Languages can override this with
    * `show_whitespace_tabs`. Default: true
    *
    * Default: `true`
    */
    whitespace_tabs_leading?: boolean;
    /**
    * Show → for tabs between words. Languages can override this with
    * `show_whitespace_tabs`. Default: true
    *
    * Default: `true`
    */
    whitespace_tabs_inner?: boolean;
    /**
    * Show → for tabs at the end of a line. Languages can override this with
    * `show_whitespace_tabs`. Default: true
    *
    * Default: `true`
    */
    whitespace_tabs_trailing?: boolean;
    /**
    * Show ↵ at the end of every line. Default: false
    *
    * Default: `false`
    */
    whitespace_newlines?: boolean;
    /**
    * Show ␍ for the CR in CRLF line endings. Stray CR characters always show
    * as `<0D>`. Default: false
    *
    * Default: `false`
    */
    whitespace_carriage_returns?: boolean;
    /**
    * Show whitespace markers (·, →) inside a selection, even if they are
    * hidden elsewhere. Default: true
    *
    * Default: `true`
    */
    whitespace_in_selection?: boolean;
    /**
    * Tab key inserts a tab character instead of spaces. Languages can
    * override this. Default: false (spaces)
    *
    * Default: `false`
    */
    use_tabs?: boolean;
    /**
    * Spaces per tab. `0` means the default (4).
    *
    * Default: `4`
    */
    tab_size?: number;
    /**
    * Indent new lines to match the previous line.
    *
    * Default: `true`
    */
    auto_indent?: boolean;
    /**
    * Typing `(`, `[`, `{`, `"`, `'` or `` ` `` inserts the closing one too. Typing a
    * closer skips over an existing one; Backspace between a pair deletes both. Default: true
    *
    * Default: `true`
    */
    auto_close?: boolean;
    /**
    * Typing `(`, `[`, `{`, `"`, `'` or `` ` `` with text selected wraps the
    * selection instead of replacing it. Default: true
    *
    * Default: `true`
    */
    auto_surround?: boolean;
    /**
    * Let the cursor go past the end of a line. "off" (default): never.
    * "block": only block selections. "on": also arrows and clicks; typing there fills with spaces.
    *
    * Default: `"off"`
    */
    virtual_space?: VirtualSpaceMode;
    /**
    * Lines to keep visible above and below the cursor when scrolling.
    *
    * Default: `3`
    */
    scroll_offset?: number;
    /**
    * Line ending for new files; opened files keep their own. "lf" (Unix/macOS,
    * default), "crlf" (Windows) or "cr" (classic Mac).
    *
    * Default: `"lf"`
    */
    default_line_ending?: LineEndingOption;
    /**
    * Remove spaces at line ends when saving. Default: false
    *
    * Default: `false`
    */
    trim_trailing_whitespace_on_save?: boolean;
    /**
    * Add a final newline when saving, if missing. Default: false
    *
    * Default: `false`
    */
    ensure_final_newline_on_save?: boolean;
    /**
    * Open files read-only if they aren't writable or are in a library folder
    * (node_modules, rustup, /usr/include, /nix/store, …). Binary files are
    * always read-only. Default: true
    *
    * Default: `true`
    */
    auto_read_only?: boolean;
    /**
    * Highlight the matching bracket when the cursor is on a bracket. Default: true
    *
    * Default: `true`
    */
    highlight_matching_brackets?: boolean;
    /**
    * Color nested brackets by depth. Needs `highlight_matching_brackets`. Default: true
    *
    * Default: `true`
    */
    rainbow_brackets?: boolean;
    /**
    * Show the completion popup automatically while typing. When off (default),
    * it only opens on request (e.g. Ctrl+Space).
    *
    * Default: `false`
    */
    completion_popup_auto_show?: boolean;
    /**
    * Suggest completions on any typing, not just after `.` or `::`.
    * Needs `completion_popup_auto_show`. Default: true
    *
    * Default: `true`
    */
    quick_suggestions?: boolean;
    /**
    * Milliseconds to wait before showing suggestions. Trigger characters like `.`
    * skip the wait. Default: 150
    *
    * Default: `150`
    */
    quick_suggestions_delay_ms?: number;
    /**
    * Show completions right away after `.`, `::` or `->`, with no delay. Default: true
    *
    * Default: `true`
    */
    suggest_on_trigger_characters?: boolean;
    /**
    * Show inline type and parameter hints from the language server.
    *
    * Default: `true`
    */
    enable_inlay_hints?: boolean;
    /**
    * Request semantic highlighting for the whole file, not just the visible
    * range. Default: false (lighter).
    *
    * Default: `false`
    */
    enable_semantic_tokens_full?: boolean;
    /**
    * Show the most severe error/warning message at the end of its line. Default: false
    *
    * Default: `false`
    */
    diagnostics_inline_text?: boolean;
    /**
    * Lines scrolled per mouse wheel notch, everywhere (minimum 1). Shift+wheel
    * is not affected. Default: 3
    *
    * Default: `3`
    */
    mouse_wheel_scroll_lines?: number;
    /**
    * Scroll a wheel notch one line at a time instead of jumping. Slow terminals
    * fall back to a jump. Needs `animations` on.
    *
    * Default: `true`
    */
    smooth_scroll?: boolean;
    /**
    * Show documentation when hovering the mouse over code.
    * On Windows it also turns on full mouse-motion tracking, which may garble input
    * on some systems. Default: true (macOS/Linux), false (Windows)
    *
    * Default: `true`
    */
    mouse_hover_enabled?: boolean;
    /**
    * Milliseconds before hover info appears. Lower is faster but loads the
    * language server more. Default: 500
    *
    * Default: `500`
    */
    mouse_hover_delay_ms?: number;
    /**
    * Max milliseconds between two clicks to count as a double-click. Default: 500
    *
    * Default: `500`
    */
    double_click_time_ms?: number;
    /**
    * Auto-save changed files to disk every `auto_save_interval_secs`. Default: false
    *
    * Default: `false`
    */
    auto_save_enabled?: boolean;
    /**
    * Seconds between auto-saves, when `auto_save_enabled` is on. Default: 30
    *
    * Default: `30`
    */
    auto_save_interval_secs?: number;
    /**
    * Keep unsaved changes, including unnamed buffers, when you quit, and restore
    * them next time. Default: true
    *
    * Default: `true`
    */
    hot_exit?: boolean;
    /**
    * Always ask before quitting, even with nothing unsaved. (Unsaved changes are
    * always confirmed.) Default: false
    *
    * Default: `false`
    */
    confirm_quit?: boolean;
    /**
    * Restore tabs, splits, cursors and the file explorer from the last exit in
    * the same directory. The session is still saved when off; `--no-restore`
    * skips both. Default: true
    *
    * Default: `true`
    */
    restore_previous_session?: boolean;
    /**
    * When started with files (e.g. `fresh main.rs`), open only those files
    * instead of the last session. Unsaved changes are still restored. Default: true
    *
    * Default: `true`
    */
    skip_session_restore_when_files_passed?: boolean;
    /**
    * Files whose cursor, scroll and folds are never remembered, because other tools
    * rewrite them. Plain names match exactly; `*`/`?` globs match the full path if they
    * include folders, else the file name. Default: `["**\/.git/**"]` (e.g. `COMMIT_EDITMSG`)
    *
    * Default: `["**\/.git/**"]`
    */
    ephemeral_file_patterns?: string[];
    /**
    * Open an empty `[No Name]` tab when the last tab is closed. When off, the
    * workspace stays blank (with `file_explorer.auto_open_on_last_buffer_close`
    * also off, nothing opens). Default: true
    *
    * Default: `true`
    */
    auto_create_empty_buffer_on_last_buffer_close?: boolean;
    /**
    * Periodically save changes to recovery files, so work survives a crash.
    *
    * Default: `true`
    */
    recovery_enabled?: boolean;
    /**
    * Seconds between recovery saves, when `recovery_enabled` is on. Default: 2
    *
    * Default: `2`
    */
    auto_recovery_save_interval_secs?: number;
    /**
    * How often (ms) to check open files for outside changes when auto-revert is
    * on. Lower is faster but uses more CPU. Default: 2000
    *
    * Default: `2000`
    */
    auto_revert_poll_interval_ms?: number;
    /**
    * Read Escape and modified keys reliably (kitty keyboard protocol, needs
    * terminal support). Default: true
    *
    * Default: `true`
    */
    keyboard_disambiguate_escape_codes?: boolean;
    /**
    * Report key repeat and release events (kitty keyboard protocol, needs
    * terminal support). Default: false
    *
    * Default: `false`
    */
    keyboard_report_event_types?: boolean;
    /**
    * Report alternate keycodes as well as the base key (kitty keyboard protocol,
    * needs terminal support). Default: true
    *
    * Default: `true`
    */
    keyboard_report_alternate_keys?: boolean;
    /**
    * Report every key as an escape code; needed for repeat/release on plain
    * keys (kitty keyboard protocol, needs terminal support). Default: false
    *
    * Default: `false`
    */
    keyboard_report_all_keys_as_escape_codes?: boolean;
    /**
    * Edits between undo history snapshots.
    *
    * Default: `100`
    */
    snapshot_interval?: number;
    /**
    * Bytes around the visible area read for syntax highlighting. More is more
    * accurate for long strings/comments but slower on big files. Default: 10000
    *
    * Default: `10000`
    */
    highlight_context_bytes?: number;
    /**
    * Files over this size (bytes) count as large: they load lazily, skip
    * language servers, and get a fixed-size scrollbar thumb. Default: 10 MB
    *
    * Default: `10485760`
    */
    large_file_threshold_bytes?: number;
    /**
    * Assumed average line length in bytes, used to estimate line positions in
    * large files. Typical: 80–120.
    *
    * Default: `80`
    */
    estimated_line_length?: number;
    /**
    * Max parallel file reads during bulk I/O. Higher helps on remote
    * filesystems. Default: 64
    *
    * Default: `64`
    */
    read_concurrency?: number;
    /**
    * How often (ms) the file explorer checks open folders for added or removed
    * files. Lower is faster but uses more CPU. Default: 3000
    *
    * Default: `3000`
    */
    file_tree_poll_interval_ms?: number;
  }
```

### `FreshConfig.EnvConfig`

Which marker files identify a project environment and how to activate it.
The same markers drive the Workspace Trust prompt. You can add or override detectors.

```typescript
interface EnvConfig {
    /**
    * Detectors in order; the first that matches the workspace root wins.
    * Defaults cover venv, direnv, mise, pipenv and poetry.
    */
    detectors?: EnvDetector[];
  }
```

### `FreshConfig.EnvDetector`

One environment detector: its markers, risk, activation command and name.

```typescript
interface EnvDetector {
    /**
    * Short label shown in the status pill (e.g. ".venv", "direnv", "mise").
    */
    name?: string;
    /**
    * Files or folders at the workspace root; the detector matches if any exists.
    */
    markers?: string[];
    /**
    * Activation risk class.
    */
    kind?: EnvKind;
    /**
    * Shell command that activates the environment, run from the workspace root.
    * Prefer relative paths (e.g. `source .venv/bin/activate`). `{dir}` expands to the
    * workspace root, but avoid it unless you control the path (shell-injection risk).
    */
    snippet?: string;
    /**
    * Paths (relative to the workspace root) of which at least one must also exist,
    * e.g. the Python interpreter inside `.venv`. Empty means markers are enough.
    */
    require?: string[];
  }
```

### `FreshConfig.EnvKind`

How risky activation is; decides whether a trust prompt is needed first.

```typescript
type EnvKind = "path-only" | "shell";
```

### `FreshConfig.ExplorerWidth`

Either a percent like "30%" (0–100) or an absolute column count like "24".

```typescript
type ExplorerWidth = string;
```

### `FreshConfig.FileBrowserConfig`

Open File dialog settings.

```typescript
interface FileBrowserConfig {
    /**
    * Show hidden files (starting with `.`) by default.
    *
    * Default: `false`
    */
    show_hidden?: boolean;
  }
```

### `FreshConfig.FileExplorerConfig`

File explorer settings.

```typescript
interface FileExplorerConfig {
    /**
    * Apply `.gitignore` rules. When off, ignored files are neither hidden nor
    * grayed out. Default: true
    *
    * Default: `true`
    */
    respect_gitignore?: boolean;
    /**
    * Show hidden files (starting with `.`) by default.
    *
    * Default: `false`
    */
    show_hidden?: boolean;
    /**
    * Show gitignored files by default.
    *
    * Default: `false`
    */
    show_gitignored?: boolean;
    /**
    * Extra patterns to ignore, on top of `.gitignore`.
    */
    custom_ignore_patterns?: string[];
    /**
    * Explorer width: a percent (`"30%"`, 0–100) or a column count (`"24"`).
    * A plain number is read as a percent; `0.3` means 30%.
    *
    * Default: `"30%"`
    */
    width?: ExplorerWidth;
    /**
    * A single click opens a temporary preview tab that the next click replaces.
    * Editing, double-click, Enter or dragging the tab makes it permanent. Default: true
    *
    * Default: `true`
    */
    preview_tabs?: boolean;
    /**
    * Screen side for the file explorer. Default: left
    *
    * Default: `"left"`
    */
    side?: FileExplorerSide;
    /**
    * Reveal and select the current file in the tree whenever you switch files.
    * Skipped while the sidebar is hidden or focused, and for files outside the
    * project. Default: false
    *
    * Default: `false`
    */
    follow_active_buffer?: boolean;
    /**
    * Focus the file explorer when the last tab is closed. Turn off for a blank
    * workspace where nothing opens by itself. Default: true
    *
    * Default: `true`
    */
    auto_open_on_last_buffer_close?: boolean;
    /**
    * Show chains of folders that each hold only one folder on one line, e.g.
    * `src/main/java/com/example`. Default: true
    *
    * Default: `true`
    */
    compact_directories?: boolean;
    /**
    * Symbol before a closed folder (one character recommended). Default: ">"
    *
    * Default: `">"`
    */
    tree_indicator_collapsed?: string;
    /**
    * Symbol before an open folder (one character recommended). Default: "▼"
    *
    * Default: `"▼"`
    */
    tree_indicator_expanded?: string;
  }
```

### `FreshConfig.FileExplorerSide`

Side placement for the file explorer panel.

```typescript
type FileExplorerSide = "left" | "right";
```

### `FreshConfig.FormatterConfig`

Formatter for a language.

```typescript
interface FormatterConfig {
    /**
    * Formatter command (e.g. "rustfmt", "prettier").
    */
    command?: string;
    /**
    * Arguments for the formatter. "$FILE" is replaced by the file path.
    */
    args?: string[];
    /**
    * Send the text on stdin and read the result from stdout (default: true).
    *
    * Default: `true`
    */
    stdin?: boolean;
    /**
    * Timeout in milliseconds (default: 10000)
    *
    * Default: `10000`
    */
    timeout_ms?: number;
  }
```

### `FreshConfig.IndentationGuideMode`

Indentation guide rendering mode.

Default: `"none"`

```typescript
type IndentationGuideMode = "none" | "all" | "active";
```

### `FreshConfig.IndentRulesConfig`

Auto-indent rules used when you press Enter. Each is an optional regex (no
look-around or back-references), matched with strings and comments ignored.
Unset patterns keep the language's built-in rules.

```typescript
interface IndentRulesConfig {
    /**
    * If the current line matches, the new line is indented one level deeper.
    * E.g. `[\{\[\(]\s*$` (ends with an open bracket) or `:\s*$` (Python).
    */
    increase_indent_pattern?: string | null;
    /**
    * If a line matches, it is dedented one level, also while typing.
    * E.g. `^\s*[\}\]\)]` (starts with a closing bracket). Include terminators
    * like Python's `:` so words starting with a keyword don't dedent.
    */
    decrease_indent_pattern?: string | null;
    /**
    * Like `increase_indent_pattern`, but only for the next line.
    * E.g. `^\s*(if|for|while)\b.*\)\s*$` (an `if` without braces).
    */
    indent_next_line_pattern?: string | null;
    /**
    * If the current line matches, the next line is dedented one level.
    * E.g. `^\s*(return|pass|raise|break|continue)\b` (Python).
    */
    dedent_next_line_pattern?: string | null;
    /**
    * Cancels `increase_indent_pattern` when the line also closes its block,
    * e.g. Ruby `\bend\b` for `def f; end`.
    */
    self_close_pattern?: string | null;
  }
```

### `FreshConfig.Keybinding`

Keybinding definition

```typescript
interface Keybinding {
    /**
    * Key name (e.g. "a", "Enter", "F1") for a single-key binding.
    */
    key?: string;
    /**
    * Modifiers (e.g. ["ctrl"], ["ctrl", "shift"]) for a single-key binding.
    */
    modifiers?: string[];
    /**
    * Key sequence for a chord, e.g. [{"key": "x", "modifiers": ["ctrl"]}, {"key": "s", "modifiers": ["ctrl"]}].
    * Overrides `key` + `modifiers`.
    */
    keys?: KeyPress[];
    /**
    * The whole binding as one string, e.g. `"C-x"`, `"C-S-Left"`, `"C-x C-s"`.
    * `key` + `modifiers` and `keys` win if set.
    */
    chord?: string;
    /**
    * Action to run (e.g. "move_left"), or "unbind" to remove the built-in binding for this key.
    */
    action?: string;
    /**
    * Arguments for the action (optional).
    */
    args?: Record<string, unknown>;
    /**
    * Condition for when the binding applies (optional, e.g. "mode == insert").
    */
    when?: string | null;
  }
```

### `FreshConfig.KeybindingMapOptions`

Available keybinding maps

```typescript
type KeybindingMapOptions = "default" | "emacs" | "vscode" | "macos" | "macos-gui";
```

### `FreshConfig.KeymapConfig`

A keymap (built-in or custom).

```typescript
interface KeymapConfig {
    /**
    * Keymap to inherit from (optional).
    */
    inherits?: string | null;
    /**
    * Keybindings in this keymap.
    */
    bindings?: Keybinding[];
  }
```

### `FreshConfig.KeyPress`

A single key in a sequence

```typescript
interface KeyPress {
    /**
    * Key name (e.g., "a", "Enter", "F1")
    */
    key?: string;
    /**
    * Modifiers (e.g., ["ctrl"], ["ctrl", "shift"])
    */
    modifiers?: string[];
  }
```

### `FreshConfig.LanguageConfig`

Settings for one language.

```typescript
interface LanguageConfig {
    /**
    * File extensions (e.g. ["rs"] for Rust).
    */
    extensions?: string[];
    /**
    * Exact file names (e.g. ["Makefile", "GNUmakefile"]).
    */
    filenames?: string[];
    /**
    * Tree-sitter grammar name.
    *
    * Default: `""`
    */
    grammar?: string;
    /**
    * Line comment prefix (e.g. "//").
    */
    comment_prefix?: string | null;
    /**
    * Auto-indent new lines.
    *
    * Default: `true`
    */
    auto_indent?: boolean;
    /**
    * Auto-close brackets and quotes. `null` uses `editor.auto_close`.
    */
    auto_close?: boolean | null;
    /**
    * Wrap selections in typed brackets/quotes. `null` uses `editor.auto_surround`.
    */
    auto_surround?: boolean | null;
    /**
    * Custom grammar file for files with this language's extensions (optional).
    * Must be a Sublime Text `.sublime-syntax` file; `.tmLanguage` is not supported.
    */
    textmate_grammar?: string | null;
    /**
    * Show → for tabs (default: true). Turn off for tab-indented languages like Go.
    *
    * Default: `true`
    */
    show_whitespace_tabs?: boolean;
    /**
    * Wrap long lines (e.g. for Markdown). `null` uses `editor.line_wrap`.
    */
    line_wrap?: boolean | null;
    /**
    * Number of text columns to wrap lines at, not counting the line-number
    * gutter. `null` or `0` uses `editor.wrap_column`.
    */
    wrap_column?: number | null;
    /**
    * Open in page view: centered text, hidden formatting marks, smart wrapping.
    * `null` means off.
    */
    page_view?: boolean | null;
    /**
    * Text width in columns in page view. `null` or `0` uses `editor.page_width`.
    */
    page_width?: number | null;
    /**
    * Tab key inserts a tab character (e.g. for Go, Makefile). `null` uses `editor.use_tabs`.
    */
    use_tabs?: boolean | null;
    /**
    * Spaces per tab. `null` or `0` uses `editor.tab_size`.
    */
    tab_size?: number | null;
    /**
    * Formatter used by the Format Buffer command.
    */
    formatter?: FormatterConfig | null;
    /**
    * Run the formatter on save.
    *
    * Default: `false`
    */
    format_on_save?: boolean;
    /**
    * Commands to run on save (e.g. linters), in order; stops at the first failure.
    * For formatting, use `formatter` + `format_on_save` instead.
    */
    on_save?: OnSaveAction[];
    /**
    * Extra characters (besides letters, digits and `_`) that count as part of a
    * word for word completion, e.g. `"-"` for Lisp/CSS, `"$"` for PHP/Bash, `"?!"` for Ruby.
    */
    word_characters?: string | null;
    /**
    * Custom auto-indent rules. Unset patterns keep the built-in ones. Works
    * without a tree-sitter grammar.
    */
    indent?: IndentRulesConfig | null;
    /**
    * Show indentation guides. `null` follows `editor.indentation_guide`,
    * except plain text, where guides are off unless set to `true`.
    */
    indentation_guide?: boolean | null;
  }
```

### `FreshConfig.LineEndingOption`

Default line ending format for new files

Default: `"lf"`

```typescript
type LineEndingOption = "lf" | "crlf" | "cr";
```

### `FreshConfig.LocaleOptions`

UI locale (language). Use null for auto-detection from environment.

```typescript
type LocaleOptions = null | "bg" | "cs" | "de" | "en" | "es" | "fr" | "it" | "ja" | "ko" | "pt-BR" | "ru" | "th" | "uk" | "vi" | "zh-CN";
```

### `FreshConfig.LspFeature`

Language server feature, for routing features to servers when a language has
several. "Merged" features combine results from all servers; "exclusive" ones use the first.

```typescript
type LspFeature = "diagnostics" | "completion" | "code_action" | "document_symbols" | "workspace_symbols" | "hover" | "definition" | "implementation" | "references" | "format" | "rename" | "signature_help" | "inlay_hints" | "folding_range" | "semantic_tokens" | "document_highlight";
```

### `FreshConfig.LspLanguageConfig`

One or more LSP server configs for this language.
Accepts both a single object and an array for backwards compatibility.

```typescript
type LspLanguageConfig = LspServerConfig[];
```

### `FreshConfig.LspServerConfig`

Language server settings.

```typescript
interface LspServerConfig {
    /**
    * Command that starts the server. Required when enabled.
    *
    * Default: `""`
    */
    command?: string;
    /**
    * Enable this server.
    *
    * Default: `true`
    */
    enabled?: boolean;
    /**
    * Display name (e.g. "tsserver"). Defaults to the command's file name.
    */
    name?: string | null;
    /**
    * Arguments for the server. If omitted, the default server's arguments are
    * used; any list, even `[]`, replaces them.
    */
    args?: string[] | null;
    /**
    * Start the server when a matching file opens (default: true). When off,
    * start it from the command palette.
    *
    * Default: `true`
    */
    auto_start?: boolean;
    /**
    * Files or folders that mark the project root: the nearest folder above the
    * file that contains one is used. Empty means `[".git"]`. With no match, the
    * file's own folder is used.
    */
    root_markers?: string[];
    /**
    * Extra environment variables for the server (override inherited ones).
    */
    env?: Record<string, string>;
    /**
    * Language ID to send to the server per file extension (no dot),
    * e.g. `{"tsx": "typescriptreact"}`.
    */
    language_id_overrides?: Record<string, string>;
    /**
    * Server-specific `initializationOptions` sent at startup.
    */
    initialization_options?: unknown;
    /**
    * Use this server only for these features. Don't combine with
    * `except_features`; if neither is set, it handles everything.
    */
    only_features?: LspFeature[] | null;
    /**
    * Use this server for everything except these features. Don't combine with
    * `only_features`.
    */
    except_features?: LspFeature[] | null;
    /**
    * Memory and CPU limits for the server.
    */
    process_limits?: ProcessLimits;
  }
```

### `FreshConfig.OnSaveAction`

Command to run when a file is saved (e.g. a linter).

```typescript
interface OnSaveAction {
    /**
    * Shell command to run. The file path is available as $FILE.
    */
    command?: string;
    /**
    * Arguments for the command. "$FILE" is replaced by the file path.
    */
    args?: string[];
    /**
    * Working directory (default: project root).
    */
    working_dir?: string | null;
    /**
    * Send the file's text on stdin.
    *
    * Default: `false`
    */
    stdin?: boolean;
    /**
    * Timeout in milliseconds (default: 10000)
    *
    * Default: `10000`
    */
    timeout_ms?: number;
    /**
    * Set to false to turn the action off without removing it (default: true).
    *
    * Default: `true`
    */
    enabled?: boolean;
  }
```

### `FreshConfig.PackagesConfig`

Package manager settings for plugins and themes.

```typescript
interface PackagesConfig {
    /**
    * Git repository URLs of plugin/theme registries.
    * Default: ["https://github.com/sinelaw/fresh-plugins-registry"]
    *
    * Default: `["https://github.com/sinelaw/fresh-plugins-registry"]`
    */
    sources?: string[];
  }
```

### `FreshConfig.PluginConfig`

Settings for one plugin.

```typescript
interface PluginConfig {
    /**
    * Load and run this plugin (default: true).
    *
    * Default: `true`
    */
    enabled?: boolean;
    /**
    * Plugin file path. Filled in automatically; don't set it by hand.
    */
    path?: string | null;
    /**
    * The plugin's own settings, as defined by its `<plugin_name>.schema.json`.
    * Shown in Settings as the plugin's page under "Plugins".
    */
    settings?: unknown;
  }
```

### `FreshConfig.ProcessLimits`

Resource limits for a process.

```typescript
interface ProcessLimits {
    /**
    * Max memory as a percent of system memory (default: 50). `null` means no limit.
    */
    max_memory_percent?: number | null;
    /**
    * Max CPU as a percent, where 100 = one core, 200 = two cores. `null` means no limit.
    */
    max_cpu_percent?: number | null;
    /**
    * Apply these limits. Default: true (the built-in config turns them on
    * only on Linux).
    *
    * Default: `true`
    */
    enabled?: boolean;
  }
```

### `FreshConfig.SearchConfig`

Starting state of the search toggles in every search UI (search/replace
prompt, Live Grep, Search & Replace panel).

```typescript
interface SearchConfig {
    /**
    * Match case (default: false, so `todo` also finds `TODO`). Flipping the
    * toggle overrides this and is saved with the workspace.
    *
    * Default: `false`
    */
    case_sensitive?: boolean;
    /**
    * Match whole words only (default: false).
    *
    * Default: `false`
    */
    whole_word?: boolean;
    /**
    * Treat the query as a regular expression (default: false).
    *
    * Default: `false`
    */
    regex?: boolean;
    /**
    * Ask before each replacement (default: false).
    *
    * Default: `false`
    */
    confirm_each?: boolean;
  }
```

### `FreshConfig.SidebarAccordion`

How the sidebar's sections share the column.

```typescript
type SidebarAccordion = "free" | "exclusive";
```

### `FreshConfig.SidebarConfig`

The sidebar column: the file explorer plus any plugin sections below it.

```typescript
interface SidebarConfig {
    /**
    * Open sections freely (`free`, default) or one at a time (`exclusive`).
    *
    * Default: `"free"`
    */
    accordion?: SidebarAccordion;
  }
```

### `FreshConfig.StatusBarConfig`

Which elements appear in the status bar, split into a left and a right
group. Elements can be freely reordered.

Example: `{"left": ["{filename}", "{cursor:compact}"], "right": ["{language}", "{encoding}"]}`

```typescript
interface StatusBarConfig {
    /**
    * Elements on the left side of the status bar.
    * Default: ["{trust}", "{remote}", "{terminal_restart}", "{cursor}", "{diagnostics}", "{cursor_count}", "{messages}"]
    */
    left?: StatusBarElement[];
    /**
    * Elements on the right side of the status bar.
    * Default: ["{read_only}", "{line_ending}", "{encoding}", "{language}", "{lsp}", "{warnings}", "{update}", "{palette}"]
    */
    right?: StatusBarElement[];
    /**
    * Text drawn between status bar elements, used as-is. Each element already
    * has a one-space margin, so `"|"` shows as `LF | UTF-8`. Empty (default) means no separator.
    *
    * Default: `""`
    */
    separator?: string;
  }
```

### `FreshConfig.StatusBarElement`

```typescript
type StatusBarElement = string;
```

### `FreshConfig.TerminalConfig`

Integrated terminal settings.

```typescript
interface TerminalConfig {
    /**
    * Jump back to the live terminal when new output arrives while you're in
    * scrollback (default: true).
    *
    * Default: `true`
    */
    jump_to_end_on_output?: boolean;
    /**
    * Shell for the terminal. Unset (default) uses `$SHELL` or the system default.
    * Doesn't change `$SHELL` for other features, and doesn't apply to remote
    * or container terminals.
    */
    shell?: TerminalShellConfig | null;
    /**
    * Windows only: when picking a shell, skip Microsoft Store app aliases, which
    * can crash on start. Turn off to use the first `pwsh.exe` on `PATH`.
    * Ignored if `terminal.shell` is set. Default: true
    *
    * Default: `true`
    */
    skip_app_execution_alias?: boolean;
    /**
    * When restoring Orchestrator agent sessions, resume the previous
    * conversation (e.g. `claude --resume`) instead of starting clean. Default: true
    *
    * Default: `true`
    */
    resume_agents?: boolean;
    /**
    * Dragging in a terminal selects text for Ctrl+C, switching to scrollback
    * (Ctrl+Space returns). A click only focuses. Default: true
    *
    * Default: `true`
    */
    mouse_drag_selects?: boolean;
    /**
    * When programs in the terminal get mouse events. `requested` (default):
    * only programs that ask for the mouse (plus wheel for full-screen apps);
    * Shift+drag still selects. `alt_screen`: every full-screen program gets all events.
    *
    * Default: `"requested"`
    */
    mouse_forwarding?: TerminalMouseForwarding;
  }
```

### `FreshConfig.TerminalMouseForwarding`

When programs in the terminal get mouse events.

```typescript
type TerminalMouseForwarding = "requested" | "alt_screen";
```

### `FreshConfig.TerminalShellConfig`

Shell command and arguments for the terminal.

```typescript
interface TerminalShellConfig {
    /**
    * Program to run (e.g. `/usr/bin/fish`, `bash`, or a script). Looked up in
    * `$PATH` if not absolute.
    */
    command?: string;
    /**
    * Arguments for the shell.
    */
    args?: string[];
  }
```

### `FreshConfig.ThemeOptions`

Available color themes

```typescript
type ThemeOptions = string;
```

### `FreshConfig.VirtualSpaceMode`

Where the cursor may move beyond the end of a line

Default: `"off"`

```typescript
type VirtualSpaceMode = "off" | "block" | "on";
```

### `FreshConfig.WarningsConfig`

Warning notification settings.

```typescript
interface WarningsConfig {
    /**
    * Show a colored status-bar indicator for errors and warnings (default: true).
    *
    * Default: `true`
    */
    show_status_indicator?: boolean;
  }
```

### `FreshMachine`

A machine opened with `editor.openMachine`. Closed on `close()` or plugin unload.

```typescript
interface FreshMachine {
  id: number;
  /** "linux" | "macos" | "windows" | "other", as the machine reports. */
  platform: string;
  home: string;
  /** The authority's own label, empty for a plain local one. */
  label: string;
  walkTree(root: string, options?: WalkTreeOptions): Promise<WalkTreeResult>;
  readFilePrefixes(requests: {
    path: string;
    maxBytes: number;
  }[]): Promise<FilePrefix[]>;
  run(program: string, args?: string[], cwd?: string): Promise<CommandResult>;
  /** Environment variables, for the names that are set. A remote machine is
  *  asked with `printenv`; never this computer's values for another machine. */
  env(names: string[]): Promise<Record<string, string>>;
  /** Idempotent: closing twice is not an error. */
  close(): Promise<boolean>;
}
```

### `FreshPluginRegistry`

Registry of typed plugin APIs surfaced through
`editor.exportPluginApi` / `editor.getPluginApi`.

Plugins that want their surface to be typed for downstream
consumers augment this interface in their own source:

```ts
// in my_plugin.ts
export type MyPluginApi = { doThing(): void };
declare global {
  interface FreshPluginRegistry {
    "my-plugin": MyPluginApi;
  }
}
```

`editor.getPluginApi("my-plugin")` then returns
`MyPluginApi | null` without any `as`-cast on the consumer side.
Plugins that skip the augmentation still work — the untyped
`getPluginApi<T = unknown>(name: string): T | null` overload
takes over.

Each plugin's augmentation is emitted to
`<config_dir>/types/plugins.d.ts` at load time (via oxc's
isolated-declarations), so init.ts sees every loaded plugin's
registry entry automatically.

```typescript
interface FreshPluginRegistry {}
```

### `GrammarInfoSnapshot`

Grammar info exposed to plugins, mirroring the editor's grammar provenance tracking.

```typescript
type GrammarInfoSnapshot = {
  /**
  * The grammar name as used in config files (case-insensitive matching)
  */
  name: string;
  /**
  * Where this grammar was loaded from (e.g. "built-in", "plugin (myplugin)")
  */
  source: string;
  /**
  * File extensions associated with this grammar
  */
  file_extensions: Array<string>;
  /**
  * Optional short name alias (e.g., "bash" for "Bourne Again Shell (bash)")
  */
  short_name: string | null;
};
```

### `GrepMatch`

A single match from project-wide grep

```typescript
type GrepMatch = {
  /**
  * Absolute file path
  */
  file: string;
  /**
  * Buffer ID if the file is open (0 if not)
  */
  bufferId: number;
  /**
  * Byte offset of match start in the file/buffer content
  */
  byteOffset: number;
  /**
  * Match length in bytes
  */
  length: number;
  /**
  * 1-indexed line number
  */
  line: number;
  /**
  * 1-indexed column number
  */
  column: number;
  /**
  * The matched line content (for display)
  */
  context: string;
};
```

### `HintEntry`

One entry in a `HintBar` — a key chord plus its label.
Renders as `<keys> <label>` with the key portion styled by the
`ui.help_key_fg` theme key.

```typescript
type HintEntry = {
  /**
  * The key chord, e.g. `"Tab"`, `"Alt+P"`, `"Esc"`.
  */
  keys: string;
  /**
  * The human-readable label for the action.
  */
  label: string;
};
```

### `InlineOverlay`

An inline overlay specifying styling for a sub-range within a text entry

```typescript
type InlineOverlay = {
  /**
  * Start offset within the entry's text. See `unit`.
  */
  start: number;
  /**
  * End offset within the entry's text (exclusive). See `unit`.
  */
  end: number;
  /**
  * Styling options for this range
  */
  style: Partial<OverlayOptions>;
  /**
  * Optional properties for this sub-range (e.g., click target metadata)
  */
  properties?: Record<string, any>;
  /**
  * Unit for `start` / `end`. Defaults to `byte`.
  */
  unit?: OffsetUnit;
};
```

### `JsDiagnostic`

Diagnostic from LSP

```typescript
type JsDiagnostic = {
  /**
  * Document URI
  */
  uri: string;
  /**
  * Diagnostic message
  */
  message: string;
  /**
  * Severity: 1=Error, 2=Warning, 3=Info, 4=Hint, null=unknown
  */
  severity: number | null;
  /**
  * Range in the document
  */
  range: JsRange;
  /**
  * Source of the diagnostic (e.g., "typescript", "eslint")
  */
  source?: string;
};
```

### `JsPosition`

Position in a document (line and character)

```typescript
type JsPosition = {
  /**
  * Zero-indexed line number
  */
  line: number;
  /**
  * Zero-indexed character offset
  */
  character: number;
};
```

### `JsRange`

Range in a document (start and end positions)

```typescript
type JsRange = {
  /**
  * Start position
  */
  start: JsPosition;
  /**
  * End position
  */
  end: JsPosition;
};
```

### `KeyEventPayload`

Payload delivered to a plugin's `editor.getNextKey()` Promise when
the next keypress arrives in the editor's input dispatch.

`key` uses the same naming as `defineMode` bindings: lowercase
names like `"escape"`, `"enter"`, `"tab"`, `"space"`, `"left"`,
`"f1"`–`"f12"`, or a single character (e.g. `"a"`, `"!"`).
Modifier flags are reported separately so plugins can recognise
chord variants without parsing.

```typescript
type KeyEventPayload = {
  /**
  * Key name (e.g. `"a"`, `"escape"`, `"f1"`).
  */
  key: string;
  /**
  * Ctrl held.
  */
  ctrl: boolean;
  /**
  * Alt held.
  */
  alt: boolean;
  /**
  * Shift held (only meaningful for non-character keys; for
  * printable characters the case is already encoded in `key`).
  */
  shift: boolean;
  /**
  * Super / Cmd / Meta held.
  */
  meta: boolean;
};
```

### `LabelAlign`

Which way a form control's label sits in its `label_width` column.

A panel-wide property, set at mount (`MountFloatingWidget.label_align`)
and read by every `Text` / `Dropdown` / `Toggle` / `Number` / `Radio`
that pads its label to a column — alignment only means something relative to
the siblings sharing that column, so it is not a per-control field.
`Left` is what every panel rendered before the option existed.

```typescript
type LabelAlign = "left" | "right";
```

### `LanguagePackConfig`

Language configuration for language packs

This is a simplified version of the full LanguageConfig, containing only
the fields that can be set via the plugin API.

```typescript
type LanguagePackConfig = {
  /**
  * Comment prefix for line comments (e.g., "//" or "#")
  */
  commentPrefix: string | null;
  /**
  * Block comment start marker (e.g., slash-star)
  */
  blockCommentStart: string | null;
  /**
  * Block comment end marker (e.g., star-slash)
  */
  blockCommentEnd: string | null;
  /**
  * Whether to use tabs instead of spaces for indentation
  */
  useTabs: boolean | null;
  /**
  * Tab size (number of spaces per tab level)
  */
  tabSize: number | null;
  /**
  * Whether auto-indent is enabled
  */
  autoIndent: boolean | null;
  /**
  * Whether to show whitespace tab indicators (→) for this language
  * Defaults to true. Set to false for languages like Go/Hare that use tabs for indentation.
  */
  showWhitespaceTabs: boolean | null;
  /**
  * Formatter configuration
  */
  formatter: FormatterPackConfig | null;
};
```

### `LayoutHints`

Layout hints supplied by plugins (e.g., Compose mode)

```typescript
type LayoutHints = {
  /**
  * Optional compose width for centering/wrapping
  */
  composeWidth?: number;
  /**
  * Optional column guides for aligned tables
  */
  columnGuides?: Array<number>;
};
```

### `LineDiffHunk`

One hunk from `computeLineDiff`: a maximal run of differing lines.
Line indices are 0-based; a line is a `\n`-terminated (or final
unterminated) segment of the input text. `old_count == 0` is a pure
insertion, `new_count == 0` a pure deletion, both non-zero a
replacement. Equal regions between hunks are not reported.

```typescript
type LineDiffHunk = {
  /**
  * First affected line in the old text (0-based).
  */
  oldStart: number;
  /**
  * Number of old-side lines in the hunk (0 for pure insertion).
  */
  oldCount: number;
  /**
  * First affected line in the new text (0-based).
  */
  newStart: number;
  /**
  * Number of new-side lines in the hunk (0 for pure deletion).
  */
  newCount: number;
};
```

### `LineTarget`

One clickable line in a buffer: press Enter on it, or click it, and the
editor opens what it points at.

```typescript
type LineTarget = {
  /**
  * Row in the source buffer, 0-indexed, that carries this target.
  */
  line: number;
  /**
  * File to open. Relative paths resolve against the window's root.
  */
  path: string;
  /**
  * Line to land on in that file, 0-indexed. Defaults to the top.
  */
  target?: number;
  /**
  * Label of the pane to open into (`setSplitLabel`). When the label
  * names no live pane — or is omitted — the editor opens beside the
  * buffer holding the targets rather than replacing it, so an index
  * never eats its own pane.
  */
  into?: string;
};
```

### `LocalPath`

```typescript
type LocalPath = {
  kind: "local";
  value: string;
};
```

### `LspServerPackConfig`

LSP server configuration for language packs

```typescript
type LspServerPackConfig = {
  /**
  * Command to start the LSP server
  */
  command: string;
  /**
  * Arguments to pass to the command
  */
  args: Array<string>;
  /**
  * Whether to auto-start the server when a matching file is opened
  */
  autoStart: boolean | null;
  /**
  * LSP initialization options
  */
  initializationOptions: Record<string, unknown> | null;
  /**
  * Process resource limits (memory and CPU)
  */
  processLimits: ProcessLimitsPackConfig | null;
};
```

### `MouseClickHookArgs`

Payload delivered to handlers registered with `editor.on("mouse_click", ...)`.

All coordinate fields are in cell (terminal character) units. `buffer_*`
fields are `null` when the click did not land in any buffer panel.

```typescript
interface MouseClickHookArgs {
  /** Screen column (0-indexed). */
  column: number;
  /** Screen row (0-indexed). */
  row: number;
  /** Mouse button: "left", "right", "middle". */
  button: string;
  /** Modifier keys (e.g. "shift"). */
  modifiers: string;
  /** X offset of the content area the click landed in. */
  content_x: number;
  /** Y offset of the content area the click landed in. */
  content_y: number;
  /** Buffer under the click, or `null` when outside any buffer panel. */
  buffer_id: number | null;
  /** 0-indexed buffer row (line number) of the click, accounting for scroll. */
  buffer_row: number | null;
  /** 0-indexed byte column inside the buffer row. */
  buffer_col: number | null;
}
```

### `OffsetUnit`

Unit for `InlineOverlay` `start` / `end` offsets.

Plugins emitting overlays for text whose byte/codepoint counts
match (pure ASCII) can stay on the `Byte` default and avoid
per-overlay UTF-8 arithmetic. Plugins working with text that
may contain multi-byte characters can emit offsets in `Char`
units and let the host convert them to byte offsets at
consumption time — which is free in Rust against the entry's
final text.

```typescript
type OffsetUnit = "byte" | "char";
```

### `OverlayColorSpec`

Color specification that can be either RGB values or a theme key.

Theme keys reference colors from the current theme, e.g.:
- "ui.status_bar_bg" - UI status bar background
- "editor.selection_bg" - Editor selection background
- "syntax.keyword" - Syntax highlighting for keywords
- "diagnostic.error" - Error diagnostic color

When a theme key is used, the color is resolved at render time,
so overlays automatically update when the theme changes.

```typescript
type OverlayColorSpec = [number, number, number] | string;
```

### `OverlayOptions`

Options for adding an overlay with theme support.

This struct provides a type-safe way to specify overlay styling
with optional theme key references for colors.

```typescript
type OverlayOptions = {
  /**
  * Foreground color - RGB array or theme key string
  */
  fg?: OverlayColorSpec | null;
  /**
  * Background color - RGB array or theme key string
  */
  bg?: OverlayColorSpec | null;
  /**
  * Whether to render with underline
  */
  underline: boolean;
  /**
  * Whether to render in bold
  */
  bold: boolean;
  /**
  * Whether to render in italic
  */
  italic: boolean;
  /**
  * Whether to render with strikethrough
  */
  strikethrough: boolean;
  /**
  * Whether to extend background color to end of line
  */
  extendToLineEnd: boolean;
  /**
  * Whether to render with reverse video (fg/bg swapped). Used for
  * block-style text carets in form controls, where a hardware
  * cursor isn't available (modal overlays).
  */
  reversed: boolean;
  /**
  * When `true`, `fg` is applied only on cells whose existing fg
  * matches this overlay's resolved bg — i.e. a same-colour fg/bg
  * collision. Lets a row-wide overlay stay legible on tokens that
  * share the bg's colour without repainting unrelated tokens.
  */
  fgOnCollisionOnly: boolean;
  /**
  * Optional URL for OSC 8 terminal hyperlinks.
  * When set, the overlay text becomes a clickable hyperlink in terminals
  * that support OSC 8 escape sequences.
  */
  url?: string | null;
};
```

### `PaneDescription`

One pane, as `describeWorkspace()` reports it: what is in it, where it
is, and the ids needed to act on it.

```typescript
type PaneDescription = {
  /**
  * Pass to `openFileInSplit`, `focusSplit`, `setSplitRatio`, ...
  */
  splitId: number;
  /**
  * Buffer shown in this pane.
  */
  bufferId: BufferId;
  /**
  * `"terminal"` (a PTY), `"file"` (backed by a path), or `"virtual"`
  * (a plugin-owned scratch buffer).
  */
  kind: string;
  /**
  * Label set by `setSplitLabel`, when this pane has one — how a script
  * finds a pane it named in an earlier run.
  */
  label: string | null;
  /**
  * Absolute path when this pane shows a file, else `null`.
  */
  path: string | null;
  /**
  * Short label — the file name, or the buffer's name for the rest.
  */
  name: string;
  /**
  * Whether this pane has focus.
  */
  active: boolean;
  /**
  * Unsaved changes.
  */
  modified: boolean;
  /**
  * On-screen geometry, in editor-area cells. Panes are listed left to
  * right, top to bottom, so `panes[0]` is the leftmost/topmost; `x`
  * and `y` say so precisely.
  */
  x: number;
  y: number;
  width: number;
  height: number;
};
```

### `PathTranslationSpec`

```typescript
type PathTranslationSpec = {
  host_root: string;
  remote_root: string;
};
```

### `PluginAnimationEdge`

Edge a slide-in effect enters from.

```typescript
type PluginAnimationEdge = "top" | "bottom" | "left" | "right";
```

### `PluginAnimationKind`

Plugin-facing animation description. Tagged by `kind`. Additional
variants can be added later; plugins must handle the `kind` they send.

```typescript
type PluginAnimationKind = {
  "kind": "slideIn";
  from: PluginAnimationEdge;
  durationMs: number;
  delayMs: number;
};
```

### `PreparingWindowResult`

Result of `createPreparingWindow` — the ids of the placeholder window,
which are already final: adopting it later keeps both.

```typescript
type PreparingWindowResult = {
  /**
  * The new window's id — a per-process handle, valid until this editor
  * exits.
  */
  windowId: number;
  /**
  * The new workspace's durable identity (`ws-…`), stable across restarts.
  */
  stableId: string;
  /**
  * The placeholder's seed buffer. Mount a widget panel here
  * (`mountWidgetPanel`) to describe the page yourself: the plugin
  * building the workspace knows what it is waiting on, what failed and
  * what the user can do about it, so the page is its to write. The
  * editor's own page — name, state, one line of explanation — is only
  * the fallback for a window nothing has described.
  */
  bufferId: number;
};
```

### `ProcessHandle`

Handle for a cancellable async operation

```typescript
interface ProcessHandle<T> extends PromiseLike<T> {
  /** Promise that resolves to the result when complete */
  readonly result: Promise<T>;
  /** Id of the spawned process (the `process_id` in onProcessStdout/onProcessStderr payloads) */
  readonly processId: number;
  /** Cancel/kill the operation. Returns true if cancelled, false if already completed */
  kill(): Promise<boolean>;
}
```

### `ProcessLimitsPackConfig`

Process resource limits for LSP servers

```typescript
type ProcessLimitsPackConfig = {
  /**
  * Maximum memory usage as percentage of total system memory (null = no limit)
  */
  maxMemoryPercent: number | null;
  /**
  * Maximum CPU usage as percentage of total CPU (null = no limit)
  */
  maxCpuPercent: number | null;
  /**
  * Enable resource limiting
  */
  enabled: boolean | null;
};
```

### `PromptSuggestion`

A single suggestion item for autocomplete

```typescript
type PromptSuggestion = {
  /**
  * What this row is, unique within the list: the plugin's own name for
  * the item (a path, a match's `file:line:col`, a record id).
  *
  * **Required.** The list keys its rows by it, so an insertion or a
  * re-rank moves the other rows instead of rewriting them, and the
  * selection stays on the row it was on. Not the label: two rows may
  * read the same and still be different things. `setPromptSuggestions`
  * throws when two suggestions share an id.
  */
  id: string;
  /**
  * The text to display
  */
  text: string;
  /**
  * Optional description, shown in the row alongside `text`
  */
  description?: string;
  /**
  * The value to use when selected (defaults to `text` when omitted)
  */
  value?: string;
  /**
  * Whether this suggestion is disabled (greyed out, defaults to false)
  */
  disabled?: boolean;
  /**
  * Optional styled rendering of `description`. When present, the
  * suggestion list renders these spans (in order) in place of the
  * plain `description` text — letting a plugin highlight a portion
  * of the row, e.g. the symbol word inside a code-line snippet.
  */
  description_spans?: Array<StyledText>;
  /**
  * Optional keyboard shortcut, shown in the row as a hint
  */
  keybinding?: string;
};
```

### `RemoteAgentSpec`

```typescript
type RemoteAgentSpec = {
  transport: RemoteAgentTransport;
  /**
  * Captured in-pod env (PATH/HOME/LANG/…) applied to LSP spawns and
  * binary-presence probes. Omit when no probe was run.
  */
  base_env?: [string, string][];
  /**
  * When true, attach as a NEW window (born-attached, coexisting with the
  * existing windows) rather than re-pointing the window showing the current
  * project. The Orchestrator sets this so a cloud session is a real session
  * row beside local ones.
  */
  window?: boolean;
  /** Window label (window mode only). Omit to use the transport's display. */
  label?: string;
  /** Optional agent argv for the new window's seed terminal (window mode). */
  command?: string[];
  /**
  * Grow this *preparing* window (from `createPreparingWindow`) into the
  * session instead of minting a new one — window mode only. The
  * Orchestrator opens a placeholder the user lands in while the connect
  * runs, so a remote workspace is somewhere to be from the moment it is
  * asked for, and a connect that fails reports on that page rather than
  * only in the dock. Ignored if the window is gone by the time the connect
  * lands.
  */
  adopt_window?: number;
};
```

### `RemoteAgentTransport`

```typescript
type RemoteAgentTransport = {
  kind: "kubectl-exec";
  /** kubeconfig context to select (`--context`); omit for the current one. */
  context?: string | null;
  namespace: string;
  pod: string;
  /** Target container in a multi-container pod (`-c`). */
  container?: string | null;
  /** Pod-side workspace root the terminal opens in. */
  workspace?: string | null;
} | {
  kind: "ssh";
  /** Login user. Optional — omit for `host` / `ssh://host`, letting ssh pick
  * the user from its own config or the current local user. */
  user?: string | null;
  host: string;
  port?: number | null;
  identity_file?: string | null;
  /** Remote directory to root the session at. */
  remote_path?: string | null;
  /** Extra `ssh` arguments (e.g. `-J jump`, `-o ProxyCommand=…`) applied to
  * every ssh invocation for this session. */
  extra_args?: string[];
};
```

### `RemoteBackendInfo`

Backend identity of a non-local session, as surfaced to plugins on
`WindowInfo.remote`, reduced to what the dock renders.

```typescript
type RemoteBackendInfo = {
  /**
  * Backend kind: `"ssh"` or `"kubernetes"`.
  */
  kind: string;
  /**
  * Short human identity for the row (e.g. `deploy@build-01`,
  * `ns/pod`).
  */
  detail: string;
  /**
  * `true` when the backend connection is currently live; `false` for a
  * dormant session (restored from disk, not yet connected, or whose
  * last connect failed).
  */
  connected: boolean;
};
```

### `RemoteIndicatorStatePayload`

```typescript
type RemoteIndicatorStatePayload = {
  kind: "local";
} | {
  kind: "connecting";
  label?: string | null;
} | {
  kind: "connected";
  label?: string | null;
} | {
  kind: "failed_attach";
  error?: string | null;
} | {
  kind: "disconnected";
  label?: string | null;
};
```

### `ReplaceResult`

Result from replacing matches in a buffer

```typescript
type ReplaceResult = {
  /**
  * Number of replacements made
  */
  replacements: number;
  /**
  * Buffer ID of the edited buffer
  */
  bufferId: number;
};
```

### `ScreenSize`

Total terminal size in cells. Returned by `editor.getScreenSize()`.

```typescript
type ScreenSize = {
  width: number;
  height: number;
};
```

### `ScrollAlign`

Where a widget should land when a plugin scrolls to it.

```typescript
type ScrollAlign = "top" | "minimal";
```

### `ScrollbarMarker`

One marker painted on a split's vertical scrollbar track, at a position
proportional to its location in the buffer (an "overview ruler" mark).

Position is a **byte offset** (`position`), which is the only coordinate
that is exact in every file-size regime — on a large file opened before the
incremental line scan completes, line numbers do not exist yet. `line` is a
convenience that the editor converts to a byte anchor when the marker is
set; it is dropped if the line cannot be resolved. Supply exactly one.

`end` turns a point marker into a range marker, so a multi-line region
(a diff hunk, a folded block) paints a proportional streak rather than a
single cell.

```typescript
type ScrollbarMarker = {
  /**
  * Byte offset of the marked location. Preferred over `line`.
  */
  position?: number;
  /**
  * 0-based logical line number, converted to a byte anchor at set time.
  * Ignored when `position` is present.
  */
  line?: number;
  /**
  * Optional exclusive end byte offset, making this a range marker.
  */
  end?: number;
  /**
  * Optional 0-based end line, **inclusive**, making this a range marker.
  * Ignored when `end` is present.
  *
  * The line counterpart to `end`, for producers that work in line
  * coordinates — a `git diff` parser knows a hunk's first and last line
  * but not their byte offsets. Without it such a plugin has to emit one
  * marker per line to paint a hunk's streak, which costs a byte lookup
  * and two anchors per line for a resolution the track cannot show.
  */
  endLine?: number;
  /**
  * Marker color — RGB array or theme key. Theme keys resolve at render
  * time, so markers follow theme changes.
  */
  color: OverlayColorSpec;
  /**
  * Priority when several markers land on the same track cell (higher
  * wins). Defaults to 0.
  */
  priority?: number;
};
```

### `SearchHandle`

```typescript
interface SearchHandle {
  searchId: number;
  take(): SearchTakeResult;
  cancel(): void;
}
```

### `SearchTakeResult`

Per-call result from `SearchHandle.take()` — the matches accumulated since
the previous call plus terminal-state flags.

```typescript
type SearchTakeResult = {
  /**
  * Matches discovered since the previous take()
  */
  matches: Array<GrepMatch>;
  /**
  * Whether the producer has finished (no more matches will arrive)
  */
  done: boolean;
  /**
  * Total number of matches the producer has emitted across all batches
  * (including ones already drained on prior take() calls)
  */
  totalSeen: number;
  /**
  * Whether the producer stopped early because it hit `maxResults`
  */
  truncated: boolean;
  /**
  * Producer error, if any (e.g., invalid regex). When set, `done` is also true.
  */
  error?: string | null;
};
```

### `SessionWithTerminalResult`

Result of `createWindowWithTerminal` — the ids of the new
window plus the terminal seeded into its split layout.

```typescript
type SessionWithTerminalResult = {
  /**
  * The new window's id — a per-process handle, valid until this editor
  * exits. Use `stableId` for anything that has to outlive the process.
  */
  windowId: number;
  /**
  * The new workspace's durable identity (`ws-…`), stable across restarts.
  */
  stableId: string;
  /**
  * The seeded terminal's id (for `sendTerminalInput`, etc.).
  */
  terminalId: number;
  /**
  * The seeded terminal buffer's id.
  */
  bufferId: number;
};
```

### `SpawnResult`

Result from spawning a process with spawnProcess

```typescript
type SpawnResult = {
  /**
  * Complete stdout as string, exactly as the process wrote it: newlines,
  * including the trailing one, are kept
  */
  stdout: string;
  /**
  * Complete stderr as string. When the process could not be started,
  * it holds the error message instead (and `exit_code` is -1)
  */
  stderr: string;
  /**
  * Process exit code (0 usually means success, -1 if killed)
  */
  exit_code: number;
};
```

### `SplitAxis`

Which way the divider runs when splitting a pane.

Named for the *divider*, not the stacking, which is the convention
vim and tmux use and the opposite of what "horizontal layout" suggests
in some editors — so the two cases are spelled out on each variant.

```typescript
type SplitAxis = "vertical" | "horizontal";
```

### `SplitCreated`

What `editor.splitWindow()` resolves to: the new pane, already
laid out, so a caller can confirm where it landed without a
follow-up `listSplits()`.

```typescript
type SplitCreated = {
  /**
  * The new pane's id — pass to `openFileInSplit`, `focusSplit`, ...
  */
  splitId: number;
  /**
  * The pane the split was created from, still live and now smaller.
  */
  sourceSplitId: number;
  /**
  * Buffer shown in the new pane.
  */
  bufferId: BufferId;
  /**
  * Geometry of the new pane (see `SplitSnapshot`).
  */
  x: number;
  y: number;
  width: number;
  height: number;
};
```

### `SplitId`

Split identifier

```typescript
type SplitId = number;
```

### `SplitPlacement`

Where a new pane goes relative to the one being split.

With `direction: "vertical"` (a vertical divider, panes side by side),
`Before` puts the new pane on the **left** and `After` on the right.
With `direction: "horizontal"`, `Before` is **above** and `After` below.

```typescript
type SplitPlacement = "before" | "after";
```

### `SplitSnapshot`

Per-split state surfaced to plugins via `editor.listSplits()`.

Plugins that need to operate on every visible buffer (multi-split
flash labels, syncing decorations across panes, ...) can iterate
this list rather than only seeing the active split's `getViewport()`.

```typescript
type SplitSnapshot = {
  /**
  * Stable split identifier; matches the values used by
  * `setSplitBuffer`, `focusSplit`, `getSplitByLabel`, etc.
  */
  splitId: number;
  /**
  * Buffer currently shown in this split.
  */
  bufferId: BufferId;
  /**
  * Label set by `setSplitLabel`, when this pane has one. Reported here so
  * a later script can re-find a pane it named earlier — the ids change
  * across restarts, the label is what the caller chose.
  */
  label: string | null;
  /**
  * Column of this pane's left edge, in terminal cells, measured from
  * the left edge of the editor area. This is what answers "which pane
  * is on the left" — compare `x` between panes rather than guessing
  * from list order.
  */
  x: number;
  /**
  * Row of this pane's top edge, in terminal cells, measured from the
  * top of the editor area. Compare `y` to tell top from bottom.
  */
  y: number;
  /**
  * Pane width in cells, separator excluded.
  */
  width: number;
  /**
  * Pane height in cells, separator excluded.
  */
  height: number;
  /**
  * Viewport (top byte / dimensions) for this split's active buffer.
  * This is the *text* viewport: it excludes the tab bar and any
  * gutter, so it is smaller than the pane rect above.
  */
  viewport: ViewportInfo;
};
```

### `SplitWindowOptions`

Options for `editor.splitWindow()`.

```typescript
type SplitWindowOptions = {
  /**
  * Divider orientation. Default `"vertical"` — panes side by side.
  */
  direction?: SplitAxis;
  /**
  * Which side the new pane lands on. Default `"after"`.
  */
  place?: SplitPlacement;
  /**
  * First child's share of the space, 0.0–1.0. Default 0.5.
  * "First" is the left/top pane regardless of `place`.
  */
  ratio?: number;
  /**
  * Open this file in the new pane. Relative paths resolve against the
  * window's root. When omitted the new pane shows the same buffer as
  * the pane it was split from, which is what the keyboard split does.
  */
  file?: string;
  /**
  * Leave focus where it was instead of moving it into the new pane.
  * Default false (the new pane takes focus, matching the keyboard
  * split).
  */
  keepFocus?: boolean;
};
```

### `StyledSegment`

One styled segment of a `TextPropertyEntry` built via the
`segments` field. Plugins use segments to describe row content
structurally — a sequence of (text, optional style, optional
nested overlays) — instead of pre-rendering the text and
computing byte/char offsets for overlays themselves. The host
concatenates segment text and emits the corresponding overlays
during `normalize_widths`.

```typescript
type StyledSegment = {
  /**
  * Verbatim text for this segment.
  */
  text: string;
  /**
  * When set, the host emits an `InlineOverlay` covering this
  * segment's text in the final entry.
  */
  style?: Partial<OverlayOptions>;
  /**
  * Additional overlays inside this segment. Offsets are in
  * the overlay's own `unit`, relative to the segment's start
  * (NOT the final entry text); the host shifts them by the
  * segment's position during concatenation.
  */
  overlays?: Array<InlineOverlay>;
};
```

### `StyledText`

A run of text with optional styling. `style` reuses
`OverlayOptions` — the same primitive plugins use for virtual
text — so a hint is just `{ text: "Alt+P cycle", style: { fg:
"ui.help_key_fg" } }`. Omitting `style` means "no styling override";
each consumer applies its own default (e.g. the floating-prompt
title uses `prompt_fg` + bold).

```typescript
type StyledText = {
  text: string;
  style?: Partial<OverlayOptions>;
};
```

### `TerminalResult`

Result of creating a terminal, returned by `createTerminal`

```typescript
type TerminalResult = {
  /**
  * The created buffer ID (for use with setSplitBuffer, etc.)
  */
  bufferId: number;
  /**
  * The terminal ID (for use with sendTerminalInput, closeTerminal)
  */
  terminalId: number;
  /**
  * The split ID (if created in a new split)
  */
  splitId: number | null;
};
```

### `TextPropertiesAtCursor`

Result of getTextPropertiesAtCursor - array of property objects

Each element contains the properties from a text property span that overlaps
with the cursor position. Properties are dynamic key-value pairs set by plugins.

```typescript
type TextPropertiesAtCursor = Array<Record<string, unknown>>;
```

### `TextPropertyEntry`

Entry for virtual buffer content with optional text properties

```typescript
type TextPropertyEntry = {
  /**
  * Text content for this entry. Entries are concatenated verbatim, so
  * end the text with a newline to put the entry on a line of its own.
  */
  text: string;
  /**
  * Optional properties attached to this text (e.g., file path, line
  * number): arbitrary metadata, read back with `getTextPropertiesAtCursor`
  */
  properties?: Record<string, unknown>;
  /**
  * Optional whole-entry styling
  */
  style?: Partial<OverlayOptions>;
  /**
  * Optional sub-range styling within this entry
  */
  inlineOverlays?: Array<InlineOverlay>;
  /**
  * Pad this entry's text with spaces to this many columns (Unicode
  * codepoints) when drawing. No-op when the text already has at least
  * this many codepoints. Applied before overlays are resolved.
  *
  * **Render-only**: the padding is applied at draw time, so
  * `getBufferText()` returns the unpadded text you supplied. Column
  * alignment cannot be checked by reading the buffer back — if you need
  * that, embed real spaces with `padEnd` instead.
  */
  padToChars?: number;
  /**
  * Truncate this entry's text to at most this many columns (Unicode
  * codepoints) when drawing. When the budget is greater than 3 the
  * truncated tail is replaced with `...`; when it is 3 or less the text
  * is cut at exactly the budget. Applied before `padToChars` and before
  * overlays are resolved.
  *
  * **Render-only**, like `padToChars`: `getBufferText()` returns the full
  * untruncated text.
  */
  truncateToChars?: number;
  /**
  * Build the entry from styled pieces. When non-empty, the host joins
  * the segments' text into `text` (replacing any `text` you supplied)
  * and adds an inline overlay over each styled segment, plus each
  * segment's own `overlays` shifted to its position. Resolved before
  * `truncateToChars` and `padToChars`.
  */
  segments?: Array<StyledSegment>;
};
```

### `TextWindowAnchor`

How a row asks to be windowed when it is wider than the panel.
See `TreeNode.windowAnchor`.

```typescript
type TextWindowAnchor = {
  /**
  * Chars at the head of the row that never move.
  *
  * A row's leading pieces are usually its *identity* rather than its
  * content — a search result's `path:line` — and a window that slid them
  * away left rows that could not be told apart. They stay put and the
  * rest of the row slides under them, the same relationship the indent
  * and checkbox glyphs already have with the body.
  */
  pinned: number;
  /**
  * Char index, in the whole row, where the span the row exists to show
  * starts. Must be at or after `pinned`; a span inside the pinned head is
  * always visible anyway.
  */
  start: number;
  /**
  * Length of the span, in chars. Zero is allowed and means a point.
  */
  len: number;
};
```

### `TokenColor`

Color carried by a `ViewTokenStyle`. Untagged so JSON plugins can
keep passing `[r, g, b]` arrays, while richer themes can use named
ANSI colors (`"Red"`, `"LightGreen"`, `"Default"`) or theme keys
(`"editor.diff_remove_bg"`). The renderer resolves named/theme
strings against the active theme at draw time; unknown strings
fall through to the terminal's default color.

A 256-color palette value is written as `"Indexed:N"` (N from 0 to
255).

```typescript
type TokenColor = [number, number, number] | string;
```

### `TreeNode`

```typescript
type TreeNode = {
  /**
  * The pre-rendered row content (text + per-row overlays).
  * The host renders this verbatim after the indent + disclosure
  * prefix; plugin overlays are byte-shifted by the prefix
  * length.
  */
  text: TextPropertyEntry;
  /**
  * 0-based depth — controls leading indent (`depth * 2` spaces).
  */
  depth: number;
  /**
  * When true, render a disclosure glyph (`▶` collapsed / `▼`
  * expanded) and emit a hit area over it that fires the `expand`
  * event. Leaf nodes (`false`) get no glyph and no expand hit;
  * the row width occupies the full row.
  */
  hasChildren: boolean;
  /**
  * A leaf drawn with no disclosure gutter: its text starts at its
  * indent, not two columns in. For rows that stand at a tree's top level
  * beside folders and should read as flush with the panel's edge (the
  * orchestrator dock's unfiled workspaces). Ignored on a node with
  * children, whose ▶/▼ is the gutter.
  */
  flush?: boolean;
  /**
  * The row can be picked up with the pointer and dropped on another row
  * of the same tree. The plugin hears, as `widget_event`s on the tree:
  * `drag` (`{ key, target }`: the row lifted and the row under the
  * pointer, `null` off every row) each time that changes, from the first
  * row the drag leaves its own for; `drop` (`{ key, target, index }`)
  * when it is released on another row; and `dragend` (`{ key }`) when
  * the drag is over, dropped or not. A plain click hears none of them. A
  * press on the row activates on release, not on the press, so a drag
  * does not first act on the row it lifts.
  */
  draggable?: boolean;
  /**
  * Per-node checkbox state. Only rendered when the parent
  * `Tree` has `checkable: true`. Omitted = no checkbox glyph;
  * `true` = `[v]`; `false` = `[ ]`. The plugin
  * owns the truth — the host fires `widget_event { event_type:
  * "toggle" }` and the plugin pushes the new state back via
  * `editor.widgetMutate(panel, { kind: "setCheckedKeys", ... })`.
  */
  checked?: boolean | null;
  /**
  * Continuation lines rendered below the node's primary `text`
  * line when the parent `Tree` has `item_height > 1`. Each entry
  * is one screen row, indented to align under the primary line's
  * body (past the indent + disclosure/checkbox prefix). The host
  * renders at most `item_height - 1` of them and blank-pads a
  * shorter node so every row in the tree is the same fixed height.
  * Ignored when `item_height == 1`.
  */
  extraLines?: Array<TextPropertyEntry>;
  /**
  * The span of `text` this row exists to show, in **chars**.
  *
  * A row wider than the panel is windowed by the host, and without this
  * the window can only start at the head of the line — which is exactly
  * where a search result's match usually is not (issue #1580). Naming the
  * span lets the host rest the window on it instead, and the reader pans
  * away from there.
  *
  * Chars rather than columns because that is the unit a plugin can count:
  * it has the string, not the terminal's width table. The host converts.
  * Out-of-range values are harmless — they resolve to the end of the text.
  */
  windowAnchor?: TextWindowAnchor | null;
  /**
  * A button drawn at the row's tail: what this row is *for*, said on
  * the row itself rather than only in a footer the eye has to travel
  * to. When set to `label`, it renders `[ label ]` against the panel's right
  * edge and emits a hit area over it that fires the `action` event
  * with the row's `index` and `key`; the keyboard reaches the same
  * thing through the tree's `activate`.
  *
  * The button is pinned like the indent is pinned: it sits outside the
  * window the body is fitted into, so a row too wide for the panel
  * slides *under* its button rather than pushing it off the edge.
  * Ignored on a bordered card (`card_borders` with `item_height > 1`),
  * whose chrome has nowhere to put one.
  */
  action?: string | null;
  /**
  * **A table row.** When the parent `Tree` declares `columns`, a node
  * that carries cells is drawn from them — one per column, each fitted
  * to its column at the width layout gives the tree and elided at the
  * end its column says — instead of from `text`. A node with no cells
  * (a group heading) is drawn from `text` across the whole row.
  */
  cells?: Array<TableCell>;
};
```

### `TsActionPopupAction`

Action button for action popups

```typescript
type TsActionPopupAction = {
  /**
  * Unique action identifier (returned in ActionPopupResult)
  */
  id: string;
  /**
  * Display text for the button (can include command hints)
  */
  label: string;
};
```

### `TsCompositeHunk`

Diff hunk for composite buffer alignment

```typescript
type TsCompositeHunk = {
  /**
  * Starting line in old buffer (0-indexed)
  */
  oldStart: number;
  /**
  * Number of lines in old buffer
  */
  oldCount: number;
  /**
  * Starting line in new buffer (0-indexed)
  */
  newStart: number;
  /**
  * Number of lines in new buffer
  */
  newCount: number;
  /**
  * Per-line operations for the hunk, in git order: one char per line —
  * `' '` context, `'-'` deletion (old only), `'+'` addition (new only).
  * When present, the side-by-side alignment follows git's classification
  * exactly (unchanged lines stay paired); when absent, the host falls back
  * to a positional pairing. Optional for backward compatibility.
  */
  ops?: string;
};
```

### `TsCompositeLayoutConfig`

Layout configuration for composite buffers

```typescript
type TsCompositeLayoutConfig = {
  /**
  * Layout type: "side-by-side", "stacked", or "unified"
  */
  type: string;
  /**
  * Width ratios for side-by-side (e.g., [0.5, 0.5])
  */
  ratios?: Array<number>;
  /**
  * Show separator between panes
  */
  showSeparator: boolean;
  /**
  * Spacing for stacked layout
  */
  spacing?: number;
};
```

### `TsCompositePaneStyle`

Style configuration for a composite pane

```typescript
type TsCompositePaneStyle = {
  /**
  * Background color for added lines (RGB)
  */
  addBg?: [number, number, number];
  /**
  * Background color for removed lines (RGB)
  */
  removeBg?: [number, number, number];
  /**
  * Background color for modified lines (RGB)
  */
  modifyBg?: [number, number, number];
  /**
  * Gutter style: "line-numbers", "diff-markers", "both", or "none"
  */
  gutterStyle?: string;
};
```

### `TsCompositeSourceConfig`

Source pane configuration for composite buffers

```typescript
type TsCompositeSourceConfig = {
  /**
  * ID of the source buffer this pane displays (required)
  */
  bufferId: number;
  /**
  * Label for this pane (e.g., "OLD", "NEW"), shown in the pane's header
  */
  label: string;
  /**
  * Whether this pane is editable
  */
  editable: boolean;
  /**
  * Style configuration
  */
  style: TsCompositePaneStyle | null;
};
```

### `TsCreateCompositeBufferOptions`

Options for creating a composite buffer (used by plugin API)

```typescript
type TsCreateCompositeBufferOptions = {
  /**
  * Buffer name (displayed in tabs/title)
  */
  name: string;
  /**
  * Mode for keybindings
  */
  mode: string;
  /**
  * Layout configuration
  */
  layout: TsCompositeLayoutConfig;
  /**
  * Source pane configurations
  */
  sources: Array<TsCompositeSourceConfig>;
  /**
  * Diff hunks for alignment (optional)
  */
  hunks: Array<TsCompositeHunk> | null;
  /**
  * When set, the first render will scroll to center the Nth hunk (0-indexed).
  * This avoids timing issues with imperative scroll commands that depend on
  * render-created state (viewport dimensions, view state).
  */
  initialFocusHunk?: number;
};
```

### `TsHighlightSpan`

Syntax highlight span for a buffer range

```typescript
type TsHighlightSpan = {
  start: number;
  end: number;
  color: [number, number, number];
  bold: boolean;
  italic: boolean;
};
```

### `TsLspMenuItem`

Plugin-contributed row in the LSP-Servers popup, passed to
`editor.setLspMenuContributions`.

```typescript
type TsLspMenuItem = {
  /**
  * Stable identifier used as the `action_id` in the resulting
  * `action_popup_result` event (prefixed by `{plugin_id}|`).
  */
  id: string;
  /**
  * Display label shown in the popup row.
  */
  label: string;
};
```

### `TsSyntaxRegion`

A run of rows in a plugin-composed buffer that carry code, for the
host's highlighter (`setSyntaxRegions`).

A composed buffer — a diff stream, a log — is not a document any
grammar can parse, so the plugin says where the code is instead. A
region is a byte range of whole rows; every row in it is handed to the
language's parser with its first `prefix` bytes skipped (a gutter, a
diff marker), and rows outside every region keep whatever the plugin
styled them with and never advance a parser.

`streams` name the parsers a row feeds, and regions that share a
stream id continue one parse across the rows between them: the old
and new side of a hunk interleave, and a comment box can sit inside a
hunk, yet each side is still read as the contiguous text it is. A row
both sides share (context) lists both. An empty list means one parser
shared by every region that says nothing.

The contract:
- Only a plugin-composed (virtual) buffer can be told this; a file has
  a grammar of its own, and the call is ignored, with a log line, for
  anything else.
- A region starts at a row's first byte and ends just past a row's
  newline. A row is coloured when its first byte lies in a region.
- Regions replace the buffer's previous set and must not overlap: a
  region that overlaps the one before it (in byte order) is dropped,
  with a log line. Setting the buffer's content clears them all.
- The first stream a region names colours its rows; the others are fed
  for their state. The host keeps the four most recently fed parsers;
  a stream fed again after four others starts afresh.
- A language nothing in the grammar set claims leaves the rows as the
  plugin styled them, with a log line.
- Regions follow the text: an edit inside the buffer moves them the
  way it moves overlays.

```typescript
type TsSyntaxRegion = {
  /**
  * Byte offset of the first row's first byte.
  */
  start: number;
  /**
  * Byte offset one past the last row's newline.
  */
  end: number;
  /**
  * What the rows are written in: a path (`src/main.rs`, `Makefile`)
  * or a language token (`py`, `rust`). Nothing is opened or read;
  * it only selects the grammar.
  */
  language: string;
  /**
  * Bytes at the start of every row that are not code.
  */
  prefix: number;
  /**
  * Parsers the rows feed, in order; the first colours the rows. Empty
  * means the shared stream `0`. See the type docs.
  */
  streams: Array<number>;
};
```

### `ViewportInfo`

Information about the viewport

```typescript
type ViewportInfo = {
  /**
  * Byte position of the first visible line
  */
  topByte: number;
  /**
  * Line number of the first visible line (`null` when the line index is unavailable, e.g. a large file before its scan)
  */
  topLine: number | null;
  /**
  * Left column offset (horizontal scroll)
  */
  leftColumn: number;
  /**
  * Viewport width in columns
  */
  width: number;
  /**
  * Viewport height in rows
  */
  height: number;
};
```

### `ViewTokenStyle`

Styling for view tokens (used for injected annotations)

This allows plugins to specify styling for tokens that don't have a source
mapping (`source_offset: null`), such as annotation headers in git blame.
For tokens with a `source_offset`, syntax highlighting is applied instead.

```typescript
type ViewTokenStyle = {
  /**
  * Foreground color. Either `[r, g, b]` or a named/theme string —
  * see `TokenColor`.
  */
  fg: TokenColor | null;
  /**
  * Background color. Either `[r, g, b]` or a named/theme string —
  * see `TokenColor`.
  */
  bg: TokenColor | null;
  /**
  * Whether to render in bold
  */
  bold: boolean;
  /**
  * Whether to render in italic
  */
  italic: boolean;
  /**
  * Whether to render with underline
  */
  underline: boolean;
};
```

### `ViewTokenWire`

Wire-format view token with optional source mapping and styling

```typescript
type ViewTokenWire = {
  /**
  * Source byte offset in the buffer. `null` for injected content (annotations).
  */
  source_offset: number | null;
  /**
  * The token content
  */
  kind: ViewTokenWireKind;
  /**
  * Optional styling for injected content (only used when `source_offset` is `null`)
  */
  style?: ViewTokenStyle;
};
```

### `ViewTokenWireKind`

Wire-format view token kind (serialized for plugin transforms)

```typescript
type ViewTokenWireKind = {
  "Text": string;
} | "Newline" | "Space" | "Break" | {
  "BinaryByte": number;
};
```

### `VirtualBufferResult`

Result of creating a virtual buffer

```typescript
type VirtualBufferResult = {
  /**
  * The created buffer ID
  */
  bufferId: number;
  /**
  * The split ID (if created in a new split)
  */
  splitId: number | null;
};
```

### `WalkTreeEntry`

```typescript
interface WalkTreeEntry {
  path: string;
  /** Path relative to the walk root, "/"-separated on every platform. */
  rel: string;
  kind: "file" | "dir" | "symlink";
  /** Unix timestamp. */
  mtime: number;
  size: number;
}
```

### `WalkTreeOptions`

```typescript
interface WalkTreeOptions {
  /** Directory basenames skipped at every depth. */
  skipDirs?: string[];
  includeHidden?: boolean;
  includeDirs?: boolean;
  /** Depth below the root; 1 is a direct child. Omitted means unbounded. */
  maxDepth?: number;
  maxEntries?: number;
}
```

### `WalkTreeResult`

```typescript
interface WalkTreeResult {
  entries: WalkTreeEntry[];
  /** True when `maxEntries` stopped the walk early. */
  truncated: boolean;
}
```

### `WidgetAction`

Action a plugin can request the widget runtime to perform on a
mounted panel, sent with `editor.widgetCommand(panelId, action)`.

All actions target the panel's currently focused widget (the host
tracks focus per panel). They are fired by the plugin's mode
bindings — Tab → `FocusAdvance{+1}`, Enter → `Activate`,
Up/Down → `SelectMove{±1}`, Backspace → `TextInputKey{"Backspace"}`,
printable chars (via `mode_text_input`) → `TextInputChar{"x"}`.

```typescript
type WidgetAction = {
  "kind": "focusAdvance";
  delta: number;
} | {
  "kind": "activate";
} | {
  "kind": "selectMove";
  delta: number;
} | {
  "kind": "textInputKey";
  key: string;
} | {
  "kind": "textInputChar";
  text: string;
} | {
  "kind": "key";
  key: string;
};
```

### `WidgetMutation`

Targeted in-place mutation of a mounted widget panel, sent with
`editor.widgetMutate(panelId, mutation)` — a faster way to apply a
small change than re-sending the whole spec. Plugins use these when
the model change touches one widget; the host applies the mutation
directly to the panel's spec / instance state and re-renders.

`editor.updateWidgetPanel` remains the right tool for structural
changes (adding/removing widgets, restructuring layout). Both
paths preserve instance state via widget keys.

```typescript
type WidgetMutation = {
  "kind": "setValue";
  widgetKey: string;
  value: string;
  cursorByte?: number | null;
} | {
  "kind": "setCompletions";
  widgetKey: string;
  items: Array<string | CompletionItem>;
} | {
  "kind": "setChecked";
  widgetKey: string;
  checked: boolean;
} | {
  "kind": "setSelectedIndex";
  widgetKey: string;
  index: number;
} | {
  "kind": "setNumber";
  widgetKey: string;
  value: number;
} | {
  "kind": "setDropdown";
  widgetKey: string;
  index: number;
} | {
  "kind": "setDualIncluded";
  widgetKey: string;
  included: Array<string>;
} | {
  "kind": "setItems";
  widgetKey: string;
  items: Array<TextPropertyEntry>;
  itemKeys: Array<string>;
} | {
  "kind": "setExpandedKeys";
  widgetKey: string;
  keys: Array<string>;
} | {
  "kind": "setCheckedKeys";
  widgetKey: string;
  checked: boolean;
  keys: Array<string>;
} | {
  "kind": "appendTreeNodes";
  widgetKey: string;
  newNodes: Array<TreeNode>;
  newItemKeys: Array<string>;
} | {
  "kind": "setRawEntries";
  widgetKey: string;
  entries: Array<TextPropertyEntry>;
} | {
  "kind": "setFocusKey";
  widgetKey: string;
};
```

### `WidgetPanelOptions`

How the host should treat a mounted panel, beyond rendering its
spec.

Every field is optional, and each one you leave out reads as what
the host did before that field existed. Unknown fields are ignored,
so options written for a newer editor don't stop an older one from
applying the ones it knows.

```typescript
type WidgetPanelOptions = {
  /**
  * When the focus key names no tabbable widget, fall back to the
  * first one.
  *
  * True is the historical behaviour and stays the default. A panel
  * for which *nothing focused* is a real resting state must say so:
  * otherwise clearing focus does not clear it, because the next
  * repaint silently re-seeds it onto whatever happens to be first.
  * The plugin's own record of focus then disagrees with the host's,
  * and a key meant for no one is delivered to that widget — on the
  * welcome screen, leaving its file finder put focus on "Show this
  * screen on startup", so the next Space turned the page off with
  * nothing on screen to say why.
  *
  * Leaving it out is what every plugin written before this field said, and
  * reads as `true`.
  */
  autoFocusFirst?: boolean;
  /**
  * The panel is a *page*: its whole content scrolls together in a
  * window the host owns, the way a document does, rather than each
  * list windowing itself to the panel's height. Lists and text areas
  * inside a page take their natural height. The arrow and page keys
  * scroll it when no widget takes them, the wheel and its scrollbar
  * move it, and `scrollToWidget` moves it to a widget by key.
  *
  * A buffer-mounted panel only; the dock and the floating panels
  * window their lists. Unspecified reads as `false`.
  */
  page?: boolean;
  /**
  * Keep this panel's focus and the reader's place on the same thing.
  *
  * For a `page` — a document laid out by
  * widgets, in one window the host scrolls — focus and where the reader is
  * are two answers
  * to one question: what am I looking at. Left independent they contradict
  * each other, and the contradiction is not cosmetic: Tab moves focus while
  * the page stays three cards above, and a movement key moves the page
  * while Enter still fires whatever the last Tab left focused — off screen,
  * unasked for.
  *
  * Saying so makes the host maintain both directions. The movement keys
  * (`Up`/`Down`, the page keys, `Home`/`End`) move a *reading row* through
  * the page's content instead of scrolling the window, and focus goes to
  * the widget on the row it lands on — or to nothing, when the row carries
  * none. A focus move (Tab, Shift+Tab, a plugin's `setFocusKey`) puts the
  * reader on the focused widget's own region, and the window follows
  * minimally, so a Tab between two controls of one card does not move the
  * page under them.
  *
  * "Nothing focused" is a state this option produces constantly — most rows
  * of a page are prose — so a panel declaring it almost certainly wants
  * `autoFocusFirst: false` too, and the Tab ring seeds from the reader
  * rather than from the top of the document.
  *
  * Leaving it out reads as `false`: every panel written before this field keeps
  * focus and the window independent.
  *
  * It makes `autoFocusFirst` false whatever the panel said. The pair is not a
  * setting with two useful values; it is one broken combination, so
  * it is not representable rather than advised against.
  */
  focusFollowsCursor?: boolean;
};
```

### `WidgetSpec`

Declarative widget tree. Each variant is one node; nested
composition is via `Row { children }` / `Col { children }`.

`key` is the stable identifier used by the reconciler to match a
node across `MountWidgetPanel` / `UpdateWidgetPanel` calls — when
the plugin re-emits a Spec, instance state (cursor offset, scroll,
expanded keys, hover) is preserved on nodes whose `key` matches.
Plugins should provide stable keys for any widget that owns
instance state; stateless widgets (`HintBar`, `Toggle`, `Button`,
`Spacer`) can omit it.

```typescript
type WidgetSpec = {
  "kind": "row";
  children: Array<WidgetSpec>;
  key?: string | null;
  /**
  * When true, children that don't fit on one line reflow onto
  * additional lines (growing the row's height) instead of being
  * truncated. Children are never split — wrap happens at child
  * boundaries — so wrap a logical group (e.g. a toggle + its
  * accelerator) in a nested non-wrapping `Row` to keep it intact.
  * Ignored when the row contains multi-line (block) children.
  */
  wrap: boolean;
  /**
  * Settle a wrapping row's lines against its right edge: buttons
  * flush right while they fit, wrapping from the left when they do
  * not. Read only when `wrap` is set.
  *
  * The point is that the plugin does not have to know which of those
  * two layouts it is getting — the host lays the row out at a width
  * the plugin cannot see, and a guess made here is a guess about a
  * frame that has not happened yet.
  */
  justifyEnd: boolean;
} | {
  "kind": "col";
  children: Array<WidgetSpec>;
  key?: string | null;
} | {
  "kind": "hintBar";
  entries: Array<HintEntry>;
  key?: string | null;
} | {
  "kind": "toggle";
  checked: boolean;
  label: string;
  focused: boolean;
  /**
  * Neither checked nor unchecked: renders a neutral `[-]` chip.
  * Used for values that are unset and inherit from a lower
  * config layer (issue #2345) — a definite `[ ]` would read as
  * "the user turned this off". Defaults to `false`.
  */
  indeterminate: boolean;
  /**
  * Form-style layout: render `label: [v]` (label first, chip
  * after) instead of the default `[v] label`. In this layout
  * the toggle's hit area covers only the chip, so clicks on
  * the label don't flip the value. Defaults to `false`.
  */
  labelFirst: boolean;
  /**
  * The label column a form's controls share, in display cells.
  * `0` = no column alignment. Defaults to `0`.
  *
  * **It means something in both layouts, and not the same thing.**
  * With `label_first`, it pads this toggle's own label so its chip
  * lines up with its siblings' value cells. Chip-first, the toggle
  * has no label in that column at all, so it indents the *chip*
  * there instead — which is how `[v] Remember this machine` sits
  * under the fields above it rather than at the panel's edge. A
  * chip-first toggle in a panel that sets a `label_width` therefore
  * moves right by that much plus the `: ` its siblings spend;
  * before this field was read on that path it stayed flush left.
  */
  labelWidth: number;
  /**
  * The keyboard accelerator's letter, underlined where it first
  * appears in `label` (case-insensitively) — the classic menu-bar
  * mnemonic, so `Alt+L` reads as the `l` in `Files`. Absent, or a
  * letter the label does not contain, underlines nothing.
  */
  mnemonic?: string | null;
  key?: string | null;
} | {
  "kind": "number";
  /**
  * Initial value. Read at first render only; instance state
  * takes over thereafter.
  */
  value: number;
  /**
  * Inclusive lower bound. Values clamp to it when set.
  */
  min?: number | null;
  /**
  * Inclusive upper bound. Values clamp to it when set.
  */
  max?: number | null;
  /**
  * Amount added / subtracted per step. Defaults to `1`.
  */
  step: number;
  /**
  * Render the value as an integer (no decimal point). The
  * value itself is not rounded; only the display is
  * truncated. Defaults to `false`.
  */
  integer: boolean;
  /**
  * Render the value as a percentage: display is `value * 100`
  * suffixed with `%` (so a stored `0.25` shows as `25%`).
  * Mirrors the Settings UI's float-as-percent controls.
  * Defaults to `false`.
  */
  percent: boolean;
  /**
  * Optional label rendered before the value cell. Empty =
  * omitted.
  */
  label?: string;
  /**
  * Whether this widget has visual focus. Initial-only once
  * the host owns focus (same as `Toggle`).
  */
  focused: boolean;
  /**
  * Pad the label to this display width so a column of
  * controls aligns their value cells. `0` = no padding.
  */
  labelWidth: number;
  key?: string | null;
} | {
  "kind": "dropdown";
  /**
  * The selectable options, in display order.
  */
  options: Array<string>;
  /**
  * Initial selected index into `options`. Read at first
  * render only; instance state takes over thereafter.
  * Clamped to `[0, options.len())`.
  */
  selectedIndex: number;
  /**
  * Optional label rendered before the value button. Empty =
  * omitted.
  */
  label?: string;
  /**
  * Whether this widget has visual focus. Initial-only once
  * the host owns focus.
  */
  focused: boolean;
  /**
  * Pad the label to this display width so a column of
  * controls aligns their value buttons. `0` = no padding.
  */
  labelWidth: number;
  /**
  * Whether the option list is expanded inline below the value
  * button (`▲` arrow + one row per visible option). Closed
  * (`▼`) by default.
  */
  open: boolean;
  /**
  * First visible option row when `open` and the list is
  * taller than its window. Defaults to `0`.
  */
  scrollOffset: number;
  key?: string | null;
} | {
  "kind": "radio";
  /**
  * The selectable options, in display order.
  */
  options: Array<string>;
  /**
  * Initial selected index into `options`. Read at first render
  * only; instance state takes over thereafter. Clamped to
  * `[0, options.len())`.
  */
  selectedIndex: number;
  /**
  * Optional label rendered before the options. Empty = omitted.
  */
  label?: string;
  /**
  * Whether this widget has visual focus. Initial-only once the
  * host owns focus.
  */
  focused: boolean;
  /**
  * Pad the label to this display width so a column of controls
  * aligns their option cells. `0` = no padding.
  */
  labelWidth: number;
  key?: string | null;
} | {
  "kind": "dualList";
  /**
  * The full universe of selectable options.
  */
  options: Array<DualListOption>;
  /**
  * Initial ordered set of included option values. Seed only;
  * instance state takes over after first render.
  */
  included: Array<string>;
  /**
  * Option values owned by a sibling list — filtered out of
  * this list's Available column so the two never overlap.
  */
  excluded: Array<string>;
  /**
  * Optional label rendered above the columns. Empty =
  * omitted.
  */
  label?: string;
  /**
  * Whether this widget has visual focus. Initial-only once
  * the host owns focus.
  */
  focused: boolean;
  /**
  * Which column the cursor sits in: `true` = Included,
  * `false` = Available. Seed only, like `included` — host
  * instance state takes over after first render. Hosts that
  * drive the control themselves (Settings) keep re-supplying
  * it so the rendered cursor tracks their own state.
  */
  activeIncluded: boolean;
  /**
  * Cursor row within the Available column. Seed only (see
  * `active_included`).
  */
  availableCursor: number;
  /**
  * Cursor row within the Included column. Seed only (see
  * `active_included`).
  */
  includedCursor: number;
  /**
  * Optional one-line key hint rendered under the columns
  * (e.g. `↑↓:Move  Shift+←→:Add/Remove`). Empty = omitted.
  * The control's keys are not guessable from its shape, so
  * hosts are expected to supply their own localized copy.
  */
  hint?: string;
  /**
  * Number of body rows the columns occupy. Plugin computes
  * from its viewport.
  */
  visibleRows: number;
  key?: string | null;
} | {
  "kind": "button";
  label: string;
  focused: boolean;
  intent: ButtonKind;
  key?: string | null;
  /**
  * When true, the button renders in a muted style, is dropped
  * from the Tab cycle, and clicks on it are ignored. Use for
  * actions that aren't currently available against the
  * surrounding state (e.g. "Archive" on the base session). The
  * button still occupies its layout cell so the surrounding
  * row doesn't reshuffle when the disabled flag flips.
  */
  disabled: boolean;
  /**
  * When false, the button is dropped from the Tab cycle (but
  * still renders and stays clickable). Used for radio-style
  * groups — a row of buttons where only the *active* option
  * should be a Tab stop and ←/→ moves the selection within
  * the group, so Tab advances one stop per group rather than
  * one stop per option. Defaults to true (ordinary buttons
  * are tabbable).
  */
  focusable: boolean;
  /**
  * Render the label alone — no `[ ]` frame, no focus-marker
  * gutter — turning the button into a bare *icon affordance*
  * (a `×` close glyph, a `▾` chevron) rather than a framed
  * action. Use it where the glyph itself is the control and a
  * frame would read as clutter; keep the default `false` for
  * anything with a word on it.
  *
  * This controls layout only; `hover_style` controls how the
  * button looks under the pointer.
  */
  bare: boolean;
  /**
  * Stretch the button across the full width it is laid out in:
  * the panel's content width, its share of an enclosing `Row`,
  * or the width an anchored popup settled on.
  *
  * This exists because focus / hover paint the button's *own*
  * cells: a natural-width button leaves the rest of its row
  * unhighlighted even when the surrounding container pads the
  * row out (a `LabeledSection` pads every child to its inner
  * width). Dropdown and context-menu entries are rows of a
  * menu, not free-standing actions, so their highlight has to
  * span the row.
  *
  * **It is a width, not a longer label.** The host sizes the
  * button's box to its content and lets the enclosing column
  * stretch it, so the label stays the label and how wide the
  * row is stays layout's answer — including an answer nobody
  * can predict, like a dock the user just dragged. It used to
  * be spelled by padding the label with spaces out to a width
  * the caller had to supply, which is why it once carried a
  * warning against using it inside an anchored popup that hugs
  * its content: a box sized by its own padded text cannot hug.
  * A box sized by its content can, and the stretch is then what
  * widens every row to the widest one — so a menu no longer
  * pads its labels to align them.
  *
  * A label too long for the width it is given is truncated at
  * the tail with an `…`.
  */
  fullWidth: boolean;
  /**
  * Style applied while the pointer is over this button. Omit it
  * (the default) to leave it looking the same hovered as not.
  *
  * Hover is host state — it changes with mouse motion and no
  * plugin round-trip — so the plugin declares the *appearance*
  * once in the spec and the host applies it as the pointer
  * moves. Nothing crosses the plugin bridge on a hover.
  *
  * It outranks focus styling while both apply: the pointer is
  * the more immediate signal, and the one the user is actively
  * driving.
  *
  * For a close glyph, `ui.tab_close_hover_fg` is the editor's
  * shared "close affordance under the pointer" key — the tab
  * `×` and the file explorer's `×` both read it, so a plugin
  * naming it gets the same highlight users already know.
  */
  hoverStyle?: Partial<OverlayOptions>;
  /**
  * How the button looks at rest — not focused, not hovered,
  * not disabled. Omit it (the default) to keep the look its
  * `intent` gives it.
  *
  * The sibling of `hover_style`, and the answer to the same
  * question one state earlier: `hover_style` could say what a
  * control looks like under the pointer, but nothing could say
  * that it is a control at all. A bare button is just its
  * label, so without this the only way to mark a word as
  * clickable was to spend a colour on it — and `intent` offers
  * three fixed looks, none of them an underline.
  *
  * Focus, hover and disabled each still win over it, in that
  * order of immediacy.
  */
  style?: Partial<OverlayOptions>;
} | {
  "kind": "spacer";
  cols: number;
  flex: boolean;
  key?: string | null;
} | {
  "kind": "divider";
  /**
  * Glyph repeated across the full width. Defaults to `─`.
  */
  ch: string;
  /**
  * Optional whole-rule styling (e.g. a dim `fg`). Same shape as a
  * styled segment's `style`.
  */
  style?: Partial<OverlayOptions>;
  key?: string | null;
} | {
  "kind": "list";
  items: Array<TextPropertyEntry>;
  /**
  * Optional parallel array of per-item widget specs. When
  * non-empty it **overrides** `items`: each entry is rendered
  * via the normal widget renderer into a multi-row block
  * (e.g. a `LabeledSection` for a rounded "card"/"pill"), and
  * the list lays items out, selects, scrolls, and routes
  * clicks in *item* units — one card per logical item,
  * regardless of how many terminal rows it occupies. All
  * cards share a uniform height (the tallest item's row count;
  * shorter items pad). `item_keys` / `selected_index` are
  * still indexed per item. Interactive widgets nested inside a
  * card aren't routed yet — the whole card is one `select`
  * hit. Leave empty for the classic one-row-per-`items` list.
  */
  itemSpecs?: Array<WidgetSpec>;
  itemKeys: Array<string>;
  selectedIndex: number;
  /**
  * Number of rows of the panel's available height the list
  * should occupy. Omitted = auto: the host sizes the
  * window from the panel height it already knows, so the
  * plugin never re-derives layout arithmetic. An explicit
  * value pins the window to that many rows, exactly as
  * before. (Legacy fallback when the host has no height for
  * the surface: 20 rows.)
  */
  visibleRows?: number | null;
  /**
  * Whether `Tab` / `Shift+Tab` will land focus on this
  * list. Defaults to `true` (lists are normal tabbable
  * widgets). Picker-style usage typically sets this to
  * `false` so Tab moves between the filter input and
  * the action buttons, while Up/Down on the focused
  * filter still forwards to the list via host smart-key
  * dispatch.
  */
  focusable: boolean;
  /**
  * Typing jumps the selection to the next item whose text starts
  * with what was typed (the listbox pattern's type-ahead). Off by
  * default: a list that is a command surface — Git Log's `q`, a
  * dock's single-key actions — binds those letters in its mode, and
  * the focused widget is asked first. Turn it on for a list of names
  * to find, such as a file browser.
  */
  typeAhead: boolean;
  key?: string | null;
} | {
  "kind": "tree";
  nodes: Array<TreeNode>;
  itemKeys: Array<string>;
  selectedIndex: number;
  /**
  * Rows of the panel's available height the tree occupies.
  * Omitted = auto from the host-known panel height;
  * an explicit value pins the window as before. (Legacy
  * fallback when the host has no height: 20 rows.)
  */
  visibleRows?: number | null;
  /**
  * Seed set of expanded item keys, drawn until the host's
  * instance state has an expansion of its own (a Right/Left,
  * a disclosure click, a selection write, or a
  * `setExpandedKeys` mutation). From then on the
  * instance state is what is drawn and navigated, and
  * changing this field on later specs has no effect — use
  * `editor.widgetMutate(panel, { kind: "setExpandedKeys", ... })` to change it.
  */
  expandedKeys: Array<string>;
  /**
  * When true, every node with `checked` set renders a
  * `[v]` / `[ ]` glyph and emits a `toggle` hit area over
  * the glyph. Click on the glyph fires `widget_event {
  * event_type: "toggle", payload: { key, checked: <new> } }`;
  * the plugin updates its model and pushes the new state
  * back via `editor.widgetMutate(panel, { kind: "setCheckedKeys", ... })`.
  */
  checkable: boolean;
  /**
  * Fixed number of screen rows each node occupies. `1` (the
  * default) is the classic single-line tree. A larger value
  * renders every node as a card of that many rows — the
  * node's primary `text` plus its `extra_lines`, blank-padded
  * to this height. Windowing/scroll stay node-based (the node
  * budget becomes `visible_rows / item_height`), so all
  * existing single-line trees are unaffected.
  */
  itemHeight: number;
  /**
  * When true (and `item_height > 1`), each *card* node — a
  * leaf carrying `extra_lines` and no checkbox — renders
  * inside a rounded border (`╭─…─╮` / `╰─…─╯` spanning the
  * panel width), taking `item_height + 2` rows: top border,
  * `item_height` content rows, bottom border. Non-card nodes
  * (e.g. folder headers) render as plain single rows instead
  * of being blank-padded to the card height. Restores the
  * bordered-pill look the Orchestrator dock's card density
  * had before it moved to a tree (issue #2703). Scroll and
  * selection stay node-based; rows per node just vary.
  */
  cardBorders: boolean;
  /**
  * Columns of indent per depth level. `2` (the default) is the
  * classic tree step. A panel only a couple of dozen columns wide
  * that nests several levels deep can drop to `1` and spend those
  * columns on node text instead — each level is still marked by
  * the disclosure glyph (or the blank standing in for one).
  */
  indentCols: number;
  /**
  * When true, a click anywhere on a node with children toggles its
  * expansion (and selects it), not only a click on the disclosure
  * glyph. The toggle fires `expand` with `{ index, key, expanded }`.
  */
  toggleOnClick: boolean;
  /**
  * **A table.** Columns declared here turn every node that carries
  * `cells` into a row of them: the host measures the cells, fits the
  * columns to the width layout gives the tree (the widest gives
  * first), elides each cell at its column's end, and draws a header
  * row of the titles above the tree. Empty (default): a plain tree.
  */
  columns?: Array<TableColumn>;
  key?: string | null;
} | {
  "kind": "text";
  /**
  * Initial text. Spec value is read at first render only;
  * instance state takes over thereafter.
  */
  value: string;
  /**
  * Initial byte-offset cursor within `value`. A negative
  * number means "no cursor"; other values are clamped
  * host-side to the byte length of `value`.
  */
  cursorByte: number;
  /**
  * Whether this widget has visual focus.
  */
  focused: boolean;
  /**
  * Optional label rendered before / above the editing
  * region. Empty = omitted.
  */
  label?: string;
  /**
  * Placeholder shown when unfocused and `value` is empty.
  */
  placeholder?: string | null;
  /**
  * Number of visible rows of editing region. `0` falls back
  * to `1` (single-line). `1` = single-line behaviour;
  * `>= 2` = multi-line behaviour. See the type-level doc
  * for the per-mode semantics.
  */
  rows: number;
  /**
  * Visible column width. `0` = auto-fit (single-line) or
  * panel width (multi-line). When set, single-line
  * head-truncates with `…` and multi-line tail-truncates
  * per-line.
  */
  fieldWidth: number;
  /**
  * Single-line soft cap on visible chars after the
  * `field_width` pad. `0` = no cap. Ignored when `rows > 1`.
  */
  maxVisibleChars: number;
  /**
  * Stretch the visible field to fill the available
  * width of the enclosing container. Overrides
  * `field_width` when set: the renderer computes
  * `panel_width - label_overhead - bracket_overhead` as
  * the effective visible width. Multi-line widgets
  * already fill the panel width by default; this flag is
  * most useful for single-line inputs inside a
  * `LabeledSection` or a flexible row.
  */
  fullWidth: boolean;
  /**
  * Optional completion candidates. When non-empty AND
  * `label` is non-empty (the chrome trigger), the
  * renderer paints a popup directly under the input,
  * inside a unified box: the input's normal `╰─...─╯`
  * bottom border becomes a dimmed `┄` separator, the
  * labeled section's side borders extend down through
  * the candidate rows, and a single `╰─...─╯` bottom
  * closes the whole block. Candidates render left-
  * aligned with the input's text (the position right
  * after `[`), with the host-managed selected index
  * highlighted.
  *
  * Smart-key dispatch on a focused Text-with-completions:
  * Up/Down moves selection (host-internal, no event),
  * Tab fires `completion_accept` with the selected
  * candidate, Enter / Escape fire `completion_dismiss`
  * (the dispatcher's normal "Enter focus-advance / Esc
  * close panel" only runs once the popup is closed).
  *
  * Plugins push candidates in response to the text
  * widget's `change` event via
  * `editor.widgetMutate(panel, { kind: "setCompletions", ... })`. An empty `items`
  * closes the popup.
  */
  completions?: Array<string | CompletionItem>;
  /**
  * How many candidate rows the popup paints at once
  * when it opens. Excess candidates stay reachable
  * via Up/Down (host auto-scrolls to keep selection
  * in view) or the mouse wheel; a thumb glyph paints
  * in the right edge of the popup whenever there's
  * more to scroll. `0` (default) falls back to `5`.
  */
  completionsVisibleRows: number;
  /**
  * A multi-line field that **grows with its text**: the smallest
  * number of editing rows it shows (`rows` when `0`). Only read when
  * `max_rows` is set.
  */
  minRows: number;
  /**
  * A multi-line field that **grows with its text**: when `> 0`, the
  * editing region is as tall as its value wraps to — at the width
  * layout actually gives it — between `min_rows` and this, and
  * scrolls (keeping its caret in view) past it. `0` (default): the
  * region is `rows` tall, as before.
  */
  maxRows: number;
  /**
  * Paint the caret as a REVERSED block cell inside the row
  * (in addition to publishing the hardware-cursor position).
  * Modal form surfaces (e.g. Settings) use this — a hardware
  * cursor isn't shown over a modal, so the block cell is the
  * only visible caret. Defaults to `false`.
  */
  blockCaret: boolean;
  /**
  * Selection byte range within `value` (`start`, `end`), shown
  * with the selection background while the widget is focused.
  * `-1` for either end = no selection. Seed-only, like
  * `cursor_byte`: once host-owned instance state exists, the
  * editor's own selection wins. Mirrors `Number`'s
  * `edit_sel_start`/`edit_sel_end`.
  */
  selStart: number;
  selEnd: number;
  /**
  * Form label-column width. When `> 0` (and `label` is set) the
  * single-line field pads the label to this column and separates
  * it from the value with `: `, so a column of `Text`, `Toggle`,
  * `Number`, and `Dropdown` controls aligns their value cells
  * (the Settings entry dialog sets it to the page's max label
  * width). `0` (default) keeps the compact `label [value]` form
  * plugins get by default. Clamped to keep the cell on-screen on
  * narrow surfaces.
  */
  labelWidth: number;
  /**
  * Reject every mutating operation (typing, Backspace/Delete,
  * Cut, Paste) while keeping caret motion, selection, and Copy.
  * Implied by `markdown`.
  */
  readOnly: boolean;
  /**
  * Render `value` as a markdown *document* (multi-line only,
  * `rows > 1`): the host renders it through the same markdown
  * engine as LSP hover docs — headings, emphasis, inline code,
  * links, syntax-highlighted fences — word-wrapped to the
  * widget's width. The caret, selection, and Copy operate on
  * the rendered plain text, so what you copy is what you see.
  * Markdown mode is **forcibly read-only**: the value only
  * changes via a spec update.
  */
  markdown: boolean;
  /**
  * A single-line field that offers a list of values to pick from as
  * well as free text — a combo box (the ARIA combobox pattern). Drawn
  * with a `▼` in the last cell inside its `]` (`▲` while its
  * completion list is open), so the field says it has a list before
  * it is focused. The list itself is still the plugin's
  * `completions`: with the list closed, ↓ / Alt+↓ or a press on the
  * arrow fires `completion_request`, which the plugin answers with
  * `setCompletions`; a press on the arrow with the list open closes
  * it (`completion_dismiss`). Opening on focus is left out on
  * purpose — a list that opens as a form is walked covers the fields
  * under it. Defaults to `false`.
  */
  combo: boolean;
  key?: string | null;
} | {
  "kind": "labeledSection";
  /**
  * Legend text printed in the top border. Empty = no
  * legend (the top border becomes one unbroken line).
  */
  label: string;
  /**
  * The single wrapped widget. Boxed because `WidgetSpec`
  * is recursive.
  */
  child: WidgetSpec;
  /**
  * When this section is a Block child of a Row, request
  * `width_pct` percent of the row's `panel_width` instead
  * of the equal-split default. Multiple siblings with
  * `width_pct` set sum to ≤ 100; the remainder splits
  * equally among siblings without an explicit width.
  * Out-of-range values (0 or > 100) fall back to the
  * equal-split path.
  */
  widthPct?: number | null;
  /**
  * When this section is a Block child of a Row, request exactly
  * this many columns. Takes precedence over `width_pct`.
  *
  * A percent cannot express "a third of the row": the integer
  * rounding does not divide, so three equal siblings either
  * overflow the panel — and the host wraps the last one onto a
  * line of its own — or leave a ragged remainder that all lands
  * on one side. Columns are what a caller with a measure in mind
  * actually has, and asking in them is exact.
  */
  widthCols?: number | null;
  key?: string | null;
  /**
  * How the section's own chrome — its border and its legend —
  * looks while `key` is the hovered widget.
  *
  * A section emits no hit area of its own, so it never becomes
  * the hovered widget by being pointed at. Give it the key of
  * the control inside it and the frame answers with that
  * control: a card whose rows share one key lights as a card
  * rather than one row at a time.
  */
  hoverStyle?: Partial<OverlayOptions>;
} | {
  "kind": "windowEmbed";
  /**
  * Numeric editor-window id, matching `WindowId(N).0`.
  * `0` (or any unknown id) renders empty placeholder
  * rows without dispatching the per-window render.
  */
  windowId: number;
  /**
  * Number of visible rows the embed should occupy.
  */
  rows: number;
  key?: string | null;
} | {
  "kind": "label";
  text: string;
  style?: Partial<OverlayOptions>;
  /**
  * Indent into the field column of a form whose controls share
  * this label width. `0` = flush left.
  */
  labelWidth: number;
  /**
  * Two or more styled runs on the one row — a state glyph in the
  * state's colour, then the name it belongs to. When non-empty
  * these replace `text`, exactly as they do on a
  * `TextPropertyEntry`; `style` still covers the whole row.
  * Without this a plugin needing two inks on a line has to drop
  * to `Raw`, which is a list of whole rows and no longer a label.
  */
  segments?: Array<StyledSegment>;
  /**
  * Break the text across rows instead of clipping it, at the width
  * layout settles on. Continuation rows start at the line's own
  * leading indent — the marker gutter and `label_width` count, so a
  * wrapped field hint stays inside the field column — and a word too
  * long for a row of its own is broken rather than run off the edge.
  *
  * The alternative is the plugin wrapping the prose itself, which
  * means deciding the width, which is layout's answer and not the
  * plugin's: see `fresh_ui::desc::Wrap`.
  */
  wrap: boolean;
  /**
  * How the row marks itself when it does not fit. See `Elide`.
  * Ignored when `wrap` is set.
  */
  elide: Elide;
} | {
  "kind": "raw";
  entries: Array<TextPropertyEntry>;
  key?: string | null;
} | {
  "kind": "overlay";
  child: WidgetSpec;
  key?: string | null;
} | {
  "kind": "component";
  child: WidgetSpec;
  key?: string | null;
} | {
  "kind": "popup";
  child: WidgetSpec;
  key?: string | null;
  /**
  * Anchor `[row, col]` in the panel's inner coordinates the
  * popup drops from (the host resolves the final screen rect
  * — opening below the anchor, flipping above near the frame
  * edge, clamped on screen). Omitted, it anchors at the popup's
  * own position in the tree.
  */
  anchor?: [number, number] | null;
  /**
  * When true, the popup escapes the panel's clipping and is
  * painted at screen level (what the dropdown pop-over does);
  * false keeps it panel-clipped like `Overlay`.
  */
  screenSpace: boolean;
};
```

### `WindowInfo`

Information about an editor session (plugin-visible). Returned
by `editor.listWindows()` and carried in the snapshot.

```typescript
type WindowInfo = {
  /**
  * Stable session id. The base session is always `1`.
  */
  id: number;
  /**
  * Durable workspace identity (`ws-…`), minted once when the workspace is
  * created and carried in its on-disk snapshot. Unlike `id` — a per-process
  * handle re-derived at every boot — this survives restarts, relabels and
  * moves, so it is the id to hand out to anything that must still mean the
  * same workspace later (an agent recording where it put its work, say).
  * Empty only for a legacy workspace file written before stable ids.
  */
  stable_id: string;
  /**
  * User-visible label (defaults to root basename).
  */
  label: string;
  /**
  * Absolute project root.
  */
  root: string;
  /**
  * Project this session belongs to — the canonical repo root
  * (or arbitrary directory) the user pointed the new-session
  * form at. For sessions without an explicit project (legacy
  * sessions, the launch session, sessions created outside the
  * orchestrator's new-session form) this equals `root`, so it
  * is never empty, `null` or `undefined`.
  */
  project_path: string;
  /**
  * `true` when the session shares its working tree with
  * other sessions (worktree-creation was off at session
  * time, or the session lives in a non-git directory).
  * Persistence-only field; defaults to `false` and isn't
  * emitted when false.
  */
  shared_worktree?: boolean;
  /**
  * Remote backend identity when this session's backend is not
  * host-local (SSH / Kubernetes). Carried for live remote windows
  * *and* for dormant (not-yet-connected / disconnected) sessions, so
  * the dock can badge a restored SSH session before any connection
  * exists. Omitted for local sessions and plugin-managed backends
  * (devcontainer), whose facet the owning plugin supplies itself.
  */
  remote?: RemoteBackendInfo | null;
};
```

### `WindowPath`

```typescript
type WindowPath = {
  kind: "authority";
  window: number;
  value: string;
};
```

### `WorkspaceDescription`

The answer to "what does the editor look like right now" — the call an
agent makes before deciding what to change.

```typescript
type WorkspaceDescription = {
  /**
  * Working directory of the active window.
  */
  cwd: string;
  /**
  * The window this script is pointed at.
  */
  windowId: bigint;
  /**
  * Durable id of that window — the one still valid after a restart.
  */
  stableId: string;
  /**
  * Every open workspace, so a script can tell whether the thing it
  * wants is in another window.
  */
  windows: Array<WindowInfo>;
  /**
  * The active window's panes, in visual order (left to right, top to
  * bottom).
  */
  panes: Array<PaneDescription>;
  /**
  * Which pane has focus; also flagged on the pane itself.
  */
  activeSplitId: number;
};
```

:::
