# Fresh Editor Plugin API

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

### Modes

Keybinding contexts that determine how keypresses are interpreted. Each buffer has a mode (e.g., "normal", "insert", "special"). Custom modes can inherit from parents and define buffer-local keybindings. Virtual buffers typically use custom modes.

## Types

### FileExplorerDecoration

File explorer decoration entry provided by plugins

```typescript
interface FileExplorerDecoration {
  path: string;
  symbol?: string | null;
  color?: [u8; 3] | null;
  priority?: number | null;
}
```

| Field | Description |
|-------|-------------|
| `path` | Absolute or workspace-relative path to decorate |
| `symbol` | Symbol to display (single character recommended) |
| `color` | RGB color for the symbol |
| `priority` | Priority for resolving conflicts (higher wins) |

### SpawnResult

Result from spawnProcess

```typescript
interface SpawnResult {
  stdout: string;
  stderr: string;
  exit_code: number;
}
```

| Field | Description |
|-------|-------------|
| `stdout` | Complete stdout as string. Newlines preserved; trailing newline included. |
| `stderr` | Complete stderr as string. Contains error messages and warnings. |
| `exit_code` | Process exit code. 0 usually means success; -1 if process was killed. |

### BackgroundProcessResult

Result from spawnBackgroundProcess - just the process ID

```typescript
interface BackgroundProcessResult {
  process_id: number;
}
```

| Field | Description |
|-------|-------------|
| `process_id` | Unique process ID for later reference (kill, status check) |

### FileStat

File stat information

```typescript
interface FileStat {
  exists: boolean;
  is_file: boolean;
  is_dir: boolean;
  size: number;
  readonly: boolean;
}
```

| Field | Description |
|-------|-------------|
| `exists` | Whether the path exists |
| `is_file` | Whether the path is a file |
| `is_dir` | Whether the path is a directory |
| `size` | File size in bytes |
| `readonly` | Whether the file is read-only |

### BufferInfo

Buffer information

```typescript
interface BufferInfo {
  id: number;
  path: string;
  modified: boolean;
  length: number;
}
```

| Field | Description |
|-------|-------------|
| `id` | Unique buffer ID |
| `path` | File path (empty string if no path) |
| `modified` | Whether buffer has unsaved changes |
| `length` | Buffer length in bytes |

### TsBufferSavedDiff

Diff vs last save for a buffer

```typescript
interface TsBufferSavedDiff {
  equal: boolean;
  byte_ranges: [number, number][];
  line_ranges?: [number, number][] | null;
}
```

### SelectionRange

Selection range

```typescript
interface SelectionRange {
  start: number;
  end: number;
}
```

| Field | Description |
|-------|-------------|
| `start` | Start byte position |
| `end` | End byte position |

### CursorInfo

Cursor information with optional selection

```typescript
interface CursorInfo {
  position: number;
  selection: { start: number; end: number } | null;
}
```

| Field | Description |
|-------|-------------|
| `position` | Byte position of the cursor |
| `selection` | Selection range `{ start, end }` if text is selected, `null` otherwise |

### TsDiagnosticPosition

LSP diagnostic position

```typescript
interface TsDiagnosticPosition {
  line: number;
  character: number;
}
```

### TsDiagnosticRange

LSP diagnostic range

```typescript
interface TsDiagnosticRange {
  start: TsDiagnosticPosition;
  end: TsDiagnosticPosition;
}
```

### TsDiagnostic

LSP diagnostic item for TypeScript plugins

```typescript
interface TsDiagnostic {
  uri: string;
  severity: number;
  message: string;
  source?: string | null;
  range: TsDiagnosticRange;
}
```

| Field | Description |
|-------|-------------|
| `uri` | File URI (e.g., "file:///path/to/file.rs") |
| `severity` | Diagnostic severity: 1=Error, 2=Warning, 3=Info, 4=Hint |
| `message` | Diagnostic message |
| `source` | Source of the diagnostic (e.g., "rust-analyzer") |
| `range` | Location range in the file |

### ViewportInfo

Viewport information

```typescript
interface ViewportInfo {
  top_byte: number;
  left_column: number;
  width: number;
  height: number;
}
```

| Field | Description |
|-------|-------------|
| `top_byte` | Byte offset of the top-left visible position |
| `left_column` | Column offset for horizontal scrolling |
| `width` | Viewport width in columns |
| `height` | Viewport height in rows |

### PromptSuggestion

Suggestion for prompt autocomplete

```typescript
interface PromptSuggestion {
  text: string;
  description?: string | null;
  value?: string | null;
  disabled?: boolean | null;
  keybinding?: string | null;
}
```

| Field | Description |
|-------|-------------|
| `text` | Display text for the suggestion |
| `description` | Optional description shown alongside |
| `value` | Optional value to use instead of text when selected |
| `disabled` | Whether the suggestion is disabled |
| `keybinding` | Optional keybinding hint |

### DirEntry

Directory entry from readDir

```typescript
interface DirEntry {
  name: string;
  is_file: boolean;
  is_dir: boolean;
}
```

| Field | Description |
|-------|-------------|
| `name` | Entry name only (not full path). Join with parent path to get absolute path. |
| `is_file` | True if entry is a regular file |
| `is_dir` | True if entry is a directory. Note: symlinks report the target type. |

### TextPropertyEntry

Entry for virtual buffer content with embedded metadata

```typescript
interface TextPropertyEntry {
  text: string;
  properties: Record<string, unknown>;
}
```

| Field | Description |
|-------|-------------|
| `text` | Text to display. Include trailing newline for separate lines. |
| `properties` | Arbitrary metadata queryable via getTextPropertiesAtCursor. |

### CreateVirtualBufferResult

Result from createVirtualBufferInSplit

```typescript
interface CreateVirtualBufferResult {
  buffer_id: number;
  split_id?: number | null;
}
```

### CreateVirtualBufferOptions

Configuration for createVirtualBufferInSplit

```typescript
interface CreateVirtualBufferOptions {
  name: string;
  mode: string;
  read_only: boolean;
  entries: TextPropertyEntry[];
  ratio: number;
  direction?: string | null;
  panel_id?: string | null;
  show_line_numbers?: boolean | null;
  show_cursors?: boolean | null;
  editing_disabled?: boolean | null;
  line_wrap?: boolean | null;
}
```

| Field | Description |
|-------|-------------|
| `name` | Buffer name shown in status bar (convention: "*Name*") |
| `mode` | Mode for keybindings; define with defineMode first |
| `read_only` | Prevent text modifications |
| `entries` | Content with embedded metadata |
| `ratio` | Split ratio (0.3 = new pane gets 30% of space) |
| `direction` | Split direction: "horizontal" (below) or "vertical" (side-by-side). Default: horizontal |
| `panel_id` | If set and panel exists, update content instead of creating new buffer |
| `show_line_numbers` | Show line numbers gutter (default: true) |
| `show_cursors` | Show cursor in buffer (default: true) |
| `editing_disabled` | Disable all editing commands (default: false) |
| `line_wrap` | Enable/disable line wrapping (None = use global setting) |

### CreateVirtualBufferInExistingSplitOptions

Options for creating a virtual buffer in an existing split

```typescript
interface CreateVirtualBufferInExistingSplitOptions {
  name: string;
  mode: string;
  read_only: boolean;
  entries: TextPropertyEntry[];
  split_id: number;
  show_line_numbers?: boolean | null;
  show_cursors?: boolean | null;
  editing_disabled?: boolean | null;
  line_wrap?: boolean | null;
}
```

| Field | Description |
|-------|-------------|
| `name` | Display name (e.g., "*Commit Details*") |
| `mode` | Mode name for buffer-local keybindings |
| `read_only` | Whether the buffer is read-only |
| `entries` | Entries with text and embedded properties |
| `split_id` | Target split ID where the buffer should be displayed |
| `show_line_numbers` | Whether to show line numbers in the buffer (default true) |
| `show_cursors` | Whether to show cursors in the buffer (default true) |
| `editing_disabled` | Whether editing is disabled for this buffer (default false) |
| `line_wrap` | Enable/disable line wrapping (None = use global setting) |

### CreateVirtualBufferInCurrentSplitOptions

Options for creating a virtual buffer in the current split as a new tab

```typescript
interface CreateVirtualBufferInCurrentSplitOptions {
  name: string;
  mode: string;
  read_only: boolean;
  entries: TextPropertyEntry[];
  show_line_numbers?: boolean | null;
  show_cursors?: boolean | null;
  editing_disabled?: boolean | null;
  hidden_from_tabs?: boolean | null;
}
```

| Field | Description |
|-------|-------------|
| `name` | Display name (e.g., "*Help*") |
| `mode` | Mode name for buffer-local keybindings |
| `read_only` | Whether the buffer is read-only |
| `entries` | Entries with text and embedded properties |
| `show_line_numbers` | Whether to show line numbers in the buffer (default false for help/docs) |
| `show_cursors` | Whether to show cursors in the buffer (default true) |
| `editing_disabled` | Whether editing is disabled for this buffer (default false) |
| `hidden_from_tabs` | Whether this buffer should be hidden from tabs (for composite source buffers) |

### TsCompositeLayoutConfig

Layout configuration for composite buffers

```typescript
interface TsCompositeLayoutConfig {
  layout_type: string;
  ratios?: number[] | null;
  show_separator?: boolean | null;
  spacing?: number | null;
}
```

| Field | Description |
|-------|-------------|
| `layout_type` | Layout type: "side-by-side", "stacked", or "unified" |
| `ratios` | Relative widths for side-by-side layout (e.g., [0.5, 0.5]) |
| `show_separator` | Show separator between panes |
| `spacing` | Spacing between stacked panes |

### TsCompositePaneStyle

Pane style configuration

```typescript
interface TsCompositePaneStyle {
  add_bg?: [number, number, number] | null;
  remove_bg?: [number, number, number] | null;
  modify_bg?: [number, number, number] | null;
  gutter_style?: string | null;
}
```

| Field | Description |
|-------|-------------|
| `add_bg` | Background color for added lines (RGB tuple) |
| `remove_bg` | Background color for removed lines (RGB tuple) |
| `modify_bg` | Background color for modified lines (RGB tuple) |
| `gutter_style` | Gutter style: "line-numbers", "diff-markers", "both", "none" |

### TsCompositeSourceConfig

Source pane configuration for composite buffers

```typescript
interface TsCompositeSourceConfig {
  buffer_id: number;
  label?: string | null;
  editable: boolean;
  style?: TsCompositePaneStyle | null;
}
```

| Field | Description |
|-------|-------------|
| `buffer_id` | Buffer ID to display in this pane |
| `label` | Label for the pane (shown in header) |
| `editable` | Whether the pane is editable |
| `style` | Pane styling options |

### TsCompositeHunk

Diff hunk configuration

```typescript
interface TsCompositeHunk {
  old_start: number;
  old_count: number;
  new_start: number;
  new_count: number;
}
```

| Field | Description |
|-------|-------------|
| `old_start` | Start line in old file (0-indexed) |
| `old_count` | Number of lines in old file |
| `new_start` | Start line in new file (0-indexed) |
| `new_count` | Number of lines in new file |

### TsCreateCompositeBufferOptions

Options for creating a composite buffer

```typescript
interface TsCreateCompositeBufferOptions {
  name: string;
  mode: string;
  layout: TsCompositeLayoutConfig;
  sources: TsCompositeSourceConfig[];
  hunks?: TsCompositeHunk[] | null;
}
```

| Field | Description |
|-------|-------------|
| `name` | Display name for the composite buffer (shown in tab) |
| `mode` | Mode for keybindings (e.g., "diff-view") |
| `layout` | Layout configuration |
| `sources` | Source panes to display |
| `hunks` | Optional diff hunks for line alignment |

### TerminalResult

Result returned when creating a terminal

```typescript
interface TerminalResult {
  bufferId: number;
  terminalId: number;
  splitId: number | null;
}
```

| Field | Description |
|-------|-------------|
| `bufferId` | The created buffer ID (for use with `setSplitBuffer`, etc.) |
| `terminalId` | The terminal ID (for use with `sendTerminalInput`, `closeTerminal`) |
| `splitId` | The split ID (if created in a new split) |

### CreateTerminalOptions

Options for creating a terminal

```typescript
interface CreateTerminalOptions {
  cwd?: string;
  direction?: string;
  ratio?: number;
  focus?: boolean;
}
```

| Field | Description |
|-------|-------------|
| `cwd` | Working directory for the terminal (defaults to editor cwd) |
| `direction` | Split direction: `"horizontal"` or `"vertical"` (default: `"vertical"`) |
| `ratio` | Split ratio 0.0–1.0 (default: 0.5) |
| `focus` | Whether to focus the new terminal split (default: true) |

### ActionSpecJs

JavaScript representation of ActionSpec (with optional count)

```typescript
interface ActionSpecJs {
  action: string;
  count?: number | null;
}
```

### TsActionPopupAction

TypeScript struct for action popup action

```typescript
interface TsActionPopupAction {
  id: string;
  label: string;
}
```

### TsActionPopupOptions

TypeScript struct for action popup options

```typescript
interface TsActionPopupOptions {
  id: string;
  title: string;
  message: string;
  actions: TsActionPopupAction[];
}
```
