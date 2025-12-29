# Fresh Editor Plugin API

This document describes the TypeScript API available to Fresh editor plugins.

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

### Modes

Keybinding contexts that determine how keypresses are interpreted. Each buffer has a mode (e.g., "normal", "insert", "special"). Custom modes can inherit from parents and define buffer-local keybindings. Virtual buffers typically use custom modes.

## Types

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

### TsLineDiff

Line diff result for plugins

```typescript
interface TsLineDiff {
  equal: boolean;
  changed_lines: [number, number][];
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
  selection?: SelectionRange | null;
}
```

| Field | Description |
|-------|-------------|
| `position` | Byte position of the cursor |
| `selection` | Selection range if text is selected, null otherwise |

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

### ActionSpecJs

JavaScript representation of ActionSpec (with optional count)

```typescript
interface ActionSpecJs {
  action: string;
  count?: number | null;
}
```

## API Reference

### Status and Logging

#### `setStatus`

Display a transient message in the editor's status bar
The message will be shown until the next status update or user action.
Use for feedback on completed operations (e.g., "File saved", "2 matches found").

```typescript
setStatus(message: string): void
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `message` | `string` | Text to display; keep short (status bar has limited width) |

#### `debug`

Log a debug message to the editor's trace output
Messages appear in stderr when running with RUST_LOG=debug.
Useful for plugin development and troubleshooting.

```typescript
debug(message: string): void
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `message` | `string` | Debug message; include context like function name and relevant values |

### Buffer Queries

#### `getConfig`

Get the current editor configuration
Returns the merged configuration (user config file + compiled-in defaults).
This is the runtime config that the editor is actually using, including
all default values for LSP servers, languages, keybindings, etc.

```typescript
getConfig(): unknown
```

#### `getUserConfig`

Get the user's configuration (only explicitly set values)
Returns only the configuration from the user's config file.
Fields not present here are using default values.
Use this with getConfig() to determine which values are defaults.

```typescript
getUserConfig(): unknown
```

#### `getActiveBufferId`

Get the buffer ID of the focused editor pane
Returns 0 if no buffer is active (rare edge case).
Use this ID with other buffer operations like insertText.

```typescript
getActiveBufferId(): number
```

#### `getCursorPosition`

Get the byte offset of the primary cursor in the active buffer
Returns 0 if no cursor exists. For multi-cursor scenarios, use getAllCursors
to get all cursor positions with selection info.
Note: This is a byte offset, not a character index (UTF-8 matters).

```typescript
getCursorPosition(): number
```

#### `getBufferPath`

Get the absolute file path for a buffer
Returns empty string for unsaved buffers or virtual buffers.
The path is always absolute. Use this to determine file type,
construct related paths, or display to the user.

```typescript
getBufferPath(buffer_id: number): string
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `buffer_id` | `number` | Target buffer ID |

#### `getBufferLength`

Get the total byte length of a buffer's content
Returns 0 if buffer doesn't exist.

```typescript
getBufferLength(buffer_id: number): number
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `buffer_id` | `number` | Target buffer ID |

#### `isBufferModified`

Check if a buffer has been modified since last save
Returns false if buffer doesn't exist or has never been saved.
Virtual buffers are never considered modified.

```typescript
isBufferModified(buffer_id: number): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `buffer_id` | `number` | Target buffer ID |

#### `getActiveSplitId`

Get the ID of the focused split pane
Use with focusSplit, setSplitBuffer, or createVirtualBufferInExistingSplit
to manage split layouts.

```typescript
getActiveSplitId(): number
```

#### `getCursorLine`

Get the line number of the primary cursor (1-indexed)
Line numbers start at 1. Returns 1 if no cursor exists.
For byte offset use getCursorPosition instead.

```typescript
getCursorLine(): number
```

#### `getAllCursorPositions`

Get byte offsets of all cursors (multi-cursor support)
Returns array of positions; empty if no cursors. Primary cursor
is typically first. For selection info use getAllCursors instead.

```typescript
getAllCursorPositions(): number[]
```

#### `isProcessRunning`

Check if a background process is still running

```typescript
isProcessRunning(#[bigint] process_id: number): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `#[bigint] process_id` | `number` | - |

#### `getHighlights`

Compute syntax highlighting for a buffer range

```typescript
getHighlights(buffer_id: number, start: number, end: number): Promise<TsHighlightSpan[]>
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `buffer_id` | `number` | - |
| `start` | `number` | - |
| `end` | `number` | - |

#### `getLineByteOffset`

Get the byte offset of a line in a buffer

```typescript
getLineByteOffset(buffer_id: number, line: number): number
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `buffer_id` | `number` | - |
| `line` | `number` | - |

#### `getBufferSavedDiff`

Get diff vs last saved snapshot for a buffer

```typescript
getBufferSavedDiff(buffer_id: number): TsBufferSavedDiff | null
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `buffer_id` | `number` | - |

#### `getAllDiagnostics`

Get all LSP diagnostics across all files

```typescript
getAllDiagnostics(): TsDiagnostic[]
```

#### `getBufferText`

Get text from a buffer range
Used by vi mode plugin for yank operations - reads text without deleting.

```typescript
getBufferText(buffer_id: number, start: number, end: number): Promise<string>
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `buffer_id` | `number` | Buffer ID |
| `start` | `number` | Start byte offset |
| `end` | `number` | End byte offset |

#### `getEditorMode`

Get the current global editor mode

```typescript
getEditorMode(): string
```

### Buffer Info Queries

#### `getBufferInfo`

Get full information about a buffer

```typescript
getBufferInfo(buffer_id: number): BufferInfo | null
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `buffer_id` | `number` | Buffer ID |

#### `listBuffers`

List all open buffers

```typescript
listBuffers(): BufferInfo[]
```

#### `getPrimaryCursor`

Get primary cursor with selection info

```typescript
getPrimaryCursor(): CursorInfo | null
```

#### `getAllCursors`

Get all cursors (for multi-cursor support)

```typescript
getAllCursors(): CursorInfo[]
```

#### `getViewport`

Get viewport information

```typescript
getViewport(): ViewportInfo | null
```

### Prompt Operations

#### `startPrompt`

Start an interactive prompt

```typescript
startPrompt(label: string, prompt_type: string): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `label` | `string` | Label to display (e.g., "Git grep: ") |
| `prompt_type` | `string` | Type identifier (e.g., "git-grep") |

#### `setPromptSuggestions`

Set suggestions for the current prompt

```typescript
setPromptSuggestions(suggestions: PromptSuggestion[]): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `suggestions` | `PromptSuggestion[]` | Array of suggestions to display |

### Buffer Mutations

#### `applyTheme`

Apply a theme by name
Loads and applies the specified theme immediately. The theme can be a built-in
theme name or a custom theme from the themes directory.

```typescript
applyTheme(theme_name: string): void
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `theme_name` | `string` | Name of the theme to apply (e.g., "dark", "light", "my-custom-theme") |

#### `reloadConfig`

Reload configuration from file
After a plugin saves config changes to the config file, call this to reload
the editor's in-memory configuration. This ensures the editor and plugins
stay in sync with the saved config.

```typescript
reloadConfig(): void
```

#### `setClipboard`

Copy text to the system clipboard
Copies the provided text to both the internal and system clipboard.
Uses OSC 52 and arboard for cross-platform compatibility.

```typescript
setClipboard(text: string): void
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `text` | `string` | Text to copy to clipboard |

#### `insertText`

Insert text at a byte position in a buffer
Text is inserted before the byte at position. Position must be valid
(0 to buffer length). Insertion shifts all text after position.
Operation is asynchronous; returns true if command was sent successfully.

```typescript
insertText(buffer_id: number, position: number, text: string): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `buffer_id` | `number` | Target buffer ID |
| `position` | `number` | Byte offset where text will be inserted (must be at char boundary) |
| `text` | `string` | UTF-8 text to insert |

#### `deleteRange`

Delete a byte range from a buffer
Deletes bytes from start (inclusive) to end (exclusive).
Both positions must be at valid UTF-8 char boundaries.
Operation is asynchronous; returns true if command was sent successfully.

```typescript
deleteRange(buffer_id: number, start: number, end: number): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `buffer_id` | `number` | Target buffer ID |
| `start` | `number` | Start byte offset (inclusive) |
| `end` | `number` | End byte offset (exclusive) |

#### `clearNamespace`

Clear all overlays in a namespace

```typescript
clearNamespace(buffer_id: number, namespace: string): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `buffer_id` | `number` | The buffer ID |
| `namespace` | `string` | The namespace to clear |

#### `setLineNumbers`

Enable/disable line numbers for a buffer

```typescript
setLineNumbers(buffer_id: number, enabled: boolean): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `buffer_id` | `number` | The buffer ID |
| `enabled` | `boolean` | Whether to show line numbers |

#### `addVirtualLine`

Add a virtual line above or below a source line

```typescript
addVirtualLine(buffer_id: number, position: number, text: string, fg_r: number, fg_g: number, fg_b: number, bg_r: i16, bg_g: i16, bg_b: i16, above: boolean, namespace: string, priority: number): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `buffer_id` | `number` | The buffer ID |
| `position` | `number` | Byte position to anchor the virtual line to |
| `text` | `string` | The text content of the virtual line |
| `fg_r` | `number` | Foreground red color component (0-255) |
| `fg_g` | `number` | Foreground green color component (0-255) |
| `fg_b` | `number` | Foreground blue color component (0-255) |
| `bg_r` | `i16` | Background red color component (0-255), -1 for transparent |
| `bg_g` | `i16` | Background green color component (0-255), -1 for transparent |
| `bg_b` | `i16` | Background blue color component (0-255), -1 for transparent |
| `above` | `boolean` | Whether to insert above (true) or below (false) the line |
| `namespace` | `string` | Namespace for bulk removal (e.g., "git-blame") |
| `priority` | `number` | Priority for ordering multiple lines at same position |

#### `setLineIndicator`

Set a line indicator in the gutter's indicator column

```typescript
setLineIndicator(buffer_id: number, line: number, namespace: string, symbol: string, r: number, g: number, b: number, priority: number): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `buffer_id` | `number` | The buffer ID |
| `line` | `number` | Line number (0-indexed) |
| `namespace` | `string` | Namespace for grouping (e.g., "git-gutter", "breakpoints") |
| `symbol` | `string` | Symbol to display (e.g., "│", "●", "★") |
| `r` | `number` | Red color component (0-255) |
| `g` | `number` | Green color component (0-255) |
| `b` | `number` | uffer_id - The buffer ID |
| `priority` | `number` | Priority for display when multiple indicators exist (higher wins) |

#### `clearLineIndicators`

Clear all line indicators for a specific namespace

```typescript
clearLineIndicators(buffer_id: number, namespace: string): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `buffer_id` | `number` | The buffer ID |
| `namespace` | `string` | Namespace to clear (e.g., "git-gutter") |

#### `submitViewTransform`

Submit a transformed view stream for a viewport

```typescript
submitViewTransform(buffer_id: number, split_id?: number | null, start: number, end: number, tokens: ViewTokenWire[], layout_hints?: LayoutHints | null): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `buffer_id` | `number` | Buffer to apply the transform to |
| `split_id` | `number | null` (optional) | - |
| `start` | `number` | Viewport start byte |
| `end` | `number` | Viewport end byte |
| `tokens` | `ViewTokenWire[]` | Array of tokens with source offsets |
| `layout_hints` | `LayoutHints | null` (optional) | Optional layout hints (compose width, column guides) |

#### `clearViewTransform`

Clear view transform for a buffer/split (returns to normal rendering)

```typescript
clearViewTransform(buffer_id: number, split_id?: number | null): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `buffer_id` | `number` | Buffer ID |
| `split_id` | `number | null` (optional) | Optional split ID (uses active split if not specified) |

#### `insertAtCursor`

Insert text at the current cursor position in the active buffer

```typescript
insertAtCursor(text: string): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `text` | `string` | The text to insert |

#### `registerCommand`

Register a custom command that can be triggered by keybindings or the command palette
fileexplorer, menu) and custom plugin-defined contexts (e.g., "normal,config-editor")

```typescript
registerCommand(name: string, description: string, action: string, contexts: string, source: string): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `name` | `string` | Unique command name (e.g., "my_plugin_action") |
| `description` | `string` | Human-readable description |
| `action` | `string` | JavaScript function name to call when command is triggered |
| `contexts` | `string` | Comma-separated list of contexts, including both built-in (normal, prompt, popup, |
| `source` | `string` | Plugin source name (empty string for builtin) |

#### `unregisterCommand`

Unregister a custom command by name

```typescript
unregisterCommand(name: string): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `name` | `string` | The name of the command to unregister |

#### `setContext`

Set or unset a custom context for command visibility
Custom contexts allow plugins to control when their commands are available.
For example, setting "config-editor" context makes config editor commands visible.

```typescript
setContext(name: string, active: boolean): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `name` | `string` | Context name (e.g., "config-editor") |
| `active` | `boolean` | Whether the context is active (true = set, false = unset) |

#### `openFile`

Open a file in the editor, optionally at a specific location

```typescript
openFile(path: string, line: number, column: number): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `path` | `string` | File path to open |
| `line` | `number` | Line number to jump to (0 for no jump) |
| `column` | `number` | Column number to jump to (0 for no jump) |

#### `openFileInSplit`

Open a file in a specific split pane

```typescript
openFileInSplit(split_id: number, path: string, line: number, column: number): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `split_id` | `number` | The split ID to open the file in |
| `path` | `string` | File path to open |
| `line` | `number` | Line number to jump to (0 for no jump) |
| `column` | `number` | Column number to jump to (0 for no jump) |

#### `spawnBackgroundProcess`

Spawn a long-running background process
Unlike spawnProcess which waits for completion, this starts a process
in the background and returns immediately with a process ID.
Use killProcess(id) to terminate the process later.
Use isProcessRunning(id) to check if it's still running.
const proc = await editor.spawnBackgroundProcess("asciinema", ["rec", "output.cast"]);
// Later...
await editor.killProcess(proc.process_id);

```typescript
spawnBackgroundProcess(command: string, args: string[], cwd?: string | null): Promise<BackgroundProcessResult>
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `command` | `string` | Program name (searched in PATH) or absolute path |
| `args` | `string[]` | Command arguments (each array element is one argument) |
| `cwd` | `string | null` (optional) | Working directory; null uses editor's cwd |

**Example:**

```typescript
const proc = await editor.spawnBackgroundProcess("asciinema", ["rec", "output.cast"]);
// Later...
await editor.killProcess(proc.process_id);
```

#### `killProcess`

Kill a background or cancellable process by ID
Sends SIGTERM to gracefully terminate the process.
Returns true if the process was found and killed, false if not found.

```typescript
killProcess(#[bigint] process_id: number): Promise<boolean>
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `#[bigint] process_id` | `number` | - |

#### `spawnProcessWait`

Wait for a cancellable process to complete and get its result

```typescript
spawnProcessWait(#[bigint] process_id: number): Promise<SpawnResult>
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `#[bigint] process_id` | `number` | - |

#### `delay`

Delay execution for a specified number of milliseconds
Useful for debouncing user input or adding delays between operations.
await editor.delay(100);  // Wait 100ms

```typescript
delay(#[bigint] ms: number): Promise<[]>
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `#[bigint] ms` | `number` | - |

**Example:**

```typescript
await editor.delay(100);  // Wait 100ms
```

#### `findBufferByPath`

Find a buffer ID by its file path

```typescript
findBufferByPath(path: string): number
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `path` | `string` | - |

#### `diffLines`

Compute line diff between two strings

```typescript
diffLines(original: string, modified: string): TsLineDiff
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `original` | `string` | - |
| `modified` | `string` | - |

#### `startPromptWithInitial`

Start a prompt with pre-filled initial value

```typescript
startPromptWithInitial(label: string, prompt_type: string, initial_value: string): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `label` | `string` | Label to display (e.g., "Git grep: ") |
| `prompt_type` | `string` | Type identifier (e.g., "git-grep") |
| `initial_value` | `string` | Initial text to pre-fill in the prompt |

#### `sendLspRequest`

Send an arbitrary LSP request and receive the raw JSON response

```typescript
sendLspRequest(language: string, method: string, params?: unknown | null): Promise<unknown>
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `language` | `string` | Language ID (e.g., "cpp") |
| `method` | `string` | Full LSP method (e.g., "textDocument/switchSourceHeader") |
| `params` | `unknown | null` (optional) | Optional request payload |

#### `setSplitScroll`

Set the scroll position of a specific split

```typescript
setSplitScroll(split_id: number, top_byte: number): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `split_id` | `number` | The split ID |
| `top_byte` | `number` | The byte offset of the top visible line |

#### `setSplitRatio`

Set the ratio of a split container

```typescript
setSplitRatio(split_id: number, ratio: number): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `split_id` | `number` | ID of the split |
| `ratio` | `number` | Ratio between 0.0 and 1.0 (0.5 = equal split) |

#### `distributeSplitsEvenly`

Distribute all visible splits evenly
This adjusts the ratios of all container splits so each leaf split gets equal space

```typescript
distributeSplitsEvenly(): boolean
```

#### `setBufferCursor`

Set cursor position in a buffer (also scrolls viewport to show cursor)

```typescript
setBufferCursor(buffer_id: number, position: number): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `buffer_id` | `number` | ID of the buffer |
| `position` | `number` | Byte offset position for the cursor |

#### `executeAction`

Execute a built-in editor action by name
This is used by vi mode plugin to run motions and then check cursor position.
For example, to implement "dw" (delete word), the plugin:
1. Saves current cursor position
2. Calls executeAction("move_word_right") - cursor moves
3. Gets new cursor position
4. Deletes from old to new position

```typescript
executeAction(action_name: string): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `action_name` | `string` | Action name (e.g., "move_word_right", "move_line_end") |

#### `executeActions`

Execute multiple actions in sequence, each with an optional repeat count
Used by vi mode for count prefix (e.g., "3dw" = delete 3 words).
All actions execute atomically with no plugin roundtrips between them.

```typescript
executeActions(actions: ActionSpecJs[]): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `actions` | `ActionSpecJs[]` | Array of {action: string, count?: number} objects |

#### `setEditorMode`

Set the global editor mode (for modal editing like vi mode)
When a mode is set, its keybindings take precedence over normal key handling.
Pass null/undefined to clear the mode and return to normal editing.

```typescript
setEditorMode(mode?: string | null): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `mode` | `string | null` (optional) | Mode name (e.g., "vi-normal") or null to clear |

### Overlay Operations

#### `addOverlay`

Add a colored highlight overlay to text without modifying content
Overlays are visual decorations that persist until explicitly removed.
Add an overlay (visual decoration) to a buffer
Use namespaces for easy batch removal (e.g., "spell", "todo").
Multiple overlays can apply to the same range; colors blend.

```typescript
addOverlay(buffer_id: number, namespace: string, start: number, end: number, r: number, g: number, b: number, bg_r: i16, bg_g: i16, bg_b: i16, underline: boolean, bold: boolean, italic: boolean): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `buffer_id` | `number` | Target buffer ID |
| `namespace` | `string` | Optional namespace for grouping (use clearNamespace for batch removal) |
| `start` | `number` | Start byte offset |
| `end` | `number` | End byte offset |
| `r` | `number` | Red (0-255) |
| `g` | `number` | Green (0-255) |
| `b` | `number` | uffer_id - Target buffer ID |
| `bg_r` | `i16` | - |
| `bg_g` | `i16` | - |
| `bg_b` | `i16` | - |
| `underline` | `boolean` | Add underline decoration |
| `bold` | `boolean` | Use bold text |
| `italic` | `boolean` | Use italic text |

#### `removeOverlay`

Remove a specific overlay by its handle

```typescript
removeOverlay(buffer_id: number, handle: string): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `buffer_id` | `number` | The buffer ID |
| `handle` | `string` | The overlay handle to remove |

#### `clearOverlaysInRange`

Clear all overlays that overlap with a byte range

```typescript
clearOverlaysInRange(buffer_id: number, start: number, end: number): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `buffer_id` | `number` | The buffer ID |
| `start` | `number` | Start byte position (inclusive) |
| `end` | `number` | End byte position (exclusive) |

#### `clearAllOverlays`

Remove all overlays from a buffer

```typescript
clearAllOverlays(buffer_id: number): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `buffer_id` | `number` | The buffer ID |

#### `addVirtualText`

Add virtual text (inline decoration) at a position

```typescript
addVirtualText(buffer_id: number, virtual_text_id: string, position: number, text: string, r: number, g: number, b: number, before: boolean, use_bg: boolean): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `buffer_id` | `number` | The buffer ID |
| `virtual_text_id` | `string` | Unique identifier for this virtual text |
| `position` | `number` | Byte position to insert at |
| `text` | `string` | The virtual text to display |
| `r` | `number` | Red color component (0-255) |
| `g` | `number` | Green color component (0-255) |
| `b` | `number` | uffer_id - The buffer ID |
| `before` | `boolean` | Whether to insert before (true) or after (false) the position |
| `use_bg` | `boolean` | Whether to use the color as background (true) or foreground (false) |

#### `removeVirtualText`

Remove virtual text by ID

```typescript
removeVirtualText(buffer_id: number, virtual_text_id: string): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `buffer_id` | `number` | The buffer ID |
| `virtual_text_id` | `string` | The virtual text ID to remove |

#### `removeVirtualTextsByPrefix`

Remove all virtual texts with IDs starting with a prefix

```typescript
removeVirtualTextsByPrefix(buffer_id: number, prefix: string): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `buffer_id` | `number` | The buffer ID |
| `prefix` | `string` | The prefix to match virtual text IDs against |

#### `clearVirtualTexts`

Remove all virtual texts from a buffer

```typescript
clearVirtualTexts(buffer_id: number): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `buffer_id` | `number` | The buffer ID |

#### `clearVirtualTextNamespace`

Clear all virtual texts in a namespace

```typescript
clearVirtualTextNamespace(buffer_id: number, namespace: string): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `buffer_id` | `number` | The buffer ID |
| `namespace` | `string` | The namespace to clear (e.g., "git-blame") |

#### `refreshLines`

Force a refresh of line display for a buffer

```typescript
refreshLines(buffer_id: number): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `buffer_id` | `number` | The buffer ID |

### File System Operations

#### `readFile`

Read entire file contents as UTF-8 string
Throws if file doesn't exist, isn't readable, or isn't valid UTF-8.
For binary files, this will fail. For large files, consider memory usage.

```typescript
readFile(path: string): Promise<string>
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `path` | `string` | File path (absolute or relative to cwd) |

#### `writeFile`

Write string content to a file, creating or overwriting
Creates parent directories if they don't exist (behavior may vary).
Replaces file contents entirely; use readFile + modify + writeFile for edits.

```typescript
writeFile(path: string, content: string): Promise<[]>
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `path` | `string` | Destination path (absolute or relative to cwd) |
| `content` | `string` | UTF-8 string to write |

#### `fileExists`

Check if a path exists (file, directory, or symlink)
Does not follow symlinks; returns true for broken symlinks.
Use fileStat for more detailed information.

```typescript
fileExists(path: string): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `path` | `string` | Path to check (absolute or relative to cwd) |

#### `fileStat`

Get metadata about a file or directory
Follows symlinks. Returns exists=false for non-existent paths
rather than throwing. Size is in bytes; directories may report 0.

```typescript
fileStat(path: string): FileStat
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `path` | `string` | Path to stat (absolute or relative to cwd) |

#### `readDir`

List directory contents
Returns unsorted entries with type info. Entry names are relative
to the directory (use pathJoin to construct full paths).
Throws on permission errors or if path is not a directory.
const entries = editor.readDir("/home/user");
for (const e of entries) {
const fullPath = editor.pathJoin("/home/user", e.name);
}

```typescript
readDir(path: string): DirEntry[]
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `path` | `string` | Directory path (absolute or relative to cwd) |

**Example:**

```typescript
const entries = editor.readDir("/home/user");
for (const e of entries) {
const fullPath = editor.pathJoin("/home/user", e.name);
}
```

### Environment Operations

#### `getEnv`

Get an environment variable

```typescript
getEnv(name: string): string
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `name` | `string` | Name of environment variable |

#### `getCwd`

Get the editor's current working directory
Returns the editor's working directory (set when the editor was started).
Use as base for resolving relative paths and spawning processes.
Note: This returns the editor's stored working_dir, not process CWD,
which is important for test isolation.

```typescript
getCwd(): string
```

### Path Operations

#### `pathJoin`

Join path segments using the OS path separator
Handles empty segments and normalizes separators.
If a segment is absolute, previous segments are discarded.
pathJoin("/home", "user", "file.txt") // "/home/user/file.txt"
pathJoin("relative", "/absolute") // "/absolute"

```typescript
pathJoin(parts: string[]): string
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `parts` | `string[]` | Path segments to join |

**Example:**

```typescript
pathJoin("/home", "user", "file.txt") // "/home/user/file.txt"
pathJoin("relative", "/absolute") // "/absolute"
```

#### `pathDirname`

Get the parent directory of a path
Returns empty string for root paths or paths without parent.
Does not resolve symlinks or check existence.
pathDirname("/home/user/file.txt") // "/home/user"
pathDirname("/") // ""

```typescript
pathDirname(path: string): string
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `path` | `string` | File or directory path |

**Example:**

```typescript
pathDirname("/home/user/file.txt") // "/home/user"
pathDirname("/") // ""
```

#### `pathBasename`

Get the final component of a path
Returns empty string for root paths.
Does not strip file extension; use pathExtname for that.
pathBasename("/home/user/file.txt") // "file.txt"
pathBasename("/home/user/") // "user"

```typescript
pathBasename(path: string): string
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `path` | `string` | File or directory path |

**Example:**

```typescript
pathBasename("/home/user/file.txt") // "file.txt"
pathBasename("/home/user/") // "user"
```

#### `pathExtname`

Get the file extension including the dot
Returns empty string if no extension. Only returns the last extension
for files like "archive.tar.gz" (returns ".gz").
pathExtname("file.txt") // ".txt"
pathExtname("archive.tar.gz") // ".gz"
pathExtname("Makefile") // ""

```typescript
pathExtname(path: string): string
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `path` | `string` | File path |

**Example:**

```typescript
pathExtname("file.txt") // ".txt"
pathExtname("archive.tar.gz") // ".gz"
pathExtname("Makefile") // ""
```

#### `pathIsAbsolute`

Check if a path is absolute
On Unix: starts with "/". On Windows: starts with drive letter or UNC path.

```typescript
pathIsAbsolute(path: string): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `path` | `string` | Path to check |

### Event/Hook Operations

#### `on`

Subscribe to an editor event
Handler must be a global function name (not a closure).
Multiple handlers can be registered for the same event.
Events: "buffer_save", "cursor_moved", "buffer_modified", etc.
globalThis.onSave = (data) => {
editor.setStatus(`Saved: ${data.path}`);
};
editor.on("buffer_save", "onSave");

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

### Virtual Buffer Operations

#### `createVirtualBufferInSplit`

Create a virtual buffer in a new horizontal split below current pane
Use for results panels, diagnostics, logs, etc. The panel_id enables
idempotent updates: if a panel with that ID exists, its content is replaced
instead of creating a new split. Define the mode with defineMode first.
// First define the mode with keybindings
editor.defineMode("search-results", "special", [
["Return", "search_goto"],
["q", "close_buffer"]
], true);
// Then create the buffer
const id = await editor.createVirtualBufferInSplit({
name: "*Search*",
mode: "search-results",
read_only: true,
entries: [
{ text: "src/main.rs:42: match\n", properties: { file: "src/main.rs", line: 42 } }
],
ratio: 0.3,
panel_id: "search"
});

```typescript
createVirtualBufferInSplit(options: CreateVirtualBufferOptions): Promise<CreateVirtualBufferResult>
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `options` | `CreateVirtualBufferOptions` | Buffer configuration |

**Example:**

```typescript
// First define the mode with keybindings
editor.defineMode("search-results", "special", [
["Return", "search_goto"],
["q", "close_buffer"]
], true);

// Then create the buffer
const id = await editor.createVirtualBufferInSplit({
name: "*Search*",
mode: "search-results",
read_only: true,
entries: [
{ text: "src/main.rs:42: match\n", properties: { file: "src/main.rs", line: 42 } }
],
ratio: 0.3,
panel_id: "search"
});
```

#### `createVirtualBufferInExistingSplit`

Create a virtual buffer in an existing split

```typescript
createVirtualBufferInExistingSplit(options: CreateVirtualBufferInExistingSplitOptions): Promise<number>
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `options` | `CreateVirtualBufferInExistingSplitOptions` | Configuration for the virtual buffer |

#### `createVirtualBuffer`

Create a virtual buffer in the current split as a new tab
This is useful for help panels, documentation, etc. that should open
alongside other buffers rather than in a separate split.

```typescript
createVirtualBuffer(options: CreateVirtualBufferInCurrentSplitOptions): Promise<number>
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `options` | `CreateVirtualBufferInCurrentSplitOptions` | Configuration for the virtual buffer |

#### `defineMode`

Define a buffer mode with keybindings
editor.defineMode("diagnostics-list", "special", [
["Return", "diagnostics_goto"],
["q", "close_buffer"]
], true);

```typescript
defineMode(name: string, parent: string, bindings: Vec<(String, String): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `name` | `string` | Mode name (e.g., "diagnostics-list") |
| `parent` | `string` | Parent mode name for inheritance (e.g., "special"), or null |
| `bindings` | `Vec<(String, String` | Array of [key_string, command_name] pairs |

**Example:**

```typescript
editor.defineMode("diagnostics-list", "special", [
["Return", "diagnostics_goto"],
["q", "close_buffer"]
], true);
```

#### `showBuffer`

Switch the current split to display a buffer

```typescript
showBuffer(buffer_id: number): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `buffer_id` | `number` | ID of the buffer to show |

#### `closeBuffer`

Close a buffer and remove it from all splits

```typescript
closeBuffer(buffer_id: number): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `buffer_id` | `number` | ID of the buffer to close |

#### `focusSplit`

Focus a specific split

```typescript
focusSplit(split_id: number): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `split_id` | `number` | ID of the split to focus |

#### `setSplitBuffer`

Set the buffer displayed in a specific split

```typescript
setSplitBuffer(split_id: number, buffer_id: number): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `split_id` | `number` | ID of the split |
| `buffer_id` | `number` | ID of the buffer to display in the split |

#### `closeSplit`

Close a split (if not the last one)

```typescript
closeSplit(split_id: number): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `split_id` | `number` | ID of the split to close |

#### `getTextPropertiesAtCursor`

Get text properties at the cursor position in a buffer
const props = editor.getTextPropertiesAtCursor(bufferId);
if (props.length > 0 && props[0].location) {
editor.openFile(props[0].location.file, props[0].location.line, 0);
}

```typescript
getTextPropertiesAtCursor(buffer_id: number): Record<string, unknown>[]
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `buffer_id` | `number` | ID of the buffer to query |

**Example:**

```typescript
const props = editor.getTextPropertiesAtCursor(bufferId);
if (props.length > 0 && props[0].location) {
editor.openFile(props[0].location.file, props[0].location.line, 0);
}
```

#### `setVirtualBufferContent`

Set the content of a virtual buffer with text properties

```typescript
setVirtualBufferContent(buffer_id: number, entries: TextPropertyEntry[]): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `buffer_id` | `number` | ID of the virtual buffer |
| `entries` | `TextPropertyEntry[]` | Array of text entries with properties |

