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

### CreateVirtualBufferOptions

Configuration for createVirtualBufferInSplit

```typescript
interface CreateVirtualBufferOptions {
  name: string;
  mode: string;
  read_only: boolean;
  entries: TextPropertyEntry[];
  ratio: number;
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
| `ratio` | Split ratio (0.3 = new pane gets 30% of height) |
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

#### `getActiveBufferId`

Get the buffer ID of the focused editor pane
Returns 0 if no buffer is active (rare edge case).
Use this ID with other buffer operations like getBufferText or insertText.

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
Returns 0 if buffer doesn't exist. Use with getBufferText to read
the full buffer: getBufferText(id, 0, getBufferLength(id)).

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

#### `getBufferText`

Extract text from a buffer by byte range
DEPRECATED: Use the view_transform hook instead, which receives tokens
from core during render. This avoids pulling buffer content and works
with huge files.
Returns empty string - plugins should use streaming transforms.

```typescript
getBufferText(_buffer_id: number, _start: number, _end: number): string
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `_buffer_id` | `number` | - |
| `_start` | `number` | - |
| `_end` | `number` | - |

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

```typescript
registerCommand(name: string, description: string, action: string, contexts: string): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `name` | `string` | Unique command name (e.g., "my_plugin_action") |
| `description` | `string` | Human-readable description |
| `action` | `string` | JavaScript function name to call when command is triggered |
| `contexts` | `string` | Comma-separated list of contexts (e.g., "normal,prompt") |

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

### Async Operations

#### `spawnProcess`

Run an external command and capture its output
Waits for process to complete before returning. For long-running processes,
consider if this will block your plugin. Output is captured completely;
very large outputs may use significant memory.
const result = await editor.spawnProcess("git", ["log", "--oneline", "-5"]);
if (result.exit_code !== 0) {
editor.setStatus(`git failed: ${result.stderr}`);
}

```typescript
spawnProcess(command: string, args: string[], cwd?: string | null): Promise<SpawnResult>
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `command` | `string` | Program name (searched in PATH) or absolute path |
| `args` | `string[]` | Command arguments (each array element is one argument) |
| `cwd` | `string | null` (optional) | Working directory; null uses editor's cwd |

**Example:**

```typescript
const result = await editor.spawnProcess("git", ["log", "--oneline", "-5"]);
if (result.exit_code !== 0) {
editor.setStatus(`git failed: ${result.stderr}`);
}
```

### Overlay Operations

#### `addOverlay`

Add a colored highlight overlay to text without modifying content
Overlays are visual decorations that persist until explicitly removed.
Use prefixed IDs for easy batch removal (e.g., "spell:line42:word3").
Multiple overlays can apply to the same range; colors blend.

```typescript
addOverlay(buffer_id: number, overlay_id: string, start: number, end: number, r: number, g: number, b: number, underline: boolean): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `buffer_id` | `number` | Target buffer ID |
| `overlay_id` | `string` | Unique ID for removal; use prefixes for batching |
| `start` | `number` | Start byte offset |
| `end` | `number` | End byte offset |
| `r` | `number` | Red (0-255) |
| `g` | `number` | Green (0-255) |
| `b` | `number` | uffer_id - Target buffer ID |
| `underline` | `boolean` | Add underline decoration |

#### `removeOverlay`

Remove a specific overlay by ID

```typescript
removeOverlay(buffer_id: number, overlay_id: string): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `buffer_id` | `number` | The buffer ID |
| `overlay_id` | `string` | The overlay ID to remove |

#### `removeOverlaysByPrefix`

Remove all overlays with IDs starting with a prefix

```typescript
removeOverlaysByPrefix(buffer_id: number, prefix: string): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `buffer_id` | `number` | The buffer ID |
| `prefix` | `string` | The prefix to match overlay IDs against |

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
addVirtualText(buffer_id: number, virtual_text_id: string, position: number, text: string, r: number, g: number, b: number, before: boolean): boolean
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
Returns the directory from which the editor was launched.
Use as base for resolving relative paths.

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
createVirtualBufferInSplit(options: CreateVirtualBufferOptions): Promise<number>
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
defineMode(name: string, parent?: string | null, bindings: Vec<(String, String): boolean
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `name` | `string` | Mode name (e.g., "diagnostics-list") |
| `parent` | `string | null` (optional) | Parent mode name for inheritance (e.g., "special"), or null |
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

