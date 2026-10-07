<!-- Generated from the plugin API source. Do not edit: change the doc comments in the Rust source, then run `cargo test -p fresh-plugin-runtime write_fresh_dts_file -- --ignored`. -->

# Buffers & Editing

Read and change buffer text, move cursors, open, save and close files, and search.

::: v-pre

## Buffer Queries

### `getActiveBufferId`

Get the active buffer ID (0 if none)
This is the buffer in the focused editor pane. Use the ID with other
buffer operations such as insertText.

```typescript
getActiveBufferId(): number;
```

### `listBuffers`

List all open buffers - returns array of BufferInfo objects

```typescript
listBuffers(): BufferInfo[];
```

### `getBufferPath`

Get file path for a buffer
Returns an empty string for unsaved buffers, virtual buffers, or an
unknown buffer ID. Use the path to determine file type, construct related
paths, or display to the user.

```typescript
getBufferPath(bufferId: number): string;
```

### `getBufferLength`

Get buffer length in bytes
Returns 0 if the buffer doesn't exist.

```typescript
getBufferLength(bufferId: number): number;
```

### `isBufferModified`

Check if buffer has unsaved changes
Returns false if the buffer doesn't exist. Setting a virtual buffer's
content does not mark it modified.

```typescript
isBufferModified(bufferId: number): boolean;
```

### `getBufferInfo`

Get buffer info by ID
Returns null if the buffer doesn't exist.

```typescript
getBufferInfo(bufferId: number): BufferInfo | null;
```

### `getLineStartPosition`

Get the byte offset of the start of a line (0-indexed line number)
Returns null if the line number is out of range

```typescript
getLineStartPosition(line: number): Promise<number | null>;
```

### `getLineEndPosition`

Get the byte offset of the end of a line (0-indexed line number)
Returns the position after the last character of the line (before newline)
Returns null if the line number is out of range

```typescript
getLineEndPosition(line: number): Promise<number | null>;
```

### `getBufferLineCount`

Get the total number of lines in the active buffer
Returns null if buffer not found

```typescript
getBufferLineCount(): Promise<number | null>;
```

### `findBufferByPath`

Find buffer by file path, returns buffer ID or 0 if not found

```typescript
findBufferByPath(path: string): number;
```

### `getBufferSavedDiff`

Get diff between buffer content and last saved version

```typescript
getBufferSavedDiff(bufferId: number): BufferSavedDiff | null;
```

### `getBufferText`

Read buffer text.

`getBufferText(id)` returns the **whole buffer** — the common case,
and the one that used to require reading `length` from
`getBufferInfo` and passing it back. `getBufferText(id, start, end)`
still reads a byte range, so existing callers are unaffected.

Byte offsets, not character or line offsets.

```typescript
getBufferText(bufferId: number, start?: number, end?: number): Promise<string>;
```

## Cursors & Viewport

### `getCursorPosition`

Get cursor position in active buffer
The position is a byte offset, not a character index. Returns 0 if there
is no cursor. For multiple cursors, use `getAllCursors`.

```typescript
getCursorPosition(): number;
```

### `getPrimaryCursor`

Get primary cursor info for active buffer
The result includes the cursor's selection, if any. Returns null if there
is no active cursor.

```typescript
getPrimaryCursor(): CursorInfo | null;
```

### `getAllCursors`

Get all cursors for active buffer

```typescript
getAllCursors(): CursorInfo[];
```

### `getAllCursorPositions`

Get all cursor positions as byte offsets

Returns an empty array if there are no cursors. For selection info use
`getAllCursors` instead.

```typescript
getAllCursorPositions(): number[];
```

### `getViewport`

Get viewport info for active buffer

```typescript
getViewport(): ViewportInfo | null;
```

### `getCursorLine`

Get the line number (0-indexed) of the primary cursor.

@deprecated Use `getPrimaryCursor()?.line` instead. This accessor cannot
represent "line index unavailable" (huge files before their line scan) —
it returns `0` in that case, indistinguishable from a real first line.
`getPrimaryCursor().line` is `number | null` and also covers every cursor
via `getAllCursors()`.

```typescript
getCursorLine(): number;
```

### `scrollToLineCenter`

Scroll a split to center a specific line in the viewport
Line is 0-indexed (0 = first line)

```typescript
scrollToLineCenter(splitId: number, bufferId: number, line: number): boolean;
```

### `scrollBufferToLine`

Scroll any split/panel showing `buffer_id` so `line` is visible.
Unlike `scrollToLineCenter`, this does not require a split id — it
updates every split's viewport whose active buffer is the given
buffer, including inner leaves of a buffer group. Use this from
a panel plugin to keep the user's "selected" row in view after
arrow-key navigation (the plugin's own selection state isn't
automatically reflected in the buffer cursor, so the core-driven
viewport would otherwise stay put).

```typescript
scrollBufferToLine(bufferId: number, line: number): boolean;
```

### `setBufferCursor`

Set cursor position in a buffer

The cursor moves in every split showing the buffer, and each viewport
scrolls to keep it visible.

```typescript
setBufferCursor(bufferId: number, position: number): boolean;
```

| Parameter | Description |
|-----------|-------------|
| `position` | Byte offset position for the cursor |

### `setBufferShowCursors`

Toggle whether the editor draws a native caret in this buffer.

Buffer-group panel buffers default to `show_cursors = false`, which
also blocks all native movement actions in `action_to_events`. Plugins
that want native cursor motion in a panel (e.g. magit-style row
navigation) call this with `true` after `createBufferGroup` returns.

```typescript
setBufferShowCursors(bufferId: number, show: boolean): boolean;
```

## Text Editing

### `insertText`

Insert text at a byte position in a buffer.

The text is inserted before the byte at `position`, and all text after it
shifts. The operation is asynchronous: the return value is true if the
command was sent, not that the edit was applied.

```typescript
insertText(bufferId: number, position: number, text: string): boolean;
```

| Parameter | Description |
|-----------|-------------|
| `position` | Byte offset where text will be inserted (0 to buffer length, at a UTF-8 char boundary) |
| `text` | UTF-8 text to insert |

### `deleteRange`

Delete a byte range from a buffer.

Both positions must be at valid UTF-8 char boundaries. The operation is
asynchronous: the return value is true if the command was sent, not that
the edit was applied.

```typescript
deleteRange(bufferId: number, start: number, end: number): boolean;
```

| Parameter | Description |
|-----------|-------------|
| `start` | Start byte offset (inclusive) |
| `end` | End byte offset (exclusive) |

### `insertAtCursor`

Insert text at cursor position in active buffer

```typescript
insertAtCursor(text: string): boolean;
```

## Opening, Saving & Closing

### `saveBufferToPath`

Save a buffer to a specific file path
Used by :w filename to save unnamed buffers or save-as

```typescript
saveBufferToPath(bufferId: number, path: string): boolean;
```

### `openFile`

Open a file, optionally at a specific line/column.

`editor.openFile(path)` is the whole request most of the time.

```typescript
openFile(path: string, line?: number | null, column?: number | null): boolean;
```

| Parameter | Description |
|-----------|-------------|
| `path` | File path to open |
| `line` | 1-based line number to jump to; omit or pass null for no jump |
| `column` | 1-based column (a byte offset within the line) to jump to; omit or pass null for no jump |

### `openFileInBackground`

Open a file in the background — no focus change, no
active-split mutation. `windowId` defaults to the active
session. Setting it to an inactive session id loads the
file's buffer and adds it as a tab in that session's
stashed split tree, ready to be revealed on next dive.
Orchestrator uses this to populate worktree sessions with
preselected files.

Pairs with `createTerminal`'s `windowId` for setting up an inactive
session's contents without diving.

```typescript
openFileInBackground(path: string, windowId?: number): boolean;
```

### `openFileInSplit`

Open a file in a specific split

```typescript
openFileInSplit(splitId: number, path: string, line?: number, column?: number): boolean;
```

| Parameter | Description |
|-----------|-------------|
| `splitId` | The split ID to open the file in |
| `path` | File path to open |
| `line` | 1-based line number to jump to; defaults to the first line |
| `column` | 1-based column (a byte offset within the line) to jump to; defaults to the first column |

### `previewFileInSplit`

Preview a file in a specific split, as the editor's single
*preview* (ephemeral) tab — what the File Explorer does on a
single click, pointed at a split you name.

Use this instead of `openFileInSplit` while the user is *browsing*
a list of locations — search results, references, diagnostics — and
call it again as the selection moves. The previous preview is
replaced rather than piling up as tabs, a file the user already had
open is switched to and never demoted to a preview, and the buffer
becomes a permanent tab as soon as they commit to it (open it,
edit it, or move focus to another split). Focus does not move, so
the panel or prompt driving the browse keeps the keys.

`line` / `column` are 1-indexed and optional. Returns `false` only
when the editor can no longer take commands (for example while it
shuts down); a file that cannot be previewed
(unreadable, or large enough that loading it would have to ask the
user about its encoding) is skipped quietly on the editor side —
a browse never raises a dialog. Pair with `dismissPreview` when the
browse ends without a choice.

```typescript
previewFileInSplit(splitId: number, path: string, line?: number, column?: number): boolean;
```

### `dismissPreview`

Drop the preview tab opened by `previewFileInSplit`, if it is still
the preview — the browse ended without a choice (the user cancelled
the prompt), so the split goes back to what it was showing.

A preview the user edited is kept and promoted to a permanent tab:
their typing was the commitment. Safe to call when there is no
preview.

```typescript
dismissPreview(): boolean;
```

### `openFileStreaming`

Open `path` as a regular buffer in forced large-file (file-backed)
mode. The file is created (empty) if missing — designed for
buffers that will be filled by a concurrent `spawnProcess` with
`stdoutTo`. Resolves with the new buffer's id, or `null` on
failure.

Pair with `refreshBufferFromDisk` to grow the buffer as the
streaming write advances.

```typescript
openFileStreaming(path: string): Promise<number | null>;
```

### `refreshBufferFromDisk`

Re-stat the file backing `bufferId` and extend the buffer if the
file has grown. Resolves with the new total byte length, or
`null` if the buffer has no file path or doesn't exist.

Used to drive a streaming display: while a `spawnProcess` writes
to a temp file, the plugin polls this on a timer so the buffer
length tracks the file length.

```typescript
refreshBufferFromDisk(bufferId: number): Promise<number | null>;
```

### `showBuffer`

Show a buffer in the current split

```typescript
showBuffer(bufferId: number): boolean;
```

### `closeBuffer`

Close a buffer. Pass `force: true` to discard unsaved changes.

A closed buffer is removed from all splits that show it.

**A modified buffer is not closed** unless `force` is set — the user's
unsaved edits are not a plugin's to throw away. A scratch buffer the
plugin created and filled itself counts as modified, so disposing of
one needs `closeBuffer(id, true)`.

The returned boolean is **"the request was delivered"**, not "the
buffer closed": this call is fire-and-forget, and the editor decides
afterwards. A refusal is logged editor-side but is invisible here, so
confirm with `listBuffers()` (after `await editor.flush()`) when it
matters. Without `force` the sequence that used to be required was
delete-the-contents, `saveBufferToPath`, then close — three
round-trips, the first two of which returned `true` while achieving
nothing.

```typescript
closeBuffer(bufferId: number, force?: boolean | null): boolean;
```

### `closeOtherBuffersInSplit`

Close other buffers in split

```typescript
closeOtherBuffersInSplit(bufferId: number, splitId: number): boolean;
```

### `closeAllBuffersInSplit`

Close all buffers in split

```typescript
closeAllBuffersInSplit(splitId: number): boolean;
```

### `closeBuffersToRightInSplit`

Close buffers to right in split

```typescript
closeBuffersToRightInSplit(bufferId: number, splitId: number): boolean;
```

### `closeBuffersToLeftInSplit`

Close buffers to left in split

```typescript
closeBuffersToLeftInSplit(bufferId: number, splitId: number): boolean;
```

### `moveTabToLeft`

Move the active tab to the left in the active split

```typescript
moveTabToLeft(): boolean;
```

### `moveTabToRight`

Move the active tab to the right in the active split

```typescript
moveTabToRight(): boolean;
```

### `markFileReadOnly`

Mark the buffer backing `path` read-only. Race-free right after
`openFile` because both are FIFO commands.

```typescript
markFileReadOnly(path: string): boolean;
```

## Clipboard

### `copyToClipboard`

```typescript
copyToClipboard(text: string): void;
```

### `setClipboard`

Copy text to the clipboard.
Copies the text to both the internal and the system clipboard. The system
copy uses OSC 52 and arboard, as enabled in the clipboard settings.

```typescript
setClipboard(text: string): void;
```

## Search & Replace

### `hasActiveSearch`

Returns true when search highlights are currently active in the buffer.
Becomes true after a search is confirmed; false once cleared.

```typescript
hasActiveSearch(): boolean;
```

### `grepProject`

Project-wide grep search (async)
Searches all files in the project, respecting .gitignore.
Open buffers with dirty edits are searched in-memory.

```typescript
grepProject(pattern: string, fixedString: boolean | null, caseSensitive: boolean | null, maxResults: number | null, wholeWords: boolean | null): Promise<GrepMatch[]>;
```

### `beginSearch`

Begin a streaming project-wide search and return a `SearchHandle`.
The producer (host) writes matches at full speed into shared state;
the consumer drains via `handle.take()` at its own cadence. Call
`handle.cancel()` to abort.

```typescript
beginSearch(pattern: string, opts?: {
  fixedString?: boolean;
  caseSensitive?: boolean;
  maxResults?: number;
  wholeWords?: boolean;
  sourceBufferId?: number;
  fileGlob?: string;
}): SearchHandle;
```

### `replaceInFile`

Replace matches in a file's buffer (async)
Opens the file if not already in a buffer, applies edits via the buffer model,
and saves. All edits are grouped as a single undo action.

Pass `regex` — the search the matches came from — to treat
`replacement` as a template: `$1`, `${name}` and `\n` are expanded
per match. Without it, `replacement` is written as is.

```typescript
replaceInFile(filePath: string, matches: number[][], replacement: string, bufferId?: number, regex?: {
  pattern: string;
  caseSensitive?: boolean;
  wholeWords?: boolean;
}): Promise<ReplaceResult>;
```

## Diff Baselines

### `registerDiffBaseline`

Register a diff baseline for a buffer (async). `kind` is one of
"saved" | "disk" | "gitRef" | "gitIndex"; `gitRef` carries the ref
for kind "gitRef". Resolves with the baseline id once the
reference content is loaded host-side — no file content ever
crosses the plugin bridge. Baselines are dropped automatically
when their buffer closes, or explicitly via
`releaseDiffBaseline`.

```typescript
registerDiffBaseline(bufferId: number, kind: string, gitRef: string | null): Promise<number>;
```

### `diffAgainstBaseline`

Diff a buffer's live content against a registered baseline
(async). Resolves with a `DiffBaselineResult`; check its
`revision` against the buffer's current version before anchoring
decorations on the hunks.

```typescript
diffAgainstBaseline(bufferId: number, baselineId: number): Promise<DiffBaselineResult>;
```

### `diffBaselinePair`

Diff two registered baselines against each other (async) — e.g.
disk vs HEAD, the git-gutter comparison. Resolves with a
`DiffBaselineResult` whose `revision` is 0.

```typescript
diffBaselinePair(oldBaselineId: number, newBaselineId: number): Promise<DiffBaselineResult>;
```

### `getBaselineLines`

Fetch baseline lines for `(startLine, count)` ranges in one
batched call (async). Lines come back without trailing newlines,
grouped per requested range — fetch only the old-side lines a
diff view actually renders.

```typescript
getBaselineLines(baselineId: number, ranges: number[][]): Promise<string[][]>;
```

### `refreshDiffBaseline`

Reload a baseline's reference content (async; call after a HEAD
move or an external write). Resolves once the fresh content is
serving.

```typescript
refreshDiffBaseline(baselineId: number): Promise<void>;
```

### `releaseDiffBaseline`

Drop a registered diff baseline.

```typescript
releaseDiffBaseline(baselineId: number): void;
```

:::
