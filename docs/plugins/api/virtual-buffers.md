<!-- Generated from the plugin API source. Do not edit: change the doc comments in the Rust source, then run `cargo test -p fresh-plugin-runtime write_fresh_dts_file -- --ignored`. -->

# Virtual Buffers & Panels

Buffers whose content a plugin writes, such as result lists and side panels, and groups of them shown as one tab.

::: v-pre

## Virtual Buffers

### `setLineTargets`

Make lines of a buffer clickable: a click or Enter on a listed line
opens what it points at.

```js
editor.setLineTargets(bufferId, [
  { line: 0, path: "src/main.rs", target: 41, into: "code" },
  { line: 1, path: "src/lib.rs",  target: 12, into: "code" },
]);
```

`line` is the row *in this buffer*; `target` is the line to land on in
the file, both 0-indexed. `into` names a pane by its `setSplitLabel`
label; when it names no live pane the target opens beside this one, so
an index never replaces itself with what you clicked.

The editor owns the behaviour, which is the point: a script that builds
an index — a search result list, an error list, a review map — exits
immediately, and a `mouse_click` handler would die with it. These
targets keep working.

Replaces any previous targets for the buffer; pass `[]` to clear.

```typescript
setLineTargets(bufferId: number, targets: LineTarget[]): boolean;
```

### `createVirtualBuffer`

Create a virtual buffer in current split (async, returns buffer and split IDs)

Without `splitId`, the buffer opens as a new tab in the current split.
This suits help panels, documentation and similar views that should
open alongside other buffers rather than in a separate split.

```typescript
createVirtualBuffer(opts: CreateVirtualBufferOptions): Promise<VirtualBufferResult>;
```

| Parameter | Description |
|-----------|-------------|
| `opts` | Configuration for the virtual buffer |

### `createVirtualBufferInSplit`

Create a virtual buffer in a new split (async, returns buffer and split IDs)

By default the new split is stacked below the current pane. Use it for
results panels, diagnostics, logs and similar. `panelId` makes updates
idempotent: if a panel with that ID already exists, its content is
replaced instead of creating a new split. Define the mode with
`defineMode` first.

`ratio` is the share of the first pane, which is the existing content
unless `before` is set.

```ts
// First define the mode with keybindings
editor.defineMode("search-results", [
  ["Return", "search_goto"],
  ["q", "close_buffer"],
], true);

// Then create the buffer
const { bufferId, splitId } = await editor.createVirtualBufferInSplit({
  name: "*Search*",
  mode: "search-results",
  readOnly: true,
  entries: [
    { text: "src/main.rs:42: match\n", properties: { file: "src/main.rs", line: 42 } },
  ],
  ratio: 0.7, // existing pane keeps 70%, the panel gets 30%
  panelId: "search",
});
```

```typescript
createVirtualBufferInSplit(opts: CreateVirtualBufferInSplitOptions): Promise<VirtualBufferResult>;
```

### `createVirtualBufferInExistingSplit`

Create a virtual buffer in an existing split (async, returns buffer and split IDs)

```typescript
createVirtualBufferInExistingSplit(opts: CreateVirtualBufferInExistingSplitOptions): Promise<VirtualBufferResult>;
```

### `setBufferMode`

Switch a virtual buffer's mode — the keybinding set that applies
while it is focused.

A panel that grows a text field (an in-panel filter) needs the
single-key commands of its normal mode to stop firing while the
user types; giving the panel a text-input mode for the duration
does that without the plugin re-registering bindings. Modes are
declared with `defineMode`; a mode name with no definition falls
back to the global bindings.

```typescript
setBufferMode(bufferId: number, mode: string): boolean;
```

### `setVirtualBufferContent`

Replace a virtual buffer's content with a list of styled **spans**.

Spans are concatenated *verbatim*: they are runs of text, not lines,
and nothing inserts separators for you. `[{text:"a"},{text:"b"}]` is
the single line `ab`; for two lines, write `[{text:"a\n"},{text:"b\n"}]`.
A buffer built without those newlines reports a plausible `length` and
a `lineCount` of 1 — read it back from `getBufferInfo` if it matters,
since otherwise the mistake is only visible on screen.

If you are an agent putting text in front of a human, prefer writing a
file and opening it (`splitWindow({ file })` /
`openFileInSplit(splitId, path)`). A file buffer gives you syntax
highlighting, search, save, and renders ANSI escape codes as colour —
so command output can go straight in. Virtual buffers exist for
plugin-owned panels: ephemeral, styled per span, driven by a mode's
keybindings.

```typescript
setVirtualBufferContent(bufferId: number, entriesArr: Record<string, unknown>[]): boolean;
```

### `getTextPropertiesAtCursor`

Get text properties at cursor position (returns JS array)

Returns the `properties` object of every entry whose text covers the
cursor, or an empty array when none does.

```ts
const props = editor.getTextPropertiesAtCursor(bufferId);
if (props.length > 0 && typeof props[0].file === "string") {
  editor.openFile(props[0].file, props[0].line as number, 0);
}
```

```typescript
getTextPropertiesAtCursor(bufferId: number): TextPropertiesAtCursor;
```

| Parameter | Description |
|-----------|-------------|
| `bufferId` | ID of the buffer to query |

## Buffer Groups

### `setPanelContent`

Set the content of a panel within a buffer group

```typescript
setPanelContent(groupId: number, panelName: string, entriesArr: Record<string, unknown>[]): boolean;
```

### `closeBufferGroup`

Close a buffer group

```typescript
closeBufferGroup(groupId: number): boolean;
```

### `setBufferGroupPanelVisible`

Show or hide one panel of a buffer group, without tearing the
group down.

The panel's buffer, its content and its scroll position all
survive being hidden — only the group's split tree changes, so
the remaining panels take over the freed space and a re-shown
panel comes back where it was. Use it for optional sidebars a
mode wants to toggle (a file list, a comments rail) instead of
closing and recreating the group.

Hiding a panel that holds focus moves focus to a panel that is
still rendered; focusing a hidden panel is a no-op. Returns
`false` if the group or panel is unknown, or if the call would
hide the group's last visible panel.

Queued, like every layout mutation: the returned bool only reports
that the command was sent.

```typescript
setBufferGroupPanelVisible(groupId: number, panelName: string, visible: boolean): boolean;
```

### `focusBufferGroupPanel`

Focus a specific panel within a buffer group

```typescript
focusBufferGroupPanel(groupId: number, panelName: string): boolean;
```

### `setBufferGroupPanelBuffer`

Re-point a buffer-group's panel at a different buffer id.

Streaming plugins (e.g. git-log) allocate one file-backed
buffer per item and call this on navigation to swap which
buffer the panel displays — instead of mutating a single
shared buffer's contents. Resolves with `true` on success.

```typescript
setBufferGroupPanelBuffer(groupId: number, panelName: string, bufferId: number): Promise<boolean>;
```

### `createBufferGroup`

Create a buffer group: multiple panels appearing as one tab.

```typescript
createBufferGroup(name: string, mode: string, layout: unknown): Promise<BufferGroupResult>;
```

## Composite Buffers

### `getCompositeCursorInfo`

Cursor info for the active composite (side-by-side diff) buffer.

Resolves with `null` when the active buffer is not a composite
buffer, otherwise an object describing the focused pane and the
0-indexed source line shown in each pane on the cursor's aligned
row (`null` where a pane has no content on that row). Lets a plugin
map a side-by-side cursor back to a concrete file version + line.

```typescript
getCompositeCursorInfo(): Promise<{
  focusedPane: number;
  paneCount: number;
  lines: Array<number | null>;
} | null>;
```

### `createCompositeBuffer`

Create a composite buffer (async)

A composite buffer displays several source buffers in a single tab/view area
with a custom layout (side-by-side, stacked or unified). This is useful for
diff views, merge conflict resolution, etc.

The options are checked when the call is made; options that don't
match the type (for example a misspelled field name) make the call
throw.

```typescript
createCompositeBuffer(opts: TsCreateCompositeBufferOptions): Promise<number>;
```

| Parameter | Description |
|-----------|-------------|
| `opts` | Configuration for the composite buffer |

### `updateCompositeAlignment`

Update alignment hunks for a composite buffer

The hunks are checked when the call is made; a hunk that doesn't match
the type (for example one with a misspelled field name) makes the call
throw.

```typescript
updateCompositeAlignment(bufferId: number, hunks: TsCompositeHunk[]): boolean;
```

| Parameter | Description |
|-----------|-------------|
| `bufferId` | The composite buffer ID |
| `hunks` | New diff hunks for alignment |

### `closeCompositeBuffer`

Close a composite buffer

```typescript
closeCompositeBuffer(bufferId: number): boolean;
```

### `flushLayout`

Force-materialize render-dependent state (like `layoutIfNeeded` in UIKit).
After calling this, commands that depend on view state created during
rendering (e.g., `compositeNextHunk`) will work correctly.

```typescript
flushLayout(): boolean;
```

### `setCompositeCursorLine`

Put a composite buffer's cursor on the row showing `line`
(0-indexed) of pane `pane` — 0 is the left/OLD pane — and scroll
it into view.

`initialFocusHunk` on `createCompositeBuffer` lands the view on a
hunk; this lands it on a *line*, which is what a plugin holding a
concrete file position wants (following a review comment, or
keeping the reader's place when a diff view flips between its
unified and side-by-side layouts). No-op if that pane has no such
line.

Queued, like every layout mutation: the returned bool only reports
that the command was sent.

```typescript
setCompositeCursorLine(bufferId: number, pane: number, line: number): boolean;
```

### `compositeNextHunk`

Navigate to the next hunk in a composite buffer

```typescript
compositeNextHunk(bufferId: number): boolean;
```

### `compositePrevHunk`

Navigate to the previous hunk in a composite buffer

```typescript
compositePrevHunk(bufferId: number): boolean;
```

## Scroll Sync

### `createScrollSyncGroup`

Create a scroll sync group for anchor-based synchronized scrolling

Used for side-by-side diff views where two panes need to scroll
together. The plugin provides the group ID, which must be unique per
plugin. Groups are removed automatically when the plugin unloads.

```typescript
createScrollSyncGroup(groupId: number, leftSplit: number, rightSplit: number): boolean;
```

| Parameter | Description |
|-----------|-------------|
| `groupId` | Plugin-assigned group ID |
| `leftSplit` | The left (primary) split; scroll is tracked in its lines |
| `rightSplit` | The right (secondary) split; follows via the anchors |

### `setScrollSyncAnchors`

Set sync anchors for a scroll sync group

Anchors map corresponding line numbers between the left and right
buffers. Each anchor is a `[leftLine, rightLine]` pair. Entries with
fewer than two numbers are ignored.

```typescript
setScrollSyncAnchors(groupId: number, anchors: number[][]): boolean;
```

| Parameter | Description |
|-----------|-------------|
| `groupId` | The group ID passed to `createScrollSyncGroup` |
| `anchors` | `[leftLine, rightLine]` pairs marking matching positions |

### `removeScrollSyncGroup`

Remove a scroll sync group

```typescript
removeScrollSyncGroup(groupId: number): boolean;
```

## View State

### `setViewState`

Set plugin-managed per-buffer view state, as seen in the active split.
`getViewState` returns the new value straight away; `null` or
`undefined` deletes the key. For a buffer backed by a file, the state
is saved with the workspace and comes back when it is restored.

```typescript
setViewState(bufferId: number, key: string, value: unknown): boolean;
```

### `getViewState`

Get plugin-managed per-buffer view state, as set by `setViewState`.
`undefined` if missing.

```typescript
getViewState(bufferId: number, key: string): unknown;
```

:::
