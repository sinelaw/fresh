<!-- Generated from the plugin API source. Do not edit: change the doc comments in the Rust source, then run `cargo test -p fresh-plugin-runtime write_fresh_dts_file -- --ignored`. -->

# Decorations

Change how buffer text looks without changing the text: colours, virtual text, hidden ranges, folds, gutter marks and more.

::: v-pre

## Overlays

### `addOverlay`

Add an overlay with styling options

Colors can be specified as RGB arrays `[r, g, b]` or theme key strings.
Theme keys are resolved at render time, so overlays update with theme changes.

Theme key examples: "ui.status_bar_fg", "editor.selection_bg", "syntax.keyword"

Options: fg, bg (RGB array or theme key string), bold, italic, underline,
strikethrough, extend_to_line_end (all booleans, default false).

Example usage in TypeScript:
```typescript
editor.addOverlay(bufferId, "my-namespace", 0, 10, {
Add an overlay with styling options

Colors can be specified as RGB arrays `[r, g, b]` or theme key strings.
Theme keys are resolved at render time, so overlays update with theme changes.

Theme key examples: "ui.status_bar_fg", "editor.selection_bg", "syntax.keyword"

Options: fg, bg (RGB array or theme key string), bold, italic, underline,
strikethrough, extendToLineEnd (all booleans, default false).

Overlays persist until removed. Use a namespace (e.g. "spell", "todo") to
remove a group at once with `clearNamespace`. Several overlays can cover the
same range.

Example usage in TypeScript:
```ts
editor.addOverlay(bufferId, "my-namespace", 0, 10, &#123;
  fg: "syntax.keyword",           // theme key
  bg: [40, 40, 50],               // RGB array
  bold: true,
  strikethrough: true,
});
```

@param bufferId - Target buffer ID
@param namespace - Namespace for grouping (use clearNamespace for batch removal)
@param start - Start byte offset
@param end - End byte offset

```typescript
addOverlay(bufferId: number, namespace: string, start: number, end: number, options: Record<string, unknown>): boolean;
```

### `setCursorLineOverlay`

Declare a one-line overlay that follows this buffer's cursor.

Takes the same options as `addOverlay` and paints the same way — the
difference is who places it. The host re-derives the range from the
cursor while drawing each frame, so the bar marks the row the caret
is on in that very frame. Painting it by hand from `cursor_moved`
cannot: the hook fires after the move that already drew, so the bar
lands a frame late and visibly trails a held arrow key.

Pass `null` to withdraw it.

```typescript
editor.setCursorLineOverlay(bufferId, {
  bg: "editor.selection_bg",
  extendToLineEnd: true,
});
```

```typescript
setCursorLineOverlay(bufferId: number, options: unknown): boolean;
```

### `clearNamespace`

Clear all overlays in a namespace

```typescript
clearNamespace(bufferId: number, namespace: string): boolean;
```

### `clearAllOverlays`

Clear all overlays from a buffer

```typescript
clearAllOverlays(bufferId: number): boolean;
```

### `clearOverlaysInRange`

Clear all overlays that overlap with a byte range

```typescript
clearOverlaysInRange(bufferId: number, start: number, end: number): boolean;
```

| Parameter | Description |
|-----------|-------------|
| `start` | Start byte position (inclusive) |
| `end` | End byte position (exclusive) |

### `clearOverlaysInRangeForNamespace`

Clear overlays in a namespace that overlap with a byte range

```typescript
clearOverlaysInRangeForNamespace(bufferId: number, namespace: string, start: number, end: number): boolean;
```

### `removeOverlay`

Remove an overlay by its handle

```typescript
removeOverlay(bufferId: number, handle: string): boolean;
```

## Virtual Text

### `addVirtualText`

Add virtual text (inline text that doesn't exist in the buffer)

```typescript
addVirtualText(bufferId: number, virtualTextId: string, position: number, text: string, r: number, g: number, b: number, before: boolean, useBg: boolean): boolean;
```

| Parameter | Description |
|-----------|-------------|
| `virtualTextId` | Unique identifier for this virtual text |
| `position` | Byte position to insert at |
| `r` | Red color component (0-255) |
| `g` | Green color component (0-255) |
| `b` | Blue color component (0-255) |
| `before` | Whether to insert before (true) or after (false) the position |
| `useBg` | Whether to use the color as background (true) or foreground (false) |

### `removeVirtualText`

Remove a virtual text by ID

```typescript
removeVirtualText(bufferId: number, virtualTextId: string): boolean;
```

### `addVirtualTextStyled`

Add styled virtual text — richer form of `addVirtualText` whose
`options` accepts an `addOverlay`-style record: `fg`/`bg` may
be RGB arrays or theme-key strings, plus `bold`/`italic`. Theme
keys are resolved at render time so the label follows theme
changes live.

`options.padToColumn` (number) pads the text so it *ends* at that
column of the row, instead of the usual single space of inlay
padding — use it for decoration that has to hold a column, such as
the right edge of a box drawn around a block. The padding is
measured as the row is laid out, so it holds the column even for
the frames between an edit and the `lines_changed` that reports it;
a width you compute here cannot, since your view of the buffer
always trails the one being drawn.

```typescript
addVirtualTextStyled(bufferId: number, virtualTextId: string, position: number, text: string, options: Record<string, unknown>, before: boolean): boolean;
```

### `removeVirtualTextsByPrefix`

Remove virtual texts whose ID starts with the given prefix

```typescript
removeVirtualTextsByPrefix(bufferId: number, prefix: string): boolean;
```

### `clearVirtualTexts`

Clear all virtual texts from a buffer

```typescript
clearVirtualTexts(bufferId: number): boolean;
```

### `clearVirtualTextNamespace`

Clear all virtual texts in a namespace

```typescript
clearVirtualTextNamespace(bufferId: number, namespace: string): boolean;
```

| Parameter | Description |
|-----------|-------------|
| `namespace` | The namespace to clear (e.g., "git-blame") |

### `clearVirtualLinesInRange`

Clear virtual lines in a namespace whose anchor byte falls in
`[start, end)`. The per-line analogue of `clearConcealsInRange`, so a
plugin can rebuild one line's virtual lines without nuking the namespace.

```typescript
clearVirtualLinesInRange(bufferId: number, namespace: string, start: number, end: number): boolean;
```

### `clearVirtualTextsInRange`

Clear *inline* virtual texts whose id starts with `idPrefix` and whose
anchor byte falls in `[start, end)`. The inline analogue of
`clearVirtualLinesInRange`, so a per-line pass can rebuild one line's
inline decorations without dropping the rest of the set.

```typescript
clearVirtualTextsInRange(bufferId: number, idPrefix: string, start: number, end: number): boolean;
```

### `addVirtualLine`

Add a virtual line (full line above/below a position)

The `options` object accepts:
  * `fg`, `bg` — either an `[r, g, b]` array (each `0..=255`) or a
    theme-key string (e.g. `"editor.line_number_fg"`).  Theme keys
    are resolved at render time so the line follows theme changes.
    Both default to `null` (no foreground / transparent background).
  * `gutterGlyph` — optional single character (any short string)
    rendered in the line-number column on this virtual line's
    first visual row. Use to mark e.g. a deletion line with "-"
    so the indicator sits next to the deleted content instead
    of on the following source line.
  * `gutterColor` — color for `gutterGlyph`, same shape as
    `fg`/`bg`. Falls back to the theme's line-number fg.

```typescript
addVirtualLine(bufferId: number, position: number, text: string, options: Record<string, unknown>, above: boolean, namespace: string, priority: number): boolean;
```

| Parameter | Description |
|-----------|-------------|
| `position` | Byte position to anchor the virtual line to |
| `above` | Whether to insert above (true) or below (false) the line |
| `namespace` | Namespace for bulk removal (e.g., "git-blame") |
| `priority` | Priority for ordering multiple lines at the same position (higher comes later) |

### `refreshLines`

Force refresh of line display

```typescript
refreshLines(bufferId: number): boolean;
```

## Conceals

### `addConceal`

Add a conceal range that hides or replaces a byte range during rendering.

`activation` optionally makes the conceal cursor-dependent:
`"unless-cursor-in"` (active only while no cursor is inside
`[scopeStart, scopeEnd)`) or `"if-cursor-in"` (active only while a
cursor IS inside it). Emitting both renderings of a
cursor-revealable decoration once — instead of rebuilding markers
on every cursor move — is what keeps cursor movement free of
marker churn (and of the cache invalidation it causes).

```typescript
addConceal(bufferId: number, namespace: string, start: number, end: number, replacement: string | null, activation?: string, scopeStart?: number, scopeEnd?: number): boolean;
```

### `clearConcealNamespace`

Clear all conceal ranges in a namespace

```typescript
clearConcealNamespace(bufferId: number, namespace: string): boolean;
```

### `clearConcealsInRange`

Clear all conceal ranges that overlap with a byte range

```typescript
clearConcealsInRange(bufferId: number, start: number, end: number): boolean;
```

### `clearConcealsInRangeForNamespace`

Clear conceal ranges overlapping a byte range, restricted to one
namespace — other plugins' conceals in the range are untouched.

```typescript
clearConcealsInRangeForNamespace(bufferId: number, namespace: string, start: number, end: number): boolean;
```

## Soft Breaks & Layout Hints

### `addSoftBreak`

Add a soft break point for marker-based line wrapping.

`activation` optionally makes the break cursor-dependent — same
semantics as `addConceal`'s activation parameters.

`prefix` optionally draws a glyph run at the head of the continuation
row, shaped `{ text, fg?, bg?, bold?, italic? }` with the same colour
spec `addOverlay` takes (a theme key string or an `[r, g, b]` array).
It is drawn *inside* the `indent` columns rather than in addition to
them, so a wrapped block quote can keep its `▌` down every row without
shifting the text. `indent` grows to fit a prefix wider than it.

```typescript
addSoftBreak(bufferId: number, namespace: string, position: number, indent: number, activation?: string | null, scopeStart?: number | null, scopeEnd?: number | null, prefix?: Record<string, unknown> | null): boolean;
```

### `clearSoftBreakNamespace`

Clear all soft breaks in a namespace

```typescript
clearSoftBreakNamespace(bufferId: number, namespace: string): boolean;
```

### `clearSoftBreaksInRange`

Clear all soft breaks that fall within a byte range

```typescript
clearSoftBreaksInRange(bufferId: number, start: number, end: number): boolean;
```

### `setLayoutHints`

Set layout hints (compose width, column guides) for a buffer/split
directly.

```typescript
setLayoutHints(bufferId: number, splitId: number | null, hints: LayoutHints): boolean;
```

## Folds

### `addFold`

Add a collapsed fold range. Hides bytes [start, end) from
rendering — the line containing `start - 1` (the fold "header")
stays visible, while subsequent lines covered by the range are
skipped.

```typescript
addFold(bufferId: number, start: number, end: number, placeholder?: string): boolean;
```

### `clearFolds`

Clear every collapsed fold range on the buffer.

```typescript
clearFolds(bufferId: number): boolean;
```

### `setFoldingRanges`

Publish a set of toggleable fold ranges on the buffer. Same
shape an LSP `foldingRange` response would take. Unlike
`addFold`, this does *not* pre-collapse anything — the
standard fold-toggle keybinding finds the range under the
cursor and collapses or expands it on demand. Replacing call
replaces the prior set.

`ranges` is a JS array of objects shaped
`{ startLine, endLine, kind? }` (lines are 0-indexed).
`kind` is one of `"comment"`, `"imports"`, `"region"` per
the LSP spec; omitted/unknown values are accepted as plain
folds.

```typescript
setFoldingRanges(bufferId: number, rangesArr: Record<string, unknown>[]): boolean;
```

## Gutter & Line Display

### `setBufferDiffGutter`

Show old/new diff line numbers in a composed diff stream's gutter,
derived by the host from the stream's `@@` headers.

```typescript
setBufferDiffGutter(bufferId: number, enabled: boolean): boolean;
```

### `setLineIndicator`

Set a line indicator in the gutter

The symbol is drawn in the gutter's indicator column. When several
indicators land on the same line, the one with the highest priority wins.
Indicator namespaces are cleared automatically when the plugin unloads.

```typescript
setLineIndicator(bufferId: number, line: number, namespace: string, symbol: string, r: number, g: number, b: number, priority: number): boolean;
```

| Parameter | Description |
|-----------|-------------|
| `line` | Line number (0-indexed) |
| `namespace` | Namespace for grouping (e.g., "git-gutter", "breakpoints") |
| `symbol` | Symbol to display (e.g., "│", "●", "★") |
| `r` | Red color component (0-255) |
| `g` | Green color component (0-255) |
| `b` | Blue color component (0-255) |
| `priority` | Priority when multiple indicators exist (higher wins) |

### `setLineIndicators`

Batch set line indicators in the gutter

```typescript
setLineIndicators(bufferId: number, lines: number[], namespace: string, symbol: string, r: number, g: number, b: number, priority: number): boolean;
```

### `clearLineIndicators`

Clear line indicators in a namespace

Removes all of this namespace's indicators from the buffer.

```typescript
clearLineIndicators(bufferId: number, namespace: string): boolean;
```

| Parameter | Description |
|-----------|-------------|
| `namespace` | Namespace to clear (e.g., "git-gutter") |

### `setLineNumbers`

Show or hide line numbers for a buffer **on the user's behalf**.

This records the same explicit per-buffer pin as "Toggle Line Numbers
(Current Buffer)": it beats any mode default, and is persisted with the
rest of the per-file workspace state. Use it for a setting the user
asked for — vi's `:set number` / `:set nonumber` are exactly that, a
typed command that happens to arrive through a plugin.

A mode stating its own preference for the buffers it has taken over
wants `setLineNumbersDefault` instead: re-asserting the pin from a
`buffer_activated` handler overwrites whatever the user chose
(issue #2931).

The return value says the command was queued, not that the gutter ended
up visible.

```typescript
setLineNumbers(bufferId: number, enabled: boolean): boolean;
```

| Parameter | Description |
|-----------|-------------|
| `enabled` | Whether to show line numbers |

### `setLineNumbersDefault`

Set this plugin's line-number *default* for a buffer, the way
`setFoldIndicators` does for the gutter's fold arrows.

Three settings decide whether the gutter shows line numbers, in this
order:

1. the user's per-buffer pin ("Toggle Line Numbers (Current Buffer)",
   or `setLineNumbers`);
2. this plugin default;
3. the global `editor.line_numbers` setting.

Pass `null` to withdraw the plugin's opinion and fall back to the
user's own setting. The plugin's value is stored separately from that
setting and is never persisted, so it can neither overwrite a
deliberate choice — "Toggle Line Numbers (Current Buffer)" and
`setLineNumbers` still win while this is set — nor leak into the saved
session. No save/restore is needed on the way out, because the user's
setting is untouched. A mode that hides the gutter should still clear
its value on the way out.

```typescript
setLineNumbersDefault(bufferId: number, enabled: boolean | null): boolean;
```

| Parameter | Description |
|-----------|-------------|
| `enabled` | The mode's default, or `null` to withdraw it |

### `setFoldIndicators`

Show or hide the gutter's fold indicators (`▾` / `▸`) for a buffer in
the active split, the way `setLineNumbers` does for line numbers.

Pass `null` to withdraw the plugin's opinion and fall back to the
user's own setting. The plugin's value is stored separately from that
setting and is never persisted, so it can neither overwrite a
deliberate choice — "Toggle Folding Indicators (Current Buffer)" still
wins while this is set — nor leak into the saved session. A mode that
hides them should still clear its value on the way out.

```typescript
setFoldIndicators(bufferId: number, enabled: boolean | null): boolean;
```

### `setIndentationGuide`

Enable or disable indentation guides for a buffer, overriding the global
`editor.indentation_guide` setting. Tool views that render non-editable
content (e.g. the Git Log commit-detail diff) disable them, and so does
markdown compose mode. `null` withdraws the override rather than forcing
guides on, so a buffer leaving compose gets back whatever the user's own
settings resolve to — the same shape `setFoldIndicators` uses.

```typescript
setIndentationGuide(bufferId: number, enabled: boolean | null): boolean;
```

### `setViewMode`

Set the view mode for a buffer ("source" or "compose")

```typescript
setViewMode(bufferId: number, mode: string): boolean;
```

### `setLineWrap`

Enable or disable line wrapping for a buffer/split

```typescript
setLineWrap(bufferId: number, splitId: number | null, enabled: boolean): boolean;
```

## Scrollbar Markers

Paint coloured marks on a split's vertical scrollbar, at positions
proportional to where they are in the buffer, like an overview ruler. Use them
with a line highlight (`addOverlay` with `extendToLineEnd`) and a gutter mark
(`setLineIndicator`), so marked content can be found even when it is scrolled
off screen.

Markers are anchored by byte offset, so they move with edits and stay correct
between refreshes. They work the same on a ten-line file and a
multi-gigabyte one: when line numbers aren't known yet, marks are placed by
byte ratio instead.

### `setScrollbarMarkers`

Replace this namespace's scrollbar markers for a buffer.

Markers are painted on the vertical scrollbar track at positions
proportional to their location in the buffer, so marked content is
visible at a glance even when it is scrolled off screen. Each marker is
positioned by byte offset (`position`, preferred — it is exact on files
of any size) or by 0-based `line`, optionally spans to `end`, and
carries an RGB triple or a theme key as its `color`.

A `line` is converted to a byte anchor when the marker is set. An `end`
byte makes a range marker that paints a proportional streak instead of a
single cell. Theme keys are resolved at render time, so markers follow
theme changes. `priority` breaks ties when several markers land on the
same track cell (higher wins).

The set is replaced atomically, so a refresh never renders a partially
rebuilt set.

```ts
editor.setScrollbarMarkers(bufferId, "my-plugin", [
  { position: 4096, color: "diagnostic.error" },
  { position: 8192, end: 9000, color: [80, 200, 120], priority: 2 },
]);
```

```typescript
setScrollbarMarkers(bufferId: number, namespace: string, markers: ScrollbarMarker[]): boolean;
```

### `setScrollbarMarkersInRange`

Replace only the scrollbar markers currently anchored in
`[start, end)`, leaving this namespace's markers elsewhere in the
buffer untouched.

This is the primitive for plugins that decorate the viewport as it
scrolls (a `lines_changed` producer): publish the region you just
scanned without resending — or losing — the rest of the file.

The `lines_changed` hook reports only the lines the editor decided to
process, usually the viewport. A whole-namespace replace from it would
delete the markers for everything off screen. With range scoping,
coverage builds up as the user explores the document. See
`markdown_compose.ts`, which marks headings this way.

```typescript
setScrollbarMarkersInRange(bufferId: number, namespace: string, start: number, end: number, markers: ScrollbarMarker[]): boolean;
```

| Parameter | Description |
|-----------|-------------|
| `start` | Start byte offset of the region (inclusive) |
| `end` | End byte offset of the region (exclusive) |

### `clearScrollbarMarkers`

Remove all scrollbar markers in a namespace

Namespaces are also cleared automatically when the plugin unloads.

```typescript
clearScrollbarMarkers(bufferId: number, namespace: string): boolean;
```

## Markers

### `createMarker`

Create or replace an interval marker `[start, end)` with `payload`,
keyed by `key`, on the given buffer. The editor keeps `start`/`end`
shifted across edits; an edit inside the range is the plugin's signal
(via after_insert/after_delete) to re-parse and update or delete it.

```typescript
createMarker(bufferId: number, key: string, start: number, end: number, payload: unknown): boolean;
```

### `updateMarker`

Update an existing marker's payload (keeping its current byte range).
Returns false if no marker with `key` exists on the buffer.

```typescript
updateMarker(bufferId: number, key: string, payload: unknown): boolean;
```

### `deleteMarker`

Delete a marker by key. Returns false if it did not exist.

```typescript
deleteMarker(bufferId: number, key: string): boolean;
```

### `queryMarkers`

Return all markers on the buffer whose range overlaps `[start, end)`,
as an array of `{ id, start, end, payload }`. O(n) over the buffer's
markers (a handful for typical documents).

```typescript
queryMarkers(bufferId: number, start: number, end: number): unknown;
```

### `getMarker`

Return a single marker by key as `{ id, start, end, payload }`, or null.

```typescript
getMarker(bufferId: number, key: string): unknown;
```

## Highlights

### `setSyntaxRegions`

Say where a buffer this plugin composed carries code, and in what
language, so the host highlights it. Replaces the buffer's
previous regions; setting the buffer's content clears them.

The regions are checked when the call is made; a region that doesn't
match the type (for example one with a misspelled field name) makes the
call throw.

```typescript
setSyntaxRegions(bufferId: number, regions: TsSyntaxRegion[]): boolean;
```

### `getHighlights`

Request syntax highlights for a buffer range (async)

```typescript
getHighlights(bufferId: number, start: number, end: number): Promise<TsHighlightSpan[]>;
```

## Text Measurement

### `charWidth`

Display width of a single Unicode code point, in terminal columns
(0 for control/zero-width, 2 for CJK/fullwidth and most emoji, else 1).

Backed by the editor's own width logic (`fresh_core::display_width`), so
plugins measure width exactly as the editor lays out cells — no
per-plugin width tables. An invalid code point returns 0.

```typescript
charWidth(codePoint: number): number;
```

### `stringWidth`

Display width of a string, in terminal columns (the sum of its
characters' widths). Prefer this over per-character `charWidth` calls
when measuring whole cells — one boundary crossing instead of many.

```typescript
stringWidth(text: string): number;
```

## File Explorer

### `setFileExplorerDecorations`

Set file explorer decorations for a namespace

Namespaces are isolated per plugin at runtime, so different plugins may
safely reuse the same namespace label without clearing each other's
explorer state.

```typescript
setFileExplorerDecorations(namespace: string, decorations: Record<string, unknown>[]): boolean;
```

| Parameter | Description |
|-----------|-------------|
| `namespace` | Namespace for grouping (e.g., "git-status") |
| `decorations` | Decoration entries (`FileExplorerDecoration` objects: `path`, `symbol`, `color` as an RGB array or theme key, optional `priority`) |

### `clearFileExplorerDecorations`

Clear file explorer decorations for a namespace

```typescript
clearFileExplorerDecorations(namespace: string): boolean;
```

| Parameter | Description |
|-----------|-------------|
| `namespace` | Namespace to clear (e.g., "git-status") |

### `setFileExplorerSlots`

Set file explorer slot overrides for a namespace

Each entry can override the leading icon, trailing badge, and/or name color
for a path. Unset fields fall back to the editor default: no leading icon,
and the badge and name color that file explorer decorations produce. Use
`suppressLeading`, `suppressTrailing`, or `suppressNameColor` to explicitly
clear a slot instead of replacing it.

Example (git-style name coloring):

```ts
editor.setFileExplorerSlots("git-status", [{
  path: "/project/src/main.rs",
  nameColor: "ui.syntax.string",
  priority: 10,
}]);
```

```typescript
setFileExplorerSlots(namespace: string, slots: Record<string, unknown>[]): boolean;
```

| Parameter | Description |
|-----------|-------------|
| `namespace` | Namespace for grouping (e.g., "git-status") |
| `slots` | Slot override entries (`FileExplorerSlotEntry` objects) |

### `clearFileExplorerSlots`

Clear file explorer slot overrides for a namespace

```typescript
clearFileExplorerSlots(namespace: string): boolean;
```

| Parameter | Description |
|-----------|-------------|
| `namespace` | Namespace to clear (e.g., "git-status") |

## Animations

### `animateArea`

Start a frame-buffer animation over an arbitrary screen region.
Returns an animation id usable with `cancelAnimation`.

```typescript
animateArea(rect: AnimationRect, kind: PluginAnimationKind): number;
```

### `animateVirtualBuffer`

Start an animation over the on-screen Rect currently occupied by a
virtual buffer. No-op if the buffer is not visible.

```typescript
animateVirtualBuffer(bufferId: number, kind: PluginAnimationKind): number;
```

### `cancelAnimation`

Cancel an animation previously started via `animateArea` or
`animateVirtualBuffer`. No-op if the ID is unknown or already done.

```typescript
cancelAnimation(id: number): boolean;
```

:::
