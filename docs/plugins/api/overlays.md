# Overlays and Virtual Text API

## Overlay Operations

### `addOverlay`

Add a visual overlay to a buffer range. Overlays persist until explicitly removed.
Use namespaces for batch removal (e.g., "spell", "todo").
Multiple overlays can apply to the same range; colors blend.

```typescript
addOverlay(buffer_id: number, namespace: string, start: number, end: number, r: number, g: number, b: number, bg_r: number, bg_g: number, bg_b: number, underline: boolean, bold: boolean, italic: boolean, extend_to_line_end: boolean): boolean
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
| `b` | `number` | Blue (0-255) |
| `bg_r` | `number` | - |
| `bg_g` | `number` | - |
| `bg_b` | `number` | - |
| `underline` | `boolean` | Add underline decoration |
| `bold` | `boolean` | Use bold text |
| `italic` | `boolean` | Use italic text |
| `extend_to_line_end` | `boolean` | Extend background to end of visual line |

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
| `b` | `number` | Blue color component (0-255) |
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

## Scrollbar Markers

Paint colored marks on a split's vertical scrollbar at positions proportional
to their location in the buffer — an "overview ruler". Use it alongside a line
highlight (`addOverlay` with `extendToLineEnd`) and a gutter mark
(`setLineIndicator`) so marked content is findable even when scrolled off
screen.

Markers are anchored by byte offset inside the editor, so they shift with edits
and stay correct between refreshes. They work identically on a ten-line file and
a multi-gigabyte one: on files too large for line numbers to be known, marks are
positioned by byte ratio instead.

#### `setScrollbarMarkers`

Replace a namespace's entire marker set for a buffer. The swap is atomic, so a
refresh never shows a partially rebuilt set.

```typescript
setScrollbarMarkers(bufferId: number, namespace: string, markers: ScrollbarMarker[]): boolean
```

A `ScrollbarMarker` is positioned by `position` (byte offset — preferred, and
exact at any file size) or `line` (0-based, converted to a byte anchor when
set). An optional `end` byte makes it a range marker that paints a proportional
streak instead of a single cell. `color` takes an RGB triple or a theme key,
resolved at render time so marks follow theme changes. `priority` breaks ties
when several markers land on the same track cell.

```typescript
editor.setScrollbarMarkers(bufferId, "my-plugin", [
  { position: 4096, color: "diagnostic.error" },
  { position: 8192, end: 9000, color: [80, 200, 120], priority: 2 },
]);
```

#### `setScrollbarMarkersInRange`

Replace only the markers currently anchored in `[start, end)`, leaving this
namespace's markers elsewhere in the buffer untouched.

```typescript
setScrollbarMarkersInRange(
  bufferId: number, namespace: string,
  start: number, end: number,
  markers: ScrollbarMarker[],
): boolean
```

This is the form to use from a `lines_changed` handler. That hook reports only
the lines the editor decided to process — usually the viewport — so a
whole-namespace replace would delete the marks for everything off screen. Range
scoping publishes just the region you scanned, and coverage accumulates as the
user explores the document. See `markdown_compose.ts`, which marks headings this
way.

#### `clearScrollbarMarkers`

Remove all of a namespace's markers. Namespaces are also cleared automatically
when the plugin unloads.

```typescript
clearScrollbarMarkers(bufferId: number, namespace: string): boolean
```
