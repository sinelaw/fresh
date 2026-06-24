# Markdown Compose: Marker-Based Block Design

Status: tables implemented (plugin-side). Plugin: `crates/fresh-editor/plugins/markdown_compose.ts`.

> **Implementation note.** Tables now use a byte-range, stable-id block index
> that lives **in the plugin** (`tableBlocks` / `TableBlock`), shifted on the
> existing `after_insert` / `after_delete` blast-radius hooks. This realizes the
> design (byte-range blocks, stable identity, edit-driven invalidation, spatial
> render lookup, no line numbers) without the core marker API of §5. The
> core-backed variant in §5 was deferred because fresh's plugin runs on a
> separate thread reading a periodically-refreshed `state_snapshot`, so a
> *synchronous* `queryMarkers` in the render path has an edit→shift→snapshot→read
> timing hazard; the plugin-owned index sidesteps it and needs no breaking API
> change. §5 remains the path to make the plugin thinner later. Fenced-code and
> other Tier-2 blocks (§7) are not yet migrated.
>
> With the block index making border redraws idempotent, `cursor_moved` returns
> to a single `refreshLines` (the targeted-recompute experiment broke table-cell
> wrap/reveal during cursor motion); correctness now comes from the block design,
> not from limiting how often decorations are rebuilt.

This document describes the move from **line-number bookkeeping** to
**byte-range interval markers** for the multi-line constructs in the markdown
compose plugin (tables first, then fenced code and other delimited blocks). It
also documents the existing **per-line clear-and-rebuild** logic used for inline
decorations (bold / italic / links / …) and — importantly — explains *why those
do not change*.

---

## 1. The bug this fixes

In compose mode the plugin draws a box-drawing frame around tables: a `┌─┬─┐`
top border above the header, `├─┼─┤` separators between rows, and a `└─┴─┘`
bottom border, plus per-cell conceals that pad columns to a shared width.

That table state is keyed by **line number**:

- the cached column widths live in a per-buffer map keyed by line number
  (`getTableWidths(buf).get(lineNumber)`), persisted via `setViewState`;
- each row's borders live in a per-line namespace `md-tb-${lineNumber}`;
- first/last-row classification is `widthMap.has(lineNumber ± 1)`.

Line numbers are the one coordinate that **every insert above the table
invalidates**. Inserting blank lines above a table renumbers all of its rows, so
the cached entries and per-line namespaces no longer line up with the rows they
describe. The classification then believes the header has a table row above it
and stops drawing the top border. The corruption is **cumulative**: the table
renders correctly for the first few inserts and only breaks once enough stale
entries have piled up (around the 5th insert with a small table).

Regression test: `crates/fresh-editor/tests/e2e/markdown_compose_table_border.rs`
hammers Enter at the top of the file and asserts on rendered output that the
table stays a single well-formed frame.

The marker layer underneath the plugin already moves decorations correctly on
edits — only the plugin's **line-number index on top** rots.

---

## 2. Two tiers of decoration

The plugin's features split cleanly by whether they carry **cross-line state**.

### Tier 1 — stateless, per-line (no markers; already correct)

Everything computable from a **single line's bytes + the cursor position**:
inline emphasis/strikethrough, links, inline code, HTML entities, ATX headings,
horizontal rules, images, the bullet/ordered/checkbox/blockquote *markers*,
soft-wrap break points, and hanging indents.

These are a pure, idempotent function of the line content, anchored by **byte
offset**, and cleared by **byte range**. They ride edits via the marker layer
and have none of the line-number bug. They are **not changed** by this design.

### Tier 2 — block-range markers (the new design)

Constructs that **span multiple source lines** and either carry shared state or
change how the lines inside them are interpreted:

| Block | Payload `kind` | Effect inside the range |
|-------|----------------|-------------------------|
| Table | `"table"` (column widths) | cell-alignment conceals + border virtual lines |
| Fenced code block ` ``` … ``` ` | `"code"` (lang, indent) | suppress markdown conceal/overlay/wrap |
| Front-matter / HTML / math block | `"raw"` | suppress markdown processing |

Each becomes **one interval marker `[start_byte, end_byte]`** with a typed
payload. This is the only tier that needs the new core API.

---

## 3. Tier 1 in detail: per-line clear-and-rebuild

This is the existing logic (`processLineConceals`) and it is the model the new
design *imitates* at the block level — so it is worth stating precisely.

### Anchoring is byte-based, never line-based

Inline spans are found with `findInlineSpans(lineContent)` and every conceal /
overlay is anchored at a **byte offset** derived from the line's current
`byteStart` plus an intra-line char→byte conversion:

```ts
const byteCS = charToByte(lineContent, span.contentStart, byteStart);
const byteCE = charToByte(lineContent, span.contentEnd,   byteStart);
editor.addOverlay(bufferId, "md-emphasis", byteCS, byteCE, { bold: true });
editor.addConceal(bufferId, "md-syntax",   rStart, rEnd, range.replacement); // hide **
```

The only use of `lineNumber` anywhere in `processLineConceals` is the *table*
width-cache lookup (`getTableWidths(buf).get(lineNumber)`). Emphasis / links /
entities never touch it — they would render identically if `lineNumber` were
`undefined`.

### The plugin tracks nothing; it clears a range and rebuilds

The plugin does **not** keep a registry of which decorations exist or where.
When it (re)processes a line it blindly clears the line's byte range and rebuilds
from content:

```ts
editor.clearConcealsInRange(bufferId, byteStart, byteEnd);                       // wipe all conceals here
editor.clearOverlaysInRangeForNamespace(bufferId, "md-emphasis", byteStart, byteEnd);
// ...then re-add fresh conceals/overlays computed from lineContent
```

The editor side is what actually knows positions: each conceal stores a
`start_marker` / `end_marker` (`MarkerId`s into the shared marker list), and its
live range is resolved on demand via `marker_list.get_position(...)`.
`clearConcealsInRange` walks the conceals, resolves each one's **current** range,
and removes those overlapping the query range. Because decorations are
marker-anchored they **shift with their line**, so passing the line's *current*
range (handed to the plugin fresh by the `lines_changed` event) always catches
exactly the decorations that belong to that line — even though they were created
at a different byte offset.

### Cursor reveal: per-span re-add, not surgical removal

When the cursor moves into an emphasis span, the plugin reveals **only that
span's** markers. It does this not by deleting one conceal but by
clearing the whole line and choosing, per span, whether to re-add the conceal:

```ts
const byteMS = charToByte(lineContent, span.matchStart, byteStart); // start of *def*
const byteME = charToByte(lineContent, span.matchEnd,   byteStart); // end   of *def*
const cursorInSpan = cursors.some(c => c >= byteMS && c <= byteME);
const skipConceal  = (isTableRow && cursorStrictlyOnLine) || cursorInSpan;
if (!skipConceal) {
  for (const range of span.concealRanges) {            // the '*' markers
    editor.addConceal(bufferId, "md-syntax", rStart, rEnd, range.replacement);
  }
}
```

For `abc *def* hij *klm*` with the cursor inside `def`: the line is cleared, then
both spans are rebuilt; `*def*` has `cursorInSpan === true` so its `*` conceals
are **not re-added** (markers visible), while `*klm*` re-adds its conceals
(markers hidden). The **overlay** (bold styling) is added regardless of the
cursor, so `def` stays bold while its markers show. Moving the cursor into `klm`
recomputes the line again and the choice flips automatically — it is re-evaluated
against the new cursor byte on every rebuild.

Triggering: this recompute runs for **only the line the cursor left and the line
it entered** (`recomposeCursorLine`), not the whole buffer. (Previously
`cursor_moved` called the buffer-wide `editor.refreshLines()`, which cleared the
editor's "seen lines" set and re-decorated everything — the change that first
exposed the table corruption.)

### Why Tier 1 needs no markers of its own

Each line's inline decorations are a pure function of *that line's* content +
cursor. There is no neighbor dependency and no persisted cache to rot. The editor
already *is* the position registry (markers), so the plugin stays stateless:
it receives the line's current bytes from `lines_changed`, clears that range, and
rebuilds. Re-deriving simply reproduces the same conceals.

This is exactly why, under the old buffer-wide refresh, **emphasis never
corrupted but tables did** — tables were the one place that consulted a
persisted, line-number-keyed cache and cross-line neighbor classification.

---

## 4. Tier 2 in detail: tables as interval markers

### Representation

A table is **one interval marker `[start_byte, end_byte]`** carrying:

```ts
payload = {
  kind: "table",
  maxW: number[],        // max raw content width per column (accumulate-and-grow)
  allocated: number[],   // viewport-constrained per-column widths used to draw
  sepRowByte?: number,   // byte offset of the source `|---|` separator row, if any
}
```

The core keeps the interval shifted on every edit. The plugin holds only the
marker **ID** (or queries spatially), reads the live span back on demand, and
**never stores byte offsets** — storing an offset across edits would just be the
line-number bug in a new disguise.

Affinity at the two ends makes "the table moves" precise: `start` is
**right-gravity** (an insert immediately above pushes the whole table down) and
`end` is **left-gravity** (typing immediately after is not swallowed into the
table).

### Lifecycle

- **Discover** (table scrolls into view / first parse): for a `|…|` line,
  `queryMarkers(line.byte_start)` returns nothing → parse the contiguous block of
  table rows, compute `maxW`/`allocated`, `createMarker(start, end, payload)`.
  Lazy and viewport-driven — never a whole-file scan (see CONTRIBUTING #2). For a
  table taller than the viewport, `maxW` still accumulates-and-grows as more rows
  are seen, but now keyed to the stable marker, not a line number.

- **Render**: for each visible line, `queryMarkers(line.byte_start)`; if a table
  marker covers it, pull `allocated` from the payload and draw the cell gaps +
  borders. No `prevIsTable`, no `widthMap.has(lineNumber ± 1)`, no line numbers.

- **Edit → `queryMarkers(blastRadius)`** (blast radius from the existing
  `after_insert` / `after_delete` `affected_start` / `affected_end`):
  - **Outside every table → do nothing.** The tree slides the table down and the
    marker-anchored borders ride along. This is the case that was broken.
  - **Inside a table** → re-parse that local span, clear decorations over the
    marker's **current** range (`clearConcealsInRange` +
    `clearVirtualLinesInRangeForNamespace`), `updateMarker` (new widths/extent) or
    `deleteMarker` / split if the edit broke the table, then redraw.

### The difference from Tier 1, stated plainly

Both tiers use the same discipline — *the plugin stores an ID/derives a range
from the editor, clears that byte range, and rebuilds from content; it never
tracks individual decorations.* The difference is **what defines the range**:

- **Tier 1 (emphasis):** the range is a **single line**, handed to the plugin
  fresh by `lines_changed` each time. No durable handle is needed because the
  unit of work is "one line of content," delivered on demand.
- **Tier 2 (tables):** the range is a **multi-line block** that has no natural
  per-event delivery and whose width state must persist across scroll. That
  durable, shift-correct handle is the **interval marker**: `getMarker(id)` (or
  `queryMarkers`) returns the block's current `[start,end]` + payload, resolved
  live exactly like a conceal's range.

In short: emphasis gets its range from the *event*; a table gets its range from a
*marker*. Neither tier holds byte offsets in plugin state.

---

## 5. Required core/API changes

**1. One change to an existing core struct.** Markers gain a **payload**
(`serde_json::Value`) — a field on the interval-tree node
(`crates/fresh-editor/src/model/marker_tree.rs`) or a side map keyed by
`MarkerId`. Everything else is additive.

**2. New plugin-facing marker API**, a thin wrapper over the existing interval
tree (which already does `[start,end]` intervals, `query(start,end)`,
`get_marker`, `set_position`, `delete`, and `adjust_for_edit` on every edit):

```
createMarker(bufferId, start, end, payload, startAffinity, endAffinity) -> id
updateMarker(bufferId, id, payload) -> bool
deleteMarker(bufferId, id) -> bool
getMarker(bufferId, id) -> { start, end, payload } | null
queryMarkers(bufferId, start, end) -> [{ id, start, end, payload }]
```

Plugin data-markers must be **isolated** from decoration anchor markers (their own
per-buffer store, or a `MarkerType` tag) so `queryMarkers` returns only the
plugin's blocks. The store must be shifted by the **same `adjust_for_edit`** path
the buffer already calls for its marker list, **before** `after_insert` /
`after_delete` fires, so the blast radius and marker coordinates share one frame.

**3. One parity clear for virtual lines (namespaced):**

```
clearVirtualLinesInRangeForNamespace(bufferId, namespace, start, end) -> bool
```

Mirrors the existing `clearOverlaysInRangeForNamespace`. Virtual *lines*
(borders) today can only be cleared by whole namespace
(`clearVirtualTextNamespace`), which is exactly why the plugin invented per-line
`md-tb-${lineNumber}` namespaces. A namespaced range-clear lets borders use a
single static namespace and be cleared by byte range. (It must be
namespace-scoped so it does not wipe other plugins' virtual lines, e.g. git-blame
headers. Border anchors must lie within the table's `[start,end]` span so the
range-clear catches them: the top border anchored at `header.byte_start` and the
bottom at `last_row_end - 1` both fall inside.)

**4. Regenerate** `fresh.d.ts`
(`cargo test -p fresh-plugin-runtime write_fresh_dts_file -- --ignored`) and
`tsc`-check the plugin (`crates/fresh-editor/plugins/check-types.sh`).

**No changes** to existing decoration signatures: `addConceal`, `addOverlay`,
`addVirtualLine`, `addSoftBreak`, and their range/namespace clears stay as-is and
do **not** return IDs. Decorations are a projection, not separately addressable;
IDs live only in the new marker API.

---

## 6. What is removed vs. kept

**Removed:** the line-number `widthMap` (`getTableWidths` / `setTableWidths` /
view-state), the per-line `md-tb-${lineNumber}` namespaces, the
`prevIsTable` / `nextIsTable` neighbor logic, and the residual buffer-wide
`refreshLines` table crutches.

**Kept (already byte-based and correct):** all Tier 1 inline / single-line
decorations and their clear-and-rebuild logic, soft-wrap + hanging indents, the
targeted cursor-reveal, and global scalars (compose on/off, compose width, column
guides, line-wrap / line-number toggles). These are not position-indexed, so they
are not a bug source and do not belong in the interval tree.

---

## 7. Generality

The identical primitive — `createMarker` / `queryMarkers` + a payload `kind` +
the namespaced range-clear — also covers **fenced code blocks**
(payload `kind: "code"`, suppress markdown inside; this also fixes the existing
"we'd need multi-line context" limitation noted in `processLineConceals`) and
other delimited regions (front-matter, HTML / math blocks). The core API does not
grow per construct; only the payload `kind` and the plugin's render dispatch
differ.
