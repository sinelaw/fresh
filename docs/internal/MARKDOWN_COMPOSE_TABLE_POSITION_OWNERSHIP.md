# Markdown Compose: Table Position Ownership

Status: **Alternative 1 (§4) is now implemented** — table borders are emitted
per line, anchored to the editor's auto-shifting virtual-line markers, and the
plugin holds no table byte positions at all. §6 records what shipped. §1–§3
record the original cross-thread desync and the event-authoritative interim fix
that preceded it; §4–§5 are the alternatives analysis that led here.

Companion to [`MARKDOWN_COMPOSE_MARKER_DESIGN.md`](./MARKDOWN_COMPOSE_MARKER_DESIGN.md),
which describes the move from line-number bookkeeping to byte-range interval
markers. That document's top "Implementation note" asserts the marker design is
safe because "the editor owns the byte coordinates and shifts every marker on
each edit … so the plugin never tracks or shifts byte offsets." §1 below is the
case where that assertion does not hold.

Plugin: `crates/fresh-editor/plugins/markdown_compose.ts`. Regression test:
`crates/fresh-editor/tests/e2e/markdown_compose_table_border.rs`
(`test_table_border_no_doubled_separators_on_marker_event_desync`).

---

## 1. The bug: marker/event desync across the thread boundary

The marker design assumed the marker coordinates the plugin reads are always
consistent with the buffer state the plugin is reacting to. They are not,
because the two arrive on different schedules across the thread boundary.

- `lines_changed` is fired **fire-and-forget** to the plugin thread
  (`render.rs` → `run_hook("lines_changed")` → `thread.rs`
  `fire_and_forget(send(RunHook))`). Its payload — each row's `byte_start`/
  `byte_end` — is a **snapshot captured at render time**, internally consistent
  for that one buffer state.
- The plugin reads the **marker coordinates** *later*, off the shared
  `Arc<RwLock<EditorStateSnapshot>>`, whenever it processes that event. The
  editor thread keeps mutating that snapshot (`shift_plugin_markers_for_edit`)
  as further edits land.

Under load the plugin can process a `lines_changed` for edit *N* only after the
editor has already shifted the marker for edit *N+1*. The marker is then a few
bytes off the event's row positions. (Observed in a logged repro: the marker
oscillated ahead/behind the event by one byte — `group=44..198 near=[t1@43..198]`
— and the discrepancy was not uniform between `start` and `end`, confirming a
genuine timing race rather than a missing shift call.)

The marker-trusting `updateTableBlocks` then **merged** the event rows into the
marker's offset stored rows:

```
edit (Enter above table)
  editor thread: buffer → state N+1 ; marker shifted to N+1
  editor thread: render → lines_changed(positions = state N) queued, fire-and-forget
        │   (plugin is behind under load)
        ▼
  plugin processes lines_changed(N):
     near = queryMarkers(...)                     ← marker at N+1   ⚠ off by one
     block.startByte = min(markerStart, gStart)   ← min(N+1, N) = N      } marker
     block.endByte   = max(markerEnd,   gEnd)     ← max(N+1, N) = N+1    } STRETCHED
     block.rows = union(markerRows@N+1, eventRows@N)  ← two sets, one byte apart
     saveBlock(...)  → persists the stretched marker + DOUBLED rows
        ▼
  drawTableBorders → one ├─┼─┤ per stored row → DOUBLED separators
```

Two properties made it stick:

1. The union assumed `markerRows == eventRows`; one byte of disagreement
   double-counted every row.
2. The stretched, doubled marker was **persisted** (`createMarker`), so a later
   convergence redraw re-read the poisoned payload and the corruption survived.

**Why no editor-side shift discipline fixes this.** The defect is *temporal*
(the read happens at the wrong time relative to the event), not a forgotten
shift. There is no ordering the editor can impose on a fire-and-forget hook that
guarantees the marker the plugin reads matches the event payload it is
processing — the plugin reads asynchronously, after arbitrary further edits.
Making the hook synchronous *would* close it, but that reintroduces the
deadlock/latency the fire-and-forget design exists to avoid.

---

## 2. Reproduction

The deterministic harness drains async plugin work to quiescence after each
keypress, so the genuine race cannot be provoked through the key API. The
regression test instead injects the race's **exact consequence** — a one-byte
marker/event offset — deterministically:

1. open a table in compose mode, let it render a clean frame;
2. nudge the plugin marker one byte ahead of the live buffer via the
   `#[doc(hidden)]` `Editor::shift_plugin_markers_for_edit` **without editing the
   buffer** (precisely the state a lagging plugin observes);
3. force a `lines_changed` redraw (a benign cursor move clears
   `seen_byte_ranges`);
4. assert the rendered frame is strictly border/content alternating.

The assertion (`assert_table_frame_well_formed`) catches doubled `├─┼─┤`
separators and blank lines inside the frame, which the older corner-counting
check does not (it still sees exactly one `┌`/`└`).

---

## 3. The current fix: event positions are authoritative

The `lines_changed` payload is the only internally-consistent position source
for a hook invocation, so it wins. `updateTableBlocks` was changed to:

- **rebuild** each visible table's rows straight from the event group (no union
  with stored rows);
- derive `startByte`/`endByte` from those live rows, never `min`/`max` with the
  marker;
- rewrite the marker coordinates from the live positions every batch
  (`createMarker`), demoting the marker to **identity + memory** only:
  - `id` → the stable border namespace `md-tb-${id}`;
  - `maxW`/`allocated` → accumulated column widths (numbers, no position
    semantics → race-immune);
- retain stored rows only for a tall table's **off-screen** continuation, and
  only when they sit more than half a row-gap beyond the batch (so a genuine
  off-screen row, a full row-gap away, is kept, while a lagged near-duplicate of
  a *visible* row, a few bytes off the boundary, is dropped).

| marker field | before | after |
|---|---|---|
| `start`/`end` | trusted to locate the table; merged via min/max | overwritten from event rows each batch |
| `rows`/`sepRows` | unioned with event rows | rebuilt from event; stored rows kept only for off-screen rows |
| `id` | stable namespace | stable namespace (unchanged) |
| `maxW`/`allocated` | accumulated | accumulated (unchanged) |

The race still happens at the thread boundary; the plugin simply no longer reads
positional truth from the marker for rows the event already describes, so an
offset marker can no longer stretch the block or double its rows. The
editor-side shifts still run as best-effort (they keep off-screen retention and
range queries roughly accurate) but are no longer load-bearing for correctness.

This is the minimal, surgical realization of one principle: **the plugin should
not be a position bookkeeper.** The rest of this document explores taking that
principle to its architectural conclusion.

---

## 4. Alternatives: move position ownership fully into the editor

Target end-state: the plugin never stores or reconstructs byte offsets. It holds
only **opaque marker handles** and **semantic data** (column widths, alignment).
All position truth lives in the editor's marker/decoration layer, which already
auto-shifts on edits. The race dissolves by construction — the only thing that
can be stale, a byte position read off a snapshot at the wrong time, no longer
exists in the plugin.

Ordered by how much has to move into the editor.

### Alternative 1 — Marker-anchored decorations (smallest editor change)

The editor's virtual-lines and conceals stop taking raw byte anchors and instead
take a **marker handle + gravity**. When the plugin first discovers a table it
creates editor-side markers (one per row, or one table marker plus per-row
anchors) and *attaches* the border virtual-lines and cell conceals to those
markers **once**. Edits then shift the markers and the attached decorations ride
along automatically; the plugin does nothing per render.

- **Plugin holds:** `tableId → { rowMarkerHandles, columnWidths }`. No bytes.
- **Plugin re-engages:** only on *structural* change (row added/removed, widest
  column changed), detected from `after_insert`/`after_delete` landing inside a
  table marker's span or from `lines_changed` *content* — never by recomputing
  where existing rows are.
- **New editor primitives:** `addVirtualLine(markerHandle, gravity, content)` and
  `addConceal(markerHandle, …)` anchored to a marker, not a byte; decorations
  dropped automatically when their anchor marker is deleted.
- **Race:** the decorations already attached are resolved editor-side from the
  current marker position → never stale. The only residual async is "add a new
  row marker," a structural event, which is race-immune from content.
- **Cost:** moderate editor change (marker-relative decoration anchors); the
  plugin loses its row arrays, `updateTableBlocks` rebuild, and off-screen
  retention but still owns *what* to draw. Marker-relative anchoring benefits
  every position-tracking plugin, not just this one.

### Alternative 2 — Semantic region marker + editor-native frame renderer (fully race-free)

The plugin tags a single marker spanning the table with a payload like
`{ kind: "table", widths: [...], align: [...] }`. The editor's renderer gains a
**table-frame decoration kind**: during render, on its own thread, for each such
marker overlapping the viewport it walks the lines *currently* inside the
marker's span (live buffer) and draws the box-drawing frame and pads/conceals
cells to `widths`. Positions are derived at render time from live text and the
auto-shifted marker; the plugin is never consulted about position.

- **Plugin holds:** `tableId → marker handle` (+ recomputes `widths` into the
  payload when content changes). No bytes, no rows array, no border drawing.
- **New editor primitives:** a renderer that frames "this region as a
  column-laid-out table" — markdown-table layout knowledge (or a generic-enough
  "framed region with N columns of widths W") in core.
- **Race:** *zero*. Frame geometry is recomputed every frame from live state on
  the editor thread; there is nothing the plugin can desync. The same-pass
  create-then-query constraint also vanishes — the plugin never queries
  positions.
- **Cost:** highest editor complexity, and it pushes markdown-presentation
  semantics into core (or forces a clean generic "framed columnar region"
  primitive other plugins could reuse — frames, callouts, code-block borders).
  The plugin shrinks to ~50 lines: parse, compute widths, upsert one marker.

This is the purest realization of the goal. The honest tension is architectural:
core is markdown-agnostic today and `markdown_compose` owns all of this;
Alternative 2 moves *rendering of the frame* into core.

### Alternative 3 — Synchronous render-time decoration callback (stateless plugin)

Invert control. During render the editor calls a **synchronous** hook: "here are
the live visible lines and their positions; return the conceals/borders." The
plugin is a pure function `(visible text) → decorations`, holding no state — not
even marker handles.

- **Plugin holds:** nothing. Cross-render width accumulation for off-screen rows
  is either dropped or is the one piece kept in a marker payload.
- **Race:** zero — the editor supplies live positions and waits for the answer in
  the same render pass.
- **Cost:** the one we deliberately designed *away* from. A synchronous plugin
  call mid-render reintroduces the plugin-calls-back-into-editor deadlock surface
  and per-frame latency at 60fps. Listed for completeness because it is the
  cleanest *conceptually* (truly stateless plugin) even if the worst
  *operationally*.

### Two problems any rewrite must still answer

1. **Cross-render column widths.** A tall table's widest row can be off-screen,
   so widths are genuine state that must persist and accumulate. In all three
   alternatives this lives as **numbers in a marker payload** (or editor-side
   region state). Numbers have no position semantics → race-immune. This is the
   one legitimate piece of "memory" that stays, and it is safe.

2. **Structural detection without positions.** "A row was added/removed" must be
   detected to add/remove an anchor (Alt 1) or to know a marker's span changed
   (Alt 2). This comes from edit *content* and the marker's own auto-maintained
   span — never from the plugin diffing byte arrays. Alt 2 gets it almost for
   free (the span is editor-maintained); Alt 1 needs care at the table's
   first/last row when an edit lands exactly on the boundary.

---

## 5. Recommendation

- **Alternative 2** is the right target for a real rewrite — ideally via a
  *generic* "framed columnar region" decoration primitive rather than a
  markdown-specific one, so core does not grow a markdown dependency and other
  plugins can reuse it. Race-free by construction; the plugin becomes tiny; it
  eliminates the entire class of plugin position-bookkeeping bugs, not just this
  one.
- **Alternative 1** is the pragmatic middle: it removes byte-position tracking
  from the plugin and kills the race for everything already attached, with a far
  smaller editor change (marker-relative decoration anchors) and without putting
  table rendering into core. If a full rewrite is too much, this is the
  high-leverage step, and marker-relative anchoring benefits every
  position-tracking plugin.
- **Alternative 3** only if the synchronous-hook cost is acceptable (non-reentrant,
  cheap); it is the cleanest conceptually but the most operationally fraught.

The shipped fix (§3) is the surgical version of the same philosophy — event
positions are truth, the marker is identity + width memory. Alternatives 1 and 2
take it to its conclusion: make the editor the sole keeper of positions, so the
plugin has nothing left to get wrong.

---

## 6. What shipped (Alternative 1, refined)

The implementation matched Alternative 1's *goal* — the plugin holds no table
byte positions — but the realization differs from the "anchor once, update in
place" sketch in §4, because investigating it surfaced two facts:

1. **Decorations already auto-shift.** A plugin virtual line stores a
   `marker_id` (`virtual_text.rs`), and `marker_list` is shifted on every edit
   inside `apply_insert`/`apply_delete`. So a border, once created, rides the
   text on its own — the plugin never needed to re-derive its position. The bug
   was that the plugin kept a *second, parallel* position store (the table block
   `rows`) and redrew borders from it; that parallel store is what desynced.

2. **Virtual lines lacked a per-range clear.** Conceals had
   `clearConcealsInRange`, which is why the conceal pass could run per line;
   virtual lines only had whole-namespace clear, which forced the borders into
   the stored-block workaround. The genuinely missing primitive was a per-range
   virtual-line clear.

So the change is:

- **New editor primitive** `clearVirtualLinesInRange(buffer, namespace, start,
  end)` — `PluginCommand::ClearVirtualLinesInRange` →
  `VirtualTextManager::clear_lines_in_range`, the direct analogue of the conceal
  range-clear (resolves each line's anchor live from the marker list).
- **Per-line borders.** `markdown_compose.ts` deletes the entire block model
  (`updateTableBlocks`, the table interval markers, `rows`/`sepRows`,
  accumulation, off-screen retention). The `lines_changed` handler now, for each
  line in the batch: clears that row's border range, then re-emits its frame
  (`emitRowBorders`) by role. Role (first / last / source-sep-adjacent) is local
  — from the row plus its immediate neighbours in the same batch. Column widths
  are computed per render from the batch's table groups (`computeRowWidths`) and
  shared with the conceal pass.
- All borders live in **one namespace** (`md-tb`); the per-line clears are
  byte-range scoped, so adjacent rows and distinct tables never collide.

### The subtlety that bit, and the rule that fixes it

The clear must span the row's **whole content range** `[byteStart, byteEnd)`,
not a single byte. Under the same async lag, a previously-emitted border rides a
few bytes ahead of the event's `byteStart`; a one-byte clear misses it and
strands a doubled separator (observed interactively, and it does *not* self-heal
because every frame re-misses by the same lag). A line-wide clear tolerates the
lag exactly as `clearConcealsInRange` does — the border anchor is always well
inside its row's range. Guarded by
`test_clear_lines_in_range_tolerates_offset_anchor`.

### What this buys, and the one trade

- No table state in the plugin → the marker/event desync class is **structurally
  impossible**, not merely patched. Tall tables, partial scroll, and edits are
  all just per-line work, like conceals.
- Trade: column widths reflect the rows *currently in the batch*, so there is no
  cross-frame width accumulation. A wider row off-screen during a partial
  mouse-wheel scroll doesn't widen the visible columns until the table is
  measured together again (any cursor move re-measures the whole viewport).
  Within any single render every visible row of a table shares one width array,
  so borders and cell conceals always line up.

`clearVirtualLinesInRange` is reusable by any plugin doing per-line virtual-line
decoration; it is the virtual-line counterpart of the conceal/overlay range
clears that already existed.
