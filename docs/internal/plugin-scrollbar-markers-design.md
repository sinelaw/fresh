# Design: plugin API for colored scrollbar markers (issue #2713)

Status: proposed
Issue: https://github.com/sinelaw/fresh/issues/2713

Plugins can already highlight a line (`addOverlay` with `extendToLineEnd`) and
mark the gutter (`setLineIndicator`). This document designs the missing third
surface: **colored markers painted on the vertical scrollbar track at
positions proportional to their location in the buffer** — the "overview
ruler" familiar from VSCode (search matches, diagnostics, diff hunks).

The design goal stated by the request: robust, elegant, clean, and performant
— working identically for a 10-line file and a 10M-line file.

---

## 1. Constraints discovered in the codebase

These facts shape every decision below; sources are cited so the eventual
implementation can verify them.

### 1.1 The scrollbar has three sizing regimes, not one

`scrollbar_line_counts` (`crates/fresh-editor/src/view/ui/split_rendering/scrollbar.rs:34`)
picks the thumb's coordinate basis per frame:

| Regime | Condition | Basis | Cost per frame |
|---|---|---|---|
| **A — exact visual rows** | wrap on, ≤ `MAX_WRAP_SCROLLBAR_LINES` (5 000) lines and ≤ `MAX_WRAP_SCROLLBAR_BYTES` (2 MiB) | `VisualRowIndex` wrapped-row counts | O(log N) steady state; O(N) rebuild per edit |
| **B — logical lines** | file ≤ `large_file_threshold_bytes` (10 MiB default) | logical line numbers | 2 × `get_line_number` = O(log n) |
| **C — byte ratio** | file > threshold | raw bytes; fixed 1-row thumb (`scrollbar.rs:194-200`) | O(1) |

Regime C exists because on a freshly opened large file **line numbers do not
exist at all**: the piece tree's `line_feed_cnt` is `None` until the
incremental line scan runs (`buffer/mod.rs:596`, `app/line_scan.rs`), and
`line_count()` returns `None`. Regime A's bail-out constants encode the
fresh#2610 lesson (`scrollbar.rs:15-23`): anything rebuilt per edit must never
be O(all lines) on a large buffer.

**Consequence:** the only coordinate that is valid, exact, and O(1) in every
regime is the **byte offset**. Markers must be byte-addressed internally, and
the marker→track projection must follow the *same* regime selection as the
thumb, or markers will visibly disagree with the thumb.

### 1.2 The track is tiny; the file is not

The scrollbar is one column wide (`split_rendering/layout.rs:83`) and at most
terminal-height tall (≈ 24–200 cells). Whatever the marker count, the
render-side representation can therefore be a **fixed-length bucket array of
`track_height` cells**. Projection is O(M) in marker count, painting is
O(track_height), and neither depends on file size. Only M must be bounded —
and beyond a few thousand markers extra fidelity is invisible anyway.

### 1.3 Byte-anchored state must ride the marker infrastructure

Anything keyed by line number silently desyncs on the first edit. Both
existing decoration stores anchor to `MarkerId`s in an interval tree with
O(log n) `adjust_for_edit` (`model/marker.rs`, `model/marker_tree.rs`), wired
into every edit path (`state.rs:584-585`, `:666-667`, `:1170-1184`, plus the
reload/undo sites). `MarginManager` (`view/margin.rs:295`) is the direct
precedent: a private `MarkerList` plus a map from marker → namespace →
decoration.

### 1.4 The existing gutter API is the anti-pattern

`setLineIndicator` (`fresh.d.ts:3352`) takes 8 positional args, RGB only (no
theme keys — `live_diff.ts:55-57` complains about this in a comment), a bare
`String` namespace, converts line→byte at set time but *accepts* lines, has no
range-scoped clear, and forgets to set `plugin_render_requested`. The new API
should instead follow `addOverlay`'s shape: namespace + options object +
`OverlayColorSpec` (`[r,g,b]` or `"theme.key"`, `api.rs:639`), batched from
day one (the plural `setLineIndicators` exists precisely because per-item
calls caused visible tearing — `live_diff.ts:796-814`).

### 1.5 Two renderers consume the scrollbar

The terminal paints per-cell backgrounds (`scrollbar.rs:233-244`). The web UI
does **not** read those cells — it receives `vscroll` + `thumbStart`/`thumbEnd`
in the scene JSON (`webui/mod.rs:1715-1727`) and draws its own DOM thumb.
Markers therefore need a scene-protocol field too, plus theme-run coverage
(`record_scrollbar_theme_runs`, `orchestration/mod.rs:1064`) so the theme
inspector labels them.

---

## 2. Plugin-facing API

Two commands, replace-set semantics, batch-only:

```ts
/** One scrollbar marker. Position is a byte offset (preferred — valid in
 *  every file-size regime) or a 0-based logical line (converted to a byte
 *  anchor at set time). Exactly one of `position` / `line` is required. */
interface ScrollbarMarker {
  position?: number;          // byte offset of the marked location
  line?: number;              // 0-based logical line (convenience)
  end?: number;               // optional end byte → range marker (diff hunks)
  color: [number, number, number] | string;  // OverlayColorSpec: RGB or "theme.key"
  priority?: number;          // default 0; highest wins per track cell
}

/** Replace this namespace's marker set for the buffer. Atomic: the old set
 *  is swapped out in the same command, so refreshes never flicker. */
setScrollbarMarkers(bufferId: number, namespace: string,
                    markers: ScrollbarMarker[]): boolean;

clearScrollbarMarkers(bufferId: number, namespace: string): boolean;
```

Design choices, and why:

- **Replace-set, not add/remove.** Every in-tree decoration consumer already
  does clear-then-rebuild per refresh (`live_diff.ts:790-794`). Making that
  the *only* write primitive removes the flicker window between the clear and
  the re-add, removes the need for handles/ids, and makes the version counter
  trivially correct. A plugin that wants incremental updates just resends the
  set — for the realistic cap (§3.3) that is cheaper than any bookkeeping.
- **Byte positions first-class, lines as sugar.** Byte offsets are the
  universal coordinate (§1.1) and what `addOverlay` already uses, so plugins
  computing overlay ranges (live_diff, search) pass the same numbers. `line`
  is converted once in the handler via `line_start_offset` (exactly what
  `handle_set_line_indicator` does at `plugin_commands.rs:2015`); on a
  not-yet-scanned large file where that returns `None` the marker is dropped
  and the failure is counted in the debug log — plugins targeting huge files
  should send bytes.
- **`OverlayColorSpec` colors.** Theme keys resolve at *render* time (the
  `OverlayFace::ThemedStyle` pattern, `view/overlay.rs:25-35`), so markers
  restyle on theme switch for free and the RGB-only mistake of
  `setLineIndicator` is not repeated.
- **Optional `end` for range markers.** A 3 000-line diff hunk in a 1M-line
  file should paint a proportional streak, not a dot. Point markers are just
  `end == position`.
- **No per-marker remove, no handles.** YAGNI under replace-set semantics;
  keeps the TS surface at two methods.

Naming follows the house conventions (§6 of the touch-point list, and
`api.rs` style): `PluginCommand::SetScrollbarMarkers { buffer_id, namespace,
markers }` / `ClearScrollbarMarkers { buffer_id, namespace }`, JS names via
`rename_all = "camelCase"`, options struct with `deny_unknown_fields` +
`#[ts(export)]`.

---

## 3. Storage: `ScrollbarMarkerManager` on `EditorState`

A new field next to `visual_row_index` (`state.rs:326`), following the
`MarginManager` shape:

```rust
pub struct ScrollbarMarkerManager {
    /// Private anchor store — markers shift with edits at O(log n) per edit,
    /// exactly like gutter indicators (margin.rs:311).
    markers: MarkerList,
    /// namespace → entries. BTreeMap so paint order is deterministic.
    namespaces: BTreeMap<String, NamespaceSet>,
    /// Bumped on every mutation; part of the projection-cache key.
    version: u64,
}

struct NamespaceSet {
    entries: Vec<MarkerEntry>,   // sorted by start marker's byte at set time
    capped: bool,                // true if the set was truncated (§3.3)
}

struct MarkerEntry {
    start: MarkerId,             // left affinity (sticks to its line on inserts above)
    end: Option<MarkerId>,       // right affinity; None for point markers
    color: OverlayColorSpec,     // resolved against the theme at paint time
    priority: i32,
}
```

### 3.1 Edit adjustment

`adjust_for_insert` / `adjust_for_delete` forwarding to the private
`MarkerList`, called from the same sites as `state.margins` (`state.rs:584`,
`:666`, `:1170`, reload/undo paths). This is the entire robustness story:
between plugin refreshes, markers stay glued to their content through
arbitrary edits, undo, and LSP workspace edits, at O(log n) per edit
regardless of marker count (lazy delta propagation in `marker_tree.rs:439`).

### 3.2 Lifecycle

- Per `(window, buffer)`, like every other decoration — two splits showing the
  same buffer share one marker set (correct: they show the same content).
- Dropped with the `EditorState` when the buffer closes.
- Plugin unload: a new `scrollbar_marker_namespaces: Vec<(BufferId, String)>`
  on `PluginTrackedState` (`quickjs_backend.rs:658`), with a compensating
  `ClearScrollbarMarkers` in the unload sweep (`:7892-7935`) — the virtual-line
  leak noted in the TODO there is a mistake this feature must not repeat.
- Both handlers set `plugin_render_requested = true` (fixing the
  `handle_set_line_indicator` omission).

### 3.3 Bounding M

A per-namespace soft cap, `MAX_SCROLLBAR_MARKERS_PER_NAMESPACE = 20_000`:
entries beyond it are dropped (the input is sorted first, so the drop is the
tail, i.e. end-of-file — matching `SearchState::MAX_MATCHES = 100_000`'s
`capped` precedent, `search_state.rs:39`). Rationale: the track has ≤ ~200
cells, so 20k markers already oversamples every cell by two orders of
magnitude; the cap exists purely to bound set-time cost (O(M log M) sort +
O(M log n) anchor inserts ≈ single-digit milliseconds) and memory (tens of
bytes per marker ⇒ ~1 MiB per full namespace). `capped` is surfaced in the
debug overlay so plugin authors can see truncation.

---

## 4. Projection: markers → track cells

The heart of the design. A cached, lazily-rebuilt bucket array per scrollbar:

```rust
pub struct ScrollbarMarkerBuckets {
    key: Option<BucketKey>,
    /// One slot per track row: winning (color, priority) or empty.
    cells: Vec<Option<(OverlayColorSpec, i32)>>,
}

struct BucketKey {
    marker_version: u64,          // ScrollbarMarkerManager.version
    pipeline_inputs_version: u64, // buffer ^ soft-breaks ^ conceals ^ vtext (line_wrap_cache.rs:139)
    track_height: u16,
    regime: Regime,               // A / B / C — must match the thumb's choice
    basis_total: u64,             // total visual rows / lines / bytes for that regime
}
```

### 4.1 Sharing the thumb's denominators — by construction, not convention

`render_scrollbar` (`scrollbar.rs:174`) already receives `(total_lines,
top_line)` from `scrollbar_line_counts`, which encodes the regime choice. The
projection function takes **the same values as arguments**:

```rust
fn project_markers(
    mgr: &ScrollbarMarkerManager,
    buckets: &mut ScrollbarMarkerBuckets,   // cached on EditorState
    regime: Regime,          // derived from (total_lines, buffer_len, threshold) —
    basis_total: u64,        //   the exact inputs render_scrollbar already has
    track_height: usize,
    line_of_byte: impl Fn(usize) -> usize,  // regime-appropriate lookup, see below
) 
```

so it is *structurally impossible* for markers and thumb to use different
math. Per marker, the coordinate is:

- **Regime C (huge files):** `row = marker_byte * H / buffer_len`. O(1) per
  marker, exact, needs no line data at all. This is why byte anchoring makes
  the feature work on a 1 GB unscanned file on the frame it opens.
- **Regime B:** `row = get_line_number(marker_byte) * H / total_lines`. On
  loaded ≤ 10 MiB buffers every chunk has `line_starts`, so this is a genuine
  O(log n) `partition_point` per marker — the expensive intra-leaf scan only
  arises in large-file mode, which is regime C by definition.
- **Regime A:** `row = visual_row_index.line_for_byte(marker_byte) → line_first_row`,
  O(log N) over in-memory prefix sums. The index is already built this frame
  by `scrollbar_visual_row_counts` (`scrollbar.rs:123`); the projection *must
  not* call `ensure_built` itself outside this regime — that is the fresh#2610
  trap, and regime A's ≤ 5 000-line bound keeps the shared rebuild cheap.

Range markers project both endpoints and fill `[row_start, row_end]`.
Per-cell aggregation: highest `priority` wins; ties broken by namespace
order (BTreeMap iteration) — deterministic across frames.

Note on geometry: markers map content-proportionally across the full track
(`coord / total * H`), while the thumb maps *scroll offset* onto
`H − thumb_size`. These coincide exactly — a marker on the first visible line
lands on the thumb's top row and one on the last visible line on its bottom
row — except when the thumb-size clamps (min 1 row, max 80% of track,
`scrollbar.rs:210-215`) engage. That residual ±1-row disagreement at extreme
ratios is accepted; it is invisible at cell resolution and identical to how
GUI editors behave.

### 4.2 Cost model

| Event | Work | Complexity |
|---|---|---|
| Steady-state frame (no changes) | key comparison | O(1) |
| Marker set replaced / edit / resize | one lazy rebuild at next render | O(M · log n + H) |
| Buffer edit (marker shift) | interval-tree delta | O(log n) |
| Paint | read `cells[row]` inside the existing per-row loop | O(H), unchanged |

The rebuild is bounded by the cap: 20 000 markers × ~17 tree levels ≈ 340k
operations, well under a millisecond, and it happens at most once per frame
because it is driven from the render path, not from the command handler.
Crucially there is **no term proportional to file size in any row** — the
tiny-file and huge-file paths differ only in which O(1)/O(log) lookup maps a
byte to a ratio.

Multi-split: two splits of the same buffer usually share `track_height`, hence
the same key. For unequal heights, `ScrollbarMarkerBuckets` holds a small FIFO
of up to 4 keyed entries (the `LineWrapCache` pattern in miniature,
`line_wrap_cache.rs:174-180`) so split layouts don't thrash the cache.

---

## 5. Painting

### 5.1 Terminal

Inside the existing per-row loop (`scrollbar.rs:233-244`), after the bg
(track or thumb) is chosen:

```rust
if let Some((color, _prio)) = buckets.cells[row] {
    // Half-block glyph: marker occupies the left half of the cell as fg,
    // the track/thumb bg stays visible in the right half — marker and
    // thumb position remain simultaneously legible in one column.
    cell = Paragraph::new("▌").style(Style::default()
        .fg(resolve(color, theme))    // theme keys resolve here, per §2
        .bg(bg));
}
```

The `▌` half-block is the elegant answer to the one-column budget: no second
column, no hiding the thumb under markers, no hiding markers under the thumb.
(Fallback for the `ascii_only` rendering mode: paint the cell bg with the
marker color on track rows and keep the thumb bg on thumb rows.)

Precedence within a cell mirrors the gutter chain (`gutter.rs:53-125`):
hover highlight > marker glyph fg > thumb/track bg.

### 5.2 Theme + inspector

`record_scrollbar_theme_runs` (`orchestration/mod.rs:1064`) gains a
`"Scrollbar Marker"` region emitting the marker's theme key (or `rgb(...)`)
per marked row, keeping the theme inspector and screenshot-diff tooling
truthful.

### 5.3 Web UI

The pane object in the scene JSON (`webui/mod.rs:1715`) gains:

```json
"vscrollMarkers": [ { "row": 14, "color": "#50c878" }, ... ]
```

resolved to concrete hex at scene-build time (the web UI has no theme-key
resolver). The frontend (`web-ui/js/30-render.js:52-56`) appends
absolutely-positioned half-width divs to the existing `.scrollbar` region —
same treatment as the thumb, ~6 lines of JS + a CSS rule per theme file.

---

## 6. Implementation touch points (checklist)

Mandatory, in dependency order:

1. **Factor the thumb formula** out of `scrollbar.rs:202-225` and
   `scrollbar_math.rs:188-197` into one shared helper *before* adding a third
   consumer (both agents' reports flagged this duplication independently).
2. `PluginCommand::{SetScrollbarMarkers, ClearScrollbarMarkers}` +
   `ScrollbarMarker` options struct in `crates/fresh-core/src/api.rs`
   (serde/TS derives per house style, `FromJs` via `rquickjs_serde`).
3. QuickJS bindings in `quickjs_backend.rs` (`JsEditorApi` impl block),
   including `PluginTrackedState.scrollbar_marker_namespaces` + unload sweep.
4. Dispatch arms (`plugin_dispatch.rs:376` match) and handlers in
   `plugin_commands.rs` (validate, sort, cap, anchor; set
   `plugin_render_requested`).
5. `ScrollbarMarkerManager` + `ScrollbarMarkerBuckets` on `EditorState`;
   adjust hooks at the `state.margins` call sites.
6. Projection + paint in `split_rendering/scrollbar.rs`; theme runs; scene
   JSON field; web-ui rendering.
7. Regenerate `fresh.d.ts` (`cargo test -p fresh-plugin-runtime
   write_fresh_dts_file -- --ignored --nocapture`) and run the plugin
   type-check test; new types added to `ts_export.rs`'s `get_type_decl`.
8. Docs: `docs/plugins/api/` page + `docs/internal/plugins.md` command table.

Testing: unit tests for the projection math per regime (including the regime
boundaries at 5 000 lines / 2 MiB / 10 MiB and empty-buffer/EOF clamping);
a semantic test that markers track edits (insert above a marker, assert its
row moved with the thumb basis); an e2e render test in the mold of
`issue_1554_scrollbar_theme_color.rs`; a large-file test asserting the
byte-ratio path never touches line APIs (no `ensure_built`, `line_count()`
still `None` after render).

## 7. Phasing

- **Phase 1** (the issue's ask): API + storage + terminal rendering, regimes
  A/B/C. Ships value to the issue author immediately.
- **Phase 2:** web-UI scene field + theme-inspector runs.
- **Phase 3 (opt-in follow-ups):** built-in producers — search matches
  (`SearchState.matches` is already a sorted, capped byte vector — a perfect
  feed), diagnostics, and diff-since-saved; hover tooltips; click-to-jump
  affordance (clicks already land proportionally via the existing
  `scrollbar_input.rs` math, so markers are click-navigable for free at cell
  resolution).

## 8. Alternatives considered and rejected

- **Derive markers automatically from overlays** (zero new API): rejected —
  `query_viewport` is viewport-scoped and its namespace filter is already
  O(total overlays) per frame (`overlay.rs:637-639`, the current worst
  per-frame term with dense search results); overlays lack any "show on
  scrollbar" bit; and plugins legitimately want the surfaces decoupled (a
  faint line tint but a loud scrollbar mark, or vice versa).
- **Extend `setLineIndicator` with a scrollbar flag**: rejected — inherits the
  RGB-only positional-args shape (§1.4), couples two surfaces that update at
  different cadences, and line-number input is undefined on unscanned large
  files.
- **Line-number-keyed storage**: rejected — desyncs on the first edit unless
  it reimplements marker adjustment; and line numbers don't exist in regime C.
- **Per-frame projection without a cache**: rejected — O(M log n) per frame
  per split is measurable at the cap; the version-keyed cache reduces the
  steady state to O(1) using the exact idiom (`VisualRowIndexKey`,
  `pipeline_inputs_version`) the codebase already trusts.
- **A second scrollbar column (dedicated marker lane)**: rejected — costs a
  text column in a terminal where width is precious, and the half-block glyph
  achieves two-lanes-in-one-cell.
