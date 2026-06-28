# Rendering & Layout

> _AI-generated: describes Fresh's architecture and design rationale, not implementation details; where it disagrees with the source, the source is authoritative._

Purpose: how Fresh turns buffer bytes into terminal cells — the per-frame render loop, the token→`ViewLine` pipeline, the line-wrap and visual-row caches that make huge files scroll cheaply, folding/wrapping/conceal/virtual-text decorations, split-pane layout, the `Scene` semantic projections shared with the web frontend, and mouse hit-testing.

---

## 1. The render loop

Entry point: the editor's `render` method, taking a ratatui `Frame`.

Fresh is an **immediate-mode** TUI: there is no retained widget tree and no dirty-rectangle diffing inside the editor. Every frame re-derives the full screen from editor state. ratatui resets its back-buffer before each draw, then crossterm diffs back-buffer vs front-buffer and emits only the changed cells — so the terminal write is incremental even though the drawing is not. The editor does **not** decide *when* to redraw; the outer event loop (in the runtime/lifecycle layer) calls `render` after input, async messages, animation ticks, or resize.

`render()` flow:
1. Drain pre-layout plugin commands; sync terminal titles; carve a left dock column so the orchestrator dock sits left of all chrome.
2. Snapshot the previous frame for animations (ratatui clears the live buffer, so the animation runner keeps its own post-apply clone).
3. Record the last frame's width/height and reset the per-cell theme map.
4. Run scroll-sync groups, request semantic ranges for visible splits, prepare visible buffers.
5. Build the vertical chrome layout with ratatui `Layout` constraints: menu bar, main content, status bar, search options, prompt line. Heights collapse to 0 when a region is hidden (e.g. status bar hidden under a suggestions popup; prompt row reclaimed for overlay prompts).
6. Carve the file-explorer sidebar out of the main content area and paint it.
7. Fire `lines_changed` plugin hooks for newly-visible lines (lets plugins add overlays *before* the content render).
8. **Render split content**: a single split-borrow of the active window yields the buffers, the split manager, and the view states, and the split renderer paints every visible leaf into the frame buffer. It returns per-leaf layout caches (split areas, tab layouts, view-line mappings, scrollbar areas, …) stored on the window for the next frame's hit-testing.
9. Post-content passes: cursor-jump animation, viewport-change hooks, popups, modals, menu bar, status bar, context menus, software cursor, dock/overlay painting, then dimming behind modals.

### `RenderStyle`, `EditorRenderConfig`, and rendering into an arbitrary buffer

A sequence of render refactors tightened the render seam, with the goal of making the split renderer reusable and keeping borrow scopes narrow:

- **`RenderStyle`** bundles the theme, an ANSI-background flag, and an `EditorRenderConfig` — the "how to render" group threaded *by reference* so the painters don't re-list a large parameter list. It is built *inside* the split-borrow closure so the theme read-guard is released before the post-render `&mut self` chrome updates.
- **`EditorRenderConfig`** is the immutable copy of the editor config flags — settings only, no buffers or geometry — so it copies freely.
- **Rendering into an arbitrary buffer**: the split renderer and its whole callee tree only ever used the frame via widget rendering / buffer access, never the hardware cursor (deferred and applied later). They now take a mutable ratatui `Buffer` directly, so the split renderer is composable into any buffer — offscreen previews (the "phantom leaf"), tests, and the web bridge — and is decoupled from a live terminal draw.

### Chrome layout cache

The chrome layout is the per-frame, screen-indexed hit-test/projection cache: popup areas, suggestions area, status-bar layout, menu layout, settings/trust-dialog layouts, the last frame's dimensions, and the flat cell-theme map (theme-key provenance per cell, indexed by row and column). The status-bar fields are grouped into a `StatusBarChrome` and the frame dimensions into a `FrameDimensions` value. Per-*window* layout (split-leaf rects, tab rects, view-line mappings) lives on the window layout cache, not here.

---

## 2. The Scene abstraction

The `Scene` is **not** a retained scene-graph. It is the set of **semantic UI projections** — the single source of truth for *what* the chrome is, computed once in the core and consumed by every frontend. The guiding principle: the TUI and the web/GUI must not re-implement the same logic. Everything *semantic* — which menus exist, which items are enabled/checked, accelerators, which menu is open, tab labels, status segments, palette suggestions, popups, the file-explorer tree, settings/keybinding modals — is derived **once** as `Serialize`-deriving view structs whose field names match the JSON the browser already consumes. A frontend then only does the rendering (model → cells for the TUI; model → HTML for web) and the input bridge (crossterm vs DOM → the shared key/mouse handlers).

Each projection is an editor method that reads editor state *plus the last frame's geometry caches*:
- `menu_view` — menu tree + open/highlight + dropdown rects from the chrome menu layout.
- `tab_bar_view`, `status_view` (reads the status-bar chrome segments — no cell scraping), `palette_view`, `popups_view`, `file_explorer_view`, `trust_dialog_view`, `widgets_view`, `context_menu_view`, `aux_modals_view`, `keybinding_editor_view`, `settings_view`.

Geometry (x/width, rects) comes from the pipeline's layout caches populated *during render*, so the projection reflects the most recent frame; clicks routed back via the mouse handler at those cells hit the same hit-tester the TUI uses. The keybinding editor and Settings ship *full* native models (every control kind); the rendered cells stay as a fallback.

---

## 3. The per-line render pipeline (token IR)

The universal intermediate representation is a flat vector of view tokens. Buffer text, plugin view-transforms, conceals, soft breaks, inlay hints, and wrapping all operate on this one stream before it crystallizes into `ViewLine`s. Pipeline shape:

```
buffer → build_base_tokens() → token stream
       → [plugin transform substitutes its own token vec]
       → apply_soft_breaks / apply_conceal_ranges / splice_inline_virtual_text
       → apply_wrapping_transform (inserts Break tokens)
       → ViewLineIterator → ViewLines (one per display row)
       → inject_virtual_lines / apply_folding → render
```

The driver is `build_view_data` — the **canonical order** (this driver is the source of truth; the doc-comment order in the transforms module is aspirational):
1. Fold accounting → fold-skip set.
2. `build_base_tokens` (fold-skipped) — or replaced wholesale by a plugin view transform's tokens if one is active.
3. `apply_soft_breaks` (Compose/PageView mode, non-empty soft breaks).
4. `apply_conceal_ranges` (whenever conceals are non-empty — the old compose-only gate was removed because it broke flash-style source-mode conceals).
5. `splice_inline_virtual_text` — **before** wrapping so inlay-hint width participates in wrap boundaries, the visual-column map, and horizontal scroll (one canonical cell layout).
6. `apply_wrapping_transform` — the effective width is the wrap column clamped to content width, less one column when wrap is on (reserving a column so the EOL cursor never lands on the scrollbar); when wrap is off it is the maximum safe line width.
7. Tokens → `ViewLine`s via the view-line iterator (ANSI-aware unless binary; fold-skip applied at the iterator level as defense-in-depth).
8. **Line-wrap cache writeback** (see §4).
9. `inject_virtual_lines` (LineAbove/LineBelow plugin rows).
10. `apply_folding` (final placeholder collapse).

### `ViewLine` — the render-ready row

A `ViewLine` carries the display text plus three parallel **per-character** maps (source bytes, styles, visual columns) and an inverse **per-visual-column** map. These give O(1) bidirectional mapping in both directions — source-byte-at-visual-column and char-at-visual-column — which is the basis of mouse hit-testing and cursor placement. It also carries tab starts, a newline flag, virtual gutter glyph / virtual line style (so an empty deletion virtual line can still be background-striped), and a `LineStart` tag.

`LineStart` tags how each row began — `Beginning`, `AfterSourceNewline`, `AfterInjectedNewline` (plugin virtual line), `AfterBreak` (wrap continuation). Only `AfterBreak` is a continuation, so wrapped sub-rows get no gutter line number.

### Base tokens

`build_base_tokens` produces text, space, newline, break, and binary-byte tokens. Notable decisions: contiguous text is coalesced; a break is force-inserted at the maximum-safe-line-width interval to bound memory on pathological lines; CRLF is collapsed (a stray carriage return in an LF file renders as a hex escape); unsafe control bytes render as binary bytes for *all* files (terminal-corruption guard); folds are segmented with a *fresh line iterator per inter-fold segment*, so collapsed source bytes are never read, decoded, or tokenized.

### Char styling — the perf-critical inner loop

The per-character style computation layers token → ANSI → syntax → semantic → overlays → selection → cursor, while tracking theme-key provenance for the cell-theme map. A render-pipeline performance pass flagged the original form — a per-cell linear scan of the full viewport-overlay slice plus a per-cell heap allocation of an overlay reference vector — at roughly a fifth of total CPU. **This is implemented**: char styling now takes a pre-computed slice of active overlays, built once per line by an advancing sweep over a sorted overlay-position index (with the overlay and selection active-sets extracted into dedicated sweep types). No per-cell allocation; no per-cell rescan.

---

## 4. Line-wrap cache (tier 1)

A bounded **per-buffer** cache from a line-wrap key → a shared, reference-counted vector of `ViewLine`s, the *exact pipeline output* for one logical line. Design rationale:

- **Single source of truth.** Every consumer that needs "how many visual rows?", "what byte at visual col N?", etc. reads the same `ViewLine` methods. No second wrap implementation to drift from. The old char-width line-wrap and char-position-to-segment helpers were **deleted** for exactly this reason — the line-wrapping primitive now holds only the wrap-config geometry.
- **Two writers, one pipeline.** The renderer populates entries as a side effect of its per-frame work (the view-data writeback); the miss handler runs the *same* pipeline scoped to one line. Hit and miss are indistinguishable to the caller.
- **Invalidation by key, no active invalidate step.** The line-wrap key combines a pipeline-inputs version with every geometry/view input (view mode, line start, effective width, gutter width, wrap column, hanging indent, wrap enabled flag). The pipeline-inputs version packs the buffer version together with the soft-break, conceal, and virtual-text versions via shifted XOR into one word. Any input change → different key → old entries unreachable, aged out by eviction.
- **Byte-budget FIFO eviction.** Entry sizes vary wildly (a few hundred bytes up to megabytes for a very long line wrapping to thousands of rows), so count-based eviction is wrong. The cache tracks approximate total bytes and evicts oldest-first when an insert would exceed a byte budget (a few megabytes), always keeping at least the new entry. **FIFO not LRU** because the dominant pattern is sequential scrolling — a line is queried a few times in close succession then rarely again. The map/order length invariant and the byte-budget bound hold after every insert.

A buffer-free variant exists for sites that have a string in hand (e.g. cursor-screen-position math) — it matches the renderer's word-boundary wrap on the same text and geometry. A wrap-geometry value carries the geometry and builds the per-line key.

### Scalability to huge files

The cache holds only the *visible* span plus whatever scroll/cursor math recently touched, bounded to the byte budget regardless of file size. Off-screen lines are never materialized into `ViewLine`s unless a consumer asks. The base-token fold path and the maximum-safe-line-width break cap further bound work on pathological lines. Whole-buffer questions are answered by the tier-2 index (next section) without materializing every line.

---

## 5. Visual-row index (tier 2)

A whole-buffer index sitting on the editor state *over* the line-wrap cache. Where tier 1 answers per-line questions, this answers whole-buffer questions in O(1)/O(log N):
- total rows — O(1).
- first row of a line / row count of a line — O(1).
- position at a row → (line index, line start byte, offset in line) — O(log N) via binary search.
- line for a byte → (line index, line start byte) — O(log N).

Storage: two parallel vectors of one-more-than-line-count entries — prefix sums (cumulative visual rows of all lines before index `i`, last entry = total) and line starts (byte offset of each line, last = buffer-length sentinel).

**Why it exists**: three consumers were each folding the per-line cache into a whole-buffer answer *per call* — scrollbar drag's visual-row-map build (per mouse-move), the per-frame scrollbar visual-row counts, and the wrapped scroll-up walk in ensure-visible (per keystroke). Profiling put the first two at the overwhelming majority of CPU during scrollbar drag on large buffers (line-wrap key hashing, per-event view-line iteration, and reallocation churn of the per-call result vectors all contributing). The index replaces all three O(N_lines) folds.

**Population & invalidation**: keyed on a visual-row-index key — the same geometry as the line-wrap key minus the line start. On a tier-1 hit it reads the entry length for free; on a miss it runs a *count-only* path — wrap and tally, skipping `ViewLine` materialization — and **does not write back** into tier 1 (avoids the per-line allocation the profile flagged; the renderer fills the real layout when the line becomes visible). It also pre-fetches buffer-wide soft breaks and virtual-line positions once and slices per-line with binary search, because soft breaks and plugin virtual lines (compose-mode table borders, git-blame headers) add rows that the scrollbar / page-down / mouse-wheel maximum-scroll-row computation must include or the buffer tail becomes unreachable.

---

## 6. Folding

Folds are tracked as **byte-offset marker pairs** in the shared marker list, so they auto-adjust on edits without manual reshifting (this fixed a fold-indicator lag bug).

- A fold range carries a start marker (left affinity), an end marker (right affinity), and a placeholder. The fold manager owns the list of ranges.
- Resolution reads current marker bytes, drops invalid entries (end at or before start, header at the buffer top, …), converts bytes→lines, and computes the header line as the line before the start. A collapsed-header map (header byte → placeholder) is what the renderer collapses against; a hidden-line count sums collapsed logical lines in a range.
- Logical→visual collapse is applied two ways: at the *token* level (`build_base_tokens` skips folded source bytes via a sorted fold-skip range set, and the view-line iterator re-applies it as defense-in-depth) and at the *placeholder* level (`apply_folding`).
- **LSP** folds live in a separate marker-backed store; when absent, an **indent-based** fallback detects foldable lines (next non-blank line more indented) and computes fold-end bytes by scanning forward. A subtle decision: a newline is treated as a line *terminator*, not content, so a bare newline reads as blank — getting this wrong makes blank lines masquerade as fold headers.

No version counter: correctness comes from markers auto-adjusting and from filtering invalid resolved ranges on every query.

---

## 7. Wrapping, conceal, soft breaks, virtual text

All four are decorations anchored to marker-list byte offsets; soft breaks, conceals, and virtual text additionally carry a monotonic version counter folded into the pipeline-inputs version.

- **Wrapping**: multi-strategy greedy soft-wrap — inter-token word-wrap, grapheme char-wrap for over-wide tokens, and within char-wrap a preference for a UAX-#29 word boundary inside a bounded lookback window (avoids mid-identifier splits), with a hard grapheme cap fallback guaranteeing forward progress (a double-width glyph in a one-column viewport still emits on its own row). A standalone string-wrap helper mirrors this exactly and shares the same lookback window so virtual-line wrap and source-line wrap agree. A back-up-to-prior-space step moves a trailing word to the next row to avoid stranded leading spaces.
- **Soft breaks**: plugin-injected break points with a hanging indent, applied *before* conceal and wrapping. The manager keeps a marker-to-index side-index for O(log N + k) range removal (marker-tree query → map to indices → descending swap-remove).
- **Conceal**: hide/replace byte ranges (the "seamless canvas" markdown experience). A conceal range has both endpoints as markers. Documented limitation: range removal can't detect a conceal that *fully spans* the query range (the marker query only finds endpoints *inside* it).
- **Virtual text**: inlay hints, git-blame headers, fold previews, diff-removal lines. A virtual-text position is either before/after a char (inline, spliced into the token stream) or line-above/line-below (full rows injected as `ViewLine`s). Foreground/background theme keys are resolved live each render so injected text follows theme changes. Discrepancy flagged: the add-with-id-and-theme-keys path omits the version bump its sibling add methods call — a latent cache-staleness bug if that path is hit.

---

## 8. Margins / gutter and dimming

- **Gutter**: width is the digit count of the total line number, clamped to a minimum number of digits (it tracks the actual digit count rather than a fixed minimum). Layout is a one-column indicator slot, the line number, then a separator. Gutter indicators (git status, diagnostics, breakpoints, fold markers) are marker-anchored in the margin manager's *own* marker list; the viewport indicator query touches only the viewport byte range and keeps the highest-priority indicator per line. In compose mode with line numbers off, the gutter stays enabled with zero width so the one-column indicator slot survives.
- **Dimming**: a pure post-process effect over the rendered cell buffer — darken cells behind a modal (a brightness reduction applied per channel), skipping the modal's own rect. No markers, no types — it runs last, directly on the frame buffer.

---

## 9. Splits and composite views

### Split tree

Emacs-style window system: a binary tree of panes, with rects computed by recursively bisecting a root rect.

The split-node enum:
- **Leaf** — one buffer (buffer id, leaf id, optional role — a utility dock is a tagged singleton).
- **Split** — internal node: direction, first/second children, a ratio (first child's fraction, clamped to a sub-unit range), plus optional fixed sizes for absolute sizing.
- **Grouped** — a subtree appearing as a *single tab* in its parent; expanded inline only when its tab is active.

Layout primitive: the split-rect computation reserves **exactly one row/column for the separator**, sizes the first child by its fixed size, else from the second's fixed size, else from the rounded ratio, and gives the remainder to the second. The tree walk returns leaf/buffer/rect triples; inactive grouped subtrees return empty. A separators walk yields divider geometry for borders and mouse hit-testing.

The split manager owns the root, the active leaf, a maximized-leaf marker, and a bounded focus-history LRU. Splitting pre-allocates IDs then replaces a leaf with a new split; closing replaces a parent with its surviving sibling; next/previous-split navigation clears maximize first to avoid focusing a hidden leaf.

Per-view state also lives here: a per-buffer-per-split view state (cursors, viewport, view mode, folds, and override flags recording user intent separately from rendered truth) and a per-window split view state (active buffer, keyed states, open tabs, and a cached layout with a dirty flag). The split view state dereferences to the active buffer view state. **Layout caching** (a ViewModel pattern): the layout is rebuilt only when absent or marked dirty.

### Composite views

For a composite buffer (side-by-side diff) rendered as multiple aligned panes inside *one* split. Unlike the split manager, panes here share a single scroll row and scroll together via row alignment. Pane-width distribution divides the content (less separators) by ratio, absorbing rounding into the last pane; pane-rect computation walks left-to-right with separator gaps. A sticky column preserves desired column across vertical nav; a scroll margin drives auto-scroll.

### Split-rendering module organization

A refactor decomposed a single large content-render function into a module split into two tiers, deliberately visible in the directory listing:
- **Self-contained leaves** — base tokens, transforms, style/char-style, view data, folding, gutter, scrollbar, layout, post-pass, spans — depend on no shared render-time carrier.
- **Orchestration** — the only code touching the shared selection/decoration context structs, quarantined so the coupling is obvious. Holds the buffer/composite/line renderers, overlays, the overlay and selection sweeps, and tail fill.

The split renderer itself is an empty façade forwarding to the orchestration layer; entry points are content rendering, content-layout computation, and phantom-leaf rendering (a buffer painted into an arbitrary off-tree rect — e.g. the live-grep preview).

---

## 10. Viewport and scrolling

The viewport holds the top byte, a top-view-line offset (visual-row offset into the first visible logical line — supports mid-line scroll under wrap), a horizontal scroll offset, a left column, and a scroll-offset margin. Effective width and gutter width feed wrap geometry.

Scrolling is **byte-oriented** (a top byte) rather than line-indexed so it survives edits. The keystroke-time scroll defers fine wrap adjustments to a render-time pass that knows the real content-area dimensions and the materialized view lines. Several skip flags coordinate the two so a scroll action isn't immediately undone by the keystroke-time scroll. Cursor-screen-position math counts visual rows (not logical lines) from the top byte to the cursor's line — using the renderer's word-boundary wrap via the buffer-free wrap helper — so popups anchor to the correct screen row in wrapped buffers. The maximum-scroll-row and scrollbar thumb sizing now read the tier-2 visual-row index instead of re-folding the per-line cache.

A synchronized-scrolling layer coordinates scrolling across panes (diff side-by-side).

---

## 11. Mouse hit-testing

Render writes per-leaf view-line mappings (built in the line renderer, returned from content rendering, cached on the window). A view-line mapping is the per-visual-row slice of a `ViewLine`'s maps.

Screen-to-buffer-position conversion turns a (column, row) into a buffer byte:
1. Adjust the content rect for compose centering and the compose gutter-reclaim shift.
2. Subtract the gutter; a gutter click returns nothing (click handler) or position 0 (drag handler) depending on whether gutter clicks are allowed.
3. Index the mapping for the visual row and look up the source byte at the visual column — the **O(1)** visual-to-char then char-source-byte lookup. Columns landing on virtual/injected content walk left to the nearest real byte; clicks past end-of-line clamp to the line-end byte; clicks below the last line use the last mapping.

Fold gutter-clicks route through fold-toggle resolution — checks collapsed headers, then LSP fold ranges (marker-resolved to current lines), then the indent-fold fallback. A planned design unifies hit-testing with the per-cell theme map (used by the theme inspector and the web bridge).

---

## 12. Unicode / grapheme width

Width is centralized so the editor, plugins, and wrap all measure identically.
- Char-width and string-width helpers are re-exported from the core crate — the single source of truth, also exposed to the plugin runtime. CJK and most emoji are double-width; control and zero-width characters are zero-width. Byte↔visual-column helpers live alongside.
- Grapheme-cluster navigation uses Unicode segmentation (UAX #29): previous/next grapheme boundary, grapheme-at, grapheme-count. Editing and cursor movement operate on grapheme clusters (Thai base+combining, ZWJ emoji, combining diacritics count as one user-perceived character).
- A lower-level visual-layout primitive builds per-line mappings handling ANSI escapes (zero width), tabs (expand to the next tab stop), and double-width and zero-width chars, with fast paths when no escape or tab is present. This is the lower-level analogue the `ViewLine` maps build on.

---

## Implemented vs planned

**Implemented:** the token IR pipeline and `ViewLine`; the tier-1 line-wrap cache (FIFO byte-budget); the tier-2 visual-row index; marker-backed folding/soft-break/conceal/virtual-text with version-keyed invalidation; the `Scene` semantic projections (menu/status/tabs/palette/popups/file-explorer/trust/widgets/keybinding/settings); the `RenderStyle`/`EditorRenderConfig` seam and rendering into an arbitrary buffer; the status-bar / frame-dimensions chrome grouping; the decomposition of content rendering into the orchestration layer; the per-cell-overlay performance optimization (pre-computed active-overlay sweep); split tree and composite views; O(1) visual-to-char hit-testing.

**Planned / aspirational (per source docs, not in code):** the transforms-module doc-comment pass order is *not* the executed order (the view-data driver is authoritative). The unified-scene design is largely realized — the keybinding editor and Settings ship full native models. (The auxiliary-modals projection deliberately omits the keybinding editor — it's Settings-grade, not a line list — but the dedicated keybinding-editor projection covers it.) A scrolling highlight cache is described as a design layered on the same versioning idea — verify against the highlight engine before relying on it as current.

**Discrepancy flagged:** the virtual-text add-with-id-and-theme-keys path does not bump its version like its siblings — a latent stale-cache risk; fully-spanning conceals are documented as not removable by range removal.
