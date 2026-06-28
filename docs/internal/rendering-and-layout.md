# Rendering & Layout

Purpose: how Fresh turns buffer bytes into terminal cells — the per-frame render loop, the token→`ViewLine` pipeline, the line-wrap and visual-row caches that make huge files scroll cheaply, folding/wrapping/conceal/virtual-text decorations, split-pane layout, the `Scene` semantic projections shared with the web frontend, and mouse hit-testing. All `path:line` references are against the tree at the time of writing; treat the code as authoritative where this doc and an older plan disagree.

---

## 1. The render loop

Entry point: `Editor::render(&mut self, frame: &mut Frame)` — `app/render.rs:59`.

Fresh is an **immediate-mode** TUI: there is no retained widget tree and no dirty-rectangle diffing inside the editor. Every frame re-derives the full screen from editor state. ratatui resets its back-buffer before each draw, then crossterm diffs back-buffer vs front-buffer and emits only the changed cells — so the terminal write is incremental even though our drawing is not. The editor does **not** decide *when* to redraw; the outer event loop (in the `runtime` crate / `app/lifecycle.rs`) calls `render` after input, async messages, animation ticks, or resize.

`render()` flow (`app/render.rs:59`+):
1. Drain pre-layout plugin commands; sync terminal titles; carve a left dock column (`compute_dock_split`, `render.rs:75`) so the orchestrator dock sits left of all chrome.
2. Snapshot the previous frame for animations (`render.rs:81` — ratatui clears the live buffer, so the animation runner keeps its own post-apply clone).
3. Record `last_frame.{width,height}` and reset the per-cell theme map (`render.rs:84`, `reset_cell_theme_map` at `app/types/layout.rs:200`).
4. `pre_sync_and_scroll_sync` (scroll-sync groups), request semantic ranges for visible splits, prepare visible buffers.
5. Build the vertical chrome layout with ratatui `Layout` constraints: `[menu_bar, main_content, status_bar, search_options, prompt_line]` (`render.rs:152`). Heights collapse to 0 when a region is hidden (e.g. status bar hidden under a suggestions popup; prompt row reclaimed for overlay prompts).
6. Carve the file-explorer sidebar out of `main_content` (`split_file_explorer_area`, `render.rs:199`) and paint it.
7. Fire `lines_changed` plugin hooks for newly-visible lines (lets plugins add overlays *before* the content render — `render.rs:207`).
8. **Render split content** (`render.rs:579`): a single `with_all_mut` split-borrow of the active window yields `(&mut buffers, &SplitManager, &mut view_states)`, and `SplitRenderer::render_content` paints every visible leaf into `frame.buffer_mut()`. Returns per-leaf layout caches (`split_areas`, `tab_layouts`, `view_line_mappings`, scrollbar areas, …) stored on the window for the next frame's hit-testing.
9. Post-content passes: cursor-jump animation, viewport-change hooks, popups, modals, menu bar, status bar, context menus, software cursor, dock/overlay painting, then dimming behind modals.

### `RenderStyle`, `EditorRenderConfig`, and `Frame → &mut Buffer`

Recent refactors tightened the render seam:

- **`RenderStyle`** (`view/ui/split_rendering/mod.rs:114`) bundles `{ theme: &Theme, ansi_background, cfg: EditorRenderConfig }` — the "how to render" group threaded *by reference* so the painters don't re-list ~16 params (commit `8a52e557e`). It is built *inside* the `with_all_mut` closure so the `self.theme` read-guard is released before the post-render `&mut self` chrome updates (`render.rs:582`).
- **`EditorRenderConfig`** (`mod.rs:54`) is the immutable copy of `config.editor.*` flags — "settings only, no buffers/geometry" — so it copies freely (commit `8c9458864`).
- **`Frame → &mut Buffer`** (commit `844cf02a9`): `render_content` and its whole callee tree only ever used `frame` via `render_widget`/`buffer_mut()`, never the hardware cursor (deferred via `pending_hardware_cursor`). They now take `&mut ratatui::buffer::Buffer`, so the split renderer is composable into any buffer — offscreen previews (the "phantom leaf", `mod.rs:256`), tests, and the web bridge — and is decoupled from a live terminal draw.

### Chrome layout cache

`ChromeLayout` (`app/types/layout.rs:87`) is the per-frame, screen-indexed hit-test/projection cache: popup areas, suggestions area, status-bar layout, menu layout, settings/trust-dialog layouts, `last_frame: FrameDimensions` (`layout.rs:146`), and the flat `cell_theme_map` (theme-key provenance per cell, indexed `row*width+col`). The status-bar fields were grouped into `StatusBarChrome` (`layout.rs:155`, commit `3b27d98df`) and the frame dims into `FrameDimensions` (commit `94350e4bc`). Per-*window* layout (split-leaf rects, tab rects, `view_line_mappings`) lives on `WindowLayoutCache`, not here.

---

## 2. The Scene abstraction

`view/scene.rs` is **not** a retained scene-graph. It is the set of **semantic UI projections** — "the single source of truth for *what* the chrome is, computed once in the core and consumed by every frontend" (`scene.rs:1`). The guiding principle (`docs/internal/UNIFIED_SCENE_DESIGN.md`): the TUI and the web/GUI must not re-implement the same logic. Everything *semantic* — which menus exist, which items are enabled/checked, accelerators, which menu is open, tab labels, status segments, palette suggestions, popups, the file-explorer tree, settings/keybinding modals — is derived **once** in `scene.rs` as `Serialize`-deriving view structs whose field names match the JSON the browser already consumes. A frontend then only does the rendering (model → cells for the TUI; model → HTML for web) and the input bridge (crossterm vs DOM → the shared `handle_key`/`handle_mouse`).

Each projection is an `impl Editor` method that reads editor state *plus the last frame's geometry caches*:
- `menu_view()` (`scene.rs:178`) — menu tree + open/highlight + dropdown rects from `chrome.menu_layout`.
- `tab_bar_view(leaf)` (`scene.rs:337`), `status_view()` (`scene.rs:369`, reads `StatusBarChrome::segments` — no cell scraping), `palette_view()` (`scene.rs:406`), `popups_view()` (`scene.rs:556`), `file_explorer_view()` (`scene.rs:603`), `trust_dialog_view()`, `widgets_view()`, `context_menu_view()`, `aux_modals_view()`, `keybinding_editor_view()`, `settings_view()`.

Geometry (`x`/`w`, rects) comes from the pipeline's layout caches populated *during render*, so the projection reflects the most recent frame; clicks routed back via `handle_mouse` at those cells hit the same hit-tester the TUI uses. The keybinding editor and Settings ship *full* native models (every control kind); the rendered cells stay as a fallback.

---

## 3. The per-line render pipeline (token IR)

The universal intermediate representation is a flat `Vec<ViewTokenWire>`. Buffer text, plugin view-transforms, conceals, soft breaks, inlay hints, and wrapping all operate on this one stream before it crystallizes into `ViewLine`s. Pipeline shape (`view/ui/view_pipeline.rs:1`):

```
buffer → build_base_tokens() → Vec<ViewTokenWire>
       → [plugin transform substitutes its own token vec]
       → apply_soft_breaks / apply_conceal_ranges / splice_inline_virtual_text
       → apply_wrapping_transform (inserts Break tokens)
       → ViewLineIterator → Vec<ViewLine> (one per display row)
       → inject_virtual_lines / apply_folding → render
```

The driver is `build_view_data(...)` in `view/ui/split_rendering/view_data.rs:33` — the **canonical order** (this file is the source of truth; the doc-comment order in `transforms.rs` is aspirational):
1. Fold accounting → `fold_skip_set`.
2. `build_base_tokens` (fold-skipped) — or replaced wholesale by `view_transform.tokens` if a plugin transform is active.
3. `apply_soft_breaks` (Compose/PageView mode, non-empty `state.soft_breaks`).
4. `apply_conceal_ranges` (whenever `state.conceals` non-empty — the old compose-only gate was removed because it broke flash-style source-mode conceals).
5. `splice_inline_virtual_text` — **before** wrapping so inlay-hint width participates in wrap boundaries, the visual-column map, and horizontal scroll (one canonical cell layout).
6. `apply_wrapping_transform` — `effective_width = wrap_column.min(content_width)` minus 1 when wrap is on (reserves a column so the EOL cursor never lands on the scrollbar); `= MAX_SAFE_LINE_WIDTH` when wrap is off.
7. Tokens → `Vec<ViewLine>` via `ViewLineIterator` (ANSI-aware unless binary; fold-skip applied at the iterator level as defense-in-depth).
8. **Line-wrap cache writeback** (see §4).
9. `inject_virtual_lines` (LineAbove/LineBelow plugin rows).
10. `apply_folding` (final placeholder collapse).

### `ViewLine` — the render-ready row

`ViewLine` (`view_pipeline.rs:32`) carries the display `text` plus three parallel **per-character** maps (`char_source_bytes`, `char_styles`, `char_visual_cols`) and an inverse **per-visual-column** map `visual_to_char`. These give O(1) bidirectional mapping in both directions — `source_byte_at_visual_col`, `char_at_visual_col` (`view_pipeline.rs:79`) — which is the basis of mouse hit-testing and cursor placement. Also: `tab_starts`, `ends_with_newline`, `virtual_gutter_glyph`/`virtual_line_style` (so an empty deletion virtual line can still be bg-striped), and `line_start: LineStart`.

`LineStart` (`view_pipeline.rs:114`) tags how each row began — `Beginning`, `AfterSourceNewline`, `AfterInjectedNewline` (plugin virtual line), `AfterBreak` (wrap continuation). Only `AfterBreak` is a continuation (`is_continuation()`, `:132`), so wrapped sub-rows get no gutter line number.

### Base tokens (`split_rendering/base_tokens.rs`)

`build_base_tokens(...)` produces `Text`/`Space`/`Newline`/`Break`/`BinaryByte` tokens. Notable decisions: contiguous `Text` is coalesced; a `Break` is force-inserted every `MAX_SAFE_LINE_WIDTH = 10_000` chars to bound memory on pathological lines; CRLF is collapsed (a stray `\r` in an LF file renders as `<0D>`); unsafe control bytes render as `BinaryByte` for *all* files (terminal-corruption guard); folds are segmented with a *fresh `LineIterator` per inter-fold segment*, so collapsed source bytes are never read, decoded, or tokenized.

### Char styling — the perf-critical inner loop

`compute_char_style` (`split_rendering/char_style.rs`) layers token → ANSI → syntax → semantic → overlays → selection → cursor, while tracking theme-key provenance for the cell-theme map. The `docs/internal/render-pipeline-perf-plan.md` plan flagged the original form — a per-cell linear scan of the full viewport-overlay slice plus a per-cell `Vec<&Overlay>` heap allocation — at ~20% of total CPU. **This is implemented**: `compute_char_style` now takes a pre-computed `active_overlays: &[&Overlay]` slice (`char_style.rs:37`), built once per line by an advancing sweep over a sorted `overlay_position_index` (`orchestration/contexts.rs:32`; `OverlayActiveSet`/`SelectionActiveSet` extracted in commits `8f526e404`, `7acdecc94`). No per-cell allocation; no per-cell rescan.

---

## 4. Line-wrap cache (tier 1)

`view/line_wrap_cache.rs` — a bounded **per-buffer** cache from `LineWrapKey` → `Arc<Vec<ViewLine>>`, the *exact pipeline output* for one logical line. Design (`docs/internal/line-wrap-cache-plan.md`):

- **Single source of truth.** Every consumer that needs "how many visual rows?", "what byte at visual col N?", etc. reads the same `ViewLine` methods. No second wrap implementation to drift from. The old char-width `wrap_line` and `char_position_to_segment` in `primitives/line_wrapping.rs` were **deleted** for exactly this reason — that file now holds only `WrapConfig` geometry (`primitives/line_wrapping.rs:1`).
- **Two writers, one pipeline.** The renderer populates entries as a side effect of its per-frame work (`view_data.rs` writeback, `:207`); the miss handler `layout_for_line` (`line_wrap_cache.rs:325`) runs the *same* four-step pipeline scoped to one line. Hit and miss are indistinguishable to the caller.
- **Invalidation by key, no active invalidate step.** `LineWrapKey` (`line_wrap_cache.rs:75`) = `pipeline_inputs_version` + every geometry/view input (`view_mode`, `line_start`, `effective_width`, `gutter_width`, `wrap_column`, `hanging_indent`, `line_wrap_enabled`). `pipeline_inputs_version` (`:103`) packs `buffer.version()` (low 32b) XOR `soft_breaks.version()<<32` XOR `conceal.version()<<48` XOR `virtual_text.version()<<16`. Any input change → different key → old entries unreachable, aged out by eviction.
- **Byte-budget FIFO eviction.** Entry sizes vary wildly (a few hundred bytes to megabytes for a 200KB line wrapping to ~2000 rows), so count-based eviction is wrong. The cache tracks approximate total bytes (`estimate_view_lines_bytes`, `:128`) and evicts oldest-first when an insert would exceed `DEFAULT_BYTE_BUDGET = 8 MiB` (`:55`), always keeping at least the new entry. **FIFO not LRU** because the dominant pattern is sequential scrolling — a line is queried a few times in close succession then rarely again (`:138`). Invariant `map.len() == order.len()` and `current_bytes <= byte_budget` hold after every insert.

`layout_for_plain_text` (`:278`) is a buffer-free variant for sites that have a `&str` in hand (e.g. cursor-screen-position math, `viewport.rs:2543`) — it matches the renderer's word-boundary wrap on the same text/geometry. `WrapGeometry` (`line_wrap_cache.rs:411`) carries the geometry and builds the per-line `LineWrapKey` via `.key(line_start, version)`.

### Scalability to huge files

The cache holds only the *visible* span plus whatever scroll/cursor math recently touched, bounded to 8 MiB regardless of file size. Off-screen lines are never materialized into `ViewLine`s unless a consumer asks. The base-token fold path and the `MAX_SAFE_LINE_WIDTH` break cap further bound work on pathological lines. Whole-buffer questions are answered by the tier-2 index (next section) without materializing every line.

---

## 5. Visual-row index (tier 2)

`view/visual_row_index.rs` — a whole-buffer index sitting on `EditorState` *over* `LineWrapCache`. Where tier 1 answers per-line questions, this answers whole-buffer questions in O(1)/O(log N):
- `total_rows()` — O(1).
- `line_first_row(i)` / `line_row_count(i)` — O(1).
- `position_at_row(r)` → `(line_idx, line_start_byte, offset_in_line)` — O(log N) via `partition_point`.
- `line_for_byte(byte)` → `(line_idx, line_start_byte)` — O(log N).

Storage (`visual_row_index.rs:77`): two parallel `Vec`s of `N_lines + 1` entries — `prefix_sums[i]` = cumulative visual rows of lines `0..i` (last = total), `line_starts[i]` = byte offset of line `i` (last = buffer length sentinel).

**Why it exists** (commit `cf643629d`): three consumers were each folding the per-line cache into a whole-buffer answer *per call* — scrollbar drag's `build_visual_row_map` (per mouse-move), `scrollbar_visual_row_counts` (per frame), and `ensure_visible`'s wrapped scroll-up walk (per keystroke). Profiling put the first two at ~98% of CPU during scrollbar drag on large buffers (`LineWrapKey::hash` ~3%, per-event `ViewLineIterator::next` ~7%, `Vec<(usize,usize)>` realloc churn ~10–15%). The index replaces all three O(N_lines) folds.

**Population & invalidation** (`ensure_built`, `:186`): keyed on `VisualRowIndexKey` (`:48`) — the same geometry as `LineWrapKey` minus `line_start`. On a tier-1 hit it reads `entry.len()` for free; on a miss it runs the *count-only* path (`count_visual_rows_for_text`) — wrap + tally, skipping `ViewLine` materialization — and **does not write back** into tier 1 (avoids the per-char `Vec<ViewLine>` allocation the profile flagged; the renderer fills the real layout when the line becomes visible). It also pre-fetches buffer-wide soft breaks and virtual-line positions once and slices per-line with `partition_point`, because soft breaks and plugin virtual lines (markdown_compose table borders, git-blame headers) add rows that scrollbar/PageDown/mouse-wheel `max_scroll_row` must include or the buffer tail becomes unreachable (`:211`).

---

## 6. Folding

`view/folding.rs`. Folds are tracked as **byte-offset marker pairs** in the shared `MarkerList`, so they auto-adjust on edits without manual reshifting (fixes "fold indicator lag", issue #1571).

- `FoldRange` (`folding.rs:11`): `start_marker` (left affinity), `end_marker` (right affinity), `placeholder`. `FoldManager` owns `ranges: Vec<FoldRange>`.
- `resolved_ranges` (`folding.rs:130`) reads current marker bytes, drops invalid entries (`end <= start`, `start_line == 0`, …), converts bytes→lines, and computes `header_line = start_line - 1`. `collapsed_header_bytes` (`:175`) returns a `BTreeMap<header_byte → placeholder>` the renderer collapses against; `hidden_line_count_in_range` (`:248`) sums collapsed logical lines.
- Logical→visual collapse is applied two ways: at the *token* level (`build_base_tokens` skips folded source bytes via a sorted `fold_skip: &[Range<usize>]`, the `ViewLineIterator` re-applies it as defense-in-depth) and at the *placeholder* level (`apply_folding`).
- **LSP** folds live in a separate marker-backed store (`LspFoldRanges`, `:291`); when absent, an **indent-based** fallback (`mod indent_folding`, `:397`) detects foldable lines (next non-blank line more indented) and computes fold-end bytes by scanning forward. A subtle decision: `\n` is treated as a line *terminator*, not content, so a bare `"\n"` reads as blank — getting this wrong makes blank lines masquerade as fold headers (`:443`).

No version counter: correctness comes from markers auto-adjusting and from filtering invalid resolved ranges on every query.

---

## 7. Wrapping, conceal, soft breaks, virtual text

All four are decorations anchored to `MarkerList` byte offsets; soft-break/conceal/virtual-text additionally carry a monotonic `version: u32` folded into `pipeline_inputs_version`.

- **Wrapping** (`split_rendering/transforms.rs::apply_wrapping_transform`): multi-strategy greedy soft-wrap — inter-token word-wrap, grapheme char-wrap for over-wide tokens, and within char-wrap a preference for a UAX-#29 word boundary inside a `WRAP_MAX_LOOKBACK = 16` column window (avoids mid-identifier splits), with a hard grapheme cap fallback guaranteeing forward progress (a double-width glyph in a 1-col viewport still emits on its own row). The standalone helper `wrap_str_to_width` in `primitives/visual_layout.rs:334` mirrors this exactly and shares `WRAP_MAX_LOOKBACK` so virtual-line wrap and source-line wrap agree. `back_up_to_prior_space` moves a trailing word to the next row to avoid stranded leading spaces (issue #1363).
- **Soft breaks** (`view/soft_break.rs`): plugin-injected break points with a hanging `indent`, applied *before* conceal and wrapping. `SoftBreakManager` keeps a `marker_to_idx` side-index for O(log N + k) range removal (marker-tree query → map to indices → descending `swap_remove`).
- **Conceal** (`view/conceal.rs`): hide/replace byte ranges ("seamless canvas" markdown). `ConcealRange` has both endpoints as markers; `marker_to_idx.len() == 2 * ranges.len()`. Documented limitation (`conceal.rs:137`): `remove_in_range` can't detect a conceal that *fully spans* the query range (the marker query only finds endpoints *inside* it).
- **Virtual text** (`view/virtual_text.rs`): inlay hints, git-blame headers, fold previews, diff-removal lines. `VirtualTextPosition` is `BeforeChar`/`AfterChar` (inline, spliced into the token stream) or `LineAbove`/`LineBelow` (full rows injected as `ViewLine`s). Theme keys (`fg_theme_key`/`bg_theme_key`) are resolved live each render so injected text follows theme changes. Discrepancy flagged: `add_with_id_and_theme_keys` (`virtual_text.rs:339`) omits the `bump_version()` its sibling `add_*` methods call — a latent cache-staleness bug if that path is hit.

---

## 8. Margins / gutter and dimming

- **`view/margin.rs`**: gutter width = `floor(log10(total_lines)) + 1`, clamped to `MIN_LINE_NUMBER_DIGITS = 2` (`update_width_for_buffer`, `:626`; issue #1204 — tracks actual digit count rather than a fixed minimum). Layout is `[indicator 1col][line number][separator]`. Gutter indicators (git status, diagnostics, breakpoints, fold markers) are marker-anchored in the margin manager's *own* `MarkerList`; `get_indicators_for_viewport` (`:449`) queries only the viewport byte range and keeps the highest-priority indicator per line. In compose mode with line numbers off, the gutter stays `enabled` with `width = 0` so the 1-col indicator slot survives (issue #2146).
- **`view/dimming.rs`**: a pure post-process effect over the rendered cell buffer — `apply_dimming` / `apply_dimming_excluding` (`:42`,`:47`) darken cells behind a modal (~60% brightness reduction, channels /3), skipping the modal's own rect. No markers, no types — it runs last, directly on `frame.buffer_mut()`.

---

## 9. Splits and composite views

### Split tree — `view/split.rs`

Emacs-style window system: a binary tree of panes, Rects computed by recursively bisecting a root Rect.

`SplitNode` (`split.rs:80`) enum:
- **`Leaf`** — one buffer (`buffer_id`, `split_id: LeafId`, optional `role: SplitRole` — `UtilityDock` is a tagged singleton).
- **`Split`** — internal node: `direction`, `first`/`second: Box<Self>`, `ratio: f32` (first child's fraction, clamped `[0.1, 0.9]`), plus optional `fixed_first`/`fixed_second: u16` for absolute sizing.
- **`Grouped`** — a subtree appearing as a *single tab* in its parent; expanded inline only when its tab is active.

Layout primitive: `split_rect_ext` (`split.rs:1003`) reserves **exactly 1 row/column for the separator** (`saturating_sub(1)`), sizes the first child by `fixed_first`, else from `fixed_second`, else `(total * ratio).round()`, and gives the remainder to the second. The tree walk `get_leaves_with_rects` / `get_visible_leaves_with_rects` (`:786`,`:819`) returns `Vec<(LeafId, BufferId, Rect)>`; inactive `Grouped` subtrees return empty. `get_separators_with_ids` (`:873`) yields divider geometry for borders and mouse hit-testing.

`SplitManager` (`split.rs:1072`) owns `root`, `active_split: LeafId`, `maximized_split`, and a `focus_history` LRU (cap 50). Splitting pre-allocates IDs then `mem::replace`s a leaf with a new `Split`; closing replaces a parent with its surviving sibling; `next_split`/`prev_split` clear maximize first to avoid focusing a hidden leaf (issue #1961).

Per-view state also lives here: `BufferViewState` (`split.rs:138`, per-buffer-per-split: cursors, viewport, view_mode, folds, override flags recording user intent separately from rendered truth — issue #474) and `SplitViewState` (`:310`, per-window: active buffer, `keyed_states`, open tabs, and the cached `layout: Option<Layout>` + `layout_dirty: bool`). `SplitViewState` `Deref`s to the active `BufferViewState`. **Layout caching** (VSCode ViewModel pattern): `ensure_layout` (`:481`) rebuilds `Layout::from_tokens` only when `layout.is_none() || layout_dirty`.

### Composite views — `view/composite_view.rs`

For a composite buffer (side-by-side diff) rendered as multiple aligned panes inside *one* split. Unlike `SplitManager`, panes here share a single `scroll_row` and scroll together via row alignment (`composite_view.rs:23`). `update_pane_widths` distributes `total - separator_count*sep_width` by ratio (rounding absorbed into the last pane); `compute_pane_rects` walks left-to-right with separator gaps. A `sticky_column` preserves desired column across vertical nav; `SCROLL_MARGIN = 3` auto-scrolls.

### Split-rendering module organization

`view/ui/split_rendering/` (split-rendering-refactor-plan, commit `28374b923` decomposed the 811-line `render_content`) is split into two tiers, deliberately visible from `ls`:
- **Self-contained leaves** — `base_tokens`, `transforms`, `style`/`char_style`, `view_data`, `folding`, `gutter`, `scrollbar`, `layout`, `post_pass`, `spans` — depend on no shared render-time carrier.
- **`orchestration/`** — the only code touching the shared `SelectionContext`/`DecorationContext` mega-structs (`orchestration/contexts.rs`), quarantined so the coupling is obvious. Holds `render_buffer`, `render_composite`, `render_line`, `overlays`, `overlay_sweep`, `selection_sweep`, `tail_fill`.

`SplitRenderer` (`mod.rs:120`) is an empty façade forwarding to `orchestration::*`; entry points `render_content`, `compute_content_layout`, `render_phantom_leaf` (renders a buffer into an arbitrary off-tree rect — e.g. the Live Grep preview, `:256`).

---

## 10. Viewport and scrolling

`view/viewport.rs`. `Viewport` (`viewport.rs:7`) holds `top_byte`, `top_view_line_offset` (visual-row offset into the first visible logical line — supports mid-line scroll under wrap), `horizontal_scroll_offset`, `left_column`, and a `scroll_offset` margin (default 3). `effective_width` (`:213`) and `gutter_width` (`:235`) feed wrap geometry.

Scrolling is **byte-oriented** (`top_byte`) rather than line-indexed so it survives edits. `ensure_visible` (`:1866`) is the keystroke-time scroll, deferring fine wrap adjustments to render-time `ensure_visible_in_layout` (`:1201`), which knows the real content-area dimensions and the materialized `view_lines`. Several `skip_*` flags coordinate the two so a scroll action isn't immediately undone by `ensure_visible` (`:165`–`:193`). `cursor_screen_position` (`:2476`) counts visual rows (not logical lines) from `top_byte` to the cursor's line — using the renderer's word-boundary wrap via `layout_for_plain_text` — so popups anchor to the correct screen row in wrapped buffers (issue #1794). `max_scroll_row` and scrollbar thumb sizing now read the tier-2 `VisualRowIndex` instead of re-folding the per-line cache.

`view/scroll_sync.rs` + `app/scroll_sync.rs` implement synchronized scrolling across panes (diff side-by-side); `scroll-sync-design.md` is the design.

---

## 11. Mouse hit-testing

Render writes `view_line_mappings: HashMap<LeafId, Vec<ViewLineMapping>>` per leaf (built in `orchestration/render_line/mod.rs:541`, returned from `render_content`, cached on the window). A `ViewLineMapping` is the per-visual-row slice of a `ViewLine`'s maps.

`screen_to_buffer_position` (`app/click_geometry.rs:54`) converts `(col, row)` → buffer byte:
1. Adjust the content rect for compose centering and the compose gutter-reclaim shift (issue #2146, `:64`–`:78`).
2. Subtract gutter; a gutter click returns `None` (click handler) or position 0 (drag handler) per `allow_gutter_click`.
3. Index `mappings[visual_row]` and call `source_byte_at_visual_col(text_col)` — the **O(1)** `visual_to_char` → `char_source_bytes` lookup. Columns landing on virtual/injected content walk left to the nearest real byte; clicks past EOL clamp to `line_end_byte`; clicks below the last line use the last mapping.

Fold gutter-clicks route through `fold_toggle_byte_from_position` (`click_geometry.rs:158`) — checks collapsed headers, then LSP fold ranges (marker-resolved to current lines), then the indent-fold fallback. Mouse-input call sites: `app/mouse_input.rs:822`, `:1414`, `:1574`, `:3055`. The `unified-hit-test-theme-plan.md` is the design for unifying hit-test + the per-cell theme map (`cell_theme_map`, used by the theme inspector and the web bridge).

---

## 12. Unicode / grapheme width

Width is centralized so the editor, plugins, and wrap all measure identically.
- `primitives/display_width.rs` re-exports `char_width` / `str_width` from `fresh-core` (`:12`) — the single source of truth, also exposed to the plugin runtime's `charWidth`/`stringWidth`. CJK = 2, most emoji = 2, control/zero-width = 0. Plus byte↔visual-column helpers.
- `primitives/grapheme.rs` does grapheme-cluster navigation via `unicode-segmentation` (UAX #29): `prev`/`next_grapheme_boundary`, `grapheme_at`, `grapheme_count`. Editing and cursor movement operate on grapheme clusters (Thai base+combining, ZWJ emoji, combining diacritics count as one user-perceived character).
- `primitives/visual_layout.rs` builds per-line `LineMappings` (`:33`) handling ANSI escapes (zero width), tabs (expand to next multiple of `TAB_WIDTH = 8`), double-width and zero-width chars, with fast paths when no ESC/tab is present. This is the lower-level analogue the `ViewLine` maps build on; `unicode-width.md` is the rationale.

---

## Implemented vs planned

**Implemented:** the token IR pipeline and `ViewLine`; `LineWrapCache` (tier 1, 8 MiB FIFO byte-budget); `VisualRowIndex` (tier 2); marker-backed folding/soft-break/conceal/virtual-text with `version`-keyed invalidation; `Scene` semantic projections (menu/status/tabs/palette/popups/file-explorer/trust/widgets/keybinding/settings); the `RenderStyle`/`EditorRenderConfig` and `Frame → &mut Buffer` seam; `StatusBarChrome`/`FrameDimensions` chrome grouping; the `render_content` decomposition into `orchestration/`; the per-cell-overlay perf optimization (pre-computed `active_overlays` sweep, the main subject of `render-pipeline-perf-plan.md`); split tree + composite views; O(1) `visual_to_char` hit-testing.

**Planned / aspirational (per source docs, not in code):** the `transforms.rs` doc-comment pass order is *not* the executed order (`view_data.rs` is authoritative). `UNIFIED_SCENE_DESIGN.md` is largely realized — `keybinding_editor_view()` (`scene.rs:1070`) and `settings_view()` (`scene.rs:1413`) ship full native models. (Note `aux_modals_view` at `scene.rs:901` deliberately omits the keybinding editor — it's Settings-grade, not a line list — but the dedicated `keybinding_editor_view` covers it.) `scene.rs` review follow-ups (title styling, gutter width) are tracked (commit `309751daa`). `scrolling-highlight-cache-design.md` describes a highlight cache layered on the same versioning idea — verify against `highlight_engine.rs` before relying on it as current.

**Discrepancy flagged:** `virtual_text.rs:339` `add_with_id_and_theme_keys` does not `bump_version()` like its siblings — a latent stale-cache risk; `conceal.rs:137` documents that fully-spanning conceals are not removable by `remove_in_range`.
