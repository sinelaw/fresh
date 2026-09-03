# Independent migration plan: fresh-editor TUI to retained mode on fresh-ui

*Produced by an agent given only the objective (robust, performant, retained-mode, no duplicate code paths, buffer model stays efficient), told to distrust all docs and comments, and forbidden from reading the existing plan.*

Legend: **[V]** verified by reading the cited lines; **[I]** inferred from verified facts but not directly observed.

## 1. Findings

### 1.1 What fresh-ui actually provides

- **[V]** Three trees: descriptions (`Node<M>`), an element arena reconciled by `(ElemType, Key)`, and a render arena. Paint produces a flat display list `LayoutSpec { frame, items, layers_from, index, cursor }` (`crates/fresh-ui/src/render/spec.rs:19-42`) of `Item { key, id, rect, clip, theme: ThemeKey, draw }` (spec.rs:77-88), `ThemeKey(Option<Rc<str>>)` (spec.rs:99), `Draw::{Fill, Border, Scrim, Lines(Vec<Rc<str>>), Scrollbar{offset,content,window}, Selectable, Host(HostId)}` (spec.rs:142-170).
- **[V]** The display list is **not retained**: `flush_paint` clears and rebuilds it every frame (`crates/fresh-ui/src/render/paint.rs:18-37`); `paint_render` clones each node's children `Vec` and allocates a `DrawList` per node (paint.rs:68-138). The module doc "Cost is O(visible items)" (paint.rs:3) is wrong in letter: it is O(render nodes with a non-empty visible rect) plus per-node allocation. Retained-ness lives in the element/render arenas, not the list. This is fine for a TUI (ratatui diffs cells at the terminal) but it means "retained mode" here means retained *layout/hit-test tree*, not retained pixels.
- **[V]** Styled text is one `Item` per fragment per row (`crates/fresh-ui/src/render/prim.rs:1100-1119`); `TextRender::layout` concatenates twice (prim.rs:1024-1047). `Node::theme()` allocates an `Rc<str>` per build (`crates/fresh-ui/src/desc.rs:1232-1235`).
- **[V]** `sync_render` never updates `Host` objects and calls `LayoutReader::invalidate()` on every description (desc.rs:1934-1937 region), so every `layout_reader` subtree is rebuilt every frame. `HostSpec::Leaf` equality is `ptr_eq` (desc.rs:904), so a factory closure created per frame never compares equal.
- **[V]** `find_by_key` is a whole-tree walk (`crates/fresh-ui/src/render/layout.rs:1080-1096`); `LayoutCx::children()` allocates a `Vec` (layout.rs:52-59); Items-mode viewport `ask_band` is O(count) (prim.rs:1383-1483).
- **[V]** Hit-testing walks pending layers topmost-first then the root (`crates/fresh-ui/src/hit.rs:326-359`, `collect_paths` 384-433), computes `text_byte_at` once per dispatch (hit.rs:511) and allocates an `Rc<Ctl>` per path. `text_byte_at` admits `Elide::Head` is wrong (prim.rs:1156-1175).
- **[V]** `Draw::Scrollbar` semantics are shared through `Draw::scrollbar_thumb` (spec.rs:196-207), so a pane scrollbar described in the tree renders identically to the dock's today.
- **[V]** `spec.rs:106-127` documents `BorderStyle` in terms of ratatui's `BorderType`, contradicting the crate's "backend-agnostic" framing (spec.rs:3-6). Cosmetic, but it shows the docs describe intent, not code.

### 1.2 How the editor draws a frame — one description, many painters

**[V]** `Editor::render` (`crates/fresh-editor/src/app/render.rs:216-1496`) runs `ui.frame(frame_tree(shell))` exactly once (render.rs:379-388), but the cells are produced by **two `fold_band` calls with ~15 legacy ratatui painters between and after them**:

| Step | Line | Kind |
|---|---|---|
| `compute_dock_split(size)` | render.rs:245 | legacy geometry computed beside the tree |
| settings window read via `ui.find_by_key(results_key())` before the description is built | 350-363 | reads *last* frame's layout to build *this* frame |
| `fold_band(.., Band::Background, ..)` | 899-907 | tree, in-flow half |
| `shade_scroll_edges` | 953 | legacy painter |
| `render_dormant_shell_page` / `render_preparing_shell_page` | 961-968 | legacy |
| `render_terminal_splits(buf, &pane_rects, true)` | 1136 | legacy |
| write `split_areas/horizontal_scrollbar_areas/tab_layouts/view_line_mappings` | 1139-1142 | geometry cache written after paint |
| `publish_popup_carets` | 1154 | legacy |
| `rerender_widget_panel`, `render_split_widget_panel_scrollbars` | 1167-1178 | legacy |
| `separator_rects(ui, ..)` -> `separator_areas`, `last_editor_content_area` | 1191-1198 | tree geometry copied into legacy cache |
| `render_hover_highlights` | 1201 | legacy |
| `publish_status_bar`, `render_prompt_popups`, `cache_buffer_popup_areas`, `cache_top_global_popup_area` | 1225-1261 | legacy |
| `render_modal_overlays`, `render_floating_widget_panel(Dock)` | 1320-1330 | legacy |
| `fold_band(.., Band::Overlay, ..)` with `EmbedHosts` | 1332-1376 | tree, out-of-flow half |
| `render_tab_drop_zone`, `render_software_cursor_and_capture` | 1400-1409 | legacy |
| caret arbitration (`pending_hardware_cursor` vs `explorer_hardware_cursor` vs `shell_caret` vs `cursor_obscured_by_overlay`) | 1436-1470 | four caret sources |
| `animations.apply_all`, full `Buffer` clone into `last_rendered_frame` | 1474-1481 | O(cells) clone per frame |
| `render_panels_and_modals`, `convert_buffer_colors` | 1486-1492 | legacy |

**[V] Host escape hatches** (every `host(`/`host_leaf(` call site in the editor): `dock.rs:90` (empty dock), `frame.rs:467` (Body preamble) and `frame.rs:755` (region fallback), `overlay_prompt.rs:108` (card bands), `splits.rs:1021` (one per pane), `widgets.rs:2553` (embed). `paint_host` (`crates/fresh-editor/src/app/shell_host.rs:381-419`) maps `HostTarget::Region` Body -> `body()`, PromptLine -> `render_prompt_line`, and MenuBar/SearchOptions/Explorer/Dock/StatusBar/Card -> no-op. So the live hatches are **Body, Pane, PromptLine, Embed**.

**[V] Comment/doc contradictions found on this path**
- render.rs:1369-1370 says "No migrated surface places [a caret] yet, so this is `None` on every frame today" — yet `shell_caret` is wired as the winning source at 1436-1470, and the explorer caret is computed by arithmetic at 439-442 with a comment admitting "This comment is not the fix; it is the admission that one is owed."
- `frame.rs:155-167` claims "0 hoisted subtrees" and that `Node::shared()` has no editor call site — true **[V]** (grep: zero uses), which means the tree gets no reconciliation savings from sharing; every frame is a full description rebuild.
- `splits.rs:963-983` (`dress`): doc admits the `n` argument "is unused ... dropped on the floor", i.e. a grid is built and discarded per frame.
- `fold.rs:8-9` claims fresh-ui backends stub `Host` as a fill; `Draw::Host` is a first-class variant the editor is expected to paint (spec.rs:168-169).
- `shell_host.rs:1344-1352` ("every region is still a `Host` leaf") is stale: menu bar, explorer, status bar, search options are native (`frame.rs:462-513`).

### 1.3 Text pane production

- **[V]** `compute_buffer_layout` (`crates/fresh-editor/src/view/ui/split_rendering/orchestration/render_buffer.rs:94-121`) **mutates model state during paint**: `state.margins.configure_for_line_numbers` (125), `update_width_for_buffer` (152-154), gutter toggles (160-182), `viewport.ensure_visible_in_rows` + `row_pass_owns_placement = true` (314-320). `build_view_data` can run **up to three times per pane per frame** (332, 363, 413) because scroll-sync and cursor-visibility corrections are discovered only after formatting.
- **[V]** The wrap index is the row-space seam: `WrapIndex::row_of_byte` is O(log lines + log rows) (`crates/fresh-editor/src/view/wrap_index.rs:725-739`), `byte_of_row` O(log lines) (742-780), `resumable_row_at_or_before` bounded by one logical line (791-807). Rebuild is O(buffer) only on geometry/buffer-version change (`rebuild_full` 598-628); decoration changes repair per line (641-702). Geometry is `WrapIndexGeometry { rule, view_mode, fold_signature }` (34-50) and `WrapRule::Word { content_width, gutter_width, .. }` (`crates/fresh-editor/src/view/wrap_machine.rs:39-45`), so **a pane width change triggers a full O(buffer) rebuild** [V], gated by `MAX_WRAP_SCROLLBAR_LINES = 5_000` / `MAX_WRAP_SCROLLBAR_BYTES = 2 MiB` (`scrollbar.rs:25-26`; used at render_buffer.rs:229-232). Above the ceilings the row path is not used and the byte-anchored `Viewport` path runs (render_buffer.rs:328).
- **[V]** The formatter (`render_view_lines`, `render_line/mod.rs:1-98`) emits `Vec<ratatui::text::Line<'static>>` with **resolved** `Style`, plus a parallel `cell_theme_map: Vec<CellThemeInfo>` with **string theme keys** (`crates/fresh-editor/src/app/types/theme.rs:12-21`). Two style representations exist for the same cell; the tree has a third (`ThemeKey`).
- **[V]** `ViewLineMapping` (`crates/fresh-editor/src/app/types/layout.rs:6-36`) is produced by paint and consumed by **keyboard** motion: `WindowLayoutCache::move_visual_line` / `find_visual_row` (layout.rs:245-349). Up/Down depend on the last painted frame, not on the wrap index.
- **[V]** The split grid is laid out twice: once by the tree (`dressed` wraps each `SplitNode::Split` in a `layout_reader` that re-runs `split_rect_ext`, `crates/fresh-editor/src/view/shell/splits.rs:1028-1074`) and once by `SplitManager::get_visible_buffers(area)` inside `BodyPainter::body()` (`shell_host.rs:234`) and `paint_separators` (`orchestration/mod.rs:326-348`). `pane()` then overrides the manager's rect with the tree's (`pane.3 = rect`, shell_host.rs:257-283) — the second layout's output is discarded except for ordering.
- **[V]** `with_grid` reassembles `EditorRenderConfig`, `FrameFacts`, `Stores` and takes the theme `RwLock` **per pane per frame** (shell_host.rs:295-371).

### 1.4 Hit-testing: two engines, one per level

- **[V]** `handle_mouse_impl` offers the event to the tree first (`crates/fresh-editor/src/app/mouse_input.rs:199-211` via `shell_dispatch`, `shell_host.rs:1353`), then falls to `pane_content_takes_pointer`, `handle_mouse_drag`, `update_lsp_hover_state` (219-312).
- **[V]** The pane's tab strip, scrollbars and content are **empty gesture nodes that forward raw screen coordinates**: `tab_strip` emits `PaneTabsPress { pane, x, y }` (splits.rs:1521-1539), `scrollbar` emits `PaneScrollbarPress { pane, axis, x, y }` (1422-1445). The appliers then hit-test those coordinates against **painter-recorded** geometry: `handle_click_tab_bar` (shell_host.rs:1897-1905) against `tab_layouts`; `handle_click_scrollbar` against `split_areas` thumb extents (`crates/fresh-editor/src/app/chrome/splits.rs:330`); `press_pane_content` -> `handle_editor_click` (chrome/splits.rs:473-510) against `view_line_mappings`. The tree answers "which surface", last frame's paint answers "what inside it".
- **[V]** `WindowLayoutCache` (layout.rs:218-243) and `ChromeLayout` (`suggestions_area`, `prompt_results_area`, `prompt_preview_area`, `cell_theme_map`, layout.rs:150-177) are the legacy hit-test stores. `webui/mod.rs:1908` also reads `split_areas`.
- **[V]** `pane_content_at` is containment over `pane_content_rect` = `find_by_key(content_key(pane))` per pane (chrome/splits.rs:575-590, 524-526): a whole-tree walk per pane per query.

### 1.5 Geometry computed more than once per frame (summary)

1. Split grid: tree `layout_reader` + `SplitManager::get_visible_buffers` + `get_separators` (1.3).
2. Frame regions: `regions_of` does 7 `find_by_key` walks per frame (`frame.rs:791-803`); `separator_rects` one per container (splits.rs:1141-1165); 154 `find_by_key` sites in the editor.
3. `compute_dock_split` (render.rs:245) beside the dock layer's own layout.
4. `panel_rect`/`find_by_key(results_key())` before the description (render.rs:350-363) — a one-frame-stale feedback loop.
5. Explorer caret computed by arithmetic from `area` (render.rs:439-442) instead of `DrawList::set_cursor`.

### 1.6 Theme-key representation

**[V]** `ShellPalette::style` -> `shell_theme::resolve(theme.as_str(), &theme)` per item (shell_host.rs:491-499, 934-950); `Ink::parse` splits on `+`/`/` and builds `String`s (883-917); `Paint::parse` allocates `Cow::Owned` (635-645); then `Theme::resolve_theme_key` is a generated nested string match (`crates/fresh-editor/src/view/theme/types.rs:2418-2429`). Every item, every frame, pays parse + allocation + string match. `Node::theme()` allocates the `Rc<str>` on the description side too.

### 1.7 A third description of chrome

**[V]** `view/scene.rs` ("the single source of truth for *what* the chrome is", scene.rs:1-12) projects menus/tabs/status/palette/popups for `webui` (webui/mod.rs:1876-2012), and `tests/scene_parity.rs` asserts TUI cells agree with it. fresh-ui's display list is explicitly designed so "a web backend patches DOM nodes by key" (spec.rs:3-6). So today there are three descriptions of chrome: the tree, `scene.rs`, and the legacy painters. This plan scopes the TUI, but Stage 8 leaves the door open to fold the scene from `LayoutSpec`.

### 1.8 Perf hazards specific to the retained path (already live)

- Per frame: `Splits` deep-cloned into `Rc` (splits.rs:958-961); every `layout_reader` closure rebuilt (~40 sites); `dress` builds-and-drops (979-991); full `Buffer` clone (render.rs:1481); per-item theme parsing (1.6); `fold_band` `Draw::Lines` writes `for ch in line.chars() { put(..); x += 1 }` with **no wide-char width** (`crates/fresh-editor/src/view/shell/fold.rs:255-262`) while `prim.rs` measures with unicode-width — a measure/paint divergence for CJK/emoji in any native text.
- Correctness-adjacent: `fold_band` reports the caret only from the Overlay band (fold.rs:331-337), so a caret set by an in-flow native surface (explorer) is dropped — which is exactly why render.rs:439-442 recomputes it by hand.

### 1.9 Test surface

**[V]** `crates/fresh-editor/tests/ui_shell_frame_parity.rs` pins frame region rects to a ratatui reference (`reference()` at 58, tests at 228-322; its own doc says "a pin, not an oracle"). `scene_parity.rs` compares web scene vs TUI cells. fresh-ui has 10,224 lines of tests including `golden.rs` (285) and `paint.rs` (1001). No test today asserts "every cell came from the display list" — `ProvenanceSink` (fold.rs:45-48) exists, and is only used for the theme inspector.

## 2. Blockers (ranked by legacy code each unblocks)

1. **Text pane rows are not a tree primitive** (blocks deleting `Draw::Host` per pane, `BodyPainter`, `with_grid`, `paint_leaf`'s cell writes, `render_terminal_splits`, `shade_scroll_edges`, `render_hover_highlights`, `render_tab_drop_zone`, the software-cursor painter, `publish_popup_carets`, `pane_rects`/`split_areas`/`view_line_mappings`/`tab_layouts`/`horizontal_scrollbar_areas`; ~6k lines across `render.rs`, `shell_host.rs`, `orchestration/`). Root cause: the formatter emits resolved `ratatui::Line`s and mutates viewport/margins during paint, so it cannot run inside `RenderObject::paint`.
2. **Pane scroll state is decided during paint** (`ensure_visible_in_rows`, `ensure_visible_in_layout_with_render_width`, three `build_view_data` passes). Blocks a pure paint and the "frame cost = on-screen" guarantee under scroll-sync.
3. **Keyboard/mouse mapping depends on last frame's paint** (`view_line_mappings`, `tab_layouts`, `split_areas`). Blocks deleting `WindowLayoutCache` and the coordinate-forwarding `UiFact::Pane*{x,y}` arms.
4. **Theme keys are strings resolved per item** and the editor keeps a second per-cell key map (`cell_theme_map`). Blocks making native text as cheap as legacy text and unifying the inspector.
5. **`fold_band` cannot place carets from in-flow surfaces and ignores wide chars**. Blocks the explorer caret hack and any native text with CJK.
6. **Overlay-band painters (prompt popups, modals, floating panels, session preview embed)** remain ratatui. Smaller, mostly independent surfaces; each blocks one `render_*` function.
7. **`layout_reader` invalidated every frame; `HostSpec::Leaf` ptr_eq; `find_by_key` walks.** Not blockers for correctness but they cap the win of Stage 1-4.

## 3. Target architecture

**Model layer (unchanged contract).** `Buffer` (piece tree), `WrapIndexSet` keyed by `WrapIndexGeometry`, highlighting, folds, decorations stay exactly as they are. The one new rule: nothing in the render path writes to `EditorState`, `Viewport` or `margins`. Wrap-index `ensure_built`, margin configuration and pane-width adoption move to a **pre-frame "reconcile" step** driven by the pane rect the tree produced last frame plus the pending resize event (see below), not by paint.

**Pane scroll state.** Owned by `SplitViewState.viewport` in **row space** when the wrap index applies (`top_row: u32`, `left_col`), with `top_byte` derived via `byte_of_row`, and byte-anchored above the size ceilings — i.e. today's two modes, but the decision "which row is top after cursor motion / scroll-sync" is made by the input applier (`ensure_visible_in_rows` after the edit, `scroll_to_end_of_view` when sync fires), never by paint. The tree's `Viewport` primitive is **not** used for panes: fresh-ui owns scroll for chrome lists (`ScrollMode::Cells/Items`), the editor owns it for text panes, and the pane node reports `Draw::Scrollbar { offset: top_row, content: total_rows, window: rows }` from that state. That is one owner per surface and no conversion.

**Row production (the seam).** A new `RowSource` object per pane, created by the applier, holds `(BufferId, LeafId, WrapIndexGeometry, top_row, height, width)` and exposes `rows(range) -> impl Iterator<Row>` where `Row { cells/runs: Vec<Run { text: Rc<str>, theme: ThemeId }>, map: RowMap }`. It walks `byte_of_row(top_row)` -> `resumable_row_at_or_before` -> `ViewLineIterator` for exactly `height` rows; cost is O(rows on screen × width) plus the index lookups. The pane's `RenderObject` (a new fresh-ui `HostSpec::Leaf` factory or, better, a first-class `Draw::Runs(Vec<Run>)` item) calls `rows()` inside `paint` and emits one item per row. `ViewLineMapping` is produced by the same call and stored on the `RowSource` (not on `WindowLayoutCache`), so mouse mapping and Up/Down read the same rows the screen shows without a second pass — and a `RowSource` can answer `move_visual_line` **without a frame** because it can format any row on demand from the index.

**Theme keys.** Replace `ThemeKey(Option<Rc<str>>)` string resolution with an interned `ThemeId(u32)` resolved once per theme change into a `Vec<Style>` table (`ShellPalette::style` becomes an index). fresh-ui keeps `ThemeKey` as the provenance carrier but the editor interns at description time (`theme_id("editor.fg")` const or lazily cached) and the fold looks up `palette[id]`. `CellThemeInfo` for the inspector is derived from the same id (name table lookup), deleting `cell_theme_map` and `apply_theme_runs`.

**Hardware cursor.** One source: `DrawList::set_cursor` from the innermost editable render object (pane, prompt line, explorer filter, settings entry). `fold_band` propagates `spec.cursor` from **both** bands; the last emitted wins (spec.rs:233-240 already says so). Overlay occlusion is expressed by `Scrim::Opaque` clearing items (paint.rs:39-66), so `cursor_obscured_by_overlay`/`cursor_suppressed_by_late_overlay` are deleted rather than ported.

**Hit-testing.** The tree is the only engine. Pane parts stop forwarding `(x, y)`: the tab strip becomes a `row()` of keyed tab nodes with per-tab handlers; scrollbars become fresh-ui scrollbar nodes whose thumb the library already drags (hit.rs handles thumb capture); content press asks `RowSource.byte_at(row, col)` through `Event`-relative coordinates. `pane_content_at`/`pane_content_rect` become `ui.hit_test` or a per-frame `HashMap<Key, Rect>` snapshot taken once from `LayoutSpec.index`, replacing 154 `find_by_key` walks.

**Cells backend.** `fold_band` becomes `fold` (single band). `Draw::Host` disappears from the editor; `HostTarget` and `HostRegion` tags are deleted. Wide-char writes go through unicode-width so the fold matches `prim.rs` measurement.

## 4. Stages

Each stage is independently shippable; ordering is by what it unblocks.

**S0 — Provenance gate (1-2 days).** Add a test-only `ProvenanceSink` that records every cell written by `fold_band` and a debug assertion in tests that every non-blank cell of the final `Buffer` was written by the fold *or* by a named legacy painter from an allowlist. Deletes nothing. Proves: a baseline count of legacy-painted cells per fixture (parity suite `ui_shell_frame_parity.rs` + a new `cells_provenance.rs`). Unblocks: every later stage measures its deletion.

**S1 — Interned theme ids and single-band caret.** Introduce `ThemeId` table in `ShellPalette`; `fold_band` resolves by index; `Node::theme` accepts a pre-interned `Rc<str>` (no per-build alloc). Make `fold_band` return the caret from both bands and fix wide-char width in `Draw::Lines`. Delete: `shell_theme::Ink::parse`/`Paint::parse` per-item path (keep a parser for theme-file load), the explorer caret arithmetic (render.rs:439-442), `cursor_suppressed_by_late_overlay`. Test: fresh-ui `golden.rs` unchanged; new editor test that CJK in explorer/status renders at correct width; `ui_shell_frame_parity` green. Unblocks: cheap native text (S3/S4).

**S2 — Scroll and margins out of paint.** Move `configure_for_line_numbers`, `update_width_for_buffer`, `ensure_built`, `ensure_visible_in_rows`, `sync_scroll_to_end` into a `reconcile_pane(leaf, rect)` step run in `render()` **before** `ui.frame` (using last frame's pane rects, or the resize event's) and after every applier that moves the cursor. `compute_buffer_layout` loses all `&mut` (signature `state: &EditorState, viewport: &Viewport`); the three-pass `build_view_data` collapses to one. Delete: `row_pass_owns_placement` toggling in paint, `may_rebuild` dance (render_buffer.rs:350-441). Test: existing `shadow_model_*` and `property_tests` (cursor visibility invariants), plus a new test asserting `build_view_data` is called once per pane per frame (counter under `cfg(test)`). Unblocks: S3.

**S3 — `RowSource` and the pane as a render object.** Implement `RowSource::rows()` over the wrap index + `ViewLineIterator`, emitting `Run`s with `ThemeId`. Add a pane render object (fresh-ui `HostSpec::Leaf` factory with stable identity, or a new `Draw::Runs`) that paints rows, gutter, tildes and sets the caret. Keep terminal grids (`render_terminal_splits`) as a second `RowSource` impl over the terminal grid. Delete: `paint_leaf` cell writes, `draw_buffer_in_split`, `render_view_lines`' `Vec<Line>` output, `BodyPainter::pane`, `with_grid`, `Draw::Host` per pane (splits.rs:1021), `HostTarget::Pane`, `shade_scroll_edges`, `render_hover_highlights` (becomes a decoration run), `render_software_cursor_and_capture` (a run style), `render_tab_drop_zone` (a layer node). Test: `scene_parity`, `e2e`, new `pane_rows_are_on_screen_only` test asserting `WrapIndexStats.lines_built` and rows formatted do not grow with document size across 1k/100k/1M-line fixtures at fixed viewport. Unblocks: S4, S5, S6.

**S4 — Tabs and scrollbars as real nodes.** Tab strip becomes keyed tab nodes (label, close button, drag handle) with the strip's own `Viewport` in Cells mode; pane scrollbars become fresh-ui scrollbar nodes fed from `SplitViewState`. Delete: `TabLayout`, `tab_layouts`, `split_areas`, `horizontal_scrollbar_areas`, `handle_click_tab_bar`'s coordinate scan, `PaneTabsPress/Hover/Wheel/Pan`, `PaneScrollbarPress/Drag/Hover` facts, `chrome/splits.rs:316-470`, most of `view/ui/tabs.rs` (2149 lines) and `split_rendering/scrollbar.rs` painters (markers stay as data). Test: existing tab/scrollbar e2e; new pointer tests through `ui.dispatch` only. Unblocks: S5.

**S5 — Delete the second split layout and geometry caches.** `BodyPainter::body`, `paint_separators`, `SplitManager::get_visible_buffers`/`get_separators` **as render inputs** (keep `visible_leaves()` for model queries), `render_content` (28-arg), `PaneAreas`, `WindowLayoutCache` (mouse/keyboard read `RowSource` and a per-frame `Key -> Rect` snapshot from `LayoutSpec.index`), `regions_of`/`separator_rects` per-frame `find_by_key` walks, `compute_dock_split` (dock layer reads its own layout), `pane_content_at` containment scans, `HostRegion::Body` host. `dress`'s dead argument and per-frame `Splits` clone go here (describe from `&Splits` with `Rc` held on the editor and bumped only on split mutation). Test: `ui_shell_frame_parity` (now the tree is the only layout; `reference()` stays as the pin), a bench asserting `find_by_key` count per frame is zero in release paths. Unblocks: S6, S7.

**S6 — Overlay-band surfaces.** Port `render_prompt_popups`, `render_modal_overlays`, `render_floating_widget_panel`, `render_panels_and_modals`, `render_session_preview_into_rect` (embed becomes a `RowSource` over the preview buffer), prompt line (`render_prompt_line`) to native descriptions with `Scrim`. Delete: `EmbedHosts`, `HostTarget::{Embed, Card, Region}`, `publish_popup_carets`, `cache_*_popup_area`, `suggestions_area`/`prompt_results_area`/`prompt_preview_area`, `cursor_obscured_by_overlay`. Test: provenance gate reaches zero legacy cells on all fixtures. Unblocks: S7.

**S7 — Single fold.** Replace `fold_band`×2 with one `fold(spec)`; delete `Band`, `Paints`, `HostPainter`, `SkipHosts`, `HostTarget`, `HostRegion` tags, `frame.rs:102-146`. `last_rendered_frame` clone replaced by an animation layer that describes its own cells (or a diff of `LayoutSpec` items). Test: provenance gate = 100 % fold; `render()` reduced to: reconcile -> describe -> `ui.frame` -> `fold` -> animations -> caret.

**S8 — fresh-ui hardening (parallel, from S1).** `LayoutReader` invalidation keyed on `Size`+`Rc` identity instead of every description; `HostSpec::Leaf` compares by a caller-supplied `Key`; `LayoutSpec.index` exposed as a `HashMap` built once; `TextRender::layout` single concatenation; per-node `DrawList` reuse; wide-char and `Elide::Head` `text_byte_at` fix. Test: fresh-ui `conformance.rs`/`layout.rs` `layout_reader` tests plus a rebuild counter. Optional S9: build `scene.rs` from `LayoutSpec` to remove the third chrome description (out of TUI scope; noted for the web bridge owner).

## 5. Decisions and risks

- **Pane scroll owned by the editor, not by fresh-ui's `Viewport`** — decided because scroll semantics (row-space with cursor-line expansion, byte fallback above ceilings, scroll-sync) are model logic; putting them in the tree would move the size ceilings into the framework. Risk: two scroll idioms in the codebase; mitigated by both ending in `Draw::Scrollbar`.
- **`Draw::Runs` vs `HostSpec::Leaf` factory for rows.** A `Leaf` keeps fresh-ui's display list untouched but relies on ptr_eq identity (desc.rs:904) and on `sync_render` not updating hosts (desc.rs:1934) — both must change (S8) or the pane re-creates its render object every frame. Recommendation: add `Draw::Runs` (one item per row, `Vec<Run>`) so the web backend gets it for free and tests can assert on it.
- **Width change = O(buffer) wrap rebuild** [V] under the ceilings. Accepted (it is today's behaviour) but S2 should debounce reconcile on drag-resize to one rebuild at release; add a test that a separator drag of N steps triggers ≤ 2 rebuilds.
- **Keyboard motion migrating from `view_line_mappings` to `RowSource`** changes behaviour for plugin-virtual rows (`is_plugin_virtual`, layout.rs:317-349). Port the "navigable row" rule verbatim and keep `shadow_model_*` tests as the oracle.
- **Terminal panes** (`render_terminal_splits`, `terminal_mouse.rs`) need their own `RowSource`; risk of a long tail (scrollback, link hover). Keep them as the last `Host` if S3 slips, but do not let that block S4/S5.
- **Documentation debt**: several comments describe a state that no longer holds (1.2). Every stage should delete the comment with the code it describes; a stale "this is still a Host" note is how the next reader gets misled.
- **Not verified**: `fresh-gui` crate's dependency on any of the legacy painters or on `scene.rs` — check before S6 deletes `render_*` functions.

## 6. Definition of done (measurable)

1. `grep -c "Draw::Host\|host(\|host_leaf(" crates/fresh-editor/src` = 0; `HostTarget`, `HostRegion` tag constants, `HostPainter`, `Band`, `Paints` do not exist.
2. Provenance gate: 100 % of non-blank cells in every e2e/parity fixture are written by the single `fold`.
3. `render()` calls exactly one description build, one `ui.frame`, one fold; `find_by_key` is called 0 times per frame in release paths (counter test).
4. `build_view_data`/`RowSource::rows` invoked once per visible pane per frame; rows formatted = pane height; `WrapIndexStats.lines_built` constant across 1k/100k/1M-line fixtures at a fixed viewport, and `rebuilds` increments only on width/geometry change.
5. No `&mut EditorState`/`&mut Viewport` reachable from any `RenderObject::paint` or from `compute_buffer_layout`.
6. `WindowLayoutCache`, `ChromeLayout::{suggestions_area, prompt_results_area, prompt_preview_area, cell_theme_map}`, `TabLayout`, `ViewLineMapping`-on-window are deleted; mouse and Up/Down go through `ui.dispatch` + `RowSource`.
7. Theme resolution per frame does zero heap allocation (`ThemeId` table); inspector reads the same ids.
8. One caret source (`LayoutSpec.cursor`); `cursor_obscured_by_overlay` and explorer arithmetic deleted.
9. Wide-char text in native surfaces renders at unicode width (test with CJK/emoji fixtures).
10. Line count of `app/render.rs` + `app/shell_host.rs` + `split_rendering/orchestration/` reduced by ≥ 60 % from today's 8418 + 2793 + ~5300.
