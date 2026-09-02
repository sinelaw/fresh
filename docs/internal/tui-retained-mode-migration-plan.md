# Migrating the editor TUI to retained mode on `fresh-ui`

**Status:** plan, third revision. First written from
`claude/fresh-editor-fresh-ui-migration-glu9af` @ `2451eb9`, re-verified
against `master` @ `8a22e12` after the squash-merge, rewritten after a
review of the first increment (`view::shell::content`) against three
criteria — architectural robustness, performance, and alignment with the
style of `fresh-ui` itself — and then cross-checked against an independent
plan written from the objective alone, with no access to this document and
instructions to distrust every comment
(`tui-retained-mode-independent-review.md`, kept verbatim beside this file).
Everything that plan found and this one had missed is folded in below and
marked *(independent)*; the one claim of its that the code contradicts is
recorded in §2.8. Where this revision reverses an earlier one, it says so
and why.

**Objective:** the terminal UI is one retained tree. One description, one
layout, one paint, one hit-test, one source of geometry — and the buffer
model that makes large files editable is a constraint on that tree, not a
casualty of it.

This plan was written by reading the code, not the prior docs. Facts about
the tree today were checked against source or against a test that was run;
what could not be checked is marked **unverified**. Comments are not
evidence — several are stale (`view::shell::widgets`'s header describes a
`covered()` gate that no longer exists; `frame::HostRegion::Body`'s says the
body "never migrates", which is a decision this plan reopens).

---

## 1. Where the TUI actually is

The frame is already a retained tree. What is left is concentrated in the
editing surface and in a scaffold that exists only to let painters and the
tree coexist.

### 1.1 What the tree already owns

`Editor::render` builds one `fresh_ui::Node` (`view::shell::frame::frame_tree`)
and lays it out once; every region's rectangle comes from that layout.
Natively described — no `Draw::Host` emitted:

- menu bar and dropdown chain, status bar, search-options row
- the file explorer sidebar, the dock column (whenever a panel is mounted)
- every overlay layer: popups, the prompt's suggestion list, context menus,
  settings, the keybinding editor, the trust prompt, the calibration wizard,
  the event-debug dialog, the theme inspector, the floating panel's frame
- plugin panel *interiors*: `view::shell::widgets` has an arm for every
  `WidgetSpec` variant but `WindowEmbed`

Input is already single-walk: `handle_mouse_impl` offers the pointer to
`shell_dispatch` first and only falls to the legacy walk when the tree
declines; `Editor::handle_key` does the same and, when the tree owns the
keyboard, refuses to fall through. `app::chrome` derives the layer stack and
nothing else.

### 1.2 What is still a `Host` leaf

`Draw::Host` is emitted for exactly four things
(`view::shell::frame::HostTarget`):

| Target | Painter | Notes |
|---|---|---|
| `Region(Body)` | `BodyPainter::body` | Separators and the panes' shared preamble |
| `Region(PromptLine)` | `Editor::render_prompt_line` | One row, via `StatusBarRenderer::render_prompt` |
| `Pane(LeafId)` | `BodyPainter::pane` → `split_rendering::paint_leaf` | Gutter, text, tabs, scrollbars, terminals |
| `Embed(u32)` | `Editor::render_session_preview_into_rect` | An editor window inside a plugin panel |

(`Card(CardRegion)` resolves but paints nothing; the bands exist so
`render_overlay_prompt` and the read-backs share rectangles.)

### 1.3 What still paints outside the tree

`Editor::render` folds the display list in **two bands** — `Band::Background`
(`render.rs:899`) before the legacy painters and `Band::Overlay`
(`render.rs:1341`) after — with these running in between:

- `render_terminal_splits` (live PTY grids)
- `render_overlay_prompt` (~630 lines; 14 of the 26 remaining
  `frame.render_widget` calls in the crate)
- `render_floating_widget_panel` (dimming, the dock divider, embeds)
- `render_split_widget_panel_scrollbars`, `rerender_widget_panel`
- `render_hover_highlights`, `render_tab_drop_zone`
- `render_modal_overlays` → `view::settings::render_settings`
- `FileBrowserRenderer::render` (the open dialog's interior)
- the dormant / preparing / placeholder shell pages
- the software cursor pass

### 1.4 The geometry that is still recorded

`app::types::layout::ChromeLayout` is the ledger of "a painter measured this
and filed it for a handler to compare against". Still populated:
`popup_areas`, `global_popup_areas`, `suggestions_area`,
`suggestions_outer_area`, `prompt_toolbar_boxes`, `prompt_results_area`,
`prompt_preview_area`, `cell_theme_map`, `split_areas`,
`horizontal_scrollbar_areas`, `separator_areas`, `tab_layouts`,
`view_line_mappings`.

`view_line_mappings` is not chrome geometry — it is the byte↔visual
projection of the text pipeline and survives, though not here: it moves to
the pane's row window (§1.5b, §3). Everything else is a second derivation
of something layout already knows.

### 1.5 Geometry is computed more than once per frame — and already three ways

This is the fact the first revision under-weighted. Today:

1. `Editor::render` lays out the shell tree (`Ui::frame`).
2. `SplitManager::get_leaves_with_rects` (`view/split.rs:951`) lays the
   *same* grid description out again in a scratch `Ui<()>` to answer where
   the panes are, and `compute_content_layout` calls it
   (`orchestration/mod.rs:943`).
3. The macro-replay path (`render.rs:6933`) does a *third*: builds the shell,
   `Ui::layout_only`, reads `regions_of`, then runs `compute_content_layout`
   — which calls (2) again inside it.

Path 3 is the precedent for Blocker A's fix and is also what the fix must
not multiply.

Beside the three layouts, the render path does other per-frame work that
exists only because the tree's answer is not trusted or not reachable
*(independent)*:

4. `compute_dock_split(size)` (`render.rs:245`) computes the dock/chrome
   split arithmetically beside the dock layer's own layout.
5. The settings window's rect is read with
   `ui.find_by_key(results_key())` *before* this frame's description is
   built (`render.rs:353`) — a one-frame-stale feedback loop.
6. `regions_of` (`frame.rs:791`) does seven `find_by_key` walks per frame,
   `separator_rects` one per container, and there are 154 `find_by_key`
   call sites in the editor; each is a whole-tree walk
   (`fresh-ui/src/render/layout.rs:1080`).
7. `Splits` is deep-cloned into an `Rc` per frame (`splits.rs:958`) and
   `dress` builds a grid it then drops (`splits.rs:963–991`, by its own
   admission).
8. `last_rendered_frame = Some(frame.buffer_mut().clone())`
   (`render.rs:1481`): a full `Buffer` clone every frame, kept so the second
   band and the animation pass can read what the first painted.

### 1.5a The text formatter mutates the model and runs up to three times *(independent)*

`compute_buffer_layout` (`render_buffer.rs:94–121`) takes
`&mut EditorState, &mut Viewport, &mut FoldManager` and uses them: it
configures margins for line numbers, adopts the pane width, toggles gutter
state, and calls `viewport.ensure_visible_in_rows` — during paint.
`build_view_data` is called at `render_buffer.rs:334`, `363` and `413`, so a
pane can be formatted **three times per frame** when scroll-sync or
cursor-visibility corrections are only discovered after the first pass.
The second revision listed the `&mut`s as a fact to preserve ("that mutation
now happens before the description reads the state"); the independent
review is right that they are the root cause of Blocker A, not a detail of
it: a formatter that writes the model cannot be a pure supply, and a pane
that may be formatted three times cannot promise "frame cost = rows on
screen".

### 1.5b Keyboard motion reads the last painted frame *(independent)*

`ViewLineMapping` is produced by paint and consumed by `Up`/`Down`:
`WindowLayoutCache::move_visual_line` / `find_visual_row`
(`app/types/layout.rs:247, 307`). Keyboard motion therefore depends on what
was last painted rather than on `WrapIndex`, which can answer the same
question for any row without a frame. Mouse mapping inside a pane is the
same: the tree answers "which surface", last frame's paint answers "what
inside it" (`press_pane_content` → `handle_editor_click` against
`view_line_mappings`; `handle_click_tab_bar` against `tab_layouts`;
`handle_click_scrollbar` against `split_areas`).

### 1.6 A third description of chrome *(independent)*

`view/scene.rs` projects menus, tabs, status and popups for `webui`
(`webui/mod.rs:1876–2012`), and `tests/scene_parity.rs` asserts the TUI's
cells agree with it. With the tree and the legacy painters that is three
descriptions of the chrome. The TUI is this plan's scope; Stage 12 leaves the
door open to build the scene from `LayoutSpec` and is where the third
description ends.

### 1.7 Code volume

```
crates/fresh-ui/src                      14,520
fresh-editor/src/view/shell              29,528   (the tree)
fresh-editor/src/widgets                 18,992   (legacy widget runtime)
fresh-editor/src/view/ui/split_rendering 16,067   (the text painter)
fresh-editor/src/view/ui/{tabs,status_bar,menu,
  file_explorer,file_browser,scrollbar,scroll_panel}  9,107
```

Already dead: `view/ui/file_explorer.rs` (456 lines; `FileExplorerRenderer`
has no call site), `FileBrowserToggle`, `FileBrowserToggleSpan`,
`FocusRegion`, `TabsRenderer`, `widgets::render::wrap_entry_between`,
`widgets::layout_box::{ancestor_path, document_order}`,
`view::ui::status_bar::input_hscroll`. (`layout_box::hit_stack` was deleted
upstream between revisions.)

---

## 2. What blocks the endgame

### 2.1 Wide characters are painted one column each — **verified, live defect**

Layout measures with `unicode-width`; every backend paints `Draw::Lines` by
advancing one column per `char` (`view::shell::fold::fold_band`,
`fresh-ui/examples/interactive.rs:348`, `fresh-ui/tests/support/screen.rs:109`).
Throwaway test against the reference backend: `text("你好")` reserved
`w=4`, painted two cells, left a two-cell hole before its sibling. In the
ratatui fold `Cell::set_char('你')` also fails to blank the continuation
cell, so everything to the right shifts.

`TextRender::paint` (`fresh-ui/src/render/prim.rs:1105`) advances `x` per
run by the same `unicode-width`, so the mismatch is per *run*, not per row:
a CJK identifier followed by a keyword paints the keyword two cells right of
where the identifier ends. `draw_buffer_in_split` handles wide characters
itself today. **Mounting real buffer text through `text_runs` regresses
every CJK/emoji file until this is fixed.** It is a prerequisite of Blocker
A's mount, not a parallel item.

### 2.2 There is no primitive for a block of styled rows

`Draw` is `Fill | Border | Scrim | Lines | Scrollbar | Selectable | Host`.
A styled run is expressible (`text_runs` → `Run { text, theme }`). A block
of *N uniform rows, each a run list, windowed by the box's height* — which is
what a text pane is — is expressible only as N `text_runs` elements under a
`layout_reader`, which is what `view::shell::content` does. It works; it is
not the shape the library wants (§3).

### 2.3 The editor re-reconciles the whole tree every frame

`Component::memo`'s own doc names the editor as the host for which "the
short-circuit never fires". `frame_tree` is rebuilt every frame and nothing
editor-side uses `Node::shared`/`shared_rc` or implements `memo`. Tolerable
for chrome; measured before, not after, the text surface lands (Stage 0.4).

### 2.4 `ThemeKey` is a string, and text pays for it per run per frame

`shell_theme::Ink` serialises `fg/bg+attr` into the opaque `ThemeKey`. Right
for chrome (it converges with the theme inspector). For a text pane the
chain per span per frame is: `Span` → `Ink::to_string` (String) →
`Run::themed` (two `Rc<str>`) → at fold `Ink::parse` (`shell_host.rs:934`,
via `Palette::style`) back to a `Style`. Four allocations and a grammar
parse per span, one to two thousand spans per full-height pane.

### 2.5 `TextRender` measures by concatenation

`TextRender::layout` (`prim.rs:1028`) builds `props.plain()` — a fresh
`String` of all runs — on every measure, even for `Wrap::None` where a width
sum suffices. Per row, per frame, per `layout_reader` re-run.

### 2.6 `layout_reader` drops layout-dirt

`TextRender` carries a `stale` flag (`prim.rs:996–1010`) solely because a
subtree rebuilt by a `layout_reader` during layout loses its dirty marks and
would otherwise paint one frame behind its description. Any primitive
mounted under a reader needs the same defensive re-shape. `content()` puts
the editor's main surface on that wart.

### 2.7 A run cannot say "inherit the background"

An `Item` carries one `ThemeKey` that the palette resolves to a *full*
`Style`. `content::ink_of` therefore always emits a background (the ground's,
when the span names none). Consequently a described `Fill` *under* a row —
cursor column, ruler, compose margin, column guide — is painted over by the
row. The four cell patches in `draw_buffer_in_split` cannot become fills
under the text; they must become styles the formatter folds into the runs.

### 2.8 Two cursor sources — four, counting arbitration

`draw_buffer_in_split` writes `pending_hardware_cursor`; the tree has
`TextProps.cursor` (byte in run) → `LayoutSpec.cursor`. A described pane
must use the latter or the frame has two opinions about the caret. The
arbitration at `render.rs:1436–1470` already weighs four
(`pending_hardware_cursor`, `explorer_hardware_cursor`, `shell_caret`,
`cursor_obscured_by_overlay`), and the explorer's caret is computed by
arithmetic from its area at `render.rs:439–442` rather than placed by the
tree *(independent)*.

*Recorded disagreement.* The independent review says `fold_band` drops a
caret set by an in-flow (Background) surface, and that this is why the
explorer computes its own. `fold.rs:325–337` says otherwise: the Overlay
pass reports `spec.cursor` for the whole spec, so a Background surface's
caret is answered one pass later, not dropped. The explorer arithmetic most
likely exists because the explorer sets no tree cursor at all
(**unverified**). Either way the fix is the same — every editable surface
places its caret through the tree and the arbitration is deleted — but the
diagnosis matters for Stage 0.1's test: assert that a Background-band caret
reaches `LayoutSpec.cursor`, not that it is currently lost.

### 2.9 Geometry queries are whole-tree walks *(independent)*

`find_by_key` walks the tree from the root; `pane_content_rect` is one walk
per pane per query (`chrome/splits.rs:524–590`), `regions_of` seven per
frame. Not a blocker for correctness; a ceiling on what Stages 2–4 can win;
Stage 5 removes it.
The answer is a per-frame `Key → Rect` snapshot built once from
`LayoutSpec.index`, or `Ui::hit_test`, never a walk in a handler.

---

## 2a. The buffer model is a constraint on the architecture

Three properties must survive intact:

1. **Edits repair, they do not invalidate.** `WrapIndex` is not keyed on
   `buffer.version()`; `damage_bytes` resynchronises within a row or two and
   totals are a Fenwick tree. A description rebuilt from the whole buffer per
   keystroke hands that back.
2. **Only the visible window is materialised.** One node (or one row of one
   primitive) per *visual row on screen*, never per document line; scrolling
   changes which rows are described, not how many.
3. **The buffer is never copied to be described.** `Run::text` is `Rc<str>`;
   what must not appear is a `String` per run per frame — and §2.4 shows the
   first increment has three.

The invariant to assert and keep asserting: **a pane's display-list item
count is a function of its on-screen rows, not of its document length.**

### Who owns the text pane's scroll — decided

**The editor, not the tree.** This is a rule, not a first-increment
compromise, and it is the one place the text pane deliberately departs from
the library's `List::windowed` idiom. `ScrollMode::Items` needs a total row
count; `WrapIndex` exists to avoid computing one for a 5 MB file. Scrolloff,
cursor-follow, horizontal scroll and `max_line_length_seen` live in
`SplitViewState.viewport` and are consulted by editing commands, not only by
paint. The tree receives a *supply* — the rows for the window the editor
chose — and clips it to the height it grants. The pane's `vscroll` thumb is
fed from editor state, not `ScrollInfo`.

---

## 2b. Blockers, ranked by the legacy code each releases

### Blocker A — pane content cannot be described

**Holds:** `view/ui/split_rendering`'s paint half (~16,000 lines),
`BodyPainter::{body, pane}`, `paint_leaf`, most of `handle_mouse_impl`'s
post-dispatch tail, every pane-shaped `ChromeLayout` field.

**Why.** A description is a layout input; a pane's height is a layout
output. Both builders the library offers below layout are `'static`
(`layout_reader`'s closure; `HostSpec::Leaf`'s factory) and cannot reach the
`Editor`.

**Library half — verified.** A `layout_reader` under a `viewport` in
`ScrollMode::Items` builds only the window: 1M rows in a 10-cell viewport →
10–12 builder calls, 10–22 items, at offsets 0, 1, 5,000, 900,000. The
windowing mechanism is not the gap; a *supply the builder can reach* is.

**What landed — `view::shell::content` (273 lines, 6 tests).** `Row`
(`Vec<Run>`), `Content` (`Rc<[Row]>`), `content(c)` (a `layout_reader`
emitting one `text_runs` per row the box has room for, clipped to
`constraints.max_h`), `runs_of`/`ink_of` (`Vec<Span>` → `Vec<Run>` through
the `Ink` grammar). `a_pane_costs_its_rows_not_its_document` pins the
supply→list half of the invariant.

**Review verdict on what landed.** The *seam* is right: the formatter/painter
split already exists in the editor (`compute_buffer_layout` →
`render_output.lines`; `draw_buffer_in_split` writes cells), so a described
pane and the legacy pane share one formatter and parity is exact by
construction. The *shape* is provisional: `layout_reader` + N `text_runs`
sits on §2.5, §2.6, §2.7 and §2.8 simultaneously, and `runs_of` pays §2.4 in
full. It is the right first increment and the wrong final one; §3 names the
final one.

**The rest of Blocker A, in order** (the second revision had four steps;
the independent review split its third — "fill the supply" — into a
model-side stage and a mount, and that split is adopted):

1. **Library first** (each small, each with a library test): §2.1 wide-char
   paint parity in all three backends; §2.5 `Wrap::None` measures by width
   sum; a `ThemeKey → Style` cache in the fold's palette keyed by `Rc`
   pointer (or decision 3 in §6, if taken now).
2. **Scroll and margins out of paint** (§1.5a) *(independent)*. Move
   `configure_for_line_numbers`, `update_width_for_buffer`, `ensure_built`,
   `ensure_visible_in_rows` and scroll-sync into a `reconcile_pane(leaf,
   rect)` step that runs before the description is built and after every
   applier that moves the cursor. `compute_buffer_layout` loses every
   `&mut` (`state: &EditorState, viewport: &Viewport`), the three-pass
   `build_view_data` collapses to one, and `row_pass_owns_placement` /
   `may_rebuild` (`render_buffer.rs:350–441`) go with it. This is what makes
   a pure supply possible; the second revision's "the mutation now happens
   before the description reads the state" was the symptom, this is the
   cure.
3. **One geometry pass per frame.** Promote the replay path
   (`render.rs:6933`) to every frame: build shell → `layout_only` → read
   pane content rects with `rect_of(content_key(id))` **off the same `Ui`**
   — not via `get_leaves_with_rects`, whose scratch grid becomes a third
   geometry the reader's clip would silently paper over. `reconcile_pane`
   takes its rect from here. `debug_assert!` supply length == granted rows;
   the clip is a release safety net, not the contract. *Why not last
   frame's rect, as the independent review proposes:* every chrome toggle
   (dock, explorer, menu bar, a separator drag) changes pane widths between
   frames, and a supply wrapped at the old width is a visibly wrong frame,
   not a short one. The pre-frame layout is the exact answer; 0.4 decides
   whether it needs a geometry-changed gate.
4. **Fill the supply and mount it.** With 2 done, `compute_buffer_layout`
   is a pure function of `(state, viewport, rect)` and runs exactly once per
   pane per frame, from the reconcile step. Convert `render_output.lines`
   with an `Ink → Rc<str>` intern table per frame (a file has few distinct
   styles). Fold the four cell patches (`render_compose_margins`,
   `render_ruler_bg`, `render_cursor_column_bg`, `render_column_guides`)
   and `apply_background_to_lines` into the formatter as run styles (§2.7).
   Put the caret on the row's node (§2.8). Mount via `PaneSlots::content`
   behind a per-pane gate; a described pane skips `draw_buffer_in_split`
   and nothing on the legacy path re-formats it to refill `cell_theme_map`.
5. **Parity.** The 324-test e2e corpus with the gate forced on for every
   pane, plus cases the corpus lacks: CJK/emoji rows, cursor-column band,
   rulers, compose margins, a 5 KB and a 5 MB file in the same pane with the
   same item count, and a `cfg(test)` counter asserting `build_view_data`
   runs once per pane per frame.
6. **The primitive** (§3) replaces `content()`'s reader+col; `Row`/`Content`
   move into `fresh-ui`; `draw_buffer_in_split`, the four patches, and the
   `pending_hardware_cursor` side channel are deleted.
7. **Motion off the frame** (§1.5b) *(independent)*. The pane's row window
   — the same object that prepares the supply — answers `move_visual_line`
   and `byte_at(row, col)` from `WrapIndex`, so `Up`/`Down` and a content
   click no longer read `view_line_mappings` off the last paint.
   `WindowLayoutCache` is deleted. The "navigable row" rule for
   plugin-virtual rows (`layout.rs:317–349`) is ported verbatim, with the
   `shadow_model_*` tests as the oracle.

### Blocker B — text in the tree is mispainted, and its cost is unknown

**Holds:** every stage that puts a glyph in a node. **Lifts it:** A.1 above
and the bench from Stage 0.4. Folded into A's ordering; kept as a name.

### Blocker C — the formatter and the painter are the same function

**Holds:** `widgets/render.rs`'s paint and hit halves (~18,000 lines) and
the paint halves of `view/ui/{tabs, status_bar}` (~4,600).
`render_spec_with_options` decides what a row says *and* where it lands in
one pass. **Lifts it:** cut it into a formatter returning rows and a painter
consuming them; same cut in `tabs.rs` / `status_bar.rs`, keeping
`calculate_tab_widths`, `split_control_reserve` and the row formatters.

### Blocker D — painters must run between the two fold bands

**Holds:** the band collapse — `Band`, `Paints`, `SkipHosts`,
`view/settings/render.rs`'s box paint, `view::dimming`. The settings box
paints its own `Clear`/ground/border because it must land *under* a painter
that runs between bands; the floating panel's dimming is a painter pass for
the same reason. **Lifts it:** `render_terminal_splits` becomes a
`paint_host` arm; the dock's content moves to the overlay band so dimming
becomes `Scrim::Dim`.

### Blocker E — `ChromeLayout`'s readers are not all input routing

**Holds:** emptying `ChromeLayout`. The web `Scene` draws from rects;
`cursor_obscured_by_overlay` asks whether the caret is under a box; column
widths feed the next frame's description. **Lifts it:** feed the web `Scene`
from `LayoutSpec`; answer `cursor_obscured_by_overlay` from the tree's
layers.

### Order

A.1–A.7 is the critical path and serial, with the exception that A.2 and
A.3 are independent of each other and of A.1. C, D and E are independent
of A and of each other.

---

## 3. Target architecture

```
Editor state ──build──▶ Node tree ──reconcile──▶ Element tree ──layout──▶ RenderObjects
                                                                              │
                                                                        LayoutSpec
                                                                              │
                                                  ┌───────────────────────────┴──────┐
                                              fold → ratatui cells          hit-test / focus
```

**One band.** The fold runs once, in paint order.

**One geometry.** Per frame: one `layout_only` to size the panes, one
`frame`. `get_leaves_with_rects` is gone (Stage 2b): the callers off the
render path read the pane rects the window retains from the last layout.
Nothing records a rectangle for a later handler; handlers ask `rect_of`.

**The text pane is a `rows` primitive, not a reader.** A single `fresh-ui`
element `rows(Rc<[Row]>)` that measures one cell per row, clips to the
granted height, paints one item per themed run, places the caret by
`(row, byte)` and hit-tests to `(row, col)`. `TextRender` is the template
(cursor-by-byte and byte↔column mapping already exist there; this is its
multi-row form). It retires the `layout_reader` dependency (§2.6), the
per-row elements (§2.2/§2.3), and the caret side channel (§2.8), and gives
the inspector a real node type. `Row`/`Content` are library-shaped already
and move with it; `runs_of`/`ink_of` stay editor-side as the ratatui→`Ink`
adapter.

**Scroll is the editor's** for the text pane (§2a). The tree never learns
the document's row count.

**Nothing in the render path writes the model** *(independent)*. No
`&mut EditorState`, `&mut Viewport` or margin mutation is reachable from
`compute_buffer_layout` or from anything that runs under `Ui::frame` or the
fold. Wrap-index `ensure_built`, margin configuration, width adoption and
cursor-follow happen in a pre-frame reconcile step, once per pane.

**The pane's row window is the one object behind paint and motion**
*(independent)*. It holds `(buffer, geometry, top_row, height, width)`,
prepares the frame's rows from `WrapIndex` + `ViewLineIterator` in
O(rows on screen × width), and answers `move_visual_line` and
`byte_at(row, col)` from the same index without a frame. `ViewLineMapping`
lives on it, not on a window cache filled by paint. A terminal pane is a
second implementation of the same interface over its grid (or stays `Host`;
decision 1).

**Handlers never walk the tree** *(independent)*. A per-frame `Key → Rect`
snapshot from `LayoutSpec.index` (or `Ui::hit_test`) answers
`pane_content_rect`, `regions_of`, `separator_rects` and the rest of the
154 `find_by_key` sites; `find_by_key` is a test and inspector convenience.

**The formatter emits styles, not patches.** Everything that today is a
cell patch after the fact — cursor column, rulers, compose margins, column
guides, `apply_background_to_lines` — becomes part of what a row *says*.
The tree paints rows; it does not paint under them.

**Theme keys.** Chrome keeps the `Ink` string grammar. The text pane must not
pay a format+parse per span: at minimum the fold caches `ThemeKey → Style`
by pointer and the formatter interns `Ink → Rc<str>` per frame; the fuller
answer is decision 3 in §6. Provenance debt is logged: `render_view_lines`
resolves theme→`Color` in the formatter, so described runs carry
`Paint::Lit` colours and the tree cannot answer "which theme key is this".
Parity-exact today; the formatter should eventually emit keys and let the
palette resolve.

**`Host` is a design choice, not a migration seam.** Two leaves keep it —
a live PTY grid and an embedded editor window — because their content is
cells no description states more cheaply. A designed `Host` takes its
rectangle from layout and its position from paint order, and records
nothing.

**`ChromeLayout` holds the fold's provenance output** and nothing else;
`view_line_mappings` lives on the row window. **`HostPainter` has two arms.** **One pointer walk,
one keyboard walk.**

---

## 4. Stages

Each stage ends with the editor building and the e2e corpus (324 tests
under `crates/fresh-editor/tests/e2e`) green.

### Stage 0 — library correctness and instruments

**0.0 Provenance gate** *(independent)*. `ProvenanceSink` (`fold.rs:45`)
already records which item wrote a cell and is used only by the theme
inspector. Make it the measure: a test-only sink under `fold_band` plus an
allowlist of named legacy painters, and a `cells_provenance.rs` that
reports, per fixture, how many non-blank cells the fold wrote and how many
a painter did. Deletes nothing; every later stage is a number on this
table going down. *Exit:* the baseline is recorded here. (The second
revision had provenance as the last stage; it is the first, because it is
how the others are measured.)

**0.1 Display-width-correct painting** (§2.1). All three backends advance by
display width and the ratatui fold blanks continuation cells; grapheme
policy written down (a combining mark consumes no column). Conformance test
in `crates/fresh-ui/tests/` and one in `view::shell::fold` against a ratatui
`Buffer`. *Exit:* `text("你好")` occupies exactly its four columns in all
three backends.

**0.2 `Wrap::None` measures without concatenation** (§2.5). *Exit:* a test
that `TextRender::layout` with `Wrap::None` allocates no string (or simply
the code no longer calls `plain()` on that path, asserted by review).

**0.3 Palette resolve cache** (§2.4). `fold_band`'s palette resolves each
distinct `ThemeKey` once per frame. *Exit:* a fold of N items with K
distinct keys parses K names.

**0.4 A frame-cost instrument.** Bench measuring, for representative frames
(empty buffer; 5k-line highlighted file; four splits; full-screen dock
panel): `LayoutSpec::items.len()`, reconcile, layout, fold time, and
allocations per frame. `EditorTestHarness::render_real` is the driver.
*Exit:* `cargo bench -p fresh-editor` prints a table; baseline recorded here.

**0.5 Memoisation on the editor side** (§2.3), per subtree via `shared_rc`
or `memo`. *Exit:* an idle frame rebuilds no pane subtree. Gates Stage 3
only.

### Stage 1 — scroll and margins out of paint (Blocker A.2) *(independent)*

`reconcile_pane(leaf, rect)` runs before the description and after every
cursor-moving applier; `compute_buffer_layout` loses every `&mut`;
`build_view_data` runs once. `row_pass_owns_placement` toggling and the
`may_rebuild` dance (`render_buffer.rs:350–441`) are deleted. Tests: the
existing `shadow_model_*` and property tests for cursor-visibility
invariants, plus the once-per-pane counter. *Exit:* the signature is
`compute_buffer_layout(state: &EditorState, viewport: &Viewport, …)` and
the counter test passes on the whole e2e corpus. Independent of Stage 2;
both gate Stage 3.

### Stage 2 — one geometry pass (Blocker A.3)

Promote the replay geometry pass to every frame; pane content rects from
`rect_of(content_key)`; `reconcile_pane` takes its rect from here; retire
the scratch grid on the render path. *Exit:* `get_leaves_with_rects` has
no caller in `render.rs` or `split_rendering/orchestration`; the frame-cost
table shows the cost of the second layout.

**Stage 2b — no scratch grid at all.** `get_leaves_with_rects` and
`get_visible_buffers` are deleted. Each window retains the `PaneRects` of
the last layout that placed its panes (`Window::pane_rects`, read through
`Window::visible_panes`); the layout funnel (`push_layout_geometry`) lays
the active window's frame out once with `layout_only` and every other
window's grid offscreen before any window seeds viewports or sizes PTYs, so
an action that changed the grid reads it as it is. The parity tests keep the
model's original walk (`reference_leaves_with_rects`, `cfg(test)`) as their
oracle. See `fresh-editor-ui-migration.md`, "And then removed (Stage 2b)".

### Stage 3 — buffer content behind a gate (Blocker A.4–A.5)

Supply from the now-pure `compute_buffer_layout`, interned inks, patches
folded into styles, caret on the row, mount via `PaneSlots::content`,
per-pane gate, parity corpus with the gate forced on. *Exit:* the e2e
corpus is green with every pane described; the 5 KB / 5 MB item-count test
passes end to end; 0.4's numbers are recorded for the described path.

### Stage 4 — the `rows` primitive (Blocker A.6)

Add `rows` to `fresh-ui` with its own tests (clip, caret, hit-test, wide
chars); replace `content()`; move `Row`/`Content` into the library; delete
`draw_buffer_in_split`, the four patches, `pending_hardware_cursor`; drop the
gate. *Exit:* `HostTarget::Pane` is gone for buffer panes; `Region(Body)` is
gone with the separators; `split_rendering`'s paint half is deleted;
`view_pipeline`, `wrap_index`, `base_tokens`, `transforms`,
`view_line_mappings` remain as the view model.

### Stage 5 — motion off the frame, and the geometry snapshot (Blocker A.7) *(independent)*

The pane's row window answers `move_visual_line` and `byte_at(row, col)`
from `WrapIndex`; `WindowLayoutCache` and `view_line_mappings`-on-window
are deleted. A per-frame `Key → Rect` snapshot from `LayoutSpec.index`
replaces `pane_content_rect`, `regions_of`, `separator_rects` and the
other `find_by_key` walks on the render and input paths;
`compute_dock_split` goes (the dock layer reads its own layout); the
`results_key()` read before the description goes (the settings window
reads the rect it was granted); the per-frame `Splits` clone and `dress`'s
build-and-drop go (describe from an `Rc<Splits>` bumped on split mutation).
*Exit:* `find_by_key` count per frame is zero on the render and input paths
(counter test); `WindowLayoutCache` does not exist; `Up`/`Down` pass the
`shadow_model_*` tests without a frame between keystrokes.

### Stage 6 — the prompt row

`StatusBarRenderer::render_prompt` keeps formatting, gives up paint; the row
becomes `text_runs` under the `PromptLine` key; caret via the tree. *Exit:*
`HostRegion::PromptLine` emits no `Draw::Host`; `render_prompt_line` deleted.

### Stage 7 — the overlay prompt card

Describe `render_overlay_prompt`'s card; delete `CardRegion`, `CARD_TAG`,
`card_host_id`, and the five `ChromeLayout` prompt fields; a toolbar entry
becomes a keyed node so `UiFact::CardToolbarPress` names the entry, not a
column. *Exit:* `HostTarget::Card` gone; `record_suggestions_geometry`
shrinks to the web projection's needs.

### Stage 8 — pane chrome: tabs, scrollbars, split controls (Blocker C, part)

Each tab a keyed node with its own handlers, so `PaneTabsPress{x, y}` and
`PaneScrollbarPress{x, y}` — gesture nodes that forward raw screen
coordinates for a painter-recorded rectangle to interpret *(independent)*
— are deleted with `handle_click_tab_bar`'s scan and
`handle_click_scrollbar`'s thumb arithmetic; `calculate_tab_widths` and
`split_control_reserve` stay as formatters; `TabLayout`, `TabHitArea`,
`TabHit`, `ChromeLayout::tab_layouts` go; scrollbars from
`Draw::Scrollbar`, whose thumb the library already drags. *Exit:*
`tab_layouts`, `horizontal_scrollbar_areas`, `split_areas` are gone; no
`UiFact::Pane*` variant carries an `(x, y)`.

### Stage 9 — the gutter

Line numbers, folds, signs, bookmarks: a fixed column of the same `rows`
primitive. `split_rendering/gutter.rs` becomes a formatter. Ordered after
Stage 4 rather than before it (the first revision had it first) because the
primitive is what makes it cheap. *Exit:* the gutter emits no `Host`.

### Stage 10 — terminals and embeds (Blocker D, part)

Stay `Host` by design (§3). `render_terminal_splits` becomes a `paint_host`
arm; `render_floating_widget_panel` reduces to the embed arm. *Exit:* no
painter runs between the bands for terminals or embeds.

### Stage 11 — collapse the scaffold (Blockers C, D, E)

Collapse `fold_band` to one `fold`; delete `Band`, `Paints`, `SkipHosts`;
drop the per-frame `Buffer` clone into `last_rendered_frame` — the
animation pass describes its own cells or diffs `LayoutSpec` items
*(independent)*; delete the caret arbitration at `render.rs:1436–1470`
(one source, `LayoutSpec.cursor`; overlay occlusion is `Scrim::Opaque`
clearing items, so `cursor_obscured_by_overlay` and
`cursor_suppressed_by_late_overlay` are deleted, not ported);
`render_settings`'s box becomes a node; `apply_dimming*` → `Scrim::Dim`;
`FileBrowserRenderer` interior described; delete `view/ui/{file_explorer,
file_browser, scroll_panel, scrollbar}.rs`, `tabs.rs`'s hit half,
`status_bar.rs`'s paint half, `widgets/layout_box.rs`, `widgets/render.rs`'s
paint and hit halves; `handle_mouse_impl`'s post-dispatch tail (~830 lines);
`HostPainter` to two arms; web `Scene` from `LayoutSpec`;
`cursor_obscured_by_overlay` from the tree's layers. *Exit:*
`grep -c "frame.render_widget" src` is 0; `ChromeLayout` holds
`cell_theme_map` only; `render` reads as reconcile → describe → `frame` →
`fold` → animations → caret; the provenance table from 0.0 reads 100 %.

### Stage 12 — provenance and the third description

`cell_theme_map` becomes the fold's `ProvenanceSink` output alone (or is
derived from `ThemeId`s, per decision 3c). Build `view::scene` from
`LayoutSpec` so the web bridge stops being a third description of the
chrome (§1.6); `tests/scene_parity.rs` is the guard.

---

## 5. Ordering

```
0.0 ─────────────────────────────────────────────────────────┐
0.1 0.2 0.3 ──┐                                              │
0.4 ──────────┼──▶ 3 ──▶ 4 ──▶ 5 ──▶ 9 ──────────────────────┤
0.5 ──────────┤                                              │
1 ────────────┤                                              ▼
2 ────────────┘                  6 ──▶ 7 ──▶ 8 ──▶ 10 ──▶ 11 ──▶ 12
```

- 0.0 first: it is the measure every other stage reports against.
- 1 and 2 are independent of each other and of 0.x; both gate 3 (the first
  real text in the tree), as do 0.1–0.3 and 0.5. 0.4 decides whether 0.5
  is enough.
- 6, 7, 8, 10 are independent of 1–5 and of each other; 11 needs all of
  them plus 5.

---

## 6. Decisions and risks

**Decisions.**

1. **Do PTY grids and embedded windows migrate?** Recommendation: they stay
   designed `Host` leaves. A `Host` whose rectangle comes from layout and
   whose paint sits in list order is retained-mode; the objective's real
   target is the second geometry, and that goes either way. The strict
   alternative is a `Draw::Cells` run-length styled-grid primitive.
2. **What is the frame budget?** Stage 3 multiplies the display list by
   visible rows × runs per row. Without a ceiling, "fast enough" has no
   answer and Stage 3 has no exit.
3. **Does `ThemeKey` stay a string?** The first revision deferred this to the
   bench. The review moves it earlier: the text pane is the first surface
   with thousands of items, and the string grammar's cost is paid per item
   per frame. Options, cheapest first: (a) pointer-keyed resolve cache in
   the fold + per-frame intern table in the formatter (no library API
   change; Stage 0.3); (b) `ThemeKey` becomes generic over the host's key
   type (`K: Clone + Eq + Hash`), so the editor passes `Ink` and nothing is
   formatted or parsed — a library change touching every backend and the
   inspector; (c) *(independent)* an interned `ThemeId(u32)` resolved once
   per theme change into a `Vec<Style>` table, with `ThemeKey` kept as the
   provenance carrier and `cell_theme_map` derived from the same ids —
   which also unifies the inspector and deletes `apply_theme_runs`. Take
   (a) now; decide between (b) and (c) on 0.4's numbers, noting that (c)
   is the only one that makes per-frame theme resolution allocation-free
   and retires the per-cell key map.
4. **Should the pre-frame `layout_only` be gated?** Only if 0.4 shows the
   second layout is not a near-no-op. The replay path's own comment is
   right that a hand-picked "may have changed" set is a replica of a layout.
   The alternative — size the supply from last frame's rect and let the
   reader's clip correct it — is rejected in §2b A.3: it produces a frame
   wrapped at the wrong width on every chrome toggle.
5. **`rows` primitive or `Draw::Runs` item?** *(independent)* The same
   thing under two names: a first-class display-list item carrying one
   row's runs, so the web backend gets it for free and tests can assert on
   it. Not a `HostSpec::Leaf` factory: `Leaf` equality is `ptr_eq`
   (`desc.rs:904`) and `sync_render` never updates hosts, so a per-frame
   factory recreates its render object every frame. Stage 4 is this.
6. **Debounce wrap rebuilds on drag-resize?** *(independent)* A pane width
   change is an O(buffer) `WrapIndex` rebuild under the size ceilings —
   today's behaviour, and a `WrapIndex` concern rather than a migration
   one. Once `reconcile_pane` exists it is the natural place to coalesce a
   separator drag to one rebuild at release; a test that N drag steps cost
   ≤ 2 rebuilds pins it. Not on the critical path.

**Risks.**

- *Parity at the cell level.* `resolve_overlay_style`'s precedence is subtle
  and under-tested; the corpus with the gate forced on is the net, and it
  must be widened with the cases §2b A.5 lists before Stage 4 deletes the
  painter.
- *`ui_shell_frame_parity.rs` is a golden, not a cross-check.* It compares
  the tree against a reference it also owns.
- *State mutation moving before the description.* `reconcile_pane` clamps
  the viewport where `compute_buffer_layout` used to, which changes *when*
  clamping is visible to commands that run between frames. Look for tests
  that depend on the old order.
- *Plugin-visible behaviour.* `lines_changed` hooks run between the bands
  and add overlays paint must see; Stage 11's collapse must preserve that
  ordering deliberately.
- *Plugin-virtual rows under motion-off-the-frame.* Moving `Up`/`Down`
  from `view_line_mappings` to the row window changes where the "navigable
  row" rule (`is_plugin_virtual`, `layout.rs:317–349`) is evaluated; port
  it verbatim and keep `shadow_model_*` as the oracle *(independent)*.
- *Terminal panes.* A terminal row window over the PTY grid has a long
  tail (scrollback, link hover, `terminal_mouse.rs`). If it slips, terminals
  stay the designed `Host` of decision 1; it must not block Stages 5–9
  *(independent)*.
- *`fresh-gui` is unverified.* Whether it depends on any legacy painter or
  on `view/scene.rs` has not been checked; check before Stage 11 deletes a
  `render_*` function *(independent)*.
- *Stale comments are a hazard, not a nuisance.* The independent review
  found five comments on the render path describing a state that no longer
  holds (`render.rs:1369` "no migrated surface places a caret yet";
  `fold.rs:8` on backends stubbing `Host`; `shell_host.rs:1344` "every
  region is still a `Host` leaf"; `frame.rs:155` on hoisting;
  `splits.rs:963` on `dress`). Every stage deletes the comment with the
  code it describes.
- *An earlier framing error, kept for the record.* The first draft said the
  disjoint borrow must move from paint to build time; `shell_frame` already
  holds `&mut Editor`. The real blocker was that a pane's height is a layout
  output and both builders below layout are `'static` — solved by the
  pre-frame `layout_only`, not by a library change. The second revision
  then treated the formatter's `&mut`s as something to sequence rather than
  remove; the third removes them (A.2).

---

## 7. Definition of done

- `Draw::Host` is emitted for live PTY grids and embedded editor windows
  only (or nothing, per decision 1).
- One fold, one band, in paint order. One `layout_only` + one `frame` per
  frame; no scratch grid on the render path.
- No `frame.render_widget` in `crates/fresh-editor/src`.
- `ChromeLayout` holds the view-model and the fold's provenance, and no
  rectangle a painter measured.
- One pointer walk and one keyboard walk. One caret source.
- Wide characters occupy the columns layout gave them, asserted in the
  library and in the fold.
- The frame-cost table is inside the budget from decision 2; a described
  pane allocates O(distinct styles) theme keys per frame, not O(spans).
- A pane's display-list item count is a function of its visible rows and
  not of its document length, asserted end to end; `WrapIndex`'s
  damage-based repair is still the only thing an edit invalidates.
- *(independent)* `build_view_data` runs once per visible pane per frame
  (counter under `cfg(test)`); rows formatted = pane height;
  `WrapIndexStats.lines_built` is constant across 1k/100k/1M-line fixtures
  at a fixed viewport.
- *(independent)* No `&mut EditorState` or `&mut Viewport` is reachable
  from `compute_buffer_layout` or from anything under `Ui::frame`/the fold.
- *(independent)* `find_by_key` is called zero times per frame on the
  render and input paths (counter test); no per-frame `Buffer` clone; no
  per-frame `Splits` clone.
- *(independent)* `WindowLayoutCache`, `TabLayout`, `cell_theme_map` and
  the prompt-card `ChromeLayout` fields do not exist; mouse and `Up`/`Down`
  go through `ui.dispatch` and the row window.
- *(independent)* The provenance gate reports 100 % of non-blank cells in
  every e2e/parity fixture written by the single fold.
