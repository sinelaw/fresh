# Migrating the editor TUI to retained mode on `fresh-ui`

**Status:** plan, written from the code on
`claude/fresh-editor-fresh-ui-migration-glu9af` @ `2451eb9`.
**Objective:** the terminal UI is one retained tree. One description, one
layout, one paint, one hit-test, one source of geometry.

This plan was written by reading the code, not the prior docs. Where it
states a fact about the tree today, that fact was checked against the
source or against a test that was run; the places where it could not be
checked are marked **unverified**. Comments in the tree are not treated as
evidence — several are stale (`view::shell::widgets`'s header describes a
`covered()` gate that no longer exists; `frame::HostRegion::Body`'s says the
body "never migrates", which is a decision this plan reopens rather than a
fact).

---

## 1. Where the TUI actually is

The migration is much further along than "start here". The *frame* is
already a retained tree; what is left is concentrated in the editing
surface and in a scaffold that exists only to let painters and the tree
coexist.

### 1.1 What the tree already owns

`Editor::render` builds one `fresh_ui::Node` (`view::shell::frame::frame_tree`)
and lays it out once. Every region's rectangle comes from that layout.
Natively described — the tree paints them, no `Draw::Host` is emitted:

- menu bar and its dropdown chain, status bar, search-options row
- the file explorer sidebar, the dock column (whenever a panel is mounted)
- every overlay layer: popups, the prompt's suggestion list, context menus,
  settings, the keybinding editor, the trust prompt, the calibration wizard,
  the event-debug dialog, the theme inspector, the floating panel's frame
- plugin panel *interiors*: `view::shell::widgets` has an arm for every
  `WidgetSpec` variant but `WindowEmbed`, built as nodes rather than routed
  through the old collector

Input is already single-walk. `handle_mouse_impl` offers the pointer to
`shell_dispatch` first and only falls to the legacy walk when the tree
declines; `Editor::handle_key` does the same and, when the tree owns the
keyboard, refuses to fall through at all. `app::chrome` no longer memoises
a box tree or dispatches through one — it derives the layer stack and
nothing else.

### 1.2 What is still a `Host` leaf

`Draw::Host` is emitted for exactly four things
(`view::shell::frame::HostTarget`):

| Target | Painter | Notes |
|---|---|---|
| `Region(Body)` | `BodyPainter::body` | Separators and the panes' shared preamble only |
| `Region(PromptLine)` | `Editor::render_prompt_line` | One row, via `StatusBarRenderer::render_prompt` |
| `Pane(LeafId)` | `BodyPainter::pane` → `split_rendering::paint_leaf` | Gutter, text, tabs, scrollbars, terminals |
| `Embed(u32)` | `Editor::render_session_preview_into_rect` | An editor window inside a plugin panel |

(`Card(CardRegion)` also resolves, but paints nothing — the bands exist so
`render_overlay_prompt` and the read-backs share one set of rectangles.)

### 1.3 What still paints outside the tree

`Editor::render` folds the display list in **two bands** — `Band::Background`
before the legacy painters and `Band::Overlay` after — with these still
running in between:

- `render_terminal_splits` (live PTY grids)
- `render_overlay_prompt` (~630 lines; 14 of the 26 remaining
  `frame.render_widget` calls in the crate)
- `render_floating_widget_panel` (dimming, the dock's divider, embeds)
- `render_split_widget_panel_scrollbars`, `rerender_widget_panel`
- `render_hover_highlights`, `render_tab_drop_zone`
- `render_modal_overlays` → `view::settings::render_settings` (the settings
  box's `Clear`, ground and border — 4 `render_widget` calls)
- `FileBrowserRenderer::render` (the open dialog's interior; the tree owns
  its box)
- the dormant / preparing / placeholder shell pages
- the software cursor pass

### 1.4 The geometry that is still recorded

`app::types::layout::ChromeLayout` is the ledger of "a painter measured
this and filed it for a handler to compare against". Still populated:

`popup_areas`, `global_popup_areas`, `suggestions_area`,
`suggestions_outer_area`, `prompt_toolbar_boxes`, `prompt_results_area`,
`prompt_preview_area`, `cell_theme_map`, `split_areas`,
`horizontal_scrollbar_areas`, `separator_areas`, `tab_layouts`,
`view_line_mappings`.

Of these, `view_line_mappings` is not chrome geometry — it is the byte↔visual
projection of the text pipeline and survives the migration. Everything else
is a second derivation of something layout already knows, and each one is a
place the two can drift.

### 1.5 Code volume

```
crates/fresh-ui/src                      14,261
fresh-editor/src/view/shell              28,941   (the tree)
fresh-editor/src/widgets                 18,149   (legacy widget runtime)
fresh-editor/src/view/ui/split_rendering 16,067   (the text painter)
fresh-editor/src/view/ui/{tabs,status_bar,menu,
  file_explorer,file_browser,scrollbar,scroll_panel}  9,107
```

`view/ui/file_explorer.rs` (456 lines) is already dead —
`FileExplorerRenderer` has no call site outside doc comments.
`FileBrowserToggle`, `FocusRegion` and `TabsRenderer` are likewise
unreferenced.

---

## 2. What blocks the endgame

Four gaps, in the order they bite. The first is a live defect, not just a
migration blocker.

### 2.1 Wide characters are painted one column each — **verified**

Layout measures text with `unicode-width`. Every backend paints
`Draw::Lines` by advancing `x` one column per `char`. All three
implementations agree with each other and disagree with layout:
`view::shell::fold::fold_band`, `fresh-ui/examples/interactive.rs:348`,
`fresh-ui/tests/support/screen.rs:109`.

Confirmed with a throwaway test against the library's reference backend:

```
text("你好") → Lines(["你好"]) rect = Rect { x: 0, y: 0, w: 4, h: 3 }
next sibling → Lines(["|X"])   rect = Rect { x: 4, ... }
painted row  → "你好  |X"        (two glyphs in two cells, two-cell hole)
```

Layout reserved four columns and the fold used two. In the ratatui fold
the failure is worse than a hole: `Cell::set_char('你')` puts a two-column
glyph in a one-column cell without blanking the continuation cell, so the
terminal renders four columns of glyphs where the buffer holds four cells
of content and everything to the right shifts.

This is already wrong today for any CJK or emoji text in migrated chrome —
a filename in the explorer, a plugin's panel label. It is *fatal* for the
text surface, whose whole job is arbitrary Unicode.

### 2.2 There is no draw primitive for a styled cell grid

`Draw` is `Fill | Border | Scrim | Lines | Scrollbar | Selectable | Host`.
A styled run of text is expressible (`text_runs` → `Run { text, theme }`,
one `ThemeKey` per run). A *grid* of independently styled cells — which is
what a live PTY is — is not, except as one item per run per row.

### 2.3 The editor re-reconciles the whole tree every frame

`Component::memo`'s own doc says it, and names the editor:

> a host that derives its description from a store every frame structurally
> cannot [hold an `Rc` across frames] … so for such a host the short-circuit
> never fires and the whole tree re-reconciles on every frame, however little
> changed. That is not a hypothetical: it is the state the editor integration
> is in.

`frame_tree` is built fresh in `Editor::render` each frame and nothing on
the editor side uses `Node::shared` or implements `memo`. This is tolerable
while the tree describes chrome. It stops being tolerable the frame a pane's
text lands in it: a 200×50 frame of four splits is on the order of a few
thousand nodes per frame instead of a few hundred.

### 2.4 `ThemeKey` is a string, and styled text pays for it per run

`shell_theme::Ink` serialises `fg/bg+attr` into the single opaque
`ThemeKey`, with `Paint::Lit(Color::Rgb)` for colours no theme named. It is
the right design for chrome — it converges with the theme inspector, and it
is why the display list and the inspector say the same thing in the same
words. But it means every styled run allocates a string on build and parses
one on fold. Chrome has hundreds of runs a frame; highlighted text has
thousands.

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

- **One band.** The fold runs once, in paint order. No painter runs between
  two halves of the display list.
- **`Host` is a design choice, not a migration seam.** Two leaves keep it
  permanently — a live PTY grid, and an embedded editor window — because
  their content is genuinely cells that no description can state more
  cheaply. What makes a `Host` legacy is not that it paints cells; it is
  that its geometry is recorded somewhere else. A designed `Host` takes its
  rectangle from layout and its position from paint order, and records
  nothing.
- **`ChromeLayout` holds `view_line_mappings` and nothing else** except the
  fold's own provenance output.
- **`HostPainter` has two arms**, both above.
- Every pointer and key goes through one walk. `handle_mouse_impl`'s
  post-dispatch tail is gone.

---

## 4. Stages

Each stage is independently shippable, and each ends with the editor
building and the e2e corpus (324 tests under `crates/fresh-editor/tests/e2e`)
green. Stages 1–4 are safe in any order after stage 0; stage 5 depends on
all of 0.

### Stage 0 — instruments, and the two things that must be true first

**0.1 Display-width-correct painting.** Fix all three backends to advance by
`char_width` and, in the ratatui fold, to blank the continuation cell of a
wide glyph. Decide and write down the grapheme policy: a combining mark must
not consume a column, so the unit of advance is the cluster, not the `char`.
Add the missing conformance test to `crates/fresh-ui/tests/` — the library
owns the invariant, so the library should assert it — plus one in
`view::shell::fold`'s test module against a ratatui `Buffer`.

*Exit:* `text("你好")` occupies exactly the four columns layout gave it, in
all three backends, and a `Screen`/`Buffer` assertion proves it.

**0.2 A frame-cost instrument.** There is no bench directory in the
workspace. Add one measuring, for a set of representative frames (empty
buffer; a 5k-line highlighted file; four splits; a full-screen dock panel):
`LayoutSpec::items.len()`, reconcile time, layout time, fold time.
`EditorTestHarness::render_real` is the right driver — it already runs a
full frame headlessly. Record the baseline in this document.

*Exit:* `cargo bench -p fresh-editor` prints a table; the numbers for
today's tree are written down here.

**0.3 Memoization on the editor side.** Give the per-window chrome and each
pane subtree an identity that survives a frame — `Rc<Node>` held on the
`Editor` and re-offered with `shared_rc` when its inputs are unchanged, or
a `Component` with a real `memo`. Which of the two is right differs by
subtree and is a judgement to make per site, not up front.

*Exit:* 0.2's reconcile time for an idle frame (no state change) drops to
approximately the cost of the dirty set alone, and a test asserts that an
idle frame rebuilds no pane subtree.

> **This stage is a hard gate for stage 5** and only for stage 5. Stages 1–4
> add tens of nodes, not thousands.

### Stage 1 — the prompt row

The smallest remaining `HostRegion`. `StatusBarRenderer::render_prompt` and
`render_file_open_prompt` keep their *formatting* (what a prompt row says is
domain knowledge) and give up their paint: the row becomes `text_runs` in
the region that already carries the `PromptLine` key, and the caret becomes
`CursorSpec` instead of the `caret` out-parameter.

*Exit:* `HostRegion::PromptLine` emits no `Draw::Host`; `render_prompt_line`
is deleted; the e2e prompt tests pass unchanged.

### Stage 2 — the overlay prompt card

`render_overlay_prompt` is the single largest block of direct ratatui
painting left. Its bands are already stated in the tree (`CardRegion`,
`card_host_id`) purely so the painter and the read-backs agree; describing
the card deletes both the painter and the band ids, and `CARD_TAG` with
them.

Takes with it: `prompt_results_area`, `prompt_preview_area`,
`prompt_toolbar_boxes`, `suggestions_area`, `suggestions_outer_area` — all
read from the tree instead of recorded. `UiFact::CardToolbarPress`'s
"the painter measured this label" seam closes: a toolbar entry becomes a
keyed node and the press names the entry, not a column.

*Exit:* `HostTarget::Card` and `CardRegion` are gone; those five
`ChromeLayout` fields are gone; `record_suggestions_geometry` shrinks to the
web projection's needs alone.

### Stage 3 — pane chrome: tabs, scrollbars, split controls

The *outline* is already the tree's: `splits::tab_strip`, `splits::scrollbar`
and `splits::controls` carry keys, gestures and rectangles. The *interiors*
are `view::ui::tabs`, which lays out tab widths, records a `TabHitArea` per
tab in `TabLayout`, and answers a click by scanning them.

Describe each tab as its own keyed node. `calculate_tab_widths` and
`split_control_reserve` stay as formatters; `TabLayout`, `TabHitArea`,
`TabHit` and `ChromeLayout::tab_layouts` go. `UiFact::PaneTabsPress { x, y }`
becomes `PaneTabSelect(TabTarget)` / `PaneTabClose(TabTarget)` — the node
knows which tab it is, so no handler has to recover it from a column.

The scrollbars are already a display-list primitive (`Draw::Scrollbar`,
whose `scrollbar_thumb` is shared so every backend renders the bar
identically). Emitting them from the pane's description retires
`horizontal_scrollbar_areas`, `record_scrollbar_theme_runs` and
`view::ui::scrollbar`.

*Exit:* a pane's strip and bars emit no `Draw::Host`; `tab_layouts`,
`horizontal_scrollbar_areas` and `split_areas` are gone.

### Stage 4 — the gutter

Line numbers, fold markers, git and diagnostic signs, bookmarks: a fixed
column of short runs, one per visual row. Small, low-risk, and it
establishes the per-row description shape stage 5 uses at scale.
`split_rendering/gutter.rs` becomes a formatter: it keeps deciding what a
gutter cell *says*, and stops deciding where it goes.

*Exit:* the gutter is `text_runs`; 0.2's item count for a full-screen frame
is measured and recorded (this is the first real data point on per-row cost).

### Stage 5 — buffer content

The one that changes the architecture rather than extending it. Do it in
three sub-stages, each behind a config flag comparing the described output
cell-for-cell against the painter's on the e2e corpus, so a divergence is a
red test rather than a bug report.

- **5a — plain content.** `paint_leaf`'s per-row `Vec<Span<'static>>`
  becomes `Vec<Run>` and the row becomes a `text_runs` node inside the
  pane's keyed viewport. Syntax highlighting only; no overlays, no
  selection. The caret becomes `CursorSpec`.
- **5b — everything layered on a cell.** Selection, search highlight, the
  current-line and current-column bands, inline diff, diagnostics squiggles,
  plugin overlays. Each is a `Fill` under the runs or a themed run, and
  `resolve_overlay_style`'s precedence has to be preserved exactly — this is
  where parity is hardest and where the cell-for-cell comparison earns its
  cost.
- **5c — retire the painter.** Delete `split_rendering`'s paint half. Keep
  `view_pipeline`, `wrap_index`, `base_tokens`, `transforms` and
  `view_line_mappings`: those are the view model, and the description is
  built *from* them.

Click-to-byte keeps going through `ViewLineMapping`, but reads the pane's
rectangle from the tree (it already does — `content_key`).

*Exit:* `HostTarget::Pane` is gone for buffer panes; `Region(Body)` is gone
with the separators; 0.2's numbers are inside the budget agreed in §6.

### Stage 6 — terminals and embeds

The remaining two. **The recommendation is that they stay `Host` leaves by
design**, and that the work here is closing their *geometry* duplication
rather than describing their cells:

- a live PTY is a grid of independently styled cells with its own damage
  model; expressing it as one display-list item per run per row is more
  expensive than the `Host` callback it replaces, and buys nothing — nothing
  hit-tests inside a PTY grid except the terminal's own mouse protocol,
  which already routes through `pane_content_takes_pointer`
- an embedded editor window is a second editor render; it is a `Host` for
  the same reason the outer one would be

What does change: `render_terminal_splits` stops being called between the
bands and becomes a `paint_host` arm, so it lands in paint order rather than
after it; and `render_floating_widget_panel` reduces to the embed arm plus
the dimming pass, which becomes a `Scrim` once stage 7 collapses the bands.

**Alternative, if the objective is read strictly:** add a `Draw::Cells`
primitive carrying a run-length-encoded styled grid, and describe them too.
This is a real library change with a real cost and it is listed in §6 as a
decision, not assumed here.

### Stage 7 — collapse the scaffold

The structural end. **The two-band fold is the last piece of migration
scaffolding**, and several described surfaces are shaped by it: the settings
box still paints its own `Clear`/ground/border in `view::settings::render`
*because* it must land under a painter that runs between the bands, and the
floating panel's dimming is a painter pass for the same reason. Neither is a
description problem; both are ordering problems that disappear when nothing
runs between the bands.

- collapse `fold_band` to one `fold`; delete `Band` and `Paints`
- `render_settings` → the box is a node; `apply_dimming*` → `Scrim::Dim`
- the file-open dialog's interior (`FileBrowserRenderer`) is described; the
  "the painter measured the label" seam its module header describes closes
  the same way stage 2's toolbar does
- delete `view/ui/file_explorer.rs`, `view/ui/file_browser.rs`,
  `view/ui/scroll_panel.rs`, `view/ui/scrollbar.rs`, `view/ui/tabs.rs`'s hit
  half, `view/ui/status_bar.rs`'s paint half
- delete `widgets/layout_box.rs` and `widgets/render.rs`'s paint and hit
  halves; keep the formatters `view::shell::widgets` calls
- `handle_mouse_impl`'s post-dispatch tail (~830 lines) goes; the tree is
  the only walk
- `HostPainter` reduces to two arms

*Exit:* `grep -c "frame.render_widget" src` is 0. `ChromeLayout` holds
`view_line_mappings` and `cell_theme_map`.

### Stage 8 — provenance and the second frontend

`cell_theme_map` becomes the fold's `ProvenanceSink` output alone — the fold
is already the only party that sees every described cell, which is why the
inspector went blank over migrated chrome before it was taught to record
there. With no painters left, no other writer exists.

Re-check `view::scene` and `webui`: the semantic projections are supposed to
be computed once and consumed by both frontends. With the TUI reading its
geometry from `LayoutSpec` and the web reading `RectView`s, confirm there is
one derivation and not two. `tests/scene_parity.rs` is the existing guard.

---

## 5. Ordering and what each stage gates

```
        ┌─────────────────────────────────────────┐
0.1 ────┼──▶ 1 ──▶ 2 ──▶ 3 ──▶ 4 ──▶ 5a ─▶ 5b ─▶ 5c ──▶ 7 ──▶ 8
0.2 ────┤                             ▲                 ▲
0.3 ────┘─────────────────────────────┘        6 ───────┘
```

- 0.1 gates everything that puts text in the tree, which is every stage.
- 0.3 gates only stage 5, and hard.
- 6 is independent of 1–5 and can be done at any time after 0.1; it gates 7
  only because 7 collapses the bands and 6 is what moves the terminal paint
  into the fold.
- 7 requires 2, 5c and 6 — it deletes what they stop using.

---

## 6. Decisions needed before stage 5, and risks

**Decisions.** These change the shape of the work and are not mine to make:

1. **Do PTY grids and embedded windows migrate?** §Stage 6 recommends they
   stay designed `Host` leaves and argues why. Reading the objective
   strictly ("everything retained") means adding a `Draw::Cells` primitive
   instead. I would take the recommendation: a `Host` whose rectangle comes
   from layout and whose paint sits in list order is already retained-mode;
   what the objective is really about is the *second geometry*, and that
   goes either way.
2. **What is the frame budget?** Stage 5 multiplies the display list by
   roughly the number of visible text rows times runs per row. Without a
   number from 0.2 and a ceiling agreed here, "is this fast enough" has no
   answer and stage 5 has no exit criterion.
3. **Does `ThemeKey` stay a string?** §2.4. Chrome can afford it; text at
   scale may not. The alternative — an interned handle, with `Ink` behind it
   — is a library change that touches every backend and the inspector. 0.2's
   numbers should decide it, not taste.

**Risks.**

- *Parity at the cell level in 5b.* Overlay precedence
  (`resolve_overlay_style`) is subtle and under-tested. The cell-for-cell
  comparison against the painter, run over the whole e2e corpus while both
  paths exist, is the mitigation, and it is the reason 5 is split in three.
- *`ui_shell_frame_parity.rs` is a golden, not a cross-check.* Its own
  header says so: the production copy of the second computation was deleted,
  so the test compares the tree against a reference it also owns. It cannot
  catch both sides being wrong together. Do not treat it as proof of a
  stage's correctness; the e2e corpus is the real net.
- *The borrow shape.* `fold`'s `HostPainter` takes `&mut self` on the host
  because painting a split needs `WindowBuffers::with_all_mut`, which is why
  `Ui` lives beside the `Editor` rather than on it. Stage 5 moves the pane's
  content *into* the description, which means the description build now
  needs that same disjoint borrow — at build time rather than paint time.
  This is the one place where the migration could turn out to need a
  restructure rather than a translation, and it should be prototyped early
  in 5a rather than discovered in 5b.
- *Plugin-visible behaviour.* `lines_changed` hooks run between the current
  two bands and add overlays that paint has to see. Collapsing the bands in
  stage 7 changes when they run relative to paint; the ordering has to be
  preserved deliberately, not incidentally.

---

## 7. Definition of done

- `Draw::Host` is emitted for live PTY grids and embedded editor windows
  only, and for no other reason (or for nothing at all, if decision 1 goes
  the other way).
- One fold, one band, in paint order.
- No `frame.render_widget` in `crates/fresh-editor/src`.
- `ChromeLayout` holds the view-model and the fold's provenance, and no
  rectangle a painter measured.
- One pointer walk and one keyboard walk.
- Wide characters occupy the columns layout gave them, asserted by a test in
  the library and one in the fold.
- The frame-cost table in §Stage 0.2 is inside the budget from decision 2.
