# The retained-mode UI, as built

**Status: the architecture shipped.** Everything on screen that is not the text
pipeline is one description, reconciled into one `fresh-ui` tree, laid out once,
folded into one display list. This document is the **as-built record** and the
**open-work register**. It replaces the nine-stage migration plan that stood
here; the stages are done except where *What is open* says otherwise, and
*What the old plan got wrong* records where that plan was wrong.

The library's own contract — the three trees, the primitive set, the layout and
event rules, the non-goals — is `crates/fresh-ui/README.md` and the module docs
it indexes. This document is about the **editor built on it**.

Sections are referred to by title, here and from the code. There are no section
numbers to drift.

---

## The shape

**One description per frame.** `Editor::shell_frame` builds a `Frame` — a plain
value carrying every surface's content model (menu labels, status-bar elements,
the sidebar's sections, each pane's chrome, every open layer). `frame_tree`
turns that value into a `Node` tree. Nothing in the tree reads geometry, and
nothing in the editor reads a rectangle the tree has not yet produced.

**Three trees, in order of cost.** Descriptions are immutable values, rebuilt
freely. Elements are the persistent instances descriptions are matched against
by `(type, key)`. Render objects hold the expensive retained data — geometry,
cached measurements, scroll offsets, focus registration. A frame reconciles the
first into the second and lays out the third.

**The tree is the whole keyboard.** Every surface that can hold the keyboard is
in it: the modals, the prompt, the menu chain, the popups, plugin panels, the
explorer's header, and the active pane's content as the base's own focus
holder. Which `KeyContext` applies is a read of the focus chain
(`Editor::get_key_context` walking `path_to(focused)`), not an enum computed by
a ladder of rungs.

**The text pipeline stays behind `Host` leaves, by decision.** A pane's content,
the terminal grid and window embeds are `HostSpec::Leaf`s: layout gives them a
rectangle, paint gives them a position in the display list, and the pipeline
writes cells into exactly that rect. This is a design choice, not an unfinished
seam — see *Decisions that stand*.

**One tree, N windows, no window named in it.** Every key is scoped by window,
so window A's first pane and window B's first pane are different elements.
Element state does not survive a restart; anything `workspace.rs` serialises
stays on the editor.

---

## Life of an input event

This is the path a keystroke or a click takes from the terminal to an `Action`.
It is one path — the same for the TUI, the web bridge, the GUI shell and the
daemon — because all four call the same two entry points.

### Ingestion

Raw bytes are parsed by `fresh-input-parser` into an `Event` (Fresh parses
terminal input itself; see `terminal-input-parsing.md`). `Editor::handle_input_event`
dispatches on the kind:

- **Key** → the calibration translator rewrites the physical chord, then
  `Editor::handle_key_press`. That resolves the *layout reading* — a chord is
  both a physical key and the character it types, and `router::layout_reading`
  decides which the keymap actually binds — and calls `Editor::handle_key`.
- **Mouse** → `Editor::handle_mouse`.
- **Resize / Paste** → their own paths.

The other frontends enter at the same two functions: the web bridge and the GUI
call `handle_key`/`handle_mouse` directly, and the daemon's server calls
`handle_key_press`/`handle_mouse`.

### The stale-tree rule

Both entry points begin with `Editor::lay_out_shell_if_stale()`.

**Input is never routed over a tree older than the facts it routes over.** Every
read that asks the tree where the keyboard is — the PTY gate, the key context,
the unfocused-popup guard — must see a tree built from state as it stands. A
write since the last frame (a plugin pushed a spec, the host moved a panel's
focus) would otherwise be invisible to the key that arrives before the next
frame.

`Editor::shell_description_stale` is the flag. `handle_key` sets it *after*
routing, because a key may change any routing-relevant fact; the applier
(*Messages, and the applier*) sets it for every message that is not a transient
pointer fact. The invariant is asserted:
`a_typed_key_lays_the_tree_out_once` and
`a_divider_drag_that_moves_nothing_lays_out_nothing` both count layouts through
`view::shell::geometry::stats`.

### Keys: the pre-band, then the tree

`handle_key_routed` runs a short, explicitly-justified pre-band before the tree
sees anything. Each member is there because it is an **observer** — it must see
a key even when something above will consume it — which no first-consumer walk
can express:

1. The event-debug dialog, which intercepts everything.
2. A plugin awaiting `editor.getNextKey()`.
3. Transient-popup dismissal (a hover popup must vanish when you start typing,
   even under a modal). The decision is `router::should_dismiss_transient_popup`.

Then the key is translated to the library's vocabulary by
`view::shell::input::key` and handed to `Editor::shell_dispatch`. A key with no
`fresh_ui::KeyCode` variant (a media key, a bare modifier) is declined here —
nothing could have been bound to it.

**There is no pipeline tail.** A key the tree declines everywhere is nobody's.

### Inside the tree

`shell_dispatch` does three things before routing, in this order, and the order
is load-bearing:

1. `apply_settled_shell_messages()` — drain what the *previous frame's* focus
   settle left in `Ui::pending_messages`. Applied before routing, they say what
   was true when the key arrived; applied after, they would overwrite a focus
   this key just moved. `Editor::render` drains the same queue at the end of
   every frame too, so a settle's facts reach the editor with the frame that
   settled them — a panel a layer covered hears it then, not on the next key
   (`UiFact::PanelKeyboard`); this drain is what is left for a key that
   arrives before any frame.
2. `lay_out_shell_if_stale()` — the stale-tree rule again, because step 1 may
   have dirtied it.
3. Snapshot `EventFacts` — what the menu bar was showing *before* any message,
   and where the pointer was. A toggle needs the "before" fact: by the time a
   press is handled the menu has already shut.

Then `Ui::dispatch(input)` → `route_input`:

**For a key** (`focus::dispatch_key`), the chain is `path_to(focused)`, root
first:

- **`propagate_key`** walks the chain **down** (`on_key_capture`) and then
  **up** (`on_key`). Capture is how an ancestor pre-empts a focused descendant
  — a plugin panel's `defineMode` bindings ride here, as an `on_key_capture` on
  its interior, and resolve *chords* as well as single keys: the prefix so far
  is `panel::Keymap::chord`, held on the window rather than per-panel, and a
  key that extends or abandons one comes back as `UiFact::ChordPending` /
  `ChordAbandoned`. A handler returns `Option<M>`; the control object carries the
  `Flow`, which is `Continue` or `Stop` and nothing else. **Claiming and acting
  are separate**: a handler that returns a message without calling `stop()` has
  acted and let the key travel on, which is what the third `Flow` variant used
  to say before it was deleted as redundant.
- **`resolve_intent`** — the nearest ancestor whose `shortcuts` bind this chord
  names an `Intent`; failing that, the root's table.
- **`run_action`** — the same intent resolves to a different action depending on
  where focus is; nearest wins.
- **`dismiss_for_key`** — Escape closing a menu is the menu's reply, not a key
  that also belongs to what is behind it. A layer that dismissed itself
  `passing_through` is out of the way and the key continues.
- **`key_stops_at_modal`** — a modal layer owns the keyboard *including the keys
  it declines*. This is the last thing a host with its own pipeline needs told.

**For a pointer event**, `route(pos)` produces the stacked hit paths and
`propagate_all` runs capture → target → bubble along them. A press clears any
stale capture first (a capture still held means its release never arrived).
A `capture_pointer` on press keeps every move and the release wherever the
pointer goes — which is how the tab drag, the dock's grip, the split separators
and both pane scrollbars work, and why the old `PointerGrab` roster is down to
one member. While a `Modality::blocks_pointer` layer is up, an event nothing
answered is claimed by the layer itself.

`Dispatch { msgs, claimed }` comes back. **The claim is the tree's word alone** —
producing a message and taking the event are different things, and there is no
second verdict folded in afterwards.

### Messages, and the applier

Handlers do not mutate the editor. They return a `UiMsg`:

```rust
enum UiMsg {
    Action(Action),   // anything a user could bind
    Ui(UiFact),       // the positional half — ~110 variants
}
```

The split exists because `Action` is the rebinding and serialization currency:
it deliberately carries no click-at-byte, no drag-to-offset, no
select-tab-(leaf, index). Those are facts about *where*, meaningful only to this
frame, and putting them in `Action` would pollute the keybinding namespace with
things nobody can bind.

`shell_dispatch` then drains what this dispatch itself queued (`take_messages`),
asks `Ui::needs_frame()` — the library's own answer to "is the frame stale",
covering the changes that produce no message at all, like a `List` writing its
own hover through an updater — and calls `apply_shell_messages(msgs, facts)`.

The applier is one loop:

- `UiMsg::Action(a)` → `Editor::handle_action(a)`. Straight into the pipeline
  that has always applied actions; the migration changed nothing about it.
- `UiMsg::Ui(fact)` → `Editor::apply_ui_fact(fact, facts)`, a match over the
  fact vocabulary.
- After each message: `shell_description_stale = true`, **unless**
  `UiFact::is_pointer_transient` — a hover, a wheel, a grip's drag change
  nothing the *next* input's routing reads, and marking them stale cost a layout
  per motion report.

Marked *after* the applier, never before: a fact describes the tree that
produced it, and its applier reads that tree.

### Reaching an `Action`

Most facts are self-contained (`Hover` restyles, `PaneScrollbarDrag` moves a
thumb). The ones that carry a key onward are the **keyboard seams**:
`PaneKey`, `SidebarKey`, `PanelKey`, `PromptKey`. A surface holding the keyboard
*claims* the key in the tree — the key is that surface's — and what it does with
a key it does not bind is its own business. For the base, that business is
`Editor::hand_key_to_editor` → `chrome::base::dispatch_base_key`, which is the
only way in:

1. The unfocused-popup interception (a merely-visible popup holds no focus, so
   nothing in the tree is listening for its cancel/focus bindings).
2. `get_key_context()` — the focus-chain read described in *The shape*.
3. The PTY forward, when the context is `Terminal` (which the pane's own leaf
   settled) and `Ui::raw_input()` agrees.
4. Plugin mode bindings, via `router::mode_key_disposition`.
5. Composite-buffer routing.
6. `router::chord_or_key` — chords first, then the context's keymap →
   `ChordDisposition::{Chord, Pending, Resolved}`.
7. `Editor::handle_action(action)`.

Everything in steps 4–6 is a pure decision function in `input::router` that never
sees the `Editor`; the shell builds a narrow view, asks what the key *means*,
and applies the effect.

### Back to a frame

`handle_input_event` returns whether a redraw is wanted. `Editor::render` builds
a fresh `Frame`, lays the tree out, runs the plugin hooks, runs the **content
pass** (which formats every text pane's rows and settles its caret), and only
then paints — so a pane's leaf places the display list's cursor from what *this*
frame settled. The fold turns the display list into cells, resolving each
`Draw::Host` to its pane.

### The one asymmetry

**The keyboard has no pipeline tail; the pointer still does.** After
`shell_dispatch` returns unclaimed, `handle_mouse_impl` continues into a legacy
walk (`pane_content_takes_pointer`, then a match on the event kind) for drags,
releases and trackers whose press is not a node's. That walk cannot say what it
changed, so it marks the description stale for every press and release it takes.
Its remaining members are listed under *Smaller residue*.

---

## Where each surface lives

| Surface | Owner |
|---|---|
| Menu bar and dropdown chain | `view::shell::menu` — content model, layers, shortcuts |
| Status bar | `view::shell::status_bar` — content model, yield order, and the reads back off the row |
| Prompt row, suggestions, overlay card | `view::shell::prompt`, `prompt_line`, `overlay_prompt` |
| Sidebar / file explorer sections | `view::shell::sidebar`, `file_explorer` |
| Dock, floating and pane-mounted panels | `view::shell::panel` — one `Interior` for all placements |
| Split grid: panes, dividers, tab strips, scrollbars | `view::shell::splits` |
| Pane content, terminal grid, window embeds | `HostSpec::Leaf` + the text pipeline |
| Settings, keybinding editor, calibration, trust | `view::shell::settings`, `keybinding`, `modal` |
| Confirmations (quit, close, delete, overwrite, …) | `view::shell::confirm` — a `Modality::Exclusive` card over a `Scrim::Dim`, driven by `app::confirm_dialog` |
| Popups, context menus, theme inspector | `view::shell::popup`, `context_menu`, `theme_info` |
| The web's projections | `view::scene` — reads rectangles off the tree by key |

`app/chrome/` is no longer a chrome system, and no longer a registry either:
what is left there is the handlers those nodes dispatch *to*, two of which are
hover reactions the `UiFact::Hover` arm calls by name.

---

## What is open

Everything still owed, one line each. The sections below give each of these
its reasoning, and also record what **closed** — kept because the argument for
the shape is what stops it growing back.

- **Composite buffer panes** — never migrated at all: `compute_pane_layout`
  writes `pane_widths`, four hand-rolled hit tests read it back, a fifth site
  hardcodes `gutter_width = 4`. The largest piece, and its own change.
- **The keyed geometry index** — `Ui::find_by_key` is still a depth-first walk
  with ~180 editor call sites, some inside per-item loops. The live asymptotic
  hole.
- **The gutter** — line numbers, folds and diagnostics are painted, not nodes,
  so `click_geometry` re-derives the compose-mode gutter reclaim by hand. The
  run form is blocked on frame order; the gutter as its own *leaf* is not.
- **Per-frame deep clones** — `panel_interior` clones the spec and re-`Rc`s it
  every frame, so `ptr_eq` can never hold. Precondition for the next one.
- **The memos sit below the work** — all three `memo` sites cache the node
  build, not the content pass `shell_frame` redoes every frame regardless.
- **Instrumentation** — no purity check, no `BuildCx`-cannot-see-geometry type
  guard, no frame-level reconcile count, and no `benches/` at all, so every
  claim about frame cost is unmeasured.
- **The web** — `view::scene`'s region views retire one at a time, as each
  surface's rows reach the display list.
- **The shell's stylesheet** — shipped, with three named gaps left: class-keyed
  *metrics*, the indented-card box, and per-side padding.
- **Smaller residue** — `Paint::Lit`, no palette-resolve cache,
  `EntryDialogState`, and the pointer's legacy walk (see *The one asymmetry*).
- **Drag state lives twice** — every drag is routed by a node's capture, but
  all except the split separator still keep their gesture in `MouseState` and
  rely on the legacy walk's blanket sweep. See *A drag's state is the
  gesture's*.

### The markdown document view

**Landed.** `WidgetSpec::Text { markdown: true, rows > 1 }` was the last widget
kind whose description ran the old immediate-mode renderer inside `build` — a
full re-shape of the document per frame, to recover a caret row — and the
reason the text projection outlived every other consumer. It is a wrapped run
in a viewport now, and the design is worth recording because four other
things had to move for it.

**The shape.** `kinds::text::markdown_document` renders the document as *one*
styled entry with the NBSP indentation normalised back to breakable spaces;
`entry_runs` turns it into runs; `text_runs(...).wrapping(Wrap::Hanging)`
inside a `viewport` lets layout wrap it at the width it settled on. A byte of
that string is the one coordinate everything shares. The caret is a one-byte
`selection_bytes` wash in the surface's ink plus `reversed` — the block caret
this surface always had — and a live selection is a wider wash that replaces
it. The run carries `prose_run_key(widget)`; the viewport keeps the widget's
own key, where the window facts and the focus ring expect it.

**The state holds the document.** `resolve_panel` carries states and never
seeded one, and the kind's renderer — which did — no longer runs for a
described panel. So `carry_instance_states` seeds a markdown document's
`Text` state from the same rendered text the run displays, and re-seeds it
when that text changes; a width change is not a text change any more. Copy
yields the document. `resolve_described_panel` has no bail-out left.

**A press and a drag are the run's.** A press answers `Event::text_byte` from
the rows layout shaped and raises `UiFact::WidgetProsePress { byte, mods }`;
the run captures the pointer, moves raise `WidgetProseDrag { byte }` (`None`
past the text means the end), and the release raises `WidgetProseRelease`.
The applier moves the caret by byte — extending from the anchor for Shift or
a drag — and the host keeps only `prose_drag: Option<(PanelKey, String)>`,
which says a press is live because a `Move` cannot say so itself. That is not
a grab: routing is the tree's capture. `PointerGrab`, `pointer_grab()`,
`WidgetTextDrag`, `handle_mouse_drag` and the arena-backed press and drag
functions are deleted.

**Vertical keys are the host's.** `Up`/`Down`/`Home`/`End` (with or without
`S-`) mean rendered rows, and `kinds::text::text_key` has neither the width
nor the tree — the sole reason the shadow editor over the reflowed rows
existed. `Editor::prose_vertical_key` runs before the kind's `on_key`: it reads
`Ui::text_rows_in(panel root, run key)`, resolves the target with `cell_of` /
`byte_of`, and moves the caret by byte, selecting for `S-`. Everything else on
the surface — character and word motion, selection, Copy — stays the kind's
and stays logical.

**The reveal is the tree's.** After a key move the row holding the caret may be
outside the window, and which row that is only layout knows. The host keeps
one `Rc<Anchor>` per panel (`Editor::prose_reveal`, a `RefCell` map — an
`Anchor` binds to its element on mount, so a fresh one each frame would bind
to nothing), the viewport is `anchor_to` it, and the applier calls
`Anchor::reveal_byte(run key, byte)`.

**Three library pieces, each with its caller in the same change:**

- `Ui::text_rows_in(root, key)` — `text_rows` searched from a subtree, the
  same standing as `item_window_in`: a widget's key is unique only inside the
  panel that owns it.
- **A captured pointer still reports the byte under it.** While a gesture
  holds the pointer the path ends at that gesture, which has no text; the
  routing now asks the deepest descendant under the pointer that answers
  (`text_byte_under`). Without it a drag could start by byte and never grow.
- **A wash carries its attributes.** The fold laid a wash's background only;
  a wash that names `reversed` is a block caret, and one that dropped its
  attributes could not be.

**Pinned by:** `a_markdown_documents_state_holds_the_document_not_its_reflow`,
`keys_over_a_markdown_document_move_by_the_rows_the_wrap_made`,
`a_press_on_the_prose_places_the_caret_by_byte_and_a_drag_selects`,
`the_caret_lands_on_the_row_the_wrap_put_it_on_after_a_width_change`,
`a_keyless_markdown_document_is_a_wrapped_run_and_nothing_else` (the welcome
screen's code sample), `a_captured_move_reports_the_byte_under_it` and
`text_rows_are_read_from_the_subtree_that_owns_the_key`.

**What it left for the deletion that followed.** With both bail-outs gone,
the text projection's geometry had no writer, and it is deleted — see *Delete
the widget text projection* below for what is left of the projection and why.

### Delete the widget text projection

**Done.** The projection — `render_collected`, `WidgetImpl::collect` and
every kind's collector, the container assemblers, `CollectedOutput`,
`RenderOutput`, `RenderContext`, `RenderOptions`, `render_spec*`,
`render_button`/`render_bare_button` — is deleted, and so is everything it
produced: the click ranges (`HitArea`), the layout-box arena (`LayoutBox`,
`layout_box.rs`), the window each list was painted into (`PaintedWindow`,
`WidgetPanelState::{painted, boxes}`), the floating panel's `Host` leaf
(`FloatingWidgetState::entries`, `Spot::content_*`,
`Panel::{height, anchored_width}`, `render_floating_spec`), and the
`widget_text_drag`/`PointerGrab` pointer path. A kind still answers for its
state, its keys and its presses (`WidgetImpl`), and the formatters the
description reads stay in `render.rs` and the kinds (`render_toggle*`,
`render_number`, `render_dropdown`, `render_tree_row`, `single_line`,
`completion_popup`, `markdown_document`, …). Every mount, update and host
re-render resolves the spec (`resolve_panel`) and nothing renders it: **no
description build runs a renderer**, and no host seam holds a rectangle,
hit range or window the tree did not lay out.

**The pane-mounted panel's buffer is derived from the tree**
(`app/pane_mirror.rs`). That buffer is where a pane panel's text is
*reported* from — `Ln`/`Col`, a plugin's `cursor_moved` and `getBufferText`,
the page reader's row and column — and it used to be the projection's rows,
written on every plugin update as a second rendering of the same spec. Now,
at the end of `Editor::lay_out_shell`, each pane panel's subtree is painted
unclipped (`fresh_ui::Ui::paint_subtree`: every row layout settled, the
ones a viewport scrolled away included, in-flow only) and folded through the
frame's own fold (`fold_band`) into a cell grid the size of the content —
for a page, its viewport's content; for anything else, its box — whose rows
become the buffer's lines. Line `n` is the tree's row `n` by construction.
The buffer is written only when the rows changed; the caret the tree placed
(`LayoutSpec::cursor`) seats the buffer cursor for a focused field, and a
page's cursor stays the reader's (`move_page_reader`). The one contract that
moved: the buffer fills on the next frame, not synchronously at mount — a
plugin that reads its own panel buffer inside the same tick as its update
reads the previous frame's rows.

**Residue the deletion exposed, now closed.** `user_scrolled` on the `List`,
`Tree` and `Text` instance states had no writer left (`latch_user_scrolled`
and the wheel branches that set it went with the window), while `resolve`
still read it and `set_selected_index` still cleared it. It is deleted,
with `text::clear_user_scrolled`. `widget_panel_render_heights` /
`widget_panels_with_stale_height` re-resolved a pane panel when its split
height changed. With nothing rendered by height, that did nothing beyond
marking the description stale, which layout does not need. It is deleted
with its bookkeeping (`record_widget_panel_render_height`,
`widget_panel_height`, `painted_panel_height`, `spec_has_auto_sized_list`,
`slot_for_panel_buffer`).

### Where the assertion was the only reader

**Closed.** Three surfaces had migrated in description only: the tree laid
them out, and the painter went on using the older mechanism, with the tree's
answer read by a `debug_assert` beside it. Recorded because the shape recurs
and the symptom — a passing test suite and a correct debug build — is the
opposite of alarming.

- **A pane's four rectangles.** `split_layout` built a throwaway `Ui`, laid
  `pane_interior` out again at the pane's box and read four rectangles back by
  key, once per pane per frame. `paint_leaf` had `pass.rects.content(split_id)`
  in hand, asserted the two agreed, and then painted into the throwaway's. Two
  further sites took it as a fallback for a case that could not arise — the
  fallback's `split_area` came from `PaneRects::visible`, so a pane missing
  from `rects` already had a zero box and the fallback laid *that* out. The
  instrument could not see any of it: `geometry::stats::note_shell_layout` is
  called from `render`, and `split_layout` never counted itself. `PaneRects` is
  the only source now, `VisibleBuffer` no longer carries a rectangle, and
  `split_layout` is `#[cfg(test)]` — which is a stronger instrument than a
  counter, because a frame cannot call it at all. It keeps its one honest job
  beside `reference_split_layout`, as the oracle the description is pinned to.
- **The status bar's own area.** `publish_status_bar` asserted that "the
  retained tree and a fresh one must lay the frame out alike". Neither side was
  fresh: both resolved to `frame::regions_of` on the same retained `Ui`, and
  `shell_region_now`'s doc forbids building a throwaway one. The claim is real
  and now lives where it can be checked —
  `frame::tests::a_retained_tree_lays_the_frame_out_like_a_fresh_one` walks one
  `Ui` through frames that add and drop rows, then compares against
  `region_rects`.
- **The gutter's width.** `app::scrollbar_math` kept its own copy of the
  formula under a doc naming the stake ("any divergence makes scroll math wrap
  at a different column than the renderer"), and it had diverged: it missed
  byte-offset mode, and its doc described a `show_line_numbers` behaviour the
  code did not have. `view::viewport::gutter_width` is the one statement; the
  flag had no reader and is gone from the call chain.

### The oracles the migration was checked against

**Closed.** Four second implementations of layout survived under `cfg(test)`,
each kept because a replacement is only as trustworthy as what it was checked
against:

- `SplitNode::reference_leaves_with_rects` — the recursion the pane grid was
  before the description was.
- `SplitNode::get_separators_with_ids` — the walk that computed separator
  positions from the ratios.
- `split_rendering::layout::reference_split_layout` — the hand-derived
  arithmetic for a pane's four rectangles.
- `split_rendering::layout::split_layout` and `SplitLayout` — not a second
  *implementation* but a second *layout*: `pane_interior` in a throwaway `Ui`,
  kept after the painter stopped calling it so the tests had something to ask.

They are deleted, and the tests they served say what they were really claiming:

- **A grid is a tiling.** `the_grid_tiles_its_box` checks that every cell of
  the box belongs to exactly one pane or one divider, at every shape and size.
  The old sweep compared the tree against `reference_leaves_with_rects`, and
  since both called `split_rect_ext` the only thing it could catch was the
  *structure* disagreeing — a separator cell not reserved, a child given the
  wrong remainder. Both break the cover, and a cover needs no second
  implementation. `every_container_has_a_divider` adds the part a cover cannot
  see: that a container the model has is actually in the tree, rather than its
  box being quietly tiled by panes alone.
- **A pane's interior is stated, not compared.** The strip is the top row and
  spans the box, the vertical bar is the last column beside the content, and
  the horizontal bar stops short of that column rather than running under it —
  which was the one part a reader got wrong from the picture, and the reason
  the frozen arithmetic was kept. The four pieces are read off the real mount
  by key (`tabs_key`, `content_key`, `vscroll_key`, `hscroll_key`), not laid
  out again.
- **Fold against layout, not against a model.** `the_fold_reaches_every_pane_at_its_own_rect`
  now compares the rectangle the fold hands each pane against the rectangle
  layout gave it. That was always the claim; the model walk was standing in the
  middle of it.
- **The hand-written rects were always the real assertion.**
  `shell::geometry`'s tests already spelled every rectangle out *and* compared
  against the oracles. The spellings stay and the oracles go.

What is left, and why it is not the same thing: `frame::region_rects` builds a
`Ui` of its own, and `a_retained_tree_lays_the_frame_out_like_a_fresh_one`
cannot be written without it — a fresh tree is that test's second side by
definition. It re-runs the one description through the one layout engine; it
does not restate any rule. `split_rect` is a `cfg(test)` alias that passes two
`None`s to the production `split_rect_ext`.

### The tab strip is measured twice

**Closed, and the library grew the axis it needed.**
`view::shell::tabs::lay_out` did not use the layout engine: it took the
strip's width from a `layout_reader`, measured every piece with `str_width`,
decided a name cap from the total, worked out whether the `<` and `>` arrows
appeared, sliced each label by column against a scroll offset the editor held,
and emitted fixed `Sizing::Cells` nodes — a finished picture handed to a tree
with nothing left to lay out, on the most-looked-at row in the editor.

Because layout never decided the widths, the offset could not come from it, so
a second full measurement ran in `view::ui::tabs` (`calculate_tab_widths`,
`tab_name_cap`, `full_tab_label_width`, `tabs_render_width`,
`scroll_to_show_tab`) behind `Window::ensure_active_tab_visible` and its five
call sites, and a third partial one in `Window::split_tabs_width`, whose
comment said to "Mirror the show-flags in `render_split_tab_bar`". All of it is
deleted, along with `SplitViewState::tab_scroll_offset` and its persistence.

**What the library was missing was narrower than it looked.** `RenderData`'s
`scroll` and `scroll_max` were already `Point`s, the clamp was already
per-axis, the cell viewport already computed `max.x`, and `arrange` already
translated by `sc.x`. What did not exist was any way to move a window across
*by reference to its content*: every `Anchor` command wrote
`Point::new(scroll.x, …)`, so the only horizontal API was `scroll_to`, which
asks the caller for the number the window is there to compute.

The axis is now the window's (`Node::scroll_axis`, published on `ScrollInfo`),
not the command's — `reveal_key` means "put this inside", and which way that is
has one right answer. Every command became axis-correct at once, with no new
public command and no second spelling of any of them.

**And the affordance is the window's too.** A one-row horizontal window has its
content on the rows a bar would need, so it caps its ends instead:
`Draw::Overflow { axis, end }`, one cell at an edge that still has content
behind it. It is a draw kind for the reason `Draw::Rule` gives — whether there
is more depends on the offset layout settled on, and a description that decided
it would have to know the offset to be built and be built to produce the
offset. The cells are reserved whenever the content overflows, *not* per end,
or the reservation would depend on the offset that depends on it; the glyphs
come and go inside them, so the tabs no longer shift by a column the moment the
`<` appears, which the painter's strip did. The terminal draws `<` and `>`
there, as it always did, because the glyph is the backend's.

The editor's whole remaining say is `Window::reveal_active_tab`: which tab to
show is a fact about the pane, and where that puts the window is the window's
answer. The wheel is still a fact (it dismisses transient popups and fires the
plugin hook on its way) but it moves the window rather than an offset.

**The `+` and the caps are the same kind of button.** The `+` rides in the
window after the last tab, where the painter put it when the tabs fitted; the
painter *pinned* it to the right edge when they did not, which is a placement
that depends on the overflow the window now owns. Revealing the strip's end
rather than the tab is what keeps it beside the last tab there too: `+` follows
the last tab, so on the last tab the window is asked for the button and brings
the tab with it. All three of `+`, `×` and the caps light on hover, in one
pair of theme names.

Hover on a cap is the library's, for the same reason the press is: a cap is
drawn because the *window* knows there is more that way, so the window is what
knows when the pointer is on it. `Geom::pointer` carries where the pointer is
into paint, and `Node::scrollbar_hover_theme` is the second name the surface
gives it.

**A node that draws from the pointer needs a frame when the pointer moves.**
Reading `Geom::pointer` at paint says what the cap looks like; it does not say
when to look again. `Enter` and `Leave` are the tree's answer to "the pointer
arrived", and they fire when it crosses an *element's* boundary — but a cap is
not an element, it is cells the window reserved, so sliding sideways out of the
tabs and onto the `<` crosses nothing. The cap lit only when the pointer
happened to cross some other node's edge on the way in: entering from above the
strip worked, entering from beside it did not. So `update_hover` asks the cap
the same question the press asks — `overflow_cap_hit`, before the move and
after it — and marks the window when the answer changes. A pointer wandering
inside one element still costs nothing; a cap lighting or going out costs the
frame it needs. `Ui::needs_frame` counts `layout_dirty` for this, which it did
not: every change that is not an element's had been invisible to the host loop
asking whether a frame was owed.

A cap's geometry has one statement of it, `render::object::overflow_caps`,
which both the paint and the hit walk read — a button that lights where it
cannot be pressed is not a button.

**And a cap is as wide as the buttons beside it.** One cell is enough to say
"there is more this way" and stays the default, but on this strip the `+` is a
padded `" + "` and a one-cell arrow is the odd one out to the pointer as much
as to the eye. `Node::scroll_cap_width` is the window's say in it: the measure
reserves that many cells at each end and the backend centres the glyph in them.
The strip asks for `NEW_TAB_BUTTON_WIDTH` and gives the caps the `+`'s own two
theme pairs, so `<`, `>` and `+` are one kind of thing and nothing about
meeting one tells you which it was.

**The name cap stays conditional, and one attempt to make it unconditional is
worth recording.** A tab name is capped at `TAB_NAME_MAX_COLS` only when the
tabs, with their names whole, are wider than the strip — as it was before this
arc. For a while here it was capped at *every* width, on the reasoning that the
old rule needed a measurement of the whole strip and a window can show what does
not fit.

That reasoning had a hole. The reason the old rule needed a pre-layout
measurement is that the cap was applied as **string truncation in the
description** — `label` called `elided_tab_name` and put an already-shortened
name in the node, which is the pre-fitted string this whole document is about,
in the one surface it is most about. Nothing had to be measured early; the
decision had to stop being made early.

It is **feedback** now, the same shape as the palette's column widths:
`tabs::natural_width` (what these labels measure uncapped) against the window's
outer width from the frame before. The outer width is what the strip row leaves
after the control cluster, so it does not move with the names — which is what
keeps the predicate from feeding itself and a frame from capping, fitting,
un-capping and overflowing again. One frame late after a resize, unset on the
very first frame, and both of those show whole names, which the window scrolls.

**The cost of getting it wrong was a 26-column name elided on a 160-column
screen showing one tab**, and four tests written by other people said so —
two of them by waiting for a label that could no longer appear, so they hung
rather than failed. They pass unchanged again.

**There was never a second rule here, though it looked like one.** A tab
disambiguated by path was appearing as `…/añadido.txt` where master showed the
whole `*719a543:notas/añadido.txt*`, which read as a separate decision about
path prefixes and was written up as one. It is the same cap: `elided_tab_name`
shortens a path-shaped name from the *front* (`elide_path_label`), so the file
name survives where trailing truncation would throw away the part that
identifies it. One rule, two shapes, and making it conditional again fixed
both — the test that hung on the full title passes with that title restored.

The cap's one wart, on master as here: two tabs that differ only in a prefix
the elision removes come out reading the same. Worth knowing before raising
`TAB_NAME_MAX_COLS` as the answer to anything.

**Checked against master, screen for screen.** Both binaries driven through the
same scripted scenarios in tmux — tab strip (hover, steps, wheel, activation,
close, `+`, drag-reorder, context menu), menu bar and dropdowns, palette, find
bar, file explorer with git decorations, settings dialog, keybinding editor,
file browser, splits, scrollbar drag — capturing every screen *with its
colours*, so a hover highlight is part of the comparison. 98 screens at three
terminal widths.

Every difference is one of the two intended ones: the strip's own rows, and the
status bar's `…` for `...`. Everything else is byte-identical, including every
hover highlight outside the strip.

One difference is worth recording because it is not a redraw of the same state:
after a wheel over the command palette, the branch's column widths are those of
the rows now on screen while master still shows the previous frame's, catching
up on the next input. That is `Ui::needs_frame` counting `layout_dirty` — the
wheel marks the window, so the frame it changed is drawn now. The same fix that
lights an overflow cap.

**Also checked against the installed 0.5.1 release**, driven side by side in
tmux:
identical cell for cell at every width tried, for the initial scroll, the
`<`/`>` steps, the wheel, tab activation and a vertical split — but for the
intended differences. The caps are three cells at each edge rather than one
arrow beside the content, reserved whenever the content overflows so the tabs
do not jump when the `<` appears (the release's do), which moves the strip
left; they wear the `+`'s ground rather than the separator's; and the status
bar writes `…` where the release wrote `...`, so one more character of the
message fits.

It also fixes one thing. After a vertical split the release leaves the active
tab cut off under the pinned `+` — `hotel_indexer.` with its `rs ×` missing —
because the offset was computed against a width that did not match what the
strip laid out, which is the defect `split_tabs_width` was added to paper over.
The window has one width and the tab is whole.

**Still true, and untouched:** hit testing, hover, the drag drop zone and the
web all read tab rectangles off the tree by key (`chrome::splits::tab_rects`,
`scene::tab_bar_view`), and `resolve_tab_names` and `elided_tab_name` were
never paint.

### Composite buffer panes never migrated

**Open.** Not a leftover — this surface has no description at all.
`orchestration::render_composite` computes a hand-rolled column split
(`compute_pane_layout`: ratio times available width, round, reserve a
separator) and writes it into `CompositeViewState::pane_widths`. Four
event-time readers then hand-roll a hit test over that vector:
`input::composite_router::click_to_pane`, two identical walks inside
`composite_buffer_actions::handle_composite_click`, and `pane_width` with an
`.unwrap_or(40)`; a fifth site hardcodes `let gutter_width = 4`.

That is the `screen_space` class exactly, and composite panes are not on
`app::types::layout`'s closed roster — which says adding a surface there
requires a ruling. The work is a pane-strip description under `view::shell::`
and four keyed tree queries in place of the walks. It is the largest piece
left and wants its own change.

### The status bar does layout by hand

**Closed.** `Editor::status_bar_description` measured element widths, computed
a left budget through `view::shell::status_bar::left_budget`, truncated the
element that straddled it and dropped the ones past it — all before the
description was built, which is the migration's own failure criterion: a
description with a pre-fitted string is still a picture the old renderer drew.
`Node::priority` ("higher yields last") had landed for exactly this and its doc
names this bar as the case it was built for; only the prompt's suggestion
columns had switched.

`shell::status_bar::yields_last` is the whole of what `left_max_width =
available - right_width - 1` computed: the right side and one spacing cell are
sized first, the left takes the remainder, and the row still paints left to
right. The cut is `Elide::Tail`, made at paint against the width layout settled
on — which also retires the hand-rolled truncation loop that walked spans to
keep their colours, and `view::ui::status_bar::truncate_to_width`, whose `...`
the tree writes as `…`.

Two things worth knowing, because both are places the mechanism does not map
one-for-one:

- **Priority is read off a row's direct children.** A clickable element's is
  the gesture wrapper, not the runs inside it, so a priority set on the runs is
  read by nobody and the right side quietly stops being reserved.
  `a_clickable_right_element_is_reserved_too` is the guard; the older
  regression test happened to use inert elements and would not have caught it.
- **The narrow-bar boundary changed shape.** `render_status` reserved nothing
  for the right below 15 cells, and `left_budget` kept that verbatim on the
  grounds that a boundary is behaviour. There is no width budget to switch off
  any more — the reservation is the yield order and applies at every width — so
  the boundary is now a statement about *which elements are on the bar*, which
  is the decision the editor already makes for the right-hand drop: below
  `BOTH_SIDES_MIN`, a right side that will not fit beside the left is not on
  the bar, rather than being kept and clipped to a few cells of itself. What
  was behaviour — the left side surviving on a bar too narrow for both — is
  preserved. What is not is one cell: where the right side alone overflows the
  row, the left used to be given a single column and is now given none.

What stays app-side is unchanged and is not geometry: which right-hand elements
appear at all is a content decision made from measured text, because a
description that listed elements layout would then silently discard would be
lying about what is on the bar.

### The keyed geometry index

`Ui::find_by_key` is a depth-first walk of the element tree, and
`view::shell::rect_of` wraps it. There are ~180 call sites in the editor, some
inside per-item loops (the web's dropdown projection does one full walk per
row). The design has always called for the library to publish `Key → Rect` as an
O(1) read after layout; nothing has been built. This is the live asymptotic hole.

**The caches it was going to retire are gone ahead of it, and the roster is
now empty.** `popup_areas` and `global_popup_areas` both read the popup's box
off the tree and then re-derived the content rect from it by hand, in two
copy-pasted blocks of border arithmetic — so they were a second *statement*,
not merely a cache. The content slot carries `popup::popup_content_key` now and
`popup::inner_rects_of` reads it, the web's projection and the transient-popup
probe ask the tree directly, and three of `PopupAreaLayout`'s seven fields
turned out to have no reader at all. `prompt_toolbar_boxes`, the roster's third
entry, named a field that had not existed for some time.

The last two, `suggestions_area` and `suggestions_outer_area`, were a copy of
`shell::prompt::{suggestions_list_rect, suggestions_rect}` — already a read of
the tree — kept for one reader, the web `Scene`, which now asks for them the
way it already asked `overlay_prompt::regions_of` for the card's bands in the
same function. The count beside them was `prompt.suggestions.len()` copied, and
the doc comment listed a third consumer, `cursor_obscured_by_overlay`, which
exists nowhere.

What is left under that name is `suggestions_window`, and it is not a
rectangle and not a cache: it is **feedback**. The palette's description
measures its columns against the rows that will be on screen, and which rows
those are is the window the *previous* layout arrived at — the one thing about
the popup a fresh read cannot supply, because the tree is the thing being
described. So `ChromeLayout`'s paint-recorded geometry roster is empty, and the
class it enumerated has no members.

### Per-frame deep clones

`Editor::panel_interior` does `spec.clone()` then `Rc::new(...)` at two sites, so
the `Rc` is fresh every frame and `ptr_eq` can never hold; `instance_states` is
cloned wholesale beside it. `WidgetPanelState::spec` is an owned `WidgetSpec`.
The registry should hold the `Rc` and hand out clones of it — which is also the
precondition for the next item.

### The memos sit below the work

Three `memo` call sites exist: the file explorer's rows, the menu bar and the
status bar. Each memoises the *node build* from a content model that
`shell_frame` recomputes every frame regardless — the menu walk, the status-bar
content pass. The memo skips the cheap half. Memoising the content computation,
and adding one at the panel seam keyed on
(`Rc::ptr_eq`, state version, focus fact, hover), is the actual work.

Note that `Node::shared()` has no editor call site **and cannot have one**: it
asks the caller to hold an `Rc<Node>` across frames, and a host deriving its
description from a store has no `Rc` to keep. `Component::memo` is the answer
for this host. Counting `.shared()` sites measures something the architecture
rules out. The same goes for ambients: the theme travels as theme *keys* on runs
(that is the provenance design — see *Decisions that stand*), and exactly one
struct threads a resolved `Arc<Theme>`. There is no hand-threaded context to
convert.

### Instrumentation

- **The purity check.** A state-generation counter per element, sampled before
  and after `build` and asserted unchanged in debug builds, with `Cache<T>` the
  one exemption. Not built — and *The markdown document view* is exactly the
  kind of thing it would catch.
- **Geometry validity in the type.** `BuildCx` should expose no geometry at all;
  only a `LayoutReader`'s closure should receive a `Geometry`, so "read after
  layout" is a type rather than a `debug_assert`.
- **Reconcile accounting.** `Ui` counts builds per element; there is no
  frame-level count of reconciled elements or re-laid-out nodes, so "a frame
  with no state change reconciles a bounded number" cannot be asserted. The
  editor-level counter (`geometry::stats`, one shell layout per frame) is
  asserted and is the coarse version.
- **A frame benchmark.** `crates/fresh-editor/benches` does not exist. Any
  claim about frame cost is currently unmeasured.

### The gutter

Line numbers, folds and diagnostics as **runs** beside the pane's leaf is not
reachable, and the reason is the frame's own order: what a gutter row shows
depends on which *view line* it holds, and view lines come out of the content
pass, which runs after layout by design so that the caret a frame places is the
one that frame settled. What *is* reachable is the gutter as its **own leaf**
next to the content's, painted by the same pass — worth doing on its own, since
the content leaf's rectangle would stop including a margin it subtracts back out
of every byte-at-cell answer, and a press in the gutter would become the gutter
node's. The run form waits on the same thing the web does: a pane's rows
reaching the display list.

### The web

The DOM fold of the display list ships for plugin panels. `view::scene`'s
remaining region views are a redundant projection layer — they read rectangles
off the tree rather than re-deriving them, so they are not a second geometry
system — and each retires as its surface's rows reach the display list.

### The shell's stylesheet

*(Folded in from the retired `shell-stylesheet-design.md`.)*

**A class names what a thing is; each backend owns a table from class to
appearance.** The terminal's table is Rust (`app::shell_style`), the web's is
CSS, and the description carries the naked label with no decoration at all. A
node carries a *list* of classes, space-separated, as CSS does — `button primary
focused`, not one name per combination.

**Shipped.** `Node::classes(..)` sets the slot; `layout` inherits it exactly as
it inherits the theme, and every `Item` carries a `Classes` beside its
`ThemeKey` — an opaque string the library stores and never interprets. `Rule`
carries `border: Sides` and `pad_x`; `shell::widgets::button_node` builds a
button from the naked label with the frame reserved, and the fold's `Draw::Fill`
arm draws the glyphs. The web reads it: `TreeItemView::classes` → `data-class`
→ a pill in CSS, on the same item the terminal draws `[` and `]` on. That is the
payoff — the web is no longer re-colouring a bracketed string, because there is
no bracketed string to re-colour.

**And the fill is a width now, not a longer label.** `full_width` used to be
spelled by padding the label out to the enclosing panel's columns
(`fill_button_label`), so that the band a focused button paints — which is its
box's ground — reached the row's end. The box is `Auto` under the column's
`Stretch` instead, and the run says `Elide::Tail` for the `…` the padding helper
wrote by hand. Two consequences, and both are the point: a menu *inside an
anchored popup* can now ask for full-width rows — the box hugs its widest row
and the stretch widens the rest to it — where filling used to blow the popup out
to the panel width; and the orchestrator's `menuRows` stopped padding its own
labels to the widest to align them, so the two modes it kept for those two cases
turn out to have been one request. It is the same rule as `Draw::Rule`, whose
doc states it generally: how much of a thing fits "is a function of the
rectangle layout settled on, which is the whole reason this is a draw kind and
not text of a computed length".

**Open, and the order is forced:**

1. **`Draw::Border` must become one four-sided border**, each side present or
   absent, with a corner style the terminal reads and the web ignores — so
   `Box` and `Sides` stop being two variants and become one shape (the button's
   `Sides` is already half of it). This is also the structure/appearance line
   held properly: *"there is a box here and it groups these children"* is
   structural and belongs to the description; *"a box is drawn `╭─╮`"* is
   appearance and belongs to the class. `Draw::Border(BorderStyle::Rounded)` is
   on the wrong side of that line today.
2. **Then the `card` class.** Blocked on the above, and the block is a finding
   rather than a deferral: the dock's `open_card_edge` **drops the right side**
   so the active card opens onto the editor beside it, and `Draw::Border` draws
   four sides or none. Until a box can drop a side, a `card` class can only
   describe the cards that are *not* selected — the one state nobody is looking
   at. A partial version was considered and rejected: putting the class on the
   block's `col` gives the web a box the width of the *panel*, while an indented
   card is two columns narrower. That is a wrong answer shipped, not a smaller
   right one.
3. **Per-side padding (`Edges`).** `Pad` is symmetric in `x` and `y`. Not yet
   needed — the focus marker turned out to belong to the row, not the button —
   and `Rule::reserved_x` asserts that symmetry rather than assuming it, so the
   day a rule wants sides of different widths that assertion is the trigger.

**And the item that belonged to the text-projection chain is closed.**
`render_button` went with the projection; the button's frame is the node's
own (`button_node`, reading `widgets::frame::Frame::BUTTON`), and the class
table is the one statement of its ink.

**Rules that stand:** an unknown class decorates nothing and is *not* an error,
while an unresolvable **ink** stays the loud failure it is — two slots, two
disciplines, the strict one keeping its audit. A class reaches the DOM as an
attribute value and is never interpolated into markup, because an open
vocabulary means the string may come from a plugin. Theme provenance survives a
class: the `fg_key`/`bg_key` pair still reaches the inspector.

**Still undecided:** whether a plugin may set classes (the mechanism allows it
the moment the vocabulary is open; the widget spec would need a field and the
web a policy for names it does not recognise), and whether the rule table ever
becomes theme-file data. Nothing depends on either.

### Smaller residue

Still open:

- The pointer's legacy walk (see *The one asymmetry*), whose members are now
  the terminal's own mouse and the multi-click detector; the markdown drag was
  its last grab and is the run's own capture.
- `Paint::Lit` — the theme-provenance escape hatch, still live in the fold. An
  item whose ink resolved to literal colours files nothing, so it is a blank
  *tier* in the audit rather than a blank surface.
- No palette-resolve cache.
- `EntryDialogState` still carries the settings entry dialog's own state model.

- `SettingsState` and `EntryDialogState` are still two models of one thing.
  Their shared list-row gestures now live once, on
  `view::settings::surface::SettingsSurface` (`select_list_row`,
  `commit_list_draft`, `remove_list_row`, `live_text`, `spec_for`,
  `current_spec`, `live_list_row`). Every copy was reachable: a coverage run
  showed `state.rs::remove_list_row` and `entry_dialog.rs::select_list_row`
  never executing, but that was a test gap, not dead code. What each type
  still does its own way is what a value change means and how a text-list
  row opens.

Closed, and worth keeping the reasoning for:

- **The settings dialog's second copy of its own heights is gone.**
  `ScrollItem for TreeRow` fed `ScrollablePanel::ensure_focused_visible`, which
  walked every row's height to compute an offset nothing read — the tree
  column's window is its `List` element's, and the list reveals its own
  selection. `ScrollablePanel`, `ScrollItem` and `FocusRegion` went with it,
  and with them the body's old measuring kit (`ItemBox`,
  `SettingItem::{layout_box, description_rows_for}`,
  `SettingControl::control_height`), whose note in `items.rs` had claimed it
  was already deleted. `ScrollState` survives for the keybinding editor's
  table, which is not the tree's yet.
- **The `ChromeComponent` registry is gone.** Its last cargo was two hover
  reactions, dispatched through a trait whose `on_hover_change` had a `false`
  default body — so a surface that was registered but had not written one took
  the default in silence. The menu bar did exactly that: hovering a submenu
  parent opened nothing while `menu_hover_reaction` sat with no callers at
  all. The `UiFact::Hover` arm now calls both reactions by name (with `|`, not
  `||`, so one answering "changed" cannot decide whether the other is offered
  the move), and a reaction that is not run is a name that does not resolve.
  `app::chrome`'s modules stay: they are where each surface's `Editor` methods
  live, which was never the registry's doing.
- **`render_phantom_leaf` is not a duplicate, and this note used to say it
  was.** It was listed here as "two paint paths in one function", because
  `paint_card_preview` reaches it down one branch and `shell_host::paint_embed`
  down the other. They are not two spellings of one job: `paint_embed` draws
  *another window's whole grid* into the card, and `render_phantom_leaf` draws
  *one buffer* into a rectangle through the per-leaf pipeline. Both write cells
  because the pane content pass does; that is *The gutter* above, not a
  duplication. Nothing to delete here.

### The sweep for code nothing calls

**Closed, and it is a method rather than a one-off.** `pub` hides dead code
from the compiler, and a migration leaves its residue `pub`: the old half of a
pair stays exported long after the new half stopped calling it. So the sweep is
mechanical — every `pub fn` whose name appears exactly once in the workspace,
counting the editor, the library, their tests and the examples — and it runs to
a fixed point, because rustc *does* report private dead code and each round of
deletions exposes another. The stopping condition is that the dead-code warning
set is unchanged.

**In `fresh-ui` it found nine, three of them whole mechanisms.** The working
rule is that a library change needs a caller in the same PR; these had none.
`Anchor::scroll_to_end` took `Command::ScrollToEnd` and its arm in the command
loop with it. `Event::release_pointer` took `Ctl::release_request` and the
branch in `apply_controls` that cleared the capture — a second way out of a
capture that nothing ever took, where every caller relies on the first (release
the pointer, or unmount). `Commands::is_bound` asked a question that sending
answers. The rest were sugar: `Event::is_key`, `Node::on_secondary_click` (the
gesture kind stays — it is the context menu's), `Node::child_if_some`,
`InitCx::geometry_of`, `GeomHandle::rect_of_key`.

**In the editor it found the other half of things already deleted.**
`ExplorerTrailingSlotProvider::hit_test_width`, its two overrides and the
`COMPATIBILITY_TRAILING_SLOT_HIT_WIDTH` they returned — the trailing slot is a
node, and how wide it is to the pointer is its rectangle.
`entry_dialog::layout_field_action_buttons`, whose comment said it was "shared
by the renderer and the click hit-tester so their geometry can't drift" when
neither had existed for some time. `Popup::scroll_state`, which packed
`(total, visible, offset)` "for scrollbar rendering" the window does itself,
and `Popup::description_height`, which wrapped the description a second time to
measure it. The prompt's own `select_next_suggestion`,
`select_prev_suggestion`, `scroll_results` and `get_final_input`.
`FileExplorerRenderer`, by then a namespace around one predicate about paths,
which sits beside the row that asks it.

**And then the same sweep over the whole view layer: 880 lines, nineteen
files.** Some was plainly the migration's — `Popup`'s four unused builders (the
fields stay; every one is set by a struct literal and read), its three
`is_*_popup` predicates, `MarginManager::{get_at_line, right_total_width}`,
`LineWrap::{cursor_sig_for_line, char_position_in_layout}`, `Viewport::
{mark_needs_sync, sync_with_cursor, ensure_cursors_visible}`, the widget
renderer's `blank_list_row`, `render_section_top_border` and
`wrap_in_side_border` with the two border constants only they used. Some
predated it and was dead anyway: `ScrollSyncManager`'s group API and the
`next_id` only `create_group` touched, ten `SettingsState` accessors, ten
`SplitManager` ones, seven on `CompositeViewState`, and a scatter of others.
The distinction did not change what to do about any of it.

**The count misses what tests keep alive.** A `pub fn` whose only callers
are tests appears more than once, so the sweep passes over it. A later
coverage run found these: `Popup::{with_position, with_width,
with_max_height, with_transient}`, `PopupListItem::with_icon`,
`MarginAnnotation::breakpoint` and `MarginManager::{without_line_numbers,
get_line_indicator, remove_line_indicator, annotation_count}`. They are
deleted, and their tests now go through the production path:
`Editor::show_popup`, field assignment as the hover path does it,
`get_indicators_for_viewport` and `render_line`. `update_width_for_buffer`
was on the same list but has callers in split rendering, and stays. To
catch this class, count callers outside `#[cfg(test)]` modules and
`tests/`, not names.

---

### A drag's state is the gesture's

**Open for five drags, done for one.** Since the pointer migration, every drag
is routed by a node's pointer capture: the pane scrollbars, the split
separator, the file explorer's border, tabs, and text selection. The routing
moved, but the state did not. Most gestures still keep it in the window's
`MouseState` (`dragging_scrollbar`, `drag_start_row`, `drag_start_top_byte`,
`dragging_horizontal_scrollbar`, `drag_start_hcol`, `drag_start_left_column`,
`dragging_file_explorer`, `drag_start_position`, `drag_start_explorer_width`,
the `drag_selection_*` fields, …). `clear_active_window_drag_state` clears
them in the legacy walk's `Up` arm and in `release_pane_content`. A captured
release never reaches that walk, so the sweep is either redundant or covering
for a release a finalizer forgot.

**The pattern, from the split separator** (`app::chrome::splits::SeparatorDrag`)
and the sidebar divider before it (`app::sidebar::SidebarDrag`):

1. One value per gesture, typed. Build it whole on the press: what is being
   dragged, where the press landed, and whatever the gesture measures from
   (the separator's ratio at the press). Store it as
   `Option<ThatDrag>` on the `Editor`, not as loose fields.
2. Each captured move reads it. The grip reports only moves that came to it
   by capture (`Event::captured`, in `view::shell::grip::draggable`), so a
   hover is never mistaken for a drag, whatever is stored.
3. The release takes it (`Option::take`, or `= None`).
4. Nothing sweeps it. A capture that ends without a release, because the
   node unmounted, leaves a value no move can read, and the next press
   replaces it.

**What is left to follow it:** the file explorer's border (the last user of
`drag_start_position`), both scrollbars (`PaneScrollbarDrag` still gates on
`dragging_*scrollbar` because the vertical bar's uncaptured move is its hover
highlight; split the two by `captured` the way the grip does), and the pane
content's text-selection and terminal-grid drags. When the last one moves,
`clear_active_window_drag_state` and the `Up` arm's sweep go with them.

---

## What the old plan got wrong

Recorded so the corrections are not re-derived:

- **The markdown document view was listed as needing nothing and unlocking
  nothing.** It is the only open item that unblocks others.
- **Flex yield order was listed as owed.** The library half landed with one
  consumer, the prompt's suggestion columns; the status bar — the surface that
  motivated the primitive — switched later. Both use it now.
- **"Delete `app/chrome/`" was misstated.** What is there now is message
  handlers, not a duplicate chrome system. The item is *move these beside their
  surfaces*, which is placement, not deletion of a second authority.
- **`layout_box.rs` was listed as deleted before it was.** It went with the
  projection's geometry, after the markdown document view stopped needing an
  arena — see *Delete the widget text projection* — not before.
- **The deletion ledger conflated two kinds of survivor.** `HostRegion` and
  `HostTarget` survive as a *key namespace* for readers that ask where a region
  is — no region is a `Host` any more. `popup_areas` was called a cache of tree
  geometry, which was half right and the wrong half: it took the outer rect off
  the tree and then re-derived the inner one by hand, so it *was* a second
  statement of geometry, and it is deleted rather than retired. See *The keyed
  geometry index*.
- **"The painter reads the tree" was asserted where it was not true.** Three
  surfaces kept the old mechanism as the answer the painter used and demoted
  the tree's to a `debug_assert` beside it, so a release build ran on the
  mechanism the migration had supposedly replaced — see *Where the assertion
  was the only reader*. A `debug_assert_eq!` between a new answer and an old
  one is a migration step, not a finished one; the finished form has one
  answer and nothing to compare it to.

---

## Decisions that stand

Not re-argued:

- The editor owns the text pane's scroll; edits repair, they do not invalidate;
  only the visible window is materialised.
- `Host` is a design choice, not a migration seam. A designed host takes its
  rectangle from layout and its position from paint order, and records nothing.
  The pane's text, the terminal grid and window embeds stay hosts.
- `Modality::Focus` and `Modality::Pointer` are permanent: each is one channel's
  claim for a surface whose other channel is elsewhere.
- The kinds' key handlers are host-side; kinds are not plugin-extensible.
- Precedence is layer declaration order. Paint order and keyboard order are
  independent by design; a layer names its focus scope when the two differ.
- The layer hit-test rule: the first layer with any path at the point wins — so
  a layer says what a press anywhere on it means, or is not a layer. A
  decoration is as big as what it decorates.
- Selection is the host's; the tree says where selecting is meaningful.
- `Persisted` is for new incidental view state; `workspace.rs` is the editor's.
- One tree, N windows, no window named in it.
- Geometry is produced by layout or recorded by ruling, never by accident.
- Composition is the only extension mechanism; the cost is verbosity.
- Theme provenance is total: a `Draw` carries a theme key, the fold resolves it,
  and the inspector answers over every surface — including a pane's cells, which
  the pipeline writes through the same sink.

---

## Working rules

- **A library change needs a caller in the same PR and a test that fails without
  it.** "Would another consumer want this?" is not a reason; it is what admitted
  six unused variants.
- **A surface is done when the tree measures it.** Cell-identical output and
  pointer parity are necessary and not sufficient; a description with a rect, a
  width or a pre-fitted string is still a picture. The status bar and the tab
  strip were the two live examples and both are closed; *Composite buffer panes
  never migrated* is the surface left, and it never had a description at all.
- **Assert the tree's focus, not the registry's.** Every focus failure this arc
  had came with a registry that agreed with itself.
- **Send two events before rendering** when the property is about ordering.
- **Never test a windowed list with one-cell items only.**
- **Delete the comment with the code.** Reviews repeatedly found load-bearing
  claims the code contradicted. Three of them survived into this arc and were
  each refuted by a grep: a roster entry for a field that no longer existed, a
  helper "shared by the renderer and the click hit-tester" when neither did,
  and a recorder naming a consumer (`cursor_obscured_by_overlay`) that is
  nowhere in the workspace.
- **`pub` hides dead code from the compiler, so sweep by name.** A migration
  leaves its residue exported: the old half of a pair stays `pub` long after
  the new half stopped calling it, and rustc says nothing. Count each `pub fn`
  name across the workspace — editor, library, tests, examples — and one
  occurrence means the definition. Run it to a fixed point, because private
  dead code *is* reported and every round exposes more; stop when the
  dead-code warning set stops changing. See *The sweep for code nothing
  calls*.
- **Check `--no-default-features --features runtime --all-targets` and
  `--all-features`** before every push.

---

## Residue that belongs to other work

Named here so it is not rediscovered as a gap in this arc:

- **Sidebar feature asks** (from `sidebar-sections-design.md`, *What it needs
  from the host*): reveal on `SetSelectedIndex`, so a selection the plugin sets
  scrolls into the tree's window the way a keyboard move does; sticky ancestors
  in `Tree`, which is now the widget computing the indices the explorer
  computes in `FileTreeView::sticky_display_indices` and handing them to its
  `List` as `pinned` — the window's half (the room, the ceiling, the bar) is
  the library's already; and tabs within a sidebar section.
- `LspFeature::DocumentSymbols` exposed to plugins — the code-outline half of
  #1791, which turns the Markdown contents section into an outline section with
  a different scan.
- Plugin sections on the web (returns with *The web*).
- The scrollbar-markers plugin API, which `Draw::Scrollbar`'s marks unblocked.
- The LSP hover tooltip that cannot be dismissed through the gutter
  (pre-existing, identical on master).
- **`WidgetRegistry::{has_focus_follower, focus_follower_of}`**, and the
  `focus_follows_cursor` field behind them. The view-layer sweep found them
  unreferenced, and left them: they are the gate on a plugin-facing
  `focusFollowsCursor` feature that was written and not wired up, not something
  the migration abandoned. Deleting a half-built feature is a product call.
