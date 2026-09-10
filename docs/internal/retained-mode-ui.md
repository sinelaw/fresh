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
   this key just moved.
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
  its interior. A handler returns `Option<M>`; the control object carries the
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
| Status bar | `view::shell::status_bar` — but see *The status bar does layout by hand* |
| Prompt row, suggestions, overlay card | `view::shell::prompt`, `prompt_line`, `overlay_prompt` |
| Sidebar / file explorer sections | `view::shell::sidebar`, `file_explorer` |
| Dock, floating and pane-mounted panels | `view::shell::panel` — one `Interior` for all placements |
| Split grid: panes, dividers, tab strips, scrollbars | `view::shell::splits` |
| Pane content, terminal grid, window embeds | `HostSpec::Leaf` + the text pipeline |
| Settings, keybinding editor, calibration, trust | `view::shell::settings`, `keybinding`, `modal` |
| Popups, context menus, theme inspector | `view::shell::popup`, `context_menu`, `theme_info` |
| The web's projections | `view::scene` — reads rectangles off the tree by key |

`app/chrome/` is no longer a chrome system: what is left there is the handlers
those nodes dispatch *to*, plus two hover reactions.

---

## What is open

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

**What it leaves for the deletion that follows.** With both bail-outs gone,
the text projection has no writer: `WidgetPanelState::{painted, boxes}`,
`render_collected`, `layout_box.rs` and `render_button` have no caller, and
`Text::on_wheel`'s document branch reads an arena nothing fills. See *Delete
the widget text projection* below.

### Delete the widget text projection

**Narrowed, by decision.** The projection has two kinds of output, and only
one of them is dead.

Its **text** — `RenderOutput::entries` — is load-bearing for a pane-mounted
panel: the panel's *buffer* is those rows (`set_virtual_buffer_content`),
code-tour reads that buffer back (`getBufferText`, in `lineRangeBytes`) to
place its step highlight, and the page's reading row and the status bar's
`Ln`/`Col` ride on it — "the mirror follows rather than leads". So
`render_collected` stays as the mirror's producer, and `render_button` stays
as the mirror's text formatter (it already reads `Frame::BUTTON`; the
duplication the stylesheet work set out to end is gone). It runs on mount and
update, never inside a description build: **no description build runs a
renderer** holds today. Deriving the mirror from the tree's own rows is its
own project — the same capability the gutter-as-runs and the web's remaining
projections wait on — and a prerequisite for deleting the collector outright.

Its **geometry** — the box arena and the hit ranges — has no reader left, and
that deletion is open. An attempt to do it by scripted, compiler-driven
statement removal corrupted `containers.rs` and `render.rs` twice and was
reverted; it needs to be done per file with the text in view. The map:

- `CollectedOutput::{shift_channels, absorb_child, push_self_box}` in
  `render.rs`: the `boxes`/`hits`/`painted` legs of the first two, all of the
  third; the `focus_ring(&collected.boxes)` → `tabbable` derivation in
  `render_spec`'s assembly; the `PaintedWindow` folds around lines 494–575.
- `RenderOutput::{hits, tabbable, painted, boxes}` and
  `CollectedOutput::{hits, boxes, painted, self_scroll}`.
- `containers.rs`: the three assemblers' `hits: &mut Vec<HitArea>` /
  `out_boxes: &mut Vec<LayoutBox>` parameters (`assemble_inline_row`,
  `assemble_wrapped_row`, `zip_row_blocks`) and every argument at their
  call sites; `RowPiece::hits`; the `hits`/`boxes`/`painted` locals in
  `collect_row`, `collect_col`, `collect_section` and their struct fields;
  the `painted` assertions in its tests.
- Per kind: `out.hits.push(HitArea { .. })`, `out.push_self_box(..)`,
  `out.self_scroll = Some(BoxScroll { .. })`, `out.painted.insert(..)` in
  `list.rs`, `tree.rs`, `text.rs`, `button.rs`, `toggle.rs`, `number.rs`,
  `dual_list.rs`, `dropdown.rs`, `popup.rs`, `hint_bar.rs`, `raw.rs`,
  `spacer.rs`, `divider.rs`, `window_embed.rs`, `component.rs`.
- `registry.rs`: `HitArea`, `PaintedWindow`, `WidgetPanelState::{painted,
  boxes}`, `painted_viewport`, the `&mut PaintedWindow` accessor, and the
  `painted`/`boxes` parameters of `mount` and `update_side_effects`;
  `widgets/mod.rs`'s re-exports; `layout_box.rs` whole.
- Host: the five mount/update sites in `plugin_dispatch.rs` and the
  re-render tail in `widget_runtime.rs` pass `out.painted`/`out.boxes` —
  drop the arguments; `widget_viewport`'s `painted_viewport` fallback (the
  spec's `Viewport::from_spec` is what remains); `painted_panel_height`
  and `widget_panels_with_stale_height`, which must read the pane's height
  from the tree instead; `Text::on_wheel`'s document branch, which reads an
  arena nothing fills.
- Then `FloatingWidgetState::entries` (read only by `panel_description`'s
  `Host` fallback), `Spot::{content_rows, content_cols}` and the painted
  branches of `Panel::{height, anchored_width}` — every described box says
  `Auto` — and the `painter::{centered, anchored}` parity tests that pin the
  painted arithmetic. And `app/chrome/`'s two hover reactions move beside
  their surfaces, and the module goes.

### The status bar does layout by hand

`Editor::status_bar_description` measures element widths, drops right-hand
elements until the rest fits, computes a left budget through
`view::shell::status_bar::left_budget`, and truncates strings — **all before the
description is built**. That is the migration's own failure criterion: a
description with a pre-fitted string is still a picture the old renderer drew.

The primitive that replaces it already ships. `Node::priority` ("higher yields
last") landed and the prompt's suggestion columns use it. The status bar — the
surface that motivated the primitive — never switched. This is the smallest
open item and it closes a stated done-criterion.

### The keyed geometry index

`Ui::find_by_key` is a depth-first walk of the element tree, and
`view::shell::rect_of` wraps it. There are ~180 call sites in the editor, some
inside per-item loops (the web's dropdown projection does one full walk per
row). The design has always called for the library to publish `Key → Rect` as an
O(1) read after layout; nothing has been built. This is the live asymptotic hole,
and it also retires the last recorded-rectangle caches (`popup_areas` and
friends, which are now caches *of* tree geometry rather than a second layout).

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

**And one item that belongs to the text-projection chain, not the stylesheet.**
`render_button` cannot be deleted while `render_floating_spec` /
`render_panel_spec` still run the whole text projection on every mount and
repaint: that path's entire output is a string, and a naked label plus a class
says nothing to it. What keeps that path alive is the anchored-panel bail-out —
see *The markdown document view*, "What it does not clear". The duplication it existed to end is already gone —
`widgets::frame::Frame::BUTTON` holds the glyphs and the padding, and both the
runtime's text and the shell's reserved columns read it, with a test asserting
they agree on width. `render_button` goes when the text pipeline goes.

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

- The pointer's legacy walk (see *The one asymmetry*), whose members are now
  the terminal's own mouse and the multi-click detector; the markdown drag was
  its last grab and is the run's own capture.
- `Paint::Lit` — the theme-provenance escape hatch, still live in the fold. An
  item whose ink resolved to literal colours files nothing, so it is a blank
  *tier* in the audit rather than a blank surface.
- No palette-resolve cache.
- `EntryDialogState` still carries the settings entry dialog's own state model.

---

## What the old plan got wrong

Recorded so the corrections are not re-derived:

- **The markdown document view was listed as needing nothing and unlocking
  nothing.** It is the only open item that unblocks others.
- **Flex yield order was listed as owed.** The library half landed and has a
  consumer; only the status bar never switched.
- **"Delete `app/chrome/`" was misstated.** What is there now is message
  handlers, not a duplicate chrome system. The item is *move these beside their
  surfaces*, which is placement, not deletion of a second authority.
- **`layout_box.rs` was listed as deleted. It is not** — 314 lines, still the
  home of `LayoutBox`, `BoxScroll`, `focus_ring` and `hit_path`, and still
  reached by `render_collected` and three kinds. It goes with the text
  projection, not before it.
- **The deletion ledger conflated two kinds of survivor.** `HostRegion` and
  `HostTarget` survive as a *key namespace* for readers that ask where a region
  is — no region is a `Host` any more. `popup_areas` survives as a cache of tree
  geometry, retired by *The keyed geometry index*. Neither is the second system
  the ledger implied.

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
  width or a pre-fitted string is still a picture — *The status bar does layout
  by hand* is the live example.
- **Assert the tree's focus, not the registry's.** Every focus failure this arc
  had came with a registry that agreed with itself.
- **Send two events before rendering** when the property is about ordering.
- **Never test a windowed list with one-cell items only.**
- **Delete the comment with the code.** Reviews repeatedly found load-bearing
  claims the code contradicted.
- **Check `--no-default-features --features runtime --all-targets` and
  `--all-features`** before every push.

---

## Residue that belongs to other work

Named here so it is not rediscovered as a gap in this arc:

- **Sidebar feature asks** (from `sidebar-sections-design.md`, *What it needs
  from the host*): reveal on `SetSelectedIndex`, so a selection the plugin sets
  scrolls into the tree's window the way a keyboard move does; sticky ancestors
  in `Tree`, which is the explorer's `viewport_display_indices` logic moved one
  level down into the widget; and tabs within a sidebar section.
- `LspFeature::DocumentSymbols` exposed to plugins — the code-outline half of
  #1791, which turns the Markdown contents section into an outline section with
  a different scan.
- Plugin sections on the web (returns with *The web*).
- The scrollbar-markers plugin API, which `Draw::Scrollbar`'s marks unblocked.
- The LSP hover tooltip that cannot be dismissed through the gutter
  (pre-existing, identical on master).
