# Finishing the `fresh-ui` migration: from a retained *layout engine* to retained-mode UI

**Status:** active plan. Supersedes the plan half of
[`fresh-editor-ui-migration.md`](./fresh-editor-ui-migration.md), which is kept
for its survey of the pre-migration codebase and for the record of what the first
wave decided and why.

**Audience:** whoever picks this up next. It assumes you have not read the old
plan and does not require you to.

---

## 0. The one-paragraph version

The first wave moved the editor's chrome off hand-written painters and onto a
`fresh-ui` description tree. It succeeded at the *structural* half: there is one
tree, one layout pass, one hit-test, and several surfaces that used to compute
their geometry twice now compute it once. It did **not** adopt the *retained*
half. Nothing is memoised, almost nothing is a `Component`, the focus ring is
switched off wherever a library widget appears, and — the load-bearing problem —
the plugin-panel adapter builds its descriptions **by calling the immediate-mode
painter it was meant to replace** and wrapping the strings that come back. The
result is a retained layout-and-hit-test engine driven by a full immediate-mode
rebuild every frame. That is a real improvement on what came before, and it is
not the goal. This plan is how to close the gap.

---

## 1. Where the integration actually stands

### 1.1 The retained half is unadopted

- **Nothing is memoised.** The library's only skip is identity comparison on a
  shared subtree, which requires the caller to hold an `Rc<Node>` across frames.
  The editor never does, so the short-circuit never fires and the entire shell is
  re-reconciled on every frame — including every terminal tick.
- **Almost nothing is a `Component`.** Two exist in the whole adapter; everything
  else is a free function from props to a node. That is a legitimate style on its
  own, but combined with no memoisation it means the tree owns very little state
  worth preserving — so "reconciliation preserves state" has almost nothing to
  preserve.
- **Focus is not the tree's.** The focus ring is explicitly disabled on
  essentially every library list in the shell. Keys are routed to a surface by
  layer modality and then handed straight back to that surface's legacy
  dispatcher. The tree answers *which surface*; the old handler answers *what the
  key means*.
- **Nothing verifies that a frame was requested.** The library can report that
  its tree is stale; the editor consults that in exactly one place, and no test
  covers it.

### 1.2 The plugin-panel adapter depends on the renderer it replaces

This is the central problem, and it is worse than "two renderers for one format".
They are not independent: the adapter calls `widgets::render`'s collector
*inside* description-building for `Text`, `List`, `Tree`, `Dropdown` and
`DualList` — the five most complex variants, and the ones behind every settings
control and every plugin form field. For those, wrapping, padding, alignment and
formatting remain the immediate-mode runtime's; the tree positions the resulting
strings and hit-tests byte ranges it was handed.

The coupling runs both ways through mutable shared state. The runtime writes
instance state, hit areas, boxes and focus into its registry; the description
reads that state back out and is handed it as context. A described panel's outer
box is sized by the *runtime's* row count while its interior is sized by the
tree, and nothing checks that the two agree.

A transitional duplicate is two implementations of which you can delete one. Here
the tree cannot function without the runtime running first, and which of the two
is authoritative was decided by a `covered()` gate evaluated separately on
several code paths. *(`covered()` is gone as of Phase 2.4 — the gate had no
`false` arm left once `WindowEmbed` became a `Host` leaf. The class of defect
below is what it cost while it existed, and the plan's later references to it
are corrected in place.)* This has already produced shipped bugs of one shape: the runtime's
scroll offset moved, the description did not read it, and the hit-test probe did
— putting hover and the context menu on a different row from the one drawn. Each
was fixed by making a *reader* stand down, not by unifying the state.

### 1.3 The `Host` seam, and the two-band fold

The seam itself is sound: a `Host` leaf hands a legacy painter the rectangle the
tree laid out, so a surface can migrate without its neighbours. The buffer
viewport and terminal grid should stay a `Host` permanently — a retained tree has
no business owning a text renderer, and every comparable framework keeps the same
escape hatch.

What hollows it out is the **two-band fold**. Because there is one display list
and many painters that are not in it, the fold is cut in half and the legacy
painters run in between. The tree's z-order is therefore not authoritative, and
callers must know which painters run in which band. Get it wrong and the failure
is silent, and nothing enforces it. It has already forced a real compromise: a
dimming scrim that cannot be declared in the tree, because the surface it should
dim is painted after the tree's overlay band.

### 1.4 What is genuinely done, and should not be re-litigated

- **The settings dialog.** Its rectangle registry and hit-test are gone; every
  surface answers its own press, sharing one hit vocabulary with the web
  frontend. The strongest work in the wave.
- **The per-pane `Host` leaf**, which collapsed "the rectangle a pane is painted
  at" and "the rectangle it is clicked at" into one rectangle.
- **The modal mouse-capture band** and the surface-precedence predicate beside
  it, deleted in favour of exclusive-modality layers.
- **The parallel hover walk**, deleted.
- **`fresh-ui` itself**: one dependency, its own demo and goldens, a flat keyed
  display list with a clean host-interleaving seam, and a scheduler whose dirty
  marks do not propagate. A second host could use it.

### 1.5 Known defects, carried

| id | what |
|---|---|
| **F.6** | Ctrl+Right-click theme inspection is blank over every *described* surface — menu bar, status bar, explorer, settings, popups, dock. Every writer of the per-cell theme map is a painter, and a described surface has no painter. |
| **F.8** | The dock's active card keeps its heavy selection marker and never becomes the seamless tab: the pass that rewrites it sits behind the described-panel early-out. |
| **F.2** | `Paint::Lit` — the one colour in the display list with no theme name. |
| **F.7** | Pre-existing, identical on `master`: an LSP tooltip left through the gutter can never be dismissed by moving the mouse, because the dismissal is gated on request state the gutter branch clears. |
| **F.9** | A press on a described *pane*-mounted panel's dead space still reaches the editor-click path, which scans the runtime's stored hit list and resolves it against a screen-to-line projection that is empty for that pane. Every other probe was gated on "is this panel described"; this one was not. Presses that land on a widget are stopped by the node, so what gets here is exactly the case the projection cannot answer. The projection now says `None`, which all three of its readers already expect. |

F.6, F.7, F.8 and F.9 are fixed on this branch; F.2 is open. The table is kept
whole because each row names a *class* of defect the migration produces, and the
fix is only interesting next to the shape that caused it. F.9's class is the one
worth restating: a gate applied at four of five sites is not a gate, and the site
that was missed was the one whose fallback was silently degenerate rather than
merely stale.

---

## 2. The principles this plan is judged against

Each is the general form of a bug that has already shipped here.

1. **A description states; it does not re-derive.** If layout knows a rectangle,
   nothing may recompute it by arithmetic or by scanning rendered output. An
   attempt to locate the dock's active card by scanning painted cells for box
   glyphs was written and reverted during this work — it is the same duplication
   this migration exists to remove, one layer further downstream.
2. **State the rule the painter enforced, or lose it.** Every regression in this
   migration has had this shape: border corner styles, hover repaint requests,
   row-fill semantics, caret placement on click, a key with no variant in the
   library's vocabulary. The question when migrating a surface is never "does it
   look the same" but "what did the old code *guarantee* that the description
   does not say".
3. **Identity is declared, not positional.** A stateful node without a key is a
   state-corruption bug waiting for a sibling to be inserted above it.
4. **One authority per fact** — not two writers and a gate deciding which counts.
5. **A comment that argues for a property the code lacks is a defect.** In a
   codebase where the comments are the design document, this matters as much as a
   wrong branch.

---

## 3. The plan

Ordered. Each phase states its exit condition — something *observable*, not
merely "done".

### Phase 1 — Stop the bleeding (the merge gate)

Small, mechanical, each closing a live defect class.

**1.1 Key the stateful widgets in the adapter.** The list, tree and text nodes
the adapter builds carry no key, so their identity is positional among their
siblings. Two live consequences: a plugin re-emitting its spec with one extra
sibling above a list *remounts* it, silently resetting scroll and hover; and
because component elements are matched by type, two different lists at the same
position update in place and one inherits the other's scroll offset. The spec's
own key is already in hand at each site.
*Exit:* a spec rebuilt with a sibling inserted above a scrolled list keeps its
offset, and two sibling lists that swap order do not swap offsets.

**1.2 Assert that a frame is *requested*.** Every e2e harness method renders
unconditionally, so a missing repaint request is invisible to the entire suite by
construction — which is how three bugs of that shape shipped and were fixed
blind. Expose the dispatch's "something changed" answer to the harness and assert
it for a hover only the tree observes.
*Exit:* the test fails if the staleness check is removed.

**1.3 Fix F.6.** The fold already resolves each display-list item's style and
holds its rect, clip, theme and key; one recorder there replaces the per-surface
provenance walks. The open question is the region name — a surface label the fold
does not know. Derive it from the item's key, carry it on a band or host tag, or
drop it and let the foreground/background keys carry the information, which is
what the theme editor actually consumes.
*Exit:* Ctrl+Right-click over the status bar, a menu row, the explorer and a dock
session row each report a real key.

**1.4 Correct or delete the comments that argue for properties the code lacks.**
Known offenders: the modal module's claim that settings and the keybinding editor
hit-test rectangles their painters recorded (they no longer do), and its
reference to "each of the remaining three" of a two-variant enum; the render
module's claim that chrome provenance is recorded during each region's own paint
(false for most of the surfaces it names — that *is* F.6); the explorer caret
described as a "layout query" when it is a region origin plus hard-coded border
offsets, in a codebase that has the right mechanism and uses it correctly
elsewhere; the frame module's argument that shared-subtree memoisation is doing
work (it is unused); the shell module's "proven to reproduce the ratatui
rectangles exactly", where the test itself says it is a pin rather than an
oracle; the adapter's claim that the `covered` gate "is what remains of a
boundary that has closed", when it means only "the adapter has an arm"; and a
splits helper whose first statement discards the parameter its doc comment is
about.

**1.5 Land F.8.** The seamless tab is two per-row facts: the active card's rows
drawn rounded with no right border, and the dock's divider interrupted across
that band with a scoop above and below. What is missing is the row band shared
between the adapter (which draws the rows) and the divider (which draws one glyph
per row). Derive it from the spec, or anchor to the selected card's node key —
each card block is already keyed. Do not read the previous frame's layout: that
reintroduces the one-frame lag already fixed once.

### Phase 2 — Break the `WidgetSpec` cycle

This is the phase that turns a veneer into retained-mode UI. Nothing after it is
safe until it lands. *(This paragraph used to end "and until it does, stop
widening `covered()`" — that instruction is spent: 2.4 deleted the gate, so
there is nothing left to widen.)* The
`WidgetSpec` API itself does not change — existing plugins must keep working
untouched. That is a hard constraint, and it is satisfiable: the format is the
contract, the renderer is not.

**2.1 Element state.** Scroll offsets, cursor positions, selection and
dropdown-open flags move out of the runtime's registry and into element state,
read and written by the same party. Today one side writes and the other reads a
frame later, and they have already disagreed.

**The original exit condition was wrong, and working the phase is what showed
it.** "The description no longer receives runtime state" conflates two things
that live in the same map. A field's value, a dropdown's selection, a dual
list's included set are *model* state — what the plugin is told about through
`text_change` and `dropdown_select`, and what it owns. Those legitimately reach
the description as context and stay. What does not belong there is *view* state:
where a window sits, whether a pop-over is up.

Within the view half, a second distinction decides the fix, and it is the one
that matters:

* A **derivation** is a pure function of what is in hand — a clamped index, a
  sanitized set, a focus-gated flag. Recomputed identically on every read, so
  writing it back stores nothing and merely makes the walk an authority. The fix
  is a *deletion*.
* A **fold** takes its own previous value as an input — "move the window just
  far enough to keep the caret in view" gives three different answers for the
  same caret depending on where the window already was. It cannot be recomputed,
  so the fix is a *move* into the element.

*Exit:* no fold is written by one party and read by another, and the hit-test
probe has no described-versus-painted branch.

*Done.* The render walk no longer decides anything for `Dropdown`, `Number`
or `DualList` — all three were writing derivations back — and carries their
stored entry through only so the whole-map replace does not collect it. One rule
had to move rather than vanish: a blur closes a dropdown, which the walk used to
enforce by storing its result, and which is now applied at every read.

`List` and `Tree` turned out to need nothing: their described arms read no
runtime state at all, because the element already owns the window. The registry's
copy is vestigial for a described panel and retires with the painter, in 2.4.

`Text` was the remainder and the only real one. Five of its seven fields were
merely carried and now are. Of the two folds left, the caret window has moved
into element state — a component whose state is a `Cell`, seeded once from the
registry and never read from there again — which settles the disagreement where
the walk decided the window at the width the *registry* recorded while the
description drew it at the width layout gave.

**No library change was needed, and the belief that one was is worth recording.**
`Component::build` takes `&Self::State` and so cannot mutate it, which reads like
a prohibition on owning a fold. It is not: a component's state is its own
scratch, nothing in reconciliation reads it, and memoisation compares props — so
interior mutability is the entire mechanism. The same argument the library
already makes for a caret ("it exists only while this field is on screen")
applies to a window.

The completion list's forward-only offset is the same shape and has not moved: it
is carried on the runtime's `SetCompletions` state, which is the plugin's.

**2.2 Describe the five collected variants for real.** Text, list, tree, dropdown
and dual-list stop going through the runtime's collector. Each becomes a
description built from the *spec*: wrapping via the library's text render, hit
areas from node rectangles rather than byte ranges, padding and alignment as
layout. One at a time; take dropdown first, as the smallest and the one whose
pop-over is already described.

The shape each one takes is the dropdown's: the pure halves of the collector —
where the state comes from, how the rows are windowed, what column a float drops
under — are factored into functions the collector and the description both call,
so a rule has one copy rather than two that can drift. The *formatter* does not
move: what a widget's row says is domain knowledge, and rewriting it would be
rewriting thousands of lines to get the same cells. What moves is where the row
is, what a press on it means, and the fact that building the description no
longer runs a whole immediate-mode render into a scratch state map it discards.

Three of the five are already partly across, so the remaining work is smaller
than "five variants" suggests: list and tree have real described arms and reach
the collector only for a card list's item subtrees and a residual tree shape; a
multi-line text field likewise. What is wholly uncrossed is the single-line text
field — every form field in the editor — and the dual list.
*Exit:* the runtime's collector has no caller in the shell.

*Progress:* the dropdown has crossed, and is the worked example for the rest.
Remaining: the single-line text field, the dual list, a residual tree shape, and
three calls the already-described arms still make into the collector for a card
list's item subtrees, a multi-line field's document, and a card tree.

**2.3 Retire the mirror as a rendering input.** *Done, bar one measurement.* The
text-property mirror is now only a *text* mirror — for search, copy and change
counting.

An audit of every reader found the paint role had already stood down for
pane-mounted panels (the text pass and the scrollbar pass both return early for a
described pane) and never existed for the dock, whose box is the carved column.
What was left was the floating panel's frame, sized by arithmetic over the
mirror's row count and widest row. A described box now measures its own interior;
the counts stay for a *painted* one and go with it, because a `Host` leaf has no
intrinsic size and the mirror is the only thing that can answer for it.

**The exception, which is a real one.** An anchored popup's *width* is still the
mirror's count even when described. The box hugs its content horizontally, so the
tree wants to say "as wide as you need" — but the interior is built by a layout
reader, which needs a width as a *number* before it can produce a row, and under
an indefinite constraint that number is the whole screen: a divider would come
out screen-wide and set the very width it was asked about. The height has no such
loop. What removes it is the interior stating its own natural width, which is the
same step that lets the layout reader go — and that is 2.2's job, not this one's.
*Exit:* deleting the mirror changes no pixel. Met for height and for every other
slot; the anchored width is the one remaining read, and it is named in the code.

**2.4 Then delete the second renderer.** *Done.* The widget runtime's paint role
is gone; it keeps only what the plugin API genuinely needs.

The unlock was not a deletion but the removal of an *exclusion*.
`WindowEmbed` — a real editor window inside a panel — was held out of the
coverage gate on the reading that painting its own cells and being described are
alternatives. They are not: painting its own cells is what a host leaf *is*, and
the tree has handed rectangles to host leaves all along. Once the embed became
one, the gate had no `false` left to return, every branch on it was dead, and
the second renderer went with them: the generic collector adapter, the interior
painter, two probes that had become no-ops on every path, and three helpers each
of which had already been replaced by a description — including the one that
located the dock's active card by scanning painted cells for box glyphs.

Two things were deliberately kept here and are worth knowing, **and the reason
given for the first was wrong — it is deleted now.** `probe_floating_widget`
was kept on the reading that a dock open with *no* mounted panel has no
interior, so its column still emits a press that reaches it. The press does
reach `handle_floating_widget_click`; the probe cannot answer it. With no
panel in the slot there is no `last_inner_rect` and no panel key, and the
handler returns above the probe — and with a panel in the slot the interior
*is* described, so the column emits `DockFocus` and the press never comes.
Tracing every emitter of both facts found no path that reaches the probe with
a rectangle to test against, so it went, and the stored overlay rows and the
box mirror went with it, together with the pop-over rectangles and the
scrollbar tracks the deleted painter was the only writer of.

The same audit named the fields of the runtime's per-panel output whose every
consumer is a path a described panel never takes: the window-embed rectangles
(dead by construction — a panel containing one is not described), the stored
overlay rows (the description re-derives its own), and the stored focus cursor
for the floating and dock slots (the described caret comes from the tree's own
cell). The pop-over survives as a single field — the open dropdown's key — and
not as a structure.

Everything else still has a live reader on a described panel, and each retires
with a different step rather than with this one: the mirror's rows with 2.3
above; instance state, the focus key, the row budget and the box arena with 2.1
and 3.2, since a described panel's Tab still builds its ring from the arena and
its wheel still routes through it.

The hit list is the one that needs a decision rather than a step. The
described-versus-painted branch closed the floating and dock probe completely,
but two readers were never gated: a press on a described *pane*'s dead space
still scans the stored hits through the editor-click path, against a byte
projection that is empty for that pane; and the web frontend projects the hit
list verbatim, because for it the runtime *is* the renderer and the coverage gate
is a terminal-only question. The first is a defect to close. The second is a
question this plan should answer before 2.4: is the hit list a rendering output,
or part of what the plugin API owes a non-terminal frontend?

### Phase 3 — Adopt the retained half

**3.1 Memoisation, which needs a library change first.** The only available skip
requires hoisting a shared node across frames, which is structurally impossible
for a host that derives its props from a store every frame — which is exactly
what the editor does. The library and its only consumer have incompatible cost
models, and the library is the one that should bend: add a props-equality memo or
a repaint-boundary equivalent, then apply it to the surfaces that rebuild
unchanged on most frames.
*Exit:* a frame with no state change reconciles a bounded number of elements,
asserted.

**3.2 Focus becomes the tree's.** Re-enable the focus ring one surface at a time,
moving each surface's key interpretation from its legacy dispatcher onto its
nodes. The per-surface "here is your key back" facts shrink and then go.
*Exit:* tab order in the settings dialog is the tree's, and the base key
dispatcher is not reached while a modal is up.

*Progress:* the settings category tree interprets its own keys — eight of them
arrive as what they *mean* rather than as a raw key handed back — and the plugin
widgets are on the tree's ring, which leaves the registry's focused-key string a
mirror written from one place rather than the authority on which control is live.

**Its exit condition had two blockers and one is now gone.** The first was that
the dialog's middle tab stop — the settings *body* — had nothing focus could
land on: its controls are plugin widgets, and no plugin widget was on the ring
at all. They are now, and the body renders through the same adapter, so the ring
has real targets there.

**But "so `move_focus` has real targets there" was itself an overclaim**, caught
by the review in §6, and in the same shape as the Tab blocker below. Being on the
ring is necessary and is not sufficient: (i) the `WidgetFocus` applier returns
early for every slot but Dock and Floating, so a `FocusGained` from a settings
widget is dropped; and (ii) the dialog attaches an `on_key` that stops every key,
and the library runs the focused chain *before* intent resolution, so Tab becomes
`UiFact::ModalKey(Settings)` and `move_focus` is never reached.

**And following that thread to the dock says the ring is inert there too, for a
different reason and a worse one.** `panel::keys_layer` is a `Modality::Focus`
layer holding a single `autofocus()`ed focusable whose `on_key` emits
`UiFact::PanelKey(slot)` — and it is declared for exactly the case that matters,
a panel with the keyboard. `Modality::Focus` makes it the topmost modal, and
`focus_scope` retains only focusables *within* the topmost modal. The panel's
widgets are in the dock column, not in that layer. So:

- the panel's focus scope contains one node, the key sink, and traversal there
  cannot reach a widget at all; and
- `apply_autofocus` pulls focus back into the active scope on the next frame, so
  a widget focused by a click is un-focused by the frame that follows it.

`UiFact::WidgetFocus` still fires, because the transient `FocusGained` happens
before the settle — which is why the registry key looked like a mirror. It is a
mirror of a focus the tree does not keep.

So 3.2's honest state is: **every interactive widget is focusable and none of
them is reachable.** The remaining work is not "re-enable the ring" but making
the panel's keyboard the widgets' own — the panel's subtree inside the keyboard
scope, with `PanelKey` as the fallback for keys no widget claims, rather than a
sink that owns the scope and holds focus. Until that lands, "delete the second
focus ring" is not available: the box-arena ring in `handle_widget_focus_advance`
is the only ring that works.

*That landed as S2, and what it makes available is narrower than "delete the
second ring".* A panel whose interior the tree describes — with something in it
to focus — now names that interior as its keyboard layer's scope, and Tab there
is the tree's. A panel without one keeps the sink and keeps the arena, so the
arena cannot be deleted: deleting it would take Tab away from exactly those
panels. What *is* available is that the two rings stop disagreeing.
`handle_widget_focus_advance` is the single seam every host-driven advance
arrives at — `WidgetAction::FocusAdvance`, `KeyFx::focus_advance`, the
smart-key `Tab` — and it now asks the tree whether the tree is holding this
panel's focus (`has_focus_within` on the interior's scope key, which is a fact
only the tree has) before falling back. Where the tree holds it, `move_focus`
is the move and the registry's key is written by the `WidgetFocus` mirror;
where it does not, the arena is untouched. The proxy shape to avoid is asking
the *runtime* whether the panel has focus targets: that is one fact with two
sources, and the runtime cannot know where the tree's focus actually is.

One thing found while doing it and not fixed: `Ui::pending_messages` — where
`apply_autofocus` leaves the focus change it settles — is never drained by the
editor. `Ui::dispatch` returns only what handlers produced during routing, and
nothing calls `take_messages`. So the settle's `FocusGained` never reaches the
host, and the backlog grows for the life of the process.

**The second blocker, as previously recorded here, was not real.** It was written
down twice — that Tab is overloaded inside the body, committing an edit and
staying put, so reaching the next control takes two presses, pinned by three
tests. Checking the source says otherwise. `handle_text_editing_input`'s Tab arm
already does `commit_text_edit(); stop_editing(); toggle_focus()` — commit *and*
advance, in one press, and its comment explains the commit was added because a
value typed and dismissed with Tab was being dropped on Save. Of the three tests
cited, two use Tab only to commit and assert the value was accepted, and the
third asserts merely that the screen changes, explicitly allowing either "move to
the next setting" or "move to the footer panel". None pins a two-press model.

What is actually left is smaller and is a design question rather than an
obstacle. `toggle_focus` advances to the next *panel* — Categories → Settings →
Footer — so Tab inside the body leaves the body rather than stepping to the next
control in it. That was the only thing it could mean while the body's controls
were not focusable. They are now, so Tab there could step control by control and
leave the panel only at the end, which is what the ring makes available and what
this kind of dialog does elsewhere. Whether it *should* is a taste call about the
dialog, not a migration blocker.

*What the plugin widgets need, now established.* The adapter contains exactly one
`focusable` call — the `Component` arm's scope — so **no plugin widget is on the
tree's ring at all**; the runtime's focus key, a string resolved across the whole
panel, is the sole authority, and the adapter's remaining `focusable(false)`
calls say so outright.

The library has the piece required: `on_focus_change` on a focusable node. So the
shape is available — declare each interactive widget focusable and keyed, and let
that handler write the runtime's focus key, making it a *mirror* of the tree
rather than a competitor. That is the same one-authority move Phase 2.1 made for
the folds.

The constraint to respect is that this cannot be done one *kind* at a time. A
ring containing only buttons would let Tab cycle buttons and skip fields, which
is worse than a ring the tree does not own at all. The unit is a panel: every
interactive kind in it goes on the ring together, or none does.

**3.3 Reconsider the focus and pointer modalities.** Their own documentation says
they are only meaningful for a surface whose interior lives outside the tree.
They are migration-shaped API in a general-purpose library; when 3.2 completes
they should be re-examined and probably removed.

**3.4 Collapse the two-band fold.** As phases 2 and 3 shrink the painter set, the
fold stops being cut in half and the tree's z-order becomes authoritative.
*Exit:* the fold runs once per frame.

### Phase 4 — The remainder

**4.1 Every region but the body stops being a `Host`.** The dock, menu bar,
explorer, status bar, search options and prompt line each become descriptions all
the way down. Of those, only the prompt line is still painted: the others are
described, and an empty dock or an itemless status bar is the only way their
`Host` is reached at all.

The body — the split grid, buffers, terminals and the text pipeline — stays a
`Host`, permanently and correctly. **But the tab strip is inside that boundary
and does not belong to it.** `view/ui/tabs.rs` is 1,369 non-test lines of
ordinary chrome — tabs, close buttons, a `+`, scroll arrows — and it is the last
painter-recorded-rectangle hit-test in the editor: `render.rs` files
`tab_layouts` and the event path in `app/chrome/splits.rs` and `app/tab_drag.rs`
reads them back. `view/shell/splits.rs` says so plainly ("The strip is the node;
its interior is still the painter's"). Declaring the body permanent is right for
the text; extending that to the strip fences the hardest remaining piece of
chrome inside a boundary labelled out of scope. **4.1 is not finished while the
strip is hit-tested against what it painted.**

**4.2 F.2.** `Paint::Lit` retires into a dynamic theme tier once plugins can
register named keys, after which provenance is total.

**4.3 Delete the dead painters, carefully.** *Done, and it was the smaller prize
the warning promised.* Reachability was established properly this time — a public
function with no external callers is not dead if its own module calls it, and one
only its tests call is coverage rather than weight — and what survived that check
was one scroll-panel painter with its three layout types, an orphaned constructor
set, two stray accessors, and a dead-in-place query API. The large chrome
painters the phase hoped to reclaim (the file browser, the file-open prompt, the
menu's layout pass, the tab renderer) are all still load-bearing through the
render module. One whole type is unreachable and was left standing because
removing it reaches outside this phase's files; it is named in the code.

**4.4 Macro replay stops using the retained tree as a calculator.** *Done.* It
took the tree, ran a full frame for geometry, and put it back — per replayed
action, so a frame's side effects were multiplied by the length of the macro. The
library now offers a geometry pass: build, reconcile, measure, arrange, and stop.
Autofocus, queued reveals, behaviour delivery and paint are the things a frame
*does* because it was shown, and a question is not one.

Caching the answer was considered and rejected: the key would have to be the
frame description, which is deliberately not comparable, and a hand-picked subset
of its fields is the same replica-of-a-layout that this call site already carried
once and got wrong three ways. A scratch tree was rejected for a sharper reason —
descriptions are not tree-agnostic here, because a description carries handles
that bind to whichever tree reconciles them, so laying one out twice takes the
binding away from the live tree. Two writers for one fact.

---

## 4. Test debt

The description-level unit tests are good: they build a frame, run it through the
real fold and palette into a real cell buffer, and assert on cells and dispatched
messages. The distinguishable-palette trick that makes cell assertions meaningful
is a genuinely good idea.

The integration oracles are thin. The frame-parity test compares against a frozen
copy of code that has since been deleted — its own header calls it a pin rather
than an oracle. The scene-parity test asserts that labels appear *somewhere* in
the rendered cells, which is a substring check rather than a layout or behaviour
oracle. The one test claiming the adapter agrees with the runtime covers a single
shape, and is near-tautological because the description is built from the
runtime's own rows.

Three additions, in priority order:

1. **Frame-requested assertions** (Phase 1.2) — the highest-value test on this
   list, because it closes a class the existing suite cannot see at all.
2. **A real parity oracle per collected variant**, added *as* Phase 2.2 migrates
   each one: spec in, cells out, compared against the runtime's answer across the
   shapes the runtime branches on rather than one shape.
3. **Reconciliation-identity tests** (Phase 1.1) — insert a sibling, reorder two
   lists, assert state follows the key.

---

## 5. How to work on this

- **Drive the UI by hand.** Most defects here were found by driving a real
  terminal, not by the suite — because the suite renders unconditionally and
  asserts on content. Use an isolated `HOME`/`XDG_*` and read the screen with
  escape sequences included: a background-only scrollbar is invisible to a plain
  capture, and has caused a false "it isn't drawn" conclusion more than once.
- **Compare against `master`, not against intent.** Twice a "regression" turned
  out to be `master`'s behaviour, and once a "pre-existing bug" turned out to be
  a regression. Build the base binary when it matters.
- **One push per CI run.** A push cancels the running workflow; batching is the
  difference between getting signal and spending forty minutes learning nothing.
- **Do not run the full suite locally.** Push and read CI.
- **Check a non-default feature configuration before pushing.** `cargo check
  -p fresh-editor --lib` and targeted test filters do not compile the
  `plugins`-gated arms out, so a call added to a `#[cfg(feature = "plugins")]`
  item compiles cleanly for the whole session and breaks
  `cargo check --no-default-features --features runtime --all-targets`. S6 did
  exactly that: `slot_of_panel` carried a gate inherited from its one previous
  caller, and the ring's new call site is ungated. The gate was incidental —
  `PanelKey` and both panel slots are ungated — so the fix was to drop it, not
  to gate the caller and let the ring take a different path in that build.

---

## 6. What an independent review found

A reviewer with no stake in this plan read the branch (354 commits, ~52k
insertions) and was told only the objective — that the plan itself might be
wrong. Its verdict is recorded here because it is sharper than this document was:

> substantially, genuinely retained for the editor's chrome, and not retained at
> all for the plugin-widget half

and it judged this document's remaining-work list to understate the gap "by a
wide margin". The specific corrections, all verified against the source:

- **Four comments argued for properties the code lacks** — the class §1.4 exists
  to close. One of them was written *while closing that class*, claiming the
  status bar is a `Host` because its prompt states paint outside the fold; the
  prompt row is a different region and is painted inside the fold. Another
  declared F.6 open twenty lines below the commit that fixed it. Three separate
  reviewers had been misled by these. Fixed.
- **The card bands sat in the region host-id space**, resolving to `Dock`,
  `MenuBar`, `Explorer`, `Body` and `StatusBar`. Fixed, with a tag and a test.
- **2.2's exit is not met, and the two survivors are the expensive ones**: a
  `render_collected` per card per frame, and one that formats a multi-line text
  field's *whole document* per frame — both from inside the retained description
  build. Two more immediate-mode renders sit on the click path purely to resolve
  a caret byte by measuring the text they just produced.
- **3.1 is barely started at the seam that matters.** Three memo sites, three
  `Component` impls. Worse, the three memos wrap the node build *after*
  `shell_frame` has already run the menu walk, the status-bar content pass and a
  deep clone of every panel's spec — the expensive half is outside the memo.
- **There are two focus rings over one `focus_key`.** The tree's writes it via
  `UiFact::WidgetFocus`; `handle_widget_focus_advance` writes it from the
  immediate-mode box arena. No gate between them.
- **3.2's note here was wrong in the same shape as the Tab claim.** The settings
  body's widgets are on the tree's ring, but the applier drops every slot but
  Dock and Floating, and the dialog's `on_key` stops every key before intent
  resolution — so the tree's ring in the settings dialog is inert.
- **F.2 is wider than "plugins cannot register named keys."** Provenance drops
  any item whose ink resolved to literals, and the status bar hard-codes literals
  for its own separator, so provenance is blank there today with no plugin
  involved.

The gate it named, which supersedes the ordering in §3 for this branch: close the
two `render_collected` calls and the two click-path re-renders; delete the second
focus ring or make the arena one a mirror; fix the host-id collision; sweep the
four comments.

It also named what should not be re-litigated: the read-back family reads the
tree that painted rather than a parallel walk, the settings dialog's painter is
down to a box and a divider column, the widget-panel interior painter really is
deleted, and `fresh-ui`'s own focus fixes are real bugs found with the right
explanation.

---

## 6b. Escape does not close a plugin's floating panel. **This branch's.**

**Retracted, in full.** This section previously argued the failure was master's,
on two grounds: master's Web UI workflow has failed every run since 2026-08-10
with a similar `TimeoutError`, and the router's panel-Escape path, the outcome
handling and the orchestrator plugin are all byte-identical to master. Both
grounds are true. The conclusion drawn from them is false.

**The controlled test.** Same isolated `HOME`/`XDG_*`, same workspace, same
sequence — palette, Toggle Dock, click `[ New Task… ▾ ]`, click `New Task…`,
then Escape — driven in a real terminal against both binaries:

| | Escape #1 | #2 | #3 |
|---|---|---|---|
| `master` | **closes** | — | — |
| this branch | open | open | open |

Five presses on the branch in an earlier run also left it open. The dialog is
unclosable by keyboard here and closes on the first press on master.

**Why the earlier reasoning failed, which is the part worth keeping.** "Master's
CI is also red" is not "master has this bug", and I treated them as the same
claim. The diff evidence pointed at the wrong layer: the router and the plugin
*are* unchanged — but what this branch rewired is the keyboard layer that
decides whether Escape reaches the router at all (`panel::keys_layer`'s scope,
S2). A trace showed the fact arriving at `dispatch_floating_widget_key` with
`slot=Floating` and the router answering `FallThrough`, which is consistent with
both stories; what distinguishes them is running master, and I did not until the
UI was driven by hand.

**Status: open, and the highest-priority defect on the branch.** It is a
keyboard-only regression — the dialog still closes by clicking its `[×]` — on a
surface a plugin raises, which makes it a plugin-facing break rather than a
chrome one.

---

## 6c. Open defects found while finishing, not fixed here

**`Ui::pending_messages` is never drained by the editor — and it poisons
repaint, not just focus.** `needs_frame()` returns true forever once anything
lands there (`fresh-ui/src/schedule.rs:673`), `shell_dispatch` reads it as
`tree_stale` and folds it into `changed`, so after the first autofocus settle
the editor reports "changed" for every input event and repaints unconditionally
— defeating the optimisation the comment above it introduces. The Vec also
grows for the life of the process. `apply_autofocus`
settles focus after a frame and leaves the resulting `FocusGained` there;
`Ui::dispatch` returns only what handlers produced during routing, and nothing
calls `take_messages`. So a focus change the *tree* settled has never reached
the host, and the backlog grows for the life of the process.
`advance_panel_focus_in_tree` takes and discards it before moving — applying it
would deliver a stale `WidgetFocus` for whatever was autofocused frames ago —
but draining it properly is its own change, and until then autofocus is
invisible to the host.

**A mounted-but-unfocused panel still advances on the arena**, so its registry
key can name a widget the tree's focus is not on, until focus next enters the
panel. Closing it means seeding the tree from the registry when a panel takes
focus: the second direction of the focus-key mirror, which belongs with that
mirror's own step.

**`UiFact::DockContext { x, y }` carries a cell its applier destructures away** —
the same dead-payload shape as `DockPress`, which collapsed into `DockFocus`,
but a fact's shape rather than a dead path, so it wants its own decision.

**A settings entry-dialog `[x]` is unreachable by mouse. A regression, not
pre-existing — this entry said "pre-existing" and was wrong.** The described
`List` arm reports `at: None` (`view/shell/widgets.rs:1105`, `:1289`) and
`entry_text_list_press` reads it as `at.unwrap_or(0)`
(`view/settings/mouse.rs:445`), so `text_list_target` never answers `Button`.
Master resolved it from a real column: `handle_text_list_click(idx, sub_row,
col, layout)` hit-tested the trailing button
(`origin/master:crates/fresh-editor/src/view/settings/mouse.rs:1026`,
`:1113-1115`). Clicking `[x]` on a committed row focuses it instead of removing
it. Uncovered by any test — a click on that column would have failed on the
first frame.

Do **not** fix it by sending the piece-local `x` the way `row_pieces` does: a
piece's column cannot be rebased to a row's, so that produces wrong-but-plausible
columns, which is worse than always-zero because it looks tested.

---

## 6d. The web's plugin panels, deleted — and what bringing them back costs

**Deliberate, and authorised.** The web frontend's plugin-panel support was
removed so the retained side could stop having a second consumer of the
immediate-mode collector's output. It will be brought back once the retained
work is finished, and it must be brought back *differently*. This section exists
so that is a decision someone makes rather than a gap someone rediscovers.

### What was deleted

| where | what it was |
|---|---|
| `view/scene.rs` | `WidgetSurfaceView`, `WidgetInstanceView`, `WidgetHitView`, `Editor::widgets_view` — the dock's and the floating panel's `WidgetSpec`, the registry's instance-state map, the recorded hit list's *identity* half, the focused key and the panel's screen rect, one entry per mounted surface |
| `webui/mod.rs` | the `regions.widgets` key of the scene payload, and `apply_widget`'s `"panel"` arm |
| `app/widget_runtime.rs` | `deliver_widget_hit_by_index`, `deliver_widget_hit_semantic`, `synthesize_list_hit`, `synthesize_tree_hit`, `synthesize_control_hit`, `copy_context_anchor_cell`, `set_widget_text_cursor` |

`web-ui/js/65-widgets.js` is **not** deleted, and the reason is narrower than
"it is shared". `widgetEl` has one live caller left — the overlay prompt
toolbar (`50-palette.js`, `{kind:"toolbar"}`) — so the file's toolbar routing
still ships; `routeWidget`'s `surface:"panel"` branch, `routeControl`,
`widgetSurfaceEls` and the two dock-overlay helpers are all unreachable. The
settings dialog, the keybinding editor and the aux modals build their elements
in the same file but post `sendSettings`, so nothing there changed. Every reader
of the region elsewhere in the JS guards on `regions.widgets || []` and degrades
to "no dock" with no edit. The dead run is marked at its start.

### What it did, precisely

The web never received geometry for a plugin panel. It received the spec, laid
it out itself in DOM, and echoed a click back as an *identity* (`widgetKey` +
`eventType` + `payload`) with the recorded hit's index as a tiebreaker. The
index existed because the recorded list is windowed to the TUI's visible rows
while the browser renders the whole list, so a click on a row outside the TUI's
window matched no recorded hit — which is why the three `synthesize_*_hit`
functions existed, rebuilding the `HitArea` the renderer *would* have emitted
from the spec and the instance state.

**Those synthesizers were the most valuable thing in the deleted set**, and it
is worth being explicit about why: they are the only written instance of the
derivation §4.1 of `fresh-editor-plugin-widgets-end-state.md` wants everything to
use — `HitArea`'s identity half as a pure function of `(spec, instance state)`.
Restoring the web should re-derive, not restore them from git.

### Why it had to go

`WidgetPanelState::hits` had two classes of reader. One reads its *geometry*:
`hit_test_row_aware`, for a pane-mounted panel that rides the buffer's scroll,
where the recorded rows really are the rows on screen. That one is correct and
stays. The other read its *identity* half, and every member of that class was
the web. While it existed, the immediate-mode collector had to run in full for a
panel the tree had already laid out, purely so a list nobody in the TUI reads
could be shipped to a frontend. Deleting it is what makes "the display list is
the output" reachable at all.

### What the replacement is

**The web should consume the display list**, the way it already consumes the
status bar, the file browser, the settings dialog and every pane rectangle. A
described plugin panel is nodes in the same tree those come from; there is no
plugin-specific projection to write. Concretely:

1. The fold already resolves every item's rect, clip, style and key. A panel's
   subtree is addressable by the keys the adapter already applies
   (`widget_focus:<key>`, `panel_interior`, `panel_frame`, `panel_body`).
2. A click comes back as a *cell*, and is dispatched through the same
   `fresh_ui` hit path the TUI uses — which means the web stops needing a hit
   list, an index, an ordering, or a synthesizer.
3. Text-caret placement comes back as a byte through the same route S0
   (`Event::text_byte`) gives the TUI, rather than through
   `set_widget_text_cursor`.

The obligation the deleted code met is real; the *storage* it met it with was
not, which is exactly what §7.2 of the end-state doc concluded before this was
authorised.

### Tests knowingly broken

The playwright web-UI suite's dock and plugin-widget coverage now fails: the
scene ships no `regions.widgets`, so nothing renders and no `/widget` panel
click is accepted. Those tests are **left failing on purpose** — not deleted,
not weakened — because a green suite over a deleted feature is worse than a red
one. They are the specification of what the display-list replacement has to
satisfy.

**What this section originally missed, found by reading the CI log rather than
reasoning about it.** "Those tests fail" was wrong about the blast radius. The
driver is one long top-level script, and the first thing the deletion broke was
not an assertion but a `locator.click()` on the dock's "Filters" disclosure —
an uncaught `TimeoutError`, which kills the node process outright. Everything
after line 185 therefore never ran: the keybinding editor, Settings and its
three sections, the WebSocket push transport, region diffs, shared-view
mirroring, per-region DOM patching, zoom, TUI-parity placement, the wave
animation, Open File, submenu seams, terminal selection, Alt-hold selection,
the theme system, embedded-terminal hover, dropdown anchoring and touch pan —
about twenty sections, every one of them testing something the web still does
correctly. The deletion had quietly taken the *whole* web suite dark, and the
job's single red X looked the same either way.

The two dock-dependent regions are now guarded by a `webDockPanels` flag
(`web-ui/test/drive.mjs`): absent, they print a `SKIP` and the suite continues.
The missing surface is still one recorded `FAIL`, so the job stays red and the
feature cannot quietly pass — but the twenty sections behind it are guarding
again. `SKIP` counts as neither pass nor fail for exactly that reason.

**The general point, which is not about the web:** a suite that aborts on the
first failure reports the same colour whether one thing is broken or a hundred
are untested. Deleting a feature that other tests interact with *incidentally*
needs the interaction guarded, or the deletion silently buys a green-looking
red. Worth checking the next time a surface is removed rather than migrated.

**And the guard itself had the same bug one level down.** With the twenty
sections running again, four of them failed immediately — the terminal-selection
checks, which assert on status-bar segment text. Not a discovery: a regression
from the guard. The section's opening probe toggles the *editor's* dock open to
make a surface appear; the surface never appears now, but the dock does open,
and the skip meant nothing closed it again. It ate ~30 columns for the rest of
the run, and the narrower status bar truncated `"… read only (Ctrl+Space …)"`
out of existence — the precise failure a comment elsewhere in that file has
always warned about. The probe now undoes its own toggle when it comes up empty.

So the rule has two halves, and the second is the one that bit: **guard the
interaction, and undo the probe.** A check that changes state to decide
something is not read-only, and skipping the work that would have cleaned up
after it leaves the state behind for every test that follows.

---

## 6e. Why the immediate-mode collector still runs for a described panel

The stated target was that it stop entirely. It cannot yet, and the reason is
four live outputs rather than an oversight. For a **described dock or floating
panel** — after the web deletion — `render_floating_spec`'s outputs stand as:

| output | live consumer for a described panel |
|---|---|
| `entries` | an *anchored* popup's `content_cols` (`Panel::anchored_width`, §6.6 of the end-state doc). Nothing else: `content_rows` is read only for a `Host` interior, which means no panel at all |
| `hits` | **none** |
| `focus_cursor`, `overlays`, `embeds` | **none** — the slot arm returns before the buffer write |
| `instance_states` | **live.** The collector is the seeder and sanitizer of the state a plugin's re-emitted spec must not lose |
| `focus_key` | **live** (the clamp onto a key that exists in the spec) |
| `effective_rows` | **live** — `List`/`Tree`'s `on_key`/`on_wheel` page and bound arithmetic |
| `boxes`, `tabbable` | **live, narrowly**: `handle_widget_focus_advance`'s arena ring, for a panel that is mounted but does not hold the tree's focus |
| `popup` | **live** — `UiFact::WidgetPopupDismiss` reads `fwp.popup.widget_key` to find the open dropdown |

Stopping the render therefore needs, in order of size:

1. **`resolve` split out of `collect`** (end-state doc R2 / S7). Not mechanical:
   `List`'s state resolution runs through `plan_list_layout`, which needs the
   panel width *and* measures card items to get `item_height`, so the state
   writer is entangled with the layout. This is the real blocker.
2. **`effective_rows` delivered rather than stored** (§4.5) — the row window is
   the viewport's, and the key event must carry it.
3. `popup` and `focus_key` become derivations over `(spec, instance state)`;
   both are small once (1) exists.
4. `entries` stops being read once `rule()` retires `node()`'s width parameter
   and `anchored_width` becomes `Sizing::Auto` (§6.6).

**What is *already* true and was not before:** for a described panel the
collector's *pointer and focus* outputs are no longer authoritative anywhere.
`hits` has no reader, the box arena answers no wheel (`handle_widget_panel_wheel_at`
declines a described panel outright, and says why), and
`Interior::has_focus_targets` derives from the spec through the same predicate
the tree's ring admits by instead of reading the recorded `tabbable`.

**And the per-frame double render is down to one arm.** `render_collected` is
called inside a description build in exactly one place now
(`view/shell/widgets.rs`, the markdown text-area's reflow) — S8's markdown half,
whose replacement is a wrapped viewport with `cursor_byte`. The card-list arm has
crossed. So "two full rendering pipelines run for every plugin panel" is now true
only of a panel containing a markdown document view.

---

## 6f. One class, found three times: who decides a key was taken

Three defects on this branch turned out to be the same question with three
different answers in the code. Each was found by a different route — driving a
terminal by hand, an unbiased review, and an agent chasing a 180s hang — and
each looked local until the third one landed.

| where | what it did |
|---|---|
| `panel::interior` | called `e.stop()` as it emitted `PanelKey`; `stop()` **is** the claim in `fresh-ui`, so a router that answered `FallThrough` could never hand the key back |
| `shell_dispatch` | folded the interior's answer as `claimed \|\| took`, so a decline was unrepresentable even once one existed |
| `on_the_ring` | made `Slot::Pane` widgets *traversable*, so Tab resolved to `move_focus` in a surface that has no keyboard layer and names no scope — claimed by traversal, and the plugin's own binding never ran |

The symptoms were unrelated on the surface: a dialog that would not close on
Escape, a dock chord that needed two presses, and nine `search_replace` tests
hanging on a `wait_until` whose condition Tab was supposed to satisfy.

**The gap is that "was this key taken" has no single authority in the tree.** It
is currently settled by a `stop()` at one seam, a boolean fold at another, and a
traversal flag at a third — and a described surface can claim a key three
different ways without any of them consulting the others. Every surface
described from here on inherits all three.

What would close it, in rough order of cost: a `fresh-ui` flow state meaning
*stop propagation and intents, but do not claim* (the general answer, and a
library API change); or, short of that, a single editor-side helper that every
`Modality::Focus` seam routes its verdict through, so the three sites become one.
The `Option<bool>` fold is that helper's first half and is already in place.

**Test shape that catches this class:** assert `Dispatch.claimed` at every
`Modality::Focus` seam, not just which message was produced. The regression that
started this was shipped by a test that checked the message and dropped
`got.claimed` on the floor; `view/shell/panel.rs`, `frame.rs` and `prompt.rs` —
exactly the three `Modality::Focus` surfaces — still contain far fewer `.claimed`
assertions than the surfaces where declining does not matter.

---

## 6g. A second class, found three times: where a panel's focus actually is

The four `orchestrator_dock` tests that still hung after §6f's six fixes —
`dock_slash_filters_and_enter_returns_to_list`,
`dock_filter_clears_when_focus_leaves_so_reentry_shows_all`,
`dock_enter_on_focused_button_runs_button_action` and
`picker_space_toggles_focused_checkbox_not_list` — were not four problems.
They were three, and the three are one question left unanswered: **a described
plugin panel has two rings — the tree's focus and the widget registry's
`focus_key` — and nothing said which is authoritative, or when.**

None of the three is §6f's defect. That class is "who decides a key was
*taken*"; this one is "where the panel's focus *is*", and the two meet only in
that a described surface answers both from more than one place. The dock's `/`
was routed correctly and claimed correctly on every keystroke: what was wrong
was the widget it was routed *to*.

| where | what it did |
|---|---|
| `Ui::relink_from_pub` (`fresh-ui`) | assigned the focus parent's **entire** child list from what one relinked subtree contributed. A `layout_reader`'s nearest focus ancestor is almost never its own node — it is whatever focusable encloses it — so the widgets beside the reader, and every *other* reader under the same ancestor, were its siblings in that list. The last reader to rebuild won. A panel whose interior held two readers ended every frame with a scope containing only the second one's contribution: for the orchestrator's picker, nothing. `apply_autofocus` then fell back to focusing the empty scope itself, and Tab moved nothing — while the panel's nine focusables sat in the tree with their focus parent pointing correctly at that same scope. |
| `shell_dispatch` | drained `Ui::pending_messages` **after** routing the input. A settle's facts describe the frame that produced them, so applied afterwards they overwrote what the key had just decided with where focus was one frame ago. The dock's `/` was exactly that: the applier moved the panel's focus to the filter, the mount frame's pending `WidgetFocus { sessions }` landed on top of it in the same loop, and every character typed after it went to the session list while the filter never filtered. |
| the focus mirror | had one direction only. `UiFact::WidgetFocus` wrote the registry from what the tree decided; nothing wrote the tree from what the *host* decided. Every host-side move made while the panel was already focused — a plugin's `setFocusKey`, a kind's focus effect, the dock's `/` landing on its filter — left the two rings on different widgets, because `apply_autofocus` deliberately leaves focus alone once it is inside the scope. The description painted its marker where the registry said; the next Tab moved from where the tree said. |

The symptoms were again unrelated on the surface: a picker whose Tab did
nothing, a dock filter that swallowed its own keystrokes, and a dropdown that
left traversal one stop off.

**The gap is that the tree/registry mirror was specified in one direction and
assumed in both.** The landing when focus *enters* a panel is a property of the
description (`on_the_ring`'s `autofocus`), and that half was right. A move made
while the panel is already focused cannot be expressed as a description at all —
there is no frame boundary to settle it against — so it must be an imperative
write, and until now there was none. `Editor::focus_panel_widget_in_tree` is
that write, and it is called from both host-side deciders:
`set_panel_focus_and_notify` (every key-, click- and advance-driven move) and
the plugin's own `WidgetMutation::SetFocusKey`.

The third writer of that key — the re-clamp `rerender_widget_panel` performs
when the focused widget is no longer tabbable in a new spec — deliberately does
*not* push. A widget that left the spec left the tree with it, so the tree's
focused element is gone and `apply_autofocus` has to settle anyway; the
description's `autofocus` mark then lands it on the clamped key. That is the
*entering* half doing its job, not a gap.

**What is worth generalising:** `relink_from_pub`'s defect is the library's, not
the editor's, and it is live for any consumer that puts two `layout_reader`s
under one focusable. The fix recomputes the parent's child list over the current
element tree rather than trusting one subtree's contribution — the same set and
order a full `relink_node` walk produces. Any future incremental relink needs the
same treatment: **a subtree may rebuild itself, but it may never author a list it
can only see part of.**

**Test shape that catches this class:** frame a scope holding two readers where
only the first contributes focusables, then Tab twice — `fresh-ui`'s
`a_second_reader_does_not_empty_its_focus_parents_ring`. And on the editor side,
assert the *tree's* focus after a host-driven `set_panel_focus_and_notify`, not
just the registry's: the registry agreed with itself throughout every one of
these failures.

---

## 6h. The `resolve`/`collect` split, designed — and §6e corrected

§6e named "splitting `resolve` out of `collect`" as the blocker to retiring the
immediate-mode collector, and said the entanglement was that `List`'s state
resolution runs through `plan_list_layout`, which needs the panel width. A
design pass over the collector, every kind impl and the `fresh-ui` viewport
internals says that is **true but much narrower than stated**, and that three of
§6e's four follow-on items are wrong about what they depend on.

### What §6e got wrong

**The width reaches resolution through exactly one number, on one path.**
`plan_list_layout` takes `panel_width` only for `render_list_cards`, which
renders each `item_spec` and takes the tallest as `item_height`. The classic
path never touches width (`visible_items = avail_rows`). `Tree` is not
entangled even on its card path — its `item_height` is the spec's own field.
One field of one variant, not a pervasive coupling.

**And for a described panel that resolution has almost no reader at all.**
`scroll_offset` is folded only by `List::on_wheel`, and the wheel router
declines a described panel before reaching any kind; the scrollbar writer is
likewise painted-only. `item_height` is read only by those same painted paths.
The described list owns its window in element state
(`fresh_ui::List::windowed_stateful`). The one field with a live described
reader is `selected_index`. So `resolve` for `List` needs `clamp(stored, total)`
and `carry(user_scrolled)` — the shape `kinds::dropdown::resolve` already has.

**`focus_key` is already pure** — `collect_tabbable` is a `box_meta` spec walk
with no geometry, and the clamp runs *before* collection and is returned
verbatim. It does not depend on the split; it depends on nobody having lifted it
out. Ten lines, available today.

**`popup` is independent of the split too.** Its only described reader wants one
string, and the described `Dropdown` arm already builds its own pop-over and
never reads `fwp.popup`. It reduces to "which keyed `Dropdown` does `resolve`
report open", a spec walk.

**§6e also filed `boxes`/`tabbable` as needing the arena.** They do not: the
arena's only described consumer reads `focusable`, `key`, `focus_trap` and
`parent` — all `box_meta` facts plus tree shape. No rectangle is consulted. The
arena's rectangles matter only to wheel routing, the scrollbar pass and the
text-drag region, all painted-only.

### What §6e got right, in direction but not in unit — and the bug under it

`effective_rows` should indeed be delivered rather than stored. But it is
written in **rows** and consumed by a pager that moves in **items**:
`select_move` adds its delta to the selection and clamps against the item count.
`on_wheel` divides by `item_height` first and says why; `on_key` never did. **A
list of three-row cards in a twelve-row window paged eleven cards when four were
on screen** — on the painted and described paths alike, with no test in the
suite covering widget paging at all.

A second bug sits beside it: `activate_event` reads the stored index with no
upper clamp, unlike `select_move`. Today the collector's write-back sanitises it
a frame earlier, so it is latent — and deleting that write-back naively would
have made Enter on a shrunken list fire an out-of-range `index` with an empty
`key`. **Both are fixed now**, ahead of the split, each with a test.

### Derivation or fold, field by field

`WidgetInstanceState` is host-internal — no `serde`, not in `fresh_core::api`,
and the web's projection of it is deleted — so this changes nothing a plugin
sees.

| field | nature | verdict |
|---|---|---|
| `total` | `items.len()` | derivation; never store |
| `effective_sel` | `clamp(stored, total)` | derivation; delete the write-back. Its *input* `selected_index` is a real fold |
| `scroll_offset` | follow-selection clamp over its own previous value | fold — but the **painter's**, not the widget's. Move to a paint-output channel |
| `visible_items` | `avail_rows / item_height` | derivation over geometry; *deliver* |
| `item_height` | measurement of cards at a width | derivation over geometry; *deliver* |
| `user_scrolled` | latched by wheel, cleared by selection moves | genuine fold, owned by the handlers; keep, stop the walk writing it |

### Where the width comes from, in a retained world

It does not need to come from anywhere: resolution needed width only because it
was written inside a painter. The genuinely measured quantity is the card band,
and **the tree already measures it** — the described card arm declares
`RowHeight::UniformMeasured`, which the viewport resolves by laying every item
out at the current width and republishing the answer within the same layout
pass.

Which surfaces a disagreement worth naming: the runtime re-renders cards one
column narrower *only when the list overflows*, while the description asks for
`width - 1` unconditionally. A card that wraps differently at `w` and `w-1` gets
a different band from the two, and nothing checks that they agree.

### The shape, and the one risky step

`resolve_panel(spec, prev, prev_focus_key, auto_focus_first) -> { instance_states,
focus_key, tabbable }`, pure and width-free; `collect` keeps everything else
minus the state writes; and the three fields that leave `WidgetInstanceState`
become an explicit `PaintedWindow { rows, items, offset, item_height }` — the
honest statement that they are not the widget's state but the last paint's
window. `effective_rows` retires in favour of a `Viewport { items }` delivered
to `on_key`/`on_wheel`, answered by one host-side resolver.

Sequenced: **S0** `list::resolve` and the clamp at every read (done in part —
the two bugs above); **S1** `PaintedWindow`; **S2** stop writing derivations;
**S3** `Viewport` as a parameter; **S4** the `fresh-ui` read; **S5** point the
described branch at the tree; **S6** `resolve_panel`, and stop calling the
collector.

**S4 is a library change and must not be pinned editor-side.** The number exists
and is already computed correctly — the viewport publishes its window in items —
but nothing outside the tree can read it: `GeomSnapshot` carries `scroll` and
`content` but not `window`/`band`. It needs `GeomSnapshot::{window, band}` and
`Ui::item_window(&Key)`, both ordinary reads of layout by an outside caller,
same standing as `rect_of`.

### S4 landed, and it corrected two things this design said

**The window rectangle is mixed-unit, not item-unit.** For `ScrollMode::Items`
it is `Rect { x: 0, y: <items>, w: <cells>, h: <items> }` — the vertical axis
counts items, the horizontal counts cells. The text above reads as though "the
window" were wholly in items. **S5 must take only `.y`/`.h` from it.**

**`content` is not the total the offset runs over, and `scroll()`'s own doc said
it was.** For an item viewport `content` is `Size::new(inner_w, rows)` — the
window's own size — while `scroll_max.y` is `n - rows`. So a consumer reaching
for "how many items are there" through `Geometry::scroll()` gets the window
back a second time. The hit path already knows this and reconstructs the total
as `scroll_max + window` (`hit.rs::bar_extents`); §6i exists because that
reconstruction had the units wrong. Left as-is for now — out of S4's scope —
but it is a live trap for S5, and `content` deserves the same unit-tagging as
`window` the moment anything outside the tree reads it.

**`item_window` answers `None` for a cell-scrolling viewport**, rather than its
height in cells. `Some(rows-in-cells)` from a method with `item` in its name is
the §6i defect wearing a name that promises it cannot happen; `Ui::window` is
the unit-tagged way to ask that question. And the descend to the nearest
viewport is contract, not convenience: `List::focusable(false)` puts the
viewport one level under the keyed element, focusable puts it two, and a
breadth-first descend covers both without the caller knowing which.

**S5 is the risky step, and the only one.** It is the first frame on which a
described panel's paging and scroll bounds come from a different number than
before — and the two numbers have silently disagreed all along: the collector's
is the panel's inner height narrowed by the `Col` fill pass, the tree's is the
list node's `.flex(1)` share. Where a described panel's `Col` has other
children they differ *today*. The suite renders unconditionally and will not
show it, so this one gets driven by hand.

**But the blast radius is two surfaces, not "wherever a `Col` has other
children" — that was too broad.** The described arm is
`Some(r) => Sizing::Cells(r), None => .flex(1)`, and the collector resolves
`(Some(v), _) => v` (`kinds/list.rs:529`, `kinds/tree.rs:649`): **an explicit
`visible_rows` wins unconditionally and is never superseded by the height
budget**, so with a count named in the spec both sides use the same number and
cannot disagree. Only the `None` branch can diverge.

In the shipped plugins that branch is exactly two widgets, both Trees, both
omitting the count deliberately and saying so: `plugins/search_replace.ts:821`
and `plugins/audit_mode.ts:2135`. **Those two are what to drive by hand — not
the orchestrator dock**, which passes an explicit count and is the surface one
would reach for first.

(The comment on `List::on_key`'s pager claimed the opposite — that "even an
explicit one can be superseded" — and was false against both resolvers.)

**Residual to establish before S6, by tracing emitters rather than by
inspection:** `handle_widget_text_selection_drag` and `Text::on_wheel` both read
`boxes`' scroll payloads and neither carries an explicit described-panel
decline, unlike the wheel router. §1.5 already names "a gate applied at four of
five sites is not a gate" as this migration's signature failure.

---

## 6i. A third class, found twice: a window and the bar beside it are in different units

Two e2e tests hung on a described plugin panel — `scrollbar_click_scrolls_the_
session_list` and `completion_popup_scrolls_with_mouse_wheel` — and the guess
going in was one seam ("the tree owns the scrollbar, the runtime owns the
scroll window"). They were two defects, and neither is that seam. What they
share is narrower and more useful: **for a windowed list, the number of things
in the window, the number of cells the bar is drawn in, and the unit the offset
counts are three different quantities, and every place that conflates two of
them is wrong exactly where an item is more than one cell tall.**

| where | what it did |
|---|---|
| `Draw::scrollbar_thumb` (`fresh-ui`) | took `(offset, content, track)` and computed the thumb's length as `track²/content` — which is `track × (track/content)`, i.e. it *assumed the window was the track*. True for a cell-scrolling viewport, where they are the same number, and false for an item-scrolling one: sixteen five-row cards showing four of them in a 22-cell track gave `⌈22·22/16⌉ = 31`, clamped to the whole track. Every card list in the editor — the orchestrator's sessions, the dock's — drew a bar that filled its own track: no position, no length, nothing to grab. `Draw::Scrollbar` has carried `window` since the band was measured; both backends threw it away with a `let _ = window`. |
| `Ui::scrollbar_hit`'s press path (`fresh-ui`) | reconstructed the content as `scroll_max + rect.h` — the offset's ceiling, in items, plus the bar's height, in cells. For the same list that made the *hit-side* thumb 15 of 22 cells while the painted one was 22, so the two disagreed about where the thumb was, and a press anywhere in the top two-thirds of the track read as landing inside it. A press inside the thumb picks it up where it was touched and moves nothing, by design — so clicking the track of a card list did nothing at all, in silence, with the bar under the pointer. |
| the described `Text`'s candidate list (the editor) | is the one window in a described panel that is *not* a viewport: `completion_popup` slices its rows out of `completion_scroll_offset`, host state the plugin's `SetCompletions` writes, and scrolling it also sets `completion_navigated`, which is what makes Enter accept the highlighted row. Its float claimed the notch, handed it to the modal, and `handle_widget_panel_wheel_at` declined the described panel outright — correctly, on the coordinate-space grounds it states at length — so nothing moved it. The gate was right and there was no other route. |

The first two are the library's and are fixed there: `scrollbar_thumb` takes
the window, and the hit path derives content and window from `ScrollInfo`
(`window.h` in the offset's own unit, `scroll_max + window` for the content)
rather than from the rectangle. For a cell-scrolling viewport `window.h` *is*
`rect.h`, so every existing bar is arithmetically unchanged — which is what
made the defect invisible: the whole test suite scrolls cells.

The third is the editor's, and it is closed the way §6g's mirror was — by
saying imperatively what no description can express. `UiFact::WidgetWheel`
carries the widget the tree hit-tested the notch onto, and
`Editor::wheel_widget_by_key` runs the same `kinds::behavior(..).on_wheel` the
box arena would have run. **The tree names the widget; the runtime moves the
window.** That is not a hole in the arena's gate: the gate is about a
*coordinate space* the described panel does not have, and naming the widget is
precisely what removes the need for one.

**Test shape that catches this class:** never test a windowed list with
one-cell items only. Both library defects are invisible at `item_rows(1)` and
both are a single assertion away at `item_rows(5)` —
`pressing_the_track_of_an_item_scrolling_viewport_scrolls_it` and
`a_scrollbar_thumb_measures_the_window_not_the_track`. And on the editor side,
assert the *fact* a float raises for a wheel, not only the one it raises for a
press: the completion box's press path was covered throughout, and its wheel
had no test at any layer.

---

## 7. Merge posture for the current branch

**Superseded — this said "should land, but not as-is", and the gate it named is
now met.** Phase 1 is done, and Phase 2 is done through the collector's
retirement: for a described panel the immediate-mode renderer no longer runs,
`resolve_panel` answers what it used to answer, and the window comes from
whichever layer laid the widget out. The full CI suite passes on every platform.

Kept as a record of what the gate *was*, because it was the right gate: F.6 was
a shipped feature dead over most of the chrome; 1.1 was a live state-corruption
class; 1.2 closed the blind spot that let three bugs ship. The Phase 2 argument
— that it is the difference between a retained layout engine and retained-mode
UI, and gets more expensive the more surfaces adopt the adapter first — is why
it was not deferred past the merge.

### What is true at the merge point

- **Green**, except the web UI suite, which is red on purpose: the web's
  plugin-panel projection is deleted (§6d) and its absence is one recorded
  failure. The ~20 sections behind it run and pass. A green suite over a deleted
  feature would be worse.
- **Four defect classes are documented from the code**, not from theory: who
  decides a key was taken (§6f), where a panel's focus actually is (§6g), a
  window and the bar beside it in different units (§6i), and the design that
  corrected this document's own account of its blocker (§6h).
- **The plugin-facing `WidgetSpec` API is unchanged.** Every existing plugin
  keeps working; the whole migration is a backend swap.

### What the next branch owes, in the order it should be done

1. **Rebuild the web's plugin panels from the display list.** This is the one
   knowingly-red thing, and it is deliberate: it was dropped to be brought back
   properly rather than ported twice. Everything the replacement must satisfy is
   still standing in `web-ui/test/drive.mjs`, guarded rather than deleted.
2. **The markdown text-area's reflow** — the last `render_collected` call inside
   a description build, and the reason a panel holding a document view still
   keeps the collector.
3. **The anchored floating panel's `content_cols`** — the one measurement that
   cannot yet be `Sizing::Auto`, and the other reason the collector survives.
4. **`WidgetInstanceState::Text::scroll`**, which leaves with the text-area
   renderer rather than with the collector, and
   `handle_widget_text_selection_drag`, which computes in the wrong coordinate
   space for a described pane panel and works only while the description's row
   order matches the projection's.

### The thing worth carrying forward that is not a task

Three of this document's claims were wrong, all three mine, and all three wrong
in the same direction: right about *what* was coupled, wrong about *how far* the
coupling reached, always overstating it. §6e's blocker was one field of one
variant. §6h's "wherever a `Col` has other children" was two plugin surfaces.
§6d's "those tests fail" was a whole suite going dark.

Overstating a coupling is not harmless caution — it hides the cheap fix behind
an imagined expensive one, and it makes the dangerous step look like every other
step. The correction each time came from reading the code or the log, never from
re-reading this document. Treat it as a map, not as evidence.
