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

## 7. Merge posture for the current branch

The branch is a large net improvement and should land — but not as-is. Phase 1 is
the gate: F.6 is a shipped feature dead over most of the chrome, 1.1 is a live
state-corruption class, 1.2 closes the blind spot that let three bugs ship, and
1.4 is free. Phase 2 is the difference between a retained layout engine and
retained-mode UI, and the more surfaces adopt the adapter before it lands, the
more expensive it becomes.
