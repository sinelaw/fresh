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
is authoritative is decided by a `covered()` gate evaluated separately on several
code paths. This has already produced shipped bugs of one shape: the runtime's
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
| **F.9** | A press on a described *pane*-mounted panel's dead space still reaches the editor-click path, which scans the runtime's stored hit list and resolves it against a screen-to-line projection that is empty for that pane. Every other probe was gated on "is this panel described"; this one was not. Presses that land on a widget are stopped by the node, so what gets here is exactly the case the projection cannot answer. |

F.6, F.7 and F.8 are fixed on this branch; F.2 and F.9 are open. The table is kept
whole because each row names a *class* of defect the migration produces, and the
fix is only interesting next to the shape that caused it.

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
safe until it lands, and **until it does, stop widening `covered()`.** The
`WidgetSpec` API itself does not change — existing plugins must keep working
untouched. That is a hard constraint, and it is satisfiable: the format is the
contract, the renderer is not.

**2.1 Element state.** Scroll offsets, cursor positions, selection and
dropdown-open flags move out of the runtime's registry and into element state,
read and written by the same party. Today one side writes and the other reads a
frame later, and they have already disagreed.
*Exit:* the description no longer receives runtime state as context, and the
hit-test probe no longer has a described-versus-painted branch.

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

**2.3 Retire the mirror as a rendering input.** Once 2.2 lands, the text-property
mirror is only a *text* mirror — for search, copy and change counting — and no
longer feeds paint.

**This is one function.** An audit of every reader found the mirror's paint role
already stood down for pane-mounted panels (the text pass and the scrollbar pass
both return early for a described pane) and never existed for the dock (whose box
is the carved column). What remains is the *centered and anchored* floating
panel, whose outer box is sized by arithmetic over the mirror: its row count
becomes the frame's height and the widest row's display width becomes its width.
The interior is already a node whose intrinsic size layout can measure, so the
fix is for the spot to carry no counts at all. Until it does, deleting the mirror
shrinks every centered plugin modal and every right-click context popup.
*Exit:* deleting the mirror changes no pixel.

**2.4 Then delete the second renderer.** The widget runtime's paint role goes;
it keeps only what the plugin API genuinely needs.

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
the way down. The body — the split grid, buffers, terminals and the text pipeline
— stays a `Host`, permanently and correctly.

**4.2 F.2.** `Paint::Lit` retires into a dynamic theme tier once plugins can
register named keys, after which provenance is total.

**4.3 Delete the dead painters, carefully.** This is a smaller prize than it
sounds. An audit during this work found that the tab, menu, status-bar, file
browser, file explorer, scrollbar and scroll-panel modules all still have live
callers, mostly through the split renderer and the settings/widget stack.
Reachability has to be established properly: a public function with no external
callers is not dead if its own module calls it, and one only its tests call is
coverage rather than weight.

**4.4 Macro replay stops using the retained tree as a calculator.** It currently
takes the tree, runs a full frame for geometry, and puts it back — per replayed
action. A full frame mounts and disposes elements, applies autofocus and can fire
a reveal. Bounded blast radius, wrong shape.

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

---

## 6. Merge posture for the current branch

The branch is a large net improvement and should land — but not as-is. Phase 1 is
the gate: F.6 is a shipped feature dead over most of the chrome, 1.1 is a live
state-corruption class, 1.2 closes the blind spot that let three bugs ship, and
1.4 is free. Phase 2 is the difference between a retained layout engine and
retained-mode UI, and the more surfaces adopt the adapter before it lands, the
more expensive it becomes.
