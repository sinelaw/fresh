# The retained-mode UI: the end state, and the road to it

**Status:** the design authority and the active plan for everything on
screen — the chrome, the plugin panels, and the text buffer's place in the
tree as a `Host` node (§3.7). It replaces, and folds in what was
still open from, every earlier plan and review of this arc — the widget
library design and its implementation plan, the widget-framework v2 review,
the chrome event-model plan, the settings-widget unification plan, the two
TUI retained-mode plans, the three migration reviews, the two parity
ledgers, the first-wave migration doc, the retained-mode plan and the
plugin-widgets end-state doc. Those documents were maps of a migration in
progress; this one describes the destination and the way there. Where an
earlier document recorded a decision that still holds it is restated in §8,
so nothing has to be re-argued from a deleted file.

**Audience:** whoever does the work. It assumes `crates/fresh-ui/README.md`
has been read once.

**Evidence discipline.** Every claim about the code was read at `147bd98`.
Line numbers drift; item names are the stable half of a citation. A claim
that was not verified against the source is marked **verify** and says what
would confirm it.

**What this document is not.** It is not a list of compromises. Every
earlier plan carried a "for now", a "transitional apology", a fallback that
was "fine to defer". This one states the design that is right and the
sequence that reaches it. Where a step is expensive it says so; where a step
is risky it says what to drive by hand. It does not offer a cheaper shape
that leaves two authorities for one fact.

---

## 0. The objective, in one page

There is **one description** of the screen, a pure function of editor state
built once per frame. There is **one tree** it reconciles into, which owns
every rectangle, every focus position, every scroll offset and every hover.
There is **one display list** the tree folds into, and **two backends** that
consume it: the terminal's cell buffer and the web's DOM. There is **one
input path**: a key or a pointer event is routed over the tree, and the
tree's answer is the answer. The text pipeline — buffer content, syntax,
wrapping, folding, the terminal grid, embedded windows — is reached through
`Host` leaves the tree lays out and never looks inside; it is the one thing
that paints itself, by design and permanently.

The buffer is not outside the tree: it is a `Host` node the tree lays out,
hits, focuses and asks for a byte, whose cells the text pipeline paints
directly (§3.7).

Everything that used to sit beside the tree goes: the ranked keyboard
tables, the recorded rectangles, the per-surface hit tests, the second and
third focus rings, the immediate-mode widget painter's authority over
anything a user can point at, the parallel projections the web painted from.
A surface is finished when the tree *measures* it — no rectangle, width or
pre-fitted string reaches its description — and when nothing outside the
tree can answer a question about it.

Five properties, each the general form of a bug this arc has already
shipped and fixed, are what the design is judged against:

1. **A description states; it does not re-derive.** If layout knows a
   rectangle, nothing recomputes it. A description type with a rect field
   has merged the description and the render object.
2. **State the rule the painter enforced, or lose it.** The question when
   migrating a surface is never "does it look the same" but "what did the old
   code guarantee that the description does not say".
3. **Identity is declared.** A stateful node without a key is a corruption
   waiting for a sibling.
4. **One authority per fact.** One writer function; any number of deciders;
   projections that follow and never lead.
5. **A comment that argues for a property the code lacks is a defect.** Every
   stage deletes the comment with the code it describes.

---

## 1. The library, as built and as it must become

### 1.1 The model that stands

`fresh-ui` is a retained, reconciling tree with no backend and one runtime
dependency. Three trees, in cost order: immutable **descriptions**
(`Node<M>`), rebuilt freely, no identity, no side effects; persistent
**elements**, matched by `(type, key)` at a position, owning component
state; **render objects**, holding geometry, focus registration and host
handles. A frame is reconcile → layout (constraints down, sizes up, parents
place children; anchored layers resolve in a second stage of the same pass)
→ fold into a flat, ordered, absolute, keyed display list. Input runs the
other way: hit-test the render tree, then capture → target → bubble; keys
travel the focus chain, then resolve through Shortcuts → Intents → Actions
with no central context enum. Layers are ordinary children laid out
out-of-flow, each a stacking context, with modality, scrim and dismissal as
declared properties. Focus is a separate tree whose registration is held by
the render object, so it survives reconciliation. Composition is the only
extension mechanism. Descriptions carry no geometry; geometry is readable
only after layout, through a handle whose validity window the type states.

All of that is built, tested against a demo application with goldens and a
conformance suite, and is not in question. The seven goals in the crate
README are the review checklist for every change below.

### 1.2 What it must grow

Each of these is a *library* capability, stated with the consumer that needs
it and the test that fails without it. That is the standing rule for a
library change (§7): underivability from the library's own outputs, plus a
caller in the same PR. None of them is an editor-shaped hole; each is a
general property a second host would want.

**L1 — Focus follows the description.** `apply_autofocus`
(`crates/fresh-ui/src/focus/mod.rs:529`) settles three cases: focus already
inside the active scope (nothing), a scope opened (land on its `autofocus`
mark), a scope closed (restore). It needs the fourth: **the active scope's
mark moved since it last settled.** Change-detected by the mark's `Key`, per
scope element, never by comparing the mark with focus — a mark that merely
disagrees with focus is the tree's own ring having moved and the description
catching up next frame; a mark that *moved* is a decision the tree has not
followed. A mark that goes away rests focus on the scope's own element, which
is what an empty scope already does (`:575`). The landing is a real
`focus_element` (handlers fire, the echo lands in `pending_messages`). When a
confinement holds marks at more than one depth — a dialog's own fallback
and the fallback of a dialog it opened inside itself — the innermost wins,
because the surface that opened last is the more specific statement. Two
refinements found by building it: a confinement seen for the *first* time with focus already
inside it follows its mark (the panel just took the keyboard around a focus
that was already there — that is its entry); and **a scope may mark its own
element**, which is how a description says "nothing inside is focused, and
that is a resting state" — the tree rests on the scope and Tab starts from
outside the ring, rather than landing on the first control and telling the
host about a focus nobody chose. `active_scope()` (`:434`) names the layer's
scope ahead of an inner `focus_scope` node, and the landing is unconfined,
so a host decision may move focus out of a trapped subtree. And the settle
runs over the tree that will be painted: a `layout_reader` builds during the
layout pass and raises its dirt after the drain, so `settle` flushes that
dirt *before* `apply_autofocus` as well as after — otherwise a mark inside a
reader nested in a reader (a dialog's footer inside its box) is found a
frame late and focus rests on the scope meanwhile. Three more, found by
blurring the dock: **the root is a scope** — outside every confinement the
whole tree settles against its own mark, so a mark moving from one subtree
to another (the keyboard handed from a panel back to the pane behind it) is
followed like a mark moving inside a dialog; **a confinement that stops
confining releases what it held** — the dock's interior outlives the
keyboard layer that scoped to it, and focus left inside it by that layer
goes to the enclosing scope's mark rather than staying on a surface that
has given the keyboard up; and **a mark on a `skip_traversal` element
counts** — `skip` says Tab does not stop there, not that focus may never
rest there, and the base's own focus holder is exactly such an element.
Which is the last piece: **focus is never nowhere.** The active pane's
content is a keyed, skipped focusable the description marks whenever no
panel holds the keyboard (`splits::content_surface`; a pane-mounted
panel's wrapper, when none of its widgets does), and it observes every key
(`Flow::Observe`) until the buffer host's keymap rides on the tree (L3). A
blurred panel's widgets stop marking (`widgets::Ctx::keyboard`, the host's
focus fact for the slot), so the tree's focus — and with it the key context
— leaves the panel on the frame the blur is described. This is the whole
of the focus problem in sinelaw/fresh#3176; §2.2 says why. *Landed*, with
its tests in `crates/fresh-ui/tests/focus.rs` and
`app/widget_runtime.rs` (`blurring_a_panel_moves_the_trees_focus_to_the_pane_behind_it`).

**L2 — One vocabulary for "was this key taken".** Today a key is claimed by
`e.stop()` at one seam, by a boolean fold at another and by a traversal
flag at a third, and a surface can claim a key three ways with none
consulting the others. The library grows a third disposition beside *claim*
and *decline*: **observe** — the handler acted and the key must still reach
the surface beneath (what the widget kinds call `PassAfter`). `Dispatch.
claimed` is derived from that vocabulary alone. `on_key_capture` (`:99`)
already exists and is the down-leg counterpart; with the three
dispositions every key policy in the editor — a plugin's mode binding, a
dialog's chord, a kind's "I updated myself but the key is still yours" — is
expressible on the node that owns it. *Landed*: `Flow::Observe` /
`Event::observe()` end propagation and the tree's intent resolution
without claiming, and `dispatch_key` reports the key unclaimed; and
`Dispatch.claimed` is the tree's word alone — the `Option<bool>` fold the
editor's seams completed after the fact is gone. What made that possible
is a ruling about the seams themselves: a seam that hands a key to a host
interior (the prompt's, a focused panel's) *claims* it, because the key is
that surface's, and what the surface does with a key it does not bind is
its own business — for both, handing it on to the editor's own keyboard
(`Editor::hand_key_to_editor`, from the applier). A decline is `None`
with no `stop()`, as a collapsed sidebar header does; nothing declines by
answering twice.

**L3 — The keymap rides on the description.** A node can declare
`shortcuts` — chord → intent, or chord → message — built by the host from
its keybinding resolver for the context that node *is*: the buffer host
declares the buffer's mode, a plugin panel's scope declares the plugin's
`defineMode` bindings, the root declares the global chords. Then "which
`KeyContext` applies" is a read of the focus chain, not an enum computed by a
ladder of rungs that mutate and decline. The library already has
`Shortcut → Intent → Action`; what it lacks is the message-valued shortcut
and the guarantee that a shortcut declared on an ancestor pre-empts a
focused descendant's default handling when the host says it must (which is
L2's capture leg plus the shortcut table consulted on it). *Landed for the
plugin panels, with no library change*: the capture leg and a
message-valued handler were already there, so a panel's keymap is
`view::shell::panel::Keymap` — the plugin's mode and the resolver — set on
its interior as an `on_key_capture` that answers a key the mode explicitly
binds with `UiMsg::Action`. The router's mode pre-emption
(`mode_has_binding`, `mode_pre_empts`, `WidgetKeyOutcome::FallThrough`,
`WidgetPanelView::editor_mode`) and `Interior::claims_tab` went with it.
Still to come: the buffer host's mode and the root's global chords (S7).

**L4 — The traversal order of a subtree is queryable.** `Ui::
traversal_order(root)`, `next_in(root, from, dir)`, `enclosing_focus_scope`
and `contains`: the same policy `move_focus` uses, as a pure read, available
whether or not focus is inside the subtree. This is what lets a plugin's
`FocusAdvance` on a panel that does *not* hold the tree's focus advance the
panel's focus fact along the one ring there is, instead of a second ring
walked over the spec. *Landed*; `focus_ring_scoped_in_spec` is gone.

**L5 — A wrapped-text viewport in source coordinates.** `cursor_byte`
(`desc.rs:1554`) exists. What is missing is the viewport over wrapped
`text_runs` whose scroll unit is a rendered row and whose caret and selection
are bytes of the logical string, painted where layout put them. The consumer
is the markdown document view (the last immediate-mode render inside a
description build, `view/shell/widgets.rs:2065`); the test is a caret placed
by byte landing on the right row after a width change.

**L6 — A keyed geometry index.** `find_by_key` has 160 call sites in the
editor and walks the tree. After layout the library publishes `Key → Rect`
(and the scroll/window facts `GeomSnapshot` already carries) as an O(1)
read. Every motion-path consumer — tab drag, scrollbar drag, hover — reads
it. This retires the editor's last recorded-rectangle caches (§3.7).

**L7 — Flex yield order.** Three surfaces (status bar, prompt, search
options) have needed "when space runs out, this child gives way first", and
`flex` cannot say it; the status bar carries a hand-written `left_budget`
for it. A yield priority on flex children, resolved in the same remainder
distribution the design already specifies, is the concept the third
occurrence proved missing.

**L8 — Scrollbar markers.** `Draw::Scrollbar` carries no marks; the overview
ruler (search hits, diagnostics, git hunks — see
`plugin-scrollbar-markers-design.md`) needs them as part of the bar's own
item rather than as a second overlay measured against it.

**L9 — The speculative-build purity check, and geometry validity in the
type.** Both were deferred in the implementation plan's last register and
are still owed. The purity check: a state generation counter per element,
sampled before and after `build`, asserted unchanged in debug builds, with
`Cache<T>` the one exemption. Geometry validity: `BuildCx` exposes no
geometry at all; only a `LayoutReader`'s closure receives a `Geometry`, so
"read after layout" is a type, not a `debug_assert`. `Controller`'s scope
convention gets the same treatment: the `Updater` a child installs is bound
to its own element id by construction.

**L10 — Modality, corrected.** `Modality::Focus` — confinement without a
swallow — is permanent: a non-modal panel (the dock, a sidebar section, a
pane's panel) confines traversal while it is focused and lets the keys it
does not bind reach the buffer host behind it, which is exactly what a
minibuffer's `Ctrl+P` needs. Its doc (`desc.rs:588`) says the opposite and
is corrected. `Modality::Pointer` is permanent too, and for the mirrored
reason: it is one channel's claim for a surface whose *other* channel is
elsewhere — the file-open dialog owns the pointer while its keys are the
prompt's (`prompt::keys_layer`), the floating panel owns it while its keys
are its own `Modality::Focus` layer. An earlier draft of this ledger called
it a painted interior's seam; the painted interior is gone (§3.6) and the
claim is unchanged, which is what shows it was never the painter's.

**L11 — Reconcile accounting.** `diagnose.rs` counts rebuilds; the frame
must also report reconciled elements and re-laid-out nodes so the editor
can assert "a frame with no state change reconciles a bounded number" — the
memoisation exit (§3.10) needs a number, not a feeling.

**L12 — `Draw::Runs`-shaped text for the pane host's chrome.** The body's
own chrome — gutter, tab strip, scrollbars — becomes nodes (§3.7). The text
content stays a host. What the fold needs from the library for that boundary
to be one pass is already there (`HostPainter` is the general form of the
two-band fold's host arm, `view/shell/fold.rs:121`); what the *pane* needs
is a run primitive that can say "inherit the background" so the four cell
patches the text pipeline applies today become run styles. Small, and it is
the last thing between the two-band fold and one pass.

**L13 — A host leaf answers the scroll-facts read.** The viewport publishes
offset, content and window; a host leaf that scrolls its own content (the
buffer, the terminal's scroll-back) must be able to answer the same read so
a scrollbar node beside it and `Ui::window` need not know which kind of node
they are looking at. **Verify** whether `RenderObject` already has the
method a viewport answers through; if it is viewport-private, it becomes a
trait method with the viewport as one implementor.

**L14 — A group is entered at its cursor.** `Node::enters_at(key)` on a
focusable names the stop traversal lands on when it *enters* that subtree
from outside, in either direction; inside, reading order. A card list whose
selected card the arrows move, a radio group, a tab strip: Tab from the
tree lands on the selected card, not the first, and Shift+Tab from the
footer likewise. `FocusEntry::group` carries the innermost group and its
resolved entry; `FocusScope::entered_at` is the one rule. *Landed*, for the
settings body, with its tests in `crates/fresh-ui/tests/focus.rs`.

**L15 — Flex contributes nothing to an intrinsic measure.** A stack hands
its `Flex` children the whole loose maximum even when it is being asked
how big it wants to be, so a row of `label · spacer · accelerator` inside a
`Sizing::Auto` box makes the box as wide as the frame. The rule a flex
layout has everywhere else is that flex divides the room a *definite*
extent leaves, and contributes its floor to an intrinsic one. Making that
the library's rule is what lets a menu dropdown and an anchored panel say
`Auto` instead of measuring their rows' text (`menu::content_width`,
`Panel::anchored_width`). It is not a local change: the settings entry
dialog and the keybinding editor both place flex children under a loose
measure today and read the maximum, so the rule lands with those surfaces
sized by what they hold (S5), not before.

### 1.3 What the library does not do, by decision

No reactive dependency graph; no cell-level damage tracking; no animation
system; no mutable descriptions; no variable-height virtual list (uniform
extent, measured or stated — `RowHeight::UniformMeasured` is the answer to a
card list); no selection model (`Draw::Selectable` says where selecting is
meaningful, the host selects); no `Host` in the plugin vocabulary. Repaint
boundaries remain an optimisation to measure before building.

---

## 2. Keyboard and focus, in full

This is the section sinelaw/fresh#3176 asked for, and it is first because
every other surface's keyboard depends on it.

### 2.1 The invariant

> **One focus fact per panel, one writer function, any number of deciders,
> and a tree projection that lags by at most a frame and never leads.**

The fact is `WidgetPanelState::focus_key`
(`crates/fresh-editor-core/src/widgets/registry.rs:439`): which control the
panel says is its own. It exists even when the panel does not hold the tree's
focus, because that is where focus will land when it does, and because a
plugin may set it at any time. The deciders are the plugin
(`WidgetMutation::SetFocusKey`), the host's key policy, a click, a
`FocusAdvance`, and the tree's own ring reporting a landing
(`UiFact::WidgetFocus`). The writer is one function with those callers and
no others. The projection is the tree: the description marks the fact's
widget `autofocus` (`view/shell/widgets.rs:622`, already true) and L1 makes
the tree follow the mark whenever it moves.

### 2.2 Why the issue's end state is half right

- **Right:** the gap is the library's missing fourth case, and #3163's
  `pending_panel_tree_focus` (`app/mod.rs:1213`) plus its replay in
  `lay_out_shell_tree` (`app/render.rs:4380`) are a third piece of focus
  state whose only job is to make two mechanisms converge a frame later.
  Both go, and the regression test that came with them
  (`a_focus_onto_a_widget_the_next_frame_builds_lands_on_that_frame`,
  `app/widget_runtime.rs:4098`) survives their deletion.
- **Wrong, "`SetFocusKey` becomes a spec property."** `WidgetSpec` and
  `WidgetMutation` are the plugin contract; `setFocusKey` has thirty-odd
  call sites in bundled plugins alone. And focus is written by the host too
  — a Tab, a click, the dock's `/`, the re-clamp when the focused widget
  leaves the spec. A spec field would be a second writer with a stale copy.
  The mutation stays and means what it means; it writes the fact.
- **Wrong, "`focus_key` becomes read-only; `WidgetFocus` stops writing it."**
  The tree's ring is a legitimate decider. If its landings stop reaching the
  fact, the router routes the next key to the old widget and the plugin's
  `focus` event never fires. What goes is the *host-side call into the tree*
  (`focus_panel_widget_in_tree`, `app/widget_runtime.rs:1434`), not the
  tree's report.
- **Unreachable, "zero writers outside the tree's settle."** An unfocused
  panel's fact is the only record of where focus will land. The tree cannot
  write a fact about a subtree it is not in.

### 2.3 The design

1. **L1 lands in the library**, with its five tests (mark moves while focus
   is on the old one → lands; user moved and the mark is stale → stays; mark
   appears on the frame its element is first built → lands; mark moves while
   focus is outside the scope → nothing; mark goes away → rests on the scope
   root).
2. **The editor deletes the imperative write**: `focus_panel_widget_in_tree`,
   `pending_panel_tree_focus`, `retry_pending_panel_tree_focus`, the replay
   call. `set_panel_focus_and_notify` (`:1515`) writes the fact, notifies the
   kinds, fires the plugin's `focus` event, and touches no tree. The
   `SetFocusKey` applier (`app/plugin_dispatch.rs:5609`) writes the fact and
   lets `rerender_widget_panel` re-clamp.
3. **The writer is narrowed by the compiler.** `WidgetRegistry::set_focus_key`
   becomes private to the registry; the host door, the plugin mutation and
   `resolve_panel`'s spec-change clamp are its three named callers.
4. **Input is never routed over a tree older than the facts it routes over.**
   Today `set_panel_focus_and_notify` moves the tree synchronously, so two
   keys in one batch with a focus write between them resolve correctly. After
   (2) the tree learns of the write on the next frame. The rule that keeps
   the guarantee: a write to any panel state the description reads marks the
   description stale, and `shell_dispatch` (`app/shell_host.rs:1446`) lays
   the tree out before routing when it is. The frame build is pure by
   property 1, so building it from dispatch has no side effect but layout —
   checked: `shell_frame` writes only the two caches of what it built
   (`menu_layout_frame`, `shell_frame_status_bar`), which the next build
   overwrites. The e2e harness renders between events and cannot see this
   class; the test sends two events before rendering. *Landed*:
   `Editor::lay_out_shell_if_stale`, at the head of `shell_dispatch` and of
   a host-driven focus advance. *Refined*: what leaves the description stale
   is decided where an event is spent, not by the event. Every key does
   (its route is the editor's own pipeline); a fact does unless it is one of
   the pointer's transient ones (`UiFact::is_pointer_transient` — a hover, a
   wheel, a grip's press and drag, a scrollbar's thumb), and a hover reaction
   that changed state marks the description itself; the legacy pointer walk,
   which cannot say what it changed, marks it for every press and release it
   takes and never for a motion report. A drag along a divider therefore
   costs no layout (`geometry_pass::a_divider_drag_that_moves_nothing_lays_out_nothing`).
5. **One ring.** `handle_widget_focus_advance` (`:1216`) and its spec walk
   (`focus_ring_scoped_in_spec`) are deleted. A `FocusAdvance` on a panel the
   tree is focused in is `Ui::move_focus`; on one it is not, it is
   `next_in(interior, fact, dir)` (L4) written to the fact. Same order, one
   source.
6. **Every panel is on the ring, including pane-mounted ones.** *Landed for
   the ring*: a pane panel's wrappers are no longer `skip_traversal`, its
   interior is keyed (`interior_key(Slot::Pane(leaf))`) and the host's
   advance walks it through `Ui::next_in` like every other panel's. What
   keeps its keys the host's meanwhile is the interior *observing* every key
   (L2): the tree decides nothing for a key that reaches a pane widget,
   because the plugin's `defineMode` binds them outside the tree. *Landed
   in full*: the observer is gone; the pane's panel is the same
   `panel::interior` the dock's is, with its buffer's mode as the keymap on
   its capture leg (L3), and the active pane's panel has a `Modality::Focus`
   layer (`Frame::pane_keys`) declared first among the keyboard layers, so
   every other one outranks it by declaration order. Tab the mode does not
   bind is the tree's, confined to the panel; every other unbound key is
   `PanelKey(Pane)` and goes to the buffer's route. The layer is keyed as
   the base's (`panel::is_base_layer`) so the overlay gates — the PTY gate,
   bracketed paste — do not read it as something layered over the content.
7. **The settings dialog is on the ring.** *Landed.* Its layer names the
   box as its scope (`scope_at`); the stops inside it are the category tree
   (or the narrow strip), each **card**, and the footer's buttons, each a
   keyed focusable the description marks `autofocus` when the dialog's focus
   fact names it; the `keys` seam is off the ring and declines Tab, so
   `fresh-ui` walks the stops in reading order — entering the body at its
   selected card (L14) — and each landing comes back as
   `UiFact::SettingsFocus`. The fact is `SettingsState::{focus_panel,
   selected_item, footer_button_index}` with one writer,
   `SettingsState::focus_on`, reached by the keys, the mouse, the search
   jump and the tree's landing alike; `FocusManager<T>`, `settings_state.
   focus`, `toggle_focus` and the dispatcher's Tab arms are deleted.

   Two decisions the build made. **The card is the stop, not the control in
   it** (`Slot::widgets_on_ring`): under the edit-mode model `view/settings`
   still keeps, the control becomes live by the host's decision (Enter) and
   paints its caret only then, so a control on the ring would either show a
   caret it cannot honour or double the stops; a live control claims its own
   keys, Tab included, and Tab there commits the edit and stays on the card.
   §3.6 makes the control the stop when that model goes, and the entry
   dialog's ring — its rows, per-field affordances and composite sub-rows,
   all of them the host's model today — goes with it (the `SettingsEntry`
   slot is off the ring for the same reason). **A search is not stepped out
   of with Tab**: the query row is the one stop while it runs and hands Tab
   to the dispatcher with every other key, as it always did.
8. **The keyboard tables go.** With L3 the router's mode-binding check,
   `KeyContext` as a computed enum (`input/keybindings.rs:232` stays as the
   *vocabulary* of contexts; `get_key_context`'s ladder goes), `layer_rank`
   (`app/chrome/mod.rs:52`), `LayerKind` and `overlay_stack` (`app/overlay.rs`)
   are all derivable: precedence is layer declaration order, "does a modal
   block terminal input" is `Ui::raw_input()` (exists, `schedule.rs:730`),
   and the base key dispatcher is reached only through the buffer host's own
   key handler. *Landed for the context and the gates*: every surface with
   a key section puts a key on the node that holds focus while it has the
   keyboard (a modal's seam, a popup's seam, the prompt's sink, a panel's
   interior or sink, the settings box), `frame::key_context_of` is the one
   table from those keys to a `KeyContext`, and `Editor::get_key_context`
   walks the focus chain outward and takes the first answer — over a tree
   laid out from the facts, since `handle_key`/`handle_mouse` lay out before
   routing and any applied message marks the description stale. The PTY
   gate is "focus is in a layer, or a popup or the centred panel is up";
   "the content holds the keyboard" is "focus is in no layer"; "a modal
   covers the content" is `Ui::modal_up` (a layer that swallows keys or
   blocks the pointer); the unfocused-popup guard is `Ui::keyboard_owned`.
   `Layer`, `LayerKind`, `layer_rank`, `overlay_layers` and every
   `ChromeComponent::layers` are deleted; `app/chrome/` keeps the two hover
   reactions and the `Editor` methods the facts land in until §3.1 moves
   them. The PTY gate on `raw_input()` proper waits for the terminal to be a
   host leaf (S7); the base dispatcher's Normal-context resolution waits for
   the buffer's (S7).
9. **The dock's key policy is the plugin's.** `router.rs:222`'s dock branch
   hardcodes one plugin's widget-key conventions. Two generic pieces replace
   it: a **focus-trapped container answers its own navigation** (the kinds
   already declare `focus_trap`, `arrows_advance_focus`, `activate_event`,
   `widgets/kinds/mod.rs:62`, `:327–366`: inside a trap, arrows advance
   within it, Enter is the focused widget's activation, Esc fires `cancel` at
   the trap and returns focus to its opener); and **the panel's mode is a
   shortcut table on its scope** (L3), so `/`, Esc, F2, the Alt chords and
   Enter-on-the-list are `defineMode` bindings the orchestrator declares.
   The orchestrator's three dropdowns become `focus_trap` containers of the
   `Button`s they already are; its `dock_menu_*` handlers
   (`plugins/orchestrator.ts:11769`) go. `is_left_dock` and `is_sidebar`
   collapse into `non_modal`, then the router's dock branch has nothing left.
   *Landed*, with one decision the build made: the dock's three dropdowns
   are `list` widgets rather than focus-trapped `Button` containers — a
   list's ↑/↓ and Enter are its own kind's and reach the plugin as `select`
   and `activate`, which is the trap's navigation without a trap. The dock
   mounts with `mode: DOCK_MODE` (a new `mountFloatingWidget` option; the
   panel's own keymap, so the buffer's mode neither shadows nor is shadowed
   by it) and `DOCK_MODE_BINDINGS` names the chords: `/`, Esc, Enter,
   Space, F2, Menu, Alt+T/I/P/N, each a plugin command that decides from
   the plugin's own focus mirror. The router's dock branch, `DockEvent`,
   `FocusWidget`, `fire_dock_widget_event` and the `dock_*` events are
   deleted; `WidgetPanelView` is `non_modal` and two fields. Two things
   the build found. The plugin's mirror of its own focus (`pickerFocusKey`)
   is written when the plugin *decides* (`focusDockControl`), not only when the
   host's `focus` event confirms it a round-trip later — two keys in a row
   read the same mirror. And a blur is a focus write like any other: it
   marks the tree stale and the description stops marking the panel's
   widgets, so the tree's focus — and the key context read off it — leaves
   the dock on the same key (L1's root-scope and release rules).

### 2.4 Tests that pin the contract

In `app/widget_runtime.rs`, before any of it changes: the two existing
tests; the tree's own Tab is not undone by the next frame's stale mark; a
decision on an unfocused panel moves nothing until the panel is entered and
then lands; `autoFocusFirst:false` with an empty key is not re-seeded by the
tree's entry landing (it *was* — the tree landed on the first control and
the echo wrote it back, the welcome-screen bug returned by the back door;
the interior now marks itself when the panel names nothing, and the tree
rests there); two decisions in one batch resolve the second from the
first. And in every `Modality::Focus` seam, `.claimed` asserted on both
branches — the shape that would have caught the three "who took this key"
defects.

---

## 3. The editor, surface by surface

### 3.1 The frame is a pure function

`view/shell/frame.rs::Frame` is built once per frame from editor state and
nothing else. The `Editor` keeps the `Ui`, the `Persisted` store and its
model; it keeps no `shell_*` scratch — `shell_frame_status_bar`,
`shell_hover`, `shell_pointer_event` (three readers left) and
`shell_key_event` go, each replaced by the fact travelling with the event or
being read from the tree. `app/chrome/` — thirteen modules whose trait is
down to `layers()` and two hover reactions — is deleted; the layer
declarations move into the frame builder beside the surface that owns them,
which is where the code already says they belong. `Ui::pending_messages`
is drained at the head of every dispatch (done, `shell_host.rs:1579`).

### 3.2 Pointer

Every press, move, wheel and drag is the tree's. `PointerGrab`
(`app/chrome/mod.rs:75`, five variants: widget text, widget scrollbar,
terminal-select-pending, text selection, tab drag) becomes `capture_pointer`
on the gesture that starts it — the tab strip's tab (§3.7), the scrollbar
thumb, a described text field. Text selection *inside the buffer host* is
the host's own, delivered through `HostLeaf::hit`; text selection inside a
described popup (`PopupTextSelection`) is the host's too, over a region the
tree names with `Draw::Selectable` — a decision, not a leftover.
`is_mouse_over_transient_popup` (`app/mouse_input.rs:797`) and
`ChromeLayout::popup_areas`/`global_popup_areas` — a cache of the tree's own
answer — go with L6. `handle_widget_text_selection_drag`
(`app/widget_runtime.rs:2892`) computes in the painter's coordinate space and
goes with `layout_box.rs` (§3.5).

### 3.3 Paint and theme

**One fold.** The two-band fold (`view/shell/fold.rs:92`) exists because
legacy painters run between the tree's in-flow content and its layers. When
the only painters left are the per-pane text host, the terminal grid and
window embeds, `HostPainter` paints them inline and the fold runs once per
frame; `Band`, `Paints::HostsOnly`, `suppress_chrome_cells` and the ordering
defect that `render_panels_and_modals` paints after the caret commits all go.
Frame-buffer animations are an effect the backend applies to the final cell
buffer after the fold, declared as such; they are not tree state.

**One caret.** `LayoutSpec.cursor` is the only cursor source; the pane host
reports its caret through the leaf; the described caret is a zero-width
marker the fold reads through `cell_of`. The recorded disagreement about a
Background-band caret is settled by a test that asserts the caret *reaches*
`LayoutSpec.cursor`, not by prose.

**Total provenance.** Every cell's theme key comes from the fold's
`ProvenanceSink` (done for described surfaces). `Paint::Lit` — the one
colour with no theme name — retires into a dynamic tier plugins register
named keys into, and the status bar's literal separator gets a key; then
Ctrl+Right-click never says "no theme key recorded here". The theme-name
grammar (`fg_key/bg_key+attrs`, `shell_host.rs:594`) stands. `ThemeKey`
stays a string at the description; the fold's resolve gets a cache keyed by
the interned key, and whether that becomes a `ThemeId(u32)` is decided by
the frame benchmark (§3.10), not before it exists.

**Styles are asserted.** Shell tests fold through a distinguishable palette,
not `Style::default()` (`fold.rs:586` and five more sites).

### 3.4 Chrome: menus, popups, prompt, status bar, sidebar, dock

Status bar, search options, sidebar (with sections, dividers, accordion and
persistence — shipped), dock, floating panels, context menus, popups: described,
and the tree measures them. What is left:

- **Menu dropdowns are content.** *Landed (S6)*: `DropdownLevel { from,
  rows }` says what a level hangs off and what its rows read
  (`RowBody::Item { text, trail }`, `Separator`); `menu::describe` derives
  the bar and the chain from the menu state, and the tree places them —
  `Anchor::Node` on the bar label with `Place::Below` and `Fit::CLAMP`, a
  submenu `Place::RightOf` its parent row with the one-row rise as an anchor
  offset and `Fit::FLIP` to the left at the right edge, every level `within`
  the frame below the bar. A row is its text, a flex spacer and its trail, so
  an accelerator sits against the border; the box is as wide as its widest
  row, measured from the rows' text (a stack hands flex the whole loose room
  under an intrinsic measure, so `Sizing::Auto` there would take the frame —
  the library rule L15 would let it say `Auto`). `MenuRenderer`,
  `MenuLayout`, `fit_dropdown_area`, `calculate_dropdown_width`,
  `items_to_show`, `menu_layout_now` and `apply_menu_theme_runs` are deleted;
  the fold records the menu's provenance like every other item's, and the
  web's `menu_view` reads label and row rectangles off the tree. The flip
  rule is `Fit::FLIP`'s, pinned by `a_submenu_flips_left_at_the_right_edge`.
- **The prompt line is the last host region among the chrome**
  (`frame.rs:30`, `PromptLine`). Its input row is a `TextField` over the
  editor's one `TextEdit` engine, which deletes `text_click.rs`. *Landed*:
  its overlay toolbar is described through the same adapter as every
  panel. The toolbar is the plugin's panel `PROMPT_TOOLBAR_PANEL_ID` in the
  registry (`Slot::PromptToolbar`), described in the card's header band as
  tall as its controls lay out; the card is the prompt keyboard layer's
  scope (`prompt::keys_layer` → `scope_at(card_key())`), with the input row
  as the focus holder the layer's sink used to be, so Tab walks input →
  controls → input on the tree's one ring. A control's landing is
  `WidgetFocus`, the input's is `CardInputFocus`, and the panel's
  `focus_key` is the one fact; the toolbar's capture-leg rule (arrows,
  paging, typing are the query input's) is on its interior node
  (`panel::interior_capturing`). The third focus ring is gone:
  `overlay_toolbar_keys`, `cycle_overlay_focus`, `handle_overlay_toolbar_key`,
  `prompt_toolbar_boxes`, `Prompt.toolbar_focus`, `Prompt.toolbar_widget`,
  `CardToolbarPress` and `render_spec_no_autofocus` are deleted;
  `toggleOverlayToolbarWidget` runs the control's kind against the panel.
- **The editor scrollbar column.** A popup clamps to the frame, not to the
  frame minus the split's scrollbar; that inset is stated by the chrome
  column node once the split grid is a region with its own rectangle (§3.7).

### 3.5 Plugin panels: one authority, one projection that answers

`WidgetSpec` is the frozen wire format. `kinds::WidgetImpl` is the one
behaviour authority: capabilities, `on_key`/`on_pointer`/`on_wheel`/
`on_focus_change`, `resolve(spec, prev) → model state`, `describe(spec, ctx)
→ Node`, and `events(spec, state) → semantic hit table`. The **node
projection is the only one that answers the pointer, the keyboard and the
geometry.** The text formatters survive as `project_text`, producing the
text mirror a panel's buffer holds for search, copy and `lines_changed` —
derived from the same spec, never consulted about anything but the bytes.

What that deletes from `WidgetPanelState`: `hits` (identity derived by
`events()`, geometry the mirror's own), `tabbable` (the tree's order, L4),
`boxes` and `layout_box.rs` (the tree's rectangles; the wheel is the
viewport's scroll chain; the kinds stop building `LayoutBox`es for popups
and completions), `painted`/`effective_rows` (delivered as a `Viewport` on
the key event from the node that dispatched it), the scroll folds that are
the viewport's. What stays: `spec` (as `Rc<WidgetSpec>`, swapped by IPC),
the model half of `instance_states` (values, selections, expansions,
completions — the promise that a re-emitted spec loses nothing), the focus
fact, `auto_focus_first`. The collector (`widgets/render.rs`, 8.8k lines)
shrinks to the formatters.

**The panel class that rides the buffer's scroll is retired.** `git_log`
today builds its selection on the buffer cursor moving through mirror lines,
and that is the one reason a `WidgetSpec → TextPropertyEntry` projection
still has authority anywhere. It was never a documented contract — it is an
implementation leak a bundled plugin read. The end state is that every
mounted panel is described, the mirror is derived, `git_log` selects through
the `List`'s own selection model with `reveal`, and `mouse_click` buffer
coordinates are not a panel API. `hit_test_row_aware`, `row_select_hit`,
`click_handlers.rs`'s byte-range scan and `pane_panel_owns_its_scroll` go.
*Landed in part*: `git_log`'s log and the package manager's list are
`scrollable: false` panes whose `List` windows itself and reports its
selection (`select`), the cursor-driven mirror and its `cursor_moved`
subscription are gone, and **a pane's panel answers its keys through the
same widget router the dock's does** (`Editor::dispatch_pane_panel_key`,
from the `PanelKey(Pane)` applier; what its widgets decline is
`FallThrough`, the buffer's own route). Three rules that came with it: the
router names a caret key with its modifiers (`S-Right`, `C-S-Left`) so a
field's selection and word motion are the kind's on every surface; a
focused text field takes a printable key ahead of the panel's mode
bindings, on the keymap node (`panel::Keymap::text_focused`) rather than
in a host stage; and a described interior is its layer's scope whether or
not it holds a Tab stop (an interior with none holds focus itself), so the
key sink exists only for a panel the adapter cannot describe. The mode
stage's own text-widget branches — its selection moves, clipboard
forwarding and focused-field gate — are deleted as unreachable. **A panel that is a page** — the welcome screen: a document longer than
its pane, scrolling as a whole — declares `WidgetPanelOptions::page`, and
the description puts its content in one viewport the tree owns
(`splits::panel_content`, `fresh_ui::viewport` with a scrollbar), with a
`fresh_ui::behavior::Anchor` as the host's handle on that window
(`Editor::page_anchors`). Its lists take their natural height; the wheel
and the bar move the window; the arrow, page, Home and End keys scroll it
when no widget takes them (`handle_widget_key`); and `scrollToWidget` is
`Anchor::top_key` / `reveal_key` on the widget's node — every keyed widget
carries `widgets::widget_node_key` in the tree, focusable or not. The
library grew `Anchor::scroll_by`, `scroll_by_pages` and `scroll_to_end`
for it. The mirror buffer under a page neither scrolls nor shows a caret.
*Landed, the deletions*: with no pane-mounted panel left outside the
tree, `pane_panel_owns_its_scroll` is gone and a pane's panel is described
whenever one is mounted; `WidgetPanelState::hits` with `hit_test`,
`hit_test_row_aware`, `row_select_hit` and `row_of_widget`,
`click_handlers.rs`'s byte-range scan, the hover probe over the projection
(`update_mounted_widget_hover`), the wheel-by-boxes path
(`handle_widget_panel_wheel_at` and both routes into it, with the
`DockScroll` fact), the split-mounted scrollbar pass and its tracks
(`render_split_widget_panel_scrollbars`, `WidgetScrollbarTrack`,
`PointerGrab::WidgetScrollbar`) and the registry's host-driven list scroll
are deleted. A panel's wheel is its viewports' by the library's chain, its
bar captures the pointer itself, and a press on a widget is the node's.

**Kinds keep their key handlers, reached from the node.** A described
widget's `on_key` raises `UiFact::WidgetKey { slot, widget, key, viewport }`
and the applier runs `behavior(kind).on_key`; the three dispositions (L2) map
the kind's `KeyDisposition` one-to-one. One implementation of "what Enter
means on a dropdown" for the TUI, the web and the plugin's smart-key path.

**The markdown document view** crosses on L5; **the anchored panel's width**
becomes `Sizing::Auto` once `node_body`'s width parameter is gone
(`rule()` exists; `Divider` uses it; the compiler names the rest) and
`content_cols`, `anchored_width`, `Interior::avail_height` go with it.

### 3.6 Settings and the modals

The settings dialog's body is already the adapter's; its keyboard is §2.3(7).
What remains is the *model*: `view/controls/` (6,271 lines of `*State`
structs and seven `input.rs` editing engines) is deleted, `SettingControl`
wraps model state kept the way a plugin panel keeps it (a keyed
`WidgetInstanceState` store — `settings_state.widget_states` exists), input
routes through `handle_widget_key`, and the entry-edit dialog stack
(`EntryDialogState`) becomes a stack of floating layers over `WidgetSpec`
with its own store. `SettingsLayout`'s painter is already down to a box and a
divider; the box goes with the fold. The keybinding editor, calibration
wizard and workspace-trust dialog follow the same shape. The file-open
dialog is the last surface whose interior is a painter recording cell spans
for hit tests to read back (`FileBrowserRenderer`, `Window::
file_browser_layout`); it is described like the rest, and its layer keeps
the pointer claim it has (L10).

*Landed.* `view/controls/` is gone. Every `SettingControl` variant is a
model value — what the JSON carries, and a label — and each settings
surface (the page, each level of the entry-dialog stack) keeps one
`WidgetPanelState::surface`: the store the widget kinds read and write,
whose `focus_key` names the control that is *live*, or one of its rows. A
key or a press on a live control goes to its kind (`view::settings::live`
— the same `on_key`, `on_text`, `on_pointer` a plugin panel's widgets
answer) and the events the kind reports are written to the model, where a
plugin would receive a `widget_event`. The kinds grew what the settings'
own engines had and they lacked: `Number` has an in-place draft (typed
digits with a caret and a selection, Enter commits, Tab commits before it
advances, Escape and a blur abandon it — instance state, so the plugin
sees only the `change`), `Dropdown` restores the selection it opened on
when Escape closes its list, and every kind takes typed text through
`on_text`. The spec's `edit_text` fields are gone with the engines.

The dual list is `SettingControl::DualList { options, included, excluded }`
— the included set is the model's, applied from the kind's `change` as it
moves, and the sibling's `excluded` follows it; which column the keyboard
drives and where its cursors sit are `kinds::dual_list`'s instance state,
painted only while the control is live. The kind grew the Shift chords the
settings had (`S-Up`/`S-Down` reorder, `S-Left`/`S-Right` *carry* the item
and follow it into its new column — `DualOp::Carry`), its ops are public,
and the page's Enter on a live dual list is the kind's Space. The JSON
editor is `SettingControl::Json { text }` under the multi-line `Text` kind:
the text is the model's as it is typed (its validity is read off it — a
warning row under the field, the dialog's legend), Enter is the kind's
newline, and Tab or Escape leave it keeping a text that parses and putting
back one that does not; an unset value opens as an empty field with a hint
in it and reads back as `null`. The node the kind is handed is found *by
key* in the control's description (`widget_map::live_widget`), so a
control whose description is a `Col` — a label row over a text area —
dispatches to the keyed node inside it. `view/controls/dual_list` and
`JsonEditState` are deleted.

The row-based composites are the same widgets a plugin's form is made of.
A map (`SettingControl::Map { entries, value_schema, display_field,
no_add }`) and an object array (`ObjectArray { items, item_schema,
display_field }`) are one `List` keyed by the path, an entry per row and
the `[+] Add new` row last; its selection is the surface's *cursor*, kept
in the store by the `List` kind, live while the card is selected — the
arrows are the kind's `select_move`, and an arrow at either end of the
rows (the kind moved nothing) is the page's, which moves on to the next
card; Enter is the kind's `activate`, which the surface answers by opening
the row's entry dialog (`composite_activate`). The description is told
which row the cursor is on (`setting_control_to_widget_aligned`'s
`cursor`) so the row can say `[Enter to edit]` and the add row can be the
one the list selects. A text list (`TextList { items, integer }`) is its
rows as *fields*: one `Text` keyed `{path}::row::{i}` per item with a
`[x]` `Button` keyed `{path}::remove::{i}` beside it, and the add row's
field keyed `{path}::add` last, labelled `[+] Add new` — all edited by the
`Text` kind, the live one painting its own caret. A row's `change` is
applied to the model as it is typed; the add row's draft is the field's
until Enter (or Tab, or leaving the field) makes it an item
(`live::text_list::take_draft`); Up and Down open the adjacent row's
field — on the page they stop at either end, and in the dialog the form
enters the list at its first item's field from above (its add row's from
below) and leaves it past the add row; Delete removes the item the field
is in. The node a kind is
handed is found by key in the control's description
(`widget_map::live_widget`), so a row's field dispatches like any
control. The page's and the dialog's `sub_focus`, `editing_text`,
`update_focus_states`, `FocusState` and every per-row editing method are
deleted, as is `UiFact::WidgetHit::at`, the press column the old text
list's `[x]` was found by: the `[x]` is a button, and its press names it.

The file-open dialog is described (`shell::file_browser`): a `Browser`
value built from `FileOpenState` — the directory, the two toggles with
their localized labels and live shortcuts, the navigation shortcuts, the
sort, the entries with their columns formatted, and which of them the
keyboard is on — laid out as a bordered box captioned with the directory
(`truncate_path` against the width the tree gives the strip), two
navigation rows, a header row and a `List` of entry rows with the bar's
column reserved. Every control is a keyed node that answers its own press
(`UiFact::BrowserToggle`, `BrowserShortcut`, `BrowserSort`,
`BrowserSelect`, `BrowserActivate`; `BrowserNavigation` for a press on the
band beside the shortcuts) and reports its own hover
(`HoverTarget::FileBrowser`); the rows' hover and the bar are the list's.
The list's window is the viewport's: `FileOpenState::scroll_offset`,
`last_visible_rows` and the clamp the renderer had to feed are deleted, a
controlled selection is revealed when it moves, and a new directory is a
new list (the list sits in a scope keyed by the directory) that starts at
the top. The web reads the rectangles and the window back by key
(`file_browser::rects`, `window`) and keeps its card. `FileBrowserRenderer`,
`FileBrowserLayout`, `Window::file_browser_layout`, the three coordinate
facts, the four hit tests in `file_open_input.rs` and the six
`HoverTarget::FileBrowser*` variants are deleted.

The modals are the tree's, box and channel alike. The settings dialog's
layer draws its own rounded ring, ground, caption and the divider between
its columns (`render_settings` is deleted), is `Modality::Exclusive` with
its own `Scrim::Dim`, and a press on its slack stops at the box. The
floating plugin panel's layer claims its own pointer (`Modality::Pointer`)
and dismisses an anchored popup on an outside press through the layer's
`Dismiss` (`UiFact::PanelClosed`), which was the one arm of the modal
pointer handler that did anything. With that the pointer half of the modal
seam is gone: `modal::Slot`, `modal::layer`, `UiFact::ModalPointer`,
`Frame::modal`, `Editor::modal_slot`, `handle_settings_mouse`,
`handle_floating_modal_mouse`, `FloatingWidgetPanel::last_inner_rect`,
`SettingsState::{scroll_up, scroll_down, scroll_to_ratio, hover_position}`
and the `KeybindingEditorLayout` husk are deleted; `UiFact::ModalKey` and
`modal::keys` remain the keyboard's seam. Two library rules landed with it:
a pointer event that a pointer-blocking layer let nothing answer is
*claimed* — by the layer — unless a press dismissed it (the tree's word is
the claim, so a host pipeline behind the tree stops where the modal says
it must), and the fold applies `Scrim::Dim` as the painters' own dim
(`dimming::dim_buffer`) rather than restyling the frame in the palette's
ground, which is what every described modal's scrim had been doing.

### 3.7 The body: the text buffer as a `Host` node

The text buffer is the reason this editor exists and the one thing on
screen that a description tree must not try to own: a multi-gigabyte file
is a persistent piece tree with lazy loading, a wrap index that repairs on
edit instead of invalidating, a highlighter that converges from checkpoints,
and a renderer that materialises only the visible window. None of that is
tree-shaped, and describing it as rows would hand all three back. So the
buffer is a **`Host` node** — and a *designed* one, in the library's own
sense: an ordinary render object with exactly the capabilities a built-in
primitive has and no others (`crates/fresh-ui/src/render/object.rs:323`),
which takes its rectangle from layout, its position from paint order, and
records nothing.

Today a pane is the weaker of the two forms the library offers:
`HostSpec::Plain` (`desc.rs:885`), a `PlainHost` that answers layout with
"whatever I am given" and paints one `Draw::Host(id)` item that the fold's
`HostPainter` (`app/shell_host.rs:475`) resolves back to the pane and paints
through the text pipeline. It answers nothing else: no hit, no byte under a
cell, no focus registration, no scroll facts. Everything the tree cannot ask
it is answered beside it — `view_line_mappings` and the click-to-byte scan
in the editor's mouse path, the caret handed to the painter by `&mut`, the
scrollbar thumb from `WindowLayoutCache`. `view/shell/content.rs` is the
other idea, written and never called: the pane's rows as `text_runs`
supplied to a `layout_reader`. **That is not the design and is deleted.**
Its own invariant — a pane's item count is a function of its rows — is
right, and the host form keeps it with no per-row items at all.

#### 3.7.1 The node

`pane(leaf)` in `view/shell/splits.rs` builds `host_leaf(factory)` with a
factory that is one `Rc` per pane for the life of the pane (the
`HostSpec::Leaf` equality is `Rc::ptr_eq`, `desc.rs:904`; a fresh closure
per frame would relayout every pane every frame). The object it makes is
`BufferHost`, a `HostLeaf` keyed by the pane's `LeafId`, and it is the
**only** thing in the tree that knows there is a document. Its props —
buffer id, scroll position, wrap width policy, gutter width, active-ness —
are pushed into the live object through `as_any_mut` each frame, never by
replacing it, so what it retains (the last rect, the last row window, its
own caret) survives the rebuild exactly as a viewport's offset does.

#### 3.7.2 Layout

`layout` returns the constraints' maximum: a pane is as big as the grid
gives it and has no intrinsic size, because a document has no natural
height. `relayout_boundary` is true — nothing below a pane can change its
size, and nothing inside it is measured by the tree. The wrap width the
pipeline uses is the rect's width minus the gutter it declares, computed
once here and nowhere else; the pipeline's `compute_content_layout` takes
that rect and does not derive one. Layout of a pane is O(1) and does not
touch the document.

#### 3.7.3 Paint

`paint` pushes one `Draw::Host(id)` and sets the display list's cursor
(`LayoutSpec.cursor`) from the caret the last content pass produced. The
fold's `HostPainter` resolves the id to the pane and calls the text
pipeline's `render_content` into the cell buffer for exactly that rect. So
the display list carries **one item per pane**, the pipeline writes cells
directly, and the caret has one source. The pipeline paints with theme keys
through the same `ProvenanceSink` the fold uses, so the inspector answers
over a pane's cells as it does over chrome. The per-cell patches the
pipeline applies after the fact (selection, current line, matching bracket,
the four the old plans list) become run styles that inherit the background
(L12), and the pipeline stops rewriting cells the fold has written.

#### 3.7.4 Pointer

`hit` claims the whole rect; `text_byte_at(local)` answers the byte under a
cell from the pipeline's own row-to-byte map — `view_line_mappings` stays,
and it moves *into* the host object as the one place the map lives. A press
is a gesture on the pane node like any other: the leaf's press handler
raises the editor's place-caret message with the byte the event carries,
selection drag is `capture_pointer` on that gesture, and the wheel is a
message the leaf raises with the notch and its own row window. No mouse
path in the editor scans rectangles to find a pane, and `handle_editor_click`
receives a byte, not a cell.

#### 3.7.5 Keyboard and focus

The leaf carries `focus_reg`: a pane is focusable, and the active pane is
the focused element when no chrome scope is up. Its shortcuts (L3) are the
buffer's mode — the keybinding resolver projected for that pane's mode and
context — so a key reaching the leaf resolves to an editor action on the
focus chain, and the base key dispatcher becomes the leaf's own handler
rather than a fallback the tree declines into. `takes_raw_input` is false:
the buffer is not a PTY. `Modality::Focus` chrome above it (dock, prompt,
sidebar) confines traversal and lets unbound keys fall to this leaf, which
is the property `Focus` exists for.

#### 3.7.6 Scroll

The editor owns the pane's scroll, as a rule (§8). The leaf does not use a
`Viewport`; it publishes the facts a scrollbar needs — offset, content
rows, window rows, in one unit — through the same scroll-facts read a
viewport answers (**verify**: `RenderObject` exposes `ScrollInfo` for a
viewport today; the leaf implements the same method), so the pane's
scrollbar is an ordinary `Draw::Scrollbar` node beside the leaf, with L8's
markers for the overview ruler, whose press and drag raise scroll messages
the editor applies. `Ui::window(key)` answers for a pane exactly as for a
list.

#### 3.7.7 The invariants, asserted

- A pane's display-list item count is a function of its on-screen rows,
  never of its document length (one item, plus its chrome).
- Layout of a pane touches no document byte; only `paint` materialises the
  window, and only the visible one.
- Edits repair; nothing in the tree invalidates the wrap index or the
  highlighter.
- A pane's props are pushed into a live object; the object is never
  replaced across frames while the pane exists (asserted by counting host
  leaf constructions per frame).
- The pane's rect, wrap width, caret and byte-under-cell each have one
  source, and it is the leaf.

#### 3.7.8 The same node, for terminals and embeds

The terminal grid and a window embed are the same kind of node with
different answers: the PTY leaf says `takes_raw_input` so a modal above it
suppresses raw input by the tree's own `raw_input()` test (`schedule.rs:730`),
and its scroll-back is its own scroll facts; an embed is a leaf whose paint
delegates to the embedded window's own pane leaves. `HostRegion`,
`HostTarget` and the seven-slot enumeration go: the fold resolves a
`Draw::Host(id)` to a leaf by id, and there is one painter callback.

#### 3.7.9 The chrome around it

Everything around the content is nodes: the split grid's rectangles and
dividers (done, `view/shell/geometry.rs`), **the tab strip**
(`view/ui/tabs.rs`, the last painter-recorded-rectangle hit test: tabs,
close buttons, `+`, scroll arrows, tab drag as a captured gesture), the
scrollbar (§3.7.6), the gutter (line numbers, folds, diagnostics as runs
beside the leaf, with L12's inherit-background). Then `WindowLayoutCache`
is deleted outright: `view_line_mappings` lives in the leaf, `split_areas`,
`tab_layouts` and the thumb extents are `rect_of` reads (L6). The
provenance gate (`cells_provenance`: which cells were fold-written, which
painter-written) is the test that says the only painter-written cells left
are inside host leaves.

### 3.8 Plugin API

Frozen: `WidgetSpec`, `WidgetMutation`, `widget_event`, `WidgetAction`, and
the promise that instance state survives a re-emitted spec by key. Growth is
additive and optional: `WidgetPanelOptions.auto_focus_first` is the
precedent; `mode` (the panel's `defineMode` name, §2.3(9)) is the next.
`visibleRows` is optional and auto-sizes. Kinds are not plugin-extensible: an
open kind set costs an IPC round trip per keystroke and breaks the second
backend. `cursorByte` is the spelling that stands. `DocumentSymbols` to
plugins, sticky tree ancestors, tabs within a sidebar section are feature
asks recorded in §9, not this arc's.

### 3.9 The web

The web consumes **the display list**: a fold of `LayoutSpec` into DOM by
key, the way the terminal folds it into cells, with `Draw::Host` panes
remaining cell slices — the display list never replaces the cell channel for
the text. `view/scene.rs`'s per-region JSON projections retire surface by
surface as the DOM fold covers them, and plugin panels return this way — not
by restoring the deleted `synthesize_*_hit` functions but because a panel is
nodes like everything else. A click comes back as a cell and goes through
the same hit path; a text press comes back as `text_byte`. The
`webDockPanels` guard in `crates/fresh-editor/web-ui/test/drive.mjs:206`
comes out and the two dock sections run again. Every deletion in this
document is checked with `--all-features`, because the web is a second
caller.

*Landed, for the plugin panels.* `Editor::tree_view` (`view/scene.rs`)
ships `regions.tree`: the surfaces (dock column, floating layer, plugin
sidebar sections) and every display-list item those subtrees and their
layers produced, with the fold's resolved colours; `web-ui/js/72-tree.js`
folds them into DOM at their cell rectangles, and input needs nothing of its
own because the document-level handlers already map a pixel to a cell and
the server routes the cell over the tree. The old widget-panel renderer and
its hit routing are deleted, the suite's dock sections read the tree, and
the suite runs to its end again. Still native: the menu bar, status bar,
explorer, popups, palette, settings and the modals — each retires onto this
projection when its chrome crosses (§3.4, §3.6).

### 3.10 Performance

The frame is measured before it is optimised: a `crates/fresh-editor/benches`
frame-cost benchmark (retained and cold, idle dock, terminal tick) is the
standard every "too expensive to rebuild" claim is held to. Then:
`panel_interior` stops deep-cloning every panel's `WidgetSpec` per frame
(`app/render.rs:8352`; the registry holds an `Rc`); each panel's node is a
`memo` keyed on (`Rc::ptr_eq`, model-state version, focus fact, hover); the
menu walk and the status-bar content pass memoise on their inputs; the
palette resolve is cached; L6 replaces `find_by_key` on the motion path. The
exit is L11's counter asserted: a frame with no state change reconciles a
bounded number of elements, and a terminal tick with an idle dock does not
rebuild the dock.

### 3.11 Windows and persistence

One retained tree, N windows, no window named in it: every key is scoped so
window A's first pane and window B's first pane are different elements
(done), and no UI state is reachable from two windows at once. `Persisted`
is for new incidental view state only; anything `workspace.rs` serialises
stays on the editor, because elements are disposed on unmount and do not
survive a restart. If a stage changes `workspace.rs`, something was
misclassified.

---

## 4. Definition of done

**Deleted** (each a grep that returns nothing outside tests and history):
`focus_panel_widget_in_tree`, `pending_panel_tree_focus`,
`handle_widget_focus_advance`, `focus_ring_scoped_in_spec`,
`FocusManager`, `Prompt.toolbar_focus`, `overlay_toolbar_keys`,
`prompt_toolbar_boxes`, `layout_box.rs`, `text_click.rs`,
`WidgetPanelState::{hits, tabbable, boxes, painted}`,
`hit_test_row_aware`, `pane_panel_owns_its_scroll`,
`render_spec_no_autofocus`, `render_collected` inside `view/shell`,
`content_cols`, `anchored_width`, `router.rs`'s dock branch and every
widget-key string literal in it, `dock_menu_*` in the host, `KeyContext`
computed by ladder, `layer_rank`, `LayerKind`, `overlay_stack`,
`app/chrome/`, `PointerGrab`, `shell_pointer_event`, `shell_hover`,
`shell_frame_status_bar`, `is_mouse_over_transient_popup`, `popup_areas`,
`global_popup_areas`, `DropdownLevel::{x, y, width}`, `fit_dropdown_area`,
`menu_layout_now`, `view/ui/tabs.rs`'s hit test, `tab_layouts`,
`split_areas`, `Band`, `Paints::HostsOnly`, `suppress_chrome_cells`,
`Paint::Lit`, `view/controls/`, `EntryDialogState`,
`SettingsLayout`, `HostRegion`, `HostTarget`, `HostSpec::Plain` for a pane,
`view/shell/content.rs`, `WindowLayoutCache`, `view/scene.rs` region views
other than pane cells, and the `webDockPanels` guard.

**Asserted** (each a test that fails if the property is lost):
the fourth autofocus case's five library tests; the six focus-contract
tests of §2.4; `.claimed` at every `Modality::Focus` seam; input never
routed over a stale tree; a frame with no state change reconciles a bounded
number of elements; theme provenance is total (the inspector answers over
every surface); a pane's item count is a function of its rows; the caret reaches `LayoutSpec.cursor`; a pane is one display-list item and
no host leaf is constructed on a frame that mounts no pane; styles asserted
through a distinguishable palette; `cells_provenance` reports no painter-written cell outside the
hosts; the frame benchmark exists and is read in review; every deletion
builds with `--all-features` and with `--no-default-features --features
runtime --all-targets`.

**Not deleted, by decision:** the text pipeline behind the pane's host leaf,
the terminal grid, window embeds, `view_line_mappings` (inside the leaf),
the `TextEdit` engine,
`Modality::Focus`, the kinds' key handlers, the text mirror as a derived
projection, `WidgetSpec` and its mutations.

---

## 5. The road

Nine stages. Each has an observable exit; each leaves the tree working;
dependencies are stated so stages can proceed in parallel where they do
not touch. The first is the issue; the order after it is by what unlocks
the most deletion per step.

| stage | what | needs | unlocks |
|---|---|---|---|
| **S1 Focus** | L1; the six contract tests; delete the imperative write and the replay; narrow the writer; the stale-tree rule; L4 and one ring; pane panels on the ring; settings on the ring (§2.3 1–7) | — | S2 |
| **S2 Keys** | L2, L3; keymap projected onto the tree; dock policy into the plugin's mode + focus-trap navigation; router dock branch deleted; `KeyContext` ladder, `layer_rank`, `LayerKind`, `overlay_stack`, `app/chrome/` deleted; PTY gate on `raw_input()` (§2.3 8–9, §3.1) | S1 | S3, S6 |
| **S3 Panel authority** | `events()` and the hit split; registry fields deleted; prompt toolbar described; `layout_box.rs` and `text_click.rs` deleted; anchored `Auto`; `git_log` onto the list's selection and the buffer-riding class retired; kinds' keys from the node (§3.5, §3.4 prompt) | S2 (for `WidgetKey`) | S8, S9 |
| **S4 Markdown** | L5; the last in-build render; `Text::scroll` leaves instance state | — | — |
| **S5 Settings model** | `view/controls/` deleted; entry dialogs as layers over `WidgetSpec`; the file-open dialog described; the three modals (§3.6, L10) | S1 (keyboard) | — |
| **S6 Menus** | dropdown content model in the description; legacy menu layout and `menu_layout_now` deleted (§3.4) | S2 (shortcuts on menus) | S9 |
| **S7 The pane as a host leaf, and its chrome** | `BufferHost` as a `HostSpec::Leaf` with hit, byte, focus, scroll facts and caret (§3.7.1–3.7.8); `content.rs` deleted; tab strip, scrollbars, gutter as nodes; `PointerGrab` → captures; L6, L8, L12, L13; `WindowLayoutCache` deleted; one fold, one caret; provenance gate (§3.2, §3.3, §3.7) | S2 (shortcuts on the leaf) | S8 |
| **S8 Theme and performance** | `Paint::Lit` retired; resolve cache; benchmark; `Rc<WidgetSpec>`, memo at the seam; L7, L9, L11; the `ThemeKey` decision (§3.3, §3.10) | S3, S7 | — |
| **S9 Web** | DOM fold of the display list — *landed for the plugin panels*; the scene's remaining region views retire as each surface crosses (§3.9) | S3, S6 | — |

**Status.** S1 is landed on this branch: L1 with its library tests, L4,
the deletion of
`focus_panel_widget_in_tree` / `pending_panel_tree_focus` / the replay, of
the spec ring and of `WidgetPanelState::tabbable`, the stale-tree rule, the
narrowed writer (`WidgetRegistry::decide_focus`), the six contract tests in
`app/widget_runtime.rs`, pane panels on the ring with their keyboard
layer (§2.3(6)), and the settings dialog on the ring (§2.3(7)) with
`FocusManager` gone. **S2** is landed in its main part: the key context
and the four overlay gates are reads of the tree (§2.3(8)), the layer
stack is deleted, a plugin panel's mode is a keymap on its interior (L3),
the dock's policy is the plugin's (§2.3(9)), and the claim is the tree's
word alone (L2). Open in S2: the buffer host's own keymap on
the tree — today the base's keys still cross `PanelKey` / the pane's
content surface into `dispatch_base_key` — and the PTY gate on
`raw_input()` (§3.1). **S3** has its first slice landed: no panel rides
the buffer's scroll (§3.5) — a pane's panel answers its keys through the
widget router, `git_log` and the package manager select through the
List, and the welcome screen is a page in one host-owned viewport. The
deletions that slice unblocks are landed too: the byte-range scan, the
hover probe, the wheel-by-boxes path, the split scrollbar pass and
`WidgetPanelState::hits` are gone, and every mounted panel is the tree's.
The prompt toolbar is described (§3.4), the third focus ring with it.
**S6** is landed: the menu chain is a content model the tree places, and
the legacy layout walk is deleted (§3.4). **S5**'s controls are landed:
every settings control is a model value edited by its kind against each
surface's store — the scalars, the dual list, the JSON editor, and the
row-based composites as the `List` and `Text` kinds — and `view/controls/`
is deleted, the file-open dialog is described — the last painted
interior with a recorded-span hit test — so `FileBrowserRenderer` and
`Window::file_browser_layout` are gone, and the modals are the tree's box
and channel alike: `render_settings`, the modal pointer slot and
`UiFact::ModalPointer` are deleted (§3.6). **S5 is landed.**
Open in S3: the kinds' keys from the node, `painted`/`boxes` (the text
projection still runs on the plugin's mount and update, and for the two
exceptions `resolve_described_panel` names), `text_click.rs` and the
anchored panel's `Auto` width.

**S1 is one PR** and closes sinelaw/fresh#3176. **S2 is its own PR** and
the only one that changes what a key does over a shipped surface; it gets
the hand-driven checklist from #3163 (`↑`/`↓`/Enter/Esc in all three dock
dropdowns, `/` to the filter and Esc back, F2 on a row, `Alt+N`, `Ctrl+P`
over the dock still opening the palette) before and after. **S3 and S7** are
where the most code leaves. **S4** is the largest single library change and
nothing depends on it. **S9** is the one with a red test waiting on it.

Each stage's commit that deletes code also deletes the comments that argued
for it (property 5); §2, §3 and §4 name the ones this document itself makes
false — `apply_autofocus`'s "three cases", `on_the_ring`'s "no description
can express it", `widget_focus_key`'s reference to the deleted function,
`Modality::Focus`'s doc, the router's "ACKNOWLEDGED RESIDUE", the
`WidgetFocus` applier's "it has one writer", and the `focus_key` field doc.

---

## 6. Risks, and what to drive by hand

- **S1's stale-tree rule** costs a layout on the rare dispatch with a write
  between two keys, and none on a motion report. If the frame builder is not
  pure, that is found first and fixed first.
- **S2** changes every key over the dock. Land the plugin change and the
  host change in one commit; drive the checklist; assert `.claimed`.
- **S3's `git_log` migration** changes a bundled plugin's selection model;
  drive commit selection, `j`/`k`, click, and search in the log by hand.
- **S6's placement** has one known eleven-cell divergence; decide it in a
  test before the description is written.
- **S7's tab drag** is the last `PointerGrab`; drive drag between panes and
  across the dock edge.
- **S9** is where a wrong payload is a silently wrong plugin event; the
  parity test is "for every panel shape, the DOM fold's hit identity equals
  the TUI's".
- Two divergences from the painters were taken deliberately and stand: a
  popup is bounded by the window, not the chrome column; the bottom-anchored
  suggestion list scrolls only when the pointer is over it.

---

## 7. How to work on this

- **A library change needs a caller in the same PR and a test that fails
  without it.** "Would another consumer want this?" is not a reason; it is
  what admitted six unused variants. The inverse failure — the library is
  right and the caller is wrong — gets the same test.
- **A wave is done when the tree measures the surface.** Cell-identical
  output and pointer parity are necessary and not sufficient; a description
  with a rect, a width or a pre-fitted string is still a picture the old
  renderer drew.
- **If the answer to a review item is "no", the artifact is a test.**
- **Assert the tree's focus, not the registry's.** Every focus failure this
  arc has had came with a registry that agreed with itself.
- **Send two events before rendering** when the property is about ordering.
- **Drive the UI by hand** with an isolated `HOME`/`XDG_*` and read the
  screen with escape sequences; compare against `master`, not against
  intent.
- **Never test a windowed list with one-cell items only.**
- **Check `--no-default-features --features runtime --all-targets` and
  `--all-features`** before every push; one push per CI run.
- **Delete the comment with the code.** Reviews found load-bearing claims
  the code contradicted at a rate that was not improving.
- **Cell-identical where a surface is a port; say so where it is not.**

---

## 8. Decisions that stand and are not re-argued

- The editor owns the text pane's scroll; edits repair, they do not
  invalidate; only the visible window is materialised.
- `Host` is a design choice, not a migration seam: a designed host takes its
  rectangle from layout and its position from paint order and records
  nothing. The pane's text, the terminal grid and window embeds stay hosts.
- `Modality::Focus` and `Modality::Pointer` are permanent: each is one
  channel's claim for a surface whose other channel is elsewhere (L10).
- The kinds' key handlers are host-side; kinds are not plugin-extensible.
- Precedence is layer declaration order; paint order and keyboard order are
  independent by design; a layer names its focus scope when the two differ.
- The layer hit-test rule: the first layer with any path at the point wins,
  so a layer says what a press anywhere on it means or is not a layer; a
  decoration is as big as what it decorates.
- Selection is the host's; the tree says where selecting is meaningful.
- `Persisted` is for new incidental state; `workspace.rs` is the editor's.
- One tree, N windows, no window named in it.
- Geometry is produced by layout or recorded by ruling, never by accident.
- Composition is the only extension mechanism; the cost is verbosity.
- Repaint boundaries are measured before they are built.
- Settings was migrated after its prerequisites, and that order was right.

---

## 9. Residue that belongs to other work

Named here so it is not rediscovered as a gap in this arc: sticky ancestors
in `Tree` and tabs within a sidebar section (sidebar feature asks);
`DocumentSymbols` exposed to plugins (the code-outline half of #1791);
plugin sections on the web (returns with §3.9); the scrollbar-markers plugin
API (`plugin-scrollbar-markers-design.md`, which L8 unblocks); the LSP hover
tooltip that cannot be dismissed through the gutter (pre-existing, identical
on master); the `_`/`%5F` slug readability question and the placeholder-row
product question from #3163.
