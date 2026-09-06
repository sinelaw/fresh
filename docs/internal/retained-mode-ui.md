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
`focus_element` (handlers fire, the echo lands in `pending_messages`). A
`debug_assert` that a scope holds at most one mark. **Verify** that
`active_scope()` (`:434`) names the *layer's* scope when both a layer scope
and an inner `focus_scope` node are present; the mark lookup must be over
the layer's scope, and the landing unconfined, because a host decision may
legitimately move focus out of a trapped subtree. This is the whole of the
focus problem in sinelaw/fresh#3176; §2.2 says why.

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
expressible on the node that owns it.

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
L2's capture leg plus the shortcut table consulted on it).

**L4 — The traversal order of a subtree is queryable.** `Ui::
traversal_order(scope)` and `next_in(scope, from, dir)`: the same policy
`move_focus` uses, as a pure read, available whether or not focus is inside
the subtree. This is what lets a plugin's `FocusAdvance` on a panel that does
*not* hold the tree's focus advance the panel's focus fact along the one ring
there is, instead of a second ring walked over the spec.

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
is corrected. `Modality::Pointer` exists for one painted interior
(`view/shell/modal.rs:123`) and is deleted with it (§3.6).

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
   property 1, so building it from dispatch has no side effect but layout.
   **Verify** that the frame builder writes no editor state; if it does,
   that write is a defect on its own. The e2e harness renders between events
   and cannot see this class; the test sends two events before rendering.
5. **One ring.** `handle_widget_focus_advance` (`:1216`) and its spec walk
   (`focus_ring_scoped_in_spec`) are deleted. A `FocusAdvance` on a panel the
   tree is focused in is `Ui::move_focus`; on one it is not, it is
   `next_in(interior, fact, dir)` (L4) written to the fact. Same order, one
   source.
6. **Every panel is on the ring, including pane-mounted ones.** A pane panel
   today has no keyboard layer and its wrapper is `skip_traversal`
   (`widgets.rs:637`) because Tab there is the plugin's mode binding. With L3
   the binding is a shortcut on the panel's scope, so the pane's panel gets
   the same `Modality::Focus` layer the dock has, active while its pane is
   the active pane, and Tab is the tree's everywhere.
7. **The settings dialog is on the ring.** Its layer names the card body as
   its scope; its `keys` node (`view/shell/settings.rs:319`) becomes a
   capture handler for the dialog's chords and a bubble fallback that claims
   only what nothing inside answered (L2); the `WidgetFocus` applier
   (`shell_host.rs:1790`) gains the `Settings` and `SettingsEntry` arms it
   drops today (`:1796`); `FocusManager<T>` (`view/ui/focus.rs:31`) and
   `settings_state.focus` are deleted; Tab inside the body steps control to
   control and leaves at the end.
8. **The keyboard tables go.** With L3 the router's mode-binding check,
   `KeyContext` as a computed enum (`input/keybindings.rs:232` stays as the
   *vocabulary* of contexts; `get_key_context`'s ladder goes), `layer_rank`
   (`app/chrome/mod.rs:52`), `LayerKind` and `overlay_stack` (`app/overlay.rs`)
   are all derivable: precedence is layer declaration order, "does a modal
   block terminal input" is `Ui::raw_input()` (exists, `schedule.rs:730`),
   and the base key dispatcher is reached only through the buffer host's own
   key handler.
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

### 2.4 Tests that pin the contract

In `app/widget_runtime.rs`, before any of it changes: the two existing
tests; the tree's own Tab is not undone by the next frame's stale mark; a
decision on an unfocused panel moves nothing until the panel is entered and
then lands; `autoFocusFirst:false` with an empty key is not re-seeded by the
tree's entry landing (**verify** which way this comes out today — if it
re-seeds, that is the welcome-screen bug the option was added for, returned
by the back door); two decisions in one batch resolve the second from the
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

- **Menu dropdowns are still a picture.** `DropdownLevel { x, y, width,
  rows }` (`view/shell/menu.rs:179`) carries a rect and pre-fitted rows
  computed by `fit_dropdown_area`, `calculate_dropdown_width` and
  `items_to_show` (`view/ui/menu.rs`). The chain is already nested as layers
  (a mouse bug fixed that); the content model moves into the description —
  rows as nodes, width by `Auto`, the one-row rise by `Anchor::Node` (which
  has real callers now), placement by `Place`/`Fit` — and the legacy layout
  walk and the web's `menu_layout_now` reader (`view/scene.rs:195`) go. The
  known eleven-cell difference between the editor's flip rule and
  `Fit::FLIP` is decided by a test naming the chosen behaviour.
- **The prompt line is the last host region among the chrome**
  (`frame.rs:30`, `PromptLine`). Its input row is a `TextField` over the
  editor's one `TextEdit` engine; its overlay toolbar (`Toggle`s and
  `Button`s, painted by `render_spec_no_autofocus` at `app/render.rs:4728`
  and `:6532`) is described through the same adapter as every panel, which
  deletes the third focus ring (`overlay_toolbar_keys`,
  `prompt_toolbar_boxes`, `Prompt.toolbar_focus`) and `text_click.rs`.
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
wizard and workspace-trust dialog follow the same shape; the two that still
hit-test painter rectangles (`workspace_trust_dialog`, `Window::
file_browser_layout`) are the last consumers of `Modality::Pointer`, and it
goes with them (L10).

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
`Paint::Lit`, `Modality::Pointer`, `view/controls/`, `EntryDialogState`,
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
| **S5 Settings model** | `view/controls/` deleted; entry dialogs as layers over `WidgetSpec`; the three modals; `Modality::Pointer` deleted (§3.6, L10) | S1 (keyboard) | — |
| **S6 Menus** | dropdown content model in the description; legacy menu layout and `menu_layout_now` deleted (§3.4) | S2 (shortcuts on menus) | S9 |
| **S7 The pane as a host leaf, and its chrome** | `BufferHost` as a `HostSpec::Leaf` with hit, byte, focus, scroll facts and caret (§3.7.1–3.7.8); `content.rs` deleted; tab strip, scrollbars, gutter as nodes; `PointerGrab` → captures; L6, L8, L12, L13; `WindowLayoutCache` deleted; one fold, one caret; provenance gate (§3.2, §3.3, §3.7) | S2 (shortcuts on the leaf) | S8 |
| **S8 Theme and performance** | `Paint::Lit` retired; resolve cache; benchmark; `Rc<WidgetSpec>`, memo at the seam; L7, L9, L11; the `ThemeKey` decision (§3.3, §3.10) | S3, S7 | — |
| **S9 Web** | DOM fold of the display list; scene region views retired; plugin panels return; guard removed (§3.9) | S3, S6 | — |

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
  between two keys. If the frame builder is not pure, that is found first
  and fixed first.
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
- `Modality::Focus` is permanent; `Modality::Pointer` is not.
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
