# Migrating the Fresh editor UI onto `fresh-ui`

> _AI-generated. **This document opens with what is left.** Sections I–IV are
> the live plan: where the integration actually stands, everything still to do
> and how each piece lands in the library's idiom, the rules that keep it
> there, and what is already done. Everything from `## 0. Situation` onward is
> the **appendix** — the original current-state survey, target design and wave
> plan, kept for their reasoning and their record, and kept at their original
> numbering because code comments cite it (`§4.4`, `§4.6`, `§6.2 item 7`,
> `S1`–`S5`, `M0`–`M9`). The editor-side companion to
> [`widget-library-design.md`](widget-library-design.md) (the architecture
> authority for `fresh-ui`) and
> [`widget-library-implementation-plan.md`](widget-library-implementation-plan.md).
> Where this doc and those disagree about the target, they win; where they
> disagree about the current editor, the source wins._

---

## I. Where the integration actually stands

**We have adopted the library's structural half and almost none of its
retained half.** That is worth stating plainly, because it is not visible from
the surface-by-surface progress and it is the reason most of what is left is
hard.

Counted in `view/shell/` and `app/shell_host.rs`:

| Adopted | Not adopted |
|---|---|
| `Modality` (18), `Dismiss` (27), `Scrim` (8) — layers, exclusivity, dismissal | `Component` — **0** surfaces own state |
| `layout_reader` (13), `resolve` (8) — app logic keyed on extent | `.shared()` / `shared_rc` — **0** hoisted subtrees |
| `viewport` (3), `widgets::*` (9), `host_leaf` (5) | `provide` / `Ambient` — **0** values flowing down |
| `on_capture` (3) — capture-phase observers | `Behavior` — **0** (`Tasks`, `Ticker`, `Cache`, `Persisted`) |
| The display list, `rect_of` read-back, one geometry | `focus_scope` / `focus_within` — **0**; `focusable` twice |

**The shape that follows from that.** `frame_tree(shell.clone())` is called
once per frame with a `Frame` value computed fresh from `Editor`. The
description is a pure function of editor state, rebuilt whole, every frame.
Every closure in it is new, so the reconciler's `Rc::ptr_eq` short-circuit
never fires; every element is re-reconciled on every PTY tick. §4.7 predicted
this exactly — "that is the *opposite* of the library's cost model" — and
committed M0 to writing down the practice rules that would prevent it (which
subtrees are `Shared`, which state moves into components) **before M1**. Those
rules were never written. This is the one commitment in the plan that was
skipped rather than decided.

**It was the right trade to make, and it is the wrong one to keep.** Rebuilding
from `Editor` is what allowed a surface to migrate in one change without moving
its state first: the description could be derived from where the state already
was. That is why nine surfaces landed. But the bill is now due, and it is
itemised as the remaining work:

- **The keyboard (A below) is hard because there is no focus tree.** Keys
  cannot route by focus when two nodes in the whole shell are focusable.
- **~~The `shell_*` / `pending_*` fields exist because there are no
  ambients.~~ Half right, and the wrong half was the prescription.** They do
  exist because a value had to cross a `&mut self` boundary. But an ambient
  carries a value *down the description to nodes that read it*, and **not one
  of these six is read by a node** — they are read by host painters and by the
  dispatch code wrapped around the tree. Three were per-frame or per-event
  locals wearing a field's clothes, and are now locals (`pending_pane_chrome`
  as a `BodyPainter` parameter; `shell_menu_open_before` and `shell_hover_at`
  as `EventFacts`). `shell_hover` is genuinely editor state on a different
  lifetime — written at dispatch, read at paint. `shell_frame_status_bar` is a
  frame cache the tree could answer instead. `shell_pointer_event` is B.4.
  **Kept as a finding**: "there is no ambient" was a real gap and also the
  wrong tool for this, and the plan reached for it because the gap was
  recently named rather than because these values wanted to flow down.
- **`WidgetInstanceState` cannot become element state until elements own
  state**, which means components — so C below is gated on the same gap.

So there is a **zeroth item**, ahead of the rest: adopt the retained half.
It is listed first below because two of the four groups after it depend on it.

---

## II. What is left, and how each piece lands

One line for what it is, one for how it lands in the library's idiom, one for
what to avoid. "Avoid" is not style advice: each one names a shape that would
pass review, ship, and reproduce the thing the migration is removing.

### 0. Adopt the retained half (gates A and C)

| # | What | How | Avoid |
|---|---|---|---|
| 0.1 | The whole description is rebuilt from `Editor` every frame. | Hoist the invariant subtrees into `.shared()` and hold the `Rc` — the menu bar's labels, the explorer's rows, a pane's interior — so the identity short-circuit fires. | Rebuilding "because it is cheap". It is cheap *per node*; the cost is re-reconciling every element on every terminal tick. |
| 0.2 | ~~Six~~ **three** `shell_*` / `pending_*` fields carry values around the tree. **Half done.** | Ask what reads it. A value read *by a node* is an `Ambient`; a value read by a painter or by the dispatch around the tree is a **parameter**, and three of the six were that (done: `pending_pane_chrome`, `shell_menu_open_before`, `shell_hover_at`). Left: `shell_frame_status_bar` (a frame cache the tree can answer), `shell_hover` (real editor state, different lifetime), `shell_pointer_event` (B.4). | Reaching for an ambient because it is the primitive we just noticed. It carries values *down to nodes*; none of these six was read by a node, and three were locals. A field is not evidence of a missing ambient — it is evidence of a value with no home, and "local" is a home. |
| 0.3 | No surface owns its own state. | Surfaces whose state is *theirs* — a list's scroll, a tree's expansion, a field's cursor — become `Component`s that own it. | Moving editor-model state into components. The rule is §4.3's: state the editor acts on stays on the editor; state that exists only because something is on screen belongs to the element. |
| 0.4 | §4.7's practice rules were never written. **Written below.** | See "The practice rules" under §III — five rules, each with the failure it prevents. | Skipping them again. They are the difference between a retained tree and an immediate-mode one with extra steps. |
| ~~0.5~~ **Done.** | **One retained tree, N windows, and no window ever named in it.** Turned 0.3 from a refactor into a correctness change; see below. | `fresh_ui::scope(window_scope(id))` around the chrome column and the overlays that hang off it; the dock, the modals, the trust prompt and the inspector outside it. A `MemStore` on the editor, and `forget_window_ui_state` at the three places a window id stops existing — because the tree cannot tell "switched away from" (keep) from "closed" (drop), and only the host can. | A `Ui` per window. It looks like the encapsulated answer and it breaks the dock, which is editor-global by design. |

#### 0.5 in full: the tree is shared across windows, and the windows are not

The editor is N independent workspaces — `Editor.windows`, one active — each
owning its own splits, buffers, explorer, dock, chrome layout and mouse state.
The retained tree is **not** one of those things it owns: there is a single
`Ui` on the `Editor`, reconciled each frame against whichever window is
active, and no key anywhere in `view/shell/` mentions a window.

**It has not bitten because nothing is retained yet.** `Component` count is
zero, so the tree is geometry and gestures, both re-derived from the active
window every frame. Switching windows re-derives everything, and there is no
state to carry.

**It bites the moment 0.3 lands.** A pane is keyed `Key::Pair("pane", id)`, and
`SplitManager::next_split_id` starts at **1 in every window** — so window A's
first pane and window B's first pane have the *same key*. Reconciliation is by
`(type, key)` at a position; it will match them, and window B's list will
inherit window A's scroll offset. That is not a corner case, it is the default
layout of two windows.

**And it contradicts a principle this codebase already states.** From
`Window::authority`'s own doc: *"Owned outright by this window, never shared
with another: it lives here (not in the `Clone` `WindowResources`) so the type
system prevents one workspace's authority/trust/env from leaking into another
(issue #2280)."* The shell tree is precisely the shared mutable state that
sentence is about — it is simply newer than the sentence.

**The obvious fix is wrong, and the orchestrator dock is what proves it.**

A `Ui` per window (`Window.shell_ui`) looks right — cross-window matching
becomes impossible rather than avoidable, and each window keeps its element
state while inactive. It is the wrong shape, because **the frame is not wholly
per-window.** The dock is carved out of the frame *before* the window's chrome
column, its state is `Editor.dock` — the field's own doc says "the
**editor-global** left dock panel" — it lists and switches between *all*
windows, and it deliberately persists while the active window changes. Give
each window its own tree and the dock is described into every one of them, its
element state follows whichever window is active, and the sessions list loses
its scroll on every workspace switch. That is the same bug this item exists to
prevent, arrived at from the other side.

So the scoping has to be **inside one tree**, and the frame is two scopes side
by side:

```text
row([ dock            // editor-scoped: outside the window key
    , window_area     // window-scoped: under Key::Pair("window", active)
    ])
```

Editor-scoped, with the dock: the four modals, the workspace-trust prompt and
the theme inspector — everything whose state is on the `Editor` and whose
lifetime is not a window's.

**And the objection to that shape is already answered by a library primitive
we have not adopted.** A changed key at the window position *discards* the
subtree, so switching windows throws away its element state. Two things make
that fine:

- **Existing serialized view state is model state and stays where it is** —
  the explorer's state, the split view states, the buffer set all live on the
  `Window` already. The dock demonstrates the same rule from the other side:
  its scroll survives switches precisely because the dock is *not* in the
  window subtree.
- **New incidental view state uses `Persisted`.** The library has
  `behavior::Persisted<T>`, a host `Store` installed with `Ui::set_store`, and
  a `PersistenceScope` ambient; a value is read at construction and written
  back at teardown, and — in the behavior's own words — *"keys are anchored to
  the enclosing `PersistenceScope` rather than to tree position, so moving a
  widget does not lose its value and **two widgets at the same position under
  different documents do not share one**."* Windows are the documents. That
  sentence is this problem, written down in the library before we met it.

So the window subtree carries two things at its root: the key that bounds
identity, and `PersistenceScope(window_id)` that bounds persistence. Editor
adoption of the second today: `Persisted<…>` **0 uses**, `Ui::set_store`
**never called**. This is §I again — we were about to invent a workaround for
a gap the library does not have.

**With one caveat, and it is load-bearing: the primitive is present but
unproven.** `Persisted`, `Store` and `PERSISTENCE_SCOPE` are declared and
exported, and the path is complete on paper — `attach` reads from the store,
`teardown` writes back, `Services.store` carries it, `Ui::set_store` installs
it. But there is **not one test and not one usage anywhere in the crate**, and
the demo it would be proven in is single-document: tasks, a filter, a theme, a
menu, a million-row list, no documents and no switching. What is demonstrated
today is that the types compile. See F.5 — that gap closes *before* 0.3 and 0.5
lean on it, not after.

What this costs is discipline in one place instead of everywhere: one key, at
one node, rather than `WindowId` threaded through every window-owned subtree
where a single omission is a silent leak between workspaces.

**The six editor-global shell fields sort themselves by the same question.**
`shell_hover`, `shell_hover_at`, `shell_menu_open_before`,
`shell_frame_status_bar` and `pending_pane_chrome` are per-window facts parked
on the `Editor` and move to `Window`; `shell_pointer_event` is frame-scoped and
dies with B.4. 0.2 turns most of them into ambients, and the scope they are
provided at is the scope they belong to.

### A. The keyboard engine

| # | What | How | Avoid |
|---|---|---|---|
| A.1 | `base`, `menu`, `popups`, `prompt`, `context_menu` implement `on_layer_key`. **Free today.** | The surface declares `focusable()` / `focus_scope()`; its bindings ride down as `Shortcut { key, intent }` data and the library's Shortcuts → Intents → Actions chain resolves them. The menu bar already ships this shape (`MenuShortcut`) — copy it, do not reinvent it. | Resolving key → action *before* dispatch and handing the tree an `Action`. §6.2 decision 1 settled this the other way: bindings flow down as data, the tree resolves. |
| A.2 | `dock` + `floating_modal` `on_layer_key` hand the key to the widget dispatcher. | Rides with C. | Migrating them early behind a shim that calls the old dispatcher from a `GestureKind::Key` handler. That is the walk with a new caller. |
| A.3 | The four `modals` `on_layer_key` hand it to painter interiors. | Rides with B. | As A.2. |
| A.4 | `layer_rank` — a central ordered list of surfaces. | Delete it. Precedence is *derived*: layer order, `Modality::Exclusive`, focus-scope containment — all already in the tree. | A `key_rank` property on layers, or a `Behavior` that walks them in order. Goal 2 forbids the central list by name; a renamed one is the same list. |
| A.5 | `KeyContext` — the mode enum the walk keyed on, and the largest remnant by reference count. | "Which bindings apply" becomes *where focus is*: a scope provides its shortcut set as an ambient, resolution walks the focus path up. | Keeping `KeyContext` as an ambient. That is the enum with a new home; the point is that containment already answers it. |

### B. The modal interiors

| # | What | How | Avoid |
|---|---|---|---|
| B.1 | Settings (~20k lines) hit-tests rectangles its own painter recorded. | Composition: it is a form, and the library ships button, toggle, radio group, number, text field, list, tree and dropdown. Its state becomes component state; its scroll a `viewport`; its double-click `Event::clicks`. | Mounting it as one `Host` and calling the wave done. That keeps every duplicate it owns and adds a leaf that never shrinks. |
| B.2 | The keybinding editor (~3.7k), with its own scrollbar and double-click semantics. **The box has moved** (`view::shell::keybinding`); nine of its ten recorded rectangles have not. | As B.1; its table is a `List`. The frame first, the interior after — C.6's order. `keybinding_modal_area`'s four lines (ninety percent, capped at 120, floored at 60×20, centred with `area.x` added back so it lands beside the dock) are a layer that names the region it may occupy; the painter and the mouse handler read one rectangle instead of computing and recording it. The cap has no property to be — `min_w` exists and `max_w` does not — so the width comes from a `layout_reader`, which is §4.4's sanctioned form. | — |
| ~~B.3~~ **Done.** | The calibration wizard. | `view::shell::calibration`. It turned out to be the *whole* of its wave rather than a smaller B.1: it has no mouse — single letters drive it — and no recorded rectangles, so once the box became a description there was nothing left behind the seam. `apply_dimming` over the frame is `Scrim::Dim`; `Layout::vertical([Length(5), Min(8), Length(4)])` is a column; and the capture phase's `key_idx - available_height + 1`, with its `saturating_sub(2)` for two footer rows the painter knew it had drawn, is a `viewport` and one `Anchor::reveal`. **`modal::Slot::Calibration` retired with it** — a slot beside a described modal routes a pointer to a surface that never wanted one. | Keeping the slot "for symmetry". Three modals still need it; this one stopped. |
| B.4 | `Editor::shell_pointer_event` — the event travels on the editor, not in the message. | Delete it. It exists because the interiors tell a drag from a move; the library routes drags by **pointer capture**, so they stop asking. | Generalising it into a typed side channel. "Routed, not transported" was a transitional apology, not a design. |
| B.5 | ~~`cursor_suppressed_by_late_overlay` — a hand-kept list~~ **The entry was stale: it is already derived.** It reads the overlay stack — "every present layer suppresses except the ones that don't paint over a chrome caret's cell" — and its own comment records that it *used* to be a seven-item hand list that had drifted from `hide_cursor`'s. A new modal surface registers a layer and is suppressed with no edit. | Still deleted, and still with B: "did something cover the caret" is the display list's order once the interiors are in it. What is left is the reading, not the list. | Adding an entry. There is nothing to add an entry to any more. |
| B.6 | `ChromeComponent` and `app/chrome/`. | Deleted outright once A.1–A.3 and B.1–B.3 land. | — |

### C. The plugin panels

| # | What | How | Avoid |
|---|---|---|---|
| C.1 | `WidgetSpec → Node` for dock / floating / anchored / the prompt's toolbar. | Map the 19 variants onto `widgets::*` — the library's own set. Goal 1: there is no privileged internal surface, so a plugin's `Toggle` is the `Toggle` the settings form uses. | A second widget runtime host-side: translating `WidgetSpec` into bespoke painters. That is `widgets/kinds/*` again with a tree under it. |
| C.2 | `WidgetInstanceState` — a `HashMap<String, _>` keyed by the widget's key. | Element state. Goal 4 names this failure exactly: identity by tree position and key, "never by hashing an identifier stack into a side table". | Keeping the map "because keys are already unique". The map is the side table. |
| C.3 | `HitArea` (byte ranges) and `LayoutBox` (a parent-linked, z-ordered arena). | Both deleted. `LayoutBox` is a second layout tree; goal 5 allows one. | Keeping `LayoutBox` as the web bridge's hit list. The web is a consumer of the display list (D.3), which already carries keyed rectangles. |
| C.4 | `WidgetMutation`'s fast path — a channel that patches retained state in place. | An ordinary rebuild. Goal 3: a rebuild costs one allocation per node, so the incentive the fast path answers does not exist. | Keeping it as an optimisation without measuring it against 0.4's benchmark. |
| C.5 | Buffer-mounted panels (`mountWidgetPanel`). | A subtree in the pane's content slot; the virtual buffer stays as a text mirror for search, copy and the `lines_changed` hooks. Removes the documented limitation that mounted panels drop overlays and popups. | Deciding it by which is less work. It is the only open *design* question left in the whole migration: it was deferred deliberately, to be taken with C.1's experience in hand, not settled by default. |
| C.5b | **The dock is editor-global UI built from `Editor.windows`, not from the active window.** Its content is the orchestrator's `WidgetSpec`; its column, grip and blur observer are already nodes. | Its content lands like any other panel (C.1), mounted *outside* the window key per 0.5. Its own two remainders go with it: `chrome::Dock::on_layer_key` (A.2) and the scrollbar-reveal hover, which reads zones the plugin publishes from inside the panel. | Building its description from `active_window()`. `shell_frame` does that for nearly everything else, and the dock is the one surface for which it is wrong. |
| ~~C.6~~ **Done.** | The floating panel's frame — border, title, `[×]`, placement. | `view::shell::panel`: layout places the box, the fold paints it, the painter reads its rectangle and its content rectangle back with no fallback arithmetic. `[×]` is a node that stops its own press, so `close_button_rect` — a rectangle the painter filed for a mouse arm to compare against — is gone from the state, the arm and the web projection. **The scrim did not move**, and the reason is paint order: the dock's own panel is painted after the tree's overlay band, so a `Scrim` here would be overpainted and the frame would read half-dimmed. It goes with C.5b. | — |

#### What C and B actually weigh

Stated here because the plan describes both as a mapping and a form, and the
numbers change how they should be scheduled — neither is a wave that lands in
one change.

| | Lines | What it is |
|---|---|---|
| **C.1–C.4** | **~17,200** | `crates/fresh-editor/src/widgets/`: `render.rs` alone is 7,832, `kinds/*` another 7,000, plus `registry`, `actions`, `layout_box`, `text_click`. This is a complete widget runtime — layout, paint, hit-testing, focus and event routing — and C.1 replaces it rather than adapting it. |
| **B.1–B.3** | **~24,000** | Settings (`view/settings/`, ~20k with `render.rs` at 4,040 and `widget_map.rs` at 1,303), the keybinding editor, the calibration wizard. |

**And they are not independent — B.1 largely rides on C.1.** Settings'
*controls* are already `WidgetSpec`: `view/settings/widget_map.rs` maps a
`SettingControl` onto a widget kind ("Once Settings renders the resulting tree
through `widgets::render_spec`, the two frameworks are one" — its own module
doc, phase 3 of a unification that has landed), `render.rs` calls
`render_spec_no_autofocus` and paints its `RenderOutput`, and `mouse.rs`
hit-tests through `WidgetTextClickGeometry::from_render_output`. So the
control layer of the settings dialog — rendering *and* hit-testing — is the
widget runtime, and C.1 carries it.

What stays B-specific is the dialog around the controls: the category
navigation, the search, the item rows, and the editing input path. The
~24,000 figure is therefore the *file* count, not the migration's; **the
remaining bulk is C.1's ~17,200, and B.1 shrinks to the page chrome once it
lands.** That reverses the plan's implicit ordering, which listed B before C
and treated them as separable.

**Why this matters for sequencing.** Both have to land whole. A panel is
either described as a `Node` or painted by the runtime; there is no frame in
which half of it is each, and a partial C.1 is precisely the "second widget
runtime host-side" that C.1's own *Avoid* column forbids. So neither can be
sliced by variant — they slice by *surface*: one panel slot, or one settings
page, migrated end to end with the old path still serving the rest.

Everything else in this document is gated on one of these two. That is the
honest shape of what is left: the ungated items are done, and the remainder is
two subsystem replacements plus the deletions they unblock.

#### C.1 in full: the nineteen variants, and where each one lands

Written down because C.1 is the largest remaining item and every other
ungated item is behind it, and because the mapping is the decision — once it
is settled the work is mechanical. `WidgetSpec` does not change: the plugin
API is frozen, and this is a change of what the host does with it.

| `WidgetSpec` | `fresh-ui` | Note |
|---|---|---|
| `Row { wrap }` | `row()`, `.wrap_children()` when `wrap` | The one variant with no counterpart until [#3108](https://github.com/sinelaw/fresh/pull/3108) added wrapping boxes. Its rule — break at a child boundary, never split a child — is the library's now. |
| `Col` | `col()` | |
| `Spacer` | `widgets::spacer` | |
| `Divider` | `widgets::divider` | |
| `Text` | `text()` / `text_runs()` | Styled spans are runs. |
| `Raw` | `text_runs()` | The escape hatch is `TextPropertyEntry[]`, which is already a run list; nothing interprets it, which is the point. |
| `HintBar` | `row()` of runs | Composition, not a widget: a hint bar is `<keys> <label>` pairs with a separator. |
| `LabeledSection` | `col().border()` + a title strip | The shape `popup::border_strip` already has. |
| `Toggle` | `widgets::Toggle` | `label_first` / `label_width` are the row's order and a `Sizing::Cells` on the label. `indeterminate` needs a third glyph state — the one small library change this table implies. |
| `Number` | `widgets::Number` | |
| `Dropdown` | `widgets::Dropdown` | Its pop-over is a `Layer`; the `screen_space` escape below is the same mechanism. |
| `DualList` | ~~`widgets::DualList`~~ **the adapter, unchanged** | **The table was wrong here too, twice over.** The library's `DualList` is two `List`s side by side, each with its own scroll; `WidgetSpec::DualList` is a *two-column table of paired rows*, one hit per cell in the same row. And it does not scroll at all — its body is `max(available, included, visible_rows)` tall with no offset anywhere — so it needs no substitution and has no bar to lose. It crosses through the adapter, and the only thing it needed was for a row to stop being one target. |
| `Button` | `widgets::Button` | |
| `List` | `widgets::List` | |
| `Tree` | ~~`widgets::Tree`~~ **`widgets::List`** | **The table was wrong here.** `WidgetSpec::Tree` is already *flat* — `nodes: Vec<TreeNode>` with a `depth` and a `has_children` flag — and its expansion is the **plugin's**: `expanded_keys` comes down in the spec and goes back through `WidgetMutation`. `widgets::Tree` builds its own nesting from recursive roots and owns `expanded` in element state, so it would fight the plugin for the one fact the plugin is authoritative for. The spec's tree is a controlled list of pre-rendered rows, and that is what it maps onto. |
| `Component` | `focus_scope()` + `key()` | Its two jobs are exactly those: trap Tab inside the subtree, and name it. Not a component in the library's sense, and it should not become one — it owns no state. |
| `Overlay` | `layer()` anchored to its own position | "Anchors at the row it would have occupied but the rows below do not shift" is `Place::Over` on a layer whose anchor is the node's slot. |
| `Popup { anchor, screen_space }` | `layer()`, `offset` from the panel body or anchored to the node, `within` the panel unless `screen_space` | `screen_space` is precisely "not confined to the panel's region", which is the `within` the base PR added. The *anchor* is panel-inner in both modes — a description cannot turn that into a frame coordinate, because it does not know where the panel is — so it hangs off the body and says how far inside. |
| `WindowEmbed` | a `Host` leaf | A real editor window inside a panel: cells, like every other `Host`. G's rule applies — this one never migrates. |

**The mapping is done.** `view/shell/widgets.rs` describes every variant but
`WindowEmbed` — which is a `Host` leaf by G's rule and never migrates — each
asserted against `render_spec`'s own answer. Nine are written out
(`Row` with `wrap`, `Col`, `Spacer`, `Divider`, `HintBar`, `Raw`,
`LabeledSection`, `Button`, `Toggle`), four are thin (`Component`, `Overlay`,
`Popup`, `Number`), and five go through the adapter described above.

**The coverage boundary is not the mapping's edge.** Every variant is
described; not every one is *mounted*. Some kinds own their scroll in the
runtime: the collector windows the rows itself and reports the offset on a
`LayoutBox`, and the painter draws a bar over the rightmost column from that.
The adapter turns rows into nodes and has nothing to say about a bar, so
describing one of them today would render it correctly and **silently lose its
scrollbar**, which is worse than painting it whole. Wrapping already-windowed
rows in a `viewport` does not fix it either: there would be nothing to scroll,
so the bar would be wrong rather than missing.

**The boundary is the scrollbar, not the offset**, and three of the five kinds
first held back turned out to be on the near side of it once asked rather than
assumed:

* `DualList` does not scroll at all. Its body is `max(available, included,
  visible_rows)` tall and its instance state carries no offset, so it emits
  every row and there is no bar to lose.
* `Dropdown` has a `scroll_offset`, which is what put it on the far side — but
  the host's pop-over pass paints a border and the rows and nothing else.
  `render_dropdown` clamps the scroll, slices the window and hands over each
  visible row with its absolute index; describing that reproduces it exactly.
* `Tree` is a *flat, controlled* list of pre-rendered rows whose expansion is
  the plugin's, so it crossed on `widgets::List` with `List` itself.

A `List` of *cards* — `item_specs`, each item a little block — crossed on a
library change rather than a boundary argument: `List` stamped `Cells(1)` on
every row and `ScrollMode::Items` counted one item per cell, so `row_rows` is
what it needed. The gutter is reserved whether the bar is there or not, which
also removes the reflow the runtime had (every card re-rendered one column
narrower the moment one more session made the list overflow).

A `Tree` with `card_borders` crossed on the other window the library has.
Its rows are *heterogeneous* — `item_height + 2` for a card node, one for a
folder header — and reading the renderer through settled which window it
wants: **it scrolls in rows, not in nodes.** The offset is a row into the
flattened list and a card straddling either edge is emitted and clipped, so a
`List` snapping to whole items would have been a behaviour change. A
cells-scrolling `viewport` is the same behaviour, and it owns the offset. The
one thing that needed saying afterwards was the reveal — the runtime scrolled
to keep the selection visible by writing the offset it also read; here the
offset is the viewport's, so what is left is "put this row in the window",
which is `Anchor::reveal`. (`item_height > 1` without `card_borders` does not
occur — the only producer sets the two together — so there is no third arm.)

**Multi-line `Text` closed it.** It was the last kind whose bar the panel's
painter drew, and it crossed the same way: the collector is asked for the
*whole* document — its `rows` is the window, so handing it one as tall as the
text makes it emit every line and clamp its own scroll to zero — and the
window is then `List::windowed` over those rows, one cell each, which is the
row scroll the runtime had. The caret is what the list reveals, which is the
whole of the auto-clamp. A label stays outside, because the collector windowed
only the text under it.

**So the boundary is closed.** `covered` answers yes to every variant but
`WindowEmbed`, which is a real editor window inside a panel and a `Host` leaf
by G's rule. What is left of C is no longer coverage: it is mounting the dock
(C.5b), deleting `LayoutBox` and the byte-range scan once nothing reads them
(C.3), and turning `WidgetInstanceState` into element state (C.2) — of which
the scroll, for every kind that had one, is now done.

**`List` has already crossed, and it is the proof of the shape.**
`widgets::List` windows its rows out of a `viewport`, so the scroll is the
element's and `scrollbar()` *is* the bar — the thing the adapter could not
describe comes free once the window belongs to the tree. Selection stays
controlled, which is what keeps the plugin API frozen: the plugin sets it, the
host's keys move it, and the list's own `Anchor` reveals it whenever it moves,
"the owner passing a new one down" included — the auto-clamp the runtime did
by hand, for free. The rows themselves are still the runtime's, because what a
row *says* is not this migration's business.

**That is why C.2 comes before full coverage, not after.** `widgets::List` and
`widgets::Tree` own their scroll, and then the bar is the viewport's and comes
free. The kinds still behind the boundary cross it when their state does. The panels that
are mounted today are the ones made of controls — forms, confirmations,
button rows — which is most of what the dock's dialogs are.

**What is left of C is no longer the mapping.** It is: mounting a panel on the
described path behind `covered()`; deleting `LayoutBox` and the byte-range
scan once nothing reads them (C.3); and replacing the collectors' formatting
with `widgets::List` / `widgets::Tree` so a plugin's list is the settings
form's list, which is where `WidgetInstanceState` becomes element state (C.2).
Two things the first variants settled:

* **The hit becomes a payload.** `deliver_widget_hit` — the dispatch all three
  frontends share — takes a `HitArea` and does the rest: focus the owner, run
  the kind's `on_pointer`, fire the plugin's `widget_event`. It does not
  change. What changes is that the tree *finds* the widget, by hit-testing a
  rectangle it laid out, instead of the host reconstructing it from a row and
  a byte offset. So `UiFact::WidgetHit` carries the same `HitArea` the runtime
  recorded, and a byte range stops being a hit-test. A toggle in form layout
  shows what that buys: its hit was restricted to the chip by a pair of byte
  offsets, and it is now restricted by where the nodes are.
* **Not every variant can be parity-checked on its cells.** `LabeledSection`
  draws its frame as *text* — `╭─ label ─…─╮` in an entry, `│ … │` around
  every child row — because entries are all the runtime has. The tree has a
  border and uses it, so the cells differ on purpose and the assertion is
  **geometric**: the child gets `panel_width - 4`, one row down and two
  columns in, which is `inner_width` plus what `shift_channels` shifted six
  recorded channels by. Chrome variants are checked that way; leaf and text
  variants are checked cell for cell.

**The route for the five heavy variants** (`Text`, `List`, `Tree`,
`Dropdown`, `DualList`), which is different from the nine already done and
worth stating before it is discovered twice. Each of them ends in a
`CollectedOutput` — entries, hits, overlays, a focus cursor — produced by a
collector that already exists and already knows the kind's rendering. So the
step that unlocks all five at once is a generic **`CollectedOutput` → `Node`**
adapter: rows become nodes, each hit becomes a gesture on the sub-range it
covers, each overlay becomes a layer. That is what deletes `LayoutBox` and the
byte-range scan (C.3) for every variant simultaneously, rather than five times.

**It is a stage, not the end.** After it, the runtime is a *formatter*: it
still decides what a list row looks like, and the tree owns where it is and
what a press on it means. Replacing that formatting with `widgets::List` and
`widgets::Tree` — so a plugin's list is the list the settings form uses, which
is goal 1 — is the step after, and it is where `WidgetInstanceState` becomes
element state (C.2). Doing the adapter first is what makes that step a
substitution rather than a rewrite.

**Three things the table settles that were open.**

* **`indeterminate` was the only library gap the *set* had**, and it is one
  glyph state on `Toggle`, not a new widget. Mounting the set found two more,
  both about the same thing and both in the base PR: **a coordinate needs a
  space to be in**. `Anchor::Point` and `Anchor::Cell` resolved to a frame
  coordinate whatever region the layer named, so `within` moved a screen
  anchor's origin but not a point's — an inconsistency the region's own
  docstring already ruled against ("the bounds are the whole coordinate space
  the placement works in, not just a right-hand limit"). And `offset` says
  where a layer's real anchor is when it is *inside a leaf*: the `[value ▼]`
  button inside a row a widget runtime laid out, a completion row of a
  sub-render with no node of its own. It shifts the anchor rather than the
  result, so a pop-over that flips above still clears the button it hangs
  off — the same fact `set_host_anchor` publishes, for a caller that holds the
  offset rather than the rectangle.
* **`Component` is not a `Component`.** It is a focus scope with a key, and
  reading it as the library's `Component` would give a plugin's subtree host
  state it never asked for. The name collides; the concept does not.
* **`Popup`'s two modes are one node**, because a layer already distinguishes
  "confined to a region" from "confined to the frame". Before #3108 they
  would have been two mechanisms.

What the table does *not* settle is C.5 — whether a buffer-mounted panel is a
subtree in the pane's content slot or stays a virtual buffer. That was
deferred deliberately, to be taken with C.1's experience in hand.

### D. Paint arrangements still mixed

| # | What | How | Avoid |
|---|---|---|---|
| ~~D.1~~ **Already done, and the entry was stale.** | The status bar's *prompt* states. | Checked against the source: `StatusBarRenderer::render_prompt` and `render_file_open_prompt` reach the buffer only through `Editor::render_prompt_line`, whose sole caller is `paint_host`'s `HostRegion::PromptLine` arm. G2 closed this when it moved the prompt row into the fold; the entry described the half that had already landed. | — |
| D.2 | `render_panels_and_modals` paints after the caret commits. | Closes with B and C; nothing to design. | Another deferral field. B.5 is what one costs. |
| D.3 | `suppress_chrome_cells` / `Paints::HostsOnly` — the web's parallel path. | The web is a **consumer of the display list**, not a mode that suppresses half the fold. Goal 7. End state: one list, two backends, and `Paints` is deleted. | A third `Paints` mode. Each one is a place the two frontends can disagree about what exists. |

### E. Residual recorded geometry

| # | What | How | Avoid |
|---|---|---|---|
| E.1 | ~~`WindowLayoutCache` is 40 fields.~~ ~~**Eight**~~ **Seven** — `file_explorer_area` is gone — and the rule sorts the rest. | Apply one rule: **a record is legitimate iff it cannot be derived from layout.** Stay: `tab_layouts` and `view_line_mappings` (measured text). Go: `split_areas`' content and scrollbar rectangles — `content_key`, `vscroll_key` and `hscroll_key` are keyed nodes, so the painter is recording what layout already placed; only `thumb_start`/`thumb_end` are its own. `last_editor_content_area` is the one honest record and its doc says why: `apply_layout` asks before the frame that would set it.

**`file_explorer_area` is done.** It had one reader (`file_explorer_view`, the web projection) and one writer, and the writer derived it from `HostRegion::Explorer` five lines above the store — a cache of a derivation in the same function. The reader asks the region directly now. Presence stayed app state, as the deleted comment said it should: `file_explorer_visible` says whether the sidebar is there, and the rectangle says where.

**`split_areas` needs an oracle first, and that is the whole of what is left here.** Its rects are written at paint time and read by mouse handlers *between* frames, which is the staleness the rule is about — but they route every click in the editor, and the parity that would make the swap safe is an integration-level question (a live `Editor` with panes), not a unit one. The pattern is E.2's: derive both, assert they agree, then delete the recorded one. Note also that `split_layout` lays `pane_interior` out in a **throwaway `Ui` per pane per frame** while the shell tree already contains the same description under the same keys — that is a duplicate *derivation* rather than a duplicate record, so it cannot disagree, but it is the same rectangle computed twice and it goes with this. | Re-recording a rectangle for speed. 0.4's benchmark is the answer, and every rectangle recorded twice is a chance to disagree. |
| ~~E.2~~ **Done.** | `separator_areas` was assembled from **two** producers: a second layout walk (`get_separators_with_ids`, re-running `split_rect_ext` against a rectangle the caller supplied) for the main grid, and the painter's own recording for grouped subtrees, which the first could not see. | `separator_rects` — the model says which containers exist and which way each splits, layout says where each divider landed. Grouped subtrees need no special case (their dividers have been ordinary nodes since S5) and neither does maximization (a maximized frame describes no divider, so it has no rectangle). The removed walk stays as the oracle `the_dividers_are_where_the_separators_are` checks the tree against — the one job a second derivation is good for. | — |
| E.3 | `PointerGrab` — the drag state machine. | The library's pointer capture, which is already there and is what B.4 also needs. | Porting the state machine. It is the thing capture exists to replace. |

### F. Library-side (both are `fresh-ui` changes, not editor ones)

| # | What | How | Avoid |
|---|---|---|---|
| F.1 | `Draw::Scrollbar` carries `{offset, content, window}` and no marker channel, so the plugin overview-ruler API keeps scrollbars behind a `Host`. | **Two options, and the prior art prefers the second.** Extend the library's scrollbar; or keep the library out of it — VS Code's overview ruler is a canvas overlay that services inject zones into (`afterLineNumber` + a semantic colour), mapping line numbers to fractions of the total height with no knowledge of text layout at all. That is an editor-side `OverviewRuler` node over the scroll track, and it keeps line-height and buffer knowledge out of a library whose goal 6 is composition. Settle it before the wave that needs it. | Working around it in the editor *by accident* — which is what happens if nobody chooses, since the `Host` is already there. Appendix risk 1: a wave that needs a library change is a signal to stop and fix the library. |
| F.2 | `Paint::Lit` — a colour with no theme name, for plugin RGB and markdown spans. | Plugins register named keys; `resolve_theme_key` grows a dynamic tier. Then provenance is total. | Leaving it. It is the one thing in the display list that is not traceable to a theme entry, and it is honest about that only because it is temporary. |
| F.3 | **A subtree is either mounted — reconciled, laid out, painted, hit-tested — or gone, with its elements disposed and its `Tasks` cancelled.** There is no mounted-but-inactive state. `Sizing::Cells(0)` still reconciles and lays out; `PointerMode::Ignore` removes only hits. | Genuinely absent, and generic. The shape to copy is React 19.2's `<Activity mode="hidden">`, and its precision is the useful part: keep the element tree **and its state**, *unmount the effects* (subscriptions, timers), and defer updates to a low-priority queue. Flutter's `Offstage` is the cautionary version — it drops the child from layout and paint but keeps tickers running unless `TickerMode` is also disabled, which is the manual orchestration a primitive exists to remove. **Not needed for windows** — a switch is a user action, one cold rebuild is ~163µs, and `Persisted` covers the state — so raise it on its merits. | Reaching for it *for* the window case, or emulating it with a zero-sized subtree, which pays reconcile and layout to hide something. |
| ~~**F.5**~~ **Done** ([#3108](https://github.com/sinelaw/fresh/pull/3108)). | **`Persisted` / `Store` / `PERSISTENCE_SCOPE` had zero tests and zero uses in the library**, and the demo under `tests/support/demo/` is single-document. 0.3 and 0.5 both rest on behaviour nobody has run. | A multi-document scenario in the demo — two documents, a switch, per-document incidental state that survives it — and unit tests for the four things that are currently assumptions: that `teardown` fires when a *key change* discards a subtree (not only on `Ui` drop); that it fires before the replacement's `attach`; that **deferred disposal** does not reorder those two; and that a `Persisted` under the wrong scope is detectable. | Adopting it on the strength of the doc comment. It is a good doc comment. |
| ~~**F.4**~~ **Done** ([#3108](https://github.com/sinelaw/fresh/pull/3108)). | Nothing tied an **identity boundary** to a **persistence scope**. A subtree can be keyed without providing a `PersistenceScope`, or scoped without a key, and both mistakes are silent. | A single primitive that does both — `scope(id, child)` — so the invariant cannot be half-declared. Goal 2's spirit: derive it from structure rather than ask every author to remember two things that are always used together. | Documenting the pairing instead. 0.5 is the first place it matters and it will not be the last. |

### G. Not a gap

`HostRegion::Body`'s per-pane `Host` leaves. Buffer and terminal cells stay
cells: that leaf never migrates, and S5 subdivided it rather than removing it.
Listed so a reader working down this list does not try to close it.

### Prior art, and where it changes the above

A survey of Flutter, React, Compose, SwiftUI, VS Code, Zed, Emacs, IntelliJ,
Unreal Slate, Unity, Godot and Dear ImGui against these twelve questions. What
it changed, what it confirmed, and — because a survey that agrees with you is
worth less than one that does not — what it got wrong about this codebase.

**It changed two entries.**

- **F.3 has a precise shape now**: React 19.2's `<Activity mode="hidden">` —
  keep the tree *and its state*, **unmount the effects**, defer updates to a
  low-priority queue. Flutter's `Offstage` is the cautionary version: it drops
  the child from layout and paint but leaves tickers running unless
  `TickerMode` is disabled too, which is exactly the manual orchestration a
  primitive should remove.
- **F.1 gained a better option.** VS Code's overview ruler is *architecturally
  decoupled from text layout*: services inject zones (`afterLineNumber` plus a
  semantic colour) into a canvas over the scroll track, and the ruler maps line
  numbers to fractions of total height knowing nothing about the text engine.
  An editor-side `OverviewRuler` node keeps line-height and buffer knowledge
  out of the library, which is goal 6's argument. Extending `Draw::Scrollbar`
  is now the *second* option, not the only one.

**It confirmed four, which is worth recording because they were judgement
calls.** Multi-document scoping as an editor-side convention over a
lightweight generic scope node (0.5, and `scope()` is that node). Global chrome
as editor-side, with an explicit warning against building a `Scaffold` or
`Workspace` primitive — the dock stays ours (C.5b). Settings built from a
declarative model application-side rather than by framework reflection (B.1).
A logic-less serialized description for plugin UI, RFW-shaped (C.1).

**And it was wrong about this codebase three times, each in the same
direction** — recommending as a *new library primitive* something `fresh-ui`
already has:

- a "context-aware action dispatcher" resolving keys to intents — that is the
  `focus/` module's Shortcuts → Intents → Actions chain, shipped. A.1 is
  adoption, not construction.
- an explicit pointer-capture API — shipped, and E.3 is adoption.
- a "paint-phase theme context" so a colour change does not relayout —
  `ThemeKey` is resolved at fold time, so it already cannot.

That pattern is itself the finding, and it is §I restated from outside: the
gaps a reader sees in this migration are mostly capabilities the library has
and the editor has not adopted.

**One suggestion worth keeping and not acting on yet.** Several engines make a
document scope an *event* boundary as well as a state one, so an event in one
document cannot bubble to the root and back down into another. Ours does not
need it — only one window is mounted at a time — but if inactive windows ever
become mounted (F.3), it becomes load-bearing on the same day.

**On the evidence.** The Flutter, React and VS Code claims are checkable
against primary sources; several game-engine and IntelliJ ones rest on
secondary write-ups, and a few citations are blog posts. Treat the
architecture as well-sourced and the performance characterisations as
indicative.

---

## III. The rules that keep this in the idiom

The library's README states seven goals. Each one forbids something this
migration has already been tempted by at least once; that is what makes them
useful as a review checklist rather than a preamble.

1. **One library for everything on screen.** → A plugin's widget is the
   editor's widget. *Forbids* C.1's second runtime.
2. **Generic registration and propagation, no hand-specified exceptions.** →
   Precedence is derived from structure. *Forbids* `layer_rank` (A.4), and any
   successor to it.
3. **A rebuild costs one allocation per node.** → There is no reason to mutate
   retained state. *Forbids* `WidgetMutation`'s fast path (C.4) — and equally
   forbids treating cheap rebuilds as licence to rebuild the root (0.1).
4. **Identity is explicit** — tree position and an author-supplied key. →
   *Forbids* `WidgetInstanceState`'s side table (C.2).
5. **One source of geometry.** Layout computes; hit-testing, painting and tests
   read. → *Forbids* `LayoutBox` (C.3) and every recorded rectangle in E.
6. **Composition is the only extension mechanism.** → *Forbids* B.1's
   "mount it as a `Host`" shortcut.
7. **Backend independence.** The display list is the seam. → *Forbids*
   `Paints::HostsOnly` (D.3).

### The practice rules (0.4)

§4.7 committed M0 to writing these *before* M1 and they were never written;
this is that debt, paid from what nine migrated surfaces actually taught. They
are stated as rules because the failure each one prevents is silent — the tree
still renders, and the only symptom is that a retained tree costs what an
immediate-mode one costs.

1. **A description that a frame did not change should be the same `Rc`.**
   The reconciler short-circuits on `Rc::ptr_eq`, and today it never fires:
   `frame_tree(shell.clone())` builds every closure fresh, so every element is
   re-reconciled on every PTY tick. `.shared()` on a subtree whose inputs did
   not change is what makes the short-circuit reachable. *Prevents:* paying
   cold-rebuild cost (163µs) on a frame that changed one cell.
2. **State that exists only because something is on screen belongs to the
   element; state the editor acts on stays on the editor.** §4.3's rule, and
   the only one that reliably sorts a case. A list's scroll offset, a tree's
   expansion, a field's cursor — element. A buffer's contents, a window's
   splits, anything a command mutates — editor. *Prevents:* both failure
   directions — model state trapped in a component where no action can reach
   it, and view state on the editor that every surface must remember to reset.
3. **A value read by a node is an ambient; a value read by anything else is a
   parameter.** The correction 0.2 records. *Prevents:* the reflex that made
   `provide` look like the answer for six fields none of which a node reads.
4. **Identity is declared where the thing is, and a scope boundary is one
   node.** `key()` at the position that owns the identity, and
   `fresh_ui::scope` where a key and a `PersistenceScope` must travel together
   — never one without the other. *Prevents:* 0.5's cross-window match, and
   its mirror image, a scope with no key that silently gives two documents one
   element.
5. **Measure against the benchmark before optimising against the model.**
   A whole frame is 122µs retained and 163µs cold. Anything justified as "too
   expensive to rebuild" is claiming a number; the number exists. *Prevents:*
   C.4's mutation fast path being kept on intuition, and its opposite — a
   `.shared()` cache guarding something cheaper than the guard.

**And one rule about layers, learned the same way.** Layers are hit-tested
top down and **the first layer with any path at the point wins** — and a
`PointerMode::Transparent` node still produces a path. So "transparent" means
*the hit continues behind me within this layer*, not *across* layers: a
decorative layer laid over a claim-everything one does not fall through to it,
it swallows the press and nothing handles it. A layer therefore has to say what
a press anywhere on it means, or not be a layer. `PointerMode::Ignore` does not
rescue this either — it skips the node's whole subtree, so a container using it
takes its own buttons out with it.

C.6 shipped with exactly that mistake and its own tests caught it, which is the
argument for writing the tests at the same time rather than after: the panel's
frame was a transparent layer above the modal's claim, and every press on the
box's chrome and content area went nowhere.

And one rule of this migration's own, learned the expensive way and worth
keeping at the top: **the tree runs first, so anything that used to sit between
it and the legacy walk now sits behind whatever the tree claims.** Three things
broke on it — the right-click that clears the tab menus, the terminal's own
mouse, and the smooth-scroll walk — and each was silent, because "it still
compiles and mostly works" is what a routing change looks like from outside.

**How we will know it is finished.** `app/chrome/` is gone. `KeyContext`,
`PointerGrab`, `layer_rank`, `Paints` and `shell_pointer_event` are deleted.
`WindowLayoutCache` holds only what paint alone can know. There is one display
list with two consumers. No file contains a list whose order is precedence.
And no UI state is reachable from two windows at once — the rule
`Window::authority` already states, applied to the tree.

---

## IV. Already done

Nine surfaces, the whole pointer walk, and the six gaps below. The stage and
wave tables in the appendix (`S1`–`S5`, `M0`–`M9`) carry the detail.

| Gap | What it was |
|---|---|
| **G2** | The prompt line took its rectangle from the tree and painted outside the fold — a third arrangement beside "native" and "`Host`". |
| **G3** | A pane's split controls were two recorded rect lists (the painter's running `cx`). |
| **G4** | Two `impl Window` methods still scanned `split_areas` for the pane under a cell. |
| **G5** | `tab_drag` guessed the strip's row as `content_rect.y - 1`, "assuming 1 row for tabs" — wrong for every pane with no strip. |
| **G6** | Two different things were named `editor_content_area`. |
| **G1 (part)** | — still open; see A. |

**Three things those closures turned up that no plan predicted**, kept here
because each is a shape that will recur:

- **A caret can ride a second channel too.** The prompt placed its own with
  `frame.set_cursor_position`, and the deferred commit skipped whenever any
  prompt was up so the buffer's could not override it. Two channels, resolved
  by a guard. One channel resolves it by paint order.
- **`gesture()` wraps its child.** Appending the control cluster to the tab
  strip made a sibling of the row, and every child landed at the strip's
  origin. Structure is not decoration: where a node sits decides what it means.
- **"Which pane covers this cell" is not "what would a click hit".** The first
  is containment, the second is `Ui::hit_test`, and they differ exactly where a
  popup covers the cell. The tempting answer was the wrong one.

---

# Appendix — the original survey, target and wave plan

> _Everything below is the document as it was written before the migration
> started, plus the record of how each stage actually went. It is kept for its
> reasoning — the current-state survey in §2 is still the best account of what
> the editor was, and §4's target design is still the target — and it is kept
> at its **original numbering**, because code comments cite it (`§4.4`, `§4.6`,
> `§6.2 item 7`, `S1`–`S5`, `M0`–`M9`). Sections I–IV above supersede its
> forward-looking parts._

## 0. Situation

`fresh-ui` is **built and standalone** (PR #3024 plus the R1–R11 and Part 1c–1e
follow-ups). It is a retained, reconciling UI tree — immutable descriptions →
persistent elements matched by `(type, key)` → render objects holding geometry
and focus — that emits a backend-independent `LayoutSpec` display list. Its only
runtime dependency is `unicode-width`. It has the real `RenderObject` layer, a
retained focus tree, ambients, `Tasks`, layers with modality/dismissal, pointer
capture, and the full widget set (`Button`, `Toggle`, `TextField`, `List`
eager+windowed, `Tree`, `Dropdown`, `RadioGroup`, `Number`, `DualList`). The
demo under `crates/fresh-ui/tests/support/demo/` and `examples/interactive.rs`
exercise every capability against a terminal backend — with one gap that matters
here: no backend has ever drawn real `Draw::Host` content (both folds stub it as
a `▒` fill), and that is exactly the capability this migration leans on (§4.4).

**It is not yet wired into the editor.** No file under `crates/fresh-editor`,
`crates/fresh-core`, or `crates/fresh-plugin-runtime` references `fresh_ui`.
Part 2 has not started. So this is a genuine greenfield adoption, not a
course-correction — which is the good case: the library was finished and frozen
before any surface depended on it.

The editor's UI, meanwhile, is **already halfway to this model on its own
terms** and stopped at exactly the wall `fresh-ui` was built to get past. The
`ChromeComponent` registry, the `Scene` projection, the shared
`LayoutBox`/`hit_stack` primitive, and the derived `overlay_stack()` are a
proto-retained-widget tree. What they lack — real containment, one precedence
order, per-node pointer capture, one focus ring — is precisely what `fresh-ui`
supplies. The migration is less a rewrite than **finishing a refactor the
codebase already committed to** (the forward arc named in
[`chrome-event-model-plan.md`](chrome-event-model-plan.md) and
[`widget-framework-v2-review.md`](widget-framework-v2-review.md)).

---

## 1. The one hard constraint: the keep/migrate boundary

The optimized, file-backed text buffers and the text-rendering pipeline **do not
migrate**. They keep their existing logic and are reached from `fresh-ui` through
a `Host` leaf. The boundary is not fuzzy — it is a single function signature.

### The line is `SplitRenderer::render_content`

`view/ui/split_rendering/mod.rs` exposes (all associated functions, no `&self`):

```rust
pub fn render_content(buf: &mut ratatui::buffer::Buffer, area: Rect, …)
    -> /* per-leaf layout caches: split areas, tab layouts, view_line_mappings,
         scrollbar/separator areas, close/maximize buttons */
pub fn compute_content_layout(…)  // layout-only: the same caches, WITHOUT painting
pub fn render_phantom_leaf(…)     // off-tree previews (palette preview, web slices)
```

`render_content` already paints into an **arbitrary `Buffer` at an arbitrary
`Rect`** ([`rendering-and-layout.md`](rendering-and-layout.md) calls it
composable into any buffer — offscreen previews, tests, and the web bridge).
`compute_content_layout` matters just as much for this plan: it yields the
geometry (`view_line_mappings`, and from them the caret's screen position)
*before* anything paints — the tool for same-frame caret-anchored layers
(§4.4). Everything these touch is *keep*; everything that carves the rect they
are handed and reads the caches they return is *migrate*.

One honest caveat on "untouched": `render_content`'s unit is the **whole split
tree** — separators, tab bars, scrollbars, scroll-sync, composite views in one
call. The target frame (§4.2, M9) wants per-leaf `Host` nodes with `fresh-ui`
tabs and dividers, which requires either keeping the whole grid as one `Host`
leaf or decomposing the orchestration layer per-leaf. That is an open scoping
decision (§6.2), not a settled detail.

| KEEP — behind a `Host` leaf, logic untouched | MIGRATE — onto `fresh-ui` |
|---|---|
| `view/ui/split_rendering/**` (token IR → `ViewLine`; `render_line/**` and its `CellPass` per-char state machine; gutter, folding, conceal, virtual-text, soft-break, scrollbar glyphs, composite view) | `app/chrome/**` (the whole `ChromeComponent` registry) |
| `view/line_wrap_cache.rs`, `view/wrap_index.rs`, `view/wrap_machine.rs` (tier-1/tier-2 wrap caches) | `view/scene.rs` (the `Scene` projections become the tree's data model) |
| `view/viewport.rs`, `view/folding.rs`, `view/conceal.rs`, `view/soft_break.rs`, `view/virtual_text.rs`, `view/margin.rs`, `view/composite_view.rs` | `view/ui/{menu,tabs,status_bar,suggestions,scrollbar,scroll_panel,file_explorer,file_browser,focus,layout}.rs` |
| `EditorState`, `Buffer`, the piece tree, markers, undo (see [`text-model.md`](text-model.md), [`buffers-splits-undo.md`](buffers-splits-undo.md)) | `view/popup.rs` + `view/popup/**`; `view/settings/**`; `view/controls/**`; `view/keybinding_editor.rs`; `view/workspace_trust_dialog.rs` |
| `Viewport` scroll math, `view_line_mappings` (mouse→byte hit-test slices), the per-cell theme-provenance map | The frame layout, dock/sidebar carve, and the modal z-band in `app/render.rs` |

**What crosses the seam.** The text view is not a black box; three of its outputs
are read by chrome and must keep flowing:

1. `view_line_mappings` — per-visual-row `ViewLineMapping` slices used for O(1)
   screen→byte hit-testing and click-to-cursor. Published by the backend fold,
   keyed by `HostId` (§4.4) — or derived pre-paint via `compute_content_layout`.
2. The per-cell theme-provenance map (`CellThemeInfo`) — read by the Scene/web
   projection and the theme inspector.
3. The **caret**. The hardware cursor is committed at end-of-frame and arbitrated
   against late overlays (`cursor_suppressed_by_late_overlay`). `fresh-ui` has
   `LayoutSpec.cursor` (a `TextField` sets it) — but that field is fixed during
   the library's paint pass, while `render_content` computes the buffer caret at
   *fold* time, after the spec is finalized. The frame therefore needs one "who
   owns the caret" arbitration that merges the host caret outside the library's
   cursor mechanism, or derives it early via `compute_content_layout` (§4.4).

**Ordering the seam must preserve.** Today geometry is "painted, then projected":
`render_content` writes the layout caches *during* paint, and chrome/Scene read
them *after*. `fresh-ui` inverts this for its own tree — layout is a distinct pass
before paint, geometry comes from the layout pass, and **paint never touches
cells** (`render/spec.rs` opens with exactly that sentence): a render object
emits display-list items, and per-cell content has one route across the seam,
`Draw::Host(HostId)`. So the two orderings meet **in the backend fold**, not in
the leaf: the leaf's `layout` reserves the rect and its `paint` emits
`Draw::Host(id)`; the editor-driven fold, walking `LayoutSpec.items` in paint
order after `ui.frame()` returns, maps that item back to `render_content` and
captures the caches editor-side, keyed by `HostId`. Event handlers read those
caches on the next event; same-frame consumers (caret-anchored layers) use
`compute_content_layout` at build time instead. §4.4 spells out the mechanism
and its consequences.

---

## 2. The current UI, as it actually is

This section is the survey the migration needs. It samples the surfaces the
request named — editor/window, menus, file explorer, the widget system, settings,
splits, prompts, the plugin widget API — plus the mouse/keyboard event dispatch
that ties them together.

### 2.1 The frame and the `Editor` object

`Editor` (`app/mod.rs`) is the central object but **not** a buffer owner. Buffers
and splits live on `Window` (`app/window/**`); `Editor` holds a `windows` map and
an `active_window`, and derives the active buffer/splits/explorer/prompt/popups
through accessors (`active_window()`, `active_state()`, `split_manager()`,
`active_chrome()`, `active_layout()`). It also holds the cross-cutting services:
config, theme (`Arc<RwLock<Theme>>`), registries, keybindings, clipboard, the
plugin manager and async bridge, the dock and floating-panel state, and the
widget registry.

`Editor::render(&mut self, frame: &mut Frame)` (`app/render.rs`) is **immediate
mode**: the whole screen is re-derived every frame; the runtime loop decides
*when* to call it. The ordered flow is, in essence:

1. drain pre-layout plugin commands (the one place inside draw that takes the
   plugin lock);
2. `compute_dock_split` → carve the left dock column;
3. animation snapshot; reset the cell-theme map; scroll-sync; request semantic
   ranges for visible splits;
4. a ratatui `Layout` splits the chrome area into
   `[menu_bar, main_content, status_bar, search_options, prompt_line]`;
5. carve the file-explorer sidebar out of `main_content`; render it;
6. **`SplitRenderer::render_content(frame.buffer_mut(), editor_content_area, …)`**
   inside a single `WindowBuffers::with_all_mut` split-borrow — this is the text
   pipeline, and it returns the per-leaf layout caches
   (`split_areas`, `tab_layouts`, `view_line_mappings`, scrollbar/separator
   areas) onto `active_layout_mut()`;
7. paint the chrome on top, in a fixed order: status bar, search options, prompt
   line, prompt/buffer/global popups, menu bar (last), context menus, tab-drag,
   software cursor, deferred hardware-cursor commit, frame-buffer animations;
8. `render_panels_and_modals` — dock, full-screen modals, floating panel, trust
   modal (the topmost z-band);
9. `convert_buffer_colors` (256/16 fallback) over the finished buffer;
   `bump_ui_gen()`.

The whole of steps 2, 4, 5, 7, 8 is chrome composition — the part that migrates.
Step 6 is the `Host` leaf.

### 2.2 Chrome: a registry that is a proto-widget-tree

There **is** a central abstraction, and it is already component-shaped.

- `trait ChromeComponent: Sync` (`app/chrome/mod.rs`) — one ZST implementor per
  surface. Its methods are the parallel dispatch interface: `collect` (contribute
  geometry boxes), `hover`/`on_hover_change`, `on_pointer`, `on_wheel`/`on_hwheel`,
  `capture_mouse`, `on_key` (pre-band grab), `on_layer_key` (the keyboard walk),
  and `layers` (precedence contribution).
- `components() -> &'static [&dyn ChromeComponent]` — **the** registry, 17
  entries: Settings, KeybindingEditor, CalibrationWizard, WorkspaceTrust,
  ThemeInfo, ContextMenu, Prompt, Popups, FileBrowser, FloatingModal, Dock,
  Splits, Menu, FileExplorer, StatusBar, SearchOptions, Base.

Notably, **the text view is itself a component** (`Splits`, contributing a
`chrome:editor` box) and the keyboard/pointer floor is a component (`Base`,
z-0). The editor content is the lowest-precedence participant in the same tree,
not a privileged root — which is exactly the `fresh-ui` stance (no privileged
internal surface) and makes the mapping natural.

Per-surface, the pattern is uniform: `collect` reads a geometry rect (from a
paint cache or a live derivation) and pushes a kind-tagged `LayoutBox` stamped
with the component's registry index; the handlers delegate to `impl Editor`
methods that hold the real behavior and mutate `Editor`/`Window` fields. The
components are stateless; **all state is on `Editor` or `Window`.**

### 2.3 Two decoupled precedence systems

This is the single most important structural fact for the migration, and the
thing `fresh-ui` collapses. Pointer stacking and keyboard/modal precedence are
**two separate orderings that deliberately disagree**:

- **Pointer z** — each `LayoutBox` carries a `z` on an ×10 band scheme (context
  menu ~180 … tabs 60 … scrollbars 50 … status 40 … editor 10 … base 0). The
  pointer walk is `hit_stack` (effective-z desc, then depth desc, then document
  order), with registry order as the intra-band tiebreak.
- **Keyboard / modal rank** — a *separate* hand-tuned constant table,
  `chrome::layer_rank`: `SETTINGS=900 … MENU=860 PROMPT=850 POPUP=840
  CONTEXT_MENU=830 FLOATING_MODAL=820 DOCK=810 EDITOR_BASE=0`. Each component
  contributes `(rank, Layer)` via `layers()`; `Editor::overlay_stack()`
  concatenates all contributions (plus a hardcoded EventDebug head at 1000) and
  stable-sorts rank-descending into **the** single ordered `OwnedLayer` list.

`overlay_stack()` is consumed by the keyboard walk, the mouse-capture band, the
PTY-input gate (`presents_blocking_overlay`), `modal_overlay_active`,
`popup_blocked_by_higher_modal`, and `get_key_context`. The two orderings
intentionally diverge (e.g. a menu's *keyboard* layer outranks the prompt, but
its *boxes* sit in a lower pointer band; context menus rank below popups for
keyboard but their boxes sit at the top pointer band), and the relationships are
pinned by tests. Precedence is therefore **data spread across 17 `layers()`
impls and two constant tables**, not a property of a tree.

### 2.4 Geometry: two sources, one deliberate seam

Chrome geometry comes from two places, and a migration underway (slice 7 of the
chrome-event-model plan) is moving surfaces from the first to the second:

- **Paint-recorded caches** — `ChromeLayout` (editor-global) and
  `WindowLayoutCache` (per-window splits/tabs/explorer). `render_content` and the
  popup painters write rects here *during* paint; `collect` reads them at event
  time.
- **Live-derived** — `status_bar_layout_now()`, `menu_layout_now()` recompute
  geometry from state at event time, and the paint pass debug-asserts paint ==
  derivation. Their retired cache fields are gone. This class is itself a
  waypoint, not a destination: a live derivation is still a *second* spelling of
  the paint walk's arithmetic, kept honest by an assertion that release builds
  compile out. A migrated surface has neither — the tree lays it out once and
  everyone reads the result. `search_options_layout_now()` was the first to go
  that way (§6.2); `Editor::search_option_spans_now()` reads its spans back off
  the laid-out tree instead.

A subset **stays paint-recorded by explicit ruling**: `popup_areas`,
`global_popup_areas`, the prompt suggestions/toolbar/preview rects, the file
browser layout, the workspace-trust dialog rect, and the floating-panel paint
fields. The reason is real and constrains the migration: these anchor to
**paint-produced text layout** — the cursor's screen position, the wrap maps. Any
`fresh-ui` tree that positions a completion popup at the caret must therefore be
able to read the caret's post-layout screen position out of the `Host` leaf. This
is the §1 "what crosses the seam" requirement seen from the other side.

### 2.5 The `Scene` projection

`view/scene.rs` is **not** a retained scene graph — it is a set of
`Serialize`-deriving **semantic projections** computed once per frame from
`Editor` state plus the last-frame geometry caches: `MenuView`, `TabBarView`,
`StatusView`, `PaletteView`, `ScenePopup`, `FileExplorerView`,
`FileBrowserView`, `TrustDialogView`, `WidgetSurfaceView`, `ContextMenuView`,
`AuxModalView`, `KeybindingEditorView`. It is the single source of truth for
*what the chrome is* (which items/tabs/rows exist, enabled/checked, and their
rects), and it is consumed by both the TUI painter and the web frontend. It
deliberately **excludes buffer text** — the web bridge slices *rendered cells*
out of the framebuffer for the buffer and preview panes (the `PaletteView`
preview rect is exactly such a slice).

This is a gift to the migration: `Scene` is already "the description of the
chrome, minus geometry, minus text." It becomes the props the `fresh-ui`
components read. It is also the parity oracle — `scene_parity.rs` is the check
that the web projection has not diverged, and it must keep passing through every
wave.

### 2.6 Event dispatch — keyboard

Entry `app/input.rs`, `handle_key_press` → `handle_key`. Ordered pre-band stages,
then the derived walk:

1. `bump_ui_gen()` (invalidate per-event memos);
2. event-debug intercept;
3. `dispatch_terminal_input` (terminal mode, `terminalBypass`, scrollback) —
   short-circuits if `presents_blocking_overlay()`;
4. `try_resolve_next_key_callback` (plugin `getNextKey()`);
5. **pre-band grabs**: `for c in components() { c.on_key(...) }` — by ruling only
   two members participate (ThemeInfo as observer, ContextMenu as a
   custom-dispatch modal);
6. transient-popup dismissal observer;
7. **the walk**: `dispatch_layer_keyboard` walks `overlay_stack()` top-down,
   offering the key to each layer's owner via `on_layer_key`; first `Some` stops.
   `Base` always answers, so the walk always terminates.

The pipeline **tail** — mode bindings → composite router → chord/keybinding
resolution — lives in `Base`'s handler, reaching the keybinding resolver, which
resolves key→`Action` against the `KeyContext` from `get_key_context()`.
`KeyContext` (an enum in `fresh-core/action.rs`) is the current stand-in for a
focus ring: it is *derived* from the topmost keyboard-owning layer, not stored.

Note a deliberate asymmetry: the keyboard side does **not** get the
one-tree-per-event treatment the pointer side does, because key handlers
"mutate then decline" (a popup rung processes `ClosePopup` and falls through),
which forces per-handler `get_key_context()` re-derivation.

### 2.7 Event dispatch — pointer

Entry `app/mouse_input.rs`, `handle_mouse` → `handle_mouse_impl`:

1. **modal capture band** — walk `overlay_stack()` in rank order; the first
   component whose modal is up claims the *whole* mouse channel via
   `capture_mouse`. (This replaced a deleted `dispatch_modal_mouse` ladder.)
2. pre-walk observers (LSP-rename cancel, GPM cursor);
3. **terminal forwarding gate** — suppressed when a `pointer_grab` is active, a
   context menu is open, or an opaque chrome box covers the point;
4. **one chrome tree per event** — `chrome_tree(self)` collects every component's
   boxes, validated-memoized on `(ui_gen, overlay_stack)` with a debug oracle
   that rebuilds and compares;
5. dispatch: `dispatch_pointer` for presses, `dispatch_wheel` for scroll. Both
   walk the `hit_stack`; `dispatch_pointer` dedups on `(owner, kind)` and honors
   `Disposition::{Consumed, PassAfter, Pass}` (a declining *opaque* box absorbs
   the event); `dispatch_wheel` has **no** opacity gate and **no** dedup, so a
   declining surface lets the wheel keep falling for scroll-chaining.

Fine click geometry is pure and `Editor`-free (`app/click_geometry.rs`):
`screen_to_buffer_position*` maps (col,row) + content rect + gutter +
`ViewLineMapping` → buffer byte + virtual-space overshoot.

### 2.8 Drags, hover, focus — the three ad-hoc clusters

These are the three places the flat tree cannot express what it needs, and each
is a hand-rolled substitute for a `fresh-ui` primitive:

- **Drags → `PointerGrab`.** `pointer_grab(ed)` is a hand-ordered match over ~13
  drag flags scattered across `Editor` and `mouse_state` (`dock_resizing`,
  `widget_text_drag`, `dragging_scrollbar`, `dragging_horizontal_scrollbar`,
  `selecting_in_popup`, `dragging_prompt_scrollbar`, `dragging_popup_scrollbar`,
  `dragging_separator`, `dragging_file_explorer`, `terminal_drag_pending`,
  `dragging_text_selection`, `dragging_tab`, …). The grab owns the pointer from
  press to release regardless of what is under it. This is exactly what per-node
  `cx.capture_pointer()` replaces — one slot, owned by the node that started the
  drag.
- **Modal capture → `capture_mouse`.** Full-screen modals (Settings, keybinding
  editor, calibration wizard, workspace trust) and the floating modal contribute
  **no boxes**; they swallow the entire mouse channel before the walk, and each
  has a bespoke `handle_*_mouse`. This is what `Modality::Exclusive` + a
  `FocusScope` replaces.
- **Hover** is two disjoint systems: a chrome hover-target walk
  (`update_hover_target`, memoized, offering enter/leave transitions to every
  component) and content trackers kept **outside** the walk on purpose — the LSP
  hover state machine and terminal-link hover, because their debounced
  request/keep-alive state cannot be expressed as an enter/leave diff. In
  `fresh-ui`, hover is framework state on the render object; a component
  *mirrors* it via `on_enter`/`on_leave` (the G1 finding in the implementation
  plan). The content trackers stay as they are — they live behind the `Host`
  leaf.

**Focus** today is not one thing: keyboard focus is the derived `KeyContext`;
"which split/buffer is active" is `Window` state guarded by `set_active_buffer`
and `focus_split` (with a pane-buffer invariant that once caused a panic);
per-popup `focused` flags; per-panel `widget_registry.focus_key`; and the
overlay-toolbar focus ring derived from layout boxes. `fresh-ui` unifies all of
these into one focus tree with scopes and a traversal policy.

### 2.9 The command → action → event pipeline (unchanged by this work)

Three deliberately separate vocabularies, and **all three survive the
migration**:

| Layer | Type | Role |
|---|---|---|
| Command | `Command` (`fresh-core/command.rs`) | user-facing, localized, context-filtered palette entry |
| Action | `Action` enum, ~230 variants (`fresh-core/action.rs`) | the rebinding & serialization currency; executed by `handle_action` |
| Event | `Event::{Insert,Delete,MoveCursor,BulkEdit,…}` | the buffer-mutation, undo, and plugin-hook unit |

The separation exists for rebindability (Actions round-trip to strings; Events
are position-specific and would not replay), undo/hooks (Events are the
transaction record), and layout-independence (`MoveUp` resolves to a concrete
`Event::MoveCursor` late). `fresh-ui` handlers return **messages** — but the
message type cannot simply *be* `Action`: `Action` deliberately has no
positional variants (no click-at-byte, no drag-to-offset, no
tab-select-(leaf, index)) precisely because it is the serializable rebinding
currency. The tree's message type is therefore a wrapper —
`enum UiMsg { Action(Action), /* UI-geometry messages */ }` — with the
UI-geometry half consumed by `update` and never entering the keybinding
namespace. (The library's own demo makes the same choice: its message type is a
bespoke `Msg`, not a reused action enum.) The Shortcuts → Intents → Actions
chain sits *in front of* the pipeline as before: a key on the focus chain
resolves to an `Intent`, an `Intent` to an `Action` at the focused node, and
from there the existing `handle_action` pipeline is unchanged. This is the
single most important "keep" on the input side. One more keep: fresh-ui's
static `Intent`/`Shortcut` tables cannot absorb the user-rebindable, chorded,
multi-keymap resolver — the realistic shape is that the resolver survives as
the root fallback key handler (today's `Base` tail), with per-node shortcuts
reserved for widget-local keys.

### 2.10 The plugin widget system

Plugins describe UI as a data tree and the host owns everything else. The wire
type is `WidgetSpec` (`fresh-core/api.rs`) — a serde-tagged, `#[ts(export)]`
**closed enum**, 19 kinds: containers (`Row`/`Col`/`LabeledSection`/`Overlay`/
`Component`/`Popup`), structural (`Spacer`/`Divider`/`HintBar`/`Raw`), controls
(`Toggle`/`Button`/`Number`/`Dropdown`/`DualList`), data views
(`List`/`Tree`/`Text`), and `WindowEmbed` — a rect reserved inside a plugin
panel for the native per-window render path, i.e. host content *inside*
plugin-described UI (it matters for M6; see §4.6). Plugins build it with `plugins/lib/widgets.ts` and call
`panel.set(spec)`, which issues `PluginCommand::MountWidgetPanel` /
`UpdateWidgetPanel`.

Host-side (`widgets/**`, `app/widget_runtime.rs`): one `match` on kind
(`kinds/mod.rs::behavior`) dispatches to a `WidgetImpl` per kind
(`collect`/`box_meta`/`on_key`/`on_pointer`/…). The central rule is
**spec/instance separation**: spec values are initial-only; after first render
host-owned `WidgetInstanceState` (list scroll, selection, tree expansion, text
edit state) is authoritative, keyed by the widget's stable string `key` in a
`HashMap`. Identity/diffing is manual string-key matching carried forward each
render; a `WidgetMutation` fast path (`SetValue`, `SetItems`, `SetExpandedKeys`,
`AppendTreeNodes`, …) mutates in place to dodge re-transmitting a large tree
(the `js_to_json` walk of a 5000-node tree blocks the JS thread ~1s). Events flow
back through the `widget_event` hook, delivered only to the owning plugin, with a
deliberate **one-frame lag** (PluginCommands drain on the next frame).

This is a hand-rolled, string-keyed, side-table version of exactly what
`fresh-ui` is: a retained tree with `(type, key)` identity, element-owned state,
and a reconciler. The `WidgetInstanceState` map is the "side-table problem" the
design doc's Appendix A calls out by name.

### 2.11 Settings and the controls library

Settings is **mid-unification** and instructive. Schema drives it: schemars runs
offline over the config struct, the committed `config-schema.json` is parsed into
a `SettingCategory` tree, and `build_pages` turns each entry into a
`SettingControl` (10 variants: Toggle, Number, Dropdown, Text, TextList, DualList,
Map, ObjectArray, Json, Complex). `x-` schema extensions carry UI hints
(`x-enum-from: "$themes"` pulls live theme options, etc.).

The important part is what has and has not been unified:

- **Rendering is already on the widget framework.** There is no `Control` trait
  and no per-control paint code — `view/settings/widget_map.rs` projects the full
  control state into a `WidgetSpec` every frame and renders through
  `widgets::render_spec`. The old `view/controls/*/render.rs` paint modules are
  **deleted**.
- **State, input, layout, and theming remain bespoke.** Each control keeps a
  `*State`/`*Colors`/`*Layout`/`*Event` module with hand-written
  `handle_key`/`handle_mouse`, and `view/settings/input.rs` routes keys through
  per-control editing handlers. So the same widgets have **two state stores**
  (the `SettingControl` states *and* a bridge `HashMap<key, WidgetInstanceState>`)
  and `widget_map.rs` re-seeds one from the other every frame.
- **The keybinding editor is a third, entirely separate hand-rolled modal** —
  table + search + edit dialog — that uses none of `view/controls`,
  `SettingControl`, or `WidgetSpec`.

The duplication a `fresh-ui` migration removes here is concentrated in the
control *state/input/layout/theming* layer, not rendering — and it folds three
systems (controls, plugin widgets, keybinding editor) into one.

---

## 3. Why migrate — the shared root cause

Every recurring UI bug class in the survey traces to one root, stated in
[`widget-framework-v2-review.md`](widget-framework-v2-review.md): **the tree is
flat.** No component sets `LayoutBox.parent`; `focusable`/`focus_trap`/`scroll`
are reserved and unset at chrome level. Containment is faked, precedence is
tabulated, drags are a flag ladder, modals bypass the walk. Concretely:

1. **~10 full-frame "guard" boxes** simulate "outside my rect" containment —
   `menu_close_guard`, `context_menu_close_guard`, `dock_blur`, `transient_guard`,
   `popup_guard`, `clear_explorer_menu`, `tab_menu_clear_guard`, and the prompt's
   **five** per-gesture full-frame boxes. Each is a real parent/clip/modal node's
   job, done by hand.
2. **Two precedence orderings** (pointer z-bands + `layer_rank`) that a real tree
   expresses once as tree order + stacking contexts + `Modality`.
3. **The `PointerGrab` flag ladder** — ~13 drag flags — that one per-node pointer
   capture replaces.
4. **`capture_mouse` modals** that bypass the walk, replaced by a focus-trap node
   + scrim.
5. **String-keyed side tables** (`WidgetInstanceState`, the Settings bridge map)
   that element identity replaces.
6. **Two/three parallel control vocabularies** (chrome, plugin widgets, settings
   controls) that one widget set replaces.

The design doc's Appendix A ("What this replaces in Fresh today") is the full
mapping; the survey above is where each row of it actually lives.

---

## 4. The target design — the whole UI as one `fresh-ui` tree

The end state is one `fresh-ui` description tree, rebuilt each frame from `Editor`
state, with the buffer/terminal panes as `Host` leaves. `Editor::render` stops
being an immediate-mode painter and becomes: build the tree, hand it to
`ui.frame(build(editor), size)`, fold the returned `LayoutSpec` into the ratatui
`Buffer`. Input becomes: translate the terminal event to `fresh_ui::Input`,
`ui.dispatch` it, apply the returned messages — `Action`s plus UI-geometry
messages (§2.9) — through the existing pipeline.

### 4.1 The root tree

Sketch (message type `UiMsg`, the `Action` wrapper from §2.9), mirroring the
demo's shape and the design doc's §15 examples:

```rust
fn build(ed: &Editor) -> Node<UiMsg> {
    provide(&THEME, ed.theme_snapshot(),                     // §4.5
      col().children([
        menu_bar(ed).if_(ed.menu_bar_visible),               // M3
        row().flex(1).children([
            dock_column(ed).w(Cells(ed.dock_width)).if_(ed.dock.visible),  // M9 carve, M6 content
            file_explorer(ed).w(Cells(ed.explorer_width)).if_(ed.explorer_open), // M9
            split_grid(&ed.split_tree()).flex(1),            // M9 — Host leaves
        ]),
        search_options(ed).if_(ed.search_active),            // M1
        status_bar(ed).h(Cells(1)),                          // M1
        prompt_line(ed).if_(ed.prompt.is_some()),            // M5 (bottom mode)
      ])
      // overlays are children of the node they belong to, not a global z-stack:
      .child_if(ed.any_context_menu(), || context_menu(ed))  // M2
      .child_if(ed.palette_overlay(),  || palette(ed))       // M5
      .child_if(ed.any_modal(),        || modal(ed)))        // M7
}
```

The two precedence tables (§2.3) **do not survive**. Precedence is tree order plus
stacking contexts plus `Modality`; a submenu nests as a further `Layer` anchored
to its row; a modal declares `Modality::Exclusive` and everything else falls out.

### 4.2 Per-surface mapping

Each surface maps onto a small, already-built combination of primitives and
widgets. The `Scene` projection (§2.5) is the props each reads.

| Surface | `fresh-ui` expression | Notes |
|---|---|---|
| **Menu bar + dropdowns** (`MenuView`) | `row()` of `Dropdown`s; submenus are nested `Layer`s anchored to their row, `Modality::Inert`, `dismiss(OUTSIDE_POINTER∣ESCAPE)`. Mnemonics are `Intent`s on the root focusable (as in the demo). | Replaces the close-guard box, the dropdown z-number, the rank entry, and the hover auto-switch machine. Hover auto-switch is `on_enter` firing a `Toggle` message while a menu is open. |
| **Context menus** (`ContextMenuView`) | `Layer{ anchor: Point(click), place: Below, fit: FLIP∣CLAMP, modality: Inert, dismiss: OUTSIDE_POINTER∣ESCAPE }` wrapping a `List::keyed(...).autofocus()`. The menu is a child of the node it acts on, so its target is tree position, not stored state. | The demo's `context_menu` is this verbatim. Replaces the four `Window` context-menu structs' *highlight* (element state), the close-guard box, and the pre-band keyboard grab. |
| **Prompt / command palette** (`PaletteView`) | `Layer` (Center overlay or Bottom line) → `FocusScope(col([ TextField, toolbar?, row([ List::keyed(results).selected_id(...), preview? ]) ]))`. Re-key on `prompt_type` to reset editing state. | Query is *controlled* (committed to `prompt_histories`); caret/selection/scroll are element state. Selection stores the result **id**, not an index. Replaces the overlay toolbar ring, the click scrim, the position-blind wheel box, the `SearchPrompt`/`Prompt` context switch, and the manual-scroll latch. |
| **Info/hover/signature popups** (`ScenePopup`) | `Layer{ anchor: Point(caret_screen_pos), place: Below, fit, dismiss: ANY_KEY∣OUTSIDE_POINTER }` over `Viewport(styled markdown body).selectable().max_h(n)`. Non-modal. (There is no `TextRun::markdown` in the library — the body is composed from today's `PopupContent::Markdown(Vec<StyledLine>)`; see the inline-styling decision, §6.2.) | **Anchor needs the caret's pre-paint screen position via `compute_content_layout`** (§1, §4.4). The LSP hover *state machine* stays behind the leaf; only the rendered popup migrates. |
| **Splits / tabs / scrollbars** (`TabBarView`) | `split_grid` recursion: `Leaf → col([ tab_strip, row([ Focusable(Host::buffer(id)).flex(1), vscrollbar ]) ])`; `Split → flex_dir([ a, Gesture(Divider).on_press(capture_pointer), b ])`. | The divider captures the pointer on press — the whole drag mechanism, replacing the separator arm of `PointerGrab`. The active split's border is `focus_within`. Buffers/terminals are `Host` leaves (§4.4). This shape requires decomposing `render_content` per leaf — an open scoping decision (§1 caveat, §6.2). |
| **File explorer** (`FileExplorerView`) | `Tree` (or `List::windowed` over `get_display_nodes()`), selection controlled by `Window` state, `expanded_dirs` controlled (it is serialized). Context menu as above. | The `FileTree` model (lazy `TreeNode`, sort/filter, incremental search, decorations) is app state the `Tree` renders; only rendering/hit-testing/scroll move onto the widget. |
| **Dock + floating plugin panels** (`WidgetSurfaceView`) | The `WidgetSpec` → `Node<Action>` translation (§4.6) mounted in a dock column or a `Layer`. | `WidgetInstanceState` dissolves into element state. This is the plugin-API-visible wave. |
| **Settings** | The schema-driven form built directly from `SettingControl` as `col()` of `Toggle`/`Number`/`Dropdown`/`TextField`/`DualList`/`List`/`Tree`, inside a `Modality::Exclusive` `Layer` + `FocusScope`. | Deletes `widget_map.rs` (no per-frame projection), the dual state store, the bespoke `input.rs` handlers, and `view/controls/*`. The keybinding editor becomes another form in the same modal. |
| **Status bar / ~~search-options row~~** (`StatusView`) | `row().h(1)` of `TextRun`/`Button` segments; already live-derived, so the least coupled. | The M1 warm-up wave. The search-options row is **landed** — a `row()` of `gesture(text_runs(..))` toggles with fixed-width gaps between them, `Press` producing the real `Action::ToggleSearch*` and nothing positional but `UiFact::Hover`. |
| **Full-screen modals** (`TrustDialogView`, `AuxModalView`, `KeybindingEditorView`) | `Layer{ anchor: Screen(Center), modality: Exclusive, scrim: Dim, dismiss }` + `FocusScope`. | `Modality::Exclusive` subsumes whole-channel capture, `blocks_terminal_input`, and the hover/cursor suppression lists as one property. |

**Inline styling is an unsolved decision that cuts across this table.**
`TextProps` is a single unstyled `Rc<str>`, and a display-list `Item` carries
one `ThemeKey` — while today's chrome is full of intra-line styling: palette
fuzzy-match highlights, menu mnemonics, markdown popups, settings search
highlights, git-status coloring in explorer rows. Either the library grows
styled spans (a one-time library change, decided before M3), or the editor
composes one `TextRun` node per styled span and accepts the node count — under
a cell-identical acceptance bar. Decide before M3 (mnemonics) and M5 (match
highlights); see §6.2.

### 4.3 State homes

Every wave begins by classifying the surface's fields into four homes. This is
the discipline the implementation plan §6 sets out; the survey lets us fill the
column concretely.

| Home | Owner | Editor examples |
|---|---|---|
| **App state** (prop, passed down) | `Editor`/`Window` | which menu is *present*, which plugin/spec a panel mounts, `dock_width`, `menu_bar_visible`, the `SettingControl` values being edited |
| **Element state** (disposed with the widget) | the element | menu highlight, context-menu highlight, prompt scroll/caret, popup scroll, theme-info popup, dropdown open flag |
| **Framework state** (render objects) | `fresh-ui` | one focus position (replaces `key_context`, `dock.focused`, popup `focused`, `Prompt.toolbar_focus`), all `PointerGrab` drag flags (→ pointer capture), hover, multi-click detection |
| **Session state** (serialized ⇒ app state) | serde structs read by daemon/workspace/orchestrator | per-split scroll, `tab_scroll_offset`, `expanded_dirs`, `prompt.input` history |

**The invariant that guards this:** if a wave changes `workspace.rs`
serialization, something was misclassified. Persisted view state must be app
state because elements are disposed on unmount and do not survive a restart. The
library's `Persisted<T>` is for **new incidental state only** — it is *not* the
home for Fresh's existing typed, versioned serde view-state, which the daemon and
orchestrator read independently of any UI component. The restore suites
(`workspace_persistence_gates.rs`, `daemon_workspace_restore_parity.rs`, the
`orchestrator_*_restore` tests) are the guard.

Consequence worth stating plainly: **`Editor` gets smaller.** Most of its UI
fields are view state, so a wave mostly *deletes* fields — the god-object shrinks
as surfaces move to element/framework state.

### 4.4 The `Host` seam — where the text pipeline plugs in

The library's rule is that **paint never touches cells**: `RenderObject::paint`
has the signature `fn paint(&self, g: Geom, out: &mut DrawList)` and emits
display-list items; per-cell content has exactly one route across the seam,
`Draw::Host(HostId)` — "content the host owns and draws itself." The mechanism
is therefore split in two, and who-calls-whom matters:

- **The leaf** (`BufferHost`, on the exported `HostLeaf`/`RenderObject` path,
  closed in R2/D12): `layout` reserves the rect; `paint` emits `Draw::Host(id)`
  and nothing else; `hit` returns `Hit::Opaque` — hit results are *not*
  messages ("layout, paint and hit-testing never see a message");
  `takes_raw_input` reports PTY ownership so `Modality::Exclusive` above it
  derives suppression via `raw_input_leaves()` instead of the current
  `blocks_terminal_input` flag.
- **The fold** (editor-side, after `ui.frame()` returns): walking
  `LayoutSpec.items` in paint order, a `Draw::Host(id)` item calls back into
  the editor — *this* is where the `WindowBuffers::with_all_mut` disjoint
  borrow is taken and `render_content` runs into the shared cell buffer at the
  item's rect. The returned caches (`view_line_mappings`, cell-theme map,
  pending hardware cursor) are stored editor-side keyed by `HostId`. The
  library never sees them and never owns the buffers.

Consequences the plan must own:

- **The `Ui` cannot live on the `Editor`.** The fold reads the display list off
  the `Ui` while calling back into `&mut Editor`; if the `Ui` were a field of
  `Editor`, those borrows would conflict and the callback could not take the
  `with_all_mut` split. `Ui` and `Editor` are siblings, as in the library's
  tutorial — so `Editor::render(&mut self, frame)` becomes a free function or a
  method taking the `Ui` as a parameter. Asserted at compile time by
  `_the_ui_must_not_live_on_the_editor` in `app/shell_host.rs`.

- **Click→byte is an element-level concern.** A `Gesture` wrapping the host
  node maps the event position through the fold-published mappings and
  `click_geometry` in its handler, emitting a `UiMsg`; the render object itself
  cannot.
- **The caret arrives after the spec.** `LayoutSpec.cursor` is fixed during the
  library's paint pass, but `render_content` computes the hardware cursor at
  fold time. Either the frame merges the host caret outside the library's
  cursor mechanism (fold output wins for buffer carets), or geometry is derived
  early via `compute_content_layout` — which caret-anchored popups need anyway
  for same-frame anchoring.
- **The fold-callback API** — a per-`HostId` callback over `&mut Editor`, run
  mid-fold in paint order — is the concrete API the whole seam hangs on. Both
  of the library's own backends stub `Draw::Host` as a `▒` fill, so it had to
  be built here; it now exists in prototype as `view::shell::fold::HostPainter`
  with `impl HostPainter for Editor` calling `render_content` (§5.0).
- **A relayout footgun**: `HostSpec::Leaf` compares by factory-`Rc` pointer
  equality, so a `build()` that allocates a fresh leaf closure each frame
  forces relayout of every host leaf every frame. Host factories must be
  hoisted (`Shared`/cached fields), and the M0 prototype should assert it.

### 4.5 Theme integration

`fresh-ui` says only *where* appearance comes from: every `Item` carries a
`ThemeKey` string, and the backend maps it to colors (the demo's `style()` fn).
The editor already has a rich theme system (`view/theme`, syntect category
mapping, live preview) and a per-cell theme-provenance map. Integration is: the
TUI backend maps `ThemeKey → resolved Theme colors` (the same lookup
`*Colors::from_theme` does today), the theme is an **ambient** (`provide(&THEME,
…)`) so a theme change dirties only its dependents rather than forcing a root
rebuild — a benefit realized only where subtrees are hoisted into `Shared` or
components, since under a naive whole-root rebuild-per-frame the root rebuilds
regardless (§4.7) — and buffer text keeps its own per-cell theming inside the
`Host` leaf untouched. One granularity note: today's theme inspector reads a
per-*cell* provenance map; migrated chrome carries one `ThemeKey` per
display-list *item*, so chrome provenance is synthesized at fold time at item
granularity (buffer cells keep the per-cell map via the leaf) — confirm the
inspector survives the coarsening (§6.2). `convert_buffer_colors` (256/16 fallback) stays a post-process over
the folded cell buffer.

### 4.6 The plugin boundary

A plugin already sends a whole description tree; that *is* layer 1 crossing a
wire (design §13). The migration keeps the wire type `WidgetSpec` where it is
(`fresh-core`, unchanged for compatibility) and adds a **host-side translation**
`WidgetSpec → Node<Action>` in `fresh-editor` (the M6 wave). The reconciler moves
host-side, so `WidgetInstanceState` (list scroll, tree expansion, selection)
becomes element state — a plugin re-sending its spec no longer loses scroll.

**And that is the only externally visible change.** An earlier draft listed a
second one — *keyed builders require a key function*, breaking every
`widgets.ts` `List`/`Tree` call without keys, shipped a release ahead behind a
load-time deprecation warning. That was wrong, and §5's M6 row records why:
per-row state is held by *index*, not by row key, so unkeyed rows have nothing
to lose. `WidgetSpec` is frozen. Every plugin that works today goes on working,
unchanged, and the audit that warned about unkeyed widgets is deleted.

- **State survival changes.** A plugin that compensated for state loss on re-send
  now sees state persist. Changelog item.

The plugin vocabulary stays a **stable subset** of the internal one (no `Host`,
no `Modality::Exclusive`, no focus policies, no arbitrary `M`), versioned with a
`.d.ts`, so the internal vocabulary can evolve without breaking plugins. One
kind needs care under that rule: `WidgetSpec::WindowEmbed` *is* host content
inside plugin UI. The subset still exposes no `Host` — the host-side
translation maps `WindowEmbed` to a nested `Host` leaf itself, so the
translation, not the plugin, names the leaf.

### 4.7 Rebuild cost and frame scheduling

The sketch above has `Editor::render` rebuild the whole chrome description from
`Editor` every frame. That is the *opposite* of the library's cost model, which
bounds rebuilds by pushing state down, hoisting invariant subtrees into
`Shared`, and letting ambients dirty only their dependents; a whole-root
rebuild per frame makes the identity short-circuit inert (fresh closures every
build) and re-reconciles every element on every PTY tick. At chrome scale
(hundreds of nodes) that may well be fine — but it is an assumption, not a
measurement, and it inverts the model the library documents. M0 therefore
includes a benchmark — full chrome build + reconcile at a sustained input rate
(≥60 events/s) — and the practice rules that fall out of it (which subtrees
must be `Shared`, which state moves into components so dirty marks stay deep,
host-factory hoisting per §4.4) are written down before M1.

---

## 5. The migration plan

This refines Part 2 of
[`widget-library-implementation-plan.md`](widget-library-implementation-plan.md)
(the deletion ledger and verification strategy hold as written) with the
concrete current-state findings. The one-implementation-at-a-time rule is
unchanged, and so are the wave *contents*.

**The acceptance test needs a third criterion.** Cell-identical output and
pointer parity are necessary and they are not sufficient: the dropdown chain
passes both while its description carries a rect, a pre-chosen width, a
pre-decided item count and strings already fitted to a width nothing measured.
A surface that passes those two has moved its *pixels* into the tree; it has
not necessarily moved its *content model*. So the bar is:

1. cell-identical output,
2. pointer and keyboard parity, and
3. **the tree measures this surface** — no rect, width or fitted string reaches
   the description pre-computed.

Where a wave meets 1 and 2 but not 3, say so rather than calling it migrated.
Today: the **search-options row meets all three** — the first surface that does,
and the reason it was scheduled ahead of the file explorer despite being the
smaller piece; the status bar has not started; the **menu-bar dropdowns meet 1
and 2 but not 3** (§6.2 item 5); the context menus meet all three except that
their opening point is handed in; the frame's regions meet 3 by construction,
being `Host` leaves whose only description *is* a rectangle the tree assigns. What does change is the **direction**: §5.0 argues — and a PoC
demonstrates — that the frame should migrate **first**, not last, which
reverses M0's mount point and dissolves M9 into the stages that follow. The
wave table in §5.2 is kept because its per-surface deletions are still the
work; read it as *what* each step deletes, with §5.0's stages as *when*.

### 5.0 Direction — outside-in, and why it inverts the original order

There are two ways to sequence this, and the choice matters more than the
wave contents.

- **Inside-out** — what the implementation plan's Part 2 assumes, and what the
  wave table below inherits: mount `fresh-ui` subtrees into rectangles carved
  by today's frame, migrating leaf surfaces first and the frame **last** (M9).
- **Outside-in** — make the frame a `fresh-ui` tree **first**, with every
  region below it a `Host` leaf that the fold paints using today's painters,
  then replace regions with native descriptions one at a time. The buffer is
  the last `Host` leaf standing.

**Outside-in is the better order**, for three reasons that all follow from the
code rather than from taste:

1. **It is the library's native shape.** `Ui::frame(root, size)` takes a
   whole-frame `Size`; there is no sub-rect mount. Inside-out has to invent one
   (M0 item 4), build every wave on it, and then delete it at M9. Outside-in
   uses the API as designed and invents nothing.
2. **Overlays get real semantics on day one.** A `Layer`'s stacking, modality,
   dismissal and focus scoping are properties of *being a child of the tree*.
   Mounted into a rectangle, a migrated context menu still needs the old
   precedence machinery for everything outside that rectangle — so M2's
   mechanisms are half-simulated, then redone once the frame inverts. Under the
   shell, the first migrated overlay is a real `Layer` immediately. That is
   where the value of this migration actually is (§3).
3. **The hybrid precedence rule already exists.** Today `Base` is the floor —
   pointer z0, `layer_rank::EDITOR_BASE` 0, "everything not otherwise
   claimed". The shell's legacy `Host` leaf *is* that floor. Migrated surfaces
   sit above it as ordinary tree children, and the legacy region shrinks
   monotonically. No new precedence concept is introduced during the
   transition, and the two tables (§2.3) die at the end rather than being
   emulated throughout.

The costs are real but bounded: the fold callback and caret merge are needed on
day one (already M0's hard part — earlier, not extra); dispatch runs in three
stages while the transition lasts (legacy capture band → `fresh-ui` dispatch →
legacy floor), collapsing to one when the last region migrates; and legacy
overlays painted after the fold are invisible to `fresh-ui` hit-testing, so the
legacy capture band — which today already arbitrates exactly this — stays in
front until overlays migrate.

**What does not change:** the wave *contents* (M1–M8) and the deletion ledger.
Only the frame's position moves, from last to first.

#### The PoC, and what it found

The whole shell rests on one testable claim: **`fresh-ui` can reproduce the
editor's frame rectangles exactly.** If it cannot, every region shifts and the
cell-identical bar is lost at step one. `crates/fresh-editor/tests/ui_shell_frame_parity.rs`
tests it: it builds the frame skeleton as a `fresh-ui` description, folds it to
`Draw::Host` items, and compares the rectangles against the ratatui `Layout`
calls `render.rs` actually makes — across 192 visibility combinations
(menu / status / search-options / prompt-line / dock / explorer on either side)
crossed with a grid of terminal sizes.

Two findings:

1. **Parity holds wherever the visible fixed rows fit** — exactly, including
   the dock and sidebar carves, at every width band from 1 column to 200. The
   frame layout is five `Length(n)` rows and one `Min(0)`, which
   `Sizing::Cells` and `flex(1)` reproduce without rounding. The shell can
   therefore take over the frame with no visible change.
2. **One divergence, in the deep-squeeze band.** When the visible fixed rows
   *cannot* all fit (frame height below the number of one-row regions), both
   engines give the content row nothing and drop a row — but a *different* one:
   ratatui's solver starves an interior row and keeps the last; `fresh-ui`
   fills in order and starves the last. `render.rs` flags this band on purpose
   ("running the actual split … keeps small-terminal squeeze behavior identical
   by construction").

#### The fold, prototyped

The frame parity above proves the *layout*. The other half — the fold, where
the backend meets `Draw::Host` — is prototyped in
`crates/fresh-editor/src/view/shell/fold.rs` (display list → cells, with a
`HostPainter` callback) and `crates/fresh-editor/src/app/shell_host.rs`
(`impl HostPainter for Editor`, calling `render_content`). Four things it
establishes:

- **Paint order is preserved across the seam.** Host regions are painted
  *inline* as their item is reached, so a chrome item later in the list lands
  on top — the popup-over-a-buffer case. A fold that collected host items and
  painted them in a second pass would invert this; the test
  `chrome_painted_after_a_host_lands_on_top_of_it` pins it.
- **The borrow works, on one condition.** `with_grid` assembles everything a
  pane's paint needs — the `WindowBuffers::with_all_mut` disjoint split, the
  theme read-guard, the config bundle — from `&mut Editor` *inside* the
  callback, while the display list being folded is borrowed from the `Ui`.
  That type-checks only because **the `Ui` does not live on the `Editor`**:
  `ui.spec()` borrows the `Ui`, the callback borrows the editor, and the two
  must be separate objects. This is the arrangement the library's own tutorial
  uses (`app` and `ui` side by side in `main`), and it is now a compile-time
  assertion (`_the_ui_must_not_live_on_the_editor`) rather than a claim.
- **The caret rule falls out instead of being listed.** `LayoutSpec.cursor` —
  set by a focused native `TextField` — wins over a caret a host region wrote
  through the `pending_hardware_cursor` out-parameter. That reproduces today's
  "an overlay's field takes the caret from the buffer" without the
  `cursor_suppressed_by_late_overlay` suppression list: if a native field has
  focus, it set the cursor, so it wins by construction.
- **`impl HostPainter for Editor` is the thing that shrinks.** Every region
  still listed in its `match` is one the old painters own; each stage moves one
  out into a native description, until only `HostRegion::Body` — the buffer and
  terminal grid — is left. That one never migrates.

#### Input, the other direction

`view/shell/input.rs` carries events the other way: the crossterm key and mouse
events `Editor::handle_key_press` and `Editor::handle_mouse` already receive,
translated into `fresh_ui::Input`.

The rule that makes the hybrid dispatch of S1 work is that translation is
**lossy in one direction only**. Everything the library understands passes
through faithfully; everything it does not returns `None` and stays on the
existing path. The shell takes what it understands and the legacy floor keeps
the rest, which is exactly the three-stage arrangement §5.0 describes.

Writing this adapter is also what found the library's one real gap, and it is
worth recording how that was decided, because the same judgement recurs. Three
things did not cross the seam. Each was tested against two questions — *is the
concept backend-neutral* (meaningful for a terminal, the web DOM and a test
alike), and *does the editor actually need it*:

| Gap | Verdict |
|---|---|
| **Horizontal wheel** | **Fixed in the library.** The scroll model was already two-dimensional (`ViewportProps::scroll` is a pair, `ScrollInfo::max` is a `Point`) but `Input::Wheel` had no axis and `scroll_chain` only moved `y` — an axis implied by the geometry everywhere except where it could be driven. The editor genuinely uses it (`ScrollLeft`/`ScrollRight`, a whole `on_hwheel` arm). Closed by the wheel-axis change this migration is stacked on. |
| **Keys with no counterpart** (`Insert`, media keys) | **Left alone.** Tested empirically rather than assumed: every key token bound by the shipped keymap was diffed against `KeyCode`, and the gap is empty. `KeyCode` is deliberately an abstract vocabulary, not a mirror of crossterm; widening it preemptively invites terminal specifics to leak. Grow it when a real binding needs it. |
| **Key kinds** (press / repeat / release) | **Left alone, and declining is correct.** Treating a repeat as a press is the right behaviour for every widget in the set — a held arrow should move a list. No widget needs release, and a `kind` field would force every consumer to reason about a case none of them handle. |

The general rule, worth applying to the two library changes §5.4 already
predicts: fix the library when the gap is an **internal asymmetry** — something
the model already half-expresses — and leave it alone when the gap is only
"the backend has a concept the library chose not to have".

Two translations encode decisions rather than mechanics:

- **Drag is reported as a move.** The library routes it by pointer capture, so
  the node that took the press keeps receiving motion without the backend
  distinguishing the two. That is the whole drag mechanism, and it is what
  replaces the `PointerGrab` flag ladder (§2.8).
- **The physical chord is reported, not the layout reading.**
  `fresh_input_parser::KeyPress` carries both, and the editor's keymap decides
  which wins. That decision belongs to the keybinding resolver, which survives
  at the root fallback (§2.9) — not to a widget.

What the prototype does *not* yet do: thread the real per-frame state (hover
targets, LSP-waiting, cursor hiding) instead of defaults, publish `BodyOutput`
(`view_line_mappings`, tab layouts) to the geometry bridge, and run the
three-stage dispatch for real. None touches the borrow, which was the open
question; all are mechanical work for S1.

Finding 2 is **not a layout bug to fix in `fresh-ui`.** It is a signal that
*which rows are visible* belongs in `build()` as a function of the available
height — app state deciding structure — rather than being left to
solver-specific starvation order. Deciding that explicitly is better behavior
than either engine's accident, and it removes the dependence entirely. It is
recorded as a decision (§6.2) and pinned by a test so it cannot drift
unnoticed.

The PoC also surfaced a smaller structural point worth carrying into M0: the
dock's bail-out rules (`EDITOR_MIN`/`DOCK_MIN`) are **app logic keyed on the
frame width**, and `build()` cannot read geometry. A real shell resolves that
width from state before building (a resize is an event like any other), or uses
`LayoutReader`. The same will be true of every "how wide is the frame" decision
that currently reads `size` at the top of `render`.

#### Landing S1 without a flag day

The frame does not have to switch over in one commit. This codebase already has
the right technique for hoisting geometry: when the chrome-layout work moved
surfaces from paint-recorded caches to live derivations, the paint pass
**debug-asserted that paint == derivation** before the cache was deleted
(§2.4). The same applies here.

**S1a** — `Editor::render` builds the shell's `Frame` from the same visibility
flags it already computes, runs `region_rects`, and `debug_assert`s every
region against the rectangle the ratatui `Layout` just produced. In debug
builds this runs on every frame, so the ~315-file e2e suite becomes the check
that `fresh-ui` can take the frame over — thousands of real frames at real
sizes, with real dock, explorer, prompt and suggestion states. Release builds
are untouched, and nothing about painting changes.

The assertion deliberately skips the squeeze band (frame shorter than its fixed
rows), where the two engines starve different rows by design. That is the
decision recorded in §6.2, not a defect, and it is pinned separately.

**S1b** — **landed.** S1a came back clean across the whole e2e suite, so the
frame's geometry now comes from the shell: one description, laid out once,
giving every region its rectangle. The five-row `Layout` and the sidebar carve
are gone from `render`.

Three things were needed to make the swap safe, and they are the reusable part
of the lesson:

- **Hidden regions still have positions.** A zero-height prompt row is where
  the suggestions popup anchors, so the description carries *every* region,
  hidden ones at zero size, mirroring the `Length(0)` constraints the ratatui
  layout used. The parity test compares all seven, empty ones included.
- **Decisions and geometry separate cleanly.** `split_file_explorer_area` both
  decided whether a sidebar shows and computed where it goes; it is now
  `file_explorer_layout_request`, which answers only "is there one, how wide,
  which side". The shell turns that into rectangles. Splitting the two is what
  let the geometry move without the policy moving with it.
- **Presence is app state, not geometry.** A hidden sidebar has a zero-width
  rectangle like anything else, so callers keep distinguishing the two by
  `Option` rather than by measuring.

The S1a assertion is removed with the code it checked — with only one
derivation left in `render`, it would have compared the shell against itself.
`tests/ui_shell_frame_parity.rs` keeps both honest instead, and covers far more
combinations than a running editor reaches.

#### S1 complete: the seam is live and inert

Three pieces closed it, and they share a shape worth naming — **each is
load-bearing and changes nothing**:

- **The retained tree persists.** `Editor::shell_ui` holds the `Ui` across
  frames, so element state, focus and the dirty set survive. It is held in an
  `Option` and **moved out for the duration of a frame** rather than borrowed
  from `self`: the display list is borrowed from the `Ui` while the fold calls
  back into `&mut Editor`, and as a plain field those borrows conflict. That is
  the same disjointness the sibling arrangement gives; either satisfies the
  constraint §4.4 records.
- **Native items paint through the fold.** `fold_native` walks the display list
  and skips `Host` items, so a region can move into the tree on its own while
  the ones around it keep their existing painters and their existing
  rectangles. When the last region is native this collapses into the general
  `fold`. A test pins the property the working state depends on: a frame of
  host regions paints *nothing*.
- **Input reaches the shell first.** `shell_dispatch` offers each translated
  event to the tree ahead of the legacy walk — stage two of the three-stage
  arrangement, with the modal-capture band still ahead of it and the existing
  walk still the floor. No node carries a handler yet, so it declines
  everything and every event lands where it always did.

The message type landed with it: `UiMsg::{Action, Ui}` (§2.9). Everything a
user could bind stays an `Action` and goes through `handle_action` unchanged;
positional facts get a `Ui` variant that is applied and never serialized.

**What this buys:** a surface stops being a `Host` leaf and immediately draws
through the fold, takes its own input, and keeps its own state — with no
further plumbing. That is what makes S2 and S3 a sequence of independent swaps
rather than one flag day.

#### The wave order was wrong, and the code said so

The plan ordered M1 (status bar) before M2 (context menus) by increasing risk.
Reading both renderers reverses that:

- `render_status` is ~3,200 lines carrying a right-side **drop heuristic**
  (shed low-priority elements until they fit alongside a ~40%-capped-at-40 left
  budget, never dropping the first), a narrow-terminal case under 15 columns,
  per-element truncation, separator theming, per-element styling that varies
  with LSP state, and cell-provenance runs. Reproducing it cell-identically is
  a wave in itself.
- `render_context_menu` is ~50 lines: clear the area, pad each label, draw a
  bordered block.

So context menus went first — which is also what the plan wanted for a
different reason: M2 is its designated go/no-go, the first surface to need a
layer at all.

**Context menu paint, migrated.** A context menu is now an ordinary `Layer` in
the frame's tree rather than a separately-ranked surface with its own painter.
Three things kept it cell-identical:

- **The position is the old one.** The layer anchors at the point
  `ContextMenu::clamped_position` already computed, rather than letting `fit`
  place it — so the menu lands on the same cells, and the hit-testing that has
  *not* migrated keeps agreeing with what is drawn.
- **The padding is the old one**, reproduced character for character
  (`" {:<width$}"`) rather than re-derived, because the row is what the cells
  actually contain.
- **The border glyphs changed to plain** (`┌┐└┘`). The fold drew rounded
  corners, which nothing had noticed because no surface had used a border yet;
  ratatui's default — and every bordered surface in the editor — is plain.

**Paint moved late.** The fold used to run straight after layout, which was
fine while it painted nothing. An overlay sits *above* the content around it,
so layout still runs early (the regions need rectangles) and paint now runs
where the context-menu paint used to be. Paint order is what puts a menu on
top.

`render_context_menus` and `render_context_menu` are deleted.

**Pointer input and dismissal, migrated.** This is the first place the
migration's central claim pays out: behaviour that was *written down* becomes
behaviour that is *declared*.

| Was | Is |
|---|---|
| a full-frame `chrome:context_menu_close_guard` box, pushed at z180, with a pointer arm that dismissed and consumed | `Modality::Exclusive` on the layer — everything outside is non-interactive because the layer says so, and no host leaf beneath it takes raw input |
| a `chrome:context_menu` box plus `handle_click_context_menus`, hit-testing the pointer against the menu's rect to decide activate / dismiss / inert-border | the rows' own `on_click`, and `Dismiss::OUTSIDE_POINTER` for everything else |
| `hover` + `on_hover_change`, walking hover targets to produce `HoverTarget::ContextMenuItem` and feed the highlight back | the rows' own `on_enter` |
| a "right-click inside an open menu" arm, so the menu is not re-opened or re-targeted | `on_secondary_click` that stops propagation |

`app/chrome/context_menu.rs` loses 138 lines: its `collect` is now empty, and
`on_pointer`, `hover`, `on_hover_change` and `handle_click_context_menus` are
gone.

**One thing worth recording**, because it decides parity: dismissal is
evaluated on the **press**, not the release (`hit.rs`'s `Input::Press` arm),
which is exactly when the close-guard box dismissed too. A first version of the
test watched only the release and saw nothing.

**Keyboard, migrated.** Arrows, Enter and the modal swallow were a pre-band
keyboard grab — a whole pipeline stage that ran before every layer rank. They
are now an `on_key` on a focused child of the layer, and the pre-band grab
stage is down to one component (the theme inspector's observer).

Escape is the interesting one: it is *not* handled. The layer declares
`Dismiss::ESCAPE`, and a key that dismisses a layer is answered by that layer.
The handler must therefore decline it without stopping — stopping claims the
key inside `propagate_key`, which returns before `dispatch_key` reaches
dismissal at all, so the menu would swallow Escape and stay open. That is the
shape of every modal that migrates: *act on what you own, stop what you swallow,
and let the layer's declared dismissals through untouched.*

`ContextMenu::on_key` and `handle_context_menu_key` are deleted.

**Geometry, unified.** The layer anchored at the point
`ContextMenu::clamped_position` computed — a bridge, while hit-testing was
still legacy, that outlived its reason and left the editor with two places that
decided where a menu goes. The layer now takes the **raw** click point with
`Fit::CLAMP`, which is the same arithmetic (`x.min(frame - box).max(0)`)
declared instead of written, and `clamped_position` is deleted.

Its second caller was the web `Scene`, and unpicking that is the part worth
recording, because every later wave hits it:

- **Suppression is a fold decision, not a tree one.** The web bridge renders
  with `suppress_chrome_cells` so the cells carry buffer interiors only, and
  the menu's tree derivation was gated on that flag — which meant the retained
  spec had no menu on exactly the path that needed its rectangle. The gate
  moved to the `fold_native` call: the description is built either way, and
  only the cell-writing half is skipped. That is what "backends are folds over
  the display list" is worth in practice — two frontends, one layout.
- **Consumers read the spec.** `context_menu::menu_rect(spec)` returns where
  the menu actually landed, found through `LayoutSpec::index` by the layer's
  key, and `Editor::shell_menu_rect` is its caller-side partner (the same shape
  as `shell_region_now`, for a surface that is not a host region). The `Scene`
  asks it instead of re-deriving the clamp.

What remains of the old implementation is `layer_rank::CONTEXT_MENU`, and only
because the PTY gate reads `blocks_terminal_input` off the overlay stack while
the library derives the same fact from `raw_input()` — which is only meaningful
once host leaves *declare* that they take raw input. Every region is a
`PlainHost` today, so deriving it now would report the terminal blocked on
every frame. It retires with the terminal grid's own host leaf (S5).

#### Menu-bar dropdowns, paint

The second overlay, and the first with real structure: a dropdown is a *chain*
— the menu's own box plus one per open submenu level, each placed against the
one before it. Each level is now a `Layer`, and the chain is the order they are
declared in. That is the whole of "a submenu paints over the level it opened
from"; the old renderer got the same result by painting in a loop, and a
z-ordered scheme would have had to state it as a rule.

The interesting work was not the tree. It was that the old dropdown decided
three things in one pass and spelled two of them twice:

- **`MenuRowStyle`** replaces two style ladders that had already drifted.
  `build_dropdown_item_line` chose colours for the cells and
  `record_dropdown_item_run` chose provenance keys for the theme inspector —
  the same decision, written out separately, and the recorder's copy did not
  know about hover, so it reported a hovered row as an ordinary one. There is
  now one ladder with three renderings: `style()` for ratatui, `theme_keys()`
  for the inspector, `shell_theme()` for the display list. The inspector's
  hover bug goes with it.
- **`dropdown_item_text`** is what a row *says*, separated from how it looks —
  the padding, the checkbox glyph, the keybinding hint, the submenu arrow,
  reproduced character for character because the row is what the cells contain.
- **The walk publishes the description.** `MenuLayout` gains
  `shell_dropdowns`: the same pass that decides each level's rectangle now also
  says what its rows read, and the shell paints from that. Not a second
  derivation of the menu — the only one.

`render_dropdown_level` no longer writes a cell, so its `frame`, `theme` and
`draw` parameters are gone, as is the ratatui `Paragraph`/`Block` that drew the
box.

**Paint only, deliberately.** The levels carry no modality, no dismissal and no
handlers, and each is anchored at the rectangle `fit_dropdown_area` already
chose. Pointer input still runs through `chrome::Menu`'s boxes and the
full-frame `chrome:menu_close_guard`. That is the same three-step shape the
context menus used — cells, then input, then let the layer's own `fit` decide
placement — and it is what keeps the not-yet-migrated hit-testing agreeing with
what is drawn.

#### The fold, split into two bands

The single fold was the migration's real ordering constraint, and it was
stricter than the plan admitted. There is one display list and *many* legacy
painters, and the legacy painters are not in the list — so one fold pass can
only sit on one side of them. It sat late, which made every native surface
paint above everything unmigrated. That is right for overlays and wrong for
everything else: the file explorer paints *first* among the legacy painters, so
making it native under one fold would have put the sidebar on top of the body,
the popups and the modals, silently.

`fold_native` now takes a `Band`. `render` calls it twice — `Background` before
any legacy painter, `Overlay` after all of them — and the legacy painters run
in between. Each band lands where its surface belongs, and the
"migrate top-down through the old paint order" rule retires with it.

**The cut is the library's**, and briefly was not. The first version derived it
here, by matching item keys against a hand-kept list of the frame's layer
families — which is precisely the "no hand-specified exceptions" the library's
second goal rules out, and it was already wrong twice over:

- **A scrim carries no key**, and is pushed *before* its layer's own items. An
  index-derived boundary puts it on the background side, so a modal's dimming
  would paint under the content it exists to dim. That is the rest of S3.
- **A layer need not be keyed at all.** `widgets::Dropdown`'s is not, so its
  whole pop-over produces no index entry and reads as in-flow content — it
  would vanish under the legacy painters. That is the first library widget S4
  reaches for.

`LayoutSpec::layers_from` says it outright (base PR #3052), with `in_flow()`
and `layers()` as the two halves, and `OVERLAY_FAMILIES` and its guard test are
deleted. Same diagnosis as `Dispatch::claimed`: the library already computed it
and threw it away.

**When a `fresh-ui` change is warranted**, stated properly, because this keeps
coming up and a vague version of it has already been applied wrongly in both
directions:

- **Underivability.** Can a correct consumer compute this from the library's
  *existing outputs*? If yes, it is not a library change, however convenient.
  `layers_from` fails that test — a scrim carries no key and is pushed before
  its layer's items, and `widgets::Dropdown`'s layer carries no key at all, so
  every backend-side derivation is silently wrong. A placement offset passes it:
  anchor to a different node.
- **Internal inconsistency.** The library contradicting its own contract is a
  library bug wherever it surfaces. An `Item` declares a rect and both in-repo
  backends painted outside it — the fix belongs there, not in every caller
  pre-fitting every string.
- **A caller in the same PR, plus a test that fails without it.** No primitive
  lands on speculation. `layers_from` had `fold_band` and the demo's F2 band;
  `Draw::Lines` clipping had a failing golden. `Anchor::Node` and four `Place`
  variants had neither and have never been constructed by anything, anywhere.

*Not* "would another consumer want this?" — that question is unfalsifiable, is
reliably answered yes by whoever wants the feature, and at value granularity
rejects everything (no second consumer wants `Sizing::Cells(12)` "with the same
meaning" either). It is what admitted the six unused variants.

That PR also closed a second gap it turned up. A `Draw::Lines` run can be
longer than the rect carrying it, because layout hands a constrained node the
width it was *allowed* rather than the width its content wants — and every
backend in the repo, the fold included, drew the string while honouring only
the *inherited* clip. So an over-long row painted straight through its own
border, which is what the ratatui `Paragraph`'s silent truncation had been
hiding. An item declares how much room it has; the backends clip to it now.

**What this unblocks, all at once:** the status bar and the search-options row
(S2, which had been stuck precisely because popups paint over the bar's row),
the menu bar's own row, the dock column, and the file explorer. None of them
depends on the overlay waves any more — their order is now a scheduling choice.

#### The menu bar row, native — the first background region

The first surface migrated *under* the legacy painters rather than over them,
and the two-pass fold's first real consumer.

- **`BarLabelStyle`** is the bar's `MenuRowStyle`: one ladder (normal / active /
  hovered) with three renderings — `style()` for ratatui, `theme_keys()` for the
  theme inspector, `shell_theme()` for the display list.
- **The mnemonic is a run, not a sibling.** One underlined character inside a
  label is text styled *within itself*, which is exactly what `text_runs` was
  added for; laying the three pieces out side by side would let them wrap and
  truncate independently. The underline is carried as part of the run's theme
  name (`menu.bar.item.active.mnemonic`), because an item carries one
  `ThemeKey` and the library never interprets it.
- **`MenuRenderer::render` is gone.** It painted; `compute_layout` did the same
  walk with painting switched off, and a `debug_assert` held them in step.
  Nothing paints a cell there any more, so there is one walk and nothing to
  keep in step. `render_menu_bar` becomes `record_menu_theme_runs` — the walk's
  only remaining side effect is the inspector's provenance.

**A region is named, not announced.** `regions_of` used to scan the display
list for `Draw::Host` items, which works only while every region *is* a host: a
native region emits no such item, and neither does a region that paints nothing
at all (a hidden row, a bar with no labels). Both still have rectangles that
callers ask for by name. Every region node now carries `region_key(r)` — host
leaf or native alike — and `regions_of` is a layout query (`Ui::find_by_key` +
`rect_of`) rather than a paint scan. That is goal 5 applied to the migration's
own bookkeeping: layout computes rectangles, everything else reads them.

The parity sweep caught this, which is the argument for having built it:
migrating the bar dropped region 2 from 8586 of its cases, silently, until the
oracle said so.

#### Menu pointer input, and the close guard

`app/chrome/menu.rs` goes from 204 lines to 71 — `collect`, `hover`,
`on_hover_change`, `on_pointer`, `handle_click_menu_bar` and
`handle_click_menu_dropdown_surface` all deleted, along with
`compute_menu_dropdown_hover` and the coordinate-driven
`handle_menu_dropdown_click`. What is left is the layer entry and the keyboard
grab.

| Was | Is |
|---|---|
| `chrome:menu_bar` box at z120, hit-tested against `MenuLayout::menu_at` | the bar labels' own `on_click`, which already know which menu they open |
| `chrome:menu_dropdown` box per level, plus `hit_test` → `(depth, index)` | each row's own `on_click`, which already knows its level and position |
| a full-frame `chrome:menu_close_guard` box at z110 | `Dismiss::OUTSIDE_POINTER` on the outermost level |
| `hover` + `on_hover_change` walking hover targets | each row's `on_enter`, feeding the *same* `menu_hover_reaction` |

Three things are worth recording, because each is a rule the next surface will
meet.

**Modality is `None`, not `Exclusive`.** Clicking another bar label while a
menu is open must close the first and open the second from one press — every
platform does. An exclusive layer makes the bar underneath inert and costs the
user a click. With `None` the dismissal fires first and the label's own click
follows, so the pair reads "close this, open that".

**Which makes the toggle a matter of *when*, not of what to remember.**
Clicking the *open* menu's own label closes it. Dismissal runs first, so by the
time any message is applied the menu is already shut and asking "is this menu
open?" answers no and reopens it — a toggle that never toggles.

The first fix had the label close over its own open-ness at build time, and it
was wrong in the running editor while passing every test: the main loop
repaints between press and release, so the release ran against a tree rebuilt
with nothing open. `mouse_click` in the harness sends both halves back to back
and could not express the gap; `mouse_click_with_repaint` now can, and
`test_mouse_click_toggles_menu_across_a_repaint` is the test that fails without
the fix.

The fix is to put the toggle where the state is. The bar acts on the **press**,
which is also what the pre-migration code did (`MouseEventKind::Down`), so the
dismissal and the toggle land in one dispatch — and `shell_dispatch` snapshots
`menu_state.active_menu` before applying a single message of it. The general
shape is the opposite of what the first attempt suggested: *a handler that needs
to know what was true before the event should not carry the answer; it should
be dispatched where the answer is still there to read.*

It also produces the right gesture split for free. The bar acts on press and
the rows act on release, which is precisely press-on-bar, drag-to-item,
release-to-activate — how a menu bar is used.

**A migrated surface claims the pointer over its own cells.** The hover target
lives in one field (`mouse_state.hover_target`) that the legacy walk rewrites
on every move. With the bar's chrome box deleted, that walk finds nothing there
and clears what the tree just set. So the bar row and the dropdown boxes take
`Move` — after which the legacy walk does not run for that event at all. Every
background region that migrates will need the same, until the walk itself goes.

`activate_menu_item(depth, index, menu)` is split out of the coordinate form: a
row that answers its own click already knows which row it is, and should not
have to hand back a cell for a hit-test to turn into the index it started from.

One deliberate behaviour change: a **right**-click outside an open menu used to
be swallowed by the guard box, which consumed any button. Dismissal fires for
any button but claims only the primary one, so a right-click now closes the
menu *and* goes on to open the context menu — the same ruling the context-menu
wave made, for the same reason.

#### Four corrections from the second review

An unbiased agent reviewed the branch against the library's stated goals
(`docs/internal/fresh-ui-migration-review-2.md`). Its verdict was that the
frame swap and the context-menu wave are on-goal and the menu-bar wave was
not. Four things came out of it, beyond the band cut above.

**The caret is wired, not asserted about.** `fold_native`'s return value was
taken, `debug_assert`ed `None`, and dropped with `let _ =` — a seam that would
be discovered missing by the first native field having no cursor. It now flows
into the end-of-frame commit, ahead of both the buffer's caret and the
sidebar's, and it needs none of the obscured/suppressed guards those two carry:
a native field is *in* the tree, so if it has focus it is on top by
construction. `cursor_suppressed_by_late_overlay` retires with the last
unmigrated overlay rather than growing another entry.

**`build` no longer reads layout.** `shell_frame` reached `menu_layout_now` →
`shell_region_now` → the retained tree, so building the description consulted
the rectangles the *previous* frame produced — the loop the library's own
`Ui::rect` refuses at runtime, and one frame stale into the bargain. The bar's
rectangle was never a layout result anyway: it is the chrome column's top row,
and `compute_dock_split` already decided that column from state alone.
`menu_layout_in(bar_rect)` takes it as an argument now.

**The tests could not see a style.** Every shell test rendered through a
palette answering `Style::default()`, so a highlighted row, a bold label and an
underlined mnemonic all came out identical — which is how four cell-level bugs
reached CI in one wave. `fold::test_palette` gives each theme name a distinct
colour and reproduces the two modifiers the real palette applies, and the
migrated surfaces now assert cell styles: the mnemonic run differs from the
characters beside it, the highlighted context-menu row differs from its
neighbours, and a dropdown painted over bold cells comes out unbold — the
display-list-is-not-a-diff rule, pinned at the cell.

**And the exact message list, not `contains`.** The click-bubbling bug produced
`[MenuBarPress, CloseMenu]` — open and shut in one gesture — and passed a
`contains` assertion happily. The input tests assert the whole list now.

#### Revised stages

| Stage | What moves | Why here |
|---|---|---|
| **S1** | Frame skeleton: every region a `Host` leaf, painted by today's painters. Input fully delegated. | **Landed, including the body.** The frame's geometry is the shell's, the retained tree persists across frames, native items paint through the fold, and input is offered to the shell ahead of the legacy walk. The split grid is now painted *by* the fold, through `HostPainter::paint_host`, with the rectangle layout gave it — `render`'s own hundred-line assembly of `render_content`'s 28 arguments is deleted and `shell_host::with_grid` is the only copy. It had to be: two assemblies of one call drift, and the unreached one had, dropping five of the seven results and passing `BodyState::default()`. What made the second copy necessary was `suppress_chrome_cells` skipping the fold outright — which skipped the body with it — so the fold grew `Paints::HostsOnly`: a frontend that draws the tree's surfaces itself still needs its host regions painted, because the panes are cells even on the web. S5's decomposition (§6.2 item 7) has since subdivided that leaf rather than removing it: a pane is its own `Host`, and the body's is the separators' and the panes' shared preamble. |
| **S2** | The live-derived regions — status bar, search-options row — become native descriptions. | **Done.** Both surfaces describe what is on them and layout decides every column. The **search-options row** was the first surface to meet the third acceptance criterion. The **status bar** was the one that paid for it twice: `render_status` placed every element *and* emitted a `StatusBarLayout`, then `compute_status_layout` re-ran the whole walk at event time over state that may have moved. `clickable_rects`, `plugin_token_areas`, `segments` and `provenance_runs` all read the laid-out tree now; plugin tokens became first-class (they were a second loop the click rail reached only after missing every built-in). What stayed app-side is *which* right-hand elements appear when the bar is too narrow — a content decision from measured text, not geometry. |
| **S3** | Overlays become real `Layer`s: context menus → dropdowns/menu bar → popups → prompt/palette → modals. | The value stage. Each one deletes guard boxes, a rank entry, and a slice of the capture band. **Context menus: done** (below) — paint, pointer, dismissal, keyboard and geometry, with only the `blocks_terminal_input` rank entry left behind. **Menu bar: paint and pointer migrated** — the bar row is a native background region, the dropdown chain is a stack of layers, and the close guard is a dismissal property. **Popups: done** — placement, paint, wheel, scrollbar, rows, close button, dismissal, links and text selection are all in the tree, and `view/popup.rs`'s painter and all four of its chrome boxes are deleted. **Prompt / palette: done** — both suggestion lists are the tree's (the overlay's anchors to the card's results band by name), the card is mouse-modal, and `shell_owns_suggestions`, which existed to tell the two apart, is gone with them. **Theme inspector and file-open dialog: done.** **The modals: done, on the pointer side.** Settings, the keybinding editor, the calibration wizard, the workspace-trust prompt and the floating panel each owned the whole mouse channel through `ChromeComponent::capture_mouse` — a band ahead of every walk including the shell's, and the reason `placed_surface_outranks_shell` existed. Capture is what a modal *is*, and `Modality::Exclusive` says it, so **the band and the precedence constant are both gone and there is one pointer walk.** The workspace-trust prompt migrated outright — layer, `Scrim::Dim`, rows, radios and buttons — retiring `TrustDialogLayout` (a paint-recorded roster entry), its painter, its `Vec<Seg>` row plan, its own word-wrap, its scroll clamp and `handle_workspace_trust_mouse`. The other four keep their interiors, which hit-test rectangles their own painters recorded and tell a drag from a move — something a tree `Event` deliberately cannot, since the library routes drags by pointer capture. So the tree answers *which* surface an event belongs to and the event stays on the editor: routed, not transported (`Editor::shell_pointer_event`), and that channel retires with the interiors. What is left of S3 is the keyboard, which is one wave rather than per-surface.

**And the pointer walk itself is gone.** With S5's panes on the tree, no component contributed a box, so every question the walk answered was already being answered before it ran. `chrome_tree` and its validated memo, `ChromeBox`, `ChromeTreeBuilder` and `hit_stack`; `Disposition`, `PointerPress`, `ChromePointer` and `pointer_walk_step`; `dispatch_pointer`, `dispatch_wheel`, the click / right-click / double / triple entries and `handle_horizontal_scroll`; `compute_hover_target`, `update_hover_target` and `mouse_state.hover_target`; `ChromeComponent::{collect, hover, on_pointer, on_wheel, on_hwheel}`; and `ui_gen`, whose only reader was the memo. `ChromeComponent` is a keyboard interface now — `layers`, `on_layer_key`, and the two hover reactions — which is what makes the keyboard wave the last of S3 rather than one of two remainders.

Three things had to move *out* of the gap between the two walks on the way, and they are one lesson: **the tree runs first, so anything that used to sit between it and the box walk now sits behind whatever the tree claims.** The right-click that clears the transient tab menus became a capture-phase listener on the frame — "anywhere" cannot mean it in a walk that only runs for what the tree declined. A live terminal's own mouse and the Ctrl+Click that opens a path it printed became `pane_content_takes_pointer`, asked at the head of the pane content's own handlers. And the smooth-scroll walk, which turns a multi-line notch into a slide, moved *ahead* of dispatch: it had been splitting only the notches nothing took, so every migrated surface had silently been jumping the whole notch since its wheel became a node.

**The two remainders are both blocked, and neither is a loose end.**

*The keyboard grab* had a precedence decision in front of it, and **that decision is settled and shipped**. The question was: does keymap resolution move ahead of shell dispatch, or does a migrated surface ask the keymap from inside its description? It is the second. `Editor::menu_shortcuts` reads the `menu` keymap section when the description is built and hands the tree `MenuShortcut { key, intent }` values, which `menu_intents` resolves with nothing in front of them. `menu_action_binding` — the consult that sat in the layer walk, *behind* the shell, and was therefore swallowed by any capture-all `on_key` above it — is gone. Every later keyboard-owning surface has the same answer available: bindings flow down as data, and the tree resolves key → intent → action.

What is actually left is the **wave**, and its shape is now clear rather than blocked: `base`, `dock`, `floating_modal`, `menu`, the four `modals` and `popups` still implement `on_layer_key`. Several of those hand the key to an interior that is still the painter's — a modal's `dispatch_input`, the dock's widget command dispatcher — so their keyboard migrates when that interior does. The ones whose interiors are already the tree's are the ones to take first, and there are five of them: `base`, `menu`, `popups`, `prompt`, `context_menu`. That is G1 in §5.1b, which orders this wave and the rest of the remaining gaps ahead of the plugin one.

*The rank entries* cannot go until the rest of S3 does. `blocks_terminal_input` is contributed by six components — popups, dock, floating modal, base, menu, context menu — and `presents_blocking_overlay` is the single source of truth for "is anything modal up?". Removing the two migrated entries would mean an open context menu or menu stops blocking PTY routing. They retire with the last unmigrated overlay, not before. |
| **S4** | Dock column, file explorer, plugin panels. | **File explorer: done** — the panel is a native region, rows and slots are measured by the tree, `trailing_slot_screen_bounds` and the old renderer's paint half are deleted, and the grip paints its own hover column via `layout_reader`. **Dock column: done** — its press, right-press, wheel and width grip are nodes, and the blur observer moved to a capture-phase listener on the frame, which fixed it: as the surfaces beside the dock became nodes, each one that claimed a press stopped blurring a focused dock, because the shell runs ahead of the walk the full-frame guard box lived in. The column's *content* is still a `Host` leaf. **Plugin panels remain**, and they are the M6 wave rather than a remainder: `WidgetSpec` → `Node` translation and element state replacing `WidgetInstanceState`. It no longer waits on anything: S3's ordering went with the two-pass fold, and §6.2's "colour that is not a theme name" is decided and shipped — `Paint::Lit` carries a plugin's `OverlayColorSpec::Rgb` as a `#rrggbb` literal the grammar reads back.

**And there is no plugin API change.** `WidgetSpec` is frozen: every plugin that works today goes on working, unchanged, and M6 is entirely a backend swap. The wave was recorded as needing *required* per-item keys on `List`/`Tree`, on the reasoning that identity becomes `(type, key)` under the library's reconciler, so unkeyed rows would all share the key `""` and per-row state would have nothing to belong to. Three things say otherwise, and each was checked rather than assumed:

* **The library already reconciles unkeyed siblings positionally.** `Ui::reconcile_children`: a keyed description looks its key up anywhere in the old child list, and *"an unkeyed description matches the old child at the same index, and only if that child is itself unkeyed: position is the implicit key."* Unkeyed rows do not collapse onto a shared key; they have none, and fall to index matching.
* **No state belongs to a row element.** `WidgetInstanceState` is a map keyed by the *widget's* key, and per-row state lives inside that entry as an index — `List { selected_index, scroll_offset, … }`, `Tree { selected_index, expanded_keys, … }`. Positional reconciliation therefore gives a row exactly the identity the current renderer gives it, because `selected_index` *is* a position.
* **`Tree` expansion is already keyed by plugin-supplied node keys** (`expanded_keys` against each `TreeNode`'s own key). That is existing API, not something M6 introduces.

What per-item keys would actually buy is selection and scroll surviving a **reorder or a mid-list insert** — today `selected_index: 3` follows the position, so inserting a row at the top moves the user's selection. That is a real improvement and worth having on its own merits, but it *changes* current behaviour rather than preserving it, and it is orthogonal to this wave. `item_keys` stays optional: used where a plugin supplies it, ignored where it does not.

The deprecation audit that warned about this (`widgets::keying`, and `Editor::unkeyed_widget_warnings`) is **deleted**. It was shipped, user-visible, and told plugin authors that `itemKeys` "will become required" — a promise made on the premise above, which does not hold here.

One identity constraint does survive, unchanged: a card list's `item_specs` nest arbitrary widget subtrees, and those widgets' state is keyed by their own `key` in one flat map, so two cards whose nested `Text` widgets share a key collide. That is true today; M6 inherits it rather than creating it. |
| **S5** | Splits, tabs, scrollbars decompose; the buffer becomes the only `Host` leaf. | **The pointer side is done, and `chrome/splits.rs` no longer has a component in it.** The grid is a description (`view::shell::splits`) that the model itself lays out — `get_leaves_with_rects` and `split_layout` are reads of it — and every surface a pane has is a node keyed by the pane it belongs to: its dividers, its tab strip (which took the split controls with it, since those are drawn *over* the tab row and a box said so with `z`), both scrollbars, and its content. **Which chrome a pane has is one rule** (`PaneChrome::resolve`) resolved once per frame by `Window::pane_chrome`, so the description and the painter cannot disagree about whether a pane has a strip; it had been four copies of the boolean algebra, two of which were quietly wrong about `Fixed` panels and live terminal grids. **A buffer group's panels are panes now too** — that layout lives in a side map and used to be dispatched into a pane's interior only at paint time, which is why its separators stayed recorded rectangles long after the main tree's became nodes; mounted in the pane's content node, which *is* the rectangle the painter uses, they are ordinary dividers. Each handler behind these nodes used to open by asking every recorded rectangle in turn whether it contained the point; they take a pane now. What they still read back is geometry that genuinely records the last paint — a scrollbar thumb's extent, the tab renderer's per-tab columns — and the pane's own content rectangle, read from the node that defines it. **And the paint side is done with it.** A pane is its own `Host`: the fold reaches one at a time and hands each the rectangle layout gave it, so the rectangle a pane is painted at and the rectangle it is clicked at are the same rectangle rather than two that agree. The body's leaf keeps only what belongs to no pane — the pass every pane shares and the separators between them. `render_content` is three named phases over three carriers (`FrameFacts`, `Stores`, `PaneAreas`), which is what let a trait method carrying a target and a rectangle call the middle one; and the painter moved off the `Editor` onto a frame-scoped `BodyPainter`, taking `pending_body_state` and `pending_body_output` with it. S5 is complete. |

### 5.1 M0 — the seam (a genuine prototype, not just plumbing)

M0 is re-scoped from "pure plumbing" to a **prototype gate**: the Host seam
(§4.4) has never been exercised by any backend, and every wave stands on it.
Seven pieces:

1. **TUI backend** in `fresh-editor`: `LayoutSpec` → ratatui `Buffer`, mapping
   `Item::theme` (`ThemeKey`) through the resolved `Theme` and synthesizing
   chrome theme provenance per item. (The `examples/interactive.rs` fold is the
   reference shape for chrome — note it stubs `Draw::Host`.)
2. **The fold-callback API** — the concrete shape of "backend meets
   `Draw::Host`": a per-`HostId` callback over `&mut Editor`, run mid-fold in
   paint order, taking the `with_all_mut` borrow and running `render_content`
   at the item's rect (§4.4). Nothing like it exists yet; this *is* the
   prototype.
3. **`HostLeaf` impls** — `BufferHost` and a terminal-grid host emitting
   `Draw::Host(id)`, with hoisted factories (the `HostSpec::Leaf` pointer-
   equality footgun, §4.4).
4. **The frame skeleton** — the whole frame as a `fresh-ui` tree with one
   `Host` leaf per region, replacing the ratatui `Layout` carves. Proven
   rect-for-rect against today's layout by
   `crates/fresh-editor/tests/ui_shell_frame_parity.rs` (§5.0). Under
   outside-in this replaces the inside-out mount point, which is not built at
   all.
5. **Input adapter** — terminal event → `fresh_ui::Input`, and messages back
   out as `UiMsg` (Actions + UI-geometry) into the existing pipeline.
6. **Caret arbitration** — one "who owns the caret this frame" decision,
   replacing `cursor_suppressed_by_late_overlay`: the fold-time host caret
   merged against `LayoutSpec.cursor`, with `compute_content_layout` for
   same-frame anchors.
7. **Geometry bridge** — fold-published caches keyed by `HostId`
   (`view_line_mappings`, cell-theme map) readable by handlers, plus the
   pre-paint path for caret-anchored layers.

**Exit:** the frame renders through the shell with every region still painted
by today's painters and cell output unchanged, a one-line status segment
renders and takes a click as a native description, **and** a prototype
`BufferHost` proves the seam end-to-end — real
buffer cells under `Draw::Host`, one caret-anchored popup anchored via
`compute_content_layout`, one click-to-cursor through a `Gesture` handler —
plus the §4.7 rebuild benchmark. No wave is scheduled until this exit holds.

### 5.1b The remaining gaps — moved to the front

Section II at the top of this document is the live list, with how each piece
lands in the library's idiom and what to avoid while landing it. Section IV
records the six that closed. This heading is kept because commits cite it.

### 5.2 Waves (increasing risk)

| Wave | Surface | New mechanism exercised | Deletes (survey-grounded) |
|---|---|---|---|
| **M1** | Status bar, ~~search-options row~~ (**row done**) | static layout, click targets | the live-derived `status_bar_layout_now` path and its `StatusView` painter. The search-options half is landed: `chrome/search_options.rs`, `SearchOptionsLayout` (+`compute`/`checkbox_at`), `SearchOptionsHover`, `StatusBarRenderer::render_search_options` and both of the row's `debug_assert_eq!` oracles are deleted |
| **M2** ⟵ **go/no-go** | Context menus (tab / new-tab / explorer / close-split) | `Layer`, `Modality::Exclusive`, `dismiss`, list nav | `chrome/context_menu.rs`, its close-guard box, its `on_key` pre-band grab, its rank entry, the four `Window` context-menu highlight fields |
| **M3** | Menu bar, dropdowns, submenus | nested layers, hover auto-switch, mnemonics | `chrome/menu.rs`, the `view/ui/menu.rs` dispatch half, the menu close-guard box, the hover auto-switch machine |
| **M4** | Info/hover/signature popups, theme inspector | transient dismissal via observers, scroll, text selection | `chrome/popups.rs`, `chrome/theme_info.rs`, `view/popup_mouse.rs` remnants, the transient-dismiss pre-band stage (the LSP hover *state machine* stays behind the leaf) |
| **M5** | File browser, prompt / command palette | `FocusScope`, text input, results list, preview | `chrome/prompt.rs`, `chrome/file_browser.rs`, `view/prompt_input.rs`, the overlay toolbar ring, the click scrim, the position-blind wheel box, the manual-scroll latch |
| **M6** *(after §5.1b's G1–G6)* | Plugin panels: dock + floating | `WidgetSpec` → `Node` translation, element state replacing `WidgetInstanceState`. **No plugin API change** — `WidgetSpec` is frozen, and the wave is entirely a backend swap. | `widgets/kinds/*` dispatch, `widget_runtime.rs`, `WidgetInstanceState`, `WidgetMutation` fast path |
| **M7** | Modals: workspace trust, keybinding editor, calibration wizard | `Modality::Exclusive` | `chrome/modals.rs`, `capture_mouse`, `blocks_terminal_input`, the cursor/hover suppression lists, the bespoke `handle_*_mouse` |
| **M8** | Settings (+ keybinding editor form) | the largest interior; rendering already on `WidgetSpec` | `view/settings/*` control layer, `view/controls/*`, `widget_map.rs`, the dual state store, the bespoke settings `input.rs` |
| **M9** | Frame: splits, tabs, scrollbars, dock column, explorer pane | the frame itself; all else nests inside | `chrome/splits.rs`, `chrome/base.rs`, `chrome/mod.rs` (registry, `layer_rank`, `chrome_tree`), `mouse_input.rs` dispatch engines, `PointerGrab`, the chrome half of `render.rs`, `KeyContext` |

**M2 is the decision point.** It is the first wave using layers, modality,
dismissal and focus together. If the seam and the model hold there, the later
waves apply the same mechanisms; if not, the library is corrected before wave
three rather than after eight surfaces depend on it.

**M9 dissolves under outside-in.** The wave table above is inherited from the
inside-out plan, where the frame migrated last. Under §5.0 the frame is S1
instead, so M9's contents split: the frame layout, dock and explorer carves
move to the front (S1), and what remains — the split grid, tabs, scrollbars,
and the removal of `app/chrome/`, `PointerGrab`, `KeyContext` and the two
precedence tables — becomes S5, the last stage, because it is the only one that
touches the KEEP side (§6.2). The deletions are the same; only their order
changes. M2 remains the go/no-go: it is still the first wave to use layers,
modality, dismissal and focus together, and under the shell it does so for
real rather than through a mount point.

**Surfaces the wave table doesn't name**, and where they land:

- **Tab-drag ghost / drop indicator** (`app/tab_drag.rs`) — the *grab* is
  pointer capture; the floating preview is a `Layer` following the pointer.
  M9.
- **Frame-buffer animations** (`view/animation.rs`) — cell-level snapshots of
  the previous frame with no display-list expression; they stay post-processes
  over the folded buffer, beside `convert_buffer_colors` (or are retired — a
  reviewed decision, §6.2).
- **Warning domains** (`app/warning_domains.rs`) — plugin-visible status
  indicators + popup content; rides M1 (segments) and M4 (popup); named in the
  M6 changelog because plugins observe it.
- **Orchestrator dock shell pages and session preview**
  (`render_dormant_shell_page`, `render_session_preview_into_rect`) — the
  preview is a second `Host` leaf (the phantom-leaf path); M6/M9.
- **`WidgetSpec::WindowEmbed`** — host content inside plugin UI; translated
  host-side to a nested `Host` leaf (§4.6). M6.
- **Live-grep preview and quick-open hint overlays** — M5 (the preview is a
  phantom-leaf `Host`).
- **`fresh-gui` native macOS menus** (`crates/fresh-gui/src/native_menu.rs`) —
  built from the `Menu` model, which stays app state; M3 must keep that model
  intact and name the second menu frontend in its PR.
- **Scrollbar overview-ruler markers** (plugin API) — `Draw::Scrollbar` carries
  only `{offset, content, window}`; see §6.2 before M9.

### 5.3 Verification

The e2e suite (~315 files) is the primary mechanism, used as-is:

1. **Cell output stays byte-identical** per wave (the existing snapshot/visual
   harness). A diff is a defect or a reviewed intended change. Reproducing exact
   spacing — including cases that look wrong — is a real part of each wave; make
   any deliberate visual change *separately*, after the wave, so a regression is
   distinguishable from an intended change.
2. **`scene_parity.rs` passes** through every wave — the web projection has not
   diverged. As each surface migrates, its `Scene` projection becomes the
   component's props rather than a separate output, but the projected data must
   match until the web frontend consumes `LayoutSpec` directly.
3. **The standing parity oracles** (event-time geometry vs paint walk; focus
   ring) stay enabled until the surface they cover migrates, then are removed
   with it.
4. **Per-wave routing tests** — the existing precedence tests (clicks not
   reaching the buffer through a popup, modality, focus order) pass unchanged
   against the new implementation.
5. **New `LayoutSpec`-level assertions** by key are *added* alongside the cell
   assertions, not in place of them.

### 5.4 Risks and stop points

1. **L1/L2 semantics are fixed** (they are — Part 1 is done and its deviation
   register is closed). The library is frozen; a wave that needs a library change
   is a signal to stop and fix the library, not to fork behavior into the editor.
   Two such changes are already foreseeable (styled text spans, scrollbar
   markers — §6.2): schedule them as library work *before* the wave that needs
   them rather than discovering them mid-wave.
2. **Cell-identical output is a hard constraint**, and the biggest single cost
   per wave.
3. **M6 changes plugin-visible behavior** (state survival) but does *not*
   break the API. The "required keys" half of this risk was checked and
   withdrawn — see §4.6 — so `WidgetSpec` is frozen and the wave needs no
   release cycle of its own.
4. **M8 (Settings) is optional as a stopping point.** It is the largest interior
   and the least coupled to dispatch; stopping after M7 with Settings still on
   the current (already-half-unified) path is a supported end state.
5. **Two implementations of one surface must not persist across waves.** A wave
   that cannot delete its predecessor indicates a defect in the seam — fix the
   seam rather than accumulate a second UI stack.

   **A wave may split by channel, and must say so.** That rule is about the
   same behaviour existing twice, not about a surface having to migrate in one
   step. Paint, pointer, keyboard and modality can move separately — the
   context menu moved paint, then pointer, and still has its keyboard grab and
   its `layer_rank` entry — and that is legitimate *provided each channel has
   exactly one implementation at every moment*. The menu's pointer input lives
   in the tree and nowhere else; its keyboard grab lives in the chrome
   component and nowhere else.

   The cost is a surface spread across three files mid-wave, which is only
   tolerable while it is *visibly* mid-wave. So a split wave names, in the
   surface's own module doc, which channels have moved and which have not —
   `view/shell/context_menu.rs` does — and the wave is not finished until the
   last channel lands and the old file is deleted.
6. **The caret and the caret-anchored popups are the subtle seam.** Get the M0
   caret arbitration and geometry-cache bridge right, or M4/M5 popups mis-anchor.

---

## 6. Decisions and open questions

An independent review of this doc against the source (the review verified the
§2 survey nearly claim-for-claim, corrected the §4.4 seam mechanism to the
fold-callback form now described there, and investigated the original open
questions) settles the first five below and leaves a concrete pre-wave decision
list.

### 6.1 Answered

1. **Web frontend endgame — a parallel track, not a wave.** `Scene` +
   `scene_parity` stay the web wire format through M9: until the frame inverts,
   the UI is hybrid, and a `LayoutSpec`-consuming web frontend would need both
   protocols at once. After M9 the web can consume `LayoutSpec` (it is designed
   for DOM patching by key) — but buffer panes remain cell slices either way,
   because they are `Draw::Host`; the display list never fully replaces the
   cell channel.
2. **`WidgetMutation` — still needed, reinterpreted.** `List::windowed` solves
   *render* cost, not *wire* cost: the 5000-node `js_to_json` walk happens at
   the plugin↔host boundary regardless of how rows are rendered. Keep
   `AppendTreeNodes`/`SetItems` & co. as **edits to the host-side data source**
   that the windowed builders read by index, rather than edits to a retained
   widget tree. The plugin API survives unchanged.
3. **Mutate-then-decline keyboard rungs — expressible per node.** A bubbling
   handler that returns `None` continues the walk, so "process and fall
   through" ports directly. What does *not* port is **cross-layer guard
   coupling** — a rung consulting a higher layer's state mid-walk (the
   unfocused-popup Esc interception guarded by
   `popup_blocked_by_higher_modal`; the popup rung entangled with the
   deferred-action queue). Each such rung is converted in M2/M4 into a
   capture-phase observer on an ancestor or an `Action` that re-dispatches —
   with a routing test per rung, because cell-identical output cannot see
   these behaviors.
4. **Content hover trackers — feasible, but not via a leaf-raised message.**
   Render objects cannot emit messages; they don't need to. The LSP-hover and
   terminal-link state machines already live in app state (`app/hover.rs` +
   the async bridge); `build(ed)` includes the popup `Layer` when that state
   says so. The hard part is the anchor — the §4.4 geometry bridge /
   `compute_content_layout` — plus one verification: pointer *motion* must
   keep reaching the leaf for the debounce machines while `fresh-ui` hover
   exists above it.
5. **Multi-window — one `Ui`, window subtree keyed by `WindowId`.** Fresh
   windows are not concurrent OS windows; exactly one is rendered per frame.
   Cross-window content (orchestrator session preview, `WindowEmbed`) appears
   as additional `Host` leaves. Switching windows disposes the outgoing
   subtree's element state — acceptable because anything that must survive a
   switch is `Window` app state by the §4.3 rule, matching today's behavior.

### 6.2 Open decisions — settle each before the wave it blocks

1. ~~**How does the editor learn the tree claimed an event?**~~ — **decided,
   and it was the review's finding rather than the plan's.** The seam's
   contract was briefly carried in the message channel: `shell_dispatch` read
   "claimed" off "did any message come back", and a `UiFact::Consumed` message
   existed to mean "there is no message". That produced a real bug (a
   right-click outside a menu closed it and then failed to open the next one)
   and silently swallowed hover from the legacy trackers.

   `fresh-ui` reports it now — `Dispatch { msgs, claimed }` — which the
   library's own gap rule required, since `propagate_all` already computed it
   and threw it away. **The general lesson: when the editor finds itself
   inferring something about routing, that is a missing library capability, not
   a place for an editor convention.**
2. ~~**Migrate in paint order**~~ — **superseded.** The rule was: one fold,
   running after every legacy painter, so a region may become native only once
   everything that paints over it already has. It was the migration's real
   ordering constraint and it was stricter than it looked — the file explorer
   paints *first* among the legacy painters, so under one fold it could not
   migrate until nearly everything else had.

   The fold is two passes now, `Background` before every legacy painter and
   `Overlay` after all of them, so each band lands where its surface belongs
   and the ordering rule retires. Nothing replaces it: which band an item
   belongs to is `LayoutSpec::layers_from`'s answer, not a convention anyone
   has to maintain.
3. ~~**The fold-callback API**~~ — **shipped, and it is the render path.**
   `HostPainter` + `impl HostPainter for Editor`; paint order, clipping, the
   caret rule and the borrow are covered by tests, and the split grid is
   painted through it. The per-frame state and the seven published rectangles
   travel on the editor (`pending_body_state` / `pending_body_output`) rather
   than through the call, because `paint_host` carries a region and a
   rectangle and nothing else — a display list is geometry, not the editor's
   hover state. The `Ui`-beside-`Editor` constraint it revealed is recorded in
   §4.4.
4. ~~**Colour that is not a theme name**~~ — **decided and shipped: a name is
   always real theme keys.**

   The entry here proposed minting `dyn:N` names during build. That was wrong
   twice over — it makes `build` mutate a table, which goal 3 forbids in as many
   words, and `dyn:17` destroys the provenance a `ThemeKey` exists to carry. The
   review's content-derived alternative was better but still invented a name
   space.

   What shipped needs neither, because the editor already had the answer:
   `Theme::resolve_theme_key("section.field")` is a generic, table-generated
   name resolver, and `Theme::resolve_modifier_key` reads the attribute declared
   on the same table row. The shell was hand-writing a twenty-arm match over
   names of its own beside it.

   **A shell `ThemeKey` is `fg_key/bg_key`**, optionally `+bold` / `+underline`.
   A cell needs two colours and an `Item` carries one name, so the name is a
   pair; both halves resolve through the editor's own table. Nothing is invented,
   every colour on screen traces to an entry a user can edit, and the six
   spellings for two orthogonal attributes
   (`menu.bar.item{,.mnemonic}{,.active,.hover}`) collapse into one pair plus
   composable attributes — which is the point, because the blow-up arrives in
   earnest with the explorer's git status × selection × cut × focus.

   It also converges two things that were saying the same thing in different
   words: the theme inspector has always recorded provenance as
   `ThemeRun { fg_key, bg_key }`, exactly this pair. `MenuRowStyle::shell_theme`
   is now derived from `theme_keys()`, so the display list and the inspector
   cannot disagree — and unifying them fixed a real drift, where a hovered bar
   label reported its *resting* keys to the inspector while painting hover
   colours.

   **The one genuine limit**, for whoever migrates the explorer: plugins may
   send `OverlayColorSpec::Rgb`, a shipped wire variant, and a raw colour is not
   a theme entry. Two things make it smaller than it looks. Every in-repo slot
   provider already sends `ThemeKey` (`syntax.string`, `syntax.type`,
   `ui.file_status_modified_fg`, and LSP diagnostics), so the built-ins need
   nothing. And the plugins that genuinely need raw colour are the ones
   displaying colour *as data* — `theme_editor.ts`'s swatch grid — which should
   not be theme entries. When the explorer migrates, the choice is to give that
   family a self-describing name (`rgb:7f3fbf`, resolved by the same parser) or
   to leave those rows behind a `Host` leaf. Neither needs a side table, and
   neither is needed before then.

   `resolve_overlay_color` collapsing a spec to a `Color` inside the slot
   *cache* — long before any description exists — is the thing to move when that
   day comes.

5. **The dropdown chain's placement** (the tail of M3). *A dependency, not a
   decision — and the previous entry here got both its facts wrong.*

   It claimed `Anchor::Node` "can name the parent but not the offset", so a
   `Place` offset was needed as a base PR. False: **anchor to the row above**.
   `Place::RightOf` then yields the wanted rectangle for every row, today, with
   no library change. It also claimed `fit_dropdown_area` "runs once, so nothing
   can drift" — it ran twice per frame in release and three times in debug, from
   three different bar rectangles, held together by a `debug_assert_eq!` that
   release compiles out. (Now one walk; see `Editor::menu_layout_frame`.)

   The real blocker is upstream of placement: **the chain has no content model
   in the tree to place.** `DropdownLevel` carries `x`, `y`, `width` on a type
   whose only job is to produce a description — the design doc's own stop sign
   ("a description type with a rect field indicates layers 1 and 3 have been
   merged") — plus pre-fitted strings, a pre-decided item count, and a
   pre-chosen width. Nothing is measured, so `Place::RightOf` has nothing to
   place against and `Fit` is not expressible at all.

   And the two rules genuinely differ, so turning `fit` on is a pixel change,
   not a refactor. Measured on one frame (w=22, parent box x=4 w=12, sub w=10,
   no room right): `Fit::FLIP` gives `sub.x = 12`, the editor's rule gives
   `x = 1` — **eleven cells apart**. Vertically `fit_dropdown_area` never flips;
   it *drops items* to fit, where `Fit` would move the box and keep them all.

   **The order, therefore:** rows become real nodes → the box measures itself →
   `Fit` becomes expressible → the flip/truncation divergence above becomes a
   reviewed pixel change → *only then* is the border offset a question, and the
   leading answer is `Anchor::Node(row_above)`, which needs nothing.

   If a primitive is still wanted after that, it is **not** a scalar offset. It
   is the relationship — *place the layer so that this descendant lands on the
   anchor* — which survives a border-thickness change and has a real second
   consumer in this repo's future: `crates/fresh-gui/src/native_menu.rs`, where
   a macOS-style popup button puts the *selected* item over the trigger.

   **Before relying on any of it, note the API is largely unexercised**:
   `Anchor::Node` has zero callers and zero tests in the whole repository, and
   four of six `Place` variants (`Above`, `RightOf`, `LeftOf`, `Fill`) are never
   constructed anywhere — library, tests, demo, or editor. Whoever uses one owes
   it a test first.

6. ~~**Inline styled text**~~ — **not a gap; it was already there.** Listed as
   blocking M3/M5, but `text_runs` takes styled pieces as one logical string
   that measures, wraps and truncates as a unit. Mnemonics, match highlights
   and the explorer's git colouring all use it. Checked against the library
   rather than the note, while surveying for the second base PR.
7. **Per-leaf decomposition of `render_content`** (blocks S5). *Decided: split
   the orchestration per leaf. The grid's structure is layout, and layout is
   the tree's.*

   The alternative — keep the whole grid as one `Host` leaf — was never really
   open. It would leave the frame's **dominant** region as the one place the
   shell does not own the geometry, and the argument for the shell owning the
   rest applies here with more force, not less: `SplitNode::get_leaves_with_rects`
   is a second layout engine, recursing over ratios and reserving a cell per
   separator, and everything downstream (separator drags, per-leaf scrollbars,
   tab strips, click-to-byte) is keyed on rectangles it alone produces.

   **The rule is expressible today, exactly.** `split_rect_ext` reserves one
   cell for the separator, converts the ratio to cells, and clamps the first
   child to a minimum so the sibling keeps `MIN_PANE_{WIDTH,HEIGHT}`. That is
   `row([first.w(Cells(n)), divider.w(Cells(1)), second.flex(1)])`, with `n`
   from the same helper — a ratio pinned to a minimum is **app logic keyed on
   the available extent**, the same shape as the dock's bail-out, and
   `Frame::resolve_dock` is the pattern for computing it before `build()`
   rather than inside it. So there is no new library capability here and no
   pixel change to negotiate: the tree can be asserted equal to
   `get_leaves_with_rects` over a sweep of shapes and sizes, the way
   `ui_shell_frame_parity` does for the frame.

   What the decision costs is the **orchestration split**, and that is the real
   work rather than the description. `render_content` mixes three things: a
   cross-leaf preamble (`expand_visible_buffers`, the active split, "are there
   several"), a per-leaf paint, and seven accumulated output vectors. Per-leaf
   means the preamble is computed once into state the callback reads, the paint
   becomes `paint_leaf(id, rect, …)`, and the accumulation happens across calls
   instead of inside one. `render_phantom_leaf` shows the per-leaf path exists.

   **Two things the tree has to express that `get_leaves_with_rects` does not**,
   found while settling this and worth having before the estimate:

   - `split_layout` reserves a leaf's tab-bar row and its vertical and
     horizontal scrollbar columns, per leaf, from flags that differ per leaf (a
     live-PTY terminal suppresses its scrollbar; a non-scrollable panel has
     none; a buffer-group panel suppresses its tab bar). So the leaf's `Host`
     is the **content** rect, and the pane is
     `col([tab_strip?, row([Host::leaf(id), vscrollbar?]), hscrollbar?])` —
     which is the target shape anyway, arrived at by necessity rather than by
     ambition.
   - `expand_visible_buffers` recurses: a leaf whose active tab is a
     `Grouped` node lays that subtree out *inside its own content rect*, and
     the inner leaves are panes in their own right. So `split_grid` is
     mutually recursive with the pane, not a flat walk over leaves.

   Neither needs anything the library lacks — both are ordinary nested layout.
   They do mean the wave is "the grid **and** the pane", not "the grid, then
   the pane later".

   **And the real blocker is the model's query, not the description.** A tree
   that recursed over `SplitNode` calling `split_rect_ext` would agree with
   `get_leaves_with_rects` by construction — one rule, two walks — but it would
   still be two walks, and the goal is not to add one. The end state is that
   the tree *is* the layout and `get_leaves_with_rects` becomes a read of it;
   `WindowLayoutCache::split_areas` already holds exactly that answer.

   What stops the swap is `SplitManager::get_visible_buffers`, which has
   around eight callers outside the render path — `plugin_dispatch`, `window`,
   `composite_buffer_actions`, `lifecycle`, `terminal`. An earlier draft of
   this paragraph said they ask two different questions, a read and a
   hypothetical. **They do not**, and the correction matters: the one caller
   that looked hypothetical (`composite_buffer_actions::flush_layout`) passed a
   rectangle it invented — the whole terminal, which is not the box the grid is
   laid out in — and then dropped every rect it got back. It wanted the leaf
   *set*, which does not depend on the box at all. It asks `visible_leaves()`
   now, and the invented rectangle is gone.

   So there is one question: **where are the leaves in the box the editor
   currently has**. What makes it awkward is *when* it is asked. `apply_layout`
   calls it after setting a new size and before the frame that would lay
   anything out, so it cannot be a read of the last laid-out tree — it needs
   the answer for a size no tree has seen yet.

   `fresh-ui` can give that answer: `Ui::frame(node, size)` lays a description
   out at any size, not only the current one. So the end state is one
   implementation — the grid description — asked live by the render path and
   speculatively by `apply_layout`.

   **And it is affordable.** That was the last open question and it is now
   measured (item 10): a *whole frame* — menu bar, status bar, prompt row,
   dock, explorer and body — lays out in **122µs retained and 163µs cold**, in
   a debug build on a loaded container. `apply_layout` runs on resize, a
   handful of times a second at worst. `split_tabs_width` is the per-frame
   caller and the grid subtree is a fraction of what that figure covers. The
   pure function can become a layout without a budget argument.

   **Landed.** `SplitNode::get_leaves_with_rects` is a read of
   `shell::splits::grid`, laid out at whatever box the caller has. The rule
   inside it — `split_rect_ext`, the ratio pinned to `MIN_PANE_*` — is
   unchanged and still the model's; what moved is the recursion around it. The
   original walk stays as `reference_leaves_with_rects`, compiled only under
   `cfg(test)`, because a replacement is only as trustworthy as what it was
   checked against: seven shapes across five sizes, plus the dividers against
   `get_separators_with_ids` and the maximized case.

   A three-pane grid costs ~38µs cold in a debug build, against callers that
   run once a frame or on resize.

   What this buys is the next step, and the first of it has landed: **the
   dividers are gestures.** A divider node knows which container it is, so
   `handle_click_split_separator` — which walked a recorded list of separator
   rectangles comparing a click against each in turn to recover that identity
   — is gone, along with the `chrome:split_separators` box and its hover rail.
   The drag it arms is still `PointerGrab::SplitSeparator`, which retires with
   the pointer-capture wave.

   **And the pane boundary showed itself exactly where this entry predicted.**
   A `Grouped` subtree is laid out inside a pane's *interior* — past the tab
   bar and the scrollbars the painter reserves — so its dividers were not in
   the main tree's description at all, and moving the main ones broke the
   grouped drag until they were separated out under their own box. That is the
   "grid **and** the pane" this entry warns about, met in practice: the grid
   alone was landable, and it stopped at the pane's edge.

   **The pane's interior has since landed too, and the pointer half of S5 with
   it.** In order, each step landing on its own:

   - **The rule for which chrome a pane has.** Four places decided whether a
     pane gets a tab strip and which scrollbars, each writing the boolean
     algebra out again — and the paint's copy narrowed it by two refinements
     the others did not know about (a `Fixed` panel earns no bar; a terminal
     streaming its live grid gives up the scrollbar column), so the outer pane
     of a buffer group and `flush_layout` were laying panes out a column wider
     than the paint recorded. It is `PaneChrome::resolve` now, gathered once by
     `Window::pane_chrome` and read by both the description and the painter.
     The three callers that cannot see a buffer map say so in one place rather
     than in three copies of `&& !`.
   - **The tab strip**, which took the split controls with it: those are drawn
     over the tab row, which two boxes said with z 70 over z 60 — an ordering a
     node has to state itself, because the tree runs *before* the legacy walk.
     `tab_bar_split_at` is gone with it.
   - **The buffer group's grid**, mounted in the pane's content node — which
     *is* the rectangle the painter lays that group out in. Its panels are
     panes with their own keys and their own `PaneChrome`; its dividers are
     ordinary dividers. `chrome:group_separators` and
     `handle_click_group_separator` are gone.
   - **Both scrollbars**, then **the content**. With those, `chrome/splits.rs`
     has no `ChromeComponent` in it at all: the file is the handlers the nodes
     dispatch to. Each of them used to open by asking every recorded rectangle
     in turn whether it contained the point; they take a pane. What they still
     read back is geometry that genuinely records the last paint — a thumb's
     extent, the tab renderer's columns — and the pane's own content rectangle,
     read from the node that defines it.

   Two things had to move with the content, and both are the same lesson: **the
   tree runs first, so anything that used to sit between the old capture band
   and the legacy walk now sits behind whatever the tree claims.** A live
   terminal's own mouse and the Ctrl+Click that opens a path it printed both
   ran in that gap; they are `Editor::pane_content_takes_pointer`, asked at the
   head of the content's own handlers, which is where they belonged. And
   clicking a scrollbar *track* jumps the thumb under the pointer and says so
   by writing the hover target — to the legacy walk's field, which the tree's
   answer now shadows.

   **And the paint half has landed, so item 7 is closed.** A pane is its own
   `Host`. The fold reaches one at a time and hands each the rectangle layout
   gave it — the same rectangle the pointer half already routes by, which is
   what makes them one answer rather than two that agree. What is left to the
   body's own leaf is what belongs to no pane: the pass every pane shares, and
   the separators between them, which are the gaps.

   The orchestration split this entry called "the real work rather than the
   description" went in three steps, each landing on its own:

   - **The three phases named.** `prepare_content` returns the `ContentPass`
     every pane shares (the expanded visible list, the active split, "are there
     several"), `paint_leaf` is one pane's, and `paint_separators` is what
     belongs to neither. `render_content` is those three in order.
   - **Carriers instead of forty parameters.** `paint_leaf` came out of the
     loop with its locals as its signature, which was deliberate — the body
     moved without an edit inside it — and unusable from a trait method that
     carries a target and a rectangle. `FrameFacts` is what every pane reads
     and none of them writes (`Copy`, because `RenderStyle` is); `Stores` is
     what a pane writes through; `PaneAreas` is the sink. A **sink** rather
     than a return value is the shape the flip needs: the panes are painted one
     call at a time, so there is nothing for one of them to return this in.
   - **The flip.** `pane_inert` becomes a stack — a pane is two things over one
     rectangle, the painter's cells and the geometry that answers for them —
     with `host(pane_host_id(leaf))` under the interior, because the interior
     paints nothing.

   Two things moved with it. The painter is **frame-scoped now**: `paint_host`
   carries a target and a rectangle and nothing else, so whatever a painter
   needs beyond those two travelled as fields on the `Editor`, and the flip
   needed two more. `BodyPainter` is built by `render`, folded with, and taken
   from; `pending_body_state` and `pending_body_output` went with it, and
   `BodyOutput` is the split renderer's own sink rather than a second list of
   the same rectangles. And `record_scrollbar_theme_runs` runs **after** the
   fold, because `apply_theme_runs` patches cells the panes are still
   appending — it needs every pane painted, which is what "after the fold"
   means now that a pane is its own host.

   What remains of this entry is not the grid: the dividers' drag is still
   `PointerGrab::SplitSeparator` and retires with the pointer-capture wave, and
   the scrollbars stay behind a `Host` pending item 8.
8. **Scrollbar markers** (blocks M9). `Draw::Scrollbar` carries only
   `{offset, content, window}`; the plugin overview-ruler marker API has no
   expression. Extend the library's scrollbar, keep scrollbars behind the
   `Host` leaf, or drop the API.
9. ~~**The message-type split**~~ — **decided and shipped** as
   `UiMsg::{Action, Ui(UiFact)}` (`view/shell/msg.rs`). Anything bindable stays
   an `Action`; positional facts are `UiFact` and are never serialized.
10. ~~**Frame scheduling and rebuild cost**~~ — **measured.** It was written
   as an M0 exit criterion; S1, the context-menu wave and the frame swap all
   shipped without it, so calling it a gate was wrong. It became load-bearing
   for a different reason — item 7 needs to know whether a layout can be asked
   for on demand — so it is taken now, by
   `a_frame_layout_is_cheap_enough_to_ask_for_on_demand`.

   A whole frame, with the menu bar, status bar, prompt row, dock, explorer
   and body: **122µs retained, 163µs cold**, in a debug build on a loaded
   container. Retained is the reconcile a per-frame caller pays; cold is what
   a caller with no `Ui` of its own pays, which is the shape `apply_layout`
   would use. Both are far below anything that would make "lay it out and ask"
   the wrong answer, which is what item 7 needed to know. The test reports the
   figure and asserts only a bound three orders of magnitude clear of it — a
   wall-clock threshold is a flake waiting for a loaded runner.
11. **Row visibility under squeeze** — still open; S1 shipped without deciding
   it, and the divergence is *recorded* by
   `squeeze_band_starves_a_different_row_than_ratatui`, not resolved by it. When the visible fixed rows
   cannot fit, `fresh-ui` and ratatui starve different rows (§5.0). Decide
   which rows `build()` drops as a function of available height, making the
   choice explicit app state instead of inheriting either engine's starvation
   order. Pinned today by
   `squeeze_band_starves_a_different_row_than_ratatui`.
12. **Frame-buffer animations.** Cell-snapshot effects have no display-list
   expression: keep them as post-processes over the folded buffer (beside
   `convert_buffer_colors`), or retire them deliberately.
13. **Theme-inspector granularity** (§4.5). Chrome provenance coarsens from
   per-cell to per-item; confirm the inspector survives, with buffer cells
   keeping the per-cell map via the leaf.
   *Sharpened by the search-options row.* Every migrated surface now carries
   its provenance in the display list already — a shell theme name **is** the
   `fg_key/bg_key` pair the inspector records (§ `shell_theme`) — so the
   inspector could be fed from the fold, once and for all surfaces, instead of
   from a per-surface `theme_runs` walk. What stops it today is a type: the
   fold's names are `String`s built at build time and `ThemeRun`'s keys are
   `&'static str`. Until that is settled, a migrated surface whose legacy walk
   is deleted records nothing, and the inspector says "No theme key recorded
   here" — which is what the search-options row said before it migrated too
   (issue #2362 pinned exactly that), so nothing regressed; it simply did not
   improve. The menu bar is the exception only because its legacy walk still
   runs to produce runs.
14. ~~**A bordered box does not clip its children**~~ — **fixed in the second
   base PR** (#3095). `.border()` insets what is inside it but nothing bounded
   it, and only a `Viewport` bounded anything at all — so a row the panel could
   not fit painted over the panel's own right border. `BoxProps.clip` now
   exists, `border()` implies it, and the bound is the *content* rect rather
   than the outer edge. Pinned in `shell::file_explorer`'s tests as the escape
   (`clip(false)`) and the frame surviving (the default). The fourth item in
   the family that produced the first base PR — `pointer_mode` on any node,
   `min_w`/`min_h`, `Event::clicks` — and the first that was a correctness bug
   rather than a missing expression.

   Three things listed here as gaps were not gaps, and are closed rather than
   deferred: inline styled spans are `text_runs` (was item 6); modal focus
   containment is `Modality::Exclusive` + `FocusScope`; and content shaped by
   its own extent — a rule, a divider, the explorer's own full-height grip
   glyph — is what `layout_reader` is for. Still genuinely absent: markers on
   `Draw::Scrollbar` (item 8).

15. **Wheel-semantics parity** (M4/M5). Today's wheel walk has no dedup and no
   opacity gate *by ruling*; `fresh-ui` chains wheels by "a `Viewport` claims
   only if its offset changed." Close but not identical — e.g. a
   scrolled-to-bound popup over the buffer. Land an explicit parity test
   before relying on either behavior.
