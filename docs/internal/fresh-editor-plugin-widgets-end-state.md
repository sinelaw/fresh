# The plugin widgets' end state

**Status:** a design, not a plan phase. It argues a target architecture for
`crates/fresh-editor/src/widgets/` and the surfaces that consume it, and then
an ordering to reach it. It is a companion to
[`fresh-editor-retained-mode-plan.md`](./fresh-editor-retained-mode-plan.md),
not a section of it; where the two disagree, §1.6 below says so and says why.

**Evidence discipline.** Every load-bearing claim carries a file and a line,
read at `2f06f0aef`. Claims I did not verify against the source are labelled
**inferred** and say what would confirm them. Line numbers drift; the item
names are the stable part of each citation.

---

## 0. The thesis in five sentences

`WidgetSpec` is a description, and the editor already renders it three
different ways: into a `Node` tree (dock, floating panel, settings, the panes
whose panel owns its scroll), into `TextPropertyEntry` rows inside a real
buffer (the panes whose panel rides the *buffer's* scroll — `git_log`), and
into a JSON projection the web frontend paints itself
(`view/scene.rs::widgets_view`, line 1161). The second of those is **not** a
duplicate of the first: for that class of panel the rows genuinely are buffer
lines, and the buffer's cursor, scroll, search and `mouse_click` coordinates
are a contract plugins read (`app/render.rs::pane_panel_owns_its_scroll`, line
8060; `crates/fresh-editor/plugins/git_log.ts` lines 137–145, 257–260). So the
end state is not "delete the runtime": it is **one behaviour authority and
three projections**, with the *node* projection as the only one that answers
the pointer, the keyboard and the geometry.

What must die is not the collector but its **second life as a source of
truth**: the hit list, the focus key, the tabbable ring, the box arena and the
row budget that the collector files into `WidgetRegistry` and that dispatch
then reads back. Every one of those is either a fact the tree already holds,
or a fold that belongs to the element that renders it. Delete those six
fields and the "two renderers, and a gate deciding which counts" problem is
gone even though `WidgetSpec → TextPropertyEntry` still exists — because it
will no longer be *consulted* about anything except the text it produces.

---

## 1. What is actually there, verified

### 1.1 Sizes

| module | non-test lines |
|---|---|
| `widgets/render.rs` | 3 494 |
| `widgets/kinds/*` (17 files) | 7 294 |
| `widgets/mod.rs` | 42 |
| `widgets/registry.rs` | 776 |
| `widgets/layout_box.rs` | 266 |
| `widgets/actions.rs` | 306 |
| `widgets/text_click.rs` | 141 |
| **total** | **12 319** (17 915 with tests) |

Counted as "lines before the first `#[cfg(test)]`". The adapter that replaces
its render half, `view/shell/widgets.rs`, is 3 224 non-test lines of 6 013.

### 1.2 `widgets/` is not one thing

The directory name and the plan both say "immediate-mode renderer". Roughly
half of it is not. `widgets/kinds/mod.rs` (line 162) declares a
`WidgetImpl` trait with **eleven** members, of which exactly one — `collect`
— is rendering:

- `collect` — spec → rows + hit areas + next instance state (rendering);
- `box_meta` — the node's tag, key, and dispatch flags (focusable, scrollable,
  pointer-opaque, focus-trap, picker-scroll-target);
- `on_key`, `on_pointer`, `on_wheel`, `on_focus_change` — the behaviour;
- `activate_event`, `arrows_advance_focus`, `picker_nav`,
  `activates_on_picker_enter`, `picker_activate_event` — declared capabilities
  the panel-level router asks instead of matching kind strings.

`behavior()` (line 311) is the single surviving `match` on a spec kind, and it
is total. This is the part of `widgets/` that is *right* and should survive
whole. The adapter already depends on it — `view/shell/widgets.rs::on_the_ring`
(line 456) asks `box_meta` rather than restating the focusability rules, and
its own doc says why: "Writing the eight arms out again would be the ninth copy
of a rule."

The behaviour layer is also already decoupled from the registry *mount*:
`app/input_dispatch.rs::toggle_overlay_toolbar_widget` (line 517) builds a
throwaway `WidgetPanelState` purely to run one kind's `on_key`. That is
evidence the trait's `&mut WidgetPanelState` parameter is really "a place to
put instance state", not "a mounted panel".

### 1.3 Where the collector still runs

Verified by grepping every caller of `render_spec*` / `render_collected`:

| site | when | what it is for |
|---|---|---|
| `plugin_dispatch.rs:5147, 5226, 5648, 5741` | per plugin mount/update IPC | writes the registry's six side-effect fields, plus buffer text for pane panels |
| `widget_runtime.rs::rerender_widget_panel` (889) | per host-driven state change (focus, key, wheel, popup hover, stale height) | the same |
| `view/shell/widgets.rs:1152` | **every frame, once per card** | a card list's item subtrees |
| `view/shell/widgets.rs:1809` | **every frame** | a markdown document's whole reflowed text |
| `app/render.rs:4509` and `6313` | every frame the overlay prompt is up | measures, then *paints*, the prompt toolbar |
| `app/shell_host.rs:1256` (`settings_widget_hit`) | per click on a settings text field | resolve a caret byte |
| `view/settings/mouse.rs:495` (`settings_entry_widget_hit`) | per click on an entry-dialog text field | the same |

**This corrects the brief.** The runtime does not "run inside the retained
description build on every frame" in general. It runs *inside* the description
build for exactly two arms — card-list items and markdown documents — and both
are documented at their site as the last two. Everything else is event-driven,
or is a separate painted surface (the overlay prompt toolbar), or is a click
path.

What *does* run every frame for every panel is `panel_interior`
(`app/render.rs:8064`), which deep-clones the panel's whole `WidgetSpec` and
its whole instance-state map. For the orchestrator dock that is a several-
thousand-node clone per frame. Its own sibling `panel_is_described`
(`app/render.rs:7888`) exists precisely because that price "is the right price
once a frame and the wrong one on every motion event" — which concedes it is
paid once a frame.

### 1.4 What `WidgetRegistry` holds, and who reads it

`WidgetPanelState` (`widgets/registry.rs:281–316`):

| field | written by | read by |
|---|---|---|
| `spec` | plugin IPC | everything |
| `buffer_id` | mount | routing |
| `hits` (290) | the collector | `deliver_widget_hit_by_index`/`_semantic` (282, 312), `click_handlers.rs:285`, `probe_floating_widget` (2841), `scene.rs:1246`, `text_click.rs` |
| `instance_states` (293) | the collector **and** every `on_key`/`on_pointer`/`on_wheel` | the description (`Ctx::states`), the web projection, the collector |
| `focus_key` (299) | the collector, `handle_widget_focus_advance` (1183), `UiFact::WidgetFocus` (`shell_host.rs:1564`) | the description, `router::WidgetPanelView`, the web projection |
| `tabbable` (304) | the collector | `handle_widget_focus_advance` fallback (1198), click-to-focus (189) |
| `effective_rows` (310) | the collector | `List::on_wheel`/`on_key`, `Tree::on_wheel`/`on_key` |
| `boxes` (315) | the collector | `focus_ring_scoped` (1196), wheel routing (1479), `probe_floating_widget` (2873), text-drag (2449) |

### 1.5 The pointer and keyboard, verified

- The described nodes already carry their own hits: `hit_node`
  (`view/shell/widgets.rs:2795`) wraps a piece in a gesture that emits
  `UiFact::WidgetHit { slot, hit, at, clicks }`. The `HitArea` it carries is
  used **semantically** (key, kind, event type, payload, `context_click`); its
  geometry fields are not consulted — except `byte_start`, see §5.2.
- `probe_floating_widget` (2841) is reachable only from
  `handle_floating_widget_click` (2915), whose two callers are
  `UiFact::DockPress` (`shell_host.rs:2079`) and
  `chrome/floating_modal.rs:83`. `DockPress` is documented as "only reachable
  for a dock with no described panel" (`view/shell/msg.rs:383`), and with no
  panel the probe's first line returns `None`
  (`self.panel(slot)?.last_inner_rect?`). The floating arm is reached through
  `UiFact::ModalPointer`, which `frame_box`'s claim returns `None` for when the
  panel is described (`view/shell/panel.rs:290–298`). Since
  `panel_interior` is now unconditional, **I believe both are dead**
  — *inferred*; what would confirm it is a `debug_assert!(false)` at the top of
  `probe_floating_widget` under a run of the dock and floating e2e suites.
- `panel.entries` and `panel.overlays` (the floating panel's row mirror) have
  exactly one reader between them: `probe_floating_widget` lines 2887 and 2891.
- The panel keyboard is `view/shell/panel.rs::keys_layer` (216): a full-frame
  `Modality::Focus` layer whose only child is an `autofocus()`ed focusable
  emitting `UiFact::PanelKey(slot)`. The premise in the brief is correct in
  every particular. `focus_scope()` (`fresh-ui/src/focus/mod.rs:331`) collects
  the active scope's subtree and then `retain`s only nodes `is_within` the
  topmost keyboard-owning layer (line 372–374); the panel's widgets are in the
  dock column (`frame.rs:629`) or in `panel::layer_for` (`frame.rs:710`),
  neither of which is inside `keys_layer` (`frame.rs:539, 543`).
- Keys bubble only. `dispatch_key` (`focus/mod.rs:574`) runs `propagate_key`
  (674) over `chain.iter().rev()` — target first, then ancestors — and only if
  nothing stopped it does it reach `resolve_intent` / `run_action` /
  `default_for_intent` (753), which is where `Intent::Next → move_focus` lives.
  **There is no key capture phase**, although the pointer path has one
  (`hit.rs:507`, `Phase::Capture`).
- The settings dialog's `keys` (`view/shell/settings.rs:299`) is a bubble
  `on_key` that calls `e.stop()` unconditionally. It is the *fallback holder of
  focus* (its own doc, 283–297), so with nothing else focusable it is also the
  target — and even when a control is focused, the control declines Tab and
  this stops it before intent resolution.
- There are **three** rings over widget specs, not two: the tree's
  (`on_the_ring`, writing `UiFact::WidgetFocus`), the panel box arena's
  (`handle_widget_focus_advance`, 1183), and the overlay prompt toolbar's
  (`app/input_dispatch.rs::overlay_toolbar_keys`, 422, over
  `prompt_toolbar_boxes`).
- `UiFact::WidgetFocus`'s applier drops every slot but Dock and Floating
  (`shell_host.rs:1569`), so a settings widget's focus gain is discarded. The
  plan's §6 records this correctly.

### 1.6 Where I disagree with the plan and with the brief

1. **"~12 500 non-test lines of immediate-mode renderer" understates what half
   of it is.** ≈ 7 300 of those lines are `kinds/`, and the majority of each
   kind file is `on_key` / `on_pointer` / `on_wheel` / capability declarations
   / pure state helpers, not rendering. Treating the directory as one thing to
   delete is the wrong unit.
2. **"The runtime still runs inside the retained description build on every
   frame"** is true of two arms, not of the runtime. §1.3.
3. **`covered()` no longer exists.** The plan's "until Phase 2 lands, stop
   widening `covered()`" and `app/render.rs:4279`'s "`panel_interior`'s
   `covered` gate" both describe a function that is gone; `panel_interior` is
   unconditional. See §9 for the comment defects this left.
4. **The plan's 2.2 progress list is stale in the branch's favour.** It says
   "Remaining: the single-line text field, the dual list, a residual tree
   shape, and three calls". Verified: the single-line field, the dual list and
   the tree have all crossed; **two** `render_collected` calls remain.
5. **Plan 3.3 — "the focus and pointer modalities … should be re-examined and
   probably removed" — is wrong for `Modality::Focus`.** Its own doc
   (`fresh-ui/src/desc.rs:544–547`) argues it is only meaningful for a surface
   whose interior lives outside the tree. A plugin panel described all the way
   down still has keys that are the *host's*: `router::widget_panel_key`
   returns `BlurUnconsumed` so an unbound `Ctrl+P` over the dock reaches the
   command palette (`input/router.rs:393`), and `FallThrough` whenever the
   plugin's own `defineMode` binds the key (`input/router.rs:355, 365`). A
   plugin's key policy is declared through `defineMode`, which is outside
   `WidgetSpec` and therefore outside anything the tree can see. So
   "confinement without a swallow" over a fully-described surface is a
   permanent requirement, and that doc paragraph is the thing to correct.
6. **A latent correctness bug on the described path**, see §5.2: the caret byte
   is computed as `hit.byte_start + at`, adding a byte offset to a column.

---

## 2. The target architecture

### 2.1 One authority, three projections

```
                    WidgetSpec  (frozen wire format)
                          |
                  kinds::WidgetImpl          <- the ONE authority
        +-----------------+-----------------+------------------+
        | resolve(spec, prev) -> instance state (folds + model) |
        | describe(spec, ctx) -> Node                           |
        | project_text(spec, ctx, w) -> rows                    |
        | events(spec, state) -> semantic hit table             |
        | on_key / on_pointer / on_wheel / on_focus_change      |
        | capabilities (focusable, scrollable, picker_nav, ...)  |
        +--------------------------------------------------------+
             |                    |                     |
        node projection      text projection       json projection
        (fresh-ui tree)      (TextPropertyEntry)   (scene.rs -> web)
        dock, floating,      pane panels that      every panel
        settings, panes      ride the buffer's
        that own scroll      scroll
```

The four rules that make this an architecture rather than a diagram:

**R1 — Only the node projection answers questions.** Geometry, hit-testing,
focus order and the row window are the tree's. The text projection produces
bytes and nothing else; the json projection produces a description and nothing
else. Neither files anything the other reads.

**R2 — `resolve` is the only writer of instance state during a build.** Today
`collect` writes `next_state` and the caller replaces the whole map, while
`on_key`/`on_pointer` mutate the same map directly. Splitting `resolve` out
means the build-time writer is a pure `(spec, prev) -> next` that seeds from
the spec on first mount and sanitizes against the current spec thereafter, and
it is run once per spec change rather than once per render of any projection.

**R3 — A fold lives with the thing that renders it.** The node projection's
folds are element state (`fresh_ui` viewport offsets, the single-line field's
horizontal window). The text projection's folds are the registry's. They are
not the same fold and must not share a slot, because they are windows onto
different layouts.

**R4 — Every semantic event is derived, never recorded.** `HitArea`'s
identity half is a pure function of `(spec, instance state)`; its geometry
half is an output of the text projection. Splitting the type is what lets the
identity half be computed on demand — which the code has *already* half done:
`deliver_widget_hit_semantic` (312) resolves a click by `(widget_key,
event_type, payload)` and falls back to `synthesize_list_hit` /
`synthesize_tree_hit` / `synthesize_control_hit`, each of which rebuilds "the
`HitArea` the renderer would have emitted" from the spec. Those synthesizers
are the right implementation; the recorded list is the fallback that should go.

### 2.2 What the end-state module tree looks like

```
widgets/
  spec_state.rs   instance state + resolve   (~800 lines, from registry.rs
                  + the resolve halves of kinds/*)
  kinds/          behaviour + capabilities + describe + project_text
  text/           the text projection: render.rs' formatters, kept whole
  registry.rs     panel identity, buffer id, spec, instance states, and a
                  focus-key *mirror* with one writer (§4.3).
                  Five fields, not eight.
```

`layout_box.rs`, `text_click.rs`, `HitArea`'s geometry fields, `hits`,
`tabbable`, `effective_rows` and `boxes` are gone.

---

## 3. Question 1 — does the immediate-mode runtime need to exist?

**No, as a renderer of anything the user points at. Yes, as three narrower
things, and one of them is permanent.**

### 3.1 What is deleted

- **The paint of any surface the tree describes.** For dock, floating,
  settings, settings-entry and scroll-owning pane panels, `collect`'s rows are
  already unread except through `probe_floating_widget` (§1.5). Deleting the
  probe deletes `panel.entries`, `panel.overlays`, and the whole reason those
  panels are rendered by the collector at all.
- **`hits`, `tabbable`, `effective_rows`, `boxes`**, and `focus_key`'s
  three writers, which become one. §4.
- **`layout_box.rs` in full.** Its three consumers are the second focus ring
  (`focus_ring_scoped`), wheel routing (`hit_path`), and the probe. The tree
  answers all three: focus order is `focus_scope()`, the wheel is the
  viewport's scroll chain, and the pointer is the hit path.
- **`text_click.rs` in full**, and with it both click-path re-renders. §5.2.
- **`WidgetTextClickGeometry`'s stamping**, whose own doc already predicts its
  end: "When Settings controls are eventually mounted as real panels, this
  snapshot and its stamping become redundant" (`text_click.rs:22–24`). The
  real answer is better than that prediction — see §6.3.

### 3.2 The three legitimate remaining jobs

**(a) Instance-state ownership keyed by widget key — permanent.** The
`WidgetSpec` doc says it outright at the type level: "instance state (cursor
offset, scroll, expanded keys, hover) is preserved on nodes whose `key`
matches" (`fresh-core/src/api.rs:1962–1969`), and every stateful variant's doc
says its spec field is "initial-only … instance state takes over thereafter".
That is a frozen promise. It cannot become element state wholesale, because
element state dies when the element unmounts and the promise is that a
plugin's re-emitted spec, or a panel scrolled off screen, does not lose the
value. So: **model state and cross-projection state stay keyed in the
registry; per-projection view folds move into the projection.** §4 draws the
line field by field.

**(b) Plugin event delivery and capability declarations — permanent.** The
`WidgetImpl` capability set is what lets the panel key router avoid matching
kind strings, and `activate_event` / `picker_activate_event` /
`arrows_advance_focus` / `picker_nav` are all pure functions of a spec. They
are unaffected by how the widget is drawn. `KeyFx` / `PointerFx` are the
right shape and should not change.

**(c) The text projection — permanent, and this is the finding that most
changes the plan.** `pane_panel_owns_its_scroll` (`app/render.rs:8060`)
divides pane-mounted panels in two, and the comment above it explains the
division precisely: a panel that rides the *buffer's* scroll has a real cursor
moving through real lines, and `git_log.ts` builds its whole selection model on
that ("Selection is cursor-driven … a row click places the buffer cursor, and
`cursor_moved` mirrors it into the selection", lines 257–260; the mode's `j`/`k`
are `move_down`/`move_up`, lines 137–145). For that class, `WidgetSpec →
TextPropertyEntry` is not a rendering duplicate — it *is* the semantics. The
plugin API's own founding claim is that "the buffer mid-bytes are
indistinguishable from hand-rolled output" (`widgets/render.rs:8`).

Deleting the text projection would therefore either break `git_log` or require
the split grid to describe a buffer, which the plan already declares
permanently out of scope ("The body … stays a `Host`, permanently and
correctly", §4.1). **So the honest end state keeps `WidgetSpec →
TextPropertyEntry` and keeps the formatters that make it, and what it loses is
its authority.** It renders into a buffer, and the buffer's own machinery —
`view_line_mappings`, `mouse_click`'s `buffer_row`/`buffer_col`, the byte-range
hit scan — resolves clicks on it, exactly as it does for any other virtual
buffer. That is `click_handlers.rs:285`'s existing path, and it should survive
*there and only there*.

The corollary: `WidgetRegistry::hit_test_row_aware` (699) survives, narrowed to
one caller and one surface. It stops being "the panel hit test" and becomes
"where in this text projection's rows did the click land", which is what its
byte-range scan actually is.

### 3.3 The residue that has no home yet

The overlay prompt toolbar (`app/render.rs:4509` measurement, `6313` paint) is
a `WidgetSpec` surface that is neither described nor a buffer: it is painted by
a ratatui painter, hit-tested against `prompt_toolbar_boxes`
(`shell_host.rs:1997`), and has its own focus ring. It is small — the bundled
use is a row of toggles — and it is the one place where deleting `layout_box`
costs something. **It should become the node projection**, on the same seam
every other described surface uses, and that is a prerequisite for deleting
`layout_box.rs`. It is not large: the toolbar is `Toggle`s and `Button`s, both
of which have described arms already.

---

## 4. Question 2 — who owns which state

Applying the derivation / fold / model distinction to `WidgetPanelState`.

### 4.1 `hits` — **derivation. Deleted.**

`HitArea` is two types wearing one name:

- *Identity*: `widget_key`, `owner_key`, `widget_kind`, `event_type`,
  `payload`, `context_click`, `row_target`. A pure function of
  `(spec, instance state)` — which is exactly what `synthesize_list_hit`
  (`widget_runtime.rs:394`) and its two siblings already are.
- *Geometry*: `buffer_row`, `byte_start`, `byte_end`, `overlay`. An output of
  the text projection.

End state: `WidgetImpl::events(&self, spec, key, state) -> Vec<WidgetEvent>`
yields the identity half. The node projection attaches one to each node it
builds (which is what `hit_node` already does, only with a recorded value
instead of a derived one). The web projection maps over the same function. The
text projection pairs each with a byte range in its own rows. Nothing is
stored, so nothing can be stale — which closes a defect class the code already
has scar tissue for: `deliver_widget_hit_semantic`'s entire identity-before-
index resolution exists because "a raw index goes stale the moment the plugin
re-renders between the pushed frame and the click" (line 297–301).

**A test that would have caught the class:** press a dock row, then push a spec
update that reorders the rows *in the same frame batch*, and assert the plugin
receives the event for the row that was drawn.

### 4.2 `instance_states` — **split by kind of state.**

| what | class | end state |
|---|---|---|
| `Text::editor` (value + cursor + selection) | **model** — the plugin is told through `change` and can set it via `WidgetMutation` | stays in the registry, keyed |
| `List/Tree::selected_index` | **model** — `select` events, `SetSelectedIndex` | stays |
| `Tree::expanded_keys` | **model** — `expand` events, `SetExpandedKeys` | stays |
| `Dropdown::selected_index`, `Number::value`, `DualList::included` | **model** | stays |
| `Text::completions*` (4 fields) | **model** — pushed by `SetCompletions` | stays |
| `Dropdown::open` | **view fold** (a pop-over's open-ness) | element state of the described trigger; the registry copy goes |
| `Text::scroll` (single-line horizontal window) | **fold** | already moved — `view/shell/widgets.rs::windowed` (2504) holds it in a `Cell` seeded once |
| `Text::scroll` (multi-line row window) | **fold** | the viewport's, once the markdown arm crosses (§6.2) |
| `List/Tree::scroll_offset` | **fold** | the viewport's for the node projection; the registry's for the text projection. **Two folds, one slot today** |
| `List/Tree::user_scrolled` | **derivation over a fold** — "did the last move come from the pointer" | it is the viewport's reveal policy; deleted for the node projection |
| `List::item_height` | **derivation from layout** | deleted; §6.1 |

The `scroll_offset` row is the sharpest one and is a **live one-slot-two-folds
hazard today**, not a future one. For a described panel the element owns the
window (`view/shell/widgets.rs:1001` uses `List::windowed_stateful` and reads no
registry scroll; the arm's own comment at plan §2.1 says so), while
`List::on_wheel` (`kinds/list.rs:19`) and `WidgetRegistry::set_list_scroll`
(`registry.rs:508`) still write the registry's. I could not find a live path
that reaches those writers for a described panel — the wheel goes to the
viewport's scroll chain and the scrollbar press path is inside
`handle_floating_widget_click` — so today this is dead writes rather than a
visible bug (**inferred**; what would confirm it is instrumenting
`set_list_scroll` and driving a dock wheel and scrollbar drag by hand). It
stops being safe the moment either projection reads the other's.

**The fix is not to merge them.** They are windows onto two different
layouts — the tree's and the text's — and merging is exactly the mistake the
`Text` caret window already made once ("the walk decided the window at the
width the *registry* recorded while the description drew it at the width layout
gave", plan §2.1). The fix is that the registry's `scroll_offset` becomes a
field of the **text projection's** state, not of the shared instance state, and
the node projection never has one.

### 4.3 `focus_key` — **one authority, and it should be the tree's.**

Today three writers: the collector's clamp, `handle_widget_focus_advance`
(1183), and `UiFact::WidgetFocus` (`shell_host.rs:1585`). End state: the tree
owns focus (§5); `focus_key` survives as a **mirror** written from exactly one
place — the `WidgetFocus` applier — because two consumers genuinely need a
string: `router::WidgetPanelView::focus_key` (`app/input.rs:525`), which the
dock's plugin-specific key policy branches on, and the web projection
(`scene.rs:1264`), for which there is no tree at all. A mirror with one writer
is fine; the defect is the three.

### 4.4 `tabbable` — **derivation. Deleted.**

It is `box_meta().focusable` in document order, which is what
`focus_scope()` produces from the same declarations
(`view/shell/widgets.rs::on_the_ring` already applies `box_meta` to build the
tree's ring). Two rings computing one order is the definition of the problem.

### 4.5 `effective_rows` — **a fact of layout, delivered rather than stored.**

Its four readers are `List`/`Tree`'s `on_wheel` and `on_key`, which need "how
many rows is this widget showing" for the page step and the wheel bound. In the
end state the tree knows that (it is the viewport's window height) and the
behaviour layer does not; so the key event must *carry* it. Concretely:
`on_key` grows a `viewport: Option<u32>` in its context, filled by the node
that dispatched the key from its own `LayoutInfo::scroll_window`. That is a
delivered fact, not a stored one, and it removes the "an auto-sized widget's
spec carries no number at all" problem (`registry.rs:308`) by construction.

### 4.6 `boxes` — **the tree's, and deleted.** §3.1.

---

## 5. Question 3 — keyboard and focus

### 5.1 The shape

Three things have to be true at once, and today no two of them are:

1. The panel's *widgets* are in the focus scope its keyboard confines.
2. The panel's keyboard **precedence** relative to prompts, popups and menus is
   unchanged.
3. Keys the widgets decline still reach the host, because a plugin's
   `defineMode` bindings and the dock's own gestures are not in `WidgetSpec`
   and no node can know them.

(2) is the constraint that makes this hard. Precedence in `fresh-ui` is
declaration order and nothing else — `topmost_modal_index`
(`focus/mod.rs:393`) scans `pending_layers` in reverse, and `LayerProps` has no
z (`desc.rs:676`). And the frame deliberately declares the two panel keyboards
**first**, under prompt, popups and menus (`frame.rs:526–544`), while
declaring the panel's *content* last so it paints on top (`frame.rs:710`).
The comment at `frame.rs:520–524` states the principle explicitly: paint order
and keyboard order "are independent by design".

So "put the interior inside `keys_layer`" would fix (1) and break (2): the
interior would paint under the menus. And "move the content layer to where the
keys layer is" would fix (1) and break paint order.

### 5.2 What the library must grow: a layer names its scope

```rust
// fresh_ui::LayerProps
/// The element whose focusables this layer confines traversal to.
/// `None` — the default, and what every layer did before this existed — is
/// the layer's own subtree.
///
/// Keyboard precedence and paint order are already independent here: a layer
/// declared early owns the keyboard under one declared late, and a layer
/// declared late paints over one declared early. A surface whose *content*
/// must paint late and whose *keyboard* must rank early therefore cannot say
/// what it means with one layer. Naming the scope separates the two facts
/// that were welded to one node.
///
/// Resolves the way `within` does: an element carrying the key, else nothing.
pub scope: Option<Key>,
```

with the two-line change in `focus/mod.rs`: `active_scope()` (407) returns the
named element's focus registration when the topmost keyboard-owning layer has
one, and `focus_scope()`'s `retain` (372) tests containment in that element
rather than in the layer.

`view/shell/panel.rs::keys_layer` then becomes:

```rust
layer()
    .modality(Modality::Focus)
    .pointer_mode(PointerMode::Ignore)
    .scope(interior_key(slot))          // the panel's described root
    .child(fallback_keys(slot))         // NOT autofocus, NOT the scope
```

and the sink stops being `autofocus()`ed and stops being the scope. The panel
body's root node carries `interior_key(slot)` and a `focusable().scope()`
registration.

**Why this shape rather than the alternatives.** (i) *Make the panel's content
layer `Modality::Focus`* — collapses the two orders the frame's own comment
says are independent, and would reorder paint for the floating panel and the
menus. (ii) *A node-level `Focusable::confine()` with no layer* — plausible,
but the confining surface also has to answer `Ui::focus_confined()` and
`Ui::keyboard_owned()` (`focus/mod.rs:658, 624`), which are defined over
layers; a node-level confiner would need a parallel definition of both, which
is two mechanisms for one question. (iii) *Keep the sink and teach traversal to
step out of it* — that is a special case in the library's core traversal for
one host's shape, and the library's whole line here is that confinement is
containment.

### 5.3 The fallback, and what happens to `PanelKey`

`UiFact::PanelKey` does not go away; it **shrinks from a claim to a decline.**
Today the sink stops every key (`panel.rs:227` returns a message and the
applier at `shell_host.rs:2461` runs the whole router). In the end state the
fallback node — an ancestor of the widgets, inside the named scope — runs on
the *bubble* leg after every widget has declined, and:

- **Declines** (returns `None`, no `e.stop()`) the keys the tree resolves:
  `Tab`, `Shift+Tab`/`BackTab`, and the directional keys when nothing inside
  claimed them. `propagate_key` returns `false`, `resolve_intent` maps them,
  and `default_for_intent` (753) moves focus. This is what makes Tab work.
- **Claims** (`e.stop()`, emits `PanelKey`) everything else. The applier runs
  `dispatch_floating_widget_key` (`app/input.rs:501`) exactly as it does today,
  and its `BlurUnconsumed` / `FallThrough` outcomes still hand the key back
  through `shell_interior_took_key` (`shell_host.rs:1405`).

**One ordering problem this exposes, and it needs a second library change.**
`router::widget_panel_key` checks `mode_has_binding` *before* it decides
anything (`input/router.rs:326, 355, 365`): a plugin's `defineMode` entry for
`Enter` or for a bare `/` must win over the focused widget. On the bubble leg
the widget has already claimed the key by the time the panel's fallback runs.
So the panel needs a handler that runs **before** the focused element:

```rust
// fresh_ui::Focusable
/// A key listener offered on the way *down*, root to target, before the
/// focused element sees it.
///
/// The pointer has had this since the hit path was written (`Phase::Capture`,
/// `hit.rs:507`); keys did not, and the asymmetry is not principled. A
/// surface-level policy — a plugin's mode binding, a dialog's own chord — has
/// to be able to pre-empt a focused control without swallowing every key the
/// control would have handled, and a bubble listener can only do the second.
pub fn on_key_capture(self, h: Handler<M>) -> Self
```

implemented by giving `propagate_key` (674) a forward leg before its reverse
one, `Phase::Capture`, same `Ctl` and same stop semantics. This is a small,
general library change — it makes the two channels symmetric — and it is the
piece that lets both the panel *and* the settings dialog stop swallowing.

### 5.4 The second ring and the per-kind handlers

`handle_widget_focus_advance` (1183) **is deleted**, and with it the box arena
it reads. The `WidgetAction::FocusAdvance` plugin command
(`fresh-core/src/api.rs`, `WidgetAction::FocusAdvance`) is frozen and must keep
working, so it re-implements as "ask the tree to move focus": the applier
raises a `UiFact` that calls `Ui::move_focus(FocusDir::Next/Prev)`. That is
one call, and it is the same ring the user's Tab uses — which is the whole
point.

`KeyFx::focus_advance` (`kinds/mod.rs:93`) — the kind's request that Enter
commit-and-advance — routes the same way.

The per-kind `on_key` handlers **stay**, and this is a deliberate limit on the
migration. The alternative is declaring each kind's keys as `Shortcut`s and
`Intent` actions on its node, which is the library-native form, but:

- The kinds' key vocabulary is string-named (`"Shift+Tab"`, `"PageUp"`) and
  reaches them from three callers: the TUI router, the plugin's
  `WidgetAction::Key` smart-key path (`widget_runtime.rs:1034`), and the web.
  A node-declared shortcut serves only the first.
- `KeyDisposition::PassAfter` (`kinds/mod.rs:155`) — "I updated myself but the
  key must still act on the surface beneath" — has no equivalent in
  `Flow::Stop`/continue. Adding one to the library for this is a bigger change
  than it earns.

So the end state is: **the tree decides *which element* has the key and
resolves the traversal intents; the kind decides what its own keys mean.** The
node's `on_key` is a thin adapter that raises `UiFact::WidgetKey { slot,
widget, key }` and the applier calls `behavior(...).on_key(...)`. That keeps
one implementation of "what Enter means on a Dropdown" across all three
frontends, which is the property that matters.

### 5.5 The settings dialog

Same disease, same cure, and it is *smaller* than the panel's because the
dialog has no plugin key policy to respect:

- `settings::keys` (299) becomes a **capture** handler for the dialog's own
  chords (Ctrl+S, Esc, and the category-panel keys `tree_keys` already
  claims) plus a **bubble** handler that claims what nothing inside answered.
  It stops being unconditional.
- The dialog's layer names the card body as its focus scope, so the body's
  widgets are reachable.
- `UiFact::WidgetFocus`'s applier grows a `Settings` / `SettingsEntry` arm
  (`shell_host.rs:1569` currently returns) writing `settings_state`'s own
  focus, so a focus the tree decided reaches the dialog's model.
- `toggle_focus`'s panel-at-a-time Tab (Categories → Settings → Footer) becomes
  control-at-a-time inside the body, which the plan already calls a taste call
  rather than a blocker and which the ring makes free.

---

## 6. Question 4 — what `fresh-ui` must grow

Five gaps. Two are the ones already named in the tree; three I found.

### 6.1 A windowed list whose uniform item height is *measured*

**The gap.** `List::row_rows` (`fresh-ui/src/widgets/list.rs:392`) takes a
`u16`, and `ScrollMode::Items { count, height }` (`desc.rs:326`) is uniform "because
that is what makes an index answerable without measuring". A card list's height
is uniform too — `WidgetSpec::List`'s doc says "All cards share a uniform height
(the tallest item's row count; shorter items pad)" — but the number is only
knowable by measuring every item at the current width. That is why
`view/shell/widgets.rs:1152` runs `render_collected` per card per frame.

**The API.**

```rust
/// How tall one row of a windowed list is.
pub enum RowHeight {
    /// The caller states it. The million-row case: no item is ever measured.
    Cells(u16),
    /// Uniform, but *measured*: every item is laid out once at the current
    /// width and the tallest sets the band. The window stays index-addressable
    /// — which is the invariant, not "never measure" — at the cost of O(count)
    /// measurement whenever the width or the item set changes.
    UniformMeasured,
}
impl<M> List<M> { pub fn row_height(self, h: RowHeight) -> Self }
```

**Why this shape.** Three were considered. *Per-item measured heights* breaks
index addressing and would need a prefix-sum index — a different widget, as
`row_rows`' own doc says. *A caller-supplied predictor* over the spec is a
second copy of the container layout rules, which is the duplication the whole
migration exists to remove. `UniformMeasured` keeps the invariant that matters
(an index answers without measuring *at scroll time*), moves the measurement
inside layout where the width is real, and caches it against
`(width, item identity)` so it is paid on resize and on data change rather than
per frame. The editor's current cost is the same O(count) work through a whole
second rendering engine, every frame; this is strictly cheaper.

The second blocker at that site — `mark_list_card_selected` rewriting `╭─│`
into `┏━┃` over rendered cells — is **not** a library gap. It is a `Draw::Border`
whose `BorderStyle` differs when selected, plus bold on the subtree. It is a
redesign of the marker rather than a translation, and the test that pins the
glyphs (`a_selected_card_is_marked_in_its_own_glyphs`) changes with it. Say so
in the commit rather than pretending it is a port.

### 6.2 Wrapped text whose window is the library's

**The gap.** `view/shell/widgets.rs:1809` asks the collector to reflow a whole
markdown document because "a row is not a function of a line": the document is
parsed, wrapped to the panel width, and a *shadow editor* is kept over the
reflowed text so the caret and selection address rendered lines.

`fresh-ui` already has the wrapping (`Wrap::Word`, `Wrap::Hanging`,
`desc.rs:213–250`) and already argues that only the thing that wraps can know
where it broke. What it does not have is the *window*: a viewport onto wrapped
text whose scroll unit is a rendered row.

**The API.** `viewport(text_runs(...).wrap(Wrap::Word))` with
`ScrollMode::Cells` almost works; what is missing is that the caret/selection
must be expressible in *source* coordinates and painted in *rendered* ones.
So:

```rust
/// Where the caret is, in bytes of the run's own logical string, rather than
/// in columns of a row the caller had to wrap itself.
///
/// `TextProps::cursor` is a column, which forces the caller to have wrapped
/// the text to know which row and column that is. Only the layout knows,
/// which is the same argument `Wrap::Hanging` already makes.
pub fn cursor_byte(self, byte: usize) -> Self
```

plus its inverse for the pointer, §6.3. With those two, the markdown arm is
`text_runs(parsed_runs).wrap(Word).cursor_byte(caret)` inside a viewport, and
the shadow editor becomes element state instead of a per-frame reflow.

This is the single largest library change on the list and it is honest to say
so: it puts a text model's coordinate mapping into the layout. The alternative
— factoring the reflow out of `render_markdown_text_area` the way
`text_area_geom` was factored out — leaves the mapping in the editor and keeps
two wrap engines. Among bundled plugins only `code-tour.ts` uses it, so the
work can be deferred without blocking anything else.

### 6.3 A pointer press on text reports a byte, not a column

**The gap, and it is a live bug.** `hit_node`
(`view/shell/widgets.rs:2833`) sends `at: Some(e.local.x)` — a **column**
within the piece's rect (`fresh-ui/src/hit.rs:525` sets `local` as
`pos - rect`). The applier computes
`clicked_byte = hit.byte_start.saturating_add(at)` (`shell_host.rs:1456`) —
adding a column to a **byte**. For a single-line field the hit spans the whole
row (`kinds/text.rs:1288–1296` sets `byte_start: 0`), so the caret byte comes
out as `column - valueInnerStart`, where `valueInnerStart` is in bytes
(`kinds/text.rs:1298`). Correct for ASCII labels and ASCII values; wrong for a
localized label or any non-ASCII value.

**Scope:** the plugin-panel slots only (`Dock`, `Floating`, `Pane`). The two
settings slots return before this line (`shell_host.rs:1463, 1469`) and take
the re-render path instead, which is why the bug has not been seen in the
dialog where most of the editor's own fields live.

The two settings click paths that "re-run the renderer just to resolve a caret
byte" are, on inspection, **the only correct implementation** — they map column
to byte through `grapheme_byte_at_visual_column` over the row they re-rendered
(`text_click.rs::value_byte_at`, 127). So the brief's framing of those two
sites as pure waste is half right: they are expensive *and* they are the ones
that work.

**The API.** Only the thing that laid the text out knows where each grapheme
landed — and once the field's head-truncation is `Elide::Head`
(`desc.rs:213`) rather than the formatter's own slicing, only the *library*
knows. Today the formatter truncates and the library merely places the result,
which is why the mapping can be reconstructed at all; the moment `Elide` or
`Wrap` does the cutting, it cannot.

```rust
/// The byte of the run's logical string under a press on a text node.
/// `None` for a press on a node that is not a text run.
///
/// The caller supplied the string; the library decided which of it is visible
/// and where each grapheme landed. Reporting a column asks the caller to redo
/// that decision, and there is no way to do it right without redoing the
/// layout — which is what the editor's two click paths do today, and what its
/// third does *wrongly* by treating the column as a byte.
pub text_byte: Option<usize>,   // on fresh_ui::Event
```

This deletes `text_click.rs`, both re-renders, and the units bug in one move,
and it is the reason to do it before anything else in this area.

### 6.4 A layer names its focus scope

§5.2. `LayerProps::scope: Option<Key>`.

### 6.5 A key capture phase

§5.3. `Focusable::on_key_capture`, `Phase::Capture` on the key leg of
`propagate_key`.

### 6.6 One that is *not* a gap, and should be recorded as such

The anchored popup's width (`view/shell/panel.rs::anchored_width`, 177) is
documented as needing "the interior stating its own natural width". That is not
a missing library feature: `Sizing::Auto` already means "whatever the content
needs, within the incoming constraint" (`desc.rs:153`). The loop is caused by
the adapter's own `node(spec, width, cx)` signature
(`view/shell/widgets.rs:325`) — it takes a width as a *number* because two
arms need one: `Divider` repeats a glyph `width` times (line 588) and the
runtime's row padding is width-relative.

The fix is a **description primitive, not a sizing rule**:

```rust
/// A rule: one glyph repeated across the node's own rect at paint time.
/// `Sizing::Flex(1)` then says "as wide as the row", and nothing has to know
/// the number before layout runs.
pub fn rule<M>(glyph: &str) -> Node<M>
```

— i.e. `Draw::Fill` with an optional glyph. With `rule()` and `Elide` handling
truncation, `node()`'s `width` parameter can go, `anchored_width` becomes
`Sizing::Auto`, and `Interior::avail_height` follows. Small, and it unblocks a
named residue.

---

## 7. Question 5 — hit-testing and the pointer

### 7.1 What replaces what

| today | end state |
|---|---|
| `hit_test_row_aware` on the described path (`click_handlers.rs:285` for pane panels; `probe_floating_widget` for the dock) | the node's own rectangle, via `hit_node`. Already true for every described surface. |
| `row_select_hit`'s nearest-row fallback (`registry.rs:720`) | `row_pieces`' trailing `Sizing::Flex(1)` hit node (`view/shell/widgets.rs:3086`), which already replaces it and whose comment says so |
| `HitArea.overlay` + the `on_overlay` surface parameter | layer stacking. A described pop-over is a `layer()`; the hit path already resolves top-down. |
| `probe_floating_widget`'s `hit_path` over `boxes` | the tree's hit path |
| the two click-path re-renders | `Event::text_byte`, §6.3 |
| `deliver_widget_hit_by_index` (the web's index) | keep the entry point (it is the frontend's wire contract) but resolve through `events()` §4.1; the index becomes a tiebreaker only, which `deliver_widget_hit_semantic` already treats it as |
| `hit_test_row_aware` for a **buffer-text** pane panel | **kept**, one caller (`click_handlers.rs:285`), over the text projection's own rows |

The last row is the one that makes this a design rather than a deletion list.
`click_handlers.rs`'s scan is not a duplicate layout: it is the buffer's own
`(row, byte)` coordinate space, the one `mouse_click` delivers to plugins, and
for a panel whose rows are buffer lines that is the right and only space.

### 7.2 The answer to the plan's open question

Plan §2.4 asks: "is the hit list a rendering output, or part of what the plugin
API owes a non-terminal frontend?" **Verified: neither, as stored.** What the
web receives (`view/scene.rs::WidgetHitView`, 1054–1062) is `index`,
`widget_key`, `widget_kind`, `event_type`, `payload` — **no geometry at all**.
The web lays the spec out itself and sends back an index plus the identity.
So the hit list crossing to the web is the *semantic* half only, and it is
already derivable. The obligation is real; the storage is not.

---

## 8. Question 6 — sequencing

Each step is independently shippable and leaves the tree working. Sizes are
"non-test lines touched", counted from the sites cited above; they are
estimates and are stated as ranges where I could not bound them by reading.

### S0 — `Event::text_byte` in `fresh-ui`, and the three consumers (small: ~150 lib + ~120 editor)

Add the field (§6.3); wire it in `hit.rs`'s dispatch for `Desc::TextRun`
targets; change `hit_node` to send it; change the three appliers
(`shell_host.rs:1456`, `settings_widget_hit`, `settings_entry_widget_hit`) to
read it. Delete `text_click.rs` and both `render_spec_no_autofocus` click-path
calls.

**Risk:** low, and it *fixes* a bug rather than risking one. The one hazard is
that `Elide`/`Wrap` must report the byte through the same path the paint took;
a run split across `text_runs` pieces measures as one logical string
(`desc.rs:258–262`), so the mapping is well-defined.
**Prerequisite for:** nothing. Do it first because it is the only step that
closes a correctness defect.
**Test:** press into a field whose label is `"名前: "` and assert the caret
lands on the clicked grapheme. No such test exists today.

### S1 — `LayerProps::scope` + `Focusable::on_key_capture` in `fresh-ui` (small: ~120 lib)

§5.2, §5.3. Both are additive; nothing in the library or the editor changes
behaviour until a caller opts in.

**Risk:** low in isolation. `focus_scope`'s `retain` is load-bearing for every
modal in the editor — the comment at `focus/mod.rs:354–374` records what broke
last time it was wrong — so the change must be "when the layer names a scope,
test containment in *that*; otherwise unchanged", with a test per branch.
**Prerequisite for:** S2, S3.

### S2 — the panel's keyboard becomes the widgets' (medium: ~250 editor)

`keys_layer` names the interior's scope and stops autofocusing a sink; the
panel body's root carries the scope key and a `focusable().scope()`; the
fallback declines Tab/Shift+Tab and the unclaimed directionals and claims the
rest; `router::widget_panel_key` loses the arms the tree now resolves (`Tab`,
`Shift+Tab`, and — behind the mode check, which moves to the capture handler —
the directionals when a widget wants them); `handle_widget_focus_advance` and
`focus_ring_scoped` are deleted; `WidgetAction::FocusAdvance` re-routes to
`Ui::move_focus`.

**Risk: the highest on this list.** It changes what every key over a focused
dock does, and the dock's key policy is the one deliberately plugin-shaped seam
in the router (`input/router.rs:218–225`). The specific hazards: (i) the mode-
binding precedence must move to the capture handler *in the same commit*, or a
plugin that binds `Enter` loses it to a focused button; (ii) `blur_floating_panel`
on `BlurUnconsumed` must still fire, which means the fallback claims Ctrl/Alt
chords rather than declining them; (iii) `apply_autofocus`
(`focus/mod.rs:502`) will now land focus on a *widget* when a panel opens,
firing a `focus` widget_event a plugin did not previously see on mount.
**Prerequisite:** S1. **Prerequisite for:** S3, S6.
**Exit:** Tab in a focused dock steps control to control; a key no widget and
no plugin mode binds still opens the command palette.

### S3 — the settings dialog's keyboard (small–medium: ~150 editor)

§5.5. Same two library pieces, no plugin policy, and its category tree already
interprets its own keys.

**Risk:** moderate and contained. The dialog's `keys` node is the fallback
holder of focus and dropping focus altogether makes `keyboard_owned` false and
leaks keys to the buffer (its own doc, `settings.rs:292–297`) — so the node must
stay reachable and must keep claiming whatever the body declines.
**Prerequisite:** S1 (and S2 in practice, to have one worked example).

### S4 — `rule()` retires `node()`'s width parameter (small–medium: ~200 lib + editor)

§6.6. Add `rule()`; rewrite the `Divider` arm; audit the remaining width uses in
`view/shell/widgets.rs` (`Spacer{flex}` and the `full_width` arms are already
`Flex`/`Elide` shaped); drop the parameter; make `anchored_width` `Auto` and
delete `Spot::Anchored::content_cols`.

**Risk:** low–moderate. The named risk is that some arm's use of `width` is
load-bearing in a way I did not read — I audited `Divider` and `Spacer` and
inferred the rest. **What would confirm it:** delete the parameter and see what
does not compile; the compiler is a complete oracle here.
**Prerequisite for:** nothing, but it is a prerequisite in spirit for calling
the description width-independent.

### S5 — `HitArea` splits; `hits` leaves the registry (medium: ~500 editor)

Introduce `WidgetEvent` (identity) and keep the geometry fields only on the
text projection's output. `WidgetImpl::events()` derives the identity half;
`hit_node`, the web projection and `deliver_widget_hit_semantic` all call it;
the three `synthesize_*_hit` functions become its implementation rather than a
fallback. `WidgetPanelState::hits` is deleted;
`hit_test_row_aware`/`row_select_hit`/`surface_hit` move onto the text
projection's output and keep their one caller.

**Risk:** moderate, and concentrated in the web frontend, where a wrong
`payload` is a silently wrong plugin event rather than a visible break. The
mitigation is that this step *strengthens* an existing invariant — the code
already prefers identity over index and already synthesizes — so the parity
test is "for every panel shape, `events()` equals the recorded `hits`' identity
half", asserted against the collector while it still exists.
**Prerequisite:** none. **Prerequisite for:** S7.

### S6 — delete the second and third focus rings and the box arena (medium: ~400 deleted)

After S2 and S3: `focus_ring_scoped`, `focus_ring`, `hit_path`,
`layout_box.rs`, `panel.boxes`, `prompt_toolbar_boxes`. Requires the **overlay
prompt toolbar to become described** first (§3.3) — its ring and hit test are
the last `layout_box` consumers besides the probe.

**Risk:** low once the prerequisites land; the failure mode is a compile error,
not a behaviour change. The toolbar's own migration is the moderate part
(~200 lines) and it has no plugin-visible contract beyond the toggle events.
**Prerequisite:** S2, S3, and the toolbar migration.

### S7 — the collector stops rendering described surfaces (medium: ~300 editor)

Delete `probe_floating_widget`, `handle_floating_widget_click`,
`panel.entries`, `panel.overlays`, and the `UiFact::DockPress` arm; split
`collect` into `resolve` (state) and `project_text` (rows) so mount/update runs
`resolve` for every panel and `project_text` only for a buffer-text pane panel;
`rerender_widget_panel` stops rendering for floating/dock/settings and becomes
"resolve, then request a frame".

**Risk:** moderate. This is where "two writers, one gate" finally closes, and
also where the `scroll_offset` split of §4.2 must land — `resolve` must not
touch the node projection's folds. The named hazard is the auto-sized list
(`spec_has_auto_sized_list`, `widget_runtime.rs:111` and the stale-height
re-render at `render.rs:1170`): the row budget currently comes from a rendered
height, and after this step it must come from the viewport. Land §4.5's
delivered `viewport` on `on_key`/`on_wheel` in the same commit.
**Prerequisite:** S5 (hits), S6 (boxes), and S2 (focus_key mirror).
**Exit:** `render_spec_with_options` has no caller for a dock, floating,
settings or scroll-owning pane panel.

### S8 — the two in-build `render_collected` calls (large, and the size is real)

The card list (§6.1) needs `RowHeight::UniformMeasured` in the library plus the
selection-marker redesign. The markdown document (§6.2) needs `cursor_byte` and
a wrapped viewport, plus giving the shadow editor an owner.

**Size:** the library changes are ~300 and ~400 lines; the editor side is ~150
and ~250; the marker redesign is small but changes a pinned test. Call it
**~1 100 lines across four commits**, and it is the only step on this list that
should not be attempted in one.
**Risk:** the markdown half is the highest-uncertainty item in this document —
it moves a text coordinate mapping into layout, and I have not read
`render_markdown_text_area` closely enough to bound it.
**Prerequisite:** none of the others, and nothing depends on it. It can be
deferred indefinitely at the cost of two per-frame renders on two surfaces.

### The order, and what may be dropped

| step | needs | needed by |
|---|---|---|
| **S0** text_byte | — | — (do first: it closes a defect) |
| **S1** library focus pieces | — | S2, S3 |
| **S2** panel keyboard | S1 | S6, S7 |
| **S3** settings keyboard | S1 (S2 as worked example) | S6 |
| **S4** `rule()` | — | — |
| **S5** hit split | — | S7 |
| **S6** delete rings + arena | S2, S3, toolbar migration | S7 |
| **S7** collector stops rendering | S2, S5, S6 | — |
| **S8** the two in-build renders | — | — |

If only three steps are ever done, they should be **S0, S1+S2, S7** — the bug,
the keyboard, and the authority. S8 is the one whose absence costs performance
rather than correctness, and it is the one to cut.

---

## 9. Defects found while reading

Each is the class the plan's §1.4 exists to close: a comment that argues for a
property the code lacks. None is fixed here — this document changes no code —
but every one is cheap, and four of them are residue from deleting
`covered()`.

1. **`view/shell/widgets.rs:252–259`** — the doc comment for the deleted
   `covered()` function was left in place and is now contiguous with
   `state_key`'s, so `state_key`'s rustdoc begins "Whether every node of this
   spec is a variant this module describes." It also names a test
   `every_variant_but_the_host_leaf_is_covered` that does not exist; the test is
   `every_variant_is_covered` (line 4223).
2. **`view/shell/widgets.rs:25 and 31`** — the module doc refers to
   ``[`covered`]`` (a broken intra-doc link) and instructs the reader how to
   read a function that is gone.
3. **`view/shell/widgets.rs:2307`** — "`covered` gates this" on the
   unreachable-variant arm. Nothing gates it; the arm is unreachable because
   `node_body` is total.
4. **`app/render.rs:4279–4281`** — "`panel_interior`'s `covered` gate is what
   makes that decision". `panel_interior` has no gate.
5. **`view/shell/frame.rs:621–623`** — "the panel's widgets stay the widget
   runtime's until `WidgetSpec` becomes a `Node`", on the dock. They are nodes;
   `dock_interior` is passed on the next line.
6. **`widgets/render.rs:10–21`** — "v1 dispatches on four kinds … Future kinds
   (`Toggle`, `Button`, `TextInput`, …) extend the dispatch". All nineteen
   exist. Same in **`widgets/mod.rs:9–12`**.
7. **`widgets/kinds/mod.rs:17–22`** — "The trait currently has a single entry
   point, [`WidgetImpl::collect`]". It has eleven members.
8. **`fresh-ui/src/desc.rs:544–547`** — `Modality::Focus`'s doc asserts that
   "A surface described all the way down has no keys left over to fall
   through". A described plugin panel does; see §1.6(5).
9. **The units bug**, §6.3. Not a comment defect but a code one, and its
   comment (`shell_host.rs:1444–1455`) argues confidently for the wrong
   arithmetic: "`at` is the column inside the hit's own piece and `byte_start`
   is where that piece begins … so their sum is the entry byte."

---

## 10. What I did not verify

- That `probe_floating_widget` is unreachable (§1.5). Strong circumstantial
  evidence; confirmable with a `debug_assert!`.
- That no live path writes `WidgetInstanceState::List::scroll_offset` for a
  described panel (§4.2). Confirmable by instrumenting `set_list_scroll` and
  `List::on_wheel`.
- The size of `render_markdown_text_area`'s reflow and how much of it would
  move into `fresh-ui` (S8). I read its call site, not its body.
- Whether the settings body's controls can reach a `focusable` scope without
  restructuring `settings.rs`'s card layout. I read the key handlers and the
  adapter call sites, not the full card build.
- Every claim about the web frontend beyond `view/scene.rs` and
  `webui/mod.rs:1076–1091`. I did not read the TypeScript side.
