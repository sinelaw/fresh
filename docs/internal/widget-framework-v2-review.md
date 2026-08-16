# Widget Framework: Critical Review and a Proposal for v2

Purpose: audit the widget framework in `fresh-core` / `crates/fresh-editor/src/widgets/`
and the UI chrome built around it; explain *structurally* why focus, event routing,
wheel targeting, and text input have been a recurring bug source rather than a run of
unrelated defects; compare the design against the state of the art (Dear ImGui, the
CSS/flexbox model, Tailwind, htmx, TanStack); and propose the shape of a v2 that makes
Fresh a genuinely good plugin-authoring target.

> Convention: this is an ANALYSIS + PLANNED doc. Claims about current behaviour are
> cited to source; where this doc and the source disagree, the source wins. Nothing
> here is implemented.

---

## 0. TL;DR

The widget framework's central design decision — **a widget tree renders to a flat
vector of styled text rows** (`TextPropertyEntry`) which the host writes as a virtual
buffer's content — is the root of the recurring bug classes. That representation has
no vertical axis, no z-order, no clipping, and no coordinate space. Every UI need that
requires one of those four has been met by punching a *side channel* out of the
renderer to the host (`overlays`, `dropdown_popup`, `embeds`, `scroll_regions`) or by
pushing the work back onto the plugin author (`visibleRows`, hand-counted chrome rows,
blank padding rows).

Concretely, in `crates/fresh-editor/src/widgets/render.rs`:

```
$ grep -c "panel_height\|avail_height\|available_rows" widgets/render.rs
0
```

The layout engine takes `panel_width` and threads it down
(`allocate_row_child_widths`). It never learns how tall it is. Every scrollable
widget therefore carries a plugin-supplied `visibleRows`, and every plugin that
hosts a list re-derives the editor's own layout arithmetic in TypeScript.

That single gap explains most of the reported symptoms:

| Reported symptom | Structural cause |
|---|---|
| Scroll goes to the wrong element | No scroll *containers* — only a hand-ordered ladder of surfaces in `mouse_input.rs` and per-widget `visibleRows` |
| Focus management fights us | Three unrelated focus models (widget `focus_key`, `FocusManager<T>`, per-component ad-hoc rings) + a 15-variant hardcoded layer enum |
| Mouse/keyboard propagation | No capture/bubble, no hit-test tree; 72 hand-written dispatch functions in one file and 29 hand-rolled `*Layout` hit caches |
| Text input misbehaves | Three text-editing engines, and typed characters take a *different, slower path* than selection keys |
| Reimplementing cursor ops | `TextEdit` is not reachable from a plugin; plugins mirror `{value, cursor}` and feed `cursorByte` back every frame |
| Dropdowns are painful | Popups are not a layer; `Dropdown` is special-cased as a singleton field on `RenderOutput` with a hardcoded 8-row window |

Event handling has a **second, orthogonal axis** (§2.6): even inside the widget
framework, per-kind behaviour is a match on `WidgetSpec` at 280+ sites across four
files, and the key path is not a dispatch table at all but a ladder of per-kind
short-circuits. Routing and encapsulation are separate problems; a hit-tree fixes the
first and nothing about the second.

The recommendation is **not** "rewrite the widget framework." It is: give the existing
declarative tree a real **box/constraint layout pass with a vertical axis**, a real
**layer + hit-test tree**, **one** focus/input path, and **one `WidgetImpl` trait** in
place of the per-kind matches — then delete the four side channels and the parallel
control library that exist only because those things are missing.

---

## 1. What ships today

### 1.1 The stack

| Layer | Where | Shape |
|---|---|---|
| Widget vocabulary | `fresh-core/src/api.rs` (`WidgetSpec`) | 17-variant tagged enum, `Serialize`/`TS`-derived |
| Reconciler / renderer | `fresh-editor/src/widgets/render.rs` (10,440 lines) | spec + prev instance state → rows, hits, focus |
| Registry | `fresh-editor/src/widgets/registry.rs` | `(plugin, panel_id)` → spec, hits, instance state |
| Host runtime | `fresh-editor/src/app/widget_runtime.rs` (4,220 lines) | key/mouse dispatch, floating & dock panels |
| Plugin front-end | `plugins/lib/widgets.ts` (1,255 lines) + generated `fresh.d.ts` | builder functions |
| Semantic projection | `fresh-editor/src/view/scene.rs` | `Scene` view structs shared with web/GUI frontends |

The good parts are genuinely good and should survive any v2:

- **Spec/instance separation.** The plugin re-emits its whole tree on every model
  change; keyed instance state (scroll, selection, expansion, caret) survives. This is
  the React/virtual-DOM insight applied correctly, and it is why plugin code reads as
  declarative rather than as a pile of imperative widget mutations.
- **Semantic-not-visual API.** `ButtonKind::{Normal, Primary, Danger}` rather than
  colours; `TreeNode` rather than glyph strings. Plugins describe intent; the host owns
  theme. This is the right boundary and is what makes theme switching free.
- **Host-owned hit-testing.** Plugins never see row/col. Correct, and a real security
  property in a sandboxed-plugin design.
- **One semantic model, multiple frontends.** `Scene` feeding the TUI, the wgpu/winit
  GUI (`fresh-gui` — which paints the same ratatui cell grid, so it rides the TUI
  renderer rather than being an independent one), and the web bridge, with a Rust
  parity test that fails on divergence, is stronger than what most terminal editors
  have. Any v2 must preserve it. Two caveats the rest of this doc has to respect:
  there are exactly **two** independent *widget* renderers — the row renderer
  (TUI+GUI) and `web-ui/js/65-widgets.js`, ~1,000 lines of hand-written JS that walks
  raw `WidgetSpec` with its own CSS flex layout and must be hand-mirrored for every
  vocabulary change — and the parity test (`tests/scene_parity.rs`) covers tabs,
  status bar, and menus but **zero widget-panel surfaces** today.

### 1.2 The paint model

`render_spec` walks the tree and returns `RenderOutput`:

```rust
pub struct RenderOutput {
    pub entries: Vec<TextPropertyEntry>,   // the rows
    pub hits: Vec<HitArea>,                // (row, byte_start..byte_end)
    pub instance_states: HashMap<String, WidgetInstanceState>,
    pub focus_key: String,
    pub tabbable: Vec<String>,
    pub focus_cursor: Option<FocusCursor>,
    pub embeds: Vec<EmbedRect>,            // side channel 1
    pub overlays: Vec<OverlayRow>,         // side channel 2
    pub scroll_regions: Vec<ScrollRegion>, // side channel 3
    pub dropdown_popup: Option<DropdownPopup>, // side channel 4
}
```

The first five fields are the model. The last four are each an admission that the
model cannot express something:

- `embeds` — a widget cannot contain a non-text surface, so `WindowEmbed` returns a
  rect for the host to paint into afterwards.
- `overlays` — a widget cannot be drawn *over* another, so `Overlay` children are
  extracted into a second list the host paints in a later pass. `HitArea` grew an
  `overlay: bool` flag and the registry grew a second hit-test entry point
  (`WidgetRegistry::overlay_hit_test`) to keep the two coordinate spaces apart.
- `scroll_regions` — a widget cannot clip its children, so an overflowing `List`
  exports its geometry for the host to paint and drag a scrollbar over.
- `dropdown_popup` — `Overlay` rows are clipped to the panel, so a dropdown near a
  modal's bottom edge would be cut off. Rather than fix clipping, the open dropdown is
  hoisted to a singleton `Option` on the output and drawn at screen coordinates. Its
  window is a constant: `DROPDOWN_VISIBLE_OPTIONS: usize = 8`. Only one dropdown can
  be open per panel — a limitation, though a milder one than an app-wide singleton.

That is four escape hatches in one struct, all tracing to the same missing feature:
**the renderer has no coordinate space, only a row list.**

---

## 2. Five places we fight the framework

These are ordered by how much pain per line of code they cause.

### 2.1 The layout engine has no vertical axis — so every plugin does layout by hand

`render_spec` threads `panel_width` down and nothing else. `Col` stacks children and
returns however many rows they happened to produce. There is no "you have 24 rows,
distribute them," no `flex-grow` on the vertical axis, no intrinsic-vs-available
height distinction.

The consequence is that **the row budget is a plugin responsibility**, and it shows:

```ts
// plugins/search_replace.ts
// Tree visible rows = panel viewport height minus the chrome
// (line 1 + options row + separator + footer = 4 rows)
const fixedRows = 5;
const visibleRows = Math.max(3, getViewportHeight() - fixedRows);
```

Note the comment says 4 and the constant is 5. That is exactly the failure mode of
hand-counted layout: the arithmetic and its rationale drift, silently, and the only
symptom is a row of content clipped or a dead row at the bottom.

The orchestrator goes further and reimplements `justify-content: space-between`:

```ts
// plugins/orchestrator.ts
// The host tree renders only its actual content rows (it does not pad itself out to
// `visibleRows`), so with few sessions the hints used to sit directly under the last
// card with dead space *below* them. Measure the rows the visible tree content
// occupies and fill the gap with blank, non-interactive rows...
const padRows = bottomRows > 0 ? Math.max(0, listRows - treeRows) : 0;
const bottomPad = padRows > 0 ? [raw(Array.from({length: padRows}, () => ({text: ""})))] : [];
```

A plugin is emitting **blank text rows to push a footer to the bottom of a panel.**
That is the terminal-UI equivalent of `<br>` spacers, and it is the framework's fault,
not the plugin's.

The settings port hit the same wall from the host side. Per
`settings-widget-unification-plan.md` §5.3.1, `list_of` sets the embedded `List`'s
`visible_rows = rows.len()` — i.e. "no viewport" — which pushed the `[+] Add new` row
off-screen on tall maps and hung two e2e tests. The root cause is recorded there as a
composite-visibility problem; it is more precisely *the absence of a height constraint
to propagate*.

**Assumption to reject:** "widgets are text, and text has no height, so the plugin
knows best." The plugin knows least — it cannot see the dock drag width, the terminal
resize, or the sibling panel. The host has all of it.

### 2.2 Wheel routing is a hand-ordered ladder, not a hit-test

`Editor::handle_vertical_scroll` (`app/mouse_input.rs`) is a single `else if`
chain over every scrollable surface in the application, in priority order, written by
hand:

```
SHIFT → horizontal
overlay prompt (live grep)      → preview pane or result list
bottom-anchored prompt dropdown
file browser
any popup
floating widget panel           (modal: consumes even on a miss)
dock
mounted split widget panel
else → wheel_surface_at(col,row) → pane / explorer / tab strip / nothing
```

Every branch carries an issue number in its comment (#2119, #2969) because every
branch was added after a bug where the wheel reached the wrong surface. That is the
signature of a missing abstraction: the ordering is *data* being expressed as
*control flow*, so it can only be extended by someone who holds the whole ladder in
their head.

What is missing is what every GUI toolkit has: **a scrollable-container hit test**.
"Find the topmost element under the pointer; walk up its ancestors; the first one with
overflow consumes the delta; if it is already at its bound, keep walking (that is
scroll chaining)." That is ~20 lines against a proper tree, and it makes the modal
"consume even on a miss" rule fall out of the layer's own opacity rather than being
special-cased.

The click path is the same disease at larger scale: `mouse_input.rs` is 4,713 lines and
72 functions — 16 of them `handle_click_*` alone (menu bar, file explorer, scrollbar,
horizontal scrollbar, status bar, search options, split separator, split controls, tab
bar, suggestions, prompt scrollbar, popup scrollbar, global popups, buffer popups,
context menus, …) — each hit-testing against its own cached rect struct. There are
**29 distinct `*Layout` structs** across `view/` and `app/` whose only job is "remember
where I painted things so I can hit-test them next frame."
`docs/internal/rendering-and-layout.md` §11 and
`input-keybindings-actions.md` §8 both list a unified `HitArea` + z-index as PLANNED.
It is the single highest-value unshipped item in the codebase.

### 2.3 Three text-editing engines, and typing takes a different path from selecting

There are three independent implementations of "a caret in a string":

1. `model` buffer + `Cursors` + `Event` — the real editor. Correct: graphemes,
   multi-cursor, undo as a transaction log, plugin hooks.
2. `primitives/text_edit.rs::TextEdit` (819 lines) — the widget framework's engine.
   Correct-ish: graphemes, word nav, selection. No undo.
3. `view/prompt.rs::Prompt` — the command palette / file finder /
   search input, with its own `input: String`, `cursor_pos: usize`,
   `selection_anchor: Option<usize>`, **and its own `undo_stack`/`redo_stack`**.

Three engines means three sets of grapheme bugs, three different behaviours for
Ctrl+Left at a word boundary, three answers to "does Ctrl+Z work here."

Worse, within the widget framework the *same field* is driven by two paths of
different latency. In `app/input.rs`:

```rust
ModeKeyDisposition::TextInput(ch) => {
    // ... Deliberately still an async plugin action: a mode's other bindings
    // (Space, Backspace, …) edit the same field through the same queue, and taking
    // a host-side shortcut here would let plain characters overtake them and
    // scramble the typed text.
    let action_name = match view.effective_mode.as_deref() {
        Some(mode) => format!("mode_text_input@{}:{}", mode, ch),
        None => format!("mode_text_input:{}", ch),
    };
    Some(self.handle_action(Action::PluginAction(action_name)))
}
ModeKeyDisposition::WidgetSelection(mv) => {
    // ... host-side, synchronous, straight into the focused TextEdit
    self.with_focused_text_editor(&panel_id, |e| e.move_word_left_selecting())
}
```

A typed `a` is **encoded into an action-name string**, dispatched as a
`PluginAction`, and marshalled across the QuickJS channel; the plugin's handler then
issues a `textInputChar` widget command back to the host, whose `TextEdit` applies the
edit (the plugin does not perform the edit itself, and on the fast path deliberately
does not re-emit its spec — see `search_replace.ts`'s `mode_text_input` handler). So
the round trip costs *latency and ordering coupling*, not a full spec rebuild.
Shift+Left, by contrast, is applied synchronously, host-side, to the same `TextEdit`
with no round trip at all. The comment is honest about the hazard the split creates
and chooses the queued path for every mutating key to keep ordering — which is the
right local call and the wrong global architecture.

The string encoding deserves its own note. `"mode_text_input@orchestrator:a"` is a
keystroke serialised into the `Action` enum's plugin-action namespace, then parsed back
out. The `@mode` qualifier exists only because the unqualified name was a global
handler name that multiple plugins fought over — the comment above it says so. This is
a character-level input event travelling through a *command* channel.

And because the host's `TextEdit` is not addressable from TypeScript, every plugin
form field carries a mirror:

```ts
// plugins/orchestrator.ts — 18 of these in one plugin
sshHost:     { value: string; cursor: number };
projectPath: { value: string; cursor: number };
filter:      { value: string; cursor: number };
```

with `cursorByte: openDialog.filter.cursor` fed back into the spec on every render.
To be precise about where the defect is: the host *does* enforce the seed-only rule at
runtime — for a keyed `Text` widget with instance state, the renderer takes the value
and caret from the retained `TextEdit` and ignores the spec's `value`/`cursor_byte`
(see the keyed-Text arm in `widgets/render.rs`). The re-fed `cursorByte` is therefore
dead weight, not a live race. The defect is that nothing in the *type* says so:
`cursorByte`/`value` look like live inputs in `widgets.ts`, so every plugin dutifully
maintains a mirror the host will never read after frame one. The fix is expressing the
rule in the API (`initialValue`/`initialCursor`), not adding enforcement that already
exists.

### 2.4 Focus is three systems stitched together, and popups aren't a layer

Three focus models coexist:

- `RenderOutput::{focus_key, tabbable}` — the widget framework's, a `String` key plus a
  declaration-ordered tabbable list recomputed on every render.
- `view/ui/focus.rs::FocusManager<T>` — a generic `Vec<T> + current: usize` ring, used
  by Settings.
- Ad-hoc rings inside individual components.

`Prompt` demonstrates the collision. It is a hand-rolled composite widget (text field +
suggestion list + title + footer) that had a real `WidgetSpec` bolted onto it:

```rust
// view/prompt.rs
pub toolbar_widget: Option<fresh_core::api::WidgetSpec>,
/// Overlay focus ring position: `None` = the query input is focused ...
/// `Some(key)` = that toolbar control is focused ... Tab/Shift+Tab cycle
/// input → toggles → input.
pub toolbar_focus: Option<String>,
```

Two focus rings in one component, manually interleaved by Tab handling, because the
prompt *hosts* widgets but is not *made of* them.

Above all of this sits `app/overlay.rs`, introduced (correctly) to stop the keyboard
and mouse precedence ladders from drifting apart. But look at what it is: a **15-variant
enum naming every specific UI surface in the application** —

```rust
pub(crate) enum LayerKind {
    EventDebug, Settings, KeybindingEditor, CalibrationWizard, WorkspaceTrust,
    Menu, Prompt, Popup, NewTabMenu, TabContextMenu, FileExplorerContextMenu,
    CloseSplitMenu, FloatingModal, Dock, Editor,
}
```

— rebuilt from scratch every time it is queried, by reading fifteen different `Option`
fields off `Editor`. Four of those variants (`NewTabMenu`, `TabContextMenu`,
`FileExplorerContextMenu`, `CloseSplitMenu`) are *the same widget*: an anchored list of
labels with a highlighted row. They are four separate structs in
`app/types/context_menu.rs` with four separate key handlers, in a codebase that already
has `PanelPlacement::Anchored` floating widget panels.

This unified the *precedence* while leaving the *composition* unsolved. Layers do not
nest, are not data, and cannot be created by a plugin. Adding any new modal surface
means: a new field on `Editor`, a new enum variant, a new arm in `overlay_layers()`,
and a new arm in each of the four dispatchers that match on kind.

### 2.5 Settings runs both frameworks at once, and the old one still owns input

This is the "where we perhaps *shouldn't* be using widgets" case — or more precisely,
where a half-migration is worse than either end state.

Per `settings-widget-unification-plan.md`, the render leg is done: every Settings
control paints through `widgets::render_spec`, and `view/controls/*/render.rs` is
deleted. But `view/controls/` still holds **6,203 lines**, including **seven
`input.rs` modules** — the per-control key/mouse state machines. And the bridge between
the two is `view/settings/widget_map.rs` (1,303 lines), which each frame projects the
full live control state — edit buffers, carets, selections, dropdown-open flags,
focused-row hints — into a fresh `WidgetSpec`. Its own header says it plainly:

> Editing still runs through the settings input path — this migrates the *view*.

So a settings text field today is: `TextInputState` (domain model) → embedded
`TextEdit` (engine) → `widget_map` projection → `WidgetSpec::Text` → `render_spec` →
rows, with input going `settings/input.rs` → `TextInputState`, entirely bypassing the
widget runtime's own `handle_widget_key`. Two models, one render path, one input path
that doesn't match the render path.

The plan document is candid about the cost: the first swap regressed 25 e2e tests and
introduced a hang, fixed by making the projection *more* faithful — i.e. by making the
mirror more complete rather than removing the mirror. And Settings still runs a third
scrolling model on top: `view/ui/scroll_panel.rs`'s `ScrollablePanel` (self-described
as "inspired by Flutter Sliver, WPF ScrollViewer, Qt QAbstractScrollArea"), used by
nothing else.

There is a real question underneath this, and it is worth asking rather than assuming:
**should Settings be a widget panel at all?** A modal form with a categories sidebar,
search, per-field reset/inherit affordances, and a nested entry-editor stack is not
obviously the same thing as a plugin's side panel. The honest answer is that it *should*
be — but only once the framework has vertical layout, real scroll containers, and a
layer stack. Migrating the render leg onto a framework that lacks all three is what
produced the 1,303-line projection layer. **The migration was sequenced before its
prerequisites.**

---

### 2.6 The second axis: routing is only half the problem

The five items above are all about **routing** — which surface receives an event.
There is an orthogonal axis: **encapsulation** — where the behaviour for a given
element type lives. A hit-tree with capture/bubble fixes the first and does nothing
for the second, and it is worth being explicit that these are separate problems.

Today the widget framework is a closed `enum WidgetSpec` plus a match on that enum at
every site that needs per-kind behaviour:

| Site | `WidgetSpec::` match sites |
|---|---|
| `widgets/render.rs` | 142 |
| `app/widget_runtime.rs` | 75 |
| `widgets/actions.rs` | 33 |
| `view/settings/widget_map.rs` | 32 |

Plus `WidgetInstanceState` (a parallel enum in `registry.rs`), the `widgets.ts`
builder, and the generated `fresh.d.ts`. **Adding one widget kind touches seven
files.**

The render side is the healthy case: `render_collected` is essentially one dispatch
match calling per-kind `collect_*` helpers, so the behaviour is at least *located*.
The input side is the unhealthy case. `handle_widget_key` is not a dispatch table at
all — it is a ladder of short-circuits, each keyed on the focused widget's kind, each
placed *ahead* of the general path:

```rust
// completion-popup short-circuit — intercepts Tab/Up/Down/Enter/Esc
let completions_open = matches!(key, "Tab"|"Up"|"Down"|"Enter"|"Escape")
    && self.focused_text_completions_open(panel_key);
if completions_open { /* ... */ }

// "Dropdown-popup short-circuit: ... Mirrors the completions short-circuit above."
let dropdown_open = matches!(key, "Up"|"Down"|"Enter"|"Space"|"Escape")
    && self.focused_dropdown_open(panel_key);
if dropdown_open { /* ... */ }
```

That is structurally the same defect as `handle_vertical_scroll` (§2.2), reproduced
*inside* the widget framework: a priority ordering expressed as control flow, extended
by prepending a branch. The comment on the second block — "mirrors the short-circuit
above" — is the tell.

The remaining kind checks in `widget_runtime.rs` are worse than either: clustering
their line numbers shows roughly **40 separate one-or-two-line `matches!(spec,
WidgetSpec::X {..})` probes** scattered through the key-handling code, rather than one
dispatch point. Behaviour for a kind is neither centralised nor encapsulated — it is
smeared.

**Why both short-circuits exist at all:** the completion popup and the dropdown option
list are not child boxes. They are side channels (§1.2), so they are not *deeper in
the tree* than the field they belong to, so ordinary innermost-first dispatch cannot
give them the key. The short-circuits are hand-simulated depth. Fix the tree (§4.5)
and route by depth (§4.2), and both blocks delete themselves — not because they were
refactored, but because the condition they encode becomes structurally true.

That still leaves the vtable question, which §4.2 does *not* answer: after bubbling
picks the right box, the box has to *do* something, and today "doing something" is a
match arm in a central function rather than code the widget owns. See §4.3.

---

## 3. Where the design sits against the state of the art

### 3.1 Dear ImGui — what it actually got right

The usual framing ("immediate vs retained") is the least interesting axis, and Fresh
already sits in the more defensible spot: `WidgetSpec` is retained-declarative with
keyed instance state, which is strictly better than ImGui's ID-stack for a system where
the UI author lives across an async IPC boundary. Rebuilding the tree per frame in
TypeScript and reconciling host-side is right, and should not change.

The things ImGui gets right that Fresh does not:

- **A layout cursor with a real content region.** ImGui always knows the available
  rect. `GetContentRegionAvail()` is the primitive Fresh lacks entirely; every
  `visibleRows` in a plugin is a manual reimplementation of it.
- **A window/child-window abstraction that *is* the clip and scroll unit.**
  `BeginChild` gives clipping, scrolling, and a scrollbar in one call. Fresh's
  equivalent is `List.visible_rows` + the `ScrollRegion` side channel + host paint —
  three pieces the plugin author must assemble.
- **Explicit input capture flags.** `io.WantCaptureMouse` / `WantCaptureKeyboard` is a
  single boolean answering "did the UI eat this event." Fresh answers it with
  `overlay_layers()` plus four dispatchers matching on `LayerKind`.

What Fresh should *not* copy: ImGui's ID stack (positional identity is fragile —
Fresh's explicit `key` is better), and its lack of accessibility/semantic model
(Fresh's `Scene` projection is a genuine advantage over ImGui and over every other TUI
toolkit).

### 3.2 CSS layout — the missing model, and the parts to steal

The browser box model is the thing Fresh is missing, minus the parts nobody wants:

- **Two-phase layout: intrinsic size, then available size.** Children report what they
  want; the parent distributes what exists. Fresh does phase one only, on one axis.
- **Flexbox on both axes.** `flex-grow` is exactly what `spacer(flex: true)` is for
  horizontally — and there is no vertical equivalent, which is why the orchestrator
  pads with blank rows. `justify-content: space-between` on a `Col` deletes that hack.
- **`overflow: scroll` as a *container property*, not a widget property.** This is the
  single change that fixes wheel routing: any box can be a scroll container, wheel
  events walk the ancestor chain, scroll chaining is the natural default. It also
  deletes `visibleRows` from the plugin API — the container's height is whatever the
  layout gave it.
- **Stacking contexts / z-index.** Popups, dropdowns, tooltips, and context menus stop
  being four special cases and become "a box positioned relative to an anchor in a
  higher stacking context."

What to steal explicitly: the box model, flex on both axes, `overflow`, stacking
contexts, and `position: absolute`-with-anchor. What to leave: the cascade, inheritance,
selectors, and anything resembling a stylesheet language.

**Tailwind** is worth naming for one narrow reason, and it is not utility classes. Its
real contribution is the **constrained design-token scale**: you cannot pick an
arbitrary spacing value, so a large codebase stays visually coherent without a design
review. Fresh already has the mechanism (theme keys, `ButtonKind` intents) but has not
extended it to *spacing and density*. A `Density::{Compact, Comfortable}` token plus a
small fixed spacing scale on `Row`/`Col` would let the dock, settings, and every plugin
agree on padding without any of them hardcoding `spacer(1)` — which they all currently
do. `plugins.md` §7.3 already lists "full role-based theming (only button intent
ships)" as PLANNED; this is the same item, extended to layout.

### 3.3 htmx — the one idea worth taking, and the one to reject

htmx's insight is that **the server owns state and the client sends semantic
intents**, avoiding a duplicated client-side model. Fresh's widget framework is already
htmx-shaped and *more* correct than htmx here: the plugin sends a spec, the host owns
interaction state, events come back semantic (`activate`/`change`/`select`) with no
coordinates. That is the right architecture and the review should say so clearly.

The idea worth taking: htmx's **swap granularity** (`hx-swap`, out-of-band swaps). Fresh
has `WidgetMutation` fast paths, but the *default* path is "re-emit the whole tree." For
the orchestrator dock — thousands of lines building a full spec on every keystroke,
crossing the QuickJS boundary — a targeted "replace the subtree under key `X`" swap
would cut both latency and the surface area for reconciliation bugs.

The idea to reject: htmx's string-templated, stringly-typed contract. Fresh's
`ts-rs`-generated `fresh.d.ts` is a strictly better boundary and must be kept. (And
`"mode_text_input@mode:char"` is exactly the stringly-typed failure htmx invites — it
should be a typed hook argument, not a synthesised action name.)

### 3.4 TanStack — headless-first, and what it implies for `List`/`Tree`/`Table`

TanStack's discipline is **headless logic**: Table, Virtual, and Query ship state
machines with zero rendering, so the same sorting/paging/virtualisation logic serves
any renderer. Fresh has the raw material — `WidgetInstanceState` already holds
`scroll_offset`, `selected_index`, `item_height`, `expanded_keys` — but it is welded
into a 10,440-line `render.rs` rather than being an addressable, testable core.

Two concrete implications:

- **TanStack Virtual is the model for `List`/`Tree`.** Its virtualiser takes
  `count`, `getScrollElement`, `estimateSize` and returns the visible window. Fresh's
  `List` takes `visibleRows` from the *plugin*, which is the same computation performed
  in the wrong process. Inverting this (host computes the window from the container
  height it just assigned, plugin supplies a row-count and a row-builder) removes the
  entire `visibleRows` family of bugs — including the settings `list_of` hang.
- **TanStack Query is the model for the plugin data path.** Plugin panels today are
  push-only: the plugin computes a full spec and pushes it. There is no notion of
  "this subtree depends on data that is loading / stale / errored," so every plugin
  hand-rolls its own `inFlight` / `lastError` fields (the orchestrator has both). A
  `WidgetSpec::Async { key, placeholder }` that the host renders as a skeleton until
  the plugin resolves it would delete a lot of duplicated plugin state and make
  loading states consistent across the app.

### 3.5 Honest scorecard

| Dimension | Fresh today | State of the art | Verdict |
|---|---|---|---|
| Declarative + keyed reconciliation | ✅ | React/TanStack | **Ahead of most TUI toolkits** |
| Semantic (non-visual) plugin API | ✅ | — | **Ahead of ImGui, Ratatui, Bubble Tea** |
| Multi-renderer semantic projection (`Scene`) | ✅ + parity test | — | **Genuinely rare; keep** |
| Sandboxed plugin UI with host-owned hit-test | ✅ | — | **Strong security posture** |
| Layout: horizontal | partial (flex spacer, `width_pct`) | flexbox | Behind |
| Layout: vertical | ❌ none | flexbox | **Critical gap** |
| Scroll containers / clipping | ❌ per-widget `visibleRows` | `overflow`, TanStack Virtual | **Critical gap** |
| Z-order / stacking | ❌ 4 side channels, 1 dropdown max | stacking contexts | **Critical gap** |
| Focus model | 3 competing | 1 tree-derived ring | Behind |
| Event propagation | hand-ordered ladders | capture/bubble on a hit tree | Behind |
| Text input | 3 engines, 2 latencies | 1 engine | Behind |
| Design tokens | theme keys only | Tailwind scale | Minor gap |

---

## 4. What v2 should be

Five changes. They are ordered so that each is independently shippable and each makes
the next one smaller. Nothing here requires abandoning `WidgetSpec`, the TS builders, or
the reconciler — the vocabulary is fine; the *engine underneath it* is what changes.

### 4.1 A real layout pass: constraints in, boxes out

Split the current single-pass `render_spec` into:

```
spec ──measure(available: Constraints) ──► intrinsic sizes
     ──arrange(final: Rect) ────────────► LayoutBox tree (rects, clip, z, scroll)
     ──paint(LayoutBox) ────────────────► cells / TextPropertyEntry / DOM
```

`Constraints { min_w, max_w, min_h, max_h }` — the Flutter/WPF two-phase model, which is
the right one for a system that must also drive a GPU window and a browser.

Immediate consequences:

- `Col` gets `flex`, `justify`, and `align`. The orchestrator's blank-row padding and
  `search_replace`'s `getViewportHeight() - fixedRows` both delete.
- `visibleRows` disappears from the public API. A `List` inside a box of height 12 shows
  12 rows because that is what `arrange` gave it. The settings `list_of` hang cannot be
  expressed.
- The `LayoutBox` tree *is* the hit-test tree (§4.2). No more `*Layout` cache structs.
- Paint becomes a separate, swappable back end — which is what finally lets the TUI, the
  wgpu GUI, and the web bridge share layout instead of sharing only semantics.

Keep the row-based paint back end. Widgets should still render as styled text rows in
the terminal; the change is that *layout no longer happens in the row domain*.

### 4.2 One hit-test tree with capture/bubble, replacing every ladder

With a `LayoutBox` tree carrying `rect`, `clip`, `z`, and `scroll`, mouse dispatch
becomes:

1. `hit(point) -> Vec<&LayoutBox>` — deepest-first path from root, respecting `clip`
   and `z`.
2. Capture phase root→leaf (lets a modal claim without knowing what is beneath it).
3. Target.
4. Bubble leaf→root; the first ancestor that handles it consumes it. For a wheel event,
   "handles it" means "is a scroll container not already at its bound" — scroll chaining
   for free.

This replaces the `handle_vertical_scroll` ladder, the 16 `handle_click_*` arms,
and the 29 `*Layout` caches. It also makes `LayerKind` mostly
unnecessary: a modal is a box with `z` above its siblings and `pointer_opaque: true`,
not an enum variant plus four dispatcher arms.

Retain `overlay_layers()` for the handful of genuinely application-global precedence
rules (event-debug intercepts everything; PTY routing gates) — but shrink it from
fifteen surface-specific variants to two or three *semantic* ones.

### 4.3 Per-kind behaviour behind a trait, so components are self-contained

§4.2 answers *which box* gets an event. It does not answer *what the box does with
it* — that is still a match arm in a central function. Fix it by separating the **wire
type** from the **behaviour**:

- `WidgetSpec` stays a closed, `Serialize`/`Deserialize`/`TS`-derived tagged enum. It
  crosses a sandbox boundary and feeds three renderers plus the `Scene` parity test;
  it must remain plain data.
- Behaviour moves behind one trait, looked up by the enum tag:

```rust
trait WidgetImpl {
    fn measure(&self, spec: &WidgetSpec, st: &InstanceState, c: Constraints) -> Size;
    fn arrange(&self, spec: &WidgetSpec, st: &mut InstanceState, r: Rect) -> Vec<LayoutBox>;
    fn paint(&self, spec: &WidgetSpec, st: &InstanceState, b: &LayoutBox, s: &mut dyn Surface);
    fn on_event(&self, spec: &WidgetSpec, st: &mut InstanceState,
                ev: &Event, cx: &mut EventCtx) -> Handled;
}

fn behavior(spec: &WidgetSpec) -> &'static dyn WidgetImpl { /* the ONE match */ }
```

Exactly one match on `WidgetSpec` survives, in one file. The 142 + 75 + 33 match sites
of §2.6 become impl bodies that only ever see their own variant. Adding a widget kind
becomes: one enum variant, one `impl WidgetImpl`, one TS builder — instead of seven
files.

`render_collected`'s per-kind `collect_*` helpers are already ~80% of `measure` +
`arrange` + `paint`; the extraction is mechanical. The genuinely new work is
`on_event`, which is what dissolves `handle_widget_key`'s short-circuit ladder:
`Dropdown::on_event` claims Up/Down *when its own `open` state is set* and returns
`Handled::No` otherwise, letting the key bubble. No central function needs to know
that dropdowns exist.

**`EventCtx` is the second half of self-containment.** A widget must be able to act
without reaching into `Editor` — the current handlers cannot, which is why they live
on `Editor` in the first place. `EventCtx` gives them a scoped capability set:
request focus, request a repaint, start/stop a pointer grab, emit a semantic event to
the owning plugin, push a child popup. This is the same shape as
`DeferredAction`/`InputContext` in `input-keybindings-actions.md` §3.1, which already
solved this problem for modal components — generalise that, don't invent a second one.

**The extensibility fork, stated explicitly.** "Self-contained component" can mean two
different things and they have very different costs:

| | Closed kind set, host-side `WidgetImpl` | Open kind set, plugin-defined behaviour |
|---|---|---|
| Who adds a kind | host PR, one impl | any plugin |
| Event cost | synchronous, in-process | one IPC round-trip **per event** |
| Text input | fixed by §4.5 | reintroduces the `mode_text_input` latency for every component |
| `Scene` parity / web + GUI renderers | intact | broken — they cannot paint an unknown kind |
| Sandbox | intact | plugin code runs inside layout |

**Recommendation: the closed set.** Open kinds re-create the exact defect §4.5 exists
to remove, and would silently break the multi-frontend parity that is one of Fresh's
real advantages (the web widget renderer cannot paint a kind it doesn't know). Plugins get composition instead, which is 90% of what they actually
want: a `WidgetSpec::Component { key, child }` node that is a **focus scope and an
event scope** — Tab cycles within it, events bubble to its boundary and can be
stopped there. Plugins already build subtrees with TypeScript functions; this makes
such a subtree behave as a unit rather than as inlined nodes, and gives it a stable
identity for keyed reconciliation and targeted swaps (§4.7 phase 7).

### 4.4 One focus ring, derived from the layout tree

Delete `FocusManager<T>` and every component-local ring. Focus becomes:

- `FocusId` = the `LayoutBox`'s stable key path.
- Tab order = document order of focusable boxes in the layout tree, honouring a
  `focus_trap: bool` on any box (modals set it; the prompt's toolbar problem in §2.4
  becomes "the prompt is a focus trap containing an input and a toolbar" and the
  interleaving code disappears).
- One `focused: Option<FocusId>` on the editor. `Editor.focused_widget`,
  `Prompt.toolbar_focus`, `SettingsState`'s ring, and dock focus all collapse into it.

Keyboard dispatch then mirrors mouse: capture from root to the focused box, bubble back.
The `is_terminal_ui_action` / `allows_normal_fallthrough` whitelists in
`input/keybindings.rs` §5.2 become "the event bubbled past every UI box without being
consumed, so the editor gets it" — which is what those whitelists are approximating by
hand.

### 4.5 One text-editing engine, addressable from plugins

- Promote `TextEdit` to `fresh-core` as *the* single-field editing engine, with undo.
- Delete `Prompt`'s inline engine; make the prompt's query field a `Text` box.
- Delete the seven `view/controls/*/input.rs` state machines; route Settings through
  `handle_widget_key`.
- **Replace `mode_text_input@<mode>:<char>` with a typed key fast lane through the
  same ordered queue.** An earlier draft proposed making printables synchronous
  host-side; adversarial review against the source showed that would *create* the
  interleaving hazard, not remove it — plugins bind Backspace/Space/Enter in their
  `defineMode` keymaps as async plugin actions (`search_replace.ts` binds
  `["Backspace", "search_replace_backspace"]`), and the mode keymap resolves *before*
  the printable fallthrough, so "x, Backspace, a" would apply as `a`-then-Backspace.
  Making *all* editing keys synchronous requires pre-empting the mode keymap — an
  audited, plugin-visible precedence break that also has to carve out `getNextKey`
  capture (flash, vi-mode) and keymap-level interception (search history's Up/Down
  proxy). So the sequenced fix is: **step 1**, keep one ordered queue but make the
  keystroke a typed event (`WidgetKeyInput { mode, key }` or a `WidgetAction` fast
  lane) instead of a synthesised `PluginAction` string — this fixes the stringly
  encoding and the mode-collision namespace now, with zero plugin breakage;
  **step 2**, only after the widget runtime owns all editing keys for focused text
  boxes (post-§4.3 `on_event`), move the *whole* editing-key family host-side in one
  audited change, never printables alone.
- Drop `cursorByte`/`value` from the plugin-facing spec as apparent live inputs. The
  host already enforces seed-only at runtime (§2.3); rename to
  `initialValue`/`initialCursor` so the type says what the runtime does, and delete
  the 18 `{value, cursor}` mirrors in `orchestrator.ts`.

This is the change that most directly answers "text input not working correctly" and
"having to reimplement cursor operations."

### 4.6 Popups as boxes in a stacking context, not as side channels

Add `WidgetSpec::Popup { anchor: Key, placement, child }`. It participates in layout
like anything else, in a higher stacking context, clipped to the *screen* rather than
to its panel.

Then delete: `RenderOutput::dropdown_popup`, `RenderOutput::overlays`,
`HitArea::overlay`, `WidgetRegistry::overlay_hit_test`, `DROPDOWN_VISIBLE_OPTIONS`, and
the one-dropdown-at-a-time restriction. `ScrollRegion` and `EmbedRect` also fold in:
a scroll container's scrollbar is painted from its own `LayoutBox`, and `WindowEmbed`
becomes a box with a foreign paint back end.

Fold the four context-menu structs in `app/types/context_menu.rs` into a single
anchored-popup widget, and let plugins mount context menus — which they currently
cannot.

### 4.7 Sequencing

Re-ordered after adversarial review against the source (which found that the original
"layout first" phase 1 was not a true increment — it changed the plugin API, the web
renderer, and e2e visual assertions all at once). Each phase ships independently and
pays for itself:

| Phase | Work | Unblocks |
|---|---|---|
| 0 | **Test net**: widget surfaces in `scene_parity.rs`; a headless `render_spec` snapshot harness; first unit tests around `handle_widget_key` (today: zero) | Every later phase gets a tripwire |
| 1 | `WidgetImpl` trait + `EventCtx`; extract the per-kind matches into impls (behaviour-preserving; covered by the existing ~150 render.rs unit tests) | Collapses 280+ match sites to one; adding a kind touches 1 file, not 7; shrinks every later phase |
| 2 | **Height, minimally**: `avail_height` on `RenderContext`, populated at the three render call sites from floating/dock/split geometry; `List`/`Tree` *default* their window from it when the spec omits `visibleRows` (explicit values keep winning → zero plugins break); extend the resize-rerender loop to split-mounted panels; migrate one plugin (`search_replace.ts`) as proof | Kills the `visibleRows` bug class without new abstractions |
| 3 | Vertical flex/justify as a render-internal two-pass over collected row-counts, still emitting rows; then the real `Constraints`/`LayoutBox` tree, kept **internal** to the renderer (still flattening to rows + `HitArea`s, because rows/bytes are the wire format for click delivery, the web bridge's hit-index protocol, and the focus cursor) | Deletes the padding hacks; geometry substrate for 4 |
| 4 | Hit tree with capture/bubble, **panel-local first**: replace the two `handle_widget_key` short-circuits and `overlay_hit_test`; only then grow outward to the app-level wheel/click ladders as non-widget chrome gains boxes | Deletes the short-circuits, then the ladder and `*Layout` caches |
| 5 | Unified focus ring + focus traps | Deletes `FocusManager`, `toolbar_focus`, dock-focus special cases |
| 6 | Single `TextEdit`; typed key fast lane (§4.5 step 1); host-side editing keys only as the later audited step 2 | Fixes the stringly encoding now, the latency later |
| 7 | `Popup` in a stacking context; `Component` focus/event scope | Deletes 4 side channels + 4 context-menu structs |
| 8 | Finish Settings onto the framework; delete `view/controls/` and `widget_map.rs` | ~7,500 lines deleted |
| 9 | Design tokens (density/spacing scale), `Async` subtree, targeted swaps | Consistency + latency |

Phases 1 and 4 are the two halves of the event problem and are independent of each
other: 4 fixes *routing* (which box), 1 fixes *encapsulation* (what the box does).
Phase 1 goes first because it is a mechanical extraction with no behaviour change and
removes the "touch seven files" tax from everything after it.

Phase 8 is deliberately last. Per §2.5, attempting it before the phases above is what
produced the projection layer; the plan document's own §5.3.1 is a record of paying
that cost.

**Implementation status.** Shipped on this plan's first PR: phase 1 in
full (17 kinds behind `WidgetImpl`, one dispatch); phase 2 in full
(`avail_height`, auto-sized `List`/`Tree`, `search_replace.ts`
migrated); phase 3's substrate — the internal `LayoutBox` tree
(`widgets/layout_box.rs`: rects, z, clip-order, parent links, built by
collection and shifted through every container path alongside the
embed/scroll-region channels) — with vertical flex/justify still open;
phase 4 panel-local — wheel dispatch bubbles the hit path with scroll
chaining, and both `handle_widget_key` short-circuits are deleted in
favour of kind-owned `on_key` (`KeyDisposition::PassAfter` carrying the
dismiss-then-act contract); phase 5 panel-local — the published Tab
ring derives from the box tree (spec-walk ring retained pre-collection
only, pinned equal by debug assert) and `focus_trap` scoping is live;
phase 6 step 1 (typed key fast lane); and phase 7's tree half — the
completion popup and dropdown pop-over are boxes (z=1 opaque /
screen-space z=2) parented to their owning field, which is the
structural fact the deleted short-circuits used to hand-simulate.
The outward growth has since begun: `WidgetRegistry::overlay_hit_test`
is deleted — which surface the pointer is on (base rows vs a covering
popup) is the box tree's call, passed as a parameter to the one
byte-ranged resolver, and the right-click path now shares the same
probe as left-click/hover (fixing its overlay blind spot). The two
big precedence ladders are data: `handle_vertical_scroll`'s else-if
chain is `WHEEL_SURFACES` and `handle_mouse_click`'s 18-step chain is
`CLICK_SURFACES` — const tables walked with a uniform consumed/pass
contract, mid-chain guards included as explicit entries. Adding a
surface is a variant + a table position + a dispatch arm; the
ordering no longer lives in control flow. What the tables are *not*
yet: a real hit-test — containment still lives inside each surface's
handler because non-widget chrome has no boxes. Since then: the
`ScrollRegion` channel is deleted — the scroll payload rides the
scrollable widget's own box (one object, one shift path; the
scrollbar painters and drag read the box tree) — and vertical flex
shipped: `Spacer { flex }` inside a height-budgeted `Col` absorbs the
leftover rows (the single-fill pass takes precedence), so
bottom-pinned chrome needs no plugin row arithmetic. Still open, in
plan order: chrome boxes + true hit-tested
capture/bubble replacing the tables and the `*Layout` caches, and the
short gesture-scoped double-/right-click chains (4), the app-level
focus unification — `FocusManager`,
`Prompt.toolbar_focus`, dock focus (5), host-side editing keys (6 step
2), `WidgetSpec::Popup` + side-channel/`overlay_hit_test` deletion (7 —
plugin-visible wire change, budget the web renderer mirror), Settings
(8), and tokens/`Async` (9).

Two standing costs every phase must budget for, found in review: (a) the **web widget
renderer** (`web-ui/js/65-widgets.js`) hand-mirrors the spec vocabulary and must be
updated for every kind/field change; (b) the **e2e suites assert on screen strings**,
so any phase that intentionally changes visual output (2, 3, 7, 8) pays an assertion-
churn tax — which is exactly why phase 0 exists.

---

## 5. What not to do

- **Do not build a CSS engine.** Box model, flex, overflow, stacking. No cascade, no
  selectors, no stylesheet language.
- **Do not move layout into the plugin.** The current pain is precisely that layout
  leaked across the IPC boundary. v2 moves it *back*.
- **Do not make the plugin API imperative.** Retained-declarative + keyed reconciliation
  is the best thing about the current design.
- **Do not weaken host-owned hit-testing.** Plugins must keep never seeing raw
  coordinates.
- **Do not make widget *kinds* plugin-extensible.** Self-contained components should
  mean a host-side `WidgetImpl` per kind (§4.3), not plugin-authored behaviour running
  per event. The latter puts an IPC round-trip on every keystroke and breaks the
  web/GUI renderers, which cannot paint a kind they don't know.
- **Do not regress the `Scene` projections.** Multiple frontends from one semantic
  model, guarded by a parity test, is a strategic asset; layout v2 must sit *below*
  it — and phase 0 must first extend that parity test to widgets, which it does not
  cover today.
- **Do not migrate Settings first.** Again.

---

## 6. Estimated deletion

Rough, source-counted, assuming all phases land:

| Deleted | Lines |
|---|---|
| `view/controls/` (7 input state machines + domain models) | ~6,200 |
| `view/settings/widget_map.rs` | ~1,300 |
| `view/ui/scroll_panel.rs` | ~770 |
| Wheel/click ladders + `*Layout` caches in `mouse_input.rs` | ~1,500 (of 4,713) |
| `Prompt`'s inline text engine + `toolbar_focus` interleaving | ~400 |
| Context-menu structs collapsed into one anchored popup | ~600 |
| `RenderOutput` side channels + `overlay_hit_test` | ~500 |
| **Total** | **~11,000** |

Against roughly 3,000–4,000 lines of new layout/hit-test/focus engine. The net is not
the point — the point is that ~11,000 lines currently exist *because* those 3,000 do
not.
