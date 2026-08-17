# One Widget Library — Design Spec

> _AI-generated design doc. **Entirely PLANNED** — nothing here ships today._
>
> Target: one tree/layout/event/render library used by **both** the
> plugin-facing widgets **and** all editor chrome (menus, prompt, popups,
> context menus, dock, splits, modals, status bar).
>
> Third doc in an arc. [`widget-framework-v2-review.md`](widget-framework-v2-review.md)
> diagnosed the panel side and built the per-kind `WidgetImpl` dispatch, the
> `LayoutBox` arena and hit-path propagation;
> [`chrome-event-model-plan.md`](chrome-event-model-plan.md) registered the chrome
> surfaces behind one pointer walk and one keyboard walk. This doc is where
> those point. It assumes both arcs as its starting point rather than
> restating them.

---

## 0. The commitment

**Separate the cheap description of the UI from the expensive thing that
holds state.** Everything below follows from that; get it wrong and the
project spends its life fighting symptoms.

Concretely, for Fresh: rebuilding a description is free and happens whenever
state changes; layout, paint caches, focus registration and platform handles
persist and are touched only where something actually changed.

This supersedes the "rebuild the whole tree every frame and look state up in
a map keyed by widget key" model. That model is what Fresh has today
(`WidgetInstanceState` keyed by `Key`), and it is the ID-side-table pattern —
identity is implicit, collisions are silent, and a rename resets state with
no diagnostic. A retained tree exists precisely to make identity explicit.

---

## 1. The three trees

Conflating any two of these is the mistake that ends projects.

| | What it is | Lifetime | Holds |
|---|---|---|---|
| **1. Description** `Node<M>` | Immutable recipe. `Button { label, on_press }` describes a button; it is not one. | One rebuild. Garbage immediately. | Props supplied by the parent. Nothing durable. |
| **2. Element** `Element` | Identity. Knows which description produced it, its children, its local state, its render object. | Lifetime of the logical component. | `(type, key)`, component state, child links, dirty flag, depth. |
| **3. Render object** `dyn RenderObject` | The expensive thing. | Created rarely, mutated constantly. | Computed geometry, cached measurements, paint state, focus registration, host handles. |

Sanity check: **if you want to read layout geometry off a description, layers
1 and 3 have collapsed** and the design is already broken. Descriptions never
carry rects.

### Where Fresh's current types land

| Today | Layer | Note |
|---|---|---|
| `WidgetSpec`, chrome `collect()` boxes | 1 | Already description-shaped. `WidgetSpec` needs to shed nothing but optional keys (§3). |
| `WidgetInstanceState` map, `PanelState` | 2 | Moves onto elements. The map goes away. |
| `LayoutBox` arena, `ChromeLayout` caches, paint-recorded rects | 3 | Becomes render-object fields, computed by the layout pass. |
| `WidgetImpl` | split | `collect` → layer-1 build or layer-3 paint depending on the kind; `on_*` → render-object event handling. |

---

## 2. Layer 1 — Descriptions

A description tree is a pure function of state. Constructing one must have
**no side effects**: no platform resources, no registration, no mutation of
anything outside itself.

```rust
pub struct Node<M> {
    pub key: Option<Key>,
    pub desc: Desc<M>,
    pub children: Vec<Node<M>>,
}

pub enum Desc<M> {
    // primitives — the only things with render objects (§9)
    Box(BoxProps),           // constraint layout: row/col/stack/pad/align
    TextRun(TextProps),      // leaf paint
    Viewport(ViewportProps), // clip + scroll offset
    Gesture(GestureProps<M>),// pointer region + listeners
    Focusable(FocusProps<M>),// focus registration + key listeners
    Layer(LayerProps),       // out-of-flow, stacking context (§10)
    Host(HostId),            // foreign content: buffer split, PTY grid

    // composition — builds a subtree from props (+ state)
    Component(Rc<dyn Component<M>>),
}
```

Rules that make this safe:

- **Value types, constructor arguments only.** No post-construction setters.
- **No durable references out.** If imperative code needs a handle (a scroll
  controller, a text-field handle), the *caller* creates it and passes it in.
  A description never hands one back — that would smuggle a durable reference
  into a disposable object.
- **`M` is the message type.** Chrome uses in-process closures
  (`Handler<M> = Rc<dyn Fn(&Event) -> Option<M>>`); the plugin bridge uses
  message *names* because it crosses a serialization boundary (§11). Both are
  "events flow up through explicit callbacks"; only the encoding differs.

The failure this prevents: descriptions that accumulate state, so rebuilding
becomes destructive, so you avoid rebuilding, so you start mutating layer 3
directly — and now there are two sources of truth.

---

## 3. Layer 2 — Elements and reconciliation

```rust
pub struct Element<M> {
    key: Option<Key>,
    type_id: TypeId,              // of the description variant / component
    desc: Node<M>,                // the description that produced this
    state: Option<Box<dyn Any>>,  // component-local state (§4)
    children: Vec<ElementId>,
    render: Option<RenderId>,     // primitives only
    parent: Option<ElementId>,
    depth: u32,
    needs_build: bool,
}
```

### The matching rule

Reconciling a node's children against new descriptions, **position by
position**:

```
same type AND same key  ->  same logical element; update in place
otherwise               ->  unmount the old subtree, mount the new
```

Position is the implicit key when none is given. Unkeyed static UI therefore
works with zero ceremony and degrades correctly.

Two derived behaviors, deliberate and documented:

- **Changing a key forces a remount.** This is the sanctioned way to reset a
  subtree — e.g. re-keying the prompt on `prompt_type` change clears its
  editing state without a manual reset path.
- **Type changes always remount.** No cleverness about "similar" types. If
  the type changed, the author changed their mind about what this is.

### Keys are the author's job

No algorithm can tell whether the third row is "the same item, moved" or "a
different item in the same slot" — that lives only in the domain model. So
**make forgetting visible**: keyed builders take a key function as a required
argument, not an optional field.

```rust
List::keyed(items, |it| Key::from(it.id), |it| row(it))   // key fn required
```

For Fresh this matters most in the plugin API, where `WidgetSpec.key` is
optional today. Required-key list/tree builders are a breaking change to
`widgets.ts` worth making before chrome depends on the framework.

---

## 4. Local state lives on the element

A component's private state — scroll offset, dropdown open flag, animation
value, tree expansion, list selection — belongs to layer 2. The description
carries only what the parent supplied. When reconciliation matches an element
to a new description, it **keeps the state and swaps the description**.

That single move is what makes "rebuild everything" cheap without losing
anything.

```rust
pub trait Component<M> {
    type State: Default + 'static;
    fn build(&self, state: &Self::State, cx: &mut BuildCx<M>) -> Node<M>;
}

impl BuildCx<'_, M> {
    fn set_state(&mut self, f: impl FnOnce(&mut S));  // mutate + mark dirty (§5)
}
```

Decide three things up front and the API writes itself: how state is
allocated, how it is disposed, and what a component sees when it is
remounted. Answers here: allocated on mount, dropped on unmount, and a
remount is indistinguishable from a first mount (that is the point of key
changes).

**This replaces Fresh's `WidgetInstanceState` map.** Today a widget's scroll
offset is looked up by string key in a side table; after this it is a field on
the element, visible in a debugger, disposed with the element, impossible to
collide.

---

## 5. Scheduling: mark, then flush once

Updates do not propagate. They **mark**.

`set_state` does exactly two things: mutate the element's state, and add the
element to a dirty set. Nothing else happens synchronously.

```rust
fn flush(&mut self) {
    assert!(!self.building, "set_state during build at {}", self.current);
    self.dirty.sort_by_key(|e| self.depth(e));   // shallowest first
    while let Some(e) = self.dirty.pop_front() {
        if self.is_disposed(e) || !self.needs_build(e) { continue; }
        self.rebuild(e);
    }
    self.flush_layout();
    self.flush_paint();
}
```

**Depth ordering matters more than it looks.** If a parent and child are both
dirty, the parent rebuilds first and may reconcile the child anyway — so no
element is rebuilt twice in a pass, and no element is rebuilt just before its
parent disposes it.

**No reactive graph.** No dependency tracking, no observer registration. State
flows down through constructor arguments; events flow up through explicit
callbacks. Everything that happens is a function call visible in a stack
trace.

Be honest about the limit: a callback can `set_state` on an ancestor, which
rebuilds, which fires the callback again. This design does not make cycles
impossible — it makes them **legible**: a visible call chain in one place
rather than an emergent property of an invisible graph.

**Guard the invariant loudly.** Reconciliation must not be re-entrant. A
`set_state` during build throws, naming the offending element. That single
assertion kills an entire class of otherwise-undebuggable bug.

Fresh already has the right shape for the plugin half: plugin commands are
queued and applied between frames (the documented one-frame lag), which is a
flush boundary by construction.

---

## 6. Layer 3 — Render objects and the pipeline

Reconciliation decides **what exists**. It does not decide geometry.

```rust
pub trait RenderObject {
    fn layout(&mut self, c: Constraints, cx: &mut LayoutCx) -> Size;
    fn paint(&self, g: Geom, out: &mut DrawList);
    fn hit(&self, local: Point) -> Hit;
    fn relayout_boundary(&self) -> bool { false }
}
```

### 6.1 The constraint model — decide this first

**This is the least reversible decision in the framework.** Layout
ergonomics, error messages and performance all descend from it.

Choice: **box constraints, Flutter-style, in integer cells.** Constraints
(`min_w`, `max_w`, `min_h`, `max_h`) propagate down; sizes come back up;
parents position children. One pass, no solver, no fractional-cell rounding
bugs.

- `Sizing::{Cells(n), Flex(w), Pct(p), Auto}` resolve into constraints.
- **Intrinsic sizing is opt-in and documented as expensive** (a second
  measurement of a subtree). `Auto` on a large subtree is the trap; name it.
- If you find yourself needing a second layout pass for a normal case, that is
  a defect in the constraint model, not a performance problem.

### 6.2 Layout pass

Walks the render tree in tree order, honoring dirty flags. `needs_layout`
propagates **up to the nearest relayout boundary** — a node whose size cannot
change as a result (fixed-size box, viewport with a fixed rect). A status-bar
text change then relayouts the status bar, not the split grid.

Output per node: `rect`, `clip`, `scroll window`. This is the **only** source
of geometry — hit-testing, the display list, the Scene projection and tests
all read it, and nothing reads geometry recorded during paint.

### 6.3 Paint pass and the display list

Painting produces a flat, ordered, absolute, keyed display list rather than
touching cells:

```rust
pub struct LayoutSpec {
    pub frame: Size,
    pub items: Vec<Item>,                 // paint order IS list order
    pub index: Vec<(Key, Range<usize>)>,  // key -> items: hit-test, tests, web patching
    pub cursor: Option<CursorSpec>,
}

pub struct Item {
    pub key: Option<Key>,
    pub rect: Rect,        // absolute
    pub clip: Rect,        // ancestor clips intersected
    pub theme: ThemeKey,   // per-item provenance (theme inspector)
    pub draw: Draw,        // Fill | Border | Scrim | Lines | Scrollbar | Host
}
```

Backends are folds over `items`: TUI writes a ratatui `Buffer`; the web
backend patches DOM by `Key`; tests assert on `items`/`index` instead of
scraping cells. TUI/web parity becomes one IR with two presenters rather than
two implementations.

The paint walk skips work four ways — off-screen (`rect ∩ clip` empty),
occluded (an opaque full-frame `Layer` above), scrolled out (a `Viewport`
emits only `[offset, offset+visible)`), and absent (never built). Cost is
**O(visible items)**, so a `List` over 100k rows emits a screenful.

### 6.4 Boundaries, and what Fresh actually needs

Keep dirty flags per pass and per node. But be honest about the terminal:
crossterm already diffs cells, and a full display list for one screen is
small. So **`needs_layout` boundaries are load-bearing; `needs_paint`
boundaries are an optimization** that can land later. Do not build repaint
boundaries before measuring.

---

## 7. Hit-testing and pointer propagation

Hit-testing walks the **render tree** back-to-front, collecting the path from
the deepest hit node to the root. Because it is built from persisted
geometry, it is correct for overlapping and transformed content in a way a
description walk never could be.

```
path   = hit_test(point)          // deepest -> root
capture: root -> target           // each node may claim
target
bubble : target -> root           // each node may claim
```

- `Flow::Stop` is the cancellation flag. There is no third disposition:
  acting without claiming is simply not stopping.
- `PointerMode::Opaque` is the default; `Transparent` passes through after its
  own handlers; `Ignore` is not hittable.
- **Pointer capture is per-node**: a press handler calls
  `cx.capture_pointer()`, and subsequent moves/ups route there regardless of
  position until release or unmount. That is the entire drag model — it
  replaces a global grab enum and its ordered derivation.
- Wheel bubbles; a `Viewport` claims it only if it actually moved, so scroll
  chaining is structural.

---

## 8. Focus is a separate tree

The design point most often missed. Focusable render objects register into a
**focus tree** that mirrors — but is not identical to — the render tree.

```rust
pub struct FocusNode { scope: bool, ordinal: Option<i32>, skip: bool }

pub trait TraversalPolicy {
    fn next(&self, scope: &FocusScope, from: FocusId, dir: Dir) -> Option<FocusId>;
}
```

- **Scopes** group; a modal `Layer` opens one and traps traversal inside it.
- **Policy is pluggable** and application-specific: reading order (default),
  explicit ordinals, and directional 2D traversal are all legitimate. Fresh
  wants reading order for forms and directional for the split grid.
- **Keyboard propagates along the focus chain, not the hit chain.** A key
  pressed in the prompt input is offered to the input, then its focus
  ancestors, up to the app root.
- **Focus survives reconciliation**, because registration lives on the render
  object and reconciliation preserves matched elements. Write that test on
  day one — it is the direct payoff of the three-tree split, and the thing
  Fresh gets wrong today (a rebuild that re-seeds `focus_key` from a spec
  walk).

### Shortcuts → Intents → Actions

Worth adopting wholesale, because it is a better factoring than Fresh's
current `KeyContext` enum:

```
key chord --[Shortcuts map on the focus chain]--> Intent   (what the user meant)
Intent    --[Actions map on the focus chain]---> handler  (how THIS part of the tree does it)
```

The same `Intent::Cancel` is handled differently depending on where focus is —
the prompt cancels itself, a modal closes, the buffer clears its selection —
with no central context enum and no precedence table. `KeyContext` and
`KeyScope` both dissolve into "where is focus".

---

## 9. Composition is the only extension mechanism

No second kind of thing. No privileged primitives with capabilities user code
cannot have.

**Primitives** (the only descriptions with render objects) are the ones that
cannot be composed: `Box`, `TextRun`, `Viewport`, `Gesture`, `Focusable`,
`Layer`, `Host`.

**Everything else is a `Component`**, including things that feel built-in:

```rust
Button   = Focusable(Gesture(Box(TextRun)))
Toggle   = Focusable(Gesture(Box([TextRun(mark), TextRun(label)])))
List     = Viewport(Box::col(items.map(row)))          // + selection state
Dropdown = Component { Button, and a Layer when open }
```

The cost is verbosity — deeply nested descriptions are the known complaint
about this style. Mitigate with convenience constructors and good defaults,
**never** with a privileged escape hatch. `Host` is the one exception and it
is justified: buffer text and PTY grids have their own renderers, and the
primitive is "a render object with custom layout/paint/hit", which user code
can also implement.

---

## 10. Floating: `Layer`

A `Layer` is a normal child in the description tree — so it inherits
containment, focus scoping, propagation and its owner's identity — but its
render object lays out and paints out of flow.

```rust
pub struct LayerProps {
    pub anchor: Anchor,        // Parent | Node(Key) | Point(x,y) | Screen(Align)
    pub place: Place,          // Below | Above | RightOf | LeftOf | Over | Fill
    pub fit: Fit,              // FLIP | CLAMP | SHIFT
    pub modality: Modality,    // None | Inert | Exclusive
    pub scrim: Option<Scrim>,
    pub dismiss: Dismiss,      // OUTSIDE_POINTER | ESCAPE | ANY_KEY | ANY_INPUT
}
```

- **Stacking**: a `Layer` opens a stacking context, so its descendants never
  interleave with content outside it. No global z scale to co-ordinate.
- **Modality**: `Inert` marks everything outside the subtree non-interactive;
  `Exclusive` additionally cuts host raw input (PTY). Pointer, keyboard, focus
  traversal, hover and cursor visibility all derive from `inert` — one
  property replacing five encodings.
- **Dismissal is declarative**, and `OUTSIDE_POINTER` is an ancestor test the
  reconciler tree already supports. This is what replaces full-frame guard
  boxes.

---

## 11. The plugin boundary

The bridge is a natural fit: a plugin already sends a whole `WidgetSpec` tree
and the host applies it between frames. That *is* layer 1 crossing a wire.

- **The reconciler lives host-side.** The plugin sends descriptions; the host
  reconciles them against persistent elements. Plugin-visible state (list
  scroll, tree expansion, selection) stops living in a host side table keyed
  by widget key and becomes element state.
- **Callbacks are by name** across the wire (`on: { activate: "open" }`)
  rather than by reference. Same semantics, different encoding.
- **Keys become required** for keyed builders in `widgets.ts` (§3).
- **`WidgetSpec` stays a stable subset** of the internal description
  vocabulary. It is versioned public API with a `.d.ts`; chrome's vocabulary
  must be able to grow without ratcheting it. Not exposed: `Host`,
  `Modality::Exclusive`, focus policies, arbitrary `M`.

---

## 12. Examples

### 12.1 Menu bar and dropdown

```rust
struct MenuBar { menus: Rc<[Menu]> }

impl Component<Action> for MenuBar {
    type State = MenuState;                       // open index, submenu path

    fn build(&self, s: &MenuState, cx: &mut BuildCx<Action>) -> Node<Action> {
        Box::row().h(1).children(self.menus.iter().enumerate().map(|(i, m)| {
            Focusable::new(Gesture::new(Box::pad(1, 0).child(TextRun::new(&m.title)))
                .on_click(cx.handler(move |_| Msg::Toggle(i)))
                .on_enter(cx.handler(move |_| Msg::Hover(i))))     // auto-switch
                .key(Key::from(("menu", i)))
                .child_if(s.open == Some(i), || dropdown(m, i, cx))
        }))
    }
}

fn dropdown(m: &Menu, i: usize, cx: &mut BuildCx<Action>) -> Node<Action> {
    Layer::new()
        .anchor(Anchor::Parent).place(Place::Below).fit(FLIP | CLAMP)
        .modality(Modality::Inert)
        .dismiss(OUTSIDE_POINTER | ESCAPE)
        .child(List::keyed(m.items, |it| Key::from(it.id), |it| item_row(it))
            .autofocus()
            .on_activate(cx.emit(move |e| Action::RunMenuItem(i, e.index))))
}
```

Gone: the close-guard box (`dismiss`), the z number (stacking context), the
rank entry (tree position + `Modality`), the central hover machine (a
listener), the menu's key dispatcher (focus chain + intents). Open state is
element state, so a rebuild does not close the menu.

### 12.2 Split grid

```rust
fn splits(t: &SplitTree) -> Node<Action> {
    match t {
        Leaf(id) => Box::col().children([
            tab_strip(*id).h(1),
            Box::row().flex(1).children([
                Focusable::new(Host::new(HostId::Buffer(*id)))
                    .key(Key::from(("buf", id)))
                    .flex(1)
                    .actions([(Intent::Cancel, Action::ClearSelection)]),
                vscrollbar(*id).w(1),
            ]),
        ]),
        Node { dir, a, b, ratio } => Box::flex_dir(*dir).children([
            splits(a).flex(*ratio),
            Gesture::new(Divider::new())
                .key(Key::from(("sep", t.id())))
                .on_press(|cx| { cx.capture_pointer(); Msg::BeginResize })
                .on_move(|cx, p| Msg::Resize(p.x)),
            splits(b).flex(100 - *ratio),
        ]),
    }
}
```

`capture_pointer` is the whole btop-resize fix, from the primitive. The buffer
is a `Host` leaf keeping the token-IR pipeline. Wheel over the buffer scrolls
it; at its bound it bubbles and dies at the root.

### 12.3 Context menu

```rust
Gesture::new(tab_button(tab))
    .key(Key::from(("tab", tab.id)))
    .on_secondary_click(cx.emit(move |e| Action::OpenTabMenu(tab.id, e.point)))
    .child_if_some(menu_for(tab.id), |c| {
        Layer::new()
            .anchor(Anchor::Point(c.at)).fit(FLIP | CLAMP)
            .modality(Modality::Inert)
            .dismiss(OUTSIDE_POINTER | ESCAPE)
            .child(List::keyed(c.items, |i| Key::from(i.id), menu_row)
                .autofocus()
                .on_activate(cx.emit(move |e| Action::TabMenuItem(tab.id, e.index))))
    })
```

The menu is a *child of what it acts on*, so its target is structural rather
than carried in menu state. All four native context menus are this, with
different item lists.

### 12.4 Command palette

```rust
impl Component<Action> for Prompt {
    type State = PromptState;                     // query, selection, scroll

    fn build(&self, s: &PromptState, cx: &mut BuildCx<Action>) -> Node<Action> {
        Layer::new()
            .key(Key::from(self.kind))            // re-key on kind => fresh state
            .anchor(Anchor::Screen(if self.overlay { Center } else { Bottom }))
            .modality(if self.overlay { Modality::Inert } else { Modality::None })
            .scrim(self.overlay.then(Scrim::dim))
            .dismiss(ESCAPE)
            .child(FocusScope::new(Box::col().children([
                TextField::new(&s.query)
                    .autofocus()
                    .on_change(cx.set_state(|s, v| s.query = v))
                    .on_submit(cx.emit(|_| Action::PromptConfirm)),
                toolbar(self).if_(self.has_options),
                Box::row().flex(1).children([
                    List::keyed(&self.results, |r| Key::from(r.id), result_row)
                        .flex(1)
                        .selected(s.selection)
                        .on_select(cx.set_state(|s, i| s.selection = i))
                        .on_activate(cx.emit(|e| Action::PromptConfirmAt(e.index))),
                    preview(self).flex(1).if_(self.overlay),
                ]),
            ])))
    }
}
```

`FocusScope` is the overlay toolbar ring. The scrim replaces the click-scrim
box. Re-keying on `prompt_type` is the reset path. Query and selection are
element state, so a results refresh does not disturb them — which is the bug
class the current prompt works around with `manual_scroll` latches.

### 12.5 Transient popup

```rust
Layer::new()
    .anchor(Anchor::Point(h.at)).place(Place::Below).fit(FLIP | CLAMP)
    .dismiss(ANY_KEY | OUTSIDE_POINTER)
    .child(Viewport::new(TextRun::markdown(&h.body)).selectable().max_h(20))
```

Non-modal, so the buffer keeps focus; `ANY_KEY` dismissal is a root-level
observer installed by the layer while mounted, which is the "must see the key
even when a modal above consumes it" requirement without a pipeline stage.

### 12.6 Modal dialog

```rust
Layer::new()
    .anchor(Anchor::Screen(Center)).modality(Modality::Exclusive)
    .scrim(Some(Scrim::dim))
    .dismiss(if d.cancellable { ESCAPE } else { Dismiss::NONE })
    .child(FocusScope::new(Box::col().pad(1).border().children([
        Viewport::new(TextRun::markdown(&d.body)).flex(1),
        RadioGroup::keyed(&d.options, |o| Key::from(o.id)).autofocus(),
        Box::row().gap(2).children([
            Button::new("OK").on_press(cx.emit(|_| Action::ConfirmTrust)),
            Button::new(d.secondary()).on_press(cx.emit(|_| Action::CancelTrust)),
        ]),
    ])))
```

`Modality::Exclusive` is today's whole-channel mouse capture, keyboard
capture, PTY block, hover suppression and cursor suppression — one property.

---

## 13. What this changes in Fresh

| Today | Here |
|---|---|
| `WidgetInstanceState` map keyed by widget key | element state (§4) |
| focus re-seeded from a spec walk each render | focus registration on the render object, surviving reconciliation |
| optional `key` on `WidgetSpec` | required key functions on keyed builders |
| full-frame guard boxes | `Layer.dismiss` + ancestor test |
| `layer_rank`, hand-assigned box `z` | tree position + stacking contexts + `Modality` |
| `KeyContext` / `KeyScope` | focus chain + Shortcuts→Intents→Actions |
| `PointerGrab` enum + grab-keyed routing | per-node `capture_pointer` |
| `Disposition::{Consumed, PassAfter, Pass}` | `Flow` + not-stopping |
| `pointer_opaque` opt-in | `PointerMode::Opaque` default |
| pre-band stages | root-scope listeners / layer-declared observers |
| chrome layout cache written during paint | layout-pass output, single geometry source |
| `Scene`'s hand-written `*_view` methods | `LayoutSpec` |
| residual per-kind matches | composition + capabilities on render objects |
| rebuild-everything-per-frame | mark dirty, flush once, depth-ordered |

---

## 14. Build order

The order is not negotiable, because steps 1–2 define the semantics and
everything later is downstream. A mistake in the renderer is one module; a
mistake in the reconciler is everything.

1. **The reconciler against a fake renderer** that logs create/update/dispose.
   Get keying and remount semantics right before any pixels exist.
2. **The dirty-marking scheduler**, with the re-entrancy assertion. Test:
   dirty a parent and child in one tick, assert exactly one build each.
3. **Layout**, once the constraint model (§6.1) is settled on paper.
4. **Hit-testing and propagation.**
5. **Focus, last** — it depends on everything above, and it is where you find
   out whether the retained tree really persists the way you think it does.

Then adopt surface by surface, cheapest first: context menus → menu dropdowns
→ info popups → file browser → status bar → prompt. Splits and terminals stay
`Host` leaves permanently. Settings and the keybinding editor last.

---

## 15. Risks and prerequisites

1. **The constraint model is the least reversible decision.** Settle §6.1
   before step 3, on paper, with the awkward cases written out (a `List` with
   `Auto` height inside a flex column inside a modal).
2. **Paint-time compositing gates chrome layers.** Layers over host content
   need clipping and z — the same gap that makes buffer-mounted panels drop
   their overlay/popup channels today.
3. **The geometry inversion is the risky refactor**, not the tree. Moving from
   paint-recorded to layout-recorded rects touches every hit-test and the
   Scene projection; keep the existing parity oracle running through it.
4. **Freeze the plugin-visible subset** before chrome depends on internal
   vocabulary.
5. **Retained-tree memory and disposal.** Every element and render object must
   have a defined disposal path (focus deregistration, host handles, captured
   pointers). Leaks here are invisible until they are a bug report about a
   ghost focus target.
6. **Screen-scraping tests.** The e2e suite asserts on rendered cells; hold
   cell output byte-identical through the layout/paint migration and add
   `LayoutSpec`-level assertions as the *new* tests rather than rewriting the
   old ones.
7. **Do not build repaint boundaries early** (§6.4). Measure first; crossterm
   already diffs cells.
