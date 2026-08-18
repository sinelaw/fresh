# One Widget Library — Design Spec

> _AI-generated design doc. **Entirely PLANNED** — nothing here ships today._
>
> A UI library for Fresh: one tree, one layout model, one event model, one
> paint output, used by both the plugin-facing widgets and all editor chrome.

---

## 1. Goals

1. **One library for everything on screen.** Menus, prompt, popups, context
   menus, dock, splits, modals, status bar, and plugin panels are all built
   from the same primitives with the same event and layout rules. No
   privileged internal surface.
2. **Generic registration and propagation, with no hand-specified
   exceptions.** Precedence, modality, dismissal and focus order are *derived*
   from structure and declared properties — never from a central ordered list
   that authors must edit to add a surface.
3. **Rebuilding the UI description is free.** Describing what should be on
   screen costs an allocation and nothing else, so nobody avoids rebuilding
   and nobody mutates render state behind the framework's back.
4. **Identity is explicit.** Which on-screen thing is "the same thing" as last
   frame is answered by the tree and an author-supplied key, visible in a
   debugger — never by hashing an identifier stack into a side table.
5. **One source of geometry.** Layout computes rects; hit-testing, painting,
   tests and the web projection all read them. Nothing derives geometry while
   drawing.
6. **Composition is the only extension mechanism.** Anything the library can
   build, user code can build, from the same parts.
7. **Backend independence.** Paint produces a display list; TUI cells, the web
   DOM, and test assertions are three consumers of it, not three
   implementations of the UI.

## 2. Non-goals

- **Buffer and terminal text rendering.** The token IR → `ViewLine` pipeline,
  syntax highlighting, folding, wrapping and PTY grids stay as they are,
  reached through a `Host` leaf.
- **A reactive dependency graph.** No observers, no signals, no automatic
  invalidation tracking. State flows down through constructor arguments;
  events flow up through explicit callbacks.
- **Making update cycles impossible.** They are made *legible* instead — a
  visible call chain in one place rather than an emergent property of an
  invisible graph.
- **Damage tracking / dirty rectangles at the cell level.** crossterm already
  diffs the back buffer.
- **An animation system.** The existing frame-buffer animation runner is
  unchanged.
- **Retained mutable widget objects at the description layer.** Descriptions
  are values; only the framework holds anything durable.

---

## 3. The model — three trees

The whole design rests on one commitment: **separate the cheap description of
the UI from the expensive thing that holds state.**

| | What it is | Lifetime | Holds |
|---|---|---|---|
| **1. Description** `Node<M>` | An immutable recipe. `Button { label, on_press }` *describes* a button; it is not one. | One rebuild, then garbage. | Props supplied by the parent. Nothing durable. |
| **2. Element** `Element` | Identity. Knows which description produced it, its children, its local state, its render object. | The lifetime of the logical component. | `(type, key)`, component state, child links, dirty flag, depth. |
| **3. Render object** `dyn RenderObject` | The expensive thing. | Created rarely, mutated constantly, disposed when the component genuinely goes away. | Computed geometry, cached measurements, paint state, focus registration, host handles. |

The split exists so that rebuilding layer 1 — which happens freely and often —
costs almost nothing, while layer 3, where the real cost lives, is touched
only where something actually changed.

**Sanity check:** if you want to read layout geometry off a description,
layers 1 and 3 have collapsed and the design is broken. Descriptions never
carry rects.

## 4. Data flow

```
   state
     │  build (pure)
     ▼
  Node<M> descriptions ──reconcile──▶ Elements ──own──▶ Render objects
     ▲                                   │                    │
     │                              set_state            layout pass
     │                                   │                    │  (constraints down,
     │                                   ▼                    │   sizes up)
     │                              dirty set                 ▼
     │                                   │              geometry: rect, clip, scroll
     │                          flush (depth-ordered)         │
     └───────────────────────────────────┘                    ├──▶ paint pass ──▶ LayoutSpec ──▶ backend
                                                              │                                  (TUI / web / tests)
                                                              └──▶ hit-test ──▶ propagation path
                                                                                      │
   focus tree ◀──register── Focusable render objects                                  ▼
        │                                                                     capture / target / bubble
        └──▶ key events ──▶ Shortcuts ──▶ Intent ──▶ Actions ──▶ handlers ──▶ set_state / app messages
```

Four passes per flush, each tree-ordered with its own dirty flags: **build**,
**layout**, **paint**, and (on input) **dispatch**.

---

## 5. Descriptions

A description tree is a pure function of state. Constructing one has **no side
effects**: no platform resources, no registration, no mutation of anything
outside itself.

```rust
pub struct Node<M> {
    pub key: Option<Key>,
    pub desc: Desc<M>,
    pub children: Vec<Node<M>>,
}

pub enum Desc<M> {
    // primitives — the only descriptions with render objects (§11)
    Box(BoxProps),            // constraint layout: row / col / stack / pad / align
    TextRun(TextProps),       // leaf paint
    Viewport(ViewportProps),  // clip + scroll offset
    Gesture(GestureProps<M>), // pointer region + listeners
    Focusable(FocusProps<M>), // focus registration + key listeners
    Layer(LayerProps),        // out-of-flow, stacking context (§12)
    Host(HostId),             // foreign content: buffer split, PTY grid

    // a subtree the reconciler may skip when unchanged (§6)
    Shared(Rc<Node<M>>),

    // composition — builds a subtree from props (+ state)
    Component(Rc<dyn Component<M>>),
}
```

- **Value types, constructor arguments only.** No post-construction setters.
- **No durable references out.** If imperative code needs a handle (a scroll
  controller, a text-field handle), the *caller* creates it and passes it in.
  A description never hands one back.
- **`M` is the message type.** In-process code uses closures
  (`Handler<M> = Rc<dyn Fn(&Event) -> Option<M>>`); the plugin bridge uses
  message *names*, because it crosses a serialization boundary (§13). Same
  semantics, different encoding.

## 6. Elements and reconciliation

```rust
pub struct Element<M> {
    key: Option<Key>,
    type_id: TypeId,              // of the description variant / component
    desc: Node<M>,                // the description that produced this
    state: Option<Box<dyn Any>>,  // component-local state (§7)
    children: Vec<ElementId>,
    render: Option<RenderId>,     // primitives only
    parent: Option<ElementId>,
    depth: u32,
    needs_build: bool,
}
```

Reconciling a node's children against new descriptions, **position by
position**:

```
same type AND same key  ->  same logical element; update in place
otherwise               ->  unmount the old subtree, mount the new
```

Position is the implicit key when none is given, so unkeyed static UI works
with zero ceremony and degrades correctly.

Two derived behaviors, deliberate and documented:

- **Changing a key remounts.** This is the sanctioned way to reset a subtree —
  re-key the prompt on `prompt_type` and its editing state clears with no
  manual reset path.
- **Type changes always remount.** No cleverness about "similar" types. If the
  type changed, the author changed their mind about what this is.

**Keys are the author's job and cannot be inferred.** No algorithm can tell
whether the third row is "the same item, moved" or "a different item in the
same slot" — that information exists only in the domain model. So the API makes
forgetting visible: keyed builders take a key function as a *required*
argument.

```rust
List::keyed(items, |it| Key::from(it.id), |it| row(it))   // key fn required
```

### Rebuild propagation — the identity short-circuit

When a matched child's new description is **the same instance** the element
already holds, the subtree is skipped entirely. Otherwise the description is
swapped and the subtree rebuilds.

Reference identity is the only skip rule. Structural equality is a lie once
handlers are involved — descriptions are rebuilt every time, so closure
identity always differs, which is expected and harmless because nothing
memoizes on it. Deep comparison is the hidden cost this design exists to avoid.
Identity is O(1) and never silently wrong.

Skipping is therefore **author-controlled**: hand back the same instance, via
`Desc::Shared(Rc<Node<M>>)` held in a field, or interned at a boundary (§13).
By default a `set_state` near the root rebuilds every description below it.
That is honest, and the containment tools are the design's own:

1. **push state down**, so dirty elements are deep and their subtrees small;
2. **ambient values dirty their dependents directly** (§7), so broadcast data
   like a theme change never needs a root rebuild;
3. **hoist invariant subtrees** into `Shared`.

The ordering matters. In a language with cheap compile-time-constant
descriptions, (3) does most of the work; here it costs an `Rc` and a field, so
(1) and (2) carry the load and hoisting is a distant third. Description rebuild
is also not layout or paint — layer 3's dirty flags mean an unchanged result
reconciles cheaply — but description churn is real, and this is the only lever
against it.

## 7. State and scheduling

Private state — scroll offset, dropdown open flag, tree expansion, list
selection, animation value — lives on the **element**. The description carries
only what the parent supplied. Reconciliation keeps the state and swaps the
description; that single move is what makes free rebuilds lossless.

```rust
pub trait Component<M> {
    type State: Default + 'static;
    fn build(&self, state: &Self::State, cx: &mut BuildCx<M>) -> Node<M>;
}

impl BuildCx<'_, M> {
    fn set_state(&mut self, f: impl FnOnce(&mut S));  // mutate + mark dirty
}
```

State is allocated on mount, dropped on unmount, and a remount is
indistinguishable from a first mount — that is what makes key changes a
reliable reset.

### The frozen-tree invariant

**The tree is structurally frozen between flushes.** All mounting, unmounting
and reordering happens inside `flush`. This is *why* `set_state` defers instead
of rebuilding synchronously, and three otherwise-separate rules fall out of it:

- `set_state` during event dispatch is the normal case, not a hazard — no
  handler can tear down a node while the propagation walk still holds a path of
  element ids;
- a handler that calls a parent callback (which may `set_state` on an ancestor)
  always finishes on a live object, because disposal is deferred;
- a dirty mark on an element that flush then disposes is dropped silently.

Within flush the dirty set drains shallowest-first, with depth evaluated at
flush time, and **rebuilding a subtree clears dirty marks inside it** — a
descendant's build has already read its current fields.

Teardown runs **children before parents** (reverse of construction), so a child
releasing a parent-owned handle finds the parent alive. Within one state
object, registered behaviors tear down in reverse registration order.

### Ambient values

Prop-drilling a theme, a locale or a service handle through every intermediate
component is not acceptable, and a reactive graph is not the answer. One
primitive:

- `Ambient<T>` — a typed token, declared statically;
- `Provide(ambient, value, child)` — an ordinary description whose element
  holds the value and a list of dependents;
- `cx.read(ambient)` — walks up once, registers this element as a dependent,
  returns the value.

When a `Provide` element's value is swapped for a non-identical one, it marks
its registered dependents dirty. One explicit hop, a visible dependent list, no
tracking and no graph. There is no `didChangeDependencies` equivalent because
rebuild-and-re-read *is* the protocol.

The price, stated plainly: every dependent rebuilds regardless of which part of
the value it actually read. That is what having no tracker costs.

**Constructor reads are snapshots.** `read` is legal in the state constructor,
but the constructor does not re-run — a value cached in a field from it is
stale after the next change, silently. Anything that must track an ambient is
read in `build()`. (This is `initState` + `didChangeDependencies` in a new
costume; the rule is what dissolves it.)

### Behaviors and teardown

Reusable stateful concerns are ordinary objects in named fields, enrolled for
teardown:

```rust
let tasks    = cx.register(Tasks::new());
let ticker   = cx.register(Ticker::new(on_tick));
let expanded = cx.register(Persisted::new("expanded", HashSet::new()));
```

The shipped set: **`Tasks`** (async), **`Ticker`** (per-frame callbacks),
**`Cache`** (the memo carve-out, §8.4), **`Controller`** / **`Anchor`**
(imperative commands), **`Focusable`** (§10), **`Persisted`** (rehydration).
`Drop` covers most teardown for free; `register` exists for teardown that needs
ordering, or that must run before the tree is dismantled.

**Async.** `Tasks` owns it, with two guarantees that replace an `is_mounted`
flag entirely: results are delivered on the UI scheduler between frames, never
concurrently with build, layout or paint; and **delivery never happens after
teardown**. `launch_replacing(tag, …)` gives latest-wins for the common race;
anything subtler validates at delivery time.

**Imperative commands.** A `Controller` is constructed by the *owner*, passed
down in a description, and bound by the child at construction. Commands are
method calls forwarded to the bound state. This is the sanctioned escape from
one-way data flow and it is deliberately narrow: **a command may touch only the
target's local state** — one that changes controlled state is a data-flow
violation. `Anchor` is the same mechanism with the framework as registrar, so
host code can address a mounted surface by handle. Neither is the forbidden
side table: binding is explicit, state still lives on the element, and the
registry maps handle → element, not identity → state.

**Persistence.** `Persisted<T>` rehydrates from a host store at construction
and checkpoints at teardown, scoped by a `PersistenceScope` ambient so keys
anchor to explicit document or route ids rather than tree position. Unmount
still destroys the state object; this is rehydration on next construction, not
survival. It is **for new incidental state only** — existing serialized view
state stays app state, for the reasons in the implementation plan.

### Controlled and uncontrolled

"On the element" is the default, not the only option. Some state must outlive
the element or be readable by something other than the widget that draws it:
it is persisted, a command acts on it, or another subsystem mirrors it.

- **Uncontrolled** — the element owns it; nothing outside ever sees it. A menu
  highlight, a popup's scroll offset, a dropdown's open flag.
- **Controlled** — the owner passes the value down as a prop and receives a
  change event. The element holds only the *editing session* around it (caret,
  selection, scroll within the field).

The same widget supports both: `List::keyed(..).selected(s).on_select(..)` is
controlled; omit `selected` and the list keeps its own selection.

**The rule for deciding.** State is *not* element state if any of these is
true — otherwise it is:

1. it survives a restart (it is serialized somewhere);
2. a command, action, or plugin can act on it from outside the widget;
3. another subsystem reads it (persistence, the web projection, tests).

Framework-owned state is a third home again: focus position, `Viewport` scroll,
pointer capture and hover live on render objects, and neither the application
nor the component declares them.

**Updates mark; they do not propagate.** `set_state` mutates the element's
state and adds it to a dirty set. Nothing else happens synchronously.

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

- **Depth ordering matters more than it looks.** If a parent and child are both
  dirty, the parent rebuilds first and may reconcile the child anyway — so no
  element rebuilds twice in a pass, and none rebuilds just before its parent
  disposes it.
- **Re-entrancy is a hard error.** `set_state` during build throws, naming the
  offending element. This one assertion prevents an entire class of otherwise
  nearly undebuggable bug.

## 8. Render objects and the pipeline

Reconciliation decides **what exists**. It does not decide geometry.

```rust
pub trait RenderObject {
    fn layout(&mut self, c: Constraints, cx: &mut LayoutCx) -> Size;
    fn paint(&self, g: Geom, out: &mut DrawList);
    fn hit(&self, local: Point) -> Hit;
    fn relayout_boundary(&self) -> bool { false }
}
```

### 8.1 The constraint model

**The least reversible decision in the framework** — layout ergonomics, error
messages and performance all descend from it.

Choice: **box constraints in integer cells.** Constraints (`min_w`, `max_w`,
`min_h`, `max_h`) propagate down; sizes come back up; parents position
children. One pass, no solver, no fractional-cell rounding.

- `Sizing::{Cells(n), Flex(w), Pct(p), Auto}` resolve into constraints.
- **Intrinsic sizing is opt-in and documented as expensive** (it measures a
  subtree twice). `Auto` on a large subtree is the trap; name it in the docs.
- If a normal case needs a second layout pass, that is a defect in the
  constraint model, not a performance problem.

### 8.2 Layout pass

Walks the render tree in tree order, honoring dirty flags. `needs_layout`
propagates **up to the nearest relayout boundary** — a node whose own size
cannot change as a result (a fixed-size box, a viewport with a fixed rect). A
status-bar text change then relayouts the status bar, not the split grid.

Output per node: `rect`, `clip`, `scroll window`. This is the **only** source
of geometry.

**Geometry accessors are illegal during `build()`** — a debug assert in the
same class as the re-entrancy assert. If build could read layout, build would
depend on layout which depends on build: a cycle, or a permanent one-frame lag.
`build()` is a function of description, fields and ambients, full stop.

The element handle is split so the validity window is a type-level fact rather
than a footnote:

- `cx.services` — scheduler, focus, ambients, anchor registration. Valid from
  construction.
- `cx.geometry` — rect, size, scroll window. Valid only after first layout, and
  never inside `build()`.

For the legitimate case — structure that depends on incoming constraints —
`LayoutReader(|constraints| -> Node<M>)` is a description whose builder runs
*during* the layout pass, with constraints as an explicit argument. The
dependency becomes scoped and visible instead of cyclic. Post-layout geometry
stays legal in event handlers, tickers and task callbacks.

`LayoutReader` is the one place build runs mid-layout: `set_state` is illegal
in its builder, and under intrinsic sizing it may run more than once per frame
with different constraints.

### 8.3 Paint pass and the display list

Paint produces a flat, ordered, absolute, keyed display list rather than
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
    pub theme: ThemeKey,   // per-item provenance
    pub draw: Draw,        // Fill | Border | Scrim | Lines | Scrollbar | Host
}
```

Backends are folds over `items`: the TUI writes a ratatui `Buffer`; the web
backend patches DOM by `Key`; tests assert on `items` / `index` rather than
scraping cells.

The walk skips work four ways — off-screen (`rect ∩ clip` empty), occluded (an
opaque full-frame `Layer` above), scrolled out (a `Viewport` emits only
`[offset, offset+visible)`), and absent (never built). Cost is **O(visible
items)**: a list over 100k rows emits a screenful.

### 8.4 Speculative builds and the cache carve-out

The framework may call `build()` any number of times without committing —
intrinsic measurement, tests, offscreen rendering. Authors must not count
builds.

That makes local memoization safe, but it needs an explicit exemption from the
purity assertion: a `Cache<T>` field is excluded from the mutation check, under
the contract that **writes are idempotent functions of build inputs** — same
inputs, same value. Everything else stays immutable across `build()`.

### 8.5 Boundaries

Keep dirty flags per pass and per node. For a terminal specifically:
**`needs_layout` boundaries are load-bearing; repaint boundaries are an
optimization to defer** — the display list for one screen is small and
crossterm already diffs cells. Measure before building them.

## 9. Pointer: hit-testing and propagation

Hit-testing walks the **render tree** back-to-front, collecting the path from
the deepest hit node to the root. Because it is built from persisted geometry,
it is correct for overlapping and transformed content in a way a description
walk never could be.

```
path   = hit_test(point)          // deepest -> root
capture: root   -> target         // each node may claim
target
bubble : target -> root           // each node may claim
```

| Concern | Mechanism |
|---|---|
| claim | `Flow::Stop`. No third disposition — acting without claiming is simply not stopping. |
| default behavior | `cx.prevent_default()`, orthogonal to claiming. |
| opacity | `PointerMode::Opaque` is the default; `Transparent` passes through after its own handlers; `Ignore` is not hittable. |
| drag | `cx.capture_pointer()` — per node. Moves and ups route there regardless of position until release or unmount. That is the entire drag model. |
| scroll chaining | Wheel bubbles; a `Viewport` claims it only if it actually moved. |

## 10. Focus

Focusable render objects register into a **focus tree** that mirrors — but is
not identical to — the render tree.

```rust
pub struct FocusNode { scope: bool, ordinal: Option<i32>, skip: bool }

pub trait TraversalPolicy {
    fn next(&self, scope: &FocusScope, from: FocusId, dir: Dir) -> Option<FocusId>;
}
```

- **Scopes** group; a modal `Layer` opens one and traps traversal inside it.
- **Policy is pluggable**: reading order (default), explicit ordinals, and
  directional 2D traversal are all legitimate. Forms want reading order; the
  split grid wants directional.
- **Keyboard propagates along the focus chain, not the hit chain.** A key
  pressed in a text field is offered to the field, then its focus ancestors,
  up to the app root.
- **Focus survives reconciliation**, because registration lives on the render
  object and reconciliation preserves matched elements. Worth a test on day
  one — it is the direct payoff of the three-tree split.
- **Registration is a behavior**: `cx.register(Focusable::new(on_focus_change))`.
  `request_focus()` is an imperative command — legal in handlers, never in
  `build()`. A component that *renders* focus mirrors it into its own state
  through `on_focus_change`; focus itself is never component state.

### Shortcuts → Intents → Actions

```
key chord --[ Shortcuts map on the focus chain ]--> Intent   (what the user meant)
Intent    --[ Actions   map on the focus chain ]--> handler  (how THIS part does it)
```

The same `Intent::Cancel` is handled differently depending on where focus is —
a prompt cancels itself, a modal closes, a buffer clears its selection — with
no central context enum and no precedence table.

## 11. Composition and primitives

No second kind of thing. No privileged primitives with capabilities user code
cannot have.

**Primitives** (the only descriptions with render objects) are the ones that
cannot be composed:

```
Box   TextRun   Viewport   Gesture   Focusable   Layer   Host
```

**Everything else is a `Component`**, including things that feel built-in:

```rust
Button   = Focusable(Gesture(Box(TextRun)))
Toggle   = Focusable(Gesture(Box([TextRun(mark), TextRun(label)])))
List     = Viewport(Box::col(items.map(row)))          // + selection state
Dropdown = Component { Button, and a Layer when open }
```

The cost is verbosity — deeply nested descriptions are the known complaint
about this style. Mitigate with convenience constructors and good defaults,
**never** with a privileged escape hatch. `Host` is the one exception, and it
is the same escape hatch user code has: a render object with custom layout,
paint and hit-testing.

## 12. Layers

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
  interleave with content outside it. There is no global z scale to
  co-ordinate.
- **Modality**: `Inert` marks everything outside the subtree non-interactive;
  `Exclusive` additionally cuts host raw input (PTY). Pointer, keyboard, focus
  traversal, hover and cursor visibility all derive from `inert` — one
  property, not five encodings.
- **Dismissal is declarative.** `OUTSIDE_POINTER` is an ancestor test the tree
  already supports.

## 13. The plugin boundary

A plugin sends a whole description tree and the host applies it between
frames. That *is* layer 1 crossing a wire.

- **The reconciler lives host-side.** Plugins send descriptions; the host
  reconciles them against persistent elements, so plugin-visible state (list
  scroll, tree expansion, selection) lives on elements rather than in a host
  side table.
- **Callbacks are by name** across the wire (`on: { activate: "open" }`)
  rather than by reference.
- **Keys are required** for keyed builders in the TypeScript API.
- **The plugin vocabulary is a stable subset** of the internal one. It is
  versioned public API with a `.d.ts`, so the internal vocabulary must be able
  to grow without ratcheting it. Not exposed: `Host`, `Modality::Exclusive`,
  focus policies, arbitrary `M`.

---

## 14. Examples

### 14.1 Menu bar and dropdown

```rust
struct MenuBar { menus: Rc<[Menu]> }

impl Component<Action> for MenuBar {
    type State = MenuState;                       // open index, submenu path

    fn build(&self, s: &MenuState, cx: &mut BuildCx<Action>) -> Node<Action> {
        Box::row().h(1).children(self.menus.iter().enumerate().map(|(i, m)| {
            Focusable::new(Gesture::new(Box::pad(1, 0).child(TextRun::new(&m.title)))
                .on_click(cx.handler(move |_| Msg::Toggle(i)))
                .on_enter(cx.handler(move |_| Msg::Hover(i))))   // auto-switch while open
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
        .child(List::keyed(m.items, |it| Key::from(it.id), item_row)
            .autofocus()
            .on_activate(cx.emit(move |e| Action::RunMenuItem(i, e.index))))
}
```

Which menu is open is element state, so a rebuild does not close it. Submenus
nest as further layers anchored to their row, arbitrarily deep, with no new
precedence rules.

### 14.2 Split grid

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

The separator captures the pointer on press, so the drag keeps routing to it
even when the cursor crosses a full-screen terminal. Buffer content is a
`Host` leaf on its own renderer.

### 14.3 Context menu

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

The menu is a child of the thing it acts on, so its target is structural
rather than carried in menu state. Every context menu in the app is this
shape with a different item list.

### 14.4 Command palette

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
                        .selected_id(s.selected)          // an id, never an index
                        .on_select(cx.set_state(|s, id| s.selected = Some(id)))
                        .on_activate(cx.emit(|e| Action::PromptConfirmAt(e.id))),
                    preview(self).flex(1).if_(self.overlay),
                ]),
            ])))
    }
}
```

`FocusScope` contains Tab cycling to the prompt. Selection is element state, so
refreshing the result list does not disturb it — and it stores the selected
*id*, not an index, so a shrinking result list cannot leave it dangling. The
query is controlled: the app owns the value (it is committed and pushed to
history), the element owns only the caret, selection and scroll around it.

Storing identity rather than position is the general form of the rule below:
**validate stored references against the current description at every read —
in `build()` and in handlers alike.** Storing an id dissolves the staleness
class instead of guarding against it.

### 14.5 Transient popup

```rust
Layer::new()
    .anchor(Anchor::Point(h.at)).place(Place::Below).fit(FLIP | CLAMP)
    .dismiss(ANY_KEY | OUTSIDE_POINTER)
    .child(Viewport::new(TextRun::markdown(&h.body)).selectable().max_h(20))
```

Non-modal, so the buffer keeps focus and keeps receiving keys; `ANY_KEY`
dismissal is a root-level observer installed by the layer while it is mounted,
so it fires even when a modal above consumes the key.

### 14.6 Modal dialog

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

`Modality::Exclusive` covers pointer, keyboard, hover, cursor and host raw
input in one property.

---

## 15. Build order

Not negotiable: steps 1–2 define the framework's semantics and every later
decision is downstream. A mistake in the renderer is a rewrite of one module;
a mistake in the reconciler is a rewrite of everything.

1. **The reconciler, against a fake renderer** that logs create / update /
   dispose — including the identity short-circuit (§6) and **transactional
   subtree reconcile**: tree mutations for a subtree are buffered or unwound so
   a failure part-way leaves the last committed content intact. Three later
   rules depend on that one capability (the error policy, deferred disposal,
   and constructor unwinding), so it belongs here rather than being discovered
   three times.
2. **The dirty-marking scheduler**, with the re-entrancy assertion and the
   frozen-tree invariant (§7). Test: dirty a parent and a child in one tick,
   assert exactly one build each.
3. **`register` and teardown fan-out**, child-first — every later primitive is
   a behavior, so this is the substrate they all sit on.
4. **Ambient values** (§7) — needed before any real surface, because without
   them theme and locale are prop-drilled into every signature and the
   signatures are painful to change later.
5. **Diagnostics** — element dump with type, key and state; rebuild counters;
   last-dirty cause (which `set_state` site, which ambient, or "parent").
   Deliberately early: devtools retrofitted onto a reconciler are much harder
   than devtools designed in, and this is the inspectability the three-tree
   split was sold on.
6. **Layout**, once the constraint model (§8.1) is settled on paper, with the
   awkward cases written out (a list with `Auto` height inside a flex column
   inside a modal). Then the geometry assert and `LayoutReader` (§8.2).
7. **Hit-testing and propagation.**
8. **`Tasks`**, with its two delivery guarantees (§7).
9. **Focus, last** — it depends on everything above, and it is where you find
   out whether the retained tree really persists the way you think it does.

## 16. Risks and open decisions

1. **The constraint model** (§8.1) is the least reversible choice here. Settle
   it before step 3.
2. **Paint-time compositing** — layers over host content need clipping and z
   in the paint backend; nothing visual works without it.
3. **Disposal discipline.** Every element and render object needs a defined
   disposal path: focus deregistration, host handles, captured pointers. Leaks
   are invisible until they are a bug report about a ghost focus target.
4. **Intrinsic sizing cost.** `Auto` is the ergonomic default people will
   reach for; make the expensive cases loud.
5. **Verbosity of composition-only** (§11). Budget for a convenience layer, and
   hold the line against privileged primitives when it gets tempting.
6. **The identity skip is weaker here than in Flutter.** `Shared` costs an `Rc`
   and a field, where a `const` widget costs nothing, so hoisting will not be
   reached for as reflexively. If description churn shows up in profiles, the
   fix is pushing state down and leaning on ambients — not a structural-equality
   escape hatch, which would reintroduce the hidden cost this design excludes.
7. **Transactional reconcile is a real implementation constraint**, not a
   detail of the error policy (§15, step 1). Design the reconciler for it from
   the start; bolting it on afterwards means revisiting every mutation site.
8. **Surface area.** The state-class contract stayed at three members and the
   lifecycle stayed construct / build / teardown — but the framework now ships
   `Ambient`/`Provide`, `Tasks`, `Ticker`, `Controller`/`Anchor`, `Persisted`,
   `Focusable`, `Cache` and `LayoutReader`. Each needs semantics, teardown
   behavior and tests. That they are all behaviors or ordinary descriptions is
   evidence the load-bearing walls are right; it does not make them free.

---

# Appendix A — What this replaces in Fresh today

Mapping from current mechanisms to the primitive that subsumes them.

| Today | Here |
|---|---|
| `WidgetInstanceState` map keyed by widget key | element state (§7) |
| focus re-seeded from a spec walk each render | focus registration on the render object, surviving reconciliation |
| optional `key` on `WidgetSpec` | required key functions on keyed builders |
| full-frame guard boxes (close / dismiss / blur / clear) | `Layer.dismiss` + ancestor test |
| `layer_rank` table, hand-assigned box `z` | tree position + stacking contexts + `Modality` |
| `capture_mouse` | `Modality::Exclusive` |
| `blocks_terminal_input`, `modal_overlay_active`, cursor suppression | derived from `inert` / `HostLeaf::raw_input` |
| `KeyContext` / `KeyScope` | focus chain + Shortcuts → Intents → Actions |
| `PointerGrab` enum + grab-keyed drag/release routing | per-node `capture_pointer` |
| `Disposition::{Consumed, PassAfter, Pass}` | `Flow` + not-stopping |
| `pointer_opaque` opt-in | `PointerMode::Opaque` default |
| keyboard pre-band stages | root-scope listeners / layer-declared observers |
| `HitArea` / `row_target` / `owner()` | `target` vs `currentTarget` on the propagation path |
| `PanelPopup`, `screen_space` boxes, paint-recorded rects | `Layer` + layout-pass geometry |
| chrome layout cache written during paint | layout-pass output, one geometry source |
| `Scene`'s hand-written `*_view` methods | `LayoutSpec` |
| residual per-kind matches | composition + render-object capabilities |
| rebuild-everything-per-frame | mark dirty, flush once, depth-ordered |

### Where today's types land in the three trees

| Today | Layer | Note |
|---|---|---|
| `WidgetSpec`, chrome `collect()` boxes | 1 | Already description-shaped. |
| `WidgetInstanceState` map, `PanelState` | 2 | Moves onto elements; the map goes away. |
| `LayoutBox` arena, `ChromeLayout` caches, paint-recorded rects | 3 | Becomes render-object fields computed by the layout pass. |
| `WidgetImpl` | split | `collect` → build or paint by kind; `on_*` → render-object event handling. |

### The side-table problem specifically

Fresh currently recovers per-widget persistence by looking up a widget key in
a `HashMap`. That is the ID-side-table pattern: identity is implicit,
collisions are silent, and renaming a key resets state with no diagnostic. A
retained tree exists precisely to make identity explicit and inspectable —
this is the single largest behavioral change the design buys, and §3 and §7
are where it lands.

### Per-surface

- **Menu bar** — replaces the close-guard box, the dropdown's z number, the
  rank-table entry, the central hover auto-switch machine and the menu's own
  key dispatcher.
- **Split grid** — replaces the 13-variant grab enum and its hand-ordered
  derivation, plus the wheel "floor" arm (an unclaimed wheel just reaches the
  root).
- **Context menus** — replaces menu state carrying its own target, the
  pre-band keyboard grab, and the "right-click inside an open menu" arm.
- **Command palette** — replaces the overlay toolbar focus ring, the click
  scrim box, the position-blind wheel box, the `SearchPrompt`-vs-`Prompt`
  context switch, and the manual-scroll latch that exists to stop a result
  refresh from disturbing scroll and selection.
- **Transient popups** — replaces the transient-dismiss pre-band stage.
- **Modals** — replaces whole-channel mouse capture, the rank entry,
  `blocks_terminal_input`, the hover-suppression list and the
  cursor-suppression list.

# Appendix B — Adoption notes

**Prerequisite work.** Paint-time compositing (layers over host content) gates
anything visual. The constraint model must be settled before layout. Moving
from paint-recorded to layout-recorded geometry is the risky refactor — it
touches every hit-test and the Scene projection; keep the existing parity
oracle running through it. Freeze the plugin-visible subset before internal
vocabulary spreads into it.

**Adoption order**, cheapest first: context menus → menu dropdowns → info
popups → file browser → status bar → prompt. Splits and terminals stay `Host`
leaves permanently. Settings and the keybinding editor last.

**Tests.** The e2e suite asserts on rendered cells, so hold cell output
byte-identical through the layout/paint migration and add `LayoutSpec`-level
assertions as the *new* tests rather than rewriting the old ones.

**Context.** This doc follows
[`widget-framework-v2-review.md`](widget-framework-v2-review.md) (the panel
side: per-kind dispatch, the `LayoutBox` arena, hit-path propagation) and
[`chrome-event-model-plan.md`](chrome-event-model-plan.md) (chrome surfaces
behind one pointer walk and one keyboard walk). Both are prerequisites in
understanding, not in code.
