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
3. **Rebuilding a description costs one allocation per node.** No registration,
   no resource acquisition, no side effects. This removes the incentive to
   avoid rebuilds and to mutate render state directly.
4. **Identity is explicit.** Which on-screen object corresponds to which one in
   the previous frame is determined by tree position and an author-supplied
   key, both inspectable, rather than by hashing an identifier stack into a
   side table.
5. **One source of geometry.** Layout computes rects; hit-testing, painting,
   tests and the web projection read them. Geometry is not derived during
   paint.
6. **Composition is the only extension mechanism.** The primitives available to
   library code are the primitives available to user code.
7. **Backend independence.** Paint produces a display list. TUI cells, the web
   DOM and test assertions are consumers of that list rather than separate
   implementations.

## 2. Non-goals

- **Buffer and terminal text rendering.** The token IR → `ViewLine` pipeline,
  syntax highlighting, folding, wrapping and PTY grids stay as they are,
  reached through a `Host` leaf.
- **A reactive dependency graph.** No observers, no signals, no automatic
  invalidation tracking. State flows down through constructor arguments;
  events flow up through explicit callbacks.
- **Preventing update cycles.** A handler can mark an ancestor dirty, which
  rebuilds and re-invokes the handler. The design does not prevent this; it
  constrains it to a call chain that appears in a stack trace, rather than an
  emergent property of a dependency graph.
- **Cell-level damage tracking.** crossterm diffs the back buffer already.
- **An animation system.** The existing frame-buffer animation runner is
  unchanged.
- **Mutable objects at the description layer.** Descriptions are values; only
  the framework retains anything across frames.

---

## 3. The model — three trees

The design separates the description of the UI, which is cheap to construct
and discard, from the objects that hold state, which are not.

| | What it is | Lifetime | Holds |
|---|---|---|---|
| **1. Description** `Node<M>` | An immutable recipe. `Button { label, on_press }` *describes* a button; it is not one. | One rebuild, then garbage. | Props supplied by the parent. Nothing durable. |
| **2. Element** `Element` | Identity. Knows which description produced it, its children, its local state, its render object. | The lifetime of the logical component. | `(type, key)`, component state, child links, dirty flag, depth. |
| **3. Render object** `dyn RenderObject` | Computed and retained data. | Created rarely, mutated constantly, disposed when the component genuinely goes away. | Computed geometry, cached measurements, paint state, focus registration, host handles. |

Rebuilding layer 1 allocates and discards descriptions. Layer 3 holds the
computed data and is mutated only where an input changed.

**Invariant:** descriptions carry no geometry. A description type with a rect
field indicates layers 1 and 3 have been merged.

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

Position is the implicit key when none is given, so unkeyed static subtrees
require no key annotations.

Two consequences of the rule:

- **Changing a key remounts the subtree.** This is the supported way to reset
  state: re-keying the prompt on `prompt_type` clears its editing state without
  a separate reset path.
- **A type change always remounts.** There is no partial-match rule for
  similar types.

**Keys cannot be inferred.** Given a reordered list, no algorithm can
distinguish "the same item at a new position" from "a different item at the
same position"; that distinction exists only in the domain model. Keyed
builders therefore take a key function as a *required* argument.

```rust
List::keyed(items, |it| Key::from(it.id), |it| row(it))   // key fn required
```

### Rebuild propagation — the identity short-circuit

When a matched child's new description is **the same instance** the element
already holds, the subtree is skipped entirely. Otherwise the description is
swapped and the subtree rebuilds.

Reference identity is the only skip rule. Structural equality does not hold
across rebuilds once handlers are present: descriptions are reconstructed each
time, so closure identity always differs. No framework mechanism compares
handler identity, and components are forbidden from doing so (§16). Deep
comparison is excluded because its cost is proportional to subtree size and is
not visible at the call site. Identity comparison is O(1).

Skipping is therefore controlled by the author: return the same instance, via
`Desc::Shared(Rc<Node<M>>)` held in a field, or interned at a boundary (§13).
Absent that, a `set_state` near the root reconstructs every description below
it. Three mechanisms bound the cost:

1. **push state down**, so dirty elements are deep and their subtrees small;
2. **ambient values dirty their dependents directly** (§7), so broadcast data
   like a theme change never needs a root rebuild;
3. **hoist invariant subtrees** into `Shared`.

The ordering is specific to Rust. Where descriptions can be compile-time
constants, (3) is the primary mechanism; here `Shared` requires an `Rc`
allocation and a field to hold it, so (1) and (2) apply more often. Note also
that reconstructing a description does not imply layout or paint work: layer
3's dirty flags skip unchanged output. These three mechanisms are the only ones
that reduce description reconstruction.

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

Passing a theme, locale or service handle explicitly through every intermediate
component is impractical at this scale, and a reactive dependency graph is
excluded by §2. One primitive covers the case:

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
but the constructor does not re-run, so a value cached in a field from it is
not updated when the ambient changes, and no error is reported. Values that
must track an ambient are read in `build()`. Without this rule the design
would require a post-change hook equivalent to `didChangeDependencies`.

### Behaviors and teardown

Reusable stateful concerns are objects held in named fields and enrolled for
teardown:

```rust
let tasks    = cx.register(Tasks::new());
let ticker   = cx.register(Ticker::new(on_tick));
let expanded = cx.register(Persisted::new("expanded", HashSet::new()));
```

The shipped set: **`Tasks`** (async), **`Ticker`** (per-frame callbacks),
**`Cache`** (the memo carve-out, §8.4), **`Controller`** / **`Anchor`**
(imperative commands), **`Focusable`** (§10), **`Persisted`** (rehydration).
`Drop` handles teardown in most cases; `register` is required where teardown
must run in a defined order, or before the tree is dismantled.

**Async.** `Tasks` provides two guarantees, which together remove the need for
an `is_mounted` flag: results are delivered on the UI scheduler between frames,
never concurrently with build, layout or paint; and delivery does not occur
after teardown. `launch_replacing(tag, …)` cancels any prior launch under the
same tag. Other race semantics are the caller's responsibility, validated at
delivery time.

**Imperative commands.** A `Controller` is constructed by the owner, passed
down in a description, and bound by the child at construction. Commands are
method calls forwarded to the bound state. Scope is restricted: **a command may
touch only the target's local state.** A command that changes controlled state
violates the data-flow rule in §7. `Anchor` is the same mechanism with the
framework as registrar, allowing host code to address a mounted surface by
handle. Neither is an ID side table: binding is explicit, state remains on the
element, and the registry maps handle → element rather than identity → state.

**Persistence.** `Persisted<T>` reads from a host store at construction and
writes at teardown, scoped by a `PersistenceScope` ambient so keys are anchored
to document or route ids rather than tree position. Unmount still destroys the
state object; the value is restored at the next construction. Scope: **new
incidental state only.** Existing serialized view state remains app state; see
the implementation plan for the reason.

### Controlled and uncontrolled

Element-owned is the default, not the only option. Some state must outlive the
element, or be readable by something other than the component that renders it:
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

- **Depth ordering.** If a parent and a child are both dirty, the parent
  rebuilds first and may reconcile the child as part of that pass. No element
  rebuilds twice per flush, and no element rebuilds immediately before its
  parent disposes it.
- **Re-entrancy is an error.** `set_state` during build panics, naming the
  element. Without the assertion, the resulting stale-state failures have no
  direct symptom at the point of the error.

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

This is the decision with the widest downstream effect: layout API shape, error
reporting and layout cost all follow from it, and changing it later invalidates
every `RenderObject` implementation.

Choice: **box constraints in integer cells.** Constraints (`min_w`, `max_w`,
`min_h`, `max_h`) propagate down; sizes propagate up; parents position
children. Single pass, no constraint solver, no sub-cell rounding.

- `Sizing::{Cells(n), Flex(w), Pct(p), Auto}` resolve into constraints.
- **Intrinsic sizing is opt-in and measures a subtree twice.** `Auto` over a
  large subtree is the case where this cost appears; document it at the API.
- A common case requiring a second layout pass indicates a defect in the
  constraint model rather than a performance problem.

### 8.2 Layout pass

Walks the render tree in tree order, honoring dirty flags. `needs_layout`
propagates **up to the nearest relayout boundary** — a node whose own size
cannot change as a result (a fixed-size box, a viewport with a fixed rect). A
status-bar text change then relayouts the status bar, not the split grid.

Output per node: `rect`, `clip`, `scroll window`. This is the **only** source
of geometry.

**Geometry accessors are rejected during `build()`**, by debug assert. If
`build()` read layout output, build would depend on layout, which depends on
build: either a cycle, or a fixed one-frame lag. `build()` is a function of the
description, the state fields and ambient values.

The element handle is split so that the validity window is expressed in the
type rather than in documentation:

- `cx.services` — scheduler, focus, ambients, anchor registration. Valid from
  construction.
- `cx.geometry` — rect, size, scroll window. Valid only after first layout, and
  never inside `build()`.

For structure that depends on incoming constraints,
`LayoutReader(|constraints| -> Node<M>)` is a description whose builder runs
during the layout pass, receiving constraints as an argument. The dependency is
then scoped to that node and evaluated in the correct pass. Post-layout
geometry remains available in event handlers, tickers and task callbacks.

`LayoutReader` is the only case where build runs during layout: `set_state` is
rejected in its builder, and under intrinsic sizing it may run more than once
per frame with different constraints.

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

The framework may call `build()` any number of times without committing the
result: intrinsic measurement, tests, offscreen rendering. Component code must
not depend on the number of calls.

Local memoization is compatible with this, but requires an exemption from the
purity assertion: a `Cache<T>` field is excluded from the mutation check, under
the contract that **writes are idempotent functions of the build inputs** —
equal inputs produce equal values. All other fields remain unchanged across
`build()`.

### 8.5 Boundaries

Dirty flags are per pass and per node. For a terminal frontend specifically:
`needs_layout` boundaries are required, while repaint boundaries are an
optimization that can be deferred — the display list for one screen is small,
and crossterm diffs cells already. Measure before implementing them.

## 9. Pointer: hit-testing and propagation

Hit-testing walks the **render tree** back-to-front, collecting the path from
the deepest hit node to the root. The path is derived from retained geometry,
so it accounts for overlapping and transformed content, which a walk over
descriptions cannot.

```
path   = hit_test(point)          // deepest -> root
capture: root   -> target         // each node may claim
target
bubble : target -> root           // each node may claim
```

| Concern | Mechanism |
|---|---|
| claim | `Flow::Stop`. There is no third disposition: acting without claiming is expressed by not stopping. |
| default behavior | `cx.prevent_default()`, orthogonal to claiming. |
| opacity | `PointerMode::Opaque` is the default; `Transparent` passes through after its own handlers; `Ignore` is not hittable. |
| drag | `cx.capture_pointer()`, per node. Subsequent moves and releases route to that node regardless of pointer position, until release or unmount. This is the complete drag mechanism. |
| scroll chaining | Wheel events bubble; a `Viewport` claims one only if its offset changed. |

## 10. Focus

Focusable render objects register into a **focus tree** that mirrors — but is
not identical to — the render tree.

```rust
pub struct FocusNode { scope: bool, ordinal: Option<i32>, skip: bool }

pub trait TraversalPolicy {
    fn next(&self, scope: &FocusScope, from: FocusId, dir: Dir) -> Option<FocusId>;
}
```

- **Scopes** group focus nodes; a modal `Layer` opens a scope and confines
  traversal to it.
- **The traversal policy is pluggable**: reading order (default), explicit
  ordinals, and directional 2D traversal. Forms use reading order; the split
  grid uses directional.
- **Keyboard events propagate along the focus chain, not the hit chain.** A key
  pressed in a text field is offered to the field, then to its focus ancestors,
  up to the root.
- **Focus is preserved across reconciliation**, because registration is held by
  the render object and reconciliation preserves matched elements. This should
  be covered by a test early, since it depends on the retained tree behaving as
  specified.
- **Registration is a behavior**: `cx.register(Focusable::new(on_focus_change))`.
  `request_focus()` is an imperative command, permitted in handlers and
  rejected in `build()`. A component that renders a focus indicator mirrors the
  state via `on_focus_change`; focus itself is not component state.

### Shortcuts → Intents → Actions

```
key chord --[ Shortcuts map on the focus chain ]--> Intent   (what the user meant)
Intent    --[ Actions   map on the focus chain ]--> handler  (how THIS part does it)
```

The same `Intent::Cancel` resolves to different actions depending on focus
position: a prompt cancels itself, a modal closes, a buffer clears its
selection. No central context enum or precedence table is involved.

## 11. Composition and primitives

There is one extension mechanism. No primitive has capabilities unavailable to
user code.

**Primitives** — the only descriptions with render objects — are those that
cannot be expressed as compositions:

```
Box   TextRun   Viewport   Gesture   Focusable   Layer   Host
```

**All other widgets are `Component`s**, including those that would
conventionally be built in:

```rust
Button   = Focusable(Gesture(Box(TextRun)))
Toggle   = Focusable(Gesture(Box([TextRun(mark), TextRun(label)])))
List     = Viewport(Box::col(items.map(row)))          // + selection state
Dropdown = Component { Button, and a Layer when open }
```

The cost of this rule is verbosity: description trees nest deeply. The
mitigation is convenience constructors and defaults, not a privileged
primitive. `Host` is the single exception, and it is available to user code on
the same terms: a render object with custom layout, paint and hit-testing.

## 12. Layers

A `Layer` is an ordinary child in the description tree, so it inherits
containment, focus scoping, propagation and its owner's identity. Its render
object lays out and paints out of flow.

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

- **Stacking**: a `Layer` opens a stacking context, so its descendants do not
  interleave with content outside it. There is no global z scale requiring
  coordination between surfaces.
- **Modality**: `Inert` marks everything outside the subtree non-interactive;
  `Exclusive` additionally suppresses host raw input (PTY). Pointer routing,
  keyboard routing, focus traversal, hover and cursor visibility are all
  derived from `inert`, rather than from separate per-concern flags.
- **Dismissal is declarative.** `OUTSIDE_POINTER` is evaluated as an ancestor
  test over the existing tree.

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
  versioned public API with a `.d.ts`; keeping it a subset allows the internal
  vocabulary to change without a corresponding change to plugin API. Not
  exposed: `Host`, `Modality::Exclusive`, focus policies, arbitrary `M`.

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
nest as further layers anchored to their row, to arbitrary depth, without
additional precedence rules.

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

The separator captures the pointer on press, so subsequent motion routes to it
regardless of what the cursor passes over, including a full-screen terminal.
Buffer content is a `Host` leaf using its own renderer.

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

The menu is a child of the node it acts on, so its target is determined by tree
position rather than stored in menu state. Each context menu in the editor has
this structure with a different item list.

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

Non-modal, so the buffer retains focus and continues receiving keys. `ANY_KEY`
dismissal is implemented as a root-level observer installed by the layer while
mounted, so it runs even when a modal above the popup claims the key.

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

`Modality::Exclusive` covers pointer routing, keyboard routing, hover, cursor
visibility and host raw input as a single property.

---

## 15. Build order

The order is constrained: steps 1–2 fix the framework's semantics, and later
steps depend on them. An error in the renderer is confined to one module; an
error in the reconciler affects every component built on it.

1. **The reconciler, against a fake renderer** that logs create / update /
   dispose — including the identity short-circuit (§6) and **transactional
   subtree reconcile**: tree mutations for a subtree are buffered or unwound so
   that a failure part-way through leaves the last committed content intact.
   Three later rules require this capability — the error policy, deferred
   disposal, and constructor unwinding — so it is implemented here rather than
   added afterwards.
2. **The dirty-marking scheduler**, with the re-entrancy assertion and the
   frozen-tree invariant (§7). Test: dirty a parent and a child in one tick,
   assert exactly one build each.
3. **`register` and teardown fan-out**, child-first. Every later primitive is a
   behavior, so this is a prerequisite for all of them.
4. **Ambient values** (§7). Required before any surface is built: without them,
   theme and locale appear in every intermediate component signature, and
   signatures are costly to change once components exist.
5. **Diagnostics** — element dump with type, key and state; rebuild counters;
   last-dirty cause (`set_state` site, ambient, or parent rebuild). Placed
   early because adding introspection to a reconciler after the fact requires
   changes at every mutation site.
6. **Layout**, once the constraint model (§8.1) is specified, including the
   cases listed there (a list with `Auto` height inside a flex column inside a
   modal). Then the geometry assert and `LayoutReader` (§8.2).
7. **Hit-testing and propagation.**
8. **`Tasks`**, with the two delivery guarantees in §7.
9. **Focus, last.** It depends on all of the above, and it is the first
   component of the system that fails if the retained tree does not preserve
   elements as specified.

## 16. Risks and open decisions

1. **The constraint model** (§8.1) has the widest downstream effect and the
   highest cost to change. Specify it before step 6.
2. **Paint-time compositing.** Layers over host content require clipping and z
   ordering in the paint backend. No layer-based surface renders without it.
3. **Disposal.** Every element and render object requires a defined disposal
   path covering focus deregistration, host handles and captured pointers. A
   missed path produces a retained focus target or handle with no immediate
   symptom.
4. **Intrinsic sizing cost.** `Auto` is the default authors will select; the
   cases where it measures a large subtree twice need to be identifiable at the
   API and in diagnostics.
5. **Verbosity** (§11). A convenience-constructor layer is required; the
   alternative of adding privileged primitives is excluded by goal 6.
6. **The identity skip is less effective here than in Flutter.** `Shared`
   requires an `Rc` allocation and a field; a `const` widget requires neither.
   If description reconstruction appears in profiles, the available responses
   are pushing state down and using ambients. Structural equality is excluded
   for the reason in §6.
7. **Transactional reconcile** (§15, step 1) is a constraint on the reconciler
   rather than a property of the error policy. Adding it later requires
   revisiting every tree-mutation site.
8. **Surface area.** The state-class contract is three members and the
   lifecycle is construct / build / teardown, but the framework also defines
   `Ambient`/`Provide`, `Tasks`, `Ticker`, `Controller`/`Anchor`, `Persisted`,
   `Focusable`, `Cache` and `LayoutReader`. Each requires specified semantics,
   teardown behavior and tests.

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

Fresh currently recovers per-widget persistence by looking up a widget key in a
`HashMap`. In that arrangement identity is implicit: key collisions are not
reported, and renaming a key discards the associated state without a
diagnostic. The retained tree makes identity explicit and inspectable. This is
the largest behavioral difference between the two models; §3 and §7 specify
it.

### Per-surface

- **Menu bar** — replaces the close-guard box, the dropdown's z number, the
  rank-table entry, the central hover auto-switch machine and the menu's own
  key dispatcher.
- **Split grid** — replaces the 13-variant grab enum and its ordered
  derivation, and the wheel floor arm (an unclaimed wheel reaches the root and
  is discarded there).
- **Context menus** — replaces menu state carrying its own target, the
  pre-band keyboard grab, and the "right-click inside an open menu" arm.
- **Command palette** — replaces the overlay toolbar focus ring, the click
  scrim box, the position-blind wheel box, the `SearchPrompt`/`Prompt` context
  switch, and the manual-scroll latch that prevents a result refresh from
  altering scroll and selection.
- **Transient popups** — replaces the transient-dismiss pre-band stage.
- **Modals** — replaces whole-channel mouse capture, the rank entry,
  `blocks_terminal_input`, the hover-suppression list and the
  cursor-suppression list.

# Appendix B — Adoption notes

**Prerequisite work.** Paint-time compositing (layers over host content) is
required before any layer-based surface renders. The constraint model must be
specified before layout. Moving from paint-recorded to layout-recorded geometry
affects every hit-test and the Scene projection; the existing parity oracle
should remain enabled throughout that change. The plugin-visible subset should
be fixed before internal vocabulary is used at the boundary.

**Adoption order**, by increasing cost: context menus → menu dropdowns → info
popups → file browser → status bar → prompt. Splits and terminals remain `Host`
leaves. Settings and the keybinding editor last.

**Tests.** The e2e suite asserts on rendered cells. Cell output should remain
byte-identical through the layout and paint migration, with `LayoutSpec`-level
assertions added as new tests rather than replacing the existing ones.

**Context.** This doc follows
[`widget-framework-v2-review.md`](widget-framework-v2-review.md) (the panel
side: per-kind dispatch, the `LayoutBox` arena, hit-path propagation) and
[`chrome-event-model-plan.md`](chrome-event-model-plan.md) (chrome surfaces
behind one pointer walk and one keyboard walk). Both are prerequisites in
understanding, not in code.
