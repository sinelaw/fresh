# fresh-ui

A retained, reconciling UI library with no backend, no terminal dependency and
no editor dependency. It builds a tree of immutable descriptions, matches them
against a persistent element tree, and emits a flat display list. Something else
draws that list — a terminal, the web DOM, or a test.

The same library is meant to build both the editor's own chrome (menus, prompt,
splits, status bar, modals) and the panels plugins contribute. There is no
privileged internal surface.

Its one runtime dependency is `unicode-width`.

## Why

Editor chrome and plugin panels had drifted into two hand-maintained systems:
per-surface dispatch tables, a central precedence list authors edited to add a
menu, geometry recomputed during paint, focus and modality decided by special
cases. This library replaces that with one set of primitives and a small number
of general rules.

The goals, in full:

1. **One library for everything on screen.** Menus, prompt, popups, context
   menus, dock, splits, modals, status bar and plugin panels are built from the
   same primitives with the same event and layout rules.
2. **Generic registration and propagation, with no hand-specified exceptions.**
   Precedence, modality, dismissal and focus order are *derived* from structure
   and declared properties, never read from a central ordered list.
3. **Rebuilding a description costs one allocation per node.** No registration,
   no resource acquisition, no side effects — so there is no incentive to avoid
   rebuilds or to mutate retained state directly.
4. **Identity is explicit.** Which on-screen object is which across frames is
   decided by tree position and an author-supplied key, both inspectable —
   never by hashing an identifier stack into a side table.
5. **One source of geometry.** Layout computes rectangles; hit-testing,
   painting and tests read them. Geometry is not derived during paint.
6. **Composition is the only extension mechanism.** The primitives available to
   library code are the primitives available to user code.
7. **Backend independence.** Paint produces a display list; TUI cells, the web
   DOM and test assertions are consumers of that list.

Explicit non-goals include a reactive dependency graph (no signals or
observers — state flows down through constructor arguments, events flow up
through callbacks), cell-level damage tracking, an animation system, mutable
descriptions, and sub-cell precision. The full list is in the design doc.

## The model — three trees

Work is split across three trees, in order of cost:

1. **Descriptions** (`Node<M>` / `Desc<M>`) — immutable values, rebuilt freely,
   with no identity and no side effects. Cheap to produce and discard.
2. **Elements** — the persistent instances descriptions are matched against by
   `(type, key)` at a position. They own component state and survive a rebuild;
   an element is what a key identifies.
3. **Render objects** (`dyn RenderObject`) — the retained, expensive data:
   computed geometry, cached measurements, scroll offsets, focus registration,
   host handles. Created rarely, mutated constantly, disposed when a component
   genuinely goes away.

Reconciliation matches a new description to an existing element by `(type, key)`
at each position, with an identity short-circuit on `Rc::ptr_eq` for shared
subtrees. The render tree skips nodes with no geometry of their own, so a
component, a provider or a shared wrapper contributes identity or data but not a
rectangle.

A frame is: reconcile the description tree into the element tree, lay out the
render tree (constraints down, sizes up, parents place children), and fold the
result into a display list. Input runs the other way: hit-test the render tree,
then capture → target → bubble along the resulting path.

## Subsystems

| Area | What it does | Source |
|------|--------------|--------|
| Descriptions | `Node` / `Desc`, the primitive set, and the builder DSL | [`src/desc.rs`](src/desc.rs) |
| Elements & reconcile | the persistent tree, `(type, key)` matching, transactional subtree reconcile, deferred disposal | [`src/element.rs`](src/element.rs) |
| Scheduling | mark-and-flush, depth-ordered drain, the frozen-tree-between-flushes invariant, re-entrancy guard | [`src/schedule.rs`](src/schedule.rs) |
| Layout | box constraints in integer cells, relayout boundaries, the constraint cache, incremental relink/arrange | [`src/render/layout.rs`](src/render/layout.rs), [`src/render/geom.rs`](src/render/geom.rs) |
| Render objects | the `RenderObject` trait and the primitives' implementations | [`src/render/object.rs`](src/render/object.rs), [`src/render/prim.rs`](src/render/prim.rs) |
| Display list | the flat, ordered, absolute, keyed `LayoutSpec` seam, and the paint walk | [`src/render/spec.rs`](src/render/spec.rs), [`src/render/paint.rs`](src/render/paint.rs) |
| Pointer | hit-testing, capture/target/bubble, stacked paths, pointer capture, scroll chaining | [`src/hit.rs`](src/hit.rs) |
| Focus | a separate focus tree, reading-order and directional traversal, the Shortcuts → Intents → Actions chain | [`src/focus/`](src/focus/) |
| Events | `Input`, `Event`, `GestureKind`, `Flow`, keys and modifiers | [`src/event.rs`](src/event.rs) |
| Behaviors | `Tasks`, `Ticker`, `Cache`, `Controller`, `Anchor`, `Persisted`, `Focusable` | [`src/behavior/`](src/behavior/) |
| Ambients | `Ambient<T>`, `provide`, dependent tracking | [`src/ambient.rs`](src/ambient.rs) |
| Services / geometry | the validity-window split between what is readable at construction and what is readable after layout | [`src/services.rs`](src/services.rs) |
| Widgets | button, toggle, radio group, number, text field, list, tree, dropdown, menu | [`src/widgets/`](src/widgets/) |
| Diagnostics | element dump with type, key and state; rebuild counters | [`src/diagnose.rs`](src/diagnose.rs) |

## Examples

A complete demo application — menu bar and dropdown, a split with a draggable
divider, a task list, a context menu leading to a modal, a command palette, a
million-row virtual list — lives under [`tests/support/demo/`](tests/support/demo/),
outside the library tree. Two binaries drive it:

```text
cargo run -p fresh-ui --example demo          # a scripted session, printed frame by frame
cargo run -p fresh-ui --example interactive   # the same app, live in a real terminal (Ctrl-Q to quit)
```

The interactive backend ([`examples/interactive.rs`](examples/interactive.rs))
folds the display list into coloured cells with crossterm and translates its key
and mouse events into the library's own `Input`. It is an ordinary consumer of
the display list — the shape a real host renderer takes — and depends on
crossterm only as a dev-dependency, so the library itself keeps its single
runtime dependency.

## Tests

```text
cargo test -p fresh-ui
```

Coverage is per-subsystem unit tests, golden functional tests over the demo
application (re-record with `UPDATE_GOLDEN=1`), property tests (`proptest`), and
a conformance suite. The test-support backends — a character-grid renderer and a
recording renderer — are under [`tests/support/`](tests/support/), shared by the
test binaries and the examples.

## Further reading

- [`docs/internal/widget-library-design.md`](../../docs/internal/widget-library-design.md)
  — the design specification: goals, the three trees, layout, focus, layers,
  events, state and effects, and worked examples. The authority on architecture.
- [`docs/internal/widget-library-implementation-plan.md`](../../docs/internal/widget-library-implementation-plan.md)
  — the build plan, the deviation registers recording where the code diverged
  from the design and how each divergence was closed, and the migration plan.
