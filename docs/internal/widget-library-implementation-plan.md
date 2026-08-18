# Widget Library — Implementation Plan

> _AI-generated plan. **Part 1 has started; Part 2 is PLANNED.** Phases L0
> **Part 1 is built** in `crates/fresh-ui`: L0 through L7, the demo application,
> and its golden and property tests. It is **not yet faithful to the design
> document** — §3's deviation register lists nine places the implemented model
> differs and ten specified pieces that are not built. Part 1b (§4) closes them.
> Part 2 (the migration) does not start until it does._
>
> How to build [`widget-library-design.md`](widget-library-design.md) as a new
> crate, and how to move every existing UI surface onto it. Two parts, in
> order: **build the library standalone**, then **migrate surface by surface**.

---

## 1. Shape of the work

**Part 1** builds `fresh-ui` as a self-contained crate with no knowledge of
Fresh. It is testable and shippable on its own, against a fake renderer.

**Part 2** migrates surfaces one at a time. Each wave moves one surface onto
the library and **deletes its previous implementation in the same change.**
Retaining both implementations produces two code paths for the same surface,
which is the condition this work is intended to remove.

### What is in scope

Current line counts, as the scale of Part 2:

| Area | Lines | Wave |
|---|---:|---|
| `view/ui/` (menu, status bar, tabs, scrollbar, …) | ~29,300 | M1–M3, M9 |
| `view/settings/` | ~19,800 | M8 |
| `widgets/` (panel framework, 17 kinds) | ~17,000 | M6 |
| `app/render.rs` | ~6,000 | M9 |
| `view/controls/` | ~6,200 | M8 |
| `app/chrome/` (15 components) | ~5,900 | M1–M7 |
| `app/widget_runtime.rs` | ~2,900 | M6 |
| `app/mouse_input.rs` | ~1,100 | M2–M9 |

### What never migrates

Buffer text rendering (token IR → `ViewLine`, highlighting, folding, wrapping)
and terminal grids are unchanged, reached through `Host` leaves. These are the
cases `Host` exists for.

---

## 2. The crate

```
crates/fresh-ui/
  src/
    lib.rs
    key.rs          Key, KeyPath
    desc.rs         Node<M>, Desc<M>, props structs, builders
    component.rs    Component<M>, the type-erased AnyComponent<M>
    element.rs      Element, arena, reconcile (identity skip, transactional)
    schedule.rs     dirty set, flush, re-entrancy guard, frozen-tree invariant
    ambient.rs      Ambient<T>, Provide, dependent lists
    behavior/       register/teardown; Tasks, Ticker, Controller/Anchor,
                    Persisted, Cache
    diagnose.rs     element dump, rebuild counters, last-dirty cause
    render/
      object.rs     RenderObject trait, Geom, Constraints
      layout.rs     layout pass, relayout boundaries
      paint.rs      paint pass, DrawList
      spec.rs       LayoutSpec, Item, Draw
    hit.rs          hit-testing, propagation, pointer capture
    focus/
      tree.rs       FocusNode, FocusScope
      policy.rs     TraversalPolicy: reading order, ordinal, directional
      intent.rs     Shortcuts -> Intent -> Actions
    prim/           Box, TextRun, Viewport, Gesture, Focusable, Layer, Host
    widgets/        Button, Toggle, List, Tree, Dropdown, … (Components)
    test/
      fake.rs       recording renderer: logs create/update/dispose
      harness.rs    drive events, assert on LayoutSpec
```

**Dependencies are restricted to** `unicode-width` for cell measurement and
`bitflags`. Specifically excluded:

- **not `ratatui`** — the library emits a display list; the TUI backend that
  turns `Item`s into cells lives in `fresh-editor`;
- **not `fresh-core`** — `WidgetSpec` is the plugin wire format and stays
  where it is; the spec→`Node` translation lives in `fresh-editor`;
- **not `fresh-editor`** — the dependency runs one way only.

`HostId` is an opaque newtype in `fresh-ui`; `fresh-editor` implements
`HostLeaf` for buffer splits and terminal grids.

Added to `[workspace] members` and `default-members`. `cargo test -p fresh-ui`
must pass with the rest of the workspace absent from the build graph; this is
the check that the crate is in fact independent.

---

## 3. Part 1 — build the library

Each phase ends with tests that fix its semantics. The order is constrained:
L1 and L2 define the framework's semantics and every later phase depends on
them.

**Status.** Part 1 is complete. L0 through L7 are implemented, their exit
criteria are covered by tests in `crates/fresh-ui/tests/`, and a demo
application in `crates/fresh-ui/src/demo/` exercises every capability the
library has. The crate's only dependency is `unicode-width` (`proptest` is a
dev-dependency, for the property tests); it pulls in no other workspace crate,
so `cargo test -p fresh-ui` builds the library and nothing else.

The tests come in three kinds, and they catch different things:

- **Exit-criteria tests**, one per phase, fixing the semantics that phase
  defines.
- **Golden functional tests** (`tests/golden.rs`), which drive the demo through
  a scripted session and compare each frame character for character against a
  recorded screen in `tests/golden/`. Re-record with `UPDATE_GOLDEN=1`. These
  are what catch a change that is locally correct everywhere and wrong when
  assembled, and a diff shows the interface moving rather than a list of
  rectangles changing.
- **Property tests** (`tests/properties.rs`), which state what must hold of
  every session rather than one: the flex division conserves its total and is a
  function of its inputs; children never overlap and never leave their parent;
  layout is reproducible; a key permutation preserves the element set; nothing
  paints outside the frame; the display list stays bounded by the frame rather
  than by the data; focus never points at a disposed element; and a long
  session accumulates no elements.

Three defects were found this way that the exit-criteria tests did not reach: a
`LayoutReader`'s children were disposed and remounted every frame, autofocus
never fired when a modal opened over an already-focused field, and a flow
container could place a child outside its own content box when the gaps alone
exceeded the available space.

Passing every exit criterion is not the same as matching the specification. The
deviation register below is a section-by-section audit of the design document
against the source; closing it is the definition of done for Part 1.

### Deviations from the design document

Every point where the implementation differs from
[`widget-library-design.md`](widget-library-design.md), from a section-by-section
audit of the spec against the source. Each is classified:

| Class | Meaning | Disposition |
|---|---|---|
| **A** | The implemented model differs from the spec. | Close in code — §4. |
| **B** | Specified and not built. | Build — §4. |
| **C** | The code is right and the spec is inconsistent, silent, or unexpressible in Rust. | Correct the spec. |

Class A and B are open. **The design document is the agreement; where it and the
code disagree, the code changes.**

Class C is **decided**: the architecture is what matters and the spelling is not,
so the Rust-legal names stand and the document is corrected to match (R11). No
class-C item changes the model — each is a name Rust rejects, a place the
document contradicts its own examples, or a place it is silent.

Section references below are to the design document unless marked otherwise.

#### A — the model differs

**A1. There is no `RenderObject` trait.** §3 names three trees, the third being
`dyn RenderObject` with `layout` / `paint` / `hit` / `relayout_boundary` (§8).
What exists is a `RenderData` struct on the element, and per-primitive behaviour
written as a `match` on `ElemType` inside `layout_dispatch`, `paint_node` and
`hit_node`. The observable semantics are the same because the correspondence is
one to one, but the layer the spec names does not exist as a layer.

**A2. `Host` is not a user-supplied render object.** A consequence of A1, and the
larger half of it. §11: "`Host` is the single exception, and it is available to
user code on the same terms: a render object with custom layout, paint and
hit-testing." What is implemented is a leaf that fills its constraints and emits
`Draw::Host` for the backend. Application code cannot supply measurement, paint
or hit-testing for anything. Goal 6's escape hatch is missing.

**A3. `Tasks` inverts ownership.** §7: `launch_replacing(tag, …)` launches the
work. The implementation hands the application a `Send` handle and the
application launches. Both delivery guarantees hold, but the call shape in the
spec does not work, and the framework never sees the work.

**A4. The windowed list is not built on `Viewport`.** §11: "`List::virtual` is a
`Component` over `Viewport` … whose `build` reads the viewport's scroll window."
The implementation is a component over `LayoutReader` that owns its own scroll
and never instantiates a `Viewport`, so it inherits neither the viewport's clip
nor its scrollbar, and the scroll window is not the mechanism.

**A5. There is no focus tree.** §4 and §10: "Focusable render objects register
into a focus tree that mirrors — but is not identical to — the render tree."
`focus_scope()` walks the element tree on every query and builds a `FocusScope`
from scratch. Nothing is registered and nothing is retained. Focus survives
reconciliation for a different reason than the spec gives — it names an element
rather than being held by a render object.

**A6. The `Focusable` *behavior* does not exist.** §10: "Registration is a
behavior: `cx.register(Focusable::new(on_focus_change))`." Only the
`Desc::Focusable` primitive exists, so a component cannot become focusable
without wrapping itself in one. Build order step 3 states the general rule this
breaks: "every later primitive is a behavior".

**A7. The context handle is not split.** §8.2 specifies `cx.services` (valid from
construction) and `cx.geometry` (valid only after first layout, never inside
`build`), and gives the reason: "the validity window is expressed in the type
rather than in documentation". The implementation has one context and a runtime
`debug_assert` in `Ui::rect` — the arrangement the spec argues against.

**A8. `PointerMode::Transparent` does not run its handlers.** §9: "`Transparent`
passes through after its own handlers." The implementation makes the node
unhittable, so its handlers never run at all. This needs a hit test that returns
a stacked path rather than one path.

**A9. Layout never measures twice.** §8.1: "Intrinsic sizing is opt-in and
measures a subtree twice", and §17 risk 4 asks for those cases to be
identifiable at the API and in diagnostics. Layout is strictly single-pass:
`Auto` resolves against the incoming loose constraint. Every case in the spec and
every test resolves correctly, but a case that genuinely needs two measurements
is silently wrong rather than slow. `Align::Stretch` applying only when the cross
extent is definite is a rule invented to avoid needing the second pass.

#### B — specified and not built

**B1. Five of the six behaviors.** §7 names the shipped set as `Tasks`,
`Ticker`, `Cache`, `Controller`/`Anchor`, `Focusable`, `Persisted`. Only `Tasks`
exists.

**B2. The speculative-build contract.** §8.4: the framework may call `build()`
any number of times without committing the result, `Cache<T>` is exempted from
the purity check, and all other fields are unchanged across builds. Neither the
carve-out nor the purity check exists, because nothing memoizes yet.

**B3. `Persisted` and `PersistenceScope`.** §7. Absent, so there is no
rehydration path for new incidental state.

**B4. `Controller` / `Anchor`.** §7. Absent, so there is no imperative command
path and no way for host code to address a mounted surface by handle.

**B5. `Ticker`.** §7. Absent, so there is no per-frame callback.

**B6. `LayoutSpec.cursor`.** §8.3 declares it. It is never populated: there is no
text cursor, so a focused field cannot place one.

**B7. `Modality::Exclusive` does not suppress host raw input.** §12 and
Appendix A: `blocks_terminal_input` is meant to be "derived from `inert` /
`HostLeaf::raw_input`". There is no `HostLeaf` and no raw-input concept.

**B8. `DualList`.** Named in L7. Not built.

**B9. `request_focus()` is not rejected in `build()`.** §10 requires the
rejection. It is unreachable from `BuildCx` today, which is not the same thing as
being reported.

**B10. The plugin boundary (§13).** Scheduled as M6 in Part 2; listed here for
completeness, not as a defect.

#### C — proposed spec corrections

Each of these is a place where following the spec literally is impossible,
self-contradictory, or contradicted by the spec's own examples. **These need a
decision; they are not closed unilaterally.**

**C1. `col()` / `row()` / `stack()` instead of `Box::col()`.** A type named `Box`
in scope shadows `std::boxed::Box` for every user of the crate. `Desc::Box` keeps
the name.

**C2. `event::MouseButton` instead of `Button`.** The widget of that name is more
often written.

**C3. `List::windowed` instead of `List::virtual`.** `virtual` is a reserved
word.

**C4. `Sizing` on `Node`, not in `BoxProps`.** §5 puts sizing nowhere; §8.1
implies `BoxProps`; but the spec's own examples write `Viewport::new(..).flex(1)`
and size a `Component` with `.w(Cells(20))`, neither of which is a `Box`. Sizing
must be a node-level attribute for the spec's own examples to compile.

**C5. `Component::init`.** §7's sketch shows one method and `State: Default`,
which admits no context; §7's prose and §17's risk 8 both describe a
construct / build / teardown lifecycle, and §7 says an ambient read "is legal in
the state constructor". `init` is the construct phase the prose already assumes;
the sketch should show it.

**C6. `Provide` and `LayoutReader` are `Desc` variants.** §7 calls `Provide` "an
ordinary description whose element holds the value and a list of dependents" and
§8.2 calls `LayoutReader` "a description"; §5's enum lists neither. The enum
listing is incomplete. Related: the ambient's identity is part of `ElemType`, so
one ambient cannot update in place over another at the same position — a
strengthening consistent with §6's type-match rule, not a change to it.

**C7. Ambient identity is `Rc::ptr_eq`.** §7 says a value "swapped for a
non-identical one" marks dependents, without defining identity. Pointer identity
is the same rule §6 gives for descriptions, is O(1), and needs no bound on `T`.

**C8. Signed positions.** `Point` and `Rect` origins are `i32`, because content
scrolled above or left of its viewport has a negative origin. Saturating it to
zero would silently move content.

**C9. Relayout re-entry uses a dirty-boundary list, and `arrange` walks the whole
tree.** §8.2's guarantee — a change stops at the nearest boundary — holds for
measurement, which is the expensive half. Assigning absolute positions afterwards
carries no measurement and doing it whole keeps out-of-flow layer collection
correct.

**C10. `Behavior::teardown` takes `&self`.** A behavior is shared between the
element's registry and the state field holding it, and `build` only ever sees
`&State`.

**C11. The target phase belongs to the deepest hit element.** A `Gesture`
wrapping a `TextRun` sees `Capture` and `Bubble`, not `Target`. §9 does not say
which element is the target when the deepest hit carries no listeners; this
matches the DOM.

**C12. `cx.children()` for slot children.** §5 does not say how children passed
to a `Component` description reach the component.

**C13. The root is a child of the frame.** `Auto` at the root means "fill", an
explicit request there is honoured, and the `(type, key)` rule applies to the
root as it does to children. §6 is silent on the root.

**C14. Transactional reconcile rolls back and re-raises.** §16 step 1's
requirement — a failure leaves the last committed content intact — holds. What
happens after the rollback is the error policy, which no section specifies.

### L0 — Skeleton
Crate, CI wiring, `Key`, `Node<M>`, `Desc<M>` with props structs, no behavior.
Descriptions constructible and comparable.
**Exit:** `cargo test -p fresh-ui` runs; a description tree can be built and
asserted against as a plain data structure.

### L1 — Reconciler, against a fake renderer
The element tree, mount/update/unmount, `(type, key)` matching at a position.
The fake renderer records `create` / `update` / `dispose` calls.
**Exit — required cases:**
- unkeyed children reorder → updates in place, positionally;
- keyed children reorder → same elements, no dispose;
- key changed → dispose + create (state gone);
- type changed → dispose + create;
- child removed from the middle → exactly one dispose;
- nested subtree remount disposes depth-first, once each.

Two capabilities are implemented here rather than later, because adding either
afterwards requires revisiting every mutation site in the reconciler:

- **the identity short-circuit** — a matched child whose new description is the
  same instance is skipped, with `Desc::Shared` as the mechanism for returning
  the same instance;
- **transactional subtree reconcile** — tree mutations for a subtree are
  buffered or unwound, so a failure part-way through leaves the last committed
  content intact. Three separate rules depend on this single capability: the
  error policy, deferred disposal, and unwinding a failed constructor.

This phase is a prerequisite for all later ones: an error here propagates to
every component built on the reconciler.

### L2 — Scheduler
`set_state`, the dirty set, depth-ordered flush, disposal skip, the
re-entrancy assertion.
**Exit:**
- parent and child both dirty in one tick → exactly one build each, parent
  first;
- an element disposed by its parent's rebuild is not rebuilt;
- `set_state` during build panics with the element named;
- `set_state` from a handler outside build coalesces into the next flush.

### L2a — `register` and teardown fan-out
Behavior enrollment, child-first teardown, reverse-registration order within a
state object. Every later primitive is a behavior, so this is their substrate.
**Exit:** a behavior registered in a nested component is torn down exactly once,
before its parent's, when the subtree unmounts.

### L2b — Ambient values
`Ambient<T>`, `Provide`, `cx.read` with explicit dependent lists and
dirty-on-change.
**Exit:** a dependent rebuilds when its provider's value changes and not
otherwise; a value read in a constructor and cached in a field is reported by a
debug assert (the snapshot rule in the design doc), which is the case where
this primitive otherwise produces stale output without an error.
**Why here:** without ambients, theme and locale appear in every intermediate
component signature, and signatures are costly to change once components
exist.

### L2c — Diagnostics
Element dump (type, key, state), rebuild counters, last-dirty cause — which
`set_state` site, which ambient, or "parent".
**Exit:** a failing reconciler test can be diagnosed from the dump alone.
**Why here:** adding introspection to a reconciler after the fact requires
changes at every mutation site, and this is the inspectability the retained
tree is intended to provide.

### L3 — Layout
Box constraints in integer cells; `Sizing::{Cells, Flex, Pct, Auto}`;
`measure`/`arrange`; relayout boundaries; `Viewport` scroll windows.
**Prerequisite:** the constraint model is specified before implementation,
including these cases — a list with `Auto` height inside a flex column inside a
modal; a row whose children all request `Flex` in zero available space; text
that must wrap to a width determined by a sibling.
Includes the two-bit dirty scheme (`needs_layout` / path-marked
`child_needs_layout`), the constraint-keyed layout cache, deterministic flex
remainder distribution, and deferred layer resolution (design doc §8.1–§8.2).
**Exit:** layout golden tests for those cases; a dirty text node inside a
fixed-size box relayouts that box and nothing above it; a clean node handed
equal constraints returns its cached result without visiting its subtree; flex
remainders distribute identically across runs; geometry access inside
`build()` trips a debug assert, and `LayoutReader` gets its constraints as an
argument during the layout pass.

### L4 — Paint
`DrawList`, the paint walk, `LayoutSpec` with the keyed index; culling
(off-screen, occluded, scrolled-out).
**Exit:** display-list snapshots; a 100k-row list emits a screenful of items;
an opaque full-frame layer suppresses items beneath it.

### L5 — Hit-testing and propagation
Back-to-front hit-test, path construction, capture/target/bubble, `Flow::Stop`,
`prevent_default`, `PointerMode`, pointer capture, wheel chaining.
**Exit:** ordering tests (capture root→target, bubble target→root); an opaque
sibling above blocks; capture survives the pointer leaving the node's rect; a
`Viewport` at its bound lets the wheel through.

### L5a — `Tasks`
Async ownership: launch, cancel-on-teardown, `launch_replacing(tag, …)`.
**Exit:** a result delivered after teardown never reaches a handler; two
launches under one tag leave only the later one live; delivery lands between
frames, never during build, layout or paint.

### L6 — Focus
Focus tree, scopes, `TraversalPolicy` (reading order, ordinal, directional),
key routing along the focus chain, Shortcuts → Intents → Actions.
**Exit:** focus is preserved across reconciliation — rebuild the tree, assert
focus unchanged. Also: a modal scope confines traversal; the same `Intent`
resolves to
different actions at two focus positions.

### L7 — The widget set
`Button`, `Toggle`, `TextField`, `List`, `Tree`, `Dropdown`, `DualList`,
`Number`, `Divider`, `Spacer`, `RadioGroup` — all as `Component`s over the
seven primitives, with no privileged access. `List` ships both forms: eager
(`List::keyed`) and windowed (`List::virtual`, design doc §11), the second
building descriptions only for the visible index range.
**Exit:** each has behavior tests through the public event path; none reaches
into framework internals; a `List::virtual` over 10^6 items builds, lays out
and paints O(visible) work per frame, measured by the fake renderer's
create/update counts.

**Part 1 is done when** `fresh-ui` builds and tests standalone, a demo binary
drives a small app (a list, a form, a menu, a modal) through the fake renderer
with no Fresh code involved, **and the deviation register carries no open class-A
or class-B item**. The first two hold: `src/demo/` is the application,
`examples/demo.rs` is the binary, and `src/test/screen.rs` is the reference
backend. The third is Part 1b (§4).

---

## 4. Part 1b — closing the deviations

Part 2 (§5) does not start until class A and class B are closed. Migrating nine
surfaces onto a library that differs from its own specification would encode the
difference into every one of them, and the point of the exercise is a single
generic model rather than a second set of hand-specified exceptions.

The order is by dependency, not by size. **R1 is first because A2, A4, A5 and A8
are all consequences of it**, and each would be written twice if it were closed
before the layer it belongs in exists.

Throughout: the 123 existing tests are the regression suite. A phase that
changes behaviour must say which test changed and why; a phase that does not
should leave every one of them passing untouched.

### R1 — The render-object layer *(closes A1)*

Introduce the third tree the spec names.

```rust
pub trait RenderObject {
    fn layout(&mut self, c: Constraints, cx: &mut LayoutCx) -> Size;
    fn paint(&self, g: Geom, out: &mut DrawList);
    fn hit(&self, local: Point) -> Hit;
    fn relayout_boundary(&self) -> bool { false }
}
```

A `RenderId` arena beside the element arena; `Element::render: Option<RenderId>`,
which the spec's own `Element` sketch already has. One implementation per
primitive, holding what §3 says render objects hold: computed geometry, cached
measurements, paint state, focus registration, host handles. `LayoutCx` gives an
implementation the ability to lay out its children and read back their sizes.

The three `match ElemType` bodies — `layout_dispatch`, the per-type arm of
`paint_node`, the mode logic in `hit_node` — move into implementations and
disappear from the framework.

**Exit:** no `match` on `ElemType` remains in `render/` or `hit.rs`; the arena
holds render objects; all 123 tests pass unchanged.

**Cost:** the largest phase. It touches every file under `render/`, plus
`hit.rs`, `focus/`, and the element arena. Expect the layout and paint tests to
be the ones that catch mistakes.

### R2 — `Host` as a user render object *(closes A2)*

`Desc::Host` carries an `Rc<dyn RenderObject>` the application supplied, not an
opaque id. The crate keeps `HostId` as a convenience for hosts that only need a
rectangle, implemented in terms of the general form.

**Exit:** a test outside the primitive set defines a host leaf with its own
measurement and hit-testing — a fixed 3:1 aspect box that only hits its left
half — and it lays out, paints and hit-tests correctly. Goal 6 then holds as
written.

### R3 — `Tasks` launches *(closes A3)*

Restore the spec's call shape without naming a runtime: `Ui` carries a spawner
hook (`fn(Box<dyn FnOnce() + Send>)`, defaulting to `std::thread::spawn`), and
`Tasks::launch_replacing(tag, work)` uses it. The handle becomes internal. The
two delivery guarantees are already implemented and must not regress.

**Exit:** `cx.register(Tasks::new())` then `tasks.launch_replacing("sync", |h| …)`
works as §7 writes it; the existing four `Tasks` tests pass against the new
shape; a host that installs its own spawner sees the work on its own executor.

### R4 — The windowed list over `Viewport` *(closes A4)*

`LayoutReader`'s callback takes a `LayoutInfo { constraints, scroll_window }`
rather than bare constraints, so a reader inside a `Viewport` can read the
window the spec says it reads. `List` becomes `Viewport(LayoutReader(rows))`,
inherits the viewport's clip and scrollbar, and stops owning a scroll offset.
The `follow` flag added for wheel-versus-selection goes away with it: the
viewport owns the window, the list owns the selection.

**Exit:** `List` contains a `Viewport`; the million-row test still mounts under
100 elements; the scrollbar appears without the list emitting it; the eager and
windowed forms still behave identically to keyboard, wheel and selection.

### R5 — The focus tree *(closes A5, A6)*

A retained focus registry: a `Focusable` render object registers on mount and
deregisters on disposal, so `focus_scope()` is a lookup rather than a tree walk.
Add the behavior form, `cx.register(Focusable::new(on_focus_change))`, so a
component becomes focusable without wrapping itself in a primitive — build order
step 3's "every later primitive is a behavior".

**Exit:** `focus_scope()` performs no tree walk; a component made focusable by
`register` alone participates in traversal, receives key events and appears in
the scope; disposal deregisters it, verified by the focus tests already written.

### R6 — Split the context handle *(closes A7)*

`cx.services` on `BuildCx` (scheduler, focus, ambients, anchor registration) and
`cx.geometry` only where geometry is valid — event handlers, tickers, task
callbacks. `BuildCx` gains no path to a rect, so the validity window is a
compile error rather than a debug assertion.

**Exit:** the `geometry is not readable during build` test is replaced by one
that no longer compiles when uncommented, recorded as a `trybuild` case or a
doc-test marked `compile_fail`.

### R7 — Transparent pointer regions *(closes A8)*

Hit-testing returns a stacked path: a `Transparent` node contributes itself and
the search continues behind it. Propagation runs each path in turn until one
claims.

**Exit:** a transparent overlay's handler runs *and* the node behind it receives
the event; an `Opaque` node still stops the search; `Ignore` still removes its
subtree. The existing transparency test is rewritten — it currently asserts the
weaker behaviour.

### R8 — Intrinsic sizing *(closes A9)*

The two-pass measurement §8.1 specifies, with a counter per node so the
double-measure cases are identifiable in the dump — §17 risk 4. Then revisit
whether `Align::Stretch` can drop its "only when the cross extent is definite"
caveat, which exists only to avoid the second pass.

**Exit:** a child whose width depends on a sibling whose height depends on that
child resolves correctly; the dump reports which subtrees were measured twice;
the layout goldens are unchanged, or each change is explained.

### R9 — The remaining behaviors *(closes B1–B5)*

`Ticker`, `Cache` with §8.4's speculative-build contract and the purity check it
exempts, `Controller`/`Anchor`, `Persisted` with its `PersistenceScope` ambient.
Each needs specified semantics, teardown behaviour and tests — §17 risk 8.

**Exit:** one behaviour test each, plus: a `Cache` field survives a speculative
build while a non-`Cache` field mutated during build is reported; a `Controller`
command reaches its bound element and is rejected when it would touch controlled
state; `Persisted` restores across an unmount/remount under a changed scope.

### R10 — Loose ends *(closes B6–B9)*

`LayoutSpec.cursor` populated from the focused field's caret; a `HostLeaf`
raw-input flag so `Modality::Exclusive` derives PTY suppression rather than
declaring it; `DualList`; `request_focus` reported when called during build.

**Exit:** the demo's text field places a cursor; a host leaf under an exclusive
layer reports that it takes no raw input; the widget set matches L7's list.

### R11 — Reconcile the spec *(class C)*

With A and B closed, amend `widget-library-design.md` for the fourteen class-C
items, each with the reason recorded above. C1–C5 and C6 are the substantive
ones: the `Desc` enum listing, the `Component` trait sketch, where `Sizing`
lives, and three names that Rust will not accept. The rest are places the spec is
silent and the implementation had to choose.

**Exit:** a reader can follow the design document and write code that compiles,
and no statement in it is contradicted by `crates/fresh-ui`.

### Sequencing

```
R1 ──┬── R2
     ├── R4
     ├── R5 ── R6
     └── R7
R3   (independent)
R8   (after R1)
R9   (after R5, for Focusable)
R10  (after R2 for HostLeaf, after R9 for the cursor)
R11  (last)
```

R1 is the only phase that must land before the others can start. R3 can be done
at any point. R11 is last because the spec should be amended against the finished
code, not against an intention.

---

## 4b. Part 1c — the review register

After R1–R11 landed, a reviewer read the finished sub-crate against
`widget-library-design.md`. It found fifteen places where the code and the
document still disagreed. Unlike the class A/B/C register in §3, these are not
deviations introduced by the plan — they are the plan not having gone far
enough, plus six ordinary defects the new code introduced.

All fifteen are closed. Each entry states what was wrong, what the design
requires, and what closing it consisted of. The conformance suite in
`tests/conformance.rs` has one test per finding.

### The architectural three

**D1 — the per-kind `match` was relocated, not eliminated.** R1 moved eleven
dispatch sites from `ElemType` to `Desc` rather than removing them, and the
render objects were partly inert: `LayerRender`'s `anchor`/`place`/`fit`/
`modality`/`scrim` and `FocusRender`'s `ordinal`/`skip`/`scope` were written at
construction and never read, because the behaviour still lived in `layout.rs`
and `focus/`. Goals 2 and 6 were therefore only partly met.

*Closed by* giving `RenderObject` two query methods — `layer() -> Option<LayerGeom>`
and `focus_reg() -> Option<FocusReg>` — and one factory, `Desc::sync_render`,
which both creates and updates. The second layout stage, paint, hit-testing,
focus and raw-input routing now ask the object. What still matches on a
description is exactly what is typed by the message (`Handler<M>` cannot live on
a render object, which is not generic over `M`) plus the description-level diff
that decides whether layout has to run. Adding a primitive is one `Desc`
variant, one render object, and one arm.

**D2 — the `Focusable` behavior received focus transitions but no keys.** R5's
exit criterion was the Shortcuts → Intents → Actions chain reaching the focused
element regardless of *how* it registered. A behavior-registered focusable got
`FocusGained`/`FocusLost` and nothing else.

*Closed by* `FocusConfig`, read from the description or from the behavior, and
consulted by `propagate_key`, `resolve_intent`, `run_action`,
`fire_focus_change`, and `listeners`. `Focusable` gained `on_key`, `shortcut`,
`action`, `autofocus` and `focus_within`. The routing code no longer knows which
form is in play.

**D4 — R6 overshot.** `Geometry<'a, M>` borrows the tree, so no handler, ticker
or task callback could hold one: the validity-window split had a window nobody
could reach through.

*Closed by* `GeomHandle` — an owned, cloneable handle taken from `InitCx`,
reading a per-frame snapshot store keyed by element. The store is refreshed for
watched elements only, so the cost is the number of handles rather than the size
of the tree. The window is still enforced: reading during `build` trips the same
debug assertion `Ui::rect` uses.

### The six defects

| # | Defect | Closed by |
|---|--------|-----------|
| D3 | Any modal layer, including `Modality::Inert` (which `Dropdown` uses), made *nested* layers unhittable: the search restricted itself to the modal's subtree, which skips out-of-flow children. | `hit_paths` treats the modal as a **floor in resolution order**, not a subtree. Layers resolved after it are above it and stay live. |
| D5 | `PointerMode` and focus `ordinal`/`skip`/`scope` changes after mount never arrived: `update_render` ran only when `layout_relevant_changed` said something moved, and it says false for `Gesture` and `Focusable`. | Props always reach the render object and the focus tree; the diff now decides only whether the *layout pass* re-runs. |
| D6 | `apply_autofocus` assigned `self.focus = None` directly, so nothing was told it lost focus when a scope opened or closed. | The transition is fired in every branch, including the one where there is nowhere for focus to go. |
| D7 | R8's second measure updated the child's recorded size but not the parent's own, so the remeasure was computed and discarded. | The parent's size is recomputed from what came back whenever any child was measured again. |
| D8 | Disposal never cleared `captured`, `focus`, `press`, `hover` or `focus_restore`, and `ElementId`s are recycled — so the next element in a recycled slot inherited a capture or a focus it had nothing to do with. | `forget_element`, called from `dispose_subtree`, drops every framework reference including the geometry store entry and the pending-layer entries. |
| D9 | `apply_anchors` re-ran `arrange` without clearing `pending_layers`, duplicating every layer: double paint, double dismissal messages. | The list is cleared and the layers re-resolved, so a frame resolves each layer once however many arranges it took. |

### The six loose ends

| # | Loose end | Closed by |
|---|-----------|-----------|
| D10 | `raw_input` was a whole-tree boolean: an exclusive layer switched raw input off even for a leaf *inside* it. | `raw_input_leaves()` answers per element, using the ancestor test the modality rule actually implies. `raw_input()` is `next().is_some()`. |
| D11 | `LayoutSpec.cursor` was plumbed but nothing ever set it — no widget called `cursor_at`. | `TextField` places the cursor at its caret while focused. |
| D12 | The host-leaf path was neither exported nor exercised. | `HostLeaf`, `RenderObject`, `LayoutCx`, `Geom`, `Hit`, `ScrollInfo`, `LayerGeom`, `FocusReg` and `host_leaf` are exported; a test defines a leaf outside the library and asserts it measures, paints, hits and takes raw input. |
| D13 | `ViewportProps::scroll`, `selectable` and `max_h` were declared and never read, though §15.5 of the design uses all three. | `max_h` bounds the window; `scroll` is the initial offset, applied once, after which the offset is framework-owned; `selectable` emits `Draw::Selectable` — the library holds no selection model, and says only where selecting is meaningful, the same way `ThemeKey` says only where appearance comes from. |
| D14 | A `LayoutReader`'s subtree was relinked from nothing, stripping the inherited theme off everything the builder emitted. | The reader's provenance is carried into the subtree it produced. |
| D15 | Three of the four passes were whole-tree: `relink` and `arrange` walked every node every frame regardless of what changed. | Both skip clean subtrees. `arrange` compares the rectangle it would write against the one already there and consults a path-marked dirty bit set when a node is measured; a skipped subtree re-publishes its cached out-of-flow descendants so paint order does not depend on how much work the pass did. `relink` skips a subtree that is neither marked nor handed different inheritance, replaying what it contributed. |

**Exit:** `tests/conformance.rs` passes, the 124 pre-existing tests pass
unchanged, and every framework-side `match` on `Desc` is either typed by the
message or is the single factory.

---

## 5. Part 2 — migrate

### M0 — The seam
The only wave that is pure plumbing, and the one everything else depends on:

1. **TUI backend** in `fresh-editor`: `LayoutSpec` → ratatui `Buffer`, with
   the per-cell theme map preserved from `Item::theme`.
2. **`HostLeaf` impls** for buffer splits and terminal grids, delegating to
   the existing renderers.
3. **A mount point**: render a `fresh-ui` subtree into a given rect inside the
   current frame, and route the events that land in that rect to it.
4. **Input adapter**: crossterm events → `fresh-ui` events, and back out as
   `Action`s.

**Exit:** a trivial `fresh-ui` surface (a one-line status segment) renders and
takes a click inside the real editor, with everything else untouched.

### Waves

Ordered by increasing risk. Each wave: build the surface, switch to it, delete
the previous implementation, with cell output unchanged.

| Wave | Surface | First exercises |
|---|---|---|
| **M1** | Status bar, search-options row | Static layout, click targets |
| **M2** | Context menus (tab / new-tab / explorer / close-split) | `Layer`, `Modality::Inert`, `dismiss`, list navigation |
| **M3** | Menu bar, dropdowns, submenus | Nested layers, hover auto-switch, mnemonics |
| **M4** | Info popups, hover/signature help, theme inspector | Transient dismissal via observers, scroll, text selection |
| **M5** | File browser, prompt / command palette | `FocusScope`, text input, results list, preview |
| **M6** | Plugin panels: dock + floating | `WidgetSpec` → `Node` translation, element state replacing `WidgetInstanceState`, **plugin API change** |
| **M7** | Modals: workspace trust, keybinding editor, calibration wizard | `Modality::Exclusive` |
| **M8** | Settings | The largest interior; partially migrated to `WidgetSpec` at the view layer already |
| **M9** | Frame layout: splits, tabs, scrollbars, dock column, explorer pane | The frame itself; all other surfaces nest inside it |

**M2 is the first decision point.** It is the first wave using layers,
modality, dismissal and focus together. If the seam and the model hold there,
the later waves apply the same mechanisms; if they do not, the library is
corrected before wave three rather than after eight surfaces depend on it.

**M9 is last by construction.** Until it lands, `fresh-ui` surfaces are mounted
into the existing frame layout. M9 inverts that relationship: the frame becomes
a `fresh-ui` tree with `Host` leaves for buffers and terminals, and the previous
chrome layout code is removed.

### Plugin API change (M6)

Keyed builders take a required key function. This breaks `widgets.ts` and
every plugin using `List`/`Tree` without keys.

- Ship the new builders one release ahead, with the old ones deprecated and
  warning at load.
- Element state means a plugin that re-sends its spec no longer loses list
  scroll or tree expansion. This is a behavior change that some plugins may
  have compensated for, and belongs in the changelog.

---

## 6. State: where each field goes

Most UI state today lives on `Editor` and `Window`. "It moves to the element"
is true for a lot of it and wrong for the rest, so every wave starts by
classifying the surface's fields into **four homes**.

| Home | Owner | Test |
|---|---|---|
| **App state** | `Editor` / `Window`, passed down as props | Persisted, or a command / plugin / other subsystem acts on it |
| **Element state** | the element, disposed with it | Lifetime is exactly the widget's; nothing outside rendering cares |
| **Framework state** | render objects, owned by `fresh-ui` | Focus position, `Viewport` scroll, pointer capture, hover |
| **Session state** | serialized, therefore app state by construction | It has to survive a restart |

The classification for the real fields:

| Today | Home | Note |
|---|---|---|
| `menu_state.active_menu` + highlight | element | Pure view state |
| `tab_context_menu` / `new_tab_menu` / `file_explorer_context_menu` / `close_split_menu` `.highlighted` | element | The menu's *presence* is app state; its highlight is not |
| `workspace_trust_scroll` | element | |
| `theme_info_popup` | element | Debug instrument, no persistence |
| `prompt.scroll_offset`, `manual_scroll`, completion popup | element | The editing session |
| `prompt.input` | **controlled** | Committed by `PromptConfirm`, fed to `prompt_histories` — the value is app state, the caret/selection are not |
| `file_explorer` expanded dirs | **controlled** | Serialized as `expanded_dirs`; app state that the tree renders |
| per-split scroll, `tab_scroll_offset` | **controlled** | Serialized in `workspace.rs` |
| `settings_state` | app | The config being edited; the form around it is element state |
| `widget_registry` panel mounts | app | Which plugin, which spec |
| `WidgetInstanceState` (list scroll, tree expansion, selection) | element, **controlled where a plugin drives it** | `set_list_scroll` and friends become props + change events (§M6) |
| `mouse_state.*` drag flags, `dock_resizing`, `widget_text_drag` | framework | Pointer capture replaces the whole cluster |
| `key_context`, `dock.focused`, `Prompt.toolbar_focus`, popup `focused` | framework | One focus position |
| `previous_click_time` / `click_count` | framework | Multi-click detection |
| `dock_width`, `menu_bar_visible` | app | Persisted / user setting |

### Two consequences

**`Editor` gets smaller, not bigger.** Most of its UI fields are view state
today, so a wave mostly *deletes* fields and adds none. App state that stays
does not move — the component just receives it as a prop.

**Serialization is the check.** Persisted view state must be app state, since
elements are disposed on unmount and do not survive a restart. Therefore:

> If a wave changes `workspace.rs` serialization, something was misclassified.

**On `Persisted`.** The library ships a `Persisted<T>` behavior that rehydrates
from a host store at construction and checkpoints at teardown. It is for **new
incidental state only** — a panel's last scroll position, a disclosure state
nothing else reads. It is **not** the mechanism for Fresh's existing persisted
view state works, because that state is not a key-value bucket: per-split
scroll, `tab_scroll_offset` and `expanded_dirs` live in a typed, versioned
serde structure that the daemon, workspace restore and orchestrator persistence
all read and write independently of any UI component. Routing those through
component-owned rehydration would invert ownership — the serializer would have
to ask the UI tree for values, or the UI would write a parallel bucket needing
reconciliation against the typed struct — and it would break the invariant
above, which is the best guard we have on this whole class.

That is a cheap, checkable invariant, and the restore suites
(`workspace_persistence_gates.rs`, `daemon_workspace_restore_parity.rs`, the
`orchestrator_*_restore` tests) are its guard. Write the four-way
classification into the wave's PR description before writing code.

## 7. Verification

The 312 e2e files are the primary verification mechanism for a migration of
this size. They are used as-is rather than rewritten.

1. **Cell output stays byte-identical.** Each wave's acceptance criterion is
   that rendered output does not change, checked with the existing
   snapshot/visual-testing harness. A diff is either a defect or an intended
   change that has been reviewed as such.
2. **New tests assert on `LayoutSpec`** by key — structure and geometry rather
   than cell contents. Existing assertions are not rewritten; these are added
   alongside them.
3. **`scene_parity.rs` passes** through every wave. It is the check that the web
   projection has not diverged.
4. **The standing parity oracles** (event-time geometry vs paint walk, focus
   ring) remain enabled until the surface they cover is migrated, then are
   removed with it.
5. **Per-wave routing tests.** The existing precedence tests — clicks not
   reaching the buffer through a popup, modality, focus order — must pass
   unchanged against the new implementation.

---

## 8. Deletion ledger

A wave is complete when the previous implementation has been removed, not when
the new surface works. Each wave names what it deletes; the change is not
complete while any listed item remains.

| Wave | Deletes |
|---|---|
| M2 | `chrome/context_menu.rs`, the context-menu close-guard box, its `on_key` pre-band grab, its rank entry |
| M3 | `chrome/menu.rs`, `view/ui/menu.rs` dispatch half, the menu close-guard box, the hover auto-switch machine |
| M4 | `chrome/popups.rs`, `chrome/theme_info.rs`, `view/popup_mouse.rs` remnants, the transient-dismiss pre-band stage |
| M5 | `chrome/prompt.rs`, `chrome/file_browser.rs`, `view/prompt_input.rs`, the overlay toolbar ring, the click scrim, the position-blind wheel box |
| M6 | `widgets/kinds/*` dispatch, `widget_runtime.rs`, `WidgetInstanceState` |
| M7 | `chrome/modals.rs`, `capture_mouse`, `blocks_terminal_input`, the cursor/hover suppression lists |
| M8 | `view/settings/*` control layer, `view/controls/*` |
| M9 | `chrome/splits.rs`, `chrome/base.rs`, `chrome/mod.rs` (registry, `layer_rank`, `ChromeTreeBuilder`), `mouse_input.rs` dispatch engines, `PointerGrab`, the chrome half of `render.rs`, `KeyContext` |

When M9 lands, `app/chrome/` and the `LayoutBox` arena no longer exist.

---

## 9. Risks and stop points

1. **L1 and L2 fix the framework's semantics.** All later phases depend on
   them. Budget for one revision of both before L3.
2. **The constraint model** (L3) has the highest cost to change later. Specify
   it, including the cases listed there, before writing layout code.
3. **Cell-identical output is a hard constraint.** Reproducing existing spacing
   behavior exactly, including cases that look incorrect, is a substantial part
   of each wave. Changing them during a migration makes it impossible to
   distinguish a regression from an intended change; make those changes
   separately, after the wave.
4. **M6 changes plugin-visible behavior** (state survival) and breaks the API
   (required keys). It needs a release cycle of its own.
5. **M8 (Settings) is optional.** It is the largest interior and the least
   coupled to dispatch. Stopping after M7, with Settings remaining on the
   previous implementation, is a supported end state.
6. **Two implementations of one surface must not persist across waves.** A wave
   that cannot delete its predecessor indicates a defect in the seam; correct
   the seam rather than accumulating a second UI stack.

## 10. Sequencing summary

```
L0 skeleton
  -> L1 reconciler + identity skip + transactional reconcile
  -> L2 scheduler + frozen-tree invariant
  -> L2a register/teardown -> L2b ambients -> L2c diagnostics
  -> L3 layout (+ geometry assert, LayoutReader) -> L4 paint
  -> L5 hit-test -> L5a tasks -> L6 focus -> L7 widgets

M0 seam -> M1 status bar -> M2 context menus [GO/NO-GO] -> M3 menus -> M4 popups
        -> M5 prompt -> M6 plugin panels -> M7 modals -> M8 settings -> M9 frame
```

Part 1 is self-contained: its own crate, its own test suite, no effect on the
editor. Part 2 is a sequence of individually reversible changes, each verified
by unchanged cell output and by the deletion ledger.
