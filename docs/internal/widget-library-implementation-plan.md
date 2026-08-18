# Widget Library — Implementation Plan

> _AI-generated plan. **Entirely PLANNED** — no part of this has started._
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
    desc.rs         Node<M>, Desc<M>, props structs
    element.rs      Element, ElementTree, reconcile (identity skip, transactional)
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
**Exit:** layout golden tests for those cases; a dirty text node inside a
fixed-size box relayouts that box and nothing above it; geometry access inside
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
seven primitives, with no privileged access.
**Exit:** each has behavior tests through the public event path; none reaches
into framework internals.

**Part 1 is done when** `fresh-ui` builds and tests standalone, and a demo
binary drives a small app (a list, a form, a menu, a modal) through the fake
renderer with no Fresh code involved.

---

## 4. Part 2 — migrate

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

## 5. State: where each field goes

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

## 6. Verification

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

## 7. Deletion ledger

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

## 8. Risks and stop points

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

## 9. Sequencing summary

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
