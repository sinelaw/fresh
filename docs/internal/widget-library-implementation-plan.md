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

**Part 2** migrates surfaces one at a time. Each wave swaps one surface onto
the library and **deletes its old implementation in the same change** —
keeping both alive is the drift trap this whole arc exists to remove.

### What is in scope

Current line counts, as the honest scale of Part 2:

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

Buffer text rendering (token IR → `ViewLine`, highlighting, folding,
wrapping) and terminal grids stay exactly as they are, reached through
`Host` leaves. They are the reason `Host` exists.

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

**Dependencies: as close to none as possible.** `unicode-width` for cell
measurement, `bitflags`, and nothing else. Specifically **not**:

- **not `ratatui`** — the library emits a display list; the TUI backend that
  turns `Item`s into cells lives in `fresh-editor`;
- **not `fresh-core`** — `WidgetSpec` is the plugin wire format and stays
  where it is; the spec→`Node` translation lives in `fresh-editor`;
- **not `fresh-editor`** — the dependency runs one way only.

`HostId` is an opaque newtype in `fresh-ui`; `fresh-editor` implements
`HostLeaf` for buffer splits and terminal grids.

Added to `[workspace] members` and `default-members`. `cargo test -p fresh-ui`
must pass with the rest of the workspace absent from the build graph — that is
the check that the isolation is real.

---

## 3. Part 1 — build the library

Each phase ends with tests that pin its semantics. The order is not
negotiable: L1 and L2 define the framework's meaning and everything later is
downstream of them.

### L0 — Skeleton
Crate, CI wiring, `Key`, `Node<M>`, `Desc<M>` with props structs, no behavior.
Descriptions constructible and comparable.
**Exit:** `cargo test -p fresh-ui` runs; a description tree can be built and
asserted against as a plain data structure.

### L1 — Reconciler, against a fake renderer
The element tree, mount/update/unmount, `(type, key)` matching at a position.
The fake renderer records `create` / `update` / `dispose` calls.
**Exit — the matrix that matters:**
- unkeyed children reorder → updates in place, positionally;
- keyed children reorder → same elements, no dispose;
- key changed → dispose + create (state gone);
- type changed → dispose + create;
- child removed from the middle → exactly one dispose;
- nested subtree remount disposes depth-first, once each.

Two capabilities land here rather than later, because retrofitting either one
means revisiting every mutation site in the reconciler:

- **the identity short-circuit** — a matched child whose new description is the
  same instance is skipped wholesale, with `Desc::Shared` as the author's way
  to hand one back;
- **transactional subtree reconcile** — tree mutations for a subtree are
  buffered or unwound, so a failure part-way through leaves the last committed
  content intact. Three separate rules depend on this single capability: the
  error policy, deferred disposal, and unwinding a failed constructor.

**Do not proceed until this is right.** A mistake here is a rewrite of
everything after it.

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
otherwise; a value read in a *constructor* and cached in a field is caught by a
debug assert (the snapshot rule), since that is the one way this primitive
silently goes stale.
**Why here:** without ambients, theme and locale are prop-drilled into every
signature — and signatures are the most painful thing to change late.

### L2c — Diagnostics
Element dump (type, key, state), rebuild counters, last-dirty cause — which
`set_state` site, which ambient, or "parent".
**Exit:** a failing reconciler test can be diagnosed from the dump alone.
**Why here:** devtools retrofitted onto a reconciler are far harder than
devtools designed in, and this is the inspectability the three-tree split was
sold on. Moving it late is the kind of decision only ever validated the
expensive way.

### L3 — Layout
Box constraints in integer cells; `Sizing::{Cells, Flex, Pct, Auto}`;
`measure`/`arrange`; relayout boundaries; `Viewport` scroll windows.
**Prerequisite:** the constraint model is settled *on paper* first, with the
awkward cases written out — a list with `Auto` height inside a flex column
inside a modal; a row whose children all want `Flex` in zero space; text that
must wrap to a width that depends on a sibling.
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
**Exit:** **focus survives reconciliation** — rebuild the tree, assert focus
unchanged. Also: a modal scope traps traversal; the same `Intent` resolves to
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

Ordered by rising risk. Each wave: build the surface, swap it, delete the old
implementation, keep cells identical.

| Wave | Surface | First exercises |
|---|---|---|
| **M1** | Status bar, search-options row | Static layout, click targets |
| **M2** | Context menus (tab / new-tab / explorer / close-split) | `Layer`, `Modality::Inert`, `dismiss`, list navigation |
| **M3** | Menu bar, dropdowns, submenus | Nested layers, hover auto-switch, mnemonics |
| **M4** | Info popups, hover/signature help, theme inspector | Transient dismissal via observers, scroll, text selection |
| **M5** | File browser, prompt / command palette | `FocusScope`, text input, results list, preview |
| **M6** | Plugin panels: dock + floating | `WidgetSpec` → `Node` translation, element state replacing `WidgetInstanceState`, **plugin API change** |
| **M7** | Modals: workspace trust, keybinding editor, calibration wizard | `Modality::Exclusive` |
| **M8** | Settings | The largest interior; already half-migrated to `WidgetSpec` at the view layer |
| **M9** | Frame layout: splits, tabs, scrollbars, dock column, explorer pane | The frame itself; everything else nests inside it |

**M2 is the go/no-go.** It is the first wave that uses layers, modality,
dismissal and focus together. If the seam and the model hold there, the rest
is repetition; if they do not, stop and fix the library before wave three.

**M9 last, deliberately.** Until it lands, `fresh-ui` surfaces are mounted
*into* the existing frame layout. M9 inverts that: the frame becomes a
`fresh-ui` tree with `Host` leaves for buffers and terminals, and the old
chrome layout code goes.

### Plugin API change (M6)

Keyed builders take a required key function. This breaks `widgets.ts` and
every plugin using `List`/`Tree` without keys.

- Ship the new builders one release ahead, with the old ones deprecated and
  warning at load.
- Element state means a plugin that re-sends its spec no longer loses list
  scroll or tree expansion — worth calling out in the changelog, because it
  changes behavior plugins may have worked around.

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

**Serialization is the tripwire.** Persisted view state must be app state,
because elements are disposed on unmount and do not survive a restart. So:

> If a wave changes `workspace.rs` serialization, something was misclassified.

**On `Persisted`.** The library ships a `Persisted<T>` behavior that rehydrates
from a host store at construction and checkpoints at teardown. It is for **new
incidental state only** — a panel's last scroll position, a disclosure state
nothing else reads. It is deliberately *not* how Fresh's existing persisted
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

The 312 e2e files are the safety net that makes a migration of this size
feasible. The strategy is to lean on them, not rewrite them.

1. **Cells stay byte-identical.** Every wave's acceptance criterion is that
   rendered output does not change. Use the existing snapshot/visual-testing
   harness; a diff is either a bug or a deliberate, reviewed change.
2. **New tests assert on `LayoutSpec`**, by key — structure and geometry, not
   scraped cells. Do not rewrite old assertions to match; add these alongside.
3. **`scene_parity.rs` stays green** through every wave — it is the check that
   the web projection did not silently drift.
4. **Keep the standing parity oracles** (event-time geometry vs paint walk,
   focus ring) running until the surface they cover is migrated, then delete
   them with it.
5. **Per-wave routing tests**: the golden precedence tests that exist today
   (clicks not reaching the buffer through a popup, modality, focus order)
   must pass unchanged against the new implementation.

---

## 7. Deletion ledger

A wave is not done when the new surface works — it is done when the old one is
gone. Each wave names what it deletes, and the PR is not complete until that
list is empty of survivors.

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

1. **L1/L2 are load-bearing.** Everything downstream assumes their semantics.
   Budget for getting them wrong once and redoing them before L3.
2. **The constraint model** (L3) is the least reversible decision. Settle it on
   paper, with the awkward cases, before writing layout code.
3. **Cell-identical output is a hard constraint, and it will hurt.** Reproducing
   existing spacing quirks exactly is unglamorous work, and the temptation to
   "fix" them mid-migration is what turns a refactor into a regression hunt.
   Fix them in separate, reviewed changes afterwards.
4. **M6 changes plugin-visible behavior** (state survival) and breaks the API
   (required keys). It needs a release cycle of its own.
5. **M8 (Settings) is optional.** It is the largest interior and the least
   coupled to dispatch. Stopping after M7 with Settings on the old path
   indefinitely is a legitimate end state.
6. **Two implementations must never coexist for long.** If a wave cannot delete
   its predecessor, that is a signal the seam is wrong — stop and fix it rather
   than accumulating a second UI stack.

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

Part 1 is a self-contained project with its own test suite and no risk to the
editor. Part 2 is a sequence of small, reversible swaps, each provable by
unchanged pixels and a shrinking deletion ledger.
