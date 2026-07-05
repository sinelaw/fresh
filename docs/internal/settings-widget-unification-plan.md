# Unifying the Settings controls and the plugin widget framework

> _Forward-looking plan, not a description of what ships today._ Fresh
> currently has **two** independent UI-control implementations. This document
> records why, what each provides, and a phased plan to collapse them into the
> single declarative widget framework that is already exposed to plugins.
>
> Convention note: this is a PLANNED doc. The status tables below label what is
> IMPLEMENTED versus PLANNED; where a claim disagrees with the source, the
> source wins.

---

## 1. The problem in one paragraph

Fresh ships two mature-but-separate control systems. The **plugin widget
runtime** (`WidgetSpec`) is a declarative, host-reconciled tree that plugins
author in TypeScript — it owns layout, focus, hit-testing, scroll, and cursor,
and is the framework we expose and want to grow. The **Settings control
library** (`view/controls/`) is a host-only, immediate-mode (ratatui) set of
richer form controls that the Settings dialog drives directly. The two share no
code: Settings never touches `WidgetSpec`, the widget runtime never calls
`view/controls/`, and the controls each system implements only partly overlap.
The goal is one framework — the plugin-facing widget tree — that Settings and
plugins both render through.

---

## 2. The two frameworks today (IMPLEMENTED)

### 2.1 Plugin widget runtime — `WidgetSpec`

- **Core types:** `fresh_core::api::WidgetSpec` (tagged enum), plus
  `WidgetAction`, `WidgetMutation`, `HintEntry`, `TextPropertyEntry`.
- **Runtime:** `crates/fresh-editor/src/widgets/` — a registry keyed by
  `(plugin, panel id)`, a reconciler that renders a spec to
  text-property entries + hit areas + host-owned instance state, and pure
  action/mutation helpers.
- **Front-end:** `crates/fresh-editor/plugins/lib/widgets.ts` builders,
  surfaced in the generated `fresh.d.ts`.
- **Model:** the spec is *initial only*; after first render, host-owned instance
  state (List scroll/selection, Text value/cursor/scroll, Tree
  scroll/selection/expansion) is authoritative. Events flow back through the
  `widget_event` hook (`activate`/`toggle`/`change`/`submit`/`select`/`expand`/…);
  targeted updates flow forward through `WidgetMutation` fast paths.
- **Paint surface:** the reconciler emits `TextPropertyEntry` rows written as a
  buffer's virtual content (plus floating/dock and overlay variants).
- **Consumers:** ~7 plugins (search/replace, git log, orchestrator, theme
  editor, live grep, dashboard, package manager) plus the New Session dialog.

**Kinds that ship:** `Row`, `Col`, `Spacer`, `Divider`, `HintBar`, `Toggle`,
`Button`, `Text` (unified single-line / multi-line by row count), `List`,
`Tree`, `LabeledSection`, `WindowEmbed`, `Overlay`, `Raw` (escape hatch).

**Kinds it lacks (the gap with Settings):** no `Number`, no `Dropdown`, no
`Map`, no `DualList`, no `KeybindingList`.

### 2.2 Settings control library — `view/controls/`

- **Location:** `crates/fresh-editor/src/view/controls/` — host-only, no plugin
  exposure, no TypeScript bindings.
- **Pattern per control:** a `*State` (data + behavior), a `*Colors`
  (`from_theme`), a `*Layout` (hit geometry), and a `render_*` that paints into
  a **ratatui `Frame`** and returns the `*Layout` for hit-testing.
- **Controls:** `Toggle`, `NumberInput`, `Dropdown`, `TextInput`, `TextList`,
  `MapInput`, `DualList`, `KeybindingList`, `Button`.
- **Consumer:** the Settings dialog (`view/settings/`) maps each schema field to
  a `SettingControl` variant — `Toggle`, `Number`, `Dropdown`, `Text`,
  `TextList`, `DualList`, `Map`, `ObjectArray` (keybinding list), `Json`
  (multiline editor for object/complex values), `Complex` (uneditable) — and
  drives the `render_*` calls plus its own `SettingsLayout` aggregation, focus
  order, and modal entry-dialog stack.

### 2.3 Overlap and divergence

| Control | `view/controls/` (Settings) | `WidgetSpec` (plugins) |
|---|---|---|
| Toggle / checkbox | ✅ | ✅ |
| Button | ✅ | ✅ |
| Single-line text | ✅ (`TextInput`) | ✅ (`Text`, rows=1) |
| Multi-line text | ✅ (`Json` editor) | ✅ (`Text`, rows≥2) |
| List of strings | ✅ (`TextList`) | ~ (`List` is read/select, not add/remove-edit) |
| Number | ✅ (`NumberInput`) | ❌ |
| Dropdown / select | ✅ (`Dropdown`) | ❌ |
| Map / dict | ✅ (`MapInput`) | ❌ |
| Ordered-subset picker | ✅ (`DualList`) | ❌ |
| Keybinding-list editor | ✅ (`KeybindingList`) | ❌ |
| Tree | ❌ | ✅ |
| Row/Col layout, flex | ❌ (hand-rolled `SettingsLayout`) | ✅ |

Four controls (Toggle, Button, single- and multi-line Text) are implemented
**twice**. Five rich inputs live only in Settings; layout/flex and Tree live
only in the widget runtime.

---

## 3. Why they are separate — and the decision that stands

Two competing design notes (now in git history, removed in the docs
consolidation) proposed opposite directions:

- **`UNIFIED_UI_FRAMEWORK_PLAN.md`** — keep the Rust `view/controls/` library as
  the source of truth and hand-write **TypeScript mirrors** so plugins get the
  same controls.
- **`plugin-widget-library-design.md`** — a Rust-resident declarative
  `WidgetSpec` tree with a thin TS front-end; the host reconciles. Its appendix
  **explicitly rejected** the TS-mirror approach.

The `WidgetSpec` design won and shipped for plugins. The TS-mirror plan did not.
This plan therefore commits to a single direction, consistent with what already
ships: **`WidgetSpec` is the union framework; Settings migrates onto it.** In
the shipped design's own roadmap, "Settings adoption" was listed as remaining,
unstarted work — that is the gap this plan closes.

---

## 4. The real architectural tension (why this is not "pure refactoring")

The retired design note framed Settings adoption as moving renderers to a shared
location — "pure refactoring, no new behavior." That undersells one fact: **the
two systems paint to different surfaces.**

- `view/controls/render_*` draws **immediate-mode into a ratatui `Frame`** and
  returns a `*Layout`.
- `widgets/render.rs` produces **`TextPropertyEntry` rows** (virtual buffer
  content) plus `HitArea`s, under the spec/instance-state model.

A `Toggle` renderer cannot be literally shared because one writes frame cells and
the other emits text-property rows. Unification therefore has to separate
**control logic** (state transitions, value formatting, hit *geometry*, focus
rules — all surface-independent) from **paint** (the surface-specific step).
Three options:

- **(A) Settings-as-widget-panel.** Settings stops calling `view/controls/`
  directly and instead emits a `WidgetSpec` tree rendered by the widget runtime
  into a floating/overlay panel — exactly like a plugin. This is the true
  end-state and matches the stated goal ("switch the Settings UI to the widget
  framework").
- **(B) Teach the widget runtime to paint ratatui frames.** Rejected — it
  inverts the shipped model (virtual-buffer content is the widget runtime's
  whole point) and keeps two paint paths forever.
- **(C) Shared logic core + two thin paint adapters.** Extract per-control
  state/layout/formatting into a surface-agnostic core; keep a ratatui adapter
  (Settings, near-term) and a text-property adapter (widgets). A stepping stone
  toward (A) that de-duplicates behavior without a big-bang Settings rewrite.

**Recommendation:** target **(A)** as the end-state, use **(C)** as the
mechanism to get there without duplicating control behavior in flight. Add the
missing kinds to `WidgetSpec` first (§5.1), which is pure additive value even if
the Settings port lands later.

---

## 5. Phased plan

### 5.1 Phase 1 — Bring the rich inputs into `WidgetSpec` (additive, non-breaking)

Add the five missing kinds so the widget framework becomes a superset of what
Settings needs. Each is a new `WidgetSpec` variant, a `widgets/render.rs`
renderer emitting text-property rows + hit areas, instance-state and
mutation/action plumbing consistent with the existing kinds, and a
`plugins/lib/widgets.ts` builder + `fresh.d.ts` entry.

- `Number` — value + min/max/step, increment/decrement hit areas, integer and
  float-as-percent modes (mirror `NumberInputState`).
- `Dropdown` — options + selected index; opening a dropdown needs the compositor
  (§5.4) for the popup list, so ship a closed/inline-cycling form first and the
  popup form after Layer lands.
- `Map` — key→value entries with expand/collapse and add/remove; reuse the Tree
  disclosure machinery for the expandable rows.
- `DualList` — two-column ordered-subset picker with cross-exclusion.
- `KeybindingList` — the `ObjectArray` editor for keybinding rows.

Deliverable: plugins can build number/dropdown/map/dual-list/keybinding inputs.
Independent of any Settings change; ship and test on their own.

### 5.2 Phase 2 — Extract the surface-agnostic control core (option C)

For the controls that exist on both sides (Toggle, Button, Text, and the Phase-1
additions), factor out the state machine, value formatting, and hit *geometry*
into one place, leaving two thin paint adapters:

- a **ratatui adapter** that keeps `view/controls/render_*` working for Settings
  unchanged in behavior, and
- a **text-property adapter** used by `widgets/render.rs`.

This removes the double implementation of Toggle/Button/Text behavior and makes
the two surfaces provably consistent. No user-visible change; regression-tested
against existing Settings and widget tests.

### 5.3 Phase 3 — Settings emits a `WidgetSpec` tree (option A)

Replace the hand-rolled `SettingsLayout` / `SettingControl` render loop with a
function that maps each schema field to a `WidgetSpec` node (`LabeledSection` +
the matching control kind) inside `Row`/`Col` layout, and mount it as a
floating/overlay widget panel. Focus order, scroll, and hit-testing come from
the widget runtime instead of `SettingsLayout`. The schema→control mapping
(`items.rs`) stays; only its *output* changes from `SettingControl` to
`WidgetSpec`.

Sequencing: do the flat settings page first; the modal **entry dialog** for
Map/ObjectArray entries is a nested modal stack and depends on the compositor
(§5.4).

### 5.4 Phase 4 — Compositor / `Layer`: **reuse, don't build**

Original framing: build a new compositor. On inspection this is unnecessary —
**the editor already has one**, and the remaining modal surfaces should reuse it:

- **`overlay.rs`** is a first-class, ordered overlay-**layer** stack
  (`Editor::overlay_layers()`) that already unifies keyboard focus, mouse
  early-capture, terminal-input gating, and z-order across *every* overlay —
  Settings, Menu, Prompt, Popup, `FloatingModal`, Dock. Precedence lives in one
  place.
- **`FloatingWidgetState`** already renders a `WidgetSpec` as a **centered
  modal**, a **left dock**, or an **anchored context-menu popup**
  (`PanelPlacement::{Centered, LeftDock, Anchored}`) — with input capture,
  click-outside dismiss, scrollbars, and `WindowEmbed`. It rides the
  `FloatingModal` layer.
- **`OverlayRow` / `WidgetSpec::Overlay`** paint rows *on top* of a panel at a
  given buffer row — the mechanism behind `Text` completion popups, rendered by
  both the buffer and floating-panel paint paths.

So the modal surfaces map onto existing infra with no new subsystem:

| Surface | Reuses |
|---|---|
| **Dropdown option popup** | `OverlayRow` (IMPLEMENTED — see `emit_dropdown_overlays`) |
| **Settings entry-dialog** (Map / ObjectArray entry editing) | a floating widget panel via `MountFloatingWidget` (centered modal) — the widget-tree replacement for `EntryDialogState` |
| **Confirm prompts / tooltips / context menus** | `PanelPlacement::{Centered, Anchored}` + the `FloatingModal` overlay layer |

The one genuine gap is **stacking**: today only one `floating_widget_panel` is
mounted at a time (`MountFloatingWidget` replaces any existing one), whereas a
nested entry-dialog-over-modal needs a small stack. Settings already models this
with its own `entry_dialog_stack: Vec<EntryDialogState>`; generalizing the
floating-panel slot into a short stack (top-of-stack captures input; Esc pops
one) is the *only* net-new host work — and it's a localized change to the
floating-panel accessors, not a new compositor.

Status: the Dropdown popup ships on `OverlayRow` today, proving the reuse path.
The floating-panel stack + entry-dialog-on-`WidgetSpec` is the remaining piece,
sequenced before §5.3's modal entry-dialog surface.

### 5.5 Phase 5 — Expose and document

With Settings and plugins both on `WidgetSpec`, finalize the TypeScript builder
surface for the Phase-1 kinds, regenerate `fresh.d.ts`, and update
`plugins.md` §7 and `config-themes-settings.md` §4 to describe one framework.
Retire the now-dead `SettingsLayout`/`SettingControl` render path.

---

## 6. Sequencing and dependencies

```
Phase 1 (rich kinds) ─────────────┐         independent, ship first
                                   ├─► Phase 3 (Settings → WidgetSpec, flat page)
Phase 2 (shared logic core) ──────┘
                                   
Phase 4 (compositor/Layer) ──────► Phase 3 modal surfaces (entry dialog, dropdown popup)
                                   
                                   └─► Phase 5 (expose + docs + delete dead path)
```

Phases 1 and 2 are safe, non-breaking, and independently valuable. Phase 3's
flat page can land before the compositor; its modal surfaces cannot. Phase 4 is
the long pole and is *already* on the widget roadmap for reasons unrelated to
Settings — Settings should be a co-driver of it, not a separate effort.

---

## 7. Risks and open questions

- **Paint-surface parity.** The ratatui and text-property adapters (§5.2) must
  stay pixel-consistent as controls evolve; snapshot tests on both surfaces are
  the guard.
- **Settings-specific affordances.** The modified-indicator gutter, per-field
  `[Reset]`/`[Inherit]`/`[Clear]` buttons, and the layer-source band are
  Settings concepts, not generic widget concepts. Decide whether these become
  generic widget decorations or stay Settings-side chrome wrapped around the
  widget tree. Recommendation: keep them Settings-side initially; generalize
  only if a plugin wants them.
- **Compositor scope creep.** `Layer` unifies popups/tooltips/modals across the
  whole editor; scoping it to what Settings + dropdowns need first avoids
  blocking on the full design.
- **Performance.** Settings pages can be large; the reconciler's full re-emit is
  cheap because instance state is preserved, but confirm on the largest settings
  page before deleting the direct-render path.
- **`List` vs `TextList`.** The widget `List` is select/scroll; Settings'
  `TextList` is add/remove/edit. Phase 1 must decide whether to extend `List`
  with an editable mode or keep `TextList` a distinct kind. Recommendation: a
  distinct editable-list kind, since the editing affordances differ materially.

---

## 8. Definition of done

One declarative widget framework, authored the same way by plugins and by the
built-in Settings dialog: `WidgetSpec` carries every control Settings needs,
each control has a single surface-agnostic implementation, Settings renders by
mounting a widget panel rather than driving `view/controls/` directly, and the
old `SettingsLayout`/`SettingControl` render path is deleted.
