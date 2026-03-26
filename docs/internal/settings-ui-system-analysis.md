# Settings UI System Analysis: Limitations & Missing Features

## Overview

This document analyzes the bugs found in `settings-ui-improvement-plan.md` as
symptoms of deeper structural limitations in the schema-driven UI system. Rather
than treating each bug individually, we identify the systemic gaps they reveal.

---

## 1. Flat Focus Model vs. Recursive Dialog Structure

**Bugs revealed:** C2, H1, H4, L4

The system has a **structural mismatch** between its dialog nesting capability
and its focus/input model:

- **Main settings** uses a 3-panel `FocusManager<FocusPanel>` (Categories /
  Settings / Footer) with Tab cycling between regions. This works well.

- **Entry dialogs** use a simpler binary model: `focus_on_buttons: bool` +
  `selected_item: usize`. There is no `FocusManager` — Tab and Down both call
  `focus_next()` identically (`input.rs:305-313`), making Tab redundant rather
  than region-cycling.

- **Input priority** is a hardcoded chain (`input.rs:27-66`):
  entry_dialog → confirm_dialog → reset_dialog → help → search → Ctrl+S →
  panel routing. Ctrl+S sits *below* the entry dialog check, so it's
  unreachable when any dialog is open. The entry dialog handler has no Ctrl+S
  of its own.

- **Escape propagation** (C2): `handle_entry_dialog_text_editing` handles Esc
  and returns `Consumed`, but the bug report indicates the dialog still closes.
  This suggests either the return value isn't checked properly, or the
  `editing_text` flag is cleared by `stop_editing()` before the navigation
  handler runs, causing a double-dispatch within the same event.

**Systemic limitation:** The focus model was designed for a single-level modal.
When the dialog stack was added, the entry dialog got a *simplified copy* of the
focus logic rather than reusing the same `FocusManager` abstraction. Each new
dialog level would need its own ad-hoc focus wiring.

**Missing feature:** A unified focus model that works recursively — each dialog
level should have the same panel/region/item/sub-item focus hierarchy, with
Tab always cycling regions and Down/Up always navigating items within a region.

---

## 2. Composite Control Navigation is Not Integrated

**Bugs revealed:** H2, H3, H5, L3

The navigation system treats items as atomic units by default. Composite
controls (TextList, Map, ObjectArray) that contain sub-focusable elements are
handled through separate, inconsistent mechanisms:

- **ObjectArray** has first-class support in `focus_next()`/`focus_prev()`
  (`entry_dialog.rs:310-365`). Navigation enters the control, visits each
  binding and the add-new button, then exits.

- **TextList and Map** use a separate `sub_focus: Option<usize>` mechanism with
  `sub_focus_next()`/`sub_focus_prev()` (`entry_dialog.rs:484-521`). But these
  methods are **never called** from `handle_entry_dialog_navigation` — the
  navigation handler calls `focus_next()` which skips sub-items entirely. This
  is why H2 (`[+] Add new` unreachable) and H5 (Root Markers not focusable)
  occur.

- **H3** (Down skips Command): The alphabetical sort at
  `entry_dialog.rs:106` (`items.sort_by_key(|item| !item.read_only)`) puts all
  read-only items first, then editable items. Combined with
  `first_editable_index` tracking, the navigation skips read-only items.
  But if an editable item's name sorts between two others asymmetrically
  (e.g., "Command" between "Auto Start" and "Enabled"), the sort-then-skip
  logic can produce different traversal orders for forward vs. backward.

**Systemic limitation:** There's no unified "focusable element" abstraction.
ObjectArray got special-cased into `focus_next/prev`, while TextList/Map have
a parallel mechanism (`sub_focus`) that's wired in the main settings panel but
not in entry dialogs. Each composite control type needs its own navigation
plumbing.

**Missing feature:** A `FocusableItem` trait or a flattened focus list where
composite controls contribute their sub-elements to the tab order. Navigation
would walk this flat list regardless of nesting depth, and composite controls
would just declare their focusable regions (which they already do via
`focus_regions()` in `items.rs:419-495` — but this is only used for scroll
calculations, not keyboard navigation).

---

## 3. Schema Type Resolution Loses Nullable/Union Semantics

**Bugs revealed:** M1

The schema parser resolves multi-type declarations by taking only the first
type (`schema.rs:215-220`):

```rust
fn primary(&self) -> Option<&str> {
    match self {
        Self::Single(s) => Some(s.as_str()),
        Self::Multiple(v) => v.first().map(|s| s.as_str()),
    }
}
```

For `Option<String>` serialized as `{"type": ["string", "null"]}`, this
correctly resolves to `"string"`. But when the schema has
`additionalProperties` (even implicitly through serde), the resolution path
may hit the object/map branch instead.

**M1 specifically:** The `name` field in `LspServerConfig` is `Option<String>`,
which Serde can serialize as `{"type": ["string", "null"]}` — but if the schema
generator emits it differently (e.g., with `anyOf` or `oneOf`), the type
resolution falls through to `Complex` or gets misidentified.

**Systemic limitation:** The type system is strictly single-type. There's no
support for:
- `oneOf` / `anyOf` / `allOf` combinators
- Discriminated unions (tagged enums beyond simple string enums)
- Optional wrappers (`null | T` treated as just `T` with no "unset" control)
- Conditional schemas (`if/then/else`)

**Missing feature:** Union type support, at minimum `T | null` producing a
control with an explicit "unset/default" toggle alongside the value editor.

---

## 4. Display Field Resolution Doesn't Handle Indirection

**Bug revealed:** M3

`get_display_value()` in `map_input/mod.rs:86-105` applies `value.pointer(field)`
directly to the map entry's value. For LSP config, the value is an *array* of
server objects:

```json
{ "python": [{ "command": "pylsp", "enabled": true }] }
```

The display field `"/command"` is applied to the array `[{...}]`, not to the
first element. The pointer returns `None`, falling through to the
`"[1 items]"` display.

**Systemic limitation:** The schema metadata (`x-display-field`) is a single
JSON pointer with no awareness of intermediate collection types. The display
system assumes the value is directly an object, but the actual data model can
have arrays-of-objects as Map values.

**Missing feature:** Display field resolution that can navigate through arrays
(e.g., apply pointer to first element) or support richer display expressions
(e.g., `"[0]/command"` or a format string like `"{command} ({args.length} args)"`).
Also missing: proper pluralization (`"1 items"` → `"1 item"`).

---

## 5. No Minimum Size Guards

**Bug revealed:** M6

The rendering code (`render.rs`) computes layout areas using arithmetic that
can underflow or produce zero-sized rects at very small terminal sizes. There's
no pre-render check for minimum viable dimensions. The entry dialog has a
minimum width clamp (`clamp(50, 90)`) but the main settings modal doesn't guard
against the terminal being smaller than the clamp minimum.

**Systemic limitation:** Layout calculations assume sufficient space. The
`saturating_sub` calls prevent negative values but produce 0-width/0-height
areas that can cause panics in ratatui's rendering or in division operations.

**Missing feature:** A minimum-size gate at the top of `render_settings()` that
short-circuits to a "terminal too small" message, similar to how many TUI apps
handle this. Each dialog level should declare its minimum viable size.

---

## 6. Entry Dialog Rendering Doesn't Fully Reuse Main Settings Rendering

**Bug revealed:** C1

The entry dialog renderer (`render_entry_dialog` in `render.rs:2840+`) calls
`render_control()` for each item — the same function used by the main settings
panel. Yet text input renders correctly in the main panel and in keybinding
dialogs but not in LSP Edit Item dialogs (C1).

The likely cause is a state synchronization issue: `editing_text` is tracked on
the `EntryDialogState` struct, but the render function must propagate this flag
into the control's `FocusState` so the text input widget knows to show the
cursor and live text. If `update_focus_states()` doesn't correctly set the
text control's focus to `Focused` when `editing_text` is true, the control
renders in display mode (showing the committed value, not the in-progress edit).

**Systemic limitation:** The `editing_text` boolean is dialog-level state that
must be manually synchronized with per-control `FocusState`. There's no
automatic binding between "this dialog item is being edited" and "this control
should render in edit mode." The main settings panel may have a different
synchronization path that works, while the entry dialog's path has a gap.

**Missing feature:** Controls should own their editing state internally, or
there should be a single source of truth that the render function reads
directly, rather than requiring multi-step state propagation
(dialog.editing_text → item.control.focus → render check).

---

## 7. Number Input Has No Direct-Entry Mode

**Bug revealed:** M2

`NumberInputState` has full editing support internally (`start_editing()`,
`insert_char()`, `confirm_editing()` at `number_input/mod.rs:139-174`). But the
settings input handler never activates this mode — Enter on a number field is
not routed to `start_editing()`. Numbers can only be changed via Left/Right
(decrement/increment), not by typing.

When a user presses a digit key while a number field is focused, the character
falls through to whatever text field happens to handle unmatched character
input — hence "leaking" to an adjacent text field.

**Systemic limitation:** The input routing in `handle_entry_dialog_navigation`
only handles Enter for: buttons, toggle, dropdown, text-edit start, and nested
dialog open (`input.rs:340-390`). There's no `ControlAction::StartNumberEdit`
variant. The number control's editing capability exists but is disconnected
from the input pipeline.

**Missing feature:** A `ControlAction` variant for number editing, plus routing
character events (digits) to the focused control regardless of type, rather
than having unmatched keys propagate to unrelated controls.

---

## 8. No Parent Dialog Visual Separation

**Bug revealed:** M4

When a child dialog opens, the parent remains at full brightness. The render
function calls `render_entry_dialog` for each stack level, but doesn't apply
any dimming or overlay between layers. The entry dialog clears its own area
(`Clear` widget) but the parent's uncovered edges remain fully styled.

**Systemic limitation:** The dialog stack is rendered sequentially with no
inter-layer visual treatment. Each dialog only knows about its own area.

**Missing feature:** A dimming pass between dialog layers — after rendering
dialog N but before rendering dialog N+1, apply a semi-transparent overlay
to the entire screen (or at minimum to dialog N's area).

---

## 9. No Scroll Position Indicators in Entry Dialogs

**Bug revealed:** M5

The main settings panel has `ScrollablePanel` with a scrollbar widget. Entry
dialogs track `scroll_offset` and `viewport_height` but have no scrollbar
rendering or position indicators. The user scrolls implicitly via focus
navigation with no visual feedback about position or remaining content.

**Systemic limitation:** `ScrollablePanel` (the reusable scroll component) is
only wired into the main settings panel, not into entry dialogs.

**Missing feature:** Entry dialogs should use the same `ScrollablePanel` +
scrollbar infrastructure as the main panel.

---

## 10. No Collapsible Sections in Entry Dialogs

**Bug revealed:** L2

The main settings panel has `x-section` support — settings within a category
can be grouped under section headers. But entry dialogs render all items flat
with no grouping. For complex schemas like `LspServerConfig` (12+ fields),
there's no way to mark fields as "advanced" and collapse them.

**Systemic limitation:** `x-section` is a schema-level concept that only the
main settings renderer interprets. Entry dialogs don't read `section` metadata
from their items.

**Missing feature:** Section support in entry dialogs, plus a new schema
extension like `x-collapsed: true` or `x-priority: "advanced"` to control
initial visibility.

---

## Summary: Root Cause Categories

| Category | Bugs | Core Issue |
|----------|------|------------|
| **Focus model not recursive** | C2, H1, H4, L4 | Entry dialogs duplicate rather than reuse the focus abstraction |
| **Composite controls not integrated** | H2, H3, H5, L3 | Sub-item navigation exists but isn't wired consistently |
| **Schema type system too narrow** | M1 | No union/nullable/oneOf support |
| **Display field too simple** | M3 | Single pointer can't navigate through arrays |
| **No defensive layout** | M6 | No minimum-size guards |
| **State sync gap** | C1 | editing_text flag not propagated to control render state |
| **Control actions incomplete** | M2 | Number editing exists but isn't exposed via input routing |
| **No inter-dialog visual model** | M4 | Dialog stack has no dimming/overlay between layers |
| **Scroll infrastructure not shared** | M5 | ScrollablePanel only used in main panel |
| **Sections not in dialogs** | L2 | x-section only interpreted by main renderer |

The most impactful structural fix would be **unifying the focus model** (items
1 and 2 above): a single recursive focus abstraction that works identically at
every dialog level, with composite controls contributing their sub-elements to
a flat focus list. This would fix C2, H1, H2, H3, H4, H5, L3, and L4 — eight
of the sixteen bugs.
