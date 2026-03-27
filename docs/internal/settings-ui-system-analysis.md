# Settings UI System Analysis: Limitations & Missing Features

## Overview

This document analyzes the bugs found in `settings-ui-improvement-plan.md`
(Track One) and `settings_ui_improvement_plan_track_two.md` (Track Two) as
symptoms of deeper structural limitations in the schema-driven UI system.
Rather than treating each bug individually, we identify the systemic gaps
they reveal.

Track Two's independent LSP-focused audit confirmed many Track One findings
and uncovered additional issues, particularly around navigation asymmetry and
the implicit edit-mode activation model.

---

## 1. Flat Focus Model vs. Recursive Dialog Structure

**Bugs revealed:** C2, H1, H4, L4; confirmed by T2-Bug2, T2-Bug3, T2-Bug6

The system has a **structural mismatch** between its dialog nesting capability
and its focus/input model:

- **Main settings** uses a 3-panel `FocusManager<FocusPanel>` (Categories /
  Settings / Footer) with Tab cycling between regions. This works well.
  Track Two's Observation A explicitly notes this as "the correct behavior
  [that] should be the model for fixing the entry dialog."

- **Entry dialogs** use a simpler binary model: `focus_on_buttons: bool` +
  `selected_item: usize`. There is no `FocusManager` — Tab and Down both call
  `focus_next()` identically (`input.rs:305-313`), making Tab redundant rather
  than region-cycling. Track Two confirms: `Tab` cycles
  `field₁ → field₂ → ... → fieldₙ → field₁` without ever reaching buttons.

- **Button focus rendering exists but is dead code** (T2-Bug3): The rendering
  code at `render.rs:3067` correctly checks `dialog.focus_on_buttons` and
  renders `>` prefix with BOLD+REVERSED styling. But because navigation never
  sets `focus_on_buttons = true`, buttons always appear unfocused. The feature
  is implemented in the render layer but unreachable from the input layer.

- **Input priority** is a hardcoded chain (`input.rs:27-66`):
  entry_dialog → confirm_dialog → reset_dialog → help → search → Ctrl+S →
  panel routing. Ctrl+S sits *below* the entry dialog check, so it's
  unreachable when any dialog is open. The entry dialog handler has no Ctrl+S
  of its own. Track Two notes that `Ctrl+Enter` exists as an undocumented
  save shortcut (`input.rs:365-367`) but is not shown in the status bar and
  has terminal compatibility issues.

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

**Bugs revealed:** H2, H3, H5, L3; significantly deepened by T2-Bug5

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

- **Track Two reveals the scope is worse than Track One found** (T2-Bug5):
  scripted navigation traces show that **5 of 12 fields** in the LSP Edit Item
  dialog are completely unreachable via Down arrow (Auto Start, Enabled, Except
  Features, Only Features, Root Markers). The Up arrow visits a *different* set
  of fields, with some visited twice. The asymmetry is caused by composite
  controls (Maps, TextLists) consuming Down/Up presses for internal sub-focus
  navigation, which advances `selected_item` without the visual focus matching:

  ```
  Down: Args → Command → Env → Init Options → Lang Id Overrides →
        Name → Process Limits → [Buttons] → Args → ...
        (Skipped: Auto Start, Enabled, Except Features, Only Features, Root Markers)

  Up:   ... → Name → Process Limits → [Buttons] → Args → Command →
        Command → Env → Env → Except Features → ...
        (Some items visited twice, others still skipped)
  ```

  This means simple boolean/string fields sandwiched between composite controls
  are invisible to keyboard navigation.

- **H3** (Down skips Command): Track One attributed this to alphabetical sort
  issues, but Track Two's trace shows Command IS reachable via Down — it's the
  simple fields (booleans, strings) adjacent to composite controls that get
  skipped. The root cause is that `focus_next()` increments `selected_item`
  normally for simple controls, but for ObjectArray controls it enters internal
  navigation. When exiting an ObjectArray, the next `selected_item += 1` skips
  whatever simple control follows. The asymmetry between Down and Up comes from
  ObjectArray's different exit behavior in `focus_next` vs `focus_prev`.

**Systemic limitation:** There's no unified "focusable element" abstraction.
ObjectArray got special-cased into `focus_next/prev`, while TextList/Map have
a parallel mechanism (`sub_focus`) that's wired in the main settings panel but
not in entry dialogs. Each composite control type needs its own navigation
plumbing. The ObjectArray special-casing itself is buggy — it creates
asymmetric forward/backward traversal and causes adjacent simple controls
to be skipped.

**Missing feature:** A `FocusableItem` trait or a flattened focus list where
composite controls contribute their sub-elements to the tab order. Navigation
would walk this flat list regardless of nesting depth, and composite controls
would just declare their focusable regions (which they already do via
`focus_regions()` in `items.rs:419-495` — but this is only used for scroll
calculations, not keyboard navigation).

---

## 3. Implicit Edit-Mode Activation and Key Overloading

**Bugs revealed:** T2-Bug1, T2-Bug4

Track Two uncovered two issues that stem from the same design: controls require
explicit activation before they accept input, but the activation mechanism is
overloaded and undiscoverable.

**No auto-enter for text fields (T2-Bug1):** In
`handle_entry_dialog_navigation()`, `KeyCode::Char` events fall through to
`_ => {}` (`input.rs:413`) and are silently consumed. A user must press Enter
first to trigger `ControlAction::StartEditing` (`input.rs:400`), which sets
`editing_text = true`. Only then does `handle_entry_dialog_text_editing()`
process character input. There is no visual cue (blinking cursor, placeholder
text) that Enter is required first. Characters typed before Enter are silently
lost with no feedback.

**Enter/Space overloading (T2-Bug4):** The Enter key serves dual purpose:
- On boolean controls: toggles the value (`ControlAction::ToggleBool`)
- On text controls: enters edit mode (`ControlAction::StartEditing`)
- On buttons: activates the button (Save/Delete/Cancel)
- With Ctrl modifier: saves the form (`input.rs:365-367`)

There is no "submit form" action separate from "activate control". The only
way to save is to navigate to the Save button (broken per Bug 2) or use
`Ctrl+Enter` (undiscoverable). This violates the principle of least surprise
— in most form UIs, Enter submits.

**Systemic limitation:** The input model conflates two distinct user intents:
"interact with this control" and "commit the form." Every control type handles
Enter/Space differently, and the dispatch is a match on control variant
(`input.rs:370-410`) with no fallback to form-level actions.

**Missing features:**
- Auto-enter edit mode when a printable character is typed on a focused
  text/number field (forward the character to `start_editing()` + `insert_char()`)
- Visual cue for editable-but-not-yet-active text fields (cursor, placeholder)
- Separate keybindings for "activate control" (Space) vs "submit form" (Enter
  when not in an editable control)
- Discoverable save shortcut shown in the status bar

---

## 4. Schema Type Resolution Loses Nullable/Union Semantics

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

## 5. Display Field Resolution Doesn't Handle Indirection

**Bugs revealed:** M3, T2-Issue7

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

## 6. No Minimum Size Guards

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

## 7. Entry Dialog Rendering Doesn't Fully Reuse Main Settings Rendering

**Bugs revealed:** C1, T2-Bug1

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

## 8. Number Input Has No Direct-Entry Mode

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

## 9. No Parent Dialog Visual Separation

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

## 10. No Scroll Position Indicators in Entry Dialogs

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

## 11. No Collapsible Sections in Entry Dialogs

**Bugs revealed:** L2, T2-Issue8, T2-Issue9

The main settings panel has `x-section` support — settings within a category
can be grouped under section headers. But entry dialogs render all items flat
with no grouping. For complex schemas like `LspServerConfig` (12+ fields),
there's no way to mark fields as "advanced" and collapse them.

Track Two (Issue 8) provides a concrete field ordering proposal: Command,
Enabled, Name, Args, Auto Start, Root Markers as primary fields; Env,
Language Id Overrides, Initialization Options, Only/Except Features, Process
Limits as "Advanced." Track Two also notes (Issue 9) that complex types like
Process Limits are rendered as raw JSON when they could have structured
controls (number inputs for percentages, boolean for enabled).

**Systemic limitation:** `x-section` is a schema-level concept that only the
main settings renderer interprets. Entry dialogs don't read `section` metadata
from their items. Additionally, there's no schema extension for field ordering
priority — fields are always alphabetically sorted, making the most important
field (Command) appear third.

**Missing features:**
- Section support in entry dialogs, plus a new schema extension like
  `x-collapsed: true` or `x-priority: "advanced"` to control initial
  visibility
- Field ordering hints (`x-order` or `x-priority`) to override alphabetical
  sort for entry dialog fields
- Structured sub-editors for complex types that have known schemas (Process
  Limits) instead of falling back to raw JSON

---

## Summary: Root Cause Categories

| # | Category | Bugs (Track One + Two) | Core Issue |
|---|----------|------------------------|------------|
| 1 | **Focus model not recursive** | C2, H1, H4, L4, T2-2, T2-3, T2-6 | Entry dialogs duplicate rather than reuse the focus abstraction |
| 2 | **Composite controls not integrated** | H2, H3, H5, L3, T2-5 | Sub-item navigation exists but isn't wired consistently; 5 of 12 fields unreachable |
| 3 | **Implicit edit-mode activation** | T2-1, T2-4 | Characters silently lost; Enter overloaded for activate + submit |
| 4 | **Schema type system too narrow** | M1 | No union/nullable/oneOf support |
| 5 | **Display field too simple** | M3, T2-7 | Single pointer can't navigate through arrays |
| 6 | **No defensive layout** | M6 | No minimum-size guards |
| 7 | **State sync gap** | C1, T2-1 | editing_text flag not propagated to control render state |
| 8 | **Control actions incomplete** | M2 | Number editing exists but isn't exposed via input routing |
| 9 | **No inter-dialog visual model** | M4 | Dialog stack has no dimming/overlay between layers |
| 10 | **Scroll infrastructure not shared** | M5 | ScrollablePanel only used in main panel |
| 11 | **Sections/ordering not in dialogs** | L2, T2-8, T2-9 | x-section only interpreted by main renderer; no field ordering; complex types fall back to JSON |

The most impactful structural fix would be **unifying the focus model** (items
1, 2, and 3 above): a single recursive focus abstraction that works identically
at every dialog level, with composite controls contributing their sub-elements
to a flat focus list, and auto-entering edit mode on character input. This
would address the majority of confirmed bugs across both audit tracks.

---

## Architectural Alternatives & Tradeoffs

The bugs cluster around focus management (items 1-3) and the dialog model (9-11).
Below we evaluate alternative approaches, keeping in mind this is a terminal UI
with ratatui — not a web or desktop framework — so we should avoid over-engineering.

### Focus Management: Three Options

**Option A: Flat Focus List (Recommended)**

Flatten all focusable elements into a single ordered list per dialog level.
Composite controls declare their sub-elements via the existing `focus_regions()`
method (`items.rs:419-495`), which already produces the right data but is
currently only used for scroll calculations. Navigation becomes a simple
index increment/decrement on this flat list.

This is the "roving tabindex" pattern from web accessibility (ARIA): the
composite widget (Map, TextList, ObjectArray) is one tab stop, and arrow keys
move within it. Tab moves to the next top-level group (items region ↔ buttons).

```
Tab order:     [Items Region] ←Tab→ [Buttons Region]
Arrow order:   item₁ → item₁.sub₁ → item₁.sub₂ → item₂ → item₂.sub₁ → ...
```

*Tradeoff:* Requires refactoring `focus_next()`/`focus_prev()` in
`entry_dialog.rs` to use the flat list instead of `selected_item += 1` with
per-type special cases. Medium effort, but eliminates all three navigation
code paths (ObjectArray special-case, sub_focus, and the default).

*Why not the others:*

**Option B: Recursive FocusManager**

Give each dialog its own `FocusManager<FocusPanel>` identical to the main
settings panel. Entry dialogs would have `Items` and `Buttons` panels.

*Tradeoff:* Adds abstraction but doesn't solve the composite control problem —
within the Items panel, you still need to handle Maps/TextLists/ObjectArrays.
The main settings panel doesn't have this problem because its items are simpler
(no deeply nested composite controls). The panel-level abstraction helps with
Tab semantics but not with Down/Up within items.

**Option C: Tree-Based Focus (Turbo Vision / WPF Style)**

Model the focus hierarchy as a tree: Dialog → Panel → Item → Sub-item, with
events tunneling down (for shortcuts) and bubbling up (for unhandled keys).
Each node decides whether to consume or propagate.

*Tradeoff:* Most correct architecturally, but massive over-engineering for a
TUI settings dialog. The current system has at most 4 nesting levels. A tree
model adds complexity without proportional benefit. The tunneling/bubbling
pattern is valuable for large GUI frameworks with unbounded nesting, not for a
fixed-depth form editor.

### Recommendation

**Use Option A (flat focus list) + a simple Tab region toggle.**

Concretely:
1. At dialog open, build a `Vec<FocusTarget>` from all items' `focus_regions()`
2. Down/Up walk this list sequentially
3. Tab toggles `focus_on_buttons` (two regions only, no need for `FocusManager`)
4. On character input, auto-enter edit mode if the focused target is a text/number field

This is the minimum change that fixes the maximum number of bugs. It reuses
existing infrastructure (`focus_regions()`) and avoids new abstractions.

### Edit-Mode Activation

**Current:** Explicit Enter-to-edit, characters silently lost before activation.

**Alternative A (auto-enter, recommended):** Forward printable `KeyCode::Char`
events to `start_editing()` + `insert_char()` when a text or number control is
focused. This matches the behavior users expect from any form UI.

**Alternative B (always-active text fields):** Text fields are always in edit
mode when focused, like a web form `<input>`. Escape commits and moves focus.

*Tradeoff:* Alternative B is simpler but conflicts with the current model where
Up/Down navigate between fields even when a text field is focused. Auto-enter
(A) preserves the navigation-first model while removing the silent-loss
footgun.

### Dialog Visual Hierarchy

**Current:** No dimming between dialog layers. Text bleeds through.

**Recommended:** Before rendering each stacked dialog, render a full-area
`Paragraph::new("")` with a dim background color over the parent's area.
This is a 5-line change in `render.rs` — iterate the dialog stack and render
a dim overlay between each level. No new abstractions needed.

### Sections and Field Ordering in Entry Dialogs

**Current:** Flat alphabetical list. `x-section` ignored in entry dialogs.

**Recommended approach (progressive disclosure):**
1. Add `x-order: N` schema extension for explicit field ordering (simple integer)
2. Reuse existing `x-section` in entry dialog rendering — the infrastructure
   exists in the main panel, just needs to be called from `render_entry_dialog`
3. For collapsible sections, add `collapsed_sections: HashSet<String>` to
   `EntryDialogState`. When a section is collapsed, its items are skipped in
   both rendering and navigation. Enter/Space on a section header toggles it.

*Tradeoff vs. accordions/wizards:* Full accordion widgets or multi-step wizards
would be over-engineering for settings dialogs with 12-15 fields. Collapsible
sections with a simple open/closed toggle give 80% of the UX benefit at 20%
of the implementation cost.

### State Management

The current system uses a `pending_changes: HashMap<String, Value>` accumulator
with explicit save — effectively the Memento pattern. This is appropriate for
the use case. The entry dialog stack preserves `original_value` for cancel
restoration. No changes needed here — the state management is the most solid
part of the architecture.

### What NOT to Change

- **The schema-driven approach itself** is sound. The `x-` extension mechanism
  is well-designed for extensibility.
- **The dialog stack model** is correct for the nesting depth needed (max 4
  levels). No need for a general-purpose dialog manager.
- **The control widget library** (Toggle, Dropdown, NumberInput, TextInput,
  TextList, Map, ObjectArray, JsonEdit) covers the needed types. The issue
  isn't missing widgets — it's missing wiring between them and the focus/input
  systems.
- **The layer system** (User/Project/System config) works correctly and
  doesn't need architectural changes.
