# Settings UI Improvement Plan — Track Two: LSP Configuration Deep Dive

## Executive Summary

This document presents findings from a comprehensive UX audit of the Settings UI dialog,
focusing on the LSP Configuration section. Testing was conducted via `tmux`-scripted
interaction with the debug build. The audit uncovered **9 confirmed bugs** (including
3 critical ones) and **6 usability issues**, evaluated against a strict TUI UX
architecture rubric.

---

## Part 1: Test Environment & Methodology

- **Build**: `cargo build` (debug profile)
- **Terminal**: `tmux` session, 160×50, `TERM=xterm-256color`
- **Test file**: `/tmp/fresh-test/test.py`
- **Navigation path**: Command Palette → Open Settings → General category → scroll to LSP section → Edit Value → Edit Item
- **Tools**: `tmux send-keys` for input, `tmux capture-pane -p` for screen capture

---

## Part 2: Confirmed Bugs & Findings

### Bug 1 — Text Input Broken in Entry Dialog (CRITICAL)

**Reproduction**: Open any LSP entry → Edit Item → navigate to `Command` field → type characters.

**Observed**: Characters are silently consumed. The `Command` field remains empty. Focus
jumps to unrelated fields (e.g., `Auto Start` gets toggled).

**Root Cause**: In `handle_entry_dialog_navigation()` (`input.rs:294–416`), `KeyCode::Char`
events are not matched at all — they fall through to `_ => {}` (line 413) and are consumed
without action. Text fields require the user to press `Enter` first to activate
`ControlAction::StartEditing` (line 400), which sets `editing_text = true`. Only then does
`handle_entry_dialog_text_editing()` process character input. However:

1. There is no visual cue that `Enter` is required to start typing.
2. Typed characters before pressing `Enter` are silently lost.
3. The `Enter` key also triggers `ControlAction::ToggleBool` on boolean fields (line 390),
   so if focus is misaligned, `Enter` toggles the wrong control.

**Recommended Fix**:
- Auto-enter edit mode when a printable character is typed on a focused Text/Number field
  (forward `KeyCode::Char` to `start_editing()` + `insert_char()`).
- Add a visual cue (e.g., blinking cursor or `[type to edit]` hint) for text fields.

---

### Bug 2 — Tab Key Contradicts Status Bar (CRITICAL)

**Reproduction**: Open LSP Edit Item dialog → press `Tab` repeatedly.

**Observed**: `Tab` navigates sequentially through every single field (same as `Down`).
It **never** reaches the `Save`/`Cancel` buttons. The cycle is:
`field₁ → field₂ → ... → fieldₙ → field₁` (wraps without visiting buttons).

**Status bar claims**: `Tab:Fields/Buttons` — implying Tab toggles between the two regions.

**Root Cause**: In `handle_entry_dialog_navigation()` (`input.rs:310–313`):
```rust
KeyCode::Tab => {
    if let Some(dialog) = self.entry_dialog_mut() {
        dialog.focus_next(); // Same as Down arrow
    }
}
```
`Tab` calls `focus_next()` which is identical to the `Down` handler. There is no
region-toggling logic. The `focus_next()` method (`entry_dialog.rs:292–372`) cycles
through items and buttons sequentially, but in practice the button region is rarely
reached because the item cycle wraps first.

**Contrast with main settings**: The outer settings dialog correctly uses `Tab` to cycle
between three panels (Categories → Settings → Footer), with the footer buttons properly
receiving focus.

**Recommended Fix**:
- Redefine `Tab` in entry dialog to toggle between items region and buttons region
  (matching the status bar hint).
- Use `Down`/`Up` for sequential field navigation within a region.
- Ensure `Shift+Tab` performs the reverse toggle.

---

### Bug 3 — No Visible Focus Indicator on Entry Dialog Buttons

**Reproduction**: Open LSP Edit Item dialog → try to navigate to `Save`/`Cancel` buttons.

**Observed**: Buttons always render as `[ Save ]  [ Cancel ]` with no visual
differentiation. Since `Tab` never reaches them (Bug 2), the focus indicator code
(which does exist at `render.rs:3062–3075`) is never triggered.

**Root Cause**: The rendering code at `render.rs:3067` correctly checks
`dialog.focus_on_buttons` and renders a `>` prefix with BOLD+REVERSED styling when
a button is selected. However, because navigation never sets `focus_on_buttons = true`
(due to Bug 2), buttons always appear unfocused.

**Note**: The outer settings footer buttons **do** have proper focus indicators (`>[ Save ]`).
This is an entry-dialog-specific issue.

**Recommended Fix**: Fixing Bug 2 (Tab toggles regions) will naturally expose the existing
button focus rendering. No rendering changes needed — only the navigation fix.

---

### Bug 4 — Enter on Boolean Fields Toggles Instead of Saving

**Reproduction**: Navigate to `Enabled` (boolean) in Edit Item dialog → press `Enter`.

**Observed**: The boolean value toggles (e.g., `[✓]` → `[ ]`). The form is NOT saved.

**Root Cause**: `handle_entry_dialog_navigation()` at `input.rs:340–411` dispatches
`Enter`/`Space` to `ControlAction::ToggleBool` for boolean controls. There is no
distinction between "activate control" and "submit form".

**This is technically by design** — `Enter` activates the focused control — but it
violates the principle of least surprise. In most form UIs, `Enter` submits the form.
The only way to save is to navigate to the `Save` button (which is itself broken per
Bug 2) or use `Ctrl+Enter` (undiscoverable, line 365–367).

**Recommended Fix**:
- Reserve `Space` for toggling booleans and activating controls.
- Make `Enter` submit the form (save) when focus is on a non-editable control.
- Alternatively, add a discoverable `Ctrl+S` shortcut in the entry dialog.

---

### Bug 5 — Down Arrow Navigation Skips Items in Entry Dialog (CRITICAL)

**Reproduction**: Open bash LSP Edit Item → press `Down` repeatedly from `Args`.

**Observed (mapped via scripted test)**:
```
Down cycle: Args → Command → Env → Init Options → Lang Id Overrides →
            Name → Process Limits → [Buttons] → Args → ...
```
**Skipped items**: `Auto Start`, `Enabled`, `Except Features`, `Only Features`,
`Root Markers` — these are never reachable via `Down`.

**Up arrow** visits a different set of items, creating asymmetric navigation:
```
Up cycle: ... → Name → Process Limits → [Buttons] → Args → Command →
          Command → Env → Env → Except Features → Init Options → ...
```
Some items appear twice (likely sub-focus within composite controls consuming an
extra keypress), and `Auto Start`/`Enabled` are still unreachable.

**Root Cause**: The `focus_next()` method (`entry_dialog.rs:310–367`) has special
handling for `ObjectArray` controls but treats all other controls uniformly with
`selected_item += 1`. The likely cause is that composite controls (Maps, TextLists)
with sub-focus consume `Down` presses for internal navigation, causing the visual
focus to appear stuck while the internal index advances. When the composite control
is exited, the next `selected_item` value skips over intermediate simple controls.

Additionally, `update_focus_states()` (`entry_dialog.rs:524–543`) only sets
`FocusState::Focused` on `items[selected_item]`, but if the rendering of composite
controls with sub-focus creates visual ambiguity, the actual focused item and the
visually indicated item can diverge.

**Recommended Fix**:
- Audit the interaction between `focus_next()`/`focus_prev()` and composite control
  sub-focus for Maps and TextLists.
- Ensure Down/Up visits every item exactly once in a consistent order.
- Add integration tests that verify the complete navigation cycle covers all items.

---

### Bug 6 — Ctrl+S Does Not Work in Entry Dialog

**Reproduction**: Open Edit Item dialog → press `Ctrl+S`.

**Observed**: Nothing happens. Focus may shift slightly but the dialog is not saved.

**Root Cause**: In `handle_key_event()` (`input.rs:27–66`), entry dialog input is
checked first (line 29), before the global `Ctrl+S` handler (line 54). The entry dialog
handler (`handle_entry_dialog_input`) routes to `handle_entry_dialog_navigation` which
does not handle `Ctrl+S` — it falls through to `_ => {}`.

**Note**: `Ctrl+Enter` IS implemented as a save shortcut (`input.rs:365–367`), but it
is not shown in the status bar and is not universally supported by terminal emulators.

**Recommended Fix**:
- Add `Ctrl+S` handling at the top of `handle_entry_dialog_input()`, before mode routing.
- Show the shortcut in the entry dialog's help bar.
- Consider also supporting `Alt+S` as a terminal-safe fallback.

---

### Issue 7 — Array Items Show `[1 items]` Instead of Command Preview

**Reproduction**: View the LSP section in the main settings list.

**Observed**: Each LSP language entry displays `[1 items]` (e.g., `astro  [1 items]`).

**Root Cause**: `LspLanguageConfig` is defined as a JSON `array` type in the schema
(`config-schema.json:1118`), wrapping one or more `LspServerConfig` objects. The
`get_display_value()` method (`map_input/mod.rs:86–105`) falls back to
`format!("[{} items]", arr.len())` for array values. The `x-display-field: "/command"`
is set on `LspServerConfig` (the inner object), not on the outer `LspLanguageConfig`
array wrapper.

**Recommended Fix**:
- For array-typed map values, unwrap the first element and apply `display_field` to it.
  E.g., show `clojure-lsp` instead of `[1 items]`.
- If the array has multiple items, show `command₁ (+N more)`.
- Add `x-display-field` support for array-of-object types in `get_display_value()`.

---

### Issue 8 — No Logical Grouping for 11+ Fields (Information Architecture)

**Observed**: The Edit Item dialog for an LSP server config presents **12 fields** in a
flat alphabetical list:
```
Args, Auto Start, Command, Enabled, Env, Except Features,
Initialization Options, Language Id Overrides, Name,
Only Features, Process Limits, Root Markers
```

**Impact**: Users must scroll through all fields to find common ones (`Command`, `Enabled`).
Advanced fields like `Process Limits`, `Except Features`, and `Initialization Options`
are rarely needed but occupy equal visual weight.

**Recommended Fix** (phased):

**Phase 1 — Reorder fields by importance**:
```
Command (required, most important)
Enabled
Name
Args
Auto Start
Root Markers
─── Advanced ───
Env
Language Id Overrides
Initialization Options
Only Features / Except Features
Process Limits
```

**Phase 2 — Collapsible "Advanced" section**:
- Implement an accordion widget. `Enter`/`Space` toggles collapse state.
- When collapsed, `Tab`/`Down` skips all children.
- Persist collapse state in the dialog session.
- Schema extension: `"x-section": "advanced"` is already defined (`schema.rs:664`).

---

### Issue 9 — Complex Types Rendered as Raw JSON

**Observed**: `Process Limits`, `Except Features`, `Only Features`, and
`Initialization Options` are rendered as raw JSON text editors:
```
Process Limits:
  │{                           │
  │  "max_memory_percent": 50, │
  │  "max_cpu_percent": 90,    │
  │  "enabled": true           │
  │}                           │
```

**Impact**: Users must understand JSON syntax to edit these. No validation feedback
until save. `null` values for `Except Features` and `Only Features` are confusing.

**Recommended Fix**:
- **Process Limits**: Render as three named fields: `Max Memory %` (number),
  `Max CPU %` (number), `Enabled` (boolean). Create a sub-schema with
  `SettingType::Object` properties.
- **Only Features / Except Features**: Render as a multi-select checklist of
  known LSP features (completion, hover, diagnostics, etc.). Use `null` = "all features".
- **Initialization Options**: Keep as JSON editor (server-specific), but add syntax
  validation and a `null` → `{}` default hint.

---

## Part 3: Additional Observations

### Observation A — Main Settings Dialog Navigation Works Correctly

The outer settings dialog has proper three-panel focus cycling:
`Categories → Settings → Footer`, with `Tab` correctly switching panels and footer
buttons showing `>` focus indicators. This correct behavior should be the model for
fixing the entry dialog.

### Observation B — Ctrl+Enter Save Exists but Is Undiscoverable

`input.rs:365–367` implements `Ctrl+Enter` as a save shortcut in the entry dialog.
This is not documented in the status bar. Terminal compatibility is also a concern
(`Ctrl+Enter` may not be captured by all terminal emulators).

### Observation C — Mouse Hover State Exists

The entry dialog tracks `hover_item` and `hover_button` and renders hover highlights.
This is a positive UX feature but doesn't compensate for broken keyboard navigation.

### Observation D — Read-Only Field Handling Is Correct

The `Key` field in Edit Value dialogs is properly marked read-only for existing entries
and editable for new entries. Focus navigation correctly skips read-only items via
`first_editable_index`.

---

## Part 4: Improvement Plan (Phased)

### Phase 1 — Critical Bug Fixes (P0)

| # | Issue | File(s) | Effort |
|---|-------|---------|--------|
| 1 | Fix Down/Up navigation skipping items | `entry_dialog.rs` focus_next/focus_prev | Medium |
| 2 | Make Tab toggle Fields↔Buttons regions | `input.rs` handle_entry_dialog_navigation | Small |
| 3 | Auto-enter edit mode on character input | `input.rs` handle_entry_dialog_navigation | Small |
| 4 | Add Ctrl+S save in entry dialog | `input.rs` handle_entry_dialog_input | Small |
| 5 | Fix status bar to match actual keybindings | `render.rs` entry dialog help line | Small |

**Acceptance Criteria**:
- Every field in the Edit Item dialog is reachable via both Down and Up arrows.
- Down and Up visit items in the same (reversed) order with no skips.
- Tab toggles between item fields and button bar.
- Typing on a text field immediately enters characters.
- Ctrl+S saves and closes the entry dialog.

### Phase 2 — Display & Preview Improvements (P1)

| # | Issue | File(s) | Effort |
|---|-------|---------|--------|
| 6 | Show command preview instead of `[1 items]` | `map_input/mod.rs` get_display_value | Small |
| 7 | Reorder fields by importance (not alphabetical) | `entry_dialog.rs` from_schema, `schema.rs` | Medium |
| 8 | Add LSP icon to category list | `render.rs` category_icon | Trivial |

### Phase 3 — Information Architecture (P2)

| # | Issue | File(s) | Effort |
|---|-------|---------|--------|
| 9 | Collapsible "Advanced" section in entry dialogs | New accordion widget, `entry_dialog.rs` | Large |
| 10 | Structured editors for Process Limits | `items.rs`, schema changes | Medium |
| 11 | Feature checklist for Only/Except Features | New checklist widget | Large |

### Phase 4 — Polish & Discoverability (P3)

| # | Issue | File(s) | Effort |
|---|-------|---------|--------|
| 12 | Show Ctrl+S / Ctrl+Enter in entry dialog help bar | `render.rs` | Trivial |
| 13 | Add `[type to edit]` hint on focused text fields | `render.rs` entry dialog items | Small |
| 14 | Validate Command field against $PATH | New validation module | Medium |
| 15 | Terminal resize handling for entry dialogs | `render.rs` dialog sizing | Small |

---

## Part 5: TUI UX Architecture Compliance Checklist

| Principle | Current Status | Target |
|-----------|---------------|--------|
| **Dialog Modality** | ✅ Entry dialog isolates input | Maintain |
| **Visual Hierarchy** | ✅ Rounded borders, padding | Maintain |
| **Responsiveness (SIGWINCH)** | ⚠️ Dialog resizes but may clip | Add min-size warning |
| **"Where Am I?" Focus Rule** | ❌ Focus lost in entry dialog | Fix navigation (Phase 1) |
| **Strict Tab Loop** | ❌ Tab ≡ Down, never reaches buttons | Fix Tab semantics (Phase 1) |
| **Read-Only Skip** | ✅ Read-only Key field skipped | Maintain |
| **Composite Bypass** | ❌ Maps/TextLists disrupt navigation | Fix sub-focus interaction (Phase 1) |
| **Esc = Abort Context** | ✅ Esc closes dialogs | Maintain |
| **Global Save Shortcut** | ❌ Ctrl+S not wired in entry dialog | Add Ctrl+S (Phase 1) |
| **Mnemonics (Alt+Key)** | ❌ Not implemented | Consider for Phase 4 |
| **Collapsible Sections** | ❌ Not implemented | Phase 3 |

---

## Appendix A: Key Source Files

| File | Role |
|------|------|
| `crates/fresh-editor/src/view/settings/entry_dialog.rs` | Entry dialog state, focus_next/prev, update_focus_states |
| `crates/fresh-editor/src/view/settings/input.rs` | Input routing, entry dialog navigation/text handling |
| `crates/fresh-editor/src/view/settings/render.rs` | All rendering including entry dialog and buttons |
| `crates/fresh-editor/src/view/settings/state.rs` | Main settings state, panel focus management |
| `crates/fresh-editor/src/view/settings/schema.rs` | JSON schema parsing, x-display-field, x-section |
| `crates/fresh-editor/src/view/settings/items.rs` | Schema → SettingItem/SettingControl conversion |
| `crates/fresh-editor/src/view/controls/map_input/mod.rs` | MapState, get_display_value |
| `crates/fresh-editor/plugins/config-schema.json` | LSP schema definition (LspLanguageConfig, LspServerConfig) |

## Appendix B: Observed Navigation Trace (Entry Dialog)

```
# Down arrow cycle from the Edit Item dialog for bash LSP:
Args → Command → Env → Initialization Options → Language Id Overrides →
Name → Process Limits → [Save] → [Delete] → [Cancel] → Args → ...

# Skipped by Down: Auto Start, Enabled, Except Features, Only Features, Root Markers

# Up arrow cycle (different path):
... → Name → Process Limits → [Buttons] → Args → Command → Command →
Env → Env → Except Features → Except Features → Initialization Options → ...

# Some items visited twice (sub-focus), others still skipped
```
