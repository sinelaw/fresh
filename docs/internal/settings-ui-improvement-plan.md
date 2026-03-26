# Settings UI Improvement Plan

## Overview

This plan systematically **UX tests** the Settings UI dialogs against the UI design principles, identifies gaps, and defines fixes in priority order. Testing is complete; this document now serves as the consolidated findings and implementation roadmap.

## Architecture Context

Key files:

| File | Purpose |
|------|---------|
| `crates/fresh-editor/src/view/settings/input.rs` | Input routing: text editing, dropdown, navigation, entry dialog |
| `crates/fresh-editor/src/view/settings/entry_dialog.rs` | EntryDialogState: focus management, items, buttons |
| `crates/fresh-editor/src/view/settings/render.rs` | All rendering including entry dialog, buttons, help text |
| `crates/fresh-editor/src/view/settings/items.rs` | SettingControl enum, build_item_from_value |
| `crates/fresh-editor/src/view/settings/state.rs` | SettingsState, dialog stack, config layer management |
| `crates/fresh-editor/src/view/controls/map_input/mod.rs` | MapState, get_display_value() |
| `crates/fresh-editor/src/types.rs` | LspServerConfig, LspLanguageConfig |

---

## UX Testing Results

Four parallel test agents audited the Settings UI against the UI design principles. Below are the consolidated findings organized by dialog nesting level.

### Dialog Hierarchy

```
Level 1: Main Settings panel (sidebar + content)
Level 2: Edit Value dialog (e.g., LSP language entry — shows array of servers)
Level 3: Edit/Add Item dialog (e.g., individual LSP server — shows all fields)
Level 4: Inline editing / sub-dialogs (text edit, JSON code block, nested map Add Value)
```

---

## Consolidated Bug List (Prioritized)

### Critical Bugs

#### C1: Text input does not render in Level 3 Edit Item dialog
**Tested:** Navigate to LSP > open language > open server entry > focus Command field > Enter to edit > type text.
**Result:** Field appears blank while typing. Text IS captured internally but not visually updated. When Escape is pressed, the concatenated text appears briefly before the dialog closes.
**Contrast:** In the Keybindings Add Item dialog, text input renders in real-time correctly. The bug is specific to the LSP Edit Item (Level 3) dialog.
**Root cause area:** `render.rs` — text field rendering in entry dialog may not be refreshing, or cursor/edit state not properly tracked in this dialog context.

#### C2: Escape from text edit mode closes entire Edit Item dialog
**Tested:** In Level 3 Edit Item, enter text edit mode on Command field, press Escape.
**Expected:** Exit text edit mode, stay in dialog.
**Actual:** Entire Edit Item dialog closes, losing all changes.
**Contrast:** Escape from JSON code block editing (Process Limits) correctly exits edit mode without closing dialog. Keybindings dialog also handles Escape correctly.
**Root cause area:** `input.rs:127-133` — `handle_entry_dialog_text_editing()` handles Esc by calling `dialog.stop_editing()`, but the Esc event may be propagating to `handle_entry_dialog_navigation()` which calls `self.close_entry_dialog()` on Esc.

#### C3: Enter on existing array item in Edit Value (Level 2) closes dialog
**Tested:** In Level 2 Edit Value dialog showing `-> pylsp [x]`, navigate to the array item and press Enter.
**Expected:** Opens Edit Item dialog (Level 3) for that server entry.
**Actual:** Closes the Edit Value dialog entirely, returning to Level 1.
**Workaround found:** Pressing Enter on the "Value:" header label opens the Edit Item dialog correctly.

### High Priority Bugs

#### H1: Tab navigation inconsistent across dialog levels
**Level 2 (Edit Value):** Tab toggles between fields and buttons correctly. Status bar "Tab:Fields/Buttons" is accurate.
**Level 3 (Edit Item):** Tab navigates sequentially through all fields (like Down), does NOT reach buttons, wraps from last field to first field. The "Tab:Fields/Buttons" status bar text is inaccurate here.
**Main settings:** Tab moves from content to footer buttons correctly.
**Root cause:** `entry_dialog.rs` `focus_next()` is called for both Tab and Down in `input.rs:310-314`, making them identical. The Level 2 dialog may have different behavior because it has fewer items.

#### H2: [+] Add new buttons not keyboard-focusable in Level 3 sub-sections
**Tested:** In Edit Item dialog, Args, Env, Language Id Overrides all show `[+] Add new` sub-items.
**Result:** Up/Down navigation skips these buttons. Users cannot add items to sub-collections via keyboard. Focus jumps from section header directly to next field.
**Root cause area:** `entry_dialog.rs` `focus_next()`/`focus_prev()` — sub-items within composite controls (TextList, Map) may not be part of the tab order.

#### H3: Down-arrow skips Command field in Edit Item dialog
**Tested:** Navigate with Down from Auto Start field.
**Result:** Command field is skipped; focus jumps to Enabled. Up from Enabled correctly lands on Command.
**Root cause:** Asymmetric navigation logic in `focus_next()` vs `focus_prev()`, possibly related to alphabetical sorting placing Command between Auto Start and Enabled.

#### H4: Ctrl+S does not work in entry dialogs
**Tested:** Ctrl+S from within Edit Item dialog.
**Result:** No effect. Dialog stays open, nothing saved.
**Tested:** Ctrl+S from main settings.
**Result:** Closes settings entirely (triggers editor file-save, bypassing settings save).
**Root cause:** `input.rs:29-31` — entry dialog input handling intercepts all input before the Ctrl+S handler at line 53-59. The main settings Ctrl+S handler calls `CloseSettings{save:true}` which closes the whole settings panel, not just saves.

#### H5: Individual Root Marker items not keyboard-accessible
**Tested:** Root Markers shows items (pyproject.toml, setup.py, etc.) with [x] delete buttons.
**Result:** Cannot focus individual markers or their delete buttons via keyboard.

### Medium Priority Bugs

#### M1: Name field opens "Add Value" sub-dialog instead of inline text edit
**Tested:** Navigate to Name field in Edit Item, press Enter.
**Expected:** Inline text editing (like Command field).
**Actual:** Opens an "Add Value" dialog with Key/Value fields — treating Name as a Map type.
**Root cause area:** Schema interpretation — `Option<String>` for `name` field may be generating wrong control type.

#### M2: Numeric spinner typed input leaks to adjacent fields
**Tested:** In Languages section, edit Tab Size spinner, type "4".
**Result:** Character appears in adjacent Textmate Grammar text field instead of the spinner.
**Additional:** [-] and [+] buttons on spinners are not keyboard-reachable.

#### M3: LSP entries display `[1 items]` instead of command name
**Tested:** LSP map view shows `python  [1 items]` for every language.
**Expected:** `python  pylsp` using `x-display-field: "/command"`.
**Root cause:** `controls/map_input/mod.rs:86-101` — `get_display_value()` calls `value.pointer("/command")` on an array value, which returns None. Should look at first array element.
**Additional:** Grammar error: "1 items" should be "1 item".

#### M4: No parent dialog dimming when child dialog opens
**Tested:** Open Edit Value over Settings, or Edit Item over Edit Value.
**Result:** Parent dialog remains at full brightness. Text bleeds through on edges (e.g., environment variable text visible behind child dialog).
**Principle:** "The child dialog steals focus completely. The parent dialog visually dims or loses its active border color."

#### M5: No scroll indicators in long forms
**Tested:** Forms with many fields (12 fields in Add Item).
**Result:** No scrollbar, no "N more items" indicator, no position percentage. Content scrolls with focus but user has no sense of position.

#### M6: Crash on very small terminal
**Tested:** Resize terminal to 50x15 while dialog is open.
**Result:** Application crashes/exits entirely.
**Principle:** Should show `[Terminal too small]` warning instead.

### Low Priority Issues

#### L1: Complex types rendered as raw JSON
Process Limits shows `{ "max_memory_percent": 50, ... }` as raw JSON textarea. Except/Only Features show `null`. Should have structured controls (number inputs, checkboxes, enum checklists).

#### L2: 12 fields shown flat, no collapsible sections
Add Item form shows all fields alphabetically with no grouping. Most users only need Command, Args, Enabled, Name. Advanced fields (Process Limits, Except Features, Only Features, Language Id Overrides, Root Markers, Initialization Options) should be in a collapsible "Advanced" section.

#### L3: Duplicate focus stops on some fields
Some fields register two focus stops during Up/Down navigation (stopping on both section header and [+] Add new sub-item, or two stops on the same field).

#### L4: No way back from main settings button bar
Once Tab moves focus to the bottom bar buttons, Escape closes the entire Settings dialog. No way to return focus to content without closing and reopening.

#### L5: Minor cosmetic — inconsistent focus indicator spacing
Entry dialog uses `> [ Save ]` (space after `>`), main settings uses `>[ Save ]` (no space).

#### L6: No command validation on save
Can enter nonexistent commands with no feedback.

---

## What Works Well

- **Focus indicators in general:** `>` prefix and `>●` markers are clear and visible at all levels
- **Esc cascade:** Works correctly through all levels (text edit → dialog → parent dialog → settings → editor)
- **Button focus indicators:** When buttons ARE reachable, they show `>` prefix and REVERSED style
- **Checkbox toggle:** Immediate visual feedback on boolean fields
- **JSON code block editing:** Process Limits JSON editor works correctly with proper Escape handling
- **Terminal resize (moderate):** Responsive layout adapts well (sidebar → tab bar) at reasonable sizes
- **Language Id Overrides "Add Value":** Nested map editor works correctly as Level 4 dialog

---

## Reproduction Steps

Each bug below includes full steps to reproduce from a clean state. All steps assume the binary is built and a test file exists:

```bash
# Prerequisites (run once)
cargo build
mkdir -p /tmp/fresh-test && echo 'print("hello")' > /tmp/fresh-test/test.py

# Launch fresh in tmux (reusable for all tests)
tmux kill-session -t fresh 2>/dev/null
tmux new-session -d -s fresh -x 160 -y 50 \
  "TERM=xterm-256color ./target/debug/fresh /tmp/fresh-test/test.py"
sleep 2

# Helper: navigate to Settings > LSP > first language entry > Edit Item dialog
# (reusable macro — call this "NAVIGATE_TO_LSP_EDIT_ITEM")
navigate_to_lsp_edit_item() {
  tmux send-keys -t fresh C-p && sleep 0.5
  tmux send-keys -t fresh "Open Settings" && sleep 0.5
  tmux send-keys -t fresh Enter && sleep 1
  tmux send-keys -t fresh "/" && sleep 0.3
  tmux send-keys -t fresh "lsp" && sleep 0.3
  tmux send-keys -t fresh Enter && sleep 0.5
  # Now focused on first LSP language entry in the map
  tmux send-keys -t fresh Enter && sleep 0.5
  # Now in Level 2 "Edit Value" dialog showing server array
  # Navigate to Value header and press Enter to open Edit Item (Level 3)
  tmux send-keys -t fresh Down && sleep 0.2
  tmux send-keys -t fresh Enter && sleep 0.5
  # Now in Level 3 "Edit Item" dialog for the server
}

# Capture helper
cap() { tmux capture-pane -t fresh -p; }
```

### C1: Text input does not render in Edit Item dialog

```bash
navigate_to_lsp_edit_item
# Navigate to Command field (it's the 3rd editable field alphabetically)
# Fields: Args, Auto Start, Command, Enabled, ...
tmux send-keys -t fresh Down && sleep 0.2  # Args
tmux send-keys -t fresh Down && sleep 0.2  # Auto Start
tmux send-keys -t fresh Down && sleep 0.2  # Command
# Verify focus is on Command
cap | grep ">"  # should show ">● Command"
# Enter edit mode
tmux send-keys -t fresh Enter && sleep 0.3
# Type text
tmux send-keys -t fresh "test-server" && sleep 0.5
# Check: text should be visible in the field
cap | grep "test-server"
# EXPECTED: "test-server" visible in Command field
# ACTUAL: field appears blank (no match for "test-server")
```

### C2: Escape from text edit mode closes dialog

```bash
# Continue from C1 (in text edit mode on Command field)
# Press Escape to exit text edit mode
tmux send-keys -t fresh Escape && sleep 0.3
cap
# EXPECTED: Still in Edit Item dialog, text "pylsptest-server" visible in Command
# ACTUAL: Dialog closed entirely, back at Edit Value (Level 2) or Settings (Level 1)
```

### C3: Enter on array item closes Edit Value dialog

```bash
navigate_to_lsp_edit_item  # gets to Level 3, but let's restart from Level 2
# Actually, navigate fresh to Level 2 only:
tmux kill-session -t fresh 2>/dev/null
tmux new-session -d -s fresh -x 160 -y 50 \
  "TERM=xterm-256color ./target/debug/fresh /tmp/fresh-test/test.py"
sleep 2
tmux send-keys -t fresh C-p && sleep 0.5
tmux send-keys -t fresh "Open Settings" && sleep 0.5
tmux send-keys -t fresh Enter && sleep 1
tmux send-keys -t fresh "/" && sleep 0.3
tmux send-keys -t fresh "lsp" && sleep 0.3
tmux send-keys -t fresh Enter && sleep 0.5
tmux send-keys -t fresh Enter && sleep 0.5
# Now in Level 2 "Edit Value" — shows "-> pylsp [x]" and "[+] Add new"
# Navigate Down to the "-> pylsp" array item
tmux send-keys -t fresh Down && sleep 0.2
cap | grep ">"  # should show focus on "-> pylsp"
# Press Enter on the array item
tmux send-keys -t fresh Enter && sleep 0.5
cap
# EXPECTED: Opens Edit Item dialog (Level 3) for pylsp server
# ACTUAL: Dialog closes, back at Settings (Level 1)
```

### H1: Tab behavior inconsistent in Level 3

```bash
navigate_to_lsp_edit_item
# Press Tab repeatedly and capture focus position each time
for i in $(seq 1 15); do
  tmux send-keys -t fresh Tab && sleep 0.2
  echo "=== Tab $i ===" && cap | grep ">"
done
# EXPECTED: Tab toggles between fields region and buttons region (as status bar says)
# ACTUAL: Tab navigates sequentially through fields, same as Down. Never reaches buttons.
# Check status bar text:
cap | grep "Tab:"
# Shows "Tab:Fields/Buttons" — inaccurate for Level 3
```

### H2: [+] Add new not keyboard-focusable in Level 3

```bash
navigate_to_lsp_edit_item
# Navigate with Down through all fields, looking for [+] Add new under Args
cap | grep "Add new"
# [+] Add new is visible under Args, Env, Language Id Overrides
# Try to focus it with Down arrow navigation:
for i in $(seq 1 20); do
  tmux send-keys -t fresh Down && sleep 0.1
done
cap | grep "> .*Add new"
# EXPECTED: At some point, focus lands on [+] Add new
# ACTUAL: Focus skips all [+] Add new sub-items
```

### H3: Down-arrow skips Command field

```bash
navigate_to_lsp_edit_item
# Navigate to Auto Start field
tmux send-keys -t fresh Down && sleep 0.2  # Args
tmux send-keys -t fresh Down && sleep 0.2  # Auto Start
cap | grep ">"  # should show ">● Auto Start" or similar
# Press Down once more
tmux send-keys -t fresh Down && sleep 0.2
cap | grep ">"
# EXPECTED: Focus on Command
# ACTUAL: Focus on Enabled (Command skipped)
# Verify with Up:
tmux send-keys -t fresh Up && sleep 0.2
cap | grep ">"
# This SHOULD show Command (Up works correctly)
```

### H4: Ctrl+S in entry dialog

```bash
navigate_to_lsp_edit_item
# Press Ctrl+S
tmux send-keys -t fresh C-s && sleep 0.5
cap
# EXPECTED: Saves entry dialog and closes it (or saves settings)
# ACTUAL: Nothing happens. Dialog stays open, no save action.
```

### M1: Name field opens wrong dialog

```bash
navigate_to_lsp_edit_item
# Navigate to Name field (after Initialization Options, before Only Features alphabetically)
# Navigate down to find it
for i in $(seq 1 8); do
  tmux send-keys -t fresh Down && sleep 0.1
done
cap | grep ">"  # look for ">● Name"
# Press Enter
tmux send-keys -t fresh Enter && sleep 0.5
cap
# EXPECTED: Inline text edit mode in the Name field
# ACTUAL: Opens "Add Value" sub-dialog with Key/Value fields (treating Name as a Map)
```

### M2: Numeric spinner input leak

```bash
# Navigate to Languages section instead of LSP
tmux kill-session -t fresh 2>/dev/null
tmux new-session -d -s fresh -x 160 -y 50 \
  "TERM=xterm-256color ./target/debug/fresh /tmp/fresh-test/test.py"
sleep 2
tmux send-keys -t fresh C-p && sleep 0.5
tmux send-keys -t fresh "Open Settings" && sleep 0.5
tmux send-keys -t fresh Enter && sleep 1
tmux send-keys -t fresh "/" && sleep 0.3
tmux send-keys -t fresh "tab size" && sleep 0.3
tmux send-keys -t fresh Enter && sleep 0.5
# Focus should be on Tab Size (numeric spinner)
# Press Enter to edit, type a number
tmux send-keys -t fresh Enter && sleep 0.3
tmux send-keys -t fresh "4" && sleep 0.3
cap
# EXPECTED: "4" appears in Tab Size spinner
# ACTUAL: "4" appears in adjacent Textmate Grammar text field
```

### M3: LSP display shows [1 items]

```bash
tmux kill-session -t fresh 2>/dev/null
tmux new-session -d -s fresh -x 160 -y 50 \
  "TERM=xterm-256color ./target/debug/fresh /tmp/fresh-test/test.py"
sleep 2
tmux send-keys -t fresh C-p && sleep 0.5
tmux send-keys -t fresh "Open Settings" && sleep 0.5
tmux send-keys -t fresh Enter && sleep 1
tmux send-keys -t fresh "/" && sleep 0.3
tmux send-keys -t fresh "lsp" && sleep 0.3
tmux send-keys -t fresh Enter && sleep 0.5
cap | grep -E "python|rust|bash"
# EXPECTED: "python  pylsp" or "python  pyright"
# ACTUAL: "python  [1 items]"
```

### M6: Crash on very small terminal

```bash
tmux kill-session -t fresh 2>/dev/null
tmux new-session -d -s fresh -x 160 -y 50 \
  "TERM=xterm-256color ./target/debug/fresh /tmp/fresh-test/test.py"
sleep 2
tmux send-keys -t fresh C-p && sleep 0.5
tmux send-keys -t fresh "Open Settings" && sleep 0.5
tmux send-keys -t fresh Enter && sleep 1
# Resize to very small
tmux resize-window -t fresh -x 50 -y 15
sleep 1
cap
# EXPECTED: "[Terminal too small]" warning or graceful clipping
# ACTUAL: Application crashes/exits
```

---

## Implementation Plan

### Phase 1: Critical Fixes (C1, C2, C3)

These three bugs make the LSP Edit Item dialog nearly unusable.

**C2 fix (Escape closes dialog):** In `input.rs`, `handle_entry_dialog_text_editing()` handles Esc at line 127-133. Verify the `InputResult::Consumed` return prevents the event from also hitting the navigation handler. If the function returns but the event still propagates, ensure the Consumed result short-circuits further processing.

**C1 fix (text not rendering):** After fixing C2, investigate why text renders in Keybindings but not LSP Edit Item. The same `render.rs` code path should render both. Check if `editing_text` flag is set correctly, and whether the render function receives the updated text state.

**C3 fix (Enter closes Edit Value):** In `state.rs`, the Enter handler for ObjectArray items within the Edit Value dialog needs to open a nested Edit Item dialog instead of triggering the parent dialog's save/close action.

### Phase 2: Navigation Fixes (H1, H2, H3, H4, H5)

**H1 fix (Tab behavior):** In `input.rs:310-314`, differentiate Tab from Down. Tab should toggle between fields region and buttons region. Down should navigate sequentially. Update help text at `render.rs:3051`.

**H2 fix ([+] Add new focusable):** In `entry_dialog.rs`, ensure composite controls (TextList `[+] Add new`, Map `[+] Add new`) are included in the navigation order.

**H3 fix (Down skips Command):** Debug `focus_next()` to find why one field is skipped in one direction only. Likely an off-by-one in index calculation related to `first_editable_index` or the alphabetical sort.

**H4 fix (Ctrl+S):** Add Ctrl+S handling at the top of `handle_entry_dialog_input()` before text/dropdown routing:
```rust
if event.modifiers.contains(KeyModifiers::CONTROL) {
    if matches!(event.code, KeyCode::Char('s') | KeyCode::Char('S')) {
        self.save_entry_dialog();
        return InputResult::Consumed;
    }
}
```

**H5 fix (Root Markers keyboard access):** Make individual TextList items focusable with Up/Down when the TextList control has focus. Add delete-item keybinding (e.g., Delete key on focused item).

### Phase 3: Medium Priority (M1-M6)

**M1 (Name field):** Fix schema interpretation for `Option<String>` — should generate Text control, not Map.

**M2 (Spinner input leak):** Fix focus routing so typed characters go to the focused spinner, not adjacent text field.

**M3 (display field):** Fix `get_display_value()` in `controls/map_input/mod.rs` to apply display_field to first array element when value is an array:
```rust
let target = if let Value::Array(arr) = value { arr.first() } else { Some(value) };
```

**M4 (parent dimming):** Apply dimmed style to parent dialog's render area when child dialog is open.

**M5 (scroll indicators):** Add scrollbar widget to entry dialog content area.

**M6 (crash on tiny terminal):** Add minimum size check before rendering dialogs. Show `[Terminal too small]` if below threshold.

### Phase 4: Polish (L1-L6)

**L1 (structured complex types):** Verify ProcessLimits schema generates nested Object properties. Add enum checklist for LspFeature arrays.

**L2 (collapsible sections):** Add `collapsed_sections: HashMap<String, bool>` to EntryDialogState. Use `SettingItem::section` to group fields. Mark Command/Args/Enabled/Name as primary, rest as "Advanced".

**L3-L6:** Minor fixes after core issues resolved.

---

## Verification Checklist

After each phase, re-test with tmux:

- [ ] **C1:** Text renders in real-time when typing in Edit Item Command field
- [ ] **C2:** Escape from text edit mode stays in dialog, does not close it
- [ ] **C3:** Enter on array item in Edit Value opens Edit Item for that entry
- [ ] **H1:** Tab toggles fields/buttons; Down/Up navigates sequentially
- [ ] **H2:** [+] Add new buttons reachable via keyboard in all sub-sections
- [ ] **H3:** Down-arrow visits Command field (no skipping)
- [ ] **H4:** Ctrl+S saves from within entry dialog
- [ ] **H5:** Root Marker items individually focusable and deletable
- [ ] **M1:** Name field allows inline text editing
- [ ] **M2:** Spinner typed input goes to correct field
- [ ] **M3:** LSP entries show `python  pylsp` not `python  [1 items]`
- [ ] **M4:** Parent dialog dims when child opens
- [ ] **M5:** Scroll indicators visible in long forms
- [ ] **M6:** App shows warning instead of crashing on tiny terminal
