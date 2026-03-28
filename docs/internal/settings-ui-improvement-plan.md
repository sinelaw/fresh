# Settings UI Improvement Plan

## Overview

This plan tracks systematic UX testing of the Settings UI dialogs against UI design principles and NNGroup best practices. Three rounds of testing completed; this document reflects the current state.

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

## Testing History

| Round | Rebased On | Agents | Key Changes Found |
|-------|-----------|--------|-------------------|
| 1 | Initial | 4 agents | Identified 20 bugs across all categories |
| 2 | master (ced0969) | 3 agents | C1, H3, H4, M1, M3, M4 fixed. C2 behavior changed (no longer closes dialog, but Esc now does nothing) |
| 3 | master (efbf75f) | 3 agents | C3 fixed, M8 fixed (sticky headers), "Advanced" section divider added. Composite control navigation improved. |
| 4 | master (efbf75f) | 3 agents + tmux | H1 fixed (Tab sequential), M7 fixed (PageDown), composite highlight per-row, add-server persistence, TextList auto-accept |

---

## Fixed Issues (All Rounds)

| ID | Description | Fixed In |
|----|-------------|----------|
| C1 | Text input not rendering in Edit Item dialog | Round 2 |
| C3 | Enter on array item in Edit Value closes dialog | Round 3 |
| H1 | Tab only toggles between fields and Save button | Round 4 — Tab now cycles sequentially through all fields, sub-fields, and buttons |
| H3 | Down-arrow skips Command field | Round 2 |
| H4 | Ctrl+S doesn't work in entry dialogs | Round 2 |
| H5 | Individual TextList items not keyboard-focusable | Round 3/4 — composite navigation visits sub-items |
| M1 | Name field opens wrong dialog type | Round 2 |
| M3 | LSP entries display `[1 items]` instead of command | Round 2 |
| M4 | Parent dialog not dimmed when child opens | Round 2 |
| M7 | No Page Up/Down in long lists | Round 4 — PageDown/PageUp jump by viewport height |
| M8 | Section header scrolls away, losing context | Round 3 (sticky headers) |
| NEW | Composite highlight covers entire section | Round 4 — highlight now only on focused sub-row, `>` on sub-item not header |
| NEW | Adding new LSP server doesn't persist | Round 4 — fixed path computation in save_array_item_dialog_inner |
| NEW | TextList edits lost on Down/Up/Tab | Round 4 — auto-accept pending text before navigating away |

---

## Current Bug List (Prioritized)

### Critical Bugs

#### C2: Text fields always in edit mode — no way to exit
**Round 3 status:** NOT FIXED
**Behavior:** Text fields (Command, Name, Comment Prefix, etc.) auto-enter edit mode when navigated to. The inverse video cursor (`[7m]`) appears and the label turns blue (`38;5;25`) immediately on focus — no Enter press required. Escape has zero visible effect: cursor stays, label stays blue, characters can still be typed. The status bar says "Enter:Edit" but Enter is not required.
**Impact:** Users cannot distinguish "navigating fields" from "editing a field value." Accidental keystrokes modify field values. The only way to leave the dialog is Ctrl+S (save+close) or multiple Escapes (which eventually close the dialog).
**ANSI evidence:**
```
Navigating to field:  [38;5;25mCommand : [astro-ls[7m ]   <- blue label, cursor active
After pressing Esc:   [38;5;25mCommand : [astro-ls[7m ]   <- IDENTICAL
After pressing Down:  [38;5;15mCommand : [astro-ls]        <- normal white (left field)
```
**Root cause:** `editing_text` flag may not be properly toggled, or text fields use a different edit-mode mechanism (always-inline) that bypasses the `start_editing()`/`stop_editing()` flow.
**NNGroup violations:**
- User Control and Freedom — no "emergency exit" from edit mode
- Error Prevention — accidental keystrokes modify values
- Consistency — contradicts "Enter:Edit" status bar text

### High Priority Bugs

#### H1: Tab only toggles between fields and Save button
**Round 4 status:** FIXED
**Fix:** Tab now calls `focus_next()` (same as Down), cycling sequentially through all fields, sub-fields, and buttons. Shift+Tab calls `focus_prev()`. Verified with tmux: 20 Tab stops including all 3 buttons.

#### H2: [+] Add new buttons not directly focusable in Level 3
**Round 3 status:** PARTIALLY FIXED
**Behavior:** Down arrow navigation visits composite control items (e.g., Args → "--stdio" item) but still skips [+] Add new buttons.
**Workaround:** Enter on section headers (Args, Root Markers) opens inline text input with [+]. Enter on map sections (Env, Language Id Overrides) opens "Add Value" dialog. Adding items IS possible, just not through the visible [+] buttons.
**NNGroup violation:** WCAG 2.1 Level A — visible interactive elements must be keyboard-reachable.

#### H5: Individual TextList items not keyboard-editable/deletable
**Round 4 status:** FIXED
**Fix:** Composite control navigation now visits individual TextList items. Down arrow enters the section and visits each item. E2e test `test_textlist_items_keyboard_accessible` passes.

#### H6: Text fields auto-enter edit mode on navigation
**Round 3 status:** NOT FIXED (see C2 — same root cause)
**Behavior:** Navigating to any text field with Down/Up immediately activates edit mode without pressing Enter. This makes it impossible to "select" a text field without editing it.

#### H7: Status bar is static — doesn't reflect current mode
**Round 3 status:** NOT FIXED
**Behavior:** Footer always shows `↑↓:Navigate  Tab:Fields/Buttons  Enter:Edit  Ctrl+S:Save  Esc:Cancel` regardless of:
- Text field in auto-edit mode (should show "Esc:Stop editing")
- Dropdown open (should show "↑↓:Select  Enter:Confirm")
- Button focused (should show "Enter:Activate  ←→:Navigate")

### Medium Priority Bugs

#### M2: Numeric spinner typed input leaks to adjacent fields
**Status:** Not re-tested in Round 3. Likely still present.

#### M5: No position indicator in long lists
**Round 3 status:** NOT FIXED
**Tested:** Scrollbar IS present with proportional thumb size. Colors: main panel thumb `48;5;3` (olive), track `48;5;15` (white). Dialog thumb `38;5;70` (green), track `48;5;239` (dark gray). But no numeric "X of Y" indicator anywhere.

#### M7: No Page Up/Down in long lists
**Round 4 status:** FIXED
**Fix:** Added `select_next_page()`/`select_prev_page()` to SettingsState. PageDown/PageUp jump by viewport height. Home/End not yet implemented.

#### M9: No confirmation when discarding changes via Esc
**Status:** Not re-tested in Round 3. Likely still present.

#### M10: No search/filter within map lists
**Status:** Not re-tested in Round 3.

#### M11 (NEW): Map table entries have weakest focus indicator
**Round 3 finding:** Map entries (Languages, LSP) use only text color change (cyan `38;5;14` → white `38;5;231`) for focus. No background highlight, no `>` arrow. This is significantly weaker than:
- Sidebar: blue background `48;5;25` + `>` arrow
- Settings fields: dark gray background `48;5;16` + `>●` indicator
- Buttons: inverse video `[1;7m]` + `>` arrow
**NNGroup violation:** Consistency — focus indication strength should not vary by context.

#### M12 (NEW): Inconsistent scrollbar colors between contexts
**Round 3 finding:** Main panel scrollbar (olive `48;5;3` on white `48;5;15`) vs dialog scrollbar (green `38;5;70` on dark gray `48;5;239`). Minor but noticeable inconsistency.

### Low Priority Issues

#### L1: Complex types rendered as raw JSON
Process Limits, Except/Only Features, Initialization Options shown as raw JSON textareas.

#### L2: No collapsible sections (PARTIALLY ADDRESSED)
An "Advanced" section divider (`── Advanced ──` in `38;5;244` gray, bold) now separates basic from advanced fields in the Edit Item dialog. However it's not collapsible — all fields remain visible.

#### L4: No command validation on save
Can enter nonexistent commands with no feedback.

---

## What Works Well

- **Text input rendering:** Characters appear immediately in all text fields
- **Ctrl+S save shortcut:** Works from any field in entry dialogs, including during text editing
- **Focus indicators (non-map contexts):** `>` prefix with `>●` markers. Consistent `48;5;16` dark bg + `38;5;231` bright white
- **Button focus:** Bold+reverse video (`[1;7m]`) with `>` arrow. Delete button uses red (`38;5;160`)
- **Esc cascade:** Clean unwinding through all nesting levels with correct focus restoration
- **Progressive parent dimming:** Each level dims parent. Active: bright cyan borders `38;5;14`. Parent: gray `38;5;59`, dark teal `38;5;29`
- **Focus return:** After closing child dialog, focus returns to exact spawning element
- **Enter on array items:** Now correctly opens Edit Item dialog (C3 fixed)
- **LSP display field:** Shows command names with proper truncation
- **Sticky section headers:** Section labels stay pinned while scrolling through long lists
- **Scrollbar:** Present with proportional thumb. Position tracks viewport accurately
- **Checkbox toggle:** Immediate visual feedback, arrows navigate away cleanly
- **JSON code block editing:** Works correctly with proper Esc handling
- **Responsive layout:** Sidebar→tab bar adaptation at smaller terminals
- **Advanced section divider:** Visual separation between basic and advanced fields
- **Composite control navigation:** Down arrow now visits individual items within Args, Root Markers, etc.
- **Tab sequential cycling:** Tab visits every field, sub-field, and all buttons sequentially. Shift+Tab reverses.
- **Per-row composite highlighting:** Only the focused sub-row gets highlight background and `>` indicator, not the entire section
- **PageDown/PageUp:** Jump by viewport height in settings panel for fast navigation through long lists
- **Add new LSP server:** Adding a server via [+] Add new correctly persists in the parent dialog
- **TextList auto-accept:** Down/Up/Tab auto-accept pending text in TextList add-new fields

---

## Implementation Plan

### Phase 1: Text Field Edit Mode (C2, H6)

The most impactful fix — makes the entire Edit Item dialog usable for text fields.

**Problem:** Text fields behave as "always-inline-editing" rather than using the explicit `start_editing()`/`stop_editing()` pattern that JSON controls use.

**Fix approach:**
1. In `entry_dialog.rs`, ensure text fields do NOT auto-enter edit mode on focus. The `editing_text` flag should only be set when the user presses Enter/Space.
2. In `input.rs` `handle_entry_dialog_text_editing()`, ensure Escape calls `dialog.stop_editing()` and returns `InputResult::Consumed`, properly clearing the edit state.
3. When NOT in edit mode, text fields should display their value normally (no cursor, label in default color). The `[Enter to edit]` hint should appear.
4. When in edit mode, arrow keys should move the cursor within the text, not navigate fields. Tab/Esc should exit edit mode.

### Phase 2: Tab & Button Navigation (H1) — DONE

Tab now calls `focus_next()`, identical to Down. Cycles through all fields, sub-fields, and buttons sequentially. Shift+Tab mirrors in reverse.

### Phase 3: Status Bar (H7)

**Fix:** In `render.rs`, make the help text dynamic based on current state:
```
Normal:       ↑↓:Navigate  Tab:Fields/Buttons  Enter:Edit  Ctrl+S:Save  Esc:Cancel
Text editing: Type to edit  Tab/Esc:Stop editing  Ctrl+S:Save
Dropdown:     ↑↓:Select  Enter:Confirm  Esc:Cancel
On buttons:   ←→:Navigate  Enter:Activate  Tab:Back to fields  Esc:Cancel
```

### Phase 4: List Navigation & Visual Polish (H2, M5, M11)

- **H2:** Make [+] Add new buttons directly focusable (currently reachable via Enter on headers)
- **H5:** DONE — TextList items individually focusable via composite navigation
- **M5:** Add "X of Y" position indicator
- **M7:** DONE — PageDown/PageUp jump by viewport height
- **M11:** Add background highlight or `>` indicator to focused map table entries

### Phase 5: Remaining Medium & Low (M2, M9, M10, M12, L1, L2, L4)

- **M2:** Fix spinner input routing
- **M9:** Unsaved changes confirmation on Esc
- **M10:** Inline filter for map lists
- **M12:** Unify scrollbar colors
- **L1:** Structured controls for ProcessLimits, LspFeature
- **L2:** Make Advanced section collapsible
- **L4:** Command PATH validation

---

## Verification Checklist

### Phase 1
- [ ] Text fields do NOT auto-enter edit mode on navigation
- [ ] Enter activates text editing (cursor appears, label turns blue)
- [ ] Escape exits text editing (cursor gone, label normal, characters no longer insertable)
- [ ] Down/Up navigate fields when not editing
- [ ] Status bar shows "Enter:Edit" when field focused but not editing

### Phase 2 — DONE
- [x] Tab cycles sequentially through all fields, sub-fields, and buttons
- [x] Tab reaches Save, Delete, and Cancel
- [x] Shift+Tab reverses
- [x] 94 settings e2e tests pass

### Phase 3
- [ ] Status bar changes when entering text edit mode
- [ ] Status bar changes when on buttons
- [ ] Status bar changes for dropdown

### Phase 4
- [ ] [+] Add new directly focusable via Down arrow (workaround: Enter on header)
- [x] TextList items individually focusable (composite navigation)
- [ ] Delete key removes focused TextList item
- [x] Page Up/Down works in LSP/Languages lists
- [ ] "X of Y" indicator visible
- [ ] Map entry focus has background highlight
- [x] Composite highlight only on focused sub-row
- [x] Adding new LSP server persists in parent dialog
- [x] TextList auto-accepts edits on Down/Up/Tab
