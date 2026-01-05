# Theme System Usability Improvements

Analysis based on Nielsen Norman Group best practices and hands-on testing.

## Testing Summary

Both manual theme creation (Flow 3) and the interactive Theme Editor (Flow 2) were tested and verified to work. Several usability issues were identified during testing.

---

## 1. Visibility of System Status

### 1.1 Localization placeholders not interpolated
**Severity:** Critical
**Observed:** Title shows `{name}` and status shows `Updated {path}` instead of actual values
**Impact:** Users can't see what theme they're editing or where it's saved
**Fix:** Ensure `editor.t()` properly interpolates `{name}`, `{path}`, `{theme}`, `{error}` placeholders
**Location:** `plugins/theme_editor.ts` - calls to `editor.t()`

### 1.2 No visual indicator for selected field
**Severity:** High
**Observed:** When navigating, there's no highlight showing which field is currently selected
**Impact:** Users can't tell which field will be edited when pressing Enter
**Fix:** Add background highlight or cursor indicator on the active field row
**Location:** `plugins/theme_editor.ts:739` - `applyHighlighting()`

### 1.3 No live preview when editing colors
**Severity:** Medium
**Observed:** Color changes only visible after saving and applying theme
**Impact:** Users must save/apply to see results, slowing iteration
**Fix:** Add real-time preview panel or apply changes temporarily while editing

### 1.4 Theme selector doesn't show theme previews
**Severity:** Medium
**Observed:** Theme list only shows names, not what they look like
**Impact:** Users must try each theme to see it
**Fix:** Add color swatches or mini-preview next to each theme name in selector

---

## 2. Match Between System and Real World

### 2.1 Built-in JSON themes not discoverable
**Severity:** Critical
**Observed:** `themes/` directory has nord.json, dracula.json, solarized-dark.json but they don't appear in theme selector
**Impact:** Users can't use these popular themes; creates confusion about what's available
**Fix:** Scan both `themes/` directory and user themes directory in `available_themes()`
**Location:** `src/view/theme.rs:1057-1085`

```rust
// Current: only scans user themes
// Fix: also scan themes/ directory in the project
```

### 2.2 Inconsistent naming conventions
**Severity:** Low
**Observed:** Hardcoded themes use hyphens (high-contrast), but underscore/hyphen normalization exists
**Impact:** Potential confusion about valid theme names
**Fix:** Document naming convention; auto-normalize on save

### 2.3 "Custom" as default name is unclear
**Severity:** Low
**Observed:** New themes start with name "custom" which is generic
**Impact:** Users might accidentally overwrite or create poorly named themes
**Fix:** Generate unique default names like "custom-1", "custom-2" or prompt for name first

---

## 3. User Control and Freedom

### 3.1 No confirmation before discarding unsaved changes
**Severity:** High
**Observed:** Pressing 'q' immediately discards changes with only a status message
**Impact:** Accidental data loss
**Fix:** Show confirmation dialog: "Discard unsaved changes? [y/n]"
**Location:** `plugins/theme_editor.ts:1237` - `theme_editor_close()`

### 3.2 No undo for color changes
**Severity:** Medium
**Observed:** After editing a color, there's no way to revert to previous value
**Impact:** Users must remember original values or restart
**Fix:** Add undo command (Ctrl+Z) or "Reset field" option

### 3.3 Can't edit existing user themes
**Severity:** High
**Observed:** Theme Editor always starts with default/empty theme, not current theme
**Impact:** To modify an existing theme, users must copy it first
**Fix:** Add option to "Edit Current Theme" or select theme to edit when opening
**Location:** `plugins/theme_editor.ts:1184` - `open_theme_editor()`

### 3.4 No way to delete user themes
**Severity:** Medium
**Observed:** No command to remove custom themes
**Impact:** Users must manually delete files
**Fix:** Add "Delete Theme" command with confirmation

---

## 4. Consistency and Standards

### 4.1 Inconsistent navigation behavior
**Severity:** High
**Observed:** Arrow keys move line-by-line including comment/description lines, but Enter only works on field lines
**Impact:** Confusing - navigation and selection behave differently
**Fix Options:**
- A: Skip non-selectable lines when navigating
- B: Show clear visual distinction for selectable vs non-selectable lines

### 4.2 Mixed keyboard conventions
**Severity:** Medium
**Observed:** Uses both single-key shortcuts (c, n, s) and standard shortcuts (Enter, Tab)
**Impact:** Learning curve; conflicts with text editing mental models
**Fix:** Consider using Ctrl+key combinations for actions (Ctrl+S for save) to match editor conventions

### 4.3 Help information in multiple places
**Severity:** Low
**Observed:** Hints shown in: status bar, footer panel, and `?` command
**Impact:** Information scattered, may miss features
**Fix:** Consolidate help; make `?` the primary help with comprehensive info

---

## 5. Error Prevention

### 5.1 No validation before save
**Severity:** Medium
**Observed:** Can save themes with invalid/incomplete data
**Impact:** Broken theme files
**Fix:** Validate all required fields before saving; highlight missing/invalid fields

### 5.2 Theme name collision not handled gracefully
**Severity:** Medium
**Observed:** Saving with same name as existing theme overwrites without warning
**Impact:** Accidental overwrite of themes
**Fix:** Warn when name matches existing theme; offer "Overwrite" or "Rename"

### 5.3 No guidance on color accessibility
**Severity:** Low
**Observed:** Users can set any colors, including low-contrast combinations
**Impact:** May create unusable/inaccessible themes
**Fix:** Add contrast ratio indicator; warn for WCAG violations (< 4.5:1)

---

## 6. Recognition Rather Than Recall

### 6.1 Color field descriptions only visible on hover/selection
**Severity:** Medium
**Observed:** Must navigate to each field to see what it affects
**Impact:** Hard to find the right field to modify
**Fix:** Keep descriptions always visible, or add "search fields" functionality

### 6.2 No visual mapping between fields and editor regions
**Severity:** Medium
**Observed:** Field names like "popup_border_fg" require understanding the UI
**Impact:** Users must guess which field affects which part of the UI
**Fix:** Add interactive mode: click on UI element to jump to its color field

### 6.3 Named colors list not visible during editing
**Severity:** Low
**Observed:** Users must know color names or use hex values
**Impact:** Discovery of available named colors is difficult
**Fix:** Show named color palette picker, not just text suggestions

---

## 7. Flexibility and Efficiency of Use

### 7.1 No keyboard shortcut to open theme editor
**Severity:** Low
**Observed:** Must use command palette to access
**Impact:** Slower access for frequent users
**Fix:** Add configurable keybinding in default keymap

### 7.2 No batch operations
**Severity:** Low
**Observed:** Must edit colors one at a time
**Impact:** Tedious for comprehensive theme changes
**Fix:** Add "Apply to all similar" (e.g., all backgrounds) or "Adjust brightness/saturation"

### 7.3 No import/export beyond JSON files
**Severity:** Low
**Observed:** Only native JSON format supported
**Impact:** Can't import from other editors or share in other formats
**Fix:** Support importing from VS Code themes, base16, etc.

### 7.4 Copy from builtin requires knowing theme names
**Severity:** Low
**Observed:** Copy dialog shows theme names but no preview
**Impact:** Hard to choose base theme
**Fix:** Show preview of each builtin theme when copying

---

## 8. Aesthetic and Minimalist Design

### 8.1 Verbose field display with redundant whitespace
**Severity:** Low
**Observed:** Each field has comment line + field line + blank line = 3 lines per field
**Impact:** Low information density; excessive scrolling
**Fix:** Option to show compact view (field: value on single line, tooltip for description)

### 8.2 Section expand/collapse state not persisted
**Severity:** Low
**Observed:** Sections reset to default expansion on reopen
**Impact:** Users must re-collapse sections they don't need each time
**Fix:** Remember expansion state in config

---

## 9. Help Users Recognize, Diagnose, and Recover from Errors

### 9.1 Generic error messages
**Severity:** Medium
**Observed:** "Invalid color format" without showing what was entered
**Impact:** Hard to understand what went wrong
**Fix:** Show specific error: "Invalid color '#GGG': hex values must be 0-9 or A-F"
**Location:** `plugins/theme_editor.ts:860` - `parseColorInput()`

### 9.2 No error recovery for corrupted theme files
**Severity:** Medium
**Observed:** If theme JSON is malformed, unclear what happens
**Impact:** Users may lose work
**Fix:** Show specific JSON parse error with line number; offer to open in editor

---

## 10. Help and Documentation

### 10.1 No in-app tutorial or onboarding
**Severity:** Low
**Observed:** Users must discover features through trial and error
**Impact:** Steep learning curve
**Fix:** Add first-run tooltip tour or "Getting Started" popup

### 10.2 No documentation on theme file format
**Severity:** Low
**Observed:** Users creating manual JSON must examine existing files
**Impact:** Barrier to advanced customization
**Fix:** Add "Export as documented template" or link to docs from editor

### 10.3 `?` help is text-only
**Severity:** Low
**Observed:** Help shows keyboard shortcuts but no explanations
**Impact:** Users know keys but not capabilities
**Fix:** Show richer help panel with examples and descriptions

---

## Priority Matrix

| Priority | Issue | Impact | Effort |
|----------|-------|--------|--------|
| **P0 - Critical** | 1.1 Localization placeholders | High | Low |
| **P0 - Critical** | 2.1 Built-in JSON themes missing | High | Low |
| **P1 - High** | 1.2 No visual field selection | High | Medium |
| **P1 - High** | 3.1 No discard confirmation | High | Low |
| **P1 - High** | 3.3 Can't edit existing themes | High | Medium |
| **P1 - High** | 4.1 Inconsistent navigation | Medium | Medium |
| **P2 - Medium** | 1.3 No live preview | Medium | High |
| **P2 - Medium** | 3.2 No undo | Medium | Medium |
| **P2 - Medium** | 5.2 Theme name collision | Medium | Low |
| **P2 - Medium** | 6.2 No visual mapping | Medium | High |
| **P2 - Medium** | 9.1 Generic error messages | Medium | Low |
| **P3 - Low** | 3.4 Delete themes | Medium | Low |
| **P3 - Low** | 5.3 Accessibility guidance | Low | Medium |
| **P3 - Low** | 7.2 Batch operations | Low | High |
| **P3 - Low** | 8.1 Compact view | Low | Medium |

---

## Quick Wins (High Impact, Low Effort)

These improvements provide significant usability gains with minimal development effort:

1. **Fix localization interpolation** - Shows actual theme name/path instead of `{name}`
2. **Add built-in JSON themes to selector** - Scan `themes/` directory in `available_themes()`
3. **Add discard confirmation dialog** - Prevent accidental data loss when closing with unsaved changes
4. **Highlight selected field row** - Clear visual feedback for current selection
5. **Warn on theme name collision** - Prevent accidental overwrites
6. **Improve error messages** - Include the invalid input in error messages

---

## Implementation Notes

### For Issue 2.1 (Built-in JSON themes)

```rust
// In src/view/theme.rs, modify available_themes()
pub fn available_themes() -> Vec<String> {
    let mut themes: Vec<String> = vec![
        "dark".to_string(),
        "light".to_string(),
        "high-contrast".to_string(),
        "nostalgia".to_string(),
    ];

    // ADD: Scan built-in themes directory
    if let Ok(entries) = std::fs::read_dir("themes") {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.extension().is_some_and(|ext| ext == "json") {
                if let Some(stem) = path.file_stem() {
                    let name = stem.to_string_lossy().to_string();
                    if !themes.iter().any(|t| t == &name) {
                        themes.push(name);
                    }
                }
            }
        }
    }

    // Existing: Scan user themes directory
    // ...
}
```

### For Issue 3.1 (Discard confirmation)

```typescript
// In plugins/theme_editor.ts, modify theme_editor_close()
globalThis.theme_editor_close = function(): void {
  if (!state.isOpen) return;

  if (state.hasChanges) {
    // Show confirmation prompt instead of immediate close
    editor.startPrompt("Discard unsaved changes? (y/n)", "theme-discard-confirm");
    return;
  }

  doClose();
};
```

### For Issue 1.2 (Field selection highlight)

```typescript
// In applyHighlighting(), add highlight for current field
const currentField = getFieldAtCursor();
if (currentField) {
  // Add background highlight to the current field's line
  const fieldEntry = entries.find(e => e.properties?.path === currentField.path);
  if (fieldEntry) {
    addColorOverlay(bufferId, fieldByteOffset, fieldByteOffset + fieldLen,
      [60, 60, 80], // Subtle highlight color
      false // not bold
    );
  }
}
```
