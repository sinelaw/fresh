# UX Design: Search for Next Occurrence of Current Selection

## Feature Request Summary

**GitHub Issue #489**: Allow searching for the next occurrence of the currently selected text using a keyboard shortcut, without opening the find panel.

## Research: How Other Editors Implement This

### Visual Studio (Original)
- **Ctrl+F3**: Find Next Selected - searches for the next occurrence of selected text
- **Ctrl+Shift+F3**: Find Previous Selected - searches for the previous occurrence
- The selected text becomes the active search term
- Subsequent F3/Shift+F3 navigates through matches

### VSCode
VSCode has two related but distinct features:

1. **Ctrl+F3** (`editor.action.nextSelectionMatchFindAction`):
   - Searches for next occurrence of selection
   - Always finds partial word matches
   - Targeted at users coming from Visual Studio

2. **Ctrl+D** (`editor.action.addSelectionToNextFindMatch`):
   - Adds a cursor at the next occurrence (multi-cursor)
   - Context-dependent matching:
     - With explicit selection: finds partial matches
     - With cursor on word (no selection): finds whole-word matches only
   - Targeted at users coming from Sublime Text

3. **Ctrl+Shift+L** (`editor.action.selectHighlights`):
   - Selects ALL occurrences at once

### Sublime Text
- **Ctrl+D** (`quick_add_next`): Selects word under cursor, then finds/selects next occurrence
- **Ctrl+K, Ctrl+D**: Skip current occurrence, find next
- **Alt+F3**: Select ALL occurrences
- The find behavior is tied to the find panel internally

### IntelliJ IDEA
- **F3 / Cmd+G**: Find Next Occurrence (after Ctrl+F search)
- **Ctrl+G / Alt+J**: Add Selection for Next Occurrence (multi-cursor)
- Whole-word matching is context-dependent

## Proposed Design for Fresh Editor

### Overview

Implement a "quick find" feature that searches for the current selection without opening the find panel. The search term integrates with the existing search system so subsequent F3/Shift+F3 navigation works seamlessly.

### Keyboard Shortcuts

| Shortcut | Action | Description |
|----------|--------|-------------|
| **Ctrl+F3** | `find_selection_next` | Find next occurrence of selection |
| **Ctrl+Shift+F3** | `find_selection_previous` | Find previous occurrence of selection |

These shortcuts align with Visual Studio, VS Code, and the user's expectations mentioned in the issue.

### Detailed Behavior

#### 1. Basic Flow

```
User selects text → Presses Ctrl+F3 → Cursor jumps to next occurrence
                                    → Selection text becomes search term
                                    → Status bar shows "Match X of Y"
```

#### 2. Initial State Requirements

| Condition | Behavior |
|-----------|----------|
| Has selection | Use selected text as search term |
| No selection, cursor on word | Auto-expand to word under cursor (like Ctrl+D in VSCode) |
| No selection, cursor on whitespace/punctuation | Show status message "No text to search" |

#### 3. Search Options

The quick find should use **sensible defaults** that differ from the search panel:

| Option | Default for Ctrl+F3 | Rationale |
|--------|---------------------|-----------|
| Case Sensitive | **OFF** | Most "quick find" use cases are case-insensitive |
| Whole Word | **OFF** | User selected exactly what they want to find |
| Regex | **OFF** | Literal text search (escape special chars) |
| Wrap | **ON** | Seamless navigation through document |

**Note**: If the user had previously used the search panel with specific options (e.g., case-sensitive ON), those options should be preserved. Ctrl+F3 only sets the search term, not the options.

#### 4. Integration with Existing Search

After using Ctrl+F3:
- The search term is stored in `SearchState.query`
- All matches are computed and stored in `SearchState.matches`
- `F3` and `Shift+F3` continue to navigate through matches
- `Ctrl+F` opens the find panel pre-filled with the search term
- The search term is added to search history

This matches the behavior described in the feature request:
> "once you searched a 'selection occurrence', the text of the selection will be treated as if you searched for text regularly"

#### 5. Cursor and Selection After Search

| Action | Cursor Position | Selection |
|--------|-----------------|-----------|
| Find next (Ctrl+F3) | Start of match | Match is selected |
| Find previous (Ctrl+Shift+F3) | Start of match | Match is selected |

Selecting the match allows the user to:
- Immediately see what was found
- Press Ctrl+F3 again to continue (selection is maintained)
- Start typing to replace the match
- Press Escape to deselect and continue editing

#### 6. Wrap-Around Behavior

- When reaching the last match, Ctrl+F3 wraps to the first match
- Status bar shows: "Match 1 of N (wrapped)"
- When reaching the first match, Ctrl+Shift+F3 wraps to the last match

#### 7. Edge Cases

| Scenario | Behavior |
|----------|----------|
| Selection contains newlines | Use full selection (multi-line search) |
| Selection is empty string | Treat as "no selection" - expand to word |
| No matches found | Show "No matches for 'text'" in status bar |
| Single match (the selection itself) | Show "This is the only match" |
| Very long selection (>100 chars) | Still search, but truncate in status messages |

### User Interface Feedback

#### Status Bar Messages

```
Searching...                          (brief, during search)
Match 3 of 15                         (normal navigation)
Match 1 of 15 (wrapped)               (wrapped around)
No matches for 'searchterm'           (no results)
This is the only match                (single occurrence)
No text to search                     (no selection/word)
```

#### Visual Feedback

1. **Highlight all matches** in the viewport (using existing search highlight system)
2. **Current match** uses a distinct "current match" color (same as existing search)
3. **Smooth scroll** to center the match in the viewport if off-screen

### Implementation Notes

#### New Actions Required

```rust
// In src/input/keybindings.rs - Action enum
FindSelectionNext,      // Ctrl+F3
FindSelectionPrevious,  // Ctrl+Shift+F3
```

#### Core Functions

```rust
// In src/app/render.rs or similar

/// Finds next occurrence of current selection or word under cursor
fn find_selection_next(&mut self) {
    // 1. Get search text from selection or word-at-cursor
    // 2. Escape regex special characters (literal search)
    // 3. Call perform_search() with the text
    // 4. Call find_next() to move to next match
    // 5. Select the match
}

/// Finds previous occurrence of current selection or word under cursor
fn find_selection_previous(&mut self) {
    // Similar to above, but calls find_previous()
}
```

#### Integration with Existing Code

The implementation should reuse existing search infrastructure:

1. **`SearchState`** - Store the query and matches (already exists)
2. **`perform_search()`** - Compute all matches (already exists)
3. **`find_next()` / `find_previous()`** - Navigate matches (already exists)
4. **`update_search_highlights()`** - Visual feedback (already exists)

The only new code needed is:
- The action handlers that extract selection/word and call existing functions
- Keybinding entries in `default.json`

### Keybinding Configuration

Add to `/keymaps/default.json`:

```json
{
  "comment": "Quick find - search selection next",
  "key": "F3",
  "modifiers": ["ctrl"],
  "action": "find_selection_next",
  "args": {},
  "when": "normal"
},
{
  "comment": "Quick find - search selection previous",
  "key": "F3",
  "modifiers": ["ctrl", "shift"],
  "action": "find_selection_previous",
  "args": {},
  "when": "normal"
}
```

### Comparison with Existing Ctrl+D

Fresh already has `Ctrl+D` bound to `add_cursor_next_match` (multi-cursor). The proposed Ctrl+F3 is complementary:

| Shortcut | Action | Multi-cursor? |
|----------|--------|---------------|
| Ctrl+D | Add cursor at next match | Yes (cumulative) |
| Ctrl+F3 | Jump to next match | No (navigation only) |

Both features are useful:
- **Ctrl+D**: When you want to edit multiple occurrences simultaneously
- **Ctrl+F3**: When you want to review/navigate occurrences one by one

### Future Considerations

1. **Ctrl+Shift+L** - Select all occurrences (like VSCode) - could be added later
2. **Configurable defaults** - User setting for case-sensitivity default
3. **Search history integration** - Ctrl+F3 searches could appear in search history

## Summary

This design provides:
- Standard keyboard shortcuts (Ctrl+F3 / Ctrl+Shift+F3) matching user expectations
- Seamless integration with existing search (F3/Shift+F3 navigation)
- Smart defaults (word expansion when no selection)
- Clear status feedback
- Minimal new code by reusing existing search infrastructure

## Sources

- [VSCode Default Keyboard Shortcuts Reference](https://code.visualstudio.com/docs/reference/default-keybindings)
- [VSCode Issue #76960: Find Next Selection behavior inconsistency](https://github.com/microsoft/vscode/issues/76960)
- [Sublime Text Multiple Selection with Keyboard](https://www.sublimetext.com/docs/multiple_selection_with_the_keyboard.html)
- [Visual Studio Default Keyboard Shortcuts](https://github.com/MicrosoftDocs/visualstudio-docs/blob/main/docs/ide/default-keyboard-shortcuts-in-visual-studio.md)
- [Common Shortcuts in IntelliJ IDEA](https://www.baeldung.com/intellij-idea-shortcuts)
