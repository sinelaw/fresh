# Diff Plugin Audit Document

**Date:** 2024-12-19  
**Last Updated:** 2024-12-19  
**Plugin:** `plugins/diff.ts`  
**Status:** Functional - Core Features Complete, Keyboard Shortcuts Need Review

---

## Executive Summary

The diff plugin provides comprehensive diff functionality for the Fresh editor, supporting directory comparisons, git diffs, and commit comparisons. The core functionality is implemented and functional, with several enhancement opportunities identified.

### ⚠️ Critical Issue: Keyboard Shortcuts

**Status:** BROKEN - Single-letter shortcut interferes with editing

**Problem:** The `s` key shortcut for toggling side-by-side view gets typed into the buffer when users are editing, breaking the feature.

**Solution:** See "Keyboard Shortcuts Analysis" section below for detailed recommendations. 

**Recommended Fix:** Use `Ctrl+D` for toggle (uses modifier to prevent typing, mode-specific to avoid conflicts, Mac compatible).

---

## ✅ Completed Features

### 1. Core Infrastructure
- [x] Plugin structure and state management
- [x] Virtual buffer integration for diff display
- [x] Mode definitions for diff-list and diff-view
- [x] Command registration in command palette
- [x] Event handlers for prompt interactions

### 2. Directory Diff
- [x] Compare two directories recursively
- [x] Detect added, deleted, and modified files
- [x] File list display with status indicators
- [x] File-by-file diff generation
- [x] Fallback diff generation when system `diff` unavailable

### 3. Git Diff Operations
- [x] Git repository detection
- [x] Compare working directory with branch/commit/tag
- [x] Parse git diff output (`--name-status`)
- [x] Extract file change statistics (additions/deletions)
- [x] Support for empty ref (defaults to HEAD)

### 4. Commit Diff
- [x] Compare two commits/branches/tags
- [x] Multi-step prompt flow (left then right)
- [x] File statistics calculation
- [x] Unified diff display

### 5. User Interface
- [x] File list view with status symbols (+, -, ~, →, =)
- [x] Clickable file entries (Enter to view diff)
- [x] Unified diff view for individual files
- [x] Navigation (q to go back/close)
- [x] Status messages and help text
- [x] Text properties for file metadata

### 6. Prompt System Integration
- [x] Directory diff prompts (left/right)
- [x] Git diff prompt (single ref)
- [x] Commit diff prompts (left/right)
- [x] Prompt cancellation handling
- [x] Event handler registration

---

## ⚠️ Known Limitations & Issues

### 1. Side-by-Side Diff View
**Status:** ✅ Implemented (Keyboard Shortcuts Need Fix)  
**Impact:** Medium  
**Description:** Side-by-side view is implemented but keyboard shortcut conflicts with editing.

**Current Behavior:**
- ✅ Side-by-side view implemented with two buffers
- ✅ Line alignment and parsing working
- ✅ Toggle between unified and side-by-side
- ✗ Keyboard shortcut `s` interferes with editing (BROKEN)

**Required Work:**
- ⚠️ Fix keyboard shortcut (see Keyboard Shortcuts Analysis section)
- ⚠️ Synchronize scrolling between left/right panes (future enhancement)
- ⚠️ Improve highlighting in side-by-side view

### 2. Syntax Highlighting in Diff View
**Status:** Partially Implemented  
**Impact:** Low  
**Description:** Diff view uses text properties for line types but doesn't apply color overlays.

**Current Behavior:**
- Text properties set (added, deleted, context, header)
- No visual color highlighting applied

**Required Work:**
- Apply color overlays based on text properties
- Use COLORS constants (added, deleted, modified, context, header)
- Implement `applyDiffHighlighting()` function

### 3. Remote Diff Support
**Status:** Not Explicitly Supported  
**Impact:** Low  
**Description:** Git diff can compare with remotes, but there's no explicit remote selection UI.

**Current Behavior:**
- Users can manually type remote refs (e.g., `origin/main`)
- No autocomplete or remote listing

**Required Work:**
- Fetch and list available remotes
- Provide suggestions for remote branches
- Validate remote refs before diffing

### 4. Large File Handling
**Status:** No Special Handling  
**Impact:** Medium  
**Description:** Large files may cause performance issues or memory problems.

**Current Behavior:**
- Reads entire file contents into memory
- No size limits or chunking

**Required Work:**
- Add file size checks
- Implement streaming/chunked diff for large files
- Show warning for very large files
- Option to skip binary files

### 5. Binary File Detection
**Status:** Not Implemented  
**Impact:** Low  
**Description:** Binary files are not detected or handled specially.

**Current Behavior:**
- Attempts to diff binary files as text
- May show garbled output

**Required Work:**
- Detect binary files (magic bytes, git attributes)
- Show "Binary files differ" message
- Option to show hex diff or skip

### 6. Rename Detection (Directory Diff)
**Status:** Not Implemented  
**Impact:** Low  
**Description:** Directory diff doesn't detect file renames, only shows as deleted+added.

**Current Behavior:**
- Renamed files appear as separate deleted and added entries
- No similarity matching

**Required Work:**
- Implement similarity detection (content hash, filename similarity)
- Show renamed files with old→new path
- Use git's rename detection algorithm if available

### 7. Error Handling
**Status:** Basic  
**Impact:** Medium  
**Description:** Some error cases may not be handled gracefully.

**Current Behavior:**
- Basic try/catch blocks
- Error messages shown in status bar
- Some operations may fail silently

**Required Work:**
- More comprehensive error handling
- User-friendly error messages
- Recovery from partial failures
- Validation of paths and git refs

### 8. Performance Optimization
**Status:** Not Optimized  
**Impact:** Low  
**Description:** Directory comparison may be slow for large directories.

**Current Behavior:**
- Sequential file reading
- No caching
- No progress indicators

**Required Work:**
- Parallel file reading where possible
- Cache file contents during comparison
- Progress indicators for long operations
- Early exit on user cancellation

---

## 🔄 Partially Implemented Features

### 1. File Statistics
**Status:** Implemented for Git Diffs Only  
**Description:** Shows additions/deletions for git diffs but not for directory diffs.

**Current:**
- ✅ Git diff: Shows (+X/-Y) statistics
- ❌ Directory diff: No statistics shown

**Required:**
- Calculate line differences for directory diffs
- Show statistics in file list

### 2. Diff Highlighting
**Status:** Structure Ready, Not Applied  
**Description:** Text properties are set but visual highlighting not applied.

**Current:**
- ✅ Text properties set (type: "added", "deleted", etc.)
- ❌ Color overlays not applied

**Required:**
- Implement `applyDiffHighlighting()` function
- Apply overlays based on text properties
- Use defined COLORS constants

---

## 📋 Outstanding Work Items

### High Priority
1. **Fix Keyboard Shortcuts** - Replace single-letter shortcuts with proper key combinations
   - Replace `s` with `F7` or `]` (see Keyboard Shortcuts Analysis)
   - Update mode definitions
   - Update help text in status messages
   - Test in various terminal environments

2. **Apply Visual Highlighting** - Implement color overlays for diff lines
   - Use existing COLORS constants
   - Apply based on text properties
   - Enhance readability

3. **Error Handling Improvements** - Add comprehensive error handling
   - Validate all inputs
   - Handle edge cases (missing files, permission errors)
   - Provide clear error messages

4. **Large File Handling** - Add size checks and warnings
   - Check file size before reading
   - Warn user for large files
   - Option to skip or limit diff size

### Medium Priority
4. **Side-by-Side View Enhancements** - Improve existing side-by-side view
   - ✅ Two-pane layout implemented
   - ⚠️ Synchronize scrolling between panes
   - ⚠️ Improve highlighting accuracy
   - Better for large changes

5. **Directory Diff Statistics** - Add line count statistics
   - Calculate additions/deletions
   - Display in file list
   - Match git diff behavior

6. **Binary File Detection** - Handle binary files gracefully
   - Detect binary files
   - Show appropriate message
   - Skip or show hex diff option

### Low Priority
7. **Remote Branch Suggestions** - Improve remote diff UX
   - List available remotes
   - Autocomplete remote branches
   - Validate remote refs

8. **Rename Detection** - Detect file renames in directory diff
   - Content similarity matching
   - Filename similarity
   - Show as rename instead of delete+add

9. **Performance Optimizations** - Improve speed for large directories
   - Parallel file operations
   - Caching
   - Progress indicators

10. **Filtering & Search** - Add file filtering capabilities
    - Filter by status (added/deleted/modified)
    - Search file paths
    - Sort options

---

## 🧪 Testing Status

### Manual Testing
- [x] Directory diff with small directories
- [x] Git diff with working directory
- [x] Commit diff between two commits
- [x] File list navigation
- [x] File diff viewing
- [x] Navigation (back/close)
- [x] Prompt cancellation

### Not Tested
- [ ] Large directory comparisons
- [ ] Large file diffs
- [ ] Binary file handling
- [ ] Remote branch diffs
- [ ] Error conditions (missing files, permissions)
- [ ] Edge cases (empty directories, identical files)

### Automated Testing
- [ ] Unit tests for diff parsing
- [ ] Unit tests for git operations
- [ ] Integration tests for plugin
- [ ] E2E tests for user workflows

---

## 📚 Documentation Status

### Completed
- [x] Plugin header documentation
- [x] Function comments
- [x] Type definitions
- [x] Usage examples in code

### Missing
- [ ] User guide/documentation
- [ ] Command reference
- [ ] Examples and screenshots
- [ ] Troubleshooting guide

---

## 🔗 Integration Status

### Editor Integration
- [x] Command palette registration
- [x] Mode definitions
- [x] Event handler registration
- [x] Virtual buffer creation
- [x] Status messages

### Git Integration
- [x] Git repository detection
- [x] Git diff commands
- [x] Git ref parsing
- [x] File statistics from git

### File System Integration
- [x] Directory reading
- [x] File reading
- [x] Path operations
- [x] File existence checks

---

## 📊 Code Quality Metrics

### Lines of Code
- **Total:** ~991 lines
- **Core Logic:** ~600 lines
- **UI/Display:** ~200 lines
- **Git Operations:** ~150 lines
- **Directory Operations:** ~100 lines

### Code Organization
- ✅ Well-structured with clear sections
- ✅ Type definitions present
- ✅ Constants defined
- ✅ State management implemented
- ⚠️ Some functions could be split further
- ⚠️ Error handling could be more consistent

### Type Safety
- ✅ TypeScript types defined
- ✅ Interface definitions present
- ⚠️ Some `any` types may exist (needs review)

---

## 🎯 Recommendations

### Immediate Actions
1. **Apply visual highlighting** - Quick win to improve UX
2. **Add error handling** - Improve robustness
3. **Test with real-world scenarios** - Identify edge cases

### Short-term (1-2 weeks)
1. **Implement side-by-side view** - Major UX improvement
2. **Add directory diff statistics** - Feature parity with git diff
3. **Binary file detection** - Prevent garbled output

### Long-term (1+ months)
1. **Performance optimizations** - For large repositories
2. **Advanced features** - Filtering, search, sorting
3. **Testing suite** - Automated tests for reliability

---

## 📝 Notes

### Design Decisions
- **Unified diff format chosen** for initial implementation (simpler, standard)
- **Virtual buffers used** for consistency with other plugins (git_log, git_blame)
- **Prompt-based input** for flexibility (supports any git ref format)
- **Text properties** for metadata (enables future enhancements)

### Dependencies
- Git must be installed and in PATH
- System `diff` command optional (fallback implemented)
- Editor virtual buffer system
- Editor prompt/event system

### Compatibility
- Works with any git repository
- Supports standard git ref formats
- Compatible with editor's plugin system
- No external npm dependencies

---

## ✅ Acceptance Criteria Status

### Core Requirements
- [x] Diff two directories ✓
- [x] Diff local with remote/branch ✓
- [x] Diff local with commit/tag ✓
- [x] Diff two commits ✓
- [x] Show different files ✓
- [x] Click on diff lines to show differences ✓

### Enhancement Requirements
- [x] Side-by-side diff view ✅ (keyboard shortcuts need fix)
- [ ] Syntax highlighting in diff view (partially implemented)
- [ ] Binary file handling
- [ ] Large file optimization

---

## 🎉 Summary

The diff plugin is **functionally complete** for the core requirements. All primary features work as specified:
- Directory diff ✓
- Git diff (local vs remote/branch/commit) ✓
- Commit diff ✓
- File list with clickable entries ✓
- Individual file diff viewing ✓

The plugin is ready for use, with several enhancement opportunities identified for future improvements. The codebase is well-structured and follows the patterns established by other plugins in the Fresh editor.

**Overall Status: Production Ready (with known enhancement opportunities)**

---

## ⌨️ Keyboard Shortcuts Analysis

### Current Implementation (BROKEN)

**Issue:** Single-letter shortcuts interfere with normal editing.

**Current Shortcuts:**
- `q` - Close/quit diff view ✓ (works, standard pattern)
- `s` - Toggle side-by-side/unified view ✗ (BROKEN - interferes with editing)
- `Return` - View file diff ✓ (works, standard pattern)

**Problem:**
- Single letter `s` gets typed into the buffer when user is editing
- **CRITICAL:** ANY single key (including brackets `]`, `[`) will result in typing in the pane
- **REQUIREMENT:** Must use key combinations with modifiers (Ctrl, Alt, Shift) to prevent editing

### Research: IntelliJ IDEA Diff Shortcuts

**IntelliJ IDEA Standard Diff Shortcuts:**
- `F7` / `Shift+F7` - Navigate to next/previous change
- `Ctrl+D` - Show diff (opens diff dialog)
- `Ctrl+Shift+D` - Compare with Clipboard
- `Ctrl+Alt+Shift+D` - Compare with Branch
- **View Toggle:** Typically accessed via toolbar button or context menu
- **Side-by-side toggle:** Usually via View menu or toolbar, not a standard keyboard shortcut

**Note:** IntelliJ doesn't have a standard keyboard shortcut for toggling unified/side-by-side view. It's typically done via UI buttons.

### Research: Other Editors

**VSCode:**
- `F7` - Go to next diff
- `Shift+F7` - Go to previous diff
- View toggle via UI buttons (no standard keyboard shortcut)

**Git Tools:**
- `]` / `[` - Navigate between hunks (common in git tools)
- `j` / `k` - Navigate lines (vim-style)
- View toggle typically via UI

### Existing Shortcuts in Fresh Editor

**Available Patterns:**
- `q` - Close/quit (used by: git_log, git_blame, merge_conflict, search_replace, etc.) ✓
  - ⚠️ **NOTE:** Single key - only works in read-only/special modes, not in editable panes
- `Return` / `Enter` - Action/select (used by: git_log, search_replace, diagnostics, etc.) ✓
  - ⚠️ **NOTE:** Special key - works in special modes
- `Escape` - Cancel/close (used by: git_log, git_blame, search_replace, etc.) ✓
  - ⚠️ **NOTE:** Special key - works in special modes
- `Ctrl+key` - Special actions (used by: merge_conflict for C-j, C-k, C-u, C-t, C-b, C-s, C-q)
  - ✅ **REQUIRED:** Must use modifier keys to prevent typing in editable panes
- `F8` / `Shift+F8` - Error navigation (used by: diagnostics)
- `]` / `[` - Navigation (used by: merge_conflict for conflict navigation in read-only mode)
  - ⚠️ **NOTE:** Single keys only work in read-only modes, NOT in editable panes

**Conflicts to Avoid:**
- Single letters without modifiers (interfere with editing)
- `Ctrl+S` - Save (already used globally)
- `Ctrl+Q` - Quit (already used globally)
- `Ctrl+F` - Search (already used globally)
- `Ctrl+R` - Replace (already used globally)
- `Ctrl+D` - Add cursor next match (already used globally)
- `Alt+S` - Selection menu (already used globally)
- `F8` - Jump to next error (already used in normal mode)
- `Shift+F8` - Jump to previous error (already used in normal mode)
- `F10` - Menu activate (already used globally)
- `F1` - Show help (already used in normal mode)
- `F2` - LSP rename (already used in normal mode)
- `F3` - Find next (already used in normal mode)
- `F5` - Stop macro recording (already used in normal mode)
- `F9` - Toggle keyboard capture (already used in terminal mode)
- `F12` - LSP goto definition (already used in normal mode)
- `F7` - ✅ **AVAILABLE** (not currently used)
- `Shift+F7` - ✅ **AVAILABLE** (not currently used)
- `F4` - ✅ **AVAILABLE** (not currently used)
- `F6` - ✅ **AVAILABLE** (not currently used)
- `F11` - ✅ **AVAILABLE** (not currently used)

### Proposed Keyboard Shortcuts

#### Option 1: Function Keys (Not Recommended - Mac Issues)
**Rationale:** Function keys are commonly used for navigation in diff views, but have Mac compatibility issues.

| Action | Mac Shortcut | Windows/Linux Shortcut | Rationale |
|--------|--------------|----------------------|-----------|
| Toggle side-by-side/unified | `Fn+F7` ⚠️ | `F7` ✅ | Common in diff tools for navigation |
| Next change | `Fn+F7` ⚠️ | `F7` ✅ | Standard IntelliJ/VSCode pattern |
| Previous change | `Fn+Shift+F7` ⚠️ | `Shift+F7` ✅ | Standard IntelliJ/VSCode pattern |
| Close/Quit | `q` ✅ | `q` ✅ | Keep existing, standard pattern |
| View file diff | `Return` ✅ | `Return` ✅ | Keep existing, standard pattern |

**Platform-Specific Notes:**
- **Mac:** Requires `Fn` modifier (unreliable, may conflict with system functions)
- **Windows/Linux:** Direct function keys work reliably

**Pros:**
- ✅ No interference with editing (uses function keys)
- ✅ Follows IntelliJ/VSCode conventions
- ✅ Function keys are dedicated for navigation
- ✅ Easy to remember (F7 = diff navigation)
- ✅ Works well on Windows/Linux

**Cons:**
- ⚠️ **Mac requires Fn key** - Function keys don't work directly on Mac
- ⚠️ **Mac system mappings** - F-keys often mapped to brightness, volume, etc.
- ⚠️ **Cross-platform issues** - Inconsistent behavior across platforms
- ⚠️ F7 might be used for other navigation in future
- ⚠️ Some terminals may not support function keys well
- ❌ **Not recommended for Mac users**

#### Option 2: Ctrl+Key Combinations (RECOMMENDED)
**Rationale:** Follows merge_conflict plugin pattern, REQUIRED to prevent typing in panes.

| Action | Mac Shortcut | Windows/Linux Shortcut | Conflict Check | Solution |
|--------|--------------|----------------------|----------------|----------|
| Toggle side-by-side/unified | `Ctrl+D` ✅ | `Ctrl+D` ✅ | "add cursor next match" in normal mode | Mode-specific binding (only in diff-view mode) |
| Toggle side-by-side/unified (alt) | `Ctrl+Shift+D` ✅ | `Ctrl+Shift+D` ✅ | No conflict | Alternative if Ctrl+D doesn't work |
| Next change | `Ctrl+N` ✅ | `Ctrl+N` ✅ | "new file" in normal mode | Mode-specific binding (only in diff-view mode) |
| Previous change | `Ctrl+P` ✅ | `Ctrl+P` ✅ | "command palette" in normal mode | Mode-specific binding (only in diff-view mode) |
| Close/Quit | `q` ✅ | `q` ✅ | Works in special modes | Keep existing |
| View file diff | `Return` ✅ | `Return` ✅ | Works in special modes | Keep existing |

**Platform-Specific Notes:**
- **Mac:** `Ctrl` key works reliably (no Fn needed, different from Cmd)
- **Windows/Linux:** `Ctrl` key is standard modifier
- **Both:** Same shortcuts work identically across platforms

**Pros:**
- ✅ **REQUIRED:** Uses modifier keys to prevent typing in panes
- ✅ **Cross-platform:** Same shortcuts work on Mac, Linux, Windows
- ✅ Follows merge_conflict pattern (`C-j`, `C-k` for navigation)
- ✅ Mode-specific bindings prevent conflicts with global shortcuts
- ✅ Intuitive (D for diff, N for next, P for previous)
- ✅ Mac compatible (Ctrl works without Fn key, unlike function keys)

**Cons:**
- ⚠️ Requires two hands (but necessary to prevent typing)
- ⚠️ Some combinations conflict globally, but mode-specific bindings solve this

#### Option 3: Alt+Key Combinations
**Rationale:** Alt keys are used for menus, could work for diff-specific actions.

| Action | Mac Shortcut | Windows/Linux Shortcut | Rationale |
|--------|--------------|----------------------|-----------|
| Toggle side-by-side/unified | `Option+D` ⚠️ | `Alt+D` ⚠️ | D for "diff" |
| Next change | `Option+N` ⚠️ | `Alt+N` ⚠️ | N for "next" |
| Previous change | `Option+P` ⚠️ | `Alt+P` ⚠️ | P for "previous" |
| Close/Quit | `q` ✅ | `q` ✅ | Keep existing |
| View file diff | `Return` ✅ | `Return` ✅ | Keep existing |

**Platform-Specific Notes:**
- **Mac:** Uses `Option` key (same as Alt, but labeled differently)
- **Windows/Linux:** Uses `Alt` key
- **Both:** Same physical key, different labels

**Pros:**
- ✅ Works in all terminals
- ✅ Single modifier key
- ✅ Alt/Option keys less commonly used for editing

**Cons:**
- ⚠️ Alt/Option+D might conflict with menu navigation
- ⚠️ Alt/Option+N, Alt/Option+P might conflict with other features
- ⚠️ Less standard for diff navigation
- ⚠️ Different key labels (Option vs Alt) may confuse users

#### Option 4: Bracket Keys (NOT VIABLE - Single Keys Cause Typing)
**Rationale:** Follows merge_conflict plugin pattern, but single keys cause typing in panes.

| Action | Mac Shortcut | Windows/Linux Shortcut | Status |
|--------|--------------|----------------------|--------|
| Toggle side-by-side/unified | `]` | `]` | ❌ Single key causes typing |
| Next change | `Shift+]` or `}` | `Shift+]` or `}` | ❌ Single key causes typing |
| Previous change | `[` | `[` | ❌ Single key causes typing |

**Platform-Specific Notes:**
- **Mac:** Same issue - single keys cause typing
- **Windows/Linux:** Same issue - single keys cause typing
- **Both:** Identical problem across all platforms

**Issue:**
- ❌ **ANY single key (including brackets) results in typing in the current pane**
- ❌ **REQUIREMENT:** Must use key combinations with modifiers (Ctrl, Alt, Shift)
- ❌ Not viable for diff view panes on any platform

**Note:** merge_conflict uses single keys in read-only navigation panels, but even those may cause issues. The editable "merge-result" mode uses `C-j`, `C-k` (Ctrl+key) combinations.

### Recommendation: Option 2 (Ctrl+Key) - REQUIRED

**⚠️ CRITICAL REQUIREMENT:** ANY single key (including brackets, letters, etc.) results in typing in the current pane. Must use key combinations with modifiers.

**Primary Recommendation: Ctrl+Key Combinations (Required)**

| Action | Mac Shortcut | Windows/Linux Shortcut | Notes |
|--------|--------------|----------------------|-------|
| Toggle side-by-side/unified | `Ctrl+D` ✅ | `Ctrl+D` ✅ | Mode-specific, won't conflict with "add cursor" |
| Toggle (alternative) | `Ctrl+Shift+D` ✅ | `Ctrl+Shift+D` ✅ | Alternative if Ctrl+D conflicts |
| Navigate to next change | `Ctrl+N` ✅ | `Ctrl+N` ✅ | Mode-specific, won't conflict with "new file" |
| Navigate to previous change | `Ctrl+P` ✅ | `Ctrl+P` ✅ | Mode-specific, won't conflict with "command palette" |
| Close/quit | `q` ✅ | `q` ✅ | Works in special modes |
| Close/quit (alternative) | `Escape` ✅ | `Escape` ✅ | Alternative to q |
| View file diff | `Return` ✅ | `Return` ✅ | Works in special modes |

**Conflict Resolution:**
- ⚠️ `Ctrl+D` is "add cursor next match" in normal mode → ✅ Mode-specific binding in `diff-view` mode
- ⚠️ `Ctrl+N` is "new file" in normal mode → ✅ Mode-specific binding in `diff-view` mode
- ⚠️ `Ctrl+P` is "command palette" in normal mode → ✅ Mode-specific binding in `diff-view` mode

**Rationale:**
- ✅ **REQUIRED:** Uses modifier keys to prevent typing in panes
- ✅ Works on Mac, Linux, Windows (Ctrl works on all platforms)
- ✅ Follows merge_conflict pattern (`C-j`, `C-k` for navigation)
- ✅ Mode-specific bindings prevent conflicts with global shortcuts
- ✅ Intuitive (D for diff, N for next, P for previous)

**Note:** Mode-specific bindings solve conflicts:
- `Ctrl+D` in `diff-view` mode = toggle view (doesn't conflict with global "add cursor")
- `Ctrl+N` in `diff-view` mode = next change (doesn't conflict with global "new file")
- `Ctrl+P` in `diff-view` mode = previous change (doesn't conflict with global "command palette")
- Mode-specific bindings take precedence when in diff-view mode

**Function Keys (Not Recommended for Mac)**

| Action | Mac Shortcut | Windows/Linux Shortcut | Status |
|--------|--------------|----------------------|--------|
| Toggle/Navigate | `Fn+F7` ⚠️ | `F7` ✅ | Mac requires Fn, unreliable |
| Next change | `Fn+F6` ⚠️ | `F6` ✅ | Mac requires Fn, unreliable |
| Previous change | `Fn+Shift+F7` ⚠️ | `Shift+F7` ✅ | Mac requires Fn, unreliable |

**Note:** Function keys work well on Windows/Linux but are problematic on Mac due to Fn requirement and system function mappings.

### Implementation Notes

1. **Mode Context:** Shortcuts only active in `diff-view` mode, not in normal editing
   - Mode-specific bindings override global bindings when in diff-view mode
   - Example: `Ctrl+D` in diff-view mode = toggle view, but `Ctrl+D` in normal mode = add cursor
2. **Format:** Use "C-d", "C-n", "C-p" format (like merge_conflict uses "C-j", "C-k")
3. **Inheritance:** Consider inheriting from "special" mode (read-only) to prevent editing conflicts
4. **Help Text:** Update status messages to show available shortcuts (e.g., "Ctrl+D: toggle view")
5. **Consistency:** Follow patterns established by merge_conflict plugin (Ctrl+key combinations)
6. **CRITICAL:** All shortcuts MUST use modifier keys (Ctrl, Alt, Shift) to prevent typing in panes

### Shortcut Comparison Table

#### Mac Keyboard

| Action | Current | Option 1 (F-keys) | Option 2 (Ctrl) | Option 3 (Alt/Option) | Option 4 (Brackets) |
|--------|---------|------------------|-----------------|---------------------|---------------------|
| Toggle view | `s` ✗ | `Fn+F7` ⚠️ | `Ctrl+D` ✓✓ | `Option+D` ⚠️ | `]` ✗ Single key |
| Next change | - | `Fn+F6` ⚠️ | `Ctrl+N` ✓✓ | `Option+N` ⚠️ | `Shift+]` ✗ Single key |
| Prev change | - | `Fn+Shift+F7` ⚠️ | `Ctrl+P` ✓✓ | `Option+P` ⚠️ | `[` ✗ Single key |
| Close | `q` ✓ | `q` ✓ | `q` ✓ | `q` ✓ | `q` ✓ |
| View diff | `Return` ✓ | `Return` ✓ | `Return` ✓ | `Return` ✓ | `Return` ✓ |

#### Windows/Linux Keyboard

| Action | Current | Option 1 (F-keys) | Option 2 (Ctrl) | Option 3 (Alt) | Option 4 (Brackets) |
|--------|---------|------------------|-----------------|----------------|---------------------|
| Toggle view | `s` ✗ | `F7` ✓ | `Ctrl+D` ✓✓ | `Alt+D` ⚠️ | `]` ✗ Single key |
| Next change | - | `F6` ✓ | `Ctrl+N` ✓✓ | `Alt+N` ⚠️ | `Shift+]` ✗ Single key |
| Prev change | - | `Shift+F7` ✓ | `Ctrl+P` ✓✓ | `Alt+P` ⚠️ | `[` ✗ Single key |
| Close | `q` ✓ | `q` ✓ | `q` ✓ | `q` ✓ | `q` ✓ |
| View diff | `Return` ✓ | `Return` ✓ | `Return` ✓ | `Return` ✓ | `Return` ✓ |

**Legend:**
- ✓✓ = Recommended (uses modifiers, mode-specific, no conflicts, cross-platform)
- ✓ = Available but has limitations or platform-specific issues
- ⚠️ = Mac compatibility issues, potential conflicts, or platform differences
- ✗ = Conflicts or causes typing in panes

**Note:** Option 2 (Ctrl+Key) is the recommended choice for all platforms. It works identically on Mac, Windows, and Linux. Single keys (including brackets) cause typing in panes and are not viable on any platform.

---

## 📋 Final Keyboard Shortcut Recommendation

### Selected: Option 2 - Ctrl+Key Combinations

**Decision:** Use Ctrl+key combinations to prevent typing in panes. Mode-specific bindings prevent conflicts.

**Proposed Shortcuts (Cross-Platform):**

| Action | Mac | Windows/Linux | Notes |
|--------|-----|---------------|-------|
| Toggle side-by-side/unified | `Ctrl+D` | `Ctrl+D` | Mode-specific, won't conflict |
| Navigate to next change | `Ctrl+N` | `Ctrl+N` | Mode-specific, future enhancement |
| Navigate to previous change | `Ctrl+P` | `Ctrl+P` | Mode-specific, future enhancement |
| Close/quit | `q` | `q` | Works in special modes |
| Close/quit (alternative) | `Escape` | `Escape` | Alternative to q |
| View file diff | `Return` | `Return` | Works in special modes |

**Platform Compatibility:**
- ✅ **Mac:** All shortcuts work identically (Ctrl key available, no Fn needed)
- ✅ **Windows:** All shortcuts work identically
- ✅ **Linux:** All shortcuts work identically
- ✅ **Cross-platform:** Same shortcuts across all platforms

**Why Ctrl+Key:**
1. ✅ **REQUIRED** - Uses modifier keys to prevent typing in panes
2. ✅ **Cross-Platform Identical** - Same shortcuts work on Mac, Linux, Windows
   - **Mac:** `Ctrl` key works reliably (no Fn needed, different from Cmd)
   - **Windows/Linux:** `Ctrl` key is standard modifier
3. ✅ **Mac Compatible** - Ctrl works without Fn key (unlike function keys)
4. ✅ **Consistent** - Matches merge_conflict pattern (C-j, C-k)
5. ✅ **Mode-Specific** - Bindings only active in diff-view mode, no global conflicts
6. ✅ **Intuitive** - D for diff, N for next, P for previous

**Next Steps:**
1. Review this recommendation
2. Approve or suggest alternatives
3. Update implementation in `diff.ts`
4. Test on Mac, Linux, and Windows
5. Update help text and status messages

---

*Last Updated: 2024-12-19*

