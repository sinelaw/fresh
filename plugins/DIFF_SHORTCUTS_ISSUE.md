# GitHub Issue: Diff Plugin Keyboard Shortcuts - IntelliJ Compatibility

## Summary

The diff plugin needs keyboard shortcuts that work reliably across platforms (Mac, Windows, Linux) and don't interfere with editing. This issue compares IntelliJ IDEA's industry-standard diff shortcuts with Fresh editor's current shortcuts, identifies conflicts, and proposes solutions.

## Problem Statement

1. **Current Issue:** Single-key shortcuts (like `s` for toggle) result in typing in the buffer instead of triggering actions
2. **Requirement:** Must use key combinations with modifiers (Ctrl, Alt, Shift) to prevent typing
3. **Platform Compatibility:** Need shortcuts that work identically on Mac, Windows, and Linux
4. **Industry Standards:** IntelliJ IDEA and VSCode have established conventions for diff navigation

## IntelliJ IDEA Standard Diff Shortcuts

| Action | IntelliJ Shortcut (Mac) | IntelliJ Shortcut (Windows/Linux) | Industry Standard |
|--------|------------------------|----------------------------------|-------------------|
| Navigate to next change | `F7` | `F7` | ✅ Standard (IntelliJ, VSCode) |
| Navigate to previous change | `Shift+F7` | `Shift+F7` | ✅ Standard (IntelliJ, VSCode) |
| Show diff dialog | `Cmd+D` (Mac) / `Ctrl+D` (Win/Linux) | `Ctrl+D` | ⚠️ Conflicts with Fresh |
| Compare with Clipboard | `Cmd+Shift+D` (Mac) / `Ctrl+Shift+D` (Win/Linux) | `Ctrl+Shift+D` | ✅ Available |
| Compare with Branch | `Cmd+Alt+Shift+D` (Mac) / `Ctrl+Alt+Shift+D` (Win/Linux) | `Ctrl+Alt+Shift+D` | ✅ Available |
| Toggle unified/side-by-side | Via UI button/menu | Via UI button/menu | ⚠️ No standard shortcut |

**Note:** IntelliJ doesn't have a standard keyboard shortcut for toggling unified/side-by-side view - it's typically done via UI buttons.

## Fresh Editor Current Shortcuts

| Action | Fresh Shortcut | Context | Status |
|--------|---------------|---------|--------|
| Add cursor next match | `Ctrl+D` | Normal mode | ✅ Currently used |
| New file | `Ctrl+N` | Normal mode | ✅ Currently used |
| Command palette | `Ctrl+P` | Normal mode | ✅ Currently used |
| Save | `Ctrl+S` | Normal mode | ✅ Currently used |
| Quit | `Ctrl+Q` | Normal mode | ✅ Currently used |
| Search | `Ctrl+F` | Normal mode | ✅ Currently used |
| Replace | `Ctrl+R` | Normal mode | ✅ Currently used |
| Jump to next error | `F8` | Normal mode | ✅ Currently used |
| Jump to previous error | `Shift+F8` | Normal mode | ✅ Currently used |
| **F7** | **Not used** | - | ✅ **AVAILABLE** |
| **Shift+F7** | **Not used** | - | ✅ **AVAILABLE** |
| **F6** | **Not used** | - | ✅ **AVAILABLE** |
| **F4** | **Not used** | - | ✅ **AVAILABLE** |

## Conflict Analysis

### IntelliJ Shortcut vs Fresh Shortcut Conflicts

| IntelliJ Action | IntelliJ Shortcut | Fresh Conflict | Fresh Current Use | Resolution Options |
|----------------|------------------|----------------|-------------------|-------------------|
| Show diff dialog | `Ctrl+D` | ❌ **CONFLICT** | `Ctrl+D` = Add cursor next match | Option A: Use mode-specific binding<br>Option B: Use `F7` for diff navigation only<br>Option C: Use `Ctrl+Shift+D` |
| Navigate next change | `F7` | ✅ **NO CONFLICT** | Not used | ✅ **Available - Recommended** |
| Navigate prev change | `Shift+F7` | ✅ **NO CONFLICT** | Not used | ✅ **Available - Recommended** |
| Toggle unified/side-by-side | No standard shortcut | N/A | N/A | Need to propose new shortcut |

## Proposed Solutions

### Option 1: Adopt IntelliJ Standards (Recommended)

**Rationale:** IntelliJ shortcuts are industry-standard and familiar to many developers.

| Action | Mac Shortcut | Windows/Linux Shortcut | Conflict Resolution |
|--------|-------------|------------------------|---------------------|
| Navigate to next change | `F7` | `F7` | ✅ No conflict - F7 is available |
| Navigate to previous change | `Shift+F7` | `Shift+F7` | ✅ No conflict - Shift+F7 is available |
| Toggle unified/side-by-side | `F6` | `F6` | ✅ No conflict - F6 is available |
| Close/quit diff view | `q` | `q` | ✅ Standard pattern (keep existing) |
| View file diff | `Return` | `Return` | ✅ Standard pattern (keep existing) |

**Pros:**
- ✅ Follows industry standards (IntelliJ, VSCode)
- ✅ No conflicts with existing Fresh shortcuts
- ✅ Function keys don't interfere with editing
- ✅ Familiar to developers using IntelliJ/VSCode

**Cons:**
- ⚠️ Mac requires `Fn` key for function keys (but this is standard Mac behavior)
- ⚠️ Some Mac keyboards map F-keys to system functions (brightness, volume)
- ⚠️ Less intuitive than mnemonic shortcuts (D for diff, N for next)

**Mac Compatibility Note:**
- Mac users can configure "Use F1, F2, etc. keys as standard function keys" in System Preferences
- Or use `Fn+F7` when F-keys are mapped to system functions
- This is standard Mac behavior and accepted by Mac users

### Option 2: Mode-Specific Ctrl+Key (Alternative)

**Rationale:** Use Ctrl+key combinations that are mode-specific to avoid conflicts.

| Action | Mac Shortcut | Windows/Linux Shortcut | Conflict Resolution |
|--------|-------------|------------------------|---------------------|
| Toggle unified/side-by-side | `Ctrl+D` | `Ctrl+D` | Mode-specific: only active in `diff-view` mode |
| Navigate to next change | `Ctrl+N` | `Ctrl+N` | Mode-specific: only active in `diff-view` mode |
| Navigate to previous change | `Ctrl+P` | `Ctrl+P` | Mode-specific: only active in `diff-view` mode |
| Close/quit diff view | `q` | `q` | Standard pattern (keep existing) |
| View file diff | `Return` | `Return` | Standard pattern (keep existing) |

**Pros:**
- ✅ Works identically on Mac, Windows, Linux (no Fn key needed)
- ✅ Mode-specific bindings prevent conflicts
- ✅ Mnemonic (D for diff, N for next, P for previous)
- ✅ No Mac-specific configuration needed

**Cons:**
- ⚠️ Conflicts with global shortcuts (but resolved via mode-specific binding)
- ⚠️ Less standard than IntelliJ shortcuts
- ⚠️ Requires understanding of mode-specific behavior

### Option 3: Hybrid Approach

**Rationale:** Use IntelliJ standards for navigation, custom shortcut for toggle.

| Action | Mac Shortcut | Windows/Linux Shortcut | Rationale |
|--------|-------------|------------------------|-----------|
| Navigate to next change | `F7` | `F7` | IntelliJ standard |
| Navigate to previous change | `Shift+F7` | `Shift+F7` | IntelliJ standard |
| Toggle unified/side-by-side | `Ctrl+Shift+D` | `Ctrl+Shift+D` | No conflict, mnemonic |
| Close/quit diff view | `q` | `q` | Standard pattern |
| View file diff | `Return` | `Return` | Standard pattern |

**Pros:**
- ✅ Navigation follows IntelliJ standards
- ✅ Toggle uses available shortcut (no conflict)
- ✅ Best of both worlds

**Cons:**
- ⚠️ Mixed conventions (function keys + Ctrl+key)
- ⚠️ Mac F-key limitations still apply

## Recommendation

### Primary Recommendation: **Option 1 - Adopt IntelliJ Standards**

**Rationale:**
1. **Industry Standard:** IntelliJ and VSCode use `F7` / `Shift+F7` for diff navigation - this is what developers expect
2. **No Conflicts:** `F7` and `Shift+F7` are currently unused in Fresh editor
3. **Platform Compatibility:** Works on all platforms (Mac users can configure F-keys or use Fn)
4. **Familiarity:** Developers switching from IntelliJ/VSCode will feel at home
5. **Consistency:** Aligns Fresh with industry-standard tools

**Implementation:**
- Use `F7` for "navigate to next change" (future enhancement)
- Use `Shift+F7` for "navigate to previous change" (future enhancement)
- Use `F6` for "toggle unified/side-by-side view" (immediate need)
- Keep `q` for close/quit (standard pattern)
- Keep `Return` for view file diff (standard pattern)

### Alternative: **Option 2 - Mode-Specific Ctrl+Key**

If the Fresh team prefers to avoid function keys entirely (e.g., for Mac compatibility concerns), Option 2 provides a cross-platform solution using mode-specific bindings.

## Questions for Fresh Team

1. **Are you open to adopting IntelliJ-standard shortcuts (`F7`, `Shift+F7`) for diff navigation?**
   - These are currently unused in Fresh
   - They're industry-standard and familiar to developers
   - Mac compatibility can be handled via system preferences or Fn key

2. **What's your preference for Mac compatibility?**
   - Accept that Mac users may need to use `Fn+F7` or configure system preferences?
   - Or prefer cross-platform shortcuts that work identically without configuration?

3. **Are mode-specific keybindings acceptable?**
   - Would allow `Ctrl+D` in diff-view mode to differ from normal mode
   - Provides more flexibility but requires mode awareness

4. **Priority: Industry standards vs Fresh conventions?**
   - Should Fresh align with IntelliJ/VSCode standards for familiarity?
   - Or maintain Fresh-specific conventions for consistency?

## Implementation Details

### If Option 1 (IntelliJ Standards) is Chosen:

```typescript
// Mode definition for diff-view
editor.defineMode(
  "diff-view",
  "normal", // or "special" for read-only
  [
    ["F7", "diff_next_change"],        // Navigate to next change
    ["Shift+F7", "diff_prev_change"],  // Navigate to previous change
    ["F6", "diff_toggle_view"],        // Toggle unified/side-by-side
    ["q", "diff_close"],               // Close/quit
    ["Return", "diff_view_file"],      // View file diff (in list mode)
  ],
  true // read-only
);
```

### If Option 2 (Mode-Specific Ctrl+Key) is Chosen:

```typescript
// Mode definition for diff-view
editor.defineMode(
  "diff-view",
  "normal",
  [
    ["C-d", "diff_toggle_view"],       // Ctrl+D: Toggle view (mode-specific)
    ["C-n", "diff_next_change"],       // Ctrl+N: Next change (mode-specific)
    ["C-p", "diff_prev_change"],       // Ctrl+P: Previous change (mode-specific)
    ["q", "diff_close"],                // Close/quit
    ["Return", "diff_view_file"],      // View file diff
  ],
  true // read-only
);
```

## References

- [IntelliJ IDEA Keyboard Shortcuts Reference](https://www.jetbrains.com/help/idea/mastering-keyboard-shortcuts.html)
- [VSCode Keyboard Shortcuts Reference](https://code.visualstudio.com/docs/getstarted/keybindings)
- Fresh Editor keymaps: `fresh/keymaps/default.json`
- Diff plugin audit: `fresh/plugins/DIFF_AUDIT.md`

## Related Issues

- Diff plugin keyboard shortcuts need review (current implementation uses single-key shortcuts that interfere with editing)

---

**Status:** Awaiting Fresh team decision on shortcut preferences

**Labels:** `enhancement`, `keyboard-shortcuts`, `diff-plugin`, `cross-platform`, `ux`

