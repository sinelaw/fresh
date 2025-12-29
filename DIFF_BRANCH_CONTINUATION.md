# Diff Branch Continuation Plan

**Status**: Working! Review Diff UX is functional
**Last Updated**: 2025-12-28

## Phase 0: Compilation Fixes (DONE)

All 14 compilation errors have been fixed:
- Made `TsHighlightSpan` public with `pub` keyword and public fields
- Added `#[derive(Debug, Clone)]` traits
- Added `#[op2(fast)]` attribute to `op_fresh_find_buffer_by_path`
- Fixed Color RGB extraction using pattern matching on `Color::Rgb`
- Added `review_hunks: Vec::new()` initialization
- Added match arms for `SetReviewDiffHunks` and `HighlightsComputed`

---

## Phase 1: Fix UX Bug (DONE)

### Problem (Fixed)

When running "Review Diff", the status showed "Generating Review Diff Stream..."
but the *Review Diff* buffer never appeared.

### Root Cause

In `renderReviewStream()`, unused code was opening files for syntax highlighting prep:
- `editor.createVirtualBuffer()` for HEAD: versions
- `editor.openFile()` for working copies

Both stole focus before the Review Diff buffer was created.

### Fix Applied

Removed the unused `headBuffers`/`workingBuffers` preparation code and the
`mapHighlights` function. The Review Diff buffer now displays correctly with
colorized diff output (pink for removed lines, blue for context headers)

---

## Phase 2: Complete the Review Diff Feature

### 2.1 Unified Review Stream (audit_mode.ts)
**Status**: Mostly complete
- [x] Git diff parsing
- [x] Hunk display with box borders
- [x] Stage/Discard actions (s/d keys)
- [x] Hunk navigation (n/p keys)
- [x] Refresh on buffer activation
- [ ] Apply staged hunks to git index
- [ ] Persist staging decisions

### 2.2 Side-by-Side Drill-Down
**Status**: Scaffolded
- [x] Opens HEAD version in virtual buffer
- [x] Sets up synchronized scrolling
- [ ] Sync scroll doesn't work reliably
- [ ] Missing: Back navigation to unified view

### 2.3 Syntax Highlighting for Virtual Buffers
**Status**: WIP in Rust
- [x] `RequestHighlights` command added
- [x] `HighlightsComputed` response added
- [ ] Fix compilation errors (see Phase 1)
- [ ] Wire up in plugin to use real highlighting

### 2.4 Composite Buffer Architecture
**Status**: Proposed in docs, not implemented
- [ ] `SectionDescriptor` struct
- [ ] Multi-source token synthesis
- [ ] Coordinate mapping for editable hunks
- [ ] Input routing to source buffers

---

## Phase 3: Advanced Features (Future)

### 3.1 Conflict Resolution (3-Pane Merge)
- Visual layout: LOCAL | RESULT | REMOTE
- l/r keys to pick changes
- Editable center pane

### 3.2 Hunk Editing
- Allow editing within the unified view
- Changes sync back to working copy

### 3.3 Integration with AI Workflows
- Accept/reject changes from Claude Code
- Batch operations

---

## Files Modified in This Branch

| File | Purpose |
|------|---------|
| `src/services/plugins/api.rs` | `ReviewHunk`, `SetReviewDiffHunks`, `RequestHighlights`, `HighlightsComputed` |
| `src/services/plugins/runtime.rs` | `TsHighlightSpan`, `op_fresh_get_highlights` |
| `src/app/mod.rs` | `review_hunks` field |
| `src/app/plugin_commands.rs` | `handle_request_highlights` |
| `plugins/audit_mode.ts` | Main Review Diff plugin |
| `docs/AUDIT_AND_VERIFICATION_DESIGN.md` | Feature design |
| `docs/COMPOSITE_BUFFER_ARCHITECTURE.md` | Architecture spec |

---

## Recommended Next Steps

1. ~~**Fix all 14 compilation errors**~~ ✅ DONE
2. ~~**Fix the UX bug**~~ ✅ DONE - Removed unused buffer prep code
3. ~~**Test the Review Diff UX**~~ ✅ Working - Review Diff buffer displays with colors
4. **Fix synchronized scrolling** in drill-down view
5. **Add "Apply to Index" functionality** to actually stage hunks via git
6. **Evaluate whether Composite Buffer Architecture is needed** for current goals
