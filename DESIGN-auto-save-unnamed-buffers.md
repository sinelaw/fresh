# Design: Auto-Save/Restore for Unnamed Buffers

**Issue:** [#1148](https://github.com/sinelaw/fresh/issues/1148)

## Problem Statement

When Fresh exits normally, unnamed (unsaved/scratch) buffers are discarded after
a "discard" confirmation. However, force-killing Fresh preserves them via the
crash recovery system. Users want unnamed buffers to persist across normal
exit/restart cycles, matching Sublime Text and Notepad++ behavior ("anonymous
scratchpad").

## Existing Infrastructure

| System | What it does | Storage |
|---|---|---|
| **Workspace persistence** (`workspace.rs`, `app/workspace.rs`) | Saves/restores split layout, open tabs, cursors, scroll positions, etc. Runs on clean exit and periodically (5s debounce). Skips unnamed buffers (leaf `file_path: None`). | `$XDG_DATA_HOME/fresh/workspaces/{encoded_path}.json` |
| **Recovery service** (`services/recovery/`) | Emacs-style crash recovery. Auto-saves modified buffer *contents* periodically. Cleaned up on normal exit (`end_session` deletes all recovery files). Handles unnamed buffers (generates stable `recovery_id`). | `$XDG_DATA_HOME/fresh/recovery/{hash}.meta.json` + `.chunk.*` |
| **Persistent auto-save** (`file_operations.rs`) | Saves modified file-backed buffers to their original file on disk. Only for buffers that *have* a file path. Controlled by `auto_save_enabled` config. | Original file on disk |
| **Quit flow** (`app/mod.rs:quit()`) | Counts modified buffers, prompts "discard/cancel", sets `should_quit`. No "keep unnamed" option. | N/A |

## Design Alternatives

---

### Alternative A: Extend Workspace Persistence (Inline Content)

**Approach:** Store unnamed buffer contents directly in the workspace JSON file.

**Changes:**
1. Add `unnamed_buffers: Vec<SerializedUnnamedBuffer>` to `Workspace` struct, where each entry holds `id`, `content: String`, cursor/scroll state, and `display_name`.
2. In `capture_workspace()`, serialize unnamed buffer contents alongside layout.
3. In `restore_workspace()`, recreate unnamed buffers from stored content.
4. In `serialize_split_node()`, reference unnamed buffers by ID instead of skipping them.
5. Bump `WORKSPACE_VERSION` to 2.
6. Remove the quit prompt for unnamed-only modifications (or add a "keep" option).

**Pros:**
- Single source of truth: layout + content in one file.
- Atomic: buffer content and tab position are always consistent.
- Simple restore logic (workspace restore already handles tab ordering).
- No new files/directories.

**Cons:**
- Workspace JSON can grow large if users keep many/large scratch buffers.
- Workspace save becomes heavier (serializing buffer contents every 5s).
- Version migration needed for existing workspace files.
- Couples buffer content storage with UI layout persistence.

**Best for:** Projects where unnamed buffers are small (notes, scratchpads).

---

### Alternative B: Extend Recovery Service (Don't Clean Up on Exit)

**Approach:** Keep recovery files for unnamed buffers across normal exit. The
recovery system already saves unnamed buffer contents with stable IDs; just
don't delete them on clean shutdown.

**Changes:**
1. In `end_session()`, only clean up recovery files for *file-backed* buffers; preserve unnamed buffer recovery files.
2. **Force a final recovery save for all unnamed buffers on clean exit.** The periodic auto-recovery-save is throttled by interval, so there may be unsaved edits since the last periodic save. Before `end_session()` cleanup runs, flush all dirty unnamed buffers to recovery storage (bypass the interval check). This is the same `save_buffer()` path used by `auto_recovery_save_dirty_buffers()` but unconditional.
3. Add `unnamed_buffers: Vec<UnnamedBufferRef>` to `Workspace` struct (just IDs + metadata, not content) so restore knows which recovery files to load and where they go in the tab layout.
4. In workspace restore, load unnamed buffer contents from recovery files by ID.
5. In `capture_workspace()` / `serialize_split_node()`, emit unnamed buffer references (recovery ID) instead of `file_path: None`.
6. Modify quit flow to skip the "discard" prompt when the only modified buffers are unnamed (their content is already recovery-saved).

**Pros:**
- Reuses existing recovery infrastructure (chunked storage, atomic writes, proven crash resistance).
- Minimal new code; recovery files already contain the content.
- Large buffers handled efficiently (chunked format).
- Content storage decoupled from workspace JSON (keeps it small).
- Recovery files already have cleanup/age-out logic (7 days default).

**Cons:**
- Two systems must stay in sync (workspace references recovery files by ID).
- If recovery files are deleted independently (e.g., manual cleanup), unnamed buffers silently disappear.
- Recovery `end_session` cleanup logic becomes conditional.
- Recovery files were designed as temporary crash artifacts, not persistent storage; conceptual mismatch.

**Best for:** General use; clean separation of concerns, good scalability.

---

### Alternative C: Dedicated Unnamed Buffer Store

**Approach:** Create a new, purpose-built persistence layer specifically for
unnamed buffer contents.

**Changes:**
1. New module `services/unnamed_buffers.rs` with its own storage directory (`$XDG_DATA_HOME/fresh/unnamed_buffers/`).
2. Each unnamed buffer gets a file: `{uuid}.json` containing content + metadata.
3. Save on edit (debounced, like recovery) and on exit.
4. Workspace references unnamed buffers by UUID.
5. Restore loads from the unnamed buffer store.
6. Configurable retention policy (max count, max age, max total size).
7. New config option: `persist_unnamed_buffers: bool` (default true).

**Pros:**
- Clean, purpose-built abstraction; no conceptual mismatch.
- Independent lifecycle from both workspace and recovery.
- Can add features specific to unnamed buffers (retention policies, size limits).
- Easy to test in isolation.

**Cons:**
- Most new code (~300-400 lines).
- Third persistence system adds cognitive overhead.
- Partially duplicates recovery service logic (atomic writes, chunked storage, cleanup).
- Yet another directory to manage.

**Best for:** If unnamed buffer persistence needs diverge significantly from recovery (custom retention, sharing across workspaces, etc.).

---

### Alternative D: Auto-Assign Temporary File Paths

**Approach:** When an unnamed buffer is created, immediately assign it a
hidden temp file path (e.g., `$XDG_DATA_HOME/fresh/scratch/scratch-{uuid}.txt`).
The buffer becomes "file-backed" and all existing save/restore infrastructure
just works.

**Changes:**
1. In `new_buffer()`, assign a path in a scratch directory instead of `file_path: None`.
2. Mark buffers with a `is_scratch: bool` flag so UI still shows "[unnamed]" or "scratch-1".
3. Existing persistent auto-save (`auto_save_persistent_buffers`) saves them to the scratch files.
4. Existing workspace persistence tracks them as normal file paths.
5. On explicit "Save As", move from scratch path to user-chosen path and clear `is_scratch`.
6. Cleanup: delete scratch files for buffers explicitly closed by user.

**Pros:**
- Zero new persistence code; existing auto-save + workspace handles everything.
- Simplest conceptual model ("every buffer has a file").
- Scratch files are real files, inspectable, recoverable by external tools.
- Works immediately with existing features (grep, file explorer if desired).

**Cons:**
- Scratch directory accumulates files if user never explicitly closes buffers.
- `auto_save_enabled` must be true (or scratch buffers need separate auto-save logic).
- File paths leak into workspace JSON, git status (if inside working dir), etc.
- Conceptual mismatch for users who think of unnamed buffers as ephemeral.
- Need to handle cleanup of orphaned scratch files.

**Best for:** Simplicity-first approach; works well if the team is comfortable with "every buffer is a file."

---

## Comparison Matrix

| Criterion | A: Workspace Inline | B: Recovery Reuse | C: Dedicated Store | D: Temp Files |
|---|---|---|---|---|
| New code | ~150 lines | ~100 lines | ~350 lines | ~80 lines |
| Large buffer handling | Poor (JSON bloat) | Good (chunked) | Good (custom) | Good (real files) |
| Crash safety | Atomic (workspace) | Proven (recovery) | New (needs testing) | Proven (auto-save) |
| Conceptual clarity | Medium | Medium | High | Medium |
| Workspace JSON size | Grows with content | Small (IDs only) | Small (IDs only) | Normal (paths) |
| Existing infra reuse | Moderate | High | Low | Very high |
| Future flexibility | Low | Medium | High | Low |
| Risk of data loss | Low | Medium (sync) | Low | Low |

## Recommendation

**Alternative B (Extend Recovery Service)** offers the best balance:
- Minimal new code by reusing battle-tested recovery infrastructure.
- Clean separation: workspace stores *references*, recovery stores *content*.
- Handles large unnamed buffers efficiently via existing chunked format.
- Natural fit: recovery already handles unnamed buffers during crash; this just extends that to normal exit.

**Alternative D (Temp Files)** is the simplest if the team prefers "everything is a file" and `auto_save_enabled` is acceptable as a dependency.

## Quit Flow Changes (All Alternatives)

The quit prompt currently offers only "discard / cancel". Proposed changes:

1. **Force-flush unnamed buffers on exit:** Before any cleanup, call a
   final recovery save for all dirty unnamed buffers (bypassing the
   interval throttle). This ensures no edits are lost between the last
   periodic auto-recovery-save and the moment of exit. In `main.rs` /
   `gui/mod.rs` shutdown path, call a new
   `flush_unnamed_buffer_recovery()` method before `end_recovery_session()`.
2. **If only unnamed buffers are modified:** Skip the prompt entirely (content is auto-persisted).
3. **If file-backed buffers are also modified:** Keep the prompt but clarify that unnamed buffers will be preserved regardless.
4. **New config option:** `persist_unnamed_buffers: bool` (default: `true`). When `false`, current behavior is preserved.

## E2E Tests

Per CONTRIBUTING.md, any new user flow must include e2e tests that send
keyboard/mouse events and examine rendered output (not internal state).
The existing `tests/e2e/recovery.rs` and `tests/e2e/workspace.rs` provide
the patterns to follow.

**Required test cases:**

1. **Unnamed buffer survives save/restore cycle:**
   Create an unnamed buffer, type content, save workspace + flush recovery,
   create a new editor session, restore workspace, verify the unnamed buffer
   content appears on screen with the correct tab position.

2. **Multiple unnamed buffers restored in correct tab order:**
   Open two unnamed buffers in specific tab positions alongside file-backed
   buffers, type distinct content in each, save/restore, verify both appear
   in the correct tabs with correct content.

3. **Quit with only unnamed modified buffers skips prompt:**
   Create an unnamed buffer, type content, trigger quit — verify the editor
   exits without showing the "discard/cancel" prompt (content is
   auto-persisted).

4. **Quit with mixed modified buffers still prompts:**
   Open a file-backed buffer and an unnamed buffer, modify both, trigger
   quit — verify the prompt appears (for the file-backed buffer) but
   unnamed buffer content is preserved after discard.

5. **Closing an unnamed buffer explicitly removes its recovery data:**
   Create an unnamed buffer, type content, explicitly close the tab —
   verify recovery files for that buffer are cleaned up (not restored on
   next session).

6. **`persist_unnamed_buffers: false` preserves old behavior:**
   With the config option disabled, verify unnamed buffers are discarded on
   quit (current behavior unchanged).

## Open Questions

1. Should there be a maximum number/size of persisted unnamed buffers?
2. Should unnamed buffers persist across *all* workspaces or only per-workspace?
3. Should there be a UI indicator (tab badge, status bar) showing that an unnamed buffer is auto-persisted?
4. Should `Ctrl+Q` / close-window silently persist (Sublime behavior) or still prompt for file-backed changes?
