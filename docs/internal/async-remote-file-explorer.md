# Async Remote File Explorer

## Problem

The file explorer blocks the main editor thread on remote filesystem I/O.
When the remote host becomes unreachable (e.g. DHCP lease change, network
drop), the editor freezes permanently because these blocking calls have no
timeout.

### Root cause

`poll_file_tree_changes()` and `file_explorer_toggle_expand()` (plus 5 other
call sites) use `runtime.block_on(...)` to perform remote filesystem operations
synchronously on the main thread. The underlying `AgentChannel::request()` has
no timeout — if the remote never responds, the main thread blocks forever.

### Affected code

**`file_operations.rs` — `poll_file_tree_changes()`**
- Calls `self.filesystem.metadata()` synchronously for every expanded dir
- Calls `runtime.block_on(tree.refresh_node())` for changed dirs

**`file_explorer.rs` — 6 `block_on` call sites:**

| Line | Operation | Context |
|------|-----------|---------|
| 275 | `toggle_node` | User expands/collapses a dir |
| 406 | `refresh_node` | User manually refreshes |
| 442 | `refresh_node` | After creating a file |
| 498 | `refresh_node` | After creating a directory |
| 586 | `refresh_node` | After deleting (trash) |
| 707 | `refresh_node` | After renaming |

## Solution

Three changes, layered for defense in depth.

### 1. Channel-level request timeout (30s)

Add `tokio::time::timeout(Duration::from_secs(30), ...)` around the response
wait in `AgentChannel::request()`. Returns `ChannelError::Timeout` (variant
already exists) on expiry. `request_blocking()` inherits this automatically.

This protects **all** remote operations as a safety net — file open, save,
metadata, list_dir, etc. — not just file explorer calls.

### 2. Async `poll_file_tree_changes` (two-phase)

Replace the synchronous polling loop with a background task:

**Phase 1 — main thread (non-blocking):**
- Collect expanded dirs + their stored mtimes
- Spawn a tokio task
- Set `file_tree_poll_in_progress = true`, return immediately

**Phase 2 — tokio task:**
- Call `metadata_async()` for each dir (with the 30s channel timeout)
- Wrap the whole batch in a 5s timeout so we don't wait 30s x N dirs
- Compare mtimes, send changed dirs back via
  `AsyncMessage::FileTreePollResult(Vec<(NodeId, PathBuf, SystemTime)>)`

**Phase 3 — main thread (on receiving AsyncMessage):**
- Update `dir_mod_times`
- Spawn async refresh tasks for changed dirs (see below)
- Clear `file_tree_poll_in_progress`

### 3. Async file explorer operations

Replace all 6 `block_on` calls in `file_explorer.rs` with spawned tasks.

**Prerequisites:**
- Derive `Clone` on `FileTree` (all fields are owned data + `Arc<FsManager>`)

**Pattern for toggle_node (expand/collapse):**
1. Set node to `NodeState::Loading` immediately (already exists)
2. Clone the `FileTreeView` and spawn a tokio task
3. Task calls `toggle_node` / `refresh_node` on the cloned tree
4. Task sends result back via `AsyncMessage` with the updated tree
5. Main thread swaps in the new tree, does post-processing (gitignore load,
   decoration rebuild, selection update, status message)

**Pattern for post-mutation refreshes (create/delete/rename):**
- The filesystem mutation itself (create, rename, delete) remains synchronous
  — these are user-initiated one-shot operations, protected by the 30s channel
  timeout
- Only the `refresh_node` call after the mutation becomes async
- If the refresh fails, the mutation already succeeded; the tree is just stale
  (user can manually refresh)

## UX behavior

### Normal operation (remote responsive)

No visible change. Expand/collapse may feel slightly more responsive since the
main thread isn't blocked during directory listing.

### Remote becomes unresponsive

**Editor never freezes.** Input, rendering, and all non-remote features
continue working normally.

| Feature | Behavior |
|---------|----------|
| File tree auto-poll | Stops updating silently. Tree shows last known state. Debug log on timeout. |
| Expand/collapse dir | Shows "Loading..." state. Times out after 30s, shows error in status bar, node reverts. |
| Create/delete/rename | Mutation times out after 30s, error in status bar. |
| Open/save file | Times out after 30s, error in status bar. |

### No new UI elements

A stale file tree is an acceptable degraded state. The user discovers the
remote is down when they try an active operation (open, save, expand) and see
the timeout error.

### Out of scope

- Reconnection logic (separate feature)
- Making file open/save async (much larger refactor, 30s timeout is sufficient)

## Implementation order

1. Channel timeout — smallest change, biggest safety improvement
2. `poll_file_tree_changes` async — fixes the deadlock that prompted this work
3. `file_explorer_toggle_expand` + the 5 refresh sites — completes the picture
