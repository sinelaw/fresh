# Buffers, Splits, Undo & Recovery (App Layer)

Purpose: explain how Fresh owns and identifies buffers, lays out split panes via the
window tree, separates per-buffer from per-view state, displaces/preserves markers across
undo/redo, and persists/recovers unsaved work across clean exits and crashes. Code-level,
with `path:line` references. IMPLEMENTED unless flagged PLANNED.

All paths are under `crates/fresh-editor/src/` unless noted otherwise.

---

## 1. Buffer ownership & identity

### Who owns buffers

A `Window` owns its buffers outright — there is no editor-global buffer pool. Each window
has its own content, layout, metadata, and undo logs.

- `Window` struct: `app/window/mod.rs:121`. Buffer-related fields:
  - `buffers: WindowBuffers` — the per-window content store + split layout (`mod.rs:183`).
  - `buffer_metadata: HashMap<BufferId, BufferMetadata>` (`mod.rs:190`).
  - `event_logs: HashMap<BufferId, EventLog>` — undo/redo, scoped per buffer (`mod.rs:195`).
  - `preview: Option<(LeafId, BufferId)>` — single editor-wide preview anchor (`mod.rs:459`).

`WindowBuffers` (`app/window/buffers.rs:17`) encapsulates two things behind one type:

```rust
pub struct WindowBuffers {
    map: HashMap<BufferId, EditorState>,   // buffer content
    splits: Option<Splits>,                // (SplitManager, HashMap<LeafId, SplitViewState>)
}
type Splits = (SplitManager, HashMap<LeafId, SplitViewState>);   // buffers.rs:15
```

Putting the split tree *inside* `WindowBuffers` is deliberate (commit `b2051ecf0`,
"move split tree into WindowBuffers"): a buffer cannot be removed without the split tree
being able to react, and the borrow checker forbids mutating a buffer and the layout at the
same time. Combined-mutation needs go through a small set of closure accessors
(`with_buffer_and_split`, `with_buffer_and_view_states`, `with_all_mut`) — collapsed to
3 variants in `42dbe4144`. Read accessors: `splits()`, `split_manager()`,
`split_view_states()` (`buffers.rs:130-140`); mutable equivalents `*_mut` (`buffers.rs:153-163`).

### Identity

`BufferId(pub usize)` is a `Copy` newtype (defined in `fresh-core/src/lib.rs:41`).
IDs are allocated editor-wide (not per-window) by a lock-free counter so plugin APIs can
pass them around without window qualification:

- `BufferIdAllocator(Arc<AtomicUsize>)` — `app/window_resources.rs:52`; `next()` does
  `fetch_add(1, Relaxed)`. It lives on `WindowResources` and is cloned (Arc) into every
  window. `set()` exists so workspace rehydration can restore the high-water mark.

`LeafId` / `ContainerId` / `SplitId` (in `model::event`) identify split-tree nodes;
`SplitId` is the raw id, `LeafId`/`ContainerId` are typed wrappers asserting "this is a leaf"
vs "this is a container."

### Metadata

`BufferMetadata` (`app/types/buffer_meta.rs:29`) holds non-content facts:
`kind` (`BufferKind::File { path, uri }` or `Virtual { mode }`), `display_name`,
`read_only`, `binary`, LSP bookkeeping (`lsp_enabled`, `lsp_opened_with`),
`hidden_from_tabs` (panel/composite source buffers don't show as tabs),
`synthetic_placeholder` (throwaway last-buffer holder), and
`recovery_id: Option<String>` (stable id for unnamed buffers across auto-saves).

---

## 2. The split / window tree

### Data structure

The layout is an Emacs-style binary tree of `SplitNode` (`view/split.rs:81`). Three variants:

- `Leaf { buffer_id, split_id: LeafId, role: Option<SplitRole> }` (`split.rs:83`) — one buffer.
- `Split { direction, first, second, ratio, split_id: ContainerId, fixed_first, fixed_second }`
  (`split.rs:95`) — an internal node. `ratio` (clamped 0.1–0.9, `split.rs:659`) gives the
  first child's fraction; `fixed_first`/`fixed_second` override the ratio with an absolute
  row/col count (`split_rect_ext`, `split.rs:1003`), used for headers/docks.
- `Grouped { split_id: LeafId, name, layout: Box<SplitNode>, active_inner_leaf }`
  (`split.rs:118`) — a subtree that appears as **one tab** in its parent's tab bar.

`SplitManager` (`split.rs:1072`) owns the tree:
`root`, `active_split: LeafId` (always a leaf), `next_split_id`, `maximized_split`,
`labels`, and a `focus_history: Vec<LeafId>` LRU (cap 50, `FOCUS_HISTORY_CAP` `split.rs:1100`).
`last_focused_where(predicate)` (`split.rs:1179`) walks the LRU newest-first, skipping
leaves that have since closed — file-open routing uses it to avoid landing files in the
Utility Dock.

### Why a binary tree

Splits nest arbitrarily; rectangles are computed by recursive subdivision
(`get_leaves_with_rects`, `split.rs:786`), reserving one row/col per separator
(`split_rect_ext`, `split.rs:1003`). Splitting replaces the target node in-place with a new
`Split` whose children are the old node + a new leaf (`replace_split_with_split`,
`split.rs:1300`). Closing a leaf replaces its parent `Split` with the surviving sibling
(`remove_child_static`, `split.rs:1412`) — so the tree never holds an empty container.
`split_root_positioned` (`split.rs:1282`) splits the *root* rather than the active leaf, so the
Utility Dock spans the full width below any pre-existing side-by-side panes rather than
nesting under whichever pane was active (regression test `split.rs:2056`).

### Roles & the Utility Dock

`SplitRole::UtilityDock` (`split.rs:71`) tags at most one leaf. Diagnostics, search/replace
results, terminals, and quickfix all *swap into* that single tagged leaf instead of spawning
new splits. The "at most one per role" invariant is enforced by callers via
`clear_role` before `set_leaf_role` (`split.rs:1762-1794`). Foundation landed in `46e7a133c`.

### Maximize

`maximized_split: Option<SplitId>` makes only that leaf render full-viewport
(`get_visible_buffers`, `split.rs:1474`). `next_split`/`prev_split` clear it first
(`split.rs:1596`, `1607`) — otherwise the newly-active leaf would render behind the maximized
one and the cursor would "disappear" (issue #1961, tests `split.rs:2143`, `2172`).

---

## 3. Buffer groups (grouped split nodes)

### Motivation & the design pivot

Multi-panel plugins (diff/merge tools) used to render side-by-side panels inside a *single*
virtual buffer with hand-rolled viewport math (~200 lines of boilerplate per plugin, no
per-panel scrollbar). `buffer-groups-design.md` reframed each panel as a *real buffer* in a
split subtree so they inherit viewport windowing, scrollbars, syntax highlighting, and
drag-to-resize for free. The first implementation produced the **wrong UX** — multiple
side-by-side tabs instead of one. `grouped-splitnode-design.md` fixed that by adding the
`SplitNode::Grouped` variant + `TabTarget` enum (commit `ad027603f`): the group is a single
tab; its subtree expands only when that tab is active.

### TabTarget

A split's tab bar is `open_buffers: Vec<TabTarget>` (`split.rs:322`), where
`TabTarget = Buffer(BufferId) | Group(LeafId)` (`split.rs:43`). A `Grouped` node's `split_id`
(a `LeafId`) is its tab target — it "behaves like a leaf from the outside." Visibility during
layout: `get_visible_leaves_with_rects(rect, is_group_active)` (`split.rs:819`) recurses into
a `Grouped` node only if the predicate says its tab is active, else skips its whole subtree.

### Implementation

`app/buffer_groups.rs`:
- `create_buffer_group` (`:60`) parses a JSON `LayoutDesc`
  (`Scrollable`/`Fixed`/`Split`, `:20`), creates a hidden virtual buffer per panel, builds the
  inner `SplitNode` tree + `GroupLayoutNode`, wraps them in `SplitNode::Grouped` (stashed in a
  `grouped_subtrees` map keyed by `LeafId`), creates a chrome-suppressed `SplitViewState` per
  panel leaf (`suppress_chrome = true`, `hide_tilde = true`, no line numbers), marks panel
  buffers `hidden_from_tabs`, and registers `TabTarget::Group(group_leaf_id)` in the active
  split's `open_buffers`.
- `set_panel_content` (`:385`), `focus_panel` (`:493`), `activate_group_tab` (`:741`),
  `close_buffer_group` (`:409`), and `set_buffer_group_panel_buffer` (`:569`, re-targets a
  panel at a different file-backed buffer for streaming plugins).
- `BufferGroup` runtime struct: `app/types/buffer_group.rs` (panel_buffers, panel_splits,
  representative_split).

Per the v1 design, groups have **fixed, plugin-declared layouts**: nested groups are
disallowed and user-initiated split inside a group is blocked (the plugin controls structure,
the user controls panel content).

### Composite buffers (related but distinct)

A *composite buffer* is a synthetic single tab that composes several source buffers
(side-by-side diff, stacked, unified). Unlike groups it is **visible in tabs** and has its own
cursor/selection/viewport. `SplitViewState.composite_view: Option<BufferId>` (`split.rs:347`)
marks a split as rendering a composite; its `active_buffer` still points at the focused source
buffer so normal keybindings route correctly. Model: `model/composite_buffer.rs`
(`CompositeBuffer`, `CompositeLayout`, `SourcePane`, `LineAlignment`); view state:
`view/composite_view.rs` (`CompositeViewState`, keyed by `(split_id, buffer_id)`); actions:
`app/composite_buffer_actions.rs` (`create_composite_buffer` `:481`, hunk navigation
`:300`, `handle_create_composite_buffer` `:1086`). 3-way merge layouts and per-pane
editability enforcement are scaffolded but not surfaced (PLANNED).

---

## 4. Per-buffer vs per-view state (design decision #10)

Content is shared across splits; *view* state is independent per split per buffer. This is the
fix for "the same file in two panes scrolled together."

- `EditorState` (the buffer content + decorations/highlighting) is owned once in
  `WindowBuffers::map`, keyed by `BufferId`.
- `SplitViewState` (`split.rs:310`) is per leaf split: `active_buffer`, the tab list
  `open_buffers`, `tab_scroll_offset`, focus history, `sync_group`, `composite_view`,
  group-tab markers, and a `keyed_states: HashMap<BufferId, BufferViewState>`.
- `BufferViewState` (`split.rs:138`) is **per buffer per split**: independent `cursors`
  (multi-cursor), `viewport` (scroll), `view_mode`, compose settings, `rulers`,
  `show_line_numbers`/`highlight_current_line` (with explicit `line_numbers_override`/
  `line_wrap_override` so a pinned per-buffer choice survives restart without freezing
  untouched buffers at a stale global — issue #474), per-view `folds`, and a
  `plugin_state: HashMap<String, serde_json::Value>` escape hatch so plugins store arbitrary
  per-buffer-per-split state without Rust enum churn.

`SplitViewState` derefs to its active buffer's `BufferViewState` (`split.rs:367`), so
`vs.cursors` transparently hits the active buffer. `switch_buffer` (`split.rs:423`) lazily
creates default view state for a newly-shown buffer at the split's current dimensions; folds
are intentionally *not* cloned into a new split (`Clone` impl `split.rs:276`, "fold markers are
per-view"). Workspace persistence stores per-file view state in a separate
`file_states: HashMap<PathBuf, SerializedFileState>` so cursor/scroll survive restarts
(design-decisions #10).

Related decision #11 (EditorState refactor): split `EditorState`'s mixed concerns into
`DecorationState` / `HighlightState` / `BufferFlags`. Status: `ComposeState` extracted as
proof-of-concept; remaining extractions deferred (PARTIALLY IMPLEMENTED).

Per-buffer config resolution (line wrap, wrap column, page view) is pure and language-aware:
`app/buffer_config_resolve.rs` — language override wins over the global editor default,
falling back when unset. Applied via `BufferViewState::apply_config_defaults` (`split.rs:242`)
when a buffer is first shown (e.g. `new_buffer`, `buffer_management.rs:585`).

---

## 5. Undo / redo with marker displacement

### Where undo lives

Undo is per-buffer: `EventLog` is keyed by `BufferId` in `Window.event_logs`. The app
entry points are tiny (`app/undo_actions.rs`): `handle_undo` (`:8`) calls `event_log.undo()`,
applies each inverse event to the active buffer, then restores displaced markers;
`handle_redo` (`:50`) replays forward events. Both refuse when editing is disabled and
recompute the modified flag from the log position afterward.

### Markers

"Markers" are byte-position anchors in an interval-tree-backed `MarkerList`, backing virtual
text (inlay hints, ghost text), overlays (diagnostic underlines, search highlights, semantic
tokens — each a start/end marker pair), and margins (breakpoints, line annotations). They
shift through edits via `adjust_for_insert` / `adjust_for_delete`.

### The two problems and the implemented solution

1. **Forward displacement**: bulk edits (toggle-comment, cut, paste, indent) go through
   `apply_events_as_bulk_edit`, which adjusts markers/margins inline as it edits. Fixed in
   commit `abd52d7` (forward path).

2. **Undo displacement**: a `BulkEdit` undo restores the buffer by swapping a piece-tree
   snapshot atomically — there are no per-edit insert/deletes to drive marker adjustment, so
   markers would be left at post-edit positions. The older docs
   (`undo-redo-markers-implementation-plan.md`, `bulk-edit-marker-displacement.md`) describe
   the fix as **PLANNED**.

   **DISCREPANCY — this is now IMPLEMENTED.** `Event::BulkEdit` (`model/event.rs:200`) carries
   two extra fields:
   - `edits: Vec<(usize, usize, usize)>` — `(position, delete_len, insert_len)`, descending by
     position (`event.rs:228`). Replayed as-is on redo; on undo, `inverse()` swaps
     `del_len`/`ins_len` so the reverse marker adjustments apply (`event.rs:483-501`).
   - `displaced_markers: Vec<(u64, usize)>` — `(marker_id, original_byte_position)`
     (`event.rs:234`). After adjustment, these markers are snapped back to their exact pre-edit
     positions, recovering the spacing that a range-delete would otherwise collapse.

   `LogEntry` also stores `displaced_markers` (`event.rs:575`). `EventLog::undo`
   (`event.rs:902`) returns `Vec<(Event, Vec<(u64, usize)>)>` — each inverse event paired with
   the displaced markers from the original entry. `app/undo_actions.rs:30-42` applies the
   inverse, then for non-`BulkEdit` events calls `restore_displaced_markers`; for `BulkEdit`
   it *skips* that because `state.apply(BulkEdit)` handles displaced markers internally via the
   event's own field. History: `369a4f180` (save/restore on BulkEdit undo), `11aa8fabe`
   (single-edit undo restore), `4a7d0a97e` (id-collision fix between marker_list and margins),
   `cb13364cc` (extract helpers, LSP net-delta). After hot-exit replay, LSP is re-synced so
   semantic tokens don't drift (`94c7a4514`, issue #1691).

### Undo groups

`LogEntry.group_id` (`event.rs:582`, runtime-only, not persisted) lets entries sharing an id
undo/redo as one atomic unit (e.g. a whole macro replay): `undo` keeps consuming entries in the
same group (`event.rs:915-930`).

### Fundamental limitation (accepted)

Deleting a range collapses all interior markers to the deletion start; re-inserting on undo
can't reconstruct their original internal spread — `displaced_markers` recovers the *endpoints*
but the design (and VSCode/Neovim/Emacs/Helix) accept the in-range spread as lost. The
visibility window is sub-100ms because LSP re-pushes corrected positions. Snapshot-based marker
restoration (Atom's approach) was explicitly rejected as too error-prone.

---

## 6. Hot-exit & crash recovery

Design principle (`hot-exit-improvements-plan.md`): *never silently discard unsaved data* —
unsaved buffers disappear only through explicit user action; exit/reopen/session-switch/CLI all
preserve them. CLI file args are *additive* to a restored workspace, never replacive.

### Storage format (IMPLEMENTED)

`services/recovery/` uses a unified chunked format (commit `6f09a1b07`, originally
`976e63e72` "Emacs-style file recovery"). Base dir `~/.local/share/fresh/recovery/`
(`storage.rs:81`), scoped by mode (`storage.rs:56`):
- Standalone: `recovery/default/{cwd_hash}/`
- Session (daemon) mode: `recovery/sessions/{name}/`

Per recoverable buffer:
- `{id}.meta.json` — `RecoveryMetadata` (`types.rs:110`: `original_path`, `buffer_name`,
  timestamps, `original_mtime`, `original_file_size`, `chunk_count`,
  `format_version: u32 = 2`) plus an embedded `ChunkedRecoveryIndex`.
- `{id}.chunk.N` — raw binary content per `RecoveryChunk` (`types.rs:81`:
  `offset`, `original_len`, `content`); `MAX_CHUNK_SIZE = 1 MiB`.

Small / unnamed buffers store full content as a single chunk with `original_file_size = 0`
("new buffer"); large files store only modified chunks and replay them in reverse offset
order on restore. Writes are atomic via temp-file + rename (`atomic_write`, `storage.rs:765`);
note **no fsync** (editor-crash safe, not OS-crash safe).

### Persist path

`recovery_actions.rs`:
- `auto_recovery_save_dirty_buffers` (`:421`) runs every frame, rate-limited by
  `auto_recovery_save_interval_secs`; only writes buffers with the `recovery_pending` flag.
- `save_pending_recovery_buffers` (`:444`) assigns stable recovery ids to unnamed buffers and
  flushes; `save_buffer_to_recovery` (`:599`) builds the chunks.
- On clean exit `end_recovery_session` (`:35`): if `hot_exit`, mark all modified buffers
  `recovery_pending`, flush, compute ids to preserve (`recovery_ids_to_preserve` `:66`, skips
  hidden/virtual/empty-unnamed), and call `end_session_preserving(&ids)`. If `hot_exit` is off,
  end the session normally (recovery cleared).

### Restore path

- Crash detection via a PID `session.lock` (`SessionInfo`, `types.rs:217`): if a lock exists but
  its PID is no longer running (`libc::kill(pid,0)` on Unix / `GetExitCodeProcess` on Windows,
  `storage.rs:208`), the previous run crashed.
- `recover_all_buffers` (`recovery_actions.rs:125`) **consumes** recovery files: per entry,
  `accept_recovery` loads content, opens the path (or creates an unnamed buffer), replaces
  content, marks modified, re-syncs LSP. Large files apply chunks in reverse.
- `try_restore_hot_exit_buffers` (`:283`) is the clean-exit counterpart: it `load_recovery`s
  **without deleting**, leaving files for the current session's hot-exit pipeline to own
  (decoupled from session restore in `549140fd5`, with `--restore`).
- **mtime conflict trade-off**: if the on-disk file changed since the snapshot, recovery is *not*
  silently discarded — the editor opens current disk contents, warns in the status bar, and
  **keeps** the recovery file for manual inspection (`RecoveryResult::OriginalFileModified`).
  This reverses the older behavior (silent skip + delete).
- Stale cleanup: `cleanup_orphans` (metadata without chunks or vice-versa), age-based pruning
  (`max_recovery_age_secs`, default 7 days).

`recovery_service` is shared across windows via `WindowResources` so restore is fully
window-pure (`05c851802`).

### Quit flow (`lifecycle.rs`)

`quit` (`:157`) counts modified buffers needing a prompt
(`count_modified_buffers_needing_prompt` `:230` — excludes unnamed under hot-exit and
file-backed under auto-save). With dirty buffers it shows `ConfirmQuitWithModified`; when
`hot_exit` is enabled the prompt includes a "Quit (recoverable)" option that exits without
saving and lets the recovery session preserve the changes (issue #1839, commit `b6f909f43`).
`handle_confirm_quit_modified` (`prompt_actions.rs:1406`): **Save** runs `save_all_on_exit`
then a SaveAs chain for unnamed buffers; **Discard** clears modified + `recovery_pending` on all
buffers (so nothing is preserved); **Quit** sets `should_quit` and relies on hot-exit.

### Status

IMPLEMENTED: chunked format, atomic writes, PID-lock crash detection, hot-exit persist/restore,
mtime-conflict warning, session-scoped recovery dirs, the discard-on-quit option.
PLANNED (per `hot-exit-improvements-plan.md`, tasks not yet shipped): explicit ordered tab-array
serialization (#1234), CLI-files-are-additive (#1232), some session+CLI restore flows (#1237).
Verify against `orchestrator_persistence.rs` / `workspace.rs` before relying on tab-order
guarantees.

---

## 7. Restore invariants & the orphaned-leaf bug

After a restore, three things must agree for a split: the `SplitManager` leaf's `buffer_id`,
the `SplitViewState.active_buffer`, and the leaf's presence in `open_buffers`.

`orphaned-leaf-investigation.md` (issue #1939) documents a blank-pane-on-restore bug: an empty
`[No Name]` seed buffer was left as a split's active leaf but absent from the tab list, so
`clean_orphaned_buffers` (`app/workspace.rs`, builds its referenced set from
`buffer_tab_ids()` only) removed it, leaving the leaf dangling at a dead `BufferId`. Two layers
respond:

- **Fix at source** (`99354ec97`): in `restore_split_view_state`, if the saved active tab can't
  be resolved, fall back to the first surviving tab via the normal `switch_buffer` +
  `set_split_buffer` path, so all three views agree and the seed is then correctly orphaned.
- **Defensive guard**: `effective_active_pair` (`app/window/mod.rs:2033`) — if the active
  split's buffer isn't in `buffers`, it logs the #1939 warning (`c990bf57e`) and falls back to
  any live buffer for status-bar queries. This masks the symptom for queries only; it does not
  repair the leaf, and is intentionally kept to surface future invariant violations.

Regression test: `test_restore_orphaned_active_unnamed_tab_renders_surviving_tab`
(`crates/fresh-editor/tests/e2e/workspace.rs`).

---

## 8. Buffer close lifecycle

`app/buffer_close.rs`:
- `close_buffer` (`:34`) errors if the buffer is modified; `force_close_buffer` (`:52`) closes
  unconditionally. Both funnel into `close_buffer_internal`.
- `close_tab` (`:488`) is the command/Alt+W entry; if a group tab is active it closes the whole
  group, else delegates to `close_tab_in_split` (`:530`), the single shared implementation for
  mouse-×, command, and keybinding. It prompts (`ConfirmCloseBuffer`) only when closing the last
  viewport of a modified buffer; closes the split (not just the tab) when an unmodified buffer is
  the sole tab of a non-last split; and merely re-points the tab when the buffer is still shown
  elsewhere.
- Replacement selection: `resolve_close_replacement` (`:250`) walks the split's focus-history
  LRU (skipping the closing buffer and hidden panels), then any visible buffer, then any keyed
  buffer, then any remaining buffer, and only as a last resort synthesizes a new `[No Name]`
  (`:363`) — marked `hidden_from_tabs` + `synthetic_placeholder` when
  `auto_create_empty_buffer_on_last_buffer_close` is off, so the workspace renders blank while
  still satisfying the "a split always has a buffer" invariant. Returns
  `CloseReplacement { buffer, created_empty, return_to_group }`.
- Cleanup: `purge_buffer_state` (`:395`) removes the buffer from `buffers`, `event_logs`,
  `buffer_metadata`, semantic-token bookkeeping, every split's `open_buffers`/`focus_history`,
  and panel maps. Early cleanup (`:57`) clears preview tracking, saves per-file session state,
  deletes recovery data (`delete_buffer_recovery`, `recovery_actions.rs:552`), and tears down a
  terminal buffer's PTY. LSP `didClose` happens via `disable_lsp_for_buffer`
  (`lsp_actions.rs:997`).

---

## Superseded / consolidated source docs

This doc consolidates and, where code has moved on, supersedes:

- `buffer-groups-design.md` — initial buffer-groups design (one-virtual-buffer panels);
  superseded by the `SplitNode::Grouped` approach below.
- `grouped-splitnode-design.md` — the `Grouped` + `TabTarget` redesign; now IMPLEMENTED
  (see §3); use this doc for the as-built shape.
- `orphaned-leaf-investigation.md` — #1939 root cause; fix shipped (see §7).
- `undo-redo-markers-analysis.md`, `undo-redo-markers-implementation-plan.md`,
  `bulk-edit-marker-displacement.md` — described BulkEdit undo marker displacement as PLANNED;
  it is now IMPLEMENTED via `Event::BulkEdit { edits, displaced_markers }` (see §5, the flagged
  discrepancy).
- `hot-exit-improvements-plan.md` — partially shipped; remaining tab-order/CLI-additive tasks
  still PLANNED (see §6 status).
- `editor-state-refactoring.md`, `editor-modules-refactor-plan.md` — decision #11 refactor;
  only `ComposeState` extracted so far (§4).
- `design-decisions.md` §10 (per-buffer vs per-view state) and §11 (EditorState refactor) are
  summarized in §4.
