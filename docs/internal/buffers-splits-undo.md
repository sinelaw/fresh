# Buffers, Splits, Undo & Recovery (App Layer)

> _AI-generated: describes Fresh's architecture and design rationale, not implementation details; where it disagrees with the source, the source is authoritative._

Purpose: explain how Fresh owns and identifies buffers, lays out split panes via the
window tree, separates per-buffer from per-view state, displaces/preserves markers across
undo/redo, and persists/recovers unsaved work across clean exits and crashes. Features are
IMPLEMENTED unless flagged PLANNED.

---

## 1. Buffer ownership & identity

### Who owns buffers

A `Window` owns its buffers outright — there is no editor-global buffer pool. Each window
has its own content, layout, metadata, and undo logs. A window holds a per-window content
store plus split layout (`WindowBuffers`), per-buffer metadata, per-buffer undo/redo logs,
and a single editor-wide preview anchor.

`WindowBuffers` encapsulates two things behind one type: a map from `BufferId` to buffer
content, and the split layout (a split manager paired with per-leaf view state).

Putting the split tree *inside* `WindowBuffers` is deliberate: a buffer cannot be removed
without the split tree being able to react, and the borrow checker forbids mutating a buffer
and the layout at the same time. Combined-mutation needs go through a small set of closure
accessors (a buffer-and-split accessor, a buffer-and-view-states accessor, and an all-mutable
accessor), deliberately collapsed to three variants. Read accessors expose the splits, the
split manager, and the per-leaf view states; mutable equivalents mirror them.

### Identity

`BufferId` is a `Copy` newtype over an integer (defined in `fresh-core`). IDs are allocated
editor-wide (not per-window) by a lock-free counter so plugin APIs can pass them around
without window qualification. The allocator wraps a shared atomic counter, lives on the
window-resources object, and is cloned (via `Arc`) into every window; its increment uses
relaxed ordering. A setter exists so workspace rehydration can restore the high-water mark.

`LeafId` / `ContainerId` / `SplitId` identify split-tree nodes; `SplitId` is the raw id,
while `LeafId`/`ContainerId` are typed wrappers asserting "this is a leaf" vs "this is a
container."

### Metadata

`BufferMetadata` holds non-content facts: the buffer kind (a file with path and URI, or a
virtual buffer with a mode), display name, read-only and binary flags, LSP bookkeeping
(whether LSP is enabled and what it was opened with), a flag hiding panel/composite source
buffers from the tab bar, a synthetic-placeholder flag for throwaway last-buffer holders, and
a stable recovery id for unnamed buffers across auto-saves.

---

## 2. The split / window tree

### Data structure

The layout is an Emacs-style binary tree of `SplitNode` with three variants:

- `Leaf` — references one buffer, carries a `LeafId`, and an optional split role.
- `Split` — an internal node with a direction, two children, a ratio, a `ContainerId`, and
  optional fixed-size overrides for either child. The ratio (clamped to a sub-unity band)
  gives the first child's fraction; the fixed-size overrides replace the ratio with an
  absolute row/col count, used for headers and docks.
- `Grouped` — a subtree (with a `LeafId`, a name, a boxed inner layout, and an
  active-inner-leaf pointer) that appears as **one tab** in its parent's tab bar.

`SplitManager` owns the tree: the root, the active split (always a leaf), the next-id
counter, an optional maximized split, labels, and a bounded LRU of recently focused leaves. A
"last focused where" query walks the LRU newest-first, skipping leaves that have since closed
— file-open routing uses it to avoid landing files in the Utility Dock.

### Why a binary tree

Splits nest arbitrarily; rectangles are computed by recursive subdivision, reserving one
row/col per separator. Splitting replaces the target node in-place with a new `Split` whose
children are the old node plus a new leaf. Closing a leaf replaces its parent `Split` with the
surviving sibling — so the tree never holds an empty container. A dedicated root-split path
splits the *root* rather than the active leaf, so the Utility Dock spans the full width below
any pre-existing side-by-side panes rather than nesting under whichever pane was active.

### Roles & the Utility Dock

The Utility-Dock split role tags at most one leaf. Diagnostics, search/replace results,
terminals, and quickfix all *swap into* that single tagged leaf instead of spawning new
splits. The "at most one per role" invariant is enforced by callers clearing the role before
setting it on a new leaf.

### Maximize

A maximized-split field makes only that leaf render full-viewport. Next/previous-split
navigation clears it first — otherwise the newly-active leaf would render behind the maximized
one and the cursor would "disappear."

---

## 3. Buffer groups (grouped split nodes)

### Motivation & the design pivot

Multi-panel plugins (diff/merge tools) used to render side-by-side panels inside a *single*
virtual buffer with hand-rolled viewport math — substantial boilerplate per plugin and no
per-panel scrollbar. The buffer-groups design reframed each panel as a *real buffer* in a
split subtree so panels inherit viewport windowing, scrollbars, syntax highlighting, and
drag-to-resize for free. The first implementation produced the **wrong UX** — multiple
side-by-side tabs instead of one. The grouped-`SplitNode` redesign fixed that by adding the
`SplitNode::Grouped` variant plus a `TabTarget` enum: the group is a single tab, and its
subtree expands only when that tab is active.

### TabTarget

A split's tab bar is a vector of `TabTarget`, where a `TabTarget` is either a buffer (by
`BufferId`) or a group (by `LeafId`). A `Grouped` node's `LeafId` is its tab target — it
"behaves like a leaf from the outside." During layout, the leaf-collection pass recurses into
a `Grouped` node only if the active-group predicate says its tab is active, else skips its
whole subtree.

### Implementation

Buffer-group construction parses a JSON layout description (scrollable / fixed / split
panels), creates a hidden virtual buffer per panel, builds the inner `SplitNode` tree plus a
group-layout node, wraps them in `SplitNode::Grouped` (stashed in a grouped-subtrees map keyed
by `LeafId`), creates a chrome-suppressed view state per panel leaf (chrome suppressed, tilde
column hidden, no line numbers), marks panel buffers hidden-from-tabs, and registers the group
tab target in the active split's tab list. Supporting actions set panel content, focus a
panel, activate the group tab, close the group, and re-target a panel at a different
file-backed buffer for streaming plugins. A runtime `BufferGroup` struct tracks the panel
buffers, panel splits, and a representative split.

Per the v1 design, groups have **fixed, plugin-declared layouts**: nested groups are
disallowed and user-initiated split inside a group is blocked (the plugin controls structure,
the user controls panel content).

### Composite buffers (related but distinct)

A *composite buffer* is a synthetic single tab that composes several source buffers
(side-by-side diff, stacked, unified). Unlike groups it is **visible in tabs** and has its own
cursor/selection/viewport. A view-state field marks a split as rendering a composite; its
active-buffer pointer still points at the focused source buffer so normal keybindings route
correctly. The model layer defines the composite buffer, its layout, source panes, and line
alignment; a separate composite view-state type is keyed by split and buffer; and a set of
actions create composites, navigate hunks, and handle composite creation. 3-way merge layouts
and per-pane editability enforcement are scaffolded but not surfaced (PLANNED).

---

## 4. Per-buffer vs per-view state

Content is shared across splits; *view* state is independent per split per buffer. This is the
fix for "the same file in two panes scrolled together."

- `EditorState` (the buffer content plus decorations/highlighting) is owned once in the
  window-buffers content map, keyed by `BufferId`.
- `SplitViewState` is per leaf split: active buffer, the tab list, tab-scroll offset, focus
  history, sync group, composite-view marker, group-tab markers, and a map from `BufferId` to
  `BufferViewState`.
- `BufferViewState` is **per buffer per split**: independent cursors (multi-cursor), viewport
  (scroll), view mode, compose settings, rulers, line-number and current-line-highlight flags
  (with explicit overrides so a pinned per-buffer choice survives restart without freezing
  untouched buffers at a stale global), per-view folds, and a string-keyed JSON plugin-state
  escape hatch so plugins store arbitrary per-buffer-per-split state without Rust enum churn.

`SplitViewState` derefs to its active buffer's `BufferViewState`, so a cursor access
transparently hits the active buffer. Switching buffers lazily creates default view state for
a newly-shown buffer at the split's current dimensions; folds are intentionally *not* cloned
into a new split because fold markers are per-view. Workspace persistence stores per-file view
state in a separate map keyed by path so cursor/scroll survive restarts.

A related decision splits `EditorState`'s mixed concerns into decoration, highlight, and
buffer-flag components. Status: the compose state has been extracted as a proof-of-concept;
the remaining extractions are deferred (PARTIALLY IMPLEMENTED).

Per-buffer config resolution (line wrap, wrap column, page view) is pure and language-aware: a
language override wins over the global editor default, falling back when unset. It is applied
when a buffer is first shown.

---

## 5. Undo / redo with marker displacement

### Where undo lives

Undo is per-buffer: an `EventLog` is keyed by `BufferId` in the window. The app entry points
are thin: handle-undo calls the log's undo, applies each inverse event to the active buffer,
then restores displaced markers; handle-redo replays forward events. Both refuse when editing
is disabled and recompute the modified flag from the log position afterward.

### Markers

"Markers" are byte-position anchors in an interval-tree-backed marker list, backing virtual
text (inlay hints, ghost text), overlays (diagnostic underlines, search highlights, semantic
tokens — each a start/end marker pair), and margins (breakpoints, line annotations). They
shift through edits via insert and delete adjustment.

### The two problems and the implemented solution

1. **Forward displacement**: bulk edits (toggle-comment, cut, paste, indent) go through a
   bulk-edit apply path, which adjusts markers and margins inline as it edits.

2. **Undo displacement**: a `BulkEdit` undo restores the buffer by swapping a piece-tree
   snapshot atomically — there are no per-edit insert/deletes to drive marker adjustment, so
   markers would be left at post-edit positions. Earlier planning docs described the fix as
   PLANNED. It is now **IMPLEMENTED.** The `BulkEdit` event carries two extra fields:
   - A list of edits, each a `(position, delete_len, insert_len)` triple, ordered descending
     by position. They are replayed as-is on redo; on undo, the event's inverse swaps the
     delete and insert lengths so the reverse marker adjustments apply.
   - A list of displaced markers, each a `(marker_id, original_byte_position)` pair. After
     adjustment, these markers are snapped back to their exact pre-edit positions, recovering
     the spacing that a range-delete would otherwise collapse.

   The log entry also stores the displaced markers. The log's undo returns each inverse event
   paired with the displaced markers from the original entry. The undo path applies the
   inverse, then for non-`BulkEdit` events restores the displaced markers; for `BulkEdit` it
   *skips* that because applying the `BulkEdit` event handles displaced markers internally via
   the event's own field. Commit history also covers a marker-id-collision fix between the
   marker list and margins, and a helper extraction tracking the LSP net delta. After hot-exit
   replay, LSP is re-synced so semantic tokens don't drift.

### Undo groups

A runtime-only (not persisted) group id on each log entry lets entries sharing an id undo/redo
as one atomic unit (e.g. a whole macro replay): undo keeps consuming entries in the same
group.

### Fundamental limitation (accepted)

Deleting a range collapses all interior markers to the deletion start; re-inserting on undo
can't reconstruct their original internal spread — the displaced-markers list recovers the
*endpoints*, but the design (in line with VSCode, Neovim, Emacs, and Helix) accepts the
in-range spread as lost. The visibility window is brief because LSP re-pushes corrected
positions. Snapshot-based marker restoration (Atom's approach) was explicitly rejected as too
error-prone.

---

## 6. Hot-exit & crash recovery

Design principle: *never silently discard unsaved data* — unsaved buffers disappear only
through explicit user action; exit, reopen, session-switch, and CLI all preserve them. CLI
file args are *additive* to a restored workspace, never replacive.

### Storage format (IMPLEMENTED)

The recovery service uses a unified chunked format (originally introduced as Emacs-style file
recovery). The base directory lives under the user data directory, scoped by mode:
- Standalone: a per-working-directory subdirectory.
- Session (daemon) mode: a per-session-name subdirectory.

Per recoverable buffer:
- A metadata JSON file (`RecoveryMetadata`: original path, buffer name, timestamps, original
  mtime, original file size, chunk count, format version) plus an embedded chunked-recovery
  index.
- Chunk files — raw binary content per recovery chunk (offset, original length, content),
  bounded by a maximum chunk size.

Small or unnamed buffers store full content as a single chunk with the original file size
recorded as zero ("new buffer"); large files store only modified chunks and replay them in
reverse offset order on restore. Writes are atomic via temp-file plus rename; note **no
fsync** — this is editor-crash safe, not OS-crash safe.

### Persist path

- An auto-recovery save runs every frame, rate-limited by a configurable interval; it only
  writes buffers carrying the recovery-pending flag.
- The pending-flush step assigns stable recovery ids to unnamed buffers and writes them; a
  per-buffer save step builds the chunks.
- On clean exit: if hot-exit is enabled, mark all modified buffers recovery-pending, flush,
  compute the ids to preserve (skipping hidden/virtual/empty-unnamed buffers), and end the
  session preserving those ids. If hot-exit is off, end the session normally (recovery
  cleared).

### Restore path

- Crash detection via a PID-bearing session lock: if a lock exists but its PID is no longer
  running (a zero-signal probe on Unix / an exit-code query on Windows), the previous run
  crashed.
- The recover-all path **consumes** recovery files: per entry it loads content, opens the path
  (or creates an unnamed buffer), replaces content, marks modified, and re-syncs LSP. Large
  files apply chunks in reverse.
- The clean-exit counterpart loads recovery data **without deleting**, leaving files for the
  current session's hot-exit pipeline to own (decoupled from session restore, gated behind a
  restore flag).
- **mtime conflict trade-off**: if the on-disk file changed since the snapshot, recovery is
  *not* silently discarded — the editor opens current disk contents, warns in the status bar,
  and **keeps** the recovery file for manual inspection. This reverses the older behavior
  (silent skip plus delete).
- Stale cleanup: an orphan sweep (metadata without chunks or vice-versa) and age-based pruning
  with a configurable maximum age.

The recovery service is shared across windows via the window-resources object, so restore is
fully window-pure.

### Quit flow

Quit counts modified buffers needing a prompt (excluding unnamed buffers under hot-exit and
file-backed buffers under auto-save). With dirty buffers it shows a confirm-quit-with-modified
prompt; when hot-exit is enabled the prompt includes a "Quit (recoverable)" option that exits
without saving and lets the recovery session preserve the changes. On confirmation: **Save**
runs the save-all-on-exit path then a SaveAs chain for unnamed buffers; **Discard** clears the
modified and recovery-pending flags on all buffers (so nothing is preserved); **Quit** sets
the should-quit flag and relies on hot-exit.

### Status

IMPLEMENTED: chunked format, atomic writes, PID-lock crash detection, hot-exit
persist/restore, mtime-conflict warning, session-scoped recovery dirs, the discard-on-quit
option. PLANNED (planning-doc tasks not yet shipped): explicit ordered tab-array
serialization, CLI-files-are-additive behavior, and some session-plus-CLI restore flows.

---

## 7. Restore invariants & the orphaned-leaf bug

After a restore, three things must agree for a split: the split manager's leaf `buffer_id`,
the split view state's active buffer, and the leaf's presence in the tab list.

A documented blank-pane-on-restore bug: an empty `[No Name]` seed buffer was left as a split's
active leaf but absent from the tab list, so the orphaned-buffer cleanup (which builds its
referenced set from tab ids only) removed it, leaving the leaf dangling at a dead `BufferId`.
Two layers respond:

- **Fix at source**: when restoring split view state, if the saved active tab can't be
  resolved, fall back to the first surviving tab via the normal switch-buffer plus
  set-split-buffer path, so all three views agree and the seed is then correctly orphaned.
- **Defensive guard**: an effective-active-pair query — if the active split's buffer isn't in
  the content map, it logs a warning and falls back to any live buffer for status-bar queries.
  This masks the symptom for queries only; it does not repair the leaf, and is intentionally
  kept to surface future invariant violations.

A regression test covers restoring an orphaned active unnamed tab and rendering the surviving
tab.

---

## 8. Buffer close lifecycle

- A close-buffer entry errors if the buffer is modified; a force-close entry closes
  unconditionally. Both funnel into a shared internal close.
- The close-tab entry (command / Alt+W) closes the whole group if a group tab is active, else
  delegates to a single shared per-split close implementation used by mouse-×, command, and
  keybinding. It prompts only when closing the last viewport of a modified buffer; closes the
  split (not just the tab) when an unmodified buffer is the sole tab of a non-last split; and
  merely re-points the tab when the buffer is still shown elsewhere.
- Replacement selection walks the split's focus-history LRU (skipping the closing buffer and
  hidden panels), then any visible buffer, then any keyed buffer, then any remaining buffer,
  and only as a last resort synthesizes a new `[No Name]` — marked hidden-from-tabs plus
  synthetic-placeholder when auto-creating an empty buffer on last-buffer close is off, so the
  workspace renders blank while still satisfying the "a split always has a buffer" invariant.
  It returns a close-replacement record (buffer, created-empty flag, return-to-group flag).
- Cleanup: a purge step removes the buffer from the content map, undo logs, metadata,
  semantic-token bookkeeping, every split's tab list and focus history, and the panel maps.
  Early cleanup clears preview tracking, saves per-file session state, deletes recovery data,
  and tears down a terminal buffer's PTY. LSP `didClose` happens via the per-buffer
  LSP-disable path.

---

## Superseded / consolidated source docs

This doc consolidates and, where code has moved on, supersedes:

- The initial buffer-groups design (one-virtual-buffer panels); superseded by the
  `SplitNode::Grouped` approach (see §3).
- The grouped-`SplitNode` redesign (`Grouped` plus `TabTarget`); now IMPLEMENTED (see §3); use
  this doc for the as-built shape.
- The orphaned-leaf investigation; root cause documented and fix shipped (see §7).
- The undo/redo marker analysis and implementation plan, and the bulk-edit marker-displacement
  doc — described `BulkEdit` undo marker displacement as PLANNED; it is now IMPLEMENTED via the
  `BulkEdit` event's edits and displaced-markers fields (see §5).
- The hot-exit improvements plan — partially shipped; remaining tab-order and CLI-additive
  tasks still PLANNED (see §6 status).
- The editor-state refactoring and editor-modules refactor plans; only the compose state has
  been extracted so far (see §4).
- The design-decisions entries on per-buffer vs per-view state and the `EditorState` refactor
  are summarized in §4.
