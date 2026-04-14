/// <reference path="./lib/fresh.d.ts" />

import {
  type GitCommit,
  buildCommitDetailEntries,
  buildCommitLogEntries,
  buildDetailPlaceholderEntries,
  fetchCommitShow,
  fetchGitLog,
} from "./lib/git_history.ts";

const editor = getEditor();

/**
 * Git Log Plugin — Magit-style git history interface built on top of the
 * modern plugin API primitives:
 *
 *   * `createBufferGroup` for a side-by-side "log | detail" layout that
 *     appears as a single tab with its own inner scroll state.
 *   * `setPanelContent` with `TextPropertyEntry[]` + `inlineOverlays` for
 *     aligned columns and per-theme colouring (every colour is a theme key,
 *     so the panel follows theme changes).
 *   * `cursor_moved` subscription to live-update the right-hand detail panel
 *     as the user scrolls through the commit list.
 *
 * The rendering helpers live in `lib/git_history.ts` so the same commit-list
 * view can be reused by `audit_mode`'s PR-branch review mode.
 */

// =============================================================================
// State
// =============================================================================

interface GitLogState {
  isOpen: boolean;
  groupId: number | null;
  logBufferId: number | null;
  detailBufferId: number | null;
  commits: GitCommit[];
  selectedIndex: number;
  /** Cached `git show` output for the currently-displayed detail commit. */
  detailCache: { hash: string; output: string } | null;
  /**
   * In-flight detail request id. Used to ignore stale responses when the
   * user scrolls through the log faster than `git show` can return.
   */
  pendingDetailId: number;
  /**
   * Debounce token for `cursor_moved`. Rapid cursor motion (PageDown, held
   * j/k) would otherwise trigger a full log re-render + `git show` per
   * intermediate row; we bump this id on every event and only do the work
   * after a short delay if no newer event has arrived.
   */
  pendingCursorMoveId: number;
  /**
   * Byte offset at the start of each row in the rendered log panel, plus
   * the total buffer length at the end. Populated by `renderLog` so the
   * cursor_moved handler can map byte positions to commit indices without
   * relying on `getCursorLine` (which is not implemented for virtual
   * buffers).
   */
  logRowByteOffsets: number[];
}

const state: GitLogState = {
  isOpen: false,
  groupId: null,
  logBufferId: null,
  detailBufferId: null,
  commits: [],
  selectedIndex: 0,
  detailCache: null,
  pendingDetailId: 0,
  pendingCursorMoveId: 0,
  logRowByteOffsets: [],
};

/**
 * Delay before reacting to `cursor_moved`. Long enough to collapse a burst
 * of events from held j/k or PageDown into a single render, short enough
 * that the detail panel still feels live.
 */
const CURSOR_DEBOUNCE_MS = 60;

// UTF-8 byte length — the overlay API expects byte offsets; JS strings are
// UTF-16. Matches the helper used by `lib/git_history.ts`.
function utf8Len(s: string): number {
  let b = 0;
  for (let i = 0; i < s.length; i++) {
    const c = s.charCodeAt(i);
    if (c <= 0x7f) b += 1;
    else if (c <= 0x7ff) b += 2;
    else if (c >= 0xd800 && c <= 0xdfff) {
      b += 4;
      i++;
    } else b += 3;
  }
  return b;
}

/**
 * Binary search `logRowByteOffsets` for the 0-indexed row whose byte
 * offset is the largest one ≤ `bytePos`. Returns 0 on an empty table.
 */
function rowFromByte(bytePos: number): number {
  const offs = state.logRowByteOffsets;
  if (offs.length === 0) return 0;
  let lo = 0;
  let hi = offs.length - 1;
  while (lo < hi) {
    const mid = (lo + hi + 1) >> 1;
    if (offs[mid] <= bytePos) lo = mid;
    else hi = mid - 1;
  }
  return lo;
}

// =============================================================================
// Modes
//
// A buffer group has a single mode shared by all of its panels, so the
// handlers below branch on which panel currently has focus to do the
// right thing (`Return` jumps into the detail panel when pressed in
// the log, and opens the file at the cursor when pressed in the detail).
// =============================================================================

editor.defineMode(
  "git-log",
  [
    // Arrow / vi motion — mode bindings replace globals, so we re-bind the
    // editor's built-in move actions here explicitly. Without this, j/k
    // and Up/Down do nothing in the log panel.
    ["Up", "move_up"],
    ["Down", "move_down"],
    ["k", "move_up"],
    ["j", "move_down"],
    ["PageUp", "page_up"],
    ["PageDown", "page_down"],
    ["Home", "move_line_start"],
    ["End", "move_line_end"],
    // Plugin actions.
    ["Return", "git_log_enter"],
    ["Tab", "git_log_tab"],
    ["q", "git_log_q"],
    ["Escape", "git_log_q"],
    ["r", "git_log_refresh"],
    ["y", "git_log_copy_hash"],
  ],
  true // read-only
);

// =============================================================================
// Panel layout
// =============================================================================

/**
 * Group buffer layout — a vertical split: commit log on the left (60%),
 * detail on the right (40%). Uses the runtime's JSON layout schema.
 */
const GROUP_LAYOUT = JSON.stringify({
  type: "split",
  direction: "h", // horizontal split = side by side
  ratio: 0.6,
  first: { type: "scrollable", id: "log" },
  second: { type: "scrollable", id: "detail" },
});

// =============================================================================
// Rendering
// =============================================================================

function logFooter(count: number): string {
  return editor.t("panel.log_footer", { count: String(count) });
}

function detailFooter(hash: string): string {
  return editor.t("status.commit_ready", { hash });
}

function renderLog(): void {
  if (state.groupId === null) return;
  const entries = buildCommitLogEntries(state.commits, {
    selectedIndex: state.selectedIndex,
    header: editor.t("panel.commits_header"),
    footer: logFooter(state.commits.length),
  });
  // Rebuild the byte-offset table used by cursor_moved to map positions
  // to commit indices. `offsets[i]` is the byte offset of row i; the
  // final entry is the total buffer length, so row lookups clamp
  // correctly on the last row.
  const offsets: number[] = [];
  let running = 0;
  for (const e of entries) {
    offsets.push(running);
    running += utf8Len(e.text);
  }
  offsets.push(running);
  state.logRowByteOffsets = offsets;
  editor.setPanelContent(state.groupId, "log", entries);
}

/** Byte offset of the first commit row (i.e. row 1 — row 0 is the header). */
function byteOffsetOfFirstCommit(): number {
  return state.logRowByteOffsets.length > 1 ? state.logRowByteOffsets[1] : 0;
}

function renderDetailPlaceholder(message: string): void {
  if (state.groupId === null) return;
  editor.setPanelContent(
    state.groupId,
    "detail",
    buildDetailPlaceholderEntries(message)
  );
}

function renderDetailForCommit(commit: GitCommit, showOutput: string): void {
  if (state.groupId === null) return;
  const entries = buildCommitDetailEntries(commit, showOutput, {
    footer: editor.t("panel.detail_footer"),
  });
  editor.setPanelContent(state.groupId, "detail", entries);
}

/**
 * Fetch + render the detail panel for the selected commit. Multiple rapid
 * calls can overlap; we tag each call with an id and only render the most
 * recent one so the user's final selection always wins.
 */
async function refreshDetail(): Promise<void> {
  if (state.groupId === null) return;
  if (state.commits.length === 0) {
    renderDetailPlaceholder(editor.t("status.no_commits"));
    return;
  }
  const idx = Math.max(0, Math.min(state.selectedIndex, state.commits.length - 1));
  const commit = state.commits[idx];
  if (!commit) return;

  // Cache hit — render immediately, no git invocation.
  if (state.detailCache && state.detailCache.hash === commit.hash) {
    renderDetailForCommit(commit, state.detailCache.output);
    return;
  }

  const myId = ++state.pendingDetailId;
  renderDetailPlaceholder(
    editor.t("status.loading_commit", { hash: commit.shortHash })
  );
  const output = await fetchCommitShow(editor, commit.hash);
  // Discard stale result if the user moved on.
  if (myId !== state.pendingDetailId) return;
  if (state.groupId === null) return;
  state.detailCache = { hash: commit.hash, output };
  renderDetailForCommit(commit, output);
}

// =============================================================================
// Selection tracking — keeps `state.selectedIndex` in sync with the log
// panel's native cursor so the highlight and detail stay consistent.
// =============================================================================

function selectedCommit(): GitCommit | null {
  if (state.commits.length === 0) return null;
  const i = Math.max(0, Math.min(state.selectedIndex, state.commits.length - 1));
  return state.commits[i] ?? null;
}

function indexFromCursorByte(bytePos: number): number {
  // Row 0 is the header; commits live at rows 1..N.
  const row0 = rowFromByte(bytePos);
  const idx = row0 - 1;
  if (idx < 0) return 0;
  if (idx >= state.commits.length) return state.commits.length - 1;
  return idx;
}

// =============================================================================
// Commands
// =============================================================================

async function show_git_log(): Promise<void> {
  if (state.isOpen) {
    editor.setStatus(editor.t("status.already_open"));
    return;
  }
  editor.setStatus(editor.t("status.loading"));

  state.commits = await fetchGitLog(editor);
  if (state.commits.length === 0) {
    editor.setStatus(editor.t("status.no_commits"));
    return;
  }

  // `createBufferGroup` is not currently included in the generated
  // `EditorAPI` type (it's a runtime-only binding, same as in audit_mode),
  // so we cast to `any` to keep the type checker happy.
  const group = await (editor as any).createBufferGroup(
    "*Git Log*",
    "git-log",
    GROUP_LAYOUT
  );
  state.groupId = group.groupId as number;
  state.logBufferId = (group.panels["log"] as number | undefined) ?? null;
  state.detailBufferId = (group.panels["detail"] as number | undefined) ?? null;
  state.selectedIndex = 0;
  state.detailCache = null;
  state.isOpen = true;

  // The log panel owns a native cursor so j/k/Up/Down navigate commits,
  // and the detail panel also gets a cursor so diff lines can be clicked
  // / traversed before pressing Enter to open a file.
  if (state.logBufferId !== null) {
    editor.setBufferShowCursors(state.logBufferId, true);
  }
  if (state.detailBufferId !== null) {
    editor.setBufferShowCursors(state.detailBufferId, true);
    // Per-panel mode: the group was created with "git-log" which applies
    // to the initially-focused panel (log). The detail panel's mode is
    // set when we focus into it.
  }

  renderLog();
  // Position the cursor on the first commit row (row index 1 — row 0 is
  // the "Commits:" header).
  if (state.logBufferId !== null && state.commits.length > 0) {
    editor.setBufferCursor(state.logBufferId, byteOffsetOfFirstCommit());
  }
  await refreshDetail();

  if (state.groupId !== null) {
    editor.focusBufferGroupPanel(state.groupId, "log");
  }
  editor.on("cursor_moved", "on_git_log_cursor_moved");

  editor.setStatus(
    editor.t("status.log_ready", { count: String(state.commits.length) })
  );
}
registerHandler("show_git_log", show_git_log);

function git_log_close(): void {
  if (!state.isOpen) return;
  if (state.groupId !== null) {
    editor.closeBufferGroup(state.groupId);
  }
  editor.off("cursor_moved", "on_git_log_cursor_moved");
  state.isOpen = false;
  state.groupId = null;
  state.logBufferId = null;
  state.detailBufferId = null;
  state.commits = [];
  state.selectedIndex = 0;
  state.detailCache = null;
  editor.setStatus(editor.t("status.closed"));
}
registerHandler("git_log_close", git_log_close);

async function git_log_refresh(): Promise<void> {
  if (!state.isOpen) return;
  editor.setStatus(editor.t("status.refreshing"));
  state.commits = await fetchGitLog(editor);
  state.detailCache = null;
  if (state.selectedIndex >= state.commits.length) {
    state.selectedIndex = Math.max(0, state.commits.length - 1);
  }
  renderLog();
  await refreshDetail();
  editor.setStatus(
    editor.t("status.refreshed", { count: String(state.commits.length) })
  );
}
registerHandler("git_log_refresh", git_log_refresh);

function git_log_copy_hash(): void {
  const commit = selectedCommit();
  if (!commit) {
    editor.setStatus(editor.t("status.move_to_commit"));
    return;
  }
  editor.copyToClipboard(commit.hash);
  editor.setStatus(
    editor.t("status.hash_copied", {
      short: commit.shortHash,
      full: commit.hash,
    })
  );
}
registerHandler("git_log_copy_hash", git_log_copy_hash);

/** Is the detail panel the currently-focused buffer? */
function isDetailFocused(): boolean {
  return (
    state.detailBufferId !== null &&
    editor.getActiveBufferId() === state.detailBufferId
  );
}

function git_log_tab(): void {
  if (state.groupId === null) return;
  if (isDetailFocused()) {
    editor.focusBufferGroupPanel(state.groupId, "log");
  } else {
    editor.focusBufferGroupPanel(state.groupId, "detail");
    const commit = selectedCommit();
    if (commit) editor.setStatus(detailFooter(commit.shortHash));
  }
}
registerHandler("git_log_tab", git_log_tab);

/**
 * Enter: on the log panel jumps focus into the detail panel; on the detail
 * panel opens the file at the cursor position (if any).
 */
function git_log_enter(): void {
  if (state.groupId === null) return;
  if (isDetailFocused()) {
    git_log_detail_open_file();
    return;
  }
  editor.focusBufferGroupPanel(state.groupId, "detail");
  const commit = selectedCommit();
  if (commit) editor.setStatus(detailFooter(commit.shortHash));
}
registerHandler("git_log_enter", git_log_enter);

/**
 * q/Escape: closes the entire log group when the log panel is focused,
 * otherwise steps back into the log panel (so the user's mental model
 * matches the previous "detail is a stacked view on top of the log").
 */
function git_log_q(): void {
  if (state.groupId === null) return;
  if (isDetailFocused()) {
    editor.focusBufferGroupPanel(state.groupId, "log");
    editor.setStatus(
      editor.t("status.log_ready", { count: String(state.commits.length) })
    );
    return;
  }
  git_log_close();
}
registerHandler("git_log_q", git_log_q);

// =============================================================================
// Detail panel — open file at commit
// =============================================================================

async function git_log_detail_open_file(): Promise<void> {
  if (state.detailBufferId === null) return;
  const commit = selectedCommit();
  if (!commit) return;

  const props = editor.getTextPropertiesAtCursor(state.detailBufferId);
  if (props.length === 0) {
    editor.setStatus(editor.t("status.move_to_diff"));
    return;
  }
  const file = props[0].file as string | undefined;
  const line = (props[0].line as number | undefined) ?? 1;
  if (!file) {
    editor.setStatus(editor.t("status.move_to_diff_with_context"));
    return;
  }

  editor.setStatus(
    editor.t("status.file_loading", { file, hash: commit.shortHash })
  );
  const result = await editor.spawnProcess("git", [
    "show",
    `${commit.hash}:${file}`,
  ]);
  if (result.exit_code !== 0) {
    editor.setStatus(
      editor.t("status.file_not_found", { file, hash: commit.shortHash })
    );
    return;
  }

  const lines = result.stdout.split("\n");
  const entries: TextPropertyEntry[] = lines.map((l, i) => ({
    text: l + (i < lines.length - 1 ? "\n" : ""),
    properties: { type: "content", line: i + 1 },
  }));

  const name = `${file} @ ${commit.shortHash}`;
  const view = await editor.createVirtualBuffer({
    name,
    mode: "git-log-file-view",
    readOnly: true,
    editingDisabled: true,
    showLineNumbers: true,
    entries,
  });
  if (view) {
    // Position cursor near target line — best-effort; the host may not
    // have a byte offset for virtual buffer lines until layout runs.
    editor.setStatus(
      editor.t("status.file_view_ready", {
        file,
        hash: commit.shortHash,
        line: String(line),
      })
    );
  } else {
    editor.setStatus(editor.t("status.failed_open_file", { file }));
  }
}
registerHandler("git_log_detail_open_file", git_log_detail_open_file);

// File-view mode so `q` closes the tab and returns to the group.
editor.defineMode(
  "git-log-file-view",
  [
    ["q", "git_log_file_view_close"],
    ["Escape", "git_log_file_view_close"],
  ],
  true
);

function git_log_file_view_close(): void {
  const id = editor.getActiveBufferId();
  if (id) editor.closeBuffer(id);
}
registerHandler("git_log_file_view_close", git_log_file_view_close);

// =============================================================================
// Cursor tracking — live-update the detail panel as the user scrolls through
// the commit list.
// =============================================================================

async function on_git_log_cursor_moved(data: {
  buffer_id: number;
  cursor_id: number;
  old_position: number;
  new_position: number;
}): Promise<void> {
  if (!state.isOpen) return;
  // Only react to movement inside the log panel.
  if (data.buffer_id !== state.logBufferId) return;

  // Map the cursor's byte offset to a commit index via the row-offset
  // table built in `renderLog`. This avoids relying on `getCursorLine`
  // which is not implemented for virtual buffers.
  const idx = indexFromCursorByte(data.new_position);
  if (idx === state.selectedIndex) return;
  state.selectedIndex = idx;

  // Debounce: bump the token, wait a beat, bail if a newer event has
  // arrived. The log re-render and `git show` are both expensive; a burst
  // of cursor events (held j/k, PageDown) must collapse to one render.
  const myId = ++state.pendingCursorMoveId;
  await editor.delay(CURSOR_DEBOUNCE_MS);
  if (myId !== state.pendingCursorMoveId) return;
  if (!state.isOpen) return;

  renderLog();
  refreshDetail();

  const commit = state.commits[state.selectedIndex];
  if (commit) {
    editor.setStatus(
      editor.t("status.commit_position", {
        current: String(state.selectedIndex + 1),
        total: String(state.commits.length),
      })
    );
  }
}
registerHandler("on_git_log_cursor_moved", on_git_log_cursor_moved);

// =============================================================================
// Command registration
// =============================================================================

editor.registerCommand(
  "%cmd.git_log",
  "%cmd.git_log_desc",
  "show_git_log",
  null
);
editor.registerCommand(
  "%cmd.git_log_close",
  "%cmd.git_log_close_desc",
  "git_log_close",
  null
);
editor.registerCommand(
  "%cmd.git_log_refresh",
  "%cmd.git_log_refresh_desc",
  "git_log_refresh",
  null
);

editor.debug("Git Log plugin initialized (modern buffer-group layout)");
