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
  toolbarBufferId: number | null;
  /** Click-regions for the toolbar's buttons, populated by `renderToolbar`. */
  toolbarButtons: ToolbarButton[];
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
  toolbarBufferId: null,
  toolbarButtons: [],
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

// j/k as vi-style aliases for Up/Down, plus the plugin-specific action
// keys. Everything else (arrows, Page{Up,Down}, Home/End, Shift+motion for
// selection, Ctrl+C copy, …) is inherited from the Normal keymap because
// the mode is registered with `inheritNormalBindings: true`.
editor.defineMode(
  "git-log",
  [
    ["k", "move_up"],
    ["j", "move_down"],
    ["Return", "git_log_enter"],
    ["Tab", "git_log_tab"],
    ["q", "git_log_q"],
    ["r", "git_log_refresh"],
    ["y", "git_log_copy_hash"],
  ],
  true, // read-only
  false, // allow_text_input
  true, // inherit Normal-context bindings for unbound keys
);

// =============================================================================
// Panel layout
// =============================================================================

/**
 * Group buffer layout — a one-row sticky toolbar on top, then a horizontal
 * split below with the commit log on the left (60%) and detail on the
 * right (40%). The toolbar mirrors the review-diff style: a fixed-height
 * panel above the scrollable content that holds all the keybinding hints
 * so they don't shift or scroll with the data.
 */
const GROUP_LAYOUT = JSON.stringify({
  type: "split",
  direction: "v",
  ratio: 0.05, // ignored when one side is `fixed`
  first: { type: "fixed", id: "toolbar", height: 1 },
  second: {
    type: "split",
    direction: "h",
    ratio: 0.6,
    first: { type: "scrollable", id: "log" },
    second: { type: "scrollable", id: "detail" },
  },
});

// =============================================================================
// Toolbar
// =============================================================================

interface ToolbarHint {
  key: string;
  label: string;
  /** Click action — `null` for hints that are keyboard-only (j/k, PgUp). */
  onClick: (() => void | Promise<void>) | null;
}

interface ToolbarButton {
  row: number;
  startCol: number;
  endCol: number;
  onClick: (() => void | Promise<void>) | null;
}

function toolbarHints(): ToolbarHint[] {
  return [
    { key: "Tab", label: "switch pane", onClick: git_log_tab },
    { key: "RET", label: "open file", onClick: git_log_enter },
    { key: "y", label: "copy hash", onClick: git_log_copy_hash },
    { key: "r", label: "refresh", onClick: git_log_refresh },
    { key: "q", label: "quit", onClick: git_log_q },
  ];
}

/**
 * Build the single-row toolbar. Each hint renders as a discrete button with
 * its own background so it reads as clickable; the column range of each
 * button is captured in `state.toolbarButtons` so `on_git_log_toolbar_click`
 * can map a mouse click back to the right handler.
 */
function buildToolbarEntries(width: number): TextPropertyEntry[] {
  const W = Math.max(20, width);
  const buttons: ToolbarButton[] = [];
  let text = "";
  const overlays: InlineOverlay[] = [];

  for (const hint of toolbarHints()) {
    const body = ` [${hint.key}] ${hint.label} `;
    const bodyLen = body.length;
    const gap = text.length > 0 ? 1 : 0;
    if (text.length + gap + bodyLen > W) break;

    if (gap) text += " ";

    const startCol = text.length;
    const startByte = utf8Len(text);
    text += body;
    const endByte = utf8Len(text);
    const endCol = text.length;

    overlays.push({
      start: startByte,
      end: endByte,
      style: { bg: "ui.status_bar_bg" },
    });
    const keyDisplay = `[${hint.key}]`;
    const keyStartByte = startByte + utf8Len(" ");
    const keyEndByte = keyStartByte + utf8Len(keyDisplay);
    overlays.push({
      start: keyStartByte,
      end: keyEndByte,
      style: { fg: "editor.fg", bold: true },
    });
    overlays.push({
      start: keyEndByte,
      end: endByte,
      style: { fg: "editor.line_number_fg" },
    });

    buttons.push({ row: 0, startCol, endCol, onClick: hint.onClick });
  }

  state.toolbarButtons = buttons;

  return [
    {
      text: text + "\n",
      properties: { type: "git-log-toolbar" },
      style: { bg: "editor.bg", extendToLineEnd: true },
      inlineOverlays: overlays,
    },
  ];
}

function renderToolbar(): void {
  if (state.groupId === null) return;
  const vp = editor.getViewport();
  const width = vp ? vp.width : 80;
  editor.setPanelContent(state.groupId, "toolbar", buildToolbarEntries(width));
}

function on_git_log_toolbar_click(data: {
  buffer_id: number | null;
  buffer_row: number | null;
  buffer_col: number | null;
}): void {
  if (!state.isOpen) return;
  if (data.buffer_id === null || data.buffer_id !== state.toolbarBufferId) return;
  if (data.buffer_row === null || data.buffer_col === null) return;
  const row = data.buffer_row;
  const col = data.buffer_col;
  const hit = state.toolbarButtons.find(
    (b) => b.row === row && col >= b.startCol && col < b.endCol
  );
  if (hit && hit.onClick) {
    void hit.onClick();
  }
}
registerHandler("on_git_log_toolbar_click", on_git_log_toolbar_click);

function on_git_log_resize(_data: { width: number; height: number }): void {
  if (!state.isOpen) return;
  renderToolbar();
}
registerHandler("on_git_log_resize", on_git_log_resize);

// =============================================================================
// Rendering
// =============================================================================

function detailFooter(hash: string): string {
  return editor.t("status.commit_ready", { hash });
}

function renderLog(): void {
  if (state.groupId === null) return;
  // No header row and no footer: the sticky toolbar above the group
  // carries the shortcut hints, and the commit count goes to the status
  // line when the group opens.
  const entries = buildCommitLogEntries(state.commits, {
    selectedIndex: state.selectedIndex,
    header: null,
  });
  // Rebuild the byte-offset table used by cursor_moved to map positions
  // to commit indices. `offsets[i]` is the byte offset of commit i; the
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
  const entries = buildCommitDetailEntries(commit, showOutput);
  editor.setPanelContent(state.groupId, "detail", entries);
  // Always scroll the detail panel back to the top when the selection changes.
  if (state.detailBufferId !== null) {
    editor.setBufferCursor(state.detailBufferId, 0);
  }
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
  // No header row — row 0 is commit 0.
  const idx = rowFromByte(bytePos);
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
  state.toolbarBufferId = (group.panels["toolbar"] as number | undefined) ?? null;
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
    // Wrap long lines in the detail panel — git diffs often exceed the
    // 40% split width, and horizontal scrolling a commit is awkward.
    editor.setLineWrap(state.detailBufferId, null, true);
    // Per-panel mode: the group was created with "git-log" which applies
    // to the initially-focused panel (log). The detail panel's mode is
    // set when we focus into it.
  }

  renderToolbar();
  renderLog();
  // Position the cursor on the first commit (row 0 now that the header
  // row is gone).
  if (state.logBufferId !== null && state.commits.length > 0) {
    editor.setBufferCursor(state.logBufferId, 0);
  }
  await refreshDetail();

  if (state.groupId !== null) {
    editor.focusBufferGroupPanel(state.groupId, "log");
  }
  editor.on("cursor_moved", "on_git_log_cursor_moved");
  editor.on("mouse_click", "on_git_log_toolbar_click");
  editor.on("resize", "on_git_log_resize");
  editor.on("buffer_closed", "on_git_log_buffer_closed");

  editor.setStatus(
    editor.t("status.log_ready", { count: String(state.commits.length) })
  );
}
registerHandler("show_git_log", show_git_log);

/** Reset all state + unsubscribe. Idempotent; safe to call from either
 * path (user-initiated close or externally-closed group via the tab's
 * close button, which triggers `buffer_closed`). */
function git_log_cleanup(): void {
  if (!state.isOpen) return;
  editor.off("cursor_moved", "on_git_log_cursor_moved");
  editor.off("mouse_click", "on_git_log_toolbar_click");
  editor.off("resize", "on_git_log_resize");
  editor.off("buffer_closed", "on_git_log_buffer_closed");
  state.isOpen = false;
  state.groupId = null;
  state.logBufferId = null;
  state.detailBufferId = null;
  state.toolbarBufferId = null;
  state.toolbarButtons = [];
  state.commits = [];
  state.selectedIndex = 0;
  state.detailCache = null;
}

function git_log_close(): void {
  if (!state.isOpen) return;
  const groupId = state.groupId;
  git_log_cleanup();
  if (groupId !== null) {
    editor.closeBufferGroup(groupId);
  }
  editor.setStatus(editor.t("status.closed"));
}
registerHandler("git_log_close", git_log_close);

function on_git_log_buffer_closed(data: { buffer_id: number }): void {
  if (!state.isOpen) return;
  if (
    data.buffer_id === state.logBufferId ||
    data.buffer_id === state.detailBufferId ||
    data.buffer_id === state.toolbarBufferId
  ) {
    git_log_cleanup();
  }
}
registerHandler("on_git_log_buffer_closed", on_git_log_buffer_closed);

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

  // `*<hash>:<path>*` matches the virtual-name convention the host uses
  // to detect syntax from the trailing filename's extension.
  const name = `*${commit.shortHash}:${file}*`;
  const view = await editor.createVirtualBuffer({
    name,
    mode: "git-log-file-view",
    readOnly: true,
    editingDisabled: true,
    showLineNumbers: true,
    entries,
  });
  if (view) {
    const byte = await editor.getLineStartPosition(Math.max(0, line - 1));
    if (byte !== null) editor.setBufferCursor(view.bufferId, byte);
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
