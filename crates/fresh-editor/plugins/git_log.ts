/// <reference path="./lib/fresh.d.ts" />

import {
  type GitCommit,
  buildCommitDetailEntries,
  buildCommitLogEntries,
  buildDetailPlaceholderEntries,
  fetchCommitShow,
  fetchGitLog,
} from "./lib/git_history.ts";
import { button, flexSpacer, key, list, row, WidgetPanel } from "./lib/index.ts";

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
  /** Widget panel rendering the toolbar (Row of Buttons). Created in
   * `show_git_log` once the buffer group exists; cleaned up in
   * `git_log_close`. */
  toolbarPanel: WidgetPanel | null;
  /** Widget panel rendering the log (List of commit rows). Owns
   * `selected_index` + `scroll_offset` as instance state — the
   * plugin's `state.selectedIndex` mirrors what the host reports
   * via `widget_event "select"`. */
  logPanel: WidgetPanel | null;
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
   * Debounce token for List `select` events. Rapid selection moves
   * (PageDown, held j/k) would otherwise trigger a full `git show`
   * spawn per intermediate row; we bump this id on every event
   * and only do the work after a short delay if no newer event
   * has arrived.
   */
  pendingSelectId: number;
}

const state: GitLogState = {
  isOpen: false,
  groupId: null,
  logBufferId: null,
  detailBufferId: null,
  toolbarBufferId: null,
  toolbarPanel: null,
  logPanel: null,
  commits: [],
  selectedIndex: 0,
  detailCache: null,
  pendingDetailId: 0,
  pendingSelectId: 0,
};

/**
 * Delay before spawning `git show` after a List `select` event. Long
 * enough to collapse a burst (held j/k or PageDown) into one fetch,
 * short enough that the detail panel still feels live.
 */
const SELECT_DEBOUNCE_MS = 60;

// =============================================================================
// Modes
//
// A buffer group has a single mode shared by all of its panels, so the
// handlers below branch on which panel currently has focus to do the
// right thing (`Return` jumps into the detail panel when pressed in
// the log, and opens the file at the cursor when pressed in the detail).
// =============================================================================

// j/k/Up/Down/PageUp/PageDown route to the log List widget so the host
// owns selection + scroll + auto-scroll. The List's `select` event then
// fires back into the plugin's `widget_event` handler for detail-pane
// refresh. Other plugin actions (q/r/y/Tab/Return) stay as direct
// bindings — they don't depend on which row is highlighted.
editor.defineMode(
  "git-log",
  [
    ["k", "git_log_select_up"],
    ["j", "git_log_select_down"],
    ["Up", "git_log_select_up"],
    ["Down", "git_log_select_down"],
    ["PageUp", "git_log_select_page_up"],
    ["PageDown", "git_log_select_page_down"],
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

function git_log_select_up(): void {
  if (isLogPanelActive()) {
    state.logPanel?.command(key("Up"));
  } else {
    editor.executeAction("move_up");
  }
}
function git_log_select_down(): void {
  if (isLogPanelActive()) {
    state.logPanel?.command(key("Down"));
  } else {
    editor.executeAction("move_down");
  }
}
function git_log_select_page_up(): void {
  if (isLogPanelActive()) {
    state.logPanel?.command(key("PageUp"));
  } else {
    editor.executeAction("page_up");
  }
}
function git_log_select_page_down(): void {
  if (isLogPanelActive()) {
    state.logPanel?.command(key("PageDown"));
  } else {
    editor.executeAction("page_down");
  }
}

/** True iff the log panel is the focused buffer in the group. The
 * group's bindings (j/k/Up/Down/PageUp/PageDown) apply to all panels
 * uniformly; we only want navigation to drive the List widget when
 * the user is *on* the log panel. From the detail panel, the same
 * keys must move the buffer cursor (so users can scroll the diff
 * before pressing Enter on a diff line to open the file view). */
function isLogPanelActive(): boolean {
  return (
    state.logBufferId !== null &&
    editor.getActiveBufferId() === state.logBufferId
  );
}
registerHandler("git_log_select_up", git_log_select_up);
registerHandler("git_log_select_down", git_log_select_down);
registerHandler("git_log_select_page_up", git_log_select_page_up);
registerHandler("git_log_select_page_down", git_log_select_page_down);

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
//
// The toolbar is a one-row panel mounted above the log/detail split. It's
// rendered through the widget runtime — a `Row` of `Button` widgets — so
// the host owns hit-testing, focus styling, and keystroke dispatch, and the
// plugin only handles the resulting `widget_event` actions.
//
// Each button's `key` is a stable identifier (`toolbar.tab`, `toolbar.q`,
// etc.) that `widget_event` carries back so the plugin can look up the
// right handler without per-row column arithmetic. The previous custom
// hit-region tracking (`state.toolbarButtons`, `on_git_log_toolbar_click`)
// is gone.

interface ToolbarItem {
  key: string;
  label: string;
  onClick: () => void | Promise<void>;
}

const TOOLBAR_KEY_PREFIX = "toolbar.";

function toolbarItems(): ToolbarItem[] {
  return [
    { key: "tab", label: "Tab switch pane", onClick: git_log_tab },
    { key: "ret", label: "RET open file", onClick: git_log_enter },
    { key: "y", label: "y copy hash", onClick: git_log_copy_hash },
    { key: "r", label: "r refresh", onClick: git_log_refresh },
    { key: "q", label: "q quit", onClick: git_log_q },
  ];
}

function toolbarSpec(): WidgetSpec {
  const items = toolbarItems();
  // `flexSpacer` at the end pushes the buttons to the left and lets the
  // toolbar background extend across the row.
  return row(
    ...items.map((item) =>
      button(item.label, { key: TOOLBAR_KEY_PREFIX + item.key }),
    ),
    flexSpacer(),
  );
}

function renderToolbar(): void {
  if (state.toolbarPanel === null) return;
  state.toolbarPanel.set(toolbarSpec());
}

editor.on("widget_event", (data) => {
  // Toolbar (Row of Buttons) — `activate` from keypress or click on a
  // button.
  if (
    state.toolbarPanel !== null &&
    data.panel_id === state.toolbarPanel.id()
  ) {
    if (data.event_type !== "activate") return;
    const items = toolbarItems();
    for (const item of items) {
      if (data.widget_key === TOOLBAR_KEY_PREFIX + item.key) {
        void item.onClick();
        return;
      }
    }
    return;
  }
  // Log pane (List of commit rows) — `select` fires on j/k/Up/Down/
  // PageUp/PageDown navigation and on row clicks; `activate` fires on
  // Enter or double-click.
  if (state.logPanel !== null && data.panel_id === state.logPanel.id()) {
    if (data.event_type === "select") {
      const idx =
        typeof data.payload?.index === "number" ? data.payload.index : -1;
      if (idx >= 0) void on_log_select(idx);
      return;
    }
    if (data.event_type === "activate") {
      void git_log_enter();
      return;
    }
    return;
  }
});

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
  if (state.logPanel === null) return;
  // List takes the per-row entries directly. selectedIndex: -1 on the
  // entry builder suppresses the plugin's selection styling — the host
  // renders the focused-row highlight from the List widget's instance
  // state instead.
  const items = buildCommitLogEntries(state.commits, {
    selectedIndex: -1,
    header: null,
  });
  const itemKeys = state.commits.map((c) => c.hash);
  state.logPanel.set(
    list({
      items,
      itemKeys,
      selectedIndex: state.selectedIndex,
      // Visible-rows only matters for virtualization; setting it to
      // commits.length renders all rows and lets the buffer's natural
      // scroll handle viewport. Revisit if commit lists grow into the
      // tens of thousands.
      visibleRows: Math.max(1, state.commits.length),
      key: "git-log-list",
    }),
  );
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
 * Synchronous detail refresh: render from cache if we have it, otherwise
 * a "loading…" placeholder. Never spawns git. Called immediately on every
 * selection change so the user sees instant feedback even while the real
 * `git show` is debounced.
 *
 * Returns the commit that needs fetching (cache miss) or null (cache hit
 * or no commit selected) so the caller can decide whether to spawn.
 */
function refreshDetailImmediate(): GitCommit | null {
  if (state.groupId === null) return null;
  if (state.commits.length === 0) {
    renderDetailPlaceholder(editor.t("status.no_commits"));
    return null;
  }
  const idx = Math.max(0, Math.min(state.selectedIndex, state.commits.length - 1));
  const commit = state.commits[idx];
  if (!commit) return null;

  if (state.detailCache && state.detailCache.hash === commit.hash) {
    renderDetailForCommit(commit, state.detailCache.output);
    return null;
  }

  renderDetailPlaceholder(
    editor.t("status.loading_commit", { hash: commit.shortHash })
  );
  return commit;
}

/**
 * Spawn `git show` for `commit` and render the result. Tagged with
 * `pendingDetailId` so a newer selection supersedes in-flight fetches.
 */
async function fetchAndRenderDetail(commit: GitCommit): Promise<void> {
  const myId = ++state.pendingDetailId;
  const output = await fetchCommitShow(editor, commit.hash);
  if (myId !== state.pendingDetailId) return;
  if (state.groupId === null) return;
  state.detailCache = { hash: commit.hash, output };
  // Only render if the current selection is still this commit — a rapid
  // Up/Down burst might have moved on before we got here.
  const currentIdx = Math.max(
    0,
    Math.min(state.selectedIndex, state.commits.length - 1)
  );
  if (state.commits[currentIdx]?.hash !== commit.hash) return;
  renderDetailForCommit(commit, output);
}

/**
 * Combined synchronous + asynchronous refresh used by open/refresh paths
 * where there's no burst of events to collapse.
 */
async function refreshDetail(): Promise<void> {
  const pending = refreshDetailImmediate();
  if (pending) await fetchAndRenderDetail(pending);
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

// =============================================================================
// Commands
// =============================================================================

async function show_git_log(): Promise<void> {
  if (state.isOpen) {
    // Already open — pull the existing tab to the front instead of
    // bailing out with a status message.
    if (state.groupId !== null) {
      editor.focusBufferGroupPanel(state.groupId, "log");
    }
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
  if (state.toolbarBufferId !== null) {
    state.toolbarPanel = new WidgetPanel(state.toolbarBufferId);
  }
  if (state.logBufferId !== null) {
    state.logPanel = new WidgetPanel(state.logBufferId);
  }
  state.selectedIndex = 0;
  state.detailCache = null;
  state.isOpen = true;

  // The detail panel still owns a native cursor so diff lines can be
  // clicked / traversed before pressing Enter to open a file. The log
  // panel's selection is owned by the List widget — no buffer cursor
  // needed (the focused-row highlight indicates position).
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
  // List widget's instance state is the source of truth for selection;
  // no buffer-cursor positioning needed (the renderer auto-scrolls so
  // the selected row stays visible).
  await refreshDetail();

  if (state.groupId !== null) {
    editor.focusBufferGroupPanel(state.groupId, "log");
  }
  editor.on("resize", on_git_log_resize);
  editor.on("buffer_closed", on_git_log_buffer_closed);

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
  editor.off("resize", on_git_log_resize);
  editor.off("buffer_closed", on_git_log_buffer_closed);
  // The buffer-group's `close` will tear down the panel buffers too,
  // which implicitly drops the widget panels rendering into them. We
  // still null out the handles so any stray `renderToolbar()` /
  // `renderLog()` call post-cleanup is a no-op.
  state.toolbarPanel = null;
  state.logPanel = null;
  state.isOpen = false;
  state.groupId = null;
  state.logBufferId = null;
  state.detailBufferId = null;
  state.toolbarBufferId = null;
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

/** q/Escape: closes the entire log group from any panel. */
function git_log_q(): void {
  if (state.groupId === null) return;
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
//
// j/k alias Up/Down as in the main git-log mode, and we inherit Normal
// bindings so arrows, PageUp/Down, Home/End, Ctrl+C copy, etc. still work
// in this read-only buffer — without `inheritNormalBindings`, unbound keys
// in a read-only mode fall through to the edit actions and trip the
// `editing_disabled` status message (see #566).
editor.defineMode(
  "git-log-file-view",
  [
    ["k", "move_up"],
    ["j", "move_down"],
    ["q", "git_log_file_view_close"],
    ["Escape", "git_log_file_view_close"],
  ],
  true, // read-only
  false, // allow_text_input
  true, // inherit Normal-context bindings for unbound keys
);

function git_log_file_view_close(): void {
  const id = editor.getActiveBufferId();
  if (id) editor.closeBuffer(id);
}
registerHandler("git_log_file_view_close", git_log_file_view_close);

// =============================================================================
// Selection tracking — live-update the detail panel as the user
// navigates the List. Driven by `widget_event "select"` from the host.
// =============================================================================

async function on_log_select(idx: number): Promise<void> {
  if (!state.isOpen) return;
  if (idx === state.selectedIndex) return;
  state.selectedIndex = idx;

  // Immediate feedback: cached detail or "loading" placeholder.
  // The host already re-rendered the List with the new selection
  // highlight, so we only need to update the right-hand pane.
  const pending = refreshDetailImmediate();

  const commit = state.commits[state.selectedIndex];
  if (commit) {
    editor.setStatus(
      editor.t("status.commit_position", {
        current: String(state.selectedIndex + 1),
        total: String(state.commits.length),
      }),
    );
  }

  if (!pending) return;

  // Debounce: bump the token, wait a beat, bail if a newer event has
  // arrived. `git show` is expensive; a burst of select events (held
  // j/k, PageDown) must collapse to one spawn.
  const myId = ++state.pendingSelectId;
  await editor.delay(SELECT_DEBOUNCE_MS);
  if (myId !== state.pendingSelectId) return;
  if (!state.isOpen) return;
  await fetchAndRenderDetail(pending);
}

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
