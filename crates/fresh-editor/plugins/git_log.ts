/// <reference path="./lib/fresh.d.ts" />

import {
  type GitCommit,
  buildCommitLogEntries,
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
  /**
   * The buffer-group's initial detail panel buffer (a virtual buffer
   * created by `createBufferGroup`). After the first commit is shown,
   * the panel is retargeted at a file-backed streaming buffer via
   * `setBufferGroupPanelBuffer`; this id is kept so we can close the
   * orphaned virtual buffer on group teardown.
   */
  initialDetailBufferId: number | null;
  /**
   * The buffer id currently displayed in the detail panel (one
   * file-backed buffer per visited commit). Tracked for focus checks
   * and cursor placement.
   */
  detailBufferId: number | null;
  toolbarBufferId: number | null;
  /** Widget panel rendering the toolbar (Row of Buttons). */
  toolbarPanel: WidgetPanel | null;
  /** Widget panel rendering the log (List of commit rows). */
  logPanel: WidgetPanel | null;
  commits: GitCommit[];
  selectedIndex: number;
  /**
   * Per-commit cache: sha → file-backed buffer id. Each visited
   * commit gets its own buffer pointing at `<dataDir>/git-show/<sha>.diff`,
   * which a background `git show --patch` writes into. Returning to a
   * cached commit is just a `setBufferGroupPanelBuffer` call — no
   * git invocation, scroll position preserved.
   */
  commitBuffers: Map<string, number>;
  /** sha → in-flight spawnProcess handle, for kill-on-supersession. */
  inFlightSpawns: Map<string, ProcessHandle<SpawnResult>>;
  /**
   * Debounce token for List `select` events. Rapid selection moves
   * (PageDown, held j/k) shouldn't churn through buffer swaps + spawns;
   * we bump this id on every event and only do the work after a short
   * delay if no newer event has arrived.
   */
  pendingSelectId: number;
}

const state: GitLogState = {
  isOpen: false,
  groupId: null,
  logBufferId: null,
  initialDetailBufferId: null,
  detailBufferId: null,
  toolbarBufferId: null,
  toolbarPanel: null,
  logPanel: null,
  commits: [],
  selectedIndex: 0,
  commitBuffers: new Map(),
  inFlightSpawns: new Map(),
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
    editor.executeAction("move_page_up");
  }
}
function git_log_select_page_down(): void {
  if (isLogPanelActive()) {
    state.logPanel?.command(key("PageDown"));
  } else {
    editor.executeAction("move_page_down");
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

/** Stable widget key for the log List. The host keys selection +
 * scroll instance state off this; the plugin re-pins selection
 * through it after click/keyboard `select` events. */
const LOG_LIST_KEY = "git-log-list";

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
      key: LOG_LIST_KEY,
    }),
  );
}

// =============================================================================
// Streaming detail panel
//
// Per-commit cached file-backed buffers. On commit switch we either reuse
// the existing cached buffer (instant) or spawn `git show --patch` into a
// per-SHA file and open it via `openFileStreaming`, polling for growth
// while git runs in the background. The buffer-group panel is re-pointed
// at the chosen buffer via `setBufferGroupPanelBuffer` — the same single
// tab keeps the side-by-side log/detail UX.
// =============================================================================

/**
 * Path of the per-SHA cache file. Commits are immutable; once we've
 * written one, repeat visits are zero-git.
 */
function cachePathForHash(hash: string): string {
  // `<dataDir>/git-show/<sha>.diff` — the .diff extension lets the
  // syntax-highlight grammar kick in for free.
  return `${editor.getDataDir()}/git-show/${hash}.diff`;
}

/** Polling interval while git is still writing. ~5 fps is plenty. */
const STREAM_POLL_MS = 200;

/**
 * Start a `git show --patch` for `hash`, piping stdout straight into the
 * cache file. Returns the handle so a later commit switch can `.kill()`
 * the still-running spawn.
 *
 * Caller has already verified the cache file doesn't yet exist (or wants
 * to overwrite it).
 */
function spawnGitShow(hash: string, cwd: string): ProcessHandle<SpawnResult> {
  // `--stat --patch` matches what the previous plugin used. The stat
  // header gives users a per-file changed-lines summary at the top
  // of the diff and is also what `git show` produces by default, so
  // its presence is what most readers (and tests) expect.
  //
  // The generated d.ts shows `spawnProcess(cmd, args, cwd?, stdoutTo?)`
  // as flat positional args. The runtime JS wrapper also accepts an
  // `{stdoutTo}` options object in the 4th slot, but using the flat
  // form keeps the call type-checked without a cast.
  return editor.spawnProcess(
    "git",
    ["show", "--stat", "--patch", hash],
    cwd,
    cachePathForHash(hash),
  );
}

/**
 * Poll `editor.refreshBufferFromDisk` until the spawn handle resolves,
 * then do one final catch-up refresh. Returns immediately if the
 * commit is no longer in flight (e.g. user moved on, kill() fired).
 */
async function pollUntilSpawnDone(
  hash: string,
  bufferId: number,
  handle: ProcessHandle<SpawnResult>,
): Promise<void> {
  // Wrap the handle's settlement in a non-rejecting marker promise so
  // a fast subscription loop can `await` it (or race it against a
  // delay) without worrying about whether the spawn errored. The
  // ProcessHandle is a thenable, not a real Promise, so adapt via
  // Promise.resolve().
  let done = false;
  void Promise.resolve(handle).then(
    () => {
      done = true;
    },
    () => {
      done = true;
    },
  );

  while (!done) {
    await editor.delay(STREAM_POLL_MS);
    if (!state.isOpen) return; // group closed mid-stream
    if (state.inFlightSpawns.get(hash) !== handle) return; // superseded
    await editor.refreshBufferFromDisk(bufferId);
  }
  // Final catch-up so any bytes written between the last poll and
  // process exit are visible immediately.
  await editor.refreshBufferFromDisk(bufferId);
  // Done — clear the in-flight handle if it's still ours.
  if (state.inFlightSpawns.get(hash) === handle) {
    state.inFlightSpawns.delete(hash);
  }
  // Apply diff coloring once the buffer is complete. Doing this
  // pre-completion would either churn (re-walk on every refresh) or
  // double-overlay newly-extended lines; on completion we walk once.
  await applyDiffHighlights(bufferId);
}

// =============================================================================
// Diff syntax highlighting via per-line bg overlays
//
// Sublime-syntax's bundled `Diff` definition only scopes the `diff`
// keyword, so themes only colour that. Plugins are responsible for the
// rest — same approach `live_diff` uses for inline diff coloring in
// regular buffers.
//
// One overlay per line of added/removed content is fine for the
// "normal commit" workload but explodes on giant commits (the
// rewrite-bun commit is 1M lines = 1M overlays = back to the old
// 500k-overlay problem this rewire eliminated). Gate on buffer size;
// gracefully degrade to no highlighting for outliers.
// =============================================================================

const HIGHLIGHT_BG_ADDED = "editor.diff_add_bg";
const HIGHLIGHT_BG_REMOVED = "editor.diff_remove_bg";
const HIGHLIGHT_BG_HUNK = "editor.diff_modify_bg";
const HIGHLIGHT_NAMESPACE = "git-log-diff";
/** Skip overlay highlighting above this size. ~256 KB covers
 *  basically every hand-written commit comfortably; very large
 *  generated-file diffs (lockfiles, minified code) just stay
 *  uncoloured — the cost would be a few thousand-to-a-million
 *  overlays for content the user mostly skims. */
const HIGHLIGHT_MAX_BYTES = 256 * 1024;

async function applyDiffHighlights(bufferId: number): Promise<void> {
  const total = editor.getBufferLength(bufferId);
  if (total === 0 || total > HIGHLIGHT_MAX_BYTES) return;
  const text = await editor.getBufferText(bufferId, 0, total);
  if (!text) return;

  // Walk lines tracking byte offsets; coalesce consecutive same-kind
  // rows into single ranges so a 30-line added block costs one
  // overlay, not 30.
  let byte = 0;
  let runKind: "+" | "-" | "@" | null = null;
  let runStart = 0;
  let runEnd = 0;

  const flushRun = () => {
    if (runKind === null) return;
    const bg =
      runKind === "+"
        ? HIGHLIGHT_BG_ADDED
        : runKind === "-"
        ? HIGHLIGHT_BG_REMOVED
        : HIGHLIGHT_BG_HUNK;
    editor.addOverlay(bufferId, HIGHLIGHT_NAMESPACE, runStart, runEnd, {
      bg,
      extendToLineEnd: true,
    });
    runKind = null;
  };

  for (const line of text.split("\n")) {
    const lineLen = line.length;
    const ch = line.charAt(0);
    let kind: "+" | "-" | "@" | null = null;
    if (ch === "+" && !line.startsWith("+++")) kind = "+";
    else if (ch === "-" && !line.startsWith("---")) kind = "-";
    else if (line.startsWith("@@")) kind = "@";

    if (kind !== runKind) {
      flushRun();
      if (kind !== null) {
        runStart = byte;
        runKind = kind;
      }
    }
    if (kind !== null) {
      // Include the trailing newline in the range so the bg colour
      // fills the row even on empty lines that wrap-extend.
      runEnd = byte + lineLen + 1;
    }
    byte += lineLen + 1;
  }
  flushRun();
}

/**
 * Get (or create) the file-backed buffer that displays `commit`.
 * On first call for a hash: ensure cache file exists, kick off
 * `git show` if it doesn't, openFileStreaming, start the poll loop.
 * Returns the buffer id on success or null on failure.
 */
async function ensureCommitBuffer(commit: GitCommit, cwd: string): Promise<number | null> {
  const hash = commit.hash;
  const existing = state.commitBuffers.get(hash);
  if (existing !== undefined) return existing;

  const path = cachePathForHash(hash);
  const cacheHit = editor.fileExists(path);

  if (!cacheHit) {
    // Cache miss: spawn git, polling the file as it grows. The handle
    // is stashed so a fast-scrolling user can supersede us via kill().
    const handle = spawnGitShow(hash, cwd);
    state.inFlightSpawns.set(hash, handle);
    const bufferId = await editor.openFileStreaming(path);
    if (bufferId === null) {
      handle.kill?.();
      state.inFlightSpawns.delete(hash);
      return null;
    }
    state.commitBuffers.set(hash, bufferId);
    // Fire-and-forget polling task.
    void pollUntilSpawnDone(hash, bufferId, handle);
    return bufferId;
  }

  // Cache hit: just open the existing file. No git spawned.
  const bufferId = await editor.openFileStreaming(path);
  if (bufferId === null) return null;
  state.commitBuffers.set(hash, bufferId);
  return bufferId;
}

/**
 * Show `commit` in the detail panel. Cancels any superseded in-flight
 * spawn for *other* commits (the user has navigated past them) and
 * retargets the panel at the chosen commit's buffer.
 */
async function showCommitInDetail(commit: GitCommit, cwd: string): Promise<void> {
  // Cancel anything still streaming for a commit that isn't this one —
  // the user has moved on; no point keeping git running.
  for (const [hash, handle] of state.inFlightSpawns) {
    if (hash !== commit.hash) {
      handle.kill?.();
      state.inFlightSpawns.delete(hash);
    }
  }

  const bufferId = await ensureCommitBuffer(commit, cwd);
  if (bufferId === null) {
    editor.setStatus(
      editor.t("status.failed_open_file", { file: commit.shortHash }),
    );
    return;
  }
  if (state.groupId === null) return;
  await editor.setBufferGroupPanelBuffer(state.groupId, "detail", bufferId);
  state.detailBufferId = bufferId;
  // Each commit buffer needs the same per-buffer presentation as the
  // initial virtual one: visible cursor for diff-line navigation,
  // wrap on (long minified lines unreadable in the 40% panel).
  editor.setBufferShowCursors(bufferId, true);
  editor.setLineWrap(bufferId, null, true);
  // Land at the top of the diff every time we (re-)visit a commit.
  editor.setBufferCursor(bufferId, 0);
}

async function refreshDetail(): Promise<void> {
  if (state.groupId === null) return;
  if (state.commits.length === 0) return;
  const idx = Math.max(0, Math.min(state.selectedIndex, state.commits.length - 1));
  const commit = state.commits[idx];
  if (!commit) return;
  await showCommitInDetail(commit, editor.getCwd());
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
  state.initialDetailBufferId =
    (group.panels["detail"] as number | undefined) ?? null;
  // detailBufferId starts as the initial virtual buffer; it gets
  // retargeted to a file-backed buffer on first commit selection.
  state.detailBufferId = state.initialDetailBufferId;
  state.toolbarBufferId = (group.panels["toolbar"] as number | undefined) ?? null;
  if (state.toolbarBufferId !== null) {
    state.toolbarPanel = new WidgetPanel(state.toolbarBufferId);
  }
  if (state.logBufferId !== null) {
    state.logPanel = new WidgetPanel(state.logBufferId);
  }
  state.selectedIndex = 0;
  state.commitBuffers = new Map();
  state.inFlightSpawns = new Map();
  state.isOpen = true;

  // The detail panel owns a native cursor so diff lines can be
  // clicked / traversed before pressing Enter to open a file. We set
  // the cursor on each retargeted buffer as it gets swapped in, but
  // wrap-default needs setting too — long minified lines in lock-file
  // diffs are unreadable without wrap in the 40% panel.
  if (state.initialDetailBufferId !== null) {
    editor.setBufferShowCursors(state.initialDetailBufferId, true);
    editor.setLineWrap(state.initialDetailBufferId, null, true);
  }

  renderToolbar();
  renderLog();
  // List widget's instance state is the source of truth for selection.
  // The selected row is kept in view by `scrollBufferToLine` from
  // `on_log_select` as the user navigates; the initial selection (0) is
  // already at the top so no scroll is needed here.
  await refreshDetail();

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
  // Kill any still-running `git show` spawns — we no longer care.
  for (const [, handle] of state.inFlightSpawns) {
    handle.kill?.();
  }
  state.inFlightSpawns.clear();
  // Close each per-commit buffer we created. The buffer-group's own
  // `close` (called below in `git_log_close`) tears down the panel
  // buffers (toolbar/log/initialDetail) — but retargeted file-backed
  // buffers we allocated via openFileStreaming are *outside* the
  // group's panel_buffers map by the time we got here, so we must
  // close them explicitly to avoid leaks.
  for (const [, bufferId] of state.commitBuffers) {
    editor.closeBuffer(bufferId);
  }
  state.commitBuffers.clear();
  // The buffer-group's `close` will tear down its own panel buffers
  // (toolbar/log/initialDetail) too, which implicitly drops the widget
  // panels rendering into them. We still null out the handles so any
  // stray `renderToolbar()` / `renderLog()` call post-cleanup is a
  // no-op.
  state.toolbarPanel = null;
  state.logPanel = null;
  state.isOpen = false;
  state.groupId = null;
  state.logBufferId = null;
  state.initialDetailBufferId = null;
  state.detailBufferId = null;
  state.toolbarBufferId = null;
  state.commits = [];
  state.selectedIndex = 0;
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
  // Tear down the whole group only when the *group's* buffers close
  // (toolbar / log / the initial virtual detail). A retargeted
  // file-backed commit buffer closing is normal — drop it from our
  // cache but keep the group alive.
  if (
    data.buffer_id === state.logBufferId ||
    data.buffer_id === state.initialDetailBufferId ||
    data.buffer_id === state.toolbarBufferId
  ) {
    git_log_cleanup();
    return;
  }
  // Removed from cache so a revisit re-spawns / re-opens.
  for (const [hash, bufId] of state.commitBuffers) {
    if (bufId === data.buffer_id) {
      state.commitBuffers.delete(hash);
      state.inFlightSpawns.get(hash)?.kill?.();
      state.inFlightSpawns.delete(hash);
      break;
    }
  }
}
registerHandler("on_git_log_buffer_closed", on_git_log_buffer_closed);

async function git_log_refresh(): Promise<void> {
  if (!state.isOpen) return;
  editor.setStatus(editor.t("status.refreshing"));
  state.commits = await fetchGitLog(editor);
  // The on-disk cache files are keyed by SHA and commits are
  // immutable, so they remain valid — but our in-memory buffer ids
  // for commits no longer in the visible list are stale; clear them.
  for (const [, handle] of state.inFlightSpawns) handle.kill?.();
  state.inFlightSpawns.clear();
  for (const [, bufferId] of state.commitBuffers) editor.closeBuffer(bufferId);
  state.commitBuffers.clear();
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
// Folding by file and hunk
//
// Publishes structural fold ranges into the buffer's `folding_ranges`
// via `setFoldingRanges` — the same channel an LSP `foldingRange`
// response uses. Nothing is pre-collapsed; the user toggles a range
// with the standard fold keybinding (`za` etc.), which finds the
// matching range under the cursor.
//
// The diff structure gives us two natural fold levels:
//   * per-file:  each `diff --git a/X b/Y` section
//   * per-hunk:  each `@@ -A,B +C,D @@` block within a file
// We publish both; the toggle-fold key picks the innermost containing
// range at the cursor's line.
//
// Computed once after `pollUntilSpawnDone` settles — re-running on
// every refresh would churn the marker list for no benefit (the diff
// structure is monotonic-append until exit).
// =============================================================================

interface DiffFoldRange {
  startLine: number;
  endLine: number;
}

/** Walk the buffer text and return (file-level, hunk-level) fold
 *  ranges. Lines are 0-indexed, both endpoints inclusive — the LSP
 *  shape. The "fold header" is the line at `startLine`; everything up
 *  through `endLine` collapses under it. */
function computeDiffFoldRanges(text: string): {
  files: DiffFoldRange[];
  hunks: DiffFoldRange[];
} {
  const lines = text.split("\n");
  const files: DiffFoldRange[] = [];
  const hunks: DiffFoldRange[] = [];

  let fileStart: number | null = null;
  let hunkStart: number | null = null;

  const closeHunk = (endLine: number) => {
    if (hunkStart !== null && endLine > hunkStart) {
      hunks.push({ startLine: hunkStart, endLine });
    }
    hunkStart = null;
  };
  const closeFile = (endLine: number) => {
    closeHunk(endLine);
    if (fileStart !== null && endLine > fileStart) {
      files.push({ startLine: fileStart, endLine });
    }
    fileStart = null;
  };

  for (let i = 0; i < lines.length; i++) {
    const l = lines[i];
    if (l.startsWith("diff --git ")) {
      closeFile(i - 1);
      fileStart = i;
    } else if (l.startsWith("@@ ")) {
      closeHunk(i - 1);
      hunkStart = i;
    }
  }
  closeFile(lines.length - 1);
  return { files, hunks };
}

async function publishDiffFoldRanges(bufferId: number): Promise<void> {
  const total = editor.getBufferLength(bufferId);
  if (total === 0) return;
  const text = await editor.getBufferText(bufferId, 0, total);
  if (!text) return;

  const { files, hunks } = computeDiffFoldRanges(text);

  // Merge — the host accepts a single array. "region" kind tags both
  // levels generically; the LSP spec also defines comment/imports
  // kinds which don't apply to diffs.
  const ranges = [...files, ...hunks].map((r) => ({
    startLine: r.startLine,
    endLine: r.endLine,
    kind: "region",
  }));
  editor.setFoldingRanges(bufferId, ranges);
}

// =============================================================================
// Detail panel — open file at commit
// =============================================================================

/**
 * Walk through the streaming diff buffer to find the file + line
 * context near the cursor. Diff format:
 *
 *     diff --git a/<path> b/<path>
 *     index ...
 *     --- a/<path>     (or /dev/null for additions)
 *     +++ b/<path>     (or /dev/null for deletions)
 *     @@ -old,n +new,m @@
 *     <context|+|- lines>
 *
 * Strategy:
 *  - Read up to the END of the cursor's line, not just up to the
 *    cursor's byte offset. This way a cursor sitting on a header line
 *    (`diff --git`, `+++ b/...`, `@@ ...`) still gets that line
 *    matched, matching the old text-property behaviour.
 *  - Walk backwards for the per-file header. Match either:
 *      `+++ b/<path>`             (preferred — names the new-side path)
 *      `diff --git a/<src> b/<dst>` (fallback — covers the case where
 *        the cursor is on the `diff --git` line itself, before the
 *        `+++` line has appeared in the search range)
 *  - Walk backwards for the most recent `@@ -... +<new>,<count> @@`
 *    between the header and cursor, then count context/'+' rows
 *    forward to the cursor to derive the new-side line number.
 */
async function deriveFileAndLineFromDiffCursor(
  bufferId: number,
): Promise<{ file: string; line: number } | null> {
  const cursor = editor.getCursorPosition();
  if (cursor < 0) return null;

  const bufLen = editor.getBufferLength(bufferId);
  const readEnd = Math.min(bufLen, cursor + 4096);
  if (readEnd === 0) return null;
  const text = await editor.getBufferText(bufferId, 0, readEnd);
  if (!text) return null;
  const lines = text.split("\n");

  // Locate the cursor's line index by walking byte offsets. `lines[i]`
  // covers bytes [byte, byte+len]; the `\n` separator lives at
  // byte+len, so the next line starts at byte+len+1.
  let byte = 0;
  let cursorLineIdx = lines.length - 1;
  for (let i = 0; i < lines.length; i++) {
    const lineLen = lines[i].length;
    if (cursor <= byte + lineLen) {
      cursorLineIdx = i;
      break;
    }
    byte += lineLen + 1;
  }

  // Walk back from the cursor's line for the per-file header. Match
  // either `+++ b/<path>` or `diff --git a/<src> b/<dst>` so cursor-
  // on-header cases work.
  let file: string | null = null;
  let headerIdx = -1;
  for (let i = cursorLineIdx; i >= 0; i--) {
    const l = lines[i];
    if (l.startsWith("+++ b/")) {
      file = l.slice(6).trim();
      headerIdx = i;
      break;
    }
    if (l.startsWith("+++ /dev/null")) {
      // Deletion — no new-side path. Opening the pre-image is a
      // separate flow.
      return null;
    }
    const m = /^diff --git a\/(.+?) b\/(.+)$/.exec(l);
    if (m) {
      const aSide = m[1];
      const bSide = m[2];
      file = bSide === "/dev/null" ? aSide : bSide;
      headerIdx = i;
      break;
    }
  }
  if (file === null || headerIdx < 0) return null;

  // Find the most recent `@@ ... +start,count @@` between header and
  // cursor. Default: line 1 (cursor sits on the header itself, or
  // between the header and the first hunk).
  let line = 1;
  for (let i = cursorLineIdx; i > headerIdx; i--) {
    const l = lines[i];
    const m = /^@@ -\d+(?:,\d+)? \+(\d+)(?:,\d+)? @@/.exec(l);
    if (!m) continue;
    const newStart = parseInt(m[1], 10);
    if (!Number.isFinite(newStart)) return null;
    // Walk forward from the hunk header to the cursor's line,
    // advancing the new-file line counter for context (' ') and
    // addition ('+') rows; skip deletion ('-') rows since they don't
    // exist in the new file.
    let cur = newStart;
    for (let j = i + 1; j <= cursorLineIdx; j++) {
      if (j === cursorLineIdx) {
        line = cur;
        break;
      }
      const ch = lines[j].charAt(0);
      if (ch === "+" || ch === " " || ch === "") cur += 1;
      // '-' / '\' (no-newline marker): don't advance.
    }
    break;
  }
  return { file, line };
}

async function git_log_detail_open_file(): Promise<void> {
  if (state.detailBufferId === null) return;
  const commit = selectedCommit();
  if (!commit) return;

  // The detail buffer is a plain file-backed view of `git show --patch`,
  // so we don't have plugin-attached `file`/`line` properties anymore.
  // Parse the diff backwards from the cursor to find the nearest
  // `+++ b/<path>` header (a per-file diff section opener) and the
  // most recent hunk header to derive a line number.
  const ctx = await deriveFileAndLineFromDiffCursor(state.detailBufferId);
  if (!ctx) {
    editor.setStatus(editor.t("status.move_to_diff_with_context"));
    return;
  }
  const { file, line } = ctx;

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

  // The List renders every commit row directly into the panel buffer
  // (visibleRows == commits.length), so the host's intra-widget
  // auto-scroll never fires — the buffer's own viewport has to follow
  // the selection. Scroll the row into view on every select, whether it
  // came from keyboard nav or a row click. (The host owns the selection
  // highlight and updates it for both nav and clicks; this only handles
  // the viewport.)
  if (state.logBufferId !== null) {
    editor.scrollBufferToLine(state.logBufferId, idx);
  }

  const commit = state.commits[state.selectedIndex];
  if (commit) {
    editor.setStatus(
      editor.t("status.commit_position", {
        current: String(state.selectedIndex + 1),
        total: String(state.commits.length),
      }),
    );
  }

  // Debounce: bump the token, wait a beat, bail if a newer event has
  // arrived. Even though re-pointing the panel at a cached buffer is
  // ~free, kicking off a new `git show --patch` for every intermediate
  // row in a held-j burst is wasteful. Collapse rapid selection moves.
  const myId = ++state.pendingSelectId;
  await editor.delay(SELECT_DEBOUNCE_MS);
  if (myId !== state.pendingSelectId) return;
  if (!state.isOpen) return;
  const current = state.commits[state.selectedIndex];
  if (!current) return;
  await showCommitInDetail(current, editor.getCwd());
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
