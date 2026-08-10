/// <reference path="./lib/fresh.d.ts" />

import {
  type GitCommit,
  buildCommitLogEntries,
  fetchGitLog,
} from "./lib/git_history.ts";
import {
  type GitRepo,
  git,
  resolveGitRepo,
  resolveGitRepoForPath,
} from "./lib/git_repo.ts";
import { button, flexSpacer, list, row, WidgetPanel } from "./lib/index.ts";

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
  /**
   * The repository this log view is bound to, resolved once when the view
   * opens (from the scoped file in current-file mode, else the active
   * buffer). Every git invocation for this view — the log fetch, each
   * `git show`, and opening a file at a commit — runs in `repo.root`, so
   * the view works in a monorepo sub-project whose workspace root isn't a
   * repo. `null` only before the first open.
   */
  repo: GitRepo | null;
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
   * When set, the log is scoped to a single file's history
   * (`git log -- <pathFilter>`). `null` means the full repository log.
   * Drives both the initial fetch and `git_log_refresh`.
   */
  pathFilter: string | null;
  /**
   * Per-commit cache: sha → file-backed buffer id. Each visited
   * commit gets its own buffer pointing at `<dataDir>/git-show/<sha>.diff`,
   * which a background `git show --patch` writes into. Returning to a
   * cached commit is just a `setBufferGroupPanelBuffer` call — no
   * git invocation, scroll position preserved.
   */
  commitBuffers: Map<string, number>;
  /**
   * sha → in-flight `git show` handle. Tracked so the view can kill any
   * still-running spawns when it closes (`git_log_cleanup`), and so a
   * revisit while a commit is still streaming reuses the buffer instead
   * of starting a second spawn for the same sha.
   */
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
  repo: null,
  groupId: null,
  logBufferId: null,
  initialDetailBufferId: null,
  detailBufferId: null,
  toolbarBufferId: null,
  toolbarPanel: null,
  logPanel: null,
  commits: [],
  selectedIndex: 0,
  pathFilter: null,
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

// The log pane is cursor-driven: j/k/Up/Down/PageUp/PageDown move the
// pane's real buffer cursor (normal editor movement), which scrolls via
// the standard `ensure_cursor_visible` wheel — only when the cursor
// crosses the top/bottom edge. The cursor is the source of truth for
// which commit is selected; a `cursor_moved` subscription mirrors its
// line into the List highlight + detail pane. On the detail pane the
// same keys scroll the diff. Other actions (q/r/y/Tab/Return) are direct
// bindings — they don't depend on the cursor row.
editor.defineMode(
  "git-log",
  [
    ["k", "move_up"],
    ["j", "move_down"],
    ["Up", "move_up"],
    ["Down", "move_down"],
    ["PageUp", "move_page_up"],
    ["PageDown", "move_page_down"],
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
  // Log pane (List of commit rows). Selection is cursor-driven (see the
  // `cursor_moved` handler), so the List's `select` event is ignored —
  // a row click places the buffer cursor, and `cursor_moved` mirrors it
  // into the selection. `activate` (Enter / double-click) still opens.
  if (state.logPanel !== null && data.panel_id === state.logPanel.id()) {
    if (data.event_type === "activate") {
      void git_log_enter();
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

/**
 * Path of the per-SHA completion marker. Written only after `git show`
 * exits 0, so its existence is a durable "this diff is fully written"
 * signal that survives the editor exiting (or git being killed) mid-
 * stream. Commits are immutable, so a complete diff stays valid forever
 * — the marker is never removed.
 */
function donePathForHash(hash: string): string {
  return `${cachePathForHash(hash)}.done`;
}

/**
 * Is `hash`'s cached diff known-complete? Both the diff and its
 * completion marker must exist. A diff present *without* the marker was
 * interrupted (external kill, editor exit, crash) and must be
 * regenerated rather than displayed partially.
 */
function isCommitDiffComplete(hash: string): boolean {
  return (
    editor.fileExists(editor.localPath(cachePathForHash(hash))) &&
    editor.fileExists(editor.localPath(donePathForHash(hash)))
  );
}

/** Polling interval while git is still writing. ~5 fps is plenty. */
const STREAM_POLL_MS = 200;

/**
 * Start a `git show --patch` for `hash`, piping stdout straight into the
 * cache file (the host opens it with `File::create`, which truncates any
 * existing partial content — so this safely regenerates an interrupted
 * diff). Returns the handle; it's stashed in `inFlightSpawns` so the
 * view can `.kill()` it on close.
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
 * doing one final catch-up refresh on exit. On a clean exit (code 0)
 * write the durable completion marker so future visits — this session
 * or after a restart — can trust the cached diff. We do NOT kill spawns
 * on navigation, so every spawn runs to completion and the buffer
 * always ends up fully populated; the only early-out is the view
 * closing mid-stream, which leaves no marker (the partial diff is
 * regenerated next time).
 */
async function pollUntilSpawnDone(
  hash: string,
  bufferId: number,
  handle: ProcessHandle<SpawnResult>,
): Promise<void> {
  // Wrap the handle's settlement so the poll loop can observe both that
  // it finished and whether it exited cleanly. The ProcessHandle is a
  // thenable, not a real Promise, so adapt via Promise.resolve(); a
  // rejection (spawn error / killed) leaves `cleanExit` false.
  let done = false;
  let cleanExit = false;
  void Promise.resolve(handle).then(
    (r) => {
      cleanExit = r.exit_code === 0;
      done = true;
    },
    () => {
      done = true;
    },
  );

  while (!done) {
    await editor.delay(STREAM_POLL_MS);
    if (!state.isOpen) return; // group closed mid-stream — no marker
    await editor.refreshBufferFromDisk(bufferId);
  }
  // Final catch-up so any bytes written between the last poll and
  // process exit are visible immediately.
  await editor.refreshBufferFromDisk(bufferId);
  // Done — clear the in-flight handle if it's still ours.
  if (state.inFlightSpawns.get(hash) === handle) {
    state.inFlightSpawns.delete(hash);
  }
  // Only a clean exit means the diff is fully written. A non-zero exit
  // or rejection (killed, spawn error) leaves the diff partial and
  // unmarked, so `ensureCommitBuffer` regenerates it on the next visit.
  if (!cleanExit) return;
  // Durable completion marker: an empty sidecar file whose existence
  // says "complete". Written after the bytes are on disk so a reader
  // that sees the marker also sees the full diff.
  editor.writeFile(editor.localPath(donePathForHash(hash)), "");
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
 * Get (or create) the file-backed buffer that displays `commit`'s diff.
 * Reuses a cached buffer when it's complete or still streaming; opens a
 * complete cache file zero-git; otherwise streams `git show` into the
 * cache file and polls it. A diff is "complete" only if its durable
 * completion marker exists (see `isCommitDiffComplete`), so an interrupted
 * diff (editor exit / external kill) is regenerated rather than shown
 * partially. Returns the buffer id on success or null on failure.
 */
async function ensureCommitBuffer(commit: GitCommit, cwd: string): Promise<number | null> {
  const hash = commit.hash;
  const path = cachePathForHash(hash);
  const existing = state.commitBuffers.get(hash);

  if (existing !== undefined) {
    // We already have a buffer for this commit this session. Reuse it
    // if it's still streaming (the in-flight poll will finish it) or its
    // diff is marked complete. Otherwise the stream ended without a
    // clean exit (e.g. git was killed externally) and left the buffer
    // partial — regenerate into the same buffer id so the panel retarget
    // and scroll handling are unaffected.
    if (state.inFlightSpawns.has(hash) || isCommitDiffComplete(hash)) {
      return existing;
    }
    const handle = spawnGitShow(hash, cwd);
    state.inFlightSpawns.set(hash, handle);
    void pollUntilSpawnDone(hash, existing, handle);
    return existing;
  }

  // First time this session for `hash`. A cache hit is only trustworthy
  // when the completion marker is present; a diff left without one (the
  // editor exited or git was killed mid-stream in a previous run) is
  // regenerated. `openFileStreaming` opens the file either way — for the
  // regenerate path the spawn truncates and rewrites it underneath.
  if (!isCommitDiffComplete(hash)) {
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

  // Cache hit with a valid completion marker: just open it. No git.
  const bufferId = await editor.openFileStreaming(path);
  if (bufferId === null) return null;
  state.commitBuffers.set(hash, bufferId);
  return bufferId;
}

/**
 * Show `commit` in the detail panel: ensure its diff buffer exists
 * (streaming it in if needed) and retarget the panel at it.
 *
 * We deliberately do NOT kill in-flight `git show` spawns for commits
 * the user navigated past. Killing was the source of a bug where a
 * superseded stream left its buffer empty forever; letting each spawn
 * finish guarantees every cached buffer ends up complete (and writes
 * its durable completion marker). Burst navigation is already collapsed
 * by the `SELECT_DEBOUNCE_MS` debounce, so in practice only commits the
 * user pauses on ever spawn git.
 */
async function showCommitInDetail(commit: GitCommit, cwd: string): Promise<void> {
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
  // The detail panel is a file-backed buffer (a `git show` dump), so it
  // escapes the virtual-buffer default that keeps guides out of the commit
  // list. Disable them explicitly: a diff isn't an editable indented document.
  editor.setIndentationGuide(bufferId, false);
  // Land at the top of the diff every time we (re-)visit a commit.
  editor.setBufferCursor(bufferId, 0);
}

async function refreshDetail(): Promise<void> {
  if (state.groupId === null) return;
  if (state.commits.length === 0) return;
  const idx = Math.max(0, Math.min(state.selectedIndex, state.commits.length - 1));
  const commit = state.commits[idx];
  if (!commit) return;
  await showCommitInDetail(commit, logCwd());
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

/**
 * Open the magit-style log view. `pathFilter` of `null` shows the full
 * repository history; a path scopes it to that file's commits. Shared by
 * the "Git Log" and "Git Log (Current File)" commands.
 */
/**
 * Root of the repository this log view is bound to. Falls back to the editor
 * cwd only defensively — by the time the view is open `state.repo` is always
 * set (a null resolve aborts the open).
 */
function logCwd(): string {
  return state.repo?.root ?? editor.getCwd();
}

async function openGitLog(pathFilter: string | null): Promise<void> {
  if (state.isOpen) {
    // Already open. If the requested scope differs (e.g. switching from
    // the full-repo log to a single file, or vice versa), the group's
    // tab title and contents need rebuilding — close it first so we
    // re-create with the right title. Otherwise just refocus.
    if (pathFilter !== state.pathFilter) {
      git_log_close();
    } else {
      if (state.groupId !== null) {
        editor.focusBufferGroupPanel(state.groupId, "log");
      }
      return;
    }
  }
  state.pathFilter = pathFilter;
  editor.setStatus(editor.t("status.loading"));

  // Resolve the repo once, up front. In current-file mode resolve from the
  // scoped file's own directory (so a monorepo sub-project resolves to its
  // own repo); otherwise from the active buffer. Every later git call for
  // this view runs in `state.repo.root`.
  state.repo = pathFilter !== null
    ? await resolveGitRepoForPath(editor, pathFilter)
    : await resolveGitRepo(editor);
  if (state.repo === null) {
    editor.setStatus(editor.t("status.no_commits"));
    return;
  }

  state.commits = await fetchGitLog(editor, {
    cwd: state.repo.root,
    pathFilter: pathFilter ?? undefined,
  });
  if (state.commits.length === 0) {
    editor.setStatus(editor.t("status.no_commits"));
    return;
  }

  // The tab title carries the file's basename when scoped so the user
  // can tell a file-history tab apart from the full-repo one.
  const title =
    pathFilter !== null
      ? `*Git Log: ${editor.pathBasename(pathFilter)}*`
      : "*Git Log*";

  // `createBufferGroup` is not currently included in the generated
  // `EditorAPI` type (it's a runtime-only binding, same as in audit_mode),
  // so we cast to `any` to keep the type checker happy.
  const group = await (editor as any).createBufferGroup(
    title,
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
  // Cursor-driven selection: give the log pane a real, visible cursor and
  // take ownership of it (`setBufferShowCursors` locks it so the widget
  // runtime won't clear it on repaint). The cursor's line is the selected
  // commit; `cursor_moved` mirrors it into the List highlight + detail.
  // Start on HEAD (line 0). Scrolling is the normal cursor-follow wheel.
  if (state.logBufferId !== null) {
    editor.setBufferShowCursors(state.logBufferId, true);
    editor.setBufferCursor(state.logBufferId, 0);
  }
  await refreshDetail();

  editor.on("resize", on_git_log_resize);
  editor.on("buffer_closed", on_git_log_buffer_closed);
  editor.on("cursor_moved", on_git_log_cursor_moved);

  editor.setStatus(
    editor.t("status.log_ready", { count: String(state.commits.length) })
  );
}

/** Command: show the full-repository log. */
async function show_git_log(): Promise<void> {
  await openGitLog(null);
}
registerHandler("show_git_log", show_git_log);

/**
 * Command: show the log scoped to the focused buffer's file. Available
 * whenever a buffer is focused (see the `git-log-buffer-focused` context
 * wired up at the bottom of the file). Falls back to a status message
 * when the active buffer has no on-disk path (e.g. an unsaved scratch
 * buffer).
 */
async function show_git_log_current_file(): Promise<void> {
  const bufferId = editor.getActiveBufferId();
  const filePath = bufferId ? editor.getBufferPath(bufferId) : "";
  if (!filePath || filePath === "") {
    editor.setStatus(editor.t("status.no_file"));
    return;
  }
  await openGitLog(filePath);
}
registerHandler("show_git_log_current_file", show_git_log_current_file);

/** Reset all state + unsubscribe. Idempotent; safe to call from either
 * path (user-initiated close or externally-closed group via the tab's
 * close button, which triggers `buffer_closed`). */
function git_log_cleanup(): void {
  if (!state.isOpen) return;
  editor.off("resize", on_git_log_resize);
  editor.off("buffer_closed", on_git_log_buffer_closed);
  editor.off("cursor_moved", on_git_log_cursor_moved);
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
  state.pathFilter = null;
  state.repo = null;
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
  state.commits = await fetchGitLog(editor, {
    cwd: logCwd(),
    pathFilter: state.pathFilter ?? undefined,
  });
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
  const result = state.repo
    ? await git(editor, state.repo, ["show", `${commit.hash}:${file}`])
    : await editor.spawnProcess("git", ["show", `${commit.hash}:${file}`]);
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
  //
  // Pass `initialCursorLine` (0-indexed) so the host lands the cursor on
  // the target line before the buffer becomes active. Without this, a
  // follow-up setBufferCursor would race against user input and could
  // be silently clobbered by any keypress the moment focus lands on the
  // new buffer.
  const name = `*${commit.shortHash}:${file}*`;
  const view = await editor.createVirtualBuffer({
    name,
    mode: "git-log-file-view",
    readOnly: true,
    editingDisabled: true,
    showLineNumbers: true,
    // This view shows real source (highlighted from the filename), so keep
    // indentation guides — virtual buffers default off, but a historical file
    // view is code the user is reading.
    indentationGuide: true,
    entries,
    initialCursorLine: Math.max(0, line - 1),
  });
  if (view) {
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
// Selection tracking — the log pane is cursor-driven. The buffer cursor's
// line (set by arrow-key movement or a click) is the selected commit; this
// `cursor_moved` subscription mirrors it into the List highlight and the
// detail pane. Scrolling is handled by the normal cursor-follow wheel, so
// the viewport only moves when the cursor crosses the top/bottom edge.
// =============================================================================

function on_git_log_cursor_moved(data: { buffer_id: number; line: number }): void {
  if (!state.isOpen || state.logBufferId === null) return;
  if (data.buffer_id !== state.logBufferId) return;
  // `cursor_moved.line` is 1-based; commit rows are 0-based (no header),
  // so the selected commit index is `line - 1`.
  const idx = data.line - 1;
  if (idx < 0 || idx >= state.commits.length) return;
  void selectCommitLine(idx);
}

async function selectCommitLine(idx: number): Promise<void> {
  if (!state.isOpen) return;
  if (idx === state.selectedIndex) return;
  state.selectedIndex = idx;

  // Move the List's highlight bar to the cursor's row. The cursor itself
  // is the real (plugin-owned) buffer cursor, so it stays exactly where
  // the user moved or clicked it — this only repaints the row styling,
  // and the repaint preserves the cursor position.
  state.logPanel?.setSelectedIndex(LOG_LIST_KEY, idx);

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
  await showCommitInDetail(current, logCwd());
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

// The "current file" command is gated on a plugin-defined context that is
// active whenever a buffer is focused, so it only surfaces in the command
// palette when there's a file to scope the log to. The context is kept in
// sync with focus changes via the buffer_activated / buffer_deactivated
// hooks below.
const BUFFER_FOCUSED_CTX = "git-log-buffer-focused";

function updateBufferFocusedContext(): void {
  editor.setContext(BUFFER_FOCUSED_CTX, editor.getActiveBufferId() !== 0);
}
editor.on("buffer_activated", updateBufferFocusedContext);
editor.on("buffer_deactivated", updateBufferFocusedContext);
// Seed the context from the buffer that's already focused at load time.
updateBufferFocusedContext();

editor.registerCommand(
  "%cmd.git_log_current_file",
  "%cmd.git_log_current_file_desc",
  "show_git_log_current_file",
  BUFFER_FOCUSED_CTX
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
