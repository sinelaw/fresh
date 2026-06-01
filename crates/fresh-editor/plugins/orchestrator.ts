/// <reference path="./lib/fresh.d.ts" />
//
// Orchestrator — multi-agent / multi-worktree session orchestration.
//
// MVP scope (`docs/internal/orchestrator-sessions-design.md`):
//
//   - "Orchestrator: Open" opens a floating overlay prompt listing
//     every session with its state column. Up/Down navigates,
//     Enter dives into the selected session.
//   - "Orchestrator: New Session" opens a single floating widget
//     form with three optional fields (session name, agent
//     command, branch), allocates a worktree-rooted session and
//     spawns the agent in a terminal attached to it.
//   - "Orchestrator: Kill Selected" closes the session whose row is
//     currently highlighted in the open prompt.
//   - Agent state column updates from terminal_output regex and
//     terminal_exit code: RUNNING / AWAITING / READY / ERRORED.

import {
  button,
  col,
  flexSpacer,
  FloatingWidgetPanel,
  hintBar,
  key as widgetKey,
  labeledSection,
  list,
  row,
  overlay,
  spacer,
  styledRow,
  text,
  textInputChar,
  toggle,
  windowEmbed,
  type WidgetSpec,
} from "./lib/widgets.ts";

const editor = getEditor();

// =============================================================================
// Types
// =============================================================================

// A session's coarse activity, inferred from its agent terminal:
//   "working" — the terminal emitted output within the last
//               IDLE_AFTER_MS (the agent is actively producing).
//   "idle"    — quiet: waiting for input, finished, exited, or just
//               sitting. Also the honest default before we've seen any
//               output, since we have no evidence of work yet.
// This is deliberately only two states: it's all the terminal-output
// signal can honestly support. We don't poll the process, so "working"
// means "printing", not "alive" — an agent that goes quiet to think
// reads as idle until it prints again.
type AgentState = "working" | "idle";

// One row in the completion popup. `kind: "history"` items
// render with a leading `↶` marker + italic styling so the user
// can tell at-a-glance that the row came from their submission
// history rather than from the live completion source. Sent to
// the host via `formPanel.setCompletions`; the host renders the
// marker + style.
type CompletionItem = { value: string; kind?: "history" };

interface AgentSession {
  // Editor's stable session id.
  id: number;
  // Display label (defaults to root basename — Orchestrator never
  // renames externally-created sessions).
  label: string;
  // Absolute filesystem root.
  root: string;
  // Canonical project root this session belongs to (set at
  // create time from the Project Path field). Equals `root`
  // for sessions without an explicit project — the host
  // normalises at the API boundary so plugins never have to
  // distinguish `null`/`undefined`/`""`.
  projectPath: string;
  // `true` if the session was created with the worktree
  // checkbox unchecked (shared worktree / non-git path).
  sharedWorktree: boolean;
  // The terminal id Orchestrator spawned in this session, if any.
  terminalId: number | null;
  // Coarse activity, recomputed from `lastOutputAt` at render time
  // (see `sessionState`). Not authoritative on its own — the timestamp
  // is. ("active" — the focused window — is computed separately from
  // `editor.activeWindow()`.)
  state: AgentState;
  // Wall-clock ms of the most recent terminal_output for this session,
  // or null if it has never produced output (or has no terminal). This
  // is the real signal; `state` is just `Date.now() - lastOutputAt`
  // bucketed against IDLE_AFTER_MS.
  lastOutputAt: number | null;
  // Wall-clock ms when orchestrator.new fired createWindow.
  createdAt: number;
  // `true` when this row is a worktree discovered on disk (via
  // `git worktree list`) that has no live editor window yet.
  // Discovered rows carry a synthetic negative `id`, no
  // `terminalId`, and dive by *attaching* a new session to
  // `root` rather than switching to an existing window. They are
  // dropped from `orchestratorSessions` the moment a real window
  // is opened at the same `root`.
  discovered?: boolean;
  // Branch checked out in this worktree (best-effort, for
  // display). Set for discovered rows; left undefined for live
  // sessions where the tab/label already carries the identity.
  branch?: string;
}

// =============================================================================
// Module state — editor-global, survives every dive.
// =============================================================================

const orchestratorSessions = new Map<number, AgentSession>();

// Stable synthetic ids for discovered (on-disk, not-yet-opened)
// worktrees, keyed by canonical path. Live windows own the
// positive id space (editor `WindowId`s); discovered rows take
// negative ids so the two never collide and the existing
// `orchestratorSessions.get(id)` call sites keep working. Ids
// stay stable across rescans so the dialog selection doesn't
// jump when the worktree set is refreshed. `-1` is reserved as a
// "no selection" sentinel elsewhere, so allocation starts at `-2`.
const discoveredIdByPath = new Map<string, number>();
let nextDiscoveredId = -2;
function discoveredIdFor(path: string): number {
  let id = discoveredIdByPath.get(path);
  if (id === undefined) {
    id = nextDiscoveredId--;
    discoveredIdByPath.set(path, id);
  }
  return id;
}

// New-session form state. `null` ⇒ the floating form isn't
// open. Each field's `value` + `cursor` mirrors what the host
// renders inside the panel's TextInput widgets; the `submitting`
// flag debounces double-Enter on the Create button; `lastError`
// is rendered as a styled error row inside the form when the
// most recent submit failed (status bar would get clobbered —
// see MEMORY.md).
interface NewSessionForm {
  // Project Path: the directory the session is rooted at. When
  // `createWorktree` is true (default for git paths) this is
  // the *base* repo for `git worktree add`. When false, this
  // is the session root itself (no git interaction).
  projectPath: { value: string; cursor: number };
  name: { value: string; cursor: number };
  cmd: { value: string; cursor: number };
  branch: { value: string; cursor: number };
  // Whether to create a new git worktree under
  // `<XDG>/orchestrator/<slug>/<session>/` (true) or run the
  // session directly inside `projectPath` (false). Enabled
  // only when the resolved `projectPath` is inside a git
  // working tree (`projectPathIsGit === true`). Forced to
  // false on non-git paths and the checkbox is disabled.
  createWorktree: boolean;
  submitting: boolean;
  lastError: string | null;
  // Resolved canonical project root from the editor's cwd —
  // surfaced as the Project Path placeholder. Empty while the
  // async probe runs at `openForm` time.
  defaultProjectPath: string;
  // `true`: resolved Project Path is inside a git working
  // tree (worktree checkbox enabled). `false`: non-git path
  // (checkbox disabled, branch field inert). `null`: probe
  // in flight (keep checkbox in its last-known state).
  projectPathIsGit: boolean | null;
  // `true` when the resolved Project Path is itself an existing
  // *linked* worktree (created by `git worktree add`). In that
  // case leaving "Create a new git worktree" unchecked attaches
  // the session to it as a managed worktree rather than treating
  // it as a shared root. The probe defaults the checkbox to
  // unchecked when it first detects this, and `buildFormSpec`
  // surfaces an explanatory hint. `null` while the probe runs.
  projectPathIsLinkedWorktree: boolean | null;
  // Concrete session name the auto-generator would produce
  // for the current Project Path (e.g. "session-3"). Surfaced
  // as the Session Name placeholder so the user sees the
  // exact name an empty submit would create. Empty while the
  // refs probe runs.
  defaultSessionName: string;
  // Resolved default branch (e.g. "origin/main"). Empty while
  // the async `git fetch + symbolic-ref` probe is in flight;
  // the branch input's placeholder reads this so the user sees
  // the exact base ref the worktree will fork off if they
  // leave the field blank.
  defaultBranch: string;
  // True when the default branch fell through to bare `HEAD`
  // because no `origin` is configured. Surfaced in the
  // placeholder as `HEAD  (no origin configured)` so the user
  // knows why.
  defaultBranchIsHeadFallback: boolean;
  // Previously-submitted Agent Command (persisted across editor
  // sessions via `orchestrator.last_cmd`). Rendered as the cmd
  // field's *placeholder*, and used as the actual command when
  // the user leaves the field blank — submitting "" with a
  // visible placeholder of "python3" was confusing because the
  // host ignored the hint and spawned a bare shell. Now the
  // placeholder is the command if the value is empty.
  lastCmd: string;
  // True when this form was opened from the picker (Alt+N or
  // the "+ New Session" button). On cancel (Esc / Cancel
  // button) we re-open the picker so the user lands back where
  // they were instead of being dropped into the bare editor.
  fromPicker: boolean;
  // Token incremented every time the user changes the Project
  // Path field. Async probes (is-git, session-name, default-
  // branch) capture the token at launch and bail on result if
  // a newer token has been issued — prevents stale probes from
  // overwriting fresh state on rapid typing.
  probeToken: number;
  // Per-field input-history cursor. -1 = "not in history"
  // (showing the user's current draft). 0 = most recent, 1 =
  // older, etc. (Now only consulted by the host-side `↶`
  // history rows mixed into the completion popup — Up/Down on a
  // history-bearing field reopens the popup, where historical
  // entries appear after live completion candidates.)
  historyCursor: { project_path: number; name: number; cmd: number; branch: number };
  // Saved draft text per field: when the user first presses Up
  // we squirrel away whatever was in `value` so Down can
  // restore it.
  historyDraft: { project_path: string; name: string; cmd: string; branch: string };
  // Inline-dropdown completion state. `field` names which input
  // the suggestion list belongs to; the list is only rendered
  // while that input is focused. `items` is the post-filter set
  // (already in display order); `selectedIndex` is the
  // highlighted row. `anchor` is the value the user had typed
  // when the candidates were last fetched — used to ignore
  // stale async results that land after the user keeps typing.
  // `token` mirrors the project-path probe pattern: every fresh
  // fetch bumps it; results bail if they're not the latest.
  completion: {
    field: "project_path" | "branch" | null;
    items: CompletionItem[];
    selectedIndex: number;
    anchor: string;
    token: number;
  };
}
let form: NewSessionForm | null = null;
let formPanel: FloatingWidgetPanel | null = null;

const NEW_SESSION_MODE = "orchestrator-new-form";

// Open dialog state. `null` ⇒ the picker isn't mounted. Lives
// alongside the new-session form state but is independent of
// it — the two dialogs share the orchestrator mode plumbing but
// not their data.
interface OpenDialogState {
  // Filter input value + cursor byte. Mirrors what the host
  // renders inside the panel's filter TextInput.
  filter: { value: string; cursor: number };
  // Subset of `orchestratorSessions` keys that pass the filter,
  // in display order. Recomputed on every filter change.
  filteredIds: number[];
  // The selection inside the list widget. The host owns the
  // authoritative copy as instance state; this mirror lets
  // `buildOpenSpec` render the matching preview pane without a
  // round-trip.
  selectedIndex: number;
  // Active session at the moment the dialog opened. Recorded
  // so a future "Esc restores active" affordance has the
  // anchor it needs.
  originalActiveSession: number;
  // When non-null, the preview pane swaps to a confirmation
  // panel for the named action against the listed session ids.
  // A single-element `ids` is the per-row Stop/Archive/Delete
  // path; a multi-element `ids` is a bulk action over the
  // checkbox selection. Cleared on Cancel or after the action
  // completes.
  pendingConfirm:
    | { action: "stop" | "archive" | "delete"; ids: number[] }
    | null;
  // Rows the user has checkbox-selected (Space, or click) for a
  // bulk Stop/Archive/Delete. Holds session ids — positive for
  // live windows, negative for discovered on-disk worktrees
  // (which bulk-delete via `git worktree remove`). Survives filter
  // and scope changes; pruned against the live set on every
  // refresh. Bulk mode (the dedicated selection bar) engages once
  // two or more rows are checked.
  selectedIds: Set<number>;
  // `true` shows the discovered on-disk worktree rows in the list.
  // The "Show all worktrees" checkbox below the scope control toggles
  // it (Alt+T / `orchestrator_toggle_worktrees`). Defaults to false
  // (worktrees hidden) — discovery is opt-in. Remembered across opens
  // via `lastShowWorktrees`.
  showWorktrees: boolean;
  // `true` hides "trivial" sessions — those with no terminal and at
  // most one open file/buffer (empty-unnamed-buffer and single-file
  // shells left behind by one-off editor launches). The "Show
  // empty/1-file sessions" checkbox (Alt+I / `orchestrator_toggle_trivial`)
  // flips it. Defaults to true; remembered across opens via
  // `lastHideTrivial`. The active session and discovered worktree rows
  // are never hidden by this filter regardless of the flag.
  hideTrivial: boolean;
  // Progress marker for an in-flight *bulk* action. While set, the
  // selection bar shows "Archiving 2/3…" and its buttons are
  // hidden so a second Enter can't re-fire mid-batch. Cleared when
  // the batch finishes.
  bulkInFlight:
    | { action: "stop" | "archive" | "delete"; total: number; done: number }
    | null;
  // Rows the embed reserves and rows the sessions list shows.
  // Captured once at dialog-open from the editor's viewport so
  // the layout stays constant across re-renders — recomputing
  // mid-dialog would let the size jitter when the active
  // window's viewport changes (e.g. terminal buffer's shorter
  // height vs. a file buffer's).
  listVisibleRows: number;
  embedRows: number;
  // Toggle between "compact preview" (default — buttons + live
  // embed only, no info row) and "details" (state + path metadata
  // row visible above the embed). Compact is the default because
  // the embed is the part the user actually wants to see; the
  // metadata row is rarely read and just eats embed height.
  showDetails: boolean;
  // The session id whose lifecycle action (archive / delete) is
  // currently running. While set:
  //   - that session's preview pane swaps to an "Archiving…" /
  //     "Deleting…" panel with no action buttons, so the user
  //     sees the operation is in flight rather than wondering
  //     why their click took no effect.
  //   - the user can still navigate to other sessions and act on
  //     them; only the in-flight session is disabled.
  // Cleared by the async handler on success or failure. The row
  // disappears from the list naturally once the editor's
  // `window_closed` hook fires `refreshOpenDialog`.
  inFlight: { action: "archive" | "delete"; sessionId: number } | null;
  // Last user-visible error from a refused lifecycle action
  // (e.g. "cannot archive the base session", "dive elsewhere
  // first…"). Rendered as a banner row above the filter so it's
  // hard to miss — the status bar at the bottom of the screen is
  // too easy to skip over when the user's eyes are on the dialog.
  // Cleared on the next nav / filter change.
  lastError: string | null;
  // Which sessions the list foregrounds:
  //   - "current": only sessions belonging to the active window's
  //     project (the default — launching in project B shouldn't
  //     bury you under project A's sessions). A trailing affordance
  //     row advertises how many sessions live in other projects.
  //   - "all": every session, across every project, each row
  //     labeled with its project so cross-project rows are obvious.
  // Toggled with the scope key (⌥P by default). The filter input
  // always searches globally regardless of scope, so typing a name
  // from another project still surfaces it.
  scope: "current" | "all";
}
let openDialog: OpenDialogState | null = null;
let openPanel: FloatingWidgetPanel | null = null;
// When the open panel is mounted as the persistent left dock rather
// than the centered modal picker. The dock reuses the same panel +
// `openDialog` state; these flags drive the dock-only behaviours
// (live-switch on nav, Enter/Esc blur instead of close).
let dockMode = false;
// True while the dock is visible but blurred — keyboard focus is in
// the editor and the dock just reflects the active session. The
// toggle command re-focuses; Enter/Esc blur.
let dockBlurred = false;
// Monotonic token so a rapid run of ↑/↓ only commits the *last*
// selection after the debounce window (30ms) — see `scheduleDockSwitch`.
let dockSwitchToken = 0;
// Default dock width on a "typical" terminal, and the bounds the
// responsive width is clamped to. The dock scales with the terminal
// (`dockDefaultWidth`) between these; a user drag still overrides it
// (the host persists the dragged width — see `handle_floating_panel_control`).
const DOCK_WIDTH_COLS = 32;
const DOCK_MIN_WIDTH_COLS = 24;
const DOCK_MAX_WIDTH_COLS = 40;
// Fraction of the terminal width the dock targets by default.
const DOCK_WIDTH_FRACTION = 0.28;

// Responsive default dock width: ~`DOCK_WIDTH_FRACTION` of the terminal,
// clamped to [`DOCK_MIN`..`DOCK_MAX`]. Re-evaluated on resize so the dock
// grows/shrinks with the window. Falls back to the fixed default when the
// screen size isn't known yet.
function dockDefaultWidth(): number {
  const w = editor.getScreenSize().width;
  if (w <= 0) return DOCK_WIDTH_COLS;
  const target = Math.round(w * DOCK_WIDTH_FRACTION);
  return Math.max(DOCK_MIN_WIDTH_COLS, Math.min(DOCK_MAX_WIDTH_COLS, target));
}

// Inner content width for a given dock width: the host reserves one
// column for the right border plus an editor-side gutter, so list rows
// get `dockWidth - 2` cells. Floored so a clamped/narrow dock still
// renders something. Drives name/tag truncation and the `dockRule`.
function dockContentCols(dockWidth: number): number {
  return Math.max(8, dockWidth - 2);
}
// Which dock zone has keyboard focus: the session list (default) or the
// filter input. Tracked from the host's `focus` widget_event. The host
// (dispatch_floating_widget_key) reads the panel focus directly to route
// Enter/Esc/Space//'; this mirror is informational for the plugin.
let dockFocus: "list" | "filter" = "list";
// Full focused-widget mirror for the open dialog (both dock and
// centered-picker modes). Updated from every `focus` widget_event.
// Used by `toggleSelectCurrent` so a Space keypress while focus is
// on a filter checkbox toggles *that* checkbox rather than the list
// — see the OPEN_MODE `["Space", "orchestrator_toggle_select"]`
// binding below for why the mode binding can't be made conditional
// upstream (it has to swallow Space unconditionally to keep it out
// of the filter text-input).
let pickerFocusKey: string = "sessions";
// Scope is remembered across opens of the picker (module state
// survives dialog close). Defaults to "all" so the picker opens
// showing every session; flipping it with the Project control / Alt+P
// updates this and the next open honours it.
let lastOpenScope: "current" | "all" = "all";
// Remembered across opens, like `lastOpenScope`: whether the
// discovered on-disk worktree rows are shown. Defaults to false
// (worktrees hidden) — surfacing them is opt-in via "Show all
// worktrees" (Alt+T).
let lastShowWorktrees = false;
// Remembered across opens: whether "trivial" sessions are hidden.
// Defaults to true — every editor launch on a throwaway directory or a
// single file leaves a workspace file behind, which restores as a shell
// window and clutters the list. Hiding them by default keeps the picker
// focused on real sessions; the "Show empty/1-file sessions" checkbox
// (Alt+I) reveals them.
let lastHideTrivial = true;

// Per-session content summary keyed by canonical session root, built
// from the on-disk workspace files. The restored shell windows don't
// carry their open-tab layout (it's lazily re-warmed on first dive), so
// the workspace file is the only place to learn how much a session
// holds. Rebuilt each time the picker opens. A session is "trivial"
// when it has no terminal and at most one real file/unnamed buffer —
// the empty-unnamed-buffer and single-file cases the filter targets.
interface SessionContent {
  files: number;
  hasTerminal: boolean;
  trivial: boolean;
}
const sessionContentByRoot = new Map<string, SessionContent>();

// Roots from the editor (`WindowInfo.root`) and from workspace files
// (`working_dir`) are both canonical absolute paths, but normalise a
// trailing slash so the two always key the same map entry.
function normRoot(p: string): string {
  return p.length > 1 && p.endsWith("/") ? p.slice(0, -1) : p;
}

// Scan `<dataDir>/workspaces/*.json` and summarise each session's open
// content. Mirrors the host's own `discover_sessions` (which keys on the
// file's `working_dir`), so a root matches regardless of how the
// filename was percent-encoded. Best-effort: unreadable / unparseable
// files are skipped, and a missing summary is treated as "not trivial"
// (shown) by the filter, so we never hide a session we couldn't classify.
function scanSessionContent(): void {
  sessionContentByRoot.clear();
  const dir = editor.pathJoin(editor.getDataDir(), "workspaces");
  let entries: DirEntry[];
  try {
    entries = editor.readDir(dir);
  } catch {
    return;
  }
  if (!entries) return;
  for (const e of entries) {
    if (!e.is_file || !e.name.endsWith(".json")) continue;
    const raw = editor.readFile(editor.pathJoin(dir, e.name));
    if (!raw) continue;
    let ws: Record<string, unknown>;
    try {
      ws = JSON.parse(raw);
    } catch {
      continue;
    }
    const wd = ws["working_dir"];
    if (typeof wd !== "string") continue;
    let files = 0;
    let hasTerminal = Array.isArray(ws["terminals"]) &&
      (ws["terminals"] as unknown[]).length > 0;
    const splits = ws["split_states"];
    if (splits && typeof splits === "object") {
      for (const sv of Object.values(splits as Record<string, unknown>)) {
        const tabs = (sv as Record<string, unknown> | null)?.["open_tabs"];
        if (!Array.isArray(tabs)) continue;
        for (const t of tabs) {
          if (t && typeof t === "object") {
            if ("File" in t || "Unnamed" in t) files++;
            else if ("Terminal" in t) hasTerminal = true;
          }
        }
      }
    }
    sessionContentByRoot.set(normRoot(wd), {
      files,
      hasTerminal,
      trivial: !hasTerminal && files <= 1,
    });
  }
}

const OPEN_MODE = "orchestrator-open";

// =============================================================================
// Session-list reconciliation
// =============================================================================

function reconcileSessions(): void {
  const editorSessions = editor.listWindows();
  const seen = new Set<number>();
  for (const s of editorSessions) {
    seen.add(s.id);
    const existing = orchestratorSessions.get(s.id);
    if (!existing) {
      orchestratorSessions.set(s.id, {
        id: s.id,
        label: s.label,
        root: s.root,
        projectPath: s.project_path,
        sharedWorktree: s.shared_worktree ?? false,
        terminalId: null,
        // Idle until the terminal actually prints something — we have
        // no evidence of work yet. `lastOutputAt` is the real signal;
        // `state` is recomputed from it at render time.
        state: "idle",
        lastOutputAt: null,
        createdAt: Date.now(),
      });
    } else {
      existing.label = s.label;
      existing.root = s.root;
      existing.projectPath = s.project_path;
      if (s.shared_worktree != null) existing.sharedWorktree = s.shared_worktree;
    }
  }
  // Live windows live in the positive id space; their absence from
  // `listWindows()` means they were closed, so drop them. Discovered
  // worktrees (negative ids) are NOT backed by a window and must
  // survive this sweep — they're pruned separately, against the
  // on-disk worktree set, by `refreshDiscoveredWorktrees`.
  for (const id of orchestratorSessions.keys()) {
    if (id > 0 && !seen.has(id)) orchestratorSessions.delete(id);
  }
  // A worktree that's now open as a live window must not also linger
  // as a discovered row. Drop any discovered entry whose root a live
  // session already occupies.
  const liveRoots = new Set<string>();
  for (const s of orchestratorSessions.values()) {
    if (!s.discovered) liveRoots.add(s.root);
  }
  for (const [id, s] of orchestratorSessions) {
    if (s.discovered && liveRoots.has(s.root)) orchestratorSessions.delete(id);
  }
}

// =============================================================================
// Discovered-worktree scan
//
// Surfaces worktrees that exist on disk but have no live editor
// window, so the user doesn't have to add them by hand. Because
// open sessions can span several repos, `git worktree list` must
// run once *per project*: the scan set is the distinct canonical
// repo roots of every live session, plus the editor's cwd repo.
// Each linked worktree not already open (and not an
// orchestrator-internal tree) becomes a discovered row that dives
// by attaching a fresh session to it.
// =============================================================================

let discoveryInFlight = false;

function isInternalWorktreePath(path: string): boolean {
  // The sync-workspace and the `.archived/` graveyard are
  // orchestrator bookkeeping, not user sessions.
  return path.includes(".sync-workspace") || path.includes("/.archived/");
}

async function refreshDiscoveredWorktrees(): Promise<void> {
  if (discoveryInFlight) return;
  discoveryInFlight = true;
  try {
    reconcileSessions();

    // (1) Candidate dirs: every live session's root + the editor
    //     cwd. Resolve each to its canonical main repo root and
    //     dedupe so a repo with N open worktrees is scanned once.
    const candidates = new Set<string>([editor.getCwd()]);
    for (const s of orchestratorSessions.values()) {
      if (!s.discovered) candidates.add(s.root);
    }
    const mainRoots = new Set<string>();
    for (const dir of candidates) {
      const canonical = await resolveCanonicalRepoRoot(dir);
      if (canonical) mainRoots.add(canonical);
    }

    // (2) Roots already occupied by a live session — discovered rows
    //     for these would be duplicates.
    const liveRoots = new Set<string>();
    for (const s of orchestratorSessions.values()) {
      if (!s.discovered) liveRoots.add(s.root);
    }

    // (3) Scan each repo and collect the linked worktrees worth
    //     surfacing.
    const foundPaths = new Set<string>();
    for (const repoRoot of mainRoots) {
      const listed = await listLinkedWorktrees(repoRoot);
      if (!listed) continue;
      for (const wt of listed.worktrees) {
        if (liveRoots.has(wt.path)) continue;
        if (isInternalWorktreePath(wt.path)) continue;
        foundPaths.add(wt.path);
        const id = discoveredIdFor(wt.path);
        const label = wt.branch || editor.pathBasename(wt.path);
        const existing = orchestratorSessions.get(id);
        if (existing) {
          existing.label = label;
          existing.root = wt.path;
          existing.projectPath = listed.mainRoot;
          existing.branch = wt.branch;
        } else {
          orchestratorSessions.set(id, {
            id,
            label,
            root: wt.path,
            projectPath: listed.mainRoot,
            sharedWorktree: false,
            terminalId: null,
            // Discovered on-disk rows have no live terminal; they render
            // a `· on-disk` tag, not a pill, so state is moot — idle.
            state: "idle",
            lastOutputAt: null,
            createdAt: Date.now(),
            discovered: true,
            branch: wt.branch,
          });
        }
      }
    }

    // (4) Prune discovered rows that vanished from disk (or got
    //     opened, picked up by the liveRoots check above).
    for (const [id, s] of orchestratorSessions) {
      if (s.discovered && !foundPaths.has(s.root)) {
        orchestratorSessions.delete(id);
        discoveredIdByPath.delete(s.root);
      }
    }
  } finally {
    discoveryInFlight = false;
  }
  if (openPanel) refreshOpenDialog();
}

// =============================================================================
// Session display helpers
// =============================================================================

// A session counts as "working" only if its terminal printed something
// within this window. Agents are bursty — they pause to think or wait on
// the model between chunks — so a few seconds of grace keeps the dot from
// flickering idle mid-task. Too long and a finished agent reads as busy;
// 5s is a reasonable middle.
const IDLE_AFTER_MS = 5000;

// Coarse activity for a session, derived purely from how recently its
// terminal produced output. This is the single source of truth — the
// stored `state` field is just a cache of this for persistence/sorting.
// No output ever (or no terminal) ⇒ idle: we have no evidence of work.
function sessionState(s: AgentSession): AgentState {
  if (s.lastOutputAt === null) return "idle";
  return Date.now() - s.lastOutputAt < IDLE_AFTER_MS ? "working" : "idle";
}

function ageString(createdAt: number): string {
  const sec = Math.max(0, Math.floor((Date.now() - createdAt) / 1000));
  if (sec < 60) return `${sec}s`;
  if (sec < 3600) return `${Math.floor(sec / 60)}m`;
  return `${Math.floor(sec / 3600)}h`;
}

// =============================================================================
// Status symbol
//
// Each live session shows a single status symbol in the row's left margin —
// before the checkbox and name — so every name lines up in the same column
// regardless of state. Activity is derived from how recently the session's
// terminal printed (see `sessionState`):
//
//   working : `*` in the warning/progress colour — terminal actively printing
//   idle    : `✓` in the added/green colour       — quiet / waiting / done
//
// `*` is ASCII; `✓` (U+2713) is a single-cell glyph present in essentially
// every terminal font — both avoid the box-drawing / half-block / emoji
// glyphs that render unevenly. Colours are theme keys so they track the
// active theme. On-disk (discovered) rows have no agent process, so they get
// no symbol (a blank margin) and keep their `· on-disk` tag instead.
// =============================================================================

interface StatusSymbol {
  // The single glyph painted in the left margin.
  glyph: string;
  // Theme key for the glyph colour, resolved by the host.
  fg: string;
}

const STATE_SYMBOL: Record<AgentState, StatusSymbol> = {
  // In progress — amber/warning, an asterisk reads as "busy/spinner".
  working: { glyph: "*", fg: "diagnostic.warning_fg" },
  // Done/quiet — green check, the universal "complete" mark.
  idle: { glyph: "✓", fg: "file_status_added_fg" },
};

// Width of the left status margin: glyph + trailing space.
const STATUS_MARGIN_W = 2;

// =============================================================================
// Open dialog — widget-based session picker (Phase 1 of the
// open-dialog redesign; see docs/internal/
// orchestrator-open-dialog-and-lifecycle.md).
//
// Dive is the only action the dialog wires up directly. Other
// lifecycle commands (Stop / Archive / Delete / New) ship in
// later phases. New session is still reachable through the
// "Orchestrator: New Session" palette command in the meantime.
// =============================================================================

// Case-insensitive substring match over a session's label and
// root path. Ordering: prefix-of-label hits beat substring hits,
// then ties broken by label length so shorter matches surface
// first. Empty needle returns the full list in numeric-id order.
//
// The picker is cross-project by design — every session is a
// candidate regardless of which project the active window
// points at — so there is no project-scope filter here.
// Project a session belongs to, as a comparison key. Prefer the
// canonical `projectPath` recorded at create time; fall back to
// the session root for sessions that predate the field (the base
// session, externally-created windows).
function projectKeyOf(s: AgentSession): string {
  // The host guarantees `projectPath` is always a non-empty string
  // (defaults to `root` when no explicit project is set), so no
  // `?? root` / `|| root` defence is needed here.
  return s.projectPath;
}

// The project the user is currently "in" — the active window's
// project. Falls back to the editor cwd when the active window
// isn't a tracked session (shouldn't normally happen, but keeps
// scoping well-defined).
function currentProjectKey(): string {
  const s = orchestratorSessions.get(editor.activeWindow());
  return s ? projectKeyOf(s) : editor.getCwd();
}

// Short, human-readable label for a project key — the trailing
// `parent/base` of the path, matching the new-session form's
// `deriveProjectLabel` style.
function projectLabel(key: string): string {
  const base = editor.pathBasename(key);
  const parent = editor.pathBasename(editor.pathDirname(key));
  if (parent && parent !== base) return `${parent}/${base}`;
  return base || key;
}

// Resolve the id list for the current filter + scope.
//
// Scope only constrains the *empty-filter* view: with no needle
// and `scope === "current"`, the list shows just the active
// project's sessions (current project first, by id). As soon as
// the user types, the search goes global regardless of scope —
// hiding a session the user is explicitly searching for would be
// the worse surprise. `scope === "all"` always shows everything,
// sorted by project (current project first) so rows are grouped
// rather than interleaved.
function filterSessions(needle: string): number[] {
  reconcileSessions();
  const scope = openDialog?.scope ?? "current";
  const showWorktrees = openDialog?.showWorktrees ?? false;
  const hideTrivial = openDialog?.hideTrivial ?? false;
  const cur = currentProjectKey();
  let allIds = Array.from(orchestratorSessions.keys());
  // "Show all worktrees" is opt-in: by default the discovered on-disk
  // worktree rows are filtered out.
  if (!showWorktrees) {
    allIds = allIds.filter((id) => !orchestratorSessions.get(id)!.discovered);
  }
  // "Hide empty/1-file sessions": drop the restored shells that hold no
  // real work. The active session is always kept (you must be able to
  // see where you are), and discovered worktree rows are governed by
  // their own toggle, not this one. A session with no summary (e.g. a
  // freshly created agent session not yet written to disk) is kept too.
  if (hideTrivial) {
    const activeId = editor.activeWindow();
    allIds = allIds.filter((id) => {
      const s = orchestratorSessions.get(id)!;
      if (s.discovered || id === activeId) return true;
      const c = sessionContentByRoot.get(normRoot(s.root));
      return !c || !c.trivial;
    });
  }

  const isDisc = (id: number): number =>
    orchestratorSessions.get(id)!.discovered ? 1 : 0;

  // Sort by (current-project-first, project, live-before-discovered,
  // then id) so an "all" view groups the current project's sessions
  // at the top and other projects' below, and within each project the
  // pre-existing live sessions come first with the discovered on-disk
  // worktrees listed after them.
  // The dock is persistent and switches the active session constantly,
  // so it must NOT reorder as the active project changes — pin a stable
  // order (project, then id). The modal picker, opened fresh each time,
  // keeps the current-project-first grouping.
  const pinCurrentFirst = !dockMode;
  const byProjectThenId = (a: number, b: number): number => {
    const sa = orchestratorSessions.get(a)!;
    const sb = orchestratorSessions.get(b)!;
    const aCur = projectKeyOf(sa) === cur ? 0 : 1;
    const bCur = projectKeyOf(sb) === cur ? 0 : 1;
    if (pinCurrentFirst && aCur !== bCur) return aCur - bCur;
    const ka = projectKeyOf(sa);
    const kb = projectKeyOf(sb);
    if (ka !== kb) return ka < kb ? -1 : 1;
    const da = isDisc(a);
    const db = isDisc(b);
    if (da !== db) return da - db;
    return a - b;
  };

  if (!needle) {
    const ids = allIds.slice().sort(byProjectThenId);
    if (scope === "current") {
      return ids.filter((id) => projectKeyOf(orchestratorSessions.get(id)!) === cur);
    }
    return ids;
  }

  const n = needle.toLowerCase();
  type Scored = { id: number; score: number; len: number };
  const matches: Scored[] = [];
  for (const id of allIds) {
    const s = orchestratorSessions.get(id)!;
    const label = s.label.toLowerCase();
    const root = s.root.toLowerCase();
    if (label.startsWith(n)) {
      matches.push({ id, score: 0, len: label.length });
    } else if (label.includes(n)) {
      matches.push({ id, score: 1, len: label.length });
    } else if (root.includes(n)) {
      matches.push({ id, score: 2, len: label.length });
    }
  }
  // Live sessions before discovered worktrees at equal relevance, so
  // the on-disk rows still trail the real sessions in search results.
  matches.sort(
    (a, b) =>
      a.score - b.score || isDisc(a.id) - isDisc(b.id) || a.len - b.len ||
      a.id - b.id,
  );
  return matches.map((m) => m.id);
}

// Header row above the session list: a single dim `NAME`, indented to
// sit over the per-row name (status margin + checkbox = STATUS_MARGIN_W
// + 4 cols). The status symbol lives in the left margin now, so there's
// no separate status column to label.
function sessionsColumnHeader(): WidgetSpec {
  const text = " ".repeat(STATUS_MARGIN_W + 4) + "NAME";
  return {
    kind: "raw",
    entries: [
      styledRow([{ text, style: { fg: "ui.menu_disabled_fg" } }]),
    ],
  };
}

// Build one rendered list-item row for `id`:
//   `<sym> [ ] <name>  <project basename>`
// A leading status symbol (working `*` / idle `✓`, blank for on-disk rows)
// sits in the left margin so every name lines up regardless of state. Then
// the multi-select checkbox, the name (active bold, discovered dim), and an
// optional cross-project basename tag. `contentWidth` is the list column's
// inner width in cells — the dock passes ~30, the modal passes its wider
// session-column width.
function renderListItem(
  id: number,
  activeId: number,
  contentWidth: number,
): TextPropertyEntry {
  const s = orchestratorSessions.get(id);
  if (!s) {
    return styledRow([{ text: "(unknown)" }]);
  }
  const isActive = id === activeId;
  const isDiscovered = !!s.discovered;
  const isChecked = openDialog?.selectedIds.has(id) ?? false;

  // Left status margin: live sessions get a coloured working/idle symbol
  // (recomputed from the output timestamp at render time — the stored
  // `state` can be stale since nothing fires when a session goes quiet).
  // On-disk rows have no agent, so they get a blank margin of the same
  // width to keep names aligned.
  const marginEntry = isDiscovered
    ? { text: " ".repeat(STATUS_MARGIN_W) }
    : (() => {
        const sym = STATE_SYMBOL[sessionState(s)];
        return { text: sym.glyph + " ", style: { fg: sym.fg, bold: true } };
      })();

  // Multi-select checkbox. `[x]` when this row is in the bulk selection,
  // `[ ]` otherwise — toggled with Space (the rebindable
  // `orchestrator_toggle_select`) or a click. Kept contiguous with the
  // name (`[ ] <name>`) — other code and tests key off that.
  const checkbox = {
    text: isChecked ? "[x] " : "[ ] ",
    style: isChecked
      ? { fg: "ui.help_key_fg", bold: true }
      : { fg: "ui.menu_disabled_fg" },
  };

  const entries: { text: string; style?: Record<string, unknown> }[] = [
    marginEntry,
    checkbox,
    {
      text: s.label,
      style: isActive
        ? { fg: "ui.help_key_fg", bold: true }
        : isDiscovered
        ? { fg: "ui.menu_disabled_fg" }
        : undefined,
    },
  ];
  // Running tally of the row width so far (margin + checkbox + name).
  let leftWidth = STATUS_MARGIN_W + 4 + s.label.length;

  // On-disk tag for discovered worktrees — they have no live state, so
  // this is how the row advertises it's an unopened on-disk worktree.
  if (isDiscovered) {
    const tag = " · on-disk";
    if (leftWidth + tag.length <= contentWidth) {
      entries.push({
        text: tag,
        style: { fg: "ui.menu_disabled_fg", italic: true },
      });
      leftWidth += tag.length;
    }
  }

  // PROJECT tag: basename for cross-project rows only; current-project
  // rows leave it blank. Its presence/absence is the switch signal other
  // code waits on, so keep it unless it genuinely doesn't fit.
  const proj = projectKeyOf(s);
  if (proj !== currentProjectKey()) {
    const tag = editor.pathBasename(proj);
    const tagText = "  " + tag;
    if (leftWidth + tagText.length <= contentWidth) {
      entries.push({
        text: tagText,
        style: { fg: "ui.menu_disabled_fg", italic: true },
      });
      leftWidth += tagText.length;
    }
  }

  return styledRow(entries as Parameters<typeof styledRow>[0]);
}

// Preview-pane content for the currently selected session.
// Plain info for Phase 1; later phases append pgid/pids + the
// last terminal lines.
function buildPreviewEntries(
  s: AgentSession | undefined,
): TextPropertyEntry[] {
  if (!s) {
    return [
      styledRow([
        {
          text: "No session selected",
          style: { fg: "editor.whitespace_indicator_fg", italic: true },
        },
      ]),
    ];
  }
  const activeId = editor.activeWindow();
  const isActive = s.id === activeId;
  // The focused window is labelled "active"; everything else shows its
  // live working/idle activity (recomputed from the output timestamp).
  const stateText = isActive ? "active" : sessionState(s);
  const headerEntries: { text: string; style?: Record<string, unknown> }[] = [
    {
      text: stateText,
      style: isActive
        ? { fg: "ui.help_key_fg", bold: true }
        : { fg: "ui.menu_disabled_fg" },
    },
    { text: "  " },
    { text: ageString(s.createdAt), style: { fg: "ui.menu_disabled_fg" } },
  ];
  if (!s.discovered && !ownsWorktree(s)) {
    // In-place / launch session: runs inside a real checkout, owns no
    // dedicated worktree. Surfaced so the user knows Archive doesn't
    // apply (Delete just forgets it, leaving the directory untouched).
    headerEntries.push(
      { text: "  " },
      { text: "in-place", style: { fg: "ui.menu_disabled_fg", italic: true } },
    );
  }
  return [
    styledRow(headerEntries as Parameters<typeof styledRow>[0]),
    styledRow([
      { text: s.root, style: { fg: "ui.menu_disabled_fg" } },
    ]),
  ];
}

// A session "owns" a removable git worktree when it was created as a
// dedicated `git worktree add` (project path set, not a shared/in-place
// root) or was discovered on disk via `git worktree list`. Only these
// have a worktree to `git worktree remove`/`move`. The launch session
// (the dir the editor was started in) and in-place sessions run inside
// a real checkout, so Archive (which moves the worktree) doesn't apply
// and Delete simply forgets the session without touching the directory.
function ownsWorktree(s: AgentSession): boolean {
  // "Has an explicit project that's separate from this session's
  // root" means the session is a worktree of that project — Archive
  // / Delete apply. `projectPath === root` is the "no separate
  // project" case (host normalises absence → root); skip those too.
  return (
    !!s.discovered || (s.projectPath !== s.root && !s.sharedWorktree)
  );
}

// =============================================================================
// Multi-select / bulk actions
//
// The user checkbox-selects rows (Space — the rebindable
// `orchestrator_toggle_select` — or a click). Once two or more rows
// are checked the preview pane swaps to the bulk selection bar
// (`buildBulkPane`) offering Stop / Archive / Delete over the whole
// set, with a single confirmation for the batch. Rows ineligible for
// a given action (the base session; live sessions sharing a worktree)
// are skipped, and each button's count reflects only the eligible
// members.
// =============================================================================

type BulkAction = "stop" | "archive" | "delete";

// Checked ids that still resolve to a known session, in the dialog's
// current display order (so the bulk bar lists them the way the list
// shows them). Selection persists across filter/scope changes, so an
// id can be checked while filtered out of view — those still count.
function selectedSessions(): number[] {
  if (!openDialog) return [];
  const order = openDialog.filteredIds;
  const seen = new Set<number>();
  const out: number[] = [];
  for (const id of order) {
    if (openDialog.selectedIds.has(id) && orchestratorSessions.has(id)) {
      out.push(id);
      seen.add(id);
    }
  }
  // Checked-but-filtered-out rows, appended in id order so the count
  // stays honest even when a search hides part of the selection.
  for (const id of openDialog.selectedIds) {
    if (!seen.has(id) && orchestratorSessions.has(id)) out.push(id);
  }
  return out;
}

// Is `id` a legal target for `action`? Base session is never
// touched. Stop only applies to live windows. Archive/Delete apply
// to discovered worktrees (removable on disk) and to live sessions
// that own their worktree outright (not shared with siblings or the
// project root).
function bulkEligible(action: BulkAction, id: number): boolean {
  const s = orchestratorSessions.get(id);
  if (!s) return false;
  // Stop kills the agent process group — only meaningful for a live
  // session that actually spawned one (never the launch session, which
  // has no agent terminal, so signalling it can't touch the editor).
  if (action === "stop") return !s.discovered && id > 0 && !!s.terminalId;
  // Delete forgets any session. When it owns a worktree the worktree is
  // removed too; otherwise (launch/in-place) it's just dropped.
  if (action === "delete") return id > 0 || !!s.discovered;
  // Archive applies to any session: a worktree session moves to the
  // graveyard; a launch/in-place session is recorded at its own root.
  return id > 0 || !!s.discovered;
}

function eligibleSelected(action: BulkAction): number[] {
  return selectedSessions().filter((id) => bulkEligible(action, id));
}

// Drop checked ids whose session has vanished (closed window,
// pruned worktree) so the selection can't grow stale references.
function pruneSelection(): void {
  if (!openDialog) return;
  for (const id of [...openDialog.selectedIds]) {
    if (!orchestratorSessions.has(id)) openDialog.selectedIds.delete(id);
  }
}

// Blank-row separator used inside the Sessions column between
// the filter, the new-session button, and the list.
function sessionsSeparator(): WidgetSpec {
  return spacer(0);
}

// A full-width horizontal rule (`────`) used in the dock to divide the
// header chrome from the session list. Rendered in the dim disabled-menu
// colour (a quiet grey, not the louder popup-border accent) so it reads
// as a subtle separator rather than competing with the pills below.
// `width` is the content width so it stops at the border.
function dockRule(width: number): WidgetSpec {
  return {
    kind: "raw",
    entries: [
      styledRow([
        { text: "─".repeat(Math.max(1, width)), style: { fg: "ui.menu_disabled_fg" } },
      ]),
    ],
  };
}

// Smallest list height we'll show even when there are only a
// couple of sessions — keeps the preview pane (which matches the
// list height) usable rather than collapsing to a sliver.
const MIN_LIST_ROWS = 6;

// Upper bound on session rows for this terminal — the list height
// when the panel is at its full `heightPct: 90` budget. Sized off
// the full terminal (not the active buffer's viewport — that
// shrinks with vertical splits and made the picker collapse to
// ~half its budget).
function maxListRowsForScreen(): number {
  const screen = editor.getScreenSize();
  const h = screen.height > 0 ? screen.height : 30;
  const panelH = Math.floor(h * 0.9);
  // Chrome that isn't list rows: panel borders (2) + title (1) +
  // spacer (1) + footer (1) + sessions-section borders (2) +
  // column chrome above the list (New + Project + Worktree-filter +
  // Trivial-filter + Filter + separator + header = 7) = 14. Floor at
  // MIN_LIST_ROWS so a tiny terminal still shows something.
  return Math.max(MIN_LIST_ROWS, panelH - 14);
}

// Inner width (cells) of the modal picker's session column, used to
// size name/tag truncation. The panel is 90% of the terminal and the
// sessions `labeledSection` is `widthPct: 34` of that; subtract the
// section's border (2) + inner padding (2). Floored so a narrow terminal
// still renders a usable column.
function modalSessionColWidth(): number {
  const screen = editor.getScreenSize();
  const w = screen.width > 0 ? screen.width : 80;
  const panelW = Math.floor(w * 0.9);
  const sectionW = Math.floor(panelW * 0.34);
  return Math.max(dockContentCols(DOCK_MIN_WIDTH_COLS), sectionW - 4);
}

// Compose the right-hand preview pane. Normally it shows info
// + action buttons (Stop, Archive, Delete); when a destructive
// action is pending confirmation it swaps to a "Confirm
// <action>?" panel with [ Confirm <action> ] / [ Cancel ]
// buttons. Cancel is default-focused for safety.
function buildPreviewPane(s: AgentSession | undefined): WidgetSpec {
  // In-flight overlay: when the selected session is currently
  // being archived/deleted, swap the preview pane for a
  // non-interactive status panel. The git operations take a few
  // hundred ms; without this the user clicks Confirm Archive and
  // sees no visible reaction until the editor's `window_closed`
  // hook eventually fires and drops the row. The overlay makes
  // the in-flight state explicit and hides the action buttons so
  // a second click can't double-fire.
  if (openDialog?.inFlight && s && openDialog.inFlight.sessionId === s.id) {
    const label = openDialog.inFlight.action === "archive"
      ? "Archiving…"
      : "Deleting…";
    return labeledSection({
      label,
      child: col(
        {
          kind: "raw",
          entries: [
            styledRow([
              {
                text: `${label} [${s.id}] ${s.label}`,
                style: { bold: true, fg: "ui.menu_disabled_fg" },
              },
            ]),
            styledRow([{ text: "" }]),
            styledRow([
              {
                text: "Waiting for git…",
                style: { fg: "ui.menu_disabled_fg", italic: true },
              },
            ]),
          ],
        },
      ),
    });
  }
  // Confirmation panel — single-row Stop/Archive/Delete or a bulk
  // batch. Independent of the cursor row: the confirmed ids live in
  // `pendingConfirm`, so it renders whenever a confirm is pending.
  if (openDialog?.pendingConfirm) {
    return buildConfirmPane(openDialog.pendingConfirm);
  }
  // Bulk selection bar: two or more rows checked (or a bulk action
  // in flight) → operate on the whole batch rather than the cursor
  // row.
  if (selectedSessions().length >= 2 || openDialog?.bulkInFlight) {
    return buildBulkPane();
  }
  // Match the sessions column's content height so the two panes'
  // bottom borders land on the same row. Sessions column inside its
  // borders = New (1) + Project (1) + Worktree-filter (1) +
  // Trivial-filter (1) + Filter (1) + separator (1) + header (1) +
  // list (listVisibleRows) = listVisibleRows + 7. Preview inside its
  // borders = button row (1) + spacer (1) + embedRows, so embedRows
  // must equal listVisibleRows + 5. When details ARE shown, two info
  // rows + a spacer eat three more lines — `_DETAILS_CHROME_ROWS`
  // accounts for that.
  const totalEmbedBase = (openDialog?.listVisibleRows ?? MIN_LIST_ROWS) + 5;
  const detailsOn = openDialog?.showDetails ?? false;
  const _DETAILS_CHROME_ROWS = 3; // 2 info rows + 1 spacer
  const embedRows = Math.max(
    3,
    totalEmbedBase - (detailsOn ? _DETAILS_CHROME_ROWS : 0),
  );
  // Gate the action buttons on having a session to act on. When
  // the filter matches nothing (or no session is highlighted) the
  // preview pane shows just "No session selected" + an empty
  // embed reservation — showing Stop/Archive/Delete in that state
  // is misleading because they have nothing to operate on. The
  // empty `windowEmbed({windowId: 0})` is a no-op on the host
  // side but keeps the preview pane the same height as the
  // (padded) sessions list pane so the dialog doesn't shrink
  // jarringly when the filter matches nothing.
  if (!s) {
    return labeledSection({
      label: "Preview",
      child: col(
        { kind: "raw", entries: buildPreviewEntries(s) },
        windowEmbed({ windowId: 0, rows: embedRows, key: "live-preview" }),
      ),
    });
  }
  // The "details" toggle: when off, the picker shows just the
  // action buttons + the live embed (compact, max embed height).
  // When on, the state/age/path metadata row appears above the
  // embed and the embed shrinks to make room. Toggle button
  // labels with the *target* state — pressing `[ Details ]`
  // turns details on, pressing `[ Preview ]` turns them off
  // (back to compact).
  const detailsToggleLabel = detailsOn ? "Preview" : "Details";
  // Discovered worktree: no live window to embed, so there's
  // nothing to Stop / Archive / Delete yet. Offer only "Open"
  // (Visit attaches a fresh session to the worktree) and describe
  // what diving will do. The empty `windowId: 0` embed keeps the
  // pane the same height as live-session previews so the dialog
  // doesn't jump when the selection moves between row kinds.
  if (s.discovered) {
    const openButtonRow = row(
      button("Open", { intent: "primary", key: "visit" }),
      flexSpacer(),
      button("Stop", { key: "stop", disabled: true }),
      spacer(2),
      button("Archive", { key: "archive", disabled: true }),
      spacer(2),
      button("Delete", { intent: "danger", key: "delete", disabled: true }),
    );
    const info: TextPropertyEntry[] = [
      styledRow([
        { text: "On-disk worktree (not open)", style: { fg: "ui.menu_disabled_fg", bold: true } },
      ]),
      styledRow([{ text: "" }]),
      styledRow([{ text: "branch  ", style: { fg: "ui.menu_disabled_fg" } }, { text: s.branch || "(detached)" }]),
      styledRow([{ text: "path    ", style: { fg: "ui.menu_disabled_fg" } }, { text: s.root }]),
      styledRow([{ text: "" }]),
      styledRow([
        {
          text: "Press Enter to open this worktree as a session.",
          style: { fg: "ui.help_key_fg", italic: true },
        },
      ]),
    ];
    return labeledSection({
      label: `${s.label}  —  on-disk worktree`,
      child: col(
        openButtonRow,
        spacer(0),
        { kind: "raw", entries: info },
        spacer(0),
        windowEmbed({ windowId: 0, rows: Math.max(3, embedRows - 6), key: "live-preview" }),
      ),
    });
  }
  // Per-action availability. The row always renders all four
  // buttons (no layout shift between selections), but each is
  // marked disabled when its action would be refused against the
  // current selection. Disabled buttons show in `ui.menu_disabled_fg`,
  // drop out of the Tab cycle, and reject clicks — matching the
  // same conditions that `stopSelectedSession`, `enterConfirm`,
  // and the lifecycle handlers already check internally.
  //
  //  * Stop: only a live session with an agent terminal can be
  //    stopped (the launch session has none).
  //  * Archive: every session can be archived — a worktree session moves
  //    to the graveyard; a launch/in-place session is recorded at its own
  //    root. Closing the last live window opens a replacement first.
  //  * Delete: forgets the session, removing the worktree only when one
  //    is owned (otherwise the directory is left untouched); the last
  //    live window likewise gets a replacement before it closes.
  const stopDisabled = s.discovered || !s.terminalId;
  const archiveDisabled = false;
  const deleteDisabled = false;
  const buttonRow = row(
    button("Visit", { intent: "primary", key: "visit" }),
    spacer(2),
    flexSpacer(),
    button(detailsToggleLabel, { key: "toggle-details" }),
    spacer(2),
    button("Stop", { key: "stop", disabled: stopDisabled }),
    spacer(2),
    button("Archive", { key: "archive", disabled: archiveDisabled }),
    spacer(2),
    button("Delete", {
      intent: "danger",
      key: "delete",
      disabled: deleteDisabled,
    }),
  );
  const embedWidget = windowEmbed({
    windowId: s.id,
    rows: embedRows,
    key: "live-preview",
  });
  const body = detailsOn
    ? col(
        buttonRow,
        spacer(0),
        { kind: "raw", entries: buildPreviewEntries(s) },
        spacer(0),
        embedWidget,
      )
    : col(buttonRow, spacer(0), embedWidget);
  // Surface the launch session in the preview label so it's always
  // visible (the list-row badge gets truncated at 25% column width).
  // It's the dir the editor was started in — informational only; it's
  // deletable like any other session once another window exists.
  const sectionLabel = s.id === 1
    ? `${s.label}  —  launch session`
    : s.label;
  return labeledSection({
    label: sectionLabel,
    child: body,
  });
}

// The per-action bullet lines shown in the confirmation panel.
// `delete` adds a separate red "uncommitted changes" line in the
// caller because it needs distinct styling.
function confirmActionLines(action: BulkAction): string[] {
  switch (action) {
    case "stop":
      return [
        "  • send SIGTERM to all session processes",
        "  • SIGKILL after a short grace period",
        "",
        "The worktree and session record remain.",
      ];
    case "archive":
      return [
        "  • SIGKILL all session processes",
        "  • close the editor session",
        "  • move the worktree to .archived/",
        "",
        "Reversible via Unarchive.",
      ];
    case "delete":
      return [
        "  • stop all session processes",
        "  • run `git worktree remove`",
        "  • drop the session record",
      ];
  }
}

// Confirmation panel for a Stop/Archive/Delete over one or many
// sessions. A single id renders the familiar per-session prompt; two
// or more render a batch prompt that lists the targets. The Confirm
// button reuses the same `confirm-<action>` key the single path
// always used, so the existing widget_event handlers fire for both —
// they read `pendingConfirm.ids`.
function buildConfirmPane(
  confirm: { action: BulkAction; ids: number[] },
): WidgetSpec {
  const { action, ids } = confirm;
  const cap = action[0].toUpperCase() + action.slice(1);
  const existing = ids.filter((id) => orchestratorSessions.has(id));
  const bulk = existing.length > 1;
  const diskNote = (id: number): string =>
    orchestratorSessions.get(id)?.discovered ? "  · on-disk" : "";
  const entries: TextPropertyEntry[] = [];
  if (bulk) {
    entries.push(
      styledRow([
        { text: `${cap} these ${existing.length} sessions?`, style: { bold: true } },
      ]),
      styledRow([{ text: "" }]),
    );
    for (const id of existing.slice(0, 8)) {
      const ss = orchestratorSessions.get(id)!;
      entries.push(
        styledRow([
          { text: `  ${ss.label}` },
          { text: diskNote(id), style: { fg: "ui.menu_disabled_fg", italic: true } },
        ]),
      );
    }
    if (existing.length > 8) {
      entries.push(
        styledRow([
          {
            text: `  … and ${existing.length - 8} more`,
            style: { fg: "ui.menu_disabled_fg", italic: true },
          },
        ]),
      );
    }
  } else {
    const id = existing[0];
    const ss = id !== undefined ? orchestratorSessions.get(id) : undefined;
    entries.push(
      styledRow([
        { text: `${cap} session ${ss?.label ?? ""}?`, style: { bold: true } },
      ]),
    );
  }
  entries.push(
    styledRow([{ text: "" }]),
    styledRow([{ text: bulk ? "For each session this will:" : "This will:" }]),
  );
  for (const line of confirmActionLines(action)) {
    entries.push(styledRow([{ text: line }]));
  }
  if (action === "delete") {
    entries.push(
      styledRow([{ text: "" }]),
      styledRow([
        {
          text: "Uncommitted changes will be lost.",
          style: { fg: "ui.status_error_indicator_fg", bold: true },
        },
      ]),
    );
  }
  return labeledSection({
    label: bulk ? `Confirm ${cap} — ${existing.length} sessions` : `Confirm ${cap}`,
    child: col(
      { kind: "raw", entries },
      spacer(0),
      row(
        flexSpacer(),
        button("Cancel", { key: "confirm-cancel" }),
        spacer(2),
        button(`Confirm ${cap}`, { intent: "danger", key: `confirm-${action}` }),
      ),
    ),
  });
}

// The dedicated bulk selection bar (Layout B). Shown in place of the
// per-session preview when two or more rows are checked. The bulk
// action buttons sit at the *top* of the pane; the list of affected
// sessions renders below as a scrollable `list` widget (so a long
// selection scrolls — keyboard, wheel, and the draggable scrollbar —
// rather than overflowing the pane). Each action's count is the
// number of *eligible* members; an action with no eligible members is
// disabled.
function buildBulkPane(): WidgetSpec {
  const sel = selectedSessions();
  const stopN = eligibleSelected("stop").length;
  const archiveN = eligibleSelected("archive").length;
  const deleteN = eligibleSelected("delete").length;

  const inflight = openDialog?.bulkInFlight ?? null;
  const actionRow = inflight
    ? row(
        {
          kind: "raw",
          entries: [
            styledRow([
              {
                text: `${inflight.action[0].toUpperCase()}${inflight.action.slice(1)}ing ${inflight.done}/${inflight.total}…`,
                style: { fg: "ui.menu_disabled_fg", italic: true },
              },
            ]),
          ],
        },
        flexSpacer(),
      )
    : row(
        button(`Stop (${stopN})`, { key: "bulk-stop", disabled: stopN === 0 }),
        spacer(2),
        button(`Archive (${archiveN})`, {
          key: "bulk-archive",
          disabled: archiveN === 0,
        }),
        spacer(2),
        button(`Delete (${deleteN})`, {
          intent: "danger",
          key: "bulk-delete",
          disabled: deleteN === 0,
        }),
        flexSpacer(),
        button("Clear", { key: "bulk-clear" }),
      );

  // Affected-sessions list. Flag the rows a destructive action will
  // skip so the count discrepancy explains itself.
  const items: TextPropertyEntry[] = sel.map((id) => {
    const ss = orchestratorSessions.get(id)!;
    const rowParts: StyledSegment[] = [{ text: `  ${ss.label}` }];
    if (!ss.discovered && !ownsWorktree(ss)) {
      rowParts.push({
        text: "  · in-place (forgotten, not removed)",
        style: { fg: "ui.menu_disabled_fg", italic: true },
      });
    } else if (ss.discovered) {
      rowParts.push({
        text: "  · on-disk worktree",
        style: { fg: "ui.menu_disabled_fg", italic: true },
      });
    }
    return styledRow(rowParts);
  });
  const itemKeys = sel.map((id) => `bulksel-${id}`);
  // Match the preview pane's height: content = action row (1) +
  // spacer (1) + list, and the embed pane reserves `listVisibleRows
  // + 4` for its body — so the list takes that height and the two
  // panes' bottom borders line up.
  const listRows = Math.max(3, (openDialog?.listVisibleRows ?? MIN_LIST_ROWS) + 4);

  return labeledSection({
    label: `Bulk actions — ${sel.length} selected`,
    child: col(
      actionRow,
      spacer(0),
      list({
        items,
        itemKeys,
        // Display-only: no highlighted row, and out of the Tab cycle
        // (focus belongs on the action buttons). Up/Down still scroll
        // it via the host's smart-key forwarding, and the scrollbar
        // drags it.
        selectedIndex: -1,
        visibleRows: listRows,
        focusable: false,
        key: "bulk-list",
      }),
    ),
  });
}

function buildOpenSpec(): WidgetSpec {
  if (!openDialog) return col();
  const filtered = openDialog.filteredIds;
  // Fill the panel's full height budget (the list pads with blank
  // rows when there are few sessions) so the dialog stays
  // vertically full rather than collapsing to a short floating box.
  openDialog.listVisibleRows = maxListRowsForScreen();
  const colWidth = modalSessionColWidth();
  const activeId = editor.activeWindow();
  const items = filtered.map((id) => renderListItem(id, activeId, colWidth));
  const itemKeys = filtered.map(String);
  const selIdx = filtered.length === 0
    ? -1
    : Math.max(0, Math.min(openDialog.selectedIndex, filtered.length - 1));
  // Gate on the *index* (selIdx < 0 means "filter matched nothing"),
  // not the sign of the id: discovered worktrees carry negative ids
  // and must still resolve to their row here.
  const selectedSession = selIdx >= 0
    ? orchestratorSessions.get(filtered[selIdx])
    : undefined;

  // The "New Session" button advertises Alt+N (or whatever the
  // user re-bound `orchestrator_open_new_from_picker` to). The
  // label reads the binding dynamically through the host's
  // `getKeybindingLabel` so a re-bound key shows correctly, and
  // the host's `format_keybinding` already renders Mac-native
  // symbols (⌥, ⌘, …) when running on macOS — no plugin-side
  // platform detection needed.
  //
  // The button is the *first* tabbable in the dialog (top of the
  // sessions column, before the filter input) so default focus
  // lands on it directly — Enter creates a new session without
  // requiring the user to navigate first.
  const newKey = editor.getKeybindingLabel(
    "orchestrator_open_new_from_picker",
    OPEN_MODE,
  );
  const newLabel = newKey ? `+ New  ${newKey}` : "+ New";
  const inConfirm = openDialog.pendingConfirm !== null;
  // While a confirmation prompt is up the filter is rendered
  // without a `key`. The host's `collect_tabbable` only adds
  // widgets that carry a non-empty key, so a keyless text widget
  // is unreachable by Tab and doesn't receive `mode_text_input`
  // — the bracketed input still paints normally, just inert.
  // Keeping the visual chrome (instead of swapping it for a
  // "(disabled)" label) means the dialog doesn't reflow under
  // the user's eyes when the confirm view opens / closes.
  const filterInput = text({
    value: openDialog.filter.value,
    cursorByte: openDialog.filter.cursor,
    label: "Filter",
    placeholder: "type to search… ( / )",
    fullWidth: true,
    key: inConfirm ? undefined : "filter",
  });
  const errorBanner: WidgetSpec | null = openDialog.lastError
    ? {
        kind: "raw",
        entries: [
          styledRow([
            {
              text: "⚠ ",
              style: { fg: "ui.status_error_indicator_fg", bold: true },
            },
            {
              text: openDialog.lastError,
              style: { fg: "ui.status_error_indicator_fg" },
            },
          ]),
        ],
      }
    : null;

  // Scope chrome. The title keeps the active project visible; the
  // `Project:` control below is the clickable scope switch.
  const scope = openDialog.scope;
  const curKey = currentProjectKey();
  const curName = projectLabel(curKey);
  const scopeKey = editor.getKeybindingLabel("orchestrator_toggle_scope", OPEN_MODE);
  const titleSuffix = scope === "current" ? `  —  ${curName}` : "  —  all projects";
  const sectionLabel = "Sessions";
  // `Project:` control — a visible, clickable scope switch with the
  // Alt+P hint baked into the button label. Shows the current
  // project's name when scoped, "All" when showing every project.
  // Inert while a confirm prompt is up so it can't steal focus.
  const scopeWord = scope === "current" ? editor.pathBasename(curKey) : "All";
  const scopeButtonLabel = scopeKey ? `${scopeWord} ▾   (${scopeKey})` : `${scopeWord} ▾`;
  const scopeButton = button(scopeButtonLabel, {
    key: openDialog.pendingConfirm !== null ? undefined : "scope-toggle",
  });
  const projectControlRow = row(
    {
      kind: "raw",
      entries: [
        styledRow([{ text: "Project: ", style: { fg: "ui.menu_disabled_fg" } }]),
      ],
    },
    scopeButton,
    flexSpacer(),
  );
  // Per-project filter checkbox, on its own row under the Project
  // control: opt-in toggle that surfaces the discovered on-disk
  // worktree rows. A `toggle` (single `[ ]`/`[v]` — no double
  // bracket) that's clickable and bound to Alt+T
  // (`orchestrator_toggle_worktrees`, rebindable). The label carries
  // the live keybinding hint, mirroring the Project control's
  // "(Alt+P)". Inert while a confirm prompt is up.
  const worktreeKey = editor.getKeybindingLabel(
    "orchestrator_toggle_worktrees",
    OPEN_MODE,
  );
  const worktreeLabel = worktreeKey
    ? `Show all worktrees   (${worktreeKey})`
    : "Show all worktrees";
  const worktreeFilterRow = row(
    toggle(openDialog.showWorktrees, worktreeLabel, {
      key: openDialog.pendingConfirm !== null ? undefined : "worktree-show",
    }),
    flexSpacer(),
  );
  // Content filter checkbox, beneath the worktree one. The flag is
  // `hideTrivial`, but the checkbox reads as an opt-in "show" toggle to
  // match the worktree row: unchecked (default) hides the empty /
  // single-file shells, checking it reveals them. Inert during confirm.
  const trivialKey = editor.getKeybindingLabel(
    "orchestrator_toggle_trivial",
    OPEN_MODE,
  );
  const trivialLabel = trivialKey
    ? `Show empty/1-file sessions   (${trivialKey})`
    : "Show empty/1-file sessions";
  const trivialFilterRow = row(
    toggle(!openDialog.hideTrivial, trivialLabel, {
      key: openDialog.pendingConfirm !== null ? undefined : "hide-trivial",
    }),
    flexSpacer(),
  );

  return col(
    {
      kind: "raw",
      entries: [
        styledRow([
          {
            text: "ORCHESTRATOR :: Sessions",
            style: { fg: "ui.popup_border_fg", bold: true },
          },
          {
            text: titleSuffix,
            style: { fg: "ui.menu_disabled_fg" },
          },
        ]),
      ],
    },
    ...(errorBanner ? [errorBanner] : []),
    spacer(0),
    // Two-pane: sessions list | preview. Renderer's `row()`
    // horizontally zips multi-line children so this composes
    // the wireframed shape directly. Width split 25 / 75 —
    // the preview pane carries the action buttons and the
    // (Phase 7) live-window render, so it earns the bulk of
    // the dialog.
    row(
      labeledSection({
        label: sectionLabel,
        // 34% (was 25%): wide enough that the per-row project tag in
        // the all-projects view (`· <project>`) and longer session
        // labels render without truncating to `· tmp_o…`. The preview
        // pane still keeps the majority for the live window embed.
        widthPct: 34,
        // Sessions column: New button, Project (scope) control,
        // Filter, separator, column header, list. The button is
        // first so it gets initial focus (Enter immediately opens the
        // new session form). Separators are long `─` strings that the
        // renderer truncates to the column's inner width — no need to
        // measure cells from the plugin side.
        child: col(
          row(
            button(newLabel, {
              intent: "primary",
              // Drop the key while a confirm prompt is up so the
              // button is non-tabbable and click-inert — same
              // pattern the filter input uses. Otherwise it stays
              // the first tabbable in the panel and the confirm
              // view's "first-tabbable wins" focus fallback lands
              // here instead of on Cancel.
              key: inConfirm ? undefined : "new-session",
            }),
            flexSpacer(),
          ),
          projectControlRow,
          worktreeFilterRow,
          trivialFilterRow,
          filterInput,
          sessionsSeparator(),
          sessionsColumnHeader(),
          list({
            items,
            itemKeys,
            selectedIndex: selIdx,
            // `listVisibleRows` is the fitted list height; the 5 rows
            // of column chrome above it (New / Project / Filter /
            // separator / header) and the matching preview embed are
            // accounted for separately so both panes stay the same
            // height and the footer hint stays on-screen.
            visibleRows: openDialog.listVisibleRows,
            // Excluded from the Tab cycle — Up/Down on the
            // filter input forwards to this list via host
            // smart-keys, so Tab jumps straight to the action
            // buttons instead of stopping here.
            focusable: false,
            // Drop the `key` while a confirmation prompt is up so
            // `find_scrollable_widget_key` (`plugin_dispatch.rs`)
            // can't find this list — Up/Down on the focused Cancel
            // button would otherwise forward to the list and let
            // the user move the selection off the session being
            // confirmed (which would break the confirm view because
            // it only renders when the selected row matches
            // `pendingConfirm.sessionId`).
            key: inConfirm ? undefined : "sessions",
          }),
        ),
      }),
      // Preview pane has no explicit width — picks up the
      // remaining width by default since the sessions list took 34%.
      buildPreviewPane(selectedSession),
    ),
    row(
      flexSpacer(),
      hintBar([
        { keys: "↑↓", label: "nav" },
        { keys: "Enter", label: "dive" },
        {
          keys: editor.getKeybindingLabel("orchestrator_toggle_select", OPEN_MODE) ||
            "Space",
          label: "select",
        },
        {
          keys: scopeKey || "⌥P",
          label: scope === "current" ? "all projects" : "current only",
        },
        { keys: "Tab", label: "focus" },
        { keys: "Esc", label: "close" },
      ]),
      flexSpacer(),
      syncIndicator(),
    ),
  );
}

// Tiny status glyph rendered at the trailing edge of the
// footer. `↻` while a push is in flight, `⤒` when the last
// push failed (with the error in the tooltip — for now, just a
// status-bar setStatus on focus), and an empty entry otherwise
// so the layout stays put.
function syncIndicator(): WidgetSpec {
  let glyph = "";
  let style: { fg?: string; italic?: boolean } | undefined;
  switch (syncStatus) {
    case "syncing":
      glyph = " ↻ ";
      style = { fg: "editor.whitespace_indicator_fg" };
      break;
    case "error":
      glyph = " ⤒ ";
      style = { fg: "ui.status_error_indicator_fg" };
      break;
    default:
      glyph = "   ";
  }
  return {
    kind: "raw",
    entries: [styledRow([{ text: glyph, style }])],
  };
}

// Surface a lifecycle-action refusal in two places: the dialog
// itself (a coloured banner above the filter, hard to miss while
// the user's attention is on the dialog) and the status bar
// (matches the long-standing convention and survives if the
// dialog closes). Pass the bare reason — the picker prepends
// "Orchestrator: " for the status bar.
function setDialogError(msg: string): void {
  if (openDialog) {
    openDialog.lastError = msg;
  }
  editor.setStatus(`Orchestrator: ${msg}`);
}

function clearDialogError(): void {
  if (openDialog?.lastError) {
    openDialog.lastError = null;
  }
}

function refreshOpenDialog(): void {
  if (!openPanel || !openDialog) return;
  pruneSelection();
  openDialog.filteredIds = filterSessions(openDialog.filter.value);
  // Clamp the selection into range so a fresh filter or a
  // session vanishing under us doesn't leave us pointing past
  // the end of the list.
  if (openDialog.filteredIds.length === 0) {
    openDialog.selectedIndex = 0;
  } else if (openDialog.selectedIndex >= openDialog.filteredIds.length) {
    openDialog.selectedIndex = openDialog.filteredIds.length - 1;
  } else if (openDialog.selectedIndex < 0) {
    openDialog.selectedIndex = 0;
  }
  openPanel.update(dockMode ? buildDockSpec() : buildOpenSpec());
  // The list widget's `selectedIndex` in the spec is initial-only;
  // pin it via mutation so re-renders don't snap back to 0.
  if (openDialog.filteredIds.length > 0) {
    openPanel.setSelectedIndex("sessions", openDialog.selectedIndex);
  }
}

function openControlRoom(opts: { dock?: boolean } = {}): void {
  const asDock = opts?.dock === true;
  if (openPanel) {
    // A panel already occupies the shared dock/dialog slot. If the dock
    // is showing and the user asked for the modal picker (Orchestrator:
    // Open), the dock already *is* the live session list — refocus it
    // and say so, rather than silently doing nothing. The modal and the
    // dock share one panel + state today, so they can't both render at
    // once; full coexistence is the deferred P1 redesign (see
    // docs/internal/orchestrator-dock-gaps.md). An `asDock` re-entry
    // (Toggle Dock) never reaches here — `toggleDock` handles it first.
    if (!asDock && dockMode) {
      restoreDockAfterForm();
      editor.setStatus(
        "Orchestrator: the dock already lists sessions — hide it (Toggle Dock) to open the full picker",
      );
    }
    return;
  }
  reconcileSessions();
  // Summarise on-disk session content up front so the trivial filter
  // has data on the first render.
  scanSessionContent();
  const activeId = editor.activeWindow();
  // Seed with the screen-max; buildOpenSpec refits to the session
  // count on the first render (and every render after).
  const listVisibleRows = maxListRowsForScreen();
  openDialog = {
    filter: { value: "", cursor: 0 },
    filteredIds: [],
    selectedIndex: 0,
    originalActiveSession: activeId,
    pendingConfirm: null,
    listVisibleRows,
    embedRows: Math.max(3, listVisibleRows + 3),
    showDetails: false,
    inFlight: null,
    lastError: null,
    // Restore the last-used scope (defaults to "all"); the Project
    // control / Alt+P updates it for next time.
    scope: lastOpenScope,
    selectedIds: new Set<number>(),
    showWorktrees: lastShowWorktrees,
    hideTrivial: lastHideTrivial,
    bulkInFlight: null,
  };
  // Set `dockMode` BEFORE the initial `filterSessions("")`. The sort
  // inside `filterSessions` keys off `pinCurrentFirst = !dockMode`: the
  // dock wants stable lex order, the modal picker wants current-first.
  // Doing the filter first (when `dockMode` is still its previous /
  // initial `false` value) made the dock's INITIAL render use current-
  // first ordering, while every subsequent `refreshOpenDialog`
  // (active_window_changed, window_created, …) used the stable lex
  // sort. Switching the active project then visibly reordered the
  // dock list — precisely what the dock comment forbids.
  openPanel = new FloatingWidgetPanel();
  if (asDock) {
    dockMode = true;
    dockBlurred = false;
  } else {
    dockMode = false;
  }
  openDialog.filteredIds = filterSessions("");
  const activeIdx = openDialog.filteredIds.indexOf(activeId);
  openDialog.selectedIndex = activeIdx >= 0 ? activeIdx : 0;
  if (asDock) {
    // Persistent, non-modal full-height left column. Mount, then
    // re-anchor to the dock (which sets the content-wrap width to the
    // dock columns) and re-render so the spec lays out at dock width.
    // Mount straight into the host's dedicated dock slot so it
    // coexists with a centered modal (the New-Session form) instead
    // of being replaced by it. `asDock` carves the left column and
    // wraps the content to the dock width.
    openPanel.mount(buildDockSpec(), {
      widthPct: 100,
      heightPct: 100,
      asDock: true,
    });
    editor.floatingPanelControl(openPanel.id(), "dock", dockDefaultWidth());
    openPanel.update(buildDockSpec());
  } else {
    // 90% × 90% of the terminal — the open dialog wants room for
    // a real session list + preview pane, unlike the new-session
    // form which stays compact.
    openPanel.mount(buildOpenSpec(), { widthPct: 90, heightPct: 90 });
  }
  if (openDialog.filteredIds.length > 0) {
    openPanel.setSelectedIndex("sessions", openDialog.selectedIndex);
  }
  // Visit is the dialog's primary action — land focus there on
  // mount so Enter immediately opens the selected session. The
  // tabbable order is unchanged (new-session → filter → preview-
  // pane buttons); we just override the default-first-tabbable
  // selection. The host clamps to the first tabbable when "visit"
  // isn't in the spec (empty filter result, no session), which is
  // safe — there's nothing to act on then anyway.
  // In the dock the focusable session list is the default focus
  // (↑↓ switch, Enter blurs to editor). The modal lands on Visit.
  const initialFocus = asDock ? "sessions" : "visit";
  openPanel.setFocusKey(initialFocus);
  // Seed the `pickerFocusKey` mirror — `setFocusKey` only fires the
  // `focus` widget_event when the inner key actually *changes*, so on
  // a fresh mount it may not fire (no previous focus to differ from).
  pickerFocusKey = initialFocus;
  if (asDock) {
    // The dock has no editor mode — its keys are handled at the host
    // floating-panel layer (mode bindings would be shadowed by the
    // active session's buffer mode).
    dockFocus = "list";
    editor.setEditorMode(null);
  } else {
    editor.setEditorMode(OPEN_MODE);
  }

  // Discover worktrees that exist on disk but aren't open yet and
  // fold them into the list. Async (it shells out to git per
  // project); the dialog renders immediately with live sessions and
  // gains the discovered rows when the scan lands.
  void refreshDiscoveredWorktrees();
}

function closeOpenDialog(): void {
  if (openPanel) {
    openPanel.unmount();
    openPanel = null;
  }
  openDialog = null;
  dockMode = false;
  dockBlurred = false;
  editor.setEditorMode(null);
}

// ---------------------------------------------------------------------
// Global left dock
//
// The dock reuses the open-dialog state/panel but is mounted as a
// full-height, non-modal left column (host `floatingPanelControl`
// "dock"). It renders a single-column session list (the modal's
// two-pane picker would be unreadable at dock width). Navigating the
// list switches the active window live (debounced), so the editor to
// the dock's right *is* the preview.
// ---------------------------------------------------------------------

// Single-line compact detail for the selected session, pinned just
// above the action row. One line keeps the list-fill maths exact.
function dockDetailLine(s: AgentSession | undefined): WidgetSpec | null {
  if (!s || s.discovered) return null;
  return {
    kind: "raw",
    entries: [
      styledRow([
        { text: "▸ ", style: { fg: "ui.help_key_fg" } },
        { text: s.branch || "(detached)", style: { fg: "ui.menu_disabled_fg" } },
      ]),
    ],
  };
}

// Action buttons for the selected live session — same keys as the
// modal's preview pane so the existing widget_event handlers fire.
function dockActionRow(s: AgentSession | undefined): WidgetSpec | null {
  if (!s || s.discovered) return null;
  const hasWorktree = ownsWorktree(s);
  const isLastWindow = s.id > 0 && liveWindowCount() <= 1;
  return row(
    button("Stop", { key: "stop", disabled: !s.terminalId }),
    spacer(1),
    button("Arch", { key: "archive", disabled: !hasWorktree || isLastWindow }),
    spacer(1),
    button("Del", { intent: "danger", key: "delete", disabled: isLastWindow }),
    flexSpacer(),
  );
}

// Compact in-place confirmation for the dock (Delete is the only
// action that confirms). Reuses `confirm-cancel` / `confirm-<action>`
// keys so the modal's handlers run unchanged. Two lines.
function dockConfirmRows(
  confirm: { action: BulkAction; ids: number[] },
): WidgetSpec[] {
  const cap = confirm.action[0].toUpperCase() + confirm.action.slice(1);
  const id = confirm.ids[0];
  const label = orchestratorSessions.get(id)?.label ?? `#${id}`;
  return [
    {
      kind: "raw",
      entries: [
        styledRow([
          {
            text: `${cap} ${label}?`,
            style: { fg: "ui.status_error_indicator_fg", bold: true },
          },
        ]),
      ],
    },
    row(
      button("Cancel", { key: "confirm-cancel" }),
      spacer(1),
      button(`${cap}`, { intent: "danger", key: `confirm-${confirm.action}` }),
      flexSpacer(),
    ),
  ];
}

// Compact single-column spec for the dock. Reuses the same `sessions`
// list key + filter/scope/action keys as the modal so the existing
// `widget_event` handlers fire unchanged. The session list fills the
// available height; the hint bar is pinned to the bottom by sizing the
// list to consume everything above the fixed bottom block.
function buildDockSpec(): WidgetSpec {
  if (!openDialog) return col();
  const filtered = openDialog.filteredIds;
  const activeId = editor.activeWindow();
  // Content width tracks the responsive dock width we request from the
  // host (`dockDefaultWidth`, re-issued on resize), bounded by what the
  // host can actually grant: it keeps EDITOR_MIN (~20) cols for the
  // buffer, so on a narrow terminal the real dock is `screenW - 20` and
  // below that it's hidden. Taking the min keeps name/tag truncation and
  // the `dockRule` in step with the visible width.
  const EDITOR_MIN_COLS = 20;
  const screenW = editor.getScreenSize().width;
  const grantable = screenW > 0 ? screenW - EDITOR_MIN_COLS : DOCK_WIDTH_COLS;
  const dockW = Math.min(dockDefaultWidth(), grantable);
  const contentW = dockContentCols(dockW);
  const items = filtered.map((id) => renderListItem(id, activeId, contentW));
  const itemKeys = filtered.map(String);
  const selIdx = filtered.length === 0
    ? -1
    : Math.max(0, Math.min(openDialog.selectedIndex, filtered.length - 1));
  const selected = selIdx >= 0 ? orchestratorSessions.get(filtered[selIdx]) : undefined;
  const confirm = openDialog.pendingConfirm;
  const inConfirm = confirm !== null;
  const scope = openDialog.scope;
  const newKey = editor.getKeybindingLabel(
    "orchestrator_open_new_from_picker",
    OPEN_MODE,
  );
  const newLabel = newKey ? `+ New ${newKey}` : "+ New";
  // Dock toggle labels stay terse — the dock is narrow, and the rebind
  // hints live in the wider modal picker. Just the plain noun here.
  const worktreeLabel = "all worktrees";
  // Checked = show trivial (empty / single-file) sessions; unchecked
  // (default) hides them so the dock focuses on real work. Same
  // `hide-trivial` widget key the modal uses, so the existing
  // `widget_event` toggle handler fires for the dock too.
  const trivialLabel = "show empty";

  // Pinned bottom block: a confirm prompt (separator + 2 rows) OR
  // detail + actions (separator + 0–2 rows), then the hint bar. The
  // list is sized to consume the height above so the hint stays glued
  // to the bottom.
  const hintRow = row(
    flexSpacer(),
    hintBar([
      { keys: "↑↓", label: "switch" },
      { keys: "Enter", label: "edit" },
      { keys: "Esc", label: "editor" },
    ]),
    flexSpacer(),
  );
  let bottom: WidgetSpec[];
  let bottomRows: number;
  if (inConfirm && confirm) {
    bottom = [sessionsSeparator(), ...dockConfirmRows(confirm), hintRow];
    bottomRows = 1 + 2 + 1;
  } else {
    const detail = dockDetailLine(selected);
    const actions = dockActionRow(selected);
    bottom = [];
    bottomRows = 1; // hint
    if (detail || actions) {
      bottom.push(sessionsSeparator());
      bottomRows += 1;
    }
    if (detail) {
      bottom.push(detail);
      bottomRows += 1;
    }
    if (actions) {
      bottom.push(actions);
      bottomRows += 1;
    }
    bottom.push(hintRow);
  }

  // Size the list to fill the dock. The dock draws only a right border
  // (no top/bottom), so its content area is the full terminal height.
  // Fixed top chrome is 6 rows (title, New/scope, worktrees toggle,
  // empty/1-file toggle, filter, rule).
  const screen = editor.getScreenSize();
  const innerH = Math.max(8, screen.height > 0 ? screen.height : 30);
  const listRows = Math.max(MIN_LIST_ROWS, innerH - 6 - bottomRows);
  openDialog.listVisibleRows = listRows;

  return col(
    {
      kind: "raw",
      entries: [
        styledRow([
          { text: "ORCHESTRATOR", style: { fg: "ui.popup_border_fg", bold: true } },
        ]),
      ],
    },
    row(
      button(newLabel, {
        intent: "primary",
        key: inConfirm ? undefined : "new-session",
      }),
      flexSpacer(),
      button(scope === "current" ? "this ▾" : "all ▾", {
        key: inConfirm ? undefined : "scope-toggle",
      }),
    ),
    row(
      toggle(openDialog.showWorktrees, worktreeLabel, {
        key: inConfirm ? undefined : "worktree-show",
      }),
      flexSpacer(),
    ),
    row(
      toggle(!openDialog.hideTrivial, trivialLabel, {
        key: inConfirm ? undefined : "hide-trivial",
      }),
      flexSpacer(),
    ),
    text({
      value: openDialog.filter.value,
      cursorByte: openDialog.filter.cursor,
      label: "Filter",
      placeholder: "/ to search",
      fullWidth: true,
      key: inConfirm ? undefined : "filter",
    }),
    dockRule(contentW),
    list({
      items,
      itemKeys,
      selectedIndex: selIdx,
      visibleRows: listRows,
      // Focusable in the dock (unlike the modal, where Up/Down forward
      // from the filter): the list itself is the default focus so
      // ↑↓ drive live-switch and Enter blurs to the editor.
      focusable: !inConfirm,
      key: inConfirm ? undefined : "sessions",
    }),
    ...bottom,
  );
}

// Commit the highlighted session as the active window after a short
// debounce, so holding ↑/↓ to traverse the list doesn't thrash through
// every session in between. `fromEdge` drives the directional wipe.
function scheduleDockSwitch(fromEdge: "top" | "bottom" | null): void {
  const token = ++dockSwitchToken;
  console.warn(`[dock-switch] scheduled token=${token} fromEdge=${fromEdge}`);
  void (async () => {
    await editor.delay(30);
    if (token !== dockSwitchToken) {
      console.warn(`[dock-switch] token=${token} superseded by ${dockSwitchToken}`);
      return;
    }
    if (!openDialog || !openPanel || !dockMode || dockBlurred) {
      console.warn(
        `[dock-switch] token=${token} skipped: openDialog=${!!openDialog} openPanel=${!!openPanel} dockMode=${dockMode} dockBlurred=${dockBlurred}`,
      );
      return;
    }
    const id = openDialog.filteredIds[openDialog.selectedIndex];
    if (typeof id !== "number" || id <= 0) {
      console.warn(`[dock-switch] token=${token} no valid id: ${id}`);
      return;
    }
    if (orchestratorSessions.get(id)?.discovered) {
      console.warn(`[dock-switch] token=${token} id=${id} is discovered, skipping`);
      return;
    }
    if (id === editor.activeWindow()) {
      console.warn(`[dock-switch] token=${token} id=${id} already active`);
      return;
    }
    console.warn(`[dock-switch] token=${token} firing setActiveWindow(${id}) fromEdge=${fromEdge}`);
    if (fromEdge) editor.setActiveWindowAnimated(id, fromEdge);
    else editor.setActiveWindow(id);
  })();
}

// Toggle command (bind to a key of choice; reachable as
// "Orchestrator: Toggle Dock" in the command palette). Simple
// 2-state: visible → hide, hidden → show + focus. (A blurred-but-
// visible dock is re-focused by clicking it.) A 3-state toggle can't
// work reliably because invoking the toggle via a chord first blurs
// the focused dock — the toggle would then always see "blurred".
function toggleDock(): void {
  if (openPanel && dockMode) {
    closeOpenDialog();
    return;
  }
  // A centered modal picker is open — leave it alone.
  if (openPanel) return;
  openControlRoom({ dock: true });
}

registerHandler("orchestrator_dock_toggle", toggleDock);

// Stop every process one session owns. Sends SIGTERM first via the
// host's `signalWindow` (which fans out through the window's
// process-group tracker), then follows up with SIGKILL after a short
// grace period so ill-behaved agents that ignore SIGTERM still get
// reaped. The session record stays put — Stop only kills processes,
// it doesn't touch the worktree or the editor session. Returns false
// for ids it can't stop (base session, discovered worktrees with no
// live window).
function stopOne(id: number): boolean {
  const s = orchestratorSessions.get(id);
  if (!s || id <= 0 || s.discovered || !s.terminalId) return false;
  editor.signalWindow(id, "SIGTERM");
  // SIGKILL fallback for agents that ignore SIGTERM. The host's
  // signalWindow is idempotent on already-exited process groups, so
  // the second call is safe whether or not the first one took.
  // QuickJS has no `setTimeout`; `editor.delay(ms)` is the async
  // sleep primitive, which we kick off but don't await.
  void editor.delay(2000).then(() => {
    editor.signalWindow(id, "SIGKILL");
  });
  return true;
}

// ---------------------------------------------------------------------
// Archive manifest — `<XDG>/orchestrator/<repo-slug>/archived.json`.
// Records sessions that have been archived (stopped + worktree moved
// to `.archived/`). Used today by the Archive action; Unarchive and
// "Show archived" surface in a follow-up phase.
// ---------------------------------------------------------------------

interface ArchivedSession {
  label: string;
  /** Current path of the moved worktree, under `.archived/`. */
  root: string;
  /** Path the worktree lived at before archiving. */
  original_root: string;
  /** Branch the worktree was on. */
  branch: string;
  /** ISO 8601 timestamp of when the session was archived. */
  archived_at: string;
}

interface ArchiveManifest {
  version: number;
  sessions: ArchivedSession[];
}

function archiveManifestPath(repoRoot: string): string {
  return editor.pathJoin(
    editor.getDataDir(),
    "orchestrator",
    slugify(repoRoot),
    "archived.json",
  );
}

function loadArchiveManifest(repoRoot: string): ArchiveManifest {
  const path = archiveManifestPath(repoRoot);
  const raw = editor.readFile(path);
  if (!raw) return { version: 1, sessions: [] };
  try {
    const parsed = JSON.parse(raw);
    if (
      parsed && typeof parsed === "object" &&
      Array.isArray(parsed.sessions)
    ) {
      return parsed as ArchiveManifest;
    }
  } catch (_) {
    // Fall through to fresh manifest — bad data shouldn't
    // brick the dialog.
  }
  return { version: 1, sessions: [] };
}

function saveArchiveManifest(repoRoot: string, m: ArchiveManifest): boolean {
  const path = archiveManifestPath(repoRoot);
  const dir = editor.pathDirname(path);
  if (!editor.createDir(dir)) return false;
  return editor.writeFile(path, JSON.stringify(m, null, 2));
}

// Pick a session id to make active so that `excludeId` can be
// closed. `close_window` refuses to close the active window, so
// archive/delete of the currently-active session needs to switch
// away first. Prefers a session already visible in the open
// dialog's current filter (keeps the user in roughly the same
// project context they were browsing), falls back to the base
// session — which always exists and can't itself be archived /
// deleted, so this is guaranteed to return a valid target.
function pickNextActiveSession(excludeId: number): number {
  if (openDialog) {
    const inFilter = openDialog.filteredIds.find(
      (sid) => sid !== excludeId && sid > 0,
    );
    if (typeof inFilter === "number") return inFilter;
  }
  for (const sid of orchestratorSessions.keys()) {
    if (sid !== excludeId && sid > 0) return sid;
  }
  // No other live window. Callers guard against closing the last
  // window before reaching here, so this is a safe no-op swap (id 1
  // is no longer guaranteed to exist — it's deletable like any other).
  return excludeId;
}

// Number of real editor windows. Discovered on-disk rows have negative
// ids and are not windows. The editor must always host at least one
// window; archiving/deleting the last live window therefore opens a
// replacement first (see `ensureReplacementWindow`).
function liveWindowCount(): number {
  let n = 0;
  for (const s of orchestratorSessions.values()) {
    if (s.id > 0) n += 1;
  }
  return n;
}

// Closing the last live window would leave the editor with nothing to
// show, so before archiving/deleting the sole remaining session we open
// a fresh terminal session in `projectRoot` — the project that session
// belonged to, i.e. "the last project" — and dive into it. The new
// window becomes active, so the caller can then close the old one
// normally. No-op when another live window already exists (the caller
// just switches to it instead). Returns true when a replacement opened.
async function ensureReplacementWindow(projectRoot: string): Promise<boolean> {
  if (liveWindowCount() > 1) return false;
  const label = editor.pathBasename(projectRoot) || "session";
  try {
    const result = await editor.createWindowWithTerminal({
      root: projectRoot,
      label,
      cwd: projectRoot,
    });
    // `createWindowWithTerminal` fires `window_created`, which reconciles
    // the new window into the model; set it eagerly too so the immediate
    // close-and-switch below sees a second live window.
    orchestratorSessions.set(result.windowId, {
      id: result.windowId,
      label,
      root: projectRoot,
      projectPath: projectRoot,
      sharedWorktree: false,
      terminalId: result.terminalId,
      state: "idle",
      lastOutputAt: null,
      createdAt: Date.now(),
    });
    return true;
  } catch (e) {
    editor.setStatus(
      `Orchestrator: could not open a replacement session — ${
        e instanceof Error ? e.message : String(e)
      }`,
    );
    return false;
  }
}

// Resolve the *main* repo root a session's worktree belongs to, so
// `git worktree move/remove` runs from a stable directory (never from
// inside the tree being moved/removed). Prefers the canonical
// `projectPath` recorded at create/discovery time, falling back to
// resolving from the worktree itself.
async function worktreeRepoRoot(s: AgentSession): Promise<string | null> {
  // `projectPath` is the canonical repo root when the session is a
  // worktree of a separate project, and equals `root` otherwise (the
  // host normalises absence → root). Resolve once; if the canonical
  // path is unavailable (non-git, etc.), fall back to `root` so the
  // caller still gets something to dedupe against.
  const r = await resolveCanonicalRepoRoot(s.projectPath);
  if (r) return r;
  if (s.projectPath !== s.root) {
    return await resolveCanonicalRepoRoot(s.root);
  }
  return null;
}

interface LifecycleResult {
  ok: boolean;
  err?: string;
  repoRoot?: string;
}

// Archive a single session: SIGKILL its processes (archive is a
// "done with this for now" action — no graceful teardown needed since
// the worktree stays on disk), close the editor session, move the
// worktree to the `.archived/` graveyard, and append a manifest
// entry so Unarchive can reverse it. Handles both live sessions and
// discovered on-disk worktrees (the latter have no window to close).
// Does NOT trigger sync — the caller batches one sync per repo after
// the whole run.
async function archiveOne(id: number): Promise<LifecycleResult> {
  const s = orchestratorSessions.get(id);
  if (!s) return { ok: false, err: "session gone" };
  const removable = ownsWorktree(s);

  // Live session: the editor must always host a window. If this is the
  // only one, open a replacement in its project first; then switch away
  // (close_window refuses the active window), SIGKILL the process group
  // so pty children release any worktree locks, and close the session.
  if (!s.discovered && id > 0) {
    await ensureReplacementWindow(s.projectPath ?? s.root);
    if (id === editor.activeWindow()) {
      editor.setActiveWindow(pickNextActiveSession(id));
    }
    if (s.terminalId) editor.signalWindow(id, "SIGKILL");
    editor.closeWindow(id);
    // Brief settle so the filesystem reflects the pty's exit before we
    // move the worktree out from under it.
    if (removable) await editor.delay(250);
  }

  if (removable) {
    // Owns a worktree: move it to the `.archived/` graveyard so git's
    // bookkeeping stays consistent and Unarchive can move it back.
    const repoRoot = await worktreeRepoRoot(s);
    if (!repoRoot) return { ok: false, err: "not a git repository" };
    const archivedRoot = editor.pathJoin(
      editor.getDataDir(),
      "orchestrator",
      slugify(repoRoot),
      ".archived",
      s.label,
    );
    const parent = editor.pathDirname(archivedRoot);
    if (!editor.createDir(parent)) {
      return { ok: false, err: `could not create ${parent}`, repoRoot };
    }
    const moveRes = await spawnCollect(
      "git",
      ["-C", repoRoot, "worktree", "move", s.root, archivedRoot],
      repoRoot,
    );
    if (moveRes.exit_code !== 0) {
      return {
        ok: false,
        err: lastNonEmptyLine(moveRes.stderr) || "worktree move failed",
        repoRoot,
      };
    }
    const manifest = loadArchiveManifest(repoRoot);
    manifest.sessions.push({
      label: s.label,
      root: archivedRoot,
      original_root: s.root,
      branch: s.branch || s.label,
      archived_at: new Date().toISOString(),
    });
    saveArchiveManifest(repoRoot, manifest);
    // A discovered row has no window_closed hook to drop it — remove it
    // from the model directly.
    if (s.discovered) {
      orchestratorSessions.delete(id);
      discoveredIdByPath.delete(s.root);
    }
    return { ok: true, repoRoot };
  }

  // In-place / launch session: there's no separate worktree to move, so
  // archiving just records the session at its own root (original_root ===
  // root, no graveyard move) — listing it as archived and letting a
  // future Unarchive reopen a window there — then drops the live record.
  // The window was already closed above.
  const repoRoot = (await resolveCanonicalRepoRoot(s.root)) ?? s.root;
  const manifest = loadArchiveManifest(repoRoot);
  manifest.sessions.push({
    label: s.label,
    root: s.root,
    original_root: s.root,
    branch: s.branch || s.label,
    archived_at: new Date().toISOString(),
  });
  saveArchiveManifest(repoRoot, manifest);
  orchestratorSessions.delete(id);
  return { ok: true, repoRoot };
}

// ---------------------------------------------------------------------
// Cross-machine recovery (Phase 6)
//
// Every lifecycle action that mutates the local archive manifest also
// fires an asynchronous push to `refs/heads/<user>/fresh-sessions` on
// origin so the same sessions can be recovered on another machine.
// The push runs in the background and never blocks the user-visible
// action; failures get surfaced through `syncStatus` (and a small ⤒
// glyph in the dialog footer when the error is fresh).
//
// The branch is orphan-style: a single root file `sessions.json` and
// commits with the sessions snapshot. We maintain it through a
// dedicated worktree at `<XDG>/orchestrator/.sync-workspace` so we don't
// disturb the user's normal `git worktree` set.
// ---------------------------------------------------------------------

type SyncStatus = "idle" | "syncing" | "error";
let syncStatus: SyncStatus = "idle";
let syncError: string | null = null;

function deriveSyncUser(): string {
  // Priority order documented in
  // docs/internal/orchestrator-open-dialog-and-lifecycle.md.
  const envOverride = editor.getEnv("FRESH_SESSIONS_USER");
  if (envOverride && envOverride.trim()) return envOverride.trim();
  const localPart = (envEmailLocalPart() || "").trim();
  if (localPart) return localPart;
  const u = editor.getEnv("USER");
  if (u && u.trim()) return u.trim();
  return "fresh";
}

function envEmailLocalPart(): string | null {
  // Best-effort sync read of git config user.email's local-part.
  // Reading from env first (since spawnProcess is async) keeps
  // deriveSyncUser synchronous; users with no env override will
  // probably have `$USER` available as fallback.
  const email = editor.getEnv("GIT_AUTHOR_EMAIL") ||
    editor.getEnv("EMAIL");
  if (!email) return null;
  const at = email.indexOf("@");
  return at > 0 ? email.slice(0, at) : null;
}

function syncWorkspacePath(): string {
  return editor.pathJoin(editor.getDataDir(), "orchestrator", ".sync-workspace");
}

// Fire-and-forget sync. Never blocks the caller; updates
// `syncStatus`/`syncError` and refreshes the dialog (if open)
// so the footer indicator can reflect the result.
function triggerSyncAsync(repoRoot: string): void {
  void (async () => {
    syncStatus = "syncing";
    if (openPanel) refreshOpenDialog();
    const result = await syncSessions(repoRoot);
    if (result.ok) {
      syncStatus = "idle";
      syncError = null;
    } else {
      syncStatus = "error";
      syncError = result.err ?? "unknown error";
    }
    if (openPanel) refreshOpenDialog();
  })();
}

interface SyncResult {
  ok: boolean;
  err?: string;
}

async function syncSessions(repoRoot: string): Promise<SyncResult> {
  const user = deriveSyncUser();
  const branch = `${user}/fresh-sessions`;
  const wt = syncWorkspacePath();

  // Ensure the sync worktree exists and is on the right branch.
  // First-time setup creates the worktree as an orphan branch
  // with no parent commit (cleanest history; no leftover files
  // from the original tree).
  if (!editor.createDir(editor.pathDirname(wt))) {
    return { ok: false, err: "createDir failed for sync workspace parent" };
  }
  const branchExists = await spawnCollect(
    "git",
    ["-C", repoRoot, "show-ref", "--verify", "--quiet", `refs/heads/${branch}`],
    repoRoot,
  );
  const wtExists = await spawnCollect(
    "git",
    ["-C", repoRoot, "worktree", "list", "--porcelain"],
    repoRoot,
  );
  const wtAlreadyTracked = wtExists.exit_code === 0 &&
    wtExists.stdout.includes(wt);

  if (!wtAlreadyTracked) {
    if (branchExists.exit_code === 0) {
      const addRes = await spawnCollect(
        "git",
        ["-C", repoRoot, "worktree", "add", wt, branch],
        repoRoot,
      );
      if (addRes.exit_code !== 0) {
        return { ok: false, err: lastNonEmptyLine(addRes.stderr) };
      }
    } else {
      // Create an orphan worktree by adding detached then
      // switching to a new orphan branch.
      const addRes = await spawnCollect(
        "git",
        ["-C", repoRoot, "worktree", "add", "--detach", wt, "HEAD"],
        repoRoot,
      );
      if (addRes.exit_code !== 0) {
        return { ok: false, err: lastNonEmptyLine(addRes.stderr) };
      }
      const orphanRes = await spawnCollect(
        "git",
        ["-C", wt, "checkout", "--orphan", branch],
        wt,
      );
      if (orphanRes.exit_code !== 0) {
        return { ok: false, err: lastNonEmptyLine(orphanRes.stderr) };
      }
      // Strip everything inherited from HEAD's tree so the
      // orphan branch starts clean.
      await spawnCollect("git", ["-C", wt, "rm", "-rf", "."], wt);
    }
  }

  // Snapshot active + archived sessions into the JSON that
  // lives at the root of the sync branch.
  const snapshot = await buildSyncSnapshot(repoRoot);
  const sessionsPath = editor.pathJoin(wt, "sessions.json");
  if (!editor.writeFile(sessionsPath, JSON.stringify(snapshot, null, 2))) {
    return { ok: false, err: "writeFile sessions.json failed" };
  }

  const addRes = await spawnCollect(
    "git",
    ["-C", wt, "add", "sessions.json"],
    wt,
  );
  if (addRes.exit_code !== 0) {
    return { ok: false, err: lastNonEmptyLine(addRes.stderr) };
  }
  // The commit may noop when nothing changed — git exits with
  // 1 in that case, which we treat as success rather than an
  // error.
  const commitRes = await spawnCollect(
    "git",
    [
      "-C",
      wt,
      "commit",
      "--allow-empty-message",
      "-m",
      "Update sessions",
    ],
    wt,
  );
  if (commitRes.exit_code !== 0 && !commitRes.stdout.includes("nothing to commit")) {
    // Permissive: stderr "nothing to commit" / "working tree clean"
    // means there was nothing new to push. Skip the push and
    // report success.
    if (!commitRes.stderr.includes("nothing to commit")) {
      // Other commit failures: report.
      return { ok: false, err: lastNonEmptyLine(commitRes.stderr) };
    }
  }

  const pushRes = await spawnCollect(
    "git",
    ["-C", wt, "push", "origin", branch],
    wt,
  );
  if (pushRes.exit_code !== 0) {
    return { ok: false, err: lastNonEmptyLine(pushRes.stderr) };
  }
  return { ok: true };
}

async function buildSyncSnapshot(repoRoot: string): Promise<unknown> {
  const manifest = loadArchiveManifest(repoRoot);
  return {
    version: 1,
    machine_id: editor.getEnv("HOSTNAME") || "unknown",
    updated_at: new Date().toISOString(),
    active: Array.from(orchestratorSessions.values()).map((s) => ({
      label: s.label,
      branch: s.label,
      base_ref: "origin/master",
      created_at: new Date(s.createdAt).toISOString(),
    })),
    archived: manifest.sessions,
  };
}

// Delete a single session: close the editor session, then — only when
// the session owns a worktree — `git worktree remove --force` to drop
// it from disk (and prune any archive-manifest entry). A launch or
// in-place session owns no worktree, so Delete just forgets it: the
// window closes and the directory is left untouched (a fresh session
// can always be opened there again). Handles discovered on-disk
// worktrees (no window to close). Does NOT trigger sync — the caller
// batches it.
async function deleteOne(id: number): Promise<LifecycleResult> {
  const s = orchestratorSessions.get(id);
  if (!s) return { ok: false, err: "session gone" };
  const removable = ownsWorktree(s);

  if (!s.discovered && id > 0) {
    // The editor must keep at least one window. If this is the only live
    // one, open a replacement in its project first (so a removable
    // session can't `git worktree remove` the tree the editor is still
    // sitting in, and the editor never goes empty). Then swap away
    // (close_window refuses the active window), SIGKILL only when there's
    // an agent terminal — a launch/in-place session has none — and close.
    await ensureReplacementWindow(s.projectPath ?? s.root);
    if (id === editor.activeWindow()) {
      editor.setActiveWindow(pickNextActiveSession(id));
    }
    if (s.terminalId) editor.signalWindow(id, "SIGKILL");
    editor.closeWindow(id);
    if (removable) await editor.delay(250);
  }

  let repoRoot: string | undefined;
  if (removable) {
    const rr = await worktreeRepoRoot(s);
    if (!rr) return { ok: false, err: "not a git repository" };
    repoRoot = rr;
    // `--force` because the worktree may have unstaged changes the user
    // explicitly chose to discard via the confirm step.
    const removeRes = await spawnCollect(
      "git",
      ["-C", rr, "worktree", "remove", "--force", s.root],
      rr,
    );
    if (removeRes.exit_code !== 0) {
      return {
        ok: false,
        err: lastNonEmptyLine(removeRes.stderr) || "worktree remove failed",
        repoRoot,
      };
    }

    // Drop the matching manifest entry too, in case the session was
    // already archived (delete-from-archived drops dormant sessions).
    const manifest = loadArchiveManifest(rr);
    const before = manifest.sessions.length;
    manifest.sessions = manifest.sessions.filter((e) => e.label !== s.label);
    if (manifest.sessions.length !== before) {
      saveArchiveManifest(rr, manifest);
    }
  }

  if (s.discovered) {
    orchestratorSessions.delete(id);
    discoveredIdByPath.delete(s.root);
  } else if (id > 0) {
    // Drop the live record explicitly. `close_window` fires
    // `window_closed` → reconcile, which also prunes it, but an in-place
    // / launch session left the directory untouched, so without this it
    // could linger in the model and "come back" when the dialog reopens.
    orchestratorSessions.delete(id);
  }
  return { ok: true, repoRoot };
}

// Unified runner for a confirmed Stop / Archive / Delete over one or
// many ids. Re-filters to eligible targets at execution time (the
// selection or single row may have gone stale between confirm and
// run), drives the in-flight progress markers, runs the per-id cores
// sequentially, prunes acted-on ids from the selection, and triggers
// one sync per touched repo at the end.
async function runConfirmedAction(
  action: BulkAction,
  ids: number[],
): Promise<void> {
  if (!openDialog) return;
  const targets = ids.filter((id) => bulkEligible(action, id));
  if (targets.length === 0) {
    setDialogError(`nothing eligible to ${action} in the selection`);
    refreshOpenDialog();
    return;
  }

  if (action === "stop") {
    let n = 0;
    for (const id of targets) if (stopOne(id)) n += 1;
    editor.setStatus(`Orchestrator: stop signal sent to ${n} session(s)`);
    // Stop leaves sessions in place; drop them from the selection so
    // the bulk bar reflects that the action ran.
    for (const id of targets) openDialog.selectedIds.delete(id);
    refreshOpenDialog();
    return;
  }

  const single = targets.length === 1;
  if (single) {
    openDialog.inFlight = { action, sessionId: targets[0] };
  } else {
    openDialog.bulkInFlight = { action, total: targets.length, done: 0 };
  }
  refreshOpenDialog();

  const touchedRepos = new Set<string>();
  let okCount = 0;
  let lastErr = "";
  for (let i = 0; i < targets.length; i++) {
    const id = targets[i];
    const res = action === "archive" ? await archiveOne(id) : await deleteOne(id);
    if (res.ok) {
      okCount += 1;
      if (res.repoRoot) touchedRepos.add(res.repoRoot);
    } else {
      lastErr = res.err ?? "failed";
    }
    openDialog?.selectedIds.delete(id);
    if (openDialog?.bulkInFlight) openDialog.bulkInFlight.done = i + 1;
    refreshOpenDialog();
  }
  if (openDialog) {
    openDialog.inFlight = null;
    openDialog.bulkInFlight = null;
  }

  const verb = action === "archive" ? "archived" : "deleted";
  if (okCount === 0) {
    setDialogError(`${action} failed: ${lastErr || "unknown error"}`);
  } else if (lastErr) {
    setDialogError(`${verb} ${okCount}/${targets.length}; last error: ${lastErr}`);
  } else {
    editor.setStatus(`Orchestrator: ${verb} ${okCount} session(s)`);
  }
  for (const repo of touchedRepos) triggerSyncAsync(repo);
  refreshOpenDialog();
  // The batch emptied the selection, so the pane is back in
  // single-preview mode — restore focus to Visit (the bulk buttons
  // it may have been on are gone).
  if (openPanel && selectedSessions().length < 2 && !openDialog.pendingConfirm) {
    openPanel.setFocusKey("visit");
  }
}

// `Alt+N` from inside the picker opens the new-session form — saves
// the user the "Esc, Ctrl+P, type Orchestrator: New Session, Enter"
// dance when they realise mid-picker that they want to spawn another
// agent. All other keys (Up/Down/Enter/Tab/Esc/printable chars)
// route through `dispatch_floating_widget_key`'s smart-key defaults
// since OPEN_MODE doesn't claim them here.
editor.defineMode(
  OPEN_MODE,
  [
    ["M-n", "orchestrator_open_new_from_picker"],
    // Scope toggle: flip the list between "current project only"
    // and "all projects". Registered as a mode chord so it's
    // user-rebindable and renders cross-platform (⌥P / Alt+P).
    ["M-p", "orchestrator_toggle_scope"],
    // `/` jumps focus to the filter input — the familiar
    // search-focus shortcut. (As a mode chord it's intercepted even
    // while the filter has focus, so `/` can't be typed as filter
    // text; session names don't contain `/`, so that's an
    // acceptable trade for the quick-focus.)
    ["/", "orchestrator_focus_filter"],
    // Space toggles the highlighted row's membership in the bulk
    // selection. Bound as a mode chord (not a widget smart-key) so
    // it's user-rebindable in the keybinding editor and fires
    // regardless of which control holds focus — the host's
    // `dispatch_floating_widget_key` defers any explicitly-bound
    // mode key, including bare chars, before the text-input path.
    // The trade (same as `/`) is that Space can't be typed into the
    // filter while the picker is open; session names don't contain
    // spaces, so that's acceptable.
    ["Space", "orchestrator_toggle_select"],
    // Alt+T toggles "Show all worktrees" — the opt-in filter that
    // surfaces discovered on-disk worktree rows. Rebindable, same as
    // the scope toggle.
    ["M-t", "orchestrator_toggle_worktrees"],
    // Alt+I toggles "Show empty/1-file sessions" — reveals the trivial
    // restored shells hidden by default. Rebindable, same as the others.
    // (Alt+E is unavailable: it's the Edit menu's mnemonic, which the
    // menu bar claims before the picker's mode keymap sees it.)
    ["M-i", "orchestrator_toggle_trivial"],
  ],
  true,
  true,
);

// The dock's Enter / Esc / Space / "/" are handled at the host's
// floating-panel layer (see dispatch_floating_widget_key), not via an
// editor mode — `defineMode` bindings resolve against the active
// buffer's mode, which the dock floats over, so a session with a
// buffer-local mode would shadow them. Up/Down use the host's generic
// list smart-keys, which fire the `select` event we live-switch on.

registerHandler("orchestrator_open_new_from_picker", () => {
  if (!openDialog) return;
  // The New-Session form is a centered modal in the host's dedicated
  // floating slot, which coexists with the dock's own slot. From the
  // dock, leave the dock mounted underneath (it keeps showing the live
  // session list); from the centered picker, replace it with the form.
  if (dockMode) {
    // Hand keyboard focus to the form by blurring the dock; the host
    // routes keys to the focused centered modal first.
    dockBlurred = true;
    openForm({ fromPicker: true });
    return;
  }
  closeOpenDialog();
  openForm({ fromPicker: true });
});

registerHandler("orchestrator_focus_filter", () => {
  if (!openDialog || !openPanel) return;
  openPanel.setFocusKey("filter");
  if (dockMode) dockFocus = "filter";
});

// Space (rebindable): toggle the highlighted row in/out of the bulk
// selection. Manages focus across the single↔bulk transition: when
// the second row is checked the preview pane swaps to the bulk bar
// (so the now-absent "visit" focus would otherwise be clamped to a
// random tabbable), and when the selection drops back below two the
// per-session preview — with its "visit" button — returns.
function toggleSelectCurrent(): void {
  if (!openDialog || !openPanel) return;
  // Inert while a confirm prompt is up — the selection is frozen
  // behind the confirmation panel.
  if (openDialog.pendingConfirm) return;
  // Context-sensitive Space dispatch. OPEN_MODE binds Space to
  // `orchestrator_toggle_select` *unconditionally* — it must, to keep
  // Space out of the filter text input (the host's
  // dispatch_floating_widget_key defers any explicitly-bound mode key
  // before the text-input path). We branch on the focused widget so
  // Space on the filter checkboxes / scope chip toggles *that*
  // control rather than the list multi-select. Other focused widgets
  // (sessions list, Visit button, +New, the filter input itself) fall
  // through to the list multi-select — preserving today's behaviour
  // for widgets that don't expose a natural toggle.
  switch (pickerFocusKey) {
    case "worktree-show":
      toggleShowWorktrees();
      return;
    case "hide-trivial":
      toggleHideTrivial();
      return;
    case "scope-toggle":
      toggleScope();
      return;
  }
  const id = openDialog.filteredIds[openDialog.selectedIndex];
  if (typeof id !== "number") return;
  const wasBulk = selectedSessions().length >= 2;
  if (openDialog.selectedIds.has(id)) {
    openDialog.selectedIds.delete(id);
  } else {
    openDialog.selectedIds.add(id);
  }
  clearDialogError();
  refreshOpenDialog();
  // The dock has no bulk preview pane / Visit button; just toggle the
  // checkbox and keep focus on the list.
  if (dockMode) {
    openPanel.setSelectedIndex("sessions", openDialog.selectedIndex);
    return;
  }
  const isBulk = selectedSessions().length >= 2;
  if (!wasBulk && isBulk) {
    // Entering bulk mode — land focus on a bulk button (Up/Down from
    // a button still drives the list, so navigation keeps working).
    openPanel.setFocusKey("bulk-archive");
  } else if (wasBulk && !isBulk) {
    // Back to single preview — restore focus to Visit.
    openPanel.setFocusKey("visit");
  }
}
registerHandler("orchestrator_toggle_select", toggleSelectCurrent);

function toggleScope(): void {
  if (!openDialog) return;
  openDialog.scope = openDialog.scope === "current" ? "all" : "current";
  // Remember the choice for the next time the picker opens.
  lastOpenScope = openDialog.scope;
  // Keep the highlighted session selected across the scope flip
  // when it survives into the new list; otherwise fall back to the
  // top. The filter value is untouched — toggling scope with an
  // active filter just widens/narrows the global-search base.
  const prevId = openDialog.filteredIds[openDialog.selectedIndex];
  openDialog.filteredIds = filterSessions(openDialog.filter.value);
  const nextIdx = prevId !== undefined ? openDialog.filteredIds.indexOf(prevId) : -1;
  openDialog.selectedIndex = nextIdx >= 0 ? nextIdx : 0;
  refreshOpenDialog();
}

registerHandler("orchestrator_toggle_scope", toggleScope);

// Flip "Show all worktrees" — reveal/hide the discovered on-disk
// worktree rows. Preserves the highlighted row across the re-filter
// where possible; drops now-hidden discovered rows from the bulk
// selection. Shared by the Alt+T chord and the checkbox click.
function toggleShowWorktrees(): void {
  if (!openDialog) return;
  openDialog.showWorktrees = !openDialog.showWorktrees;
  lastShowWorktrees = openDialog.showWorktrees;
  // Hiding worktrees shouldn't leave them lingering in the selection.
  if (!openDialog.showWorktrees) {
    for (const id of [...openDialog.selectedIds]) {
      if (orchestratorSessions.get(id)?.discovered) {
        openDialog.selectedIds.delete(id);
      }
    }
  }
  const prevId = openDialog.filteredIds[openDialog.selectedIndex];
  openDialog.filteredIds = filterSessions(openDialog.filter.value);
  const nextIdx = prevId !== undefined ? openDialog.filteredIds.indexOf(prevId) : -1;
  openDialog.selectedIndex = nextIdx >= 0 ? nextIdx : 0;
  refreshOpenDialog();
}

registerHandler("orchestrator_toggle_worktrees", toggleShowWorktrees);

// Flip "Show empty/1-file sessions" — reveal/hide the trivial restored
// shells. Preserves the highlighted row across the re-filter where
// possible; drops now-hidden rows from the bulk selection. Shared by the
// Alt+I chord and the checkbox click.
function toggleHideTrivial(): void {
  if (!openDialog) return;
  openDialog.hideTrivial = !openDialog.hideTrivial;
  lastHideTrivial = openDialog.hideTrivial;
  const prevId = openDialog.filteredIds[openDialog.selectedIndex];
  openDialog.filteredIds = filterSessions(openDialog.filter.value);
  // Hiding trivial rows shouldn't leave them lingering in the selection.
  if (openDialog.hideTrivial) {
    const visible = new Set(openDialog.filteredIds);
    for (const id of [...openDialog.selectedIds]) {
      if (!visible.has(id)) openDialog.selectedIds.delete(id);
    }
  }
  const nextIdx = prevId !== undefined ? openDialog.filteredIds.indexOf(prevId) : -1;
  openDialog.selectedIndex = nextIdx >= 0 ? nextIdx : 0;
  refreshOpenDialog();
}

registerHandler("orchestrator_toggle_trivial", toggleHideTrivial);

// =============================================================================
// New-session floating form
// =============================================================================

function slugify(p: string): string {
  // Drop any leading separator so the slug isn't anchored to the
  // filesystem root; replace remaining separators with underscores.
  return p.replace(/^[\\\/]+/, "").replace(/[\\\/]+/g, "_");
}

// =============================================================================
// Input history (Up / Down) for the new-session form
//
// Per-field MRU lists keyed under `orchestrator.history.<field>` in
// the editor's global plugin-state store (persisted across editor
// restarts). Submit appends the resolved value to each field's
// history; Up/Down on a focused input walks the list (saving the
// user's in-progress draft on the first ↑ so ↓ can return to it).
// Capped at 100 entries per field, MRU-trimmed.
// =============================================================================

type HistoryField = "project_path" | "name" | "cmd" | "branch";
const HISTORY_FIELDS: HistoryField[] = ["project_path", "name", "cmd", "branch"];
const HISTORY_CAP = 100;

/// Plugin-side focus tracker for the new-session form. The host
/// owns the actual focus key, but doesn't expose a "what's
/// focused right now?" query to plugins, and doesn't fire focus-
/// change events. So we mirror the cycle ourselves: openForm
/// resets to the first tabbable, Tab / S-Tab advance / retreat,
/// `change` events on a known widget snap focus to that widget
/// (covers mouse clicks too).
///
/// The mirror is "best-effort" — it can drift if the host
/// reorders focus in ways we don't intercept (e.g. an explicit
/// `focusAdvance` action we issued ourselves), but for the
/// keys this form actually binds it stays in sync.
let formFocusCycle: string[] = [];
let formFocusIndex = 0;

function rebuildFormFocusCycle(): void {
  if (!form) {
    formFocusCycle = [];
    formFocusIndex = 0;
    return;
  }
  const worktreeEnabled = form.projectPathIsGit !== false;
  const branchInert = !(worktreeEnabled && form.createWorktree);
  const cycle: string[] = ["project_path"];
  if (worktreeEnabled) cycle.push("worktree");
  cycle.push("name", "cmd");
  if (!branchInert) cycle.push("branch");
  cycle.push("cancel", "create");
  formFocusCycle = cycle;
  if (formFocusIndex >= cycle.length) formFocusIndex = 0;
}

function formFocusedKey(): string {
  return formFocusCycle[formFocusIndex] ?? "";
}

function advanceFormFocus(delta: 1 | -1): void {
  if (formFocusCycle.length === 0) return;
  formFocusIndex =
    (formFocusIndex + delta + formFocusCycle.length) % formFocusCycle.length;
}

function snapFormFocusTo(key: string): void {
  const idx = formFocusCycle.indexOf(key);
  if (idx >= 0) formFocusIndex = idx;
}

function historyKey(field: HistoryField): string {
  return `orchestrator.history.${field}`;
}

function readHistory(field: HistoryField): string[] {
  const raw = editor.getGlobalState(historyKey(field));
  if (Array.isArray(raw)) {
    return raw.filter((v): v is string => typeof v === "string");
  }
  return [];
}

function writeHistory(field: HistoryField, items: string[]): void {
  editor.setGlobalState(historyKey(field), items as unknown as object);
}

function appendHistory(field: HistoryField, value: string): void {
  const v = (value || "").trim();
  if (!v) return;
  const prev = readHistory(field).filter((x) => x !== v);
  prev.unshift(v);
  if (prev.length > HISTORY_CAP) prev.length = HISTORY_CAP;
  writeHistory(field, prev);
}

/// Map a focused widget key to its history field, or null if the
/// key isn't a history-bearing input.
function focusToHistoryField(focusKey: string): HistoryField | null {
  return (HISTORY_FIELDS as readonly string[]).includes(focusKey)
    ? (focusKey as HistoryField)
    : null;
}

/// Walk the history of `field` by `delta` (-1 = older / ↑, +1 =
/// newer / ↓). Updates the form's value, cursor, and history
/// cursor in place. No-op when the history is empty (or when ↓
/// is hit past the bottom of the stack).
function walkHistory(field: HistoryField, delta: -1 | 1): void {
  if (!form) return;
  const history = readHistory(field);
  if (history.length === 0) return;
  const slot = formSlot(field);
  if (!slot) return;

  const curr = form.historyCursor[field];
  let next = curr + delta; // -1 → 0 for first ↑

  if (next < -1) {
    // Already at the draft slot, ↓ does nothing more.
    return;
  }
  if (next >= history.length) {
    // Past the oldest entry — stay put.
    return;
  }

  if (curr === -1 && delta === -1) {
    // First ↑: save the in-progress draft so the user can ↓
    // back to whatever they were typing.
    form.historyDraft[field] = slot.value;
  }

  if (next === -1) {
    // ↓ off the top of the stack → restore the saved draft.
    slot.value = form.historyDraft[field];
  } else {
    slot.value = history[next];
  }
  slot.cursor = slot.value.length;
  form.historyCursor[field] = next;

  // Sync the rendered widget so cursor + value match (the host
  // tracks text input state separately from the spec).
  if (formPanel) {
    formPanel.setValue(field, slot.value, slot.cursor);
  }
  // Re-probe defaults if the user just rolled history into the
  // Project Path field.
  if (field === "project_path") scheduleProjectPathReprobe();
  renderForm();
}

function formSlot(field: HistoryField): { value: string; cursor: number } | null {
  if (!form) return null;
  switch (field) {
    case "project_path": return form.projectPath;
    case "name": return form.name;
    case "cmd": return form.cmd;
    case "branch": return form.branch;
  }
}

function lastNonEmptyLine(s: string): string {
  const lines = (s || "").split(/\r?\n/).filter((l) => l.trim().length > 0);
  return lines.length ? lines[lines.length - 1].trim() : "";
}

/// Split the user's "Agent Command" string into an argv suitable for
/// `editor.createTerminal({ command })`. Honours single- and
/// double-quoted segments so `claude --append "hello world"` parses
/// as three args rather than four. Backslash escaping is intentionally
/// *not* supported — agent commands are short typed-in strings; if
/// they need that level of escaping the user should write a wrapper
/// shell script.
///
/// Returns `[]` for an empty or whitespace-only input.
function splitAgentCmd(s: string): string[] {
  const out: string[] = [];
  let cur = "";
  let quote: '"' | "'" | null = null;
  for (let i = 0; i < s.length; i++) {
    const c = s[i];
    if (quote) {
      if (c === quote) {
        quote = null;
      } else {
        cur += c;
      }
      continue;
    }
    if (c === '"' || c === "'") {
      quote = c;
      continue;
    }
    if (c === " " || c === "\t") {
      if (cur.length > 0) {
        out.push(cur);
        cur = "";
      }
      continue;
    }
    cur += c;
  }
  if (cur.length > 0) out.push(cur);
  return out;
}

async function spawnCollect(
  command: string,
  args: string[],
  cwd: string,
): Promise<SpawnResult> {
  return await editor.spawnProcess(command, args, cwd);
}

/// Resolve the origin's default branch as `"origin/<name>"` from
/// the locally-cached symbolic-ref. Returns `"HEAD"` when there's
/// no `origin` remote (purely-local repos) or the symbolic ref is
/// missing — the caller treats that as the silent fallback.
///
/// Deliberately does NOT fetch: `refs/remotes/origin/HEAD` is set
/// at clone time and only changes when the remote renames its
/// default branch (rare). A network round-trip per dialog open
/// is too high a cost for that case.
async function detectDefaultBranch(repoRoot: string): Promise<string> {
  return (await detectDefaultBranchWithFallback(repoRoot)).ref;
}

/// Like `detectDefaultBranch` but also reports whether we had to
/// fall back to bare `HEAD` because no `origin` is configured. The
/// caller uses that to surface a context note in the placeholder
/// ("HEAD  (no origin configured)") so the user isn't confused
/// about why their repo's default isn't being detected.
async function detectDefaultBranchWithFallback(
  repoRoot: string,
): Promise<{ ref: string; isHeadFallback: boolean }> {
  const res = await spawnCollect(
    "git",
    ["-C", repoRoot, "symbolic-ref", "refs/remotes/origin/HEAD"],
    repoRoot,
  );
  if (res.exit_code === 0) {
    const trimmed = (res.stdout || "").trim();
    const prefix = "refs/remotes/";
    if (trimmed.startsWith(prefix)) {
      // e.g. "refs/remotes/origin/main" → "origin/main". This is
      // what the new worktree is forked off, so the user sees the
      // exact ref name they'd otherwise have to type by hand.
      return { ref: trimmed.slice(prefix.length), isHeadFallback: false };
    }
  }
  return { ref: "HEAD", isHeadFallback: true };
}

/// Resolve a directory to the *main* worktree's root if it's
/// inside a git working tree. Returns `null` for non-git paths
/// so the caller can pick the no-git path explicitly.
async function resolveCanonicalRepoRoot(
  cwd: string,
): Promise<string | null> {
  const top = await spawnCollect(
    "git",
    ["rev-parse", "--show-toplevel"],
    cwd,
  );
  if (top.exit_code !== 0) return null;
  const toplevel = (top.stdout || "").trim();
  if (!toplevel) return null;
  // `--git-common-dir` returns the shared `.git` dir even when
  // we're inside a linked worktree. `dirname(...)` gives the
  // main worktree's root, which is what we want as the
  // canonical project identifier.
  const common = await spawnCollect(
    "git",
    ["rev-parse", "--path-format=absolute", "--git-common-dir"],
    toplevel,
  );
  if (common.exit_code === 0) {
    const parent = editor.pathDirname((common.stdout || "").trim());
    if (parent) return parent;
  }
  return toplevel;
}

/// Is `path` inside a git working tree? Returns `null` on any
/// error so the caller can keep its UI in a "in-flight / unknown"
/// state rather than flipping to a wrong answer.
async function pathIsInsideGitWorkTree(
  path: string,
): Promise<boolean | null> {
  if (!path) return null;
  const res = await spawnCollect(
    "git",
    ["-C", path, "rev-parse", "--is-inside-work-tree"],
    path,
  );
  if (res.exit_code !== 0) return false; // non-zero = not a repo
  return (res.stdout || "").trim() === "true";
}

// =============================================================================
// Worktree classification & discovery
//
// Two distinct git facts drive the "attach to an existing worktree"
// flows:
//
//   * `classifyWorktree(path)` answers "is this path a *linked*
//     worktree, and if so what repo does it belong to?" — used by
//     the new-session form to attach (rather than fork) when the
//     user points Project Path at an existing worktree.
//   * `listLinkedWorktrees(repoRoot)` enumerates every linked
//     worktree of a repo (via `git worktree list --porcelain`) —
//     used to surface on-disk worktrees in the Open dialog without
//     the user adding them by hand.
// =============================================================================

interface WorktreeInfo {
  // `git rev-parse --show-toplevel` for the path.
  toplevel: string;
  // Canonical main-worktree root (dirname of `--git-common-dir`).
  // This is the repo the worktree belongs to, used as the
  // session's `projectPath` so attached worktrees group under
  // their repo in the picker.
  mainRoot: string;
  // `true` when the path is a *linked* worktree (its per-worktree
  // git dir differs from the shared common dir), i.e. a tree
  // created by `git worktree add` rather than the main checkout.
  isLinked: boolean;
  // Branch checked out there (`refs/heads/<name>` short form), or
  // empty when detached.
  branch: string;
}

/// Classify `path` as a git worktree. Returns `null` when `path`
/// is not inside any git work tree (the caller then treats it as a
/// plain directory / shared root).
async function classifyWorktree(path: string): Promise<WorktreeInfo | null> {
  if (!path) return null;
  const top = await spawnCollect("git", ["-C", path, "rev-parse", "--show-toplevel"], path);
  if (top.exit_code !== 0) return null;
  const toplevel = (top.stdout || "").trim();
  if (!toplevel) return null;

  // The per-worktree git dir vs. the shared common dir: they are
  // equal for the main worktree and differ for every linked
  // worktree (`<common>/worktrees/<id>`). That difference is the
  // canonical "is this a linked worktree?" test.
  const [gitDir, commonDir] = await Promise.all([
    spawnCollect("git", ["-C", toplevel, "rev-parse", "--path-format=absolute", "--git-dir"], toplevel),
    spawnCollect(
      "git",
      ["-C", toplevel, "rev-parse", "--path-format=absolute", "--git-common-dir"],
      toplevel,
    ),
  ]);
  const gd = gitDir.exit_code === 0 ? (gitDir.stdout || "").trim() : "";
  const cd = commonDir.exit_code === 0 ? (commonDir.stdout || "").trim() : "";
  const isLinked = gd !== "" && cd !== "" && gd !== cd;
  const mainRoot = cd ? editor.pathDirname(cd) : toplevel;

  const head = await spawnCollect(
    "git",
    ["-C", toplevel, "rev-parse", "--abbrev-ref", "HEAD"],
    toplevel,
  );
  let branch = head.exit_code === 0 ? (head.stdout || "").trim() : "";
  if (branch === "HEAD") branch = ""; // detached

  return { toplevel, mainRoot, isLinked, branch };
}

interface ParsedWorktree {
  path: string;
  branch: string;
  detached: boolean;
}

/// Parse `git worktree list --porcelain` output. Blocks are
/// separated by blank lines; the first block is the main worktree,
/// the rest are linked. Each block has a `worktree <path>` line
/// plus `branch refs/heads/<name>` or `detached`.
function parseWorktreePorcelain(stdout: string): ParsedWorktree[] {
  const out: ParsedWorktree[] = [];
  let cur: ParsedWorktree | null = null;
  for (const raw of (stdout || "").split(/\r?\n/)) {
    const line = raw.trimEnd();
    if (line.startsWith("worktree ")) {
      if (cur) out.push(cur);
      cur = { path: line.slice("worktree ".length), branch: "", detached: false };
    } else if (cur && line.startsWith("branch ")) {
      const ref = line.slice("branch ".length);
      cur.branch = ref.replace(/^refs\/heads\//, "");
    } else if (cur && line === "detached") {
      cur.detached = true;
    } else if (line === "" && cur) {
      out.push(cur);
      cur = null;
    }
  }
  if (cur) out.push(cur);
  return out;
}

/// Enumerate the *linked* worktrees of `repoRoot` (excludes the
/// main worktree, which is the repo's own checkout). Returns the
/// parsed entries with the main-repo root resolved so callers can
/// tag discovered sessions with the right `projectPath`.
async function listLinkedWorktrees(
  repoRoot: string,
): Promise<{ mainRoot: string; worktrees: ParsedWorktree[] } | null> {
  const res = await spawnCollect(
    "git",
    ["-C", repoRoot, "worktree", "list", "--porcelain"],
    repoRoot,
  );
  if (res.exit_code !== 0) return null;
  const all = parseWorktreePorcelain(res.stdout || "");
  if (all.length === 0) return null;
  // The first entry is always the main worktree.
  const mainRoot = all[0].path;
  const worktrees = all.slice(1);
  return { mainRoot, worktrees };
}

/// Slugify a project basename into a git-ref-safe, label-friendly stem
/// for auto session names. git refs forbid spaces, `~^:?*[\` etc., so
/// collapse anything outside `[A-Za-z0-9._-]` to a dash; the name
/// doubles as the worktree branch.
function sessionNameBaseFor(repoRoot: string): string {
  const raw = editor.pathBasename(repoRoot) || "";
  const slug = raw.replace(/[^A-Za-z0-9._-]+/g, "-").replace(/^-+|-+$/g, "");
  return slug.length > 0 ? slug : "session";
}

async function nextAutoSessionName(
  repoRoot: string,
  options?: { persist?: boolean },
): Promise<string> {
  // Root the auto-name in the project (`<project>-1`, `<project>-2`, …)
  // rather than a bare `session-N`, so a dock row tells you which
  // project the session belongs to (F6). The name also seeds the
  // worktree branch.
  //
  // Persisted counter so consecutive empty submits keep incrementing
  // even across plugin reloads. But the counter alone isn't
  // sufficient: a previous run may have left a branch / worktree behind
  // (orchestrator's archive / external git delete / interrupted
  // submit), so `<project>-${counter+1}` can collide and
  // `git worktree add` would fail with the noisy "already used by
  // worktree at …" message. Probe the local git refs once and
  // increment past any reserved `<project>-N` name before returning.
  //
  // `persist: false` (the default) computes the name without
  // advancing the persisted counter — for placeholder previews
  // that happen on every Project Path keystroke. The submit
  // path passes `persist: true` so consecutive submissions
  // increment normally.
  const persist = options?.persist === true;
  const base = sessionNameBaseFor(repoRoot);
  const counterBefore = (editor.getGlobalState("orchestrator.session_counter") as
    | number
    | undefined) ?? 0;
  let next = counterBefore + 1;

  // Collect existing branch names that look like `<project>-N` so we
  // can skip past them. `git for-each-ref` is faster and tighter
  // than parsing `git worktree list` output. `.` is the only
  // regex-special char the slug can contain, so escape it.
  const refs = await spawnCollect(
    "git",
    ["-C", repoRoot, "for-each-ref", "--format=%(refname:short)", "refs/heads/"],
    repoRoot,
  );
  const taken = new Set<number>();
  if (refs.exit_code === 0) {
    const re = new RegExp(`^${base.replace(/[.]/g, "\\.")}-(\\d+)$`);
    for (const line of (refs.stdout || "").split(/\r?\n/)) {
      const m = re.exec(line.trim());
      if (m) {
        taken.add(parseInt(m[1], 10));
      }
    }
  }
  while (taken.has(next)) {
    next += 1;
  }
  if (persist) {
    editor.setGlobalState("orchestrator.session_counter", next);
  }
  return `${base}-${next}`;
}

// Three distinct styles for the header line: section keyword
// ("ORCHESTRATOR"), structural separators ("::"), and step label. The
// border-fg key picks up the same accent the floating panel border
// uses, so the title visually anchors to the dialog chrome.
const HEADER_KEYWORD_STYLE = {
  fg: "ui.popup_border_fg",
  bold: true,
} as const;
const HEADER_SEP_STYLE = { fg: "ui.menu_disabled_fg" } as const;
const HEADER_LABEL_STYLE = { fg: "ui.menu_active_fg", bold: true } as const;

// Subtitle splits the static prefix "Project:" from the project
// path so each gets its own foreground — matching the three-tier
// (label / label-value / input) palette the design calls for.
const SUBTITLE_LABEL_STYLE = { fg: "ui.menu_disabled_fg" } as const;
const SUBTITLE_VALUE_STYLE = { fg: "ui.help_key_fg", bold: true } as const;

function buildFormSpec(): WidgetSpec {
  if (!form) return col();

  // Worktree-toggle enable state. The checkbox is disabled
  // (rendered without a `key` so the host skips it in the tab
  // cycle, and the label gets a `(disabled — non-git)` suffix)
  // when the resolved Project Path is not inside a git working
  // tree. `null` (probe in flight) keeps it in its last-known
  // state — no flicker on rapid typing.
  const worktreeEnabled = form.projectPathIsGit !== false;
  const effectiveCreateWorktree = worktreeEnabled && form.createWorktree;
  const branchInert = !effectiveCreateWorktree;

  // Branch placeholder: surface origin/main, fall back to a
  // contextual hint when no origin is configured, and become
  // inert when worktree creation is off.
  let branchPlaceholder: string;
  if (branchInert) {
    branchPlaceholder = !worktreeEnabled
      ? "no git — N/A"
      : form.projectPathIsLinkedWorktree === true
      ? "existing worktree — N/A"
      : "shared worktree — N/A";
  } else if (!form.defaultBranch) {
    branchPlaceholder = "detecting default branch…";
  } else if (form.defaultBranchIsHeadFallback) {
    branchPlaceholder = "HEAD  (no origin configured)";
  } else {
    branchPlaceholder = form.defaultBranch;
  }

  const children: WidgetSpec[] = [
    // === Header: centered title (no stale `Review Synthesized`). =
    row(
      flexSpacer(),
      {
        kind: "raw",
        entries: [
          styledRow([
            { text: "ORCHESTRATOR", style: HEADER_KEYWORD_STYLE },
            { text: " :: ", style: HEADER_SEP_STYLE },
            { text: "New Session", style: HEADER_LABEL_STYLE },
          ]),
        ],
      },
      flexSpacer(),
    ),
    spacer(0),
    // === Project Path: the new top-of-form field. ================
    // Placeholder surfaces the resolved canonical repo root (or
    // editor cwd for non-git launches). Empty submit uses the
    // placeholder verbatim, so the user can land on a sensible
    // default just by pressing Enter through the form.
    // The completion popup hangs off the bottom of this Text
    // widget — host-rendered chrome, no separate widget. The
    // plugin pushes candidates via `formPanel.setCompletions`
    // and reacts to the `completion_accept` event when the user
    // hits Tab; the labeledSection wrapper extends its side
    // borders down through the popup automatically.
    labeledSection({
      label: "Project Path",
      child: text({
        value: form.projectPath.value,
        cursorByte: form.projectPath.cursor,
        placeholder: form.defaultProjectPath || "detecting project root…",
        fullWidth: true,
        key: "project_path",
      }),
    }),
    // === Worktree toggle. ========================================
    // Enabled only when the Project Path resolves to a git work
    // tree. When disabled, render with a dim-fg `raw` row using
    // the same `[ ] / [v]` glyph (so the user still recognises
    // it as a checkbox) and append a `(disabled — non-git)`
    // suffix. The raw row has no `key`, so it stays out of the
    // Tab cycle and Space-to-toggle has nothing to land on.
    worktreeEnabled
      ? toggle(
          effectiveCreateWorktree,
          "Create a new git worktree for this session",
          { key: "worktree" },
        )
      : {
          kind: "raw",
          entries: [
            styledRow([
              {
                text: "[ ] Create a new git worktree for this session",
                style: { fg: "editor.whitespace_indicator_fg" },
              },
              {
                text: "  (disabled — non-git)",
                style: { fg: "editor.whitespace_indicator_fg", italic: true },
              },
            ]),
          ],
        },
    // Existing-worktree hint: when Project Path points at a linked
    // worktree, explain what the (un)checked box now means so the
    // attach behaviour isn't a silent surprise.
    ...(form.projectPathIsLinkedWorktree === true
      ? [{
          kind: "raw" as const,
          entries: [
            styledRow([
              {
                text: form.createWorktree
                  ? "  ↳ existing worktree here — uncheck to attach instead of forking a new one"
                  : "  ↳ existing worktree — this session will attach to it",
                style: { fg: "ui.help_key_fg", italic: true },
              },
            ]),
          ],
        }]
      : []),
    // === Form body: labeled, full-width inputs. ==================
    // Labels are plain — the `▸` glyph used to be baked into all
    // three strings and stayed put regardless of focus, which was
    // misleading. The input's own focused-bg styling (set by the
    // host based on the panel's focus_key) is the authoritative
    // focus cue.
    labeledSection({
      label: "Session Name",
      child: text({
        value: form.name.value,
        cursorByte: form.name.cursor,
        // Concrete default (e.g. "session-3") rather than the
        // literal `(auto-generated)` — the user sees the exact
        // name an empty submit would create. Empty while the
        // ref probe runs.
        placeholder: form.defaultSessionName || "auto-generating…",
        fullWidth: true,
        key: "name",
      }),
    }),
    labeledSection({
      label: "Agent Command",
      child: text({
        value: form.cmd.value,
        cursorByte: form.cmd.cursor,
        // Empty submission spawns a bare terminal — the host
        // picks the shell with the same logic it uses for any
        // other embedded terminal, so the plugin doesn't have
        // to second-guess `$SHELL` resolution. If the user
        // submitted a non-empty cmd in the previous run we
        // surface it here as a hint (placeholder only — see
        // `NewSessionForm.lastCmd`).
        placeholder: form.lastCmd || "terminal",
        fullWidth: true,
        key: "cmd",
      }),
    }),
    labeledSection({
      label: "Branch",
      child: text({
        value: form.branch.value,
        cursorByte: form.branch.cursor,
        placeholder: branchPlaceholder,
        fullWidth: true,
        // Drop the key when the branch field is inert so Tab
        // skips it — there's no `git worktree add` to apply
        // it to.
        key: branchInert ? undefined : "branch",
      }),
    }),
  ];
  if (form.lastError) {
    children.push(spacer(0));
    children.push({
      kind: "raw",
      entries: [
        styledRow([
          {
            text: "Error: ",
            style: { fg: "ui.status_error_indicator_fg", bold: true },
          },
          { text: form.lastError },
        ]),
      ],
    });
  }
  children.push(
    spacer(0),
    // === Button row: bottom-right aligned. =======================
    row(
      flexSpacer(),
      button("Cancel", { intent: "danger", key: "cancel" }),
      spacer(2),
      button("Create Session", { intent: "primary", key: "create" }),
    ),
    spacer(0),
    // === Footer: keybinding helper, centered. ====================
    row(
      flexSpacer(),
      hintBar([
        { keys: "Tab", label: "next / accept" },
        { keys: "S-Tab", label: "prev" },
        { keys: "↑↓", label: "suggest / history" },
        { keys: "Space", label: "toggle" },
        { keys: "Enter", label: "advance / act" },
        { keys: "Esc", label: "cancel" },
      ]),
      flexSpacer(),
    ),
  );
  return col(...children);
}

// Derive a "my_org/project_name" style label from the current
// working directory's tail. Orchestrator never opens this dialog
// outside of a workspace; if the cwd has fewer than two
// components we fall back to whatever's there.
function deriveProjectLabel(): string {
  const cwd = editor.getCwd();
  const base = editor.pathBasename(cwd);
  const parent = editor.pathBasename(editor.pathDirname(cwd));
  if (parent && parent !== base) return `${parent}/${base}`;
  return base || cwd;
}


function renderForm(): void {
  if (!form || !formPanel) return;
  // Keep the focus mirror in step with the spec's tabbable set
  // (worktree may toggle disabled, branch may go inert) on every
  // render, BEFORE we ship the spec — `rebuildFormFocusCycle`
  // clamps the index if the previously focused entry has
  // disappeared.
  rebuildFormFocusCycle();
  formPanel.update(buildFormSpec());
}

function openForm(options?: { fromPicker?: boolean }): void {
  const lastCmd =
    (editor.getGlobalState("orchestrator.last_cmd") as string | undefined) ?? "";
  form = {
    projectPath: { value: "", cursor: 0 },
    name: { value: "", cursor: 0 },
    // Empty value — `lastCmd` shows as the placeholder. If the
    // user submits an empty cmd, the placeholder is used as the
    // actual command (see `submitForm`). This makes the
    // placeholder a genuine "press Enter to re-use this" hint
    // rather than a visual lie.
    cmd: { value: "", cursor: 0 },
    branch: { value: "", cursor: 0 },
    // Default checkbox state is `true` (the historical behaviour
    // of "always create a worktree"); the renderer demotes this
    // to `false` automatically when the resolved Project Path is
    // non-git.
    createWorktree: true,
    submitting: false,
    lastError: null,
    defaultProjectPath: "",
    projectPathIsGit: null,
    projectPathIsLinkedWorktree: null,
    defaultSessionName: "",
    defaultBranch: "",
    defaultBranchIsHeadFallback: false,
    lastCmd,
    fromPicker: !!options?.fromPicker,
    probeToken: 0,
    historyCursor: { project_path: -1, name: -1, cmd: -1, branch: -1 },
    historyDraft: { project_path: "", name: "", cmd: "", branch: "" },
    completion: { field: null, items: [], selectedIndex: 0, anchor: "", token: 0 },
  };
  formPanel = new FloatingWidgetPanel();
  // Width 60 / height 90: the host shrinks the panel to its actual
  // content height when content is shorter than the requested cap,
  // so a generous height ceiling doesn't waste space on tall
  // terminals (the form usually renders ~20 rows). The previous
  // 50% cap was a fixed canvas in disguise — on a 24-row terminal
  // it left the dialog 12 rows tall, clipping the Branch input,
  // the Cancel / Create Session buttons, and the hint bar.
  formPanel.mount(buildFormSpec(), { widthPct: 60, heightPct: 90 });
  editor.setEditorMode(NEW_SESSION_MODE);
  // Mirror the host's focus cycle so Up/Down can route to the
  // right field's history. Initial focus is on `project_path`
  // (the first tabbable in `buildFormSpec`).
  rebuildFormFocusCycle();
  formFocusIndex = 0;

  // Kick off the placeholder probes (canonical repo root,
  // default branch, next session name) against the editor's
  // cwd. Each probe is async and re-renders on completion.
  void probeProjectPathDefaults();
}

/// Resolve placeholders for the Project Path / Session Name /
/// Branch fields based on the *currently-effective* project
/// path: the user-typed value if any, else the editor's cwd
/// (the canonical-root probe runs against the latter). Re-runs
/// on every Project Path keystroke (debounced via the caller).
async function probeProjectPathDefaults(): Promise<void> {
  if (!form) return;
  const token = ++form.probeToken;
  const typedPath = form.projectPath.value.trim();

  // (1) Default Project Path: only meaningful when the user
  //     hasn't typed anything. Resolve cwd → canonical root,
  //     fall back to cwd verbatim for non-git launches.
  if (!typedPath) {
    const resolved = await resolveCanonicalRepoRoot(editor.getCwd());
    if (!form || form.probeToken !== token) return;
    form.defaultProjectPath = resolved || editor.getCwd();
  } else {
    // User typed a path: that IS the project, no canonical
    // resolution needed. Defaults that depend on it (session
    // name, default branch) still need to run against it below.
    form.defaultProjectPath = typedPath;
  }

  // (2) Is-inside-work-tree probe drives the worktree checkbox.
  const effectivePath = typedPath || form.defaultProjectPath;
  const isGit = await pathIsInsideGitWorkTree(effectivePath);
  if (!form || form.probeToken !== token) return;
  form.projectPathIsGit = isGit;

  // (2b) Existing-linked-worktree detection. When the path is a
  //      worktree created by `git worktree add` (not the repo's main
  //      checkout), default the checkbox to *unchecked* so the
  //      natural action is to attach to it. Only flip on the
  //      detection transition so we don't fight a user who
  //      deliberately re-checks "create a new worktree".
  const wasLinked = form.projectPathIsLinkedWorktree;
  if (isGit) {
    const info = await classifyWorktree(effectivePath);
    if (!form || form.probeToken !== token) return;
    form.projectPathIsLinkedWorktree = info?.isLinked === true;
  } else {
    form.projectPathIsLinkedWorktree = false;
  }
  if (form.projectPathIsLinkedWorktree && wasLinked !== true) {
    form.createWorktree = false;
  }

  // (3) Default branch + session name probes only make sense on
  //     a git path. On non-git, leave both empty (the renderer
  //     surfaces a "no git — N/A" branch placeholder, and the
  //     session name still works against the counter alone).
  if (isGit) {
    const [{ ref, isHeadFallback }, sessionName] = await Promise.all([
      detectDefaultBranchWithFallback(effectivePath),
      nextAutoSessionName(effectivePath),
    ]);
    if (!form || form.probeToken !== token) return;
    form.defaultBranch = ref;
    form.defaultBranchIsHeadFallback = isHeadFallback;
    form.defaultSessionName = sessionName;
  } else {
    // Non-git: still surface a numeric placeholder for Session
    // Name so the user sees what an empty submit will produce.
    // `nextAutoSessionName` falls back cleanly when the refs
    // probe fails (no git → empty set → counter+1).
    const sessionName = await nextAutoSessionName(effectivePath);
    if (!form || form.probeToken !== token) return;
    form.defaultBranch = "";
    form.defaultBranchIsHeadFallback = false;
    form.defaultSessionName = sessionName;
  }
  renderForm();
}

/// Schedule a debounced re-probe after the user changes the
/// Project Path field. 200ms feels snappy without spawning a
/// git subprocess on every keystroke. QuickJS has no
/// `setTimeout` — `editor.delay(ms)` is the async-sleep
/// primitive; the `probeToken` already enforces "only the
/// latest scheduled probe wins" so back-to-back keystrokes
/// collapse cleanly without an explicit timer handle.
function scheduleProjectPathReprobe(): void {
  if (!form) return;
  const token = ++form.probeToken;
  void editor.delay(200).then(() => {
    if (!form || form.probeToken !== token) return;
    void probeProjectPathDefaults();
  });
}

// =============================================================================
// Inline-dropdown completion (Phase 7)
//
// For Project Path and Branch we render a `list` below the input
// when the candidate set is non-empty. Candidates are fetched
// asynchronously (filesystem read for paths, git for branches);
// the `completion.token` makes only the freshest fetch's result
// land — same pattern as the project-path is-git probe.
// =============================================================================

const COMPLETION_VISIBLE_ROWS = 6;
const COMPLETION_MAX_ITEMS = 50;

/// Fire a fresh fetch of completion candidates for the named
/// field. Stale fetches (older `token`) discard their results
/// on completion. Caller is responsible for re-rendering once
/// the fetch lands — `setCompletionItems` does that.
function scheduleCompletionRefresh(
  field: "project_path" | "branch",
): void {
  if (!form) return;
  const anchor = form[field === "project_path" ? "projectPath" : "branch"].value;
  const token = ++form.completion.token;
  form.completion.field = field;
  form.completion.anchor = anchor;
  // Path completion reads from `editor.readDir`, which is a
  // synchronous host call (no IPC waiting). Run it inline so
  // Tab pressed immediately after the last keystroke picks
  // from the up-to-date candidate list rather than a stale
  // one — the user reported that with the debounce in place,
  // typing "repo" + Tab would accept the *previous* prefix's
  // top match (e.g. "Desktop") because the popup hadn't
  // refreshed yet.
  if (field === "project_path") {
    const items = computePathCompletions(anchor);
    if (!form || form.completion.token !== token) return;
    setCompletionItems(field, items);
    return;
  }
  // Branch completion shells out to `git for-each-ref` — that
  // *is* async, so a sync flush isn't possible. Keep the
  // 150ms debounce so we coalesce rapid typing into a single
  // subprocess invocation; Tab during the gap accepts the
  // last known list, which is the same behaviour `bash`'s
  // tab completion exhibits while a long-running compspec is
  // catching up.
  void editor.delay(150).then(async () => {
    if (!form || form.completion.token !== token) return;
    const items = await fetchBranchCompletions(anchor);
    if (!form || form.completion.token !== token) return;
    setCompletionItems(field, items);
  });
}

/// Synchronous variant of `fetchPathCompletions` — same logic,
/// but doesn't go through a `Promise` so it can run inline from
/// the `change` event handler. `fetchPathCompletions` keeps the
/// async signature for the legacy debounce path (in case the
/// fetcher ever grows an async step), but delegates here so the
/// two paths can't drift.
function computePathCompletions(typed: string): string[] {
  const slashIdx = typed.lastIndexOf("/");
  let parent: string;
  let basename: string;
  if (slashIdx < 0) {
    parent = typed ? "." : editor.getCwd();
    basename = typed;
  } else if (slashIdx === 0) {
    parent = "/";
    basename = typed.slice(1);
  } else {
    parent = typed.slice(0, slashIdx);
    basename = typed.slice(slashIdx + 1);
  }
  const entries = editor.readDir(parent);
  const matches = entries
    .filter((e) => !basename || e.name.startsWith(basename))
    .filter((e) => !e.name.startsWith(".") || basename.startsWith("."));
  matches.sort((a, b) => {
    if (a.is_dir !== b.is_dir) return a.is_dir ? -1 : 1;
    return a.name.localeCompare(b.name);
  });
  const prefix = parent.endsWith("/") ? parent : `${parent}/`;
  return matches.map((e) => `${prefix}${e.name}${e.is_dir ? "/" : ""}`);
}

function setCompletionItems(
  field: "project_path" | "branch",
  items: string[],
): void {
  if (!form) return;
  // Compose the popup row list: live completion candidates
  // first (regular `kind: undefined`), then any history entries
  // for this field that aren't already in the live list,
  // marked `kind: "history"` so the host renders them with the
  // `↶` marker + italic. Duplicate suppression keeps the popup
  // from showing the same path twice when a candidate happens
  // to match a previous submission.
  const live: CompletionItem[] = items
    .slice(0, COMPLETION_MAX_ITEMS)
    .map((value) => ({ value }));
  const histField = focusToHistoryField(field);
  let composed: CompletionItem[] = live;
  if (histField) {
    const seen = new Set(live.map((i) => i.value));
    const historyRows: CompletionItem[] = readHistory(histField)
      .filter((v) => !seen.has(v))
      .slice(0, COMPLETION_MAX_ITEMS)
      .map((value) => ({ value, kind: "history" as const }));
    composed = [...live, ...historyRows].slice(0, COMPLETION_MAX_ITEMS);
  }
  form.completion.field = field;
  form.completion.items = composed;
  form.completion.selectedIndex = 0;
  // Push the candidate list to the host's Text-widget instance
  // state. The host repaints the popup chrome (dim separator,
  // side borders, selected-row highlight) on its own — the
  // plugin doesn't need to drive a re-render.
  if (formPanel) {
    formPanel.setCompletions(field, form.completion.items);
  }
}

function closeCompletion(): void {
  if (!form) return;
  if (form.completion.field === null && form.completion.items.length === 0) {
    return;
  }
  const prevField = form.completion.field;
  form.completion.field = null;
  form.completion.items = [];
  form.completion.selectedIndex = 0;
  form.completion.token += 1; // invalidate any in-flight fetch
  // Mirror the close in host instance state so its popup goes
  // away in the same frame. Without this the host would keep
  // painting the candidate list until the next spec push
  // happened to land for this widget.
  if (formPanel && prevField) {
    formPanel.setCompletions(prevField, []);
  }
}

/// Split typed Project Path into (parent, basename), list
/// `parent` via the host's `readDir`, and filter to entries
/// whose name starts with `basename`. Directories get a
/// trailing `/` so the user sees the type and Tab keeps
/// descending. Empty input lists the user's home directory's
/// top-level entries as a starting point.
async function fetchPathCompletions(typed: string): Promise<string[]> {
  // Heuristic for "where to list". `parent` is everything up
  // to and including the last `/`; `basename` is the unfinished
  // tail we filter on. `/foo/ba` → parent `/foo/`, basename
  // `ba`. `bar` (no slash) → parent `.`, basename `bar`. `/`
  // → parent `/`, basename `""`. Delegates to the sync
  // `computePathCompletions` so the two paths can't drift —
  // see `scheduleCompletionRefresh` for the sync use case.
  return computePathCompletions(typed);
}

/// List the project's local + remote branches and tags via
/// `git for-each-ref` (one subprocess instead of three). Filter
/// by substring of the typed value — branch names commonly
/// carry slash-separated prefixes (`feat/`, `release/`) that
/// the user often doesn't type first.
async function fetchBranchCompletions(typed: string): Promise<string[]> {
  if (!form) return [];
  const projectPath = form.projectPath.value.trim() || form.defaultProjectPath;
  if (!projectPath) return [];
  if (form.projectPathIsGit === false) return [];
  const res = await spawnCollect(
    "git",
    [
      "-C",
      projectPath,
      "for-each-ref",
      "--format=%(refname:short)",
      "refs/heads/",
      "refs/remotes/",
      "refs/tags/",
    ],
    projectPath,
  );
  if (res.exit_code !== 0) return [];
  const lines = (res.stdout || "")
    .split(/\r?\n/)
    .map((l) => l.trim())
    .filter((l) => l.length > 0 && l !== "origin/HEAD");
  const needle = typed.toLowerCase();
  const matches = needle
    ? lines.filter((l) => l.toLowerCase().includes(needle))
    : lines;
  // Dedup the common `origin/<branch>` vs `<branch>` pair when
  // the local copy exists. Prefer the local short name; drop the
  // origin alias unless the user explicitly typed `origin`.
  const local = new Set(matches.filter((l) => !l.includes("/")));
  const wantsOrigin = needle.startsWith("origin/");
  const filtered = matches.filter((l) => {
    if (!wantsOrigin && l.startsWith("origin/")) {
      const bare = l.slice("origin/".length);
      if (local.has(bare)) return false;
    }
    return true;
  });
  // Stable order: exact-match-first, then prefix-match, then
  // substring; ties broken by length so shorter names surface.
  filtered.sort((a, b) => {
    const ascore = a.toLowerCase() === needle ? 0 : a.toLowerCase().startsWith(needle) ? 1 : 2;
    const bscore = b.toLowerCase() === needle ? 0 : b.toLowerCase().startsWith(needle) ? 1 : 2;
    if (ascore !== bscore) return ascore - bscore;
    return a.length - b.length || a.localeCompare(b);
  });
  return filtered;
}

/// Apply the user-accepted completion candidate to its field.
/// Fired in response to the host's `completion_accept` event
/// (Tab on a Text-with-open-completions): the host has already
/// figured out which row was selected — we just write it into
/// the form model and update the field's value. For Project
/// Path accepts that end in `/` (directory descent) we re-
/// fetch the candidate list for the new path so the user can
/// keep Tab-ing into deeper subdirs without first typing
/// anything; the host preserves the open popup across the
/// fetch, so it just refreshes in place.
function applyAcceptedCompletion(
  field: "project_path" | "branch",
  item: string,
): void {
  if (!form) return;
  const slot = field === "project_path" ? form.projectPath : form.branch;
  slot.value = item;
  slot.cursor = item.length;
  if (formPanel) formPanel.setValue(field, slot.value, slot.cursor);
  if (field === "project_path") {
    scheduleProjectPathReprobe();
  }
  // Always close the dropdown on accept — including when the accepted
  // item is a directory. Re-popping it here (the old behaviour) left the
  // popup covering the worktree / name fields and, because Tab *accepts*
  // while a popup is open, a Tab-to-advance user got stuck re-accepting
  // instead of moving to the next field (F8). Descending deeper still
  // works by typing — the field's `change` handler re-pops the
  // completion — and the next Tab now advances as expected.
  closeCompletion();
}

function closeForm(): void {
  if (formPanel) {
    formPanel.unmount();
    formPanel = null;
  }
  form = null;
  editor.setEditorMode(null);
}

// When the New-Session form was opened on top of a still-mounted dock,
// closing the form returns keyboard focus to the dock (rather than
// reopening a centered picker). Returns true when it handled the
// restore — i.e. the dock is live.
function restoreDockAfterForm(): boolean {
  if (!openPanel || !dockMode) return false;
  dockBlurred = false;
  dockFocus = "list";
  editor.floatingPanelControl(openPanel.id(), "focus", 0);
  openPanel.setFocusKey("sessions");
  refreshOpenDialog();
  return true;
}

// Cancel path: tear down the form, and if it was reached via the
// picker (Alt+N or "+ New Session" button), reopen the picker so
// Esc behaves like a true "back" rather than dropping the user
// into the bare editor. When the dock is still mounted underneath,
// just hand focus back to it instead.
function cancelForm(): void {
  const wasFromPicker = !!form?.fromPicker;
  closeForm();
  if (restoreDockAfterForm()) return;
  if (wasFromPicker) {
    openControlRoom();
  }
}

async function submitForm(): Promise<void> {
  if (!form || form.submitting) return;
  form.submitting = true;
  form.lastError = null;
  renderForm();

  // Honour the placeholder: when the user leaves Agent Command
  // blank, fall back to `lastCmd` (the placeholder text). The
  // placeholder is rendered as a hint — if the user accepts it by
  // pressing Enter on an empty field, the dialog should actually
  // run that command rather than silently spawning a bare shell.
  const cmd = form.cmd.value.trim() || form.lastCmd.trim();
  const branchInput = form.branch.value.trim();

  // Project Path: typed value wins; otherwise the resolved
  // canonical-root placeholder (or, if that probe never
  // completed, the editor cwd). The picked value drives the
  // entire submission flow.
  const projectPath = form.projectPath.value.trim() ||
    form.defaultProjectPath ||
    editor.getCwd();

  // Re-probe is-git so we trust the latest filesystem state
  // rather than a possibly-stale UI flag (race: user pressed
  // Enter while the debounced probe was still in flight).
  const isGit = await pathIsInsideGitWorkTree(projectPath);
  if (!form) return;
  const createWorktree = isGit === true && form.createWorktree;

  // Resolve the repo's main worktree root when we're in a
  // worktree-create flow — same logic as before, but rooted at
  // `projectPath` instead of cwd so the user can target a repo
  // other than the one the editor was launched in.
  let repoRoot = projectPath;
  if (createWorktree) {
    const canonical = await resolveCanonicalRepoRoot(projectPath);
    if (canonical) repoRoot = canonical;
  }

  // Session name resolution: explicit value wins. Otherwise
  // auto-generate by scanning `refs/heads/session-N` for the
  // next free index (the same probe that filled the
  // placeholder).
  const sessionName = form.name.value.trim() ||
    (await nextAutoSessionName(repoRoot, { persist: true }));
  if (!form) return;

  // Session root resolution:
  // - createWorktree=true  → fresh worktree under
  //   `<XDG>/orchestrator/<slug>/<session>/`.
  // - createWorktree=false → run inside `projectPath` itself
  //   (shared worktree / non-git path / multiple sessions on
  //   the same root).
  const root = createWorktree
    ? editor.pathJoin(
        editor.getDataDir(),
        "orchestrator",
        slugify(repoRoot),
        sessionName,
      )
    : projectPath;

  if (createWorktree) {
    const parent = editor.pathDirname(root);
    if (!editor.createDir(parent)) {
      if (!form) return;
      form.submitting = false;
      form.lastError = `mkdir failed: ${parent}`;
      editor.setStatus(`Orchestrator: ${form.lastError}`);
      renderForm();
      return;
    }

    // A freshly `git init`'d repo has an unborn HEAD (no commits), and
    // `git worktree add` then fails with the cryptic, un-actionable
    // "fatal: invalid reference: HEAD". Detect that specific case up
    // front and tell the user what to do, instead of surfacing two
    // layers of raw git error from the add + fallback below.
    const headProbe = await spawnCollect(
      "git",
      ["-C", repoRoot, "rev-parse", "--verify", "--quiet", "HEAD"],
      repoRoot,
    );
    if (headProbe.exit_code !== 0) {
      if (!form) return;
      form.submitting = false;
      form.lastError =
        "This repository has no commits yet — make an initial commit before creating a worktree session.";
      editor.setStatus(`Orchestrator: ${form.lastError}`);
      renderForm();
      return;
    }

    const defaultBranch = await detectDefaultBranch(repoRoot);
    const branchName = branchInput || sessionName;
    // Try `-b <new>` first; if it fails because the branch
    // already exists, fall back to checking out the existing
    // branch into a new worktree.
    let addRes = await spawnCollect(
      "git",
      ["-C", repoRoot, "worktree", "add", root, "-b", branchName, defaultBranch],
      repoRoot,
    );
    if (addRes.exit_code !== 0) {
      const fallback = await spawnCollect(
        "git",
        ["-C", repoRoot, "worktree", "add", root, branchName],
        repoRoot,
      );
      if (fallback.exit_code !== 0) {
        if (!form) return;
        form.submitting = false;
        // Prefer the fallback's stderr: when both attempts
        // fail, the `-b` branch's error is usually "branch
        // already exists" (which is *why* we tried the
        // fallback), and the fallback's error is the more
        // informative one.
        form.lastError = lastNonEmptyLine(fallback.stderr) ||
          lastNonEmptyLine(addRes.stderr) ||
          "git worktree add failed";
        editor.setStatus(`Orchestrator: ${form.lastError}`);
        renderForm();
        return;
      }
      addRes = fallback;
    }
  }

  if (cmd) {
    editor.setGlobalState("orchestrator.last_cmd", cmd);
  }

  // Attach-to-existing-worktree: when the user opted out of
  // creating a worktree but pointed Project Path at an *existing
  // linked worktree* (one created by `git worktree add`, possibly
  // for a repo Fresh has never opened before), treat it as the
  // dedicated worktree it is rather than a shared root. That means
  // `shared_worktree = false` (so Archive / Delete can
  // `git worktree move` / `remove` it) and a `project_path` of the
  // owning repo so the session groups with its siblings. A path
  // that's the repo's *main* worktree, or a non-git directory, stays
  // shared — you can't `git worktree remove` either of those.
  const attachInfo = !createWorktree ? await classifyWorktree(root) : null;
  if (!form) return;
  const isLinkedAttach = attachInfo?.isLinked === true;
  const effectiveProjectPath = isLinkedAttach ? attachInfo!.mainRoot : projectPath;

  // Branch / cmd values used for the per-window state record —
  // `branchName` only exists in the worktree-create flow above; for
  // an attached linked worktree we report its checked-out branch;
  // for the shared-worktree / non-git case we leave it blank.
  const reportedBranch = createWorktree
    ? (branchInput || sessionName)
    : (isLinkedAttach ? attachInfo!.branch : "");

  // Append the user-effective values to per-field input
  // history so ↑/↓ can recall them on the next form open.
  appendHistory("project_path", projectPath);
  appendHistory("name", sessionName);
  if (cmd) appendHistory("cmd", cmd);
  if (createWorktree) appendHistory("branch", reportedBranch);

  closeForm();
  // When the form was opened over the dock, the new session dives in
  // below — hand keyboard focus to the dived-into terminal by blurring
  // the dock (it stays visible and refreshes to show the new row).
  if (openPanel && dockMode) {
    dockBlurred = true;
    editor.floatingPanelControl(openPanel.id(), "blur", 0);
  }

  // Spawn the new window + agent terminal atomically. Compared to
  // the legacy `createWindow → window_created hook → createTerminal`
  // chain this avoids the transient `[No Name]` tab the host's
  // eager seed used to leave alongside the agent terminal: the
  // terminal IS the new window's seed buffer, so the window is
  // born with a single tab.
  const argv = splitAgentCmd(cmd);
  // Shared only when we neither created a worktree nor attached to an
  // existing linked one (i.e. a non-git dir or the repo's main tree).
  const sharedWorktree = !createWorktree && !isLinkedAttach;
  try {
    const result = await editor.createWindowWithTerminal({
      root,
      label: sessionName,
      cwd: root,
      command: argv.length > 0 ? argv : undefined,
      title: argv.length > 0 ? argv[0] : undefined,
    });
    const id = result.windowId;
    // `createWindowWithTerminal` already dove into the new window,
    // so `setWindowState` writes to it.
    editor.setWindowState("project_path", effectiveProjectPath);
    editor.setWindowState("shared_worktree", sharedWorktree);
    // If we attached to a worktree that was sitting in the picker as
    // a discovered row, drop that placeholder — this live window
    // supersedes it.
    const discId = discoveredIdByPath.get(root);
    if (discId !== undefined) {
      orchestratorSessions.delete(discId);
      discoveredIdByPath.delete(root);
    }
    const tracked: AgentSession = {
      id,
      label: sessionName,
      root,
      projectPath: effectiveProjectPath,
      sharedWorktree,
      terminalId: result.terminalId,
      state: "running",
      createdAt: Date.now(),
      branch: reportedBranch || undefined,
    };
    orchestratorSessions.set(id, tracked);
    // Refresh the dock so the freshly-created session shows up.
    if (openPanel && dockMode) refreshOpenDialog();
  } catch (e) {
    editor.setStatus(
      `Orchestrator: failed to start session — ${
        e instanceof Error ? e.message : String(e)
      }`,
    );
  }
}

/// Open a session in an existing worktree without creating one —
/// the dive action for a discovered row, and the building block the
/// new-session form reuses when the user points Project Path at an
/// existing linked worktree. Spawns a bare terminal (no agent
/// command) rooted at the worktree, tags the window with its
/// canonical project + `shared_worktree = false` so Archive / Delete
/// manage it as the real worktree it is, then drops the discovered
/// placeholder (the live window supersedes it).
async function attachToWorktree(opts: {
  root: string;
  projectPath: string;
  label: string;
  branch?: string;
  discoveredId?: number;
}): Promise<void> {
  try {
    const result = await editor.createWindowWithTerminal({
      root: opts.root,
      label: opts.label,
      cwd: opts.root,
    });
    const id = result.windowId;
    editor.setWindowState("project_path", opts.projectPath);
    editor.setWindowState("shared_worktree", false);
    if (opts.discoveredId !== undefined) {
      orchestratorSessions.delete(opts.discoveredId);
      discoveredIdByPath.delete(opts.root);
    }
    orchestratorSessions.set(id, {
      id,
      label: opts.label,
      root: opts.root,
      projectPath: opts.projectPath,
      sharedWorktree: false,
      terminalId: result.terminalId,
      state: "running",
      createdAt: Date.now(),
      branch: opts.branch,
    });
  } catch (e) {
    editor.setStatus(
      `Orchestrator: failed to attach session — ${
        e instanceof Error ? e.message : String(e)
      }`,
    );
  }
}

function startNewSession(): void {
  if (form) return; // already open
  openForm();
}

// Form key bindings — each delegates to smart-key dispatch on the
// panel, which routes to the focused widget. `mode_text_input`
// handles printable input outside this list.
// Enter is bound to a thin shim that closes the completion
// dropdown without accepting (Tab is the only accept path —
// matches bash / fish / readline path-completion conventions),
// then forwards Enter to the host's smart-key dispatch so the
// normal behaviour applies: Enter-on-button → activate (Cancel
// cancels, Create Session submits via their `widget_event`
// "activate" branches), Enter-on-text-input → focus advance.
// Without the shim, the host's picker-style Enter wiring would
// fire the sibling completion list's activate event and silently
// overwrite the typed text with the highlighted suggestion.
const FORM_MODE_BINDINGS: [string, string][] = [
  ["Tab", "orchestrator_form_key_tab"],
  ["S-Tab", "orchestrator_form_key_shift_tab"],
  ["Enter", "orchestrator_form_key_enter"],
  ["Escape", "orchestrator_form_key_escape"],
  ["Backspace", "orchestrator_form_key_backspace"],
  ["Delete", "orchestrator_form_key_delete"],
  ["Home", "orchestrator_form_key_home"],
  ["End", "orchestrator_form_key_end"],
  ["Left", "orchestrator_form_key_left"],
  ["Right", "orchestrator_form_key_right"],
  ["Up", "orchestrator_form_key_up"],
  ["Down", "orchestrator_form_key_down"],
];

editor.defineMode(NEW_SESSION_MODE, FORM_MODE_BINDINGS, true, true);

function dispatchFormKey(name: string): void {
  if (!form || !formPanel) return;
  formPanel.command(widgetKey(name));
}

// Tab / Enter / Up / Down / Escape are all routed straight to
// the host's smart-key dispatch via `dispatchFormKey`. The host
// owns the completion popup state (instance state on the Text
// widget), so when the popup is open it short-circuits these
// keys to popup-specific behaviour (accept, dismiss, move
// selection) and falls through to the widget's default key
// handling otherwise. The plugin just reacts to the events the
// host emits — `completion_accept` and `completion_dismiss`,
// handled in the `widget_event` dispatch below.
registerHandler("orchestrator_form_key_tab", () => {
  if (completionVisibleForFocused()) {
    // Host fires completion_accept; plugin's widget_event
    // handler applies the value. No focus advance.
    dispatchFormKey("Tab");
    return;
  }
  advanceFormFocus(1);
  dispatchFormKey("Tab");
});
registerHandler("orchestrator_form_key_enter", () => {
  // When the popup is open, the host's smart-key fires
  // `completion_dismiss` (plugin syncs local state via that
  // event) without firing the form's picker-Enter or focus
  // advance — Enter is "dismiss the popup, stay focused on
  // the text input". When the popup is closed, Enter falls
  // through to the host's normal Text-widget Enter (picker
  // activate or focus advance). On a focus advance, the host
  // fires a `widget_event { event_type: "focus" }` and the
  // plugin snaps `formFocusIndex` from that authoritative
  // signal — see the `focus` branch in the widget_event
  // handler below.
  dispatchFormKey("Enter");
});
registerHandler(
  "orchestrator_form_key_shift_tab",
  () => {
    // Shift+Tab doesn't accept — it always reverses focus.
    // (The convention is that S-Tab is the "go back" gesture;
    // overloading it to accept-then-go-back is more confusing
    // than useful.)
    closeCompletion();
    advanceFormFocus(-1);
    dispatchFormKey("Shift+Tab");
  },
);
registerHandler("orchestrator_form_key_escape", () => {
  // When the popup is open, the host dismisses on Escape and
  // emits `completion_dismiss`; the plugin's local state
  // resync happens in the widget_event handler. Only when
  // the popup is already closed does Escape cancel the form.
  if (completionVisibleForFocused()) {
    dispatchFormKey("Escape");
    return;
  }
  if (form) cancelForm();
});
registerHandler(
  "orchestrator_form_key_backspace",
  () => dispatchFormKey("Backspace"),
);
registerHandler("orchestrator_form_key_delete", () => dispatchFormKey("Delete"));
registerHandler("orchestrator_form_key_home", () => dispatchFormKey("Home"));
registerHandler("orchestrator_form_key_end", () => dispatchFormKey("End"));
registerHandler("orchestrator_form_key_left", () => dispatchFormKey("Left"));
registerHandler("orchestrator_form_key_right", () => dispatchFormKey("Right"));
registerHandler("orchestrator_form_key_up", () => {
  // Popup-open: dispatch straight through so the host moves
  // the popup-selection cursor.
  // Popup-closed: on a completion-bearing field
  // (project_path / branch) re-fetch the popup so the user
  // gets back live candidates AND any `↶`-marked history rows
  // mixed in (see `setCompletionItems`). On a history-bearing
  // non-completion field (name / cmd) walk history in place.
  // Otherwise pass through.
  if (completionVisibleForFocused()) {
    dispatchFormKey("Up");
    return;
  }
  const focusKey = formFocusedKey();
  if (focusKey === "project_path" || focusKey === "branch") {
    scheduleCompletionRefresh(focusKey);
    return;
  }
  const histField = focusToHistoryField(focusKey);
  if (histField) {
    walkHistory(histField, -1);
  } else {
    dispatchFormKey("Up");
  }
});
registerHandler("orchestrator_form_key_down", () => {
  if (completionVisibleForFocused()) {
    dispatchFormKey("Down");
    return;
  }
  const focusKey = formFocusedKey();
  if (focusKey === "project_path" || focusKey === "branch") {
    scheduleCompletionRefresh(focusKey);
    return;
  }
  const histField = focusToHistoryField(focusKey);
  if (histField) {
    walkHistory(histField, 1);
  } else {
    dispatchFormKey("Down");
  }
});

/// Is the completion popup open for the currently focused
/// input? Tracked plugin-side because the plugin still needs
/// to know in order to gate history-walk (Up/Down on an empty-
/// popup history-bearing input walks the history list, not
/// the popup). The host's instance state is authoritative for
/// the popup itself; the plugin mirrors the open/closed bit
/// here by populating `form.completion.items` from
/// `setCompletionItems` and clearing it from
/// `closeCompletion` / on the `completion_dismiss` event.
function completionVisibleForFocused(): boolean {
  if (!form) return false;
  const c = form.completion;
  if (c.field === null || c.items.length === 0) return false;
  return formFocusedKey() === c.field;
}

// Printable input arrives via the global `mode_text_input` action.
// Other plugins may also register a `mode_text_input` handler;
// guard on `form` so this handler is a no-op outside the form.
//
// Special-case: a space character on a focused Toggle / Button
// is "activate this control", not "insert a literal space into
// the value". The host's smart-key dispatch already does this
// for `widgetCommand({kind: "key", name: "Space"})`, but the
// mode binding for "Space" is shadowed by the global text-input
// path (printable chars route to `mode_text_input` ahead of the
// custom mode keymap), so we intercept here instead.
function orchestrator_mode_text_input(args: { text: string }): void {
  if (!form || !formPanel || !args?.text) return;
  formPanel.command(textInputChar(args.text));
}
registerHandler("mode_text_input", orchestrator_mode_text_input);

// Open the confirm panel for `action` against the currently
// selected session, rebuild the spec, and ensure the Cancel
// button gets default focus.
//
// `buildOpenSpec` drops the `key` from the filter input and the
// `+ New Session` button while `pendingConfirm` is set, so they
// fall out of the Tab cycle. Cancel still isn't the first
// tabbable in raw declaration order, though — `setFocusKey`
// pins it explicitly so a stray Enter on mount is a no-op
// rather than a worktree wipe (confirm prompts for destructive
// actions should be biased toward the safe path).
function enterConfirm(action: "stop" | "archive" | "delete"): void {
  if (!openDialog || !openPanel) return;
  const id = openDialog.filteredIds[openDialog.selectedIndex];
  if (typeof id !== "number" || id <= 0) return;
  // Every live session can be stopped/archived/deleted now: Archive
  // records a launch/in-place session at its own root (no worktree to
  // move) and worktree sessions move to the graveyard; closing the last
  // live window opens a replacement first (see `ensureReplacementWindow`
  // in `archiveOne` / `deleteOne`). So no eligibility refusal here — just
  // confirm and run.
  openDialog.pendingConfirm = { action, ids: [id] };
  openPanel.update(buildOpenSpec());
  openPanel.setFocusKey("confirm-cancel");
}

// Open the confirm panel for a *bulk* action over the current
// checkbox selection. Filters to the eligible members up front (so
// the confirm count matches what will actually run); refuses with a
// banner when nothing is eligible.
function enterBulkConfirm(action: BulkAction): void {
  if (!openDialog || !openPanel) return;
  const targets = eligibleSelected(action);
  if (targets.length === 0) {
    setDialogError(`no selected session can be ${action === "stop" ? "stopped" : action + "d"}`);
    refreshOpenDialog();
    return;
  }
  // All three actions confirm — even Stop, so a bulk Stop over a
  // large selection isn't a single mis-key away. The confirm panel
  // lists the targets and shows the eligible count.
  openDialog.pendingConfirm = { action, ids: targets };
  openPanel.update(buildOpenSpec());
  openPanel.setFocusKey("confirm-cancel");
}

editor.on("widget_event", (e) => {
  // ---------------------------------------------------------------------
  // New-session form
  // ---------------------------------------------------------------------
  if (form && formPanel && e.panel_id === formPanel.id()) {
    if (e.event_type === "focus") {
      // Host fires this whenever the panel's focused widget
      // changes — key-driven (Tab / Shift-Tab / Enter focus-
      // advance), click-driven, or any other host-side focus
      // mutation. The plugin keeps a local `formFocusIndex`
      // mirror so handlers like Up/Down can look up the right
      // history field without first asking the host; we snap
      // that mirror from the authoritative signal here so the
      // plugin never has to predict host-side focus rules.
      snapFormFocusTo(e.widget_key);
      return;
    }
    if (e.event_type === "change") {
      const field = e.widget_key;
      const payload = (e.payload ?? {}) as Record<string, unknown>;
      const value = payload.value;
      const cursor = payload.cursorByte;
      if (typeof value !== "string") return;
      const slot = field === "project_path"
        ? form.projectPath
        : field === "name"
        ? form.name
        : field === "cmd"
        ? form.cmd
        : field === "branch"
        ? form.branch
        : null;
      if (slot) {
        slot.value = value;
        if (typeof cursor === "number") slot.cursor = cursor;
        // Typing in any history-bearing field invalidates the
        // history cursor — the user is composing a new draft.
        const histField = focusToHistoryField(field);
        if (histField) form.historyCursor[histField] = -1;
        // Snap our focus mirror to wherever the change just
        // landed — covers mouse-click focus changes (no Tab key
        // for us to intercept).
        snapFormFocusTo(field);
      }
      if (field === "project_path") {
        scheduleProjectPathReprobe();
        scheduleCompletionRefresh("project_path");
      } else if (field === "branch") {
        scheduleCompletionRefresh("branch");
      } else {
        // Any other field's change implicitly closes the
        // dropdown (the user moved on).
        closeCompletion();
      }
      return;
    }
    if (e.event_type === "toggle" && e.widget_key === "worktree") {
      const payload = (e.payload ?? {}) as Record<string, unknown>;
      const checked = payload.checked;
      if (typeof checked === "boolean") {
        form.createWorktree = checked;
      } else {
        form.createWorktree = !form.createWorktree;
      }
      renderForm();
      return;
    }
    if (e.event_type === "completion_accept") {
      // Host fires this on Tab against a Text widget with an
      // open completion popup. The payload carries the
      // candidate that was highlighted.
      const payload = (e.payload ?? {}) as Record<string, unknown>;
      const value = payload.value;
      if (typeof value !== "string") return;
      if (e.widget_key === "project_path" || e.widget_key === "branch") {
        applyAcceptedCompletion(e.widget_key, value);
      }
      return;
    }
    if (e.event_type === "completion_dismiss") {
      // Host fires this on Enter / Esc against a Text widget
      // with an open popup. Sync plugin-side state so the
      // history-walk gate (Up/Down on an empty-popup history-
      // bearing field) reads `false` again.
      closeCompletion();
      return;
    }
    if (e.event_type === "activate") {
      if (e.widget_key === "create") {
        void submitForm();
      } else if (e.widget_key === "cancel") {
        cancelForm();
      }
      return;
    }
    if (e.event_type === "cancel") {
      // Host fires this when Esc unmounts the floating panel —
      // mirror our own state and (if reached from the picker)
      // bounce back to the picker so Esc is "back", not "out".
      const wasFromPicker = !!form?.fromPicker;
      form = null;
      formPanel = null;
      editor.setEditorMode(null);
      if (restoreDockAfterForm()) return;
      if (wasFromPicker) {
        openControlRoom();
      }
      return;
    }
    return;
  }

  // ---------------------------------------------------------------------
  // Open dialog (session picker)
  // ---------------------------------------------------------------------
  if (openPanel && openDialog && e.panel_id === openPanel.id()) {
    if (e.event_type === "blur") {
      // Host fired this because focus left the dock (Enter/Esc dive or
      // leave, editor click, or an unhandled chord like Ctrl+P). The
      // dock stays visible; the host stops routing keys to it.
      if (dockMode) {
        dockBlurred = true;
        // Leaving the dock resets the filter so re-entering always
        // shows the full session list. A stale filter (e.g. an old
        // "/gamma") otherwise silently hides sessions on the next
        // focus, with only the filter box as a clue — and there is no
        // one-key clear from the list. (See F5.)
        if (openDialog.filter.value !== "") {
          openDialog.filter.value = "";
          openDialog.filter.cursor = 0;
          dockFocus = "list";
          const activeId = editor.activeWindow();
          const all = filterSessions("");
          openDialog.filteredIds = all;
          const activeIdx = all.indexOf(activeId);
          openDialog.selectedIndex = activeIdx >= 0 ? activeIdx : 0;
          // The filter input is a controlled widget: clearing our
          // local state only changes what we *filter by*. The text
          // box keeps its own buffer until we push the empty value
          // back, so reset it explicitly — otherwise the list shows
          // every session while the box still reads "gamma".
          openPanel?.setValue("filter", "", 0);
          refreshOpenDialog();
        }
      }
      return;
    }
    if (e.event_type === "focus") {
      // Focus (re-)entered the dock / picker — a mouse click on a
      // row/filter, a host-driven focus move, or the symmetric
      // refocus_floating_panel notification fired by the host's
      // un-dive mouse handler. Track the zone (dockFocus) and the
      // exact focused widget (pickerFocusKey); mark the dock active.
      if (typeof e.widget_key === "string" && e.widget_key.length > 0) {
        pickerFocusKey = e.widget_key;
      }
      if (dockMode) {
        dockBlurred = false;
        dockFocus = e.widget_key === "filter" ? "filter" : "list";
      }
      return;
    }
    if (e.event_type === "dock_space") {
      // Host Space on the dock → toggle the highlighted row's
      // multi-select checkbox.
      if (dockMode) toggleSelectCurrent();
      return;
    }
    if (e.event_type === "dock_new") {
      // Host Alt+N on the dock → open the new-session form. The form is
      // a centered modal in a separate slot, so the dock stays visible.
      if (dockMode) {
        dockBlurred = true;
        openForm({ fromPicker: true });
      }
      return;
    }
    if (e.event_type === "dock_activate") {
      // Host Enter on the dock's session list. Mirrors the dialog's
      // `activate` branch: a discovered (on-disk) worktree has no live
      // window to switch to, so attach a fresh session at it; any other
      // row is already active via the arrow live-switch, so Enter just
      // hands keyboard focus to the editor (the dock stays visible).
      if (!dockMode || !openPanel || !openDialog) return;
      const id = openDialog.filteredIds[openDialog.selectedIndex];
      const sel = typeof id === "number" ? orchestratorSessions.get(id) : undefined;
      if (sel && sel.discovered) {
        void attachToWorktree({
          root: sel.root,
          projectPath: sel.projectPath ?? sel.root,
          label: sel.label,
          branch: sel.branch,
          discoveredId: sel.id,
        });
        return;
      }
      dockBlurred = true;
      editor.floatingPanelControl(openPanel.id(), "blur", 0);
      editor.setEditorMode(null);
      return;
    }
    if (e.event_type === "dock_toggle_worktrees") {
      // Host Alt+T on the dock — the dialog's OPEN_MODE chord has no
      // equivalent in the dock (no editor mode), so the host routes it
      // here. Share the same flip the click/Alt+T-in-dialog use.
      if (dockMode) toggleShowWorktrees();
      return;
    }
    if (e.event_type === "dock_toggle_trivial") {
      if (dockMode) toggleHideTrivial();
      return;
    }
    if (e.event_type === "dock_toggle_scope") {
      if (dockMode) toggleScope();
      return;
    }
    if (e.event_type === "change" && e.widget_key === "filter") {
      const payload = (e.payload ?? {}) as Record<string, unknown>;
      const value = payload.value;
      const cursor = payload.cursorByte;
      if (typeof value !== "string") return;
      openDialog.filter.value = value;
      if (typeof cursor === "number") openDialog.filter.cursor = cursor;
      // Filter change implies the user has moved on from any
      // previous error — clear the banner so it doesn't shadow
      // the typing experience.
      clearDialogError();
      // Preserve highlighted session across the filter narrowing
      // when possible — if the previously selected id is still in
      // the new filtered set, keep it; otherwise reset to 0.
      const prevId = openDialog.filteredIds[openDialog.selectedIndex];
      const next = filterSessions(value);
      openDialog.filteredIds = next;
      const nextIdx = prevId !== undefined ? next.indexOf(prevId) : -1;
      openDialog.selectedIndex = nextIdx >= 0 ? nextIdx : 0;
      refreshOpenDialog();
      return;
    }
    // List selection. Keyboard nav fires this with `widget_key`
    // "sessions" (the list's own key); a mouse click on a row fires it
    // with `widget_key` set to the clicked item's key, carrying the
    // list key in `payload.list_key` instead — accept both so clicking a
    // row selects it (highlight + preview) just like arrowing to it.
    if (
      e.event_type === "select" &&
      (e.widget_key === "sessions" ||
        ((e.payload ?? {}) as Record<string, unknown>).list_key === "sessions")
    ) {
      const payload = (e.payload ?? {}) as Record<string, unknown>;
      const idx = payload.index;
      console.warn(
        `[dock-select] event reached plugin: idx=${idx} widget_key=${e.widget_key} dockMode=${dockMode} dockBlurred=${dockBlurred}`,
      );
      if (typeof idx === "number") {
        const prevIdx = openDialog.selectedIndex;
        openDialog.selectedIndex = idx;
        clearDialogError();
        if (dockMode) {
          // The editor to the dock's right is the preview: arrowing
          // the list switches the active window live (debounced),
          // wiping down when moving down the list and up when moving up.
          openPanel.update(buildDockSpec());
          openPanel.setSelectedIndex("sessions", openDialog.selectedIndex);
          const fromEdge = idx > prevIdx ? "bottom" : idx < prevIdx ? "top" : null;
          scheduleDockSwitch(fromEdge);
          return;
        }
        // Update preview pane.
        openPanel.update(buildOpenSpec());
        // Re-pin the list selection so the spec re-emit doesn't
        // snap it back to 0.
        openPanel.setSelectedIndex("sessions", openDialog.selectedIndex);
        // Up/Down on a focused action button (Stop / Archive /
        // Delete / Details / +New Session) routes to the sessions
        // list via the host's smart-key dispatch but leaves focus
        // on the button. Snap focus back to Visit so the user can
        // press Enter to open the newly-highlighted session — the
        // dialog's whole reason for being. Idempotent when focus
        // is already on Visit. Skipped in bulk mode and during a
        // confirm, where "visit" isn't in the spec.
        if (selectedSessions().length < 2 && !openDialog.pendingConfirm) {
          openPanel.setFocusKey("visit");
        }
      }
      return;
    }
    if (
      e.event_type === "activate" &&
      (e.widget_key === "sessions" || e.widget_key === "visit")
    ) {
      const id = openDialog.filteredIds[openDialog.selectedIndex];
      const sel = typeof id === "number" ? orchestratorSessions.get(id) : undefined;
      if (sel && sel.discovered) {
        // Discovered worktree: there's no window to switch to —
        // open one by attaching a fresh session to the worktree.
        closeOpenDialog();
        void attachToWorktree({
          root: sel.root,
          projectPath: sel.projectPath ?? sel.root,
          label: sel.label,
          branch: sel.branch,
          discoveredId: sel.id,
        });
        return;
      }
      if (typeof id === "number" && id > 0 && id !== editor.activeWindow()) {
        editor.setActiveWindow(id);
      }
      if (dockMode && openPanel) {
        // Dock stays visible; Enter just hands keyboard focus to the
        // editor (the session is already active via live-switch).
        editor.floatingPanelControl(openPanel.id(), "blur");
        dockBlurred = true;
        editor.setEditorMode(null);
        return;
      }
      closeOpenDialog();
      return;
    }
    if (e.event_type === "activate" && e.widget_key === "new-session") {
      closeOpenDialog();
      openForm({ fromPicker: true });
      return;
    }
    if (e.event_type === "activate" && e.widget_key === "scope-toggle") {
      toggleScope();
      return;
    }
    if (e.event_type === "activate" && e.widget_key === "toggle-details") {
      openDialog.showDetails = !openDialog.showDetails;
      refreshOpenDialog();
      return;
    }
    if (e.event_type === "toggle" && e.widget_key === "worktree-show") {
      // The toggle widget reports the new checked state; route through
      // the shared flip so the Alt+T chord and the click stay in sync.
      toggleShowWorktrees();
      return;
    }
    if (e.event_type === "toggle" && e.widget_key === "hide-trivial") {
      // Same pattern as the worktree toggle: route the click through the
      // shared flip so the checkbox and the Alt+I chord stay in sync.
      toggleHideTrivial();
      return;
    }
    if (e.event_type === "activate" && e.widget_key === "stop") {
      enterConfirm("stop");
      return;
    }
    if (e.event_type === "activate" && e.widget_key === "archive") {
      enterConfirm("archive");
      return;
    }
    if (e.event_type === "activate" && e.widget_key === "delete") {
      enterConfirm("delete");
      return;
    }
    // Bulk action bar (Layout B) — Stop / Archive / Delete over the
    // checkbox selection, plus Clear.
    if (e.event_type === "activate" && e.widget_key === "bulk-stop") {
      enterBulkConfirm("stop");
      return;
    }
    if (e.event_type === "activate" && e.widget_key === "bulk-archive") {
      enterBulkConfirm("archive");
      return;
    }
    if (e.event_type === "activate" && e.widget_key === "bulk-delete") {
      enterBulkConfirm("delete");
      return;
    }
    if (e.event_type === "activate" && e.widget_key === "bulk-clear") {
      openDialog.selectedIds.clear();
      refreshOpenDialog();
      openPanel.setFocusKey("visit");
      return;
    }
    if (e.event_type === "activate" && e.widget_key === "confirm-cancel") {
      openDialog.pendingConfirm = null;
      openPanel.update(buildOpenSpec());
      return;
    }
    // Confirmed Stop / Archive / Delete — single row or bulk batch.
    // The ids were captured into `pendingConfirm` by enterConfirm /
    // enterBulkConfirm; `runConfirmedAction` re-checks eligibility,
    // drives the in-flight markers, and triggers sync.
    if (
      e.event_type === "activate" &&
      (e.widget_key === "confirm-stop" ||
        e.widget_key === "confirm-archive" ||
        e.widget_key === "confirm-delete")
    ) {
      const confirm = openDialog.pendingConfirm;
      openDialog.pendingConfirm = null;
      if (confirm) {
        void runConfirmedAction(confirm.action, confirm.ids);
      }
      if (openPanel) openPanel.update(buildOpenSpec());
      return;
    }
    if (e.event_type === "cancel") {
      // Esc unmounted the panel — sync our own state.
      openDialog = null;
      openPanel = null;
      editor.setEditorMode(null);
      return;
    }
    return;
  }
});

// Legacy kill helper retained for the `Orchestrator: Kill Selected`
// command-palette command. In the widget-based picker (Phase 1)
// the open dialog has no kill action — Phase 3-5 will replace
// this with Stop / Archive / Delete. When invoked while the
// open dialog is up, it targets that dialog's selection; when
// invoked from the palette outside the dialog, it status-bars
// with guidance.
function killSelected(): void {
  if (!openDialog) {
    editor.setStatus(
      "Orchestrator: open the session list (Ctrl+P → Orchestrator: Open) first",
    );
    return;
  }
  const ids = openDialog.filteredIds;
  if (ids.length === 0) {
    editor.setStatus("Orchestrator: no session selected");
    return;
  }
  const id = ids[Math.max(0, Math.min(openDialog.selectedIndex, ids.length - 1))];
  if (id <= 0) {
    editor.setStatus("Orchestrator: select a session row first");
    return;
  }
  if (id === editor.activeWindow()) {
    editor.setStatus(
      "Orchestrator: dive elsewhere first, then kill this session",
    );
    return;
  }
  const s = orchestratorSessions.get(id);
  if (s && s.terminalId !== null) {
    editor.closeTerminal(s.terminalId);
  }
  editor.closeWindow(id);
}

// =============================================================================
// Lifecycle hook handlers
// =============================================================================

editor.on("window_created", () => {
  // The orchestrator's own new-session flow uses
  // `createWindowWithTerminal` (atomic — populates the window
  // before returning), so by the time this hook fires for one of
  // our spawns the session is already tracked. Other plugins or
  // host actions creating windows just need the picker to
  // refresh.
  refreshOpenDialog();
});

editor.on("window_closed", () => {
  refreshOpenDialog();
});

editor.on("active_window_changed", () => {
  refreshOpenDialog();
});

// Re-flow the open-picker on terminal resize. The dialog's
// `listVisibleRows` / `embedRows` are captured at open-time
// (orchestrator.ts:`openControlRoom`); without this subscription
// they stay frozen at the pre-resize values and the live preview
// embed gets clipped (or leaves blank space) when the user
// resizes their tmux pane. The host also re-renders the panel
// against the new screen width unconditionally (see
// `Editor::resize` in `lifecycle.rs`); this handler just refreshes
// the spec so the *plugin's* row-count knobs adopt the new
// viewport at the same time.
editor.on("resize", () => {
  if (openDialog && openPanel) {
    // Make the dock responsive: re-issue its width on every resize so it
    // scales with the terminal. Uses the focus-preserving `dock_width`
    // op (not `dock`, which would steal keyboard focus back from the
    // editor); the host ignores it unless the panel is docked and still
    // lets a user-dragged width win. buildOpenSpec/buildDockSpec also
    // refit `listVisibleRows` + content width on the refresh below.
    if (dockMode) {
      editor.floatingPanelControl(openPanel.id(), "dock_width", dockDefaultWidth());
    }
    refreshOpenDialog();
  }
});

// =============================================================================
// Agent activity tracking from terminal output / exit
//
// We only claim what the terminal can prove: a session is "working" while
// it's actively printing, "idle" once it goes quiet. The signal is the
// timestamp of the last output; `sessionState` buckets it against
// IDLE_AFTER_MS at render time. We don't poll the process, so this tracks
// *output*, not liveness — a wedged agent reads idle, same as a finished
// one, which is the honest limit of what we can see from here.
//
// Keyed by `window_id`, not the one terminal id Orchestrator spawned: a
// session is its editor window (its id == the session id), so output from
// ANY terminal in that window counts — a second shell the user opened, an
// agent that re-execs, etc. The host fires `terminal_output` on every PTY
// read, so this also lights up for in-place redraws and carriage-return
// progress bars, not just newline-terminated lines.
// =============================================================================

editor.on("terminal_output", (payload) => {
  const s = orchestratorSessions.get(payload.window_id);
  if (s) {
    // Stamp the moment of output. `sessionState` turns this into
    // working/idle; the cached `state` is updated so persistence and
    // any non-render reader see a fresh value too.
    s.lastOutputAt = Date.now();
    s.state = "working";
    refreshOpenDialog();
  }
});

editor.on("terminal_exit", (payload) => {
  const s = orchestratorSessions.get(payload.window_id);
  if (s) {
    // A terminal in this session ended — it can't be the source of work
    // anymore. Drop to idle and clear the timestamp so the row reads idle
    // immediately rather than riding out the IDLE_AFTER_MS tail. If another
    // terminal in the same window is still printing, the next
    // `terminal_output` re-marks it working within the debounce window.
    s.lastOutputAt = null;
    s.state = "idle";
    refreshOpenDialog();
  }
});

// =============================================================================
// Commands
// =============================================================================

registerHandler("orchestrator_open", openControlRoom);
registerHandler("orchestrator_new", startNewSession);
registerHandler("orchestrator_kill", killSelected);

// `terminalBypass: true` keeps these commands reachable from a
// keyboard-focused terminal pane — a user with `Ctrl+O` bound to
// `Orchestrator: Open` shouldn't need to first hit `Ctrl+Space` to
// exit terminal mode to switch sessions. The bypass routes the
// key past `TerminalModeInputHandler` (which would otherwise
// forward it to the PTY child) and dispatches the action
// directly.
editor.registerCommand(
  "Orchestrator: Open",
  "Show all editor sessions in a floating selector",
  "orchestrator_open",
  null,
  { terminalBypass: true },
);
editor.registerCommand(
  "Orchestrator: New Session",
  "Spawn a new editor session in a worktree",
  "orchestrator_new",
  null,
  { terminalBypass: true },
);
editor.registerCommand(
  "Orchestrator: Kill Selected",
  "Close the session highlighted in the open Orchestrator prompt",
  "orchestrator_kill",
  null,
  { terminalBypass: true },
);
editor.registerCommand(
  "Orchestrator: Toggle Dock",
  "Show/hide the persistent left session dock (↑↓ switches windows live)",
  "orchestrator_dock_toggle",
  null,
  { terminalBypass: true },
);
