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
  activate,
  button,
  col,
  dropdown,
  flexSpacer,
  FloatingWidgetPanel,
  hintBar,
  divider,
  key as widgetKey,
  labeledSection,
  list,
  raw,
  row,
  wrappingRow,
  overlay,
  spacer,
  styledRow,
  text,
  textInputChar,
  toggle,
  tree,
  treeNode,
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
  // Opportunistically-gathered GitHub PR summary for this session's
  // branch (via `gh`), shown on the pill's third line. `undefined`
  // until first probed; see `PrProbe` for the lifecycle.
  pr?: PrProbe;
  // Opportunistically-gathered local git summary (ahead/behind +
  // working-tree diffstat) for the pill's second line — useful even
  // before there's a PR. Same lifecycle as `pr`.
  git?: GitProbe;
  // Wall-clock ms when this session last became the active window. Used
  // to suppress the terminal's activation redraw from registering as
  // agent activity (so selecting a session doesn't flash it `working`).
  activatedAt?: number;
  // Optional remote/cloud facet — present only for sessions whose backend is
  // not local (ssh / kubernetes / devcontainer). Drives the extra state glyph
  // + label in the picker/dock; absent for local sessions, which render
  // exactly as before (the design's "backend-opaque" facet — see
  // K8S_WORKSPACE_UX_DESIGN.md §"Orchestrator integration").
  remote?: {
    kind: SessionBackend;
    // Short human identity for the row (e.g. `deploy@build-01`, `ns/pod`).
    detail: string;
    state: "starting" | "running" | "stopped" | "error";
  };
  // Present only on a synthetic placeholder row that stands in for a
  // workspace still being created in the background (the non-blocking
  // New-Workspace flow — see `startPendingWorkspace`). While set, the row
  // has no live window (synthetic negative `id`, `terminalId` null) and
  // renders its `pending` status in place of a live pill. On success the
  // placeholder is dropped and the real live window takes its place; on
  // failure `phase` flips to `"error"` and the row offers retry / dismiss.
  pending?: PendingCreate;
}

// A workspace-create request captured off the (now-closing) New-Workspace
// form so the background worker — and any later retry — never has to read
// form state that no longer exists. One variant per submit path.
type RemoteFacet = NonNullable<AgentSession["remote"]>;
type CreateSpec =
  | {
      backend: "local";
      // Directory the session roots at (typed value or resolved default).
      projectPath: string;
      // Explicit workspace name; "" ⇒ auto-generate at create time.
      name: string;
      // Agent command; "" ⇒ a bare terminal.
      cmd: string;
      // Enable the agent's auto/reduced-approval mode (adds the agent's
      // documented flag, e.g. `claude --permission-mode auto`). Only honoured
      // for a command that resolves to a known agent with an `auto` flag;
      // ignored for a bare terminal / unknown command.
      auto: boolean;
      // Initial prompt to hand the agent at launch (positional or via the
      // agent's prompt flag). "" ⇒ no prompt. Only applied to a resolved agent
      // that documents a prompt argument; never replayed on resume.
      startPrompt: string;
      // Inject the Fresh CLI system prompt + mint a capability token so the
      // agent can drive the editor from the shell. Only honoured for a command
      // that resolves to an agent with a `systemPrompt` strategy.
      teachFreshCli: boolean;
      // "Checkout branch": an existing branch/ref to check out (worktree) or
      // switch to (in-place). "" ⇒ the detected default branch (worktree only).
      branch: string;
      // "New branch name": when set, create the worktree on a freshly-cut
      // branch off the checkout branch (or default). "" ⇒ no new branch.
      // Ignored in the non-worktree (in-place checkout) path.
      newBranch: string;
      // Create a fresh worktree (only honoured when the path is a git tree).
      createWorktree: boolean;
      // Row label + project shown on the pending dock row.
      displayLabel: string;
      displayProject: string;
    }
  | {
      backend: "ssh" | "kubernetes";
      // The host payload handed to `attachRemoteAgent`.
      spec: RemoteAgentSpec;
      // Facet stamped on both the born window and the pending placeholder.
      facet: RemoteFacet;
      displayLabel: string;
      displayProject: string;
      // The command to persist as `orchestrator.last_cmd` on success (ssh
      // remembers it), or "" to persist nothing.
      persistCmd: string;
    };

interface PendingCreate {
  // `"creating"` while the background create/connect runs; `"error"` once it
  // has failed; `"paused"` for a row restored from a previous session that
  // hasn't been resumed yet. Both `error` and `paused` offer retry / dismiss.
  phase: "creating" | "error" | "paused";
  // Human status shown on the row while creating, or the failure reason
  // when `phase === "error"`.
  message: string;
  // Everything the worker needs to (re-)run the create.
  spec: CreateSpec;
  // `true` when the user chose "Create & Visit": once the workspace is real,
  // focus follows into it (dive) instead of staying put. `false` for "Create
  // in Background" (and for restored/resumed rows — a relaunch never yanks
  // focus). Either way the create itself is non-blocking.
  visit: boolean;
}

// Local git summary + freshness bookkeeping (mirrors `PrProbe`).
interface GitProbe {
  status: "loading" | "ok";
  fetchedAt: number;
  info?: GitStat;
}

// Compact working-tree + branch-position summary, from
// `git status --porcelain=v2 --branch` and `git diff --shortstat HEAD`.
interface GitStat {
  branch?: string;
  // Commits ahead / behind the upstream (or base), when an upstream
  // is configured.
  ahead?: number;
  behind?: number;
  // Count of dirty paths (staged + unstaged + untracked).
  dirty?: number;
  // Uncommitted line churn vs HEAD.
  added?: number;
  deleted?: number;
}

// PR info gathered for a session, plus the freshness bookkeeping that
// keeps the probe opportunistic (never blocks a render, refreshes on a
// slow cadence). `status` distinguishes "haven't looked", "looking",
// "looked, no PR / gh unavailable", and "have a PR".
interface PrProbe {
  status: "loading" | "none" | "ok";
  // Wall-clock ms of the last completed probe (success or "none"),
  // used to throttle re-probes.
  fetchedAt: number;
  info?: PrInfo;
}

// The subset of `gh pr view --json …` we surface. All optional so a
// partial/older `gh` still renders what it can.
interface PrInfo {
  number: number;
  // "OPEN" | "MERGED" | "CLOSED".
  state?: string;
  isDraft?: boolean;
  // "APPROVED" | "CHANGES_REQUESTED" | "REVIEW_REQUIRED" | "".
  reviewDecision?: string;
  // gh's MERGEABLE / CONFLICTING / UNKNOWN.
  mergeable?: string;
  // Rolled-up CI: counts derived from statusCheckRollup.
  checksPass?: number;
  checksFail?: number;
  checksPending?: number;
  // Count of (review) comments.
  comments?: number;
}

// =============================================================================
// Module state — editor-global, survives every dive.
// =============================================================================

const orchestratorSessions = new Map<number, AgentSession>();

// Permanent display slot for each session, keyed by its canonical root
// (NOT its id — a discovered worktree keeps its slot when it opens and its
// negative id is swapped for a live window id). The dock orders rows by
// this slot ALONE, so the list never reshuffles: a row's position is fixed
// the first time its root is seen and never depends on the session's label,
// project, activity state, or which window is currently active. New roots
// append at the bottom. See `stableOrderKey` / the dock branch of
// `filterSessions`.
const rootDisplayOrder = new Map<string, number>();
let nextRootDisplayOrder = 0;
function stableOrderKey(s: AgentSession): number {
  const key = normRoot(s.root);
  let order = rootDisplayOrder.get(key);
  if (order === undefined) {
    order = nextRootDisplayOrder++;
    rootDisplayOrder.set(key, order);
  }
  return order;
}

// Facet to stamp onto the next born-attached remote window when it surfaces in
// `reconcileSessions` (via the core `window_created` hook). Set just before
// `attachRemoteAgent({ window: true })`, consumed by the first new live window.
let pendingRemoteFacet: AgentSession["remote"] | null = null;

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

// Pending (being-created) placeholder rows take ids from a range well
// below the discovered-worktree ids (which count down from `-2`), so the
// two synthetic id spaces can never collide in `orchestratorSessions`.
let nextPendingId = -1_000_000;
function allocPendingId(): number {
  return nextPendingId--;
}

// Only one remote attach may be in flight at a time. The host's
// `cancelRemoteAgent()` cancels *every* in-flight connect, so
// backgrounding two concurrent remote creates would let cancelling one
// tear down the other. Remote pending rows therefore run one at a time:
// `remoteAttachBusy` gates the in-flight connect and `remoteCreateQueue`
// holds the pending ids waiting their turn. Local creates have no such
// constraint and run immediately, in parallel.
let remoteAttachBusy = false;
const remoteCreateQueue: number[] = [];
// The pending id whose remote connect is currently in flight (null when
// none). Dismissing exactly this row must tear the connect down via
// `cancelRemoteAgent`; a queued row hasn't started one.
let remoteInFlightId: number | null = null;

// New-session form state. `null` ⇒ the floating form isn't
// open. Each field's `value` + `cursor` mirrors what the host
// renders inside the panel's TextInput widgets; the `submitting`
// flag debounces double-Enter on the Create button; `lastError`
// is rendered as a styled error row inside the form when the
// most recent submit failed (status bar would get clobbered —
// see MEMORY.md).
// Where a new session runs. `local` is today's worktree/folder flow; the
// other three sit behind the `Authority` seam (SSH remote host, a Kubernetes
// pod via `kubectl exec`, or a devcontainer). The New Session dialog shows a
// "Run in:" tab row so the user picks one and the body swaps to its fields.
type SessionBackend = "local" | "ssh" | "kubernetes" | "devcontainer";

const SESSION_BACKENDS: { id: SessionBackend; label: string; key: string }[] = [
  { id: "local", label: editor.t("backend.local"), key: "type-local" },
  { id: "ssh", label: editor.t("backend.ssh"), key: "type-ssh" },
  { id: "kubernetes", label: editor.t("backend.kubernetes"), key: "type-kubernetes" },
  { id: "devcontainer", label: editor.t("backend.devcontainer"), key: "type-devcontainer" },
];

interface NewSessionForm {
  // Which backend the session runs in (the "Run in:" tab selection). Drives
  // which field set `buildFormSpec` renders and which submit path runs.
  backend: SessionBackend;
  // --- SSH backend fields (rendered only when backend === "ssh") ---
  // Host as `host`, `user@host[:port]`, or a pasted `ssh://…` (user optional);
  // remote path to root the session at; optional identity file; and free-form
  // extra ssh arguments (e.g. `-J jump`, `-o ProxyCommand=…`).
  sshHost: { value: string; cursor: number };
  sshPath: { value: string; cursor: number };
  sshIdentity: { value: string; cursor: number };
  sshOptions: { value: string; cursor: number };
  // --- Kubernetes backend fields (rendered only when backend ===
  // "kubernetes") ---. `k8sTarget` names a target from `.fresh/k8s.json`;
  // when empty, the explicit context/namespace/pod/workspace fields are used
  // to attach directly (no config needed).
  k8sTarget: { value: string; cursor: number };
  k8sContext: { value: string; cursor: number };
  k8sNamespace: { value: string; cursor: number };
  k8sPod: { value: string; cursor: number };
  k8sWorkspace: { value: string; cursor: number };
  // Project Path: the directory the session is rooted at. When
  // `createWorktree` is true (default for git paths) this is
  // the *base* repo for `git worktree add`. When false, this
  // is the session root itself (no git interaction).
  projectPath: { value: string; cursor: number };
  name: { value: string; cursor: number };
  cmd: { value: string; cursor: number };
  // Initial prompt handed to a supporting agent at launch (a single-line box,
  // shown only for the local backend where agent launch is wired). Ignored for
  // a bare terminal / an agent with no prompt argument.
  startPrompt: { value: string; cursor: number };
  // "Auto mode" toggle — adds the resolved agent's auto/reduced-approval flag
  // to the launch (and resume) argv. Only meaningful for an agent that
  // documents such a flag; the checkbox is hidden otherwise.
  autoMode: boolean;
  // "Teach Fresh CLI" toggle — injects a system prompt teaching the agent the
  // `fresh` CLI and mints a capability token so it can drive the editor. Only
  // meaningful for an agent with a `systemPrompt` injection strategy; the
  // checkbox is hidden otherwise.
  teachFreshCli: boolean;
  branch: { value: string; cursor: number };
  // "New branch name" field (Advanced): when set, the new worktree is
  // created on a freshly-cut branch (`git worktree add -b <newBranch>`).
  // Empty ⇒ use the "Checkout branch" value (or the default) instead.
  newBranch: { value: string; cursor: number };
  // Whether the collapsible "Advanced…" section (worktree toggle +
  // branch fields) is expanded. Starts collapsed on every open.
  advancedExpanded: boolean;
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

// The "New Folder" dialog — a small centered floating panel with a name
// field, an "organize the current session under it" checkbox, and
// Cancel / Create Folder buttons. Replaces the old bottom-of-screen
// minibuffer prompt (and works identically in the web UI, which renders
// the same WidgetSpec). Enter submits (via a one-binding editor mode);
// Esc / Cancel dismiss.
const CREATE_FOLDER_MODE = "orchestrator-folder-dialog";
interface CreateFolderDialogState {
  // The name field: mirror of the host TextInput's value + cursor byte.
  name: { value: string; cursor: number };
  // Checkbox: file `sessionId` under the new folder on create. Default
  // true. Only shown when `sessionId` resolves to a live session.
  organizeCurrent: boolean;
  // Parent folder for the new folder (null = top level; a folder id for
  // the "New Subfolder…" path).
  parent: string | null;
  // The session the checkbox organizes — the active window for the
  // toolbar / subfolder paths, or the explicitly-moved session for the
  // "Move to → New folder…" path. null ⇒ no session to organize.
  sessionId: number | null;
  // Rename mode: the id of an existing folder this dialog renames
  // instead of creating a new one (title/button change, the organize
  // checkbox is dropped, and submit renames). Sharing the dialog keeps
  // create and rename the same UX rather than bouncing rename through
  // the bottom minibuffer prompt. null ⇒ create mode.
  renameId: string | null;
}
let createFolderDialog: CreateFolderDialogState | null = null;
let createFolderPanel: FloatingWidgetPanel | null = null;
// Mirror of the dialog's focused widget key, kept in sync from the
// host's authoritative `focus` widget_events. The dialog's mode-level
// Enter binding submits from anywhere — except when focus sits on the
// Cancel button, where Enter must cancel (pressing Enter on a focused
// Cancel that *creates* the folder is exactly backwards).
let createFolderFocusKey = "folder-name";

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
  // Dock-only: the screen row where the session tree starts (the rows
  // of chrome above it — title, toolbar, filters, divider). Captured in
  // `buildDockSpec` so keyboard-opened context menus can anchor near
  // the highlighted node without re-deriving the chrome layout.
  dockTreeTop: number;
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
  // Dock-only project filter. `null` shows every project's sessions
  // (the default); a project key restricts the dock list to that one
  // project. Driven by the dock toolbar's project dropdown (which
  // lists every project that has a session in the worktree/trivial-
  // filtered set). Independent of `scope`, which governs the modal
  // picker; the dock uses this instead.
  projectFilter: string | null;
  // `true` while the dock project dropdown overlay is open.
  projectMenuOpen: boolean;
  // Keyboard cursor (highlighted row) within the open project
  // dropdown, indexing `projectMenuKeys()` (0 = "All projects").
  // Distinct from `projectFilter`, which is the *applied* scope shown
  // with a `●`; this is just where ↑/↓ currently sit before Enter
  // commits. Only meaningful while `projectMenuOpen`.
  projectMenuIndex: number;
  // Dock-only: the collapsible "Filters" section under the toolbar is
  // expanded. When false the view/project/worktree/trivial controls are
  // hidden, leaving just the "New Task…" dropdown and the search input.
  filtersExpanded: boolean;
  // Dock-only: a transient toolbar dropdown (the "New Task…" create menu
  // or a session's "Move to folder…" menu), or null when none is open.
  // Keyboard-navigated via the shared `dock_menu_*` events (the host
  // routes them here while focus sits on a `menu-pick:` option button).
  dockMenu: DockDropdown | null;
  // Dock-only: the item key (`folder:<id>` / `session:<id>`) of the
  // currently-highlighted tree node. The tree is host-owned; this mirror
  // — updated from every `select` event — is how the dock resolves what
  // Enter / a click / a right-click acts on, since a tree node index no
  // longer maps 1:1 to a session in `filteredIds`.
  dockSelKey: string | null;
  // Dock-only: the flat node model (folders + session leaves) mirroring
  // the last-emitted tree, and the parallel item keys. Rebuilt on every
  // `buildDockSpec`; lets `dockSelKey` be resolved to a node/index.
  dockNodes: DockNode[];
  dockKeys: string[];
}

// A transient dock toolbar dropdown. `index` is the keyboard cursor into
// the menu's option list. `move` also carries the session being filed.
type DockDropdown =
  | { kind: "new"; index: number }
  | { kind: "move"; sessionId: number; index: number };
let openDialog: OpenDialogState | null = null;
let openPanel: FloatingWidgetPanel | null = null;
// The dock panel kept alive in its own host slot (PanelSlot::Dock) while
// the modal Open picker is floated over it (PanelSlot::Floating). When
// non-null, `openPanel` is the picker and this is the dock underneath;
// closing the picker hands control back to it (`restoreDockBehindPicker`)
// rather than tearing everything down. The host renders both slots, so
// the dock stays visible (dimmed + passive) in its left column beside
// the picker.
let dockPanel: FloatingWidgetPanel | null = null;
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
// Right-click context menu for a dock session row. At `stage: "menu"`
// it's an unobtrusive content-sized popup anchored at the click (no
// background dim) offering Visit / Archive / Delete against one session.
// Choosing Archive/Delete swaps the SAME panel to a centered, dimmed
// `stage: "confirm"` modal (the destructive actions require a
// confirmation), reusing `buildConfirmPane`. Visit acts immediately.
// `anchorCol`/`anchorRow` are the right-click cell, kept so a return
// from confirm→menu (Cancel) re-anchors the popup where it opened.
// The right-clicked target: a session (Visit / Move to folder… /
// Archive / Delete) or a user folder (Rename / New Subfolder / Delete
// Folder). The confirm stage only ever applies to a session's
// destructive Archive/Delete.
type DockMenuTarget =
  | { kind: "session"; id: number }
  | { kind: "folder"; id: string };
type DockMenuState =
  | { target: DockMenuTarget; anchorCol: number; anchorRow: number; stage: "menu" }
  | {
      target: { kind: "session"; id: number };
      anchorCol: number;
      anchorRow: number;
      stage: "confirm";
      action: "archive" | "delete";
    };
let dockMenuPanel: FloatingWidgetPanel | null = null;
let dockMenuState: DockMenuState | null = null;
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
// renders something. Drives session name/tag truncation. (The header
// divider is host-rendered via `divider()`, so it no longer needs this.)
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
// Dock card density, remembered across opens. "card" (default) shows
// the three-line rounded pill; "compact" shows one line per session.
// Toggled by the dock toolbar's "view" button.
let dockView: "card" | "compact" = "card";
// Remembered dock project filter (see `OpenDialogState.projectFilter`).
let lastDockProjectFilter: string | null = null;

// =============================================================================
// Dock folder tree
//
// The dock presents sessions in a user-organised hierarchy: arbitrary
// folders the user creates to group, nest, and arrange their agent
// sessions and remote connections however they like. Folders and the
// session→folder assignment are editor-global plugin state (the dock
// lists every project's sessions, so the organisation is global, not
// per-project) and persist across restarts.
//
// A session is assigned by its *canonical root path*, not its numeric
// window id: the id churns (a discovered on-disk worktree carries a
// synthetic negative id that becomes a positive window id the moment it
// is opened) and isn't stable run-to-run, whereas the root is the
// session's durable identity (§3 of orchestrator-sessions.md).
// =============================================================================

interface DockFolder {
  id: string; // stable unique id, e.g. "df3"
  name: string;
  parent: string | null; // parent folder id; null = top level
}

const FOLDERS_KEY = "orchestrator.dock.folders";
const ASSIGN_KEY = "orchestrator.dock.assignments";
const EXPANDED_KEY = "orchestrator.dock.expanded";
const FOLDER_COUNTER_KEY = "orchestrator.dock.folder_counter";

const FOLDER_NODE_PREFIX = "folder:";
const SESSION_NODE_PREFIX = "session:";
const FOLDER_GLYPH = "▤";

// Lazily-hydrated in-memory caches, written through to global state on
// every mutation so a later read (or the next launch) sees the change.
let dockFolders: DockFolder[] | null = null;
let dockAssign: Record<string, string> | null = null;
let dockExpanded: Set<string> | null = null;

function loadFolders(): DockFolder[] {
  if (dockFolders) return dockFolders;
  const raw = editor.getGlobalState(FOLDERS_KEY);
  const out: DockFolder[] = [];
  if (Array.isArray(raw)) {
    for (const e of raw) {
      if (e && typeof e === "object") {
        const rec = e as Record<string, unknown>;
        const id = rec.id;
        const name = rec.name;
        const parent = rec.parent;
        if (typeof id === "string" && typeof name === "string") {
          out.push({ id, name, parent: typeof parent === "string" ? parent : null });
        }
      }
    }
  }
  dockFolders = out;
  return out;
}

function saveFolders(): void {
  editor.setGlobalState(FOLDERS_KEY, (dockFolders ?? []) as unknown as object);
}

function loadAssign(): Record<string, string> {
  if (dockAssign) return dockAssign;
  const raw = editor.getGlobalState(ASSIGN_KEY);
  const out: Record<string, string> = {};
  if (raw && typeof raw === "object" && !Array.isArray(raw)) {
    for (const [k, v] of Object.entries(raw as Record<string, unknown>)) {
      if (typeof v === "string") out[k] = v;
    }
  }
  dockAssign = out;
  return out;
}

function saveAssign(): void {
  editor.setGlobalState(ASSIGN_KEY, (dockAssign ?? {}) as unknown as object);
}

function loadExpanded(): Set<string> {
  if (dockExpanded) return dockExpanded;
  const raw = editor.getGlobalState(EXPANDED_KEY);
  const out = new Set<string>();
  if (Array.isArray(raw)) {
    for (const v of raw) if (typeof v === "string") out.add(v);
  }
  dockExpanded = out;
  return out;
}

function saveExpanded(): void {
  editor.setGlobalState(
    EXPANDED_KEY,
    Array.from(dockExpanded ?? new Set<string>()) as unknown as object,
  );
}

function allocFolderId(): string {
  const raw = editor.getGlobalState(FOLDER_COUNTER_KEY);
  const n = (typeof raw === "number" && raw >= 0 ? Math.floor(raw) : 0) + 1;
  editor.setGlobalState(FOLDER_COUNTER_KEY, n as unknown as object);
  return `df${n}`;
}

function folderNodeKey(id: string): string {
  return FOLDER_NODE_PREFIX + id;
}
function sessionNodeKey(id: number): string {
  return SESSION_NODE_PREFIX + id;
}

// The durable key a session is filed under (see the section header).
function sessionAssignKey(s: AgentSession): string {
  return normRoot(s.root);
}

function folderById(id: string): DockFolder | undefined {
  return loadFolders().find((f) => f.id === id);
}

// Direct child folders of `parent` (null = top level), sorted by name so
// the tree order is stable and predictable.
function childFoldersOf(parent: string | null): DockFolder[] {
  return loadFolders()
    .filter((f) => (f.parent ?? null) === parent)
    .sort((a, b) => {
      const la = a.name.toLowerCase();
      const lb = b.name.toLowerCase();
      if (la !== lb) return la < lb ? -1 : 1;
      return a.id < b.id ? -1 : 1;
    });
}

// The folder a live session is filed under, or null when it is unfiled
// or its folder was deleted out from under it (treated as top level).
function folderOfSession(id: number): string | null {
  const s = orchestratorSessions.get(id);
  if (!s) return null;
  const a = loadAssign()[sessionAssignKey(s)];
  return a && folderById(a) ? a : null;
}

function createFolder(name: string, parent: string | null): string {
  const id = allocFolderId();
  loadFolders().push({ id, name, parent });
  saveFolders();
  // New folders open expanded so their contents are immediately visible.
  loadExpanded().add(folderNodeKey(id));
  saveExpanded();
  return id;
}

function renameFolder(id: string, name: string): void {
  const f = folderById(id);
  if (f) {
    f.name = name;
    saveFolders();
  }
}

// Delete a folder. Its child folders and member sessions reparent to the
// deleted folder's own parent so nothing is orphaned — the subtree
// bubbles up one level rather than disappearing.
function deleteFolder(id: string): void {
  const f = folderById(id);
  if (!f) return;
  const parent = f.parent ?? null;
  const folders = loadFolders();
  for (const c of folders) {
    if ((c.parent ?? null) === id) c.parent = parent;
  }
  dockFolders = folders.filter((x) => x.id !== id);
  saveFolders();
  const assign = loadAssign();
  for (const [k, v] of Object.entries(assign)) {
    if (v === id) {
      if (parent) assign[k] = parent;
      else delete assign[k];
    }
  }
  saveAssign();
  loadExpanded().delete(folderNodeKey(id));
  saveExpanded();
}

function assignSessionToFolder(id: number, folderId: string | null): void {
  const s = orchestratorSessions.get(id);
  if (!s) return;
  const assign = loadAssign();
  const key = sessionAssignKey(s);
  if (folderId) assign[key] = folderId;
  else delete assign[key];
  saveAssign();
}

// A flat, depth-first traversal of the dock hierarchy: each entry is
// either a user folder or a session leaf, in render order. Built by
// `buildDockTree`, mirrored 1:1 with the emitted `TreeNode[]`.
type DockNode =
  | { kind: "folder"; folderId: string }
  | { kind: "session"; sessionId: number };

interface DockTree {
  nodes: TreeNode[];
  keys: string[];
  model: DockNode[];
}

// Turn the filtered session list into the tree the dock renders:
// user folders (nested) with their member sessions, then the ungrouped
// sessions at top level. When a search is active, folders with no
// matching descendant are dropped so results aren't buried under empty
// folders.
function buildDockTree(filtered: number[], activeId: number): DockTree {
  const nodes: TreeNode[] = [];
  const keys: string[] = [];
  const model: DockNode[] = [];

  const membersByFolder = new Map<string | null, number[]>();
  for (const id of filtered) {
    const fid = folderOfSession(id);
    const arr = membersByFolder.get(fid) ?? [];
    arr.push(id);
    membersByFolder.set(fid, arr);
  }
  const countRec = (fid: string): number => {
    let n = (membersByFolder.get(fid) ?? []).length;
    for (const c of childFoldersOf(fid)) n += countRec(c.id);
    return n;
  };
  const searching = (openDialog?.filter.value ?? "") !== "";

  const emitFolder = (f: DockFolder, depth: number): void => {
    nodes.push(
      treeNode(folderNodeEntry(f, countRec(f.id)), { depth, hasChildren: true }),
    );
    keys.push(folderNodeKey(f.id));
    model.push({ kind: "folder", folderId: f.id });
  };
  // "card" density (dock only) renders session leaves as fixed-height
  // multi-row cards inside a rounded border (`cardBorders` on the
  // tree); "compact" keeps the single-line row. Folders stay one
  // single-row line either way.
  const card = dockMode && dockView === "card";
  const emitSession = (id: number, depth: number): void => {
    const primary = card ? sessionCardPrimary(id, activeId) : sessionNodeEntry(id, activeId);
    nodes.push(
      treeNode(primary, {
        depth,
        hasChildren: false,
        extraLines: card ? sessionCardExtraLines(id) : undefined,
      }),
    );
    keys.push(sessionNodeKey(id));
    model.push({ kind: "session", sessionId: id });
  };
  const walk = (parent: string | null, depth: number): void => {
    for (const f of childFoldersOf(parent)) {
      if (searching && countRec(f.id) === 0) continue;
      emitFolder(f, depth);
      walk(f.id, depth + 1);
      for (const sid of membersByFolder.get(f.id) ?? []) emitSession(sid, depth + 1);
    }
  };
  walk(null, 0);
  for (const sid of membersByFolder.get(null) ?? []) emitSession(sid, 0);

  return { nodes, keys, model };
}

// One tree row for a folder: a folder glyph, the (bold) name, and the
// recursive session count in dim parentheses.
function folderNodeEntry(f: DockFolder, count: number): TextPropertyEntry {
  const segs: Entry[] = [
    { text: FOLDER_GLYPH + " ", style: { fg: "ui.menu_disabled_fg" } },
    { text: f.name, style: { bold: true } },
  ];
  if (count > 0) {
    segs.push({ text: `  (${count})`, style: { fg: "ui.menu_disabled_fg" } });
  }
  return styledRow(segs as Parameters<typeof styledRow>[0]);
}

// ── Pending placeholder row presentation (single source of truth) ─────────
//
// A being-created placeholder is drawn on four surfaces with different
// shapes: the compact tree line (`sessionNodeEntry`), the card continuation
// lines (`sessionCardExtraLines`), the modal pill (`renderPendingPillSpec`),
// and the preview pane (`buildPreviewEntries`). The *shape* differs per
// surface, but the phase → colour / affordance mapping must not: these
// helpers own it so the surfaces can't drift (they had — creating rows
// disagreed on the hint and on whether the message italicised).

// Message colour: red once the create has failed, amber while it is still
// creating or is paused (interrupted, awaiting resume).
function pendingMsgFg(p: PendingCreate): string {
  return p.phase === "error" ? "ui.status_error_indicator_fg" : "diagnostic.warning_fg";
}

// `error` and `paused` are actionable — Enter retries / resumes them — while
// `creating` is passive (the create is running; the only action is Dismiss).
function pendingActionable(p: PendingCreate): boolean {
  return p.phase !== "creating";
}

// The one-line action hint for a pending row's phase: retry/resume when
// actionable, otherwise the "you can still dismiss this" affordance.
function pendingHintText(p: PendingCreate): string {
  return pendingActionable(p)
    ? editor.t("dock.pending_retry_hint")
    : editor.t("dock.pending_dismiss_hint");
}

// One tree row for a session leaf: state glyph, optional remote facet,
// and the name (highlighted when it's the active window). A single
// line — the tree owns indentation and the disclosure column, so the
// rich two-line PR pill of the modal picker is traded for a compact,
// nestable row here. The branch is deliberately dropped in this density
// (it's the "compact" trade — card view carries it on its second line).
function sessionNodeEntry(id: number, activeId: number): TextPropertyEntry {
  const s = orchestratorSessions.get(id);
  if (!s) return styledRow([{ text: editor.t("pill.unknown") }]);
  const isActive = id === activeId;
  const segs: Entry[] = [stateGlyphEntry(s)];
  if (s.remote) {
    segs.push({
      text: REMOTE_GLYPH[s.remote.kind] + " ",
      style: { fg: remoteStateFg(s.remote.state), bold: true },
    });
  }
  segs.push({
    text: s.label,
    style: { fg: isActive ? "ui.help_key_fg" : undefined, bold: true },
  });
  // A remote session surfaces its backend target (host / ns·pod), coloured
  // by the connection state — the same detail the pill shows on the right.
  if (s.remote) {
    segs.push({
      text: "  " + s.remote.detail,
      style: { fg: remoteStateFg(s.remote.state), italic: true },
    });
  }
  // A discovered on-disk worktree keeps its "· on-disk" tag — the "this
  // row isn't an open session yet" indicator the pill also shows.
  if (s.discovered) {
    segs.push({
      text: "  " + editor.t("pill.on_disk_worktree"),
      style: { fg: "ui.menu_disabled_fg", italic: true },
    });
  }
  // A being-created placeholder trails its status on the single compact line
  // (there's no second row to put it on).
  if (s.pending) {
    segs.push({
      text: "  " + s.pending.message,
      style: { fg: pendingMsgFg(s.pending), italic: true },
    });
  }
  return styledRow(segs as Parameters<typeof styledRow>[0]);
}

// The dock's "card" density renders each session leaf as a fixed
// 3-content-row card inside the tree; with `cardBorders` the host
// wraps those rows in a rounded `╭─…─╮` border (5 screen rows total),
// restoring the modal picker's pill look: line 1 = glyph · [facet] ·
// NAME + project; line 2 = branch + git summary (right-aligned against
// the card border); line 3 = PR badge (blank when none).
const DOCK_CARD_HEIGHT = 3;

// Card line 1 (the tree node's primary text): state glyph, optional
// remote facet, the name (highlighted when active), then a dim project
// tag. Distinct from the compact `sessionNodeEntry`, which trails the
// branch on the single line instead of the project.
function sessionCardPrimary(id: number, activeId: number): TextPropertyEntry {
  const s = orchestratorSessions.get(id);
  if (!s) return styledRow([{ text: editor.t("pill.unknown") }]);
  const isActive = id === activeId;
  const segs: Entry[] = [stateGlyphEntry(s)];
  if (s.remote) {
    segs.push({
      text: REMOTE_GLYPH[s.remote.kind] + " ",
      style: { fg: remoteStateFg(s.remote.state), bold: true },
    });
  }
  segs.push({
    text: s.label,
    style: { fg: isActive ? "ui.help_key_fg" : undefined, bold: true },
  });
  const proj = editor.pathBasename(projectKeyOf(s));
  segs.push({ text: "  " + PROJECT_ICON + " ", style: { fg: "ui.menu_disabled_fg" } });
  segs.push({ text: proj, style: { fg: "ui.menu_disabled_fg", italic: true } });
  // A remote session surfaces its backend target (host / ns·pod) coloured
  // by the connection state — pill parity (the pill shows it at the right
  // end of line 1). The discovered "· on-disk" tag is NOT repeated here:
  // the card's third line (prLineEntries) already carries it, exactly like
  // the pill.
  if (s.remote) {
    segs.push({
      text: "  " + s.remote.detail,
      style: { fg: remoteStateFg(s.remote.state), italic: true },
    });
  }
  return styledRow(segs as Parameters<typeof styledRow>[0]);
}

// Card lines 2 & 3 (continuation rows). Line 2 is the branch + a
// compact git summary, right-aligned as one group against the card's
// right border; line 3 is the PR badge (or a blank spacer when there's
// no PR, keeping every card the same height). Tree card rows are plain
// text entries (no host flex spacer), so the line carries the
// `align: "right"` entry property and the host's `render_tree_card` —
// which knows the card's *actual* inner width, responsive or dragged —
// pads it flush against the right border.
function sessionCardExtraLines(id: number): TextPropertyEntry[] {
  const s = orchestratorSessions.get(id);
  if (!s) return [];
  // A being-created placeholder shows its status in place of the git / PR
  // lines: line 2 is the creating/connecting/error message, line 3 is a
  // retry/resume hint (blank while still creating).
  if (s.pending) {
    const p = s.pending;
    const actionable = pendingActionable(p);
    return [
      styledRow([{ text: p.message, style: { fg: pendingMsgFg(p), italic: actionable } }]),
      styledRow([
        actionable
          ? { text: pendingHintText(p), style: { fg: "ui.menu_disabled_fg", italic: true } }
          : { text: " " },
      ] as Parameters<typeof styledRow>[0]),
    ];
  }
  const git = gitLineParts(s);
  const gitSegs: Entry[] = [...git.left];
  if (git.right.length) {
    gitSegs.push({ text: "   " });
    gitSegs.push(...git.right);
  }
  const gitLine = styledRow(gitSegs as Parameters<typeof styledRow>[0]);
  gitLine.properties = { align: "right" };
  const pr = prLineEntries(s);
  return [
    gitLine,
    styledRow((pr.length ? pr : [{ text: " " }]) as Parameters<typeof styledRow>[0]),
  ];
}

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

// Remote facet derived from the host's `WindowInfo.remote` backend identity.
// Present for SSH/Kubernetes sessions — including *dormant* ones restored
// from disk that have never connected this run — so their dock rows carry
// the backend glyph + detail and a disconnected ("stopped") state instead of
// masquerading as local sessions. Plugin-managed backends (devcontainer)
// carry no host facet; theirs still arrives via `pendingRemoteFacet`.
function backendFacet(info: WindowInfo): AgentSession["remote"] | undefined {
  if (!info.remote) return undefined;
  return {
    kind: info.remote.kind as SessionBackend,
    detail: info.remote.detail,
    state: info.remote.connected ? "running" : "stopped",
  };
}

// Window ids we've deliberately closed as part of a lifecycle action
// (Delete / Archive / Kill). `editor.closeWindow` is asynchronous — the
// host keeps reporting the window from `listWindows()` for a beat after we
// drop it from the model — so without this guard the very next
// `refreshOpenDialog` → `reconcileSessions` resurrects the workspace the
// user just deleted from that stale snapshot, leaving a deleted row
// lingering in the dock (and, whenever the healing reconcile lost the
// race, staying there for good). An id is removed from the set once the
// host confirms the close (the window leaves `listWindows()`), or by the
// `window_closed` hook.
const closingWindowIds = new Set<number>();

function reconcileSessions(): void {
  const editorSessions = editor.listWindows();
  const seen = new Set<number>();
  for (const s of editorSessions) {
    seen.add(s.id);
    // A window we just closed for a lifecycle action still shows up in the
    // host snapshot until the deferred close lands. Keep it out of the
    // model rather than re-adding the row the user just deleted.
    if (closingWindowIds.has(s.id)) {
      orchestratorSessions.delete(s.id);
      continue;
    }
    const existing = orchestratorSessions.get(s.id);
    if (!existing) {
      // A born-attached remote window (created by core after
      // `attachRemoteAgent({ window: true })`) surfaces here for the first
      // time. Core makes it the *active* window, so claim the pending facet
      // only for the active id — otherwise a pre-existing untracked window
      // processed first would wrongly grab it. Cleared once claimed.
      const remote =
        pendingRemoteFacet && s.id === editor.activeWindow()
          ? pendingRemoteFacet
          : undefined;
      if (remote) pendingRemoteFacet = null;
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
        remote: remote ?? backendFacet(s),
      });
    } else {
      existing.label = s.label;
      existing.root = s.root;
      existing.projectPath = s.project_path;
      if (s.shared_worktree != null) existing.sharedWorktree = s.shared_worktree;
      // Keep the backend facet in step with the host's view: adopt it when
      // missing (a dormant session that predates the facet, or one whose
      // plugin-side record was created before the snapshot carried it), and
      // track the connected/disconnected flip so a promoted or dropped
      // backend re-badges the row. A plugin-owned lifecycle state
      // ("starting") is left alone — it resolves through its own path.
      const facet = backendFacet(s);
      if (facet) {
        if (!existing.remote) {
          existing.remote = facet;
        } else if (existing.remote.state !== "starting") {
          existing.remote.state = facet.state;
        }
      }
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
  // Retire tombstones whose window the host has now actually closed: a
  // live window is added to `seen` above (before the tombstone skip), so
  // `!seen.has(id)` means the close landed and `listWindows()` no longer
  // reports it. Clearing it keeps a future window that reuses the id from
  // being wrongly suppressed.
  for (const id of [...closingWindowIds]) {
    if (!seen.has(id)) closingWindowIds.delete(id);
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
  // Quiet / waiting — a small dim dot. Deliberately understated: idle is
  // the resting state, so it shouldn't draw the eye the way a green
  // check (which reads as "done/success") did.
  idle: { glyph: "·", fg: "ui.menu_disabled_fg" },
};

// Width of the left status margin: glyph + trailing space.
const STATUS_MARGIN_W = 2;

// Remote/cloud facet glyphs — a per-backend mark prepended to a session row
// when it has a `remote` facet (ssh / kubernetes / devcontainer). Local
// sessions have no facet, so their rows are untouched.
const REMOTE_GLYPH: Record<SessionBackend, string> = {
  local: "",
  ssh: "⇅",
  kubernetes: "⎈",
  devcontainer: "⬢",
};

// Theme colour for a remote facet's state.
function remoteStateFg(state: "starting" | "running" | "stopped" | "error"): string {
  switch (state) {
    case "running":
      return "diagnostic.info_fg";
    case "starting":
      return "diagnostic.warning_fg";
    case "error":
      return "ui.status_error_indicator_fg";
    case "stopped":
      return "ui.menu_disabled_fg";
  }
}

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
  // Lock in each session's permanent display slot up front, in Map
  // insertion (first-seen) order, before any filtering or sorting. This
  // fixes the dock's row order the first time a session appears so it never
  // reshuffles afterward (see `stableOrderKey`).
  for (const id of allIds) stableOrderKey(orchestratorSessions.get(id)!);
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
      // Keep: the active session (you must see where you are), discovered
      // worktree rows (their own toggle governs them), and any remote
      // (SSH / k8s) session. A remote session is a live connection, never an
      // "empty restored shell" — and its persisted workspace records no local
      // terminal, so it looked trivial and got dropped the instant it stopped
      // being the active card. In the dock (a live switcher) that made the
      // first card vanish on the first ↓ and desynced the selection.
      if (s.discovered || id === activeId || s.remote) return true;
      const c = sessionContentByRoot.get(normRoot(s.root));
      return !c || !c.trivial;
    });
  }

  // Dock project dropdown: when a specific project is picked, hard-
  // restrict the list to that project (search included — the dock is a
  // per-project switcher once a project is chosen). `null` = all
  // projects, the default.
  if (dockMode && openDialog?.projectFilter) {
    const want = openDialog.projectFilter;
    // A being-created placeholder is always shown (its project isn't
    // meaningful until it resolves) so the user sees it appear.
    allIds = allIds.filter((id) => {
      const s = orchestratorSessions.get(id)!;
      return !!s.pending || projectKeyOf(s) === want;
    });
  }

  // Sort by (current-project-first, project, then a stable identity key)
  // so an "all" view groups the current project's sessions at the top and
  // other projects' below.
  //
  // Within a project the order is a *stable* identity key, deliberately
  // NOT the live/discovered state or the numeric id — a row must keep its
  // place when its session changes state. Opening a discovered on-disk
  // worktree turns it into a live session (and swaps its synthetic
  // negative id for a positive window id), but it should stay exactly
  // where it was instead of jumping into a "live" group and shuffling the
  // rows under you as you arrow-navigate. The key is:
  //   1. main checkout first — the project's own checkout (root ===
  //      projectPath) sits above its linked worktrees. This is stable:
  //      whether a worktree is discovered or open, its root never equals
  //      its projectPath, so it never crosses this boundary.
  //   2. then label, then root — alphabetical within each group.
  //   3. id only as a final tie-break for two otherwise-identical rows.
  //
  // The dock is persistent and switches the active session constantly, so
  // it must NOT reorder as the active project changes — it pins this
  // stable order. The modal picker, opened fresh each time, additionally
  // floats the current project to the top.
  const isWorktree = (s: AgentSession): number =>
    normRoot(s.root) === normRoot(projectKeyOf(s)) ? 0 : 1;
  const pinCurrentFirst = !dockMode;
  const byProjectThenStable = (a: number, b: number): number => {
    const sa = orchestratorSessions.get(a)!;
    const sb = orchestratorSessions.get(b)!;
    const aCur = projectKeyOf(sa) === cur ? 0 : 1;
    const bCur = projectKeyOf(sb) === cur ? 0 : 1;
    if (pinCurrentFirst && aCur !== bCur) return aCur - bCur;
    const ka = projectKeyOf(sa);
    const kb = projectKeyOf(sb);
    if (ka !== kb) return ka < kb ? -1 : 1;
    const wa = isWorktree(sa);
    const wb = isWorktree(sb);
    if (wa !== wb) return wa - wb;
    const la = sa.label.toLowerCase();
    const lb = sb.label.toLowerCase();
    if (la !== lb) return la < lb ? -1 : 1;
    if (sa.root !== sb.root) return sa.root < sb.root ? -1 : 1;
    return a - b;
  };

  if (!needle) {
    // The dock orders ONLY by each session's permanent slot — no project
    // grouping, no current-project pinning, no label/state keys — so the
    // list never reorders as the active window switches or a session's
    // fields change. The modal picker (opened fresh each time) keeps the
    // grouped, current-project-first browse order.
    const comparator = dockMode
      ? (a: number, b: number) =>
          stableOrderKey(orchestratorSessions.get(a)!) -
          stableOrderKey(orchestratorSessions.get(b)!)
      : byProjectThenStable;
    const ids = allIds.slice().sort(comparator);
    if (scope === "current") {
      // Keep being-created placeholders visible regardless of scope — they
      // were just launched and must show up in the list right away.
      return ids.filter((id) => {
        const s = orchestratorSessions.get(id)!;
        return !!s.pending || projectKeyOf(s) === cur;
      });
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
  // At equal relevance, a project's own checkout sorts before its
  // worktrees (same stable `isWorktree` grouping as the browse list), so
  // the on-disk / worktree rows still trail the project's own session in
  // search results — and a result doesn't jump when a discovered worktree
  // is opened (its root/projectPath, hence its group, don't change).
  matches.sort(
    (a, b) =>
      a.score - b.score ||
      isWorktree(orchestratorSessions.get(a.id)!) -
        isWorktree(orchestratorSessions.get(b.id)!) ||
      a.len - b.len ||
      a.id - b.id,
  );
  return matches.map((m) => m.id);
}

// Distinct project keys that have at least one session in the dock's
// worktree/trivial-filtered set — the menu contents for the dock's
// project dropdown. Deliberately ignores the active `projectFilter`
// (the menu must offer every project, not just the selected one) and
// any text filter. Sorted lexically for a stable menu order.
function dockProjectOptions(): string[] {
  const showWorktrees = openDialog?.showWorktrees ?? false;
  const hideTrivial = openDialog?.hideTrivial ?? false;
  const activeId = editor.activeWindow();
  const keys = new Set<string>();
  for (const [id, s] of orchestratorSessions) {
    if (!showWorktrees && s.discovered) continue;
    if (hideTrivial && !s.discovered && id !== activeId && !s.remote) {
      const c = sessionContentByRoot.get(normRoot(s.root));
      if (c && c.trivial) continue;
    }
    keys.add(projectKeyOf(s));
  }
  return Array.from(keys).sort();
}

// Header row above the session list: a single dim `NAME`, indented to
// sit over the per-row name (status margin + checkbox = STATUS_MARGIN_W
// + 4 cols). The status symbol lives in the left margin now, so there's
// no separate status column to label.
function sessionsColumnHeader(): WidgetSpec {
  const text = " ".repeat(STATUS_MARGIN_W + 4) + editor.t("list.col_name");
  return {
    kind: "raw",
    entries: [
      styledRow([{ text, style: { fg: "ui.menu_disabled_fg" } }]),
    ],
  };
}

// =============================================================================
// Pill (card) rendering — the richer multi-line list item
//
// Each session renders as a rounded `labeledSection` card with two
// content lines:
//
//   ╭────────────────────────────────────╮
//   │ * agent-auth              demo-proj │   line 1: status · name · project
//   │ #1287 ✓7/8 ●2 approved · merge ok   │   line 2: PR badge (or branch)
//   ╰────────────────────────────────────╯
//
// The host `list` widget lays these out one card per item (selection,
// scroll, and clicks all in item units) via the `itemSpecs` channel.
// =============================================================================

type Entry = { text: string; style?: Record<string, unknown> };

// Field icons (Alt-5 style) — a small glyph labels each field so its
// slot is unambiguous. `BRANCH_ICON` falls back gracefully: it's only
// decorative, and the branch name follows it.
const PROJECT_ICON = "▣";
// `▸` (not the git-fork `⎇`, which is absent from many monospace fonts
// and renders as tofu) — matches the branch marker the dock detail line
// already uses.
const BRANCH_ICON = "▸";

// Card line 2: branch (with icon, left) + a compact git summary
// (right-aligned) that's useful even before any PR exists —
// ahead/behind the upstream and the uncommitted diffstat
// (`+added −deleted`), or `clean`.
function gitLineParts(s: AgentSession): { left: Entry[]; right: Entry[] } {
  const dim = "ui.menu_disabled_fg";
  let branch = s.branch || (s.discovered ? editor.t("pill.branch_worktree") : editor.t("pill.branch_detached"));
  // Cap the branch so it doesn't push the right-aligned git summary off
  // the tail (the host truncates the row's *end*, which is the summary)
  // on a normal-width card. Very narrow docks may still clip it.
  const BRANCH_CAP = 28;
  if (branch.length > BRANCH_CAP) branch = branch.slice(0, BRANCH_CAP - 1) + "…";
  const left: Entry[] = [
    { text: BRANCH_ICON + " ", style: { fg: dim } },
    { text: branch, style: { fg: dim } },
  ];
  // The git summary is right-aligned on the line (see `renderPillSpec`),
  // so it's returned separately from the branch.
  const right: Entry[] = [];
  const sep = (): string => (right.length ? " " : "");
  const probe = s.git;
  // Keep the last good summary on screen while a re-poll is in flight
  // (only the very first probe, with no prior info, shows the spinner).
  const g = probe?.info;
  if (!g) {
    if (probe?.status === "loading") {
      right.push({ text: "…", style: { fg: dim, italic: true } });
    }
    return { left, right };
  }
  if (g.ahead && g.ahead > 0) {
    right.push({ text: `${sep()}↑${g.ahead}`, style: { fg: "ui.file_status_added_fg" } });
  }
  if (g.behind && g.behind > 0) {
    right.push({ text: `${sep()}↓${g.behind}`, style: { fg: "diagnostic.warning_fg" } });
  }
  if (g.added) {
    right.push({ text: `${sep()}+${g.added}`, style: { fg: "ui.file_status_added_fg" } });
  }
  if (g.deleted) {
    right.push({ text: `${sep()}−${g.deleted}`, style: { fg: "ui.file_status_deleted_fg" } });
  }
  if (right.length === 0) {
    right.push({ text: editor.t("pill.clean"), style: { fg: dim, italic: true } });
  }
  return { left, right };
}

// Card line 3: the GitHub PR badge (number, checks, comments, review,
// conflicts). Degrades to a dim placeholder when there's no PR yet,
// the probe is still running, or `gh` is unavailable.
function prLineEntries(s: AgentSession): Entry[] {
  const dim = "ui.menu_disabled_fg";
  const probe = s.pr;
  // Keep the last good badge on screen while a re-poll is in flight —
  // don't flash a placeholder over real data (only the very first probe,
  // with no prior info, shows the spinner).
  if (probe?.info && (probe.status === "ok" || probe.status === "loading")) {
    const p = probe.info;
    const out: Entry[] = [
      { text: editor.t("pill.pr_prefix"), style: { fg: dim } },
      { text: `#${p.number}`, style: { fg: "ui.help_key_fg", bold: true } },
    ];
    if (p.isDraft) out.push({ text: editor.t("pill.pr_draft"), style: { fg: dim } });
    if (p.checksFail && p.checksFail > 0) {
      out.push({ text: ` ✗${p.checksFail}`, style: { fg: "diagnostic.error_fg" } });
    } else if (p.checksPending && p.checksPending > 0) {
      out.push({ text: ` •${p.checksPending}`, style: { fg: "diagnostic.warning_fg" } });
    } else if (p.checksPass && p.checksPass > 0) {
      out.push({ text: ` ✓${p.checksPass}`, style: { fg: "ui.file_status_added_fg" } });
    }
    if (p.comments && p.comments > 0) {
      out.push({ text: ` ●${p.comments}`, style: { fg: dim } });
    }
    if (p.reviewDecision === "APPROVED") {
      out.push({ text: editor.t("pill.pr_approved"), style: { fg: "ui.file_status_added_fg" } });
    } else if (p.reviewDecision === "CHANGES_REQUESTED") {
      out.push({ text: editor.t("pill.pr_chg_req"), style: { fg: "diagnostic.warning_fg" } });
    }
    if (p.mergeable === "CONFLICTING") {
      out.push({ text: editor.t("pill.pr_conflicts"), style: { fg: "diagnostic.error_fg" } });
    }
    return out;
  }
  // A discovered on-disk worktree keeps its "· on-disk worktree" tag —
  // it's a useful "this row isn't an open session yet" indicator, not a
  // PR placeholder. A live session with no PR returns nothing here; the
  // caller renders a blank spacer line in its place (keeping the card a
  // uniform three lines) rather than a "no PR yet" placeholder.
  if (s.discovered) {
    return [{ text: editor.t("pill.on_disk_worktree"), style: { fg: dim, italic: true } }];
  }
  return [];
}

// On-disk (discovered, unopened) worktrees get a dim hollow ring in the
// status column — distinct from the live `*` working / `✓` idle glyphs.
const ON_DISK_GLYPH = "○";

// A flex row: left group, host-filled spacer, right group. The host
// right-aligns the right group to the row's *actual* width (flush to
// the edge with one column of padding), re-flowing on a dock resize —
// no plugin-side width estimate needed.
function flexLine(l: Entry[], r: Entry[]): WidgetSpec {
  return row(
    raw([styledRow(l as Parameters<typeof styledRow>[0])]),
    flexSpacer(),
    raw([styledRow(r as Parameters<typeof styledRow>[0])]),
  );
}

// The leading status glyph for a session row: the on-disk ring for a
// discovered worktree, otherwise the live working/idle state symbol.
function stateGlyphEntry(s: AgentSession): Entry {
  if (s.pending) {
    // A being-created row: amber `*` while creating (reads as busy/spinner,
    // same glyph as a working session), red `!` once it has failed. Colour
    // comes from the shared helper so glyph and message always agree.
    const glyph = s.pending.phase === "error" ? "! " : "* ";
    return { text: glyph, style: { fg: pendingMsgFg(s.pending), bold: true } };
  }
  if (s.discovered) {
    return { text: ON_DISK_GLYPH + " ", style: { fg: "ui.menu_disabled_fg" } };
  }
  const sym = STATE_SYMBOL[sessionState(s)];
  return { text: sym.glyph + " ", style: { fg: sym.fg, bold: true } };
}

// Build one session row. Two densities, picked by `dockView`:
//
//   card (default): a rounded `labeledSection` pill —
//     line 1: <state> NAME (bold)              ▣ project
//     line 2: ▸ branch        <git: ↑ahead ↓behind +add −del / clean>
//     line 3: PR #1287 ✓7/8 ●2 approved        (blank spacer when no PR)
//
//   compact: a single un-boxed line —
//     <state> NAME                    <git summary>
//
// The bulk-select checkbox only appears in the modal picker (the dock
// delegates bulk actions to it via the "manage" button), so dock rows
// drop it. The right-hand groups are right-aligned by the host (a flex
// spacer), so they adapt to the row's real width including a drag.
function renderPillSpec(
  id: number,
  activeId: number,
): WidgetSpec {
  const s = orchestratorSessions.get(id);
  if (!s) return labeledSection({ label: "", child: styledRow([{ text: editor.t("pill.unknown") }]) });
  // A being-created placeholder renders its status in place of the live
  // pill body: name on line 1, the creating/connecting/error message on
  // line 2 (amber while creating, red on failure), and a retry hint on the
  // error card's line 3.
  if (s.pending) return renderPendingPillSpec(s);
  const isActive = id === activeId;
  const nameEntry: Entry = {
    text: s.label,
    style: { fg: isActive ? "ui.help_key_fg" : undefined, bold: true },
  };
  // Remote/cloud facet glyph (ssh ⇅ / kubernetes ⎈ / devcontainer ⬢), coloured
  // by the remote state, sits just before the name. Local sessions have none,
  // so their rows render exactly as before (the facet is backend-opaque).
  const remoteGlyph: Entry[] = s.remote
    ? [{
        text: REMOTE_GLYPH[s.remote.kind] + " ",
        style: { fg: remoteStateFg(s.remote.state), bold: true },
      }]
    : [];
  const proj = editor.pathBasename(projectKeyOf(s));
  const projEntries: Entry[] = [
    { text: PROJECT_ICON + " ", style: { fg: "ui.menu_disabled_fg" } },
    { text: proj, style: { fg: "ui.menu_disabled_fg", italic: true } },
  ];
  // For a remote session, surface the backend target (host / ns·pod) on the right.
  if (s.remote) {
    projEntries.push({
      text: "  " + s.remote.detail,
      style: { fg: remoteStateFg(s.remote.state), italic: true },
    });
  }
  const git = gitLineParts(s);

  // Compact: one un-boxed line — glyph + (facet) + name on the left, the
  // compact git summary right-aligned. Branch, project tag, and PR badge are
  // dropped (that's the "compact" trade).
  if (dockMode && dockView === "compact") {
    return flexLine([stateGlyphEntry(s), ...remoteGlyph, nameEntry], git.right);
  }

  // Card line 1, left: state glyph · [facet] · NAME. In the modal picker keep
  // the multi-select checkbox between them (Space/click bulk-select); the
  // dock drops it.
  const left: Entry[] = [stateGlyphEntry(s)];
  if (!dockMode) {
    const isChecked = openDialog?.selectedIds.has(id) ?? false;
    left.push({
      text: isChecked ? "[x] " : "[ ] ",
      style: isChecked
        ? { fg: "ui.help_key_fg", bold: true }
        : { fg: "ui.menu_disabled_fg" },
    });
  }
  left.push(...remoteGlyph);
  left.push(nameEntry);

  const children: WidgetSpec[] = [
    flexLine(left, projEntries),
    flexLine(git.left, git.right),
  ];
  // Line 3 is the PR badge when there's an actual PR; when `prLineEntries`
  // returns `[]` we still emit a blank spacer line so every card is a
  // uniform three lines tall — a 2-line card next to 3-line ones looks
  // ragged in the dock.
  const prEntries = prLineEntries(s);
  const prLine: Entry[] = prEntries.length > 0 ? prEntries : [{ text: " " }];
  children.push(raw([styledRow(prLine as Parameters<typeof styledRow>[0])]));
  return labeledSection({ label: "", child: col(...children) });
}

// Row for a being-created placeholder. The dialog scope the user asked
// for lives *here*: instead of blocking the whole editor behind a modal,
// the progress/failure of this one workspace is confined to its own dock
// row. Line 1 is the state glyph + (facet) + name; line 2 is the pending
// message; line 3 is a retry/dismiss hint on failure (blank otherwise, so
// the card keeps the uniform three-line height).
function renderPendingPillSpec(s: AgentSession): WidgetSpec {
  const p = s.pending!;
  const actionable = pendingActionable(p);
  const msgFg = pendingMsgFg(p);
  const remoteGlyph: Entry[] = s.remote
    ? [{ text: REMOTE_GLYPH[s.remote.kind] + " ", style: { fg: msgFg, bold: true } }]
    : [];
  const nameEntry: Entry = { text: s.label, style: { bold: true } };
  const proj = editor.pathBasename(projectKeyOf(s));
  const projEntries: Entry[] = [
    { text: PROJECT_ICON + " ", style: { fg: "ui.menu_disabled_fg" } },
    { text: proj, style: { fg: "ui.menu_disabled_fg", italic: true } },
  ];

  // Compact: glyph + name on the left, the short status on the right.
  if (dockMode && dockView === "compact") {
    return flexLine(
      [stateGlyphEntry(s), ...remoteGlyph, nameEntry],
      [{ text: p.message, style: { fg: msgFg, italic: true } }],
    );
  }

  const children: WidgetSpec[] = [
    flexLine([stateGlyphEntry(s), ...remoteGlyph, nameEntry], projEntries),
    raw([styledRow([{ text: p.message, style: { fg: msgFg, italic: actionable } }])]),
  ];
  const hint: Entry[] = actionable
    ? [{ text: pendingHintText(p), style: { fg: "ui.menu_disabled_fg", italic: true } }]
    : [{ text: " " }];
  children.push(raw([styledRow(hint as Parameters<typeof styledRow>[0])]));
  return labeledSection({ label: "", child: col(...children) });
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
          text: editor.t("preview.no_workspace_selected"),
          style: { fg: "editor.whitespace_indicator_fg", italic: true },
        },
      ]),
    ];
  }
  // A being-created placeholder: show its status (creating/connecting or
  // the failure reason) rather than live-session detail it doesn't have.
  if (s.pending) {
    const p = s.pending;
    return [
      styledRow([{ text: s.label, style: { bold: true } }]),
      styledRow([{ text: p.message, style: { fg: pendingMsgFg(p), italic: true } }]),
      styledRow([
        { text: pendingHintText(p), style: { fg: "ui.menu_disabled_fg", italic: true } },
      ]),
    ];
  }
  const activeId = editor.activeWindow();
  const isActive = s.id === activeId;
  // The focused window is labelled "active"; everything else shows its
  // live working/idle activity (recomputed from the output timestamp).
  const stateText = isActive
    ? editor.t("preview.state_active")
    : sessionState(s) === "working"
    ? editor.t("preview.state_working")
    : editor.t("preview.state_idle");
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
      { text: editor.t("preview.in_place"), style: { fg: "ui.menu_disabled_fg", italic: true } },
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
      ? editor.t("preview.archiving")
      : editor.t("preview.deleting");
    return labeledSection({
      label,
      child: col(
        {
          kind: "raw",
          entries: [
            styledRow([
              {
                text: editor.t("preview.inflight_row", { label, id: String(s.id), name: s.label }),
                style: { bold: true, fg: "ui.menu_disabled_fg" },
              },
            ]),
            styledRow([{ text: "" }]),
            styledRow([
              {
                text: editor.t("preview.waiting_for_git"),
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
      label: editor.t("preview.title"),
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
  const detailsToggleLabel = detailsOn ? editor.t("preview.toggle_preview") : editor.t("preview.toggle_details");
  // Discovered worktree: no live window to embed, so there's
  // nothing to Stop / Archive / Delete yet. Offer only "Open"
  // (Visit attaches a fresh session to the worktree) and describe
  // what diving will do. The empty `windowId: 0` embed keeps the
  // pane the same height as live-session previews so the dialog
  // doesn't jump when the selection moves between row kinds.
  if (s.discovered) {
    const openButtonRow = row(
      button(editor.t("preview.btn_open"), { intent: "primary", key: "visit" }),
      flexSpacer(),
      button(editor.t("preview.btn_stop"), { key: "stop", disabled: true }),
      spacer(2),
      button(editor.t("preview.btn_archive"), { key: "archive", disabled: true }),
      spacer(2),
      button(editor.t("preview.btn_delete"), { intent: "danger", key: "delete", disabled: true }),
    );
    const info: TextPropertyEntry[] = [
      styledRow([
        { text: editor.t("preview.on_disk_not_open"), style: { fg: "ui.menu_disabled_fg", bold: true } },
      ]),
      styledRow([{ text: "" }]),
      styledRow([{ text: editor.t("preview.field_branch"), style: { fg: "ui.menu_disabled_fg" } }, { text: s.branch || editor.t("pill.branch_detached") }]),
      styledRow([{ text: editor.t("preview.field_path"), style: { fg: "ui.menu_disabled_fg" } }, { text: s.root }]),
      styledRow([{ text: "" }]),
      styledRow([
        {
          text: editor.t("preview.click_to_open"),
          style: { fg: "ui.help_key_fg", italic: true },
        },
      ]),
    ];
    return labeledSection({
      label: editor.t("preview.label_on_disk", { name: s.label }),
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
  // wrappingRow so the preview-pane actions reflow onto extra lines on a
  // narrow pane instead of the right-most ones (Stop / Archive / Delete)
  // being clipped off-screen. The wrap path ignores flex spacers, so a
  // fixed `spacer(4)` separates the primary "Visit" from the rest while
  // still wrapping cleanly.
  const buttonRow = wrappingRow(
    button(editor.t("preview.btn_visit"), { intent: "primary", key: "visit" }),
    spacer(4),
    button(detailsToggleLabel, { key: "toggle-details" }),
    spacer(2),
    button(editor.t("preview.btn_stop"), { key: "stop", disabled: stopDisabled }),
    spacer(2),
    button(editor.t("preview.btn_archive"), { key: "archive", disabled: archiveDisabled }),
    spacer(2),
    button(editor.t("preview.btn_delete"), {
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
    ? editor.t("preview.label_launch", { name: s.label })
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
        editor.t("confirm.stop_line1"),
        editor.t("confirm.stop_line2"),
        "",
        editor.t("confirm.stop_note"),
      ];
    case "archive":
      return [
        editor.t("confirm.archive_line1"),
        editor.t("confirm.archive_line2"),
        editor.t("confirm.archive_line3"),
        "",
        editor.t("confirm.archive_note"),
      ];
    case "delete":
      return [
        editor.t("confirm.delete_line1"),
        editor.t("confirm.delete_line2"),
        editor.t("confirm.delete_line3"),
      ];
  }
}

// Localized, capitalized verb for an action (Stop / Archive / Delete),
// used in confirmation headers and button labels.
function capAction(action: BulkAction): string {
  switch (action) {
    case "stop":
      return editor.t("confirm.cap_stop");
    case "archive":
      return editor.t("confirm.cap_archive");
    case "delete":
      return editor.t("confirm.cap_delete");
  }
}

// Localized gerund for an action (Stopping / Archiving / Deleting),
// used in the bulk progress line.
function gerundAction(action: BulkAction): string {
  switch (action) {
    case "stop":
      return editor.t("confirm.ger_stop");
    case "archive":
      return editor.t("confirm.ger_archive");
    case "delete":
      return editor.t("confirm.ger_delete");
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
  const cap = capAction(action);
  const existing = ids.filter((id) => orchestratorSessions.has(id));
  const bulk = existing.length > 1;
  const diskNote = (id: number): string =>
    orchestratorSessions.get(id)?.discovered ? editor.t("confirm.disk_note") : "";
  const entries: TextPropertyEntry[] = [];
  if (bulk) {
    entries.push(
      styledRow([
        { text: editor.t("confirm.bulk_header", { cap, count: String(existing.length) }), style: { bold: true } },
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
            text: editor.t("confirm.and_more", { count: String(existing.length - 8) }),
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
        { text: editor.t("confirm.single_header", { cap, name: ss?.label ?? "" }), style: { bold: true } },
      ]),
    );
  }
  entries.push(
    styledRow([{ text: "" }]),
    styledRow([{ text: bulk ? editor.t("confirm.for_each") : editor.t("confirm.this_will") }]),
  );
  for (const line of confirmActionLines(action)) {
    entries.push(styledRow([{ text: line }]));
  }
  if (action === "delete") {
    entries.push(
      styledRow([{ text: "" }]),
      styledRow([
        {
          text: editor.t("confirm.uncommitted_lost"),
          style: { fg: "ui.status_error_indicator_fg", bold: true },
        },
      ]),
    );
  }
  return labeledSection({
    label: bulk
      ? editor.t("confirm.label_bulk", { cap, count: String(existing.length) })
      : editor.t("confirm.label_single", { cap }),
    child: col(
      { kind: "raw", entries },
      spacer(0),
      // wrappingRow so the Cancel / Confirm pair reflows instead of the
      // Confirm button being clipped on a narrow confirmation pane. The
      // leading flex spacer is dropped (the wrap path ignores flex and
      // trims a blank that would lead a line), so the pair left-packs.
      wrappingRow(
        button(editor.t("confirm.btn_cancel"), { key: "confirm-cancel" }),
        spacer(2),
        button(editor.t("confirm.btn_confirm", { cap }), { intent: "danger", key: `confirm-${action}` }),
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
                text: editor.t("confirm.bulk_progress", {
                  verb: gerundAction(inflight.action),
                  done: String(inflight.done),
                  total: String(inflight.total),
                }),
                style: { fg: "ui.menu_disabled_fg", italic: true },
              },
            ]),
          ],
        },
        flexSpacer(),
      )
    : // wrappingRow (not row): on a narrow pane the action buttons
      // reflow onto extra lines instead of the right-most ones being
      // clipped off-screen. The wrap path ignores flex spacers, so a
      // fixed `spacer(4)` (rather than `flexSpacer()`) keeps a visible
      // gap between the destructive actions and the non-destructive
      // "Clear" while still wrapping cleanly.
      wrappingRow(
        button(editor.t("confirm.bulk_btn_stop", { count: String(stopN) }), { key: "bulk-stop", disabled: stopN === 0 }),
        spacer(2),
        button(editor.t("confirm.bulk_btn_archive", { count: String(archiveN) }), {
          key: "bulk-archive",
          disabled: archiveN === 0,
        }),
        spacer(2),
        button(editor.t("confirm.bulk_btn_delete", { count: String(deleteN) }), {
          intent: "danger",
          key: "bulk-delete",
          disabled: deleteN === 0,
        }),
        spacer(4),
        button(editor.t("confirm.bulk_btn_clear"), { key: "bulk-clear" }),
      );

  // Affected-sessions list. Flag the rows a destructive action will
  // skip so the count discrepancy explains itself.
  const items: TextPropertyEntry[] = sel.map((id) => {
    const ss = orchestratorSessions.get(id)!;
    const rowParts: StyledSegment[] = [{ text: `  ${ss.label}` }];
    if (!ss.discovered && !ownsWorktree(ss)) {
      rowParts.push({
        text: editor.t("confirm.row_in_place"),
        style: { fg: "ui.menu_disabled_fg", italic: true },
      });
    } else if (ss.discovered) {
      rowParts.push({
        text: editor.t("confirm.row_on_disk"),
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
    label: editor.t("confirm.bulk_label", { count: String(sel.length) }),
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
  const activeId = editor.activeWindow();
  // Cards size themselves to the host-laid-out width (flex right-align),
  // so no per-column width estimate is needed here.
  const itemSpecs = filtered.map((id) => renderPillSpec(id, activeId));
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
  const newLabel = newKey ? editor.t("list.new_btn_key", { key: newKey }) : editor.t("list.new_btn");
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
    label: editor.t("list.filter_label"),
    placeholder: editor.t("list.filter_placeholder"),
    fullWidth: true,
    key: inConfirm ? undefined : "filter",
  });
  const errorBanner: WidgetSpec | null = openDialog.lastError
    ? {
        kind: "raw",
        entries: [
          styledRow([
            {
              text: editor.t("list.warn_prefix"),
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

  // Scope chrome. The `Project:` control below is the clickable scope
  // switch; the current scope now reads off that button rather than a
  // title suffix (the dialog title is native modal-frame chrome).
  const scope = openDialog.scope;
  const curKey = currentProjectKey();
  const scopeKey = editor.getKeybindingLabel("orchestrator_toggle_scope", OPEN_MODE);
  const sectionLabel = editor.t("list.section_label");
  // `Project:` control — a visible, clickable scope switch with the
  // Alt+P hint baked into the button label. Shows the current
  // project's name when scoped, "All" when showing every project.
  // Inert while a confirm prompt is up so it can't steal focus.
  const scopeWord = scope === "current" ? editor.pathBasename(curKey) : editor.t("list.scope_all");
  const scopeButtonLabel = scopeKey
    ? editor.t("list.scope_btn_key", { word: scopeWord, key: scopeKey })
    : editor.t("list.scope_btn", { word: scopeWord });
  const scopeButton = button(scopeButtonLabel, {
    key: openDialog.pendingConfirm !== null ? undefined : "scope-toggle",
  });
  const projectControlRow = row(
    {
      kind: "raw",
      entries: [
        styledRow([{ text: editor.t("list.project_prefix"), style: { fg: "ui.menu_disabled_fg" } }]),
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
    ? editor.t("list.show_worktrees_key", { key: worktreeKey })
    : editor.t("list.show_worktrees");
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
    ? editor.t("list.show_trivial_key", { key: trivialKey })
    : editor.t("list.show_trivial");
  const trivialFilterRow = row(
    toggle(!openDialog.hideTrivial, trivialLabel, {
      key: openDialog.pendingConfirm !== null ? undefined : "hide-trivial",
    }),
    flexSpacer(),
  );

  return col(
    // The "ORCHESTRATOR :: Workspaces" title is native modal-frame chrome
    // now (set on the panel at mount — see `openControlRoom`), so the spec
    // starts straight at the error banner / two-pane body.
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
            items: [],
            itemSpecs,
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
        { keys: "↑↓", label: editor.t("hint.nav") },
        { keys: "Enter", label: editor.t("hint.dive") },
        {
          keys: editor.getKeybindingLabel("orchestrator_toggle_select", OPEN_MODE) ||
            "Space",
          label: editor.t("hint.select"),
        },
        {
          keys: scopeKey || "⌥P",
          label: scope === "current" ? editor.t("hint.all_projects") : editor.t("hint.current_only"),
        },
        { keys: "Tab", label: editor.t("hint.focus") },
        { keys: "Esc", label: editor.t("hint.close") },
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
  editor.setStatus(editor.t("status.prefix", { msg }));
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
  // Ensure the background probe poll is running (idempotent; it stops
  // itself when the panel closes). PR/git info is gathered on that
  // loop's own cadence, never synchronously from this refresh.
  startProbePolling();
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
  // The list/tree widget's `selectedIndex` in the spec is initial-only;
  // pin it via mutation so re-renders don't snap back to 0. In the dock
  // (a tree) the pin follows the highlighted node key, which
  // `buildDockSpec` just reconciled; in the modal (a list) it follows
  // the `filteredIds` index.
  if (dockMode) {
    const idx = openDialog.dockSelKey
      ? openDialog.dockKeys.indexOf(openDialog.dockSelKey)
      : -1;
    if (idx >= 0) openPanel.setSelectedIndex("sessions", idx);
  } else if (openDialog.filteredIds.length > 0) {
    openPanel.setSelectedIndex("sessions", openDialog.selectedIndex);
  }
  // `filteredIds` is the single source of truth for the dock's visible list.
  // While the dock is open, make Next/Prev Window cycle through exactly that
  // list (its live windows, in display order) instead of every open window —
  // so paging sessions matches what the dock shows. Discovered worktrees
  // (negative ids) aren't windows, so they're dropped here; the host also
  // skips any id that isn't currently open. Cleared in `closeOpenDialog`.
  if (dockMode) {
    editor.setWindowCycleOrder(openDialog.filteredIds.filter((id) => id > 0));
  }
}

// Move the dock's highlighted row onto the active window. Used when the
// active session changes from *outside* the dock's own ↑/↓ live-switch —
// a new session is created, or another window is focused — so the dock,
// which is a passive mirror while blurred, highlights the session the
// editor actually switched to instead of stranding the highlight on the
// previously-active row. No-op when the active window isn't in the
// (filtered) list.
function syncDockSelectionToActive(): void {
  if (!openDialog || !openPanel || !dockMode) return;
  const activeKey = sessionNodeKey(editor.activeWindow());
  const idx = openDialog.dockKeys.indexOf(activeKey);
  if (idx < 0) return;
  openDialog.dockSelKey = activeKey;
  openPanel.setSelectedIndex("sessions", idx);
}

// =============================================================================
// PR probe — opportunistic `gh` integration for the pill's second line
//
// Best-effort and non-blocking: each visible session's branch is looked
// up against `gh pr view`; results are cached on the session with a TTL
// so holding ↑/↓ doesn't fan out a probe per row, and any failure (`gh`
// missing, not a GitHub remote, unauthenticated, or no PR for the
// branch) degrades silently to the "no PR" / branch fallback.
// =============================================================================

const PR_PROBE_TTL_MS = 90_000;
const prProbesInFlight = new Set<number>();
// Git is local + cheap, so refresh it more often than the PR (network) probe.
const GIT_PROBE_TTL_MS = 15_000;
const gitProbesInFlight = new Set<number>();
// Ceiling on git probes running at once. Each probe spawns two subprocesses
// (`git status` + `git diff`), and the poll loop kicks one per *filtered*
// session every tick — so a dock listing many worktrees would otherwise fan
// out dozens of concurrent spawns on the same tick (a spawn storm that spiked
// render latency). A session skipped because the cap is full is retried on the
// next poll (~5s), comfortably inside the 15s freshness TTL, so nothing goes
// stale — the bursts just get spread across a few ticks.
const MAX_CONCURRENT_GIT_PROBES = 6;

// Resolve (and cache) a session's branch. Live sessions don't carry
// `branch` up front, so ask git in the worktree once.
async function sessionBranch(s: AgentSession): Promise<string> {
  if (s.branch) return s.branch;
  try {
    const r = await spawnCollect(
      "git",
      ["rev-parse", "--abbrev-ref", "HEAD"],
      s.root,
    );
    if (r.exit_code === 0) {
      const b = (r.stdout || "").trim();
      if (b && b !== "HEAD") {
        s.branch = b;
        return b;
      }
    }
  } catch (_e) {
    // ignore — branch stays unknown, probe will report "none"
  }
  return s.branch || "";
}

// Tally a gh `statusCheckRollup` array into pass / fail / pending.
// Handles both CheckRun (status/conclusion) and StatusContext (state).
function rollupCounts(
  rollup: unknown,
): { pass: number; fail: number; pending: number } {
  let pass = 0;
  let fail = 0;
  let pending = 0;
  if (Array.isArray(rollup)) {
    for (const c of rollup as Record<string, unknown>[]) {
      const status = String(c.status ?? "").toUpperCase();
      if (status && status !== "COMPLETED") {
        pending++;
        continue;
      }
      const concl = String(c.conclusion ?? c.state ?? "").toUpperCase();
      if (["SUCCESS", "NEUTRAL", "SKIPPED"].includes(concl)) pass++;
      else if (
        ["FAILURE", "ERROR", "CANCELLED", "TIMED_OUT", "ACTION_REQUIRED"]
          .includes(concl)
      ) fail++;
      else pending++;
    }
  }
  return { pass, fail, pending };
}

async function probePr(s: AgentSession): Promise<void> {
  if (prProbesInFlight.has(s.id)) return;
  const now = Date.now();
  if (s.pr && s.pr.status !== "loading" && now - s.pr.fetchedAt < PR_PROBE_TTL_MS) {
    return;
  }
  prProbesInFlight.add(s.id);
  // Keep any prior info visible while re-checking (avoids a flicker
  // back to the branch fallback on refresh).
  s.pr = { status: "loading", fetchedAt: s.pr?.fetchedAt ?? 0, info: s.pr?.info };
  try {
    const branch = await sessionBranch(s);
    if (!branch) {
      s.pr = { status: "none", fetchedAt: Date.now() };
      return;
    }
    const fields =
      "number,state,isDraft,reviewDecision,mergeable,statusCheckRollup,comments";
    const r = await spawnCollect(
      "gh",
      ["pr", "view", branch, "--json", fields],
      s.root,
    );
    if (r.exit_code !== 0) {
      // No PR for this branch, or `gh` unavailable / not a GH remote.
      s.pr = { status: "none", fetchedAt: Date.now() };
      return;
    }
    const j = JSON.parse(r.stdout || "{}") as Record<string, unknown>;
    const counts = rollupCounts(j.statusCheckRollup);
    const comments = Array.isArray(j.comments)
      ? j.comments.length
      : typeof j.comments === "number"
      ? j.comments
      : 0;
    s.pr = {
      status: "ok",
      fetchedAt: Date.now(),
      info: {
        number: Number(j.number),
        state: j.state ? String(j.state) : undefined,
        isDraft: Boolean(j.isDraft),
        reviewDecision: j.reviewDecision ? String(j.reviewDecision) : "",
        mergeable: j.mergeable ? String(j.mergeable) : undefined,
        checksPass: counts.pass,
        checksFail: counts.fail,
        checksPending: counts.pending,
        comments,
      },
    };
  } catch (_e) {
    s.pr = { status: "none", fetchedAt: Date.now() };
  } finally {
    prProbesInFlight.delete(s.id);
    // Surface the freshly-gathered badge (debounced so a batch of
    // probes finishing together collapses into one render).
    scheduleProbeRefresh();
  }
}

// Parse `git status --porcelain=v2 --branch` into a GitStat. Counts any
// non-`#` line as a dirty path (changed/staged/unmerged/untracked).
function parsePorcelainV2(stdout: string): GitStat {
  const g: GitStat = {};
  let dirty = 0;
  for (const line of (stdout || "").split(/\r?\n/)) {
    if (line.startsWith("# branch.head ")) {
      const b = line.slice("# branch.head ".length).trim();
      if (b && b !== "(detached)") g.branch = b;
    } else if (line.startsWith("# branch.ab ")) {
      const m = line.match(/\+(\d+)\s+-(\d+)/);
      if (m) {
        g.ahead = Number(m[1]);
        g.behind = Number(m[2]);
      }
    } else if (line && !line.startsWith("#")) {
      dirty++;
    }
  }
  g.dirty = dirty;
  return g;
}

// Opportunistic local-git summary for the pill's second line. Two cheap
// git calls (status + diffstat), throttled like the PR probe.
// Kick git probes for the open dialog's filtered sessions up to the
// concurrency cap. `probeGit` self-skips fresh / in-flight sessions, so this
// only spends slots on sessions that would actually spawn. Called from the
// poll tick and re-invoked as each probe completes, giving a continuously
// draining bounded pool: at most MAX_CONCURRENT_GIT_PROBES spawn at once, but
// freed slots refill immediately, so a large dock still refreshes promptly
// without a same-tick storm.
function drainGitProbes(): void {
  if (!openDialog) return;
  for (const id of openDialog.filteredIds) {
    if (gitProbesInFlight.size >= MAX_CONCURRENT_GIT_PROBES) break;
    const s = orchestratorSessions.get(id);
    if (s) void probeGit(s);
  }
}

async function probeGit(s: AgentSession): Promise<void> {
  if (gitProbesInFlight.has(s.id)) return;
  const now = Date.now();
  if (s.git && s.git.status === "ok" && now - s.git.fetchedAt < GIT_PROBE_TTL_MS) {
    return;
  }
  // Bound concurrent git spawns (see MAX_CONCURRENT_GIT_PROBES). Check this
  // *after* the freshness gate so an up-to-date session is still a free no-op;
  // only sessions that would actually spawn count against the cap. `drainGitProbes`
  // refills the slot this holds as soon as the probe finishes.
  if (gitProbesInFlight.size >= MAX_CONCURRENT_GIT_PROBES) return;
  gitProbesInFlight.add(s.id);
  s.git = { status: "loading", fetchedAt: s.git?.fetchedAt ?? 0, info: s.git?.info };
  try {
    const st = await spawnCollect(
      "git",
      ["status", "--porcelain=v2", "--branch"],
      s.root,
    );
    if (st.exit_code !== 0) {
      s.git = { status: "ok", fetchedAt: Date.now() }; // not a git dir → empty summary
      return;
    }
    const info = parsePorcelainV2(st.stdout || "");
    if (info.branch && !s.branch) s.branch = info.branch;
    // Uncommitted line churn vs HEAD (staged + unstaged).
    const diff = await spawnCollect("git", ["diff", "--shortstat", "HEAD"], s.root);
    if (diff.exit_code === 0) {
      const ins = (diff.stdout || "").match(/(\d+) insertion/);
      const del = (diff.stdout || "").match(/(\d+) deletion/);
      if (ins) info.added = Number(ins[1]);
      if (del) info.deleted = Number(del[1]);
    }
    s.git = { status: "ok", fetchedAt: Date.now(), info };
  } catch (_e) {
    s.git = { status: "ok", fetchedAt: Date.now() };
  } finally {
    gitProbesInFlight.delete(s.id);
    scheduleProbeRefresh();
    // Refill the slot this probe just freed so the pool keeps draining
    // between polls (preserves throughput under the concurrency cap).
    drainGitProbes();
  }
}

// Probe lifecycle: a single low-frequency poll loop, NOT action-driven.
// It ticks every PROBE_POLL_INTERVAL_MS while a panel is open and kicks
// the per-session probes, which each self-throttle to their TTL (git
// 15s, pr 90s) — so a session is hit at most once per TTL regardless of
// how often the panel re-renders (e.g. a busy agent spamming
// terminal_output no longer fans out probes). The loop stops itself when
// the panel closes.
const PROBE_POLL_INTERVAL_MS = 5_000;
let probePollActive = false;

function startProbePolling(): void {
  if (probePollActive) return;
  probePollActive = true;
  const tick = (): void => {
    if (!openPanel || !openDialog) {
      probePollActive = false;
      return;
    }
    for (const id of openDialog.filteredIds) {
      const s = orchestratorSessions.get(id);
      if (!s) continue;
      void probePr(s);
    }
    // Git probes drain at bounded concurrency (see `drainGitProbes`) rather
    // than all firing on this tick — each completion refills the freed slot,
    // so throughput is preserved without the same-tick spawn burst.
    drainGitProbes();
    void editor.delay(PROBE_POLL_INTERVAL_MS).then(tick);
  };
  tick();
}

// Coalesce the re-renders triggered by probe completions: a batch of
// probes finishing together collapses into one refresh rather than one
// per probe (the open-time storm that made the panel feel unresponsive).
let probeRefreshPending = false;
function scheduleProbeRefresh(): void {
  if (probeRefreshPending) return;
  probeRefreshPending = true;
  void editor.delay(150).then(() => {
    probeRefreshPending = false;
    if (openPanel) refreshOpenDialog();
  });
}

function openControlRoom(opts: { dock?: boolean } = {}): void {
  const asDock = opts?.dock === true;
  if (openPanel) {
    // If the dock is showing and the user asked for the modal picker
    // (Orchestrator: Open, or the dock's "Manage" button), float the
    // picker *over* the dock instead of replacing it: keep the dock
    // mounted in its own host slot (PanelSlot::Dock) and build the picker
    // as a fresh panel in the Floating slot, exactly as the New-Session
    // form coexists with the dock. The host renders both slots, so the
    // dock stays put in its left column — dimmed and passive — and the
    // picker lays into `chrome_area` beside it. Closing the picker hands
    // control back to the dock (`restoreDockBehindPicker`). An `asDock`
    // re-entry (Toggle Dock) never reaches here — `toggleDock` handles it
    // first; and a modal picker that is already up has nothing to do.
    if (!asDock && dockMode) {
      dockPanel = openPanel; // dock stays mounted behind the picker
      openPanel = null; // fall through builds the picker as a new panel
      dockBlurred = true; // the dock is now the inert background
      // dockMode flips to false in the (asDock === false) branch below.
    } else {
      return;
    }
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
    dockTreeTop: 0,
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
    projectFilter: asDock ? lastDockProjectFilter : null,
    projectMenuOpen: false,
    projectMenuIndex: 0,
    filtersExpanded: false,
    dockMenu: null,
    dockSelKey: null,
    dockNodes: [],
    dockKeys: [],
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
    openPanel.mount(buildOpenSpec(), {
      widthPct: 90,
      heightPct: 90,
      // Native modal-frame chrome replaces the in-body "ORCHESTRATOR ::
      // Workspaces" title row; `closable` renders a native `[×]` that
      // dismisses via the same cancel path as Esc. Chrome is only drawn
      // for Centered placement, so the dock mount above never gets it.
      title: editor.t("list.title"),
      closable: true,
    });
    // The control room is a global orchestrator feature: render it over
    // the full screen (covering its own dimmed dock) rather than cramped
    // into the chrome area beside the dock. A no-op when no dock is up
    // (the chrome area already is the whole frame then).
    editor.floatingPanelControl(openPanel.id(), "fullscreen", 1);
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

// When the modal Open picker was floated over a still-mounted dock,
// dropping the picker hands keyboard control back to the dock (which
// stayed mounted in the Dock slot the whole time) instead of tearing
// everything down. Returns true when it restored the dock — callers
// then stop, treating the picker as "closed to the dock". The shared
// `openDialog` may have been filtered/reselected by the picker, so reset
// it to the full list before the dock re-renders.
function restoreDockBehindPicker(): boolean {
  if (!dockPanel) return false;
  openPanel = dockPanel;
  dockPanel = null;
  dockMode = true;
  dockBlurred = false;
  dockFocus = "list";
  if (openDialog) {
    openDialog.filter = { value: "", cursor: 0 };
    const activeId = editor.activeWindow();
    openDialog.filteredIds = filterSessions("");
    const activeIdx = openDialog.filteredIds.indexOf(activeId);
    openDialog.selectedIndex = activeIdx >= 0 ? activeIdx : 0;
  }
  editor.setEditorMode(null);
  refreshOpenDialog();
  editor.floatingPanelControl(openPanel.id(), "focus", 0);
  openPanel.setFocusKey("sessions");
  return true;
}

function closeOpenDialog(): void {
  if (openPanel) {
    openPanel.unmount();
    openPanel = null;
  }
  // If a dock was kept behind the picker, hand control back to it rather
  // than dropping to the bare editor. (It re-publishes its own cycle order
  // on the next refresh, so leave the override in place here.)
  if (restoreDockBehindPicker()) return;
  openDialog = null;
  dockMode = false;
  dockBlurred = false;
  // The dock is gone — restore the default Next/Prev Window cycling (every
  // window, by id).
  editor.setWindowCycleOrder([]);
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

// Lifecycle actions, bulk-select, and per-session confirmations now
// live in the modal picker (reached via the dock's "Manage" button);
// the dock itself is a lean switcher with no destructive controls.

// Extract the single mnemonic letter from a keybinding label like
// "Alt+O" / "⌥O" → "o". Returns "" when the binding isn't a single
// trailing letter (so callers show no mnemonic rather than guessing).
function mnemonicLetter(label: string | null): string {
  if (!label) return "";
  const m = label.match(/([A-Za-z])\s*$/);
  return m ? m[1].toLowerCase() : "";
}

// The dock's top label row, styled as a menu bar (menu fg on menu bg)
// spanning the full dock width. The accelerator that focuses the dock
// (default Alt+O, looked up live so a rebind is honoured) supplies the
// mnemonic: the matching letter in "Orchestrator" is underlined — but
// only when the binding really is a single letter that appears in the
// title, never a hardcoded "O".
function dockTitleRow(): WidgetSpec {
  const title = editor.t("dock.title");
  const base = { fg: "ui.menu_fg", bg: "ui.menu_bg" };
  const mnem = mnemonicLetter(
    editor.getKeybindingLabel("toggle_dock_focus", "normal"),
  );
  const idx = mnem ? title.toLowerCase().indexOf(mnem) : -1;
  const segments: Entry[] = [];
  if (idx >= 0) {
    if (idx > 0) segments.push({ text: title.slice(0, idx), style: base });
    segments.push({
      text: title.slice(idx, idx + 1),
      style: { ...base, underline: true, bold: true },
    });
    segments.push({ text: title.slice(idx + 1), style: base });
  } else {
    segments.push({ text: title, style: { ...base, bold: true } });
  }
  // Pad to the screen width so the menu-bar background spans the whole
  // dock; the host clips the over-wide row to the actual dock columns.
  const barW = Math.max(title.length, editor.getScreenSize().width || 80);
  return {
    kind: "raw",
    entries: [
      styledRow(segments as Parameters<typeof styledRow>[0], {
        padToChars: barW,
        style: base,
      }),
    ],
  };
}

// Option keys for the dock's project dropdown, in display order. Index 0
// is always "All projects" (the empty-string key); the rest are the
// projects with a session in the worktree/trivial-filtered set. The
// project menu's keyboard cursor (`projectMenuIndex`) and the
// `dock_menu_*` nav handlers index into this list, so it's the single
// source of truth for both render and navigation.
function projectMenuKeys(): string[] {
  return ["", ...dockProjectOptions()];
}

// The `project-pick:<key>` widget key for a menu row — the host reads
// this focus key to recognise that the dropdown (not the session list)
// owns the keyboard, and to route ↑/↓/Enter/Esc to the `dock_menu_*`
// events. Empty suffix = "All projects".
function projectPickKey(optionKey: string): string {
  return `project-pick:${optionKey}`;
}

// Floating menu for the dock's project dropdown: "All projects" plus
// every project with a session in the worktree/trivial-filtered set.
// Anchored just under the toolbar via `overlay`, so it paints over the
// rows below without reflowing them. Each option is a button whose key
// (`project-pick:<key>`, empty suffix = all) the widget_event handler
// decodes. The `●` marks the *applied* filter; the `primary` intent
// marks the keyboard *cursor* (`projectMenuIndex`) — two separate
// signals so ↑/↓ can move the cursor over options without yet applying
// them, the way a standard dropdown behaves.
function dockProjectMenu(): WidgetSpec {
  const cur = openDialog?.projectFilter ?? null;
  const keys = projectMenuKeys();
  const cursor = clampMenuIndex(openDialog?.projectMenuIndex ?? 0, keys.length);
  const rows: WidgetSpec[] = keys.map((key, i) => {
    const applied = key === "" ? cur === null : key === cur;
    const label = key === "" ? editor.t("dock.all_projects") : projectLabel(key);
    return row(
      button((applied ? "● " : "  ") + label, {
        key: projectPickKey(key),
        intent: i === cursor ? "primary" : "normal",
      }),
      flexSpacer(),
    );
  });
  return overlay(labeledSection({ label: editor.t("dock.menu_label"), child: col(...rows) }));
}

// Clamp a menu cursor into `[0, len)`, tolerating an empty list.
function clampMenuIndex(idx: number, len: number): number {
  if (len <= 0) return 0;
  return Math.max(0, Math.min(idx, len - 1));
}

// Open the dock's project dropdown and hand it the keyboard. Seeds the
// cursor on the *applied* option (so ↑/↓ start from where you are) and
// moves panel focus onto that option's button — which is the signal the
// host uses to route nav keys here instead of the session list.
function openProjectMenu(): void {
  if (!openDialog || !openPanel) return;
  const keys = projectMenuKeys();
  const applied = openDialog.projectFilter;
  const idx = applied === null ? 0 : Math.max(0, keys.indexOf(applied));
  // The project control lives in the collapsible Filters section; make
  // sure it's open so the dropdown (and its anchor button) are visible.
  openDialog.filtersExpanded = true;
  openDialog.projectMenuOpen = true;
  openDialog.projectMenuIndex = clampMenuIndex(idx, keys.length);
  // Render the menu first so its buttons exist in the spec, *then* move
  // focus onto the cursor row — otherwise the host re-clamps an unknown
  // focus key back to the first tabbable.
  openPanel.update(buildDockSpec());
  openPanel.setFocusKey(projectPickKey(keys[openDialog.projectMenuIndex]));
}

// Close the dropdown and return the keyboard to the session list.
function closeProjectMenu(): void {
  if (!openDialog || !openPanel) return;
  openDialog.projectMenuOpen = false;
  openPanel.update(buildDockSpec());
  openPanel.setFocusKey("sessions");
}

// Move the dropdown cursor by `delta` (clamped, no wrap) and keep panel
// focus on the highlighted row so the host keeps routing nav keys here.
function moveProjectMenu(delta: number): void {
  if (!openDialog || !openPanel || !openDialog.projectMenuOpen) return;
  const keys = projectMenuKeys();
  const next = clampMenuIndex(openDialog.projectMenuIndex + delta, keys.length);
  openDialog.projectMenuIndex = next;
  openPanel.update(buildDockSpec());
  openPanel.setFocusKey(projectPickKey(keys[next]));
}

// Commit the cursor's option as the active project filter and close.
function acceptProjectMenu(): void {
  if (!openDialog || !openDialog.projectMenuOpen) return;
  const keys = projectMenuKeys();
  const key = keys[clampMenuIndex(openDialog.projectMenuIndex, keys.length)] ?? "";
  pickProject(key);
}

// Apply a project-dropdown option (empty = "All projects") as the dock's
// project filter, re-filter the session list, close the menu, and return
// focus to the list. Shared by mouse clicks on a row, Enter on the
// keyboard cursor, and any programmatic pick.
function pickProject(optionKey: string): void {
  if (!openDialog) return;
  openDialog.projectFilter = optionKey === "" ? null : optionKey;
  lastDockProjectFilter = openDialog.projectFilter;
  // Re-filter to the chosen project and keep the active session selected
  // when it's still in view.
  const activeId = editor.activeWindow();
  const next = filterSessions(openDialog.filter.value);
  openDialog.filteredIds = next;
  const activeIdx = next.indexOf(activeId);
  openDialog.selectedIndex = activeIdx >= 0 ? activeIdx : 0;
  closeProjectMenu();
  refreshOpenDialog();
  if (openPanel && next.length > 0) {
    openPanel.setSelectedIndex("sessions", openDialog.selectedIndex);
  }
}

// Single-column spec for the dock. The toolbar is deliberately lean —
// a "New Task…" create dropdown and a "Search Tasks" input — with the
// view/project/worktree/trivial controls tucked into a collapsible
// "Filters" section so they're available without cluttering the top.
// Below the toolbar the sessions render as a fully hierarchical **tree**
// (widget key `sessions`, so the host's dock key routing is unchanged):
// user-created folders group and nest sessions however the user likes,
// with ungrouped sessions at the top level. Lifecycle actions
// (Stop/Archive/Delete) and bulk-select live in the modal picker,
// reached from the Filters section's "Manage" button.
function buildDockSpec(): WidgetSpec {
  if (!openDialog) return col();
  const filtered = openDialog.filteredIds;
  const activeId = editor.activeWindow();
  const dockTree = buildDockTree(filtered, activeId);
  // Mirror the emitted node model so selection / activation / context
  // can resolve `dockSelKey` back to a folder or session.
  openDialog.dockNodes = dockTree.model;
  openDialog.dockKeys = dockTree.keys;
  // Keep the highlighted node key pointing at something real: default to
  // the active session's node, else the first node.
  if (!openDialog.dockSelKey || !dockTree.keys.includes(openDialog.dockSelKey)) {
    const activeKey = sessionNodeKey(activeId);
    openDialog.dockSelKey = dockTree.keys.includes(activeKey)
      ? activeKey
      : (dockTree.keys[0] ?? null);
  }
  const selIdx = openDialog.dockSelKey
    ? dockTree.keys.indexOf(openDialog.dockSelKey)
    : -1;

  const newLabel = editor.t("dock.new_btn");
  // The "New Task…" button and the search field share one row, wrapping
  // the search below the button when the dock is too narrow to hold both.
  // The button renders as "[ <label> ]" (label + 4 cols); size the search
  // field to fill the rest of a default-width dock, floored so it stays
  // usable — and so the two overflow (and wrap) on a narrow/dragged dock.
  // The host wraps against the *actual* rendered width, so this estimate
  // only needs to be close for the default-dock case.
  const newBtnCols = newLabel.length + 4;
  const dockCols = dockContentCols(dockDefaultWidth());
  const SEARCH_MIN_FIELD = 10;
  const searchField = Math.max(SEARCH_MIN_FIELD, dockCols - newBtnCols - 4);
  const toolbarWraps = newBtnCols + 1 + searchField + 2 > dockCols;
  const worktreeLabel = editor.t("dock.all_worktrees");
  const trivialLabel = editor.t("dock.show_empty");
  const projWord = openDialog.projectFilter === null
    ? editor.t("list.scope_all")
    : editor.pathBasename(openDialog.projectFilter);

  // The hints belong to the dock only while it has keyboard focus
  // (req: hide them when the editor owns the keyboard). A blurred dock
  // gives the row back to the tree.
  const showHints = !dockBlurred;
  const menuOpen = openDialog.projectMenuOpen || openDialog.dockMenu !== null;
  // While a dropdown owns the keyboard, the hints describe the dropdown
  // (choose/select/cancel), not the session tree. The tree's own hints
  // take two rows — the dock is too narrow for four hints on one line —
  // so the context-menu key (Shift+F10 / the Menu key, the only keyboard
  // route to "Move to Folder…" and the folder actions) is discoverable
  // instead of a mouse-only secret.
  const centered = (entries: Parameters<typeof hintBar>[0]): WidgetSpec =>
    row(flexSpacer(), hintBar(entries), flexSpacer());
  const bottom: WidgetSpec[] = !showHints
    ? []
    : menuOpen
      ? [centered([
          { keys: "↑↓", label: editor.t("dock.hint_choose") },
          { keys: "Enter", label: editor.t("dock.hint_select") },
          { keys: "Esc", label: editor.t("dock.hint_cancel") },
        ])]
      : [
        centered([
          { keys: "↑↓", label: editor.t("dock.hint_switch") },
          { keys: "→←", label: editor.t("dock.hint_fold") },
        ]),
        centered([
          { keys: "Enter", label: editor.t("dock.hint_edit") },
          { keys: "F2", label: editor.t("dock.hint_menu") },
        ]),
      ];
  const bottomRows = bottom.length;

  // The collapsible Filters section: a header toggle plus, when open,
  // the density / project / worktree / trivial controls and Manage.
  const filtersArrow = openDialog.filtersExpanded ? "▾ " : "▸ ";
  const filterHeader = row(
    button(filtersArrow + editor.t("dock.filters"), { key: "filters-toggle" }),
    flexSpacer(),
  );
  const filterBody: WidgetSpec[] = openDialog.filtersExpanded
    ? [
      row(
        button(editor.t("dock.view_btn", { view: dockView }), { key: "view-toggle" }),
        flexSpacer(),
        button(editor.t("dock.project_btn", { word: projWord }), { key: "project-menu" }),
      ),
      // The project dropdown floats just under its toolbar button.
      ...(openDialog.projectMenuOpen ? [dockProjectMenu()] : []),
      row(
        toggle(openDialog.showWorktrees, worktreeLabel, { key: "worktree-show" }),
        flexSpacer(),
      ),
      row(
        toggle(!openDialog.hideTrivial, trivialLabel, { key: "hide-trivial" }),
        flexSpacer(),
      ),
      row(
        button(editor.t("dock.move_btn"), { key: "move-session" }),
        flexSpacer(),
        button(editor.t("dock.manage"), { key: "manage" }),
      ),
    ]
    : [];

  // Size the tree to fill the dock. Top chrome is variable: title, the
  // New+search toolbar (1 row, or 2 when the search wraps below the
  // button on a narrow dock), filter header, divider — plus the expanded
  // filter body rows when open. The tree soaks up the rest.
  const screen = editor.getScreenSize();
  const innerH = Math.max(8, screen.height > 0 ? screen.height : 30);
  const toolbarRows = toolbarWraps ? 2 : 1;
  const chromeRows = 3 + toolbarRows + filterBody.length + bottomRows;
  const listRows = Math.max(MIN_LIST_ROWS, innerH - chromeRows);
  openDialog.listVisibleRows = listRows;
  // Rows of chrome above the tree (everything in chromeRows except the
  // bottom hint row) — where the first tree row lands on screen.
  openDialog.dockTreeTop = chromeRows - bottomRows;

  const expandedSeed = dockTreeExpandedKeys(dockTree);

  // Pin the hint bar to the dock's bottom edge. The host tree renders
  // only its actual content rows (it does not pad itself out to
  // `visibleRows`), so with few sessions the hints used to sit directly
  // under the last card with dead space *below* them. Measure the rows
  // the visible tree content occupies and fill the gap with blank,
  // non-interactive rows so `bottom` always lands on the dock's last
  // rows. Zero when the tree fills or overflows its budget.
  const treeRows = Math.min(listRows, dockTreeContentRows(dockTree, expandedSeed));
  const padRows = bottomRows > 0 ? Math.max(0, listRows - treeRows) : 0;
  const bottomPad: WidgetSpec[] = padRows > 0
    ? [raw(Array.from({ length: padRows }, () => ({ text: "" })))]
    : [];

  return col(
    dockTitleRow(),
    // New-task button + search on one row; a narrow dock wraps the search
    // to its own row beneath the button (pieces are never split).
    wrappingRow(
      button(newLabel, { intent: "primary", key: "new-session" }),
      spacer(1),
      text({
        value: openDialog.filter.value,
        cursorByte: openDialog.filter.cursor,
        placeholder: editor.t("dock.filter_placeholder"),
        fieldWidth: searchField,
        key: "filter",
      }),
    ),
    // The "New Task…" create dropdown floats just under its button.
    ...(openDialog.dockMenu?.kind === "new" ? [dockNewMenu()] : []),
    filterHeader,
    ...filterBody,
    // The "Move to folder…" dropdown floats over the tree without
    // reflowing it.
    ...(openDialog.dockMenu?.kind === "move" ? [dockMoveMenu()] : []),
    // Host-rendered full-width rule: it spans whatever width the dock is
    // actually drawn at (incl. a user drag), so it can't drift from the
    // chrome the way a plugin-computed `"─".repeat(width)` did.
    divider({ style: { fg: "ui.menu_disabled_fg" } }),
    tree({
      nodes: dockTree.nodes,
      itemKeys: dockTree.keys,
      selectedIndex: selIdx,
      visibleRows: listRows,
      expandedKeys: expandedSeed,
      // "card" density renders each session as a 3-content-row card
      // inside a rounded border (the pill look the pre-tree dock had);
      // "compact" keeps single-line rows. The host keeps
      // scroll/selection in node units either way.
      itemHeight: dockView === "card" ? DOCK_CARD_HEIGHT : 1,
      cardBorders: dockView === "card",
      // Focusable in the dock (unlike the modal, where Up/Down forward
      // from the filter): the tree itself is the default focus so ↑↓
      // drive live-switch, →← fold, and Enter dives / toggles a folder.
      focusable: true,
      key: "sessions",
    }),
    ...bottomPad,
    ...bottom,
  );
}

// Screen rows the dock tree's *visible* content occupies, mirroring the
// host renderer's accounting: descendants of a collapsed folder are
// skipped (same ancestor-stack walk as the host's `visible_indices`,
// against the same expansion set the tree is seeded/reconciled with), a
// folder header is one row, and a session leaf is a bordered card
// (`DOCK_CARD_HEIGHT` content rows + 2 border rows) in card density or
// a single row in compact.
function dockTreeContentRows(t: DockTree, expandedKeys: string[]): number {
  const card = dockView === "card";
  const open = new Set(expandedKeys);
  const ancestorOpen: boolean[] = [];
  let rows = 0;
  for (let i = 0; i < t.nodes.length; i++) {
    const depth = t.nodes[i].depth ?? 0;
    // Truncate the ancestor stack to this node's depth, then the node
    // is visible iff every remaining ancestor folder is open.
    ancestorOpen.length = Math.min(ancestorOpen.length, depth);
    if (ancestorOpen.every((o) => o)) {
      rows += card && t.model[i].kind === "session" ? DOCK_CARD_HEIGHT + 2 : 1;
    }
    // Push this node's own openness so descendants see it; leaves act
    // as open (they have no descendants to hide).
    ancestorOpen.push(t.model[i].kind !== "folder" || open.has(t.keys[i]));
  }
  return rows;
}

// The folder keys to seed the tree's expansion with. During a search
// every folder is force-open so matches aren't hidden; otherwise the
// user's persisted expansion set is used.
function dockTreeExpandedKeys(t: DockTree): string[] {
  const searching = (openDialog?.filter.value ?? "") !== "";
  if (searching) return t.keys.filter((k) => k.startsWith(FOLDER_NODE_PREFIX));
  return Array.from(loadExpanded());
}

// ---------------------------------------------------------------------
// Dock toolbar dropdowns — the "New Task…" create menu and a session's
// "Move to folder…" menu. Both reuse the project dropdown's mechanics:
// an `overlay(labeledSection(...))` of option buttons keyed `menu-pick:`,
// keyboard-navigated via the shared `dock_menu_*` events (the host
// routes them here while focus sits on a `menu-pick:` button). The `●`
// marks the applied/current choice; `primary` intent marks the cursor.
// ---------------------------------------------------------------------

interface MenuOption {
  key: string; // action key, e.g. "new:task" / "move:df3"
  label: string;
  marked?: boolean;
}

function menuPickKey(optKey: string): string {
  return `menu-pick:${optKey}`;
}

// Options for the "New Task…" create dropdown.
function dockNewOptions(): MenuOption[] {
  return [
    { key: "new:task", label: editor.t("dock.new_menu_task") },
    { key: "new:folder", label: editor.t("dock.new_menu_folder") },
  ];
}

// Options for a session's "Move to folder…" dropdown: every folder
// (indented by depth), plus "top level" and "New folder…".
function dockMoveOptions(sessionId: number): MenuOption[] {
  const cur = folderOfSession(sessionId);
  const opts: MenuOption[] = [
    { key: "move:root", label: editor.t("dock.move_root"), marked: cur === null },
  ];
  const walk = (parent: string | null, depth: number): void => {
    for (const f of childFoldersOf(parent)) {
      opts.push({
        key: `move:${f.id}`,
        label: "  ".repeat(depth) + f.name,
        marked: f.id === cur,
      });
      walk(f.id, depth + 1);
    }
  };
  walk(null, 0);
  opts.push({ key: "move:new", label: editor.t("dock.new_menu_folder") });
  return opts;
}

function dockMenuOptions(): MenuOption[] {
  const m = openDialog?.dockMenu;
  if (!m) return [];
  return m.kind === "new" ? dockNewOptions() : dockMoveOptions(m.sessionId);
}

function dockDropdownOverlay(label: string, opts: MenuOption[], cursor: number): WidgetSpec {
  const rows: WidgetSpec[] = opts.map((o, i) =>
    row(
      button((o.marked ? "● " : "  ") + o.label, {
        key: menuPickKey(o.key),
        intent: i === cursor ? "primary" : "normal",
      }),
      flexSpacer(),
    )
  );
  return overlay(labeledSection({ label, child: col(...rows) }));
}

function dockNewMenu(): WidgetSpec {
  const cursor = clampMenuIndex(
    openDialog?.dockMenu?.kind === "new" ? openDialog.dockMenu.index : 0,
    dockNewOptions().length,
  );
  return dockDropdownOverlay(editor.t("dock.menu_new_label"), dockNewOptions(), cursor);
}

function dockMoveMenu(): WidgetSpec {
  if (openDialog?.dockMenu?.kind !== "move") return col();
  const opts = dockMoveOptions(openDialog.dockMenu.sessionId);
  const cursor = clampMenuIndex(openDialog.dockMenu.index, opts.length);
  return dockDropdownOverlay(editor.t("dock.menu_move_label"), opts, cursor);
}

// Open a dock dropdown and hand it the keyboard (focus onto the cursor's
// `menu-pick:` button — the signal the host uses to route nav keys here
// instead of the session tree).
function openDockMenu(menu: DockDropdown): void {
  if (!openDialog || !openPanel) return;
  if (openDialog.projectMenuOpen) closeProjectMenu();
  openDialog.dockMenu = menu;
  openPanel.update(buildDockSpec());
  const opts = dockMenuOptions();
  const idx = clampMenuIndex(menu.index, opts.length);
  if (opts[idx]) openPanel.setFocusKey(menuPickKey(opts[idx].key));
}

function closeDockMenu(): void {
  if (!openDialog || !openPanel) return;
  openDialog.dockMenu = null;
  openPanel.update(buildDockSpec());
  openPanel.setFocusKey("sessions");
}

function moveDockMenu(delta: number): void {
  if (!openDialog || !openPanel || !openDialog.dockMenu) return;
  const opts = dockMenuOptions();
  const next = clampMenuIndex(openDialog.dockMenu.index + delta, opts.length);
  openDialog.dockMenu = { ...openDialog.dockMenu, index: next };
  openPanel.update(buildDockSpec());
  if (opts[next]) openPanel.setFocusKey(menuPickKey(opts[next].key));
}

function acceptDockMenu(): void {
  if (!openDialog || !openDialog.dockMenu) return;
  const opts = dockMenuOptions();
  const opt = opts[clampMenuIndex(openDialog.dockMenu.index, opts.length)];
  if (opt) runDockMenuOption(opt.key);
}

// Apply a dropdown choice (shared by mouse click on an option button and
// keyboard Enter on the cursor).
function runDockMenuOption(optKey: string): void {
  if (!openDialog) return;
  const menu = openDialog.dockMenu;
  if (optKey === "new:task") {
    closeDockMenu();
    dockBlurred = true;
    openForm({ fromPicker: true });
    return;
  }
  if (optKey === "new:folder") {
    closeDockMenu();
    openCreateFolderDialog(null);
    return;
  }
  if (optKey.startsWith("move:") && menu?.kind === "move") {
    const sessionId = menu.sessionId;
    const target = optKey.slice("move:".length);
    if (target === "new") {
      closeDockMenu();
      openCreateFolderDialog(null, sessionId);
      return;
    }
    assignSessionToFolder(sessionId, target === "root" ? null : target);
    closeDockMenu();
    refreshOpenDialog();
    return;
  }
  closeDockMenu();
}

// ---------------------------------------------------------------------
// Dock tree selection resolution. The tree is host-owned; `dockSelKey`
// (updated from every `select` event, seeded in `buildDockSpec`) is the
// plugin's mirror of the highlighted node.
// ---------------------------------------------------------------------

function dockSelectedNode(): DockNode | null {
  if (!openDialog || !openDialog.dockSelKey) return null;
  const idx = openDialog.dockKeys.indexOf(openDialog.dockSelKey);
  return idx >= 0 ? openDialog.dockNodes[idx] ?? null : null;
}

function dockSelectedSessionId(): number | null {
  const node = dockSelectedNode();
  return node && node.kind === "session" ? node.sessionId : null;
}

// ---------------------------------------------------------------------
// Folder operations.
// ---------------------------------------------------------------------

// Open the "New Folder" dialog. `parent` is the parent folder (null =
// top level). `assignSession` names a specific session to file under the
// new folder (the "Move to → New folder…" path); when omitted the dialog
// offers to organize the *current* (active) session instead. The dialog
// itself commits the folder on "Create Folder" — see `submitCreateFolder`.
function openCreateFolderDialog(
  parent: string | null,
  assignSession?: number,
): void {
  // The session the "organize under this folder" checkbox targets: the
  // explicitly-moved session when given, else the active window.
  const candidate = typeof assignSession === "number"
    ? assignSession
    : editor.activeWindow();
  const sessionId = candidate > 0 && orchestratorSessions.has(candidate)
    ? candidate
    : null;
  createFolderDialog = {
    // Start empty with the default shown as a placeholder, so the first
    // keystroke types a name cleanly (no pre-filled text to clear first).
    // An empty submit falls back to the default name (see submitCreateFolder).
    name: { value: "", cursor: 0 },
    organizeCurrent: true,
    parent,
    sessionId,
    renameId: null,
  };
  mountFolderDialog();
}

// UTF-8 byte length of a JS (UTF-16) string — text-input cursors are
// byte offsets host-side.
function utf8Len(s: string): number {
  let b = 0;
  for (let i = 0; i < s.length; i++) {
    const c = s.codePointAt(i)!;
    if (c > 0xffff) i++; // surrogate pair consumed both units
    b += c <= 0x7f ? 1 : c <= 0x7ff ? 2 : c <= 0xffff ? 3 : 4;
  }
  return b;
}

// Open the folder dialog in rename mode for an existing folder — the
// same centered dialog as "New Folder" (per-issue: rename used to drop
// into the bottom minibuffer prompt, inconsistent with create).
function openRenameFolderDialog(id: string): void {
  const f = folderById(id);
  if (!f) return;
  createFolderDialog = {
    // Pre-fill the current name with the cursor at its end so the user
    // can edit it in place. (Cursor offsets are UTF-8 bytes host-side.)
    name: { value: f.name, cursor: utf8Len(f.name) },
    organizeCurrent: false,
    parent: null,
    sessionId: null,
    renameId: id,
  };
  mountFolderDialog();
}

// Shared mount path for the create / rename folder dialog.
function mountFolderDialog(): void {
  // Yield the dock's keyboard while the dialog owns it (mirrors the
  // new-session form and the context-menu confirm).
  if (openPanel && dockMode) {
    dockBlurred = true;
    editor.floatingPanelControl(openPanel.id(), "blur", 0);
  }
  const renaming = createFolderDialog!.renameId !== null;
  createFolderPanel = new FloatingWidgetPanel();
  createFolderPanel.mount(buildCreateFolderSpec(), {
    widthPct: 50,
    heightPct: 42,
    focusMarker: true,
    // The dialog's title + border are now native modal-frame chrome
    // (drawn by the host around the WidgetSpec), and `closable` renders
    // a native `[×]` that dismisses via the same cancel path as Esc.
    title: renaming
      ? editor.t("dock.rename_folder_dialog_title")
      : editor.t("dock.new_folder_dialog_title"),
    closable: true,
  });
  editor.floatingPanelControl(createFolderPanel.id(), "fullscreen", 1);
  editor.setEditorMode(CREATE_FOLDER_MODE);
  // Land focus in the name field so typing goes straight to it; the
  // whole value starts selected-for-overwrite feel via a full cursor.
  createFolderFocusKey = "folder-name";
  createFolderPanel.setFocusKey("folder-name");
}

// The dialog spec: a titled section with a "Folder name:" field, an
// optional "organize <session> under this folder" checkbox, and the
// Cancel / Create Folder buttons.
function buildCreateFolderSpec(): WidgetSpec {
  const d = createFolderDialog!;
  const renaming = d.renameId !== null;
  const sess = d.sessionId != null ? orchestratorSessions.get(d.sessionId) : undefined;
  const children: WidgetSpec[] = [
    // Render the label inline ("Folder name: ") so the ": " separator
    // appears the same way in the TUI and the web UI.
    row(
      raw([
        styledRow([
          { text: editor.t("dock.new_folder_prompt") + ": ", style: { fg: "ui.menu_disabled_fg" } },
        ]),
      ]),
      text({
        value: d.name.value,
        cursorByte: d.name.cursor,
        placeholder: editor.t("dock.new_folder_default"),
        fieldWidth: 32,
        key: "folder-name",
      }),
    ),
  ];
  if (sess) {
    children.push(
      toggle(d.organizeCurrent, editor.t("dock.new_folder_organize", { name: sess.label }), {
        key: "folder-organize",
      }),
    );
  }
  children.push(
    wrappingRow(
      button(editor.t("dock.new_folder_btn_cancel"), { intent: "danger", key: "folder-cancel" }),
      spacer(2),
      button(
        renaming
          ? editor.t("dock.rename_folder_btn")
          : editor.t("dock.new_folder_btn_create"),
        { intent: "primary", key: "folder-create" },
      ),
    ),
  );
  // The dialog's title + border come from the native modal-frame chrome
  // (see `mountFolderDialog`), so the spec is just the content column.
  return col(...children);
}

// Commit the dialog: create the folder and (when the checkbox is on)
// file the target session under it. No-op on an empty name, so the
// dialog stays open for the user to type one.
function submitCreateFolder(): void {
  const d = createFolderDialog;
  if (!d) return;
  if (d.renameId !== null) {
    // Rename mode: an emptied-out name keeps the current one (renaming
    // to nothing is never what the user meant).
    const trimmed = d.name.value.trim();
    if (trimmed) renameFolder(d.renameId, trimmed);
    closeCreateFolderDialog();
    if (openPanel && dockMode) refreshOpenDialog();
    return;
  }
  // Empty name ⇒ the default ("New Folder"), matching the placeholder.
  const name = d.name.value.trim() || editor.t("dock.new_folder_default");
  const id = createFolder(name, d.parent);
  if (d.organizeCurrent && d.sessionId != null) {
    assignSessionToFolder(d.sessionId, id);
  }
  closeCreateFolderDialog();
  if (openPanel && dockMode) {
    openPanel.setExpandedKeys("sessions", Array.from(loadExpanded()));
    refreshOpenDialog();
  }
}

// Tear down the dialog and hand keyboard focus back to the dock.
function closeCreateFolderDialog(): void {
  if (createFolderPanel) {
    createFolderPanel.unmount();
    createFolderPanel = null;
  }
  createFolderDialog = null;
  editor.setEditorMode(null);
  if (openPanel && dockMode) {
    dockBlurred = false;
    editor.floatingPanelControl(openPanel.id(), "focus", 0);
    openPanel.setFocusKey("sessions");
    refreshOpenDialog();
  }
}

// Flip one folder's expansion in the persisted set and push it to the
// host-owned tree state.
function toggleDockFolderExpansion(folderKey: string): void {
  if (!openPanel) return;
  const set = loadExpanded();
  if (set.has(folderKey)) set.delete(folderKey);
  else set.add(folderKey);
  saveExpanded();
  openPanel.setExpandedKeys("sessions", Array.from(set));
  // Re-render so the hint-bar padding tracks the tree's new height
  // (see the `expand` widget_event mirror for the host-owned fold path).
  if (dockMode && openDialog) openPanel.update(buildDockSpec());
}

// Reconcile the tree's host-owned expansion with the plugin's intent:
// every folder open while a search is active, the user's persisted set
// otherwise. Call after a rebuild (so `dockKeys` is current).
function applyDockExpansion(): void {
  if (!openPanel || !openDialog) return;
  const searching = openDialog.filter.value !== "";
  const keys = searching
    ? openDialog.dockKeys.filter((k) => k.startsWith(FOLDER_NODE_PREFIX))
    : Array.from(loadExpanded());
  openPanel.setExpandedKeys("sessions", keys);
}

// ---------------------------------------------------------------------
// Dock session context menu (right-click).
//
// A centered, dimmed floating modal (its own host slot, so the dock
// stays visible behind it) offering Visit / Archive / Delete against a
// single right-clicked session. The destructive actions (Archive,
// Delete) swap the same panel to a confirmation pane — reusing the
// modal picker's `buildConfirmPane` — before they run. Visit acts
// immediately.
// ---------------------------------------------------------------------

// Visit the session behind the context menu: switch the active window to
// it and hand keyboard focus to the editor (the dock stays visible). A
// discovered on-disk worktree has no live window, so attach a fresh
// session to it instead — mirrors the dock's Enter (`dock_activate`).
function dockMenuVisit(id: number): void {
  const s = orchestratorSessions.get(id);
  if (!s) return;
  if (s.discovered) {
    // Visit "dives in" (like the live path below, which blurs to the
    // editor), so attach with dive: true.
    void attachToWorktree({
      root: s.root,
      projectPath: s.projectPath ?? s.root,
      label: s.label,
      branch: s.branch,
      discoveredId: s.id,
      dive: true,
    });
    return;
  }
  if (id > 0 && id !== editor.activeWindow()) editor.setActiveWindow(id);
  if (openPanel && dockMode) {
    dockBlurred = true;
    editor.floatingPanelControl(openPanel.id(), "blur", 0);
    editor.setEditorMode(null);
  }
}

function buildDockMenuSpec(state: DockMenuState): WidgetSpec {
  if (state.stage === "confirm") {
    // Reuse the picker's confirmation pane (single-session form). Its
    // buttons are keyed `confirm-cancel` / `confirm-<action>`, handled
    // in the dock-menu `widget_event` block.
    return buildConfirmPane({ action: state.action, ids: [state.target.id] });
  }
  // A folder's context menu: organise actions (Rename / New Subfolder /
  // Delete Folder).
  if (state.target.kind === "folder") {
    const f = folderById(state.target.id);
    const label = f?.name ?? `[${state.target.id}]`;
    return col(
      { kind: "raw", entries: [
        styledRow([{ text: FOLDER_GLYPH + " " + label, style: { bold: true } }]),
      ] },
      button(editor.t("dock.ctx_rename"), { intent: "primary", key: "ctx-rename" }),
      button(editor.t("dock.ctx_new_subfolder"), { key: "ctx-new-subfolder" }),
      button(editor.t("dock.ctx_delete_folder"), { intent: "danger", key: "ctx-delete-folder" }),
      { kind: "raw", entries: [
        styledRow([{ text: editor.t("dock.ctx_esc_close"), style: { fg: "ui.menu_disabled_fg" } }]),
      ] },
    );
  }
  const sid = state.target.id;
  const s = orchestratorSessions.get(sid);
  const label = s?.label ?? `[${sid}]`;
  // A being-created placeholder isn't a real session: no Visit / Move /
  // Archive. It offers Retry (when failed or paused) and Dismiss.
  if (s?.pending) {
    const items: WidgetSpec[] = [
      { kind: "raw", entries: [styledRow([{ text: `${label}`, style: { bold: true } }])] },
    ];
    if (pendingActionable(s.pending)) {
      items.push(button(editor.t("dock.ctx_retry"), { intent: "primary", key: "ctx-retry" }));
    }
    items.push(button(editor.t("dock.ctx_dismiss"), { intent: "danger", key: "ctx-dismiss" }));
    items.push({
      kind: "raw",
      entries: [styledRow([{ text: editor.t("dock.ctx_esc_close"), style: { fg: "ui.menu_disabled_fg" } }])],
    });
    return col(...items);
  }
  const canArchive = bulkEligible("archive", sid);
  const canDelete = bulkEligible("delete", sid);
  // Intentionally intrinsic-width content only: NO `labeledSection`,
  // `flexSpacer`, or `fullWidth` widgets — those expand to the panel
  // width and would blow the anchored popup up to ~half the screen. The
  // host frames the box (its border) and sizes it to the widest of these
  // rows, so the popup hugs its items like a real context menu.
  return col(
    { kind: "raw", entries: [
      styledRow([{ text: `${label}`, style: { bold: true } }]),
    ] },
    button(editor.t("dock.ctx_visit"), { intent: "primary", key: "ctx-visit" }),
    button(editor.t("dock.ctx_move"), { key: "ctx-move" }),
    button(editor.t("dock.ctx_archive"), { key: "ctx-archive", disabled: !canArchive }),
    button(editor.t("dock.ctx_delete"), { intent: "danger", key: "ctx-delete", disabled: !canDelete }),
    { kind: "raw", entries: [
      styledRow([{ text: editor.t("dock.ctx_esc_close"), style: { fg: "ui.menu_disabled_fg" } }]),
    ] },
  );
}

function renderDockMenu(): void {
  if (dockMenuPanel && dockMenuState) {
    dockMenuPanel.update(buildDockMenuSpec(dockMenuState));
  }
}

// Pack a screen cell into the single numeric arg `floatingPanelControl`
// takes (the host unpacks `y << 16 | x`). Both coords fit a u16.
function packCell(col: number, row: number): number {
  return (Math.max(0, row) * 65536) + Math.max(0, col);
}

// Anchor the menu popup at its stored right-click cell — an unobtrusive,
// content-sized popup with no background dim.
function anchorDockMenu(): void {
  if (!dockMenuPanel || !dockMenuState) return;
  editor.floatingPanelControl(
    dockMenuPanel.id(),
    "anchor",
    packCell(dockMenuState.anchorCol, dockMenuState.anchorRow),
  );
}

// Open the right-click context menu for the tree node at flat `index`,
// anchored at the click cell `(col, row)`. The dock stays mounted in its
// own slot behind the popup. A folder node gets folder-organise actions;
// a session node gets Visit / Move / Archive / Delete.
function openDockContextMenu(index: number, col: number, row: number): void {
  if (!openDialog) return;
  const node = openDialog.dockNodes[index];
  if (!node) return;
  const target: DockMenuTarget = node.kind === "folder"
    ? { kind: "folder", id: node.folderId }
    : { kind: "session", id: node.sessionId };
  // Align the dock's highlighted row with the right-clicked one so the
  // menu and the tree agree on the target.
  openDialog.dockSelKey = openDialog.dockKeys[index] ?? openDialog.dockSelKey;
  if (openPanel) openPanel.setSelectedIndex("sessions", index);
  dockMenuState = { target, anchorCol: col, anchorRow: row, stage: "menu" };
  if (!dockMenuPanel) dockMenuPanel = new FloatingWidgetPanel();
  // widthPct/heightPct seed the centered confirm stage; the anchored menu
  // stage ignores them (it sizes to content). Mount, then anchor.
  dockMenuPanel.mount(buildDockMenuSpec(dockMenuState), {
    widthPct: 50,
    heightPct: 44,
  });
  anchorDockMenu();
}

// Keyboard-opened context menu (Menu key / Shift+F10 on the dock tree):
// same popup as a right-click, anchored at an estimate of the
// highlighted node's screen cell. The estimate assumes an unscrolled
// tree (the plugin doesn't mirror host scroll), clamped into the list
// area — for short lists it lands exactly on the row, for scrolled ones
// it stays inside the dock, which is all a popup anchor needs.
function openDockContextMenuFromKeyboard(): void {
  if (!openDialog || !dockMode) return;
  const key = openDialog.dockSelKey;
  const idx = key ? openDialog.dockKeys.indexOf(key) : -1;
  if (idx < 0) return;
  // Rows are variable in card density: a session card is a bordered
  // DOCK_CARD_HEIGHT + 2 rows tall, a folder header a single row.
  const card = dockView === "card";
  const nodeRows = (n: DockNode | undefined): number =>
    card && n?.kind === "session" ? DOCK_CARD_HEIGHT + 2 : 1;
  let estRow = openDialog.dockTreeTop;
  for (let i = 0; i < idx; i++) estRow += nodeRows(openDialog.dockNodes[i]);
  const maxRow = openDialog.dockTreeTop +
    Math.max(0, openDialog.listVisibleRows - 1);
  const row = Math.min(estRow, maxRow);
  const col = Math.min(6, Math.max(2, dockDefaultWidth() - 4));
  openDockContextMenu(idx, col, row);
}

// Open the "Move to Folder…" dropdown for the current workspace — the
// same flow the row context menu's Move option runs. Targets the dock's
// highlighted node when it is a session, else the active window's
// session. Shared by the palette command (which first opens the dock
// when it isn't showing — the dropdown lives in its toolbar) and the
// Filters panel's Move button.
function openMoveToFolderForCurrent(): void {
  if (!openPanel || !dockMode) {
    if (openPanel) return; // a centered modal is up — leave it alone
    openControlRoom({ dock: true });
  }
  if (!openPanel || !openDialog || !dockMode) return;
  let id: number | null = null;
  const selKey = openDialog.dockSelKey;
  if (selKey?.startsWith(SESSION_NODE_PREFIX)) {
    const sel = Number(selKey.slice(SESSION_NODE_PREFIX.length));
    if (orchestratorSessions.has(sel)) id = sel;
  }
  if (id === null) {
    const active = editor.activeWindow();
    if (orchestratorSessions.has(active)) id = active;
  }
  if (id === null) return;
  // Highlight the targeted session and give the dock the keyboard so
  // the dropdown's ↑↓/Enter path works immediately.
  openDialog.dockSelKey = sessionNodeKey(id);
  dockBlurred = false;
  editor.floatingPanelControl(openPanel.id(), "focus", 0);
  openDockMenu({ kind: "move", sessionId: id, index: 0 });
}
registerHandler("orchestrator_move", openMoveToFolderForCurrent);

// Switch the popup to the centered, full-screen-dimmed confirmation for a
// destructive action. Reuses the same panel; only the placement + spec
// change.
function dockMenuEnterConfirm(action: "archive" | "delete"): void {
  if (!dockMenuPanel || !dockMenuState) return;
  if (dockMenuState.target.kind !== "session") return;
  dockMenuState = {
    target: dockMenuState.target,
    anchorCol: dockMenuState.anchorCol,
    anchorRow: dockMenuState.anchorRow,
    stage: "confirm",
    action,
  };
  editor.floatingPanelControl(dockMenuPanel.id(), "center", 0);
  editor.floatingPanelControl(dockMenuPanel.id(), "fullscreen", 1);
  renderDockMenu();
}

function closeDockContextMenu(): void {
  if (dockMenuPanel) {
    dockMenuPanel.unmount();
    dockMenuPanel = null;
  }
  dockMenuState = null;
}

// Tear down the menu and hand keyboard focus back to the dock (whose
// keys were blurred when the popup mounted). Mirrors `restoreDockAfterForm`.
function closeDockContextMenuAndRestoreDock(): void {
  closeDockContextMenu();
  if (openPanel && dockMode) {
    dockBlurred = false;
    dockFocus = "list";
    editor.floatingPanelControl(openPanel.id(), "focus", 0);
    openPanel.setFocusKey("sessions");
    refreshOpenDialog();
  }
}

// Commit the highlighted session as the active window after a short
// debounce, so holding ↑/↓ to traverse the list doesn't thrash through
// every session in between. `fromEdge` drives the directional wipe.
function scheduleDockSwitch(fromEdge: "top" | "bottom" | null): void {
  const token = ++dockSwitchToken;
  void (async () => {
    await editor.delay(30);
    // Superseded by a later keystroke — let the latest one win.
    if (token !== dockSwitchToken) return;
    if (!openDialog || !openPanel || !dockMode || dockBlurred) return;
    const id = dockSelectedSessionId();
    if (typeof id !== "number") return;
    const sess = orchestratorSessions.get(id);
    // A being-created placeholder has no window to switch to. Arrow-nav
    // must never spawn/retry from it (that would fire on every scroll-past
    // during the debounce); it just sits highlighted, showing its status.
    if (sess?.pending) return;
    // A discovered (on-disk) worktree has no window to switch to — in the
    // dock's live-switch model the highlighted row *is* the active
    // session, so opening it means attaching a fresh session at the
    // worktree. Do that without diving (keep the dock focused) so it
    // matches switching to a live row, and you can keep arrowing. The
    // 30 ms debounce above means scrolling *past* it without pausing
    // never spawns a session.
    if (sess?.discovered) {
      void attachToWorktree({
        root: sess.root,
        projectPath: sess.projectPath ?? sess.root,
        label: sess.label,
        branch: sess.branch,
        discoveredId: sess.id,
        dive: false,
      });
      return;
    }
    if (id <= 0) return;
    if (id === editor.activeWindow()) return;
    // Switching to a not-yet-connected remote (a dormant row restored from a
    // previous run, or one whose link dropped) is non-blocking: the host
    // commits into a placeholder "Connecting…" page and connects in the
    // background (`ensure_dormant_shell` + `bring_dormant_remote_online`, the
    // #2570 path). So arrow-nav is free to open it — the 30 ms debounce above
    // already means only *pausing* on a row (not scrolling past it) triggers
    // the switch, so a held ↑/↓ never fans out connects to every remote in the
    // list. The dock keeps the row highlighted and the user can arrow away at
    // any time while it connects.
    if (fromEdge) editor.setActiveWindowAnimated(id, fromEdge);
    else editor.setActiveWindow(id);
  })();
}

// A click on a dock row is a deliberate "open this session" gesture, so
// it both switches the active window *and* hands keyboard focus to the
// editor — exactly like pressing Enter (`dock_activate`). This differs
// from arrow-nav, which only live-switches and keeps focus on the dock
// so you can keep arrowing. The switch is done synchronously here (the
// 30 ms debounce in `scheduleDockSwitch` exists only to absorb held
// arrow keys; a click has no such stream) and the pending debounce token
// is bumped so any in-flight arrow switch can't clobber this one.
function diveDockSelectionFromClick(fromEdge: "top" | "bottom" | null): void {
  if (!openDialog || !openPanel || !dockMode) return;
  dockSwitchToken++;
  const id = dockSelectedSessionId();
  if (typeof id !== "number") return;
  const sess = orchestratorSessions.get(id);
  // A being-created placeholder has no window to dive into. A click/Enter
  // resumes a paused/failed one (Enter = retry); a still-creating one has
  // nothing to do but keep showing its progress.
  if (sess?.pending) {
    if (pendingActionable(sess.pending)) retryPending(id);
    return;
  }
  // A discovered (on-disk) worktree has no live window — attach a fresh
  // session and dive in (attachToWorktree hands focus to the editor).
  if (sess?.discovered) {
    void attachToWorktree({
      root: sess.root,
      projectPath: sess.projectPath ?? sess.root,
      label: sess.label,
      branch: sess.branch,
      discoveredId: sess.id,
      dive: true,
    });
    return;
  }
  if (id > 0 && id !== editor.activeWindow()) {
    if (fromEdge) editor.setActiveWindowAnimated(id, fromEdge);
    else editor.setActiveWindow(id);
  }
  // Hand keyboard focus to the activated window (mirror `dock_activate`).
  dockBlurred = true;
  editor.floatingPanelControl(openPanel.id(), "blur", 0);
  editor.setEditorMode(null);
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
      // Always mint the workspace's capability token (see runLocalCreate).
      commandAllowlist: FRESH_CLI_DEFAULT_ALLOWLIST,
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
      editor.t("status.replacement_failed", {
        error: e instanceof Error ? e.message : String(e),
      }),
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
  if (!s) return { ok: false, err: editor.t("err.workspace_gone") };
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
    // Tombstone before the (async) close so a mid-archive
    // `refreshOpenDialog` doesn't reconcile the workspace back in from the
    // still-stale window snapshot.
    closingWindowIds.add(id);
    editor.closeWindow(id);
    // Brief settle so the filesystem reflects the pty's exit before we
    // move the worktree out from under it.
    if (removable) await editor.delay(250);
  }

  if (removable) {
    // Owns a worktree: move it to the `.archived/` graveyard so git's
    // bookkeeping stays consistent and Unarchive can move it back.
    const repoRoot = await worktreeRepoRoot(s);
    if (!repoRoot) return { ok: false, err: editor.t("err.not_git_repo") };
    const archivedRoot = editor.pathJoin(
      editor.getDataDir(),
      "orchestrator",
      slugify(repoRoot),
      ".archived",
      s.label,
    );
    const parent = editor.pathDirname(archivedRoot);
    if (!editor.createDir(parent)) {
      return { ok: false, err: editor.t("err.could_not_create", { path: parent }), repoRoot };
    }
    const moveRes = await spawnCollect(
      "git",
      ["-C", repoRoot, "worktree", "move", s.root, archivedRoot],
      repoRoot,
    );
    if (moveRes.exit_code !== 0) {
      return {
        ok: false,
        err: lastNonEmptyLine(moveRes.stderr) || editor.t("err.worktree_move_failed"),
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
  if (!s) return { ok: false, err: editor.t("err.workspace_gone") };
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
    // Tombstone before the (async) close so any `refreshOpenDialog` that
    // runs before the host processes it doesn't reconcile the workspace
    // straight back in from the still-stale window snapshot.
    closingWindowIds.add(id);
    editor.closeWindow(id);
    if (removable) await editor.delay(250);
  }

  let repoRoot: string | undefined;
  if (removable) {
    const rr = await worktreeRepoRoot(s);
    if (!rr) return { ok: false, err: editor.t("err.not_git_repo") };
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
        err: lastNonEmptyLine(removeRes.stderr) || editor.t("err.worktree_remove_failed"),
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
    setDialogError(editor.t("err.nothing_eligible", { action }));
    refreshOpenDialog();
    return;
  }

  if (action === "stop") {
    let n = 0;
    for (const id of targets) if (stopOne(id)) n += 1;
    editor.setStatus(editor.t("status.stop_sent", { count: String(n) }));
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
      lastErr = res.err ?? editor.t("err.failed");
    }
    openDialog?.selectedIds.delete(id);
    if (openDialog?.bulkInFlight) openDialog.bulkInFlight.done = i + 1;
    refreshOpenDialog();
  }
  if (openDialog) {
    openDialog.inFlight = null;
    openDialog.bulkInFlight = null;
  }

  const verb = action === "archive" ? editor.t("status.verb_archived") : editor.t("status.verb_deleted");
  if (okCount === 0) {
    setDialogError(editor.t("err.action_failed", { action, error: lastErr || editor.t("err.unknown_error") }));
  } else if (lastErr) {
    setDialogError(editor.t("err.partial_done", { verb, ok: String(okCount), total: String(targets.length), error: lastErr }));
  } else {
    editor.setStatus(editor.t("status.bulk_done", { verb, count: String(okCount) }));
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
  // Turning "Show all worktrees" ON re-scans *now* so the rows reflect every
  // project's on-disk worktrees at this moment — not just whatever the single
  // scan at dialog-open time happened to catch. Discovery walks every known
  // project (each open/persisted session's repo, plus the cwd repo), so a
  // fresh scan here is what makes the toggle show worktrees across all
  // projects rather than a stale subset. Async; `refreshDiscoveredWorktrees`
  // re-renders the dialog when the scan lands.
  if (openDialog.showWorktrees) {
    void refreshDiscoveredWorktrees();
  }
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
  // Tab cycle (mirrors the host's tabbable, which now skips non-active
  // radio options): the *active* "Run in:" tab, then the active
  // backend's fields, then the shared Session Name / Agent Command,
  // then the buttons. ←/→ moves within the radio groups, never Tab —
  // so each group is a single Tab stop.
  const activeBackend = SESSION_BACKENDS.find((b) => b.id === form.backend);
  const cycle: string[] = activeBackend ? [activeBackend.key] : [];
  if (form.backend === "local") {
    const worktreeEnabled = form.projectPathIsGit !== false;
    const effectiveCreateWorktree = worktreeEnabled && form.createWorktree;
    cycle.push("project_path", "name", "cmd");
    // Agent-specific controls sit between the command and the Advanced fold,
    // matching `agentOptionsFields`' render order (Auto mode, then Start prompt).
    // "Teach Fresh CLI" moved under Advanced, so it's a Tab stop there, not here.
    const agent = activeAgentEntry();
    if (agent?.auto) cycle.push("auto_mode");
    if (agent?.prompt) cycle.push("start_prompt");
    // The "Advanced…" header is always a Tab stop; its folded fields join the
    // cycle only while the section is expanded, in render order: Teach Fresh
    // CLI, then worktree, then "Checkout branch" (a stop on any git path — it
    // drives the in-place checkout when no worktree is created), then "New
    // branch name" (only when cutting a worktree).
    cycle.push("advanced_toggle");
    if (form.advancedExpanded) {
      if (agent?.systemPrompt) cycle.push("teach_fresh_cli");
      if (worktreeEnabled) cycle.push("worktree");
      if (worktreeEnabled) cycle.push("branch");
      if (effectiveCreateWorktree) cycle.push("new_branch");
    }
  } else if (form.backend === "devcontainer") {
    cycle.push("project_path", "name", "cmd");
  } else if (form.backend === "ssh") {
    cycle.push("ssh_host", "ssh_path", "ssh_identity", "ssh_options", "name", "cmd");
  } else if (form.backend === "kubernetes") {
    cycle.push("k8s_target");
    if (form.k8sTarget.value.trim().length === 0) {
      cycle.push("k8s_context", "k8s_namespace", "k8s_pod", "k8s_workspace");
    }
    cycle.push("name", "cmd");
  }
  // The agent selector is a single dropdown Tab stop just before the Agent
  // Command field (←/→ or ↑/↓ cycles the options), present for every backend.
  const cmdIdx = cycle.indexOf("cmd");
  if (cmdIdx >= 0) {
    cycle.splice(cmdIdx, 0, "agent_dropdown");
  }
  cycle.push("create-visit", "create-bg", "cancel");
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

// =============================================================================
// Agent resume registry
//
// How known coding agents rejoin a prior conversation after an editor restart.
// This is *policy/data*: the host core knows none of it — it just persists the
// resolved `resume` argv and runs it on restore (see the `resume` option on
// `createWindowWithTerminal` and `terminal.resume_agents`). Two strategies,
// preferring the first when an agent supports it:
//
//   provision — mint a session id at launch (`<agent> … --session-id <uuid>`)
//               and resume with it (`<agent> --resume <uuid>`). Precise: the id
//               is ours from birth, so there's nothing to capture and no need
//               to read the agent's private state. The uuid is a plain argv
//               element, never interpolated into a shell string.
//   continue  — resume the most recent session in the cwd (`<agent> --continue`),
//               no id. Relies on the orchestrator's one-agent-per-worktree
//               model, where "latest in this cwd" is unambiguous.
//
// Matched by argv0 basename. Flags are each agent's documented resume
// interface; entries are easy to add and intended to become user-overridable.
// `{id}` in a template is replaced with the minted uuid (array slot only).
interface AgentResumeSpec {
  provision?: { idFlag: string; resumeArgs: string[] };
  continue?: { resumeArgs: string[] };
}
// How an agent takes an initial prompt on the command line: as a trailing
// positional (`claude "prompt"`) or behind a flag (`opencode --prompt "…"`,
// `aider -m "…"`). Absent ⇒ the agent has no launch-prompt argument and the
// New Session prompt box is hidden for it.
type AgentPromptArg =
  | { style: "positional" }
  | { style: "flag"; flag: string };
// How to hand an agent the "drive the Fresh editor from the shell" system
// prompt when "Teach Fresh CLI" is on: either appended to launch argv behind a
// flag (`claude --append-system-prompt "…"`), or written into a file the agent
// reads at startup (`AGENTS.md` for codex/opencode). Absent ⇒ the agent has no
// autonomous shell to drive the editor with, so the checkbox stays hidden.
type AgentSystemPrompt =
  | { via: "flag"; flag: string }
  | { via: "file"; path: string };
interface AgentEntry {
  // The command the New Session dropdown fills in and the basename the matcher
  // keys on.
  id: string;
  // Human label for the preset button. Falls back to `id` when omitted.
  label?: string;
  // Resolves a path/args form (e.g. `/usr/bin/claude --foo`) to this entry.
  match: RegExp;
  // Resume strategy across editor restarts (see `resolveAgentLaunch`).
  spec: AgentResumeSpec;
  // Flag(s) enabling the agent's "auto"/bypass-approvals mode. Absent ⇒ the
  // agent has no such flag (opencode gates this via config, not a flag), so the
  // "Auto mode" checkbox is hidden for it.
  auto?: string[];
  // How the agent accepts an initial prompt at launch. Absent ⇒ no prompt box.
  prompt?: AgentPromptArg;
  // How to inject the "drive the Fresh editor" system prompt when the user
  // enables "Teach Fresh CLI". Absent ⇒ the agent has no autonomous shell to
  // drive the editor (aider), so the checkbox stays hidden for it.
  systemPrompt?: AgentSystemPrompt;
}
// The four launcher-priority agents come first (claude, codex, opencode), then
// the long-standing aider entry. Order here drives the preset-row order.
const AGENT_REGISTRY: AgentEntry[] = [
  {
    // Claude Code CLI: `--session-id <uuid>` pins the session at launch;
    // `--resume <uuid>` rejoins it; `--continue` resumes the latest in cwd.
    id: "claude",
    label: "claude",
    match: /^claude$/,
    spec: {
      provision: { idFlag: "--session-id", resumeArgs: ["--resume", "{id}"] },
      continue: { resumeArgs: ["--continue"] },
    },
    // "Auto mode" = `--permission-mode auto`: the safe-autonomous mode (a
    // classifier vets actions before they run) — deliberately NOT
    // `--dangerously-skip-permissions` (which is `bypassPermissions`, the
    // unchecked maximal bypass, reserved for isolated containers).
    auto: ["--permission-mode", "auto"],
    prompt: { style: "positional" },
    systemPrompt: { via: "flag", flag: "--append-system-prompt" },
  },
  {
    // OpenAI Codex CLI: resume is a *subcommand*, not a flag — `codex resume
    // --last` rejoins the latest session in the cwd. There's no launch-time
    // session-id to pin, so it's continue-only. `--full-auto` is codex's
    // auto-approve mode (workspace-write sandbox, no prompts) — NOT the
    // `--dangerously-bypass-approvals-and-sandbox` full bypass; the initial
    // prompt is a trailing positional (`codex "…"`).
    id: "codex",
    label: "codex",
    match: /^codex$/,
    spec: { continue: { resumeArgs: ["resume", "--last"] } },
    auto: ["--full-auto"],
    prompt: { style: "positional" },
    systemPrompt: { via: "file", path: "AGENTS.md" },
  },
  {
    // opencode (SST): `--continue` resumes the latest session in the cwd.
    // "Auto"/YOLO mode is config-driven (permissions in opencode.json), so it
    // has no launch flag — the checkbox is hidden. `--prompt` pre-seeds the TUI.
    id: "opencode",
    label: "opencode",
    match: /^opencode$/,
    spec: { continue: { resumeArgs: ["--continue"] } },
    prompt: { style: "flag", flag: "--prompt" },
    systemPrompt: { via: "file", path: "AGENTS.md" },
  },
  {
    // aider keeps its conversation in the repo and reloads it with
    // `--restore-chat-history`; it has no caller-supplied session id, so it's
    // a continue-only (strategy B) agent. `--yes-always` auto-confirms; `-m`
    // hands it a message.
    id: "aider",
    label: "aider",
    match: /^aider$/,
    spec: { continue: { resumeArgs: ["--restore-chat-history"] } },
    auto: ["--yes-always"],
    prompt: { style: "flag", flag: "-m" },
  },
];

// Command ids the workspace's capability token is minted for. Conservative and
// safe: these two back the `fresh --cmd split` alias, letting a process in the
// workspace split its own view without exposing the full command surface.
// Passed to the host as `commandAllowlist` on *every* workspace creation, which
// binds the minted `FRESH_CMD_TOKEN` to exactly these ids on the new window —
// the token is always present; the "Teach Fresh CLI" toggle only controls
// whether the agent is *told* about it (the system-prompt injection).
const FRESH_CLI_DEFAULT_ALLOWLIST = ["split_vertical", "split_horizontal"];

// System prompt injected (via flag or AGENTS.md) when "Teach Fresh CLI" is on.
// Teaches the agent the verbatim `fresh` CLI verbs it can drive the editor
// with. Keep the command strings exact — they're the agent's only reference.
const FRESH_CLI_SYSTEM_PROMPT = [
  "You are running inside a Fresh editor workspace and can control it from the shell via the `fresh` CLI.",
  "Always invoke it through the `$FRESH_BIN` environment variable — it points at the exact editor binary running this workspace, so its `--cmd` verbs and `--help` match this build (never rely on a bare `fresh` from PATH, which may be a different version).",
  'Discover what you can do: `"$FRESH_BIN" --cmd cmd list --json` (lists the commands you\'re allowed to run in this workspace).',
  'Run one: `"$FRESH_BIN" --cmd cmd run <id>` (e.g. `"$FRESH_BIN" --cmd cmd run split_vertical`), or the shortcut `"$FRESH_BIN" --cmd split --vertical`.',
  'Open a file in this workspace: `"$FRESH_BIN" <path>` (this blocks until you close the file, so use it for hand-offs, not quick peeks).',
  'Open another project as a new workspace: `"$FRESH_BIN" --cmd workspace new <dir>`.',
  "These commands act only on the current workspace.",
].join("\n");

// Marker wrapping the injected block so an existing user file (codex/opencode
// `AGENTS.md`) is amended, never clobbered, and a retry/recovery run stays
// idempotent (the block is added at most once).
const FRESH_CLI_BLOCK_START = "<!-- fresh-cli:start -->";
const FRESH_CLI_BLOCK_END = "<!-- fresh-cli:end -->";

// Write (or append) the Fresh CLI system prompt into an agent-read file
// (`AGENTS.md`). If the file already exists, append a clearly-marked block
// rather than overwriting the user's content; otherwise create it fresh.
function writeFreshCliPromptFile(path: string): void {
  const block = `${FRESH_CLI_BLOCK_START}\n${FRESH_CLI_SYSTEM_PROMPT}\n${FRESH_CLI_BLOCK_END}\n`;
  if (editor.fileExists(path)) {
    const existing = editor.readFile(path) ?? "";
    // Idempotent on retry / restart-recovery: never stack duplicate blocks.
    if (existing.includes(FRESH_CLI_BLOCK_START)) return;
    const sep = existing.length === 0 || existing.endsWith("\n") ? "\n" : "\n\n";
    editor.writeFile(path, existing + sep + block);
  } else {
    editor.writeFile(path, block);
  }
}

// The registry entry a typed command resolves to (by argv0 basename), or null
// for a bare terminal / unknown command. Drives which agent-only controls
// (Auto mode, Start prompt) the form surfaces.
function agentEntryForCmd(cmd: string): AgentEntry | null {
  const argv = splitAgentCmd(cmd);
  if (argv.length === 0) return null;
  const base = editor.pathBasename(argv[0]) || argv[0];
  return AGENT_REGISTRY.find((e) => e.match.test(base)) ?? null;
}

// The agent (if any) the form's current command resolves to.
function activeAgentEntry(): AgentEntry | null {
  return form ? agentEntryForCmd(form.cmd.value) : null;
}

// Build the argv fragment that hands `prompt` to an agent per its prompt style.
function agentPromptArgs(spec: AgentPromptArg, prompt: string): string[] {
  return spec.style === "flag" ? [spec.flag, prompt] : [prompt];
}

// Presets for the New Session "Agent Command" dropdown: the plain shell
// (default), every registry agent (which a restart will resume), and a
// "custom…" entry that just hands focus to the free-text field so the user can
// type any command. Built from the registry so adding an agent surfaces it in
// the UI automatically. `custom` presets leave the command untouched.
interface AgentPreset {
  label: string;
  cmd: string;
  key: string;
  resumes: boolean;
  custom?: boolean;
}
function agentPresets(): AgentPreset[] {
  const presets: AgentPreset[] = [
    { label: editor.t("form.agent_terminal"), cmd: "", key: "agent-preset-terminal", resumes: false },
  ];
  for (const e of AGENT_REGISTRY) {
    presets.push({
      label: e.label ?? e.id,
      cmd: e.id,
      key: `agent-preset-${e.id}`,
      resumes: true,
    });
  }
  presets.push({
    label: editor.t("form.agent_custom"),
    cmd: "",
    key: "agent-preset-custom",
    resumes: false,
    custom: true,
  });
  return presets;
}

// Which preset the current command text corresponds to: an exact match on a
// known agent / the empty shell, else "custom…" (covers a typed command or an
// agent with extra args). Drives the dropdown's active highlight.
function activeAgentPresetKey(): string {
  const current = form ? form.cmd.value.trim() : "";
  const match = agentPresets().find((p) => !p.custom && p.cmd === current);
  return match ? match.key : "agent-preset-custom";
}

// Apply a dropdown choice: a normal preset fills the command field; the
// "custom…" entry just moves focus to that field so the user can type, leaving
// any existing text in place.
function applyAgentPreset(p: AgentPreset): void {
  if (!form) return;
  if (p.custom) {
    // Hand focus to the free-text field so the user can type a command.
    // Focus must be set *after* the re-render — re-mounting the spec resets
    // host focus, which would otherwise clobber the setFocusKey.
    renderForm();
    formPanel?.setFocusKey("cmd");
    snapFormFocusTo("cmd");
    return;
  }
  form.cmd.value = p.cmd;
  form.cmd.cursor = p.cmd.length;
  // The Text widget's content is host-authoritative; push the new value into
  // it (re-rendering the spec alone won't change an already-mounted input).
  formPanel?.setValue("cmd", form.cmd.value, form.cmd.cursor);
  renderForm();
}

// A v4-style unique id for an agent session handle. Not security-sensitive
// (it just names a conversation), so `Math.random` is fine — we never need
// unpredictability, only uniqueness within a user's session store.
function agentSessionUuid(): string {
  const hex = "0123456789abcdef";
  let s = "";
  for (let i = 0; i < 32; i++) {
    if (i === 8 || i === 12 || i === 16 || i === 20) s += "-";
    if (i === 12) {
      s += "4"; // version
    } else if (i === 16) {
      s += hex[8 + Math.floor(Math.random() * 4)]; // variant 8–b
    } else {
      s += hex[Math.floor(Math.random() * 16)];
    }
  }
  return s;
}

// Resolve a user's agent argv into the argv to *launch* and the argv to run on
// *restore* (resume), per the registry. Unknown commands (plain shells, custom
// agents) pass through unchanged with no resume — i.e. today's behaviour.
function resolveAgentLaunch(
  argv: string[],
  opts?: { auto?: boolean; prompt?: string; systemPrompt?: string },
): { launch: string[]; resume?: string[] } {
  if (argv.length === 0) return { launch: argv };
  const argv0 = argv[0];
  const base = editor.pathBasename(argv0) || argv0;
  const entry = AGENT_REGISTRY.find((e) => e.match.test(base));
  // Unknown command (a plain shell / custom binary): pass through untouched.
  // Auto mode and the start prompt are agent-registry features, so there's
  // nothing to inject here.
  if (!entry) return { launch: argv };

  // Auto-mode flags ride on *both* launch and resume — a resumed session
  // keeps the approval posture the user chose. Prompt is launch-only: it seeds
  // the first turn and must never be replayed when rejoining the conversation.
  const autoArgs = opts?.auto && entry.auto ? entry.auto : [];
  const prompt = (opts?.prompt ?? "").trim();
  const promptArgs = prompt && entry.prompt
    ? agentPromptArgs(entry.prompt, prompt)
    : [];
  // "Teach Fresh CLI" for a flag-style agent (claude) rides launch only —
  // like the start prompt, it seeds the first turn and must not replay on
  // resume. File-style agents (codex/opencode) get the text written into a
  // file instead, so they never reach here.
  const sysPrompt = (opts?.systemPrompt ?? "").trim();
  const sysPromptArgs = sysPrompt && entry.systemPrompt?.via === "flag"
    ? [entry.systemPrompt.flag, sysPrompt]
    : [];
  // Flags first (auto + system prompt), then the (trailing) positional prompt
  // so a positional start-prompt stays last.
  const withAuto = [...argv, ...autoArgs, ...sysPromptArgs];

  if (entry.spec.provision) {
    const id = agentSessionUuid();
    const { idFlag, resumeArgs } = entry.spec.provision;
    return {
      launch: [...withAuto, idFlag, id, ...promptArgs],
      resume: [argv0, ...resumeArgs.map((a) => a.replace("{id}", id)), ...autoArgs],
    };
  }
  if (entry.spec.continue) {
    return {
      launch: [...withAuto, ...promptArgs],
      resume: [argv0, ...entry.spec.continue.resumeArgs, ...autoArgs],
    };
  }
  return { launch: [...withAuto, ...promptArgs] };
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

// Subtitle splits the static prefix "Project:" from the project
// path so each gets its own foreground — matching the three-tier
// (label / label-value / input) palette the design calls for.
const SUBTITLE_LABEL_STYLE = { fg: "ui.menu_disabled_fg" } as const;
const SUBTITLE_VALUE_STYLE = { fg: "ui.help_key_fg", bold: true } as const;

// === New Session: session-type ("Run in:") tabs + per-backend fields =======

// Switch the New Session form to a different backend tab: swap the body,
// rebuild the Tab cycle, and land focus back on the chosen tab so repeated
// Tab/Enter or ←/→ feels stable.
function selectBackend(backend: SessionBackend): void {
  if (!form || !formPanel || form.backend === backend) return;
  form.backend = backend;
  form.lastError = null;
  closeCompletion();
  renderForm();
  const tab = SESSION_BACKENDS.find((b) => b.id === backend);
  if (tab) {
    formPanel.setFocusKey(tab.key);
    snapFormFocusTo(tab.key);
  }
}

// The first focusable input of a backend's body — where Enter on the active
// tab dives to (skipping the other tab buttons).
function firstBodyFieldKey(backend: SessionBackend): string {
  switch (backend) {
    case "local":
    case "devcontainer":
      return "project_path";
    case "ssh":
      return "ssh_host";
    case "kubernetes":
      return "k8s_target";
  }
}

// "Run in:" tab row. One button per backend; the active one is `primary`. The
// body below (`backendBodyFields`) swaps to match. The tab buttons carry keys
// (`type-local` …) so they sit in the form's Tab cycle; ←/→ also switches.
function backendTabsRow(): WidgetSpec {
  const sel: SessionBackend = form ? form.backend : "local";
  const parts: WidgetSpec[] = [
    {
      kind: "raw",
      entries: [styledRow([{ text: editor.t("form.run_in"), style: { fg: "ui.menu_disabled_fg" } }])],
    },
  ];
  for (const b of SESSION_BACKENDS) {
    parts.push(spacer(1));
    // Only the active tab is a Tab stop; ←/→ moves within the group.
    // So Tab advances one stop per group, not one per option (and the
    // `▸` focus marker only ever lands on the active tab).
    parts.push(button(b.label, {
      key: b.key,
      intent: b.id === sel ? "primary" : undefined,
      focusable: b.id === sel,
    }));
  }
  parts.push(flexSpacer());
  parts.push({
    kind: "raw",
    entries: [styledRow([{ text: editor.t("form.switch_type"), style: { fg: "ui.menu_disabled_fg", italic: true } }])],
  });
  return row(...parts);
}

// "Agent:" preset row above the Agent Command field. One button per known
// agent (plus the default plain `terminal`); picking one fills the command.
// Agents that resume across restarts are tagged with `↻`, and the row spells
// that out — so a user discovers both that `claude` is an option and that it
// gets special session handling. The resume tag only shows for the local
// backend, the one where resume is wired today.
// Agent selector: a single dropdown of the preset labels (terminal, the
// known agents, custom…). ←/→ or ↑/↓ over it cycles; the change event maps
// the chosen index back to a preset and applies it (fills the command field).
function agentPresetRow(): WidgetSpec {
  const presets = agentPresets();
  const activeKey = activeAgentPresetKey();
  const selectedIndex = Math.max(0, presets.findIndex((p) => p.key === activeKey));
  return dropdown(presets.map((p) => p.label), {
    selectedIndex,
    // Strip the trailing colon from the shared "Agent:" label — the dropdown
    // widget adds its own separator, so the raw string renders "Agent::".
    label: editor.t("form.agent").replace(/:\s*$/, ""),
    key: "agent_dropdown",
  });
}

// Agent-specific controls shown below the Agent Command field: a "Start
// prompt" box (for agents that take a launch prompt) and an "Auto mode"
// checkbox (for agents with a bypass-approvals flag). Both are local-only —
// remote backends don't route through `resolveAgentLaunch` — and adapt to the
// resolved agent: a bare terminal / unknown command shows neither, opencode
// shows only the prompt (no auto flag), etc.
function agentOptionsFields(): WidgetSpec[] {
  if (!form || form.backend !== "local") return [];
  const entry = activeAgentEntry();
  if (!entry) return [];
  const fields: WidgetSpec[] = [];
  if (entry.auto) {
    fields.push(
      toggle(form.autoMode, editor.t("form.auto_mode"), { key: "auto_mode" }),
    );
  }
  // "Teach Fresh CLI" lives under the Advanced fold (see `advancedSection`),
  // so it stays hidden by default even though it's enabled by default.
  if (entry.prompt) {
    fields.push(
      labeledSection({
        label: editor.t("form.start_prompt"),
        child: text({
          value: form.startPrompt.value,
          cursorByte: form.startPrompt.cursor,
          placeholder: editor.t("form.start_prompt_placeholder"),
          fullWidth: true,
          // Single-line to stay consistent with the form's keyboard model
          // (Enter advances focus, Ctrl+Enter submits); the whole prompt is
          // handed to the agent as one launch argument.
          key: "start_prompt",
        }),
      }),
    );
  }
  return fields;
}

// Local backend: Project Path + linked-worktree hint. The worktree toggle
// and branch fields moved into the collapsible `advancedSection()`.
function localBodyFields(): WidgetSpec[] {
  if (!form) return [];
  const fields: WidgetSpec[] = [
    labeledSection({
      label: editor.t("form.project_path"),
      child: text({
        value: form.projectPath.value,
        cursorByte: form.projectPath.cursor,
        // Label the placeholder explicitly as the default-if-blank so
        // it can't be mistaken for a real prefilled value (it's also
        // rendered dim-italic, but that's invisible in a plain
        // capture). Submitting with the field empty uses this path.
        placeholder: form.defaultProjectPath
          ? editor.t("form.project_path_default", { path: form.defaultProjectPath })
          : editor.t("form.detecting_project_root"),
        fullWidth: true,
        key: "project_path",
      }),
    }),
  ];
  if (form.projectPathIsLinkedWorktree === true) {
    fields.push({
      kind: "raw",
      entries: [
        styledRow([
          {
            text: form.createWorktree
              ? editor.t("form.linked_worktree_uncheck")
              : editor.t("form.linked_worktree_attach"),
            style: { fg: "ui.help_key_fg", italic: true },
          },
        ]),
      ],
    });
  }
  return fields;
}

// Collapsible "Advanced…" section (local backend). Collapsed → just the
// clickable header. Expanded → header + the worktree toggle (moved out of the
// always-visible body) + the "Checkout branch" and "New branch name" fields.
// Keeping these behind a fold keeps the common case (accept the defaults)
// compact while still exposing full git control.
function advancedSection(): WidgetSpec[] {
  if (!form) return [];
  const expanded = form.advancedExpanded;
  const header = button(
    `${expanded ? "▾" : "▸"} ${editor.t("form.advanced")}`,
    { key: "advanced_toggle" },
  );
  if (!expanded) return [header];

  const worktreeEnabled = form.projectPathIsGit !== false;
  const effectiveCreateWorktree = worktreeEnabled && form.createWorktree;
  const fields: WidgetSpec[] = [header];

  // "Teach Fresh CLI" — enabled by default, but folded away here so it doesn't
  // clutter the common case. Only meaningful for an agent with a systemPrompt
  // injection strategy (a bare terminal / unknown command can't be "taught").
  if (activeAgentEntry()?.systemPrompt) {
    fields.push(
      toggle(form.teachFreshCli, editor.t("form.teach_fresh_cli"), {
        key: "teach_fresh_cli",
      }),
    );
  }

  // Worktree toggle: a checkbox on a git path, else a disabled hint.
  fields.push(
    worktreeEnabled
      ? toggle(effectiveCreateWorktree, editor.t("form.create_worktree"), {
          key: "worktree",
        })
      : {
          kind: "raw",
          entries: [
            styledRow([
              {
                text: editor.t("form.create_worktree_disabled"),
                style: { fg: "editor.whitespace_indicator_fg" },
              },
              {
                text: editor.t("form.disabled_non_git"),
                style: { fg: "editor.whitespace_indicator_fg", italic: true },
              },
            ]),
          ],
        },
  );

  // "Checkout branch" — an existing branch to check out. Editable for ANY git
  // path (inert only on a non-git path): with a worktree it's the base the
  // worktree is cut from / checked out to; without one it drives an in-place
  // `git checkout` in the project dir. The placeholder reflects which.
  const branchInert = !worktreeEnabled;
  let branchPlaceholder: string;
  if (!worktreeEnabled) {
    branchPlaceholder = editor.t("form.branch_no_git");
  } else if (!effectiveCreateWorktree) {
    // In-place checkout: blank keeps the current branch.
    branchPlaceholder = editor.t("form.branch_checkout_inplace");
  } else if (!form.defaultBranch) {
    branchPlaceholder = editor.t("form.detecting_default_branch");
  } else if (form.defaultBranchIsHeadFallback) {
    branchPlaceholder = editor.t("form.branch_head_fallback");
  } else {
    branchPlaceholder = form.defaultBranch;
  }
  fields.push(
    labeledSection({
      label: editor.t("form.checkout_branch"),
      child: text({
        value: form.branch.value,
        cursorByte: form.branch.cursor,
        placeholder: branchPlaceholder,
        fullWidth: true,
        key: branchInert ? undefined : "branch",
      }),
    }),
  );

  // "New branch name" — creates the worktree on a freshly-cut branch. Only
  // meaningful when a worktree is being created (there's no isolated tree to
  // safely branch into in-place), so it's inert otherwise.
  fields.push(
    labeledSection({
      label: editor.t("form.new_branch"),
      child: text({
        value: form.newBranch.value,
        cursorByte: form.newBranch.cursor,
        placeholder: effectiveCreateWorktree
          ? ""
          : editor.t("form.new_branch_worktree_only"),
        fullWidth: true,
        key: effectiveCreateWorktree ? "new_branch" : undefined,
      }),
    }),
  );

  return fields;
}

// Devcontainer backend: a Project Path that contains a `.devcontainer/`.
function devcontainerBodyFields(): WidgetSpec[] {
  if (!form) return [];
  return [
    labeledSection({
      label: editor.t("form.project_path"),
      child: text({
        value: form.projectPath.value,
        cursorByte: form.projectPath.cursor,
        placeholder: form.defaultProjectPath
          ? editor.t("form.project_path_default", { path: form.defaultProjectPath })
          : editor.t("form.devcontainer_path_placeholder"),
        fullWidth: true,
        key: "project_path",
      }),
    }),
    {
      kind: "raw",
      entries: [
        styledRow([
          {
            text: editor.t("form.devcontainer_hint"),
            style: { fg: "ui.menu_disabled_fg", italic: true },
          },
        ]),
      ],
    },
  ];
}

// SSH backend: host (`[user@]host[:port]`), remote path, optional identity
// file, and free-form extra ssh arguments.
function sshBodyFields(): WidgetSpec[] {
  if (!form) return [];
  return [
    labeledSection({
      label: editor.t("form.ssh_host_label"),
      child: text({
        value: form.sshHost.value,
        cursorByte: form.sshHost.cursor,
        placeholder: editor.t("form.ssh_host_placeholder"),
        fullWidth: true,
        key: "ssh_host",
      }),
    }),
    labeledSection({
      label: editor.t("form.ssh_remote_path_label"),
      child: text({
        value: form.sshPath.value,
        cursorByte: form.sshPath.cursor,
        placeholder: editor.t("form.ssh_remote_path_placeholder"),
        fullWidth: true,
        key: "ssh_path",
      }),
    }),
    labeledSection({
      label: editor.t("form.ssh_identity_label"),
      child: text({
        value: form.sshIdentity.value,
        cursorByte: form.sshIdentity.cursor,
        placeholder: editor.t("form.ssh_identity_placeholder"),
        fullWidth: true,
        key: "ssh_identity",
      }),
    }),
    labeledSection({
      label: editor.t("form.ssh_options_label"),
      child: text({
        value: form.sshOptions.value,
        cursorByte: form.sshOptions.cursor,
        placeholder: editor.t("form.ssh_options_placeholder"),
        fullWidth: true,
        key: "ssh_options",
      }),
    }),
  ];
}

// Kubernetes backend: a saved target, or explicit context/namespace/pod/ws.
function k8sBodyFields(): WidgetSpec[] {
  if (!form) return [];
  const hasTarget = form.k8sTarget.value.trim().length > 0;
  const fields: WidgetSpec[] = [
    labeledSection({
      label: editor.t("form.k8s_target_label"),
      child: text({
        value: form.k8sTarget.value,
        cursorByte: form.k8sTarget.cursor,
        placeholder: editor.t("form.k8s_target_placeholder"),
        fullWidth: true,
        key: "k8s_target",
      }),
    }),
  ];
  if (!hasTarget) {
    fields.push(
      labeledSection({
        label: editor.t("form.k8s_context_label"),
        child: text({
          value: form.k8sContext.value,
          cursorByte: form.k8sContext.cursor,
          placeholder: editor.t("form.k8s_context_placeholder"),
          fullWidth: true,
          key: "k8s_context",
        }),
      }),
      labeledSection({
        label: editor.t("form.k8s_namespace_label"),
        child: text({
          value: form.k8sNamespace.value,
          cursorByte: form.k8sNamespace.cursor,
          placeholder: editor.t("form.k8s_namespace_placeholder"),
          fullWidth: true,
          key: "k8s_namespace",
        }),
      }),
      labeledSection({
        label: editor.t("form.k8s_pod_label"),
        child: text({
          value: form.k8sPod.value,
          cursorByte: form.k8sPod.cursor,
          placeholder: editor.t("form.k8s_pod_placeholder"),
          fullWidth: true,
          key: "k8s_pod",
        }),
      }),
      labeledSection({
        label: editor.t("form.k8s_workspace_label"),
        child: text({
          value: form.k8sWorkspace.value,
          cursorByte: form.k8sWorkspace.cursor,
          placeholder: editor.t("form.k8s_workspace_placeholder"),
          fullWidth: true,
          key: "k8s_workspace",
        }),
      }),
    );
  }
  fields.push({
    kind: "raw",
    entries: [
      styledRow([
        {
          text: editor.t("form.k8s_hint"),
          style: { fg: "ui.menu_disabled_fg", italic: true },
        },
      ]),
    ],
  });
  return fields;
}

// The backend-specific top fields, chosen by the active "Run in:" tab.
function backendBodyFields(): WidgetSpec[] {
  if (!form) return [];
  switch (form.backend) {
    case "local":
      return localBodyFields();
    case "devcontainer":
      return devcontainerBodyFields();
    case "ssh":
      return sshBodyFields();
    case "kubernetes":
      return k8sBodyFields();
  }
}

// While a submit is in flight the dialog is *disabled*: the editable fields and
// the backend tabs are replaced with a read-only summary and only Cancel stays
// actionable (Create is shown disabled). It holds until the attach resolves —
// success closes the dialog, failure flips `submitting` back off and re-renders
// the editable form with the error. Applies to every backend (a fresh `openForm`
// always starts with `submitting = false`, so the next open is reset).
function buildConnectingView(): WidgetSpec {
  if (!form) return col();
  const roRow = (label: string, value: string): WidgetSpec => ({
    kind: "raw",
    entries: [
      styledRow([
        { text: `${label}: `, style: { fg: "ui.menu_disabled_fg", bold: true } },
        { text: value || "—", style: { fg: "ui.menu_disabled_fg" } },
      ]),
    ],
  });
  const rows: WidgetSpec[] = [];
  if (form.backend === "ssh") {
    rows.push(roRow(editor.t("form.ro_run_in"), editor.t("backend.ssh")));
    rows.push(roRow(editor.t("form.ro_host"), form.sshHost.value.trim()));
    if (form.sshPath.value.trim()) rows.push(roRow(editor.t("form.ro_remote_path"), form.sshPath.value.trim()));
  } else if (form.backend === "kubernetes") {
    rows.push(roRow(editor.t("form.ro_run_in"), editor.t("backend.kubernetes")));
    const ns = form.k8sNamespace.value.trim();
    const pod = form.k8sPod.value.trim();
    rows.push(roRow(editor.t("form.ro_pod"), form.k8sTarget.value.trim() || `${ns}/${pod}`));
  } else {
    rows.push(roRow(editor.t("form.ro_run_in"), form.backend === "devcontainer" ? editor.t("backend.devcontainer") : editor.t("backend.local")));
    rows.push(roRow(editor.t("form.ro_project"), form.projectPath.value.trim() || form.defaultProjectPath));
  }
  const name = form.name.value.trim();
  if (name) rows.push(roRow(editor.t("form.ro_workspace"), name));

  const remote = form.backend === "ssh" || form.backend === "kubernetes";
  return col(
    // The "ORCHESTRATOR :: New Workspace" title is native modal-frame
    // chrome now (set on the panel at mount time and kept across the
    // connecting-state re-render), so no in-body banner is drawn here.
    ...rows,
    spacer(0),
    {
      kind: "raw",
      entries: [
        styledRow([
          {
            text: remote ? editor.t("form.connecting") : editor.t("form.creating_workspace"),
            style: { fg: "ui.menu_disabled_fg", bold: true, italic: true },
          },
          {
            text: editor.t("form.press_cancel_abort"),
            style: { fg: "ui.menu_disabled_fg", italic: true },
          },
        ]),
      ],
    },
    spacer(0),
    wrappingRow(
      button(editor.t("form.btn_cancel"), { intent: "danger", key: "cancel" }),
      spacer(2),
      button(editor.t("form.btn_create"), { intent: "primary", key: "create", disabled: true }),
    ),
  );
}

// Whether the form has enough input to create: the per-backend minimum
// required field. Gates the Create buttons (rendered disabled otherwise) and
// guards the submit paths so an empty form can't be submitted via Enter.
function formIsSubmittable(): boolean {
  if (!form) return false;
  switch (form.backend) {
    case "local":
    case "devcontainer":
      return !!(form.projectPath.value.trim() || form.defaultProjectPath);
    case "ssh":
      return form.sshHost.value.trim().length > 0;
    case "kubernetes":
      return (
        form.k8sTarget.value.trim().length > 0 ||
        form.k8sPod.value.trim().length > 0
      );
  }
}

function buildFormSpec(): WidgetSpec {
  if (!form) return col();
  // Disabled/connecting state: read-only summary + Cancel-only (item 3).
  if (form.submitting) return buildConnectingView();

  const children: WidgetSpec[] = [
    // The title ("ORCHESTRATOR :: New Workspace") + border are now
    // native modal-frame chrome drawn by the host (see `openForm`), so
    // the spec starts straight at the "Run in:" session-type tabs.
    // === "Run in:" session-type tabs. ============================
    backendTabsRow(),
    spacer(0),
    // === Backend-specific top fields (swap with the tab). ========
    ...backendBodyFields(),
    // === Shared fields: Session Name + Agent Command. ============
    // Labels are plain — the input's own focused-bg styling (set by
    // the host based on the panel's focus_key) is the authoritative
    // focus cue.
    labeledSection({
      label: editor.t("form.workspace_name"),
      child: text({
        value: form.name.value,
        cursorByte: form.name.cursor,
        // Concrete default (e.g. "session-3") rather than the
        // literal `(auto-generated)` — the user sees the exact
        // name an empty submit would create. Empty while the
        // ref probe runs.
        placeholder: form.defaultSessionName || editor.t("form.auto_generating"),
        fullWidth: true,
        key: "name",
      }),
    }),
    agentPresetRow(),
    labeledSection({
      label: editor.t("form.agent_command"),
      child: text({
        value: form.cmd.value,
        cursorByte: form.cmd.cursor,
        // Clearing the field falls back to the backend default: a bare local
        // terminal (the host resolves `$SHELL`), or — for SSH — letting ssh
        // spawn the remote login shell. The placeholder names that default.
        placeholder: form.backend === "ssh"
          ? editor.t("form.cmd_placeholder_ssh")
          : editor.t("form.agent_terminal"),
        fullWidth: true,
        key: "cmd",
      }),
    }),
    // Agent-specific controls (Auto mode / Start prompt), adaptive to the
    // resolved agent. Empty for a bare terminal / unknown command.
    ...agentOptionsFields(),
  ];
  // Worktree + branch controls are local-only and live behind the
  // collapsible "Advanced…" fold.
  if (form.backend === "local") {
    children.push(...advancedSection());
  }
  // Remote backends connect asynchronously and the dialog stays open until the
  // session is real (see `runRemoteAttach`). The in-flight "connecting" state
  // is rendered by `buildConnectingView` (the early return above), so nothing
  // is needed here for it.
  if (form.lastError) {
    children.push(spacer(0));
    children.push({
      kind: "raw",
      entries: [
        styledRow([
          {
            text: editor.t("form.error_prefix"),
            style: { fg: "ui.status_error_indicator_fg", bold: true },
          },
          { text: form.lastError },
        ]),
      ],
    });
  }
  children.push(
    spacer(0),
    // === Button row. =============================================
    // wrappingRow so Cancel / Create Session reflow onto a second line
    // on a narrow form instead of "Create Session" being clipped off the
    // right edge. The wrap path ignores the leading flex spacer (and
    // trims a blank that would lead a line), so the pair left-packs.
    wrappingRow(
      button(editor.t("form.btn_create"), {
        intent: "primary",
        key: "create-visit",
        disabled: !formIsSubmittable(),
        focusable: true,
      }),
      spacer(2),
      button(editor.t("form.btn_create_bg"), {
        key: "create-bg",
        disabled: !formIsSubmittable(),
        focusable: true,
      }),
      spacer(2),
      button(editor.t("form.btn_cancel"), { intent: "danger", key: "cancel" }),
    ),
    spacer(0),
    // === Footer: keybinding helper, centered. ====================
    row(
      flexSpacer(),
      hintBar([
        { keys: "Tab", label: editor.t("hint.form_next") },
        { keys: "S-Tab", label: editor.t("hint.form_prev") },
        { keys: "←→", label: editor.t("hint.form_change") },
        { keys: "↑↓", label: editor.t("hint.form_suggest") },
        { keys: "Space", label: editor.t("hint.form_toggle") },
        { keys: "Enter", label: editor.t("hint.form_advance") },
        { keys: "^Enter", label: editor.t("hint.form_create") },
        { keys: "Esc", label: editor.t("hint.form_close") },
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
    backend: "local",
    sshHost: { value: "", cursor: 0 },
    sshPath: { value: "", cursor: 0 },
    sshIdentity: { value: "", cursor: 0 },
    sshOptions: { value: "", cursor: 0 },
    k8sTarget: { value: "", cursor: 0 },
    k8sContext: { value: "", cursor: 0 },
    k8sNamespace: { value: "", cursor: 0 },
    k8sPod: { value: "", cursor: 0 },
    k8sWorkspace: { value: "", cursor: 0 },
    projectPath: { value: "", cursor: 0 },
    name: { value: "", cursor: 0 },
    // Prefill the last-used command as *actual* editable text (not a
    // placeholder), so the field value is the single source of truth: keep it,
    // edit it, or clear it to fall back to the backend default (a bare local
    // terminal, or — for SSH — the remote login shell). No hidden
    // "empty means reuse lastCmd" fallback at submit time.
    cmd: { value: lastCmd, cursor: lastCmd.length },
    startPrompt: { value: "", cursor: 0 },
    autoMode: false,
    // Teach the agent the Fresh CLI by default: the capability token is always
    // minted, so an agent that knows the `fresh` verbs is strictly more useful
    // out of the box. Only surfaces for agents with a systemPrompt strategy.
    teachFreshCli: true,
    branch: { value: "", cursor: 0 },
    newBranch: { value: "", cursor: 0 },
    advancedExpanded: false,
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
  formPanel.mount(buildFormSpec(), {
    widthPct: 60,
    heightPct: 90,
    // Reserve the `▸ ` focus-marker gutter: focus is then legible from
    // a plain terminal capture (driveable by automation) and the
    // layout stays constant as Tab moves focus between controls.
    focusMarker: true,
    // The dialog's title + border are now native modal-frame chrome
    // (drawn by the host around the WidgetSpec) rather than the in-body
    // "ORCHESTRATOR :: New Workspace" banner, and `closable` renders a
    // native `[×]` that dismisses via the same cancel path as Esc.
    title: `${editor.t("form.header_keyword")} :: ${editor.t("form.header_label")}`,
    closable: true,
  });
  // The New-Session form is a global orchestrator feature too: center it
  // over the full screen (covering its own dimmed dock) rather than in the
  // chrome area beside the dock. A no-op when no dock is up.
  editor.floatingPanelControl(formPanel.id(), "fullscreen", 1);
  editor.setEditorMode(NEW_SESSION_MODE);
  // Mirror the host's focus cycle so Up/Down can route to the right field's
  // history. The "Run in:" type tabs sit *first* in the spec, so without an
  // explicit focus the form would open on the Local tab button — typing would
  // go nowhere. Land initial focus on the active backend's first input
  // (`project_path` for Local), preserving the original "open and type"
  // behaviour; the tabs stay reachable via Shift+Tab / ←→ / click.
  rebuildFormFocusCycle();
  const firstField = firstBodyFieldKey(form.backend);
  formPanel.setFocusKey(firstField);
  snapFormFocusTo(firstField);

  // Kick off the placeholder probes (canonical repo root,
  // default branch, next session name) against the editor's
  // cwd. Each probe is async and re-renders on completion.
  void probeProjectPathDefaults();
}

/// The local directory a brand-new *local* workspace should default to.
///
/// Almost always the active window's cwd (`editor.getCwd()`). But when the
/// active window is a remote session (ssh / kubernetes / devcontainer), its
/// cwd is a path on the *remote* filesystem — `editor.getCwd()` returns the
/// active window's root, which for those backends is the remote root. Seeding
/// the New-Session form's Local Project Path with it roots the new *local*
/// worktree at a path that need not exist on this machine (the "new workspace
/// uses the remote fs for the local one" bug). In that case fall back to a
/// local tracked session's root — preferring the launch/base session (lowest
/// window id) — so the default stays on the local filesystem.
function localProjectDefault(): string {
  const cwd = editor.getCwd();
  // A non-empty authority label means the active window runs under a remote
  // authority; corroborate with the orchestrator's own per-session remote
  // facet (set for the active SSH / k8s row). Either signal ⇒ cwd is remote.
  const active = orchestratorSessions.get(editor.activeWindow());
  const activeIsRemote = editor.getAuthorityLabel().length > 0 || !!active?.remote;
  if (!activeIsRemote) return cwd;
  // Active window is remote: pick the lowest-id local (non-remote, non-
  // discovered) session's root — in practice the launch/base window. If there
  // is no local session at all, leave cwd as the last-resort value rather than
  // an empty default.
  let base: AgentSession | null = null;
  for (const s of orchestratorSessions.values()) {
    if (s.remote || s.discovered) continue;
    if (base === null || s.id < base.id) base = s;
  }
  return base ? base.root : cwd;
}

/// Resolve placeholders for the Project Path / Session Name /
/// Branch fields based on the *currently-effective* project
/// path: the user-typed value if any, else a local default
/// (the canonical-root probe runs against the latter). Re-runs
/// on every Project Path keystroke (debounced via the caller).
async function probeProjectPathDefaults(): Promise<void> {
  if (!form) return;
  const token = ++form.probeToken;
  const typedPath = form.projectPath.value.trim();

  // (1) Default Project Path: only meaningful when the user
  //     hasn't typed anything. Resolve a local default → canonical
  //     root, fall back to the local default verbatim for non-git
  //     launches. `localProjectDefault()` keeps this on the local
  //     filesystem even when the active window is a remote session.
  if (!typedPath) {
    const localDefault = localProjectDefault();
    const resolved = await resolveCanonicalRepoRoot(localDefault);
    if (!form || form.probeToken !== token) return;
    form.defaultProjectPath = resolved || localDefault;
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
    // Empty field: list the local default's siblings, not the active
    // (possibly remote) window's cwd — the Project Path is always local.
    parent = typed ? "." : localProjectDefault();
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
  // Cancelling while a remote connect is in flight: tell the host to abort it.
  // The pending `attachRemoteAgent` promise rejects with "cancelled" (its catch
  // is a no-op once `form` is null below) and the connect's late result is
  // discarded host-side, so no window is ever built.
  if (form?.submitting) {
    pendingRemoteFacet = null;
    editor.cancelRemoteAgent();
  }
  closeForm();
  if (restoreDockAfterForm()) return;
  if (wasFromPicker) {
    openControlRoom();
  }
}

// =============================================================================
// Non-blocking workspace creation
//
// Submitting the New-Workspace form no longer blocks the editor behind a
// modal "Creating…/Connecting…" view. Instead the form's inputs are captured
// into a self-contained `CreateSpec`, the form closes, and a synthetic
// placeholder row appears in the dock — "Creating…" for a local worktree,
// "Connecting…" for a remote host that may not even be reachable yet. The
// real work runs in the background; the user is free to switch to any other
// workspace meanwhile. On success the placeholder is dropped and the real
// live window takes its place; on failure the row flips to an error state
// offering retry / dismiss.
//
// Remote attaches are *serialised*: the host's `cancelRemoteAgent()` cancels
// EVERY in-flight connect, so two concurrent background attaches could not be
// cancelled independently. Remote placeholders therefore run one at a time
// through `remoteCreateQueue` / `remoteInFlightId`. Local creates have no such
// constraint and run immediately, in parallel.
//
// Still-creating specs are persisted (`savePendingSpecs`) so a quit + restart
// re-surfaces them as resumable ("paused") rows via `recoverPendingWorkspaces`
// on the `ready` hook.
// =============================================================================

type CaptureResult = { ok: true; spec: CreateSpec } | { ok: false; error: string };

// Resolve the (about-to-close) form into a `CreateSpec`, or return the
// validation error that keeps the form open. Reads field values only — no
// async work, no side effects — so the background worker never touches form
// state that no longer exists.
function captureCreateSpec(f: NewSessionForm): CaptureResult {
  const cmd = f.cmd.value.trim();
  const sessionName = f.name.value.trim();

  if (f.backend === "local") {
    // Project Path: typed value wins; otherwise the resolved canonical-root
    // placeholder (or, if that probe never completed, a local default).
    // `localProjectDefault()` keeps the fallback on the local filesystem even
    // when the active window is a remote session.
    const projectPath = f.projectPath.value.trim() ||
      f.defaultProjectPath ||
      localProjectDefault();
    const displayLabel = sessionName ||
      f.defaultSessionName ||
      editor.pathBasename(projectPath) ||
      editor.t("dock.pending_default_name");
    return {
      ok: true,
      spec: {
        backend: "local",
        projectPath,
        name: sessionName,
        cmd,
        // Only carry agent options for a command that resolves to an agent
        // that supports them, so a bare terminal / custom command never gets
        // stray flags or a prompt appended.
        auto: !!agentEntryForCmd(cmd)?.auto && f.autoMode,
        startPrompt: agentEntryForCmd(cmd)?.prompt ? f.startPrompt.value.trim() : "",
        teachFreshCli: !!agentEntryForCmd(cmd)?.systemPrompt && f.teachFreshCli,
        branch: f.branch.value.trim(),
        newBranch: f.newBranch.value.trim(),
        createWorktree: f.createWorktree,
        displayLabel,
        displayProject: projectPath,
      },
    };
  }

  if (f.backend === "kubernetes") {
    const target = f.k8sTarget.value.trim();
    const namespace = f.k8sNamespace.value.trim();
    const pod = f.k8sPod.value.trim();
    if (target && !pod) return { ok: false, error: editor.t("err.k8s_named_target") };
    if (!namespace || !pod) return { ok: false, error: editor.t("err.k8s_ns_pod_required") };
    const agentArgv = splitAgentCmd(cmd);
    const detail = `${namespace}/${pod}`;
    const label = sessionName || `k8s:${namespace}/${pod}`;
    const spec: RemoteAgentSpec = {
      transport: {
        kind: "kubectl-exec",
        context: f.k8sContext.value.trim() || null,
        namespace,
        pod,
        container: null,
        workspace: f.k8sWorkspace.value.trim() || null,
      },
      base_env: [],
      // Born-attached: a new window beside the local ones (not a global
      // restart). Core records nothing about us — the orchestrator tracks the
      // session via the `window_created` hook once the connect succeeds.
      window: true,
      label,
      command: agentArgv.length > 0 ? agentArgv : undefined,
    };
    return {
      ok: true,
      spec: {
        backend: "kubernetes",
        spec,
        facet: { kind: "kubernetes", detail, state: "starting" },
        displayLabel: label,
        displayProject: detail,
        persistCmd: "",
      },
    };
  }

  if (f.backend === "ssh") {
    // Parse `[user@]host[:port]` (also tolerates a pasted `ssh://…`). The user
    // is optional — a bare `host` lets ssh resolve it from its own config.
    const raw = f.sshHost.value.trim().replace(/^ssh:\/\//, "");
    const portMatch = raw.match(/^(.+):(\d+)$/);
    const port = portMatch ? parseInt(portMatch[2], 10) : null;
    const hostPart = portMatch ? portMatch[1] : raw;
    const at = hostPart.indexOf("@");
    const user = at > 0 ? hostPart.slice(0, at) : undefined;
    const host = at >= 0 ? hostPart.slice(at + 1) : hostPart;
    if (!host) return { ok: false, error: editor.t("err.ssh_host_required") };
    const identity = f.sshIdentity.value.trim();
    const remotePath = f.sshPath.value.trim();
    const extraArgs = f.sshOptions.value.trim()
      ? f.sshOptions.value.trim().split(/\s+/)
      : [];
    const agentArgv = splitAgentCmd(cmd);
    const target = user ? `${user}@${host}` : host;
    const spec: RemoteAgentSpec = {
      transport: {
        kind: "ssh",
        ...(user ? { user } : {}),
        host,
        port,
        identity_file: identity || null,
        remote_path: remotePath || null,
        ...(extraArgs.length > 0 ? { extra_args: extraArgs } : {}),
      },
      base_env: [],
      window: true,
      label: sessionName || `ssh:${target}`,
      command: agentArgv.length > 0 ? agentArgv : undefined,
    };
    return {
      ok: true,
      spec: {
        backend: "ssh",
        spec,
        facet: { kind: "ssh", detail: target, state: "starting" },
        displayLabel: sessionName || `ssh:${target}`,
        displayProject: target,
        persistCmd: cmd,
      },
    };
  }

  // devcontainer — no runtime plugin-to-plugin attach yet.
  return { ok: false, error: editor.t("err.devcontainer_unsupported") };
}

// `attachRemoteAgent` rejects with an Error whose message is the host's
// reason (ssh diagnostic, a window-creation failure, a bad spec, …).
function remoteAttachErrorText(e: unknown): string {
  const msg = e instanceof Error ? e.message : String(e ?? "");
  return msg.trim() || editor.t("err.connection_failed");
}

// The "Creating…" / "Connecting…" line shown on a freshly-launched row.
function pendingCreatingMessage(spec: CreateSpec): string {
  return spec.backend === "local"
    ? editor.t("dock.pending_creating")
    : editor.t("dock.pending_connecting");
}

// Insert a placeholder row for a workspace to create and — unless it is a
// restored (paused) row — close the form, surface the dock, and launch the
// background worker. Returns the placeholder's synthetic id.
function startPendingWorkspace(
  spec: CreateSpec,
  opts?: { restored?: boolean; visit?: boolean; label?: string },
): number {
  const id = allocPendingId();
  const restored = opts?.restored === true;
  const remoteFacet: RemoteFacet | undefined =
    spec.backend === "local" ? undefined : { ...spec.facet };
  orchestratorSessions.set(id, {
    id,
    // A restored row keeps the name it last showed (the resolved / relabelled
    // one persisted alongside the spec); a fresh row starts from the spec's
    // capture-time display label.
    label: opts?.label || spec.displayLabel,
    // Synthetic root — a placeholder owns no real directory yet, and a unique
    // key keeps it in its own stable dock-order slot.
    root: `pending:${id}`,
    projectPath: spec.displayProject,
    sharedWorktree: false,
    terminalId: null,
    state: "idle",
    lastOutputAt: null,
    createdAt: Date.now(),
    remote: remoteFacet,
    pending: {
      phase: restored ? "paused" : "creating",
      message: restored ? editor.t("dock.pending_interrupted") : pendingCreatingMessage(spec),
      spec,
      // A restored/resumed row never yanks focus on relaunch.
      visit: !restored && opts?.visit === true,
    },
  });
  savePendingSpecs();
  if (!restored) {
    closeForm();
    showDockForPending();
    launchPendingCreate(id);
  }
  return id;
}

// Ensure the dock is visible so a just-created placeholder is seen, without
// stealing keyboard focus — the user asked to keep working ("stay put").
function showDockForPending(): void {
  // Open the dock if nothing is up yet. (Structured so the module-level
  // `openPanel`'s narrowing survives the `openControlRoom` reassignment —
  // mirrors `openMoveToFolderForCurrent`.)
  if (!openPanel) {
    openControlRoom({ dock: true });
  }
  if (!openPanel) return;
  // Keep keyboard focus in the editor ("stay put") — only the dock is shown.
  // A centered modal picker (dockMode false) is left focused as-is.
  if (dockMode) {
    dockBlurred = true;
    editor.floatingPanelControl(openPanel.id(), "blur", 0);
    syncDockSelectionToActive();
  }
  refreshOpenDialog();
}

// Dispatch the background worker for a placeholder (local runs immediately;
// remote queues behind any in-flight connect).
function launchPendingCreate(id: number): void {
  const s = orchestratorSessions.get(id);
  if (!s || !s.pending) return;
  if (s.pending.spec.backend === "local") void runLocalCreate(id);
  else enqueueRemoteCreate(id);
}

// Update a placeholder's status line (a no-op once the row is gone).
function setPendingMessage(id: number, msg: string): void {
  const s = orchestratorSessions.get(id);
  if (!s || !s.pending) return;
  s.pending.message = msg;
  if (openPanel) refreshOpenDialog();
}

// Keep the placeholder's label in step with the resolved session name (local
// auto-generates it only inside the worker).
function relabelPending(id: number, label: string): void {
  const s = orchestratorSessions.get(id);
  if (!s || !s.pending || !label || s.label === label) return;
  s.label = label;
  savePendingSpecs();
  if (openPanel) refreshOpenDialog();
}

// Flip a placeholder into its error state with `reason`, surfacing it on the
// row (and the status bar) and dropping it from the persisted set.
function failPending(id: number, reason: string): void {
  const s = orchestratorSessions.get(id);
  if (!s || !s.pending) return;
  s.pending.phase = "error";
  s.pending.message = reason.trim() || editor.t("err.connection_failed");
  if (s.remote) s.remote.state = "error";
  editor.setStatus(editor.t("status.prefix", { msg: s.pending.message }));
  savePendingSpecs();
  if (openPanel) refreshOpenDialog();
}

// Return keyboard focus to the window that was active before a spawn/attach
// dove into its new window — the "stay put, mark ready" contract. Safe if the
// target window has since closed.
function restoreActiveWindow(id: number): void {
  if (id <= 0 || editor.activeWindow() === id) return;
  if (!editor.listWindows().some((w) => w.id === id)) return;
  editor.setActiveWindow(id);
}

// Re-run a failed or paused (restored) placeholder from scratch.
function retryPending(id: number): void {
  const s = orchestratorSessions.get(id);
  if (!s || !s.pending) return;
  s.pending.phase = "creating";
  s.pending.message = pendingCreatingMessage(s.pending.spec);
  if (s.remote) s.remote.state = "starting";
  savePendingSpecs();
  if (openPanel) refreshOpenDialog();
  launchPendingCreate(id);
}

// Drop a placeholder for good. If it is the remote connect currently in
// flight, tear that connect down first (the host's `cancelRemoteAgent`
// cancels the single in-flight attach); a queued one is just removed.
function dismissPending(id: number): void {
  const s = orchestratorSessions.get(id);
  if (!s || !s.pending) return;
  if (remoteInFlightId === id) {
    pendingRemoteFacet = null;
    editor.cancelRemoteAgent();
    remoteInFlightId = null;
  }
  const qi = remoteCreateQueue.indexOf(id);
  if (qi >= 0) remoteCreateQueue.splice(qi, 1);
  orchestratorSessions.delete(id);
  savePendingSpecs();
  if (openPanel) {
    refreshOpenDialog();
    syncDockSelectionToActive();
  }
}

// Best-effort teardown of a worktree this local create added but then had to
// abandon (the placeholder was dismissed mid-flight). Leaving it would orphan
// the branch + directory with nothing tracking it. `--force` since the tree is
// freshly created; any failure is swallowed — there's nothing else to do from
// a cancellation path.
async function discardCreatedWorktree(repoRoot: string, root: string): Promise<void> {
  await spawnCollect(
    "git",
    ["-C", repoRoot, "worktree", "remove", "--force", root],
    repoRoot,
  );
}

// Background worker: create a local worktree (when requested) and spawn the
// session window, swapping the placeholder for the real row. Idempotent on a
// resume — if the target worktree already exists (a prior run made it before
// the editor quit) the add is skipped and the session opens in place.
//
// Cancellation: `dismissPending` deletes the session from the map, so each
// `orchestratorSessions.get(id)?.pending` check below is also a cancel point.
// Because a local create has real side effects (a worktree on disk, a spawned
// window), the checkpoints after those effects undo them rather than just
// bailing — so a dismissed create leaves nothing behind.
async function runLocalCreate(id: number): Promise<void> {
  const s0 = orchestratorSessions.get(id);
  if (!s0 || !s0.pending || s0.pending.spec.backend !== "local") return;
  const spec = s0.pending.spec;
  const cmd = spec.cmd;
  const checkoutBranch = spec.branch;
  const newBranch = spec.newBranch;
  const projectPath = spec.projectPath;

  // Re-probe is-git so we trust the latest filesystem state, not a UI flag.
  const isGit = await pathIsInsideGitWorkTree(projectPath);
  if (!orchestratorSessions.get(id)?.pending) return; // dismissed mid-probe
  const createWorktree = isGit === true && spec.createWorktree;

  let repoRoot = projectPath;
  if (createWorktree) {
    const canonical = await resolveCanonicalRepoRoot(projectPath);
    if (canonical) repoRoot = canonical;
  }

  const sessionName = spec.name ||
    (await nextAutoSessionName(repoRoot, { persist: true }));
  if (!orchestratorSessions.get(id)?.pending) return;
  // Pin the auto-resolved name back into the spec so a retry or a
  // restart-recovery targets the *same* worktree instead of allocating a fresh
  // `<proj>-(N+1)` — which would bypass the `rootExists` idempotency below and
  // orphan the worktree a prior attempt already created.
  if (!spec.name) {
    spec.name = sessionName;
    savePendingSpecs();
  }
  relabelPending(id, sessionName);

  const root = createWorktree
    ? editor.pathJoin(editor.getDataDir(), "orchestrator", slugify(repoRoot), sessionName)
    : projectPath;

  // Recovery idempotency: a worktree left on disk by an interrupted run is
  // reused rather than re-added (a re-add would fail and error the row).
  const rootExists = createWorktree && editor.fileExists(root);
  // Whether *this* run put the worktree on disk (vs. reusing an existing one),
  // so a mid-flight dismissal can remove exactly what it created.
  let addedWorktree = false;
  if (createWorktree && !rootExists) {
    setPendingMessage(id, editor.t("dock.pending_adding_worktree"));
    const parent = editor.pathDirname(root);
    if (!editor.createDir(parent)) {
      failPending(id, editor.t("err.mkdir_failed", { path: parent }));
      return;
    }
    const defaultBranch = await detectDefaultBranch(repoRoot);
    // Fork point for the new worktree: the explicit checkout branch, or the
    // detected default when the user left it blank.
    const base = checkoutBranch || defaultBranch;
    if (newBranch) {
      // "New branch name" set: cut a fresh branch off `base`. A pre-existing
      // branch of that name is a hard error — NO silent fallback to checking
      // it out (that would put the user on someone else's history).
      const addRes = await spawnCollect(
        "git",
        ["-C", repoRoot, "worktree", "add", root, "-b", newBranch, base],
        repoRoot,
      );
      if (addRes.exit_code !== 0) {
        if (/already exists/i.test(addRes.stderr || "")) {
          failPending(id, editor.t("err.branch_exists", { branch: newBranch }));
        } else {
          failPending(
            id,
            lastNonEmptyLine(addRes.stderr) || editor.t("err.worktree_add_failed"),
          );
        }
        return;
      }
    } else if (checkoutBranch) {
      // "Checkout branch" set (no new branch): check that existing branch/ref
      // out into the new worktree.
      const addRes = await spawnCollect(
        "git",
        ["-C", repoRoot, "worktree", "add", root, checkoutBranch],
        repoRoot,
      );
      if (addRes.exit_code !== 0) {
        failPending(
          id,
          lastNonEmptyLine(addRes.stderr) || editor.t("err.worktree_add_failed"),
        );
        return;
      }
    } else {
      // Neither field set — today's behaviour: cut `<sessionName>` off the
      // default branch, falling back to checking out an existing branch of
      // that name (only for this default case).
      let addRes = await spawnCollect(
        "git",
        ["-C", repoRoot, "worktree", "add", root, "-b", sessionName, defaultBranch],
        repoRoot,
      );
      if (addRes.exit_code !== 0) {
        const fallback = await spawnCollect(
          "git",
          ["-C", repoRoot, "worktree", "add", root, sessionName],
          repoRoot,
        );
        if (fallback.exit_code !== 0) {
          failPending(
            id,
            lastNonEmptyLine(fallback.stderr) ||
              lastNonEmptyLine(addRes.stderr) ||
              editor.t("err.worktree_add_failed"),
          );
          return;
        }
        addRes = fallback;
      }
    }
    addedWorktree = true;
  }

  // Non-worktree (in-place) checkout: switch the project's own working tree
  // to `checkoutBranch`. Refuse unless the tree is clean AND fully pushed —
  // a checkout here mutates the user's real repo, so we never risk stranding
  // uncommitted or unpushed work (and never use `-f`). `newBranch` is ignored
  // in this path.
  if (!createWorktree && checkoutBranch) {
    setPendingMessage(id, editor.t("dock.pending_adding_worktree"));
    const statusRes = await spawnCollect(
      "git",
      ["-C", projectPath, "status", "--porcelain"],
      projectPath,
    );
    if ((statusRes.stdout || "").trim().length > 0) {
      failPending(id, editor.t("err.checkout_unclean"));
      return;
    }
    const upstreamRes = await spawnCollect(
      "git",
      ["-C", projectPath, "rev-parse", "--abbrev-ref", "--symbolic-full-name", "@{u}"],
      projectPath,
    );
    if (upstreamRes.exit_code !== 0) {
      failPending(id, editor.t("err.checkout_no_upstream"));
      return;
    }
    const unpushedRes = await spawnCollect(
      "git",
      ["-C", projectPath, "rev-list", "--count", "@{u}..HEAD"],
      projectPath,
    );
    if (parseInt((unpushedRes.stdout || "0").trim(), 10) > 0) {
      failPending(id, editor.t("err.checkout_unpushed"));
      return;
    }
    const coRes = await spawnCollect(
      "git",
      ["-C", projectPath, "checkout", checkoutBranch],
      projectPath,
    );
    if (coRes.exit_code !== 0) {
      failPending(
        id,
        lastNonEmptyLine(coRes.stderr) || editor.t("err.worktree_add_failed"),
      );
      return;
    }
  }
  if (!orchestratorSessions.get(id)?.pending) {
    // Dismissed while the worktree was being added — remove what we just
    // created so a dismissed create leaves nothing orphaned on disk.
    if (addedWorktree) await discardCreatedWorktree(repoRoot, root);
    return;
  }

  if (cmd) editor.setGlobalState("orchestrator.last_cmd", cmd);

  // Attach-to-existing-worktree classification for the no-worktree path
  // (a linked worktree the user pointed at directly).
  const attachInfo = !createWorktree ? await classifyWorktree(root) : null;
  const isLinkedAttach = attachInfo?.isLinked === true;
  const effectiveProjectPath = isLinkedAttach ? attachInfo!.mainRoot : projectPath;
  const reportedBranch = createWorktree
    ? (newBranch || checkoutBranch || sessionName)
    : (checkoutBranch || (isLinkedAttach ? attachInfo!.branch : ""));

  appendHistory("project_path", projectPath);
  appendHistory("name", sessionName);
  if (cmd) appendHistory("cmd", cmd);
  if (createWorktree) appendHistory("branch", reportedBranch);

  // "Teach Fresh CLI": inject the CLI system prompt and (below) mint a
  // capability token. `via: "file"` writes an AGENTS.md the agent reads at
  // startup; `via: "flag"` rides the launch argv (resolved just below).
  const teachEntry = spec.teachFreshCli ? agentEntryForCmd(cmd) : null;
  const teach = teachEntry?.systemPrompt ?? null;
  if (teach?.via === "file") {
    writeFreshCliPromptFile(editor.pathJoin(root, teach.path));
  }

  const argv = splitAgentCmd(cmd);
  const { launch: launchArgv, resume: resumeArgv } = resolveAgentLaunch(argv, {
    auto: spec.auto,
    prompt: spec.startPrompt,
    systemPrompt: teach?.via === "flag" ? FRESH_CLI_SYSTEM_PROMPT : undefined,
  });
  const sharedWorktree = !createWorktree && !isLinkedAttach;

  // Capture the user's current window so focus can return to it after
  // `createWindowWithTerminal` dives into the new one — unless the user chose
  // "Create & Visit", in which case focus stays in the new workspace.
  const visit = orchestratorSessions.get(id)?.pending?.visit ?? false;
  const restoreTo = editor.activeWindow();
  setPendingMessage(id, editor.t("dock.pending_starting"));
  try {
    const result = await editor.createWindowWithTerminal({
      root,
      label: sessionName,
      cwd: root,
      command: launchArgv.length > 0 ? launchArgv : undefined,
      title: launchArgv.length > 0 ? launchArgv[0] : undefined,
      resume: resumeArgv,
      // Always mint the capability token bound to this window + allowlist so
      // `fresh --cmd cmd ...` from inside the workspace is authorised — whether
      // or not the agent was taught about it. `teach` only gates the prompt.
      commandAllowlist: FRESH_CLI_DEFAULT_ALLOWLIST,
    });
    const winId = result.windowId;
    // Dismissed during the (awaited) spawn: the window was born and dove in,
    // but the user has since dismissed this workspace. Tear the window (and its
    // agent process) down and remove any worktree we added, rather than
    // resurrecting the dismissed row as a live session.
    if (!orchestratorSessions.get(id)?.pending) {
      // `close_window` refuses the active window (the spawn dove into it), so
      // move focus off it first — back where the user was, or any other window.
      restoreActiveWindow(restoreTo);
      if (editor.activeWindow() === winId) {
        const other = editor.listWindows().find((w) => w.id !== winId);
        if (other) editor.setActiveWindow(other.id);
      }
      if (result.terminalId) editor.signalWindow(winId, "SIGKILL");
      editor.closeWindow(winId);
      if (addedWorktree) await discardCreatedWorktree(repoRoot, root);
      return;
    }
    editor.setWindowState("project_path", effectiveProjectPath);
    editor.setWindowState("shared_worktree", sharedWorktree);
    const discId = discoveredIdByPath.get(root);
    if (discId !== undefined) {
      orchestratorSessions.delete(discId);
      discoveredIdByPath.delete(root);
    }
    // The real window supersedes the placeholder.
    orchestratorSessions.delete(id);
    savePendingSpecs();
    orchestratorSessions.set(winId, {
      id: winId,
      label: sessionName,
      root,
      projectPath: effectiveProjectPath,
      sharedWorktree,
      terminalId: result.terminalId,
      state: "running",
      createdAt: Date.now(),
      branch: reportedBranch || undefined,
    });
    if (visit) {
      // Create & Visit: `createWindowWithTerminal` already dove into the new
      // window; hand it the keyboard (blur the dock) so the user lands in it.
      if (openPanel && dockMode) {
        dockBlurred = true;
        editor.floatingPanelControl(openPanel.id(), "blur", 0);
        editor.setEditorMode(null);
      }
    } else {
      // Stay put: undo the dive `createWindowWithTerminal` performed.
      restoreActiveWindow(restoreTo);
    }
    if (openPanel) {
      refreshOpenDialog();
      syncDockSelectionToActive();
    }
  } catch (e) {
    failPending(id, e instanceof Error ? e.message : String(e));
  }
}

// Queue a remote placeholder and pump the (serialised) remote worker.
function enqueueRemoteCreate(id: number): void {
  const s = orchestratorSessions.get(id);
  if (s?.pending && remoteInFlightId !== null) {
    // Something is already connecting — show that this one is waiting.
    setPendingMessage(id, editor.t("dock.pending_queued"));
  }
  remoteCreateQueue.push(id);
  pumpRemoteQueue();
}

// Start the next queued remote connect when none is in flight, skipping
// placeholders that were dismissed or errored while waiting.
function pumpRemoteQueue(): void {
  if (remoteAttachBusy) return;
  let next: number | undefined;
  for (;;) {
    next = remoteCreateQueue.shift();
    if (next === undefined) return;
    const s = orchestratorSessions.get(next);
    if (s?.pending && s.pending.phase !== "error") break;
  }
  remoteAttachBusy = true;
  remoteInFlightId = next;
  void runRemoteCreate(next).finally(() => {
    remoteAttachBusy = false;
    remoteInFlightId = null;
    pumpRemoteQueue();
  });
}

// Background worker for a remote (SSH / Kubernetes) placeholder. The connect
// may take seconds and can fail (unreachable host, bad pod); until it
// resolves the row sits in its "Connecting…" state, switchable-away-from like
// any other. `attachRemoteAgent` resolves only once the authority AND the
// born-attached window exist, so the placeholder is dropped only when the
// session is truly real.
async function runRemoteCreate(id: number): Promise<void> {
  const s = orchestratorSessions.get(id);
  if (
    !s || !s.pending ||
    (s.pending.spec.backend !== "ssh" && s.pending.spec.backend !== "kubernetes")
  ) {
    return;
  }
  const spec = s.pending.spec;
  s.pending.phase = "creating";
  s.pending.message = editor.t("dock.pending_connecting");
  if (s.remote) s.remote.state = "starting";
  const visit = s.pending.visit;
  // The born window adopts this facet via the `window_created` hook.
  pendingRemoteFacet = { ...spec.facet };
  if (openPanel) refreshOpenDialog();
  try {
    // Capture the user's *current* window right before the attach dives into
    // the born one, so a "stay put" create returns focus to where they are now.
    // The connect can run — or wait queued behind another attach — for seconds
    // while the user navigates elsewhere, so the submit-time window is stale;
    // mirror the local worker, which recaptures fresh here too.
    const restoreTo = editor.activeWindow();
    await editor.attachRemoteAgent(spec.spec);
    // Success: the born-attached window is live and already tracked (the
    // hook adopted the facet). Drop the placeholder.
    orchestratorSessions.delete(id);
    savePendingSpecs();
    if (spec.persistCmd) editor.setGlobalState("orchestrator.last_cmd", spec.persistCmd);
    if (visit) {
      // Create & Visit: the attach already made the born window active; hand
      // it the keyboard so the user lands in the connected session.
      if (openPanel && dockMode) {
        dockBlurred = true;
        editor.floatingPanelControl(openPanel.id(), "blur", 0);
        editor.setEditorMode(null);
      }
    } else {
      // Stay put: the attach activated the born window — return to where the
      // user was.
      restoreActiveWindow(restoreTo);
    }
    if (openPanel) {
      refreshOpenDialog();
      syncDockSelectionToActive();
    }
  } catch (e) {
    pendingRemoteFacet = null;
    failPending(id, remoteAttachErrorText(e));
  }
}

// ---------------------------------------------------------------------
// Restart recovery — persist still-creating LOCAL placeholders so a quit +
// relaunch re-surfaces them (paused) instead of silently losing an
// in-progress workspace whose worktree may not have been created yet.
//
// Remote (SSH / Kubernetes) placeholders are deliberately NOT persisted
// here: a born-attached remote session that actually connected is already
// restored across restarts by the host's own dormant-session persistence
// (it comes back as a reconnectable remote row), and re-surfacing a connect
// that never completed would either duplicate that row or silently re-open a
// network connection on launch. Local worktree creation has no such host-side
// record, so it is the case this recovery exists for.
// ---------------------------------------------------------------------

const PENDING_KEY = "orchestrator.pending";

// Persist every LOCAL placeholder that is still creating or paused (never the
// errored ones — those are a completed, surfaced outcome; nor remote ones —
// see the section note). Called on every mutation of the pending set.
function savePendingSpecs(): void {
  const out: { spec: CreateSpec; label: string }[] = [];
  for (const s of orchestratorSessions.values()) {
    if (s.pending && s.pending.phase !== "error" && s.pending.spec.backend === "local") {
      out.push({ spec: s.pending.spec, label: s.label });
    }
  }
  editor.setGlobalState(PENDING_KEY, out as unknown as object);
}

// Rehydrate persisted local placeholders on startup as paused rows the user
// resumes (Enter) or dismisses. Nothing auto-runs — resuming is a deliberate
// keystroke, never an automatic filesystem touch on launch.
function recoverPendingWorkspaces(): void {
  const raw = editor.getGlobalState(PENDING_KEY);
  if (!Array.isArray(raw) || raw.length === 0) return;
  let restored = 0;
  for (const e of raw) {
    if (!e || typeof e !== "object") continue;
    const spec = (e as Record<string, unknown>).spec as CreateSpec | undefined;
    if (!spec || spec.backend !== "local") continue;
    // Restore the name the row last showed (persisted by `savePendingSpecs`),
    // not the generic capture-time default it would otherwise re-derive.
    const savedLabel = (e as Record<string, unknown>).label;
    const label = typeof savedLabel === "string" && savedLabel ? savedLabel : undefined;
    startPendingWorkspace(spec, { restored: true, label });
    restored++;
  }
  if (restored > 0) showDockForPending();
}

// `visit`: "Create & Visit" — focus follows into the workspace once it's real.
// `!visit`: "Create in Background" — stay put. Both are non-blocking.
async function submitForm(visit: boolean): Promise<void> {
  if (!form) return;
  // Resolve the form's inputs into a self-contained spec. A validation
  // failure (bad ssh host, missing pod) keeps the form open with the error;
  // otherwise the form closes and the create runs in the background.
  const captured = captureCreateSpec(form);
  if (!captured.ok) {
    form.lastError = captured.error;
    editor.setStatus(editor.t("status.prefix", { msg: captured.error }));
    renderForm();
    return;
  }
  startPendingWorkspace(captured.spec, { visit });
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
  /**
   * Whether to hand keyboard focus to the new window (blur the dock) once
   * attached. `true` for the "dive in" gestures (Enter, Visit) — mirrors
   * the dock's live-session Enter, which blurs to the editor. `false`
   * (default) for the "activate / live-switch" gestures (arrow-nav, a
   * row click), which open the worktree as the active session but keep
   * the dock focused so you can keep navigating — mirrors the dock's
   * live-session arrow/click switch, which never blurs. No-op in the
   * modal picker (it closes `openPanel` before calling here).
   */
  dive?: boolean;
}): Promise<void> {
  try {
    const result = await editor.createWindowWithTerminal({
      root: opts.root,
      label: opts.label,
      cwd: opts.root,
      // Always mint the workspace's capability token (see runLocalCreate).
      commandAllowlist: FRESH_CLI_DEFAULT_ALLOWLIST,
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
    // The new window is now the active session. For a "dive in" gesture
    // (Enter / Visit) the dock is still focused, so its keys would be
    // swallowed and the new session's terminal couldn't receive input —
    // mirror the dock's live-session Enter path and blur it so the new
    // window gets the keyboard. The "activate / live-switch" gestures
    // (arrow-nav, row click) deliberately keep the dock focused, exactly
    // like switching to a live session does.
    if (opts.dive && dockMode && openPanel) {
      dockBlurred = true;
      editor.floatingPanelControl(openPanel.id(), "blur", 0);
      editor.setEditorMode(null);
    } else if (dockMode && openPanel) {
      // Live-switch: keep the dock focused, but rebuild the list (the
      // `· on-disk` row's synthetic id is gone, replaced by the new live
      // window's) and move the highlight onto the now-active session so
      // the user stays put on the row they just opened.
      refreshOpenDialog();
      syncDockSelectionToActive();
    }
  } catch (e) {
    editor.setStatus(
      editor.t("status.attach_failed", {
        error: e instanceof Error ? e.message : String(e),
      }),
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
  // Ctrl+Enter submits from anywhere in the form, regardless of which
  // field is focused or whether a completion popup is open.
  ["C-Enter", "orchestrator_form_submit"],
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

// The "New Folder" dialog only needs Enter to submit from anywhere —
// everything else (typing, Backspace, Tab focus-cycle, Space on the
// toggle/buttons, Esc to cancel) uses the floating panel's default
// smart-key routing. Binding Enter here makes the host defer to us
// (mode bindings win over the generic "Enter = focus-advance") so the
// name field's Enter submits instead of just advancing focus.
const FOLDER_DIALOG_MODE_BINDINGS: [string, string][] = [
  ["Enter", "orchestrator_folder_submit"],
  ["C-Enter", "orchestrator_folder_submit"],
];
editor.defineMode(CREATE_FOLDER_MODE, FOLDER_DIALOG_MODE_BINDINGS, true, true);
registerHandler("orchestrator_folder_submit", () => {
  if (!createFolderDialog) return;
  // Enter submits from anywhere in the dialog — except on the Cancel
  // button, where it must cancel. Without this check, Tab-ing onto
  // [ Cancel ] and pressing Enter *created* the folder.
  if (createFolderFocusKey === "folder-cancel") {
    closeCreateFolderDialog();
    return;
  }
  submitCreateFolder();
});

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
  // Tab applies the highlighted candidate when the user has stepped
  // into an open dropdown (↑/↓/wheel); otherwise it advances to the
  // next field. The host owns that decision (it tracks which row, if
  // any, is highlighted), so when a popup is open we must NOT
  // optimistically advance our mirror — doing so would desync it in
  // the accept case, where the host keeps focus on the field and
  // fires `completion_accept` (no `focus` event to snap back from).
  // With no popup, the host always advances and fires an authoritative
  // `focus` event, so the optimistic advance just avoids a frame lag.
  if (!completionVisibleForFocused()) {
    advanceFormFocus(1);
  }
  dispatchFormKey("Tab");
});
// Ctrl+Enter: submit from anywhere, no matter which field is focused or
// whether a completion popup is open. Runs the primary action, "Create &
// Visit" (the "In Background" alternative is an explicit button / Enter on it).
registerHandler("orchestrator_form_submit", () => {
  if (!form) return;
  // Same gating as the Create buttons: no required input ⇒ no submit.
  if (!formIsSubmittable()) return;
  void submitForm(true);
});
registerHandler("orchestrator_form_key_enter", () => {
  if (!form || !formPanel) return;
  // Popup open: keep the existing behaviour — the host's smart-key
  // dismisses the completion popup and fires `completion_dismiss` (the
  // plugin syncs local state via that event), staying on the text input.
  if (completionVisibleForFocused()) {
    dispatchFormKey("Enter");
    return;
  }
  // Popup closed: Enter must NOT advance focus (Tab / Shift-Tab are the only
  // field movers). Activate the focused control instead — `activate()` fires
  // a Button's "activate" event (Create / Cancel / Advanced / the type tabs)
  // or a Toggle's "toggle", and is a no-op on text inputs and the dropdown.
  formPanel.command(activate());
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
// When a "Run in:" type tab is focused, ←/→ moves between tabs (switching the
// backend) rather than a text cursor. Returns true if it consumed the key.
function switchTabIfFocused(delta: 1 | -1): boolean {
  if (!form) return false;
  const idx = SESSION_BACKENDS.findIndex((b) => b.key === formFocusedKey());
  if (idx < 0) return false;
  const next = (idx + delta + SESSION_BACKENDS.length) % SESSION_BACKENDS.length;
  selectBackend(SESSION_BACKENDS[next].id);
  return true;
}
registerHandler("orchestrator_form_key_left", () => {
  if (switchTabIfFocused(-1)) return;
  dispatchFormKey("Left");
});
registerHandler("orchestrator_form_key_right", () => {
  if (switchTabIfFocused(1)) return;
  dispatchFormKey("Right");
});
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
    setDialogError(editor.t("err.no_eligible_selected", {
      verb: action === "stop"
        ? editor.t("status.verb_stopped")
        : action === "archive"
        ? editor.t("status.verb_archived")
        : editor.t("status.verb_deleted"),
    }));
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
  // "New Folder" dialog: name field, organize checkbox, Cancel / Create.
  // ---------------------------------------------------------------------
  if (createFolderPanel && createFolderDialog && e.panel_id === createFolderPanel.id()) {
    const d = createFolderDialog;
    if (e.event_type === "cancel") {
      // Esc / click-outside: the host already unmounted the panel, so
      // just drop our handle and refocus the dock.
      createFolderPanel = null;
      createFolderDialog = null;
      editor.setEditorMode(null);
      if (openPanel && dockMode) {
        dockBlurred = false;
        editor.floatingPanelControl(openPanel.id(), "focus", 0);
        openPanel.setFocusKey("sessions");
        refreshOpenDialog();
      }
      return;
    }
    if (e.event_type === "focus") {
      // Authoritative focus move (Tab / Shift+Tab / click) — mirror it
      // so the mode-level Enter binding can tell Cancel apart from the
      // rest of the dialog (see `orchestrator_folder_submit`).
      if (typeof e.widget_key === "string" && e.widget_key.length > 0) {
        createFolderFocusKey = e.widget_key;
      }
      return;
    }
    if (e.event_type === "change" && e.widget_key === "folder-name") {
      const payload = (e.payload ?? {}) as Record<string, unknown>;
      if (typeof payload.value === "string") d.name.value = payload.value;
      if (typeof payload.cursorByte === "number") d.name.cursor = payload.cursorByte;
      // Typing lands in the name field even if focus drifted; the host
      // routes printable chars to the focused TextInput only, so a
      // change event implies the field is focused again.
      createFolderFocusKey = "folder-name";
      return;
    }
    if (e.event_type === "toggle" && e.widget_key === "folder-organize") {
      const checked = (e.payload as { checked?: unknown })?.checked;
      d.organizeCurrent = typeof checked === "boolean" ? checked : !d.organizeCurrent;
      createFolderPanel.update(buildCreateFolderSpec());
      return;
    }
    if (e.event_type === "activate" && e.widget_key === "folder-cancel") {
      closeCreateFolderDialog();
      return;
    }
    if (e.event_type === "activate" && e.widget_key === "folder-create") {
      submitCreateFolder();
      return;
    }
    return;
  }
  // ---------------------------------------------------------------------
  // Dock session context menu (right-click): Visit / Archive / Delete.
  // ---------------------------------------------------------------------
  if (dockMenuPanel && dockMenuState && e.panel_id === dockMenuPanel.id()) {
    const target = dockMenuState.target;
    if (e.event_type === "cancel") {
      // Esc or a click outside dismissed the popup — the host already
      // unmounted the panel, so just drop our handle (don't unmount it
      // again) and hand keyboard focus back to the dock.
      dockMenuPanel = null;
      dockMenuState = null;
      if (openPanel && dockMode) {
        dockBlurred = false;
        dockFocus = "list";
        editor.floatingPanelControl(openPanel.id(), "focus", 0);
        openPanel.setFocusKey("sessions");
        refreshOpenDialog();
      }
      return;
    }
    if (e.event_type === "activate") {
      // Folder organise actions.
      if (target.kind === "folder") {
        if (e.widget_key === "ctx-rename") {
          // Same centered dialog UX as "New Folder" (was: a bottom
          // minibuffer prompt, inconsistent and label/value ran
          // together). The dialog blurs the dock itself.
          closeDockContextMenuAndRestoreDock();
          openRenameFolderDialog(target.id);
          return;
        }
        if (e.widget_key === "ctx-new-subfolder") {
          closeDockContextMenuAndRestoreDock();
          openCreateFolderDialog(target.id);
          return;
        }
        if (e.widget_key === "ctx-delete-folder") {
          deleteFolder(target.id);
          closeDockContextMenuAndRestoreDock();
          return;
        }
        return;
      }
      const id = target.id;
      if (e.widget_key === "ctx-retry") {
        closeDockContextMenuAndRestoreDock();
        retryPending(id);
        return;
      }
      if (e.widget_key === "ctx-dismiss") {
        closeDockContextMenuAndRestoreDock();
        dismissPending(id);
        return;
      }
      if (e.widget_key === "ctx-visit") {
        // Visit dives into the editor — no dock refocus.
        closeDockContextMenu();
        dockMenuVisit(id);
        return;
      }
      if (e.widget_key === "ctx-move") {
        // Swap the anchored popup for the "Move to folder…" dropdown in
        // the dock toolbar (so it shares the keyboard-navigable menu
        // path and the folder list).
        closeDockContextMenu();
        if (openPanel && dockMode) {
          dockBlurred = false;
          editor.floatingPanelControl(openPanel.id(), "focus", 0);
        }
        openDockMenu({ kind: "move", sessionId: id, index: 0 });
        return;
      }
      if (e.widget_key === "ctx-archive" && bulkEligible("archive", id)) {
        dockMenuEnterConfirm("archive");
        return;
      }
      if (e.widget_key === "ctx-delete" && bulkEligible("delete", id)) {
        dockMenuEnterConfirm("delete");
        return;
      }
      if (e.widget_key === "confirm-cancel") {
        // Back to the anchored menu (not all the way out) so a mis-click
        // on a destructive action is one click from recoverable.
        dockMenuState = {
          target,
          anchorCol: dockMenuState.anchorCol,
          anchorRow: dockMenuState.anchorRow,
          stage: "menu",
        };
        renderDockMenu();
        anchorDockMenu();
        return;
      }
      if (e.widget_key === "confirm-archive" || e.widget_key === "confirm-delete") {
        const action = e.widget_key === "confirm-archive" ? "archive" : "delete";
        closeDockContextMenuAndRestoreDock();
        void runConfirmedAction(action, [id]);
        return;
      }
    }
    return;
  }

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
    if (e.event_type === "change" && e.widget_key === "agent_dropdown") {
      // Agent selector: the host reports the newly-selected option index
      // (`payload.index`, verified against `handle_widget_dropdown_cycle`).
      // Map it back to a preset and apply it (fills the command field / hands
      // focus to it for "custom…"), then relay out the Tab cycle.
      const payload = (e.payload ?? {}) as Record<string, unknown>;
      const index = payload.index;
      if (typeof index === "number") {
        const presets = agentPresets();
        const preset = presets[index];
        if (preset) {
          applyAgentPreset(preset);
          rebuildFormFocusCycle();
        }
      }
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
        : field === "start_prompt"
        ? form.startPrompt
        : field === "branch"
        ? form.branch
        : field === "new_branch"
        ? form.newBranch
        : field === "ssh_host"
        ? form.sshHost
        : field === "ssh_path"
        ? form.sshPath
        : field === "ssh_identity"
        ? form.sshIdentity
        : field === "ssh_options"
        ? form.sshOptions
        : field === "k8s_target"
        ? form.k8sTarget
        : field === "k8s_context"
        ? form.k8sContext
        : field === "k8s_namespace"
        ? form.k8sNamespace
        : field === "k8s_pod"
        ? form.k8sPod
        : field === "k8s_workspace"
        ? form.k8sWorkspace
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
        // Re-render so the Create gating (which keys off the project path)
        // updates the disabled state as the user types.
        renderForm();
      } else if (field === "branch") {
        scheduleCompletionRefresh("branch");
      } else {
        // Any other field's change implicitly closes the
        // dropdown (the user moved on).
        closeCompletion();
        // The Kubernetes "Target" field toggles whether the explicit
        // context/namespace/pod/workspace inputs are shown, so a change
        // here re-lays-out the body and the Tab cycle.
        if (field === "k8s_target") {
          rebuildFormFocusCycle();
          renderForm();
        }
        // Editing the command changes which agent it resolves to, and thus
        // whether the Auto mode / Start prompt controls appear — re-lay-out.
        if (field === "cmd") {
          rebuildFormFocusCycle();
          renderForm();
        }
        // The remaining Create-gating fields: re-render so the disabled
        // state on the Create buttons tracks what the user has typed.
        if (field === "ssh_host" || field === "k8s_pod") {
          renderForm();
        }
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
    if (e.event_type === "toggle" && e.widget_key === "auto_mode") {
      const payload = (e.payload ?? {}) as Record<string, unknown>;
      const checked = payload.checked;
      if (typeof checked === "boolean") {
        form.autoMode = checked;
      } else {
        form.autoMode = !form.autoMode;
      }
      renderForm();
      return;
    }
    if (e.event_type === "toggle" && e.widget_key === "teach_fresh_cli") {
      const payload = (e.payload ?? {}) as Record<string, unknown>;
      const checked = payload.checked;
      if (typeof checked === "boolean") {
        form.teachFreshCli = checked;
      } else {
        form.teachFreshCli = !form.teachFreshCli;
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
      // "Run in:" type tabs. Enter/click on a *different* tab switches the
      // backend; on the *already-active* tab it means "move on" — advance
      // focus into the body (otherwise Enter would dead-end on the tab).
      const tab = SESSION_BACKENDS.find((b) => b.key === e.widget_key);
      if (tab) {
        if (form.backend !== tab.id) {
          selectBackend(tab.id);
        } else if (formPanel) {
          // Already on this tab — Enter means "dive into the fields": jump
          // past the other tab buttons straight to this backend's first input.
          const firstField = firstBodyFieldKey(form.backend);
          formPanel.setFocusKey(firstField);
          snapFormFocusTo(firstField);
        }
        return;
      }
      if (e.widget_key === "advanced_toggle") {
        // Fold / unfold the Advanced section (worktree + branch fields).
        form.advancedExpanded = !form.advancedExpanded;
        rebuildFormFocusCycle();
        renderForm();
        return;
      }
      if (e.widget_key === "create-visit") {
        // Gate: a Create with no required input is a no-op (the button is
        // also rendered disabled, but Enter-on-button could still reach here).
        if (!formIsSubmittable()) return;
        void submitForm(true);
      } else if (e.widget_key === "create-bg") {
        if (!formIsSubmittable()) return;
        void submitForm(false);
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
      // Esc while connecting aborts the in-flight remote attach, same as the
      // Cancel button: reject the promise and discard the late result host-side.
      if (form?.submitting) {
        pendingRemoteFacet = null;
        editor.cancelRemoteAgent();
      }
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
        // Leaving the dock also closes the project dropdown so it
        // doesn't linger over the blurred dock.
        openDialog.projectMenuOpen = false;
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
        } else {
          // Re-render so the keyboard hints drop off the blurred dock.
          openPanel?.update(buildDockSpec());
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
        const wasBlurred = dockBlurred;
        dockBlurred = false;
        dockFocus = e.widget_key === "filter" ? "filter" : "list";
        // Re-render so the keyboard hints reappear now the dock holds
        // focus again.
        if (wasBlurred) openPanel?.update(buildDockSpec());
      }
      return;
    }
    if (e.event_type === "dock_space") {
      // Space bulk-select is a modal-picker feature now; the dock
      // ignores Space (no multi-select checkboxes here).
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
      // Host Enter on the dock's session tree — the "dive in" gesture for
      // a session, or expand/collapse for a folder. A session row is
      // already the active window via the arrow/click live-switch (a
      // discovered worktree was opened on navigation too), so Enter just
      // hands keyboard focus to the editor. If the row is still discovered
      // here (Enter pressed before the debounced switch landed) attach it
      // now, diving in to match the live path below.
      if (!dockMode || !openPanel || !openDialog) return;
      const node = dockSelectedNode();
      if (node && node.kind === "folder") {
        toggleDockFolderExpansion(folderNodeKey(node.folderId));
        return;
      }
      const id = dockSelectedSessionId();
      const sel = typeof id === "number" ? orchestratorSessions.get(id) : undefined;
      // Enter on a being-created placeholder: resume a paused/failed one
      // (retry), or do nothing while it is still creating (there is no
      // window to dive into). Never blur to the editor — that would drop
      // focus onto whatever buffer sits behind the phantom row.
      if (sel && sel.pending) {
        if (pendingActionable(sel.pending)) retryPending(id as number);
        return;
      }
      if (sel && sel.discovered) {
        void attachToWorktree({
          root: sel.root,
          projectPath: sel.projectPath ?? sel.root,
          label: sel.label,
          branch: sel.branch,
          discoveredId: sel.id,
          dive: true,
        });
        return;
      }
      // Enter is the deliberate dive: if the highlighted session isn't the
      // active window yet — a disconnected remote that `scheduleDockSwitch`
      // intentionally did NOT auto-connect on arrow-nav — commit the switch
      // now. For a dormant remote this lands in its "Connecting…" shell (the
      // #2570 dive path); for a live/local row it's the switch arrow-nav would
      // otherwise have made before the debounce landed.
      if (typeof id === "number" && id > 0 && id !== editor.activeWindow()) {
        editor.setActiveWindow(id);
      }
      dockBlurred = true;
      editor.floatingPanelControl(openPanel.id(), "blur", 0);
      editor.setEditorMode(null);
      return;
    }
    if (e.event_type === "dock_context") {
      // Host Menu key / Shift+F10 on the dock tree — open the
      // highlighted node's context menu, the same one a right-click
      // anchors at the pointer. Keyboard parity for Move to Folder…
      // and the folder organise actions.
      if (dockMode) openDockContextMenuFromKeyboard();
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
      // The dock's scope control is now the project dropdown; Alt+P
      // opens/closes it instead of flipping the old current/all scope.
      // Opening hands the keyboard to the menu; closing returns it to
      // the session list.
      if (dockMode) {
        if (openDialog.projectMenuOpen) closeProjectMenu();
        else openProjectMenu();
      }
      return;
    }
    // Dock dropdown keyboard nav. The host fires these only while panel
    // focus sits on a `project-pick:` / `menu-pick:` row (a dropdown is
    // open and owns the keyboard), so ↑/↓/Enter/Esc drive whichever
    // dropdown is up instead of leaking to the session tree underneath.
    if (e.event_type === "dock_menu_prev") {
      if (openDialog.dockMenu) moveDockMenu(-1);
      else moveProjectMenu(-1);
      return;
    }
    if (e.event_type === "dock_menu_next") {
      if (openDialog.dockMenu) moveDockMenu(1);
      else moveProjectMenu(1);
      return;
    }
    if (e.event_type === "dock_menu_accept") {
      if (openDialog.dockMenu) acceptDockMenu();
      else acceptProjectMenu();
      return;
    }
    if (e.event_type === "dock_menu_cancel") {
      if (openDialog.dockMenu) closeDockMenu();
      else closeProjectMenu();
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
      // The tree force-opens every folder while a search is active (so
      // matches aren't buried) and restores the user's expansion set when
      // the search clears. `refreshOpenDialog` just rebuilt the tree, so
      // `dockKeys` is current.
      if (dockMode) applyDockExpansion();
      return;
    }
    // Right-click on a tree node → open its context menu. Only the dock
    // wires this up (the host fires `context` for right-clicks in the dock
    // column); the modal picker has its own action buttons. For the tree
    // the node key rides in `payload.key`/`widget_key`; `payload.index` is
    // the flat node index into `dockNodes`.
    if (
      e.event_type === "context" &&
      dockMode &&
      (e.widget_key === "sessions" ||
        ((e.payload ?? {}) as Record<string, unknown>).list_key === "sessions")
    ) {
      const payload = (e.payload ?? {}) as Record<string, unknown>;
      const idx = payload.index;
      const col = typeof payload.col === "number" ? payload.col : 0;
      const row = typeof payload.row === "number" ? payload.row : 0;
      if (typeof idx === "number") openDockContextMenu(idx, col, row);
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
      if (typeof idx === "number") {
        clearDialogError();
        if (dockMode) {
          // The dock is a tree. Track the highlighted node by key. A
          // *folder* row only moves the highlight (and a click toggles
          // its expansion — the disclosure column does that too). A
          // *session* row live-switches the active window: arrowing
          // wipes down/up and keeps focus on the dock so you can keep
          // arrowing; a *click* is a deliberate "open this" gesture that
          // also dives (hands keyboard focus to the window). A discovered
          // on-disk worktree has no window, so both paths attach a fresh
          // session instead.
          const key = typeof payload.key === "string"
            ? payload.key
            : (openDialog.dockKeys[idx] ?? null);
          const prevKey = openDialog.dockSelKey;
          const prevIdx = prevKey ? openDialog.dockKeys.indexOf(prevKey) : -1;
          openDialog.dockSelKey = key;
          openPanel.update(buildDockSpec());
          openPanel.setSelectedIndex("sessions", idx);
          const node = key
            ? openDialog.dockNodes[openDialog.dockKeys.indexOf(key)] ?? null
            : null;
          if (node && node.kind === "folder") {
            if (payload.via === "click") toggleDockFolderExpansion(key!);
            return;
          }
          const fromEdge = idx > prevIdx ? "bottom" : idx < prevIdx ? "top" : null;
          if (payload.via === "click") diveDockSelectionFromClick(fromEdge);
          else scheduleDockSwitch(fromEdge);
          return;
        }
        openDialog.selectedIndex = idx;
        // Update preview pane.
        openPanel.update(buildOpenSpec());
        // Re-pin the list selection so the spec re-emit doesn't
        // snap it back to 0.
        openPanel.setSelectedIndex("sessions", openDialog.selectedIndex);
        // A mouse click on an inactive on-disk worktree opens it
        // directly — no Enter/Visit "confirmation" needed. There's no
        // live window to switch to, so attach a fresh session, mirroring
        // the dialog's Visit (`activate`) path. Live rows keep
        // select+preview (opened with Enter / Visit). Arrow-nav fires
        // `select` without `via`, so it only previews.
        if (payload.via === "click") {
          const clickedId = openDialog.filteredIds[openDialog.selectedIndex];
          const clicked = typeof clickedId === "number"
            ? orchestratorSessions.get(clickedId)
            : undefined;
          if (clicked && clicked.discovered) {
            closeOpenDialog();
            void attachToWorktree({
              root: clicked.root,
              projectPath: clicked.projectPath ?? clicked.root,
              label: clicked.label,
              branch: clicked.branch,
              discoveredId: clicked.id,
            });
            return;
          }
        }
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
      if (sel && sel.pending) {
        // A being-created placeholder has no window to open — resume a
        // paused/failed one (retry); a still-creating one is a no-op.
        if (pendingActionable(sel.pending)) retryPending(id as number);
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
      if (dockMode) {
        // "New Task… ▾" is a dropdown: toggle the create menu (New Task…
        // / New Folder…) rather than opening the form directly.
        if (openDialog.dockMenu?.kind === "new") closeDockMenu();
        else openDockMenu({ kind: "new", index: 0 });
        return;
      }
      closeOpenDialog();
      openForm({ fromPicker: true });
      return;
    }
    // A dock dropdown option button was clicked (New / Move menus).
    if (
      e.event_type === "activate" &&
      typeof e.widget_key === "string" &&
      e.widget_key.startsWith("menu-pick:")
    ) {
      runDockMenuOption(e.widget_key.slice("menu-pick:".length));
      return;
    }
    // Toggle the collapsible Filters section.
    if (e.event_type === "activate" && e.widget_key === "filters-toggle") {
      openDialog.filtersExpanded = !openDialog.filtersExpanded;
      if (openPanel) openPanel.update(buildDockSpec());
      return;
    }
    // Host-owned tree expansion changed (a disclosure click or →/← on a
    // folder): mirror it into the persisted expansion set so it survives
    // re-renders and restarts.
    if (e.event_type === "expand" && e.widget_key === "sessions") {
      const payload = (e.payload ?? {}) as Record<string, unknown>;
      const key = payload.key;
      const expanded = payload.expanded;
      if (typeof key === "string" && key.startsWith(FOLDER_NODE_PREFIX)) {
        const set = loadExpanded();
        if (expanded === true) set.add(key);
        else set.delete(key);
        saveExpanded();
        // A fold changes how many rows the tree occupies; re-render so
        // the blank padding between the tree and the bottom hint bar
        // re-balances and the hints stay pinned to the dock's bottom.
        if (dockMode && openPanel) openPanel.update(buildDockSpec());
      }
      return;
    }
    // Dock "Manage" → open the full modal picker (lifecycle actions
    // Stop/Archive/Delete + bulk-select) *beside* the dock. The dock
    // stays mounted in its own slot, dimmed and passive, and Esc on the
    // picker hands control back to it (`openControlRoom` parks the dock
    // in `dockPanel` when one is showing).
    if (e.event_type === "activate" && e.widget_key === "manage") {
      openControlRoom();
      return;
    }
    // Filters panel "Move…" button → the same Move-to-Folder dropdown
    // the row context menu offers, for the highlighted/current session.
    if (e.event_type === "activate" && e.widget_key === "move-session") {
      openMoveToFolderForCurrent();
      return;
    }
    // Dock "view" button → flip card ⇄ compact density and re-render.
    if (e.event_type === "activate" && e.widget_key === "view-toggle") {
      dockView = dockView === "card" ? "compact" : "card";
      if (openPanel) openPanel.update(buildDockSpec());
      return;
    }
    // Dock project dropdown: the toolbar button toggles the menu open,
    // and each option button picks a project (empty suffix = all).
    if (e.event_type === "activate" && e.widget_key === "project-menu") {
      if (openDialog.projectMenuOpen) closeProjectMenu();
      else openProjectMenu();
      return;
    }
    if (
      e.event_type === "activate" &&
      typeof e.widget_key === "string" &&
      e.widget_key.startsWith("project-pick:")
    ) {
      pickProject(e.widget_key.slice("project-pick:".length));
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
      // Esc / native `[×]` unmounted the picker panel — sync our own
      // state. This teardown is for the centered MODAL picker only: the
      // dock (LeftDock placement) never fires `cancel` (its Esc blurs to
      // the editor, host-side), and the modal always runs with
      // `dockMode === false` — even when floated over a live dock, where
      // the dock is parked in `dockPanel`. Guard on `dockMode` so a stray
      // cancel can never tear the dock down.
      if (dockMode) return;
      openPanel = null;
      // If the picker was floated over a live dock, hand control back to
      // the dock (still mounted in its own slot) rather than dropping to
      // the bare editor.
      if (restoreDockBehindPicker()) return;
      openDialog = null;
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
    editor.setStatus(editor.t("status.kill_open_list_first"));
    return;
  }
  const ids = openDialog.filteredIds;
  if (ids.length === 0) {
    editor.setStatus(editor.t("status.kill_no_selected"));
    return;
  }
  const id = ids[Math.max(0, Math.min(openDialog.selectedIndex, ids.length - 1))];
  if (id <= 0) {
    editor.setStatus(editor.t("status.kill_select_row"));
    return;
  }
  if (id === editor.activeWindow()) {
    editor.setStatus(editor.t("status.kill_dive_elsewhere"));
    return;
  }
  const s = orchestratorSessions.get(id);
  if (s && s.terminalId !== null) {
    editor.closeTerminal(s.terminalId);
  }
  // Tombstone so reconcile drops the row immediately instead of resurrecting
  // it from the stale window snapshot until the deferred close lands.
  closingWindowIds.add(id);
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

editor.on("window_closed", (e) => {
  // The host has confirmed this window is gone, so drop any tombstone for
  // it (reconcile won't re-add it now that it's out of `listWindows()` —
  // this just keeps the set from growing).
  if (e && typeof e.id === "number") closingWindowIds.delete(e.id);
  refreshOpenDialog();
});

// Startup: re-surface any workspaces that were still being created when the
// editor last quit. They come back as paused placeholder rows the user
// resumes (Enter) or dismisses — nothing auto-runs (§ recoverPendingWorkspaces).
editor.on("ready", () => {
  recoverPendingWorkspaces();
});

// Grace window after a session becomes active during which terminal
// output is attributed to the activation redraw (or an attach's shell
// startup), not the agent — so selecting a session doesn't flash it
// `working`.
const ACTIVATION_GRACE_MS = 1500;

editor.on("active_window_changed", () => {
  const s = orchestratorSessions.get(editor.activeWindow());
  if (s) s.activatedAt = Date.now();
  refreshOpenDialog();
  // A passive (blurred) dock mirrors the active window, so keep its
  // highlighted row in sync when focus moves to another window from
  // outside the dock. While the dock holds focus the user drives ↑/↓
  // selection (and the debounced live-switch already aligns the two), so
  // re-selecting here would fight the scroll — hence the blurred guard.
  if (dockBlurred) syncDockSelectionToActive();
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

// `sessionState` buckets `working`/`idle` from `lastOutputAt` at render
// time, but a re-render only happens on *new* output. So when a session
// goes quiet nothing repaints it, and the row freezes on `working` until
// some unrelated event forces a redraw. This schedules one refresh just
// past the idle window so the working→idle flip happens on its own. The
// token collapses back-to-back outputs into a single pending sweep, and
// because it covers the *most recent* output across all sessions, the
// sweep recomputes (and idles) every quiet session at once.
let idleSweepToken = 0;
function scheduleIdleSweep(): void {
  const token = ++idleSweepToken;
  void editor.delay(IDLE_AFTER_MS + 100).then(() => {
    if (idleSweepToken !== token) return;
    refreshOpenDialog();
  });
}

editor.on("terminal_output", (payload) => {
  const s = orchestratorSessions.get(payload.window_id);
  if (s) {
    // Ignore the redraw burst a terminal emits right after its window
    // becomes active — that's not the agent working, and counting it
    // would flash the card to `working` on every selection.
    if (s.activatedAt !== undefined && Date.now() - s.activatedAt < ACTIVATION_GRACE_MS) {
      return;
    }
    // Stamp the moment of output. `sessionState` turns this into
    // working/idle; the cached `state` is updated so persistence and
    // any non-render reader see a fresh value too.
    s.lastOutputAt = Date.now();
    s.state = "working";
    refreshOpenDialog();
    // Ensure the row flips back to idle once output stops, even if no
    // further event arrives to trigger a render.
    scheduleIdleSweep();
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
  "%cmd.open",
  "%cmd.open_desc",
  "orchestrator_open",
  null,
  { terminalBypass: true },
);
editor.registerCommand(
  "%cmd.new",
  "%cmd.new_desc",
  "orchestrator_new",
  null,
  { terminalBypass: true },
);
editor.registerCommand(
  "%cmd.kill",
  "%cmd.kill_desc",
  "orchestrator_kill",
  null,
  { terminalBypass: true },
);
editor.registerCommand(
  "%cmd.dock_toggle",
  "%cmd.dock_toggle_desc",
  "orchestrator_dock_toggle",
  null,
  { terminalBypass: true },
);
editor.registerCommand(
  "%cmd.move",
  "%cmd.move_desc",
  "orchestrator_move",
  null,
  { terminalBypass: true },
);
