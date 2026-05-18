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

type AgentState = "running" | "awaiting" | "ready" | "errored" | "killed";

interface AgentSession {
  // Editor's stable session id.
  id: number;
  // Display label (defaults to root basename — Orchestrator never
  // renames externally-created sessions).
  label: string;
  // Absolute filesystem root.
  root: string;
  // Canonical project root this session belongs to (set at
  // create time from the Project Path field). `null` for
  // sessions created outside the new-session form (e.g. the
  // editor's base session, or sessions from before the
  // Project Path field shipped).
  projectPath: string | null;
  // `true` if the session was created with the worktree
  // checkbox unchecked (shared worktree / non-git path).
  sharedWorktree: boolean;
  // The terminal id Orchestrator spawned in this session, if any.
  terminalId: number | null;
  // Last parsed agent state. "active" is computed at render
  // time from `editor.activeWindow()`, not stored.
  state: AgentState;
  // Wall-clock ms when orchestrator.new fired createWindow.
  createdAt: number;
}

// =============================================================================
// Module state — editor-global, survives every dive.
// =============================================================================

const orchestratorSessions = new Map<number, AgentSession>();

// Pending session-creation intent. Stashed across the
// async `createWindow → window_created hook` handoff so the
// hook handler can attach the spawned terminal. (Internally
// the editor calls these "windows"; Orchestrator still presents
// them as "sessions" in its UX.)
let pendingNewSession:
  | {
      label: string;
      branch: string;
      cmd: string;
      root: string;
      // Recorded for `setWindowState` after `window_created`.
      // `projectPath` is the canonical project root the user
      // pointed the form at; `sharedWorktree` is `true` when the
      // session shares its working tree (no dedicated `git
      // worktree add`).
      projectPath: string;
      sharedWorktree: boolean;
    }
  | null = null;

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
  // older, etc.
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
    items: string[];
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
  // panel for the named action against the named session id.
  // Cleared on Cancel or after the action completes.
  pendingConfirm:
    | { action: "stop" | "archive" | "delete"; sessionId: number }
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
}
let openDialog: OpenDialogState | null = null;
let openPanel: FloatingWidgetPanel | null = null;
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
        projectPath: s.project_path ?? null,
        sharedWorktree: s.shared_worktree ?? false,
        terminalId: null,
        // The base session has no agent; everything else
        // defaults to "running" until a terminal_output /
        // terminal_exit arrives.
        state: "running",
        createdAt: Date.now(),
      });
    } else {
      existing.label = s.label;
      existing.root = s.root;
      if (s.project_path != null) existing.projectPath = s.project_path;
      if (s.shared_worktree != null) existing.sharedWorktree = s.shared_worktree;
    }
  }
  for (const id of orchestratorSessions.keys()) {
    if (!seen.has(id)) orchestratorSessions.delete(id);
  }
}

// =============================================================================
// Session display helpers
// =============================================================================

const STATE_GLYPH: Record<AgentState, string> = {
  running: "RUN ",
  awaiting: "WAIT",
  ready: "DONE",
  errored: "ERR ",
  killed: "KILL",
};

function ageString(createdAt: number): string {
  const sec = Math.max(0, Math.floor((Date.now() - createdAt) / 1000));
  if (sec < 60) return `${sec}s`;
  if (sec < 3600) return `${Math.floor(sec / 60)}m`;
  return `${Math.floor(sec / 3600)}h`;
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
function filterSessions(needle: string): number[] {
  reconcileSessions();
  const ids = Array.from(orchestratorSessions.keys()).sort((a, b) => a - b);
  if (!needle) return ids;
  const n = needle.toLowerCase();
  type Scored = { id: number; score: number; len: number };
  const matches: Scored[] = [];
  for (const id of ids) {
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
  matches.sort((a, b) => a.score - b.score || a.len - b.len || a.id - b.id);
  return matches.map((m) => m.id);
}

// Build one rendered list-item row for `id`. Style cues:
//   * `[id]` in `ui.help_key_fg`
//   * `ACT` (active session) in `ui.tab_active_fg` + bold
//   * other states use the default fg
//   * label in default fg
function renderListItem(id: number, activeId: number): TextPropertyEntry {
  const s = orchestratorSessions.get(id);
  if (!s) {
    return styledRow([{ text: `[${id}] (unknown)` }]);
  }
  const isActive = id === activeId;
  const isBase = id === 1;
  const stateText = isActive ? "ACT " : STATE_GLYPH[s.state];
  const entries: { text: string; style?: Record<string, unknown> }[] = [
    { text: `[${id}] `, style: { fg: "ui.help_key_fg" } },
    {
      text: stateText,
      style: isActive
        ? { fg: "ui.tab_active_fg", bold: true }
        : { fg: "ui.menu_disabled_fg" },
    },
    { text: `  ${s.label}` },
  ];
  // BASE badge: the base session is the editor process itself —
  // archive / delete would close the editor (and possibly destroy
  // the user's current worktree), so the host refuses both. The
  // badge makes that special status visible up-front instead of
  // surfacing as "Archive disabled, why?" after a Tab.
  if (isBase) {
    entries.push({
      text: " BASE",
      style: { fg: "ui.help_key_fg", bold: true },
    });
  }
  // SHARED badge for the row when this session shares its
  // worktree (with another session or with the project root
  // directly). Mirrors the preview pane's badge so the
  // shared-worktree status is visible at-a-glance from the
  // list too.
  if (s.sharedWorktree || countSiblingsAtRoot(s.root) > 1) {
    entries.push({
      text: " ⇄",
      style: { fg: "ui.menu_disabled_fg" },
    });
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
  const isBase = s.id === 1;
  const stateText = isActive ? "ACT" : STATE_GLYPH[s.state].trim();
  // Count siblings sharing the same `root`. The set includes
  // `s` itself; `> 1` means at least one other session lives at
  // the same path (shared-worktree mode, or two sessions
  // explicitly aimed at the same directory).
  const sharedCount = countSiblingsAtRoot(s.root);
  const headerEntries: { text: string; style?: Record<string, unknown> }[] = [
    {
      text: stateText,
      style: isActive
        ? { fg: "ui.tab_active_fg", bold: true }
        : { fg: "ui.menu_disabled_fg" },
    },
    { text: "  " },
    { text: ageString(s.createdAt), style: { fg: "ui.menu_disabled_fg" } },
  ];
  if (isBase) {
    // BASE badge in the preview — the long-form counterpart to
    // the list-row badge, with an inline explanation so the user
    // doesn't have to wonder why Stop / Archive / Delete are
    // greyed out.
    headerEntries.push(
      { text: "  " },
      {
        text: "BASE",
        style: { fg: "ui.help_key_fg", bold: true },
      },
      { text: " — editor session", style: { fg: "ui.menu_disabled_fg", italic: true } },
    );
  }
  if (sharedCount > 1) {
    headerEntries.push(
      { text: "  " },
      {
        text: `SHARED ×${sharedCount}`,
        style: { fg: "ui.status_error_indicator_fg", bold: true },
      },
    );
  } else if (s.sharedWorktree) {
    // Single-session shared-worktree mode (the user opted out of
    // a dedicated worktree even though no second session is on
    // this root yet). Still worth surfacing so the user knows
    // why Archive / Delete refuse to run a `git worktree
    // remove` here.
    headerEntries.push(
      { text: "  " },
      {
        text: "SHARED",
        style: { fg: "ui.menu_disabled_fg", italic: true },
      },
    );
  }
  return [
    styledRow(headerEntries as Parameters<typeof styledRow>[0]),
    styledRow([
      { text: s.root, style: { fg: "ui.menu_disabled_fg" } },
    ]),
  ];
}

/// Return the number of orchestrator sessions whose `root`
/// equals `root`. Used to surface "SHARED ×N" in the preview
/// pane and to refuse Archive / Delete on a shared root
/// while another session still lives there.
function countSiblingsAtRoot(root: string): number {
  let n = 0;
  for (const s of orchestratorSessions.values()) {
    if (s.root === root) n += 1;
  }
  return n;
}

// Blank-row separator used inside the Sessions column between
// the filter, the new-session button, and the list.
function sessionsSeparator(): WidgetSpec {
  return spacer(0);
}

// Approximate number of session rows the picker's list pane
// should show. Sized off the full terminal (not the active
// buffer's viewport — that shrinks with vertical splits and made
// the picker collapse to ~half its `heightPct: 90` budget).
function openListVisibleRows(): number {
  const screen = editor.getScreenSize();
  const h = screen.height > 0 ? screen.height : 30;
  const panelH = Math.floor(h * 0.9);
  // panel borders (2) + header (1) + spacer (1) + sessions
  // section borders (2) + filter row (1) + separator (1) +
  // new-session button row (1) + separator (1) + footer (1) =
  // 11 rows of chrome. Floor at 4 so a tiny terminal still
  // shows something.
  return Math.max(4, panelH - 11);
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
  if (openDialog?.pendingConfirm && s && openDialog.pendingConfirm.sessionId === s.id) {
    const action = openDialog.pendingConfirm.action;
    if (action === "stop") {
      return labeledSection({
        label: "Confirm Stop",
        child: col(
          {
            kind: "raw",
            entries: [
              styledRow([
                {
                  text: `Stop session [${s.id}] ${s.label}?`,
                  style: { bold: true },
                },
              ]),
              styledRow([{ text: "" }]),
              styledRow([{ text: "This will:" }]),
              styledRow([{ text: "  • send SIGTERM to all session processes" }]),
              styledRow([{ text: "  • SIGKILL after a short grace period" }]),
              styledRow([{ text: "" }]),
              styledRow([{ text: "The worktree and session record remain." }]),
            ],
          },
          spacer(0),
          row(
            flexSpacer(),
            button("Cancel", { key: "confirm-cancel" }),
            spacer(2),
            button("Confirm Stop", {
              intent: "danger",
              key: "confirm-stop",
            }),
          ),
        ),
      });
    }
    if (action === "archive") {
      return labeledSection({
        label: "Confirm Archive",
        child: col(
          {
            kind: "raw",
            entries: [
              styledRow([
                {
                  text: `Archive session [${s.id}] ${s.label}?`,
                  style: { bold: true },
                },
              ]),
              styledRow([{ text: "" }]),
              styledRow([{ text: "This will:" }]),
              styledRow([{ text: "  • SIGKILL all session processes" }]),
              styledRow([{ text: "  • close the editor session" }]),
              styledRow([{ text: "  • move the worktree to .archived/" }]),
              styledRow([{ text: "" }]),
              styledRow([{ text: "Reversible via Unarchive." }]),
            ],
          },
          spacer(0),
          row(
            flexSpacer(),
            button("Cancel", { key: "confirm-cancel" }),
            spacer(2),
            button("Confirm Archive", {
              intent: "danger",
              key: "confirm-archive",
            }),
          ),
        ),
      });
    }
    if (action === "delete") {
      return labeledSection({
        label: "Confirm Delete",
        child: col(
          {
            kind: "raw",
            entries: [
              styledRow([
                {
                  text: `Delete session [${s.id}] ${s.label}?`,
                  style: { bold: true },
                },
              ]),
              styledRow([{ text: "" }]),
              styledRow([{ text: "This will:" }]),
              styledRow([{ text: "  • stop all session processes" }]),
              styledRow([{ text: "  • run `git worktree remove`" }]),
              styledRow([{ text: "  • drop the session record" }]),
              styledRow([{ text: "" }]),
              styledRow([
                {
                  text: "Uncommitted changes will be lost.",
                  style: {
                    fg: "ui.status_error_indicator_fg",
                    bold: true,
                  },
                },
              ]),
            ],
          },
          spacer(0),
          row(
            flexSpacer(),
            button("Cancel", { key: "confirm-cancel" }),
            spacer(2),
            button("Confirm Delete", {
              intent: "danger",
              key: "confirm-delete",
            }),
          ),
        ),
      });
    }
  }
  // Match the sessions column's content height so the two panes'
  // bottom borders land on the same row. Sessions column inside
  // its borders = filter (1) + separator (1) + new-session button
  // (1) + separator (1) + list (listVisibleRows) = listVisibleRows
  // + 4. Preview inside its borders = button row (1) + spacer (1)
  // + embedRows, so embedRows must equal listVisibleRows + 2.
  // When details ARE shown, two info rows + a spacer eat three
  // more lines — `_DETAILS_CHROME_ROWS` accounts for that.
  const totalEmbedBase = (openDialog?.listVisibleRows ?? 6) + 2;
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
  // Per-action availability. The row always renders all four
  // buttons (no layout shift between selections), but each is
  // marked disabled when its action would be refused against the
  // current selection. Disabled buttons show in `ui.menu_disabled_fg`,
  // drop out of the Tab cycle, and reject clicks — matching the
  // same conditions that `stopSelectedSession`, `enterConfirm`,
  // and the lifecycle handlers already check internally.
  //
  //  * Stop: refused on the base session (id 1).
  //  * Archive / Delete: also refused on the base session, plus
  //    when this session shares its worktree with the project
  //    root (no `git worktree` entry to remove) or shares a root
  //    with other live sessions (would yank disk out from
  //    under them).
  const isBase = s.id === 1;
  const siblings = countSiblingsAtRoot(s.root);
  const sharesRoot = siblings > 1 || s.sharedWorktree;
  const stopDisabled = isBase;
  const lifecycleDisabled = isBase || sharesRoot;
  const buttonRow = row(
    button("Visit", { intent: "primary", key: "visit" }),
    spacer(2),
    flexSpacer(),
    button(detailsToggleLabel, { key: "toggle-details" }),
    spacer(2),
    button("Stop", { key: "stop", disabled: stopDisabled }),
    spacer(2),
    button("Archive", { key: "archive", disabled: lifecycleDisabled }),
    spacer(2),
    button("Delete", {
      intent: "danger",
      key: "delete",
      disabled: lifecycleDisabled,
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
  // Surface BASE in the preview section label so it's always visible
  // (the list-row badge gets truncated at 25% column width). The
  // base session is the editor process itself — closing or moving
  // its worktree would close the editor / break the user's current
  // tree, so Stop / Archive / Delete refuse against it.
  const sectionLabel = isBase
    ? `[${s.id}] ${s.label}  BASE — editor session`
    : `[${s.id}] ${s.label}`;
  return labeledSection({
    label: sectionLabel,
    child: body,
  });
}

function buildOpenSpec(): WidgetSpec {
  if (!openDialog) return col();
  // Re-derive row counts on every spec build as a fallback for the
  // resize hook not always firing reliably through tmux's SIGWINCH
  // propagation (Finding I). **One-way ratchet**: only adopt the
  // new value when it's *larger* than the current one. The
  // `editor.getViewport()` height shrinks while the picker is
  // mounted (the floating panel covers part of the buffer area),
  // and naively re-reading it on every refresh fed that shrink
  // back into the dialog size — pressing Up/Down caused the
  // picker to oscillate smaller on every keystroke. A real
  // terminal-grow event still flows through because the new
  // viewport height exceeds the cached value; a spurious shrink
  // (because the panel itself is up) is ignored.
  const liveListVisibleRows = openListVisibleRows();
  if (liveListVisibleRows > openDialog.listVisibleRows) {
    openDialog.listVisibleRows = liveListVisibleRows;
    openDialog.embedRows = Math.max(3, liveListVisibleRows - 5);
  }
  const filtered = openDialog.filteredIds;
  const activeId = editor.activeWindow();
  const items = filtered.map((id) => renderListItem(id, activeId));
  const itemKeys = filtered.map(String);
  const selIdx = filtered.length === 0
    ? -1
    : Math.max(0, Math.min(openDialog.selectedIndex, filtered.length - 1));
  const selectedId = selIdx >= 0 ? filtered[selIdx] : -1;
  const selectedSession = selectedId > 0
    ? orchestratorSessions.get(selectedId)
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
  const newLabel = newKey
    ? `+ New Session  ${newKey}`
    : "+ New Session";
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
    placeholder: "type to filter…",
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
  return col(
    {
      kind: "raw",
      entries: [
        styledRow([
          {
            text: "ORCHESTRATOR :: Sessions",
            style: { fg: "ui.popup_border_fg", bold: true },
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
        label: `Sessions (${filtered.length})`,
        widthPct: 25,
        // Sessions column: new-session button, separator,
        // filter, separator, list. The button is first so it
        // gets initial focus (Enter immediately opens the new
        // session form). Separators are long `─` strings that
        // the renderer truncates to the column's inner width —
        // no need to measure cells from the plugin side.
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
          sessionsSeparator(),
          filterInput,
          sessionsSeparator(),
          list({
            items,
            itemKeys,
            selectedIndex: selIdx,
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
      // remaining 75% by default since the sessions list took
      // 25%.
      buildPreviewPane(selectedSession),
    ),
    row(
      flexSpacer(),
      hintBar([
        { keys: "↑↓", label: "nav" },
        { keys: "Enter", label: "dive" },
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
  openPanel.update(buildOpenSpec());
  // The list widget's `selectedIndex` in the spec is initial-only;
  // pin it via mutation so re-renders don't snap back to 0.
  if (openDialog.filteredIds.length > 0) {
    openPanel.setSelectedIndex("sessions", openDialog.selectedIndex);
  }
}

function openControlRoom(): void {
  if (openPanel) return;
  reconcileSessions();
  const activeId = editor.activeWindow();
  const listVisibleRows = openListVisibleRows();
  openDialog = {
    filter: { value: "", cursor: 0 },
    filteredIds: [],
    selectedIndex: 0,
    originalActiveSession: activeId,
    pendingConfirm: null,
    listVisibleRows,
    // Mirror buildPreviewPane's chrome: 1 button row + 1 spacer
    // + 2 info rows + 1 spacer = 4 rows reserved above the embed.
    // Preview chrome above the embed: 1 button row + 1 spacer + 2
    // info rows + 1 spacer = 5 rows. The labeledSection's top/bottom
    // borders match the sessions list's, so subtracting just the
    // chrome makes the preview pane's apparent height match the
    // list pane's (`visible_rows + 2 borders`) exactly. Floored at
    // 3 so a tiny terminal still leaves enough rows for the embed
    // to paint something meaningful.
    embedRows: Math.max(3, listVisibleRows - 5),
    showDetails: false,
    inFlight: null,
    lastError: null,
  };
  openDialog.filteredIds = filterSessions("");
  const activeIdx = openDialog.filteredIds.indexOf(activeId);
  openDialog.selectedIndex = activeIdx >= 0 ? activeIdx : 0;
  openPanel = new FloatingWidgetPanel();
  // 90% × 90% of the terminal — the open dialog wants room for
  // a real session list + preview pane, unlike the new-session
  // form which stays compact.
  openPanel.mount(buildOpenSpec(), { widthPct: 90, heightPct: 90 });
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
  openPanel.setFocusKey("visit");
  editor.setEditorMode(OPEN_MODE);
}

function closeOpenDialog(): void {
  if (openPanel) {
    openPanel.unmount();
    openPanel = null;
  }
  openDialog = null;
  editor.setEditorMode(null);
}

// Stop every process the highlighted session owns. Sends
// SIGTERM first via the host's `signalWindow` (which fans
// out through the window's process-group tracker), then
// follows up with SIGKILL after a short grace period so
// ill-behaved agents that ignore SIGTERM still get reaped.
// The session record stays put — Stop only kills processes,
// it doesn't touch the worktree or the editor session.
function stopSelectedSession(): void {
  if (!openDialog) return;
  const id = openDialog.filteredIds[openDialog.selectedIndex];
  if (typeof id !== "number" || id <= 0) return;
  if (id === 1) {
    setDialogError("cannot stop the base session");
    refreshOpenDialog();
    return;
  }
  editor.signalWindow(id, "SIGTERM");
  // SIGKILL fallback for agents that ignore SIGTERM. The
  // host's signalWindow is idempotent on already-exited
  // process groups, so the second call is safe whether or
  // not the first one took. QuickJS has no `setTimeout`;
  // the host exposes `editor.delay(ms)` as the asynchronous
  // sleep primitive, which we kick off but don't await.
  void editor.delay(2000).then(() => {
    editor.signalWindow(id, "SIGKILL");
  });
  editor.setStatus(`Orchestrator: stop signal sent to session [${id}]`);
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
  return 1;
}

// Archive flow: stop all processes (SIGKILL — archive is a
// "I'm done with this for now" action, no graceful teardown
// needed since the worktree stays on disk), close the editor
// session, move the worktree to the `.archived/` graveyard,
// and append a manifest entry so a future Unarchive flow can
// reverse it.
async function archiveSelectedSession(explicitId?: number): Promise<void> {
  if (!openDialog) return;
  // Prefer the explicit id from the confirm path. Otherwise read
  // the currently selected row — used by the legacy direct-call
  // entry points. Once the row is hidden synchronously after
  // confirm, `filteredIds[selectedIndex]` no longer points at the
  // session being archived (it shifts to whatever is now under
  // the cursor).
  const id = typeof explicitId === "number"
    ? explicitId
    : openDialog.filteredIds[openDialog.selectedIndex];
  // Clear the in-flight marker so the preview pane stops showing
  // "Archiving…" if the operation refuses or fails. After
  // `closeWindow` succeeds the row is gone from `listWindows()`
  // anyway, so clearing then is harmless.
  const clearInFlight = () => {
    if (
      openDialog?.inFlight && typeof id === "number" &&
      openDialog.inFlight.sessionId === id
    ) {
      openDialog.inFlight = null;
      refreshOpenDialog();
    }
  };
  if (typeof id !== "number" || id <= 0) return;
  if (id === 1) {
    setDialogError("cannot archive the base session");
    clearInFlight();
    return;
  }
  // close_window refuses to close the active window; swap to a
  // different session first. The pick prefers something already
  // in the dialog's current filter, falls back to the base
  // session — both always exist (base is undeletable, and we'd
  // have nothing to archive without at least one session).
  if (id === editor.activeWindow()) {
    editor.setActiveWindow(pickNextActiveSession(id));
  }
  const session = orchestratorSessions.get(id);
  if (!session) {
    clearInFlight();
    return;
  }

  // Resolve the repo root from cwd (the user is in the
  // umbrella session's tree).
  const cwd = editor.getCwd();
  const top = await spawnCollect(
    "git",
    ["rev-parse", "--show-toplevel"],
    cwd,
  );
  if (top.exit_code !== 0) {
    editor.setStatus("Orchestrator: archive failed — not a git repository");
    clearInFlight();
    return;
  }
  const repoRoot = (top.stdout || "").trim();

  // SIGKILL the session's process group so the pty children
  // release any locks on the worktree, then close the editor
  // session. closeWindow already kills the pty via the child
  // killer; signaling first via the window-level pg tracker
  // catches stray subprocesses outside the pty.
  editor.signalWindow(id, "SIGKILL");
  editor.closeWindow(id);

  // Brief settle so the filesystem reflects the pty's exit
  // before we move the worktree out from under it.
  await editor.delay(250);

  // git worktree move keeps git's internal bookkeeping
  // consistent (the new path stays registered as a worktree).
  const archivedRoot = editor.pathJoin(
    editor.getDataDir(),
    "orchestrator",
    slugify(repoRoot),
    ".archived",
    session.label,
  );
  const parent = editor.pathDirname(archivedRoot);
  if (!editor.createDir(parent)) {
    editor.setStatus(
      `Orchestrator: archive failed — could not create ${parent}`,
    );
    clearInFlight();
    return;
  }
  const moveRes = await spawnCollect(
    "git",
    ["-C", repoRoot, "worktree", "move", session.root, archivedRoot],
    repoRoot,
  );
  if (moveRes.exit_code !== 0) {
    editor.setStatus(
      `Orchestrator: worktree move failed: ${
        lastNonEmptyLine(moveRes.stderr) || "unknown error"
      }`,
    );
    clearInFlight();
    return;
  }

  // Append manifest entry. The branch info is best-effort:
  // we assume Orchestrator's convention of branch==label (set in
  // the new-session form) until a session knows its branch
  // separately.
  const manifest = loadArchiveManifest(repoRoot);
  manifest.sessions.push({
    label: session.label,
    root: archivedRoot,
    original_root: session.root,
    branch: session.label,
    archived_at: new Date().toISOString(),
  });
  if (!saveArchiveManifest(repoRoot, manifest)) {
    editor.setStatus(
      "Orchestrator: archived, but failed to write archived.json",
    );
  } else {
    editor.setStatus(`Orchestrator: archived [${id}] ${session.label}`);
  }
  clearInFlight();
  triggerSyncAsync(repoRoot);
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

// Delete flow: stop processes (SIGKILL), close the editor
// session, then `git worktree remove --force` to drop the
// worktree from disk. If the session was archived (manifest
// entry exists), the manifest entry is dropped too. No
// recovery after this point.
async function deleteConfirmedSession(): Promise<void> {
  if (!openDialog || !openDialog.pendingConfirm) return;
  const { sessionId: id } = openDialog.pendingConfirm;
  openDialog.pendingConfirm = null;
  // Clear the in-flight marker on early failure. Mirrors the
  // pattern in `archiveSelectedSession` — the confirm-delete
  // handler set `inFlight` before kicking off this async work,
  // and any path that aborts before `closeWindow` needs to undo
  // it so the "Deleting…" overlay disappears.
  const clearInFlight = () => {
    if (openDialog?.inFlight && openDialog.inFlight.sessionId === id) {
      openDialog.inFlight = null;
      refreshOpenDialog();
    }
  };
  const session = orchestratorSessions.get(id);
  if (!session) {
    clearInFlight();
    return;
  }
  // Same auto-switch as archive — close_window refuses to close
  // the active window, so swap to a different session first.
  if (id === editor.activeWindow()) {
    editor.setActiveWindow(pickNextActiveSession(id));
  }

  const cwd = editor.getCwd();
  const top = await spawnCollect(
    "git",
    ["rev-parse", "--show-toplevel"],
    cwd,
  );
  if (top.exit_code !== 0) {
    editor.setStatus("Orchestrator: delete failed — not a git repository");
    clearInFlight();
    return;
  }
  const repoRoot = (top.stdout || "").trim();

  editor.signalWindow(id, "SIGKILL");
  editor.closeWindow(id);
  await editor.delay(250);

  // `--force` because the worktree may have unstaged changes
  // the user explicitly chose to discard via the confirm step.
  const removeRes = await spawnCollect(
    "git",
    ["-C", repoRoot, "worktree", "remove", "--force", session.root],
    repoRoot,
  );
  if (removeRes.exit_code !== 0) {
    editor.setStatus(
      `Orchestrator: worktree remove failed: ${
        lastNonEmptyLine(removeRes.stderr) || "unknown error"
      }`,
    );
    clearInFlight();
    return;
  }

  // Drop the matching manifest entry too, in case the session
  // was already archived (delete-from-archived is the natural
  // way to drop dormant sessions).
  const manifest = loadArchiveManifest(repoRoot);
  const before = manifest.sessions.length;
  manifest.sessions = manifest.sessions.filter(
    (e) => e.label !== session.label,
  );
  if (manifest.sessions.length !== before) {
    saveArchiveManifest(repoRoot, manifest);
  }

  editor.setStatus(`Orchestrator: deleted [${id}] ${session.label}`);
  clearInFlight();
  triggerSyncAsync(repoRoot);
}

// `Alt+N` from inside the picker opens the new-session form — saves
// the user the "Esc, Ctrl+P, type Orchestrator: New Session, Enter"
// dance when they realise mid-picker that they want to spawn another
// agent. All other keys (Up/Down/Enter/Tab/Esc/printable chars)
// route through `dispatch_floating_widget_key`'s smart-key defaults
// since OPEN_MODE doesn't claim them here.
editor.defineMode(
  OPEN_MODE,
  [["M-n", "orchestrator_open_new_from_picker"]],
  true,
  true,
);

registerHandler("orchestrator_open_new_from_picker", () => {
  if (!openDialog) return;
  closeOpenDialog();
  openForm({ fromPicker: true });
});

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

async function nextAutoSessionName(
  repoRoot: string,
  options?: { persist?: boolean },
): Promise<string> {
  // Persisted counter so consecutive empty submits produce
  // session-1, session-2, … even across plugin reloads. But the
  // counter alone isn't sufficient: a previous run may have left a
  // branch / worktree behind (orchestrator's archive / external git
  // delete / interrupted submit), so `session-${counter+1}` can
  // collide and `git worktree add` would fail with the noisy
  // "already used by worktree at …" message. Probe the local git
  // refs once and increment past any reserved name before
  // returning.
  //
  // `persist: false` (the default) computes the name without
  // advancing the persisted counter — for placeholder previews
  // that happen on every Project Path keystroke. The submit
  // path passes `persist: true` so consecutive submissions
  // increment normally.
  const persist = options?.persist === true;
  const counterBefore = (editor.getGlobalState("orchestrator.session_counter") as
    | number
    | undefined) ?? 0;
  let next = counterBefore + 1;

  // Collect existing branch names that look like `session-N` so we
  // can skip past them. `git for-each-ref` is faster and tighter
  // than parsing `git worktree list` output.
  const refs = await spawnCollect(
    "git",
    ["-C", repoRoot, "for-each-ref", "--format=%(refname:short)", "refs/heads/"],
    repoRoot,
  );
  const taken = new Set<number>();
  if (refs.exit_code === 0) {
    for (const line of (refs.stdout || "").split(/\r?\n/)) {
      const m = /^session-(\d+)$/.exec(line.trim());
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
  return `session-${next}`;
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
    branchPlaceholder = worktreeEnabled
      ? "shared worktree — N/A"
      : "no git — N/A";
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
        { keys: "↑↓", label: "history" },
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
  pendingNewSession = null;
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
  form.completion.field = field;
  form.completion.items = items.slice(0, COMPLETION_MAX_ITEMS);
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
    if (item.endsWith("/")) {
      scheduleCompletionRefresh("project_path");
      return;
    }
  }
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

// Cancel path: tear down the form, and if it was reached via the
// picker (Alt+N or "+ New Session" button), reopen the picker so
// Esc behaves like a true "back" rather than dropping the user
// into the bare editor.
function cancelForm(): void {
  const wasFromPicker = !!form?.fromPicker;
  closeForm();
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

  // Branch / cmd values used for `pendingNewSession` — `branchName`
  // only exists in the worktree-create flow above; for the
  // shared-worktree / non-git case we report whatever's currently
  // checked out (best-effort) so the new session record matches
  // the situation on disk.
  const reportedBranch = createWorktree
    ? (branchInput || sessionName)
    : "";

  // Append the user-effective values to per-field input
  // history so ↑/↓ can recall them on the next form open.
  appendHistory("project_path", projectPath);
  appendHistory("name", sessionName);
  if (cmd) appendHistory("cmd", cmd);
  if (createWorktree) appendHistory("branch", reportedBranch);

  pendingNewSession = {
    label: sessionName,
    branch: reportedBranch,
    cmd,
    root,
    projectPath,
    sharedWorktree: !createWorktree,
  };
  closeForm();
  editor.createWindow(root, sessionName);
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
  // the text input". When the popup is closed, Enter on a
  // single-line Text widget advances focus to the next
  // tabbable; on a Button/Toggle it activates. The plugin's
  // local focus mirror must advance in lockstep with the
  // host on the focus-advance branch so that the next Up/Down
  // walks the *new* field's history (not the previous text
  // input's). Without this, focus appears to move (cursor
  // gone, next field highlighted) while the plugin still
  // thinks it's on the previous text input — pressing Down
  // then walks that text input's history and silently
  // rewrites its value.
  if (!completionVisibleForFocused() && focusToHistoryField(formFocusedKey())) {
    advanceFormFocus(1);
  }
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
  // When the completion popup is open the host short-circuits
  // Up/Down to its own selection navigation — dispatch
  // straight through. Otherwise walk history for the history-
  // bearing inputs; otherwise pass through.
  if (completionVisibleForFocused()) {
    dispatchFormKey("Up");
    return;
  }
  const histField = focusToHistoryField(formFocusedKey());
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
  const histField = focusToHistoryField(formFocusedKey());
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
  // Refuse Archive / Delete on a shared root while other
  // sessions still live there. Both actions either move
  // (`git worktree move`) or remove (`git worktree remove`)
  // the on-disk path — doing that under another running
  // session would yank the rug out from under it. Stop is
  // fine: it only signals THIS session's process group, no
  // disk operation.
  if (action === "archive" || action === "delete") {
    const session = orchestratorSessions.get(id);
    if (session) {
      const siblings = countSiblingsAtRoot(session.root);
      if (siblings > 1) {
        setDialogError(
          `cannot ${action} session [${id}] ${session.label} — ${siblings - 1} other session(s) share this worktree; close them first`,
        );
        refreshOpenDialog();
        return;
      }
      if (session.sharedWorktree) {
        // Single-session shared-worktree mode: there's no
        // `git worktree` entry to remove for this session.
        // Block both lifecycle actions so we don't run
        // `git worktree remove` against a non-worktree path
        // and rm-rf the user's actual project directory.
        setDialogError(
          `cannot ${action} session [${id}] ${session.label} — session shares its working tree with the project root; close it via the editor instead`,
        );
        refreshOpenDialog();
        return;
      }
    }
  }
  openDialog.pendingConfirm = { action, sessionId: id };
  openPanel.update(buildOpenSpec());
  openPanel.setFocusKey("confirm-cancel");
}

editor.on("widget_event", (e) => {
  // ---------------------------------------------------------------------
  // New-session form
  // ---------------------------------------------------------------------
  if (form && formPanel && e.panel_id === formPanel.id()) {
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
    if (e.event_type === "select" && e.widget_key === "sessions") {
      const payload = (e.payload ?? {}) as Record<string, unknown>;
      const idx = payload.index;
      if (typeof idx === "number") {
        openDialog.selectedIndex = idx;
        clearDialogError();
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
        // is already on Visit.
        openPanel.setFocusKey("visit");
      }
      return;
    }
    if (
      e.event_type === "activate" &&
      (e.widget_key === "sessions" || e.widget_key === "visit")
    ) {
      const id = openDialog.filteredIds[openDialog.selectedIndex];
      if (typeof id === "number" && id > 0 && id !== editor.activeWindow()) {
        editor.setActiveWindow(id);
      }
      closeOpenDialog();
      return;
    }
    if (e.event_type === "activate" && e.widget_key === "new-session") {
      closeOpenDialog();
      openForm({ fromPicker: true });
      return;
    }
    if (e.event_type === "activate" && e.widget_key === "toggle-details") {
      openDialog.showDetails = !openDialog.showDetails;
      refreshOpenDialog();
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
    if (e.event_type === "activate" && e.widget_key === "confirm-cancel") {
      openDialog.pendingConfirm = null;
      openPanel.update(buildOpenSpec());
      return;
    }
    if (e.event_type === "activate" && e.widget_key === "confirm-stop") {
      openDialog.pendingConfirm = null;
      stopSelectedSession();
      if (openPanel) openPanel.update(buildOpenSpec());
      return;
    }
    if (e.event_type === "activate" && e.widget_key === "confirm-archive") {
      const id = openDialog.filteredIds[openDialog.selectedIndex];
      openDialog.pendingConfirm = null;
      // Mark the session in-flight so the preview swaps to
      // "Archiving…" and its action buttons disappear until git
      // finishes. The row stays in the list — `editor.listWindows()`
      // is still the source of truth and will drop it on
      // `closeWindow`, which is intentional: a slightly-laggy real
      // state beats a synchronously faked one that can desync from
      // git reality (e.g. when `git worktree move` fails).
      if (typeof id === "number" && id > 0) {
        openDialog.inFlight = { action: "archive", sessionId: id };
      }
      void archiveSelectedSession(id);
      refreshOpenDialog();
      return;
    }
    if (e.event_type === "activate" && e.widget_key === "confirm-delete") {
      const id = openDialog.pendingConfirm?.sessionId;
      // Mark in-flight — see comment on confirm-archive above.
      // `deleteConfirmedSession` clears `pendingConfirm` itself, so
      // we capture the id here before it goes away.
      if (typeof id === "number" && id > 0) {
        openDialog.inFlight = { action: "delete", sessionId: id };
      }
      void deleteConfirmedSession();
      refreshOpenDialog();
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
  if (id === 1) {
    editor.setStatus("Orchestrator: cannot kill the base session");
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

editor.on("window_created", async (payload) => {
  const id = payload.id;
  if (
    pendingNewSession &&
    payload.label === pendingNewSession.label
  ) {
    const intent = pendingNewSession;
    pendingNewSession = null;
    // Dive into the new session FIRST so its terminal_manager is
    // the editor-active one. Subsequent `createTerminal` /
    // `sendTerminalInput` calls then resolve against the new
    // session's window without needing a cross-window terminal
    // lookup. Creating a session is a visit-now action anyway —
    // the dive isn't user-visible flicker, it's the desired
    // landing state.
    editor.setActiveWindow(id);
    // Record the new session's project_path / shared_worktree
    // into per-window plugin state — these survive editor
    // restarts via `orchestrator_persistence.rs`, and feed the
    // Open dialog's "this project" filter on the next launch.
    // `setWindowState` writes to the active window, which we
    // just set above.
    editor.setWindowState("project_path", intent.projectPath);
    editor.setWindowState("shared_worktree", intent.sharedWorktree);
    // When the user provided a non-empty agent command, spawn it as
    // the PTY child directly (no shell middleman). Tab title reads
    // the command name ("python3", "claude", ...) instead of the
    // generic "*Terminal N*". When `cmd` is empty the host picks
    // the user's shell as before.
    const argv = splitAgentCmd(intent.cmd);
    const term = await editor.createTerminal({
      cwd: intent.root,
      focus: false,
      command: argv.length > 0 ? argv : undefined,
      title: argv.length > 0 ? argv[0] : undefined,
    });
    const tracked: AgentSession = {
      id,
      label: intent.label,
      root: intent.root,
      terminalId: term.terminalId,
      state: "running",
      createdAt: Date.now(),
    };
    orchestratorSessions.set(id, tracked);
    // Legacy `sendTerminalInput` path is no longer needed when the
    // command is spawned directly. Kept for the shell-only case
    // would be `editor.sendTerminalInput(term.terminalId, "\n")` to
    // wake up the prompt, but that's unnecessary — the shell prints
    // its own prompt on startup.
  }
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
    const listVisibleRows = openListVisibleRows();
    openDialog.listVisibleRows = listVisibleRows;
    openDialog.embedRows = Math.max(3, listVisibleRows - 5);
    refreshOpenDialog();
  }
});

// =============================================================================
// Agent state inference from terminal output / exit
// =============================================================================

// Match common AI-agent prompts: "(Y/n)", "(y/N)", "Press <key>",
// or a trailing question mark followed by optional whitespace.
// Conservative — false positives mistakenly classify a busy
// agent as "awaiting", which is recoverable by next output;
// false negatives are worse (user thinks agent is busy when
// it's actually waiting), so we err on the side of detecting.
const AWAITING_RX = /(\(\s*[YyNn]\s*\/\s*[YyNn]\s*\):?\s*$)|(Press\s+(?:enter|return|any\s+key)[^\n]*$)|(\?\s*$)/i;

editor.on("terminal_output", (payload) => {
  const last = payload.last_line || "";
  for (const s of orchestratorSessions.values()) {
    if (s.terminalId === payload.terminal_id) {
      // RUNNING is the default; flip to AWAITING only when the
      // last visible line matches a prompt pattern. New output
      // that doesn't match restores RUNNING — agents usually
      // print their next chunk over the prompt line, so this
      // gives the right transition even for chatty agents.
      s.state = AWAITING_RX.test(last) ? "awaiting" : "running";
      break;
    }
  }
  refreshOpenDialog();
});

editor.on("terminal_exit", (payload) => {
  for (const s of orchestratorSessions.values()) {
    if (s.terminalId === payload.terminal_id) {
      const code = payload.exit_code;
      // exit_code is currently always null (the editor's
      // wait-status capture is a follow-up). Treat unknown as
      // ready — Orchestrator doesn't have a better heuristic and
      // mis-marking a real error as "ready" is recoverable
      // (the user opens the dive and sees the failure).
      s.state = code === null || code === 0 ? "ready" : "errored";
      break;
    }
  }
  refreshOpenDialog();
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
