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
  spacer,
  styledRow,
  text,
  textInputChar,
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
  | { label: string; branch: string; cmd: string; root: string }
  | null = null;

// New-session form state. `null` ⇒ the floating form isn't
// open. Each field's `value` + `cursor` mirrors what the host
// renders inside the panel's TextInput widgets; the `submitting`
// flag debounces double-Enter on the Create button; `lastError`
// is rendered as a styled error row inside the form when the
// most recent submit failed (status bar would get clobbered —
// see MEMORY.md).
interface NewSessionForm {
  name: { value: string; cursor: number };
  cmd: { value: string; cursor: number };
  branch: { value: string; cursor: number };
  submitting: boolean;
  lastError: string | null;
  // Display-only "my_org/project_name" rendered in the
  // dialog's subtitle. Computed once at openForm time from the
  // current cwd so we don't subprocess on every render.
  projectLabel: string;
  // Resolved default branch (e.g. "origin/main"). Empty while
  // the async `git fetch + symbolic-ref` probe is in flight;
  // the branch input's placeholder reads this so the user sees
  // the exact base ref the worktree will fork off if they
  // leave the field blank.
  defaultBranch: string;
  // Previously-submitted Agent Command (persisted across editor
  // sessions via `orchestrator.last_cmd`). Rendered as the cmd
  // field's *placeholder* — pre-filling the value would survive
  // a failed submit and silently feed stale text into the next
  // attempt; the placeholder gives the same memory aid without
  // that hazard.
  lastCmd: string;
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
  pendingConfirm: { action: "delete"; sessionId: number } | null;
  // Rows the embed reserves and rows the sessions list shows.
  // Captured once at dialog-open from the editor's viewport so
  // the layout stays constant across re-renders — recomputing
  // mid-dialog would let the size jitter when the active
  // window's viewport changes (e.g. terminal buffer's shorter
  // height vs. a file buffer's).
  listVisibleRows: number;
  embedRows: number;
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
  const stateText = isActive ? "ACT " : STATE_GLYPH[s.state];
  return styledRow([
    { text: `[${id}] `, style: { fg: "ui.help_key_fg" } },
    {
      text: stateText,
      style: isActive
        ? { fg: "ui.tab_active_fg", bold: true }
        : { fg: "ui.menu_disabled_fg" },
    },
    { text: `  ${s.label}` },
  ]);
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
  const stateText = isActive ? "ACT" : STATE_GLYPH[s.state].trim();
  return [
    styledRow([
      {
        text: stateText,
        style: isActive
          ? { fg: "ui.tab_active_fg", bold: true }
          : { fg: "ui.menu_disabled_fg" },
      },
      { text: "  " },
      { text: ageString(s.createdAt), style: { fg: "ui.menu_disabled_fg" } },
    ]),
    styledRow([
      { text: s.root, style: { fg: "ui.menu_disabled_fg" } },
    ]),
  ];
}

// Approximate number of session rows the picker's list pane
// should show. Derived from the active buffer's viewport so the
// picker's row(list, preview) fills the panel and the hint bar
// sits flush at the panel's last row. Conservative — leaves
// room for header, filter input, footer, and section borders.
function openListVisibleRows(): number {
  const vp = editor.getViewport();
  const h = vp ? vp.height : 30;
  const panelH = Math.floor(h * 0.9);
  // header (1) + spacer (1) + filter section (3) + sessions
  // section borders (2) + hint bar (1) = 8 rows of chrome.
  // Floor at 4 so a tiny terminal still shows something.
  return Math.max(4, panelH - 8);
}

// Compose the right-hand preview pane. Normally it shows info
// + action buttons (Stop, Archive, Delete); when a destructive
// action is pending confirmation it swaps to a "Confirm
// <action>?" panel with [ Confirm <action> ] / [ Cancel ]
// buttons. Cancel is default-focused for safety.
function buildPreviewPane(s: AgentSession | undefined): WidgetSpec {
  if (openDialog?.pendingConfirm && s && openDialog.pendingConfirm.sessionId === s.id) {
    const action = openDialog.pendingConfirm.action;
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
  // The embed reserves the bulk of the preview pane so the host
  // can paint the selected window's live UI (splits, terminals,
  // syntax highlighting) underneath the action button row. The
  // row count is captured at dialog-open into `openDialog` and
  // used verbatim across re-renders, so the preview pane stays
  // a constant height regardless of which window's viewport the
  // host happens to be reporting through `getViewport()`.
  const embedRows = openDialog?.embedRows ?? 4;
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
  return labeledSection({
    label: `[${s.id}] ${s.label}`,
    child: col(
      row(
        flexSpacer(),
        button("Stop", { key: "stop" }),
        spacer(2),
        button("Archive", { key: "archive" }),
        spacer(2),
        button("Delete", { intent: "danger", key: "delete" }),
      ),
      spacer(0),
      { kind: "raw", entries: buildPreviewEntries(s) },
      spacer(0),
      // Live window render of the highlighted session's split tree.
      windowEmbed({
        windowId: s.id,
        rows: embedRows,
        key: "live-preview",
      }),
    ),
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
    spacer(0),
    labeledSection({
      label: "Filter",
      child: text({
        value: openDialog.filter.value,
        cursorByte: openDialog.filter.cursor,
        placeholder: "type to filter…",
        fullWidth: true,
        key: "filter",
      }),
    }),
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
        child: list({
          items,
          itemKeys,
          selectedIndex: selIdx,
          visibleRows: openDialog.listVisibleRows,
          // Excluded from the Tab cycle — Up/Down on the
          // filter input forwards to this list via host
          // smart-keys, so Tab jumps straight to the action
          // buttons instead of stopping here.
          focusable: false,
          key: "sessions",
        }),
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
        { keys: "Alt+N", label: "new" },
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
  const ids = Array.from(orchestratorSessions.keys()).sort((a, b) => a - b);
  const activeIdx = ids.indexOf(activeId);
  const listVisibleRows = openListVisibleRows();
  openDialog = {
    filter: { value: "", cursor: 0 },
    filteredIds: ids,
    selectedIndex: activeIdx >= 0 ? activeIdx : 0,
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
  };
  openPanel = new FloatingWidgetPanel();
  // 90% × 90% of the terminal — the open dialog wants room for
  // a real session list + preview pane, unlike the new-session
  // form which stays compact.
  openPanel.mount(buildOpenSpec(), { widthPct: 90, heightPct: 90 });
  if (openDialog.filteredIds.length > 0) {
    openPanel.setSelectedIndex("sessions", openDialog.selectedIndex);
  }
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
    editor.setStatus("Orchestrator: cannot stop the base session");
    return;
  }
  editor.signalWindow(id, "SIGTERM");
  // SIGKILL fallback for agents that ignore SIGTERM. The
  // host's signalWindow is idempotent on already-exited
  // process groups, so the second call is safe whether or
  // not the first one took.
  setTimeout(() => {
    editor.signalWindow(id, "SIGKILL");
  }, 2000);
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

// Archive flow: stop all processes (SIGKILL — archive is a
// "I'm done with this for now" action, no graceful teardown
// needed since the worktree stays on disk), close the editor
// session, move the worktree to the `.archived/` graveyard,
// and append a manifest entry so a future Unarchive flow can
// reverse it.
async function archiveSelectedSession(): Promise<void> {
  if (!openDialog) return;
  const id = openDialog.filteredIds[openDialog.selectedIndex];
  if (typeof id !== "number" || id <= 0) return;
  if (id === 1) {
    editor.setStatus("Orchestrator: cannot archive the base session");
    return;
  }
  if (id === editor.activeWindow()) {
    editor.setStatus(
      "Orchestrator: dive elsewhere first, then archive this session",
    );
    return;
  }
  const session = orchestratorSessions.get(id);
  if (!session) return;

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
  await new Promise((r) => setTimeout(r, 250));

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
  const session = orchestratorSessions.get(id);
  if (!session) {
    if (openPanel) openPanel.update(buildOpenSpec());
    return;
  }
  if (id === editor.activeWindow()) {
    editor.setStatus(
      "Orchestrator: dive elsewhere first, then delete this session",
    );
    if (openPanel) openPanel.update(buildOpenSpec());
    return;
  }

  const cwd = editor.getCwd();
  const top = await spawnCollect(
    "git",
    ["rev-parse", "--show-toplevel"],
    cwd,
  );
  if (top.exit_code !== 0) {
    editor.setStatus("Orchestrator: delete failed — not a git repository");
    if (openPanel) openPanel.update(buildOpenSpec());
    return;
  }
  const repoRoot = (top.stdout || "").trim();

  editor.signalWindow(id, "SIGKILL");
  editor.closeWindow(id);
  await new Promise((r) => setTimeout(r, 250));

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
    if (openPanel) openPanel.update(buildOpenSpec());
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
  if (openPanel) openPanel.update(buildOpenSpec());
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
  openForm();
});

// =============================================================================
// New-session floating form
// =============================================================================

function slugify(p: string): string {
  // Drop any leading separator so the slug isn't anchored to the
  // filesystem root; replace remaining separators with underscores.
  return p.replace(/^[\\\/]+/, "").replace(/[\\\/]+/g, "_");
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
      return trimmed.slice(prefix.length);
    }
  }
  return "HEAD";
}

async function nextAutoSessionName(repoRoot: string): Promise<string> {
  // Persisted counter so consecutive empty submits produce
  // session-1, session-2, … even across plugin reloads. But the
  // counter alone isn't sufficient: a previous run may have left a
  // branch / worktree behind (orchestrator's archive / external git
  // delete / interrupted submit), so `session-${counter+1}` can
  // collide and `git worktree add` would fail with the noisy
  // "already used by worktree at …" message. Probe the local git
  // refs once and increment past any reserved name before
  // returning.
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
  editor.setGlobalState("orchestrator.session_counter", next);
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
  const children: WidgetSpec[] = [
    // === Header: title flanked by separators, centered. ==========
    row(
      flexSpacer(),
      {
        kind: "raw",
        entries: [
          styledRow([
            { text: "ORCHESTRATOR", style: HEADER_KEYWORD_STYLE },
            { text: " :: ", style: HEADER_SEP_STYLE },
            { text: "New Session Dialog", style: HEADER_LABEL_STYLE },
            { text: " :: ", style: HEADER_SEP_STYLE },
            { text: "Review Synthesized", style: HEADER_LABEL_STYLE },
          ]),
        ],
      },
      flexSpacer(),
    ),
    // === Subtitle: centered project identifier. ==================
    row(
      flexSpacer(),
      {
        kind: "raw",
        entries: [
          styledRow([
            { text: "Project: ", style: SUBTITLE_LABEL_STYLE },
            { text: form.projectLabel, style: SUBTITLE_VALUE_STYLE },
          ]),
        ],
      },
      flexSpacer(),
    ),
    spacer(0),
    // === Form body: three labeled, full-width inputs. ============
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
        placeholder: "(auto-generated)",
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
        // Show the literal base ref the empty submission will
        // fork off (e.g. `origin/main`). While the probe runs
        // we still print a hint so the field isn't blank.
        placeholder: form.defaultBranch || "detecting default branch…",
        fullWidth: true,
        key: "branch",
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
        { keys: "Tab", label: "next" },
        { keys: "S-Tab", label: "prev" },
        { keys: "Enter", label: "submit" },
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
  formPanel.update(buildFormSpec());
}

function openForm(): void {
  pendingNewSession = null;
  const lastCmd =
    (editor.getGlobalState("orchestrator.last_cmd") as string | undefined) ?? "";
  form = {
    name: { value: "", cursor: 0 },
    // Empty value — `lastCmd` shows as the placeholder instead so
    // an empty submit falls back to the host's default shell
    // ("terminal") rather than silently re-running the previous
    // agent. See `NewSessionForm.lastCmd`.
    cmd: { value: "", cursor: 0 },
    branch: { value: "", cursor: 0 },
    submitting: false,
    lastError: null,
    projectLabel: deriveProjectLabel(),
    defaultBranch: "",
    lastCmd,
  };
  formPanel = new FloatingWidgetPanel();
  formPanel.mount(buildFormSpec(), { widthPct: 60, heightPct: 50 });
  editor.setEditorMode(NEW_SESSION_MODE);

  // Probe origin's default branch in the background and update
  // the branch field's placeholder once we know it. The dialog
  // is interactive immediately — the probe just refines the hint
  // from "(detecting…)" to the concrete ref name.
  void (async () => {
    const cwd = editor.getCwd();
    const top = await spawnCollect(
      "git",
      ["rev-parse", "--show-toplevel"],
      cwd,
    );
    if (top.exit_code !== 0 || !form) return;
    const repoRoot = (top.stdout || "").trim();
    const branch = await detectDefaultBranch(repoRoot);
    if (!form) return;
    form.defaultBranch = branch;
    renderForm();
  })();
}

function closeForm(): void {
  if (formPanel) {
    formPanel.unmount();
    formPanel = null;
  }
  form = null;
  editor.setEditorMode(null);
}

async function submitForm(): Promise<void> {
  if (!form || form.submitting) return;
  form.submitting = true;
  form.lastError = null;
  renderForm();

  const cmd = form.cmd.value.trim();
  const branchInput = form.branch.value.trim();

  const cwd = editor.getCwd();
  const top = await spawnCollect("git", ["rev-parse", "--show-toplevel"], cwd);
  if (top.exit_code !== 0) {
    if (!form) return;
    form.submitting = false;
    form.lastError = lastNonEmptyLine(top.stderr) || "not a git repository";
    editor.setStatus(`Orchestrator: ${form.lastError}`);
    renderForm();
    return;
  }
  const currentToplevel = (top.stdout || "").trim();

  // Resolve to the *main* worktree's root, not the current
  // worktree. When the user runs `Orchestrator: New` from inside
  // an existing orchestrator session (whose cwd is a linked
  // worktree under `<XDG>/orchestrator/<slug>/<session>`), the
  // current `--show-toplevel` is that worktree's root. Using it
  // as the slug source would produce
  // `<XDG>/orchestrator/<slug>/<slug-of-slug>/<session>` — paths
  // that nest one level deeper each time the user creates a
  // session from inside a session, eventually blowing past the
  // filesystem's path-name limits (the WARN/ERROR logs about
  // `File name too long (os error 36)`).
  //
  // `git rev-parse --path-format=absolute --git-common-dir`
  // returns the absolute path of the shared `.git` directory —
  // for the main worktree this is `<main>/.git`, for a linked
  // worktree this is the *same* `<main>/.git`. The parent of
  // that is the main worktree's root regardless of which worktree
  // we're in. The fallback to `currentToplevel` is just defensive
  // for unusual layouts (git versions older than 2.13 didn't
  // support `--path-format=absolute`, but plugin runs under
  // recent git so this is mostly belt-and-suspenders).
  const gitCommon = await spawnCollect(
    "git",
    ["rev-parse", "--path-format=absolute", "--git-common-dir"],
    currentToplevel,
  );
  const repoRoot = gitCommon.exit_code === 0
    ? editor.pathDirname((gitCommon.stdout || "").trim()) || currentToplevel
    : currentToplevel;

  // Name resolution: explicit value wins. Otherwise auto-generate
  // by scanning `refs/heads/session-N` for the next free index —
  // the counter alone can collide with branches a previous run
  // left behind. Async because the probe spawns git; placed after
  // `rev-parse` so we know we're in a git repo first.
  const sessionName = form.name.value.trim() || (await nextAutoSessionName(repoRoot));

  const root = editor.pathJoin(
    editor.getDataDir(),
    "orchestrator",
    slugify(repoRoot),
    sessionName,
  );
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
  // Try `-b <new>` first; if it fails because the branch already
  // exists, fall back to checking out the existing branch into a
  // new worktree.
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
      // Prefer the fallback's stderr: when both attempts fail, the
      // `-b` branch's error is usually "branch already exists" (which
      // is *why* we tried the fallback in the first place), and the
      // fallback's error is the more progressed / informative one.
      // Fall back to `addRes.stderr` only when the fallback didn't
      // produce its own line.
      form.lastError = lastNonEmptyLine(fallback.stderr) ||
        lastNonEmptyLine(addRes.stderr) ||
        "git worktree add failed";
      // Mirror to the status bar so the error survives if the user
      // dismisses the dialog before reading it. `form.lastError`
      // still drives the in-dialog "Error: …" row.
      editor.setStatus(`Orchestrator: ${form.lastError}`);
      renderForm();
      return;
    }
    addRes = fallback;
  }

  if (cmd) {
    editor.setGlobalState("orchestrator.last_cmd", cmd);
  }

  pendingNewSession = { label: sessionName, branch: branchName, cmd, root };
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
const FORM_MODE_BINDINGS: [string, string][] = [
  ["Tab", "orchestrator_form_key_tab"],
  ["S-Tab", "orchestrator_form_key_shift_tab"],
  ["Return", "orchestrator_form_key_enter"],
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

registerHandler("orchestrator_form_key_tab", () => dispatchFormKey("Tab"));
registerHandler(
  "orchestrator_form_key_shift_tab",
  () => dispatchFormKey("Shift+Tab"),
);
registerHandler("orchestrator_form_key_enter", () => {
  // The hint bar promises "Enter submit". The host's floating-panel
  // input dispatcher (input.rs:`dispatch_floating_widget_key`)
  // defers to plugin-defined mode bindings when present, so the
  // smart-key router's "Enter = advance focus" default doesn't fire
  // for the orchestrator-new-form mode — this handler does. From
  // anywhere in the form, Enter submits; the form's existing
  // `Esc → closeForm` keeps the cancel path unambiguous.
  if (form) {
    void submitForm();
    return;
  }
  dispatchFormKey("Enter");
});
registerHandler("orchestrator_form_key_escape", () => {
  if (form) closeForm();
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
registerHandler("orchestrator_form_key_up", () => dispatchFormKey("Up"));
registerHandler("orchestrator_form_key_down", () => dispatchFormKey("Down"));

// Printable input arrives via the global `mode_text_input` action.
// Other plugins may also register a `mode_text_input` handler;
// guard on `form` so this handler is a no-op outside the form.
function orchestrator_mode_text_input(args: { text: string }): void {
  if (!form || !formPanel || !args?.text) return;
  formPanel.command(textInputChar(args.text));
}
registerHandler("mode_text_input", orchestrator_mode_text_input);

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
      const slot = field === "name"
        ? form.name
        : field === "cmd"
        ? form.cmd
        : field === "branch"
        ? form.branch
        : null;
      if (slot) {
        slot.value = value;
        if (typeof cursor === "number") slot.cursor = cursor;
      }
      return;
    }
    if (e.event_type === "activate") {
      if (e.widget_key === "create") {
        void submitForm();
      } else if (e.widget_key === "cancel") {
        closeForm();
      }
      return;
    }
    if (e.event_type === "cancel") {
      // Host fires this when Esc unmounts the floating panel —
      // clean up our own state to match.
      form = null;
      formPanel = null;
      editor.setEditorMode(null);
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
        // Update preview pane.
        openPanel.update(buildOpenSpec());
        // Re-pin the list selection so the spec re-emit doesn't
        // snap it back to 0.
        openPanel.setSelectedIndex("sessions", openDialog.selectedIndex);
      }
      return;
    }
    if (e.event_type === "activate" && e.widget_key === "sessions") {
      const id = openDialog.filteredIds[openDialog.selectedIndex];
      if (typeof id === "number" && id > 0 && id !== editor.activeWindow()) {
        editor.setActiveWindow(id);
      }
      closeOpenDialog();
      return;
    }
    if (e.event_type === "activate" && e.widget_key === "stop") {
      stopSelectedSession();
      return;
    }
    if (e.event_type === "activate" && e.widget_key === "archive") {
      void archiveSelectedSession();
      return;
    }
    if (e.event_type === "activate" && e.widget_key === "delete") {
      const id = openDialog.filteredIds[openDialog.selectedIndex];
      if (typeof id === "number" && id > 0) {
        openDialog.pendingConfirm = { action: "delete", sessionId: id };
        openPanel.update(buildOpenSpec());
      }
      return;
    }
    if (e.event_type === "activate" && e.widget_key === "confirm-cancel") {
      openDialog.pendingConfirm = null;
      openPanel.update(buildOpenSpec());
      return;
    }
    if (e.event_type === "activate" && e.widget_key === "confirm-delete") {
      void deleteConfirmedSession();
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

editor.registerCommand(
  "Orchestrator: Open",
  "Show all editor sessions in a floating selector",
  "orchestrator_open",
  null,
);
editor.registerCommand(
  "Orchestrator: New Session",
  "Spawn a new editor session in a worktree",
  "orchestrator_new",
  null,
);
editor.registerCommand(
  "Orchestrator: Kill Selected",
  "Close the session highlighted in the open Orchestrator prompt",
  "orchestrator_kill",
  null,
);
