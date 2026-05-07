/// <reference path="./lib/fresh.d.ts" />
//
// Conductor — multi-agent / multi-worktree session orchestration.
//
// MVP scope (`docs/internal/conductor-sessions-design.md`):
//
//   - "Conductor: Open" command opens the Control Room as a
//     read-only virtual buffer in the utility dock.
//   - "Conductor: New Session" prompts for a branch name (root
//     path) and an agent command, creates an editor session +
//     terminal, and re-renders.
//   - Up / Down / Ctrl+n / Ctrl+p to navigate, Enter to dive
//     (setActiveSession on selected row), Esc to close.
//   - The Control Room auto-renders on session_created,
//     active_session_changed, session_closed.
//
// Deferred (v1.1+ — see § MVP scope in the design doc):
//
//   - Agent state inference from terminal_output/terminal_exit
//     (RUNNING / AWAITING / READY / ERRORED). Plugin currently
//     shows "—" in the STATE column until those events trigger
//     the still-empty `stateMachine.observe`.
//   - Diff via existing review-diff feature (`d` action).
//   - Merge / kill / rename actions.
//   - Collision radar (depends on watchPath).
//   - Mouse interactions.

const editor = getEditor();

// =============================================================================
// Types
// =============================================================================

type AgentState = "active" | "running" | "awaiting" | "ready" | "errored" | "killed";

interface AgentSession {
  // The editor's stable session id.
  id: number;
  // Worktree branch / display label, also passed to createSession.
  label: string;
  // Absolute filesystem root.
  root: string;
  // The terminal id Conductor spawned in this session, if any.
  terminalId: number | null;
  // Last parsed agent state. "active" applies to the editor's
  // current active session and is computed at render time, not
  // stored.
  state: AgentState;
  // Wall-clock ms when conductor.new fired createSession.
  createdAt: number;
}

interface ControlRoom {
  bufferId: number;
  splitId: number;
  selectedIndex: number;
}

// =============================================================================
// Module state — editor-global, survives every dive (`§ The "anchored
// above" picture` in the design doc).
// =============================================================================

// Map<sessionId, AgentSession>. Synced with editor.listSessions on
// every render so externally-created sessions still render.
const conductorSessions = new Map<number, AgentSession>();
let controlRoom: ControlRoom | null = null;

// Two-step "New Session" prompt: store the branch from step 1 so
// step 2's confirm handler can read it.
let pendingBranchName: string | null = null;

// =============================================================================
// Session list reconciliation
// =============================================================================

/**
 * Pull the canonical session list from the editor and reconcile
 * `conductorSessions` with it. Sessions the editor knows about that
 * Conductor never tracked (e.g. the always-present base session)
 * gain an empty AgentSession entry; sessions the editor has dropped
 * since last render are removed from the map.
 */
function reconcileSessions(): void {
  const editorSessions = editor.listSessions();
  const seen = new Set<number>();
  for (const s of editorSessions) {
    seen.add(s.id);
    const existing = conductorSessions.get(s.id);
    if (!existing) {
      conductorSessions.set(s.id, {
        id: s.id,
        label: s.label,
        root: s.root,
        terminalId: null,
        state: s.id === 1 ? "active" : "running",
        createdAt: Date.now(),
      });
    } else {
      // Editor is the source of truth for label/root.
      existing.label = s.label;
      existing.root = s.root;
    }
  }
  // Drop entries the editor has closed.
  for (const id of conductorSessions.keys()) {
    if (!seen.has(id)) conductorSessions.delete(id);
  }
}

// =============================================================================
// Control Room rendering
// =============================================================================

const COL_WIDTH = {
  id: 4,
  label: 22,
  root: 36,
  agent: 14,
  state: 16,
  age: 6,
};

function pad(s: string, w: number): string {
  if (s.length >= w) return s.slice(0, Math.max(0, w - 1)) + " ";
  return s + " ".repeat(w - s.length);
}

function ageString(createdAt: number): string {
  const sec = Math.max(0, Math.floor((Date.now() - createdAt) / 1000));
  if (sec < 60) return `${sec}s`;
  if (sec < 3600) return `${Math.floor(sec / 60)}m`;
  return `${Math.floor(sec / 3600)}h`;
}

function stateLabel(s: AgentSession, isActive: boolean): string {
  if (isActive) return "ACTIVE";
  switch (s.state) {
    case "running":
      return "RUNNING";
    case "awaiting":
      return "AWAITING (Y/n)";
    case "ready":
      return "READY";
    case "errored":
      return "ERRORED";
    case "killed":
      return "KILLED";
    default:
      return "—";
  }
}

function buildHeader(count: number): TextPropertyEntry[] {
  return [
    {
      text:
        `═ Conductor ═ ${count} session${count === 1 ? "" : "s"} ` +
        `═══════════════════════════════════════════════════════════\n`,
    },
    {
      text:
        pad("#", COL_WIDTH.id) +
        pad("LABEL", COL_WIDTH.label) +
        pad("ROOT", COL_WIDTH.root) +
        pad("AGENT", COL_WIDTH.agent) +
        pad("STATE", COL_WIDTH.state) +
        pad("AGE", COL_WIDTH.age) +
        "\n",
    },
    {
      text:
        "─".repeat(
          COL_WIDTH.id +
            COL_WIDTH.label +
            COL_WIDTH.root +
            COL_WIDTH.agent +
            COL_WIDTH.state +
            COL_WIDTH.age,
        ) + "\n",
    },
  ];
}

function buildFooter(): TextPropertyEntry {
  return {
    text:
      "\n" +
      "Enter:dive  n:new  k:close  Ctrl+n/p:cycle  Esc:close\n",
  };
}

function buildEntries(): TextPropertyEntry[] {
  const sortedIds = Array.from(conductorSessions.keys()).sort(
    (a, b) => a - b,
  );
  const entries: TextPropertyEntry[] = buildHeader(sortedIds.length);

  const activeId = editor.activeSession();
  for (let i = 0; i < sortedIds.length; i++) {
    const id = sortedIds[i];
    const s = conductorSessions.get(id)!;
    const isActive = id === activeId;
    const selected =
      controlRoom !== null && i === controlRoom.selectedIndex;
    const marker = selected ? ">" : " ";

    entries.push({
      text:
        marker +
        pad(String(id), COL_WIDTH.id - 1) +
        pad(s.label, COL_WIDTH.label) +
        pad(s.root, COL_WIDTH.root) +
        pad(s.terminalId === null ? "—" : "agent", COL_WIDTH.agent) +
        pad(stateLabel(s, isActive), COL_WIDTH.state) +
        pad(ageString(s.createdAt), COL_WIDTH.age) +
        "\n",
    });
  }

  if (sortedIds.length === 0) {
    entries.push({
      text:
        "  No active sessions.\n" +
        "  Press n to spawn the first one.\n",
    });
  }

  entries.push(buildFooter());
  return entries;
}

function rerenderControlRoom(): void {
  if (!controlRoom) return;
  reconcileSessions();
  // Clamp selection in case sessions were closed.
  const total = conductorSessions.size;
  if (total === 0) {
    controlRoom.selectedIndex = 0;
  } else if (controlRoom.selectedIndex >= total) {
    controlRoom.selectedIndex = total - 1;
  }
  editor.setVirtualBufferContent(
    controlRoom.bufferId,
    buildEntries(),
  );
}

// =============================================================================
// Open / close
// =============================================================================

async function openControlRoom(): Promise<void> {
  if (controlRoom) {
    // Already open — refocus the dock split and re-target it
    // at the Conductor buffer, in case other utilities (terminals,
    // diagnostics) have taken over the active tab in the meantime.
    editor.setSplitBuffer(controlRoom.splitId, controlRoom.bufferId);
    editor.focusSplit(controlRoom.splitId);
    rerenderControlRoom();
    return;
  }
  reconcileSessions();
  try {
    const result = await editor.createVirtualBufferInSplit({
      name: "*Conductor*",
      mode: "conductor-room",
      readOnly: true,
      role: "utility_dock",
      ratio: 0.6,
      panelId: "conductor-control-room",
      showLineNumbers: false,
      showCursors: false,
      editingDisabled: true,
      entries: buildEntries(),
    });
    controlRoom = {
      bufferId: result.bufferId,
      splitId: result.splitId ?? editor.getActiveSplitId(),
      selectedIndex: 0,
    };
    rerenderControlRoom();
  } catch (e) {
    editor.setStatus(
      "Conductor: failed to open control room: " +
        (e instanceof Error ? e.message : String(e)),
    );
  }
}

function closeControlRoom(): void {
  if (!controlRoom) return;
  editor.closeSplit(controlRoom.splitId);
  controlRoom = null;
}

// =============================================================================
// Selection / dive
// =============================================================================

function moveSelection(delta: number): void {
  if (!controlRoom) return;
  const total = conductorSessions.size;
  if (total === 0) return;
  const next = (controlRoom.selectedIndex + delta + total) % total;
  controlRoom.selectedIndex = next;
  rerenderControlRoom();
}

function selectedSessionId(): number | null {
  if (!controlRoom) return null;
  const sortedIds = Array.from(conductorSessions.keys()).sort(
    (a, b) => a - b,
  );
  if (sortedIds.length === 0) return null;
  return sortedIds[
    Math.max(0, Math.min(controlRoom.selectedIndex, sortedIds.length - 1))
  ];
}

function dive(): void {
  const id = selectedSessionId();
  if (id === null) return;
  if (id === editor.activeSession()) {
    closeControlRoom();
    return;
  }
  editor.setActiveSession(id);
  closeControlRoom();
}

function killSelected(): void {
  const id = selectedSessionId();
  if (id === null || id === 1) return;
  if (id === editor.activeSession()) {
    editor.setStatus(
      "Conductor: switch to another session before closing this one",
    );
    return;
  }
  const s = conductorSessions.get(id);
  if (s && s.terminalId !== null) {
    editor.closeTerminal(s.terminalId);
  }
  editor.closeSession(id);
}

// =============================================================================
// New session — two-step prompt flow
// =============================================================================

function startNewSession(): void {
  pendingBranchName = null;
  editor.startPrompt(
    "New session — branch / worktree name:",
    "conductor-new-branch",
  );
}

editor.on("prompt_confirmed", async (data) => {
  if (data.prompt_type === "conductor-new-branch") {
    const name = (data.input || "").trim();
    if (!name) return;
    pendingBranchName = name;
    editor.startPrompt(
      "Agent command (e.g. 'aider', 'claude -p \"...\"'):",
      "conductor-new-cmd",
    );
    return;
  }
  if (data.prompt_type === "conductor-new-cmd") {
    const cmd = (data.input || "").trim();
    const branch = pendingBranchName;
    pendingBranchName = null;
    if (!branch || !cmd) return;

    // For MVP, the worktree root is just <cwd>/.fresh/conductor/<branch>.
    // The doc's `conductor.worktree_root` setting and the actual
    // `git worktree add` are deferred — this version creates a
    // plain directory and runs the agent in it. Plugin-side wiring
    // is the same shape as the eventual git-worktree path, so
    // nothing else changes when the git side lands.
    const cwd = editor.getCwd();
    const root = editor.pathJoin(cwd, ".fresh", "conductor", branch);
    try {
      await editor.spawnProcess("mkdir", ["-p", root], cwd);
    } catch {
      // best-effort; createTerminal will surface failures
    }

    // Pre-record so the session_created hook handler can match.
    pendingNewSession = { branch, cmd, root };
    editor.createSession(root, branch);
  }
});

editor.on("prompt_cancelled", (data) => {
  if (
    data.prompt_type === "conductor-new-branch" ||
    data.prompt_type === "conductor-new-cmd"
  ) {
    pendingBranchName = null;
    pendingNewSession = null;
  }
});

// We want to spawn a terminal in the new session, but createSession
// is fire-and-forget — the session id isn't returned synchronously.
// Stash the new-session intent and complete the spawn in the
// session_created hook, matching by label.
let pendingNewSession:
  | { branch: string; cmd: string; root: string }
  | null = null;

editor.on("session_created", async (payload) => {
  if (!payload) return;
  const id = payload.id as number;
  if (
    pendingNewSession &&
    payload.label === pendingNewSession.branch
  ) {
    const intent = pendingNewSession;
    pendingNewSession = null;

    const term = await editor.createTerminal({
      cwd: intent.root,
      focus: false,
    });

    const tracked: AgentSession = {
      id,
      label: intent.branch,
      root: intent.root,
      terminalId: term.terminalId,
      state: "running",
      createdAt: Date.now(),
    };
    conductorSessions.set(id, tracked);

    // Send the agent command + Enter to the terminal.
    editor.sendTerminalInput(term.terminalId, intent.cmd + "\n");
  }

  rerenderControlRoom();
});

editor.on("session_closed", () => {
  rerenderControlRoom();
});

editor.on("active_session_changed", () => {
  rerenderControlRoom();
});

editor.on("terminal_exit", (payload) => {
  if (!payload) return;
  for (const s of conductorSessions.values()) {
    if (s.terminalId === payload.terminal_id) {
      s.state = payload.exit_code === 0 ? "ready" : "errored";
    }
  }
  rerenderControlRoom();
});

// =============================================================================
// Mode + commands + keybindings
// =============================================================================

editor.defineMode(
  "conductor-room",
  [
    ["Up", "conductor_prev"],
    ["Down", "conductor_next"],
    ["Ctrl+n", "conductor_next"],
    ["Ctrl+p", "conductor_prev"],
    ["Enter", "conductor_dive"],
    ["n", "conductor_new"],
    ["k", "conductor_kill"],
    ["Escape", "conductor_close"],
  ],
  /* readOnly */ true,
  /* allowTextInput */ false,
);

// `registerHandler` is the documented shim that exposes a function
// at the given global name so `editor.registerCommand(...,
// "<handler>")` can find it. See lib/fresh.d.ts.
registerHandler("conductor_open", openControlRoom);
registerHandler("conductor_close", closeControlRoom);
registerHandler("conductor_next", () => moveSelection(1));
registerHandler("conductor_prev", () => moveSelection(-1));
registerHandler("conductor_dive", dive);
registerHandler("conductor_new", startNewSession);
registerHandler("conductor_kill", killSelected);

editor.registerCommand(
  "Conductor: Open",
  "Show all editor sessions in the Control Room",
  "conductor_open",
  null,
);
editor.registerCommand(
  "Conductor: New Session",
  "Spawn a new editor session in a worktree",
  "conductor_new",
  null,
);
editor.registerCommand(
  "Conductor: Close",
  "Close the Control Room",
  "conductor_close",
  "conductor-room",
);
editor.registerCommand(
  "Conductor: Dive",
  "Switch to the selected session",
  "conductor_dive",
  "conductor-room",
);
editor.registerCommand(
  "Conductor: Next",
  "Select the next session",
  "conductor_next",
  "conductor-room",
);
editor.registerCommand(
  "Conductor: Previous",
  "Select the previous session",
  "conductor_prev",
  "conductor-room",
);
editor.registerCommand(
  "Conductor: Kill Session",
  "Close the selected session",
  "conductor_kill",
  "conductor-room",
);
