/// <reference path="./lib/fresh.d.ts" />
//
// Conductor — multi-agent / multi-worktree session orchestration.
//
// MVP scope (`docs/internal/conductor-sessions-design.md`):
//
//   - "Conductor: Open" opens a floating overlay prompt (same
//     UX shape as Live Grep) listing every session with its
//     state column. Up/Down navigates, Enter dives into the
//     selected session.
//   - "Conductor: New Session" prompts for branch name + agent
//     command, allocates a worktree-rooted session and spawns
//     the agent in a terminal attached to it.
//   - "Conductor: Kill Selected" closes the session whose row is
//     currently highlighted in the open prompt.
//   - Agent state column updates from terminal_output regex and
//     terminal_exit code: RUNNING / AWAITING / READY / ERRORED.
//
// Why a floating prompt rather than a utility-dock panel:
// floating overlays don't mutate the split tree, so opening
// Conductor (and closing it) leaves the user's editor layout
// undisturbed. The previous utility-dock implementation stuck
// to the dock and clashed with reopen flows. This shape mirrors
// the existing Live Grep / Quick Open patterns.

const editor = getEditor();

// =============================================================================
// Types
// =============================================================================

type AgentState = "running" | "awaiting" | "ready" | "errored" | "killed";

interface AgentSession {
  // Editor's stable session id.
  id: number;
  // Display label (defaults to root basename — Conductor never
  // renames externally-created sessions).
  label: string;
  // Absolute filesystem root.
  root: string;
  // The terminal id Conductor spawned in this session, if any.
  terminalId: number | null;
  // Last parsed agent state. "active" is computed at render
  // time from `editor.activeSession()`, not stored.
  state: AgentState;
  // Wall-clock ms when conductor.new fired createSession.
  createdAt: number;
}

// =============================================================================
// Module state — editor-global, survives every dive.
// =============================================================================

const conductorSessions = new Map<number, AgentSession>();

// Two-step "New Session" prompt: store the branch from step 1
// so step 2's confirm handler can read it.
let pendingBranchName: string | null = null;

// Pending session-creation intent. Stashed across the
// async `createSession → session_created hook` handoff so the
// hook handler can attach the spawned terminal.
let pendingNewSession:
  | { branch: string; cmd: string; root: string }
  | null = null;

// Last suggestion list shown in the open prompt. Mirrors the
// snapshot the user sees so prompt_confirmed can map the
// selected `value` back to a session id.
let promptSessionIds: number[] = [];

// Currently highlighted index in the open prompt, tracked via
// prompt_selection_changed so "Conductor: Kill Selected" knows
// which row the user is pointing at.
let promptSelectedIndex = 0;

// "Live preview by transient dive": while the prompt is open we
// dive into whichever session the user has highlighted, so the
// editor visible behind the floating overlay shows that
// session's full UI (splits, terminals, buffers, all of it,
// updating live). On Esc we restore the original active
// session; on Enter we leave the dive in place.
//
// The warm-swap migration (Step 1f, splits + view_states +
// LSPs + file explorer) makes these dives cheap — picking
// up/down through the list is a sequence of pointer swaps,
// not a UI rebuild.
let originalActiveSessionBeforePrompt: number | null = null;

// =============================================================================
// Session-list reconciliation
// =============================================================================

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
  for (const id of conductorSessions.keys()) {
    if (!seen.has(id)) conductorSessions.delete(id);
  }
}

// =============================================================================
// Suggestion rendering
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

function buildSuggestions(): PromptSuggestion[] {
  reconcileSessions();
  const ids = Array.from(conductorSessions.keys()).sort(
    (a, b) => a - b,
  );
  promptSessionIds = ids;
  const activeId = editor.activeSession();
  return ids.map((id) => {
    const s = conductorSessions.get(id)!;
    const stateText = id === activeId ? "ACT " : STATE_GLYPH[s.state];
    const ageText = ageString(s.createdAt);
    return {
      text: `[${id}] ${stateText}  ${s.label}`,
      description: `${ageText}  ${s.root}`,
      value: String(id),
    };
  });
}

function refreshPromptIfOpen(): void {
  // editor.setPromptSuggestions is a no-op when no prompt of any
  // type is open, so we can call it freely on every event.
  // When the conductor prompt is open it picks up the new list;
  // when something else is open we don't care, the next
  // `Conductor: Open` will rebuild from scratch.
  editor.setPromptSuggestions(buildSuggestions());
}

// =============================================================================
// Prompt orchestration
// =============================================================================

const PROMPT_TYPE = "conductor-room";

function openControlRoom(): void {
  promptSelectedIndex = 0;
  originalActiveSessionBeforePrompt = editor.activeSession();
  editor.startPrompt("Conductor — sessions", PROMPT_TYPE, true);
  editor.setPromptSuggestions(buildSuggestions());
  editor.setStatus(
    "Up/Down: preview  Enter: dive  Esc: cancel",
  );
}

editor.on("prompt_selection_changed", (e) => {
  if (e.prompt_type !== PROMPT_TYPE) return;
  promptSelectedIndex = e.selected_index;
  // Primitive #1: render the highlighted session's full UI in
  // the prompt's preview pane natively. No active-session
  // mutation, no flicker — the editor under the prompt stays
  // put while the right pane shows the previewed session.
  const id = promptSessionIds[promptSelectedIndex];
  if (typeof id === "number" && id !== editor.activeSession()) {
    editor.previewSessionInRect(id);
  } else {
    editor.clearSessionPreview();
  }
});

editor.on("prompt_confirmed", async (e) => {
  if (e.prompt_type === PROMPT_TYPE) {
    // Enter commits: dive into the highlighted session for
    // real. Clear the preview override so the next prompt
    // session doesn't accidentally inherit it.
    editor.clearSessionPreview();
    const id = promptSessionIds[promptSelectedIndex];
    if (typeof id === "number" && id !== editor.activeSession()) {
      editor.setActiveSession(id);
    }
    originalActiveSessionBeforePrompt = null;
    return;
  }

  if (e.prompt_type === "conductor-new-branch") {
    const name = (e.input || "").trim();
    if (!name) return;
    pendingBranchName = name;
    editor.startPrompt(
      "Agent command (e.g. 'aider', 'claude -p \"...\"')",
      "conductor-new-cmd",
      true,
    );
    return;
  }

  if (e.prompt_type === "conductor-new-cmd") {
    const cmd = (e.input || "").trim();
    const branch = pendingBranchName;
    pendingBranchName = null;
    if (!branch || !cmd) return;
    const cwd = editor.getCwd();
    const root = editor.pathJoin(cwd, ".fresh", "conductor", branch);
    try {
      await editor.spawnProcess("mkdir", ["-p", root], cwd);
    } catch {
      // best-effort; createTerminal will surface failures
    }
    pendingNewSession = { branch, cmd, root };
    editor.createSession(root, branch);
  }
});

editor.on("prompt_cancelled", (e) => {
  if (e.prompt_type === PROMPT_TYPE) {
    // Esc clears the preview override; active_session was
    // never mutated so there's nothing to roll back.
    editor.clearSessionPreview();
    originalActiveSessionBeforePrompt = null;
    return;
  }
  if (
    e.prompt_type === "conductor-new-branch" ||
    e.prompt_type === "conductor-new-cmd"
  ) {
    pendingBranchName = null;
    pendingNewSession = null;
  }
});

function startNewSession(): void {
  pendingBranchName = null;
  pendingNewSession = null;
  editor.startPrompt(
    "New session — branch / worktree name",
    "conductor-new-branch",
    true,
  );
}

function killSelected(): void {
  if (promptSessionIds.length === 0) {
    editor.setStatus("Conductor: open the session list first");
    return;
  }
  const id =
    promptSessionIds[
      Math.max(
        0,
        Math.min(promptSelectedIndex, promptSessionIds.length - 1),
      )
    ];
  if (id === 1) {
    editor.setStatus("Conductor: cannot kill the base session");
    return;
  }
  if (id === editor.activeSession()) {
    editor.setStatus(
      "Conductor: dive elsewhere first, then kill this session",
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
// Lifecycle hook handlers
// =============================================================================

editor.on("session_created", async (payload) => {
  const id = payload.id;
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
    editor.sendTerminalInput(term.terminalId, intent.cmd + "\n");
  }
  refreshPromptIfOpen();
});

editor.on("session_closed", () => {
  refreshPromptIfOpen();
});

editor.on("active_session_changed", () => {
  refreshPromptIfOpen();
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
  for (const s of conductorSessions.values()) {
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
  refreshPromptIfOpen();
});

editor.on("terminal_exit", (payload) => {
  for (const s of conductorSessions.values()) {
    if (s.terminalId === payload.terminal_id) {
      const code = payload.exit_code;
      // exit_code is currently always null (the editor's
      // wait-status capture is a follow-up). Treat unknown as
      // ready — Conductor doesn't have a better heuristic and
      // mis-marking a real error as "ready" is recoverable
      // (the user opens the dive and sees the failure).
      s.state = code === null || code === 0 ? "ready" : "errored";
      break;
    }
  }
  refreshPromptIfOpen();
});

// =============================================================================
// Commands
// =============================================================================

registerHandler("conductor_open", openControlRoom);
registerHandler("conductor_new", startNewSession);
registerHandler("conductor_kill", killSelected);

editor.registerCommand(
  "Conductor: Open",
  "Show all editor sessions in a floating selector",
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
  "Conductor: Kill Selected",
  "Close the session highlighted in the open Conductor prompt",
  "conductor_kill",
  null,
);
