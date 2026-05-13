/// <reference path="./lib/fresh.d.ts" />
//
// Conductor — multi-agent / multi-worktree session orchestration.
//
// MVP scope (`docs/internal/conductor-sessions-design.md`):
//
//   - "Conductor: Open" opens a floating overlay prompt listing
//     every session with its state column. Up/Down navigates,
//     Enter dives into the selected session.
//   - "Conductor: New Session" opens a single floating widget
//     form with three optional fields (session name, agent
//     command, branch), allocates a worktree-rooted session and
//     spawns the agent in a terminal attached to it.
//   - "Conductor: Kill Selected" closes the session whose row is
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
  row,
  spacer,
  styledRow,
  text,
  textInputChar,
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
  // Display label (defaults to root basename — Conductor never
  // renames externally-created sessions).
  label: string;
  // Absolute filesystem root.
  root: string;
  // The terminal id Conductor spawned in this session, if any.
  terminalId: number | null;
  // Last parsed agent state. "active" is computed at render
  // time from `editor.activeWindow()`, not stored.
  state: AgentState;
  // Wall-clock ms when conductor.new fired createWindow.
  createdAt: number;
}

// =============================================================================
// Module state — editor-global, survives every dive.
// =============================================================================

const conductorSessions = new Map<number, AgentSession>();

// Pending session-creation intent. Stashed across the
// async `createWindow → window_created hook` handoff so the
// hook handler can attach the spawned terminal. (Internally
// the editor calls these "windows"; Conductor still presents
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
}
let form: NewSessionForm | null = null;
let formPanel: FloatingWidgetPanel | null = null;

const NEW_SESSION_MODE = "conductor-new-form";

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
  const editorSessions = editor.listWindows();
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

// Suggestion value reserved for the "+ New session" pseudo-row.
// Distinct from every numeric session id, so prompt_confirmed
// can tell it apart.
const NEW_SESSION_VALUE = "__new__";

function buildSuggestions(): PromptSuggestion[] {
  reconcileSessions();
  const ids = Array.from(conductorSessions.keys()).sort(
    (a, b) => a - b,
  );
  // Sentinel -1 keeps promptSessionIds aligned 1:1 with the
  // visible suggestion rows so promptSelectedIndex still indexes
  // it directly. -1 means "the [+ New] pseudo-row".
  promptSessionIds = [-1, ...ids];
  const activeId = editor.activeWindow();
  const newRow: PromptSuggestion = {
    text: "+ New session",
    description: "Create a new worktree-rooted session",
    value: NEW_SESSION_VALUE,
  };
  const sessionRows = ids.map((id) => {
    const s = conductorSessions.get(id)!;
    const stateText = id === activeId ? "ACT " : STATE_GLYPH[s.state];
    const ageText = ageString(s.createdAt);
    return {
      text: `[${id}] ${stateText}  ${s.label}`,
      description: `${ageText}  ${s.root}`,
      value: String(id),
    };
  });
  return [newRow, ...sessionRows];
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
  const activeId = editor.activeWindow();
  originalActiveSessionBeforePrompt = activeId;
  // Trailing " │ " separates the static title from the user's
  // search input — without it typed characters jam right up
  // against "sessions".
  editor.startPrompt("Conductor — sessions  │  ", PROMPT_TYPE, true);
  editor.setPromptSuggestions(buildSuggestions());
  // Pseudo-row sits at index 0 (sentinel -1 in promptSessionIds),
  // so the active session's row is at indexOf(activeId).
  const activeIdx = promptSessionIds.indexOf(activeId);
  promptSelectedIndex = activeIdx > 0 ? activeIdx : 0;
  if (activeIdx > 0) {
    editor.setPromptSelectedIndex(activeIdx);
  }
  editor.setPromptFooter([
    { text: " " },
    { text: "↑↓", style: { fg: "ui.help_key_fg" } },
    { text: " preview  " },
    { text: "Enter", style: { fg: "ui.help_key_fg" } },
    { text: " dive/new  " },
    { text: "Esc", style: { fg: "ui.help_key_fg" } },
    { text: " close" },
  ]);
  editor.setStatus(
    "Up/Down: preview  Enter: dive/new  Esc: cancel",
  );
}

editor.on("prompt_selection_changed", (e) => {
  if (e.prompt_type !== PROMPT_TYPE) return;
  promptSelectedIndex = e.selected_index;
  // Render the highlighted session's full UI in the prompt's
  // preview pane. Sentinel -1 (the [+ New] row) has nothing to
  // preview — clear instead.
  const id = promptSessionIds[promptSelectedIndex];
  if (typeof id === "number" && id > 0 && id !== editor.activeWindow()) {
    editor.previewWindowInRect(id);
  } else {
    editor.clearWindowPreview();
  }
});

editor.on("prompt_confirmed", (e) => {
  if (e.prompt_type === PROMPT_TYPE) {
    // Enter commits: dive into the highlighted session, or fire
    // the new-session flow when the [+ New] pseudo-row is the
    // selected value.
    editor.clearWindowPreview();
    originalActiveSessionBeforePrompt = null;
    if (e.input === NEW_SESSION_VALUE) {
      startNewSession();
      return;
    }
    const id = promptSessionIds[promptSelectedIndex];
    if (typeof id === "number" && id > 0 && id !== editor.activeWindow()) {
      editor.setActiveWindow(id);
    }
    return;
  }
});

editor.on("prompt_cancelled", (e) => {
  if (e.prompt_type === PROMPT_TYPE) {
    // Esc clears the preview override; active_session was
    // never mutated so there's nothing to roll back.
    editor.clearWindowPreview();
    originalActiveSessionBeforePrompt = null;
    return;
  }
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

async function spawnCollect(
  command: string,
  args: string[],
  cwd: string,
): Promise<SpawnResult> {
  return await editor.spawnProcess(command, args, cwd);
}

async function detectDefaultBranch(repoRoot: string): Promise<string> {
  // `git symbolic-ref refs/remotes/origin/HEAD` → e.g.
  // `refs/remotes/origin/main`. Strip the prefix; fall back to
  // `HEAD` when no remote is set or the symbolic ref is missing.
  const res = await spawnCollect(
    "git",
    ["-C", repoRoot, "symbolic-ref", "refs/remotes/origin/HEAD"],
    repoRoot,
  );
  if (res.exit_code === 0) {
    const trimmed = (res.stdout || "").trim();
    const prefix = "refs/remotes/origin/";
    if (trimmed.startsWith(prefix)) {
      return trimmed.slice(prefix.length);
    }
  }
  return "HEAD";
}

function nextAutoSessionName(): string {
  // Persisted counter so consecutive empty submits produce
  // session-1, session-2, … even across plugin reloads.
  const counter = (editor.getGlobalState("conductor.session_counter") as
    | number
    | undefined) ?? 0;
  const next = counter + 1;
  editor.setGlobalState("conductor.session_counter", next);
  return `session-${next}`;
}

// Three distinct styles for the header line: section keyword
// ("CONDUCTOR"), structural separators ("::"), and step label. The
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
            { text: "CONDUCTOR", style: HEADER_KEYWORD_STYLE },
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
    labeledSection({
      label: "▸ Session Name",
      child: text({
        value: form.name.value,
        cursorByte: form.name.cursor,
        placeholder: "(auto-generated)",
        fullWidth: true,
        key: "name",
      }),
    }),
    labeledSection({
      label: "▸ Agent Command",
      child: text({
        value: form.cmd.value,
        cursorByte: form.cmd.cursor,
        placeholder: "(plain shell)",
        fullWidth: true,
        key: "cmd",
      }),
    }),
    labeledSection({
      label: "▸ Branch",
      child: text({
        value: form.branch.value,
        cursorByte: form.branch.cursor,
        placeholder: "(off default branch)",
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
// working directory's tail. Conductor never opens this dialog
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
    (editor.getGlobalState("conductor.last_cmd") as string | undefined) ?? "";
  form = {
    name: { value: "", cursor: 0 },
    cmd: { value: lastCmd, cursor: lastCmd.length },
    branch: { value: "", cursor: 0 },
    submitting: false,
    lastError: null,
    projectLabel: deriveProjectLabel(),
  };
  formPanel = new FloatingWidgetPanel();
  formPanel.mount(buildFormSpec(), { widthPct: 60, heightPct: 50 });
  editor.setEditorMode(NEW_SESSION_MODE);
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

  const sessionName = form.name.value.trim() || nextAutoSessionName();
  const cmd = form.cmd.value.trim();
  const branchInput = form.branch.value.trim();

  const cwd = editor.getCwd();
  const top = await spawnCollect("git", ["rev-parse", "--show-toplevel"], cwd);
  if (top.exit_code !== 0) {
    if (!form) return;
    form.submitting = false;
    form.lastError = lastNonEmptyLine(top.stderr) || "not a git repository";
    renderForm();
    return;
  }
  const repoRoot = (top.stdout || "").trim();

  const root = editor.pathJoin(
    editor.getDataDir(),
    "conductor",
    slugify(repoRoot),
    sessionName,
  );
  const parent = editor.pathDirname(root);
  if (!editor.createDir(parent)) {
    if (!form) return;
    form.submitting = false;
    form.lastError = `mkdir failed: ${parent}`;
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
      form.lastError = lastNonEmptyLine(addRes.stderr) ||
        lastNonEmptyLine(fallback.stderr) ||
        "git worktree add failed";
      renderForm();
      return;
    }
    addRes = fallback;
  }

  if (cmd) {
    editor.setGlobalState("conductor.last_cmd", cmd);
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
  ["Tab", "conductor_form_key_tab"],
  ["S-Tab", "conductor_form_key_shift_tab"],
  ["Return", "conductor_form_key_enter"],
  ["Escape", "conductor_form_key_escape"],
  ["Backspace", "conductor_form_key_backspace"],
  ["Delete", "conductor_form_key_delete"],
  ["Home", "conductor_form_key_home"],
  ["End", "conductor_form_key_end"],
  ["Left", "conductor_form_key_left"],
  ["Right", "conductor_form_key_right"],
  ["Up", "conductor_form_key_up"],
  ["Down", "conductor_form_key_down"],
];

editor.defineMode(NEW_SESSION_MODE, FORM_MODE_BINDINGS, true, true);

function dispatchFormKey(name: string): void {
  if (!form || !formPanel) return;
  formPanel.command(widgetKey(name));
}

registerHandler("conductor_form_key_tab", () => dispatchFormKey("Tab"));
registerHandler(
  "conductor_form_key_shift_tab",
  () => dispatchFormKey("Shift+Tab"),
);
registerHandler("conductor_form_key_enter", () => dispatchFormKey("Enter"));
registerHandler("conductor_form_key_escape", () => {
  if (form) closeForm();
});
registerHandler(
  "conductor_form_key_backspace",
  () => dispatchFormKey("Backspace"),
);
registerHandler("conductor_form_key_delete", () => dispatchFormKey("Delete"));
registerHandler("conductor_form_key_home", () => dispatchFormKey("Home"));
registerHandler("conductor_form_key_end", () => dispatchFormKey("End"));
registerHandler("conductor_form_key_left", () => dispatchFormKey("Left"));
registerHandler("conductor_form_key_right", () => dispatchFormKey("Right"));
registerHandler("conductor_form_key_up", () => dispatchFormKey("Up"));
registerHandler("conductor_form_key_down", () => dispatchFormKey("Down"));

// Printable input arrives via the global `mode_text_input` action.
// Other plugins may also register a `mode_text_input` handler;
// guard on `form` so this handler is a no-op outside the form.
function conductor_mode_text_input(args: { text: string }): void {
  if (!form || !formPanel || !args?.text) return;
  formPanel.command(textInputChar(args.text));
}
registerHandler("mode_text_input", conductor_mode_text_input);

editor.on("widget_event", (e) => {
  if (!form || !formPanel) return;
  if (e.panel_id !== formPanel.id()) return;
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
  }
});

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
  // Sentinel: pseudo-row "+ New" has nothing to kill.
  if (id === -1) {
    editor.setStatus("Conductor: select a session row first");
    return;
  }
  if (id === 1) {
    editor.setStatus("Conductor: cannot kill the base session");
    return;
  }
  if (id === editor.activeWindow()) {
    editor.setStatus(
      "Conductor: dive elsewhere first, then kill this session",
    );
    return;
  }
  const s = conductorSessions.get(id);
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
    // windowId attaches the terminal to the new session's split
    // tree; we then dive so the user sees the shell/agent
    // immediately — creating a session is a visit-now action.
    const term = await editor.createTerminal({
      cwd: intent.root,
      focus: false,
      windowId: id,
    });
    const tracked: AgentSession = {
      id,
      label: intent.label,
      root: intent.root,
      terminalId: term.terminalId,
      state: "running",
      createdAt: Date.now(),
    };
    conductorSessions.set(id, tracked);
    if (intent.cmd) {
      editor.sendTerminalInput(term.terminalId, intent.cmd + "\n");
    }
    editor.setActiveWindow(id);
  }
  refreshPromptIfOpen();
});

editor.on("window_closed", () => {
  refreshPromptIfOpen();
});

editor.on("active_window_changed", () => {
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
