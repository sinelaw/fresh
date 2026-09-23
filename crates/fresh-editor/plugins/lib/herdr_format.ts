/**
 * Herdr's `session.json` (snapshot version 3), read without an editor so tests
 * can import it. Field names follow Herdr's `src/persist/snapshot.rs`.
 */

interface PaneAgentSession {
  /** `herdr:<agent>` when an official integration reported it. */
  source?: string;
  agent?: string;
  kind?: string;
  value?: string;
}

export interface PaneSnapshot {
  cwd?: string;
  label?: string;
  /** A name the user gave the agent ("reviewer"), not its kind. */
  agent_name?: string;
  /** Set for agents started with `herdr agent start --kind`. */
  managed_agent_kind?: string;
  agent_session?: PaneAgentSession;
}

interface TabSnapshot {
  panes?: Record<string, PaneSnapshot>;
}

interface WorkspaceSnapshot {
  id?: string;
  custom_name?: string | null;
  identity_cwd?: string;
  tabs?: TabSnapshot[];
  /** Legacy shape. */
  panes?: Record<string, PaneSnapshot>;
}

export interface HerdrSnapshot {
  version?: number;
  workspaces?: WorkspaceSnapshot[];
}

/** One pane Herdr recorded an agent in. */
export interface HerdrAgentPane {
  workspaceId: string;
  paneId: string;
  cwd?: string;
  title: string;
  /** argv0: `claude`, `codex`, ... */
  agent: string;
  /** The agent's session id; empty when Herdr knows only which agent ran. */
  sessionId: string;
  /** What the row's evidence says. */
  saying: string;
}

/** The command an agent Herdr names runs as (`claude`, `codex`, …): what the
 *  orchestrator's resume registry keys on. */
export function herdrAgentCommand(name: string | undefined): string | undefined {
  const n = (name ?? "").toLowerCase();
  if (!n) return undefined;
  if (n.includes("claude")) return "claude";
  if (n.includes("codex")) return "codex";
  if (n.includes("opencode")) return "opencode";
  if (n.includes("aider")) return "aider";
  return n.split(/[\s/]/)[0];
}

/** Panes in id order, from both the current and the legacy shape. */
function panesOf(workspace: WorkspaceSnapshot): { id: string; pane: PaneSnapshot }[] {
  const out: { id: string; pane: PaneSnapshot }[] = [];
  const collect = (panes: Record<string, PaneSnapshot> | undefined) => {
    for (const id of Object.keys(panes ?? {}).sort()) {
      const pane = panes?.[id];
      if (pane) out.push({ id, pane });
    }
  };
  for (const tab of workspace.tabs ?? []) collect(tab.panes);
  collect(workspace.panes);
  return out;
}

/** Panes with an agent Herdr persisted. A hand-typed agent Herdr only
 *  detected is never written, so it is not here. */
export function herdrAgentPanes(snapshot: HerdrSnapshot): HerdrAgentPane[] {
  const out: HerdrAgentPane[] = [];
  for (const workspace of snapshot.workspaces ?? []) {
    const workspaceId = workspace.id ?? workspace.custom_name ?? "workspace";
    for (const { id: paneId, pane } of panesOf(workspace)) {
      const session = pane.agent_session;
      // Herdr resumes only sessions an official integration reported.
      const official = session?.agent && session.source === `herdr:${session.agent}`
        ? session
        : undefined;
      const agent = herdrAgentCommand(official?.agent ?? pane.managed_agent_kind);
      if (!agent) continue;
      // A `path` session names a transcript, which Fresh cannot resume by.
      const sessionId = official?.kind === "id" ? official.value ?? "" : "";
      out.push({
        workspaceId,
        paneId,
        cwd: pane.cwd ?? workspace.identity_cwd,
        title: pane.agent_name ?? pane.label ?? agent,
        agent,
        sessionId,
        saying: official?.value
          ? `Herdr records this pane's agent session (${official.kind}: ${official.value})`
          : `Herdr records this pane as running ${pane.managed_agent_kind ?? agent}`,
      });
    }
  }
  return out;
}
