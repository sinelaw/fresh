/**
 * Orca's persisted state (`profiles/<id>/orca-data.json`), read without an
 * editor so tests can import it. Field names checked against a real install.
 */

/** One worktree and the agent sessions Orca recorded in it. */
export interface OrcaWorktree {
  /** `<repoId>::<path>`, Orca's own key. */
  id: string;
  path: string;
  /** Orca's display name, else the repository's. Empty when neither is set. */
  name: string;
  /** Milliseconds since the epoch. */
  lastActivityAt?: number;
  agents: OrcaAgentSession[];
}

export interface OrcaAgentSession {
  /** `claude`, `codex`, ... as Orca names it. */
  agent: string;
  /** The agent's own session id; empty when Orca only knows which agent ran. */
  sessionId: string;
  title?: string;
  /** Orca's terminal tab the agent ran in. */
  tabId: string;
}

interface Tab {
  id?: string;
  launchAgent?: string;
  aiVaultTitle?: { agent?: string; sessionId?: string; title?: string };
}

interface SleepingSession {
  tabId?: string;
  worktreeId?: string;
  agent?: string;
  providerSession?: { id?: string };
  terminalTitle?: string;
}

export interface OrcaState {
  repos?: { id?: string; displayName?: string }[];
  worktreeMeta?: Record<string, {
    displayName?: string;
    lastActivityAt?: number;
    hostId?: string;
  }>;
  workspaceSession?: {
    tabsByWorktree?: Record<string, Tab[]>;
    sleepingAgentSessionsByPaneKey?: Record<string, SleepingSession>;
  };
}

/** The path half of a worktree key; null for a key of another shape. */
export function orcaWorktreePath(key: string): string | null {
  const sep = key.indexOf("::");
  if (sep < 0) return null;
  const path = key.slice(sep + 2);
  return path.length > 0 ? path : null;
}

/** Worktrees on this host, each with the agent sessions its tabs record. */
export function orcaWorktrees(state: OrcaState): OrcaWorktree[] {
  const repoNames = new Map<string, string>();
  for (const repo of state.repos ?? []) {
    if (repo.id && repo.displayName) repoNames.set(repo.id, repo.displayName);
  }
  const tabs = state.workspaceSession?.tabsByWorktree ?? {};
  const sleeping = Object.values(state.workspaceSession?.sleepingAgentSessionsByPaneKey ?? {});

  const out: OrcaWorktree[] = [];
  for (const [key, meta] of Object.entries(state.worktreeMeta ?? {})) {
    // A worktree on an SSH target is a path on that host, not this one.
    if (meta.hostId !== undefined && meta.hostId !== "local") continue;
    const path = orcaWorktreePath(key);
    if (path === null) continue;

    const agents = new Map<string, OrcaAgentSession>();
    for (const tab of tabs[key] ?? []) {
      const agent = tab.aiVaultTitle?.agent ?? tab.launchAgent;
      if (!tab.id || !agent) continue;
      agents.set(tab.id, {
        agent,
        sessionId: tab.aiVaultTitle?.sessionId ?? "",
        title: tab.aiVaultTitle?.title,
        tabId: tab.id,
      });
    }
    // A tab whose agent was running at quit keeps its session id here too.
    for (const s of sleeping) {
      if (s.worktreeId !== key || !s.tabId || !s.agent) continue;
      const known = agents.get(s.tabId);
      if (known && known.sessionId) continue;
      agents.set(s.tabId, {
        agent: s.agent,
        sessionId: s.providerSession?.id ?? known?.sessionId ?? "",
        title: known?.title ?? s.terminalTitle,
        tabId: s.tabId,
      });
    }

    const repoId = key.slice(0, key.indexOf("::"));
    out.push({
      id: key,
      path,
      name: meta.displayName || repoNames.get(repoId) || "",
      lastActivityAt: meta.lastActivityAt,
      agents: [...agents.values()],
    });
  }
  return out;
}
