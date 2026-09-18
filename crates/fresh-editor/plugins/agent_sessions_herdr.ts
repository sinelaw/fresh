/// <reference path="./lib/fresh.d.ts" />

/**
 * Herdr scanner for the agent-sessions hub. Herdr is a terminal multiplexer
 * that records which agent each pane runs.
 *
 * Config dir: `$XDG_CONFIG_HOME/herdr`, else `~/.config/herdr`; a debug build
 * uses `herdr-dev`. The default session's `session.json` sits in the config
 * dir itself; named sessions are under `sessions/<name>/`.
 *
 * A pane may carry `agent_session: {agent, kind, value}` where `kind` is `id`
 * (the agent's own session id) or `path` (its transcript).
 *
 * A legacy snapshot put panes directly on the workspace; the current one nests
 * them under `tabs`. Both are read, and every field is treated as optional.
 */

import {
  envDirs,
  joinPath,
  readHeaders,
  registerScanner,
  type CollectedSession,
  type ScannerReport,
} from "./lib/agent_scanner.ts";

const editor = getEditor();

// The structure read here is at the front; the rest is scrollback metadata.
const SNAPSHOT_BYTES = 512 * 1024;

interface PaneAgentSession {
  source?: string;
  agent?: string;
  kind?: string;
  value?: string;
}

interface PaneSnapshot {
  cwd?: string;
  agent_name?: string;
  managed_agent_kind?: string;
  agent_session?: PaneAgentSession;
}

interface TabSnapshot {
  panes?: Record<string, PaneSnapshot>;
}

interface WorkspaceSnapshot {
  id?: string;
  name?: string;
  identity_cwd?: string;
  tabs?: TabSnapshot[];
  /** Legacy shape. */
  panes?: Record<string, PaneSnapshot>;
}

interface Snapshot {
  version?: number;
  workspaces?: WorkspaceSnapshot[];
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

/** Config directories to check, release build first. */
async function configDirs(
  machine: FreshMachine,
  problems: string[],
): Promise<{ dir: string; variant: string }[]> {
  const base =
    (await envDirs(machine, ["XDG_CONFIG_HOME"], problems))["XDG_CONFIG_HOME"]
    ?? joinPath(machine, machine.home, ".config");
  // Both builds name their default session "default"; the variant keeps the ids distinct.
  return [
    { dir: joinPath(machine, base, "herdr"), variant: "" },
    { dir: joinPath(machine, base, "herdr-dev"), variant: "herdr-dev" },
  ];
}

registerScanner({
  id: "herdr",
  displayName: "Herdr",

  async scan(machine: FreshMachine): Promise<ScannerReport> {
    const problems: string[] = [];
    const sessions: CollectedSession[] = [];
    let installed = false;

    for (const { dir, variant } of await configDirs(machine, problems)) {
      // Depth 3 covers `session.json` and `sessions/<name>/session.json`.
      const walk = await machine.walkTree(dir, {
        includeHidden: true,
        includeDirs: false,
        maxDepth: 3,
        maxEntries: 2000,
      });
      if (walk.entries.length === 0) continue;
      installed = true;

      const snapshots = walk.entries.filter((e) => e.rel.endsWith("session.json"));
      const headers = await readHeaders(machine, snapshots, { maxBytes: SNAPSHOT_BYTES, problems });

      for (const { entry, text } of headers) {
        if (text === null) continue;
        let snapshot: Snapshot;
        try {
          snapshot = JSON.parse(text) as Snapshot;
        } catch (e) {
          problems.push(
            `${entry.path}: ${e instanceof Error ? e.message : String(e)}`,
          );
          continue;
        }

        const parts = entry.rel.split("/").filter((p) => p.length > 0);
        const sessionName = parts.length >= 2 ? parts[parts.length - 2] : "default";
        const prefixId = variant ? `${variant}/` : "";

        for (const workspace of snapshot.workspaces ?? []) {
          const workspaceId = workspace.id ?? workspace.name ?? "workspace";
          for (const { id: paneId, pane } of panesOf(workspace)) {
            const agent = pane.agent_session;
            // A pane with no agent recorded is a plain terminal.
            if (!agent?.value && !pane.managed_agent_kind && !pane.agent_name) continue;
            sessions.push({
              id: `${prefixId}${sessionName}/${workspaceId}/${paneId}`,
              title: pane.agent_name ?? pane.managed_agent_kind ?? agent?.agent,
              cwd: pane.cwd ?? workspace.identity_cwd,
              path: entry.path,
              mtime: entry.mtime,
              // A `path` kind names a transcript, which the hub cannot key on.
              agentSessionId: agent?.kind === "id" ? agent.value : undefined,
              evidence: [
                {
                  locator: entry.path,
                  saying: agent?.value
                    ? `Herdr records this pane's agent session (${agent.kind}: ${agent.value})`
                    : `Herdr records this pane as running ${pane.managed_agent_kind ?? pane.agent_name}`,
                },
              ],
            });
          }
        }
      }
    }

    return { sessions, installed, problems };
  },
});

editor.debug("agent-sessions: Herdr scanner registered");
