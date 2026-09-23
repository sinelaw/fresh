/// <reference path="./lib/fresh.d.ts" />

/**
 * Herdr scanner for the agent-sessions hub. Herdr is a terminal multiplexer
 * that records which agent each pane runs.
 *
 * Config dir: `$XDG_CONFIG_HOME/herdr`, else `~/.config/herdr`; a debug build
 * uses `herdr-dev`. The default session's `session.json` sits in the config
 * dir itself; named sessions are under `sessions/<name>/`.
 *
 * A pane's `agent_session: {source, agent, kind, value}` is written only when
 * an official integration (`source` = `herdr:<agent>`) reported it; `kind` is
 * `id` (the agent's session id) or `path` (a transcript). `managed_agent_kind`
 * marks an agent started with `herdr agent start`. `agent_name` is a user
 * label, not a kind. An agent Herdr only detected by process is never saved.
 * Scrollback lives in `session-history.json`, which is not read.
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
import { herdrAgentPanes, type HerdrSnapshot } from "./lib/herdr_format.ts";

const editor = getEditor();

const SNAPSHOT_BYTES = 512 * 1024;

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
        let snapshot: HerdrSnapshot;
        try {
          snapshot = JSON.parse(text) as HerdrSnapshot;
        } catch (e) {
          problems.push(
            `${entry.path}: ${e instanceof Error ? e.message : String(e)}`,
          );
          continue;
        }

        const parts = entry.rel.split("/").filter((p) => p.length > 0);
        const sessionName = parts.length >= 2 ? parts[parts.length - 2] : "default";
        const prefixId = variant ? `${variant}/` : "";

        for (const pane of herdrAgentPanes(snapshot)) {
          sessions.push({
            id: `${prefixId}${sessionName}/${pane.workspaceId}/${pane.paneId}`,
            title: pane.title,
            cwd: pane.cwd,
            path: entry.path,
            mtime: entry.mtime,
            // Import rejoins by the recorded session id, else (empty id) the
            // newest session in the pane's directory.
            agent: pane.agent,
            agentSessionId: pane.sessionId,
            evidence: [{ locator: entry.path, saying: pane.saying }],
          });
        }
      }
    }

    return { sessions, installed, problems };
  },
});

editor.debug("agent-sessions: Herdr scanner registered");
