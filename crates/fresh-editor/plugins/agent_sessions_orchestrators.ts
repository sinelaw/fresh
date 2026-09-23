/// <reference path="./lib/fresh.d.ts" />

/**
 * Orca scanner for the agent-sessions hub. Orca is an Electron app that runs
 * agents in terminal tabs, one git worktree per task.
 *
 * User data: `$ORCA_USER_DATA`, else `$XDG_CONFIG_HOME/orca`, else
 * `~/.config/orca` (Electron's userData; `~/Library/Application Support/orca`
 * on macOS). State is one
 * `profiles/<profile>/orca-data.json` per profile; `orca-data.json.bak.N`
 * beside it are older copies and are not read.
 *
 * In that file `worktreeMeta` is keyed `<repoId>::<path>` and holds no path,
 * name or branch fields: the path is the key's second half, the name comes
 * from `displayName` or `repos[].displayName`, and the branch is left to the
 * hub's `.git/HEAD` read. `hostId` other than `local` is an SSH worktree.
 * `workspaceSession.tabsByWorktree[key][].aiVaultTitle` records `{agent,
 * sessionId}` per agent tab, which Import resumes exactly.
 *
 * `~/.orca/agent-hooks/` marks that Orca manages agents even when no state
 * file parses.
 */

import {
  envDirs,
  joinPath,
  readHeaders,
  registerScanner,
  type CollectedSession,
  type ScannerReport,
} from "./lib/agent_scanner.ts";
import { orcaWorktrees, type OrcaState } from "./lib/orca_format.ts";

const editor = getEditor();

// A real one is tens of KB; tab and browser history make it grow.
const STATE_BYTES = 4 * 1024 * 1024;

/** Whether `dir` exists: a git worktree always holds a `.git` entry. */
async function exists(machine: FreshMachine, dir: string): Promise<boolean> {
  const walk = await machine.walkTree(dir, {
    includeHidden: true,
    includeDirs: true,
    maxDepth: 1,
    maxEntries: 1,
  });
  return walk.entries.length > 0;
}

registerScanner({
  id: "orca",
  displayName: "Orca",

  async scan(machine: FreshMachine): Promise<ScannerReport> {
    const problems: string[] = [];
    const env = await envDirs(machine, ["ORCA_USER_DATA", "XDG_CONFIG_HOME"], problems);
    const config = machine.platform === "macos"
      ? joinPath(machine, machine.home, "Library", "Application Support")
      : env["XDG_CONFIG_HOME"] ?? joinPath(machine, machine.home, ".config");
    const data = env["ORCA_USER_DATA"] ?? joinPath(machine, config, "orca");
    const hooksDir = joinPath(machine, machine.home, ".orca", "agent-hooks");

    const walk = await machine.walkTree(joinPath(machine, data, "profiles"), {
      includeHidden: true,
      maxDepth: 2,
      maxEntries: 200,
    });
    const states = walk.entries.filter(
      (e) => e.kind === "file" && /^[^/]+\/orca-data\.json$/.test(e.rel),
    );
    const hooks = states.length === 0 && await exists(machine, hooksDir);
    if (states.length === 0 && !hooks) {
      return { sessions: [], installed: false, problems };
    }

    const headers = await readHeaders(machine, states, { maxBytes: STATE_BYTES, problems });

    const sessions: CollectedSession[] = [];
    for (const { entry, text } of headers) {
      if (text === null) continue;
      let state: OrcaState;
      try {
        state = JSON.parse(text) as OrcaState;
      } catch (e) {
        problems.push(
          text.length >= STATE_BYTES
            ? `${entry.path}: larger than the ${STATE_BYTES} bytes read, so its worktrees could not be parsed`
            : `${entry.path}: ${e instanceof Error ? e.message : String(e)}`,
        );
        continue;
      }

      const worktrees = orcaWorktrees(state);
      // A worktree removed outside Orca stays in its state; skip it.
      const live = await Promise.all(worktrees.map((w) => exists(machine, w.path)));
      worktrees.forEach((wt, i) => {
        if (!live[i]) return;
        const mtime = wt.lastActivityAt !== undefined
          ? Math.floor(wt.lastActivityAt / 1000)
          : entry.mtime;
        const base = {
          cwd: wt.path,
          // Import opens a workspace on the folder when no agent can resume.
          openable: true,
          path: entry.path,
          mtime,
        };
        if (wt.agents.length === 0) {
          sessions.push({
            ...base,
            id: wt.id,
            title: wt.name || undefined,
            evidence: [{ locator: entry.path, saying: "Orca worktree" }],
          });
          return;
        }
        for (const a of wt.agents) {
          sessions.push({
            ...base,
            id: `${wt.id}#${a.tabId}`,
            title: a.title || wt.name || undefined,
            // Empty id: the agent's newest session in the worktree.
            agent: a.agent,
            agentSessionId: a.sessionId,
            evidence: [{
              locator: entry.path,
              saying: a.sessionId
                ? `Orca records this tab's ${a.agent} session ${a.sessionId}`
                : `Orca records this tab as running ${a.agent}`,
            }],
          });
        }
      });
    }

    if (hooks && sessions.length === 0) {
      problems.push(
        `${hooksDir}: Orca installs agent hooks here, so it manages agents on this machine, but no worktree state could be read under ${data}`,
      );
    }
    return { sessions, installed: true, problems };
  },
});

editor.debug("agent-sessions: Orca scanner registered");
