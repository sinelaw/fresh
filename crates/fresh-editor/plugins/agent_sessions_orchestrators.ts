/// <reference path="./lib/fresh.d.ts" />

/**
 * Orca scanner for the agent-sessions hub. Orca runs agents in one git
 * worktree per task and keeps its state under `$HOME`.
 *
 * Orca: user data at `$ORCA_USER_DATA`, else `$XDG_DATA_HOME/Orca`, else
 * `~/.orca`. One JSON file per profile with `worktreeMeta` keyed by worktree id.
 * `agent-hooks/` marks that Orca manages agents even when no state file parses.
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

const STATE_BYTES = 512 * 1024;

interface WorktreeMeta {
  name?: string;
  path?: string;
  branch?: string;
  repoId?: string;
}

interface PersistedState {
  worktreeMeta?: Record<string, WorktreeMeta>;
  folderWorkspaces?: Record<string, { path?: string; name?: string }>;
}

registerScanner({
  id: "orca",
  displayName: "Orca",

  async scan(machine: FreshMachine): Promise<ScannerReport> {
    const problems: string[] = [];
    const env = await envDirs(machine, ["ORCA_USER_DATA", "XDG_DATA_HOME"], problems);
    const data =
      env["ORCA_USER_DATA"]
      ?? (env["XDG_DATA_HOME"] !== undefined
        ? joinPath(machine, env["XDG_DATA_HOME"], "Orca")
        : joinPath(machine, machine.home, ".orca"));

    const walk = await machine.walkTree(data, {
      includeHidden: true,
      includeDirs: true,
      maxDepth: 2,
      maxEntries: 2000,
    });
    if (walk.entries.length === 0) {
      return { sessions: [], installed: false, problems };
    }

    const hooks = walk.entries.some((entry) => entry.rel === "agent-hooks" && entry.kind === "dir");

    const states = walk.entries.filter(
      (e) => e.kind !== "dir" && e.rel.toLowerCase().endsWith(".json"),
    );
    const headers = await readHeaders(machine, states, { maxBytes: STATE_BYTES, problems });

    const sessions: CollectedSession[] = [];
    for (const { entry, text } of headers) {
      if (text === null) continue;
      let state: PersistedState;
      try {
        state = JSON.parse(text) as PersistedState;
      } catch {
        // Not every JSON file here is state, so a parse failure is expected.
        // Unless the read filled the prefix: then the file was cut short.
        if (text.length >= STATE_BYTES) {
          problems.push(
            `${entry.path}: larger than the ${STATE_BYTES} bytes read, so its worktrees could not be parsed`,
          );
        }
        continue;
      }

      for (const [id, meta] of Object.entries(state.worktreeMeta ?? {})) {
        sessions.push({
          id,
          title: meta.name ?? meta.branch ?? id,
          cwd: meta.path,
          // A worktree Orca ran an agent in: Import opens a workspace on it.
          openable: true,
          path: entry.path,
          mtime: entry.mtime,
          evidence: [
            {
              locator: entry.path,
              saying: `Orca worktree${meta.branch ? ` on branch ${meta.branch}` : ""}`,
            },
          ],
        });
      }
    }

    if (hooks && sessions.length === 0) {
      problems.push(
        `${data}: Orca installs agent hooks here, so it manages agents on this machine, but no worktree state could be read`,
      );
    }
    return { sessions, installed: true, problems };
  },
});

editor.debug("agent-sessions: Orca scanner registered");
