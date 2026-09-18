/// <reference path="./lib/fresh.d.ts" />

/**
 * Orca, Superset and super.engineering scanners for the agent-sessions hub.
 * All three run agents in one git worktree per task and keep state under `$HOME`.
 *
 * Orca: user data at `$ORCA_USER_DATA`, else `$XDG_DATA_HOME/Orca`, else
 * `~/.orca`. One JSON file per profile with `worktreeMeta` keyed by worktree id.
 * `agent-hooks/` marks that Orca manages agents even when no state file parses.
 *
 * Superset: `~/.superset/sessions/<workspace>` (each its own git repo) and
 * `host/<org>/manifest.json` with the local host's endpoint and token. The
 * token is deliberately not read: a discovery pass must not use credentials.
 *
 * super.engineering: macOS only, read through `sc list --json`.
 */

import {
  envDirs,
  joinPath,
  readHeaders,
  registerScanner,
  type CollectedSession,
  type ScanContext,
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

registerScanner({
  id: "superset",
  displayName: "Superset",

  async scan(machine: FreshMachine): Promise<ScannerReport> {
    const problems: string[] = [];
    const root = joinPath(machine, machine.home, ".superset");

    const walk = await machine.walkTree(root, {
      includeHidden: true,
      includeDirs: true,
      maxDepth: 3,
      maxEntries: 2000,
    });
    if (walk.entries.length === 0) {
      return { sessions: [], installed: false, problems };
    }

    const sessions: CollectedSession[] = walk.entries
      .filter((e) => e.kind === "dir" && /^sessions\/[^/]+$/.test(e.rel))
      .map((entry) => ({
        id: entry.rel.slice("sessions/".length),
        title: entry.rel.slice("sessions/".length),
        cwd: entry.path,
        path: entry.path,
        mtime: entry.mtime,
        evidence: [
          { locator: entry.path, saying: "Superset session workspace (its own git repo)" },
        ],
      }));

    for (const manifest of walk.entries.filter((e) => e.rel.endsWith("manifest.json"))) {
      problems.push(
        `${manifest.path}: a Superset host manifest is present; its endpoint is not queried and its token is deliberately not read`,
      );
    }

    return { sessions, installed: true, problems };
  },
});

registerScanner({
  id: "super-engineering",
  displayName: "super.engineering",

  async scan(machine: FreshMachine, ctx: ScanContext): Promise<ScannerReport> {
    const problems: string[] = [];

    // Off macOS this is unsupported, not "not installed".
    if (machine.platform !== "macos" && machine.platform !== "") {
      return { sessions: [], installed: false, unsupported: "ships for macOS only", problems };
    }

    // `sc list` is the only source: worktrees live in an app-support database.
    if (!ctx.allowCommands) {
      return { sessions: [], installed: false, problems };
    }

    const listed = await machine.run("sc", ["list", "--json"]);
    if (listed.code !== 0) {
      if (listed.stderr.trim()) problems.push(`sc list --json: ${listed.stderr.trim()}`);
      return { sessions: [], installed: false, problems };
    }

    let rows: unknown;
    try {
      rows = JSON.parse(listed.stdout) as unknown;
    } catch (e) {
      problems.push(`sc list --json: ${e instanceof Error ? e.message : String(e)}`);
      return { sessions: [], installed: true, problems };
    }
    if (!Array.isArray(rows)) {
      problems.push("sc list --json did not return an array");
      return { sessions: [], installed: true, problems };
    }

    const sessions: CollectedSession[] = [];
    for (const row of rows as Record<string, unknown>[]) {
      const id = typeof row["id"] === "string" ? row["id"] : undefined;
      if (!id) continue;
      // The worktree path key varies by build.
      const cwd = ["worktree", "path", "directory"]
        .map((key) => row[key])
        .find((value): value is string => typeof value === "string" && value.length > 0);
      sessions.push({
        id,
        title: typeof row["name"] === "string" ? row["name"] : id,
        cwd,
        evidence: [{ locator: "sc list --json", saying: `super.engineering session ${id}` }],
      });
    }

    return { sessions, installed: true, problems };
  },
});

editor.debug("agent-sessions: Orca, Superset and super.engineering scanners registered");
