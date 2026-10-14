/// <reference path="./lib/fresh.d.ts" />

/**
 * Claude Code scanner for the agent-sessions hub.
 *
 * Store: `~/.claude/projects/<encoded-cwd>/<session-uuid>.jsonl`, with
 * sub-agent transcripts under `<session-uuid>/subagents/`.
 *
 * The encoded directory name is lossy (`~/a-b` and `~/a/b` encode the same),
 * so the cwd and branch are read from the transcript's records, which carry
 * `cwd` and `gitBranch`. The directory name is only a fallback label.
 *
 * Sub-agent transcripts are not listed: they share the parent's id, cannot be
 * resumed on their own, and a busy session has dozens. The walk stops above them.
 */

import {
  joinPath,
  jsonRecords,
  pickStringFrom,
  readHeaders,
  registerScanner,
  type CollectedSession,
  type ScanContext,
  type ScannerReport,
} from "./lib/agent_scanner.ts";
import { encodeProjectDir } from "./lib/claude_code_format.ts";

const editor = getEditor();

// Bounds what a remote scan pulls per transcript.
const HEADER_BYTES = 256 * 1024;

registerScanner({
  id: "claude-code",
  displayName: "Claude Code",

  async scan(machine: FreshMachine, ctx: ScanContext): Promise<ScannerReport> {
    const problems: string[] = [];
    const projects = joinPath(machine, machine.home, ".claude", "projects");

    // Depth 3 covers `<bucket>/<uuid>.jsonl` and `<bucket>/<uuid>/<sub>.jsonl`.
    const walk = await machine.walkTree(projects, {
      includeHidden: true,
      includeDirs: false,
      maxDepth: 3,
      maxEntries: 5001,
    });
    if (walk.entries.length === 0) {
      return { sessions: [], installed: false, problems };
    }
    if (walk.truncated) {
      problems.push(`${projects}: stopped at the walk limit, so some sessions are not reported`);
    }

    const transcripts = walk.entries.filter((e) => e.rel.toLowerCase().endsWith(".jsonl"));
    // Newest first, capped before the read.
    const headers = await readHeaders(machine, transcripts, {
      maxBytes: HEADER_BYTES,
      limit: ctx.maxSessions,
      problems,
    });

    const sessions: CollectedSession[] = [];
    for (const { entry, text } of headers) {

      // `<bucket>/<uuid>.jsonl` is depth 2; deeper is a sub-agent transcript.
      const parts = entry.rel.split("/").filter((p) => p.length > 0);
      const bucket = parts[0] ?? "";
      const isSubagent = parts.length > 2;
      const fileStem = (parts[parts.length - 1] ?? "").replace(/\.jsonl$/i, "");
      const parentId = isSubagent ? parts[parts.length - 2] : undefined;

      // Scan forward, not just the first line: the opening record is often a
      // `queue-operation` with `sessionId` but no `cwd`.
      const records = text ? jsonRecords(text) : [];
      const cwd = pickStringFrom(records, ["cwd"]);
      const branch = pickStringFrom(records, ["gitBranch"]);
      const recordedId = pickStringFrom(records, ["sessionId"]);

      const evidence = [
        {
          locator: entry.path,
          saying: cwd
            ? `Claude Code transcript; cwd read from the transcript itself`
            : `Claude Code transcript under the "${bucket}" bucket; cwd not readable, and the bucket name cannot be decoded back to a path`,
        },
      ];
      if (branch) evidence.push({ locator: entry.path, saying: `git branch ${branch}` });

      sessions.push({
        // A sub-agent shares its parent's id; the file stem is unique.
        id: isSubagent ? `${parentId}/${fileStem}` : recordedId || fileStem,
        title: isSubagent ? `sub-agent of ${parentId?.slice(0, 8) ?? "?"}` : undefined,
        cwd,
        path: entry.path,
        mtime: entry.mtime,
        agentSessionId: isSubagent ? parentId : undefined,
        // Only a real session is resumable: `claude --resume <uuid>` takes this row's `id`.
        agent: isSubagent ? undefined : "claude",
        evidence,
      });
    }

    return { sessions, installed: true, problems };
  },
});

editor.debug("agent-sessions: Claude Code scanner registered");

export { encodeProjectDir };
