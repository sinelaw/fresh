/// <reference path="./lib/fresh.d.ts" />

/**
 * Codex CLI and Codex Desktop scanners for the agent-sessions hub.
 *
 * Store: `$CODEX_HOME/sessions/YYYY/MM/DD/rollout-<timestamp>-<uuid>.jsonl`,
 * with `CODEX_HOME` defaulting to `~/.codex`. The first record is an envelope:
 * `{"type":"session_meta","payload":{"id":…,"cwd":…}}`, so the cwd is under `payload`.
 *
 * Codex Desktop shares the same store and adds `state_5.sqlite` beside it.
 * Transcripts are reported once, by the CLI scanner; the desktop scanner
 * reports presence only, to avoid listing each conversation twice.
 */

import {
  envDirs,
  joinPath,
  jsonRecords,
  pickStringFrom,
  readHeaders,
  registerScanner,
  type CollectedSession,
  type Evidence,
  type ScanContext,
  type ScannerReport,
} from "./lib/agent_scanner.ts";

const editor = getEditor();

const HEADER_BYTES = 64 * 1024;

/** `$CODEX_HOME`, or `~/.codex`. */
async function codexHome(
  machine: FreshMachine,
  problems: string[],
): Promise<{ home: string; evidence: Evidence[] }> {
  const evidence: Evidence[] = [];
  const value = (await envDirs(machine, ["CODEX_HOME"], problems))["CODEX_HOME"];
  if (value !== undefined) {
    // Evidence only when the variable actually moved the store.
    evidence.push({ locator: "CODEX_HOME", saying: value });
    return { home: value, evidence };
  }
  return { home: joinPath(machine, machine.home, ".codex"), evidence };
}

registerScanner({
  id: "codex-cli",
  displayName: "Codex CLI",

  async scan(machine: FreshMachine, ctx: ScanContext): Promise<ScannerReport> {
    const problems: string[] = [];
    const { home, evidence } = await codexHome(machine, problems);
    const sessionsDir = joinPath(machine, home, "sessions");

    // Depth 4 is `YYYY/MM/DD/<file>`.
    const walk = await machine.walkTree(sessionsDir, {
      includeHidden: true,
      includeDirs: false,
      maxDepth: 4,
      maxEntries: 5001,
    });
    if (walk.entries.length === 0) {
      return { sessions: [], installed: false, problems };
    }
    if (walk.truncated) {
      problems.push(`${sessionsDir}: stopped at the walk limit, so some sessions are not reported`);
    }
    evidence.push({
      locator: sessionsDir,
      saying: "rollout transcripts, sharded YYYY/MM/DD",
    });

    const rollouts = walk.entries.filter(
      (e) => /(^|\/)rollout-.*\.jsonl$/i.test(e.rel),
    );
    // Newest first, capped before the read.
    const headers = await readHeaders(machine, rollouts, {
      maxBytes: HEADER_BYTES,
      limit: ctx.maxSessions,
      problems,
    });

    const sessions: CollectedSession[] = headers.map(({ entry, text }) => {
      const records = text ? jsonRecords(text) : [];
      // `pickStringFrom` also looks inside a `payload` envelope.
      const cwd = pickStringFrom(records, ["cwd", "workdir", "working_directory"]);
      const recordedId = pickStringFrom(records, ["id", "session_id"]);

      // The uuid is the last five dash-separated groups; the timestamp's own dashes come first.
      const stem = (entry.rel.split("/").pop() ?? "").replace(/\.jsonl$/i, "");
      const groups = stem.split("-");
      const uuid = groups.length >= 5 ? groups.slice(-5).join("-") : stem;

      return {
        id: recordedId || uuid,
        cwd,
        path: entry.path,
        mtime: entry.mtime,
        // Codex has no id-addressed resume, only `resume --last`, so resuming
        // this row is exact only when it is the newest in its directory.
        agent: "codex",
        evidence: [{ locator: entry.path, saying: "Codex rollout transcript" }],
      };
    });

    return { sessions, installed: true, problems };
  },
});

registerScanner({
  id: "codex-desktop",
  displayName: "Codex Desktop",

  async scan(machine: FreshMachine): Promise<ScannerReport> {
    const problems: string[] = [];
    const { home } = await codexHome(machine, problems);

    // `state_5.sqlite` is written by the desktop app only, so it marks presence.
    const probe = await machine.walkTree(home, {
      includeHidden: true,
      includeDirs: false,
      maxDepth: 1,
      maxEntries: 200,
    });
    const present = probe.entries.some((entry) => entry.rel === "state_5.sqlite");

    return { sessions: [], installed: present, problems };
  },
});

editor.debug("agent-sessions: Codex scanners registered");

export { codexHome };
