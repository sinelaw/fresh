/// <reference path="./lib/fresh.d.ts" />

/**
 * Codex CLI and ChatGPT (Codex) scanners for the agent-sessions hub.
 *
 * Store: `$CODEX_HOME/sessions/YYYY/MM/DD/rollout-<timestamp>-<uuid>.jsonl`,
 * with `CODEX_HOME` defaulting to `~/.codex`. The first record is an envelope:
 * `{"type":"session_meta","payload":{"id":…,"cwd":…}}`, so the cwd is under `payload`.
 *
 * The ChatGPT desktop app's Codex shares the same store. Each transcript's
 * `session_meta` names the app that wrote it (`originator`), so the store is
 * read once and every conversation is listed once, under the app it came
 * from. Both resume with the `codex` CLI.
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

/** One read of the store, shared by the two scanners that split it. */
interface CodexStore {
  sessions: { session: CollectedSession; desktop: boolean }[];
  installed: boolean;
  desktopPresent: boolean;
  problems: string[];
}

// The two scanners run in the same scan; the store is read once for both.
let storeRead: { key: string; at: number; result: Promise<CodexStore> } | null = null;

function readCodexStore(machine: FreshMachine, ctx: ScanContext): Promise<CodexStore> {
  const key = `${machine.home}`;
  const now = Date.now();
  if (storeRead && storeRead.key === key && now - storeRead.at < 5000) return storeRead.result;
  const result = scanCodexStore(machine, ctx);
  storeRead = { key, at: now, result };
  return result;
}

/** Whether a transcript was written by the ChatGPT desktop app, from the
 *  `originator` its `session_meta` records (the CLI's is `codex_cli_rs`). */
function writtenByDesktop(originator: string | undefined): boolean {
  return /desktop|chatgpt|codex_app/i.test(originator ?? "");
}

async function scanCodexStore(machine: FreshMachine, ctx: ScanContext): Promise<CodexStore> {
  const problems: string[] = [];
  const { home, evidence } = await codexHome(machine, problems);
  const sessionsDir = joinPath(machine, home, "sessions");

  // `state_5.sqlite` is written by the desktop app only, so it marks presence.
  const probe = await machine.walkTree(home, {
    includeHidden: true,
    includeDirs: false,
    maxDepth: 1,
    maxEntries: 200,
  });
  const desktopPresent = probe.entries.some((entry) => entry.rel === "state_5.sqlite");

  // Depth 4 is `YYYY/MM/DD/<file>`.
  const walk = await machine.walkTree(sessionsDir, {
    includeHidden: true,
    includeDirs: false,
    maxDepth: 4,
    maxEntries: 5001,
  });
  if (walk.entries.length === 0) {
    return { sessions: [], installed: false, desktopPresent, problems };
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

  const sessions = headers.map(({ entry, text }) => {
    const records = text ? jsonRecords(text) : [];
    // `pickStringFrom` also looks inside a `payload` envelope.
    const cwd = pickStringFrom(records, ["cwd", "workdir", "working_directory"]);
    const recordedId = pickStringFrom(records, ["id", "session_id"]);
    const originator = pickStringFrom(records, ["originator"]);

    // The uuid is the last five dash-separated groups; the timestamp's own dashes come first.
    const stem = (entry.rel.split("/").pop() ?? "").replace(/\.jsonl$/i, "");
    const groups = stem.split("-");
    const uuid = groups.length >= 5 ? groups.slice(-5).join("-") : stem;

    const session: CollectedSession = {
      id: recordedId || uuid,
      cwd,
      path: entry.path,
      mtime: entry.mtime,
      // Codex has no id-addressed resume, only `resume --last`, so resuming
      // this row is exact only when it is the newest in its directory.
      agent: "codex",
      evidence: [{ locator: entry.path, saying: "Codex rollout transcript" }],
    };
    return { session, desktop: writtenByDesktop(originator) };
  });

  return { sessions, installed: true, desktopPresent, problems };
}

registerScanner({
  id: "codex-cli",
  displayName: "Codex CLI",

  async scan(machine: FreshMachine, ctx: ScanContext): Promise<ScannerReport> {
    const store = await readCodexStore(machine, ctx);
    const sessions = store.sessions.filter((s) => !s.desktop).map((s) => s.session);
    return {
      sessions,
      installed: store.installed && (sessions.length > 0 || !store.desktopPresent),
      problems: [...store.problems],
    };
  },
});

registerScanner({
  id: "codex-desktop",
  displayName: "ChatGPT (Codex)",

  async scan(machine: FreshMachine, ctx: ScanContext): Promise<ScannerReport> {
    const store = await readCodexStore(machine, ctx);
    const sessions = store.sessions.filter((s) => s.desktop).map((s) => s.session);
    // The store's problems are reported once, by the CLI scanner.
    return { sessions, installed: store.desktopPresent || sessions.length > 0, problems: [] };
  },
});

editor.debug("agent-sessions: Codex scanners registered");

export { codexHome };
