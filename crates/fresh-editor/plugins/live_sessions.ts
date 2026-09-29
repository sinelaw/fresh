/// <reference path="./lib/fresh.d.ts" />

/**
 * Keeps the orchestrator dock's "Elsewhere" group current: Claude and Codex
 * sessions open outside this editor's workspaces — another terminal, a
 * background job, the vendor's cloud. Polls each tool's own listing while the
 * dock is open and hands the result to the orchestrator
 * (`setElsewhereSessions`), which draws the rows and opens them.
 *
 * A separate plugin rather than part of the orchestrator so the sources can be
 * turned off by not loading it, and so the orchestrator's own tests, which load
 * only the orchestrator, never see this machine's real sessions.
 *
 * Parsing and "what opening a row means" live in `lib/live_sessions.ts`.
 */

import {
  CLAUDE_CLOUD_MAX_PAGES,
  CLAUDE_CLOUD_PAGE_SIZE,
  CLAUDE_CLOUD_SESSIONS_URL,
  claudeAccessToken,
  claudeCloudPageInfo,
  codexLocalSessions,
  parseClaudeAgents,
  parseClaudeCloud,
  parseClaudeRegistry,
  parseCodexCloud,
  parseCodexProcesses,
  parseLsofCwds,
  type LiveSession,
  type LiveSource,
} from "./lib/live_sessions.ts";

const editor = getEditor();

editor.defineConfigBoolean("enabled", {
  default: true,
  description:
    "Show Claude and Codex sessions that are open outside this editor (another terminal, a background job, the cloud) in the orchestrator dock's Elsewhere group.",
});
editor.defineConfigBoolean("claudeLocal", {
  default: true,
  description: "List Claude sessions running on this machine, from `claude agents --json`.",
});
editor.defineConfigBoolean("claudeCloud", {
  default: true,
  description:
    "List your Claude Code cloud and Remote Control sessions (claude.ai/code). Reads the Claude CLI's sign-in (~/.claude/.credentials.json, or the macOS Keychain, which may ask once) and calls the session list `claude --teleport` uses — not a published API, so it may change with any Claude Code release. Also a checkbox in the dock's Menu.",
});
editor.defineConfigBoolean("codexLocal", {
  default: true,
  description: "List Codex sessions running on this machine, found among running processes (not on Windows).",
});
editor.defineConfigBoolean("codexCloud", {
  default: true,
  description: "List open Codex Cloud tasks, from `codex cloud list --json`.",
});
editor.defineConfigString("claudeCommand", {
  default: "claude",
  description: "The Claude Code CLI to run: a name on PATH or a full path.",
});
editor.defineConfigString("codexCommand", {
  default: "codex",
  description: "The Codex CLI to run: a name on PATH or a full path.",
});
editor.defineConfigInteger("cloudMaxAgeDays", {
  default: 0,
  minimum: 0,
  maximum: 365,
  description:
    "Hide cloud sessions with no activity for this many days. 0 (the default) shows every session still active; archived ones are never shown.",
});
editor.defineConfigInteger("pollSeconds", {
  default: 15,
  minimum: 5,
  maximum: 3600,
  description:
    "How often sessions on this machine are re-listed while the dock is open. Cloud lists are fetched at most once a minute.",
});

interface Settings {
  enabled?: boolean;
  claudeLocal?: boolean;
  claudeCloud?: boolean;
  codexLocal?: boolean;
  codexCloud?: boolean;
  claudeCommand?: string;
  codexCommand?: string;
  cloudMaxAgeDays?: number;
  pollSeconds?: number;
}

// Re-read every poll so a Settings edit applies without a reload.
function settings(): Required<Settings> {
  const s = (editor.getPluginConfig() ?? {}) as Settings;
  return {
    enabled: s.enabled ?? true,
    claudeLocal: s.claudeLocal ?? true,
    claudeCloud: s.claudeCloud ?? true,
    codexLocal: s.codexLocal ?? true,
    codexCloud: s.codexCloud ?? true,
    claudeCommand: s.claudeCommand?.trim() || "claude",
    codexCommand: s.codexCommand?.trim() || "codex",
    cloudMaxAgeDays: s.cloudMaxAgeDays ?? 0,
    pollSeconds: s.pollSeconds ?? 15,
  };
}

/** The orchestrator's side. Looked up per push, so load order does not matter. */
interface ElsewhereHost {
  setElsewhereSessions(update: {
    sessions: LiveSession[];
    problems: string[];
    commands: { claude: string; codex: string };
  }): void;
}

export interface LiveSessionsApi {
  /** Re-list now, cloud included. Resolves once the dock has the answer. */
  refresh(): Promise<void>;
  /** The last answer, for a dock opened after it was pushed. */
  snapshot(): { sessions: LiveSession[]; problems: string[]; commands: { claude: string; codex: string } };
  /** Whether the Claude cloud source is on. */
  claudeCloudEnabled(): boolean;
  /** Turn it on or off: saved to the user's config (as the Settings UI
   *  would), then re-listed, so the dock's Menu can own the setting. */
  setClaudeCloud(on: boolean): Promise<void>;
}

declare global {
  interface FreshPluginRegistry {
    "live-sessions": LiveSessionsApi;
  }
}

const WINDOWS = editor.getEnv("OS") === "Windows_NT";
const CLOUD_MIN_INTERVAL_MS = 60_000;
const TICK_MS = 5_000;

// Each source's last answer, kept across polls so a cloud list fetched a
// minute ago still shows between fetches.
const lastBySource = new Map<LiveSource, LiveSession[]>();
const problemBySource = new Map<LiveSource, string>();
const lastRunAt = new Map<LiveSource, number>();
let inFlight: Promise<void> | null = null;

// The Claude sign-in, held until it expires or is refused, so the Keychain
// (which may ask the user) is read once rather than every minute.
let claudeToken: { token: string; readAt: number } | null = null;
// A Keychain read that failed is not retried until a manual refresh.
let keychainRefused = false;

function snapshot(): ReturnType<LiveSessionsApi["snapshot"]> {
  const sessions: LiveSession[] = [];
  for (const list of lastBySource.values()) sessions.push(...list);
  const s = settings();
  return {
    sessions,
    problems: [...problemBySource.values()],
    commands: { claude: s.claudeCommand, codex: s.codexCommand },
  };
}

function push(): void {
  const host = editor.getPluginApi("orchestrator") as ElsewhereHost | null;
  host?.setElsewhereSessions?.(snapshot());
}

/** A spawn that could not start (the tool is not installed) is an empty
 *  answer, not a problem. */
function missingTool(r: SpawnResult): boolean {
  return r.exit_code === -1 || /not found|No such file|cannot find|ENOENT/i.test(r.stderr);
}

/** Where the listing commands run. Not the editor's working directory: that
 *  is the user's project, and `codex cloud` writes an `error.log` into
 *  whatever directory it runs in. */
function probeDir(): string {
  const dir = editor.pathJoin(editor.getDataDir(), "orchestrator", "elsewhere", "probe");
  editor.createDir(editor.localPath(dir));
  return dir;
}

function firstLine(s: string): string {
  return s.trim().split("\n")[0]?.slice(0, 200) ?? "";
}

// ── Sources ───────────────────────────────────────────────────────

/** The Claude CLI's config directory: `$CLAUDE_CONFIG_DIR`, or `~/.claude`. */
function claudeConfigDir(): string {
  return editor.getEnv("CLAUDE_CONFIG_DIR") || editor.pathJoin(editor.getHomeDir(), ".claude");
}

/** Claude sessions running here: `claude agents --json` (terminal and `--bg`
 *  sessions, with a background job's state), plus what it leaves out — the
 *  SDK-driven sessions Claude Desktop and editor extensions run — read from
 *  the CLI's registry of running sessions. */
async function listClaudeLocal(s: Required<Settings>): Promise<LiveSession[]> {
  const r = await editor.spawnHostProcess(s.claudeCommand, ["agents", "--json"], probeDir());
  if (r.exit_code !== 0 && !missingTool(r)) {
    throw new Error(`claude agents: ${firstLine(r.stderr) || `exit ${r.exit_code}`}`);
  }
  const listed = r.exit_code === 0 ? parseClaudeAgents(r.stdout) : [];
  const seen = new Set(listed.map((x) => x.id));
  return [...listed, ...(await listClaudeRegistry()).filter((x) => !seen.has(x.id))];
}

async function listClaudeRegistry(): Promise<LiveSession[]> {
  // A pid is checked with `ps`, which Windows lacks; there the registry's
  // SDK sessions go unlisted.
  if (WINDOWS) return [];
  const dir = editor.pathJoin(claudeConfigDir(), "sessions");
  const records: string[] = [];
  const pids: number[] = [];
  for (const entry of editor.readDir(editor.localPath(dir))) {
    if (!entry.is_file || !/^\d+\.json$/.test(entry.name)) continue;
    const text = editor.readFile(editor.localPath(editor.pathJoin(dir, entry.name)));
    if (!text) continue;
    records.push(text);
    pids.push(Number(entry.name.slice(0, -".json".length)));
  }
  if (pids.length === 0) return [];
  // Which of them are still running: a session that crashed leaves its file.
  const ps = await editor.spawnHostProcess("ps", ["-o", "pid=", "-p", pids.join(",")]);
  const alive = new Set(
    ps.stdout.split("\n").map((l) => Number(l.trim())).filter((n) => n > 0),
  );
  return parseClaudeRegistry(records, alive);
}

/** The Claude CLI's sign-in: its credentials file, else (macOS) its Keychain
 *  item. Never refreshed here (see `claudeAccessToken`). */
async function readClaudeToken(force: boolean): Promise<string | null> {
  const now = Date.now();
  if (claudeToken && now - claudeToken.readAt < 10 * 60_000) return claudeToken.token;
  const configDir = claudeConfigDir();
  const file = editor.readFile(editor.localPath(editor.pathJoin(configDir, ".credentials.json")));
  let token = file ? claudeAccessToken(file, now) : null;
  if (!token && !file && !WINDOWS && (force || !keychainRefused)) {
    const r = await editor.spawnHostProcess("security", [
      "find-generic-password",
      "-s",
      "Claude Code-credentials",
      "-w",
    ]);
    token = r.exit_code === 0 ? claudeAccessToken(r.stdout, now) : null;
    keychainRefused = r.exit_code !== 0;
  }
  claudeToken = token ? { token, readAt: now } : null;
  return token;
}

async function listClaudeCloud(s: Required<Settings>, force: boolean): Promise<LiveSession[]> {
  const token = await readClaudeToken(force);
  if (!token) {
    throw new Error("Claude cloud: not signed in, or the sign-in expired — run `claude` once to refresh it");
  }
  const target = editor.pathJoin(probeDir(), "claude-cloud-page.json");
  const headers = {
    Authorization: `Bearer ${token}`,
    "anthropic-version": "2023-06-01",
    "Content-Type": "application/json",
  };
  // The list is paged and newest first: walk it until the cursor runs out,
  // or its rows fall past the age cap (older pages hold nothing shown).
  const now = Date.now();
  const cutoff = s.cloudMaxAgeDays > 0 ? now - s.cloudMaxAgeDays * 86_400_000 : null;
  const byId = new Map<string, LiveSession>();
  let cursor: string | null = null;
  for (let page = 0; page < CLAUDE_CLOUD_MAX_PAGES; page++) {
    // `statuses=active` has the server leave archived sessions out, which
    // are most of the list.
    const url = `${CLAUDE_CLOUD_SESSIONS_URL}?limit=${CLAUDE_CLOUD_PAGE_SIZE}&statuses=active` +
      (cursor ? `&cursor=${encodeURIComponent(cursor)}` : "");
    const r = await editor.httpFetch(url, target, headers);
    if (r.exit_code === 401 || r.exit_code === 403) {
      claudeToken = null;
      throw new Error("Claude cloud: the sign-in was refused — run `claude` once to refresh it");
    }
    if (r.exit_code !== 0) throw new Error(`Claude cloud: ${r.stderr || `HTTP ${r.exit_code}`}`);
    const body = editor.readFile(editor.localPath(target)) ?? "";
    for (const row of parseClaudeCloud(body, now, s.cloudMaxAgeDays)) byId.set(row.id, row);
    const info = claudeCloudPageInfo(body);
    cursor = info.next;
    if (!cursor || (cutoff !== null && info.oldest !== undefined && info.oldest < cutoff)) break;
  }
  return [...byId.values()];
}

async function listCodexCloud(s: Required<Settings>): Promise<LiveSession[]> {
  const r = await editor.spawnHostProcess(
    s.codexCommand,
    ["cloud", "list", "--json", "--limit", "20"],
    probeDir(),
  );
  if (r.exit_code !== 0) {
    if (missingTool(r)) return [];
    // Not signed in to ChatGPT is the common case and not an error worth a
    // line in the dock; anything else is.
    if (/not signed in|codex login/i.test(r.stderr)) return [];
    throw new Error(`codex cloud: ${firstLine(r.stderr) || `exit ${r.exit_code}`}`);
  }
  return parseCodexCloud(r.stdout, Date.now(), s.cloudMaxAgeDays);
}

async function listCodexLocal(): Promise<LiveSession[]> {
  if (WINDOWS) return [];
  const ps = await editor.spawnHostProcess("ps", ["-Ao", "pid=,ppid=,tty=,args="]);
  if (ps.exit_code !== 0) return [];
  const procs = parseCodexProcesses(ps.stdout);
  if (procs.length === 0) return [];
  const pids = procs.map((p) => String(p.pid));
  const lsof = await editor.spawnHostProcess("lsof", ["-a", "-d", "cwd", "-Fpn", "-p", pids.join(",")]);
  const cwds = lsof.exit_code === 0 || lsof.stdout ? parseLsofCwds(lsof.stdout) : new Map<number, string>();
  // No lsof (a slim Linux install): the kernel's own record.
  for (const p of procs) {
    if (cwds.has(p.pid)) continue;
    const link = await editor.spawnHostProcess("readlink", [`/proc/${p.pid}/cwd`]);
    if (link.exit_code === 0 && link.stdout.trim()) cwds.set(p.pid, link.stdout.trim());
  }
  return codexLocalSessions(procs, cwds);
}

// ── Polling ───────────────────────────────────────────────────────

interface SourceSpec {
  id: LiveSource;
  on: (s: Required<Settings>) => boolean;
  cloud: boolean;
  list: (s: Required<Settings>, force: boolean) => Promise<LiveSession[]>;
}

const SOURCES: SourceSpec[] = [
  { id: "claude-local", on: (s) => s.claudeLocal, cloud: false, list: (s) => listClaudeLocal(s) },
  { id: "codex-local", on: (s) => s.codexLocal, cloud: false, list: () => listCodexLocal() },
  { id: "claude-cloud", on: (s) => s.claudeCloud, cloud: true, list: (s, f) => listClaudeCloud(s, f) },
  { id: "codex-cloud", on: (s) => s.codexCloud, cloud: true, list: (s) => listCodexCloud(s) },
];

async function pollOnce(force: boolean): Promise<void> {
  const s = settings();
  const now = Date.now();
  if (force) keychainRefused = false;
  let changed = false;
  const due: SourceSpec[] = [];
  for (const src of SOURCES) {
    if (!s.enabled || !src.on(s)) {
      // A source switched off takes its rows with it.
      if (lastBySource.delete(src.id) || problemBySource.delete(src.id)) changed = true;
      continue;
    }
    const every = src.cloud
      ? Math.max(CLOUD_MIN_INTERVAL_MS, s.pollSeconds * 1000)
      : s.pollSeconds * 1000;
    const last = lastRunAt.get(src.id) ?? 0;
    if (force || now - last >= every) due.push(src);
  }
  await Promise.all(due.map(async (src) => {
    lastRunAt.set(src.id, now);
    try {
      lastBySource.set(src.id, await src.list(s, force));
      problemBySource.delete(src.id);
    } catch (e) {
      // Keep the last good answer: a network blip should not empty the group.
      problemBySource.set(src.id, e instanceof Error ? e.message : String(e));
      editor.debug(`live-sessions: ${src.id}: ${problemBySource.get(src.id)}`);
    }
    changed = true;
  }));
  if (changed) push();
}

function poll(force: boolean): Promise<void> {
  if (inFlight) return inFlight;
  inFlight = pollOnce(force).finally(() => {
    inFlight = null;
  });
  return inFlight;
}

// Ticks all the time, polls only while the dock is showing: nothing else
// shows these rows, so a closed dock costs one boolean check.
registerHandler("liveSessionsTick", () => {
  if (editor.dockOpen()) void poll(false);
});
editor.setInterval(TICK_MS, "liveSessionsTick");

registerHandler("live_sessions_refresh", async () => {
  await poll(true);
  const { sessions, problems } = snapshot();
  editor.setStatus(
    problems.length > 0
      ? editor.t("status.refreshed_problems", { n: String(sessions.length), problem: problems[0] })
      : editor.t("status.refreshed", { n: String(sessions.length) }),
  );
});
editor.registerCommand("%cmd.refresh", "%cmd.refresh_desc", "live_sessions_refresh");

async function setClaudeCloud(on: boolean): Promise<void> {
  editor.saveSetting("plugins.live_sessions.settings.claudeCloud", on);
  // The write is applied asynchronously; the source is switched by the
  // re-read in `settings()`, so wait for it before re-listing.
  for (let i = 0; i < 20 && settings().claudeCloud !== on; i++) await editor.delay(50);
  await poll(true);
}

editor.exportPluginApi("live-sessions", {
  refresh: () => poll(true),
  snapshot,
  claudeCloudEnabled: () => settings().claudeCloud,
  setClaudeCloud,
} satisfies LiveSessionsApi);
