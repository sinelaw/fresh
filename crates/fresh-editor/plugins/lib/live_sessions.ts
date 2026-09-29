/// <reference path="./fresh.d.ts" />

/**
 * The dock's "Elsewhere" group: Claude and Codex sessions that are open right
 * now but not in a workspace here — running in another terminal, as a
 * background job, or in the vendor's cloud.
 *
 * The Import dialog answers "what has ever run on this machine" from the
 * tools' transcript stores. This answers "what is open", from each tool's own
 * live listing, and includes cloud sessions this machine never saw:
 *
 *  - Claude on this machine: `claude agents --json` (interactive, desktop and
 *    `--bg` sessions, with their status).
 *  - Claude cloud and Remote Control: the session list behind
 *    `claude --teleport`, read with the Claude CLI's own sign-in. Not a
 *    published API; see `CLAUDE_CLOUD_SESSIONS_URL`.
 *  - Codex Cloud: `codex cloud list --json`.
 *  - Codex on this machine: running `codex` processes (Codex has no listing
 *    command for them), from `ps`.
 *
 * Pure: parsing, filtering and "what opening a row does". The polling lives in
 * `live_sessions.ts`, the rows in the orchestrator's dock.
 */

export type LiveSource = "claude-local" | "claude-cloud" | "codex-local" | "codex-cloud";

/** Same vocabulary as the dock's workspace glyphs. */
export type LiveState = "working" | "blocked" | "done" | "idle" | "unknown";

export interface LiveSession {
  /** `source/id`: stable across polls, unique across sources. */
  key: string;
  source: LiveSource;
  /** The tool's own id: a Claude session id, a Codex task id, a pid for a
   *  Codex process (which has no id a listing reports). */
  id: string;
  agent: "claude" | "codex";
  where: "local" | "cloud";
  title: string;
  state: LiveState;
  /** Local sessions only: the directory the agent runs in. */
  cwd?: string;
  /** Cloud sessions: `owner/name`, or the environment's label. */
  repo?: string;
  /** The page for the session in the vendor's web app. */
  url?: string;
  /** Last activity, epoch ms, when the source reports one. */
  updatedAt?: number;
  pid?: number;
  /** A `claude --bg` job's id: what `claude attach` takes. */
  jobId?: string;
  /** What a waiting session is waiting for, when the source says. */
  waitingFor?: string;
  /** A Claude Remote Control session: it runs on one of the user's own
   *  machines and is driven from claude.ai, so it is in the cloud list. */
  remoteControl?: boolean;
  /** A Remote Control session whose machine is not connected right now. */
  offline?: boolean;
  /** The app a local session runs inside, when it is not a terminal:
   *  "Claude Desktop", "VS Code", … */
  host?: string;
  /** A session with no process running it (a Claude Desktop session not
   *  archived): it can be resumed, not attached to. */
  stopped?: boolean;
  /** The SSH host a Claude Desktop session runs on; its `cwd` is there. */
  sshHost?: string;
}

/** The Claude cloud session list: what `claude --teleport` reads. Not a
 *  published API — it can change under any Claude Code release, which is why
 *  a failure reads as a note in the group's menu, never an error, and the
 *  dock's Menu can switch the source off. */
export const CLAUDE_CLOUD_SESSIONS_URL = "https://api.anthropic.com/v1/code/sessions";

const DAY_MS = 86_400_000;

function asRecord(v: unknown): Record<string, unknown> | null {
  return v !== null && typeof v === "object" && !Array.isArray(v) ? v as Record<string, unknown> : null;
}

function str(v: unknown): string | undefined {
  return typeof v === "string" && v.length > 0 ? v : undefined;
}

function num(v: unknown): number | undefined {
  return typeof v === "number" && Number.isFinite(v) ? v : undefined;
}

/** Epoch ms from a number (ms) or an ISO string. */
function when(v: unknown): number | undefined {
  const n = num(v);
  if (n !== undefined) return n;
  const s = str(v);
  if (!s) return undefined;
  const t = Date.parse(s);
  return Number.isNaN(t) ? undefined : t;
}

function parseJson(text: string): unknown {
  try {
    return JSON.parse(text);
  } catch {
    return null;
  }
}

/** Last path segment, either separator. */
export function liveBaseName(p: string): string {
  const parts = p.split(/[\\/]+/).filter((x) => x.length > 0);
  return parts[parts.length - 1] ?? p;
}

/** `/`, or a Windows drive root (`C:\\`). Never a project: a workspace
 *  there would stand for every session that happens to run there. */
export function isFilesystemRoot(p: string): boolean {
  return /^([A-Za-z]:)?[\\/]*$/.test(p.trim());
}

/** A directory compared as a key: separators unified, no trailing one. */
export function liveNormPath(p: string): string {
  const s = p.replace(/\\/g, "/");
  return s.length > 1 ? s.replace(/\/+$/, "") : s;
}

/** Too old to be "open": the source's age cap, 0 meaning none. */
function tooOld(updatedAt: number | undefined, now: number, maxAgeDays: number): boolean {
  return maxAgeDays > 0 && updatedAt !== undefined && now - updatedAt > maxAgeDays * DAY_MS;
}

/** The Claude words for a session's activity, in the dock's vocabulary. */
function claudeState(v: string | undefined): LiveState {
  switch ((v ?? "").toLowerCase()) {
    case "busy":
    case "working":
    case "running":
    case "active":
      return "working";
    case "waiting":
    case "blocked":
    case "requires_action":
    case "needs_input":
      return "blocked";
    case "completed":
    case "done":
      return "done";
    case "idle":
      return "idle";
    default:
      return "unknown";
  }
}

// ── Claude on this machine ─────────────────────────────────────────

/** `claude agents --json`: one entry per open session, interactive or
 *  background. An entry without a session id is not one this can name. */
export function parseClaudeAgents(stdout: string): LiveSession[] {
  const data = parseJson(stdout);
  if (!Array.isArray(data)) return [];
  const out: LiveSession[] = [];
  for (const raw of data) {
    const e = asRecord(raw);
    if (!e) continue;
    const sessionId = str(e.sessionId);
    const background = e.kind === "background";
    // A background entry's `id` is its job id, which `claude attach` takes.
    const jobId = background ? str(e.id) : undefined;
    const id = sessionId ?? jobId;
    if (!id) continue;
    const cwd = str(e.cwd);
    out.push({
      key: `claude-local/${id}`,
      source: "claude-local",
      id,
      agent: "claude",
      where: "local",
      title: str(e.name) ??
        (cwd && !isFilesystemRoot(cwd) ? liveBaseName(cwd) : `claude ${id.slice(0, 8)}`),
      // A background entry carries its own rolled-up `state`; an
      // interactive one only its `status`.
      state: claudeState(str(e.state) ?? str(e.status)),
      cwd,
      updatedAt: when(e.startedAt),
      pid: num(e.pid),
      jobId,
      waitingFor: str(e.waitingFor),
    });
  }
  return out;
}

/** The app behind an SDK-driven session, from the `entrypoint` the CLI
 *  records for it. */
export function claudeHostName(entrypoint: string | undefined): string {
  const e = (entrypoint ?? "").toLowerCase();
  if (e.includes("desktop")) return "Claude Desktop";
  if (e.includes("vscode")) return "VS Code";
  if (e.includes("jetbrains") || e.includes("intellij")) return "JetBrains";
  return "SDK";
}

/** Sessions in the CLI's registry (`~/.claude/sessions/<pid>.json`, one
 *  record per running `claude`) that `claude agents --json` leaves out: it
 *  reports only `interactive` and `bg` sessions, so an SDK-driven one —
 *  Claude Desktop's, an editor extension's — is missing from it. Only a
 *  record whose process is in `alive` counts: a crashed session leaves its
 *  file behind. */
export function parseClaudeRegistry(records: string[], alive: Set<number>): LiveSession[] {
  const out: LiveSession[] = [];
  for (const text of records) {
    const e = asRecord(parseJson(text));
    if (!e) continue;
    const kind = str(e.kind);
    if (kind === "interactive" || kind === "bg") continue;
    const pid = num(e.pid);
    const id = str(e.sessionId);
    if (pid === undefined || !id || !alive.has(pid)) continue;
    const cwd = str(e.cwd);
    const host = claudeHostName(str(e.entrypoint));
    out.push({
      key: `claude-local/${id}`,
      source: "claude-local",
      id,
      agent: "claude",
      where: "local",
      title: str(e.name) ??
        (cwd && !isFilesystemRoot(cwd) ? liveBaseName(cwd) : `claude ${id.slice(0, 8)}`),
      state: claudeState(str(e.status)),
      cwd,
      updatedAt: when(e.updatedAt) ?? when(e.startedAt),
      pid,
      host,
    });
  }
  return out;
}

/** Claude Desktop's Code-tab sessions, from the records it keeps under
 *  `<userData>/claude-code-sessions/<account>/<org>/local_<id>.json`
 *  (`sessionId`, `cliSessionId`, `cwd`, `title`, `createdAt`,
 *  `lastActivityAt`, `isArchived`, and the SSH connection for one that runs
 *  over SSH). An archived one is left out; a deleted one has no file.
 *
 *  Keyed by the CLI session id, the one the CLI's registry uses too, so a
 *  Desktop session that is running merges with its live row (see
 *  `mergeDesktopSessions`). `sshNames` maps a connection id to its host, from
 *  the `sshConfigs` Desktop saves in `~/.claude/settings.json`. */
export function parseDesktopSessions(
  records: string[],
  sshNames: Map<string, string> = new Map(),
): LiveSession[] {
  const out: LiveSession[] = [];
  for (const text of records) {
    const e = asRecord(parseJson(text));
    if (!e || e.isArchived === true) continue;
    const id = str(e.cliSessionId) ?? str(e.sessionId);
    if (!id) continue;
    const cwd = str(e.cwd);
    const ssh = asRecord(e.sshConfig);
    const sshHost = str(ssh?.sshHost) ?? str(ssh?.host) ?? str(ssh?.name) ??
      (str(e.sshConfigId) ? sshNames.get(str(e.sshConfigId)!) ?? str(e.sshConfigId) : undefined);
    out.push({
      key: `claude-local/${id}`,
      source: "claude-local",
      id,
      agent: "claude",
      where: "local",
      title: str(e.title)?.trim() ||
        (cwd && !isFilesystemRoot(cwd) ? liveBaseName(cwd) : `claude ${id.slice(0, 8)}`),
      state: "idle",
      cwd,
      updatedAt: when(e.lastActivityAt) ?? when(e.createdAt),
      host: "Claude Desktop",
      stopped: true,
      ...(sshHost ? { sshHost } : {}),
    });
  }
  return out;
}

/** Desktop's records beside what is running. A Desktop session with a live
 *  process is already listed (from the registry): it keeps its live state and
 *  takes Desktop's title and SSH host. The rest are listed as stopped. */
export function mergeDesktopSessions(running: LiveSession[], desktop: LiveSession[]): LiveSession[] {
  const byId = new Map(desktop.map((d) => [d.id, d]));
  const merged = running.map((r) => {
    const d = byId.get(r.id);
    if (!d) return r;
    byId.delete(r.id);
    return { ...r, title: d.title, host: "Claude Desktop", ...(d.sshHost ? { sshHost: d.sshHost } : {}) };
  });
  return [...merged, ...byId.values()];
}

// ── Claude cloud ──────────────────────────────────────────────────

/** A cloud session in one of these is closed, not open. */
const CLAUDE_CLOUD_CLOSED = new Set(["archived", "cancelled", "canceled", "rejected", "deleted"]);

/** `owner/name` from a git URL (`https://github.com/o/n.git`, `git@host:o/n`). */
export function repoFromUrl(url: string | undefined): string | undefined {
  if (!url) return undefined;
  const m = url.replace(/\.git$/, "").match(/[:/]([^/:]+\/[^/:]+)$/);
  return m ? m[1] : undefined;
}

/** Rows per page asked of the session list, and the most pages read. The
 *  list is newest first, so the pages past the age cap are never needed. */
export const CLAUDE_CLOUD_PAGE_SIZE = 100;
export const CLAUDE_CLOUD_MAX_PAGES = 20;

/** One page's cursor for the next, and the activity time of its last (so
 *  oldest) row, so the walk can stop once pages fall past the age cap. */
export function claudeCloudPageInfo(body: string): { next: string | null; oldest?: number } {
  const root = asRecord(parseJson(body));
  const data = root && Array.isArray(root.data) ? root.data : [];
  const last = asRecord(data[data.length - 1]);
  return {
    next: root ? str(root.next_cursor) ?? null : null,
    oldest: last ? when(last.last_event_at) ?? when(last.created_at) : undefined,
  };
}

/** The id a user sees for a cloud session: the list returns `cse_<x>`, while
 *  claude.ai's links — and what `claude --cloud` / `--teleport` are given —
 *  use `session_<x>`, the conversion the CLI itself makes. */
export function claudeSessionId(apiId: string): string {
  return apiId.startsWith("cse_") ? "session_" + apiId.slice(4) : apiId;
}

/** The session list's JSON (`{ data: [...] }`): every session still active,
 *  i.e. not archived.
 *
 *  Remote Control sessions (`environment_kind: "bridge"`) are in the same
 *  list: a Claude running on one of the user's machines, driven from the web
 *  app. One whose machine is not connected right now
 *  (`connection_status: "disconnected"`) is still active, so it is listed,
 *  marked offline. */
export function parseClaudeCloud(body: string, now: number, maxAgeDays: number): LiveSession[] {
  const root = asRecord(parseJson(body));
  const data = root && Array.isArray(root.data) ? root.data : [];
  const out: LiveSession[] = [];
  for (const raw of data) {
    const e = asRecord(raw);
    const apiId = e ? str(e.id) : undefined;
    if (!e || !apiId) continue;
    const id = claudeSessionId(apiId);
    const status = (str(e.status) ?? "").toLowerCase();
    if (CLAUDE_CLOUD_CLOSED.has(status)) continue;
    const remoteControl = e.environment_kind === "bridge";
    const offline = remoteControl && e.connection_status === "disconnected";
    const updatedAt = when(e.last_event_at) ?? when(e.updated_at) ?? when(e.created_at);
    if (tooOld(updatedAt, now, maxAgeDays)) continue;
    const config = asRecord(e.config);
    const sources = config && Array.isArray(config.sources) ? config.sources : [];
    const git = sources.map(asRecord).find((s) => s?.type === "git_repository");
    out.push({
      key: `claude-cloud/${id}`,
      source: "claude-cloud",
      id,
      agent: "claude",
      where: "cloud",
      title: str(e.title)?.trim() || id,
      // An offline machine is doing nothing anyone can see.
      // `worker_status` is the activity; `status` is only the lifecycle, and
      // its `active` means "not archived", not "working".
      state: offline ? "unknown" : claudeState(str(e.worker_status) ?? (status === "active" ? undefined : status)),
      repo: repoFromUrl(str(git?.url)),
      url: `https://claude.ai/code/${id}`,
      updatedAt,
      ...(remoteControl ? { remoteControl: true } : {}),
      ...(offline ? { offline: true } : {}),
    });
  }
  return out;
}

/** The access token from the Claude CLI's credentials JSON (the
 *  `~/.claude/.credentials.json` file, or the macOS Keychain item's text).
 *  Null when absent or already expired: an expired token is refreshed by the
 *  CLI the next time it runs, never here — refreshing rotates the refresh
 *  token, which would sign the CLI out. */
export function claudeAccessToken(credentials: string, now: number): string | null {
  const root = asRecord(parseJson(credentials.trim()));
  const oauth = root ? asRecord(root.claudeAiOauth) : null;
  const token = oauth ? str(oauth.accessToken) : undefined;
  if (!token) return null;
  const expires = num(oauth!.expiresAt);
  if (expires !== undefined && expires <= now) return null;
  return token;
}

// ── Codex Cloud ───────────────────────────────────────────────────

/** `codex cloud list --json`: `{ tasks: [...], cursor }`. An applied task is
 *  finished with; everything else is open. */
export function parseCodexCloud(stdout: string, now: number, maxAgeDays: number): LiveSession[] {
  const root = asRecord(parseJson(stdout));
  const tasks = root && Array.isArray(root.tasks) ? root.tasks : [];
  const out: LiveSession[] = [];
  for (const raw of tasks) {
    const e = asRecord(raw);
    const id = e ? str(e.id) : undefined;
    if (!e || !id) continue;
    const status = (str(e.status) ?? "").toLowerCase();
    if (status === "applied") continue;
    const updatedAt = when(e.updated_at);
    if (tooOld(updatedAt, now, maxAgeDays)) continue;
    const state: LiveState = status === "pending"
      ? "working"
      : status === "ready"
      ? "done"
      : status === "error"
      ? "blocked"
      : "unknown";
    out.push({
      key: `codex-cloud/${id}`,
      source: "codex-cloud",
      id,
      agent: "codex",
      where: "cloud",
      title: str(e.title) ?? id,
      state,
      repo: str(e.environment_label),
      url: str(e.url),
      updatedAt,
    });
  }
  return out;
}

// ── Codex on this machine ─────────────────────────────────────────

/** `codex` subcommands that are not a conversation: servers, auth, the cloud
 *  browser, tooling. A bare `codex`, `resume`, `fork`, `exec` and `review` are. */
const CODEX_NOT_A_SESSION = new Set([
  "app-server",
  "exec-server",
  "mcp",
  "mcp-server",
  "login",
  "logout",
  "cloud",
  "completion",
  "update",
  "doctor",
  "sandbox",
  "debug",
  "features",
  "plugin",
  "remote-control",
  "agents",
  "apply",
  "archive",
  "unarchive",
  "delete",
  "migrate-rollouts",
  "queue",
  "help",
  "proto",
  "responses-api-proxy",
]);

/** Codex's global options that take a value, so the word after one is that
 *  value and not the subcommand: `codex -c key=value app-server` is the app
 *  server, not a session named `key=value`. */
const CODEX_VALUE_FLAGS = new Set([
  "-c",
  "--config",
  "-m",
  "--model",
  "-p",
  "--profile",
  "-s",
  "--sandbox",
  "-a",
  "--ask-for-approval",
  "-C",
  "--cd",
  "-i",
  "--image",
  "--enable",
  "--disable",
  "--add-dir",
  "--local-provider",
  "--remote",
  "--remote-auth-token-env",
]);

/** Whether a process's argv (`ps` args, whitespace-split) is a Codex
 *  conversation: the program is `codex` (the native binary, or the npm
 *  launcher `codex.js` under node) and the subcommand is not a tool one. */
export function isCodexSessionArgv(argv: string[]): boolean {
  if (argv.length === 0) return false;
  let rest: string[];
  const prog = liveBaseName(argv[0]).toLowerCase();
  if (prog === "codex" || prog === "codex.exe") {
    rest = argv.slice(1);
  } else if (/^node(\.exe)?$/.test(prog) && argv[1] && /(^|[\\/])codex(\.js)?$/i.test(argv[1])) {
    rest = argv.slice(2);
  } else {
    return false;
  }
  let sub: string | undefined;
  for (let i = 0; i < rest.length; i++) {
    const a = rest[i];
    if (a.startsWith("-")) {
      // `--flag value`, not `--flag=value`: skip the value too.
      if (CODEX_VALUE_FLAGS.has(a)) i++;
      continue;
    }
    sub = a;
    break;
  }
  return sub === undefined || !CODEX_NOT_A_SESSION.has(sub);
}

/** A `ps` tty column meaning "no terminal": `?` (Linux), `??` (macOS). */
function noTerminal(tty: string): boolean {
  return tty === "" || tty === "-" || /^\?+$/.test(tty);
}

/** Codex conversations from `ps -Ao pid=,ppid=,tty=,args=`.
 *
 *  Only a process on a terminal is one someone is talking to. The Codex
 *  desktop app and editor extensions run `codex` helpers of their own —
 *  servers, and whatever else a future release adds — with no terminal
 *  (and `/` for a directory); they are not sessions, and listing them gave
 *  a row per helper with nothing to open.
 *
 *  The npm launcher and the native binary it starts are one session: a match
 *  whose parent also matched is dropped in favour of the parent. */
export function parseCodexProcesses(psOut: string): { pid: number; argv: string[] }[] {
  const rows: { pid: number; ppid: number; argv: string[] }[] = [];
  for (const line of psOut.split("\n")) {
    const m = line.trim().match(/^(\d+)\s+(\d+)\s+(\S+)\s+(.*)$/);
    if (!m || noTerminal(m[3])) continue;
    const argv = m[4].split(/\s+/).filter((a) => a.length > 0);
    if (!isCodexSessionArgv(argv)) continue;
    rows.push({ pid: Number(m[1]), ppid: Number(m[2]), argv });
  }
  const pids = new Set(rows.map((r) => r.pid));
  return rows.filter((r) => !pids.has(r.ppid)).map(({ pid, argv }) => ({ pid, argv }));
}

/** `lsof -a -d cwd -Fpn -p <pids>`: `p<pid>` then `n<path>` records. */
export function parseLsofCwds(out: string): Map<number, string> {
  const cwds = new Map<number, string>();
  let pid: number | null = null;
  for (const line of out.split("\n")) {
    if (line.startsWith("p")) pid = Number(line.slice(1));
    else if (line.startsWith("n") && pid !== null) cwds.set(pid, line.slice(1));
  }
  return cwds;
}

/** One row per running Codex conversation, titled by its directory. */
export function codexLocalSessions(
  procs: { pid: number; argv: string[] }[],
  cwds: Map<number, string>,
): LiveSession[] {
  return procs.map(({ pid }) => {
    const cwd = cwds.get(pid);
    const dir = cwd ? liveBaseName(cwd) : "";
    return {
      key: `codex-local/${pid}`,
      source: "codex-local" as const,
      id: String(pid),
      agent: "codex" as const,
      where: "local" as const,
      title: dir && !isFilesystemRoot(dir) ? dir : `codex (pid ${pid})`,
      // A process listing says nothing about activity.
      state: "unknown" as const,
      cwd,
      pid,
    };
  });
}

// ── What the dock does with them ──────────────────────────────────

/** The directory a session with no directory of its own (a cloud one) opens
 *  in as a workspace. One per session, because a workspace is one per
 *  directory: sharing one would fold every cloud session into a single
 *  workspace. Under the editor's data dir, never the user's tree. */
export function elsewhereRoot(dataDir: string, s: LiveSession): string {
  const safe = s.id.replace(/[^A-Za-z0-9_.-]/g, "_");
  return `${dataDir.replace(/[\\/]+$/, "")}/orchestrator/elsewhere/${s.source}-${safe}`;
}

/** What opening a row means. */
export type LivePlan =
  | {
      kind: "workspace";
      root: string;
      label: string;
      /** Argv for the workspace's terminal; absent means a plain shell. */
      command?: string[];
      /** Why no agent was attached, for the status bar. */
      note?: string;
    }
  | { kind: "browser"; url: string }
  /** Take a Claude cloud session over: `claude --teleport <id>` in a local
   *  checkout the user picks (it checks the session's branch out). */
  | { kind: "teleport"; command: string[] }
  /** Nothing to open it with, and why. */
  | { kind: "none"; why: string };

export interface LivePlanEnv {
  dataDir: string;
  /** Program names the user configured for the two CLIs. */
  claude: string;
  codex: string;
  /** No POSIX shell to chain a command into. */
  windows: boolean;
}

/** How a session becomes a workspace (`materialize`) or is opened with Enter.
 *
 *  A cloud session opens its page on Enter. Making one a workspace differs by
 *  vendor: a Claude cloud session is teleported (`claude --cloud <id>`, which
 *  would attach without moving it, is an account-gated feature — "not enabled
 *  for your account" on an ordinary one); a Codex Cloud task, which has no
 *  terminal to attach, becomes a workspace showing its status with a shell
 *  for `codex cloud diff/apply`.
 *
 *  A session running in another terminal is never started a second time —
 *  two processes on one conversation both write it. Its folder opens instead. */
export function livePlan(s: LiveSession, env: LivePlanEnv, materialize: boolean): LivePlan {
  switch (s.source) {
    case "claude-cloud":
      if (!materialize && s.url) return { kind: "browser", url: s.url };
      return { kind: "teleport", command: [env.claude, "--teleport", s.id] };
    case "codex-cloud": {
      if (!materialize && s.url) return { kind: "browser", url: s.url };
      const status = [env.codex, "cloud", "status", s.id];
      return {
        kind: "workspace",
        root: elsewhereRoot(env.dataDir, s),
        label: s.title,
        command: env.windows
          ? status
          : ["sh", "-c", `"$0" cloud status "$1"; exec "\${SHELL:-sh}"`, env.codex, s.id],
      };
    }
    case "claude-local":
      // A Desktop session over SSH runs, and keeps its folder, on that host.
      if (s.sshHost) return { kind: "none", why: `it runs on ${s.sshHost} over SSH` };
      // Not running (a Desktop session left open): resume it here.
      if (s.stopped && s.cwd && !isFilesystemRoot(s.cwd)) {
        return {
          kind: "workspace",
          root: s.cwd,
          label: s.title,
          command: [env.claude, "--resume", s.id],
        };
      }
      if (s.jobId && s.cwd) {
        return {
          kind: "workspace",
          root: s.cwd,
          label: s.title,
          command: [env.claude, "attach", s.jobId],
        };
      }
      return inPlace(s);
    case "codex-local":
      return inPlace(s);
  }
}

/** A session running in another terminal opens as its folder. */
function inPlace(s: LiveSession): LivePlan {
  if (!s.cwd) return { kind: "none", why: "its directory could not be read" };
  if (isFilesystemRoot(s.cwd)) return { kind: "none", why: "it runs in the filesystem root, not a project" };
  return { kind: "workspace", root: s.cwd, label: s.title, note: "running in another terminal" };
}

/** Sessions not already on screen as a workspace. A local session running in
 *  a workspace's directory is that workspace's (the agent in its terminal, or
 *  one beside it); a cloud session opened here lives at its `elsewhereRoot`. */
export function unrepresented(
  sessions: LiveSession[],
  workspaceRoots: Iterable<string>,
  dataDir: string,
): LiveSession[] {
  const roots = new Set<string>();
  for (const r of workspaceRoots) roots.add(liveNormPath(r));
  return sessions.filter((s) => {
    if (s.where === "cloud") return !roots.has(liveNormPath(elsewhereRoot(dataDir, s)));
    // A local session with no directory cannot be matched: keep it.
    return !s.cwd || !roots.has(liveNormPath(s.cwd));
  });
}

/** The short dim tail a row carries: where the session is. */
export function liveDetail(s: LiveSession): string {
  const place = s.remoteControl
    ? s.offline ? "remote control · offline" : "remote control"
    : s.source === "claude-cloud"
    ? "claude.ai"
    : s.source === "codex-cloud"
    ? "codex cloud"
    : s.jobId
    ? "claude --bg"
    : s.host
    ? s.sshHost
      ? `${s.host} · ssh ${s.sshHost}`
      : s.stopped
      ? `${s.host} · stopped`
      : s.host
    : s.pid !== undefined
    ? `pid ${s.pid}`
    : "local";
  const where = s.repo ?? (s.cwd && liveBaseName(s.cwd) !== s.title ? liveBaseName(s.cwd) : undefined);
  return where ? `${where} · ${place}` : place;
}
