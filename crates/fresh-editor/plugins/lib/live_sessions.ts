/// <reference path="./fresh.d.ts" />

/**
 * The dock's "External sessions" group: Claude and Codex sessions that are open right
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
  /** That connection's port and key, when Desktop saved them. */
  sshPort?: number;
  sshIdentity?: string;
  /** The Claude CLI Desktop installed on that host, relative to the home
   *  directory there (`.claude/remote/ccd-cli/<version>`). */
  remoteCli?: string;
  /** Claude Desktop's own id for the session (`local_…`): what its
   *  `claude://code/continue?session=` link opens. */
  desktopId?: string;
  /** The tmux pane a terminal session runs in (`session:@window.%pane`), as
   *  the CLI records it: attaching to it connects to the live session. */
  tmux?: string;
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
/** A session title as a git branch and folder name: lower case, words
 *  joined by `-`, nothing git or a file system would refuse; `session` when
 *  nothing is left. "Fix the auth bug (login.ts)" → `fix-the-auth-bug-login.ts`. */
export function liveBranchName(title: string): string {
  const slug = title
    .normalize("NFKD")
    .replace(/[\u0300-\u036f]/g, "")
    .toLowerCase()
    .replace(/[^a-z0-9._-]+/g, "-")
    .replace(/[-.]{2,}/g, "-")
    .replace(/^[-.]+|[-.]+$/g, "")
    .slice(0, 48)
    .replace(/[-.]+$/g, "");
  return slug || "session";
}

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
    const tmux = str(e.tmux);
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
      ...(tmux && TMUX_PANE_RE.test(tmux) ? { tmux } : {}),
    });
  }
  return out;
}

/** The CLI's own shape for a pane (`session:@window.%pane`); anything else
 *  in the field is not trusted as a tmux target. */
const TMUX_PANE_RE = /^[A-Za-z0-9_.-]{1,64}:@?\d{1,6}\.%?\d{1,6}$/;

/** The tmux pane each running session is in, by session id, from the
 *  registry (`claude agents --json` does not report it). */
export function registryTmuxPanes(records: string[], alive: Set<number>): Map<string, string> {
  const panes = new Map<string, string>();
  for (const text of records) {
    const e = asRecord(parseJson(text));
    const pid = e ? num(e.pid) : undefined;
    const id = e ? str(e.sessionId) : undefined;
    const tmux = e ? str(e.tmux) : undefined;
    if (id && tmux && pid !== undefined && alive.has(pid) && TMUX_PANE_RE.test(tmux)) panes.set(id, tmux);
  }
  return panes;
}

/** `tmux attach` argv that lands on `pane`: its session, then its window and
 *  the pane itself. `TMUX` is cleared so it works from a terminal that is
 *  itself inside tmux (tmux refuses a nested attach otherwise). */
export function tmuxAttachArgv(pane: string): string[] {
  const [session, rest] = [pane.slice(0, pane.lastIndexOf(":")), pane.slice(pane.lastIndexOf(":") + 1)];
  const [win, p] = rest.split(".");
  return ["env", "-u", "TMUX", "tmux", "attach-session", "-t", session, ";", "select-window", "-t", win, ";", "select-pane", "-t", p];
}

/** One SSH connection Claude Desktop saved: `{name, sshHost, sshPort?,
 *  sshIdentityFile?, id}`, in `<userData>/ssh_configs.json` (`configs`) or the
 *  `sshConfigs` of `~/.claude/settings.json`; a session record carries a copy
 *  as its `sshConfig`. */
export interface DesktopSshConnection {
  sshHost: string;
  sshPort?: number;
  sshIdentityFile?: string;
}

function sshConnection(v: unknown): DesktopSshConnection | undefined {
  const c = asRecord(v);
  const sshHost = str(c?.sshHost) ?? str(c?.host);
  if (!sshHost) return undefined;
  const port = num(c?.sshPort);
  const identity = str(c?.sshIdentityFile);
  return { sshHost, ...(port ? { sshPort: port } : {}), ...(identity ? { sshIdentityFile: identity } : {}) };
}

/** Desktop's saved SSH connections by id, from any of the files that hold
 *  them (`{configs: [...]}` or `{sshConfigs: [...]}`). */
export function parseDesktopSshConnections(texts: string[]): Map<string, DesktopSshConnection> {
  const out = new Map<string, DesktopSshConnection>();
  for (const text of texts) {
    const root = asRecord(parseJson(text));
    const list = Array.isArray(root?.configs) ? root!.configs : Array.isArray(root?.sshConfigs) ? root!.sshConfigs : [];
    for (const c of list as unknown[]) {
      const id = str(asRecord(c)?.id);
      const conn = sshConnection(c);
      if (id && conn && !out.has(id)) out.set(id, conn);
    }
  }
  return out;
}

/** The Claude CLI Desktop installed on an SSH host, relative to the home
 *  directory there, from the `hostFacts` Desktop keeps in
 *  `<userData>/ssh-remote-server-state.json` (keyed `ssh:<sshHost>:<port>`). */
export function desktopRemoteCli(stateText: string, sshHost: string, sshPort?: number): string | undefined {
  const facts = asRecord(asRecord(parseJson(stateText))?.hostFacts);
  if (!facts) return undefined;
  const exact = asRecord(facts[`ssh:${sshHost}:${sshPort ?? 22}`]);
  const any = exact ?? Object.entries(facts)
    .filter(([k]) => k === `ssh:${sshHost}` || k.startsWith(`ssh:${sshHost}:`))
    .map(([, v]) => asRecord(v))
    .find((v) => v !== null) ?? null;
  const rel = str(any?.cliRelPath);
  // Relative to the home directory, inside it: nothing else is Desktop's.
  return rel && !rel.startsWith("/") && !rel.split("/").includes("..") ? rel : undefined;
}

/** Claude Desktop's Code-tab sessions, from the records it keeps under
 *  `<userData>/claude-code-sessions/<account>/<org>/local_<id>.json`
 *  (`sessionId`, `cliSessionId`, `cwd`, `worktreePath`, `title`, `createdAt`,
 *  `lastActivityAt`, `isArchived`, and `sshConfig` — the connection — for
 *  one that runs over SSH). An archived one is left out; a deleted one has no
 *  file. The CLI runs in the worktree when the session has one.
 *
 *  Keyed by the CLI session id, the one the CLI's registry uses too, so a
 *  Desktop session that is running merges with its live row (see
 *  `mergeDesktopSessions`). `sshNames` maps a connection id to the connection
 *  (see `parseDesktopSshConnections`), for a record that names only its id. */
export function parseDesktopSessions(
  records: string[],
  sshNames: Map<string, DesktopSshConnection | string> = new Map(),
): LiveSession[] {
  const out: LiveSession[] = [];
  for (const text of records) {
    const e = asRecord(parseJson(text));
    if (!e || e.isArchived === true) continue;
    const id = str(e.cliSessionId) ?? str(e.sessionId);
    if (!id) continue;
    const desktopId = str(e.sessionId);
    const cwd = str(e.worktreePath) ?? str(e.cwd);
    const named = str(e.sshConfigId) ? sshNames.get(str(e.sshConfigId)!) : undefined;
    const conn = sshConnection(e.sshConfig) ??
      (typeof named === "string" ? { sshHost: named } : named) ??
      (str(asRecord(e.sshConfig)?.name) ? { sshHost: str(asRecord(e.sshConfig)?.name)! } : undefined);
    const sshHost = conn?.sshHost ?? str(e.sshConfigId);
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
      ...(conn?.sshPort ? { sshPort: conn.sshPort } : {}),
      ...(conn?.sshIdentityFile ? { sshIdentity: conn.sshIdentityFile } : {}),
      ...(desktopId ? { desktopId } : {}),
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
    return {
      ...r,
      title: d.title,
      host: "Claude Desktop",
      ...(d.sshHost ? { sshHost: d.sshHost } : {}),
      ...(d.sshPort ? { sshPort: d.sshPort } : {}),
      ...(d.sshIdentity ? { sshIdentity: d.sshIdentity } : {}),
      ...(d.remoteCli ? { remoteCli: d.remoteCli } : {}),
      ...(d.desktopId ? { desktopId: d.desktopId } : {}),
    };
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
export function externalRoot(dataDir: string, s: LiveSession): string {
  const safe = s.id.replace(/[^A-Za-z0-9_.-]/g, "_");
  return `${dataDir.replace(/[\\/]+$/, "")}/orchestrator/external-sessions/${s.source}-${safe}`;
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
      /** It stops the copy running elsewhere and continues the session
       *  here (see `takeoverArgv`). */
      takeover?: boolean;
    }
  /** Take a session over on the SSH host it runs on: a workspace there, in
   *  its folder, running `command`. `target` is `[user@]host[:port]`. */
  | { kind: "ssh"; target: string; identity?: string; path: string; label: string; command: string[] }
  /** A link the OS opens: a web page, or a `claude://` link Claude Desktop
   *  handles. */
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
 *  A Claude cloud session (Remote Control included) is taken over with
 *  `claude --teleport <id>` either way (`claude --cloud <id>`, which would
 *  attach without moving it, is an account-gated feature — "not enabled for
 *  your account" on an ordinary one). A Codex Cloud task opens its page on
 *  Enter; as a workspace, having no terminal to attach, it shows its status
 *  with a shell for `codex cloud diff/apply`.
 *
 *  A session running in another terminal is never started a second time —
 *  two processes on one conversation both write it. Its folder opens instead. */
export function livePlan(s: LiveSession, env: LivePlanEnv, materialize: boolean): LivePlan {
  switch (s.source) {
    case "claude-cloud":
      // Taken over, whether opened or filed: its page stays one menu item
      // away ("Open in Browser").
      return { kind: "teleport", command: [env.claude, "--teleport", s.id] };
    case "codex-cloud": {
      if (!materialize && s.url) return { kind: "browser", url: s.url };
      const status = [env.codex, "cloud", "status", s.id];
      return {
        kind: "workspace",
        root: externalRoot(env.dataDir, s),
        label: s.title,
        command: env.windows
          ? status
          : ["sh", "-c", `"$0" cloud status "$1"; exec "\${SHELL:-sh}"`, env.codex, s.id],
      };
    }
    case "claude-local":
      // A session in a tmux pane: attach to the pane, which is the live
      // session itself, not a copy of it.
      if (s.tmux && !s.stopped) {
        return {
          kind: "workspace",
          root: s.cwd && !isFilesystemRoot(s.cwd) ? s.cwd : externalRoot(env.dataDir, s),
          label: s.title,
          command: tmuxAttachArgv(s.tmux),
        };
      }
      // A Claude Desktop session over SSH runs, and keeps its conversation,
      // on that host: take it over there, with the CLI Desktop installed.
      if (s.sshHost) {
        if (!s.cwd || isFilesystemRoot(s.cwd)) {
          return { kind: "none", why: `it runs on ${s.sshHost} over SSH, in a folder Claude Desktop did not record` };
        }
        return {
          kind: "ssh",
          target: sshTargetOf(s)!,
          ...(s.sshIdentity ? { identity: s.sshIdentity } : {}),
          path: s.cwd,
          label: s.title,
          command: takeoverArgv(s.id, s.remoteCli ?? ""),
        };
      }
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
      // A Claude Desktop session running in Desktop: no terminal to attach
      // to, so take it over — stop Desktop's copy, resume it here. Without
      // a POSIX shell to do that, Desktop's own link to it (the menu has it).
      if (s.desktopId && s.cwd && !isFilesystemRoot(s.cwd)) {
        if (env.windows) {
          return materialize
            ? inPlace(s)
            : { kind: "browser", url: `claude://code/continue?session=${encodeURIComponent(s.desktopId)}` };
        }
        return { kind: "workspace", root: s.cwd, label: s.title, command: takeoverArgv(s.id, env.claude), takeover: true };
      }
      return inPlace(s);
    case "codex-local":
      return inPlace(s);
  }
}

/** Take a Claude session over where it runs: stop the copy running now, if
 *  any, then resume the conversation in this terminal. The copy is found in
 *  the CLI's own registry of running sessions (`<config>/sessions/<pid>.json`,
 *  which names the session), and only a pid still running `claude` is
 *  stopped: a registry file can outlive its process, and its pid be reused.
 *  `cli` is the Claude CLI to resume with: a program name, an absolute path,
 *  or a path relative to the home directory (where Claude Desktop installs
 *  its own on an SSH host); `claude` when empty or not there. POSIX `sh`. */
export function takeoverArgv(id: string, cli: string): string[] {
  const script = [
    'id=$1; cli=$2',
    'case $cli in "") cli=claude ;; /*) ;; */*) if [ -x "$HOME/$cli" ]; then cli="$HOME/$cli"; else cli=claude; fi ;; esac',
    'for f in "${CLAUDE_CONFIG_DIR:-$HOME/.claude}"/sessions/*.json; do',
    '  [ -f "$f" ] && grep -q "$id" "$f" || continue',
    '  pid=$(basename "$f" .json)',
    '  case "$(ps -p "$pid" -o args= 2>/dev/null)" in *claude*) ;; *) continue ;; esac',
    '  echo "Taking over: stopping the copy running now (pid $pid)"',
    '  kill "$pid"',
    '  i=0; while kill -0 "$pid" 2>/dev/null && [ $i -lt 50 ]; do sleep 0.1; i=$((i+1)); done',
    'done',
    'exec "$cli" --resume "$id"',
  ].join("\n");
  return ["sh", "-c", script, "sh", id, cli];
}

/** `[user@]host[:port]` for an SSH connection Desktop saved. */
export function sshTargetOf(s: LiveSession): string | undefined {
  if (!s.sshHost) return undefined;
  return s.sshPort && s.sshPort !== 22 ? `${s.sshHost}:${s.sshPort}` : s.sshHost;
}

/** A session running in another terminal opens as its folder. */
function inPlace(s: LiveSession): LivePlan {
  if (!s.cwd) return { kind: "none", why: "its directory could not be read" };
  if (isFilesystemRoot(s.cwd)) return { kind: "none", why: "it runs in the filesystem root, not a project" };
  return { kind: "workspace", root: s.cwd, label: s.title, note: "running in another terminal" };
}

/** Sessions not already on screen as a workspace. A local session running in
 *  a workspace's directory is that workspace's (the agent in its terminal, or
 *  one beside it); a cloud session opened here lives at its `externalRoot`;
 *  a session on an SSH host is a workspace on that host, in its folder
 *  (`remoteRoots`: `[user@]host` without the port, and the remote root). */
export function unrepresented(
  sessions: LiveSession[],
  workspaceRoots: Iterable<string>,
  dataDir: string,
  remoteRoots: Iterable<{ host: string; root: string }> = [],
): LiveSession[] {
  const roots = new Set<string>();
  for (const r of workspaceRoots) roots.add(liveNormPath(r));
  const remote = new Set<string>();
  for (const r of remoteRoots) remote.add(`${r.host}\n${liveNormPath(r.root)}`);
  return sessions.filter((s) => {
    if (s.where === "cloud") return !roots.has(liveNormPath(externalRoot(dataDir, s)));
    // A local session with no directory cannot be matched: keep it.
    if (!s.cwd) return true;
    if (s.sshHost) return !remote.has(`${s.sshHost}\n${liveNormPath(s.cwd)}`);
    return !roots.has(liveNormPath(s.cwd));
  });
}

/** The short dim tail a row carries: where the session is. */
/** Where a session is listed from and runs, for its menu: "Claude on
 *  claude.ai", "Claude Desktop over SSH (me@box)", "Codex Cloud", … */
export function liveSource(s: LiveSession): string {
  switch (s.source) {
    case "claude-cloud":
      return s.remoteControl
        ? `Claude Remote Control${s.offline ? " (machine offline)" : ""}`
        : "Claude on claude.ai";
    case "codex-cloud":
      return "Codex Cloud";
    case "codex-local":
      return "Codex in a terminal";
    case "claude-local": {
      if (s.jobId) return "Claude background job (claude --bg)";
      if (s.host) {
        const where = s.sshHost ? `${s.host} over SSH (${s.sshHost})` : s.host;
        return s.stopped ? `${where}, not running` : where;
      }
      return s.tmux ? "Claude in a terminal (tmux)" : "Claude in a terminal";
    }
  }
}

/** The folder a session runs in, or the repository a cloud one works on. */
export function liveWhere(s: LiveSession): string | undefined {
  return s.cwd ?? s.repo;
}

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
