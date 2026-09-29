/** The dock's "Elsewhere" rows: each source's listing in, open sessions out. */
import {
  claudeAccessToken,
  claudeCloudPageInfo,
  claudeSessionId,
  parseClaudeRegistry,
  parseDesktopSessions,
  mergeDesktopSessions,
  codexLocalSessions,
  elsewhereRoot,
  isCodexSessionArgv,
  liveDetail,
  livePlan,
  parseClaudeAgents,
  parseClaudeCloud,
  parseCodexCloud,
  parseCodexProcesses,
  parseLsofCwds,
  repoFromUrl,
  unrepresented,
  type LivePlanEnv,
} from "../lib/live_sessions.ts";

let failures = 0;
function eq(actual: unknown, expected: unknown, name: string): void {
  const a = JSON.stringify(actual);
  const e = JSON.stringify(expected);
  if (a !== e) {
    console.log(`FAIL ${name}\n  got      ${a}\n  expected ${e}`);
    failures++;
  } else {
    console.log(`ok   ${name}`);
  }
}

const NOW = Date.parse("2026-09-29T12:00:00Z");
const DAY = 86_400_000;

// ── claude agents --json ──────────────────────────────────────────

const agents = parseClaudeAgents(JSON.stringify([
  { pid: 123, cwd: "/home/u/fresh", kind: "interactive", startedAt: 1790662002314, sessionId: "1bda", name: "fresh-04", status: "busy" },
  { pid: 9, id: "job7", cwd: "/home/u/api", kind: "background", startedAt: 1, sessionId: "5e55", state: "blocked", status: "waiting", waitingFor: "permission" },
  { pid: 10, cwd: "/home/u/x", kind: "interactive", status: "idle" },
  "garbage",
]));
eq(agents.map((s) => s.key), ["claude-local/1bda", "claude-local/5e55"], "claude agents: one row per named session");
eq(agents[0].state, "working", "claude agents: busy is working");
eq(agents[0].title, "fresh-04", "claude agents: the session's name is the title");
eq([agents[1].state, agents[1].jobId, agents[1].waitingFor], ["blocked", "job7", "permission"], "claude agents: a background job keeps its job id and rolled-up state");
eq(parseClaudeAgents("not json"), [], "claude agents: junk output is no sessions");

const registry = parseClaudeRegistry([
  JSON.stringify({ pid: 501, sessionId: "desk1", cwd: "/home/u/site", kind: "sdk", entrypoint: "claude-desktop", status: "busy", name: "Fix the build" }),
  JSON.stringify({ pid: 502, sessionId: "vs1", cwd: "/home/u/api", kind: "sdk", entrypoint: "claude-vscode", status: "idle" }),
  JSON.stringify({ pid: 503, sessionId: "term1", cwd: "/home/u/x", kind: "interactive", status: "idle" }),
  JSON.stringify({ pid: 504, sessionId: "gone1", cwd: "/home/u/y", kind: "sdk", entrypoint: "claude-desktop", status: "idle" }),
  "not json",
], new Set([501, 502, 503]));
eq(registry.map((s) => [s.id, s.title, s.state, s.host]), [["desk1", "Fix the build", "working", "Claude Desktop"], ["vs1", "api", "idle", "VS Code"]], "claude registry: SDK sessions (Desktop, VS Code) that agents skips; not terminal ones, not dead ones");
eq(liveDetail(registry[0]), "site · Claude Desktop", "detail: a Desktop session says so");

const desktop = parseDesktopSessions([
  JSON.stringify({ sessionId: "local_a", cliSessionId: "desk1", cwd: "/home/u/site", title: "Fix the build (Desktop)", createdAt: 1, lastActivityAt: 2 }),
  JSON.stringify({ sessionId: "local_b", cliSessionId: "cli-b", cwd: "/home/u/blog", title: "Draft a post", createdAt: 1, lastActivityAt: 2 }),
  JSON.stringify({ sessionId: "local_c", cliSessionId: "cli-c", cwd: "/srv/app", title: "On the box", createdAt: 1, lastActivityAt: 2, sshConfig: { id: "s1", name: "box", sshHost: "me@box" } }),
  JSON.stringify({ sessionId: "local_d", cliSessionId: "cli-d", cwd: "/home/u/old", title: "Archived", createdAt: 1, lastActivityAt: 2, isArchived: true }),
]);
eq(desktop.map((d) => [d.id, d.title, d.sshHost ?? null]), [["desk1", "Fix the build (Desktop)", null], ["cli-b", "Draft a post", null], ["cli-c", "On the box", "me@box"]], "desktop: every Code-tab session not archived, keyed by its CLI session id");
const merged = mergeDesktopSessions(registry, desktop);
eq(merged.map((m) => [m.id, m.title, m.state, m.stopped ?? false]), [["desk1", "Fix the build (Desktop)", "working", false], ["vs1", "api", "idle", false], ["cli-b", "Draft a post", "idle", true], ["cli-c", "On the box", "idle", true]], "desktop: a running one keeps its live state and takes its Desktop title; the rest are stopped");
eq([liveDetail(desktop[1]), liveDetail(desktop[2])], ["blog · Claude Desktop · stopped", "srv/app".split("/").pop() + " · Claude Desktop · ssh me@box"], "detail: stopped and SSH Desktop sessions say so");

// ── Claude cloud ──────────────────────────────────────────────────

const cloudBody = JSON.stringify({
  data: [
    {
      id: "session_01",
      title: "Show all sessions in the dock",
      status: "active",
      worker_status: "working",
      last_event_at: new Date(NOW - 3600_000).toISOString(),
      config: { sources: [{ type: "git_repository", url: "https://github.com/sinelaw/fresh.git" }] },
    },
    { id: "session_02", title: "old", status: "active", worker_status: "idle", last_event_at: new Date(NOW - 30 * DAY).toISOString() },
    { id: "session_03", title: "archived", status: "archived", last_event_at: new Date(NOW).toISOString() },
    { id: "session_04", title: "waits", status: "active", worker_status: "waiting", created_at: new Date(NOW).toISOString() },
  ],
});
const cloud = parseClaudeCloud(cloudBody, NOW, 7);
eq(cloud.map((s) => s.id), ["session_01", "session_04"], "claude cloud: archived and stale sessions are left out");
eq([cloud[0].state, cloud[0].repo, cloud[0].url], ["working", "sinelaw/fresh", "https://claude.ai/code/session_01"], "claude cloud: state, repository and page");
eq(cloud[1].state, "blocked", "claude cloud: waiting needs you");
eq(parseClaudeCloud(cloudBody, NOW, 0).length, 3, "claude cloud: an age cap of 0 keeps every open session");
eq(repoFromUrl("git@github.com:o/n.git"), "o/n", "repoFromUrl: ssh form");
eq(claudeSessionId("cse_01ABC"), "session_01ABC", "claude cloud: the list's cse_ id is the session_ id users see");
eq(parseClaudeCloud(JSON.stringify({ data: [{ id: "cse_01ABC", title: "t", status: "active", worker_status: "idle" }] }), NOW, 0)[0].url, "https://claude.ai/code/session_01ABC", "claude cloud: links use the session_ id");

const rcBody = JSON.stringify({
  data: [
    { id: "session_rc1", title: "On my box", status: "active", environment_kind: "bridge", connection_status: "connected", worker_status: "requires_action", last_event_at: new Date(NOW).toISOString() },
    { id: "session_rc2", title: "Box is off", status: "active", environment_kind: "bridge", connection_status: "disconnected", last_event_at: new Date(NOW).toISOString() },
    { id: "session_rc3", title: "  ", status: "active", environment_kind: "bridge", last_event_at: new Date(NOW).toISOString() },
  ],
  next_cursor: "c2",
});
const rc = parseClaudeCloud(rcBody, NOW, 7);
eq(
  rc.map((s) => [s.id, s.title, s.state, s.offline ?? false]),
  [["session_rc1", "On my box", "blocked", false], ["session_rc2", "Box is off", "unknown", true], ["session_rc3", "session_rc3", "unknown", false]],
  "claude cloud: every active Remote Control session is listed, a disconnected one as offline, an untitled one by id",
);
eq(liveDetail(rc[0]), "remote control", "detail: a Remote Control session says so");
eq(liveDetail(rc[1]), "remote control · offline", "detail: and says when its machine is offline");
eq(claudeCloudPageInfo(rcBody), { next: "c2", oldest: NOW }, "claude cloud: a page's cursor and oldest row");
eq(claudeCloudPageInfo(cloudBody).next, null, "claude cloud: the last page has no cursor");

const creds = (expiresAt: number) => JSON.stringify({ claudeAiOauth: { accessToken: "tok", expiresAt } });
eq(claudeAccessToken(creds(NOW + 1000), NOW), "tok", "claude token: a live token is used");
eq(claudeAccessToken(creds(NOW - 1000), NOW), null, "claude token: an expired one is not refreshed here");
eq(claudeAccessToken("{}", NOW), null, "claude token: none signed in");

// ── Codex Cloud ───────────────────────────────────────────────────

const codexCloud = parseCodexCloud(JSON.stringify({
  tasks: [
    { id: "task_a", url: "https://chatgpt.com/codex/tasks/task_a", title: "Fix CI", status: "pending", updated_at: new Date(NOW).toISOString(), environment_label: "sinelaw/fresh" },
    { id: "task_b", title: "Done", status: "ready", updated_at: new Date(NOW).toISOString() },
    { id: "task_c", title: "Applied", status: "applied", updated_at: new Date(NOW).toISOString() },
    { id: "task_d", title: "Old", status: "ready", updated_at: new Date(NOW - 9 * DAY).toISOString() },
  ],
  cursor: null,
}), NOW, 7);
eq(codexCloud.map((s) => [s.id, s.state]), [["task_a", "working"], ["task_b", "done"]], "codex cloud: applied and stale tasks are left out");
eq(codexCloud[0].repo, "sinelaw/fresh", "codex cloud: the environment names the repository");

// ── Codex processes ───────────────────────────────────────────────

eq(isCodexSessionArgv(["/usr/local/bin/codex"]), true, "codex argv: a bare codex is a session");
eq(isCodexSessionArgv(["codex", "--full-auto", "resume", "--last"]), true, "codex argv: resume is a session");
eq(isCodexSessionArgv(["codex", "app-server"]), false, "codex argv: the app server is not");
eq(isCodexSessionArgv(["node", "/x/bin/codex.js", "mcp"]), false, "codex argv: the npm launcher's subcommand is read");
eq(isCodexSessionArgv(["/usr/bin/vim", "codex"]), false, "codex argv: another program is not");

eq(isCodexSessionArgv(["codex", "-c", "key=value", "app-server"]), false, "codex argv: a flag's value is not the subcommand");
eq(isCodexSessionArgv(["codex", "--model", "o3", "resume"]), true, "codex argv: flags with values before a session subcommand");

const procs = parseCodexProcesses([
  "  100     1 pts/3  node /usr/lib/node_modules/@openai/codex/bin/codex.js",
  "  101   100 pts/3  /usr/lib/node_modules/@openai/codex-linux-x64/vendor/bin/codex",
  "  200     1 pts/4  codex app-server",
  "  300     1 pts/5  /usr/bin/zsh",
  "  400     1 ttys002 codex exec fix the tests",
  // An app's helper: no terminal (macOS `??`, Linux `?`), whatever its args.
  "  500     1 ??     /Applications/Codex.app/Contents/Resources/codex -c k=v serve",
  "  600     1 ?      /opt/codex/codex",
].join("\n"));
eq(procs.map((p) => p.pid), [100, 400], "codex processes: on a terminal only; the launcher and its binary are one session");
const cwds = parseLsofCwds("p100\nfcwd\nn/home/u/proj\np400\nfcwd\nn/home/u/other\n");
eq([...cwds.entries()], [[100, "/home/u/proj"], [400, "/home/u/other"]], "lsof: pid to cwd");
const codexLocal = codexLocalSessions(procs, cwds);
eq(codexLocal.map((s) => [s.key, s.title]), [["codex-local/100", "proj"], ["codex-local/400", "other"]], "codex processes: titled by directory");

// ── Filtering and plans ───────────────────────────────────────────

const DATA = "/data/fresh";
const shown = unrepresented([...agents, ...cloud], ["/home/u/fresh/", elsewhereRoot(DATA, cloud[1])], DATA);
eq(shown.map((s) => s.key), ["claude-local/5e55", "claude-cloud/session_01"], "unrepresented: sessions already open as a workspace are hidden");

const env: LivePlanEnv = { dataDir: DATA, claude: "claude", codex: "codex", windows: false };
eq(livePlan(desktop[1], env, false), { kind: "workspace", root: "/home/u/blog", label: "Draft a post", command: ["claude", "--resume", "cli-b"] }, "plan: a stopped Desktop session is resumed in its folder");
eq(livePlan(desktop[2], env, false).kind, "none", "plan: one over SSH cannot be resumed here");
eq(livePlan(cloud[0], env, false), { kind: "browser", url: "https://claude.ai/code/session_01" }, "plan: Enter on a Claude cloud session opens its page");
eq(livePlan(cloud[0], env, true), { kind: "teleport", command: ["claude", "--teleport", "session_01"] }, "plan: making a Claude cloud session a workspace teleports it");
eq(livePlan(agents[1], env, false), {
  kind: "workspace",
  root: "/home/u/api",
  label: agents[1].title,
  command: ["claude", "attach", "job7"],
}, "plan: a background job is attached");
eq(livePlan(agents[0], env, true), {
  kind: "workspace",
  root: "/home/u/fresh",
  label: "fresh-04",
  note: "running in another terminal",
}, "plan: a session in another terminal opens its folder, not a second copy");
eq(livePlan(codexCloud[0], env, false), { kind: "browser", url: "https://chatgpt.com/codex/tasks/task_a" }, "plan: Enter on a Codex task opens its page");
const filed = livePlan(codexCloud[0], env, true);
eq(filed.kind === "workspace" ? filed.command?.slice(0, 2) : null, ["sh", "-c"], "plan: filing a Codex task makes a workspace with its status");
eq(livePlan({ ...codexLocal[0], cwd: undefined }, env, false).kind, "none", "plan: a process with no known directory has nothing to open");
eq(livePlan({ ...codexLocal[0], cwd: "/" }, env, false).kind, "none", "plan: a session in the filesystem root never opens a workspace there");
eq(codexLocalSessions([{ pid: 7, argv: ["codex"] }], new Map([[7, "/"]]))[0].title, "codex (pid 7)", "codex processes: a root directory is not a title");
eq(parseClaudeAgents(JSON.stringify([{ pid: 8, cwd: "/", kind: "interactive", sessionId: "abcdef1234", status: "idle" }]))[0].title, "claude abcdef12", "claude agents: a root directory is not a title");

eq(liveDetail(cloud[0]), "sinelaw/fresh · claude.ai", "detail: repository and place");
eq(liveDetail(agents[1]), "claude --bg", "detail: no directory when the title already is it");
eq(liveDetail({ ...agents[0], jobId: undefined }), "fresh · pid 123", "detail: directory when it is not the title");

if (failures > 0) {
  console.log(`${failures} failure(s)`);
  process.exit(1);
}
