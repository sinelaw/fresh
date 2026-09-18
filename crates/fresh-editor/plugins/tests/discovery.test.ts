/** The Everything dialog's row model: a scan in, rows out. */
import { discoverRowsFrom, DISCOVER_ABSENT_KEY, DISCOVER_PROBLEMS_KEY, type DiscoverScan } from "../lib/discovery.ts";
import type { CollectedSession, ToolReport } from "../lib/agent_scanner.ts";

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

const t = (key: string, params?: Record<string, string>): string =>
  params ? `${key}(${Object.values(params).join(",")})` : key;
/** claude resumes by id; codex only by "newest here"; anything else is unknown. */
const resumeArgv = (agent: string, id: string) =>
  agent === "claude" ? { argv: ["claude", "--resume", id], exact: true }
  : agent === "codex" ? { argv: ["codex", "resume", "--last"], exact: false }
  : null;

function session(o: Partial<CollectedSession> & { id: string; tool: string }): CollectedSession {
  return { title: o.id, ...o };
}
function tool(id: string, status: ToolReport["status"], note?: string): ToolReport {
  return { id, displayName: id, status, count: 0, note };
}
function scan(key: string, sessions: CollectedSession[], tools: ToolReport[] = [], problems: string[] = []): DiscoverScan {
  return { key, label: key, scan: { machine: key, sessions, links: [], tools, problems } };
}

const proj = (label: string, id = label) => ({ kind: "repo" as const, id: `repo:${id}`, label, root: "/" + id });

// ── ordering ──────────────────────────────────────────────────────

{
  const rows = discoverRowsFrom(
    [scan("m", [
      session({ id: "zeta", tool: "claude-code", agent: "claude", cwd: "/p", project: proj("payments") }),
      session({ id: "Alpha", tool: "claude-code", agent: "claude", cwd: "/p", project: proj("payments") }),
      session({ id: "one", tool: "tmux", cwd: "/a", project: proj("api") }),
      session({ id: "loose", tool: "tmux" }),
      session({ id: "mid", tool: "codex-cli", agent: "codex", cwd: "/p", project: proj("payments") }),
    ])],
    { filter: "", grouping: "project" }, resumeArgv, t,
  );
  eq(rows.filter((r) => r.group).map((r) => r.label), ["api", "payments", "discover.no_project"],
    "headings by name, the directory-less group last");
  eq(rows.filter((r) => !r.group && r.key.includes("/claude-code/") || r.key.includes("/codex-cli/")).map((r) => r.label.trim()),
    ["Alpha", "mid", "zeta"], "rows within a heading by title, case-insensitively");
}

// ── what a row can do ─────────────────────────────────────────────

{
  const rows = discoverRowsFrom(
    [scan("m", [
      session({ id: "s1", tool: "claude-code", agent: "claude", cwd: "/p" }),
      session({ id: "s2", tool: "codex-cli", agent: "codex", cwd: "/p" }),
      session({ id: "s3", tool: "gemini", agent: "gemini", cwd: "/p" }),
      session({ id: "s4", tool: "tmux", cwd: "/p", attach: { program: "tmux", args: ["attach", "-t", "s4"] } }),
    ])],
    { filter: "", grouping: "tool" }, resumeArgv, t,
  );
  const verbs = Object.fromEntries(rows.filter((r) => r.session).map((r) => [r.session!.id, r.verb]));
  eq(verbs["s1"], { kind: "resume", argv: ["claude", "--resume", "s1"], exact: true }, "an id-addressed resume is exact");
  eq(verbs["s2"], { kind: "resume", argv: ["codex", "resume", "--last"], exact: false }, "a newest-here resume says so");
  eq(verbs["s3"], { kind: "none", why: "discover.no_resume_unknown(gemini)" }, "an unknown agent has no way back");
  eq(verbs["s4"], { kind: "attach", argv: ["tmux", "attach", "-t", "s4"] }, "a live pane is attached to");
}

// ── problems are problems; absence is not ─────────────────────────

{
  const rows = discoverRowsFrom(
    [scan("m", [session({ id: "s", tool: "tmux", cwd: "/p" })],
      [tool("tmux", "found"), tool("codex-cli", "absent"), tool("super-engineering", "unsupported", "ships for macOS only"), tool("herdr", "failed", "boom")],
      ["herdr: boom"])],
    { filter: "", grouping: "tool" }, resumeArgv, t,
  );
  const keys = rows.map((r) => r.key);
  const absentAt = keys.indexOf(DISCOVER_ABSENT_KEY), problemsAt = keys.indexOf(DISCOVER_PROBLEMS_KEY);
  eq(absentAt >= 0 && problemsAt > absentAt, true, "absent tools get their own heading, before the problems");
  eq(rows.slice(absentAt + 1, problemsAt).map((r) => [r.label, r.detail]),
    [["codex-cli", "discover.not_installed"], ["super-engineering", "ships for macOS only"]],
    "absent and unsupported are listed with why, by name; a failed tool is not among them");
  eq(rows.slice(problemsAt + 1).map((r) => r.label), ["herdr: boom"], "the problems list holds only problems");
  eq(rows[absentAt].label, "discover.absent(2)", "the heading counts them");
}

{
  const rows = discoverRowsFrom(
    [scan("m", [session({ id: "s", tool: "tmux", cwd: "/p" })], [tool("tmux", "found")])],
    { filter: "", grouping: "tool" }, resumeArgv, t,
  );
  eq(rows.some((r) => r.key === DISCOVER_ABSENT_KEY || r.key === DISCOVER_PROBLEMS_KEY), false,
    "a clean scan has neither heading");
}

// ── filter ────────────────────────────────────────────────────────

{
  const rows = discoverRowsFrom(
    [scan("m", [
      session({ id: "a", tool: "tmux", cwd: "/p", project: { ...proj("payments"), branch: "feature/checkout-flow" } }),
      session({ id: "b", tool: "tmux", cwd: "/q", project: proj("api") }),
    ], [tool("codex-cli", "absent")])],
    { filter: "checkout-flow", grouping: "project" }, resumeArgv, t,
  );
  eq(rows.filter((r) => r.session).map((r) => r.session!.id), ["a"], "the filter matches the resolved branch");
  eq(rows.some((r) => r.key === DISCOVER_ABSENT_KEY), true, "absent tools are not filtered — they are the scan talking about itself");
}

console.log(failures === 0 ? "\nAll discovery tests passed." : `\n${failures} failed.`);
if (failures > 0) process.exit(1);
