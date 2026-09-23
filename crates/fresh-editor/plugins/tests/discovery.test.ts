/** The Import sessions dialog's row model: a scan in, rows out. */
import {
  discoverIsGroup,
  discoverColumns,
  discoverRowAction,
  discoverRowCells,
  discoverRowsFrom,
  discoverVerbFor,
  DISCOVER_COL_MAX,
  DISCOVER_PROBLEMS_KEY,
  type DiscoverRow,
  type DiscoverScan,
} from "../lib/discovery.ts";
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

/** A row's columns as plain strings, which is what the assertions read. */
const cells = (r: DiscoverRow): string[] => r.cells.map((c) => c.text);
/** Every cell is one char wide here, so a width is a character count. */
const measure = (s: string): number => [...s].length;

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
  eq(rows.filter(discoverIsGroup).map((r) => r.cells[0].text), ["api", "payments", "discover.no_project"],
    "headings by name, the directory-less group last");
  eq(rows.filter((r) => !discoverIsGroup(r) && r.key.includes("/claude-code/") || r.key.includes("/codex-cli/")).map((r) => r.cells[0].text.trim()),
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

// ── problems are problems; absence is not listed ─────────────────────────

{
  const rows = discoverRowsFrom(
    [scan("m", [session({ id: "s", tool: "tmux", cwd: "/p" })],
      [tool("tmux", "found"), tool("codex-cli", "absent"), tool("super-engineering", "unsupported", "ships for macOS only"), tool("orca", "failed", "boom")],
      ["orca: boom"])],
    { filter: "", grouping: "tool" }, resumeArgv, t,
  );
  const keys = rows.map((r) => r.key);
  const problemsAt = keys.indexOf(DISCOVER_PROBLEMS_KEY);
  eq(rows.some((r) => r.cells.some((c) => c.text === "codex-cli" || c.text === "super-engineering")), false,
    "a tool that is absent or unsupported is not listed at all");
  eq(rows.slice(problemsAt + 1).map((r) => r.cells[0].text), ["orca: boom"], "the problems list holds only problems");
}

{
  const rows = discoverRowsFrom(
    [scan("m", [session({ id: "s", tool: "tmux", cwd: "/p" })], [tool("tmux", "found")])],
    { filter: "", grouping: "tool" }, resumeArgv, t,
  );
  eq(rows.some((r) => r.key === DISCOVER_PROBLEMS_KEY), false, "a clean scan has no problems heading");
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
}

// ── the table ─────────────────────────────────────────────────────

{
  const rows = discoverRowsFrom(
    [scan("m", [
      session({ id: "a", tool: "claude-code", agent: "claude", cwd: "/p", title: "short", project: { ...proj("payments"), branch: "main" } }),
      session({ id: "b", tool: "tmux", cwd: "/p", title: "a much longer session title", project: { ...proj("payments"), branch: "feature/checkout" } }),
    ], [{ id: "claude-code", displayName: "Claude Code", status: "found", count: 1 },
        { id: "tmux", displayName: "tmux", status: "found", count: 1 }])],
    { filter: "", grouping: "project" }, resumeArgv, t,
  );
  const sessions = rows.filter((r) => r.session);
  eq(sessions.map(cells), [
    ["  a much longer session title", "tmux", "feature/checkout"],
    ["  short", "Claude Code", "main"],
  ], "a session row is its name, then one column per thing the heading did not say");

  // How wide each column is, and where a cell is cut, is the host table's
  // to work out from the width it lays the list out at; the plugin says only
  // what the columns are and which end of each is cut.
  const columns = discoverColumns(rows, "project", false, t);
  eq(columns.map((c) => c.title), ["discover.col_session", "discover.col_tool", "discover.col_branch"],
    "one column per cell, titled for the grouping");
  eq(columns.every((c) => c.maxWidth === DISCOVER_COL_MAX), true, "no column may take the whole panel");
  eq(discoverRowCells(sessions[0]).map((c) => c.text), cells(sessions[0]),
    "a session row's cells are its columns' text, unpadded");
  eq(discoverRowCells(sessions[0])[1].style?.fg, "ui.menu_disabled_fg", "what tells rows apart is dim");
}

{
  // A path is cut at the head: the directory it ends in is what tells two
  // checkouts of one repository apart.
  const rows = discoverRowsFrom(
    [scan("local", [session({ id: "a", tool: "claude-code", cwd: "/home/u/work/repo", title: "x" })],
      [{ id: "claude-code", displayName: "Claude Code", status: "found", count: 1 }])],
    { filter: "", grouping: "tool" }, resumeArgv, t,
  );
  const columns = discoverColumns(rows, "tool", false, t);
  eq(columns[1].elide, "head", "a directory column keeps its tail");
  eq(columns[0].elide, "tail", "a name keeps its head");
}

// ── the button ────────────────────────────────────────────────────

{
  const rows = discoverRowsFrom(
    [scan("m", [
      session({ id: "s1", tool: "claude-code", agent: "claude", cwd: "/p" }),
      session({ id: "s3", tool: "gemini", agent: "gemini", cwd: "/p" }),
      session({ id: "s4", tool: "tmux", cwd: "/p", attach: { program: "tmux", args: ["attach", "-t", "s4"] } }),
    ], [], ["boom"])],
    { filter: "", grouping: "tool" }, resumeArgv, t,
  );
  const actionOf = (id: string) =>
    discoverRowAction(rows.find((r) => r.session?.id === id)!, t);
  eq(actionOf("s1"), "discover.btn_import", "a session that can be resumed offers the button");
  eq(actionOf("s4"), "discover.btn_import", "so does one that can be attached to");
  eq(actionOf("s3"), null, "a session with no way back does not");
  eq(rows.filter((r) => discoverRowAction(r, t) !== null).length, 2,
    "and neither does a heading or a problem line");
}

{
  // An Orca worktree: resumes its recorded agent, else opens the folder.
  const wt = { id: "r::/w", tool: "orca", cwd: "/w", openable: true };
  eq(discoverVerbFor(session({ ...wt, agent: "claude", agentSessionId: "abc" }), resumeArgv, t),
    { kind: "resume", argv: ["claude", "--resume", "abc"], exact: true },
    "an openable row with a known agent resumes it");
  eq(discoverVerbFor(session({ ...wt, agent: "grok" }), resumeArgv, t).kind, "open",
    "an openable row whose agent cannot resume opens the folder");
  eq(discoverVerbFor(session({ id: "x", tool: "tmux", cwd: "/w", agent: "grok" }), resumeArgv, t).kind,
    "none", "a row that is not openable still says why it cannot resume");
}

console.log(failures === 0 ? "\nAll discovery tests passed." : `\n${failures} failed.`);
if (failures > 0) process.exit(1);
