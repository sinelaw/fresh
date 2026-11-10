/** The Workboard's own session sources (`lib/workboard_sources.ts`): running
 *  sessions and found sessions as board items. */
import type { DetectedSession } from "../lib/discovery.ts";
import type { LiveSession } from "../lib/live_sessions.ts";
import {
  detectedItemId,
  detectedItems,
  liveSessionItem,
  parseDetectedItemId,
} from "../lib/workboard_sources.ts";

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

function live(extra: Partial<LiveSession>): LiveSession {
  return {
    key: "claude-local/1",
    source: "claude-local",
    id: "1",
    agent: "claude",
    where: "local",
    title: "fix-ci",
    state: "working",
    ...extra,
  };
}

// ── Running outside the editor ─────────────────────────────────────────────

const local = liveSessionItem(live({ cwd: "/home/me/ci", state: "blocked", waitingFor: "Run the tests?", updatedAt: 5 }));
eq(
  [local.id, local.state, local.cwd, local.where, local.question, local.updatedAt],
  ["claude-local/1", "blocked", "/home/me/ci", undefined, "Run the tests?", 5],
  "live: a local session keeps its directory, state and question",
);
eq(local.project, undefined, "live: a local session's project is resolved from its directory, not named here");

const cloud = liveSessionItem(live({ key: "claude-cloud/9", source: "claude-cloud", where: "cloud", repo: "Acme/Api" }));
eq(cloud.project, { id: "repo:github.com/acme/api", label: "Acme/Api" }, "live: a cloud session's repository is its project");
eq(cloud.where, "cloud", "live: a cloud session is not on this computer");
eq(liveSessionItem(live({ where: "cloud", remoteControl: true })).where, "remote control", "live: remote control says so");
eq(
  liveSessionItem(live({ where: "cloud", repo: "my environment" })).project,
  undefined,
  "live: an environment label is not a repository",
);
eq(liveSessionItem(live({ cwd: "/srv/x", sshHost: "devbox", host: "Claude Desktop" })).where, "devbox", "live: a session over SSH runs on that host");

// ── Found by a scan ────────────────────────────────────────────────────────

const DAY = 86_400_000;
const now = 100 * DAY;
function found(id: string, mtimeDaysAgo: number | undefined, extra: Partial<DetectedSession> = {}): DetectedSession {
  return {
    machineKey: "local",
    machineLabel: "This computer",
    session: {
      id,
      tool: "claude_code",
      cwd: `/home/me/${id}`,
      ...(mtimeDaysAgo !== undefined ? { mtime: (now - mtimeDaysAgo * DAY) / 1000 } : {}),
    },
    scannedAt: now,
    ...extra,
  };
}

const items = detectedItems(
  [
    found("old", 30),
    found("week", 7),
    found("today", 0),
    found("pane", undefined),
    found("far", 1, { machineKey: "ssh:devbox", machineLabel: "devbox · ssh" }),
  ],
  now,
);
eq(items.map((i) => i.title), ["today", "far", "week", "pane"], "found: recent ones, newest first; a live listing has no age and stays");
eq(items[0].status, "claude_code", "found: the tool that found it");
eq([items[1].where, items[1].status], ["devbox · ssh", "claude_code · devbox · ssh"], "found: another machine says which");
eq(items[0].state, "idle", "found: idle");

const withProject = detectedItems(
  [{ ...found("p", 0), session: { ...found("p", 0).session, title: "Fix it", project: { kind: "repo", id: "repo:github.com/a/b", label: "a/b", root: "/x" } } }],
  now,
)[0];
eq([withProject.title, withProject.project], ["Fix it", { id: "repo:github.com/a/b", label: "a/b" }], "found: the scan's own project and title");

const id = detectedItemId(found("x", 0));
eq(parseDetectedItemId(id), { machineKey: "local", tool: "claude_code", sessionId: "x" }, "found: the id carries machine, tool and session");
eq(parseDetectedItemId("nope"), null, "found: an id it did not make");

if (failures > 0) {
  console.log(`${failures} failure(s)`);
  process.exit(1);
}
