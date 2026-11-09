/** Workboard's sorting, grouping and Board placement (`lib/workboard_model.ts`). */
import {
  autoColumn,
  beaconText,
  boardColumns,
  type Card,
  cardKey,
  type ColumnId,
  folderGroups,
  formatAge,
  inboxGroups,
  nextAttention,
  normalizeSaved,
  projectGroups,
  projectLabel,
  repoFromCommonDir,
  shiftPin,
  trackSince,
  type WorkspaceLike,
} from "../lib/workboard_model.ts";

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

let nextWindow = 1;
function ws(name: string, state: WorkspaceLike["agentState"], extra: Partial<WorkspaceLike> = {}): WorkspaceLike {
  const id = nextWindow++;
  return {
    kind: "live",
    workspaceId: `ws-${name}`,
    windowId: id,
    active: false,
    folderId: null,
    name,
    root: `/w/${name}`,
    projectPath: "/r/api",
    agentState: state,
    title: "",
    ...extra,
  };
}

function card(w: WorkspaceLike, since = 0, pin: ColumnId | null = null, question: string | null = null): Card {
  return { key: cardKey(w), ws: w, since, pin, question };
}

const names = (cs: Card[]) => cs.map((c) => c.ws.name);

// ── Inbox ──────────────────────────────────────────────────────────────────

const rate = card(ws("rate-limit", "blocked"), 300, null, "Postgres or Redis?");
const login = card(ws("login-fix", "blocked"), 100);
const tabs = card(ws("tab-names", "done"), 50);
const lsp = card(ws("lsp-cache", "working"), 10);
const bill = card(ws("billing-ui", "working"), 5);
const blog = card(ws("blog-post", "idle"), 1);
const all = [rate, login, tabs, lsp, bill, blog];

const inbox = inboxGroups(all);
eq(inbox.map((g) => g.id), ["needs", "review", "working", "parked"], "inbox: sections in the order they need you");
eq(names(inbox[0].cards), ["login-fix", "rate-limit"], "inbox: the agent that has waited longest comes first");
eq(names(inbox[2].cards), ["lsp-cache", "billing-ui"], "inbox: working keeps the dock's order");
eq(inboxGroups([lsp]).map((g) => g.id), ["working"], "inbox: empty sections are left out");
eq(
  inboxGroups([card(ws("new", "unknown", { kind: "pending" }))]).map((g) => g.id),
  ["working"],
  "inbox: a workspace still being created counts as working",
);

// ── Projects ───────────────────────────────────────────────────────────────

const web1 = card(ws("w1", "idle", { projectPath: "/r/web" }));
const web2 = card(ws("w2", "blocked", { projectPath: "/r/web" }));
const projects = projectGroups([rate, web1, tabs, web2]);
eq(projects.map((g) => g.label), ["api", "web"], "projects: one group per repo, alphabetical");
eq(names(projects[1].cards), ["w2", "w1"], "projects: each group sorted by urgency");
eq(projectLabel("/a/app", ["/a/app", "/b/app", "/c/web"]), "a/app", "projects: a clashing name gets its parent");
eq(projectLabel("/c/web", ["/a/app", "/b/app", "/c/web"]), "web", "projects: a unique name stays short");

eq(repoFromCommonDir("/home/me/api/.git\n"), "/home/me/api", "repo: a worktree's common dir names its repo");
eq(repoFromCommonDir("/srv/api.git\n"), "/srv/api.git", "repo: a bare repo is its own project");
eq(repoFromCommonDir(".git\n"), null, "repo: a relative answer is not trusted");
eq(repoFromCommonDir(""), null, "repo: no answer, no repo");
eq(repoFromCommonDir("C:\\code\\api\\.git"), "C:\\code\\api", "repo: Windows paths");

// ── Folders ────────────────────────────────────────────────────────────────

const folders = [
  { folderId: "f1", name: "Work", parent: null, depth: 0 },
  { folderId: "f2", name: "Billing", parent: "f1", depth: 1 },
  { folderId: "f3", name: "Empty", parent: null, depth: 0 },
];
const inBilling = card(ws("inv", "working", { folderId: "f2" }));
const inWork = card(ws("ops", "idle", { folderId: "f1" }));
const lost = card(ws("old", "idle", { folderId: "gone" }));
const fg = folderGroups([inBilling, rate, inWork, lost], folders);
eq(fg.map((g) => g.label), ["Work", "Work / Billing", "Not in a folder"], "folders: dock order, full names, empty left out");
eq(names(fg[2].cards), ["rate-limit", "old"], "folders: unfiled and unknown folders land together");

// ── Board ──────────────────────────────────────────────────────────────────

eq(autoColumn(ws("a", "blocked")), "needs", "board: waiting goes to Needs me");
eq(autoColumn(ws("a", "working")), "working", "board: working goes to Working");
eq(autoColumn(ws("a", "done")), "review", "board: finished goes to Review");
eq(autoColumn(ws("a", "idle", { git: { dirty: 2 } })), "review", "board: idle with uncommitted work waits in Review");
eq(autoColumn(ws("a", "idle", { git: { ahead: 1 } })), "review", "board: idle with unpushed commits waits in Review");
eq(autoColumn(ws("a", "idle", { git: { dirty: 0 } })), "backlog", "board: clean and idle is Backlog");

const pinned = card(ws("shipped-one", "blocked"), 0, "shipped");
const cols = boardColumns([rate, tabs, pinned, blog]);
eq(cols.map((c) => c.id), ["backlog", "needs", "working", "review", "shipped"], "board: every column, even empty");
eq(names(cols[4].cards), ["shipped-one"], "board: a pin wins over the automatic column");
eq(names(cols[1].cards), ["rate-limit"], "board: the pinned card left its automatic column");

const movable = card(ws("m", "working"));
eq(shiftPin(movable, 1), "review", "pin: > moves one column right");
eq(shiftPin(movable, -1), "needs", "pin: < moves one column left");
eq(shiftPin({ ...movable, pin: "review" }, -1), null, "pin: moving back onto the automatic column clears the pin");
eq(shiftPin({ ...movable, pin: "shipped" }, 1), "shipped", "pin: the last column is a wall");

// ── Beacon and jumping ─────────────────────────────────────────────────────

eq(beaconText(all), "● 2 waiting  ✓ 1 done", "beacon: waiting and done counts");
eq(beaconText([lsp, blog]), "", "beacon: empty when nothing needs you");
eq(beaconText([tabs]), "✓ 1 done", "beacon: done alone");

eq(nextAttention(all, -1)?.ws.name, "login-fix", "jump: the longest-waiting agent first");
eq(nextAttention(all, login.ws.windowId)?.ws.name, "rate-limit", "jump: never the workspace already open");
eq(nextAttention([tabs, lsp], -1)?.ws.name, "tab-names", "jump: finished work when nobody is waiting");
eq(nextAttention([lsp, blog], -1), null, "jump: nothing to do");

// ── Time in state ──────────────────────────────────────────────────────────

const a1 = ws("t", "working");
const s1 = trackSince(new Map(), [a1], 1000);
eq(s1.get(cardKey(a1)), { state: "working", since: 1000 }, "since: first seen counts from now");
const s2 = trackSince(s1, [a1], 5000);
eq(s2.get(cardKey(a1))?.since, 1000, "since: unchanged state keeps its start");
const s3 = trackSince(s2, [{ ...a1, agentState: "blocked" }], 9000);
eq(s3.get(cardKey(a1)), { state: "blocked", since: 9000 }, "since: a new state restarts the clock");
eq(trackSince(s3, [], 10000).size, 0, "since: a gone workspace is forgotten");

eq([formatAge(3000), formatAge(42_000), formatAge(4 * 60_000), formatAge(5 * 3600_000), formatAge(3 * 86400_000)], [
  "<1m",
  "<1m",
  "4m",
  "5h",
  "3d",
], "age: coarse units");

// ── Saved state ────────────────────────────────────────────────────────────

eq(normalizeSaved(undefined), { lens: "inbox", pins: {} }, "saved: nothing saved yet");
eq(
  normalizeSaved({ lens: "board", pins: { a: "review", b: "nowhere", c: 3 } }),
  { lens: "board", pins: { a: "review" } },
  "saved: malformed pins are dropped",
);
eq(normalizeSaved({ lens: "timeline" }).lens, "inbox", "saved: an unknown lens falls back to Inbox");

// ── Keys ───────────────────────────────────────────────────────────────────

eq(cardKey({ workspaceId: "", root: "/w/x" }), "root:/w/x", "key: falls back to the root");

if (failures > 0) {
  console.log(`${failures} failure(s)`);
  process.exit(1);
}
