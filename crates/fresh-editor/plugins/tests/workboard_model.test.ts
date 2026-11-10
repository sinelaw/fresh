/** Workboard's sorting, grouping and Board placement (`lib/workboard_model.ts`). */
import {
  autoColumn,
  beaconText,
  boardColumns,
  type Card,
  cardAge,
  cardKey,
  type ColumnId,
  folderGroups,
  formatAge,
  inboxGroups,
  itemSubject,
  nextAttention,
  normalizeSaved,
  projectGroups,
  shiftPin,
  trackSince,
  type WorkboardSource,
  type WorkspaceLike,
  withoutCovered,
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
// The Orchestrator reports a pending row's agent state as "idle"; the phase
// is what says where it stands.
const pending = (phase: "creating" | "error" | "paused") =>
  card(ws(`p-${phase}`, "idle", { kind: "pending", pending: { phase, message: "x" } }));
eq(inboxGroups([pending("creating")]).map((g) => g.id), ["working"], "inbox: a workspace being created counts as working");
eq(inboxGroups([pending("error")]).map((g) => g.id), ["needs"], "inbox: a failed create needs you (retry or dismiss)");
eq(inboxGroups([pending("paused")]).map((g) => g.id), ["parked"], "inbox: a paused create is parked");
eq(autoColumn(pending("creating").ws), "working", "board: a create in flight is Working");

// ── Projects ───────────────────────────────────────────────────────────────

const web1 = card(ws("w1", "idle", { projectPath: "/r/web" }));
const web2 = card(ws("w2", "blocked", { projectPath: "/r/web" }));
const projects = projectGroups([rate, web1, tabs, web2]);
eq(projects.map((g) => g.label), ["api", "web"], "projects: one group per repo, alphabetical");
eq(names(projects[1].cards), ["w2", "w1"], "projects: each group sorted by urgency");

// ── Projects by identity ───────────────────────────────────────────────────

const acme = { id: "repo:github.com/acme/api", label: "acme/api" };
const cloneA = card(ws("feature-a", "working", { projectPath: "/home/me/api", project: acme }));
const cloneB = card(ws("feature-b", "idle", { projectPath: "/tmp/api-copy", project: acme }));
const other = card(ws("x", "idle", { projectPath: "/home/me/api", project: { id: "repo:github.com/other/api", label: "other/api" } }));
const local = card(ws("y", "idle", { projectPath: "/home/me/notes" }));
const byRemote = projectGroups([cloneA, cloneB, other, local]);
eq(byRemote.map((g) => [g.label, g.cards.length]), [["acme/api", 2], ["notes", 1], ["other/api", 1]], "projects: clones share an identity, wherever they sit");
const twinA = card(ws("ta", "idle", { project: { id: "root:/a/app", label: "app" } }));
const twinB = card(ws("tb", "idle", { project: { id: "root:/b/app", label: "app" } }));
eq(projectGroups([twinA, twinB]).map((g) => g.label), ["/a/app", "/b/app"], "projects: two projects that read the same are told apart by their identity");

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

// ── Sources ────────────────────────────────────────────────────────────────

const running: WorkboardSource = { id: "ext", label: "Running", kind: "running", list: () => [], open: () => null };
const history: WorkboardSource = { id: "hist", label: "Found", kind: "history", list: () => [], open: () => null };
const ext = itemSubject(running, { id: "claude-local/1", title: "fix-ci", state: "blocked", agent: "claude", cwd: "/home/me/ci", status: "local" });
const extCard = card(ext, 0, null, "Run the tests?");
eq([ext.kind, ext.agentState, ext.root, ext.title], ["external", "blocked", "/home/me/ci", "claude"], "source: a running session is placed by its state");
eq(cardKey(ext), "src:ext:claude-local/1", "source: its key names the source");
const old1 = itemSubject(history, { id: "a", title: "old", state: "working", cwd: "/home/me/old", updatedAt: 1000 });
const old2 = itemSubject(history, { id: "b", title: "older", state: "idle", cwd: "/home/me/older", updatedAt: 500 });
eq(old1.agentState, "idle", "source: a session in history is idle whatever it says");
const mixed = inboxGroups([card(old2), extCard, card(old1), blog]);
eq(mixed.map((g) => g.id), ["needs", "parked", "detected"], "inbox: a waiting outside session needs you; history comes last");
eq(names(mixed[2].cards), ["old", "older"], "inbox: history newest first");
eq(autoColumn(old1), "backlog", "board: history is Backlog");
eq(cardAge(card(old1, 9000), 10_000), 9000, "age: a found session counts from its last activity");
eq(cardAge(extCard, 10_000), 10_000, "age: a running one from when it entered its state");

const fgOut = folderGroups([inWork, extCard, card(old1)], folders);
eq(fgOut.map((g) => g.label), ["Work", "Outside the editor"], "folders: sessions outside the editor have their own group");

eq(beaconText([extCard]), "", "beacon: counts only what the editor holds");
eq(nextAttention([extCard, tabs], -1)?.ws.name, "tab-names", "jump: never to a session outside the editor");

const atHome = ws("home", "idle", { root: "/home/me/old/" });
const coveredByRun = itemSubject(history, { id: "c", title: "ci", state: "idle", cwd: "/home/me/ci" });
const away = itemSubject(history, { id: "d", title: "remote", state: "idle", cwd: "/home/me/old", where: "devbox" });
eq(
  withoutCovered([atHome, ext, old1, old2, coveredByRun, away]).map((w) => w.name),
  ["home", "fix-ci", "older", "remote"],
  "covered: a found session where a workspace or running session already is drops out; another machine's stays",
);

// ── Keys ───────────────────────────────────────────────────────────────────

eq(cardKey({ workspaceId: "", root: "/w/x" }), "root:/w/x", "key: falls back to the root");

if (failures > 0) {
  console.log(`${failures} failure(s)`);
  process.exit(1);
}
