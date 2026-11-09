/** Workboard's widget spec (`lib/workboard_view.ts`): which widgets the
 *  panel is built from, their keys, and what each card says. Layout itself
 *  is the host's, so nothing here measures; the one size rule tested is that
 *  the plugin hands the host none. */
import { type Card, cardKey, type WorkspaceLike } from "../lib/workboard_model.ts";
import {
  cardItemKey,
  cardKeyOf,
  columnKey,
  gitText,
  groupIdOf,
  listKey,
  panelSpec,
  statusText,
  treeItemKeys,
  type ViewInput,
} from "../lib/workboard_view.ts";

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

let n = 1;
function card(name: string, state: WorkspaceLike["agentState"], extra: Partial<WorkspaceLike> = {}, question: string | null = null): Card {
  const ws: WorkspaceLike = {
    kind: "live",
    workspaceId: `ws-${name}`,
    windowId: n++,
    active: false,
    folderId: null,
    name,
    root: `/w/${name}`,
    projectPath: "/r/api",
    agentState: state,
    title: "",
    ...extra,
  };
  return { key: cardKey(ws), ws, since: 0, pin: null, question };
}

const cards = [
  card("rate-limit", "blocked", {}, "Postgres or Redis?"),
  card("tab-names", "done", { git: { branch: "tab-names", dirty: 1 } }),
  card("lsp-cache", "working", { projectPath: "/r/web", folderId: "f1" }),
  card("old-spike", "idle"),
];
const folders = [{ folderId: "f1", name: "Frontend", parent: null, depth: 0 }];
const input = (over: Partial<ViewInput> = {}): ViewInput => ({
  lens: "inbox",
  cards,
  folders,
  now: 0,
  selected: new Map(),
  collapsed: new Set(),
  ...over,
});

// deno-lint-ignore no-explicit-any
type Any = any;
/** Every node of `kind` in a spec, depth first. */
function all(spec: Any, kind: string): Any[] {
  const out: Any[] = [];
  const walk = (s: Any) => {
    if (!s || typeof s !== "object") return;
    if (s.kind === kind) out.push(s);
    for (const c of s.children ?? []) walk(c);
    if (s.child) walk(s.child);
  };
  walk(spec);
  return out;
}

// ── The list lenses: one table tree ────────────────────────────────────────

const inbox = panelSpec(input());
const [table] = all(inbox, "tree");
eq(table.key, listKey("inbox"), "inbox: one tree, keyed per lens");
eq(table.columns.map((c: Any) => c.title), ["", "Workspace", "Status", "Project", "Branch", "For"], "inbox: drawn as a table");
eq(
  table.itemKeys.filter((k: string) => groupIdOf(k) !== null),
  ["group:needs", "group:review", "group:working", "group:parked"],
  "inbox: a heading per section, in the order they need you",
);
eq(table.itemKeys[table.selectedIndex], cardItemKey(cards[0].key), "inbox: the cursor starts on the first card, not a heading");
eq(table.nodes[1].cells[2].text, "Postgres or Redis?", "inbox: a waiting agent's question is its status");
eq(table.expandedKeys.length, 4, "inbox: every group open");
eq(panelSpec(input({ collapsed: new Set(["parked"]) })).children[2].expandedKeys.includes("group:parked"), false, "inbox: a folded group stays folded");

const chosen = panelSpec(input({ selected: new Map([[listKey("inbox"), cards[2].key]]) }));
const t2 = all(chosen, "tree")[0];
eq(t2.itemKeys[t2.selectedIndex], cardItemKey(cards[2].key), "inbox: the cursor follows the card the host last reported");

const foldersTree = all(panelSpec(input({ lens: "folders" })), "tree")[0];
eq(foldersTree.nodes.filter((x: Any) => x.depth === 1).every((x: Any) => x.draggable === true), true, "folders: cards can be dragged to another folder");
eq(table.nodes.some((x: Any) => x.draggable), false, "inbox: cards are not draggable (sorting is automatic)");

// ── The Board: a row of sections, a card tree in each ──────────────────────

const board = panelSpec(input({ lens: "board" }));
eq(all(board, "labeledSection").length, 0, "board: no frame around a column (the cards are the boxes)");
const headings = all(board, "label").filter((l: Any) => l.segments?.length === 2 && /^  \d+$/.test(l.segments[1].text));
eq(
  headings.map((l: Any) => `${l.segments[0].text}${l.segments[1].text}`),
  ["Backlog  1", "Needs me  1", "Working  1", "Review  1", "Shipped  0"],
  "board: five columns, each headed by its name and count",
);
const trees = all(board, "tree");
eq(trees.map((t: Any) => t.key), ["backlog", "needs", "working", "review", "shipped"].map(columnKey), "board: one tree per column");
eq(trees.every((t: Any) => t.cardBorders && t.itemHeight === 2), true, "board: two-line bordered cards");
eq(trees[1].nodes[0].extraLines[0].segments[0].text, "api · asking · <1m", "board: a card's second line says where and how long");
const pinned = panelSpec(input({ lens: "board", cards: [{ ...cards[3], pin: "shipped" }] }));
eq(all(pinned, "tree")[4].nodes[0].text.segments.some((s: Any) => s.text.includes("⊙")), true, "board: a pinned card wears the pin");

// ── Sizes are the host's ───────────────────────────────────────────────────

for (const lens of ["inbox", "projects", "folders", "board"] as const) {
  const spec = panelSpec(input({ lens }));
  eq(all(spec, "tree").every((t: Any) => t.visibleRows === undefined), true, `${lens}: no tree is told its height`);
}

// ── Header, empty state, and reading events back ───────────────────────────

const buttons = all(inbox, "button");
eq(buttons.map((b: Any) => [b.label, b.intent]), [
  ["1 Inbox", "primary"],
  ["2 Projects", "normal"],
  ["3 Folders", "normal"],
  ["4 Board", "normal"],
], "header: lens buttons, the current one primary");
eq(all(panelSpec(input({ cards: [] })), "tree").length, 0, "empty: no tree, a message instead");

const keys = treeItemKeys(board);
eq([...keys.keys()], trees.map((t: Any) => t.key), "treeItemKeys: every tree in the spec");
eq(cardKeyOf(cardItemKey("ws-1")), "ws-1", "item keys: a card's round-trips");
eq(cardKeyOf("group:needs"), null, "item keys: a heading is not a card");

// ── What a card says ───────────────────────────────────────────────────────

eq(statusText(cards[1]), "finished, not looked at", "status: unseen finished work");
eq(statusText(card("x", "unknown")), "no agent output", "status: no signal");
eq(
  statusText(card("x", "unknown", { kind: "pending", pending: { phase: "error", message: "ssh not found" } })),
  "failed: ssh not found",
  "status: a failed create says why",
);
eq(statusText(card("x", "unknown", { kind: "pending" })), "starting…", "status: a create in flight");
eq(statusText(card("x · claude", "working", { title: "claude" })), "", "status: a title already in the name is not repeated");
eq(gitText(cards[1]), "tab-names · 1 changed", "git: branch and changes");
eq(gitText(card("x", "idle", { git: { branch: "b", ahead: 2, behind: 1 } })), "b · ↑2 ↓1", "git: ahead and behind");

if (failures > 0) {
  console.log(`${failures} failure(s)`);
  process.exit(1);
}
