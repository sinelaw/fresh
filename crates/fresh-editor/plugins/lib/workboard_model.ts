/**
 * Workboard's model: how the Orchestrator's workspaces are sorted, grouped
 * and placed on the board. Pure — no `editor` — so it is unit-tested without
 * one (`tests/workboard_model.test.ts`); `workboard.ts` feeds it
 * `listWorkspaces()` and draws what it returns.
 *
 * Four lenses over one list of cards:
 *   * Inbox    — urgency first: needs you, ready to review, working, parked.
 *   * Projects — one group per repo the workspace was cut from.
 *   * Folders  — the dock's own folders, so a hand-made arrangement is a lens.
 *   * Board    — columns derived from agent state; a card can be pinned to a
 *                column by hand, and the pin wins until it is removed.
 */

export type AgentState = "working" | "blocked" | "done" | "idle" | "unknown";

/** The part of the Orchestrator's `WorkspaceSummary` this needs. */
export interface WorkspaceLike {
  kind: "live" | "discovered" | "pending";
  workspaceId: string;
  windowId: number;
  active: boolean;
  folderId: string | null;
  name: string;
  root: string;
  projectPath: string;
  branch?: string;
  agentState: AgentState;
  title: string;
  git?: { branch?: string; dirty?: number; ahead?: number; behind?: number };
  backend?: string;
  pending?: { phase: "creating" | "error" | "paused"; message: string };
  /** The repo's remote, normalized (`normalizeRemote`), when it has one:
   *  what makes two clones of one project the same project. Filled in by
   *  the Workboard from git, not by the Orchestrator. */
  remote?: string;
}

/** The part of the Orchestrator's `FolderSummary` this needs. */
export interface FolderLike {
  folderId: string;
  name: string;
  parent: string | null;
  depth: number;
}

export type LensId = "inbox" | "projects" | "folders" | "board";

export const LENSES: readonly { id: LensId; label: string }[] = [
  { id: "inbox", label: "Inbox" },
  { id: "projects", label: "Projects" },
  { id: "folders", label: "Folders" },
  { id: "board", label: "Board" },
];

export type ColumnId = "backlog" | "needs" | "working" | "review" | "shipped";

export const COLUMNS: readonly { id: ColumnId; label: string }[] = [
  { id: "backlog", label: "Backlog" },
  { id: "needs", label: "Needs me" },
  { id: "working", label: "Working" },
  { id: "review", label: "Review" },
  { id: "shipped", label: "Shipped" },
];

/** One workspace as the board shows it. */
export interface Card {
  /** Durable key: the workspace id, or its root when it has none yet. */
  key: string;
  ws: WorkspaceLike;
  /** When the plugin first saw the workspace in its current state (ms). */
  since: number;
  /** The column it is pinned to on the Board, if any. */
  pin: ColumnId | null;
  /** The agent's question, for a workspace waiting on the user. */
  question: string | null;
}

/** A titled run of cards (an Inbox section, a project, a folder). */
export interface Group {
  id: string;
  label: string;
  /** Colours the header: the state a whole Inbox section stands for. */
  tone: AgentState | null;
  cards: Card[];
}

/** What is saved between runs. */
export interface SavedState {
  lens: LensId;
  pins: Record<string, ColumnId>;
}

export function cardKey(ws: { workspaceId: string; root: string }): string {
  return ws.workspaceId !== "" ? ws.workspaceId : `root:${ws.root}`;
}

/** Lower is more urgent. */
export function urgency(state: AgentState): number {
  switch (state) {
    case "blocked":
      return 0;
    case "done":
      return 1;
    case "working":
      return 2;
    case "idle":
      return 3;
    default:
      return 4;
  }
}

/** The state a card is shown in. A workspace still being created counts
 *  as working: something is happening and nobody is waiting on it. One
 *  whose create failed counts as needing you: it waits for a retry or a
 *  dismiss, and nothing else will move it. One restored but not resumed is
 *  parked. The Orchestrator reports a pending row's agent state as idle, so
 *  the kind and phase decide, not the agent state. */
export function effectiveState(ws: WorkspaceLike): AgentState {
  if (ws.kind === "pending") {
    switch (ws.pending?.phase) {
      case "error":
        return "blocked";
      case "paused":
        return "idle";
      default:
        return "working";
    }
  }
  return ws.agentState;
}

/** Whether the worktree carries work nobody has shipped yet. */
export function hasUnshippedWork(ws: WorkspaceLike): boolean {
  return (ws.git?.dirty ?? 0) > 0 || (ws.git?.ahead ?? 0) > 0;
}

/** Where a card sits on the Board when nobody pinned it. Idle workspaces
 *  with uncommitted or unpushed work wait in Review — the agent stopped,
 *  someone has to look — and clean idle ones fall back to Backlog. Shipped
 *  is reached only by pinning: nothing on the workspace says "merged". */
export function autoColumn(ws: WorkspaceLike): ColumnId {
  switch (effectiveState(ws)) {
    case "blocked":
      return "needs";
    case "working":
      return "working";
    case "done":
      return "review";
    default:
      return hasUnshippedWork(ws) ? "review" : "backlog";
  }
}

export function columnOf(card: Card): ColumnId {
  return card.pin ?? autoColumn(card.ws);
}

/** Stable sort by urgency, then longest in that state first. */
function byUrgency(a: Card, b: Card): number {
  return (
    urgency(effectiveState(a.ws)) - urgency(effectiveState(b.ws)) ||
    a.since - b.since
  );
}

/** Inbox: four sections in the order they need you; empty ones are left
 *  out. Needs-you and review are oldest first — the one that has waited
 *  longest is next — while working and parked keep the dock's order. */
export function inboxGroups(cards: readonly Card[]): Group[] {
  const sections: { id: string; label: string; tone: AgentState; states: AgentState[]; oldestFirst: boolean }[] = [
    { id: "needs", label: "Needs you", tone: "blocked", states: ["blocked"], oldestFirst: true },
    { id: "review", label: "Ready to review", tone: "done", states: ["done"], oldestFirst: true },
    { id: "working", label: "Working", tone: "working", states: ["working"], oldestFirst: false },
    { id: "parked", label: "Parked", tone: "idle", states: ["idle", "unknown"], oldestFirst: false },
  ];
  const groups: Group[] = [];
  for (const s of sections) {
    const members = cards.filter((c) => s.states.includes(effectiveState(c.ws)));
    if (members.length === 0) continue;
    if (s.oldestFirst) members.sort((a, b) => a.since - b.since);
    groups.push({ id: s.id, label: s.label, tone: s.tone, cards: members });
  }
  return groups;
}

function basename(path: string): string {
  const trimmed = path.replace(/[\\/]+$/, "");
  return trimmed.slice(Math.max(trimmed.lastIndexOf("/"), trimmed.lastIndexOf("\\")) + 1) || trimmed;
}

/** A project's short name: the last segment of its path (a directory, or
 *  a remote's `host/owner/repo`), with as many segments before it as it
 *  takes to tell it from every other project's — so `api`, then
 *  `acme/api` beside `other/api`, then `github.com/acme/api` beside
 *  `gitlab.com/acme/api`. */
export function projectLabel(path: string, all: readonly string[]): string {
  const parts = (p: string) => p.replace(/[\\/]+$/, "").split(/[\\/]+/).filter(Boolean);
  const own = parts(path);
  const others = all.filter((p) => p !== path).map(parts);
  for (let n = 1; n <= own.length; n++) {
    const tail = own.slice(-n).join("/");
    if (!others.some((o) => o.slice(-n).join("/") === tail)) return tail;
  }
  return own.join("/") || path;
}

/** Hosts whose paths are case-insensitive: `Acme/Api` and `acme/api` there
 *  are one repository. */
const CASELESS_HOSTS = new Set(["github.com", "gitlab.com", "bitbucket.org"]);

/** `path` with `.` and `..` segments resolved (POSIX or Windows separators). */
function resolvePath(path: string): string {
  const sep = path.includes("\\") && !path.includes("/") ? "\\" : "/";
  const lead = path.match(/^([A-Za-z]:)?[\\/]/)?.[0] ?? "";
  const out: string[] = [];
  for (const part of path.slice(lead.length).split(/[\\/]+/)) {
    if (part === "" || part === ".") continue;
    if (part === "..") out.pop();
    else out.push(part);
  }
  return lead + out.join(sep);
}

/** A git remote URL as an identity: scheme, user, port, `.git` and the
 *  host's case dropped, so `git@github.com:o/r.git`, `ssh://git@github.com/o/r`
 *  and `https://github.com/o/r` are one project. A path remote is a path:
 *  a relative one (`../api.git`) is resolved against `base`, the checkout it
 *  was read in, so two unrelated repos that both say `../api.git` stay
 *  apart. `null` when there is nothing to compare. */
export function normalizeRemote(url: string, base?: string): string | null {
  const u = url.trim();
  if (u === "") return null;
  let host = "";
  let path = "";
  const scheme = u.match(/^[a-z][a-z0-9+.-]*:\/\/(?:[^@/]*@)?([^/:]+)(?::\d+)?\/(.*)$/i);
  const scp = u.match(/^(?:[^@/]*@)?([^/:]+):(?!\/)(.*)$/);
  if (/^file:\/\//i.test(u)) {
    return resolvePath(u.replace(/^file:\/\//i, "")).replace(/\.git$/, "");
  } else if (scheme) [host, path] = [scheme[1], scheme[2]];
  else if (scp && !/^[A-Za-z]$/.test(scp[1])) [host, path] = [scp[1], scp[2]];
  else {
    const absolute = /^([A-Za-z]:)?[\\/]/.test(u);
    const full = absolute || !base ? u : `${base.replace(/[\\/]+$/, "")}/${u}`;
    return resolvePath(full).replace(/\.git$/, "") || null;
  }
  host = host.toLowerCase();
  path = path.replace(/\/+$/, "").replace(/\.git$/, "").replace(/^\/+/, "");
  if (CASELESS_HOSTS.has(host)) path = path.toLowerCase();
  if (path === "") return null;
  return `${host}/${path}`;
}

/** What a workspace's project is, for grouping: its remote when it has
 *  one — clones of one project anywhere on disk are one project — else the
 *  repository directory. */
export function projectKey(ws: WorkspaceLike): string {
  return ws.remote ? `remote:${ws.remote}` : `path:${ws.projectPath || ws.root}`;
}

/** Projects: one group per project, alphabetical, each sorted by urgency.
 *  A project with a remote is named for it (`owner/repo`, or just `repo`
 *  when no other project shares the name); one without, for its directory. */
export function projectGroups(cards: readonly Card[]): Group[] {
  const keys = [...new Set(cards.map((c) => projectKey(c.ws)))];
  const pathOf = (k: string) => k.replace(/^(remote|path):/, "");
  const all = keys.map(pathOf);
  const groups = keys.map((k) => ({
    id: `project:${k}`,
    label: projectLabel(pathOf(k), all),
    tone: null,
    cards: cards.filter((c) => projectKey(c.ws) === k).sort(byUrgency),
  }));
  return groups.sort((a, b) => a.label.localeCompare(b.label));
}

/** A folder's full name, its ancestors first: `Work / Billing`. */
export function folderPath(folderId: string, folders: readonly FolderLike[]): string {
  const names: string[] = [];
  const seen = new Set<string>();
  let id: string | null = folderId;
  while (id !== null && !seen.has(id)) {
    seen.add(id);
    const f = folders.find((x) => x.folderId === id);
    if (!f) break;
    names.unshift(f.name);
    id = f.parent;
  }
  return names.join(" / ");
}

/** Folders: the dock's folders in the dock's order, then the workspaces
 *  filed in none of them. Members keep the dock's order: the user arranged
 *  them, so this lens does not re-sort. Empty folders are left out. */
export function folderGroups(cards: readonly Card[], folders: readonly FolderLike[]): Group[] {
  const known = new Set(folders.map((f) => f.folderId));
  const groups: Group[] = [];
  for (const f of folders) {
    const members = cards.filter((c) => c.ws.folderId === f.folderId);
    if (members.length === 0) continue;
    groups.push({ id: `folder:${f.folderId}`, label: folderPath(f.folderId, folders), tone: null, cards: members });
  }
  const loose = cards.filter((c) => c.ws.folderId === null || !known.has(c.ws.folderId));
  if (loose.length > 0) groups.push({ id: "folder:", label: "Not in a folder", tone: null, cards: loose });
  return groups;
}

/** Board columns, every column present even when empty so cards have
 *  somewhere to be pinned to. Each column is sorted by urgency. */
export function boardColumns(cards: readonly Card[]): Group[] {
  return COLUMNS.map((col) => ({
    id: col.id,
    label: col.label,
    tone: null,
    cards: cards.filter((c) => columnOf(c) === col.id).sort(byUrgency),
  }));
}

export function groupsFor(lens: LensId, cards: readonly Card[], folders: readonly FolderLike[]): Group[] {
  switch (lens) {
    case "inbox":
      return inboxGroups(cards);
    case "projects":
      return projectGroups(cards);
    case "folders":
      return folderGroups(cards, folders);
    case "board":
      return boardColumns(cards);
  }
}

/** Pin `card` one column left (`-1`) or right (`+1`) of where it is shown.
 *  Returns the new pin, or `null` when moving it there lands on its
 *  automatic column — that is the same as having no pin. */
export function shiftPin(card: Card, delta: number): ColumnId | null {
  const ids = COLUMNS.map((c) => c.id);
  const at = ids.indexOf(columnOf(card));
  const to = ids[Math.max(0, Math.min(ids.length - 1, at + delta))];
  return to === autoColumn(card.ws) ? null : to;
}

/** The status-bar beacon: how many agents want you, and how many finished
 *  work nobody has looked at. Empty when there is nothing to act on, so it
 *  takes no room — a light that is always on is a light nobody reads. */
export function beaconText(cards: readonly Card[]): string {
  let blocked = 0;
  let done = 0;
  for (const c of cards) {
    const s = effectiveState(c.ws);
    if (s === "blocked") blocked++;
    else if (s === "done") done++;
  }
  const parts: string[] = [];
  if (blocked > 0) parts.push(`● ${blocked} waiting`);
  if (done > 0) parts.push(`✓ ${done} done`);
  return parts.join("  ");
}

/** The card to visit next: the longest-waiting agent that needs you, else
 *  the oldest unseen finished work — never the workspace already open. */
export function nextAttention(cards: readonly Card[], activeWindowId: number): Card | null {
  const open = cards.filter((c) => c.ws.windowId !== activeWindowId && c.ws.kind !== "discovered");
  for (const state of ["blocked", "done"] as const) {
    const hit = open.filter((c) => effectiveState(c.ws) === state).sort((a, b) => a.since - b.since)[0];
    if (hit) return hit;
  }
  return null;
}

/** Short, coarse age: `<1m`, `4m`, `2h`, `3d`. No seconds: a counter that
 *  changed every tick would rebuild the board every tick for nothing. */
export function formatAge(ms: number): string {
  const m = Math.floor(Math.max(0, ms) / 60_000);
  if (m < 1) return "<1m";
  if (m < 60) return `${m}m`;
  const h = Math.floor(m / 60);
  if (h < 48) return `${h}h`;
  return `${Math.floor(h / 24)}d`;
}

/** Track when each workspace entered its current state. A workspace seen
 *  for the first time counts from `now`: the plugin cannot know earlier. */
export function trackSince(
  prev: ReadonlyMap<string, { state: AgentState; since: number }>,
  workspaces: readonly WorkspaceLike[],
  now: number,
): Map<string, { state: AgentState; since: number }> {
  const next = new Map<string, { state: AgentState; since: number }>();
  for (const ws of workspaces) {
    const key = cardKey(ws);
    const state = effectiveState(ws);
    const old = prev.get(key);
    next.set(key, old && old.state === state ? old : { state, since: now });
  }
  return next;
}

const LENS_IDS = new Set<string>(LENSES.map((l) => l.id));
const COLUMN_IDS = new Set<string>(COLUMNS.map((c) => c.id));

/** Read saved state back, dropping anything malformed rather than failing:
 *  a hand-edited or older file must not stop the board from opening. */
export function normalizeSaved(raw: unknown): SavedState {
  const out: SavedState = { lens: "inbox", pins: {} };
  if (raw === null || typeof raw !== "object") return out;
  const r = raw as Record<string, unknown>;
  if (typeof r.lens === "string" && LENS_IDS.has(r.lens)) out.lens = r.lens as LensId;
  if (r.pins !== null && typeof r.pins === "object") {
    for (const [k, v] of Object.entries(r.pins as Record<string, unknown>)) {
      if (typeof v === "string" && COLUMN_IDS.has(v)) out.pins[k] = v as ColumnId;
    }
  }
  return out;
}

/** The repository a worktree belongs to, from `git rev-parse
 *  --path-format=absolute --git-common-dir` run in it: the directory that
 *  holds the shared `.git`, or the bare repo itself. `null` when git said
 *  nothing usable. Every worktree of one repo answers the same, which is
 *  what lets the Projects lens group them. */
export function repoFromCommonDir(stdout: string): string | null {
  const dir = stdout.trim().replace(/[\\/]+$/, "");
  if (dir === "" || !/^([A-Za-z]:)?[\\/]/.test(dir)) return null;
  const m = dir.match(/^(.*)[\\/]\.git$/);
  return m ? m[1] || "/" : dir;
}
