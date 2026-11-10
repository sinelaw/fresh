/**
 * Workboard's model: how workspaces and sessions are sorted, grouped and
 * placed on the board. Pure — no `editor` — so it is unit-tested without one
 * (`tests/workboard_model.test.ts`); `workboard.ts` feeds it the
 * Orchestrator's `listWorkspaces()` and every registered session source's
 * items, and draws what it returns.
 *
 * Four lenses over one list of cards:
 *   * Inbox    — urgency first: needs you, ready to review, working, parked,
 *                then sessions found in history.
 *   * Projects — one group per project (`lib/project_identity.ts`).
 *   * Folders  — the dock's own folders, so a hand-made arrangement is a lens.
 *   * Board    — columns derived from agent state; a card can be pinned to a
 *                column by hand, and the pin wins until it is removed.
 */

export type AgentState = "working" | "blocked" | "done" | "idle" | "unknown";

/** One card's subject: a workspace (the part of the Orchestrator's
 *  `WorkspaceSummary` this needs), or a session from a registered source in
 *  the same shape. */
export interface WorkspaceLike {
  /** `live`, `discovered`, `pending`: an Orchestrator workspace. `external`:
   *  a session running outside the editor. `detected`: one found in an
   *  agent's history. */
  kind: "live" | "discovered" | "pending" | "external" | "detected";
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
  /** The project, as `lib/project_identity.ts` resolves it: `id` groups
   *  every checkout of one repository, `label` names it. */
  project?: { id: string; label: string };
  /** A source's session: which source, and its item id there. */
  source?: { id: string; item: string };
  /** Where a source's session runs, when not on this computer. */
  where?: string;
  /** A source's session: its last activity, epoch ms. */
  updatedAt?: number;
  /** A source's session: one line about it (`claude.ai`, `codex · devbox`). */
  note?: string;
}

/** Whether `ws` is a workspace in this editor, rather than a session found
 *  outside it. */
export function inEditor(ws: WorkspaceLike): boolean {
  return ws.kind === "live" || ws.kind === "pending" || ws.kind === "discovered";
}

// ── Session sources: what other plugins add to the board ───────────────────

/** One session a source reports. */
export interface WorkboardItem {
  /** Unique within its source, stable across `list()` calls. */
  id: string;
  title: string;
  /** Running sessions are placed by it, as workspaces are. */
  state: AgentState;
  /** The agent (`claude`, `codex`) or tool that runs it. */
  agent?: string;
  /** Its working directory, when it has one on a machine. */
  cwd?: string;
  /** Its git branch, when known. */
  branch?: string;
  /** Its project, as `lib/project_identity.ts` names one; without it, the
   *  board resolves `cwd` itself, on this computer, when `where` is unset. */
  project?: { id: string; label: string };
  /** Where it runs when not on this computer: `cloud`, an SSH host, an app. */
  where?: string;
  /** What it is waiting for, when it is waiting. */
  question?: string;
  /** One line about it when it is not waiting (`stopped`, `offline`). */
  status?: string;
  /** Last activity, epoch ms. */
  updatedAt?: number;
}

/** A source of sessions for the board: the editor's own (`external`,
 *  `detected`) are registered this way too. */
export interface WorkboardSource {
  /** Unique among sources; part of each card's key, so keep it stable. */
  id: string;
  label: string;
  /** `running`: live sessions, placed by state like workspaces. `history`:
   *  sessions found on disk, listed apart and parked on the Board. */
  kind: "running" | "history";
  /** The items now. Called on every board refresh, so answer from what is
   *  in hand; fetch in `refresh`. */
  list(): WorkboardItem[];
  /** Ask for fresh items: when the board opens, and every `refreshMs` while
   *  it stays open. */
  refresh?(): void | Promise<void>;
  /** How often `refresh` runs while the board is open. Default 30 s. */
  refreshMs?: number;
  /** Enter or a double click on the item. Return a reason to show when it
   *  could not be opened. */
  open(id: string): void | string | null | Promise<void | string | null>;
}

/** The Workboard's surface, reachable through
 *  `editor.getPluginApi("workboard")`. */
export interface WorkboardApi {
  open(): void;
  close(): void;
  /** The lens the board shows (and opens on next time). */
  setLens(lens: LensId): void;
  /** Add a source of sessions; returns a function that removes it. A source
   *  with the id of one already registered replaces it. */
  registerSource(source: WorkboardSource): () => void;
}

declare global {
  interface FreshPluginRegistry {
    workboard: WorkboardApi;
  }
}

/** A source's item as a card's subject. */
export function itemSubject(source: WorkboardSource, item: WorkboardItem): WorkspaceLike {
  return {
    kind: source.kind === "running" ? "external" : "detected",
    workspaceId: "",
    windowId: -1,
    active: false,
    folderId: null,
    name: item.title,
    root: item.cwd ?? "",
    projectPath: item.cwd ?? "",
    agentState: source.kind === "running" ? item.state : "idle",
    ...(item.branch ? { branch: item.branch } : {}),
    title: item.agent ?? source.label,
    ...(item.project ? { project: item.project } : {}),
    source: { id: source.id, item: item.id },
    ...(item.where ? { where: item.where } : {}),
    ...(item.updatedAt !== undefined ? { updatedAt: item.updatedAt } : {}),
    ...(item.status ? { note: item.status } : {}),
  };
}

/** Drop the sessions found in history whose directory is already on the
 *  board — a workspace's root, or where a running session works: that
 *  workspace or session is the card for that work. */
export function withoutCovered(subjects: readonly WorkspaceLike[]): WorkspaceLike[] {
  const trim = (p: string) => p.replace(/[\\/]+$/, "");
  const covered = new Set<string>();
  for (const ws of subjects) {
    if (ws.kind === "detected" || ws.where) continue;
    if (ws.root) covered.add(trim(ws.root));
  }
  return subjects.filter((ws) => ws.kind !== "detected" || ws.where !== undefined || !covered.has(trim(ws.root)));
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

export function cardKey(ws: { workspaceId: string; root: string; source?: { id: string; item: string } }): string {
  if (ws.source) return `src:${ws.source.id}:${ws.source.item}`;
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
  const live = cards.filter((c) => c.ws.kind !== "detected");
  const groups: Group[] = [];
  for (const s of sections) {
    const members = live.filter((c) => s.states.includes(effectiveState(c.ws)));
    if (members.length === 0) continue;
    if (s.oldestFirst) members.sort((a, b) => a.since - b.since);
    groups.push({ id: s.id, label: s.label, tone: s.tone, cards: members });
  }
  // Sessions found in history are not waiting on anyone: last, newest first.
  const history = cards
    .filter((c) => c.ws.kind === "detected")
    .sort((a, b) => (b.ws.updatedAt ?? 0) - (a.ws.updatedAt ?? 0));
  if (history.length > 0) groups.push({ id: "detected", label: "Detected", tone: null, cards: history });
  return groups;
}

function basename(path: string): string {
  const trimmed = path.replace(/[\\/]+$/, "");
  return trimmed.slice(Math.max(trimmed.lastIndexOf("/"), trimmed.lastIndexOf("\\")) + 1) || trimmed;
}

/** What a card's project is, for grouping: its resolved identity when it
 *  has one, else its directory. */
export function projectKey(ws: WorkspaceLike): string {
  return ws.project?.id ?? `path:${ws.projectPath || ws.root}`;
}

/** A card's project, as a heading or a cell names it. */
export function projectOf(ws: WorkspaceLike): string {
  return ws.project?.label ?? (basename(ws.projectPath || ws.root) || "No directory");
}

/** How long a card's age reads: a found session's since its last activity,
 *  anything else's since it entered its state. */
export function cardAge(card: Card, now: number): number {
  return card.ws.kind === "detected" && card.ws.updatedAt !== undefined ? now - card.ws.updatedAt : now - card.since;
}

/** Projects: one group per project, alphabetical, each sorted by urgency.
 *  Two projects that would read the same (`api` here and `api` there) are
 *  told apart by their identity, which names the host or the path. */
export function projectGroups(cards: readonly Card[]): Group[] {
  const byKey = new Map<string, Card[]>();
  for (const c of cards) {
    const k = projectKey(c.ws);
    byKey.set(k, [...(byKey.get(k) ?? []), c]);
  }
  const named = [...byKey].map(([k, members]) => ({ k, label: projectOf(members[0].ws), members }));
  const groups = named.map(({ k, label, members }) => ({
    id: `project:${k}`,
    label: named.some((o) => o.k !== k && o.label === label) ? k.replace(/^(repo|root|path):/, "") : label,
    tone: null,
    cards: members.sort(byUrgency),
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
  const loose = cards.filter((c) => inEditor(c.ws) && (c.ws.folderId === null || !known.has(c.ws.folderId)));
  if (loose.length > 0) groups.push({ id: "folder:", label: "Not in a folder", tone: null, cards: loose });
  // Sessions outside the editor are in no dock folder; they have their own.
  const outside = cards.filter((c) => !inEditor(c.ws));
  if (outside.length > 0) groups.push({ id: "outside", label: "Outside the editor", tone: null, cards: outside });
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
  // Only what the editor holds: a session outside it is the board's to
  // show, not the status bar's to count — there is nowhere to jump to.
  for (const c of cards) {
    if (!inEditor(c.ws)) continue;
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
  const open = cards.filter(
    (c) => c.ws.windowId !== activeWindowId && (c.ws.kind === "live" || c.ws.kind === "pending"),
  );
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
