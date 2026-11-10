/// <reference path="./lib/fresh.d.ts" />

/**
 * Workboard — every Orchestrator workspace and coding-agent session on one
 * full-screen board, arranged through switchable lenses:
 *
 *   Inbox     what needs you first: waiting agents, then finished work to
 *             review, then what is running, then what is parked
 *   Projects  grouped by the repo each workspace was cut from
 *   Folders   the dock's own folders, so your hand-made arrangement is a lens
 *   Board     a kanban board that fills itself from agent state; `<` / `>`
 *             pin a card to another column, and the pin is remembered
 *
 * Plus a status-bar beacon (`● 2 waiting  ✓ 1 done`) that is empty while
 * nothing needs you; clicking it jumps to the agent that has waited longest.
 *
 * Everything it shows comes from other plugins' APIs: the workspaces from
 * the Orchestrator's, and sessions from registered sources — the editor's
 * own two (agents running outside the editor, from `live-sessions` through
 * the Orchestrator; sessions found by a scan, from `agent-discovery`) and any
 * a plugin adds with `registerSource` on this plugin's API. Projects are
 * resolved by `lib/project_identity.ts`, as the session scan resolves them.
 * It keeps only its own lens and pins. Sorting, grouping and layout are pure
 * and live in `lib/workboard_model.ts`, `lib/workboard_view.ts` and
 * `lib/workboard_sources.ts`.
 */

import type { LiveSession } from "./lib/live_sessions.ts";
import { resolveProjects } from "./lib/project_identity.ts";
import { FloatingWidgetPanel, type WidgetSpec } from "./lib/widgets.ts";
import { WindowMachines } from "./lib/window_machines.ts";
import {
  type AgentState,
  beaconText,
  type Card,
  cardKey,
  columnOf,
  effectiveState,
  type FolderLike,
  groupsFor,
  inEditor,
  itemSubject,
  LENSES,
  type LensId,
  nextAttention,
  normalizeSaved,
  type SavedState,
  shiftPin,
  trackSince,
  type WorkboardApi,
  type WorkboardSource,
  type WorkspaceLike,
  withoutCovered,
} from "./lib/workboard_model.ts";
import { detectedItems, liveSessionItem, parseDetectedItemId } from "./lib/workboard_sources.ts";
import {
  cardItemKey,
  cardKeyOf,
  COLUMN_KEY_PREFIX,
  COLUMN_KEYS,
  columnKey,
  groupIdOf,
  LENS_KEY_PREFIX,
  LIST_KEY_PREFIX,
  listKey,
  panelSpec,
  treeItemKeys,
} from "./lib/workboard_view.ts";

const editor = getEditor();

// ── The Orchestrator's surface, as far as this plugin uses it ──────────────

interface OrchestratorLike {
  listWorkspaces(): WorkspaceLike[];
  listFolders(): FolderLike[];
  getWorkspace(target: string | number): { explain?: { question?: string | null } } | null;
  focusWorkspace(target: string | number): Promise<boolean>;
  moveWorkspace(target: string | number, folderId: string | null): boolean;
  /** Agents running outside the editor that no workspace stands for. */
  listExternalSessions?(): LiveSession[];
  openExternalSession?(key: string): Promise<boolean>;
}

/** Looked up on every use: plugins load in no fixed order, and the
 *  Orchestrator can be turned off and on while the editor runs. */
function orchestrator(): OrchestratorLike | null {
  return editor.getPluginApi("orchestrator") as OrchestratorLike | null;
}

// ── State ──────────────────────────────────────────────────────────────────

const STATE_KEY = "state";
const MODE = "workboard";
const TICK_MS = 1000;
const BEACON = "attention";

let saved: SavedState = normalizeSaved(editor.getGlobalState(STATE_KEY));
let since = new Map<string, { state: AgentState; since: number }>();
let cards: Card[] = [];
let folders: FolderLike[] = [];
let panel: FloatingWidgetPanel | null = null;
/** Which card each tree's cursor is on (tree key → card key), from the
 *  host's `select` events and from `reveal`. The host keeps a tree's cursor
 *  as a row *index*, so when cards move between rows (one starts waiting and
 *  jumps up the Inbox) the host's cursor stays on the row and lands on
 *  another card. `syncHost` puts it back on this card after every render;
 *  it is the card, not the row, that the keys act on. */
const selected = new Map<string, string>();
/** What `syncHost` last told the host, per tree: the cursor's row and the
 *  open groups. A host-side change (an arrow key, a fold) arrives as an
 *  event and updates these too, so nothing is pushed back needlessly. */
const pushedRow = new Map<string, number>();
const pushedOpen = new Map<string, string>();
/** Groups the user folded, from `expand` events, so a group appearing
 *  later does not reset the others. */
const collapsed = new Set<string>();
/** Each tree's item keys in the last spec pushed, to read an event's
 *  `index` back to the item it names. */
let itemKeysByTree = new Map<string, string[]>();
/** The last spec pushed, to skip identical repaints. */
let lastFrame = "";

function save(): void {
  editor.setGlobalState(STATE_KEY, saved);
}

/** Refresh `cards` from the Orchestrator and the sources. Questions are
 *  asked only of the workspaces that are waiting — the only ones that have
 *  one. */
function refresh(): void {
  const orch = orchestrator();
  let list: WorkspaceLike[] = [];
  folders = [];
  try {
    list = orch?.listWorkspaces().filter((w) => w.kind !== "discovered") ?? [];
    folders = orch?.listFolders() ?? [];
  } catch (e) {
    editor.debug(`workboard: listWorkspaces failed: ${e}`);
    return;
  }
  const questions = new Map<string, string>();
  for (const src of sources.values()) {
    let items;
    try {
      items = src.list();
    } catch (e) {
      editor.debug(`workboard: source ${src.id} failed to list: ${e}`);
      continue;
    }
    for (const item of items) {
      const ws = itemSubject(src, item);
      list.push(ws);
      if (item.question) questions.set(cardKey(ws), item.question);
    }
  }
  list = withoutCovered(list.map(withProject));
  const now = Date.now();
  since = trackSince(since, list, now);
  cards = list.map((ws) => {
    const key = cardKey(ws);
    let question = questions.get(key) ?? null;
    // Only the open board shows questions, and each one costs the
    // Orchestrator a full listing: the beacon needs none.
    if (orch && panel && inEditor(ws) && ws.kind !== "pending" && effectiveState(ws) === "blocked") {
      try {
        question = orch.getWorkspace(ws.workspaceId || ws.windowId)?.explain?.question?.trim() || null;
      } catch {
        question = null;
      }
    }
    return { key, ws, since: since.get(key)?.since ?? now, pin: saved.pins[key] ?? null, question };
  });
}

// ── Projects ──────────────────────────────────────────────────────────────

/** Each directory's project, by `machine\ndirectory`, as
 *  `lib/project_identity.ts` resolves it: every checkout of a repository,
 *  wherever it sits, under its remote. Until it answers, a card is grouped
 *  by its directory. */
const projects = new Map<string, { id: string; label: string }>();
/** When each `machine\ndirectory` was last asked: a repository can gain a
 *  remote, so the answer is asked for again after `RESOLVE_AGAIN_MS`. */
const askedAt = new Map<string, number>();
const RESOLVE_AGAIN_MS = 60_000;
/** Machines with a resolution in flight: one at a time per machine. */
const resolving = new Set<string>();
const windowMachines = new WindowMachines(editor);
editor.on("window_closed", ({ id }) => windowMachines.close(id));
let localMachine: Promise<FreshMachine> | null = null;

/** The machine `ws`'s directory is on: this computer, or for a workspace on
 *  a remote backend, its window's. Null for a directory elsewhere (a cloud
 *  session, an SSH host's) — its source names its project, if anyone does. */
function machineOf(ws: WorkspaceLike): string | null {
  if (inEditor(ws)) return ws.backend ? (ws.windowId >= 0 ? `window:${ws.windowId}` : null) : "local";
  return ws.where ? null : "local";
}

function openMachine(machine: string): Promise<FreshMachine> {
  if (machine.startsWith("window:")) return windowMachines.get(Number(machine.slice("window:".length)));
  if (!localMachine) {
    localMachine = editor.openMachine({
      filesystem: { kind: "local" },
      spawner: { kind: "local" },
      terminal_wrapper: { kind: "host-shell" },
    });
    localMachine.catch(() => (localMachine = null));
  }
  return localMachine;
}

/** `ws` with its project, when resolved; a resolution is started for its
 *  directory when it is due. A source's own project stands. */
function withProject(ws: WorkspaceLike): WorkspaceLike {
  if (ws.project || ws.root === "") return ws;
  const machine = machineOf(ws);
  if (machine === null) return ws;
  const project = projects.get(`${machine}\n${ws.root}`);
  if (!project) queueResolve(machine, ws.root);
  return project ? { ...ws, project } : ws;
}

const dueByMachine = new Map<string, Set<string>>();

function queueResolve(machine: string, dir: string): void {
  const k = `${machine}\n${dir}`;
  if ((askedAt.get(k) ?? -Infinity) + RESOLVE_AGAIN_MS > Date.now()) return;
  const due = dueByMachine.get(machine) ?? new Set<string>();
  due.add(dir);
  dueByMachine.set(machine, due);
  pumpResolve(machine);
}

/** Resolve every directory due on `machine` in one batch: one round of
 *  file reads, two for linked worktrees. */
function pumpResolve(machine: string): void {
  const due = dueByMachine.get(machine);
  if (resolving.has(machine) || !due || due.size === 0) return;
  dueByMachine.delete(machine);
  const dirs = [...due];
  const now = Date.now();
  for (const d of dirs) askedAt.set(`${machine}\n${d}`, now);
  resolving.add(machine);
  void openMachine(machine)
    .then((m) => resolveProjects(m, dirs))
    .then((found) => {
      for (const [dir, p] of found) projects.set(`${machine}\n${dir}`, { id: p.id, label: p.label });
      refresh();
      render();
    })
    .catch((e) => editor.debug(`workboard: resolving projects on ${machine} failed: ${e}`))
    .finally(() => {
      resolving.delete(machine);
      pumpResolve(machine);
    });
}

// ── Sources ───────────────────────────────────────────────────────────────

const sources = new Map<string, WorkboardSource>();
/** When each source last had `refresh` called. */
const refreshedAt = new Map<string, number>();
const DEFAULT_REFRESH_MS = 30_000;
/** Opening the board refreshes any source this stale. */
const OPEN_REFRESH_MS = 10_000;

function registerSource(source: WorkboardSource): () => void {
  sources.set(source.id, source);
  refreshedAt.delete(source.id);
  if (panel) refreshSources();
  return () => {
    if (sources.get(source.id) !== source) return;
    sources.delete(source.id);
    refreshedAt.delete(source.id);
    refresh();
    render();
  };
}

/** Ask each source for fresh items once its interval (or `maxAge`, when
 *  given) has passed; redraw as each answers. */
function refreshSources(maxAge?: number): void {
  const now = Date.now();
  for (const src of sources.values()) {
    if (!src.refresh) continue;
    const last = refreshedAt.get(src.id);
    if (last !== undefined && now - last < (maxAge ?? src.refreshMs ?? DEFAULT_REFRESH_MS)) continue;
    refreshedAt.set(src.id, now);
    void Promise.resolve()
      .then(() => src.refresh!())
      .catch((e) => editor.debug(`workboard: source ${src.id} failed to refresh: ${e}`))
      .finally(() => {
        refresh();
        paintBeacon();
        render();
      });
  }
}

/** Open a source's card; a reason it could not open goes to the status bar. */
async function openSourceCard(card: Card): Promise<void> {
  const ref = card.ws.source!;
  const src = sources.get(ref.id);
  let why: string | null = src ? null : "its source is gone";
  if (src) {
    try {
      why = (await src.open(ref.item)) || null;
    } catch (e) {
      why = e instanceof Error ? e.message : String(e);
    }
  }
  if (why) editor.setStatus(`Workboard: could not open ${card.ws.name}: ${why}`);
}

/** Agents running outside the editor: the `live-sessions` plugin finds them
 *  and the Orchestrator keeps those no workspace stands for, as its dock
 *  lists them. Opening one does what the dock row's Open does. */
registerSource({
  id: "external",
  label: "Running outside the editor",
  kind: "running",
  list: () => (orchestrator()?.listExternalSessions?.() ?? []).map(liveSessionItem),
  refresh: () => (editor.getPluginApi("live-sessions") as { refresh(): Promise<void> } | null)?.refresh(),
  refreshMs: 60_000,
  open: async (key) => {
    const ok = await orchestrator()?.openExternalSession?.(key);
    return ok ? null : "it is no longer running";
  },
});

/** Sessions the `agent-discovery` scan found on disk or in a multiplexer,
 *  on every reachable machine. Opening one imports it, as the Import
 *  sessions dialog's Enter does. */
registerSource({
  id: "detected",
  label: "Detected",
  kind: "history",
  list: () => detectedItems(editor.getPluginApi("agent-discovery")?.detectedSessions() ?? [], Date.now()),
  refresh: () => editor.getPluginApi("agent-discovery")?.scan(),
  refreshMs: 5 * 60_000,
  open: (id) => {
    const api = editor.getPluginApi("agent-discovery");
    const ref = parseDetectedItemId(id);
    if (!api || !ref) return "session import is not available";
    return api.importSession(ref.machineKey, ref.sessionId, ref.tool || undefined);
  },
});

// ── Beacon ─────────────────────────────────────────────────────────────────

editor.defineConfigBoolean("statusBarBeacon", {
  default: true,
  description:
    "Show how many agents are waiting on you, and how many finished, in the status bar. Empty when nothing needs you; click it to jump there.",
});

editor.registerStatusBarElement(BEACON, "Agents needing you");
const beaconToken = `{${editor.pluginName()}:${BEACON}}`;

function beaconEnabled(): boolean {
  const cfg = (editor.getPluginConfig() ?? {}) as { statusBarBeacon?: boolean };
  return cfg.statusBarBeacon !== false;
}

/** Put the beacon on the status bar's right for this run, unless the user
 *  already placed it (or turned it off). Written to the runtime layer, not
 *  to config.json, so turning the setting off removes it at next start. */
function placeBeacon(): void {
  if (!beaconEnabled()) return;
  const bar = editor.getConfig().editor?.status_bar ?? {};
  const placed = [...(bar.left ?? []), ...(bar.right ?? [])].includes(beaconToken);
  if (!placed) editor.setSetting("editor.status_bar.right", [beaconToken, ...(bar.right ?? [])]);
}

function paintBeacon(): void {
  const buf = editor.getActiveBufferId();
  if (!buf) return;
  editor.setStatusBarValue(buf, BEACON, beaconEnabled() ? beaconText(cards) : "");
}

/** Jump to the agent that has waited longest, else the oldest finished
 *  work nobody has looked at. */
async function jumpNext(): Promise<void> {
  refresh();
  const card = nextAttention(cards, editor.activeWindow());
  if (!card) {
    editor.setStatus("Workboard: nothing needs you right now");
    return;
  }
  await orchestrator()?.focusWorkspace(card.ws.workspaceId || card.ws.windowId);
}

// ── The board panel ───────────────────────────────────────────────────────

function cardByKey(key: string | null | undefined): Card | null {
  return key ? cards.find((c) => c.key === key) ?? null : null;
}

function viewInput() {
  return {
    lens: saved.lens,
    cards,
    folders,
    now: Date.now(),
    selected,
    collapsed,
  };
}

function render(): void {
  if (!panel) return;
  const spec: WidgetSpec = panelSpec(viewInput());
  const frame = JSON.stringify(spec);
  if (frame === lastFrame) return;
  lastFrame = frame;
  itemKeysByTree = treeItemKeys(spec);
  panel.update(spec);
  syncHost();
}

/** Bring each tree's host-side cursor and folds into line with the cards.
 *
 *  Once a tree has state of its own the host keeps it and stops reading the
 *  spec's `selectedIndex` / `expandedKeys` (as `pkg.ts` documents), so a
 *  re-render alone cannot move them: a cursor stays on its row while the
 *  cards move under it, and a group that appears later stays folded. Each
 *  tree's cursor goes back on its card — the first card when its card has
 *  left the tree — and its folds are pushed when the groups change. */
function syncHost(): void {
  if (!panel) return;
  for (const [tree, keys] of itemKeysByTree) {
    const list = tree.startsWith(LIST_KEY_PREFIX);
    if (list) {
      const open = keys.filter((k) => {
        const g = groupIdOf(k);
        return g !== null && !collapsed.has(g);
      });
      const sig = open.join("\n");
      if (pushedOpen.get(tree) !== sig) {
        panel.mutate({ kind: "setExpandedKeys", widgetKey: tree, keys: open });
        pushedOpen.set(tree, sig);
      }
    }
    const want = selected.get(tree);
    let row = want ? keys.indexOf(cardItemKey(want)) : -1;
    if (row < 0) {
      selected.delete(tree);
      row = keys.findIndex((k) => cardKeyOf(k) !== null);
      if (row < 0) continue;
      selected.set(tree, cardKeyOf(keys[row])!);
    }
    // A card inside a folded group is not on screen: leave the cursor where
    // the host has it rather than put it on a hidden row.
    if (list && insideFolded(keys, row)) continue;
    if (pushedRow.get(tree) !== row) {
      panel.setSelectedIndex(tree, row);
      pushedRow.set(tree, row);
    }
  }
}

/** Whether row `row` of a list tree sits under a folded group heading. */
function insideFolded(keys: readonly string[], row: number): boolean {
  for (let i = row; i >= 0; i--) {
    const g = groupIdOf(keys[i]);
    if (g !== null) return collapsed.has(g);
  }
  return false;
}

/** The workspace the cursor is on, whichever lens shows it: what a lens
 *  switch keeps the cursor on. Set from every `select` the host reports
 *  and by `reveal`. */
let current: string | null = null;

/** Put the cursor on `card` in the tree that shows it, unfold the group it
 *  sits in, and focus that tree — the host scrolls the selection into view. */
function reveal(card: Card): void {
  if (!panel) return;
  const tree = saved.lens === "board" ? columnKey(columnOf(card)) : listKey(saved.lens);
  selected.set(tree, card.key);
  current = card.key;
  if (saved.lens !== "board") {
    const home = groupsFor(saved.lens, cards, folders).find((g) => g.cards.some((c) => c.key === card.key));
    if (home) collapsed.delete(home.id);
  }
  render();
  // The spec may be unchanged (the card was already the tree's), but the
  // host's cursor may not be on it: push it regardless.
  pushedRow.delete(tree);
  pushedOpen.delete(tree);
  syncHost();
  panel.mutate({ kind: "setFocusKey", widgetKey: tree });
}

/** The card under the cursor of the focused tree: its recorded card while
 *  that card is still in the tree, else the first card (where `syncHost`
 *  puts the host's cursor too). */
function focusedCard(): Card | null {
  if (!panel) return null;
  return cardInTree(panel.focusKey());
}

function cardInTree(tree: string): Card | null {
  const keys = itemKeysByTree.get(tree) ?? [];
  const want = selected.get(tree);
  if (want && keys.includes(cardItemKey(want))) return cardByKey(want);
  return cardByKey(cardKeyOf(keys.find((k) => cardKeyOf(k) !== null) ?? ""));
}

function openBoard(): void {
  if (panel) {
    closeBoard();
    return;
  }
  if (!orchestrator()) {
    editor.setStatus("Workboard needs the Orchestrator plugin, which is not loaded");
    return;
  }
  refreshSources(OPEN_REFRESH_MS);
  refresh();
  lastFrame = "";
  panel = new FloatingWidgetPanel();
  // A fresh panel has no host-side state yet: everything is pushed anew.
  pushedRow.clear();
  pushedOpen.clear();
  const spec = panelSpec(viewInput());
  itemKeysByTree = treeItemKeys(spec);
  lastFrame = JSON.stringify(spec);
  panel.mount(spec, { widthPct: 100, heightPct: 100, title: "Workboard", closable: true, mode: MODE });
  // Cover the whole frame, dock included: the board is the overview. And be
  // that tall, not as tall as the rows, so the trees fill it and scroll.
  editor.floatingPanelControl(panel.id(), "fullscreen", 1);
  editor.floatingPanelControl(panel.id(), "fill", 1);
  focusLensStart();
}

/** On opening a lens, start on what needs you: the first waiting card,
 *  else the first card. */
function focusLensStart(): void {
  // What needs you first; with nothing waiting, the lens's first card — a
  // tree must hold the focus, or the board's keys (pin, unpin, next) would
  // have no card to act on.
  const start = nextAttention(cards, -1) ??
    groupsFor(saved.lens, cards, folders).flatMap((g) => g.cards)[0] ??
    null;
  if (start) reveal(start);
  else if (saved.lens !== "board") panel?.mutate({ kind: "setFocusKey", widgetKey: listKey(saved.lens) });
}

function closeBoard(): void {
  if (!panel) return;
  panel.unmount();
  panel = null;
}

// ── Keys and events ───────────────────────────────────────────────────────

function setLens(lens: LensId): void {
  if (lens === saved.lens) return;
  saved.lens = lens;
  save();
  render();
  // The same workspace stays under the cursor, wherever this lens files it.
  const keep = cardByKey(current);
  if (keep) reveal(keep);
  else focusLensStart();
}

function cycleLens(delta: number): void {
  const i = LENSES.findIndex((l) => l.id === saved.lens);
  setLens(LENSES[(i + delta + LENSES.length) % LENSES.length].id);
}

/** Step the cursor to the next card that wants you after the current one,
 *  in the order the lens shows them: waiting agents first, then finished
 *  work. */
function selectNextWaiting(): void {
  const order = groupsFor(saved.lens, cards, folders).flatMap((g) => g.cards);
  const i = order.findIndex((c) => c.key === focusedCard()?.key);
  const rest = [...order.slice(i + 1), ...order.slice(0, i + 1)];
  const hit = rest.find((c) => effectiveState(c.ws) === "blocked") ??
    rest.find((c) => effectiveState(c.ws) === "done");
  if (!hit) {
    editor.setStatus("Workboard: nothing needs you right now");
    return;
  }
  reveal(hit);
}

async function openCard(card: Card): Promise<void> {
  closeBoard();
  if (card.ws.source) return openSourceCard(card);
  const ok = await orchestrator()?.focusWorkspace(card.ws.workspaceId || card.ws.windowId);
  if (!ok) editor.setStatus(`Workboard: could not open ${card.ws.name}`);
}

function pin(delta: number): void {
  if (saved.lens !== "board") {
    editor.setStatus("Workboard: switch to the Board lens (4) to pin cards to a column");
    return;
  }
  const card = focusedCard();
  if (!card) return;
  const next = shiftPin(card, delta);
  if (next === null) delete saved.pins[card.key];
  else saved.pins[card.key] = next;
  card.pin = next;
  save();
  reveal(card);
}

function unpin(): void {
  const card = focusedCard();
  if (!card || saved.lens !== "board" || !card.pin) return;
  delete saved.pins[card.key];
  card.pin = null;
  save();
  reveal(card);
}

/** Folders lens: a card dropped on a folder heading, or on a card filed in
 *  one, is filed in that folder ("Not in a folder" unfiles it). */
function fileDropped(itemKey: string, target: string): void {
  const card = cardByKey(cardKeyOf(itemKey));
  if (!card || !inEditor(card.ws)) return;
  const gid = groupIdOf(target);
  let folderId: string | null | undefined;
  if (gid !== null) {
    if (!gid.startsWith("folder:")) return;
    folderId = gid.slice("folder:".length) || null;
  } else {
    folderId = cardByKey(cardKeyOf(target))?.ws.folderId;
  }
  if (folderId === undefined || folderId === card.ws.folderId) return;
  try {
    orchestrator()?.moveWorkspace(card.ws.workspaceId || card.ws.windowId, folderId);
  } catch (e) {
    editor.setStatus(`Workboard: could not move ${card.ws.name}: ${e instanceof Error ? e.message : e}`);
    return;
  }
  refresh();
  reveal(cards.find((c) => c.key === card.key) ?? card);
}

const isTree = (key: string) => key.startsWith(LIST_KEY_PREFIX) || key.startsWith(COLUMN_KEY_PREFIX);

/** The item an event on a tree names: its `key`, else its `index` read back
 *  against the keys last pushed. */
function eventItem(treeKey: string, payload: Record<string, unknown>): string | null {
  if (typeof payload.key === "string") return payload.key;
  if (typeof payload.index === "number") return itemKeysByTree.get(treeKey)?.[payload.index] ?? null;
  return null;
}

editor.on("widget_event", (e) => {
  if (!panel || e.panel_id !== panel.id()) return;
  // Esc, the [×], or a click outside: the host has already unmounted it.
  if (e.event_type === "cancel") {
    panel = null;
    return;
  }
  const payload = (e.payload ?? {}) as Record<string, unknown>;
  if (e.widget_key.startsWith(LENS_KEY_PREFIX)) {
    if (e.event_type === "activate") setLens(e.widget_key.slice(LENS_KEY_PREFIX.length) as LensId);
    return;
  }
  // Focus moving into another tree (←/→ between the Board's columns) moves
  // the cursor without a `select`: the card it lands on is current now.
  if (e.event_type === "focus" && isTree(e.focus_key)) {
    const landed = cardInTree(e.focus_key);
    if (landed) current = landed.key;
  }
  if (!isTree(e.widget_key)) return;
  const item = eventItem(e.widget_key, payload);
  switch (e.event_type) {
    case "select": {
      const key = item && cardKeyOf(item);
      if (key) {
        selected.set(e.widget_key, key);
        current = key;
      }
      if (typeof payload.index === "number") pushedRow.set(e.widget_key, payload.index);
      return;
    }
    case "activate": {
      const card = cardByKey(item && cardKeyOf(item));
      if (card) void openCard(card);
      return;
    }
    case "expand": {
      const gid = item && groupIdOf(item);
      if (gid) payload.expanded === false ? collapsed.add(gid) : collapsed.delete(gid);
      // The host made this change itself; record it as already told.
      pushedOpen.set(
        e.widget_key,
        (itemKeysByTree.get(e.widget_key) ?? [])
          .filter((k) => {
            const g = groupIdOf(k);
            return g !== null && !collapsed.has(g);
          })
          .join("\n"),
      );
      return;
    }
    case "drop":
      if (saved.lens === "folders" && typeof payload.key === "string" && typeof payload.target === "string") {
        fileDropped(payload.key, payload.target);
      }
      return;
  }
});

registerHandler("workboard_next_lens", () => cycleLens(1));
registerHandler("workboard_prev_lens", () => cycleLens(-1));
registerHandler("workboard_lens_1", () => setLens("inbox"));
registerHandler("workboard_lens_2", () => setLens("projects"));
registerHandler("workboard_lens_3", () => setLens("folders"));
registerHandler("workboard_lens_4", () => setLens("board"));
registerHandler("workboard_pin_left", () => pin(-1));
registerHandler("workboard_pin_right", () => pin(1));
registerHandler("workboard_unpin", unpin);
registerHandler("workboard_select_next_waiting", selectNextWaiting);
registerHandler("workboard_close", closeBoard);

// Only the board's own commands. Moving, opening (Enter), folding (←/→ on
// a heading) and stepping between Board columns (←/→ on a card) are the
// trees' and the panel's own keys. Tab switches lens rather than walking
// the focus ring, so it is declared a dialog-wide shortcut.
editor.defineMode(
  MODE,
  [
    ["Tab", "workboard_next_lens", "shortcut"],
    ["S-Tab", "workboard_prev_lens", "shortcut"],
    ["BackTab", "workboard_prev_lens", "shortcut"],
    ["1", "workboard_lens_1"],
    ["2", "workboard_lens_2"],
    ["3", "workboard_lens_3"],
    ["4", "workboard_lens_4"],
    ["<", "workboard_pin_left"],
    [">", "workboard_pin_right"],
    // Shift+←/→ move the card under the cursor, on the Board's columns only:
    // there a card's text never pans (the tree passes the chord on), and in
    // the list lenses Shift+←/→ keep panning a long row.
    ["S-Left", "workboard_pin_left", `on:${COLUMN_KEYS.join(",")}`],
    ["S-Right", "workboard_pin_right", `on:${COLUMN_KEYS.join(",")}`],
    ["p", "workboard_unpin"],
    ["n", "workboard_select_next_waiting"],
    ["q", "workboard_close"],
  ],
  true,
  false,
);

// ── Wiring ────────────────────────────────────────────────────────────────

/** With the board closed only the beacon needs fresh counts, and a few
 *  seconds' lag there costs nothing: listing every workspace is the
 *  Orchestrator's work, so it is done every `IDLE_TICKS`th tick. */
const IDLE_TICKS = 3;
let ticks = 0;
registerHandler("workboard_tick", () => {
  ticks++;
  if (!panel && ticks % IDLE_TICKS !== 0) return;
  // Sources are asked only while the board shows them: the beacon counts
  // workspaces alone.
  if (panel) refreshSources();
  refresh();
  paintBeacon();
  render();
});
editor.setInterval(TICK_MS, "workboard_tick");


registerHandler("workboard_paint_beacon", paintBeacon);
for (const event of ["buffer_activated", "after_file_open", "focus_gained"]) {
  editor.on(event, "workboard_paint_beacon");
}
registerHandler("workboard_ready", () => {
  placeBeacon();
  refresh();
  paintBeacon();
});
editor.on("ready", "workboard_ready");
editor.on("config_changed", "workboard_ready");

editor.on("status_bar_token_clicked", (data) => {
  if (data.plugin_name === editor.pluginName() && data.token_name === BEACON) void jumpNext();
});

registerHandler("workboard_toggle", openBoard);
registerHandler("workboard_jump_next", () => void jumpNext());
editor.registerCommand("Workboard: Open", "Every workspace and agent session on one board", "workboard_toggle", null, {
  terminalBypass: true,
});
editor.registerCommand(
  "Workboard: Next Waiting Agent",
  "Jump to the agent that has waited longest for you, else the oldest finished work",
  "workboard_jump_next",
  null,
  { terminalBypass: true },
);
editor.addMenuItem({ menu: "View", label: "Workboard", action: "workboard_toggle" });

editor.exportPluginApi("workboard", {
  open: () => {
    if (!panel) openBoard();
  },
  close: closeBoard,
  setLens: (lens: LensId) => setLens(lens),
  registerSource,
} satisfies WorkboardApi);
