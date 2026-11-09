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
 * Everything it knows comes from the Orchestrator's plugin API
 * (`editor.getPluginApi("orchestrator")`); it keeps only its own lens and
 * pins. Sorting, grouping and layout are pure and live in
 * `lib/workboard_model.ts` and `lib/workboard_view.ts`.
 */

import { FloatingWidgetPanel, type WidgetSpec } from "./lib/widgets.ts";
import {
  type AgentState,
  beaconText,
  type Card,
  cardKey,
  columnOf,
  effectiveState,
  type FolderLike,
  groupsFor,
  LENSES,
  type LensId,
  nextAttention,
  normalizeRemote,
  normalizeSaved,
  repoFromCommonDir,
  type SavedState,
  shiftPin,
  trackSince,
  type WorkspaceLike,
} from "./lib/workboard_model.ts";
import {
  cardItemKey,
  cardKeyOf,
  COLUMN_KEY_PREFIX,
  COLUMN_KEYS,
  columnKey,
  groupIdOf,
  groupItemKey,
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
/** Each local workspace root's repository, as git reports it (see
 *  `resolveRepo`). The Orchestrator's own `projectPath` is used until — or
 *  unless — git answers. */
const repoOf = new Map<string, { repo: string | null; remote: string | null } | null>();

let panel: FloatingWidgetPanel | null = null;
/** Where each tree's cursor is (tree key → card key), as the host reported
 *  it in `select` events. The host owns the selection; this is only so the
 *  pin and jump keys know which card they act on, and so a re-render keeps
 *  the cursor on the same card when cards move between rows. */
const selected = new Map<string, string>();
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

/** Refresh `cards` from the Orchestrator. Questions are asked only of the
 *  workspaces that are waiting — the only ones that have one. */
function refresh(): void {
  const orch = orchestrator();
  if (!orch) {
    cards = [];
    folders = [];
    return;
  }
  let list: WorkspaceLike[];
  try {
    list = orch.listWorkspaces().filter((w) => w.kind !== "discovered");
    folders = orch.listFolders();
  } catch (e) {
    editor.debug(`workboard: listWorkspaces failed: ${e}`);
    return;
  }
  const now = Date.now();
  since = trackSince(since, list, now);
  for (const ws of list) if (!ws.backend && !repoOf.has(ws.root)) void resolveRepo(ws.root);
  cards = list.map((raw) => {
    const git = repoOf.get(raw.root);
    const ws = {
      ...raw,
      ...(git?.repo ? { projectPath: git.repo } : {}),
      ...(git?.remote ? { remote: git.remote } : {}),
    };
    const key = cardKey(ws);
    let question: string | null = null;
    if (effectiveState(ws) === "blocked") {
      try {
        question = orch.getWorkspace(ws.workspaceId || ws.windowId)?.explain?.question?.trim() || null;
      } catch {
        question = null;
      }
    }
    return { key, ws, since: since.get(key)?.since ?? now, pin: saved.pins[key] ?? null, question };
  });
}

/** Ask git which repository `root` is a checkout of, once per root, so
 *  every worktree of a repo lands in the same Projects group. */
async function resolveRepo(root: string): Promise<void> {
  repoOf.set(root, null);
  const git = (...args: string[]) => editor.spawnHostProcess("git", ["-C", root, ...args]);
  try {
    const dir = await git("rev-parse", "--path-format=absolute", "--git-common-dir");
    if (dir.exit_code !== 0) return;
    // `origin` when there is one, else the first remote: clones of one
    // project share it wherever they sit on disk.
    let url = await git("remote", "get-url", "origin");
    if (url.exit_code !== 0) {
      const first = (await git("remote")).stdout.split("\n")[0]?.trim();
      url = first ? await git("remote", "get-url", first) : url;
    }
    repoOf.set(root, {
      repo: repoFromCommonDir(dir.stdout),
      remote: url.exit_code === 0 ? normalizeRemote(url.stdout) : null,
    });
  } catch {
    // Not a git checkout, or no git: the Orchestrator's projectPath stands.
  }
}

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
    const groups = groupsFor(saved.lens, cards, folders);
    const home = groups.find((g) => g.cards.some((c) => c.key === card.key));
    if (home && collapsed.delete(home.id)) {
      render();
      panel.mutate({
        kind: "setExpandedKeys",
        widgetKey: tree,
        keys: groups.filter((g) => !collapsed.has(g.id)).map((g) => groupItemKey(g.id)),
      });
    }
  }
  render();
  const index = itemKeysByTree.get(tree)?.indexOf(cardItemKey(card.key)) ?? -1;
  if (index >= 0) panel.setSelectedIndex(tree, index);
  panel.mutate({ kind: "setFocusKey", widgetKey: tree });
}

/** The card under the cursor of the focused tree. */
function focusedCard(): Card | null {
  if (!panel) return null;
  const tree = panel.focusKey();
  return cardByKey(selected.get(tree)) ??
    cardByKey(cardKeyOf(itemKeysByTree.get(tree)?.find((k) => cardKeyOf(k) !== null) ?? ""));
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
  refresh();
  lastFrame = "";
  panel = new FloatingWidgetPanel();
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
  const start = nextAttention(cards, -1) ?? null;
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
  if (!card) return;
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
    const landed = selected.get(e.focus_key) ??
      cardKeyOf(itemKeysByTree.get(e.focus_key)?.find((k) => cardKeyOf(k) !== null) ?? "");
    if (landed) current = landed;
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

registerHandler("workboard_tick", () => {
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
  /** The lens the board shows (and opens on next time). */
  setLens: (lens: LensId) => setLens(lens),
});
