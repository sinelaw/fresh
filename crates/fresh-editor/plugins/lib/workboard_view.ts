/**
 * Workboard's panel, as a widget spec. Pure — it only builds descriptions —
 * so its structure is unit-tested (`tests/workboard_view.test.ts`).
 *
 * The host owns everything interactive here (see
 * `docs/internal/widget-controls-own-interaction.md`): selection, scrolling,
 * folding, focus moving between Board columns, and every size. Nothing in
 * this file measures text or the screen — the panel is mounted with the
 * `"fill"` control, so the trees left to size themselves fill its height:
 *
 *   * the list lenses are one `tree` drawn as a table — group headings span
 *     the row, cards are cells the host fits to the panel;
 *   * the Board is a `row` of columns (a heading over a tree) that split the width
 *     between them, its cards bordered as the dock draws them;
 *   * the lens switch is a row of buttons, the counts a styled label.
 */

import {
  button,
  col,
  divider,
  flexSpacer,
  hintBar,
  label,
  row,
  spacer,
  styledRow,
  tree,
  treeNode,
  type TreeNode,
  type WidgetSpec,
} from "./widgets.ts";
import {
  type AgentState,
  type Card,
  COLUMNS,
  effectiveState,
  type FolderLike,
  cardAge,
  formatAge,
  type Group,
  groupsFor,
  inEditor,
  LENSES,
  type LensId,
  projectOf,
} from "./workboard_model.ts";

/** The glyph and colour for each state — the Orchestrator dock's own, so a
 *  workspace reads the same on both. */
export const STATE_MARK: Record<AgentState, { glyph: string; fg: string }> = {
  blocked: { glyph: "●", fg: "diagnostic.error_fg" },
  done: { glyph: "✓", fg: "ui.file_status_added_fg" },
  working: { glyph: "*", fg: "diagnostic.warning_fg" },
  idle: { glyph: "·", fg: "ui.menu_disabled_fg" },
  unknown: { glyph: "?", fg: "ui.menu_disabled_fg" },
};

const DIM = "ui.menu_disabled_fg";
const ACCENT = "ui.help_key_fg";
const PIN_GLYPH = "⊙";

/** Widget keys. Each lens has its own list tree, so each keeps its own
 *  selection, scroll and folds. */
export const LIST_KEY_PREFIX = "wb-list:";
export const COLUMN_KEY_PREFIX = "wb-col:";
export const LENS_KEY_PREFIX = "lens:";
export const CARD_PREFIX = "card:";
export const GROUP_PREFIX = "group:";

export const listKey = (lens: LensId) => `${LIST_KEY_PREFIX}${lens}`;
export const columnKey = (id: string) => `${COLUMN_KEY_PREFIX}${id}`;
export const cardItemKey = (cardKey: string) => `${CARD_PREFIX}${cardKey}`;
export const groupItemKey = (groupId: string) => `${GROUP_PREFIX}${groupId}`;

/** The card key an item key names, or `null` for a group heading. */
export function cardKeyOf(itemKey: string): string | null {
  return itemKey.startsWith(CARD_PREFIX) ? itemKey.slice(CARD_PREFIX.length) : null;
}

/** The group id a heading's item key names, or `null` for a card. */
export function groupIdOf(itemKey: string): string | null {
  return itemKey.startsWith(GROUP_PREFIX) ? itemKey.slice(GROUP_PREFIX.length) : null;
}

export interface ViewInput {
  lens: LensId;
  cards: readonly Card[];
  folders: readonly FolderLike[];
  now: number;
  /** The selected card per tree (a tree's widget key → card key): where
   *  each tree's cursor is, as the host last reported it. */
  selected: ReadonlyMap<string, string>;
  /** Group ids the user folded in the list lenses. */
  collapsed: ReadonlySet<string>;
}

/** What a card says about itself, one short phrase. */
export function statusText(card: Card): string {
  if (card.question) return card.question;
  if (card.ws.kind === "pending") {
    const p = card.ws.pending;
    if (p?.phase === "error") return `failed: ${p.message}`;
    if (p?.phase === "paused") return "not resumed (retry from the dock)";
    return p?.message || "starting…";
  }
  // A session outside the editor: its agent and where it runs.
  if (!inEditor(card.ws)) {
    const { title, note } = card.ws;
    return note && title && !note.includes(title) ? `${title} · ${note}` : note || title;
  }
  switch (effectiveState(card.ws)) {
    case "done":
      return "finished, not looked at";
    case "unknown":
      return "no agent output";
    default:
      return card.ws.title && !card.ws.name.includes(card.ws.title) ? card.ws.title : "";
  }
}

/** A card whose status line should catch the eye: an agent asking, or a
 *  create that failed. */
function alarming(card: Card): boolean {
  return card.question !== null || card.ws.pending?.phase === "error";
}

/** Branch and working-tree state: `feat/x · 2 changed ↑1`. */
export function gitText(card: Card): string {
  // A create that failed has no checkout of its own yet: what git reports
  // is the placeholder's surroundings, not this workspace.
  if (card.ws.pending?.phase === "error") return "";
  const g = card.ws.git;
  const parts: string[] = [];
  const branch = g?.branch ?? card.ws.branch;
  if (branch) parts.push(branch);
  const changes: string[] = [];
  if ((g?.dirty ?? 0) > 0) changes.push(`${g!.dirty} changed`);
  if ((g?.ahead ?? 0) > 0) changes.push(`↑${g!.ahead}`);
  if ((g?.behind ?? 0) > 0) changes.push(`↓${g!.behind}`);
  if (changes.length > 0) parts.push(changes.join(" "));
  return parts.join(" · ");
}

export function projectName(card: Card): string {
  if (card.ws.pending?.phase === "error") return "";
  return projectOf(card.ws);
}

function mark(card: Card): { text: string; style: Partial<OverlayOptions> } {
  const m = STATE_MARK[effectiveState(card.ws)];
  return { text: m.glyph, style: { fg: m.fg, bold: true } };
}

/** The lens buttons, then the counts of what needs you, flush right. */
export function headerSpec(lens: LensId, cards: readonly Card[]): WidgetSpec {
  const lensButtons = LENSES.map((l, i) =>
    button(`${i + 1} ${l.label}`, {
      key: `${LENS_KEY_PREFIX}${l.id}`,
      intent: l.id === lens ? "primary" : "normal",
      // Clicked, or chosen with 1–4 / Tab; Tab is the lens switch, so the
      // buttons stay out of the focus ring and focus stays on the cards.
      focusable: false,
    })
  );
  const counts: StyledSegment[] = [];
  for (const st of ["blocked", "done", "working"] as AgentState[]) {
    const n = cards.filter((c) => effectiveState(c.ws) === st).length;
    if (n === 0) continue;
    const m = STATE_MARK[st];
    if (counts.length > 0) counts.push({ text: "   " });
    // Compact — the glyphs and colours are the dock's, and the Inbox's
    // sections spell them out — so the lens buttons keep their room.
    counts.push({ text: `${m.glyph} ${n}`, style: { fg: m.fg, bold: true } });
  }
  if (counts.length === 0) counts.push({ text: "nothing needs you", style: { fg: DIM } });
  return row(...lensButtons, flexSpacer(), label(counts));
}

/** A list lens as one tree drawn as a table: a heading per group, a row
 *  per card. */
export function listSpec(lens: LensId, groups: readonly Group[], input: ViewInput): WidgetSpec {
  const nodes: TreeNode[] = [];
  const itemKeys: string[] = [];
  const key = listKey(lens);
  const chosen = input.selected.get(key);
  let selectedIndex = -1;
  for (const g of groups) {
    const tone = g.tone ? STATE_MARK[g.tone].fg : ACCENT;
    nodes.push(
      treeNode(
        styledRow([
          { text: g.tone ? g.label.toUpperCase() : g.label, style: { fg: tone, bold: true } },
          { text: `  ${g.cards.length}`, style: { fg: DIM } },
        ]),
        { hasChildren: true },
      ),
    );
    itemKeys.push(groupItemKey(g.id));
    for (const card of g.cards) {
      if (card.key === chosen) selectedIndex = nodes.length;
      nodes.push(
        treeNode(styledRow([{ text: card.ws.name }]), {
          depth: 1,
          // Folders is the lens you arrange by hand: drag a card onto
          // another folder (or onto a card in it) to file it there.
          // Only a workspace has a folder to be filed in.
          draggable: lens === "folders" && inEditor(card.ws),
          cells: [
            mark(card),
            { text: card.ws.name, style: card.ws.active ? { bold: true } : {} },
            { text: statusText(card), style: { fg: alarming(card) ? STATE_MARK.blocked.fg : DIM } },
            { text: projectName(card), style: { fg: DIM } },
            { text: gitText(card), style: { fg: DIM } },
            { text: formatAge(cardAge(card, input.now)), style: { fg: DIM } },
          ],
        }),
      );
      itemKeys.push(cardItemKey(card.key));
    }
  }
  if (selectedIndex < 0) selectedIndex = itemKeys.findIndex((k) => k.startsWith(CARD_PREFIX));
  return tree({
    key,
    nodes,
    itemKeys,
    selectedIndex,
    expandedKeys: groups.filter((g) => !input.collapsed.has(g.id)).map((g) => groupItemKey(g.id)),
    toggleOnClick: true,
    columns: [
      { title: "", maxWidth: 1 },
      { title: "Workspace" },
      { title: "Status" },
      { title: "Project" },
      { title: "Branch" },
      { title: "For", maxWidth: 4 },
    ],
  });
}

/** One Board column: its name and count over a tree of two-line cards. */
export function columnSpec(group: Group, input: ViewInput): WidgetSpec {
  const key = columnKey(group.id);
  const chosen = input.selected.get(key);
  const nodes = group.cards.map((card) =>
    treeNode(
      styledRow([
        mark(card),
        { text: ` ${card.ws.name}`, style: card.ws.active ? { bold: true } : {} },
        ...(card.pin ? [{ text: ` ${PIN_GLYPH}`, style: { fg: ACCENT } }] : []),
      ]),
      {
        flush: true,
        extraLines: [
          styledRow([
            {
              text: [projectName(card), card.question ? "asking" : inEditor(card.ws) ? gitText(card) : card.ws.title, formatAge(cardAge(card, input.now))]
                .filter(Boolean)
                .join(" · "),
              style: { fg: card.question ? STATE_MARK.blocked.fg : DIM },
            },
          ]),
        ],
      },
    )
  );
  const index = group.cards.findIndex((c) => c.key === chosen);
  // A heading over the cards, no frame: the cards are the boxes, and a
  // frame around each column on top of them was a border too many.
  return col(
    label([
      { text: group.label, style: { bold: true } },
      { text: `  ${group.cards.length}`, style: { fg: DIM } },
    ]),
    tree({
      key,
      nodes,
      itemKeys: group.cards.map((c) => cardItemKey(c.key)),
      selectedIndex: index >= 0 ? index : group.cards.length > 0 ? 0 : -1,
      // Bordered cards, as the dock draws them; the host draws a card's
      // second line only in this mode.
      itemHeight: 2,
      cardBorders: true,
      indentCols: 1,
    }),
  );
}

export function boardSpec(groups: readonly Group[], input: ViewInput): WidgetSpec {
  // A gutter between columns stands in for the frames that used to part them.
  return row(...groups.flatMap((g, i) => (i === 0 ? [columnSpec(g, input)] : [spacer(2), columnSpec(g, input)])));
}

export function hints(lens: LensId): HintEntry[] {
  const out: HintEntry[] = [
    { keys: "Enter", label: "open" },
    { keys: "n", label: "next waiting" },
    { keys: "Tab/1-4", label: "lens" },
  ];
  if (lens === "board") out.push({ keys: "S-←/→", label: "move" }, { keys: "p", label: "unpin" });
  if (lens === "folders") out.push({ keys: "drag", label: "file in folder" });
  out.push({ keys: "Esc", label: "close" });
  return out;
}

/** The whole panel. */
export function panelSpec(input: ViewInput): WidgetSpec {
  const groups = groupsFor(input.lens, input.cards, input.folders);
  const body = input.cards.length === 0
    ? label("No workspaces yet. Create one from the Orchestrator dock (+ New).", { style: { fg: DIM } })
    : input.lens === "board"
    ? boardSpec(groups, input)
    : listSpec(input.lens, groups, input);
  // The trees fill the panel's height themselves; only the empty-board
  // message needs a spacer to keep the hints at the bottom.
  const filler = input.cards.length === 0 ? [flexSpacer()] : [];
  return col(headerSpec(input.lens, input.cards), divider(), body, ...filler, hintBar(hints(input.lens)));
}

/** Every tree in `spec`, by key, with its item keys: what an event's
 *  `index` refers to. */
export function treeItemKeys(spec: WidgetSpec): Map<string, string[]> {
  const out = new Map<string, string[]>();
  const walk = (node: unknown): void => {
    if (node === null || typeof node !== "object") return;
    const n = node as { kind?: string; key?: string | null; itemKeys?: string[]; children?: unknown[]; child?: unknown };
    if (n.kind === "tree" && n.key) out.set(n.key, n.itemKeys ?? []);
    for (const c of n.children ?? []) walk(c);
    if (n.child) walk(n.child);
  };
  walk(spec);
  return out;
}

/** Tree keys of the Board's columns, left to right. */
export const COLUMN_KEYS: readonly string[] = COLUMNS.map((c) => columnKey(c.id));
