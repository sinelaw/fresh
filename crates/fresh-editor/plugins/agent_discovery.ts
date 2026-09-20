/// <reference path="./lib/fresh.d.ts" />

/**
 * The Everything dialog: finds agent sessions other tools are running, on any
 * machine, and rejoins them. State, rendering, scan and cache live here; the
 * row model is `lib/discovery.ts`. The orchestrator is reached through
 * `DiscoveryHost`, looked up when the dialog opens so load order does not matter.
 */

import { hub, type CollectedSession, type ScanResult } from "./lib/agent_scanner.ts";
import {
  DISCOVER_ABSENT_KEY,
  DISCOVER_ALL_KEY,
  DISCOVER_GROUPINGS,
  DISCOVER_PROBLEMS_KEY,
  discoverAge,
  discoverGroupOf,
  discoverMatches,
  discoverIsGroup,
  discoverLayout,
  discoverRowAction,
  discoverRowEntry,
  discoverRowsFrom,
  discoverVisibleRowCount,
  quoteForAgentCmd,
  DISCOVER_INDENT_COLS,
  type DiscoverGrouping,
  type DiscoverVerb,
  type DiscoverRow,
  type DiscoverScan,
  type DiscoverTarget,
  type DiscoveryHost,
} from "./lib/discovery.ts";
import {
  FloatingWidgetPanel,
  button,
  col,
  dropdown,
  endRow,
  flexSpacer,
  label,
  raw,
  row,
  spacer,
  text,
  tree,
  treeNode,
  type WidgetEvt,
  type WidgetSpec,
} from "./lib/widgets.ts";

const editor = getEditor();

// Editor methods need their receiver; the row model takes plain functions.
const t = (key: string, params?: Record<string, string>): string => editor.t(key, params);
const measure = (s: string): number => editor.stringWidth(s);

/** The orchestrator's side of the contract, or null when it is not loaded. */
function host(): DiscoveryHost | null {
  return editor.getPluginApi("orchestrator") as DiscoveryHost | null;
}

function scanTargets(): DiscoverTarget[] {
  return host()?.scanTargets() ?? [];
}

function resumeArgv(agent: string, id: string): { argv: string[]; exact: boolean } | null {
  return host()?.resumeArgv(agent, id) ?? null;
}

// Same label column and note styling as the orchestrator's dialogs.
const FORM_LABEL_W = 15;
const NOTE_STYLE = { fg: "ui.menu_disabled_fg", italic: true } as const;

/** A button and its accelerator as one unit, so a wrapping footer never
 *  strands the accelerator on its own line. */
function withAccel(b: WidgetSpec, k: string): WidgetSpec {
  return row(b, label(k, { style: NOTE_STYLE }));
}

function fieldNote(note: string): WidgetSpec {
  return label(`↳ ${note}`, { labelWidth: FORM_LABEL_W, style: NOTE_STYLE });
}

const DISCOVER_MODE = "agent-discovery";


// Rows the results area always occupies, so the buttons stay put whatever
// the scan found. Fits a collapsed answer without scrolling.
const DISCOVER_TREE_ROWS = 14;

let discoverPanel: FloatingWidgetPanel | null = null;

interface DiscoverState {
  /** Index into `discoverMachines()`. */
  machineIndex: number;
  scanning: boolean;
  /** Identity of the scan in flight, minted per scan. A settling scan writes
   *  results only if this is still its token; the state object alone cannot
   *  tell a stale answer from a current one after a picker change. */
  scanToken: object | null;
  /** Null until the first scan. */
  rows: DiscoverRow[] | null;
  /** Empty unless the last scan failed or reported problems. */
  note: string;
  index: number;
  /** Filters the rows already in hand; never re-scans. */
  filter: { value: string; cursor: number };
  grouping: DiscoverGrouping;
  /** The last raw answer, one entry per machine scanned, so filtering and
   *  regrouping are free. */
  scans: DiscoverScan[] | null;
  /** The headings the reader has opened. Kept here even though the host
   *  toggles folds: the tree is drawn from the spec's `expandedKeys`, so a
   *  fold the spec does not mirror is undone on the next frame. */
  expanded: Set<string>;
}

// A machine's last answer and when it was given. The filter works on this,
// and the age is shown so a stale answer is not taken as current.
const discoverCache = new Map<string, { scans: DiscoverScan[]; at: number }>();

let discoverState: DiscoverState | null = null;

// Abandons the scan in flight, so closing the dialog stops a remote listing.
let discoverStop: (() => void) | null = null;

/** Every reachable machine. Resolved at scan time so a machine added since
 *  the dialog opened is included. */
function discoverAllMembers(): DiscoverTarget[] {
  return scanTargets().filter((t) => !t.all && t.spec !== null);
}

/** The folds the tree is drawn with: the reader's own set, except while the
 *  filter is on, when every group is open so the hits are visible. */
function discoverExpandedKeys(): string[] {
  const st = discoverState;
  if (!st) return [];
  if (st.filter.value === "") return [...st.expanded];
  // The problems and absent groups are not filtered, so they stay folded.
  return (st.rows ?? [])
    .filter((r) =>
      discoverIsGroup(r) && r.key !== DISCOVER_PROBLEMS_KEY && r.key !== DISCOVER_ABSENT_KEY
    )
    .map((r) => r.key);
}

function buildDiscoverSpec(): WidgetSpec {
  const st = discoverState!;
  const targets = scanTargets();
  const selectedIndex = Math.min(st.machineIndex, targets.length - 1);
  const selected = targets[selectedIndex] ?? null;
  // Only a record too incomplete to reach has a null spec. "All machines"
  // also has a null spec by design (its members hold theirs), so it must not
  // count as unreachable or Scan is disabled and drops out of the Tab ring.
  const unreachable = selected !== null && !selected.all && selected.spec === null;
  const rows = st.rows ?? [];
  const cached = selected ? discoverCache.get(selected.key) : undefined;

  const body: WidgetSpec[] = [
    row(
      dropdown(
        targets.map((t) => t.label),
        {
          selectedIndex,
          label: editor.t("discover.machine"),
          labelWidth: FORM_LABEL_W,
          key: "discover-machine",
        },
      ),
      spacer(2),
      withAccel(
        button(editor.t("discover.btn_scan"), {
          intent: "primary",
          key: "discover-scan",
          disabled: unreachable,
        }),
        "⏎",
      ),
    ),
    row(
      text({
        value: st.filter.value,
        cursorByte: st.filter.cursor,
        label: editor.t("discover.find"),
        placeholder: editor.t("discover.find_placeholder"),
        labelWidth: FORM_LABEL_W,
        fieldWidth: 34,
        key: "discover-filter",
      }),
      spacer(2),
      // A dropdown rather than a toggle button: it joins the Tab ring and
      // shows the current grouping rather than the next one.
      dropdown(DISCOVER_GROUPINGS.map((g) => editor.t(g.labelKey)), {
        selectedIndex: Math.max(0, DISCOVER_GROUPINGS.findIndex((g) => g.value === st.grouping)),
        key: "discover-grouping",
      }),
      flexSpacer(),
      ...(cached && !st.scanning
        ? [label(discoverAge(cached.at, t), { style: { fg: "ui.menu_disabled_fg" } })]
        : []),
    ),
    spacer(0),
  ];

  const note = unreachable
    ? editor.t("discover.not_attached", { machine: selected.label })
    : st.note;
  if (note) body.push(fieldNote(note), spacer(0));

  // Every state below fills the same `DISCOVER_TREE_ROWS`, so the dialog keeps
  // one shape.
  const filled = (...kids: WidgetSpec[]): WidgetSpec[] => [
    ...kids,
    raw(Array.from({ length: Math.max(0, DISCOVER_TREE_ROWS - kids.length) }, () => ({ text: "" }))),
  ];
  if (st.scanning) {
    body.push(...filled(label(editor.t("discover.scanning"))));
  } else if (st.rows === null) {
    body.push(
      ...filled(label(editor.t("discover.hint"), { style: { fg: "ui.menu_disabled_fg" } })),
    );
  } else if (rows.length === 0) {
    body.push(...filled(label(editor.t("discover.empty"))));
  } else {
    // Measured over every row, so the columns line up down the whole
    // answer rather than within each heading.
    const layout = discoverLayout(rows, measure);
    // Headings start collapsed so hundreds of sessions fit one screen.
    // `visibleRows` must be given: an auto-sized tree draws nothing here.
    body.push(
      tree({
        nodes: rows.map((r) => {
          const action = discoverRowAction(r, t);
          return treeNode(discoverRowEntry(r, layout, measure), {
            depth: discoverIsGroup(r) ? 0 : 1,
            hasChildren: discoverIsGroup(r),
            ...(action === null ? {} : { action }),
          });
        }),
        itemKeys: rows.map((r) => r.key),
        selectedIndex: Math.min(st.index, rows.length - 1),
        visibleRows: DISCOVER_TREE_ROWS,
        indentCols: DISCOVER_INDENT_COLS,
        // Read on the first frame only; `refreshDiscoverDialog` pushes later
        // fold sets with `setExpandedKeys`.
        expandedKeys: discoverExpandedKeys(),
        key: "discover-rows",
      }),
      // The tree draws only the rows it has, so the rest of the reserved
      // window is padded or the dialog shrinks after a scan.
      ...(() => {
        const drawn = discoverVisibleRowCount(rows, new Set(discoverExpandedKeys()));
        const pad = Math.max(0, DISCOVER_TREE_ROWS - Math.min(drawn, DISCOVER_TREE_ROWS));
        return pad > 0 ? [raw(Array.from({ length: pad }, () => ({ text: "" })))] : [];
      })(),
    );
  }

  body.push(
    spacer(0),
    endRow(withAccel(button(editor.t("discover.btn_close"), { key: "discover-close" }), "Esc")),
  );
  return col(...body);
}

/** Re-derive the rows from the last answer. Never re-scans. */
function reflowDiscoverRows(): void {
  const st = discoverState;
  if (!st) return;
  st.rows = st.scans === null
    ? null
    : discoverRowsFrom(st.scans, { filter: st.filter.value, grouping: st.grouping }, resumeArgv, t);
  // While filtered every group is open and Enter cannot fold a heading, so
  // the cursor lands on the first hit rather than the heading above it.
  st.index = st.filter.value === ""
    ? 0
    : Math.max(0, st.rows?.findIndex((r) => !discoverIsGroup(r)) ?? 0);
  refreshDiscoverDialog();
}

/** Show the cached answer for the selected machine, if there is one. */
function loadDiscoverCache(): void {
  const st = discoverState;
  if (!st) return;
  const target = scanTargets()[st.machineIndex] ?? null;
  const hit = target ? discoverCache.get(target.key) : undefined;
  st.scans = hit ? hit.scans : null;
  st.expanded.clear();
  reflowDiscoverRows();
}

function refreshDiscoverDialog(): void {
  if (!discoverPanel || !discoverState) return;
  discoverPanel.update(buildDiscoverSpec());
  // The spec's selection and folds are only seeds; the host owns both once
  // the reader touches the tree. Changes decided here must be pushed, or the
  // tree draws one shape and navigates another.
  discoverPanel.setExpandedKeys("discover-rows", discoverExpandedKeys());
  discoverPanel.setSelectedIndex("discover-rows", discoverState.index);
}

function openDiscoverDialog(): void {
  host()?.yieldDock();
  discoverState = {
    machineIndex: 0,
    scanning: false,
    scanToken: null,
    rows: null,
    note: "",
    index: 0,
    expanded: new Set<string>(),
    filter: { value: "", cursor: 0 },
    grouping: "project",
    scans: null,
  };
  discoverPanel = new FloatingWidgetPanel();
  discoverPanel.mount(buildDiscoverSpec(), {
    widthPct: 70,
    heightPct: 70,
    focusMarker: true,
    title: editor.t("discover.title"),
    closable: true,
  });
  // Widen the panel's layer to the whole frame so it is centred on the
  // screen. Otherwise a centred panel is clipped to the area beside the dock
  // and centred there. The panel keeps its own size.
  editor.floatingPanelControl(discoverPanel.id(), "fullscreen", 1);
  editor.setEditorMode(DISCOVER_MODE);
  discoverPanel.setFocusKey("discover-scan");
  loadDiscoverCache();
}

function closeDiscoverDialog(): void {
  // The scan is the only thing still holding the machine open.
  discoverStop?.();
  discoverStop = null;
  if (discoverPanel) {
    discoverPanel.unmount();
    discoverPanel = null;
  }
  discoverState = null;
  editor.setEditorMode(null);
  host()?.restoreDock();
}

// Scan the selected machine through the `agent-sessions` hub. A machine with
// an open window is read through it; one without is dialled and hung up after.
async function runDiscoverScan(): Promise<void> {
  const st = discoverState;
  if (!st || st.scanning) return;
  const target = scanTargets()[st.machineIndex] ?? null;
  if (!target) return;

  const members = target.all ? discoverAllMembers() : [target];
  if (members.length === 0 || members.some((m) => m.spec === null)) {
    // Only when the machine list emptied between opening and pressing Scan.
    st.note = editor.t("discover.not_attached", { machine: target.label });
    st.rows = [];
    refreshDiscoverDialog();
    return;
  }

  const api = hub();
  if (!api) {
    // A missing hub must read as a failed scan, not an empty machine.
    st.note = editor.t("discover.failed", {
      error: "the agent-sessions plugin is not loaded",
    });
    st.rows = [];
    refreshDiscoverDialog();
    return;
  }

  st.scanning = true;
  const token = {};
  st.scanToken = token;
  st.note = "";
  // Enter on the open machine list starts the scan with the list still open;
  // moving focus to Scan closes it. After the re-render, so focus lands on
  // the new spec.
  refreshDiscoverDialog();
  discoverPanel?.setFocusKey("discover-scan");
  // Machines are scanned in parallel, each on its own connection and budget.
  const stops: (() => void)[] = [];
  discoverStop = () => {
    for (const stop of stops) stop();
  };
  try {
    const answers = await Promise.all(
      members.map(async (m): Promise<DiscoverScan> => {
        try {
          const scan = await api.scan(m.spec, {
            onStarted: (stop) => stops.push(stop),
            // A connection we dialled ourselves runs no commands, so asking
            // would only produce a refusal per scanner. Transcripts are still
            // found; only the live listing (tmux, zellij) is lost.
            allowCommands: !m.connects,
          });
          return { key: m.key, label: m.label, scan };
        } catch (err) {
          // One machine failing becomes a problem line for that machine, not
          // a rejection that throws away every other machine's answer.
          return {
            key: m.key,
            label: m.label,
            scan: {
              machine: m.label,
              sessions: [],
              links: [],
              tools: [],
              problems: [editor.t("discover.failed", { error: String(err) })],
            },
          };
        }
      }),
    );
    // An abandoned scan still settles. Write nothing unless this is still the
    // current scan, or a stale answer would be shown under another machine's
    // name.
    if (discoverState !== st || st.scanToken !== token) return;
    st.scanning = false;
    st.scanToken = null;
    discoverStop = null;
    st.scans = answers;
    discoverCache.set(target.key, { scans: answers, at: Date.now() });
    st.rows = discoverRowsFrom(answers, { filter: st.filter.value, grouping: st.grouping }, resumeArgv, t);
    st.index = 0;
    st.expanded.clear();
    st.note = "";
  } catch (err) {
    if (discoverState !== st || st.scanToken !== token) return;
    st.scanning = false;
    st.scanToken = null;
    discoverStop = null;
    st.rows = [];
    st.note = editor.t("discover.failed", { error: String(err) });
  }
  refreshDiscoverDialog();
  // Focus the results. `setFocusKey` does not run the tree's focus hook, so
  // the selection is pinned too or a tree that lost focus once shows no row.
  if (st.rows !== null && st.rows.length > 0) {
    discoverPanel?.setFocusKey("discover-rows");
    discoverPanel?.setSelectedIndex("discover-rows", st.index);
  }
}

/** Hand a discovered session to the New Workspace form, filled in. The form
 *  rather than a silent launch: it shows what will run and where, and dials
 *  an unattached machine with the usual trust decision in front of it. */
function openWorkspaceForDiscovered(
  session: CollectedSession,
  verb: DiscoverVerb,
  machineKey: string | undefined,
): void {
  const st = discoverState;
  if (!st || verb.kind === "none") return;
  const quoted = verb.argv.map(quoteForAgentCmd);
  const bad = quoted.indexOf(null);
  if (bad >= 0) {
    st.note = editor.t("discover.unquotable", { arg: verb.argv[bad] });
    refreshDiscoverDialog();
    return;
  }
  // The row's machine, not the picker's: under "All machines" the picker is
  // not a machine, and it can change after a scan.
  const target = scanTargets().find((t) => t.key === machineKey) ?? null;
  if (!target?.reach) {
    st.note = editor.t("discover.machine_gone", {
      machine: st.scans?.find((s) => s.key === machineKey)?.label ?? machineKey ?? "",
    });
    refreshDiscoverDialog();
    return;
  }
  const reach = target.reach;
  closeDiscoverDialog();
  host()?.openWorkspaceForm(reach, { projectPath: session.cwd ?? "", cmd: quoted.join(" ") });
  // An inexact resume rejoins the newest session in the directory, which may
  // not be this row. Said after the form opens so its status does not overwrite it.
  if (verb.kind === "resume" && !verb.exact) {
    editor.setStatus(
      editor.t("discover.status_prefix", {
        msg: editor.t("discover.inexact_resume", {
          agent: verb.argv[0] ?? "",
          cmd: quoted.join(" "),
        }),
      }),
    );
  }
}

function openAgentDiscovery(): void {
  openDiscoverDialog();
}

/** Enter on a row: a heading folds, a session is resumed or attached, and a
 *  session with no verb says why. */
function enterDiscoverRow(index: number): void {
  const st = discoverState;
  if (!st || st.rows === null) return;
  const row = st.rows[index];
  if (!row) return;
  // Enter from the filter field acts on the tree's own selection, which
  // `select` may not have reported yet.
  st.index = index;
  if (discoverIsGroup(row)) {
    // Every group is open while the filter is on; nothing to flip.
    if (st.filter.value !== "") return;
    if (st.expanded.has(row.key)) st.expanded.delete(row.key);
    else st.expanded.add(row.key);
    refreshDiscoverDialog();
    return;
  }
  // A problem line has nothing to do.
  if (!row.session) return;
  const verb = row.verb ?? { kind: "none", why: "" };
  if (verb.kind === "none") {
    st.note = verb.why;
    refreshDiscoverDialog();
    return;
  }
  openWorkspaceForDiscovered(row.session, verb, row.machineKey);
}

function handleDiscoverEvent(e: WidgetEvt): void {
  const st = discoverState!;
  if (e.event_type === "cancel") {
    discoverPanel = null;
    closeDiscoverDialog();
    return;
  }
  // A dropdown reports `change` with the index in `payload`, not `select`.
  if (e.event_type === "change" && e.widget_key === "discover-machine") {
    const idx = ((e.payload ?? {}) as Record<string, unknown>).index;
    if (typeof idx === "number" && idx !== st.machineIndex) {
      // Stop and disown a scan in flight; it was for the machine being left.
      if (st.scanning) {
        discoverStop?.();
        discoverStop = null;
        st.scanning = false;
        st.scanToken = null;
      }
      st.machineIndex = idx;
      st.note = "";
      loadDiscoverCache();
    }
    return;
  }
  if (e.event_type === "change" && e.widget_key === "discover-filter") {
    const value = ((e.payload ?? {}) as Record<string, unknown>).value;
    const cursor = ((e.payload ?? {}) as Record<string, unknown>).cursorByte;
    if (typeof value === "string") {
      st.filter = { value, cursor: typeof cursor === "number" ? cursor : value.length };
      reflowDiscoverRows();
    }
    return;
  }
  if (e.event_type === "change" && e.widget_key === "discover-grouping") {
    const idx = ((e.payload ?? {}) as Record<string, unknown>).index;
    const next: DiscoverGrouping =
      (typeof idx === "number" ? DISCOVER_GROUPINGS[idx]?.value : undefined) ?? "project";
    if (next !== st.grouping) {
      st.grouping = next;
      // The folds are keyed by heading, and the headings just changed.
      st.expanded.clear();
      reflowDiscoverRows();
    }
    return;
  }
  // A press on a row's Import button. The host has already moved the
  // selection onto that row, so this is exactly what Enter would have done
  // — one press instead of two.
  if (e.event_type === "action" && e.widget_key === "discover-rows") {
    const idx = ((e.payload ?? {}) as Record<string, unknown>).index;
    if (typeof idx === "number") enterDiscoverRow(idx);
    return;
  }
  // A tree reports its cursor as `select` and its folds as `expand`. The
  // fold must be mirrored and re-rendered, or the next frame undoes it.
  if (
    (e.event_type === "select" || e.event_type === "expand") &&
    e.widget_key === "discover-rows"
  ) {
    const payload = (e.payload ?? {}) as Record<string, unknown>;
    const idx = payload.index;
    if (typeof idx === "number") st.index = idx;
    if (e.event_type === "expand") {
      const key = payload.key;
      if (typeof key === "string" && key !== "") {
        if (payload.expanded === true) st.expanded.add(key);
        else st.expanded.delete(key);
        refreshDiscoverDialog();
      }
    }
    return;
  }
  if (e.event_type === "activate") {
    // Enter on the tree, or in the filter field (forwarded to the tree).
    if (e.widget_key === "discover-rows") {
      const idx = ((e.payload ?? {}) as Record<string, unknown>).index;
      if (typeof idx === "number") enterDiscoverRow(idx);
      return;
    }
    if (e.widget_key === "discover-scan") {
      void runDiscoverScan();
      return;
    }
    if (e.widget_key === "discover-close") {
      closeDiscoverDialog();
      return;
    }
  }
}

// ── Wiring ────────────────────────────────────────────────────────

editor.defineMode(DISCOVER_MODE, [], true, true);

editor.on("widget_event", (e) => {
  if (discoverPanel && discoverState && e.panel_id === discoverPanel.id()) {
    handleDiscoverEvent(e);
  }
});

registerHandler("agent_discovery_open", openAgentDiscovery);
editor.registerCommand("%cmd.everything", "%cmd.everything_desc", "agent_discovery_open", null, {
  terminalBypass: true,
});

editor.exportPluginApi("agent-discovery", { open: openAgentDiscovery });

editor.debug("agent-discovery: Everything dialog registered");
