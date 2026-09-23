/// <reference path="./fresh.d.ts" />

/**
 * The Import sessions dialog's row model: how scans become rows, plus
 * `DiscoveryHost`, the contract between the dialog and the orchestrator.
 * Pure: nothing here touches the editor, so it can be tested standalone.
 */

import {
  baseName,
  type CollectedSession,
  type MachineSpec,
  type ProjectIdentity,
  type ScanResult,
} from "./agent_scanner.ts";
import { styledRow, type StyledSegment, type TextPropertyEntry } from "./widgets.ts";

/** `editor.t`, handed in rather than reached for. */
export type Translate = (key: string, params?: Record<string, string>) => string;

/** What the dialog needs from the orchestrator, which owns the machines,
 *  the windows on them, the agent registry and the New Workspace form. */
export interface DiscoveryHost {
  /** Every machine a scan can be pointed at, windows first. */
  scanTargets(): DiscoverTarget[];
  /** How `agent` rejoins session `id`: the argv, and whether it addresses
   *  that session exactly or only the newest one in its directory. Null for
   *  an agent the registry does not know. */
  resumeArgv(agent: string, id: string): { argv: string[]; exact: boolean } | null;
  /** Open the New Workspace form on `seed`, with these fields filled. */
  openWorkspaceForm(seed: FormSeed, prefill: { projectPath: string; cmd: string }): void;
  /** Open Add Machine; `done` gets the saved machine's id, or null after a
   *  cancel. */
  addMachine(done: (savedKey: string | null) => void): void;
}

/** One column of a row, and which end of it survives a cut. A path keeps
 *  its tail — the directory it ends in is what tells two checkouts of one
 *  repository apart — and everything else keeps its head. */
export interface DiscoverCell {
  text: string;
  /** Defaults to `"head"`. */
  keep?: "head" | "tail";
}

/** Rows that line up with each other. Each family has its own column
 *  widths: a tool name belongs under a tool name, not under a session's
 *  branch, and a heading's columns are not a session's at all. */
export type DiscoverFamily = "group" | "session" | "problem";

export interface DiscoverRow {
  key: string;
  family: DiscoverFamily;
  /** The row's columns, left to right. What each one holds is the family's
   *  business; that the second one is the same width on every row of that
   *  family is this file's. */
  cells: DiscoverCell[];
  /** Absent on a heading and on a problem line. */
  session?: CollectedSession;
  /** What Enter does to this row, resolved once so the renderer and the key
   *  handler agree. */
  verb?: DiscoverVerb;
  /** The machine this row came from, by `DiscoverTarget.key`. A row is acted
   *  on through its machine, which under "All machines" is not the picker's. */
  machineKey?: string;
}

/** A heading rather than a row of the thing it heads. */
export function discoverIsGroup(row: DiscoverRow): boolean {
  return row.family === "group";
}

export type DiscoverGrouping = "tool" | "project" | "branch";

/** The grouping dropdown's options, in display order. */
export const DISCOVER_GROUPINGS: { value: DiscoverGrouping; labelKey: string }[] = [
  { value: "project", labelKey: "discover.by_project" },
  { value: "branch", labelKey: "discover.by_branch" },
  { value: "tool", labelKey: "discover.by_tool" },
];

/** A machine the dialog can offer to scan. */
export interface DiscoverTarget {
  key: string;
  label: string;
  /** A window to borrow or a transport to dial. Null only when the record is
   *  too incomplete to reach the machine. */
  spec: MachineSpec | null;
  /** True when the scan dials its own connection, which is read-only. */
  connects: boolean;
  /** Where the New Workspace form opens to rejoin a session found here.
   *  Absent only on the "All machines" entry. */
  reach?: FormSeed;
  /** The "All machines" entry. No spec; members are resolved at scan time. */
  all?: boolean;
}

/** The "All machines" target's key. */
export const DISCOVER_ALL_KEY = "all";

/** One machine's answer. The label is carried, not looked up, because a scan
 *  can outlive the machine list it was taken against. */
export interface DiscoverScan {
  key: string;
  label: string;
  scan: ScanResult;
}

/** What Enter does to a row. `exact` is false when the resume takes the
 *  newest session in the directory rather than this one (codex `resume --last`). */
export type DiscoverVerb =
  | { kind: "attach"; argv: string[] }
  | { kind: "resume"; argv: string[]; exact: boolean }
  /** A folder with no agent to rejoin (an Orca worktree): the form opens on
   *  it and the agent is chosen there. */
  | { kind: "open"; argv: string[] }
  | { kind: "none"; why: string };

/** The problems heading's key; the filter's auto-expand leaves it alone. */
export const DISCOVER_PROBLEMS_KEY = "group:problems";

/** Columns a session row is indented under its heading. */
export const DISCOVER_INDENT_COLS = 2;

/** The age of a cached answer, coarsely: `just now`, `4m`, `2h`. */
export function discoverAge(at: number, t: Translate): string {
  const secs = Math.max(0, Math.floor((Date.now() - at) / 1000));
  if (secs < 45) return t("discover.age_now");
  const mins = Math.round(secs / 60);
  if (mins < 60) return t("discover.age_min", { n: String(mins) });
  return t("discover.age_hour", { n: String(Math.round(mins / 60)) });
}

/** Attach to a live terminal, else resume by the registry's argv, else "none". */
export function discoverVerbFor(
  session: CollectedSession,
  resumeArgv: DiscoveryHost["resumeArgv"],
  t: Translate,
): DiscoverVerb {
  if (session.attach) {
    return { kind: "attach", argv: [session.attach.program, ...session.attach.args] };
  }
  const agent = session.agent ?? "";
  if (!agent && session.openable && session.cwd) return { kind: "open", argv: [] };
  if (!agent) return { kind: "none", why: t("discover.no_resume_none") };
  // A row that records another tool's session (an Orca tab running Claude)
  // resumes that one; an empty id there means "the newest in the directory".
  const resume = resumeArgv(agent, session.agentSessionId ?? session.id);
  if (!resume && session.openable && session.cwd) return { kind: "open", argv: [] };
  if (!resume) return { kind: "none", why: t("discover.no_resume_unknown", { agent }) };
  return { kind: "resume", argv: resume.argv, exact: resume.exact };
}

/** Case-insensitive substring match against everything a row shows or a
 *  reader might remember. */
export function discoverMatches(session: CollectedSession, needle: string): boolean {
  if (needle === "") return true;
  const hay = [
    session.title ?? "",
    session.cwd ?? "",
    session.tool ?? "",
    session.id,
    session.project?.label ?? "",
    session.project?.branch ?? "",
    ...(session.evidence ?? []).map((e) => e.saying),
  ].join("\n").toLowerCase();
  return hay.includes(needle);
}

/** Which heading a session belongs under. Project grouping keys on the
 *  resolved identity, not the path, so two checkouts of one repository share
 *  a heading. The detail says which rung the identity came from. An
 *  unresolved session falls back to its directory rather than being pooled
 *  with every other unresolved row. */
export function discoverGroupOf(
  session: CollectedSession,
  grouping: DiscoverGrouping,
  displayName: Map<string, string>,
  t: Translate,
): { key: string; label: string; detail: string | null } {
  if (grouping === "tool") {
    const name = displayName.get(session.tool ?? "") ?? session.tool ?? "";
    return { key: `tool:${name}`, label: name, detail: null };
  }
  if (grouping === "branch") {
    const branch = session.project?.branch ?? "";
    return {
      key: `branch:${branch}`,
      label: branch || t("discover.no_branch"),
      detail: null,
    };
  }
  const project = session.project ?? discoverPathProject(session.cwd ?? "");
  if (!project) {
    return { key: "project:", label: t("discover.no_project"), detail: null };
  }
  return {
    key: `project:${project.id}`,
    label: project.label,
    detail: project.kind === "repo"
      ? project.id.slice("repo:".length)
      : t(
        project.kind === "root" ? "discover.project_root" : "discover.project_path",
        { path: project.root },
      ),
  };
}

/** The path-only identity, for a session the hub did not resolve. */
export function discoverPathProject(cwd: string): ProjectIdentity | null {
  if (cwd === "") return null;
  return {
    kind: "path",
    id: `path:${cwd}`,
    label: baseName(cwd) || cwd,
    root: cwd,
  };
}

/** Turn scans into rows: filtered, grouped, then problems. A tool that is
 *  not installed on a machine is simply not listed.
 *  Machines are merged; the machine a row is on rides with the row. */
export function discoverRowsFrom(
  scans: DiscoverScan[],
  opts: { filter: string; grouping: DiscoverGrouping },
  resumeArgv: DiscoveryHost["resumeArgv"],
  t: Translate,
): DiscoverRow[] {
  const displayName = new Map<string, string>();
  const linked = new Set<string>();
  for (const { scan } of scans) {
    for (const t of scan.tools) displayName.set(t.id, t.displayName);
    // Link endpoints are `tool/id`: a row some other tool also sees.
    for (const l of scan.links) {
      linked.add(l.host);
      linked.add(l.hosted);
    }
  }

  // The machine is named on rows only when more than one is in play.
  const manyMachines = scans.length > 1;
  const needle = opts.filter.trim().toLowerCase();
  const matched: { session: CollectedSession; from: DiscoverScan }[] = [];
  for (const from of scans) {
    for (const sn of from.scan.sessions) {
      if (discoverMatches(sn, needle)) matched.push({ session: sn, from });
    }
  }

  interface DiscoverGroup {
    label: string;
    detail: string | null;
    entries: typeof matched;
  }
  const groups = new Map<string, DiscoverGroup>();
  for (const entry of matched) {
    const { key, label, detail } = discoverGroupOf(entry.session, opts.grouping, displayName, t);
    const bucket = groups.get(key);
    if (bucket) bucket.entries.push(entry);
    else groups.set(key, { label, detail, entries: [entry] });
  }

  const rows: DiscoverRow[] = [];
  // By name, so the order is stable between scans; no-project rows go last.
  const byName = (a: string, b: string): number =>
    a.localeCompare(b, undefined, { sensitivity: "base" });
  const ordered = [...groups.entries()].sort(
    (a, b) => Number(a[0] === "project:") - Number(b[0] === "project:") || byName(a[1].label, b[1].label),
  );
  for (const [key, group] of ordered) {
    const sessions = [...group.entries].sort(
      (a, b) =>
        byName(a.session.title || a.session.id, b.session.title || b.session.id)
        || byName(a.from.label, b.from.label),
    );
    rows.push({
      // The key carries the grouping, so groupings never collide.
      key: `group:${key}`,
      family: "group",
      cells: [
        { text: group.label },
        { text: group.detail ?? "", keep: "tail" },
        { text: t("discover.group_count", { count: String(sessions.length) }) },
      ],
    });
    for (const { session, from } of sessions) {
      // `||`, not `??`: an empty title must not render as a blank row.
      const title = session.title || session.id;
      const mark = session.attached
        ? "● "
        : linked.has(`${session.tool}/${session.id}`)
          ? "↔ "
          : "  ";
      // Whatever the heading already says is left off the row. The branch
      // stays on the row: one project's sessions sit on different branches.
      // The same column holds the same kind of thing on every row of a
      // grouping — that is what lets the eye read down one instead of along
      // each row — so an empty one is left empty rather than closed up.
      const tool = displayName.get(session.tool ?? "") ?? session.tool ?? "";
      const branch = session.project?.branch ?? "";
      const cells: DiscoverCell[] = [{ text: `${mark}${title}` }];
      if (opts.grouping === "project") {
        cells.push({ text: tool }, { text: branch });
      } else if (opts.grouping === "branch") {
        cells.push(
          { text: (session.project ?? discoverPathProject(session.cwd ?? ""))?.label ?? "" },
          { text: tool },
        );
      } else {
        cells.push({ text: session.cwd ?? "", keep: "tail" }, { text: branch });
      }
      if (manyMachines) cells.push({ text: from.label });
      rows.push({
        // Keyed by machine too: two machines can hold the same tool and id,
        // and duplicate keys make the tree fold and select the wrong row.
        key: `session:${from.key}/${session.tool}/${session.id}`,
        family: "session",
        cells,
        session,
        verb: discoverVerbFor(session, resumeArgv, t),
        machineKey: from.key,
      });
    }
  }

  // Problems are not filtered: hiding one because it does not match the
  // filter would hide the reason the search found nothing.
  const problems: { key: string; text: string }[] = [];
  for (const from of scans) {
    from.scan.problems.forEach((problem, i) => {
      problems.push({
        key: `problem:${from.key}/${i}`,
        text: manyMachines && from.label ? `${from.label}: ${problem}` : problem,
      });
    });
  }
  if (problems.length > 0) {
    rows.push({
      key: DISCOVER_PROBLEMS_KEY,
      family: "group",
      cells: [{ text: t("discover.problems", { count: String(problems.length) }) }],
    });
    for (const p of problems) {
      rows.push({ key: p.key, family: "problem", cells: [{ text: p.text }] });
    }
  }
  return rows;
}

/** Rows the tree draws: every heading plus the children of open ones. The
 *  tree does not pad itself to `visibleRows`, so the dialog pads the rest. */
export function discoverVisibleRowCount(rows: DiscoverRow[], expanded: Set<string>): number {
  let n = 0;
  let open = false;
  for (const r of rows) {
    if (discoverIsGroup(r)) {
      open = expanded.has(r.key);
      n++;
    } else if (open) {
      n++;
    }
  }
  return n;
}

/** The widest a single column may grow. A path long enough to fill the
 *  panel on its own would push every column after it off the edge, so the
 *  cell is cut instead — losing one cell's tail, not the table. */
export const DISCOVER_COL_MAX = 44;

/** What each column of a session row holds, for the header over the list:
 *  the heading already says the rest. */
export function discoverColumnTitles(grouping: DiscoverGrouping, manyMachines: boolean, t: Translate): string[] {
  const titles = [t("discover.col_session")];
  if (grouping === "project") titles.push(t("discover.col_tool"), t("discover.col_branch"));
  else if (grouping === "branch") titles.push(t("discover.col_project"), t("discover.col_tool"));
  else titles.push(t("discover.col_directory"), t("discover.col_branch"));
  if (manyMachines) titles.push(t("discover.col_machine"));
  return titles;
}

/** A column of the results table, as `tree({ columns })` takes it. */
export interface DiscoverColumn {
  title: string;
  /** `"head"` cuts the head (a path keeps its tail); `"tail"` a name's. */
  elide: "head" | "tail";
  maxWidth: number;
}

/** The results table's columns: the session rows' titles, and which end of
 *  each column's cells is cut. How wide each is, and how the table fits the
 *  list, is the host's to work out from the width it lays the list out at. */
export function discoverColumns(
  rows: DiscoverRow[],
  grouping: DiscoverGrouping,
  manyMachines: boolean,
  t: Translate,
): DiscoverColumn[] {
  const session = rows.find((r) => r.family === "session");
  return discoverColumnTitles(grouping, manyMachines, t).map((title, i) => ({
    title,
    elide: session?.cells[i]?.keep === "tail" ? "head" : "tail",
    maxWidth: DISCOVER_COL_MAX,
  }));
}

/** A session row's cells as the table draws them: the name, then what tells
 *  two of them apart, dimmed. An inert row's name is dim too. */
export function discoverRowCells(r: DiscoverRow): { text: string; style?: { fg: string } }[] {
  const dim = { fg: "ui.menu_disabled_fg" };
  const inert = !r.session || !r.verb || r.verb.kind === "none";
  return r.cells.map((cell, i) => (i === 0 && !inert ? { text: cell.text } : { text: cell.text, style: dim }));
}

/** A heading or a problem line, which spans the table rather than sitting in
 *  its columns: the heading's name bold, the rest dim. */
export function discoverRowText(r: DiscoverRow): TextPropertyEntry {
  const dim = { fg: "ui.menu_disabled_fg" };
  const group = discoverIsGroup(r);
  const segments: StyledSegment[] = [];
  r.cells.forEach((cell, i) => {
    if (i > 0 && cell.text) segments.push({ text: "  " });
    if (cell.text) segments.push({ text: cell.text, style: i === 0 && group ? { bold: true } : dim });
  });
  return styledRow(segments);
}

/** The button drawn at the end of a row, or null for a row with nothing to
 *  do: the heading it is under, a tool that is not installed, a problem, or
 *  a session with no way back. */
export function discoverRowAction(r: DiscoverRow, t: Translate): string | null {
  if (discoverIsGroup(r) || !r.session) return null;
  const kind = r.verb?.kind;
  return kind && kind !== "none" ? t("discover.btn_import") : null;
}

/** Quote one argv element for the form's command field. `splitAgentCmd` has
 *  no escape character, so an element with both quote kinds returns null. */
export function quoteForAgentCmd(arg: string): string | null {
  if (arg === "") return "''";
  if (!/[\s'"]/.test(arg)) return arg;
  const hasSingle = arg.includes("'");
  const hasDouble = arg.includes('"');
  if (hasSingle && hasDouble) return null;
  return hasSingle ? `"${arg}"` : `'${arg}'`;
}

/** Where the next New Workspace form opens: a machine control entry by key
 *  (`local`, a saved machine id, `host:<alias>`), an unsaved ssh target, or a
 *  pod. Deliberately no null: an absent machine can never read as "Local". */
export type FormSeed =
  | { kind: "option"; key: string }
  | { kind: "ssh"; target: string }
  | { kind: "kubernetes"; namespace: string; pod: string };
