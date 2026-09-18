/// <reference path="./fresh.d.ts" />

/**
 * The Everything dialog's row model: how scans become rows, plus
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
import { styledRow, type TextPropertyEntry } from "./widgets.ts";

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
  /** Hand the dock's focus to a dialog, and take it back. */
  yieldDock(): void;
  restoreDock(): void;
}

export interface DiscoverRow {
  key: string;
  /** A heading rather than a session. */
  group: boolean;
  label: string;
  detail: string;
  /** Absent on a heading and on a problem line. */
  session?: CollectedSession;
  /** What Enter does to this row, resolved once so the renderer and the key
   *  handler agree. */
  verb?: DiscoverVerb;
  /** The machine this row came from, by `DiscoverTarget.key`. A row is acted
   *  on through its machine, which under "All machines" is not the picker's. */
  machineKey?: string;
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
  | { kind: "none"; why: string };

/** The problems heading's key; the filter's auto-expand leaves it alone. */
export const DISCOVER_PROBLEMS_KEY = "group:problems";

/** The heading for tools not on the machine. Named for the same reason. */
export const DISCOVER_ABSENT_KEY = "group:absent";

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
  if (!agent) return { kind: "none", why: t("discover.no_resume_none") };
  const resume = resumeArgv(agent, session.id);
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

/** Turn scans into rows: filtered, grouped, then absent tools, then problems.
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
      group: true,
      label: group.label,
      detail: group.detail
        ?? t("discover.group_count", { count: String(sessions.length) }),
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
      const tool = displayName.get(session.tool ?? "") ?? session.tool ?? "";
      const branch = session.project?.branch ?? "";
      const parts = opts.grouping === "project"
        ? [tool, branch]
        : opts.grouping === "branch"
          ? [(session.project ?? discoverPathProject(session.cwd ?? ""))?.label ?? "", tool]
          : [session.cwd ?? "", branch];
      if (manyMachines && from.label) parts.push(from.label);
      const detail = parts.filter((part) => part !== "").join("  ·  ");
      rows.push({
        // Keyed by machine too: two machines can hold the same tool and id,
        // and duplicate keys make the tree fold and select the wrong row.
        key: `session:${from.key}/${session.tool}/${session.id}`,
        group: false,
        label: `${mark}${title}`,
        detail,
        session,
        verb: discoverVerbFor(session, resumeArgv, t),
        machineKey: from.key,
      });
    }
  }

  // Absent tools get their own heading so the problems list holds only problems.
  const absent: DiscoverRow[] = [];
  for (const from of scans) {
    for (const tool of from.scan.tools) {
      if (tool.status !== "absent" && tool.status !== "unsupported") continue;
      const why = tool.status === "unsupported" ? (tool.note ?? "") : t("discover.not_installed");
      absent.push({
        key: `absent:${from.key}/${tool.id}`,
        group: false,
        label: tool.displayName,
        detail: manyMachines && from.label ? `${why}  ·  ${from.label}` : why,
      });
    }
  }
  if (absent.length > 0) {
    rows.push({
      key: DISCOVER_ABSENT_KEY,
      group: true,
      label: t("discover.absent", { count: String(absent.length) }),
      detail: "",
    });
    rows.push(...absent.sort((a, b) => byName(a.label, b.label)));
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
      group: true,
      label: t("discover.problems", { count: String(problems.length) }),
      detail: "",
    });
    for (const p of problems) {
      rows.push({ key: p.key, group: false, label: p.text, detail: "" });
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
    if (r.group) {
      open = expanded.has(r.key);
      n++;
    } else if (open) {
      n++;
    }
  }
  return n;
}

/** The width every row is padded to. Without padding the column takes the
 *  button row's width and clips longer rows. Bounded so one long path cannot
 *  stretch the panel. */
export function discoverRowWidth(rows: DiscoverRow[], width: (s: string) => number): number {
  const natural = rows.reduce((w, r) => {
    // The tree draws the child indent, but it still costs the row width.
    const indent = r.group ? 0 : DISCOVER_INDENT_COLS;
    return Math.max(w, indent + width(`${r.label}  ${r.detail}`));
  }, 0);
  return Math.max(48, Math.min(140, natural));
}

/** One row's styled text, padded to `width`. Inert rows are drawn dim. */
export function discoverRowEntry(
  r: DiscoverRow,
  width: number,
  measure: (s: string) => number,
): TextPropertyEntry {
  const dim = { fg: "ui.menu_disabled_fg" };
  const inert = !r.group && (!r.session || r.verb?.kind !== "attach" && r.verb?.kind !== "resume");
  // The pad rides on the last segment so it is dim. The tree draws the indent.
  const indent = r.group ? 0 : DISCOVER_INDENT_COLS;
  const detail = r.detail ? `  ${r.detail}` : "";
  const used = indent + measure(r.label) + measure(detail);
  const filled = used >= width ? detail : detail + " ".repeat(width - used);
  return styledRow([
    { text: r.label, style: r.group ? { bold: true } : inert ? dim : {} },
    { text: filled, style: dim },
  ]);
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
