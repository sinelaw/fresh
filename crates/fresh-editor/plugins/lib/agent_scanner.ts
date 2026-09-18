/// <reference path="./fresh.d.ts" />

/** Shared types and helpers for agent-session scanners and the hub. */

// `getEditor()` is called inside the functions that need it, not at module
// load, so the pure helpers can be imported by tests with no editor.

/** How strongly a fact is believed. */
export type Confidence = "exact" | "strong" | "likely" | "weak";

/** Which rung of the project-identity ladder an identity came from. The ladder
 *  lives in `project_identity.ts`; this file must not import it (type-only
 *  imports are kept by the plugin loader, so that would be a cycle). */
export type ProjectKind = "repo" | "root" | "path";

export interface ProjectIdentity {
  kind: ProjectKind;
  /** Grouping key, prefixed by rung so a repository and a same-named directory never collide. */
  id: string;
  /** Heading text: `owner/name` for a repository, else the directory basename. */
  label: string;
  /** The directory the identity was resolved at. */
  root: string;
  /** Branch from `.git/HEAD` (short commit when detached). Not part of `id`:
   *  two branches of one repository are one project. */
  branch?: string;
}

/** Where a fact came from. */
export interface Evidence {
  /** A path, an environment variable, or a command. */
  locator: string;
  saying?: string;
}

/** One session a scanner found. Flat on purpose: callers want rows. */
export interface CollectedSession {
  /** Stable within the tool. */
  id: string;
  /** Set by the hub to the registering scanner's id. */
  tool?: string;
  /** Falls back to the cwd basename, then the id. */
  title?: string;
  cwd?: string;
  /** Filled in by the hub, once per directory. A scanner that sets it is overwritten. */
  project?: ProjectIdentity;
  path?: string;
  /** Unix timestamp of last activity. */
  mtime?: number;
  attached?: boolean;
  /** A session id this row records for another tool, e.g. the Claude session in a tmux pane. */
  agentSessionId?: string;
  /** The agent's argv0 basename (`claude`, `codex`). Not an argv: how an agent
   *  resumes is known to the orchestrator's agent-resume registry, not here. */
  agent?: string;
  /** How to attach, when only the scanner knows. Absent for transcripts. */
  attach?: LaunchSpec;
  evidence?: Evidence[];
}

/** A command to run on the machine the session lives on. Program plus argv, never
 *  a shell string: the parts come off another tool's disk and must not be re-split. */
export interface LaunchSpec {
  /** Resolved against the remote PATH. */
  program: string;
  args: string[];
  /** Defaults to the session's own `cwd`. */
  cwd?: string;
}

/** What one scanner reports about its tool. */
export interface ScannerReport {
  sessions: CollectedSession[];
  /** Omitted means inferred from whether anything was found. */
  installed?: boolean;
  /** The tool cannot exist on this machine, and why. Distinct from `installed: false`. */
  unsupported?: string;
  /** Failures reading a tool that is there. Absent tools and stale sockets are not problems. */
  problems?: string[];
}

/** `failed` is the only status that is also a problem. */
export type ToolStatus = "found" | "absent" | "unsupported" | "failed";

export interface ToolReport {
  id: string;
  displayName: string;
  status: ToolStatus;
  count: number;
  note?: string;
}

/** What the hub tells a scanner about this scan. */
export interface ScanContext {
  /** Whether the scan may run other tools' listing commands. Check before running
   *  one: a machine opened by transport refuses spawns. */
  allowCommands: boolean;
  /** The most sessions the caller keeps, newest first. Known before the read so it
   *  can bound the read: each header is a round trip on a remote machine. */
  maxSessions: number;
}

export interface Scanner {
  /** Stable and unique; also the `tool` on every session it returns. */
  id: string;
  displayName: string;
  /** "Tool not installed" is an empty report, not a throw. A throw is recorded as
   *  a problem and does not stop the scan. */
  scan(machine: FreshMachine, ctx: ScanContext): Promise<ScannerReport>;
}

/** Two rows that look like the same work seen by different tools. */
export interface Link {
  /** `tool/id` of the hosting row, e.g. the tmux pane. */
  host: string;
  /** `tool/id` of the hosted row, e.g. the agent transcript. */
  hosted: string;
  reason: "recorded-session-id" | "same-cwd";
  confidence: Confidence;
}

export interface ScanResult {
  machine: string;
  sessions: CollectedSession[];
  links: Link[];
  tools: ToolReport[];
  /** Everything that went wrong, across every scanner. Never dropped. */
  problems: string[];
}

/** What `editor.openMachine` accepts. A machine opened by transport is read-only:
 *  it runs nothing, so scanners that need commands skip them. */
export type MachineSpec =
  | { kind: "window"; window?: number }
  | RemoteAgentTransport
  | AuthorityPayload;

export interface ScanOptions {
  /** Cap per scanner. Stores are unbounded; a picker wants the newest few. */
  maxSessionsPerTool?: number;
  /** Let scanners run other tools' listing commands. Off keeps the scan to filesystem reads. */
  allowCommands?: boolean;
  /** Skip the cross-tool correlation pass. */
  correlate?: boolean;
  /** Called once the scan starts, with a `stop` that abandons it. Stop closes the
   *  machine handle; the scan's promise still settles with what was collected. */
  onStarted?: (stop: () => void) => void;
}

export interface AgentSessionsApi {
  registerScanner(scanner: Scanner): () => void;
  unregisterScanner(id: string): boolean;
  listScanners(): { id: string; displayName: string }[];
  scan(spec: MachineSpec | null, opts?: ScanOptions): Promise<ScanResult>;
}

/** The hub, or null when it is not loaded. */
export function hub(): AgentSessionsApi | null {
  return getEditor().getPluginApi("agent-sessions") as AgentSessionsApi | null;
}

/** Register with the hub, or wait for `plugins_loaded` when it is not loaded yet. */
export function registerScanner(scanner: Scanner): void {
  const api = hub();
  if (api) {
    api.registerScanner(scanner);
    return;
  }
  const editor = getEditor();
  editor.on("plugins_loaded", () => {
    const late = hub();
    if (late) {
      late.registerScanner(scanner);
    } else {
      editor.debug(
        `agent-sessions: hub not loaded; "${scanner.id}" sessions will not be discovered`,
      );
    }
  });
}

// ── Helpers every scanner needs ───────────────────────────────────

/** Join path segments with the machine's own separator. */
export function joinPath(machine: FreshMachine, ...parts: string[]): string {
  const sep = machine.platform === "windows" ? "\\" : "/";
  return parts
    .filter((p) => p.length > 0)
    .join(sep)
    .replace(/[\\/]+/g, sep);
}

/** One entry's header. `text` is null when unreadable; the reason is already in `problems`. */
export interface HeaderRead {
  entry: WalkTreeEntry;
  text: string | null;
}

/**
 * Read the header of the newest `limit` entries in one batch. The cap is applied
 * before the read: on a remote machine the read is one round trip per file. A
 * batch cut short is paired by path, not position. Whether a file with no
 * readable header is still a session is the caller's decision.
 */
export async function readHeaders(
  machine: FreshMachine,
  entries: WalkTreeEntry[],
  opts: { maxBytes: number; limit?: number; problems: string[] },
): Promise<HeaderRead[]> {
  const wanted = [...entries].sort((a, b) => b.mtime - a.mtime);
  if (opts.limit !== undefined && wanted.length > opts.limit) {
    opts.problems.push(`reporting the newest ${opts.limit} of ${wanted.length}`);
    wanted.length = opts.limit;
  }
  if (wanted.length === 0) return [];

  const prefixes = await machine.readFilePrefixes(
    wanted.map((entry) => ({ path: entry.path, maxBytes: opts.maxBytes })),
  );
  // One problem line for the whole shortfall, not one per missing file.
  const whole = prefixes.length === wanted.length;
  const byPath = whole ? null : new Map(prefixes.map((p) => [p.path, p]));
  if (!whole) {
    opts.problems.push(
      `${wanted.length - prefixes.length} of ${wanted.length} headers not read — the read was cut short`,
    );
  }
  return wanted.map((entry, i) => {
    const prefix = whole ? prefixes[i] : byPath!.get(entry.path);
    if (prefix === undefined) return { entry, text: null };
    if (prefix.error !== undefined) {
      opts.problems.push(`${entry.path}: ${prefix.error}`);
      return { entry, text: null };
    }
    return { entry, text: prefix.text ?? "" };
  });
}

/**
 * Directories that environment variables point at, for the names set to an
 * absolute path. A relative value is ignored and reported: resolving it would
 * aim the scan somewhere unrelated, and dropping it silently would hide a store.
 */
export async function envDirs(
  machine: FreshMachine,
  names: string[],
  problems: string[],
): Promise<Record<string, string>> {
  const out: Record<string, string> = {};
  if (names.length === 0) return out;
  const env = await machine.env(names);
  for (const name of names) {
    const value = env[name];
    if (!value || value.trim().length === 0) continue;
    if (isAbsoluteOn(machine, value)) {
      out[name] = value;
      continue;
    }
    problems.push(`${name} is set to a relative path (${value}); ignoring it`);
  }
  return out;
}

/** The last component of a path, whichever separator it uses. */
export function baseName(path: string): string {
  const parts = path.split(/[\\/]/).filter((p) => p.length > 0);
  return parts.length > 0 ? parts[parts.length - 1] : "";
}

/** Whether `value` is absolute on this machine, not on the host: a Windows
 *  target's `D:\store` is absolute even when the editor runs on Linux. */
export function isAbsoluteOn(machine: FreshMachine, value: string): boolean {
  if (machine.platform !== "windows") return value.startsWith("/");
  if (value.startsWith("\\\\")) return true; // UNC
  return /^[A-Za-z]:[\\/]/.test(value);
}

/** The first line of `text` parsed as a JSON object, or null. */
export function firstJsonRecord(text: string): Record<string, unknown> | null {
  const line = text.split("\n", 1)[0]?.trim();
  if (!line) return null;
  try {
    const parsed = JSON.parse(line) as unknown;
    return typeof parsed === "object" && parsed !== null
      ? (parsed as Record<string, unknown>)
      : null;
  } catch {
    return null;
  }
}

/**
 * Parse up to `maxRecords` JSONL records. Not just the first line: a transcript's
 * first record is often bookkeeping with no cwd (Claude Code opens with a
 * `queue-operation`). A record budget rather than bytes, because one record can
 * be a megabyte. A truncated last line fails to parse and is skipped.
 */
export function jsonRecords(
  text: string,
  maxRecords = 32,
): Record<string, unknown>[] {
  const out: Record<string, unknown>[] = [];
  for (const line of text.split("\n")) {
    if (out.length >= maxRecords) break;
    const trimmed = line.trim();
    if (!trimmed) continue;
    try {
      const parsed = JSON.parse(trimmed) as unknown;
      if (typeof parsed === "object" && parsed !== null) {
        out.push(parsed as Record<string, unknown>);
      }
    } catch {
      // A partial or non-JSON line is skipped.
    }
  }
  return out;
}

/** The first of `fields` present as a non-empty string across `records`. */
export function pickStringFrom(
  records: Record<string, unknown>[],
  fields: string[],
): string | undefined {
  for (const record of records) {
    const found = pickString(record, fields);
    if (found !== undefined) return found;
  }
  return undefined;
}

/** The first of `fields` present as a non-empty string, at the top level or
 *  inside a `payload` envelope (Codex rollouts wrap their fields in one). */
export function pickString(
  record: Record<string, unknown> | null,
  fields: string[],
): string | undefined {
  if (!record) return undefined;
  const envelope = record["payload"];
  const scopes: Record<string, unknown>[] = [record];
  if (typeof envelope === "object" && envelope !== null) {
    scopes.push(envelope as Record<string, unknown>);
  }
  for (const scope of scopes) {
    for (const field of fields) {
      const value = scope[field];
      if (typeof value === "string" && value.length > 0) return value;
    }
  }
  return undefined;
}
