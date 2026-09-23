/// <reference path="./lib/fresh.d.ts" />

/**
 * Agent Sessions hub. Opens a machine, runs every registered scanner against
 * it, and merges and correlates what they return. Each tool's on-disk format
 * lives in its own scanner plugin, registered through `registerScanner`.
 *
 * A scanner only asks the machine handle questions. It never reads a path
 * itself and does not know whether the machine is local or remote.
 */

import type {
  AgentSessionsApi,
  CollectedSession,
  Link,
  ScanContext,
  ScanOptions,
  ScanResult,
  Scanner,
  ScannerReport,
  MachineSpec,
  ToolStatus,
} from "./lib/agent_scanner.ts";
import { baseName } from "./lib/agent_scanner.ts";
import { resolveProjects } from "./lib/project_identity.ts";

const editor = getEditor();

// Budget for the whole scan, not per scanner. When it runs out the caller gets
// what was collected so far plus a problem naming the scanner still being
// asked. The in-flight call is not cancelled (the plugin API cannot); closing
// the machine handle in the `finally` below takes it down.
const SCAN_BUDGET_MS = 60_000;

// Sentinel for "the budget ran out", distinct from anything a scanner returns.
const SCAN_TIMED_OUT: unique symbol = Symbol("scan budget exhausted");

declare global {
  interface FreshPluginRegistry {
    "agent-sessions": AgentSessionsApi;
  }
}

// ── Registry ──────────────────────────────────────────────────────

const scanners: Scanner[] = [];

function registerScanner(scanner: Scanner): () => void {
  if (typeof scanner !== "object" || scanner === null) {
    throw new Error("agent-sessions.registerScanner: scanner must be an object");
  }
  if (typeof scanner.id !== "string" || scanner.id.length === 0) {
    throw new Error("agent-sessions.registerScanner: id must be a non-empty string");
  }
  if (typeof scanner.displayName !== "string" || scanner.displayName.length === 0) {
    throw new Error("agent-sessions.registerScanner: displayName must be a non-empty string");
  }
  if (typeof scanner.scan !== "function") {
    throw new Error("agent-sessions.registerScanner: scan must be a function");
  }
  // Re-registering the same id replaces, so a plugin reload does not scan twice.
  unregisterScanner(scanner.id);
  scanners.push(scanner);
  scanners.sort((a, b) => a.id.localeCompare(b.id));
  return () => {
    unregisterScanner(scanner.id);
  };
}

function unregisterScanner(id: string): boolean {
  const i = scanners.findIndex((s) => s.id === id);
  if (i < 0) return false;
  scanners.splice(i, 1);
  return true;
}

// ── Scanning ──────────────────────────────────────────────────────

/** A link's two endpoints as one key. NUL cannot appear in a tool or session id. */
function pairKey(host: string, hosted: string): string {
  return `${host}\u0000${hosted}`;
}

/** `tool/id`, the key a link refers to a row by. */
function keyOf(session: CollectedSession): string {
  return `${session.tool}/${session.id}`;
}

/** Newest first, unknown mtimes last. */
function byNewest(a: CollectedSession, b: CollectedSession): number {
  return (b.mtime ?? 0) - (a.mtime ?? 0);
}

/**
 * Tie rows together across tools. A row that records another tool's session
 * id is an exact link; a shared cwd is only a likely one.
 */
function correlate(sessions: CollectedSession[]): Link[] {
  const links: Link[] = [];

  const byId = new Map<string, CollectedSession[]>();
  for (const s of sessions) {
    const list = byId.get(s.id);
    if (list) list.push(s);
    else byId.set(s.id, [s]);
  }

  for (const host of sessions) {
    if (!host.agentSessionId) continue;
    for (const hosted of byId.get(host.agentSessionId) ?? []) {
      if (hosted.tool === host.tool) continue;
      links.push({
        host: keyOf(host),
        hosted: keyOf(hosted),
        reason: "recorded-session-id",
        confidence: "exact",
      });
    }
  }

  // Skip pairs already tied by a recorded id.
  const exact = new Set(links.map((l) => pairKey(l.host, l.hosted)));
  const byCwd = new Map<string, CollectedSession[]>();
  for (const s of sessions) {
    if (!s.cwd) continue;
    const list = byCwd.get(s.cwd);
    if (list) list.push(s);
    else byCwd.set(s.cwd, [s]);
  }
  for (const group of byCwd.values()) {
    if (group.length < 2) continue;
    for (const host of group) {
      for (const hosted of group) {
        if (host === hosted || host.tool === hosted.tool) continue;
        const pair = pairKey(keyOf(host), keyOf(hosted));
        if (exact.has(pair)) continue;
        links.push({
          host: keyOf(host),
          hosted: keyOf(hosted),
          reason: "same-cwd",
          confidence: "likely",
        });
      }
    }
  }

  return links;
}

/**
 * Retitle rows whose titles collide by appending a tail of the id. The tail
 * grows until it separates them; ordinals when even the ids match.
 */
function disambiguate(clashing: CollectedSession[]): void {
  const ids = clashing.map((s) => s.id.split("/").pop() ?? s.id);
  const longest = Math.max(...ids.map((id) => id.length));
  let width = 8;
  let tails = ids.map((id) => id.slice(-width));
  while (new Set(tails).size !== ids.length && width < longest) {
    width = Math.min(width * 2, longest);
    tails = ids.map((id) => id.slice(-width));
  }
  const separated = new Set(tails).size === ids.length;
  clashing.forEach((s, i) => {
    s.title = `${s.title} (${separated ? tails[i] : String(i + 1)})`;
  });
}

async function scan(spec: MachineSpec | null, opts?: ScanOptions): Promise<ScanResult> {
  const limit = opts?.maxSessionsPerTool ?? 200;
  const problems: string[] = [];
  // Commands are on by default. A caller that opened the machine only to look
  // at it turns them off.
  const ctx: ScanContext = { allowCommands: opts?.allowCommands ?? true, maxSessions: limit };

  // The stop is handed over before the connect, so a caller can abandon a scan
  // that is still dialling. A machine still connecting is closed the moment it
  // arrives.
  let abandoned = false;
  let opened: FreshMachine | null = null;
  const closeOpened = (): void => {
    if (opened && opened.id !== 0) void opened.close();
  };
  opts?.onStarted?.(() => {
    if (abandoned) return;
    abandoned = true;
    closeOpened();
  });

  // `null` means the machine the editor is acting on. The bare ops already
  // address it, so it is the same shape with id 0 and nothing to close.
  const machine: FreshMachine =
    spec === null || spec === undefined
      ? {
          id: 0,
          platform: "",
          home: editor.getEnv("HOME") ?? editor.getEnv("USERPROFILE") ?? "",
          label: "",
          walkTree: (root, options) => editor.walkTree(root, options),
          readFilePrefixes: (requests) => editor.readFilePrefixes(requests),
          run: (program, args, cwd) => editor.runOnTarget(program, args, cwd),
          env: (names) => editor.machineEnv(names),
          close: () => Promise.resolve(true),
        }
      : await editor.openMachine(spec);
  opened = machine;
  // Abandoned while connecting: closing the handle cancels any work already
  // issued against it.
  if (abandoned) {
    closeOpened();
    return {
      machine: machine.label || "local",
      sessions: [],
      links: [],
      tools: [],
      problems,
    };
  }

  // Reading the environment is itself a command, so a read-only machine gets
  // an empty map from `machine.env`, and a store relocated by a variable then
  // reads as not installed. Reported once here rather than by every scanner.
  const dialled =
    spec !== null &&
    spec !== undefined &&
    "kind" in spec &&
    (spec.kind === "ssh" || spec.kind === "kubectl-exec");
  if (dialled && !ctx.allowCommands) {
    problems.push(
      "machine opened read-only: its environment cannot be read, so a relocated store reads as not installed",
    );
  }

  const sessions: CollectedSession[] = [];
  const tools: ScanResult["tools"] = [];

  const deadline = Date.now() + SCAN_BUDGET_MS;

  try {
    for (const scanner of scanners) {
      let report: ScannerReport;
      // The budget is shared. A scan that has already overrun does not start
      // the next scanner.
      if (abandoned) break;
      const remaining = deadline - Date.now();
      if (remaining <= 0) {
        const note = "not asked — the scan ran out of time first";
        problems.push(`${scanner.id}: ${note}`);
        tools.push({ id: scanner.id, displayName: scanner.displayName, status: "failed", count: 0, note });
        continue;
      }
      try {
        const outcome = await Promise.race([
          scanner.scan(machine, ctx),
          // Annotated so the race result narrows on the sentinel check below.
          editor.delay(remaining).then((): typeof SCAN_TIMED_OUT => SCAN_TIMED_OUT),
        ]);
        if (outcome === SCAN_TIMED_OUT) {
          const note =
            `stopped waiting after ${Math.round(SCAN_BUDGET_MS / 1000)}s — ` +
            `the machine did not answer, so anything it holds is missing from this list`;
          problems.push(`${scanner.id}: ${note}`);
          tools.push({ id: scanner.id, displayName: scanner.displayName, status: "failed", count: 0, note });
          continue;
        }
        report = outcome;
      } catch (e) {
        // One scanner throwing must not lose the rest of the scan.
        const note = e instanceof Error ? e.message : String(e);
        problems.push(`${scanner.id}: ${note}`);
        tools.push({ id: scanner.id, displayName: scanner.displayName, status: "failed", count: 0, note });
        continue;
      }

      const found = (report.sessions ?? []).slice().sort(byNewest);
      if (found.length > limit) {
        problems.push(
          `${scanner.id}: reporting the newest ${limit} of ${found.length} sessions`,
        );
        found.length = limit;
      }
      for (const s of found) {
        // The scanner's own id wins over `tool`. `||` rather than `??`: a
        // scanner with no title tends to report an empty string.
        const title = s.title || (s.cwd ? baseName(s.cwd) : "") || s.id;
        sessions.push({ ...s, tool: scanner.id, title });
      }
      const status: ToolStatus = report.unsupported !== undefined
        ? "unsupported"
        : (report.installed ?? found.length > 0)
          ? "found"
          : "absent";
      tools.push({
        id: scanner.id,
        displayName: scanner.displayName,
        status,
        count: found.length,
        note: report.unsupported,
      });
      for (const p of report.problems ?? []) problems.push(`${scanner.id}: ${p}`);
    }

    // Projects are resolved once for every cwd after the loop: one batched
    // read instead of a round trip per session. Inside the `try` because it
    // needs the machine open. On timeout rows keep their cwd and the dialog
    // groups on that.
    if (!abandoned) {
      const remaining = deadline - Date.now();
      const cwds = sessions.map((s) => s.cwd ?? "").filter((c) => c !== "");
      if (remaining > 0 && cwds.length > 0) {
        const projects = await Promise.race([
          resolveProjects(machine, cwds),
          editor.delay(remaining).then((): typeof SCAN_TIMED_OUT => SCAN_TIMED_OUT),
        ]);
        if (projects === SCAN_TIMED_OUT) {
          problems.push(
            "projects: stopped waiting — rows are grouped by directory rather than by repository",
          );
        } else {
          for (const s of sessions) {
            const found = s.cwd ? projects.get(s.cwd) : undefined;
            if (found) s.project = found;
          }
        }
      }
    }
  } finally {
    // A leaked handle holds an SSH connection open for the life of the editor.
    // Closing one already closed by `stop` resolves rather than failing.
    if (machine.id !== 0 && !abandoned) await machine.close();
  }

  // Rows of one tool with identical titles (a title that fell back to a
  // directory worked in twice) are told apart by id: stable between scans and
  // independent of the machine's clock.
  const byTitle = new Map<string, CollectedSession[]>();
  for (const s of sessions) {
    const key = `${s.tool}\n${s.title ?? ""}`;
    const seen = byTitle.get(key);
    if (seen) seen.push(s);
    else byTitle.set(key, [s]);
  }
  for (const clashing of byTitle.values()) {
    if (clashing.length >= 2) disambiguate(clashing);
  }

  sessions.sort(byNewest);
  return {
    machine: machine.label || "local",
    sessions,
    links: opts?.correlate === false ? [] : correlate(sessions),
    tools,
    problems,
  };
}

editor.exportPluginApi("agent-sessions", {
  registerScanner,
  unregisterScanner,
  listScanners(): { id: string; displayName: string }[] {
    return scanners.map((s) => ({ id: s.id, displayName: s.displayName }));
  },
  scan,
} satisfies AgentSessionsApi);

editor.debug(`Agent Sessions plugin loaded (${scanners.length} scanners registered)`);

