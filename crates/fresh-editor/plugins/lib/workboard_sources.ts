/**
 * The Workboard's own session sources, as pure mappings: sessions running
 * outside the editor (the `live-sessions` plugin's, as the Orchestrator lists
 * them) and sessions found by a scan (the `agent-discovery` plugin's). The
 * wiring — which API to ask, when to refresh, how to open — is in
 * `workboard.ts`; what a session looks like as a board item is here, so it is
 * unit-tested without an editor (`tests/workboard_sources.test.ts`).
 */

import { baseName } from "./agent_scanner.ts";
import type { DetectedSession } from "./discovery.ts";
import { type LiveSession, liveDetail } from "./live_sessions.ts";
import type { WorkboardItem } from "./workboard_model.ts";

/** A running session outside the editor, as a board item. */
export function liveSessionItem(s: LiveSession): WorkboardItem {
  const where = s.where === "cloud" ? (s.remoteControl ? "remote control" : "cloud") : s.sshHost;
  return {
    id: s.key,
    title: s.title,
    state: s.state,
    agent: s.agent,
    // A Desktop session over SSH has its cwd on that host, not here.
    ...(s.cwd ? { cwd: s.cwd } : {}),
    ...(cloudProject(s) ?? {}),
    ...(where ? { where } : {}),
    ...(s.waitingFor ? { question: s.waitingFor } : {}),
    status: liveDetail(s),
    ...(s.updatedAt !== undefined ? { updatedAt: s.updatedAt } : {}),
  };
}

/** A cloud session's repository as a project. Claude's and Codex's cloud
 *  sessions run on GitHub repositories, so `owner/name` is the identity a
 *  local checkout of it resolves to (`lib/project_identity.ts`) and the two
 *  share a Projects group. */
function cloudProject(s: LiveSession): { project: { id: string; label: string } } | null {
  if (s.cwd || !s.repo || !/^[^/\s]+\/[^/\s]+$/.test(s.repo)) return null;
  return { project: { id: `repo:github.com/${s.repo.toLowerCase()}`, label: s.repo } };
}

/** How old a found session may be and still be listed: older ones are
 *  history, not work in progress. */
export const DETECTED_MAX_AGE_DAYS = 14;

/** A found session's item id: unique across machines and tools. */
export function detectedItemId(d: DetectedSession): string {
  return JSON.stringify([d.machineKey, d.session.tool ?? "", d.session.id]);
}

/** What `detectedItemId` packed, or null for an id it did not make. */
export function parseDetectedItemId(id: string): { machineKey: string; tool: string; sessionId: string } | null {
  try {
    const v: unknown = JSON.parse(id);
    if (Array.isArray(v) && v.length === 3 && v.every((x) => typeof x === "string")) {
      return { machineKey: v[0], tool: v[1], sessionId: v[2] };
    }
  } catch {
    // Not one of ours.
  }
  return null;
}

/** The sessions a scan found, as board items: those active in the last
 *  `maxAgeDays` (a live listing such as a tmux pane has no age and is kept),
 *  newest first. */
export function detectedItems(
  found: readonly DetectedSession[],
  now: number,
  maxAgeDays = DETECTED_MAX_AGE_DAYS,
): WorkboardItem[] {
  const oldest = now - maxAgeDays * 86_400_000;
  const items: WorkboardItem[] = [];
  for (const d of found) {
    const s = d.session;
    const at = s.mtime !== undefined ? s.mtime * 1000 : undefined;
    if (at !== undefined && at < oldest) continue;
    const tool = s.tool ?? "";
    // The Orchestrator's scan targets key this computer `local`.
    const away = d.machineKey !== "local" ? d.machineLabel : "";
    items.push({
      id: detectedItemId(d),
      title: s.title || (s.cwd ? baseName(s.cwd) : "") || s.id,
      state: "idle",
      ...(s.agent || tool ? { agent: s.agent || tool } : {}),
      ...(s.cwd ? { cwd: s.cwd } : {}),
      ...(s.project?.branch ? { branch: s.project.branch } : {}),
      ...(s.project ? { project: { id: s.project.id, label: s.project.label } } : {}),
      ...(away ? { where: away } : {}),
      status: away ? `${tool} · ${away}` : tool,
      ...(at !== undefined ? { updatedAt: at } : {}),
    });
  }
  return items.sort((a, b) => (b.updatedAt ?? 0) - (a.updatedAt ?? 0));
}

