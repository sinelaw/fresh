/**
 * The known coding agents, one module each (`agent_<id>.ts`), and the
 * agent-independent helpers that read them.
 *
 * The four launcher-priority agents come first (claude, codex, opencode), then
 * the long-standing aider entry. Order here drives the preset-row order.
 */

import type { AgentEntry, AgentResumeSpec } from "./agent_types.ts";
import { AIDER } from "./agent_aider.ts";
import { CLAUDE } from "./agent_claude.ts";
import { CODEX } from "./agent_codex.ts";
import { OPENCODE } from "./agent_opencode.ts";

export type { AgentEntry, AgentPromptArg, AgentResumeSpec, AgentSystemPrompt } from "./agent_types.ts";

export const AGENT_REGISTRY: AgentEntry[] = [CLAUDE, CODEX, OPENCODE, AIDER];

/** The entry whose matcher accepts a command's argv0 basename, or null. */
export function agentEntryForBase(base: string): AgentEntry | null {
  return AGENT_REGISTRY.find((e) => e.match.test(base)) ?? null;
}

/** Whether `argv` already names a session, per the agent's `provision` flags,
 *  as `--flag value` or `--flag=value`. `id` is the uuid an id flag pins;
 *  absent when the flag selects a session without naming one (`--continue`,
 *  a bare `--resume` picker, `--resume <search term>`). */
export function namedSession(
  argv: string[],
  provision: NonNullable<AgentResumeSpec["provision"]>,
): { id?: string } | null {
  const idFlags = new Set([provision.idFlag, ...(provision.idFlags ?? [])]);
  const sessionFlags = new Set(provision.sessionFlags ?? []);
  for (let i = 1; i < argv.length; i++) {
    const arg = argv[i];
    const eq = arg.indexOf("=");
    const flag = eq > 0 ? arg.slice(0, eq) : arg;
    if (sessionFlags.has(flag)) return {};
    if (!idFlags.has(flag)) continue;
    const value = eq > 0 ? arg.slice(eq + 1) : argv[i + 1];
    return value && UUID_RE.test(value) ? { id: value } : {};
  }
  return null;
}
const UUID_RE = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i;
