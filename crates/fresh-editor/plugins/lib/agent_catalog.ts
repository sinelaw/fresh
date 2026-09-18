/**
 * Table of coding agents that keep one transcript file per session under $HOME,
 * with the cwd in the first record. Adding an agent is a row here.
 */

/** Which files under a store's root are transcripts. */
export type FileRule =
  | { kind: "any" }
  | { kind: "base-name"; value: string }
  | { kind: "base-name-prefix"; value: string }
  | { kind: "path-contains"; value: string }
  /** `state.json` whose parent starts with `session_`; skips Kimi's sibling `agents/<id>/wire.jsonl`. */
  | { kind: "kimi-state" };

export type Layout =
  | "file-stem"
  | "parent-dir-name"
  /** Format not parsed: files are reported as evidence, no sessions claimed. */
  | "opaque";

export interface AgentStore {
  id: string;
  displayName: string;
  /** Variables that relocate the store's base, in priority order. */
  baseEnv: string[];
  baseSegments: string[];
  /** Appended to the base, however it was resolved. */
  suffix: string[];
  extensions: string[];
  fileRule: FileRule;
  /** Bounded: a mis-set `*_HOME` must not crawl the whole disk. */
  maxDepth: number;
  layout: Layout;
  /** Top-level keys that hold the working directory, tried in order. */
  cwdFields: string[];
}

export const AGENT_STORES: AgentStore[] = [
  {
    id: "gemini",
    displayName: "Gemini CLI",
    baseEnv: [],
    baseSegments: [".gemini", "tmp"],
    suffix: [],
    extensions: [".json", ".jsonl"],
    fileRule: { kind: "any" },
    maxDepth: 4,
    layout: "file-stem",
    cwdFields: ["cwd", "projectRoot"],
  },
  {
    id: "copilot",
    displayName: "GitHub Copilot CLI",
    baseEnv: ["COPILOT_HOME"],
    baseSegments: [".copilot"],
    suffix: ["session-state"],
    extensions: [".jsonl"],
    fileRule: { kind: "any" },
    maxDepth: 3,
    layout: "file-stem",
    cwdFields: ["cwd", "workingDirectory"],
  },
  {
    id: "cursor",
    displayName: "Cursor Agent",
    baseEnv: [],
    baseSegments: [".cursor", "projects"],
    suffix: [],
    extensions: [".jsonl"],
    fileRule: { kind: "path-contains", value: "agent-transcripts" },
    maxDepth: 4,
    layout: "file-stem",
    cwdFields: ["cwd"],
  },
  {
    id: "grok",
    displayName: "Grok CLI",
    baseEnv: ["GROK_SESSIONS_DIR"],
    baseSegments: [".grok", "sessions"],
    suffix: [],
    extensions: [".json"],
    fileRule: { kind: "base-name", value: "summary.json" },
    maxDepth: 3,
    layout: "parent-dir-name",
    cwdFields: ["cwd", "workspace"],
  },
  {
    id: "devin",
    displayName: "Devin CLI",
    baseEnv: ["DEVIN_HOME"],
    baseSegments: [".local", "share", "devin", "cli"],
    suffix: ["transcripts"],
    extensions: [".json"],
    fileRule: { kind: "any" },
    maxDepth: 3,
    layout: "file-stem",
    cwdFields: ["cwd"],
  },
  {
    id: "hermes",
    displayName: "Hermes",
    baseEnv: [],
    baseSegments: [".hermes", "sessions"],
    suffix: [],
    extensions: [".json"],
    fileRule: { kind: "base-name-prefix", value: "session_" },
    maxDepth: 3,
    layout: "file-stem",
    cwdFields: ["cwd"],
  },
  {
    id: "rovo",
    displayName: "Rovo Dev",
    baseEnv: [],
    baseSegments: [".rovodev", "sessions"],
    suffix: [],
    extensions: [".json"],
    fileRule: { kind: "base-name", value: "metadata.json" },
    maxDepth: 3,
    layout: "parent-dir-name",
    cwdFields: ["cwd", "workspace"],
  },
  {
    id: "pi",
    displayName: "Pi",
    baseEnv: ["PI_CODING_AGENT_DIR"],
    baseSegments: [".pi", "agent", "sessions"],
    suffix: [],
    extensions: [".jsonl"],
    fileRule: { kind: "any" },
    maxDepth: 3,
    layout: "file-stem",
    cwdFields: ["cwd"],
  },
  {
    id: "omp",
    displayName: "OMP",
    baseEnv: ["OMP_CODING_AGENT_DIR"],
    baseSegments: [".omp", "agent", "sessions"],
    suffix: [],
    extensions: [".jsonl"],
    fileRule: { kind: "any" },
    maxDepth: 3,
    layout: "file-stem",
    cwdFields: ["cwd"],
  },
  {
    id: "primeagent",
    displayName: "Prime Agent",
    baseEnv: ["PRIME_CODING_AGENT_DIR", "PRIME_AGENT_SESSIONS_DIR"],
    baseSegments: [".prime", "agent", "sessions"],
    suffix: [],
    extensions: [".jsonl"],
    fileRule: { kind: "any" },
    maxDepth: 3,
    layout: "file-stem",
    cwdFields: ["cwd"],
  },
  {
    id: "openclaw",
    displayName: "OpenClaw",
    baseEnv: ["OPENCLAW_STATE_DIR"],
    baseSegments: [".openclaw"],
    suffix: ["agents"],
    extensions: [".jsonl"],
    fileRule: { kind: "path-contains", value: "sessions" },
    maxDepth: 4,
    layout: "file-stem",
    cwdFields: ["cwd"],
  },
  {
    id: "droid",
    displayName: "Factory Droid",
    baseEnv: [],
    baseSegments: [".factory", "sessions"],
    suffix: [],
    extensions: [".jsonl"],
    fileRule: { kind: "any" },
    maxDepth: 3,
    layout: "file-stem",
    cwdFields: ["cwd"],
  },
  {
    id: "cline",
    displayName: "Cline",
    baseEnv: ["CLINE_SESSION_DATA_DIR"],
    baseSegments: [".cline", "data", "sessions"],
    suffix: [],
    extensions: [".json"],
    fileRule: { kind: "any" },
    maxDepth: 3,
    layout: "parent-dir-name",
    cwdFields: ["cwd", "workspace"],
  },
  {
    id: "kimi",
    displayName: "Kimi Code",
    baseEnv: ["KIMI_CODE_HOME"],
    baseSegments: [".kimi-code"],
    suffix: ["sessions"],
    extensions: [".json"],
    fileRule: { kind: "kimi-state" },
    maxDepth: 3,
    layout: "parent-dir-name",
    cwdFields: ["cwd", "workingDirectory"],
  },
  {
    id: "opencode",
    displayName: "opencode",
    baseEnv: ["OPENCODE_DATA_DIR"],
    baseSegments: [".local", "share", "opencode"],
    suffix: [],
    extensions: [".db", ".sqlite"],
    fileRule: { kind: "any" },
    maxDepth: 2,
    layout: "opaque",
    cwdFields: [],
  },
  {
    id: "amp",
    displayName: "Amp",
    baseEnv: ["AMP_DATA_DIR"],
    baseSegments: [".amp", "threads"],
    suffix: [],
    extensions: [".json", ".jsonl"],
    fileRule: { kind: "any" },
    maxDepth: 3,
    layout: "file-stem",
    cwdFields: ["cwd"],
  },
  {
    id: "aider",
    displayName: "Aider",
    baseEnv: [],
    baseSegments: [".aider"],
    suffix: [],
    extensions: [".md", ".json"],
    fileRule: { kind: "any" },
    maxDepth: 2,
    layout: "opaque",
    cwdFields: [],
  },
  {
    id: "goose",
    displayName: "goose",
    baseEnv: ["GOOSE_DATA_DIR"],
    baseSegments: [".local", "share", "goose", "sessions"],
    suffix: [],
    extensions: [".jsonl"],
    fileRule: { kind: "any" },
    maxDepth: 2,
    layout: "file-stem",
    cwdFields: ["cwd", "working_dir"],
  },
  {
    id: "qwen",
    displayName: "Qwen Code",
    baseEnv: [],
    baseSegments: [".qwen", "tmp"],
    suffix: [],
    extensions: [".json", ".jsonl"],
    fileRule: { kind: "any" },
    maxDepth: 4,
    layout: "file-stem",
    cwdFields: ["cwd", "projectRoot"],
  },
];

/** Whether `rel` (relative to the store root, "/"-separated) is a transcript under `rule`. */
export function fileRuleAccepts(rule: FileRule, rel: string): boolean {
  const parts = rel.split("/").filter((p) => p.length > 0);
  const base = parts.length > 0 ? parts[parts.length - 1] : "";
  switch (rule.kind) {
    case "any":
      return true;
    case "base-name":
      return base === rule.value;
    case "base-name-prefix":
      return base.startsWith(rule.value);
    case "path-contains":
      // Whole path component, not substring: "sessions-old" must not match "sessions".
      return parts.includes(rule.value);
    case "kimi-state":
      return (
        base === "state.json" &&
        parts.length >= 2 &&
        parts[parts.length - 2].startsWith("session_")
      );
  }
}

/** The session id a store's layout reads out of a transcript path. */
export function sessionIdFor(layout: Layout, rel: string): string {
  const parts = rel.split("/").filter((p) => p.length > 0);
  const base = parts.length > 0 ? parts[parts.length - 1] : rel;
  if (layout === "parent-dir-name") {
    return parts.length >= 2 ? parts[parts.length - 2] : base;
  }
  // `> 0`, not `>= 0`: a dotfile like `.session` has no extension to strip.
  const dot = base.lastIndexOf(".");
  return dot > 0 ? base.slice(0, dot) : base;
}

/** Whether `rel` ends with one of `extensions`, case-insensitively. */
export function hasExtension(extensions: string[], rel: string): boolean {
  if (extensions.length === 0) return true;
  const lower = rel.toLowerCase();
  return extensions.some((ext) => lower.endsWith(ext));
}
