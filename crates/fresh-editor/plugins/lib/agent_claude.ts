import type { AgentEntry } from "./agent_types.ts";

// Claude Code CLI: `--session-id <uuid>` pins the session at launch;
// `--resume <uuid>` rejoins it; `--continue` resumes the latest in cwd.
export const CLAUDE: AgentEntry = {
  id: "claude",
  label: "claude",
  match: /^claude$/,
  spec: {
    provision: {
      idFlag: "--session-id",
      resumeArgs: ["--resume", "{id}"],
      idFlags: ["--resume", "-r"],
      sessionFlags: ["--continue", "-c", "--from-pr", "--teleport"],
      // `<config>/projects/<cwd, encoded>/<id>.jsonl`, written at the first
      // message — so a run that quit at the trust prompt leaves none. The
      // folder is matched as `*`: Claude shortens a long cwd's name and adds
      // a hash, and the session id alone is unique.
      transcriptFor: (_cwd, id, configDir) => `${configDir}/projects/*/${id}.jsonl`,
      configDir: (env, home) => env("CLAUDE_CONFIG_DIR") || `${home}/.claude`,
    },
    continue: { resumeArgs: ["--continue"] },
  },
// "Auto mode" = `--permission-mode auto`: the safe-autonomous mode (a
// classifier vets actions before they run) — deliberately NOT
// `--dangerously-skip-permissions` (which is `bypassPermissions`, the
// unchecked maximal bypass, reserved for isolated containers).
  auto: ["--permission-mode", "auto"],
  prompt: { style: "positional" },
  systemPrompt: { via: "flag", flag: "--append-system-prompt" },
};
