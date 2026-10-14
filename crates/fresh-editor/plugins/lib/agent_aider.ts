import type { AgentEntry } from "./agent_types.ts";

// aider keeps its conversation in the repo and reloads it with
// `--restore-chat-history`; it has no caller-supplied session id, so it's
// a continue-only (strategy B) agent. `--yes-always` auto-confirms; `-m`
// hands it a message.
export const AIDER: AgentEntry = {
  id: "aider",
  label: "aider",
  match: /^aider$/,
  spec: { continue: { resumeArgs: ["--restore-chat-history"] } },
  auto: ["--yes-always"],
  prompt: { style: "flag", flag: "-m" },
};
