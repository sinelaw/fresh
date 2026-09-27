import type { AgentEntry } from "./agent_types.ts";

// opencode (SST): `--continue` resumes the latest session in the cwd.
// "Auto"/YOLO mode is config-driven (permissions in opencode.json), so it
// has no launch flag — the checkbox is hidden. `--prompt` submits the text as
// the first message (it does not merely seed the input box).
export const OPENCODE: AgentEntry = {
  id: "opencode",
  label: "opencode",
  match: /^opencode$/,
  spec: { continue: { resumeArgs: ["--continue"] } },
  prompt: { style: "flag", flag: "--prompt" },
  systemPrompt: { via: "prompt" },
};
