import type { AgentEntry } from "./agent_types.ts";

// OpenAI Codex CLI: resume is a *subcommand*, not a flag — `codex resume
// --last` rejoins the latest session in the cwd. There's no launch-time
// session-id to pin, so it's continue-only.
//
// Auto mode: `--full-auto` was REMOVED from the root command (recent Codex
// rejects `codex --full-auto` outright; it survives only under `codex exec`
// as a deprecation warning that redirects to `--sandbox workspace-write`).
// Codex runs model-proposed commands inside the workspace-write sandbox —
// deliberately NOT `-s danger-full-access` nor the
// `--dangerously-bypass-approvals-and-sandbox` full bypass.
//
// `--ask-for-approval on-request` + `approvals_reviewer = "auto_review"`
// is the counterpart of claude's `--permission-mode auto`: the model
// escalates when it needs out of the sandbox, and an automated reviewer —
// not the human — rules on the request. `never` cannot be used here: it
// fails escalations outright rather than reviewing them, which silently
// breaks "Teach Fresh CLI", since the editor's control socket lives
// outside the workspace and `connect()` to it is EPERM inside the sandbox.
//
// All of these are accepted on the root command AND on the `resume`
// subcommand, so they ride launch and resume alike. The initial prompt is
// a trailing positional (`codex "…"`).
export const CODEX: AgentEntry = {
  id: "codex",
  label: "codex",
  match: /^codex$/,
  spec: { continue: { resumeArgs: ["resume", "--last"] } },
  auto: [
    "--sandbox",
    "workspace-write",
    "--ask-for-approval",
    "on-request",
    "-c",
    'approvals_reviewer="auto_review"',
  ],
  prompt: { style: "positional" },
  systemPrompt: { via: "prompt" },
};
