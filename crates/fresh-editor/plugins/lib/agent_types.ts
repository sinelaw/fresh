/**
 * Agent registry types: how Fresh launches, prompts and resumes a known
 * coding agent. Each agent's entry lives in its own `agent_<id>.ts`, and
 * `agent_registry.ts` collects them.
 */
// How known coding agents rejoin a prior conversation after an editor restart.
// This is *policy/data*: the host core knows none of it — it just persists the
// resolved `resume` argv and runs it on restore (see the `resume` option on
// `createWindowWithTerminal` and `terminal.resume_agents`). Two strategies,
// preferring the first when an agent supports it:
//
//   provision — mint a session id at launch (`<agent> … --session-id <uuid>`)
//               and resume with it (`<agent> --resume <uuid>`). Precise: the id
//               is ours from birth, so there's nothing to capture and no need
//               to read the agent's private state. The uuid is a plain argv
//               element, never interpolated into a shell string.
//   continue  — resume the most recent session in the cwd (`<agent> --continue`),
//               no id. Relies on the orchestrator's one-agent-per-worktree
//               model, where "latest in this cwd" is unambiguous.
//
// Matched by argv0 basename. Flags are each agent's documented resume
// interface; entries are easy to add and intended to become user-overridable.
// `{id}` in a template is replaced with the minted uuid (array slot only).
export interface AgentResumeSpec {
  // A command that already names its session is not given a minted id — the
  // agent rejects the combination — and is resumed by what it names:
  // `idFlags` take a session id (`--resume <id>`), `sessionFlags` select one
  // without an id (`--continue`). `idFlag` itself counts as an `idFlags` entry.
  provision?: {
    idFlag: string;
    resumeArgs: string[];
    idFlags?: string[];
    sessionFlags?: string[];
  };
  continue?: { resumeArgs: string[] };
}
// How an agent takes an initial prompt on the command line: as a trailing
// positional (`claude "prompt"`) or behind a flag (`opencode --prompt "…"`,
// `aider -m "…"`). Absent ⇒ the agent has no launch-prompt argument and the
// New Session prompt box is hidden for it.
export type AgentPromptArg =
  | { style: "positional" }
  | { style: "flag"; flag: string };
// How to hand an agent the "drive the Fresh editor from the shell" contract
// when "Teach Fresh CLI" is on: appended to launch argv behind a flag
// (`claude --append-system-prompt "…"`), or — for agents with no such flag
// (codex/opencode) — prepended to the launch prompt. Absent ⇒ the agent has no
// autonomous shell to drive the editor with, so the checkbox stays hidden.
//
// Deliberately not a file: an instruction file written into the workspace
// lands in the user's repo, where it collides with their own and shows up in
// `git status`.
export type AgentSystemPrompt =
  | { via: "flag"; flag: string }
  | { via: "prompt" };
export interface AgentEntry {
  // The command the New Session dropdown fills in and the basename the matcher
  // keys on.
  id: string;
  // Human label for the preset button. Falls back to `id` when omitted.
  label?: string;
  // Resolves a path/args form (e.g. `/usr/bin/claude --foo`) to this entry.
  match: RegExp;
  // Resume strategy across editor restarts (see `resolveAgentLaunch`).
  spec: AgentResumeSpec;
  // Flag(s) enabling the agent's "auto"/bypass-approvals mode. Absent ⇒ the
  // agent has no such flag (opencode gates this via config, not a flag), so the
  // "Auto mode" checkbox is hidden for it.
  auto?: string[];
  // How the agent accepts an initial prompt at launch. Absent ⇒ no prompt box.
  prompt?: AgentPromptArg;
  // How to inject the "drive the Fresh editor" system prompt when the user
  // enables "Teach Fresh CLI". Absent ⇒ the agent has no autonomous shell to
  // drive the editor (aider), so the checkbox stays hidden for it.
  systemPrompt?: AgentSystemPrompt;
}
