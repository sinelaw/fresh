// The coding agents the orchestrator knows how to start: the editor's agent
// registry (crates/fresh-editor/plugins/lib/agent_*.ts) as data. An agent
// session is a terminal running the agent's command line in the
// workspace's root, in the user's login shell on its host.
//
//   prompt  how the agent takes a first message at launch: a trailing
//           argument ("positional") or behind a flag; none: no prompt box.
//   auto    the flags of its own "auto" mode (fewer approval prompts);
//           none: it has no such flag.
//   resume  the arguments that take up its latest conversation in the
//           folder again ("continue"): run when the server starts again.

export const AGENTS = [
  { id: "claude", label: "Claude Code", command: "claude", prompt: { style: "positional", flag: "" }, auto: ["--permission-mode", "auto"], resume: ["--continue"] },
  {
    id: "codex", label: "Codex", command: "codex", prompt: { style: "positional", flag: "" }, resume: ["resume", "--last"],
    auto: ["--sandbox", "workspace-write", "--ask-for-approval", "on-request", "-c", "approvals_reviewer=\"auto_review\""],
  },
  { id: "opencode", label: "OpenCode", command: "opencode", prompt: { style: "flag", flag: "--prompt" }, auto: [], resume: ["--continue"] },
  { id: "aider", label: "Aider", command: "aider", prompt: { style: "flag", flag: "-m" }, auto: ["--yes-always"], resume: ["--restore-chat-history"] },
];

// The choices of the agent picker: the registry, a plain terminal, or a
// command of one's own.
export const AGENT_CHOICES = [
  ...AGENTS.map((a) => ({ value: a.id, label: a.label })),
  { value: "terminal", label: "Terminal (no agent)" },
  { value: "custom", label: "Custom command…" },
];

/** function agentById(id: String) => Object */
export function agentById(id) {
  return AGENTS.find((a) => a.id === id) || null;
}

// A word for a POSIX shell, quoted when it needs to be.
/** function shQuote(s: String) => String */
export function shQuote(s) {
  return /^[A-Za-z0-9_\-./=:@%+,]+$/.test(s) ? s : `'${s.replace(/'/g, "'\\''")}'`;
}

// The command line that starts `agent` with its auto mode (`auto`) and a
// first message (`prompt`, when the agent takes one).
/** function agentCommand(agent: Object, auto: Boolean, prompt: String) => String */
export function agentCommand(agent, auto, prompt) {
  const words = [agent.command];
  if (auto) for (const f of agent.auto) words.push(f);
  const text = prompt.trim();
  if (text && agent.prompt.style === "flag") words.push(agent.prompt.flag, text);
  if (text && agent.prompt.style === "positional") words.push(text);
  return words.map(shQuote).join(" ");
}

// The command line that takes `agent` up again in its folder, with its
// auto mode.
/** function agentResume(agent: Object, auto: Boolean) => String */
export function agentResume(agent, auto) {
  const words = [agent.command];
  for (const a of agent.resume) words.push(a);
  if (auto) for (const f of agent.auto) words.push(f);
  return words.map(shQuote).join(" ");
}
