// Copied to ~/.config/fresh/init.ts by projects/fresh/setup.sh.
// "Docs: Script Shell" opens a plain terminal the way Run Agent… does, so it
// carries the grant that lets `fresh --cmd script run` drive this editor.
registerHandler("docs_script_shell", () => {
  const orch = editor.getPluginApi("orchestrator") as any;
  void orch.runAgent({ agent: "", teach: false, wait: false });
});
editor.registerCommand("Docs: Script Shell", "Open a terminal that may script the editor", "docs_script_shell");

// "Docs: Agent Workspaces": the dock the Scripting page shows after "start
// another agent in a new workspace" — two agents at work, a plain shell, and
// this checkout. claude and codex are the fake agents on PATH.
registerHandler("docs_agent_workspaces", () => void (async () => {
  const orch = editor.getPluginApi("orchestrator") as any;
  const R = editor.getCwd();
  const mk = (o: Record<string, unknown>) => orch.newWorkspace({ path: R, teach: false, wait: false, ...o });
  await mk({ name: "fix-flaky-drain", newBranch: "fix/flaky-drain", agent: "claude" });
  await mk({ name: "flaky-repro", newBranch: "flaky-repro", agent: "codex" });
  await mk({ name: "review", newBranch: "review", agent: "" });
})());
editor.registerCommand("Docs: Agent Workspaces", "Stage the Scripting page's dock", "docs_agent_workspaces");
