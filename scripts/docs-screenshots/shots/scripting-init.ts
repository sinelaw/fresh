// Copied to ~/.config/fresh/init.ts by projects/fresh/setup.sh.
// "Docs: Script Shell" opens a plain terminal the way Run Agent… does, so it
// carries the grant that lets `fresh --cmd script run` drive this editor.
registerHandler("docs_script_shell", () => {
  const orch = editor.getPluginApi("orchestrator") as any;
  void orch.runAgent({ agent: "", teach: false, wait: false });
});
editor.registerCommand("Docs: Script Shell", "Open a terminal that may script the editor", "docs_script_shell");
