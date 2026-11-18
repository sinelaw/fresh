// Copied to ~/.config/fresh/init.ts by projects/inventory-api/setup.sh.
// "Docs: Build Workspaces" stages the Orchestrator workspaces the dock and
// Workboard screenshots show. The agents are fake: small scripts on PATH
// named migrate, bench and run-tests (written by shots/orchestrator.py), so
// the dock labels them the way it labels a real agent — by its command.
const R = editor.getCwd(); // the project Fresh was started in

async function build(): Promise<void> {
  const orch = editor.getPluginApi("orchestrator") as any;
  const log: string[] = [];
  const mk = async (opts: Record<string, unknown>) => {
    try {
      const ids = await orch.newWorkspace({ path: R, teach: false, wait: false, ...opts });
      log.push(`ok ${opts.name}`);
      return ids;
    } catch (e) {
      log.push(`FAIL ${opts.name}: ${e}`);
      return null;
    }
  };
  await mk({ name: "low-stock-alerts", newBranch: "feat/low-stock-alerts", agent: "migrate" });
  await mk({ name: "query-perf", newBranch: "perf/item-queries", agent: "bench" });
  await mk({ name: "readme-endpoints", newBranch: "docs/readme-endpoints", agent: "bash" });
  const csv = await mk({ name: "csv-export", newBranch: "feat/csv-export", agent: "run-tests" });
  const adj = await mk({ name: "adjust-validation", newBranch: "fix/adjust-validation", agent: "run-tests" });
  const review = orch.createFolder("To review");
  for (const ws of [csv, adj]) if (ws) orch.moveWorkspace(ws.workspaceId, review);
  editor.setStatus(`docs: ${log.filter((l) => l.startsWith("ok")).length} workspaces`);
  editor.writeFile(editor.getHomeDir() + "/docs-build.log", log.join("\n") + "\n");
}

registerHandler("docs_build_workspaces", () => void build().catch((e) => editor.setStatus(`docs build failed: ${e}`)));
editor.registerCommand("Docs: Build Workspaces", "Stage the Orchestrator screenshot workspaces", "docs_build_workspaces");
