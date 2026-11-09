// Workboard scenario: a dozen workspaces of different kinds. Run
// "Scenario: Build" from the palette; "Scenario: Dump" writes the
// Orchestrator's view to dump.json next to this file's REPOS dir.
const R = "__REPOS__";
const sh = (s: string) => `bash -c '${s}'`;
const ask = (q: string, delay = 4) => sh(`sleep ${delay}; echo Thinking...; echo ${q}; sleep 100000`);
const loop = (what: string, every = 0.6) => sh(`while true; do echo ${what} $RANDOM; sleep ${every}; done`);

async function build(): Promise<void> {
  const orch = editor.getPluginApi("orchestrator") as any;
  const log: string[] = [];
  const mk = async (opts: Record<string, unknown>) => {
    try {
      const ids = await orch.newWorkspace({ teach: false, wait: false, ...opts });
      log.push(`ok ${opts.name}: ${ids.workspaceId}`);
      return ids;
    } catch (e) {
      log.push(`FAIL ${opts.name}: ${e}`);
      return null;
    }
  };
  // api: one asking, one working, one clean idle, one with an unpushed commit
  const rate = await mk({ path: `${R}/api`, name: "rate-limit", agent: ask("Should I use Postgres or Redis? [y/n]") });
  const bill = await mk({ path: `${R}/api`, name: "billing-ui", agent: loop("editing Bill.tsx") });
  const spike = await mk({ path: `${R}/api`, name: "old-spike" });
  const unpushed = await mk({
    path: `${R}/api`, name: "unpushed",
    agent: sh("sleep 2; echo fix >> README.md; git -c user.email=a@b -c user.name=a commit -qam wip; echo committed; sleep 100000"),
  });
  // web: asking, finished with changes, working, crashed, a very long unicode name
  const login = await mk({ path: `${R}/web`, name: "login-fix", agent: ask("Allow running cargo test? (y/n)", 5) });
  const tabs = await mk({
    path: `${R}/web`, name: "tab-names",
    agent: sh("for i in 1 2 3; do echo step $i; sleep 1; done; echo x >> README.md; echo Done. All 42 tests pass.; sleep 100000"),
  });
  const lsp = await mk({ path: `${R}/web`, name: "lsp-cache", agent: loop("compiling", 0.8) });
  const crash = await mk({ path: `${R}/web`, name: "crashed", agent: sh("echo boom; exit 3") });
  const longName = await mk({
    path: `${R}/web`, name: "très-long-nom-名前-refactor-the-whole-session-restore-pipeline", agent: loop("refactoring", 1.5),
  });
  // docs: files open, a split and extra terminals; plus a plain shell
  const docs = await mk({ path: `${R}/docs`, name: "docs-site", agent: "bash" });
  // a non-git folder
  const notes = await mk({ path: `${R}/notes`, name: "notes", worktree: false });
  // a remote that cannot connect
  void mk({ backend: "ssh", host: "nobody@unreachable.invalid", name: "gpu-box", agent: "claude" });

  // Fill docs-site with buffers and terminals, then come back.
  const home = editor.activeWindow();
  if (docs) {
    editor.setActiveWindow(docs.windowId);
    await editor.delay(300);
    editor.openFile(`${docs.root}/README.md`);
    editor.openFile(`${docs.root}/src/lib.rs`);
    await editor.createTerminal({ cwd: docs.root, focus: false });
    await editor.createTerminal({ cwd: docs.root, direction: "horizontal", focus: false });
    await editor.delay(300);
    editor.setActiveWindow(home);
  }

  // Folders: nested, an empty one, and some workspaces left unfiled.
  const backend = orch.createFolder("Backend");
  const payments = orch.createFolder("Payments", backend);
  const frontend = orch.createFolder("Frontend");
  orch.createFolder("Someday");
  for (const [ws, f] of [[rate, backend], [bill, payments], [unpushed, payments], [login, frontend], [tabs, frontend], [lsp, frontend]] as const) {
    if (ws) orch.moveWorkspace(ws.workspaceId, f);
  }
  void [spike, crash, longName, notes];
  editor.writeFile(`${R}/../build.log`, log.join("\n") + "\n");
  editor.setStatus(`scenario: ${log.filter((l) => l.startsWith("ok")).length} workspaces`);
}

registerHandler("scenario_build", () => void build().catch((e) => editor.setStatus(`scenario failed: ${e}`)));
editor.registerCommand("Scenario: Build", "Create the Workboard demo workspaces", "scenario_build");
// Change things under an open board: a rename, an archive, a delete, a new
// workspace, and an idle agent that starts asking. Run it, then open the
// Workboard within a couple of seconds and watch.
async function churn(): Promise<void> {
  const orch = editor.getPluginApi("orchestrator") as any;
  const byName = (n: string) => orch.listWorkspaces().find((w: any) => w.name.startsWith(n));
  await editor.delay(3000);
  const log: string[] = [];
  const step = async (what: string, f: () => unknown) => {
    try { await f(); log.push("ok " + what); } catch (e) { log.push("FAIL " + what + ": " + e); }
    await editor.delay(1500);
  };
  await step("rename lsp-cache", () => orch.renameWorkspace(byName("lsp-cache").workspaceId, "lsp-cache-v2"));
  await step("archive old-spike", () => orch.archiveWorkspace(byName("old-spike").workspaceId));
  await step("delete crashed", () => orch.deleteWorkspace(byName("crashed").workspaceId));
  await step("new hotfix", () => orch.newWorkspace({ path: R + "/web", name: "hotfix", teach: false, wait: false,
    agent: "bash -c 'sleep 2; echo Deploy to production now? [y/n]; sleep 100000'" }));
  editor.writeFile(R + "/../churn.log", log.join("\n") + "\n");
}
registerHandler("scenario_churn", () => void churn());
editor.registerCommand("Scenario: Churn", "Rename, archive, delete and create under an open board", "scenario_churn");
registerHandler("scenario_dump", () => {
  const orch = editor.getPluginApi("orchestrator") as any;
  const ws = orch.listWorkspaces();
  editor.writeFile(`${R}/../dump.json`, JSON.stringify({
    ws,
    ex: ws.map((w: any) => { try { return orch.getWorkspace(w.workspaceId || w.windowId)?.explain ?? null; } catch (e) { return String(e); } }),
    folders: orch.listFolders(),
    windows: editor.listWindows(),
  }, null, 1));
});
editor.registerCommand("Scenario: Dump", "Dump the Orchestrator's view", "scenario_dump");
