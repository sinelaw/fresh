// Stage the orchestrator clip: three worktrees, three agents, one editor.
//
// A clip drives the editor with keystrokes, and cutting three worktrees
// through the New Workspace dialogue is thirty of them — thirty chances for a
// capture to desync, and none of them the thing being filmed. The scripting
// API does the same work in one call each (`newWorkspace` *is* what the
// dialogue submits), so the setup is a plugin that runs once at startup and
// the capture opens on a workspace that is already three tasks deep.
//
// It is a clip asset, not a feature: it lives in the spec's config directory
// and is copied into the capture's scratch config, so nothing here reaches a
// real install.

type Orchestrator = {
  newWorkspace(options: Record<string, unknown>): Promise<{
    workspaceId: string;
    windowId: number;
    root: string;
  }>;
  listWorkspaces(): Array<{ workspaceId: string; windowId: number; active?: boolean }>;
  focusWorkspace(target: string | number): Promise<boolean>;
};

/** One task per worktree: the branch it is cut on, the agent working it, and
 *  the files that task touches — which are what the code pane shows, because a
 *  workspace whose tabs are unrelated to its branch reads as a screenshot. */
const TASKS = [
  {
    name: "auth-bypass",
    branch: "fix/auth-bypass",
    agent: "quill",
    files: ["src/auth.rs", "src/session.rs"],
  },
  {
    name: "db-pool",
    branch: "perf/db-pool",
    agent: "marlin",
    files: ["src/db/pool.rs", "src/routes.rs"],
  },
  {
    name: "rate-limit",
    branch: "feat/rate-limit",
    agent: "tern",
    files: ["src/ratelimit.rs", "tests/api.rs"],
  },
  {
    name: "docs-refresh",
    branch: "docs/api-reference",
    agent: "scout",
    // No files, so no split: this workspace is the agent and nothing else,
    // which is both a real way to run one and the only way to film an agent
    // filling a workspace without hopping focus between panes to maximise it.
    files: [],
  },
];

registerHandler("clipSetupOnReady", async () => {
  const orch = editor.getPluginApi("orchestrator") as Orchestrator | null;
  if (!orch) {
    editor.error("clip_setup: the orchestrator plugin is not loaded");
    return;
  }

  // The workspace the editor started in — `main`, the checkout you review
  // from. Recorded before anything else exists so the setup can come back to
  // it: the clip opens on the dock with every task already running, not on
  // the last workspace it happened to create.
  const launch = orch.listWorkspaces().find((w) => w.active)?.workspaceId;

  editor.openFile("src/main.rs");
  await editor.flush();

  for (const task of TASKS) {
    // `visit: true` puts focus in the new workspace, which is what makes the
    // `splitWindow` below land in it. The window it creates holds the agent's
    // terminal; the split adds the code beside it.
    await orch.newWorkspace({
      name: task.name,
      newBranch: task.branch,
      agent: task.agent,
      visit: true,
      // The agent is a fake with no editor-driving to teach it, and the
      // system prompt would be a second bare argument it would take for its
      // seed.
      teach: false,
    });
    await editor.flush();

    if (task.files.length === 0) continue;

    // Code to the left of the agent, in the proportion the feature is usually
    // used in: the transcript is a column you glance at, the code is what you
    // are reading.
    const code = await editor.splitWindow({
      direction: "vertical",
      place: "before",
      ratio: 0.58,
      file: task.files[0],
    });
    await editor.flush();
    // A split inherits the tab list of the pane it was cut from, so the code
    // pane opens carrying a tab for the agent's terminal — which is *in* the
    // pane beside it. Dropping the tabs to the left of the file leaves each
    // pane showing what it actually holds; the terminal is untouched, being
    // open in the other split.
    editor.closeBuffersToLeftInSplit(code.bufferId, code.splitId);
    await editor.flush();
    // The task's second file, as a tab in the pane just opened — a real task
    // workspace has the files that task touches open, not one of them.
    for (const extra of task.files.slice(1)) {
      editor.openFile(extra);
      await editor.flush();
    }
  }

  if (launch) {
    await orch.focusWorkspace(launch);
  }
});

editor.on("ready", "clipSetupOnReady");
