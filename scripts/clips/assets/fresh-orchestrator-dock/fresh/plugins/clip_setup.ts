// Stage the orchestrator clip: four worktrees, four agents, one editor.
//
// A clip drives the editor with keystrokes, and cutting four worktrees through
// the New Workspace dialogue is forty of them — forty chances for a capture to
// desync, and none of them the thing being filmed. The scripting API does the
// same work in one call each (`newWorkspace` *is* what the dialogue submits),
// so the setup is a plugin that runs once at startup and the capture opens on a
// workspace that is already four tasks deep.
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

type Task = {
  name: string;
  branch: string;
  agent: string;
  /** Files opened in the right-hand pane. Empty: the agent has the workspace. */
  files: string[];
  /** Dirty the worktree and open Review Diff on the right instead of a file. */
  review?: Array<{ path: string; find: string; replace: string }>;
};

/** One task per worktree: the branch it is cut on, the agent working it, and
 *  what sits beside the agent — which is what the right-hand pane shows,
 *  because a workspace whose tabs are unrelated to its branch reads as a
 *  screenshot. */
const TASKS: Task[] = [
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
    // The one that is waiting on a person: an agent that has stopped on a
    // permission prompt, with the work it has already done open beside it as a
    // diff rather than as a file. That is the pairing the feature is for —
    // read what it did, then answer it.
    name: "rate-limit",
    branch: "feat/rate-limit",
    agent: "tern",
    files: [],
    review: [
      {
        path: "src/ratelimit.rs",
        find: "        bucket.hits += 1;",
        replace: "        bucket.hits = bucket.hits.saturating_add(1);",
      },
      {
        path: "src/routes.rs",
        find: '        .route("/healthz", get(health))',
        replace:
          '        .route("/healthz", get(health))\n' +
          "        .layer(RateLimitLayer::per_minute(60))",
      },
    ],
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
    // terminal; the split adds what to read beside it.
    const ws = await orch.newWorkspace({
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

    if (task.review) {
      // Give the agent some work to have done. `writeFile` rather than a
      // patch: the point on screen is that the review pane has real hunks in
      // it, and the file it edits is the one the branch is named after.
      for (const edit of task.review) {
        const path = `${ws.root}/${edit.path}`;
        const before = editor.readFile(path);
        if (typeof before === "string" && before.includes(edit.find)) {
          editor.writeFile(path, before.replace(edit.find, edit.replace));
        }
      }
      await editor.flush();
    }

    // An agent with nothing beside it keeps the whole workspace.
    if (task.files.length === 0 && !task.review) continue;

    // Otherwise the agent keeps the left, and what it is to be checked
    // against — the file, or the diff of what it changed — opens to its
    // right. That is the arrangement the feature is used in: the agent is the
    // thing you are watching, the code is what you watch it against.
    const right = await editor.splitWindow({
      direction: "vertical",
      place: "after",
      ratio: 0.46,
      // The review pane opens on the file the branch is about, so the split
      // has a buffer of its own; without one it shows the agent's terminal
      // twice, which is the same pane side by side rather than a layout.
      file: task.files[0] ?? task.review?.[0].path,
    });
    await editor.flush();
    // A split inherits the tab list of the pane it was cut from, so the new
    // pane opens carrying a tab for the agent's terminal — which is *in* the
    // pane beside it. Dropping the tabs to the left of what it should show
    // leaves each pane holding what it actually has; the terminal is
    // untouched, being open in the other split.
    editor.closeBuffersToLeftInSplit(right.bufferId, right.splitId);
    await editor.flush();
    // The task's other files, as tabs in the pane just opened — a real task
    // workspace has the files that task touches open, not one of them.
    for (const extra of task.files.slice(1)) {
      editor.openFile(extra);
      await editor.flush();
    }
    if (task.review) {
      // Review Diff opens into the focused pane, which is the one just made.
      // `runCommand` resolves when the command was *dispatched*, not when it
      // is done, and this one shells out to git — so without waiting for the
      // buffer it produces, the setup moves to the next workspace and the
      // review lands wherever the editor happens to be by then.
      await editor.runCommand("Review Diff");
      for (let i = 0; i < 60; i++) {
        await editor.delay(200);
        const here = editor
          .listBuffers()
          .some((b) => (b.name ?? "").includes("Review Diff"));
        if (here) break;
      }
      await editor.flush();
    }
  }

  if (launch) {
    await orch.focusWorkspace(launch);
  }
});

editor.on("ready", "clipSetupOnReady");
