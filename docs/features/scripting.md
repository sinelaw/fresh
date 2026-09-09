# Scripting the Editor

> **CLI:** `fresh --cmd script`, and `fresh --cmd help script` for the built-in guide

A running Fresh can be driven from the outside. Hand it a short TypeScript program and it will do anything the editor's plugin API can do — open files, arrange panes, resize them, show output, create a workspace, start a coding agent, open a [guided tour](./code-tours.md). The program runs, does its work, and is forgotten. Whatever it created stays.

Short ones go on standard input, from any terminal inside the editor. Longer ones live in a file. What the program returns is printed as JSON, so a script is also a way to ask the editor a question.

![Two one-liners run from a pane inside the editor, with their answers](/images/scripting-cmd-run.png)

*Two one-liners and their answers, run from a pane in the editor they are driving.*

## Examples

Every example below is a real command; the outputs are what they actually printed.

### What is the editor showing?

```sh
echo 'return editor.describeWorkspace().panes.map(p => p.name)' | fresh --cmd script run
```

```json
["command_access.rs","terminal_restore_script_token.rs","terminal"]
```

The same call reports each pane's id, kind, geometry and which one has focus — it is how a script orients itself before it changes anything.

### Open a file beside what you are reading

```sh
echo 'return editor.splitWindow({ file: "README.md", place: "before", keepFocus: true })' | fresh --cmd script run
```

```json
{"splitId":3,"sourceSplitId":0,"bufferId":8,"x":0,"y":1,"width":85,"height":42}
```

`place` picks the side, and `keepFocus` leaves your cursor where it was instead of pulling it into the new pane. The answer comes back after the layout has actually been applied, with the new pane's id — so "did it land on the left" is something you read, not something you assume.

### Jump to the line something failed on

```sh
echo 'const p = editor.describeWorkspace().panes[0]; return editor.openFileInSplit(p.splitId, "src/main.rs", 3502)' | fresh --cmd script run
```

Worth wiring into a test or build wrapper: when it fails, put the failure on screen in the pane you are already looking at.

### Show output as a buffer, not as scrollback

```sh
echo 'editor.writeFile(editor.localPath("/tmp/summary.md"), "# Test run\n\n- 42 passed\n- 1 failed\n"); return editor.splitWindow({ direction: "horizontal", file: "/tmp/summary.md", keepFocus: true })' | fresh --cmd script run
```

Write the file, open the file. You get syntax highlighting, search, and a buffer that stays put — none of which a wall of terminal output gives you.

### A task layout you can re-run

Longer than a line, so it goes in a file:

```ts
// review.ts — the file under review on the left, its test beside it,
// this terminal along the bottom. Re-runnable: it tidies up first.
const shell = editor.describeWorkspace().panes.find((p) => p.kind === "terminal");
for (const pane of editor.describeWorkspace().panes) {
  if (pane.splitId !== shell.splitId) editor.closeSplit(pane.splitId);
}
await editor.flush();

await editor.splitWindow({
  direction: "horizontal",
  place: "before",
  ratio: 0.7,
  file: "crates/fresh-editor/src/server/command_access.rs",
});
await editor.splitWindow({
  direction: "vertical",
  place: "after",
  file: "crates/fresh-editor/tests/terminal_restore_script_token.rs",
});

// Hand the keyboard back to the pane you are typing in.
editor.focusSplit(shell.splitId);
await editor.flush();
return editor.describeWorkspace().panes.map((p) => p.name);
```

```sh
fresh --cmd script run review.ts
```

![The layout the script built: source and test side by side, terminal below](/images/scripting-layout-after.png)

Two things in that script are worth copying. Layout changes are queued, so a read straight after one still sees the old state — `await` the change, or `flush()`, before reading back. And a script that rearranges panes should put the keyboard back where it found it.

Keep a couple of these in the repo and you have named layouts everyone on the team shares: one for review, one for debugging, one for the refactor that touches four files.

### Hand someone a walkthrough

A script can write a [guided tour](./code-tours.md) and open it in one go — the file, the steps, and the panel, without you touching an editor:

```sh
echo 'return editor.getPluginApi("code-tour").openTour(".fresh-tour.json")' | fresh --cmd script run
```

![A tour written and opened by a script, with the step's lines highlighted above](/images/scripting-tour-handoff.png)

### Start a workspace on a branch

```sh
echo 'return editor.getPluginApi("orchestrator").newWorkspace({ agent: "claude", newBranch: "fix/flaky" })' | fresh --cmd script run
```

```json
{"workspaceId":"ws-18c8fc1278905b84-6","windowId":7,"root":"…/orchestrator/fresh-2"}
```

That is a git worktree, a window, and an agent running in it — one line, and it resolves once the workspace is actually up.

### Tidy up after yourself

Creating a workspace is the interesting half; what you do with it afterwards is the other. The dock's own menus are published too, so a script can name a workspace, file it, and eventually clear it away:

```sh
echo '
  const orch = editor.getPluginApi("orchestrator");
  const reviews = orch.listFolders().find(f => f.name === "Reviews")?.folderId
    ?? orch.createFolder("Reviews");
  const ws = orch.listWorkspaces().find(w => w.active);
  orch.renameWorkspace(ws.workspaceId, "flaky-test #4192");
  orch.moveWorkspace(ws.workspaceId, reviews);
' | fresh --cmd script run
```

`renameWorkspace`, `moveWorkspace`, the four folder verbs (`listFolders`, `createFolder`, `renameFolder`, `deleteFolder`), the lifecycle three (`stopWorkspace`, `archiveWorkspace`, `deleteWorkspace`) and the dock's own view controls (`setDockView`, `setDockFilter`) are all the headless twins of controls on the dock. Not lookalikes — the menu item and the script call run the same function, so a rule about what may be archived, or a fix to how a folder is deleted, lands in both at once. A verb returns `false` when the thing it was pointed at does not exist, and throws when it exists but the operation was refused, so an agent that finished with a workspace can archive it and know whether it worked.

### Bring one back

Archiving a workspace does not destroy it: the worktree moves to a graveyard and the entry is recorded, so it can come back later — on this machine or another one.

```sh
echo '
  const orch = editor.getPluginApi("orchestrator");
  return orch.listArchived().map(a => `${a.name}  ${a.branch}  ${a.archivedAt}`);
' | fresh --cmd script run
```

`unarchiveWorkspace(name)` puts one back where it came from. It returns as an unopened worktree on the dock rather than a running session — the inverse of Archive, not "restore and launch" — so follow it with `focusWorkspace` when you want it open. It refuses rather than overwrites if something has since taken the path it wants back.

### Start a workspace on another machine

`newWorkspace` takes the same backend switch the dialog does:

```sh
echo '
  return editor.getPluginApi("orchestrator").newWorkspace({
    backend: "ssh",
    host: "build@ci-01:2222",
    path: "/srv/checkout",
    identity: "~/.ssh/ci",
    sshOptions: "-J bastion",
    agent: "claude",
  })
' | fresh --cmd script run
```

Every field mirrors the dialog's SSH form. Kubernetes is the one backend still dialog-only.

## Finding your way around

You do not have to memorise any of this. The CLI searches the editor's API by name or description, checks a program for unknown calls before it touches a workspace someone is looking at, and prints where the full TypeScript declarations live. `fresh --cmd help script` is the guide, and it reports the API surface of the build you are actually running.

## Where scripts can run

Driving the editor is a capability, and a workspace hands it to the terminals it starts — through **Run Agent…** or **Orchestrator: New Workspace**, whether you start an agent or a plain shell. That is why the examples above are run from a pane inside the editor. A shell somewhere else on your machine cannot drive it.

The capability is bound to the workspace that granted it, so a script can rearrange its own workspace and cannot reach into a sibling's panes. Within that workspace it is broad: a program can reach the whole plugin API, which is the honest summary of what you are handing over.

## Agents

This is the reason the channel exists.

An agent launched into a Fresh workspace is a process in a terminal. It can read files and run commands, but on its own it has no idea what the editor around it looks like and no way to change it — which leaves the editor as a dumb frame around the conversation. Given the same channel as above, it can set the editor up for the work in front of you, and you ask for it in the conversation you are already having.

**Run Agent…** and **Orchestrator: New Workspace** are the same dialog, with a switch for whether the agent starts in this workspace or a new one of its own. It offers the agents Fresh knows about — `claude`, `codex`, `opencode`, `aider` — or a plain terminal, or any command you type. You can hand the agent a first message, choose its reduced-approval mode where it has one, and decide whether to teach it that it can drive the editor at all.

![The New Workspace dialog with an agent selected and the advanced options expanded](/images/scripting-run-agent-dialog.png)

Teaching is on by default and is a single toggle. It gives the agent a short briefing on what it can ask the editor for, with worked examples; without it the agent runs as it normally would and never uses the capability. `claude`, `codex` and `opencode` can all be briefed. `aider` cannot drive the editor at all — it has no autonomous shell, it only proposes commands for you to confirm — so the option is not offered for it.

Things worth asking for:

> "Set me up for this: the file you are changing on the left, its test on the right, and a terminal along the bottom."

> "Write me a tour of this PR — start at the entry point, then the files you changed, and say why each change was needed."

> "Start another agent in a new workspace off main and have it fix the flaky test. Leave it running, I want to keep working here."

The last one does not take your focus — a background task should not pull you out of what you are doing. The new workspace appears on the Orchestrator dock, where you can see whose agent is busy and switch to it when you want to catch up.

![The Orchestrator dock listing four workspaces, two of them running agents](/images/scripting-orchestrator-dock.png)

You are letting something change a workspace you are looking at. Agents are told to prefer reversible changes and to leave panes they were not asked to touch alone, but you are handing over the controls, and it is your call per launch whether to do that.

## Worth knowing

- A program that never finishes will hang the editor's plugin work, the same way a broken plugin would.
- Agents in remote panes — a container, an SSH host, a Kubernetes pod — have no route back to the editor that started them.
- This is a different question from [Workspace Trust](./workspace-trust.md). Trust asks whether this repository is safe to load; this asks whether something may drive the editor.

## See also

- [Guided Code Tours](./code-tours.md) — the walkthroughs a script or an agent can generate for you
- [Integrated Terminal](./terminal.md) — the panes scripts and agents run in
- [Startup Script (`init.ts`)](../configuration/init.md) — the same API, run when the editor starts
- [Plugin API Reference](../plugins/api/) — the full surface
- [Workspace Trust](./workspace-trust.md)
