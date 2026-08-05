# Scripting and Agent Control

> **CLI:** `fresh --cmd script run|check|api|types`
>
> **Palette:** `Run Agent…`, `Orchestrator: New Workspace`
>
> **Built-in guide:** `fresh --cmd help script`

Fresh can be driven from the outside while it is running. `fresh --cmd script run` hands a TypeScript script to the editor you are already sitting in, and the editor evaluates it against the same plugin API its own bundled plugins use. The script can open files, split panes, resize them, write a file and show it, create a workspace on a new git worktree, start a coding agent, open a [guided code tour](./code-tours.md) — whatever the API exposes.

The script runs to completion and is then forgotten. Whatever it created stays.

> **Screenshot placeholder** — a terminal pane on the right running `fresh --cmd script run`, and the editor panes on the left rearranging as a result.

## Why this exists

The point of the feature is the coding agent that is already running in one of your panes.

An agent launched into a Fresh workspace is a process in a terminal. It can read and write files and run commands, but it has no idea what the editor around it looks like and no way to change it. That leaves the editor as a dumb frame around the conversation. If the agent can submit a script, it can set up the editor for the task at hand: put the file it is changing next to the test it is fixing, open the log it wants you to read, hand you a walkthrough of the change it just made, or start a second agent in a second workspace and let you get back to what you were doing.

So the goals are:

- **The agent arranges the UI for you.** You ask for a layout, a walkthrough, a background task, in the chat you are already having with the agent. You do not stop to arrange panes by hand.
- **No fixed verb menu.** An earlier version of this channel shipped a list of command ids over the socket. Every new capability then meant a new id, an argument schema, a CLI alias and a doc entry. The script channel carries code instead, so anything the plugin API can already do is reachable on day one.
- **Discovery is a file on disk.** The API surface is TypeScript declarations (`fresh.d.ts`, `plugins.d.ts`) that an agent can read and search, which is what coding agents are good at. There is no API to learn about the API.
- **No install and no restart.** A script is not a plugin. It is submitted, evaluated in the live editor, and gone.

## How a script reaches the editor

Every local terminal Fresh spawns inherits two variables:

| Variable | Meaning |
|---|---|
| `FRESH_SESSION` | Which editor's control socket to talk to. This is what makes a bare `fresh <file>` from inside a pane open in *this* editor instead of starting a second one. |
| `FRESH_BIN` | The exact binary running this editor. Always invoke the CLI through `"$FRESH_BIN"` — a different `fresh` earlier on `PATH` may not have the same verbs. |

Script evaluation needs one more thing: a capability token, `FRESH_CMD_TOKEN`. The Orchestrator mints one when it launches a workspace or runs an agent, bound to that workspace's window. A terminal that has no token gets a refusal:

```
no capability token: script evaluation is not authorized
```

So scripting works from panes the Orchestrator started — **Run Agent…** and **Orchestrator: New Workspace** — and not from an arbitrary shell elsewhere on your machine. Both dialogs mint a token whether you launch an agent or a plain shell, so a terminal you started through **Run Agent…** with the *Terminal* preset can script too.

The token also decides *which* workspace a script acts on. The target window is derived from the token, never passed in, so a script submitted by the agent in workspace A cannot reach into workspace B's panes.

From outside a Fresh pane, `--session NAME` picks the editor to talk to, but the token requirement still applies.

## The verbs

```
fresh --cmd script run   [FILE|-]        evaluate against this workspace (default: stdin)
fresh --cmd script check [FILE|-]        parse + check editor.* names, without running
fresh --cmd script api   <query> [--json]  search the API by name or description
fresh --cmd script types                 paths of the API declaration files
```

Source comes from a file or stdin, never from the argument vector — a script is multi-line and full of quotes, and a shell mangles that.

```sh
# stdin, via a heredoc
"$FRESH_BIN" --cmd script run <<'EOF'
return editor.describeWorkspace();
EOF

# a file
"$FRESH_BIN" --cmd script run layouts/review.ts
```

Whatever the script returns is printed as JSON. A throw exits non-zero with the message on stderr.

`script api` is the fast way in. It matches names first and doc prose second, and prints each hit with its doc comment, which usually answers the question without opening the declaration file:

```sh
"$FRESH_BIN" --cmd script api splitWindow
"$FRESH_BIN" --cmd script api tour --json
```

`script check` catches the two failures worth catching before a script touches a workspace someone is looking at: it does not parse, and it calls an `editor` member that does not exist. A misremembered method name is otherwise indistinguishable from a missing feature until you run it.

`fresh --cmd help script` prints the feature guide, including the API surface count of the build you are running — the numbers are generated from the shipped declarations, not typed into a help string.

A script that creates a workspace waits on `git worktree add` and an agent process starting, so the CLI's wait is generous. `FRESH_CMD_TIMEOUT_MS` overrides it.

## Writing a script

The body runs as the body of an async function with an `editor` global. Top-level `await` works, `return` is the answer.

```ts
// current layout: panes left to right, with geometry, kind and focus
return editor.describeWorkspace();
```

```ts
// README in a new pane to the right, without taking focus from the user
await editor.splitWindow({
  direction: "vertical",
  place: "after",
  file: "README.md",
  keepFocus: true,
});
```

`direction` names the divider: `"vertical"` is side by side, `"horizontal"` is stacked. `place` is `"before"` (left or top) or `"after"` (right or bottom, the default).

Mutations are queued, so a read in the same script sees the state from *before* them. Await the mutation, or `await editor.flush()`, before reading back:

```ts
editor.setSplitRatio(splitId, 0.3);
await editor.flush();
return editor.describeWorkspace();
```

`FRESH_WINDOW_ID` is the window the script's token is bound to. The active window can change under a script across an `await` — the user is still working — so prefer the calls that take an explicit id (`editor.windowPath`, `editor.openFileInBackground`, `editor.openFileInSplit`) over the ones that act on whatever is focused.

## Launching an agent that knows about this

**Run Agent…** and **Orchestrator: New Workspace** are the same dialog; a **Run in** switch picks the current workspace or a new one. Its fields:

- **Agent** — a preset (`claude`, `codex`, `opencode`, `aider`), a plain terminal, or a custom command line you type.
- **Run in** — current workspace, or a new workspace on its own git worktree.
- **Auto mode** — the agent's reduced-approval mode, for agents that have one.
- **Prompt** — an optional first message handed to the agent at launch.
- **Teach agent the Fresh CLI** — on by default. Injects a system prompt that documents the script channel, with worked recipes.

> **Screenshot placeholder** — the Run Agent dialog, agent dropdown open, "Run in: New workspace" selected, "Teach agent the Fresh CLI" checked.

The token is minted either way. **Teach agent the Fresh CLI** only controls whether the agent is *told* the capability is there. How the instruction is delivered depends on the agent:

| Agent | Injection | Notes |
|---|---|---|
| `claude` | `--append-system-prompt` at launch | Nothing on disk is touched. |
| `codex` | a marked block in `AGENTS.md` | Appended, never overwriting your file. |
| `opencode` | a marked block in `AGENTS.md` | Same. |
| `aider` | — | Not offered. aider has no autonomous shell; it only proposes commands for confirmation, so it cannot drive the editor on its own. |

The `AGENTS.md` block is wrapped in `<!-- fresh-cli:start -->` / `<!-- fresh-cli:end -->` markers and is added at most once, so a restart or a retry does not stack copies of it.

## Things to ask the agent for

These are all things you type into the chat you are already having with the agent in the pane. The agent writes the script; you do not.

### A layout for the task

> "Set me up for this: the file you are changing on the left, its test on the right, and a terminal along the bottom running the test in watch mode."

The agent calls `describeWorkspace()` to see what is there, then `splitWindow` and `setSplitRatio` to build the arrangement, and reads the layout back to check it did what it said.

> **Screenshot placeholder** — before and after: a single editor pane, then the three-pane arrangement the agent built.

Layouts are worth asking for whenever the shape of the work changes: a wide diff and a narrow notes pane for review, four small panes for a refactor that touches four files, one big pane and a terminal for debugging.

### A walkthrough of a change

> "Write me a tour of this PR — start at the entry point, then the three files you changed, and say why each change was needed."

The agent writes a `.fresh-tour.json` and opens it in the tour dock. You get a step list on the left, the explanation on the right, and the code opening and highlighting as you step. See [Guided Code Tours](./code-tours.md).

This works the same way for a branch, a subsystem you have never read, or the code path behind a bug report.

> **Screenshot placeholder** — the tour dock open below a source file, with the step's lines highlighted.

### A second agent, working in the background

> "Start another agent in a new workspace off `main` and have it fix the flaky test in `queue_test.rs`. Leave it running, I want to keep working here."

The agent calls the Orchestrator's API:

```ts
const orch = editor.getPluginApi("orchestrator");
return await orch.newWorkspace({
  agent: "claude",
  newBranch: "fix/flaky-queue",
  prompt: "fix the flaky test in queue_test.rs",
});
```

That creates a git worktree, launches the agent in it, and answers with `{ workspaceId, windowId, root }`. `visit` defaults to false, so your focus stays where it is — a background task should not yank you out of what you are doing.

The new workspace appears on the Orchestrator dock. Switch to it when you want to see how it went, or ask your own agent:

```ts
return editor.getPluginApi("orchestrator").listWorkspaces();
```

which reports each workspace's branch, git state and whether its agent is `working` or `idle`.

> **Screenshot placeholder** — the Orchestrator dock with three workspaces, one marked as working.

## Uses that have nothing to do with agents

The channel is a general remote control for a running editor. Anything that can run a shell command in a workspace pane can use it.

- **Committed layout scripts.** Keep `layouts/review.ts` and `layouts/debug.ts` in the repo and run them when you switch tasks. Everyone on the team gets the same arrangement, and the layout is reviewable like any other file.
- **Build and test wrappers.** Have the wrapper open the report when it finishes — `editor.splitWindow({ file: "/tmp/report.md" })` — or jump straight to the first failure with `editor.openFileInSplit(splitId, file, line)`. Writing a file and opening it beats printing to a terminal: you get syntax highlighting, search and save.
- **Post-command cleanup.** After a rebase or a merge, open every conflicted file in its own pane.
- **One-off work that does not deserve a plugin.** If you would have written a twenty-line plugin, installed it and restarted, write the twenty lines and pipe them in instead.
- **Shell aliases and Makefile targets.** `make review` can leave the editor in exactly the state the review needs.

## Limits

- **A runaway script wedges the plugin thread.** There is no timeout or memory cap, the same blast radius as a buggy `init.ts`.
- **`fresh <file>` blocks.** The nested-forward path opens the file and waits until you close the buffer — the `$EDITOR` contract. From a script, use `editor.openFileInBackground` instead.
- **Remote panes have no handle.** `FRESH_SESSION` is only set for local shells, so an agent in a docker, SSH or Kubernetes pane cannot reach the parent editor.
- **The grant is all or nothing.** Holding a token means "may do anything the user could do from a plugin". What bounds an agent is the window its token is bound to, not a list of allowed verbs. A narrower-looking grant would describe a boundary that is not there.
- **Script access is not Workspace Trust.** They answer different questions: trust asks whether this repo is safe to load, the token asks whether this agent may drive the editor. See [Workspace Trust](./workspace-trust.md).

## See also

- [Guided Code Tours](./code-tours.md) — the walkthrough format an agent can generate for you
- [Integrated Terminal](./terminal.md) — the panes agents run in
- [Startup Script (`init.ts`)](../configuration/init.md) — the same API, run at startup
- [Plugin API Reference](../plugins/api/) — the full surface a script can call
- [Workspace Trust](./workspace-trust.md)
