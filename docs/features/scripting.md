# Scripting and Agent Control

> **Palette:** `Run Agent…`, `Orchestrator: New Workspace`
>
> **CLI:** `fresh --cmd script`, and `fresh --cmd help script` for the built-in guide

Fresh can be driven from the outside while it is running. A short TypeScript program, handed to the editor you are already sitting in, can open files, arrange panes, resize them, show output, create a workspace, start a coding agent, open a [guided tour](./code-tours.md) — anything the editor's plugin API can do. The program runs, does its work, and is forgotten. Whatever it created stays.

![Searching the editor's API from a workspace terminal](/images/scripting-cmd-run.png)

*The command line runs inside the editor it drives — searching the API from the terminal pane at the bottom, with the panes it can act on above.*

## Why this exists

The point of the feature is the coding agent already running in one of your panes.

An agent launched into a Fresh workspace is a process in a terminal. It can read and write files and run commands, but it has no idea what the editor around it looks like and no way to change it, which leaves the editor as a dumb frame around the conversation. Give it a way in, and it can set the editor up for the work: put the file it is changing next to the test it is fixing, show you the output it wants you to read, hand you a walkthrough of the change it just made, or start a second agent in a second workspace so you can carry on with what you were doing.

So the goals are:

- **You ask in the conversation you are already having.** No stopping to arrange panes by hand.
- **Everything the editor can do is reachable.** The channel carries a program rather than a fixed menu of commands, so a capability does not have to be exposed one verb at a time before an agent can use it.
- **The agent can find out what it may ask for.** The editor writes its API out as TypeScript declarations, and there is a command-line search over them — reading declarations and writing code against them is what coding agents are good at.
- **Nothing to install and nothing to restart.** This is not a plugin. It is submitted, it runs in the live editor, and it is gone.

## What to ask the agent for

These are things you type to the agent in its pane. The agent writes the program; you do not.

### A layout for the task

> "Set me up for this: the file you are changing on the left, its test on the right, and a terminal along the bottom."

The agent looks at the current layout, builds the arrangement, and reads it back to check it did what it said.

Before — the workspace as the agent found it, one file and the terminal it is running in:

![One editor pane above a terminal pane](/images/scripting-layout-before.png)

After:

![Three panes: source left, test right, terminal along the bottom](/images/scripting-layout-after.png)

Layouts are worth asking for whenever the shape of the work changes: a wide diff and a narrow notes pane for review, four small panes for a refactor that touches four files, one big pane and a terminal for debugging.

### A walkthrough of a change

> "Write me a tour of this PR — start at the entry point, then the files you changed, and say why each change was needed."

You get a step list, an explanation beside it, and the code opening and highlighting as you move through it. The same request works for a branch, a subsystem you have never read, or the code path behind a bug report. See [Guided Code Tours](./code-tours.md).

![A tour written and opened by a script, with the step's lines highlighted above](/images/scripting-tour-handoff.png)

*This tour was written and opened by the agent in one go — no file to author, nothing to open by hand.*

### A second agent, working in the background

> "Start another agent in a new workspace off main and have it fix the flaky test. Leave it running, I want to keep working here."

The new workspace gets its own git worktree and its own agent, and it does not take your focus — a background task should not pull you out of what you are doing. It appears on the Orchestrator dock, where you can see whose agent is busy and switch to it when you want to catch up. You can also just ask your own agent how the other one is getting on.

![The Orchestrator dock listing four workspaces, two of them running agents](/images/scripting-orchestrator-dock.png)

*Each row is a workspace: its name, the agent in it, its branch and its git state, with a marker on the one whose agent is producing output right now.*

## Starting an agent that can do this

**Run Agent…** and **Orchestrator: New Workspace** are the same dialog, with a switch for whether the agent starts in this workspace or a new one of its own. It offers the agents Fresh knows about — `claude`, `codex`, `opencode`, `aider` — or a plain terminal, or any command you type. You can hand the agent a first message, choose its reduced-approval mode where it has one, and decide whether to teach it that it can drive the editor at all.

![The New Workspace dialog with an agent selected and the advanced options expanded](/images/scripting-run-agent-dialog.png)

Teaching is on by default and is a single toggle. What it does is give the agent a short briefing on what it can ask the editor for, with worked examples; without it, the agent runs as it normally would and simply never uses the capability. `claude`, `codex` and `opencode` can all be briefed this way. `aider` cannot drive the editor at all — it has no autonomous shell of its own, it only proposes commands for you to confirm — so the option is not offered for it.

## Driving the editor yourself

The same channel is a general remote control, and nothing about it is agent-specific. From a workspace terminal:

```sh
echo 'return editor.splitWindow({ file: "README.md" })' | fresh --cmd script run
```

That opens `README.md` in a new pane beside the current one. The editor's own launcher is reachable the same way, so a single line can also start a workspace on a new branch with an agent in it:

```sh
echo 'return editor.getPluginApi("orchestrator").newWorkspace({ agent: "claude", newBranch: "fix/flaky" })' | fresh --cmd script run
```

Longer programs come from a file. Alongside running one, the CLI will search the API by name or description, check a program for typos before it touches a workspace someone is looking at, and print the built-in guide — run `fresh --cmd help script` and try it.

Some things this is good for that have nothing to do with agents:

- **Layouts you keep.** Check a couple of arrangements into the repo and switch between them when you switch tasks. Everyone on the team gets the same layout, and it is reviewable like any other file.
- **Build and test wrappers.** Have the wrapper open the report when it finishes, or jump straight to the first failure. Writing the output to a file and opening it beats printing it: you get syntax highlighting, search and save.
- **Post-command cleanup.** After a rebase or a merge, open every conflicted file in its own pane.
- **One-off work that does not deserve a plugin.** If you would have written a twenty-line plugin, installed it and restarted, write the twenty lines and pipe them in instead.

## What it can and cannot do

Driving the editor is a capability a workspace grants when it starts an agent, bound to that workspace. An agent can rearrange the workspace it lives in and cannot reach into a sibling workspace's panes. A shell somewhere else on your machine cannot drive the editor at all.

Within its own workspace the grant is broad: holding it means being able to do what you could do from a plugin. That is deliberate — a program can reach the whole API, so a permission that looked narrower would be describing a boundary that is not there. What bounds an agent is the workspace it was given, and the fact that you decide, per launch, whether to hand it out.

Worth knowing:

- A program that never finishes will hang the editor's plugin work, the same way a broken plugin would.
- Agents in remote panes — a container, an SSH host, a Kubernetes pod — have no route back to the editor that started them.
- This is a different question from [Workspace Trust](./workspace-trust.md). Trust asks whether this repository is safe to load; this asks whether an agent may drive the editor.
- You are letting something change a workspace you are looking at. Agents are told to prefer reversible changes and to leave panes they were not asked to touch alone, but the honest summary is that you are handing over the controls.

## See also

- [Guided Code Tours](./code-tours.md) — the walkthroughs an agent can generate for you
- [Integrated Terminal](./terminal.md) — the panes agents run in
- [Startup Script (`init.ts`)](../configuration/init.md) — the same API, run when the editor starts
- [Plugin API Reference](../plugins/api/) — the full surface, for when you want to write more than a line
- [Workspace Trust](./workspace-trust.md)
