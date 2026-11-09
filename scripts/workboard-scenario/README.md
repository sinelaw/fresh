# Workboard scenario

A reproducible set of Orchestrator workspaces for trying the Workboard
(`crates/fresh-editor/plugins/workboard.ts`) by hand, in an isolated home so
nothing touches your own sessions.

```sh
PATH=$PWD/target/debug:$PATH scripts/workboard-scenario/run.sh
tmux attach -t wb
# Ctrl+P → "Scenario: Build", wait ~15 s, then Ctrl+P → "Workboard: Open"
```

What "Scenario: Build" creates (`init.ts`):

| Workspace | Repo | What it exercises |
| --- | --- | --- |
| rate-limit | api | an agent asking a question (Needs you) |
| billing-ui | api | an agent printing steadily (Working) |
| old-spike | api | a plain worktree, no agent (Parked / Backlog) |
| unpushed | api | an agent that commits and stops |
| login-fix | web | a second question, asked later |
| tab-names | web | an agent that finishes with a change (Ready to review) |
| lsp-cache | web | another working agent |
| crashed | web | an agent that exits at once (no output) |
| très-long-nom-名前-… | web | a long name with accents and wide characters |
| docs-site | docs | files open, a split and two extra terminals |
| notes | notes | a folder that is not a git repo |
| gpu-box | — | an SSH host that cannot connect (a failed create) |

It also files workspaces into nested dock folders (Backend › Payments,
Frontend) and leaves an empty one (Someday), so the Folders lens has
something to show and to drag between.

"Scenario: Many" adds about thirty more workspaces across the repos and one
on a second clone of `api` (same remote, so the Projects lens groups both), with
a mix of asking, working, finishing and idle agents, a third of them in a
"Sprint 42" folder — enough to scroll every lens and overflow Board columns.

"Scenario: Churn" — run it, then open the board within three seconds —
renames `lsp-cache`, archives `old-spike`, deletes `crashed` and creates a
`hotfix` agent that asks a question, a step every 1.5 s, all under the open
board.

Driving it from a script: `tmux send-keys -t wb …` for keys, and raw SGR
mouse sequences for clicks and drags, e.g. a click at column 32, row 2:

```sh
tmux send-keys -t wb -l $'\e[<0;32;2M'; tmux send-keys -t wb -l $'\e[<0;32;2m'
```
