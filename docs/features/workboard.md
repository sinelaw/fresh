# Workboard

> **Palette:** `Workboard: Open`, `Workboard: Next Waiting Agent` · **Menu:** View → Workboard
>
> **Needs:** the Orchestrator plugin, which provides the workspaces it shows.

The Workboard puts every Orchestrator workspace and its coding agent on one full-screen board, so you can see at a glance which agents are waiting on you, which have finished, and which are still working. Press `Enter` on a card to open that workspace.

```
┌ Workboard ─────────────────────────────────────────────────────────────────[×]┐
│[ 1 Inbox ][ 2 Projects ][ 3 Folders ][ 4 Board ]                ● 2   ✓ 1   * 2  │
│────────────────────────────────────────────────────────────────────────────────│
│       Workspace      Status                         Project  Branch        For │
│▼ NEEDS YOU  2                                                                  │
│    ●  rate-limit     Should I use Postgres or Redis? api      rate-limit    4m │
│    ●  login-fix      Allow running cargo test? (y/n) web      login-fix     1m │
│▼ READY TO REVIEW  1                                                            │
│    ✓  tab-names      finished, not looked at         web      tab-names · 3 changed │
│▼ WORKING  2                                                                    │
│    *  billing-ui                                     api      billing-ui    2m │
│Enter open  n next waiting  Tab/1-4 lens  Esc close                             │
└────────────────────────────────────────────────────────────────────────────────┘
```

The state symbols are the Orchestrator dock's own: **●** waiting on you, **✓** finished work you haven't looked at, **\*** working, **·** idle. The time on each card is how long it has been in that state, counted from when the Workboard first saw it.

The list lenses are tables: a heading per group (`←` / `→` or a click fold it), then a row per workspace with its status, project, branch, working-tree changes and how long it has been in its state. The status is a waiting agent's question, "finished, not looked at", or a failed create's reason.

## Lenses

`Tab` / `Shift+Tab` cycle through four ways of arranging the same workspaces, and `1`–`4` pick one directly. The workspace under the cursor stays under it when you switch, scrolled into view. The board reopens on the lens you used last.

| Lens | What it shows |
| --- | --- |
| **Inbox** | Sorted by what needs you: **Needs you** (agent asked a question), **Ready to review** (finished, unseen), **Working**, **Parked**. In the first two, the one that has waited longest is on top. A waiting agent's question is its status; a workspace whose create failed waits here too, with the reason. |
| **Projects** | One group per project. A project is its git remote (`origin`, else the first remote), so every worktree of a repo and every clone of it, wherever it sits on disk, lands in the same group; the ssh and https forms of one URL match. A repository with no remote is grouped by its directory. |
| **Folders** | The Orchestrator dock's folders, in the dock's order, so an arrangement you made by hand is one key away. Drag a workspace onto a folder (or onto a workspace in it) to file it there; drop it on **Not in a folder** to unfile it. |
| **Board** | A kanban board that fills itself, one column of cards each: **Backlog**, **Needs me**, **Working**, **Review**, **Shipped**. |

### How the Board places cards

Each card goes to a column from its agent's state:

| State | Column |
| --- | --- |
| Waiting on you | Needs me |
| Working, or still being created | Working |
| A create that failed (retry or dismiss it from the dock) | Needs me |
| Finished, not yet looked at | Review |
| Idle, with uncommitted or unpushed work | Review |
| Idle and clean | Backlog |

Nothing on a workspace says it was merged, so **Shipped** is reached only by hand. Press `Shift+←` / `Shift+→` (or `<` / `>`) to pin a card one column left or right. A pinned card shows **⊙** and stays where you put it while its agent's state changes. Press `p` to unpin it. Moving a card back onto its automatic column removes the pin. Pins are saved and survive restarts.

## Keys

The cards are ordinary lists and trees, so they answer the keys and the mouse the way every list in Fresh does: `↑` `↓` move, a click selects, the wheel scrolls, `Enter` opens. On the Board, `←` `→` move to the next column.

| Key | Action |
| --- | --- |
| `Enter` | Open the workspace and close the board |
| `n` | Move to the next card that needs you (waiting first, then finished) |
| `Tab` / `Shift+Tab`, `1`–`4` | Change lens (or click a lens button) |
| `Shift+←` `Shift+→`, `<` `>` | Board: move (pin) the card one column left or right |
| `p` | Board: unpin the card |
| `Esc` / `q` | Close |

## Status-bar beacon

While any agent is waiting on you or has finished unseen work, the status bar shows a count such as `● 2 waiting  ✓ 1 done`. When nothing needs you it is empty and takes no room. Click it, or run **Workboard: Next Waiting Agent**, to jump to the agent that has waited longest (or, if none is waiting, the oldest finished work). The jump never picks the workspace you are already in.

The beacon adds itself to the right of the status bar for the current run, without editing `config.json`. To place it yourself, put `{workboard:attention}` in `editor.status_bar.left` or `editor.status_bar.right`. To turn it off, set **Plugins → workboard → statusBarBeacon** to off in the Settings UI.

## From a script

The plugin exports a small API for `init.ts` or [scripts](./scripting.md):

```ts
const board = editor.getPluginApi("workboard");
board?.setLens("board"); // "inbox" | "projects" | "folders" | "board"
board?.open();
```

Bind a key to **Workboard: Open** in the [keybinding editor](./keybinding-editor.md) to bring the board up from anywhere, including from inside an agent's terminal.
