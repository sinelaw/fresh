<!-- Generated from the plugin API source. Do not edit: change the doc comments in the Rust source, then run `cargo test -p fresh-plugin-runtime write_fresh_dts_file -- --ignored`. -->

# Windows, Splits & Terminals

Split the screen, manage windows (workspaces), and run terminals.

::: v-pre

## Splits

### `getActiveSplitId`

Get the active split ID
This is the ID of the focused split pane. Use it with focusSplit,
setSplitBuffer or createVirtualBufferInExistingSplit to manage split
layouts.

```typescript
getActiveSplitId(): number;
```

### `listSplits`

List every split with its active buffer and viewport.

Plugins that need to operate on every visible buffer
simultaneously (multi-split flash labels, syncing decorations
across panes, …) iterate this list rather than only seeing
`getViewport()`'s active-split data.  Order is unspecified.

```typescript
listSplits(): SplitSnapshot[];
```

### `moveBufferToSplit`

Move a buffer into `splitId`: show it there, and remove its tab from
the pane that held it before.

Use this to *rearrange* what is where. `setSplitBuffer` only changes
which of a pane's existing tabs is visible, so building a move out of
it leaves the original tab stranded in its old pane.

Queued, like every layout mutation: the returned bool only reports that
the command was sent, not that it took effect, and a read issued right
after it still sees the old state. `await editor.flush()` before
reading back.

```typescript
moveBufferToSplit(bufferId: number, splitId: number): boolean;
```

### `splitWindow`

Split the active pane and resolve with the new pane.

This is the primitive for arranging panes. `direction` names the
*divider*: `"vertical"` puts panes side by side (left | right),
`"horizontal"` stacks them (top / bottom). `place` says which side
the new pane lands on — `"before"` is left/top, `"after"` (the
default, and what the keyboard split does) is right/bottom.

Resolves *after* the layout has been applied and the editor's cached
state refreshed, with the new pane's id and geometry — so
`listSplits()` / `describeWorkspace()` called next observe the split
that was just made, and "did it land on the left" is answered by the
`x` that comes back rather than by guessing.

```js
// Terminal on the left, README on the right:
const left = await editor.splitWindow({ direction: "vertical", place: "before" });
await editor.createTerminal({ splitId: left.splitId });
```

Rejects when the pane could not be created.

```typescript
splitWindow(opts: SplitWindowOptions): Promise<SplitCreated>;
```

### `closeSplit`

Close a split.

The last remaining split cannot be closed; the request is logged and
ignored.

Queued, like every layout mutation: the returned bool only reports that
the command was sent, not that it took effect, and a read issued right
after it still sees the old state. `await editor.flush()` before
reading back.

```typescript
closeSplit(splitId: number): boolean;
```

### `setSplitBuffer`

Show one of a split's existing tabs. To *move* a buffer into a pane —
and take it out of the pane it was in — use `moveBufferToSplit`.

Queued, like every layout mutation: the returned bool only reports that
the command was sent, not that it took effect, and a read issued right
after it still sees the old state. `await editor.flush()` before
reading back.

```typescript
setSplitBuffer(splitId: number, bufferId: number): boolean;
```

### `focusSplit`

Move focus to a split.

To open something without taking focus in the first place, prefer
`splitWindow({ keepFocus: true })` — one call, and the user's cursor
never moves.

Queued, like every layout mutation: the returned bool only reports that
the command was sent, not that it took effect, and a read issued right
after it still sees the old state. `await editor.flush()` before
reading back.

```typescript
focusSplit(splitId: number): boolean;
```

### `setSplitScroll`

Set the scroll position of a split.

Queued, like every layout mutation: the returned bool only reports that
the command was sent, not that it took effect, and a read issued right
after it still sees the old state. `await editor.flush()` before
reading back.

```typescript
setSplitScroll(splitId: number, topByte: number): boolean;
```

| Parameter | Description |
|-----------|-------------|
| `topByte` | The byte offset of the top visible line |

### `setSplitRatio`

Resize the split that `split_id` lives in.

`split_id` is a leaf id (as returned by `getActiveSplitId`,
`listSplits`, `BufferInfo.splits`, `createTerminal`); the editor
resolves it to its parent split container and sets that container's
ratio, moving the divider between this pane and its sibling. `ratio`
is the fraction of space given to the container's FIRST child
(0.0–1.0, 0.5 = equal), clamped to [0.1, 0.9]. A leaf with no parent
container (the only pane) is a no-op.

Queued, like every layout mutation: the returned bool only reports that
the command was sent, not that the resize succeeded, and a read issued
right after it still sees the old widths. `await editor.flush()` before
reading back.

```typescript
setSplitRatio(splitId: number, ratio: number): boolean;
```

### `setSplitLabel`

Set a label on a split (e.g., "sidebar")

```typescript
setSplitLabel(splitId: number, label: string): boolean;
```

### `clearSplitLabel`

Remove a label from a split

```typescript
clearSplitLabel(splitId: number): boolean;
```

### `getSplitByLabel`

Find a split by label (async)

```typescript
getSplitByLabel(label: string): Promise<number | null>;
```

### `distributeSplitsEvenly`

Distribute all splits evenly

It sets the ratio of every split container in the active window so
each leaf split gets equal space.

```typescript
distributeSplitsEvenly(): boolean;
```

## Windows

A *window* is a project-rooted bundle of editor state: file explorer,
language servers, file watchers, split layout and open buffers. It can be
swapped in and out as a unit. The window at startup is window 1; plugins
create more. The Orchestrator plugin uses windows to run agents in parallel
worktrees and shows them as *sessions*. The API calls them windows
(`Window`, `windowId`) because Fresh already uses "session" for workspace
recovery and config layers. See `docs/internal/orchestrator-sessions-design.md`
for the design.

### `getScreenSize`

Total terminal dimensions in cells. Unlike `getViewport()`
(which reports the active split, shrunk by any vertical
split layout), this reflects the full terminal — what a
floating overlay sized by `heightPct` actually gets.

```typescript
getScreenSize(): ScreenSize;
```

### `orchestratorMode`

Whether the editor was launched by a bare `fresh` in Orchestrator mode.
This reflects the launch, not the `orchestrator_mode` preference,
which stays on for `fresh FILE`. Plugins in the mode use it to
override their own settings.

```typescript
orchestratorMode(): boolean;
```

### `dockOpen`

Whether the left dock slot is open: a panel is in it, or the host is
holding the column for one its manifest declared. The plugin that
fills the dock mounts it at
`ready` iff this is true.

```typescript
dockOpen(): boolean;
```

### `dockCols`

The dock column's width in cells, open or not; `0` when the terminal
is too narrow for a dock. Lay dock content out to this: the host owns the width and re-fits it on
resize.

```typescript
dockCols(): number;
```

### `describeWorkspace`

Describe the editor as it is right now: the panes of the active
window in visual order with their geometry and contents, which one
has focus, the working directory, and every open workspace.

This is the call to start from. It answers "which pane is on the
left", "is that a terminal or a file", and "what am I pointed at"
in one read, instead of stitching `listSplits` + `getBufferInfo` +
`getActiveSplitId` together and still not knowing pane order.

It reads the editor's cached state, so it is cheap and synchronous —
but that state only reflects changes the editor has already applied.
After a mutation,
`await editor.flush()` first (or await the mutation itself, if it
returns a promise) or this reports what was true before it.

```js
const ws = editor.describeWorkspace();
const left = ws.panes[0];               // leftmost pane
const term = ws.panes.find(p => p.kind === "terminal");
```

```typescript
describeWorkspace(): WorkspaceDescription;
```

### `flush`

Wait for every mutation queued so far to be applied, then resolve.

Mutating calls are queued and the editor applies them after the call
returns, so a read issued right after a mutation reports the state
from *before* it:
`setSplitRatio(...)` followed by `listSplits()` returns the old
widths. Awaiting this closes that window, which is what lets a
single script change the layout and then verify what it changed.

```js
editor.setSplitRatio(splitId, 0.3);
await editor.flush();
return editor.describeWorkspace();   // reflects the new ratio
```

```typescript
flush(): Promise<void>;
```

### `createWindow`

Create a new editor session rooted at `root`. `root` must be
an absolute path; relative paths are rejected by the editor
(logged, no session created). The new session's id is
reported via the `window_created` hook payload — plugins
that need the id should listen for that event rather than
polling `listWindows`.

It does not switch to the new session. Call `setActiveWindow` for
that.

Returns `false` only when the editor can no longer take commands (for
example while it shuts down).

```typescript
createWindow(root: string, label: string): boolean;
```

| Parameter | Description |
|-----------|-------------|
| `label` | Display label. An empty string uses the root's base name. |

### `setActiveWindow`

Make the session with id `id` the active one. No-op if
already active. Errors (id not found) are logged on the
editor side; the JS caller can verify by reading
`activeWindow()` after.

Each session keeps its own splits, buffers and language servers, so
switching back to one does not recreate buffers or restart its
language servers.

**Not every id you can read is a window id.** The orchestrator's
`listWorkspaces()` reports a *negative* `windowId` for a workspace it
discovered on disk but has never activated — there is no window yet,
so the negative value is a placeholder, not a handle. Passing one here
returns `false`; to open such a workspace use
`getPluginApi("orchestrator").focusWorkspace(workspaceId)`, which
attaches a session at the worktree first.

Returns `false` for any non-positive id rather than throwing.

```typescript
setActiveWindow(id: number): boolean;
```

### `setActiveWindowAnimated`

Switch the active window with a directional wipe on the
incoming content. `fromEdge`: "top" | "bottom" | "left" |
"right".

Same id rules as `setActiveWindow`: a non-positive id returns `false`.

```typescript
setActiveWindowAnimated(id: number, fromEdge: string): boolean;
```

### `setWindowCycleOrder`

Restrict (and order) the windows that Next/Prev Window cycle
through to `ids`, in this order. An empty array clears the
override (back to every window, by id). Non-open ids are skipped
at cycle time.

```typescript
setWindowCycleOrder(ids: number[]): boolean;
```

### `closeWindow`

Close session `id`. Refuses to close the active session or
the base session (id 1). Logs and no-ops on failure.

The session's buffers close with it.

```typescript
closeWindow(id: number): boolean;
```

### `deleteWorkspace`

Forget a directory's persisted workspace so a permanently deleted
or archived in-place session does not reappear on the next launch.
`closeWindow` only drops the live window — this removes the on-disk
registry file the session discovery would otherwise rediscover.
Call it *after* `closeWindow` for a session whose directory stays
on disk (a worktree-owning session is forgotten by removing its
worktree instead). No-op if nothing is persisted for `root`.

```typescript
deleteWorkspace(root: string): boolean;
```

### `prewarmWindow`

Eagerly initialise an inactive session's per-session state
(file tree walk, ignore matcher, etc.) without diving.
No-op for the active session or unknown id.

```typescript
prewarmWindow(id: number): boolean;
```

### `previewWindowInRect`

Tell the editor that the floating-overlay prompt's
preview pane should render the entire split tree of
session `id` natively. `0` (or any unknown id) clears the
override and the preview falls back to the existing
path-based phantom-leaf renderer. `clearWindowPreview` clears it
too.

Orchestrator calls this on each prompt-selection-change so
the right pane shows the highlighted session's full
editor UI live — splits, terminals, syntax highlighting,
decorations — at native rendering cost.

```typescript
previewWindowInRect(id: number): boolean;
```

### `clearWindowPreview`

Clear the session-preview override. Equivalent to
`previewWindowInRect(0)` but reads better at call sites.

```typescript
clearWindowPreview(): boolean;
```

### `listWindows`

All editor sessions, sorted by id (creation order). Always
non-empty (the base session is always present).

Each entry gives the session's id, label and root, among other
fields.

```typescript
listWindows(): WindowInfo[];
```

### `activeWindow`

The currently active session id. Always present in
`listWindows()`.

```typescript
activeWindow(): number;
```

## Terminals

### `createTerminal`

Create a new terminal in a split (async, returns TerminalResult)

The `TerminalResult` holds the buffer, terminal and split IDs.

When `opts.windowId` is set, the terminal attaches to that session's
stashed split tree without diving into it. The user's current view stays
put, and the terminal becomes visible only when the user dives into the
named session. This is how Orchestrator spawns agents into background
worktrees without disturbing the foreground session.

```typescript
createTerminal(opts?: CreateTerminalOptions): Promise<TerminalResult>;
```

### `createWindowWithTerminal`

Create a new editor window seeded with an agent terminal as
its only buffer. Atomic — replaces the legacy
`createWindow` + `setActiveWindow` + `createTerminal`
chain that left a transient `[No Name]` tab alongside the
agent terminal.

```typescript
createWindowWithTerminal(opts: CreateWindowWithTerminalOptions): Promise<SessionWithTerminalResult>;
```

### `createPreparingWindow`

Open a workspace *before* its contents exist: a real window (own id,
durable stable id, label, authority) showing a "still being built"
placeholder page. Focus can move into it right away, and the dock
row is a full workspace — renameable, filable, closable — while the
slow part (a `git worktree add`, say) runs behind it.

Narrate progress with `setWindowPreparing`, then hand the id to
`createWindowWithTerminal` as `adoptWindow` to turn the placeholder
into the live session in place, ids and all.

```typescript
createPreparingWindow(opts: CreatePreparingWindowOptions): Promise<PreparingWindowResult>;
```

### `setWindowPreparing`

Update the progress line (and displayed name) on a preparing window
— `failed` switches it to the error copy — or clear the preparing
state with `done` so the window renders as an ordinary session
again. An empty `label` leaves the displayed name alone.

Returns `false` only when the editor can no longer take commands (for
example while it shuts down).

```typescript
setWindowPreparing(id: number, message: string, label: string | null, failed: boolean, done: boolean): boolean;
```

### `sendTerminalInput`

Send input data to a terminal

```typescript
sendTerminalInput(terminalId: number, data: string): boolean;
```

| Parameter | Description |
|-----------|-------------|
| `terminalId` | The terminal ID (from `TerminalResult`) |
| `data` | Data to write to the terminal PTY (UTF-8 string, may include escape sequences) |

### `closeTerminal`

Close a terminal

```typescript
closeTerminal(terminalId: number): boolean;
```

### `signalWindow`

Send `signal` ("SIGTERM" / "SIGKILL" / "SIGINT" / "SIGHUP")
to every process group the window `id` is tracking. The
window's authority decides delivery; this is the
canonical entry point for "stop everything this window
owns" rather than reaching at the terminal level. Returns
`false` only when the editor can no longer take commands (for
example while it shuts down).

```typescript
signalWindow(id: number, signal: string): boolean;
```

:::
