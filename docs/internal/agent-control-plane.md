# The Agent Control Plane

Purpose: make one agent, in one terminal, able to observe and drive the whole
Fresh environment — every workspace, pane, terminal and peer agent — cheaply
enough that a user stops clicking through workspaces and just asks. This
document records the design, the constraints that shape it, and the phased
plan. Status: **PLANNED** unless a section says otherwise.

Landed so far: the explicit-id terminal APIs and `describeEnvironment` (§4,
§6.1, §6.2), the discoverability fixes (§9), the `waiting` state (§7.1), the
agent list (§7.2), and the `pane` widget — any buffer from any window, live,
and interactive for terminals (§6.5, §6.6).

**§10's phases A, B and C have landed**: the event stream and
`script run --as`, the mailbox (`status` outbox, `inbox/`, `delegate`), and
Home. §10 records what shipped and what is deliberately deferred; everything
else here is the reasoning behind it.

The through-line: what is built so far only *observes*. A user still has to
read every answer and type every reply. Delegation is the first thing that
lets one agent act on another, and the mailbox (§8.1) is how — a pair of
directories per agent, instructions in, one-line status out.

This is the successor to
[agent-fresh-cli-exposure-plan.md](agent-fresh-cli-exposure-plan.md), whose
launcher half shipped (agent presets, resume, Start prompt, Auto mode) and
whose CLI half also shipped (the script channel). What remains unsolved is
everything *across* workspaces, which is what this document is about.

---

## 1. The goal, and why today's answer is "click through them"

A user running the Orchestrator ends up with many workspaces — one per git
worktree — each holding buffers, splits, terminals, and often a coding agent.
The agents are heterogeneous: `claude`, `codex`, `opencode`, `aider`, from
different vendors, with no shared coordination protocol and no incentive to
grow one.

Today the only way to know what any of them is doing is to switch to its
workspace and read its terminal. The dock helps — it lists workspaces with
branch and git summary — but it cannot answer the question that actually
matters when six agents are running: **which of them is waiting on me?**

The dock's activity signal has two live states, derived from whether the PTY
printed recently: working (`*`) and idle (`·`). An agent parked on a
permission prompt and an agent that finished twenty minutes ago render
identically. That is the central UX gap.

## 2. What already exists

Enough that most of this plan is connection work, not new subsystems.

| Capability | Where it lives | State |
|---|---|---|
| Script channel — TypeScript evaluated against a live editor | `server::command_access`, the `script` CLI verbs | Ships |
| Capability tokens — per-workspace, window-bound, minted at launch | `server::command_access`, injected as `FRESH_CMD_TOKEN` | Ships |
| Agent briefing at launch | Orchestrator's system-prompt injection (flag or `AGENTS.md` block) | Ships |
| Agent registry — resume/auto/prompt/system-prompt strategy per agent | Orchestrator | Ships |
| Orchestrator plugin API — workspaces, folders, lifecycle, archive, dock view | Orchestrator's `exportPluginApi` surface | Ships |
| Live clickable panels — virtual buffers, widget panels, dashboard sections | Plugin API, `dashboard` plugin | Ships |
| **Cross-window live rendering** — another session's whole split tree, terminals included, drawn natively into a rect | The window-preview primitive, used by the picker preview | Ships |
| One PTY behind several views, with a single input target | Per-split terminal rendering on `Window` | Ships (within a window) |
| Busy/idle detection from OSC 133 / OSC 9;4 plus output timing | Orchestrator session state | Ships |
| Per-terminal scrollback and log files on disk | Terminal subsystem | Ships |
| Dense authoring guides for scripts and plugins | `help script`, `help plugin` | Ships |

The `help plugin` guide in particular is already the dense, LLM-targeted
document this plan would otherwise have to write: the runtime contract, the
dev loop, and a worked auto-refreshing clickable panel with a note on which
failure each guard prevents.

## 3. The blockers

### 3.1 Ambient window scoping in the plugin dispatch layer

The plugin-facing host handlers resolve against the *active window* rather
than an id supplied by the caller. Sending input to a terminal looks up that
terminal in the active window's terminal manager; reading buffer text indexes
the active window's buffer map. Terminals and buffers are owned per-window, so
a handler that doesn't take a window id can only ever reach one window.

The consequence is not a policy restriction that could be relaxed — it is that
cross-workspace control is *unexpressible*. A script cannot read a sibling
workspace's terminal because there is no parameter in which to say which
workspace it means.

Ambient scoping also makes scripts unreliable within a single workspace. The
script channel points the editor at the token's window before the first
statement, but a script that outlives an `await` cannot assume the active
window is still its own — the user keeps working while it runs.

### 3.2 A plain terminal cannot drive the editor, and is not told why

Verified by running the editor: a terminal opened through **Open Terminal**
receives `FRESH_SESSION` but no `FRESH_CMD_TOKEN`, because only the
Orchestrator launcher mints tokens. The script channel then refuses it:

```
$ echo 'return editor.listBuffers()' | fresh --cmd script run
no capability token: script evaluation is not authorized
```

An agent started in such a terminal follows the documented instructions, is
refused, and reasonably concludes the capability does not exist. The refusal
names the missing token but not the remedy.

### 3.3 There is no cross-workspace read

`listWorkspaces` deliberately narrows its output and omits the terminal id it
holds internally, so even a caller that could address another window has no
handle to address. Nothing exposes a terminal's current screen or scrollback
through the API at all.

### 3.4 Everything is poll-shaped

There is no event stream. An agent that wants to know what changed must
re-query, which makes ambient token cost scale with wall-clock time rather
than with events.

---

## 4. Principle: explicit ids, zero ambient state

Every machine-facing call takes the id of the thing it acts on. `active` and
`focused` become values a caller may *read*, never defaults it *inherits*. A
call that does not name its target is an error, not a guess.

This applies to the plugin/script API boundary only. Human-facing code — action
dispatch, mouse input, rendering — keeps ambient focus, because there "the
thing the user is looking at" is the correct referent. The boundary being
changed is the plugin dispatch layer.

Each addressable thing gets one durable id and one runtime id:

| Thing | Durable | Runtime | Note |
|---|---|---|---|
| Workspace | `workspaceId` | `windowId` | both already exist |
| Split / pane | — | `splitId` | qualified by `windowId` |
| Buffer | path or virtual name | `bufferId` | qualified by `windowId` |
| Terminal | backing-file stem | `terminalId` | qualified by `windowId` |
| Agent | `agentId` | `terminalId` | **new** |

`agentId` is new and necessary: a peer agent needs a name that survives its PTY
being restarted, and delegation needs a stable target.

**Migration.** Add explicit-id forms; keep the ambient forms working while
emitting a deprecation into the status log; make them errors one minor release
later. `script check` gains a lint so a script author is corrected before the
script touches a workspace someone is watching.

---

## 5. Principle: no new CLI verbs

The script channel is already the authorized, capability-checked way into a
running editor, and the existing convenience verbs (`init reload`,
`command run`, `command list`) are thin wrappers that submit canned scripts.
A new capability therefore needs a new *script-reachable API*, not a new verb
and not a new socket message.

There is a second reason beyond consistency. `script api` searches the shipped
declaration files, so a capability delivered as an API member is automatically
discoverable and typed; one delivered as a CLI verb needs its own separate
documentation that can drift.

Everything in this plan lands in one of three places:

1. **`editor.*` members** — reachable via `script run`, discoverable via
   `script api`, typed in `fresh.d.ts`, checked by `script check`.
2. **Orchestrator plugin API verbs** — reachable via `getPluginApi`, typed
   into `plugins.d.ts`.
3. **`help` topics** — an existing verb taking new arguments.

Net new top-level CLI verbs: **zero**. Net new CLI flags: **one**
(`script run --as <name>`, see §6.4).

### 5.1 A script is a plugin you install in one line

Worth stating plainly because it is not obvious and it removes most of the
apparent need for new machinery: an agent script is loaded as a real plugin,
gets the full plugin bootstrap including `registerHandler`, and is
deliberately never unloaded. So a script submitted from a shell may subscribe
to events, arm intervals, publish a plugin API, and mount panels — and all of
it outlives the submission.

This is why the event stream in §6.3 needs no new protocol, and why the Home
view in §7 is a script rather than a feature.

---

## 6. The read side

### 6.1 `describeEnvironment()`  — **implemented**

A sibling of the existing `describeWorkspace()`, widened from one window to
all of them: every window with its durable id, label and root, and every
terminal in it with its id, title, alt-screen flag, grid size and pid.

One call replaces the several a caller currently makes, and covers ground no
sequence of current calls can reach.

It reports structural facts only; agent state, branch and git summary stay
with the Orchestrator, which already tracks them per session. But the split is
not merely tidiness — the Orchestrator's own `terminalId` is populated only
for workspaces *it* created, and is null for the launch workspace and for any
window reconciled from the host's window list. Measured on a live editor: a
workspace whose agent terminal the Orchestrator recorded as `null` was still
found, written to and tailed through `describeEnvironment`. So the environment
read is what makes the join total, and the agent list must key off it rather
than off the Orchestrator's bookkeeping.

Deliberately excluded: per-pane geometry. `describeWorkspace()` already gives
it for a window that matters, and folding every window's split tree in would
make the common "who needs me" poll pay for layout detail it never reads.

### 6.2 Terminal content

Two views, because they answer different questions:

```ts
getTerminalScreen(windowId, terminalId)
  -> { rows, cols, cursor, altScreen, title, oscRunning, text, seq }

readTerminal(windowId, terminalId, { sinceSeq?, lines?, stripAnsi? })
  -> { text, seq, truncated }
```

`altScreen` is first-class and load-bearing. The agents that matter here —
`claude`, `codex`, `opencode` — run as full-screen TUIs, so the on-disk
scrollback capture is a poor record of what the user actually sees; the live
grid is the honest one. A caller needs to know which view is meaningful before
it reads.

`sinceSeq` makes tailing incremental. A view following six agents pays for the
delta, and an unchanged `seq` is nearly free to confirm.

**Terminal content is for situational awareness, not for verification.**
Deciding whether a delegated task succeeded by parsing ANSI scrollback is
expensive and unreliable. Verification goes through environmental state —
git status, tests, files, lint — or through structured output captured before
the rendering layer. This distinction is a design rule, not a preference.

These text APIs serve the *agent*, which needs something cheap it can reason
over. They are not how the *human* sees another workspace's terminal — see
§6.5.

### 6.5 Live embeds: the human's view is the real terminal

Fresh already renders one window's live PTY grid inside another window's
layout. The Orchestrator picker's preview pane does exactly this: a host
primitive takes a window id and renders that session's **entire split tree —
splits, terminals, syntax highlighting, decorations — natively and live**,
into a rect belonging to the window the user is actually looking at. The
terminal-split renderer lives on `Window` rather than on the editor, precisely
so the preview path can drive it for a window that is not the active one.

So the Home tail pane is not a mirror painted into a virtual buffer. It is the
real terminal, rendered by the real renderer, at native cost. What is missing
is only the addressing and the plumbing around it:

- **Target a split, not the prompt's preview pane.** The primitive is
  currently bound to the floating prompt's preview rect. It needs to accept a
  destination split in the current window, and to accept a *terminal* within
  the source window rather than only the whole split tree.
- **Crop from the bottom.** The grid renderer clips rows past the rect's
  height, keeping the top of the grid. A tail wants the opposite. A row offset
  turns "the first N rows" into "the last N rows".
- **Size the PTY to the view that is presenting it.** A PTY has one size, and
  it belongs to whichever view is showing it right now: Home opens on an
  agent, that agent's terminal resizes to the embed pane; Home closes and
  the owning window reclaims it.

An earlier draft of this section said the opposite — crop, never resize — on
the grounds that a resize would `SIGWINCH` the agent and disturb its own
workspace. That reasoning was wrong twice over.

Fresh is one terminal: while the Home overlay is up, nobody is looking at
that workspace, so there is no second observer to disturb. The two sizes are a
sequence, not a conflict.

And cropping does not survive contact with a TUI. Cropping *rows* is fine —
the last N rows are the tail, and for a full-screen agent that is the prompt
and input area, precisely the region that answers "is it asking me something".
Cropping *columns* is not: an agent drawing a 180-column dialog into a
100-column pane loses the right edge of every box and cuts every wrapped line.
The content Home exists to show is exactly what would be clipped.

Sizing to the view also generalises what already happens in-window — a
terminal dragged into a new split re-wraps to that pane — rather than adding a
second model. Two details are part of the change, not follow-ups: resize on a
*settled* selection, since arrowing a long list would otherwise rewrap a
different terminal's scrollback per keypress; and reclaim the size on close,
or the agent is left at Home's dimensions permanently.

### 6.6 One PTY, many views, one input target

The multi-view model is already the shipped design *within* a window: two
splits may show the same terminal, one scrolled back while the other streams
the live grid, independently and off-focus; the block cursor belongs to the
focused split only, and the others "mirror the same PTY but aren't the input
target".

Extending that across windows is the whole of the interactivity story. When
the Home tail embed holds focus, keystrokes route to that terminal's PTY.
When the user instead visits the named workspace, they type into the same PTY
from there. Both are views of one process; focus decides which one is the
input target. Nothing needs to be handed over, detached, or moved — and
neither view is a second-class copy.

This is what makes Home a control room rather than a status board: an agent
that needs an answer can be answered without leaving the view that told you it
was waiting.

### 6.3 Events without a new protocol

A persistent script subscribes and appends; a reader tails the file:

```sh
fresh --cmd script run --as env-watch <<'EOF'
  const log = editor.getDataDir() + "/events.jsonl";
  registerHandler("emit", function (ev) {
    editor.appendFile(editor.localPath(log), JSON.stringify(ev) + "\n");
  });
  editor.on("agent_state_change", "emit");
  editor.on("window_created", "emit");
  return log;
EOF
```

No new socket message, no streaming CLI, and `tail -f` is a primitive every
agent already has. It is also the same mechanism as the delegation mailbox in
§8, in the other direction: events out through a file, instructions in through
a file.

### 6.4 Naming a persistent script

A submitted script is currently loaded under a generated name the caller
cannot predict, so it cannot later be unloaded or replaced by name — and
installing a live panel twice stacks two of them, hitting exactly the
re-entrancy failures `help plugin` documents.

`script run --as <name>` names the loaded plugin, making install idempotent
and `unloadPlugin` / `reloadPlugin` usable on it. This is the one new CLI flag
in the plan.

---

## 7. The show side: Home

### 7.1 The `!` state — **implemented**

The headline signal. Today's activity model has two live states; the plan adds
a third that is neither "printing" nor "not printing" but **parked at a
prompt** — blocked on the user. It is derivable from the OSC 133 command
markers the terminal subsystem already receives, refined per agent kind by the
registry.

```
 *  working      the PTY is producing output
 !  needs you    parked at a prompt — and the card says why
 ·  idle         quiet; nothing wanted
 ○  on-disk      a worktree with no live window
```

As shipped, `!` is recognised from the terminal's own screen, refined per
agent kind by the registry. That is a guess about someone else's TUI, and the
patterns are provisional by construction. **The agent's own `status` line
(§8.1) supersedes it wherever one exists** — an agent saying `waiting …`
outranks anything inferred from pixels, and the summary it writes is what it
knows it is doing rather than whatever it last printed. Screen inference stays
as the fallback for agents that write no status, so nothing regresses.

### 7.2 The agent list — **implemented**

One row per agent — glyph, name, project, agent kind, branch, and either what
it is waiting on or what it last said — sorted by urgency rather than by name,
so the list is read from the top and abandoned as soon as it turns boring.

For one release this was also its own panel, called the Fleet: the list plus
the selected agent's live terminal. That panel is gone. Home (§7.3) is the
same list and the same terminal with a chat beside them, so keeping the Fleet
meant maintaining a strictly worse copy of one view. The row model — ordering,
the reason column, the frozen-at-open order — is what survived, and it is what
Home draws.

Beside the list, the selected workspace rendered **live**. So §6.5's "the tail
is the real terminal, not a mirror" is not future work — it is how the live
pane already behaves. Arrowing the list re-points it; Enter closes Home and
lands in that workspace.

`Alt+`` moves the keyboard into the pane, which then sends keys to that
agent's PTY — so an approval prompt can be answered without leaving the view
(§6.6). Enter, the arrows, Escape and Ctrl+C forward; so do the digits and
`y`/`n` that answer a prompt. Not Tab: Tab is a key agents genuinely want
(completion, field navigation in their own TUI), so a toggle that ate it would
make the one view built for answering an agent unable to send it.

Typing works, including free-form prose, because the live pane is a `pane`
widget rather than a picture. A focused interactive pane takes any key the
panel's own mode does not claim and routes it through `key_to_pty_bytes` — the
same translation a focused terminal split uses, so app-cursor mode and
modifiers come from one place rather than a table maintained beside it. The
typing mode claims only `Alt+``, its way back out.

That is the fix for a limitation an earlier draft of this document called
"needs host work". It did — but the work was making the embed a pane, not
forwarding more keys. Per-key forwarding was deleted rather than extended.

### 7.3 Home — **implemented**

A full-screen panel, 90% × 90% like Settings, answering the three questions
you have about a machine full of agents: who needs me, what did they say, and
what do I want them to do next.

```
 File   Edit   View   Selection   Go   LSP   Help
 [No Name] ×   +
┌─────────────────────────────────────────────────────────────────────────────────────────────────┐
│╭─ ▸ chat ───────────────────────────╮╭─ 6 agents · 2 need you ──────────────────────────────────╮│
││                                    ││ ! flaky-test    fresh    codex     fix/flaky             ││
││                                    ││   Approve edit to tests/e2e.rs?                          ││
││                                    ││ ! api-refactor  fresh    claude    fix/api-shape         ││
││ flaky-test  ▸ tests/e2e.rs needs a ││   Rate limit — retry in 40s                              ││
││             ▸ wider timeout. ok?   ││ * perf-probe    fresh    claude    perf/bench            ││
││ api-refac…  ▸ rate limited,        ││   running cargo bench --bench io                         ││
││             ▸ retrying in 40s      │╰──────────────────────────────────────────────────────────╯│
││         you ▸ @flaky-test approved ││╭─ live · flaky-test ─────────────────────────────────────╮│
││ flaky-test  ▸ done, suite green    ││ running 3 tests                                          ││
││                                    ││ test e2e::retry_backoff ... FAILED                       ││
││ [@name what should it do?        ] ││ ╭─ Approve edit to tests/e2e.rs? ─╮                       ││
│╰────────────────────────────────────╯│ │  [ Yes ]  [ No ]                │                      ││
│                                      ╰──────────────────────────────────────────────────────────╯│
│   Tab chat / list / agent   ↑↓ select   @name address   Alt+` type at agent   Enter send / go     │
└─────────────────────────────────────────────────────────────────────────────────────────────────┘
```

The left column is **not an agent**. It was, in the design this replaces: a
designated "master" workspace whose terminal filled that half, which you
talked to in prose and which drove the rest. That is a fine thing to be able
to do — and you still can, by making an agent and telling it things — but it
is the wrong thing for this pane, because it puts an LLM between you and a
message you already know how to write. The messages agents send are already
in their outboxes (§8.1) and a reply is a file write. Both halves are
deterministic, so the pane is: a transcript, a line to type in, and `@name` to
decide where it goes. A line with no `@` goes to whoever spoke last, which is
what makes a back-and-forth with one agent readable.

Nothing summarises and nothing decides who should hear what. A control plane
that paraphrased its agents would be a worse control plane.

The right column is the list over the selected agent's terminal, a **live
native embed** (§6.5) rather than a text snapshot, and an input target (§6.6):
the approval prompt in the wireframe is answerable from Home, by typing,
without switching workspaces. The same terminal stays fully usable from its
own workspace at the same time.

Both columns are budgeted against one height so they end level, and the chat's
input line is pinned to the panel's last row rather than riding up and down as
messages arrive.

Home stays live from the same probe data the dock uses plus a poll of every
outbox, so keeping it current costs no agent in the loop.

Note the division of labour this creates, which is deliberate: the **human**
gets the real terminal at native rendering cost, and the **agent** gets
`readTerminal`'s cheap stripped text. Neither pays for the other's needs.

### 7.4 The dock card

Lower priority than Home, and strictly additive — the same card geometry
with a third line carrying agent kind and the last output line, and the `!`
glyph in the existing status column.

### 7.5 Declarative views

Callers describe a view; they do not write render logic. A shallow spec — one
level of nesting, named panel types, a data source and a click action per
section — keeps the emitted description small and keeps the concurrency guards
in the host, written once.

Prebuilt views: **Roster**, **Workspace Matrix**, **What Changed**,
**Diagnostics Roll-up**, **CI/PR Board**.

---

## 8. The delegate side

One verb on the Orchestrator API. The caller states intent; an **Agent
Capability Registry** — the existing agent registry, widened with a
`transports` field — chooses how to deliver it.

```ts
orch.delegate({ agentId, instruction, wait?, expect? })
```

### 8.1 The mailbox — a pair of directories per agent — **implemented**

Two files, one convention, and it carries both directions.

```
<state>/orchestrator/agents/<workspaceId>/
  inbox/          instructions in  — one file per instruction
  inbox/done/     the agent moves a file here once it has acted
  status          one line out     — the agent rewrites it as it works
```

Both halves depend on nothing but "agents read and write files", which is true
of every agent that exists, is indistinguishable from a human editing a file,
and survives any vendor CLI change. The peer checks its inbox and updates its
status because **its own briefing told it to** — not because Fresh injected
anything.

#### The outbox: agents say what they are doing

`status` is one line the agent rewrites whenever its state changes:

```
<state> <one-line summary>
```

`<state>` is one of `working` `waiting` `idle` `done` `blocked`. Everything
after the first space is shown verbatim. A shell one-liner has to be enough to
write it, because that is what agents will actually do:

```sh
echo "waiting approval to edit tests/e2e.rs" > "$FRESH_AGENT_STATUS"
```

A JSON object on one line is accepted as an alternative for agents that prefer
structure (`{"state":"waiting","summary":"…"}`), but the bare form is the one
the briefing teaches.

**This supersedes screen-scraping as the source of the `!` state and the
reason column** (§7.1, §7.2), and it is a better answer in every direction:

- **It is authoritative.** Scraping is a guess about someone else's TUI. It
  cannot tell "the word *Approve* appeared in a diff the agent is showing me"
  from "I am asking you to approve", and today's patterns are admittedly
  provisional — they match what one vendor's TUI happens to draw.
- **It does not break on redesign.** A vendor changing its prompt layout
  breaks a regex; it does not break a line the agent wrote itself.
- **It costs nothing.** No PTY read, no per-tick screen parse, no regex over
  40 rows per agent per tick. A file mtime is the whole poll.
- **It works where scraping cannot** — an agent with no TUI, a headless run, a
  remote workspace, an agent mid-redraw.
- **The summary is better.** "running cargo bench --bench io" is what the
  agent knows it is doing; the last terminal line is whatever happened to be
  printed, which is often a progress bar or a blank.

Fallback is unchanged behaviour, not an error: an agent that writes no
`status` file keeps today's screen-derived state and last-line summary. So the
outbox is strictly additive, and an agent that adopts it simply becomes more
legible than one that has not.

Home reads `status` directly — deterministic plugin code, no model in the loop
— which is what keeps the view free to run at whatever tick rate it likes.

#### One mailbox per agent

```
<root>/.fresh/agents/<name>/
    status        one line — "<state> <summary>" — rewritten in place
    inbox/        one file per instruction for it; inbox/done/ when acted on
    outbox/       one file per thing it wants to say; outbox/read/ once shown
```

Per **agent**, not per worktree. Two agents routinely share a checkout — a
build watcher and a refactorer, or simply two `claude`s in two terminals — and
one mailbox per root would have them overwriting each other's status and
racing for each other's instructions. The directory name is also the agent's
**address**: it is what you type after `@` in Home's chat, so it has to name
an agent rather than a directory for the chat to be able to say anything.

In the checkout rather than in our data dir, because that is a path both sides
can compute without coordinating. An agent someone started by hand knows its
working directory and nothing about the plugin's layout, so a data-dir
location would be discoverable only by agents we launched — which is exactly
the constraint the mailbox exists to remove. Agents we *do* launch are handed
`$FRESH_AGENT_NAME`, `$FRESH_AGENT_STATUS`, `$FRESH_AGENT_INBOX` and
`$FRESH_AGENT_OUTBOX` so they never have to derive any of it.

#### Discovery: an agent is whatever reports, not whatever we launched

Discovery lists `<root>/.fresh/agents/` over every checkout already known —
live session roots and the worktrees found on disk. Deliberately not a
filesystem walk: "every status file on this machine" is a much more expensive
question, and answering it would surface agents working on things the user is
not.

A mailbox found in a checkout that has a live window, where that window has
not claimed one, is *adopted* onto that window — someone started an agent by
hand in a workspace we happen to have open, and listing it twice (once with
its own words, once with a guess from its screen) would be worse than not
finding it. Only when it is the only mailbox there, though: with two, we
cannot tell which of them is the agent in that window's terminal, and guessing
hands one of them a terminal belonging to the other.

This is the inversion that makes the list honest. Before it, the roster was
"agents the user happened to start through the dock", and anything else was
invisible no matter how loudly it was blocked. After it, an agent is whatever
reports — and reporting costs one `echo`.

The in-repo directory carries a `.gitignore` containing `*`, which excludes
its contents and itself, so `git status` stays clean. Verified rather than
assumed: a control plane whose cost is a permanently dirty working tree would
not be worth using, and an agent that runs `git status` to decide what it
changed must not see our bookkeeping.

#### The inbox: instructions in

`delegate()` writes one file per instruction:

```
inbox/20260811T164500-a3f1.md
---
from: <agent name or "user">
to: <agent name>
intent: <short imperative>
---
<the instruction, in prose>
```

The agent's briefing says: read `inbox/` at the start of each turn, act, then
move the file to `inbox/done/`. That move is the acknowledgement — no protocol,
no ack message, and the delegation's outcome is visible as a file that did or
did not move.

#### The outbox: what an agent says, out

`status` is a state, not a conversation: it is one line and the agent
overwrites it, so anything said there is gone at the next state change. "I'm
done pushing the 3 PRs" is a thing said *once, at a moment*, that must still
be there when the user looks up two minutes later.

So the outbox is a directory beside the status file — one file per message.
Home drains it every tick, appends each message to the transcript attributed
to the agent that wrote it, and moves the file to `outbox/read/`. The move is
what makes the drain idempotent: a message is appended once however often the
drain runs, and the original stays readable. Files are drained in name order,
so a numeric timestamp keeps several messages written in one turn in the order
they were said.

The transcript is persisted, because the conversation outlives the panel: an
agent that answered while Home was closed still said it to you.

#### The rest of the ladder

The mailbox is **the primary transport**. The others stay in the design as
fallbacks for cases it cannot reach, and are ordered by risk:

1. **Launch-time prompt.** Already implemented per agent kind. A published
   interface; no risk. Best for starting a worker on a task.
2. **The mailbox.** Above. The default for anything sent to a running agent.
3. **Headless one-shot.** Documented non-interactive modes. Gated in the
   registry on credential kind, defaulting to off where a vendor's terms
   restrict automated use of an interactive subscription tier, and surfacing
   why it is unavailable rather than failing silently. Verify against current
   vendor terms before enabling.
4. **PTY injection.** Opt-in per agent kind, default off. Fresh owns the PTY
   master, so this needs no OS-level keystroke injection and is unaffected by
   the kernel hardening that disabled `TIOCSTI`. Fresh also holds the OSC 133
   markers, so it can inject only when the peer is genuinely at a prompt —
   something no external automation can determine. Bracketed-paste framed,
   rate-limited, audit-logged, and blocked from answering approval prompts.

Deliberately excluded: MCP. It would fork the authorization model away from
the capability token, require Fresh to write itself into three vendors'
configuration files, and keep a resident tool schema in every turn's context —
against a documentation surface (`help` topics) that costs nothing until
asked. The shell is the more conservative dependency.

### 8.2 Consent and blast radius

Automating agents that hold file-write and shell-execute privileges needs
explicit, visible limits.

- **Delegation overlay** — a cross-workspace instruction surfaces over the
  target pane before it lands, approvable per instruction, per session, or per
  target.
- **Recursion guards** — delegation depth limit, per-session budget, cycle
  detection. A control agent delegating to an agent that delegates back must
  terminate.
- **Audit panel** — every delegation, its transport, its approval and its
  outcome, on screen rather than in a log.

### 8.3 Reach

The capability grant gains a reach dimension alongside the window it is bound
to: `Own` (today's behaviour, the default) or `AllWindows`, minted only for a
workspace the user explicitly designates as the control workspace. One
toggle, one dock badge, revocable. The control-plane capability becomes a
legible grant rather than a side effect of ambient scoping.

---

## 9. The teach side

`help` gains topics — `agents`, `orchestrator`, `dashboards`, `ids` — beside
the existing `script`, `plugin` and `tour`. The bare `help` output becomes the
tiered index: a short list of capability areas and the exact command for each,
small enough to inline in the launch briefing.

Reference sections are generated from the artifacts the binary already embeds,
as `help script` generates from the shipped declarations. A hand-maintained
list of API members drifts; a drifted member is a failed call.

Two discoverability fixes are independent of everything else and worth doing
first because they are nearly free:

- The launch briefing names `script api`, `script check` and `script types`
  but not `help plugin` — the guide most likely to make an agent's first panel
  work. One line.
- The unauthorized-token refusal (§3.2) should say how to obtain the
  capability, not just that it is absent.

---

## 10. Phasing

Phases 1–4 are largely done; what remains is A, B, C, in that order. Each is
sequenced so the one before it removes a blocker rather than merely preceding
it.

### Shipped

| | |
|---|---|
| Explicit-id plugin dispatch, `describeEnvironment`, `readTerminal`, `sendTerminalInput(windowId)` | §4, §6.1, §6.2 |
| Live embeds and the `pane` widget — any buffer, any window, interactive for terminals | §6.5, §6.6 |
| The `!` state (screen-derived) and the agent list | §7.1, §7.2 |
| Discoverability: refusal names the remedy, briefing points at `help plugin` | §9 |

### Phase A — the event stream, and naming a script — **shipped**

`agent_state_change` / `window_created` emitted as plugin events; a persistent
script subscribes and appends JSONL; `script run --as <name>` so that script
can be installed once and replaced rather than stacked (§6.3, §6.4).

First because it is small, carries no risk, and **everything reactive is
blocked on it**. Today every surface polls — the dock polls, Home ticks,
`probeTail` re-reads screens — and a master agent can only answer when asked,
never notice. An event file is the difference between a control room and a
dashboard you have to remember to look at. `--as` is a precondition rather
than a nicety: a persistent script that cannot be named cannot be replaced,
so installing the watcher twice leaves two of them running.

### Phase B — the mailbox — **shipped**

The `status` outbox first, then `inbox/` and `delegate()`, then the guards.

Order within the phase matters. The outbox is pure gain and independent of
delegation: it makes the agent list honest immediately (§8.1) and can ship on its
own. The inbox is where the risk lives, so it lands after the view that makes
its effects visible.

1. `status` written by agents; Home and `describeEnvironment` read it;
   briefing teaches the one-liner; screen inference kept as fallback.
2. `inbox/` + `orch.delegate({agentId, instruction, wait?, expect?})`;
   briefing teaches read-act-move.
3. Consent overlay, depth limit, per-session budget, cycle detection, audit
   panel (§8.2). **Not optional** — these agents hold file-write and
   shell-execute privileges. **Not yet built**: `delegate` currently writes
   to any workspace's inbox with no approval step and no depth accounting.
   Acceptable only while the sole caller is the user through a command; it
   must land before a master agent is told to delegate on its own.

### Phase C — Home — **shipped**

The layout of §7.3: chat, agent list, live terminal. It lands last because it
is the *payoff* for A and B — with events it can notice, with the mailbox it
can act, and without either it is a terminal next to a list.

The chat replaced a designated "master agent" pane. See §7.3 for why: the
outbox already carries what agents say and `delegate` already carries what you
answer, so the pane needed a transcript and a text field, not a model.

### Deferred, deliberately

The reach dimension on the capability grant (§8.3), declarative views (§7.5),
the dock card's third line (§7.4), `help` topics (§9), and Home's
remaining polish — age column, `○` stale worktrees, row actions, the doubled
panel border. All real; none of them change what the system can do.

## 11. Risks

- **Alt-screen capture.** If reading the live grid as *text* does not work
  cleanly for TUI agents, the agent-facing side degrades to "busy, contents
  unknown" and the agent list loses its third line. The human-facing side is
  unaffected — the live embed (§6.5) renders cells, not text, and already
  works. **The `status` outbox (§8.1) retires most of this risk**: an agent
  that writes its own state is not read from the screen at all, so the
  scraping path becomes a fallback for agents that have not adopted it rather
  than the mechanism the agent list depends on.
- **Two input targets on one PTY** invites focus ambiguity: a keystroke must
  land in exactly one place, and the user must be able to tell which. The
  existing focused-split cursor rule is the model to extend, not to reinvent.
- **The id migration is broad.** It touches the whole plugin dispatch layer and
  will break existing scripts. The deprecation window and the `script check`
  lint are the mitigation; the churn buys an API that behaves the same way
  twice in a row.
- **Vendor terms change.** The headless transport must be gated in the
  registry so that adapting is a data edit, not a refactor.
- **PTY injection will break** on vendor UI redesigns. It is a leaf of the
  ladder, so when it breaks nothing above it does.
- **The mailbox depends on agents following their briefing.** An agent that
  never reads its inbox silently drops instructions, and one that never writes
  `status` is merely as legible as it is today. Both fail visibly rather than
  wrongly — an undelivered instruction is a file that never moved to
  `inbox/done/`, which the audit panel shows — but "the peer cooperates" is a
  real assumption, and it is why delegation reports delivery, not completion.
