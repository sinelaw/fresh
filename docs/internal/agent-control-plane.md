# The Agent Control Plane

Purpose: make one agent, in one terminal, able to observe and drive the whole
Fresh environment — every workspace, pane, terminal and peer agent — cheaply
enough that a user stops clicking through workspaces and just asks. This
document records the design, the constraints that shape it, and the phased
plan. Status: **PLANNED** unless a section says otherwise.

Landed so far: the explicit-id terminal APIs and `describeEnvironment` (§4,
§6.1, §6.2), the discoverability fixes (§9), the `waiting` state (§7.1), and
the Fleet view with its live pane (§7.2). Still planned: input into the embed
(§6.6), Home as a full layout (§7.3), the event stream (§6.3), `script run
--as` (§6.4), declarative views (§7.5), and everything under delegation (§8).

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
read is what makes the join total, and a Fleet view must key off it rather
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
- **Never resize the source PTY.** A PTY has one size, derived from its owning
  window. Resizing it to fit a small embed would deliver `SIGWINCH` to the
  agent and reflow its TUI in its *own* workspace — visible churn for the user
  who is working there. The embed crops; it does not negotiate.

For a full-screen TUI agent the bottom rows are the prompt and input area,
which is precisely the region that answers "is it asking me something".
Cropping to the tail is therefore not a compromise — it is the right window
onto the grid.

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

## 7. The show side: Home and the Fleet

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

### 7.2 The Fleet — **implemented**

Shipped ahead of the full Home layout, because it is the part that answers the
question. A wide panel, one row per agent — glyph, name, agent kind, branch,
and either what it is waiting on or what it last said — sorted by urgency
rather than by name, so the list is read from the top and abandoned as soon as
it turns boring.

Beneath it, the selected workspace rendered **live** by `windowEmbed`, which
turned out to already exist as a widget: the host paints the real window into
a reserved rectangle, terminals included. So §6.5's "the tail is the real
terminal, not a mirror" is not future work — it is how the Fleet's live pane
already behaves. Arrowing the list re-points the embed; Enter closes the Fleet
and lands in that workspace.

Tab switches the Fleet into a typing mode that sends keys to the selected
agent's PTY by `(windowId, terminalId)` — so an approval prompt can be
answered without leaving the view (§6.6). Enter, the arrows, Escape and
Ctrl+C forward; so do the digits and `y`/`n` that answer a prompt.

Free-form typing does **not** work, and the reason is worth recording. The
host's floating-panel key path hands a plugin only the bare characters its
mode explicitly claims — `mode_text_input` is never emitted for a
panel-focused mode, and setting the mode's `allow_text_input` flag does not
change that. So "forward every printable key" has no expression today; the
answer keys are bound one at a time. Making the embed a true input target
needs host work, and until then Enter on the row still takes you to the real
terminal, which accepts anything.

The Fleet's own strings are English-only, unlike the rest of the Orchestrator,
which is fully localised across fourteen locales. That is a gap to close, not
a decision.

### 7.3 Home

Home is an ordinary workspace layout, not a new UI mode: three splits built by
a script, holding a virtual buffer, a terminal, and a virtual buffer. It takes
the startup slot the `dashboard` plugin already owns — the buffer shown when
`fresh` is started with no file — so opening any file replaces it and nothing
new has to be explained.

```
 File   Edit   View   Selection   Go   LSP   Help
 ⌂ Home ×   api-refactor ×   +
┌ FLEET ──────────────────────────── 6 agents · 2 need you ─┐┌ master · claude ───────────────────────────┐
│                                                           ││ > what's going on?                         │
│ ! flaky-test     codex     fix/flaky                 2m   ││                                            │
│   Approve edit to tests/e2e.rs?                           ││ Six workspaces. Two need you:              │
│                                                           ││   • flaky-test  — approval on tests/e2e.rs │
│ ! api-refactor   claude    fix/api-shape             4m   ││   • api-refactor — rate limited, retrying  │
│   Rate limit — retry in 40s                               ││                                            │
│                                                           ││ perf-probe and docs-pass are working.      │
│ * perf-probe     claude    perf/bench               12m   ││ ui-polish finished 31m ago, CI green.      │
│   running cargo bench --bench io                          ││ old-migration is a 6-day-old stale worktree│
│                                                           ││                                            │
│ * docs-pass      claude    docs/tidy                 8m   ││ > approve flaky-test, and tell docs-pass   │
│   writing docs/features/agents.md                         ││   to also cover the new agents API         │
│                                                           ││                                            │
│ · ui-polish      opencode  ui/spacing               31m   ││ ✓ flaky-test   ← "approved"      (mailbox) │
│   done · 3 files · ✓ CI                                   ││ ✓ docs-pass    ← "also cover…"   (mailbox) │
│                                                           ││                                            │
│ ○ old-migration            migrate/v2                6d   ││ > _                                        │
│   on-disk worktree                                        ││                                            │
│                                                           ││                                            │
│ ↵ focus  m message  a approve  d diff  x archive          ││                                            │
└──────────────────────────────────── live · updated 1s ────┘└────────────────────────────────────────────┘
┌ TAIL · flaky-test ─────────────────────────────────────────────────────────────────── following · 40 ln ┐
│ running 3 tests                                                                                         │
│ test e2e::retry_backoff ... FAILED                                                                      │
│                                                                                                         │
│ I need to edit tests/e2e.rs to widen the timeout window.                                                │
│ ╭─ Approve edit to tests/e2e.rs? ────────────────────────────────╮                                      │
│ │  [ Yes ]   [ Yes, and don't ask again ]   [ No ]               │                                      │
│ ╰────────────────────────────────────────────────────────────────╯                                      │
└─────────────────────────────────────────────────────────────────────────────────────────────────────────┘
 Trusted  Local   6 agents · 2 need you                              Fleet live    LF  UTF-8   Palette: Ctrl+P
```

The tail pane follows the fleet selection and is a **live native embed** of the
selected workspace's terminal (§6.5), cropped to its last rows — not a text
snapshot. It is also an input target (§6.6): the approval prompt drawn in the
wireframe above is answerable from Home, by typing, without switching
workspaces. The same terminal remains fully usable from its own workspace at
the same time.

The fleet panel is a subscriber to editor events, so it stays live with no
agent in the loop. After the layout is declared, keeping it current costs
nothing.

Note the division of labour this creates, which is deliberate: the **human**
gets the real terminal at native rendering cost, and the **agent** gets
`readTerminal`'s cheap stripped text. Neither pays for the other's needs.

### 7.4 The dock card

Lower priority than the Fleet, and strictly additive — the same card geometry
with a third line carrying agent kind and the last output line, and the `!`
glyph in the existing status column.

### 7.5 Declarative views

Callers describe a view; they do not write render logic. A shallow spec — one
level of nesting, named panel types, a data source and a click action per
section — keeps the emitted description small and keeps the concurrency guards
in the host, written once.

Prebuilt views: **Fleet**, **Workspace Matrix**, **What Changed**,
**Diagnostics Roll-up**, **CI/PR Board**.

---

## 8. The delegate side

One verb on the Orchestrator API. The caller states intent; an **Agent
Capability Registry** — the existing agent registry, widened with a
`transports` field — chooses how to deliver it.

```ts
orch.delegate({ agentId, instruction, wait?, expect? })
```

### 8.1 Transport ladder

1. **Launch-time prompt.** Already implemented per agent kind. A published
   interface; no risk. Best for starting a worker on a task.
2. **Filesystem mailbox.** An inbox directory per agent. Depends only on
   "agents read files", is indistinguishable from a human editing a file, and
   survives any vendor CLI change. The peer checks its inbox because its own
   briefing told it to. **The primary transport.**
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

| Phase | Ships | Why here |
|---|---|---|
| 1 | Explicit-id plugin dispatch; reach dimension on the grant; `agentId`; terminal id and last-output on the workspace listing | Unblocks everything; nothing above works without it |
| 2 | `describeEnvironment`, `getTerminalScreen`, `readTerminal`, event emission, `script run --as` | Completes the read side |
| 2b | Split-targeted live embeds, bottom-crop row offset, cross-window input routing | The tail pane; builds on shipped rendering |
| 3 | The `!` state, Home, the Fleet view, declarative views | The first thing a user feels |
| 4 | `help` topics, generated reference, briefing pointer, refusal message | Onboarding cost to near zero |
| 5 | Capability Registry transports, `delegate`, consent overlay, recursion guards | Highest risk, on a proven foundation |

Phase 3 is deliberately ahead of Phase 5: making the fleet legible should
precede letting agents act on each other, so the delegation layer lands in an
environment where its effects are visible.

## 11. Risks

- **Alt-screen capture.** If reading the live grid as *text* does not work
  cleanly for TUI agents, the agent-facing side degrades to "busy, contents
  unknown" and the Fleet view loses its third line. The human-facing side is
  unaffected — the live embed (§6.5) renders cells, not text, and already
  works. This is the highest technical risk to the agent-facing half.
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
