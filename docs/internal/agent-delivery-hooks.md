# Reaching a running agent — what each vendor actually supports

> _**PLANNED — nothing here is implemented.** What it takes to hand a message to
> an agent that is already running, per vendor, and what Fresh would have to
> change at launch to make it possible. Supersedes an earlier draft of this file
> that surveyed only hook systems and got several details wrong; the
> corrections are listed at the end. The mailbox as it ships today is in
> [agent-control-plane.md](agent-control-plane.md) §8.1._

## The requirement

Three properties, and the third is a veto:

- **(a) Wake** — if the agent is idle, the message starts a turn.
- **(b) Queue** — if the agent is mid-turn, the message waits and is picked up
  at the next turn boundary, exactly once.
- **(c) Cannot approve** — if the agent is blocked at a permission prompt,
  delivering a message must not answer it.

(c) is why typing into the agent's terminal was rejected: a paste plus Enter
answers whatever modal is on screen, which can approve a tool call nobody read.
Any mechanism that can do that is disqualified regardless of convenience.

Today's mailbox satisfies (b) and (c) and fails (a) — a file lands in `inbox/`,
and an agent with no turn loop never polls it.

## Verdict

| Agent | Mechanism | (a) wake | (b) queue | (c) safe | Evidence |
|---|---|---|---|---|---|
| **OpenCode** | `POST /session/:id/prompt_async` | **yes** | **yes** | **yes** | **run against 1.18.18, including the permission case** |
| **Claude Code** | cross-session messaging socket | yes | yes | yes | vendor docs; **message frame undocumented** |
| **Codex** | `Stop` hook, `decision:"block"` | **no** | n/a | yes | source (`openai/codex` @ `6fc6b9d`) |

**There is no uniform answer, and Codex is the hole.** Two of the three can be
reached while idle. Codex cannot: its `Stop` hook fires on the *transition* into
idle, and once the agent is idle Codex runs nothing at all, so there is no code
of ours left executing to notice a message. Everything else here is detail; that
is the finding.

## OpenCode — verified, and the strongest of the three

`POST /session/:sessionID/prompt_async` returns `204` immediately, starting the
session if needed.

The safety question was settled empirically rather than by reading: a stub
provider was driven to a genuine pending `bash` permission, and `prompt_async`
was posted into that session. The permission stayed pending and unanswered, the
message was persisted, and it was processed as a real turn only after the human
approved. It queues; it does not answer the modal.

The mechanism is sound rather than lucky. Permission replies resolve a `Deferred`
held in a map keyed by permission id, written only by `Permission.reply` — which
is reachable only from the two permission endpoints. There is no code path from
a prompt to a permission reply. Meanwhile `Runner.ensureRunning` returns the
existing run when one is live (so no second loop spawns), and the running loop
re-reads messages each iteration and exits only when the last assistant message
answers the last user message — so a newly inserted user message keeps it going.

**Two things Fresh must change at launch:**

1. **Assign the port.** A plain `opencode` in a pane opens **no TCP port at
   all** — the TUI talks to its own server over an in-process worker transport
   at a fake `http://opencode.internal`. There is no port file, no lock file, no
   instance registry, and no port or session id in the child's environment. The
   docs' "randomly assigns a port" is misleading. Fresh must launch
   `opencode --port <N> --hostname 127.0.0.1`, allocating `N` per pane.
2. **Assign the session id.** `POST /session` first, then launch with
   `--session <id>`, so Fresh owns the pane → `(port, sessionID)` mapping with no
   discovery step. (`GET /session?directory=<worktree>` also works as a
   fallback.)

**Never post to a side-car `opencode serve`.** Sessions live in one shared
SQLite database, but the event bus is an in-process `EventEmitter` and the
runner's busy-lock is per-process. Posting to a separate server process would
run the turn *there* — the pane's TUI would neither show it nor react, and
nothing would stop two loops running against one session. Post to the pane's own
port.

Set `OPENCODE_SERVER_PASSWORD`; without it the server is unauthenticated and
says so.

**One race to guard.** If `prompt_async` lands between the loop's exit and the
runner's transition to `Idle`, the work can be discarded and the message
stranded. The window looks small but is real. Mitigation: after the `204`,
confirm via `GET /session/status` that the session went `busy`, and re-post if it
did not. Note also that `prompt_async` reports failures by publishing a
`session.error` bus event, **not** by HTTP status — a supervisor watching only
the status code will miss them.

**Reading state:** `GET /event` (SSE) carries `session.status`, `permission.asked`
and `permission.replied`. Status has only three values — `idle`, `busy`,
`retry` — so **there is no "awaiting permission" status**; a blocked session
reports plain `busy`, and you correlate with `permission.asked` or poll
`GET /permission`. Prefer `session.status` over `session.idle`, which is
deprecated.

## Claude Code — purpose-built, with one hole in the middle

Cross-session messaging (~v2.1.224+, **macOS and Linux only**) binds a
Unix-domain inbox socket per session, exported as `CLAUDE_CODE_MESSAGING_SOCKET`
with a per-session `CLAUDE_CODE_MESSAGING_TOKEN`. The documented semantics are
exactly the three properties: queued between tool calls mid-turn, a **new turn
started when idle**, and it "can't approve anything — a message from another
session never counts as your consent". Rate-limiting, repeat-dropping and queue
caps are built in.

**The hole:** the docs give the auth frame (`{"type":"auth","token":"…"}`) and
never specify the message frame. That must be established empirically before
anyone designs around it.

Launch each agent with `--settings` taking **inline JSON**, which sets
`crossSessionInbound: "accept"` (the default holds messages for approval when
the receiving session bypasses permission prompts — likely Fresh's agents) and
adds any hooks. Hook entries **merge** across settings scopes rather than
replacing, and identical handlers dedupe, so this touches none of the user's
files and leaves nothing to clean up.

**Hooks remain useful for receipt, not delivery.** A `Stop` hook that drains the
inbox is genuine proof of receipt, which the current mailbox lacks. Two traps:

- Guard on **"inbox empty"**, not on `stop_hook_active`. The canonical
  early-exit-on-`stop_hook_active` pattern from the vendor's own guide is wrong
  for a queue — after the first continuation the flag stays true, so the second
  queued message is silently dropped. It is the right guard for a *predicate*
  hook, the wrong one for a *queue*.
- Coalesce all pending messages into one injection per turn: there is a hard cap
  of 8 consecutive blocks (`CLAUDE_CODE_STOP_HOOK_BLOCK_CAP`).

`asyncRewake` claims to wake an idle session on exit 2 and would be the
hooks-only fallback, but it appears twice in the reference, never in the
changelog, and is unverified. Prototype before designing around it.

**Reading state:** `PermissionRequest` fires the moment the agent asks — use it
rather than `Notification`, whose `permission_prompt` and `idle_prompt` types are
gated behind a ~6-second "user seems away" timer that each keystroke defers.
Note that in sessions that cannot show a prompt, if no hook returns a decision
the tool call is **denied** — so for background agents this event is not purely
observational.

## Codex — no way in while idle

`Stop` can hold the agent: `decision:"block"` with a non-empty `reason` (or exit
2 with stderr) turns the reason into a real **user-role** message wrapped in
`<hook_prompt>` tags and continues the turn loop. It cannot inject
`additionalContext` — the generated output schema has `additionalProperties:
false` and no such field.

But `Stop` fires only when a turn ends. An agent that has been idle for ten
minutes runs nothing, so nothing can notice a new message. The options are all
compromises:

- **Park in the hook.** The hook command itself blocks waiting for a message, up
  to `timeout` (default 600s). The agent is then parked in a hook rather than
  idle. It costs a held turn and re-arms only at the next turn end.
- **Drain and exit.** Honest but does not solve the problem: late messages wait
  for whatever starts the next turn.
- **Own the loop instead.** Drive `codex exec` against a resumable thread and
  render the transcript in the pane, rather than hosting the interactive TUI.
  This dissolves the problem but gives up the vendor's own UI and its
  interactive permission prompts.

**There is no loop protection whatsoever.** `stop_hook_active` is present in the
payload but is purely informational — the core turn loop never reads it, and
there is no depth cap or circuit breaker anywhere. Unlike Claude Code's 8-block
cap, an unguarded blocking hook loops forever. The flag also resets on every new
user turn, so bailing on it allows exactly one injection per turn.

**The worktree trap.** Fresh runs every agent in a linked git worktree, and in a
linked worktree Codex **redirects hook discovery to the main checkout's
`.codex/` and deletes the worktree's own `hooks` table**. Dropping
`.codex/hooks.json` into the worktree — the obvious implementation — silently
does nothing.

**Use `CODEX_HOME` per agent.** A private Codex home per pane carries its own
`hooks.json`, config and trust state, is isolated from the user's `~/.codex`, and
sidesteps the redirect because it is a User layer rather than a Project one.
Hooks are gated twice — project trust *and* a per-hook trust hash — and the
startup review prompt **blocks startup**, which is fatal in an unattended pane;
`--dangerously-bypass-hook-trust` is the reliable path there.

`notify` is a usable outbound nudge: fire-and-forget, fully detached, cannot
influence the agent, single event `agent-turn-complete`, and it fires only when
the `Stop` hook did *not* block — so it signals genuine idle. The crate is named
`legacy_notify`, so treat it as deprecated in direction.

**Drift warning:** the Codex changelog carries no hooks entries at all, so this
subsystem ships changes without release-note warning. The generated JSON schemas
are the wire contract and are diffable — pin a version and diff them on upgrade.

## What this means for Fresh

Three adapters, not one mechanism. The smallest interface they fit behind is a
single call — *deliver this text to this agent* — returning **woke**, **queued**,
or **unavailable**, with the mailbox file written first in every case so the
record and the fallback survive.

Each vendor forces a launch-time change, and that is the real cost of this work:

| Vendor | What Fresh must do at launch |
|---|---|
| OpenCode | create the session, pass `--session <id> --port <N> --hostname 127.0.0.1`, set `OPENCODE_SERVER_PASSWORD` |
| Claude Code | `--settings` inline JSON (`crossSessionInbound: "accept"` + hooks); capture the socket path and token |
| Codex | per-agent `CODEX_HOME`; hooks there, not in the worktree; bypass hook trust for unattended panes |

Keep the mailbox as the store. It is the record, the audit trail, and the only
thing that works for an agent we cannot configure. What changes is that delivery
stops depending on the agent choosing to poll.

**Order of work, by confidence:** OpenCode first — it is verified end to end,
including the case that disqualifies everything else. Claude Code second, gated
on establishing the socket frame. Codex last, and it needs a decision rather than
an implementation: park in a hook, or stop hosting its TUI.

## Corrections to the earlier draft

That draft was written from a single summarizing pass over vendor documentation.
Each of these was wrong:

- Claude Code's `SessionStart` was recorded as user-facing `systemMessage` only.
  It can inject model-visible `additionalContext`.
- `Notification` was proposed as the status signal. Its timing gates make it
  unreliable; `PermissionRequest` and `Stop` are the right signals.
- The 10,000-character cap was described as needing truncation. Oversized output
  is spilled to a file and replaced with a preview and path.
- "Whose config file" was left open. `--settings` accepts inline JSON, and hooks
  merge across scopes, so nothing shared is touched.
- Codex loop safety was left as "confirm rather than assume". The answer is that
  there is none.
- OpenCode's hooks were described as unable to inject at all, citing an open
  upstream request. Issue #17412 is **closed as not planned** and its
  proof-of-concept PR was closed unmerged — but injection is possible today by
  mutating `output.output` in `tool.execute.after`, and a plugin can simply call
  `client.session.promptAsync(...)` itself. The server route is still the better
  one.
