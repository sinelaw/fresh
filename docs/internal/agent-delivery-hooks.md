# Delivering to an agent without typing at it — the hook route

> _**PLANNED — nothing here is implemented.** An assessment of whether the
> mailbox's delivery half can move onto the coding agents' own lifecycle hooks.
> The mailbox as it ships today is described in
> [agent-control-plane.md](agent-control-plane.md) §8.1; the problem this
> solves is recorded as §8 and "Not in this pass" in
> [chat-surface-gaps.md](chat-surface-gaps.md)._

## The problem, restated

The mailbox is **delivery, not receipt**. Sending writes one file into the
peer's `inbox/` and reports success. The agent acts on it only when it next
runs `fresh --cmd agent inbox --take`, which its briefing tells it to do at the
start of each turn. So:

- mid-turn, the message waits for the turn boundary — fine;
- **parked at a prompt with nothing to do, the agent runs no turn loop at all**,
  never polls, and the message sits unread while the UI has said "delivered".

That second case is not an edge: `waiting` is precisely the state a user is
most likely replying to.

The obvious fix — type the message into the agent's terminal — is written up as
§8 of the gaps doc and carries two hazards that are the reason it is only
half-built: it can double-deliver (the agent reads the paste *and* later polls
the file), and a paste plus Enter into an agent sitting at a permission prompt
**answers that prompt**, potentially approving a tool call nobody read.

Hooks avoid both. The agent's own runtime tells us when it is at a turn
boundary and lets us hand it text there — no keystrokes, no prompt to
accidentally answer, and a delivery point the agent itself defines.

## What each agent actually offers

Verified against current vendor documentation (August 2026).

### Claude Code — a direct fit

| Event | Fires | Can inject | Can hold the agent |
|---|---|---|---|
| `Stop` | when the agent finishes responding | `additionalContext` | **yes** — exit 2 "prevents Claude from stopping and continues the conversation" |
| `UserPromptSubmit` | before a typed prompt is processed | `additionalContext` | yes (blocks the prompt) |
| `SessionStart` | session begins or resumes | `systemMessage` (user-facing) | yes |
| `SessionEnd` | session terminates | — | — |
| `Notification` | Claude Code raises a notification | — (`terminalSequence` only) | no |
| `TeammateIdle` | a team teammate is about to go idle | — | **yes** — exit 2 keeps it working |
| `PostToolUse` / `PostToolBatch` | after a tool call / a parallel batch | `additionalContext` | batch: yes |

`Stop` is the one that matters. It fires at exactly the moment the current
design fails — the agent has finished and is about to go quiet — and it can
both hand over text and decline to let the agent stop. That is the whole
delivery mechanism in one event.

`Notification` is the natural place to *write* a status line, since it fires
when the agent wants the user. It cannot inject, which is fine: status is
outbound.

### Codex — same shape, one field different

Codex ships a Claude-style hook engine (`hooks.json`, or inline `[hooks]` in
`config.toml` beside an active config layer).

| Event | Can inject | Can hold the turn |
|---|---|---|
| `Stop` | **no** | yes — `decision: "block"` with `reason` |
| `UserPromptSubmit` | `additionalContext` | yes — `decision: "block"` |
| `SessionStart` | `additionalContext` | yes |
| `PostToolUse` | `additionalContext` | yes |
| `PreToolUse` | `additionalContext` | yes (`permissionDecision`) |

Codex's `Stop` cannot inject `additionalContext`, but `decision: "block"` takes
a `reason`, and the reason is what the model is handed to continue on. So the
message travels in `reason` instead. Same mechanism, different field.

Codex also has `notify`, an external program invoked on supported events —
currently only `agent-turn-complete`. That is a *side channel*, not an
injection point: useful as a nudge to make Fresh re-probe, not as delivery.

Project-local hooks load only when the project's `.codex/` layer is trusted;
user-level hooks are independent of project trust. That matters for where we
write config.

### OpenCode — hooks are the wrong tool, its server is the right one

OpenCode has a TypeScript plugin system with a rich event list —
`session.idle` (the assistant finished a turn), `chat.message`,
`tool.execute.before` / `.after`, `permission.asked`, and more.

But plugin hooks **cannot inject messages into the model's conversation**.
That is an open upstream request (anomalyco/opencode#17412), with only a
proof-of-concept that persists synthetic user messages. Building delivery on it
today would mean depending on an unmerged patch.

OpenCode instead exposes an **HTTP server**, which is a better fit than hooks:

- `POST /session/:id/message` — send a message and wait for the response
- `POST /session/:id/prompt_async` — send without waiting (204)
- `POST /tui/append-prompt` — append text to the TUI prompt
- `POST /tui/submit-prompt` — submit the current prompt

`prompt_async` is the delivery call. Started with `opencode serve`
(`--port`, default 4096); the TUI assigns a random port, discoverable via the
OpenAPI document at `/doc`.

Note this is genuinely different from §8's terminal injection even though
`append-prompt` + `submit-prompt` look similar: it goes through the app's own
API, so it lands in the prompt rather than in whatever modal happens to be on
screen. It cannot answer a permission dialog.

## The shape this suggests

**Keep the mailbox as the store; make hooks the delivery trigger.** The
directory pair stays exactly as it is — it is the record, the audit trail, and
the fallback for any agent we cannot configure. What changes is that the agent
no longer has to *remember* to poll.

At launch Fresh already controls the agent's argv, its environment, and its
working directory, so it can also write the hook configuration. Per vendor:

- **Claude Code** — a `Stop` hook that runs `fresh --cmd agent inbox --take`
  and, if anything came back, returns it as `additionalContext` and exits 2 so
  the agent continues instead of going quiet. `Notification` and `Stop` also
  write the `status` line, so status stops depending on the agent choosing to.
- **Codex** — the same `Stop` hook, with the messages in
  `decision: "block"` + `reason`.
- **OpenCode** — no hook; Fresh calls `POST /session/:id/prompt_async` when it
  writes to the inbox, and the plugin's `session.idle` event is a secondary
  nudge.
- **Anything else** — unchanged. The briefing still tells it to poll, and the
  file is still there.

This makes `--take` the single delivery path on every route: the hook calls the
same verb the agent would have called, so the file moves to `done/` exactly
once whoever triggers it. That is what disposes of §8's double-delivery
question without needing a decision about it.

## What it fixes, and what it does not

Fixes:

- An agent parked with nothing to do is handed the message at its own turn
  boundary rather than never.
- No keystrokes, so nothing can answer a permission prompt — §8's safety
  problem does not exist on this route.
- No double delivery, because delivery and acknowledgement are the same call.
- Status stops being a convention an agent can ignore, at least for the two
  vendors with a `Notification`-shaped event.

Does not fix:

- **An agent blocked mid-turn on a permission prompt.** `Stop` has not fired —
  the turn is not over — so the message still waits. No worse than today, and
  Claude Code's `Notification` fires there but cannot inject.
- **Agents with no hook system.** They keep polling.

## Open questions before building

1. **Loop safety.** A `Stop` hook that blocks feeds the agent more work, which
   ends in another `Stop`. Claude Code passes a flag indicating the stop hook
   already fired; the hook must return cleanly when the inbox is empty, and we
   should confirm the exact semantics rather than assume symmetry with Codex.
2. **Whose config file?** Hooks live in user- or project-level config that the
   user may also be editing. Writing Fresh's hook into a file we do not own
   needs a merge strategy and a way to not clobber, or a Fresh-owned config
   layer passed at launch.
3. **Output caps.** Claude Code caps hook output at 10,000 characters. Long
   messages, or a backlog of several, need truncation with a pointer to the
   files.
4. **Codex trust.** Project-local `.codex/` hooks only load in a trusted
   project, so a fresh worktree may silently not have them. User-level config
   avoids that but is machine-wide.
5. **OpenCode port discovery.** The TUI picks a random port. Fresh would need
   to find it per workspace, or launch `opencode serve` itself on a known one.
6. **Version drift.** All three of these are moving quickly. Whatever is built
   should degrade to the current polling behaviour when a hook does not fire,
   rather than assuming it did.

## Recommendation

Worth doing, and it supersedes §8 rather than complementing it. The Claude Code
and Codex routes are the same mechanism with one field renamed, so they are
close to one adapter; OpenCode is a different call but a simpler one. The
mailbox does not change shape, which means this can land incrementally, one
vendor at a time, with polling as the floor.
