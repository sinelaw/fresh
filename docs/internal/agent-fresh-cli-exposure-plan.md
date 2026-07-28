# Teaching launched agents to drive Fresh (the `fresh` CLI seam)

> _Part design record, part history. The **Launcher** half (agent presets,
> resume, Start prompt, Auto mode) and the **script channel** (Phase 2) ship
> today; Phase 3 is still planned. Phase 2's first implementation was a
> command-id channel that has since been replaced — the section on it records
> why, since the reasoning applies to anything else tempted to grow a verb
> menu._

Purpose: when the Orchestrator New-Workspace dialog launches a coding agent
(`claude`, `codex`, `opencode`, `aider`, …) into a workspace, we want the agent
to be able to *act on the surrounding editor* — open a file in the current
workspace, split the view, spawn a sibling workspace — by shelling out to
`fresh`, the same binary that is already in its `PATH`. This doc records what
that requires, what already works, and a phased plan, so the "give each agent a
system prompt that teaches it about the Fresh CLI" idea has a concrete target
rather than a hand-wave.

The answer that landed is narrower than "a verb per capability" and wider than
anything a verb list could reach: the agent submits a *script*, and what it may
call is the plugin API it can already read declarations for.

## Why `fresh` is the natural control surface

A launched agent already runs inside a Fresh terminal pane, which means:

- The `fresh` binary is on `PATH` (it is editor and client in one).
- Every local shell child inherits **`FRESH_SESSION`**, the name of the parent
  editor's control socket. So a bare `fresh <arg>` from inside a pane forwards
  to *this* editor instead of launching a second one.

That inheritance is the whole trick: the agent doesn't need credentials, a
socket path, or an SDK — it runs `fresh`, and the parent editor reacts. The only
question is *what* `fresh` forwards, and *how the agent learns what it can ask
for*.

## What ships today (IMPLEMENTED)

The control channel (`ClientControl` over the per-session control socket) exposes
three "do something in the editor" verbs, and the `fresh` binary's
nested-forward path routes to them:

| Agent runs | Effect in the parent editor | Blocking? |
|---|---|---|
| `fresh path/to/file.rs` (also `path:line:col`, ranges) | Opens the file as a buffer in the **current** workspace | **Yes** — waits until the buffer is closed (the `$EDITOR` contract) |
| `fresh some/dir/` | Opens the directory as a **new focused workspace** | No |
| `fresh --cmd script run` | Evaluates TypeScript against this workspace and prints what it returned | Until the script settles (bounded) |

The third subsumes what a verb menu would have offered: anything the plugin API
can do, an agent can do, and it learns the surface by reading the declaration
files `fresh --cmd script types` points at.

## The gap (what an agent still cannot do)

- **File open is blocking-only.** The nested-forward path always requests
  `wait: true`, so `fresh file` parks the agent until a human closes the buffer.
  There is no "open this and let me keep working" form on that path. (A script
  can call `editor.openFileInBackground` instead, which is the workaround.)
- **No deterministic window targeting.** A script's *starting* window is the
  token's, but the active window can change under it across an `await`. Only
  part of the API takes a window id explicitly; the rest acts on whatever is
  focused. This is a plugin-wide problem, not a script-channel one.
- **No execution guards.** No interrupt handler or memory cap, so a runaway
  script wedges the plugin thread until the process exits.
- **Not propagated across remote wrappers.** `FRESH_SESSION` is only set for
  local shells; a docker/ssh/kubectl pane's agent has no handle to the parent.
- **`aider` can't shell autonomously.** Unlike claude/codex/opencode, aider has
  no general shell tool — it only *proposes* commands for user confirmation, so
  it cannot be relied on to run `fresh …` on its own.

## How each agent would receive the instruction

The four launcher agents differ in how a caller injects extra system-level
guidance. The launcher would standardize on **one short instruction blob** and
deliver it per-agent:

| Agent | Injection mechanism | Autonomous shell? |
|---|---|---|
| `claude` | **`--append-system-prompt "<text>"`** (a launch flag — cleanest; no file touched) | Yes (Bash tool; pre-allow with `--allowedTools "Bash(fresh *)"` or a permission mode) |
| `codex` | Write/append a project **`AGENTS.md`** (auto-read), or `experimental_instructions_file`; no append flag | Yes (default `workspace-write` sandbox runs local commands) |
| `opencode` | Project **`AGENTS.md`**, or the **`instructions`** file-glob array in `opencode.json` | Yes (bash tool, gated by the permission engine) |
| `aider` | **`--read CONVENTIONS.md`** (read-only context) | **No** — proposes commands, needs confirmation |

`claude` is the only one with a pure launch-flag channel; the rest read a file
from the workspace. Since the launcher creates a fresh worktree per session, it
can write a generated instruction file into that worktree and point each tool at
it, without polluting the user's tracked files (e.g. an ignored path, or an
append that is reverted on teardown).

## Phased plan

**Phase 1 — instruction only, no host changes.** Add an opt-in "Teach the agent
the Fresh CLI" affordance to the New-Workspace dialog (a per-agent checkbox,
sibling to Auto mode). When set, the launcher injects a short prompt documenting
the *two verbs that already work* (`fresh <file>` to open in this workspace,
`fresh <dir>` to spawn a workspace), via each agent's mechanism above. This is
useful immediately and reversible. It also surfaces the honest limitation: file
open blocks, so the prompt tells the agent to background it (`fresh file &`) or
prefer it for hand-offs, not mid-task peeks.

**Phase 2 — a script channel (`fresh --cmd script`) — LANDED.**

Phase 2 first shipped as a *command* channel: `ListCommands` / `RunCommand` over
the control socket, a per-workspace allowlist of command ids, `fresh --cmd cmd
list|describe|run`, and thin aliases (`split`, `workspace new`, `agent
run|new`). It worked, and it was replaced — the record of why is the point of
this section.

The problem was that a fixed verb menu makes every new capability a protocol
change: an id, an argument schema, a CLI alias, a `describe` entry, an allowlist
entry. The two agent-launch verbs alone needed all five. Meanwhile an agent is
*already* good at reading type declarations and writing code against them, and
the editor already knew how to evaluate a plugin from a source string.

So the channel carries scripts:

- `ClientControl::RunScript { source }` → `ServerControl::ScriptResult { ok,
  error?, output? }`. The source is TypeScript; the target window is derived
  from the caller's token, never passed in.
- `fresh --cmd script run [FILE|-]` — source from a file or stdin (never argv: a
  script is multi-line and full of quotes). Whatever it returns is printed as
  JSON; a throw exits non-zero.
- `fresh --cmd script types` — prints the paths of `fresh.d.ts` (the whole
  `editor` API) and `plugins.d.ts` (every loaded plugin's exported API). This is
  the discovery verb: the surface is *documentation on disk*, not an RPC.

*Why it costs the runtime nothing.* The submitted source goes through the
existing load-plugin-from-source path (`init.ts`, "Load Plugin from Buffer"), so
transpilation, a per-script context and the full `editor` API are already
there. The host wraps the body in an async function that answers through
`editor.completeCommand` — an ordinary plugin API method — so the result travels
the route a plugin command's return value already took. There is no new eval
primitive.

*Reaching a plugin's own surface.* `editor.exportPluginApi(name, api)` /
`editor.getPluginApi(name)`, typed through the ambient `FreshPluginRegistry`
that each plugin's `.d.ts` emit augments. The orchestrator publishes
`runAgent()` / `newWorkspace()` there, so the headless twins of its two dialogs
are typed method calls rather than command ids taking a string map:

```ts
const orch = editor.getPluginApi("orchestrator");
return await orch.newWorkspace({ path: "/repo", agent: "claude", newBranch: "feat/x" });
```

Both still submit through the dialog's own path (`launchAgentInCurrentWorkspace`
/ `startPendingWorkspace`), so there is one launch pipeline with two front
doors. This is the general shape for the rest: a dialog that collects parameters
grows a headless twin by exporting a typed function, with no protocol work at
all.

*What a script cannot rely on.* The window it starts on is only the *starting*
window — the user keeps working, so after an `await` the active window may be
someone else's. `FRESH_WINDOW_ID` holds the token's window, and the
window-addressed API calls (`editor.windowPath`, `openFileInBackground`) take it
explicitly. That is advice, not enforcement; a deterministic window-targeting
API is the real fix and is still open (see below).

*What has no guards.* The runtime installs no interrupt handler or memory cap,
so an infinite loop in a script wedges the plugin thread exactly as a buggy
`init.ts` does. That is the blast radius already accepted for user-authored
plugin code; what makes it tolerable here is that the capability is opt-in per
launch. Bounding script execution is the natural next hardening step.

**Phase 3 — bidirectional + remote reach.** Add read-back (`ServerControl`
responses for active file / selection / workspace list) so an agent can act on
editor state, and propagate a session handle across remote wrappers (or add an
`env` option to `createWindowWithTerminal`) so agents in docker/ssh/k8s panes can
participate.

## Security model — per-workspace capability tokens

Script evaluation lets a child process drive the editor, so it is gated — but
**not** by Workspace Trust, which is a different axis (is this *repo* trusted).
Script authority is per *workspace/agent*, granted at creation, and enforced by
an **unforgeable capability token**:

- **Minting.** When the launcher creates a workspace, the server mints a random
  high-entropy id (128-bit / UUIDv4 — server-side only, nothing the agent
  supplies) and records it in a token table bound to `{ window, may_script }`.
- **Injection.** The token is stamped into the workspace's spawned process as
  its own env var, **`FRESH_CMD_TOKEN`**, distinct from `FRESH_SESSION`:
  - `FRESH_SESSION` = *addressing* — which editor's socket to reach.
  - `FRESH_CMD_TOKEN` = *authority + targeting* — the server maps it to a window
    and a grant.
- **Authorization.** The client presents `$FRESH_CMD_TOKEN` in the Hello. The
  server (a) resolves it to `{ window, may_script }`, (b) **derives the target
  window from the token** — so a token can never reach a sibling workspace and
  `RunScript` carries no window field — and (c) refuses outright unless the
  grant includes script access.
- **One flag, not a list.** The grant is binary on purpose. An allowlist of
  command ids would describe a boundary that isn't there: a script can call
  anything the plugin API exposes, including `editor.executeAction`. The honest
  reading of holding the grant is "may do anything the user could do from a
  plugin", and what actually bounds an agent is the window its token is bound
  to. A narrower-looking grant would be a lie told to whoever reads it.
- **Lifecycle.** Tokens are in-memory and per session: minted at create, revoked
  on teardown, re-minted (rotated) on resume. Same-user, unix-socket-local — not
  a network secret; the trust boundary is the pane, so any process the user runs
  in that workspace legitimately shares it.
- **Opt-in.** `allowScript` on `createTerminal` / `createWindowWithTerminal`
  decides whether a token is minted at all. Off by default: a standing
  editor-driving capability belongs only in terminals a caller explicitly grants
  it to, never in every spawned subprocess.

## Open design questions

- **Opt-in vs default.** Teaching every agent to drive the editor is powerful and
  a little surprising; Phase 1 keeps it behind a checkbox. Should Auto mode imply
  it, or stay independent? (Current lean: independent — approval posture and
  editor-control are different axes.)
- **Instruction-file hygiene.** For file-based injectors (codex/opencode/aider),
  where does the generated file live so it neither pollutes the repo nor collides
  with a user's real `AGENTS.md`? (Candidate: write into the session worktree,
  git-ignored, removed on teardown; or *append* a marked block and strip it.)
- **Blocking-open UX.** Until a non-blocking open form lands on the
  nested-forward path, the prompt must be explicit that `fresh <file>` parks the
  agent — otherwise an agent "opens a file to look at it" and hangs waiting for a
  human.
- **Bounding a script.** An interrupt handler with a deadline (and a memory cap)
  would turn a runaway script from "wedges the plugin thread" into a reported
  timeout. Open question is what the deadline should be, given a legitimate
  script can wait minutes on a workspace create.
- **Script ergonomics at scale.** `fresh.d.ts` is ~4300 lines. An agent can grep
  it, but a condensed cheat sheet — or a `script describe <symbol>` lookup —
  would spend far less of its context than the raw file.
- **aider.** Given it has no autonomous shell, it likely opts out of the whole
  feature (the dialog can hide the checkbox for it, the same way it hides Auto
  mode for opencode).
