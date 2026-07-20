# Teaching launched agents to drive Fresh (the `fresh` CLI seam)

> _Forward-looking design, not the system as built. Describes intended work.
> The **Launcher** half (agent presets, resume, Start prompt, Auto mode) ships
> today; the **CLI-exposure** half is PLANNED and needs host protocol work._

Purpose: when the Orchestrator New-Workspace dialog launches a coding agent
(`claude`, `codex`, `opencode`, `aider`, …) into a workspace, we want the agent
to be able to *act on the surrounding editor* — open a file in the current
workspace, split the view, spawn a sibling workspace — by shelling out to
`fresh`, the same binary that is already in its `PATH`. This doc records what
that requires, what already works, and a phased plan, so the "give each agent a
system prompt that teaches it about the Fresh CLI" idea has a concrete target
rather than a hand-wave.

## Why `fresh` is the natural control surface

A launched agent already runs inside a Fresh terminal pane, which means:

- The `fresh` binary is on `PATH` (it is editor and client in one).
- Every local shell child inherits **`FRESH_SESSION`**, the name of the parent
  editor's control socket. So a bare `fresh <arg>` from inside a pane forwards
  to *this* editor instead of launching a second one.

That inheritance is the whole trick: the agent doesn't need credentials, a
socket path, or an SDK — it runs `fresh`, and the parent editor reacts. The only
question is *what verbs* `fresh` forwards, and *how the agent learns they exist*.

## What ships today (IMPLEMENTED)

The control channel (`ClientControl` over the per-session control socket) exposes
exactly two "do something in the editor" verbs, and the `fresh` binary's
nested-forward path already routes to them:

| Agent runs | Effect in the parent editor | Blocking? |
|---|---|---|
| `fresh path/to/file.rs` (also `path:line:col`, ranges) | Opens the file as a buffer in the **current** workspace | **Yes** — waits until the buffer is closed (the `$EDITOR` contract) |
| `fresh some/dir/` | Opens the directory as a **new focused workspace** | No |

So a "teach the agent about Fresh" system prompt could, *with no host changes*,
honestly document these two moves. Everything else below is missing.

## The gap (what an agent cannot do today)

- **No "split the current view."** `SplitHorizontal` / `SplitVertical` exist as
  internal actions and palette commands, but there is no `ClientControl` variant
  for them — they are unreachable over the socket.
- **No generic palette/action RPC.** An agent cannot ask the editor to run an
  arbitrary command id (new terminal, toggle a panel, jump to a diagnostic, …).
- **File open is blocking-only.** The nested-forward path always requests
  `wait: true`, so `fresh file` parks the agent until a human closes the buffer.
  There is no "open this and let me keep working" form on that path.
- **No read-back.** The agent cannot query the open files, active buffer,
  cursor, selection, or workspace list — `ServerControl` carries no state.
- **`createWindowWithTerminal` has no `env` option.** The launcher cannot stamp
  extra env (e.g. a marker, or a socket hint) onto the agent process at birth.
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

## Phased plan (PLANNED)

**Phase 1 — instruction only, no host changes.** Add an opt-in "Teach the agent
the Fresh CLI" affordance to the New-Workspace dialog (a per-agent checkbox,
sibling to Auto mode). When set, the launcher injects a short prompt documenting
the *two verbs that already work* (`fresh <file>` to open in this workspace,
`fresh <dir>` to spawn a workspace), via each agent's mechanism above. This is
useful immediately and reversible. It also surfaces the honest limitation: file
open blocks, so the prompt tells the agent to background it (`fresh file &`) or
prefer it for hand-offs, not mid-task peeks.

**Phase 2 — widen the wire protocol.** Add `ClientControl` variants and dispatch
in *both* control-socket handlers (the daemon server and the in-process/direct
handler, which must stay symmetric):

- `SplitView { direction }` → the split actions.
- `RunCommand { id, args? }` → a **whitelisted** subset of palette command ids
  (split, new terminal, new workspace, focus navigation). A whitelist, not
  arbitrary action dispatch, keeps the blast radius small.
- A non-blocking `OpenFiles { wait: false }` form reachable from the
  nested-forward path (today it hardcodes `wait: true`).

Expose these as stable `fresh` subcommands (e.g. `fresh split --vertical`,
`fresh cmd <id>`), so the system prompt can name durable commands rather than
socket internals. Extend the prompt to document them.

**Phase 3 — bidirectional + remote reach.** Add read-back (`ServerControl`
responses for active file / selection / workspace list) so an agent can act on
editor state, and propagate a session handle across remote wrappers (or add an
`env` option to `createWindowWithTerminal`) so agents in docker/ssh/k8s panes can
participate.

## Open design questions

- **Opt-in vs default.** Teaching every agent to drive the editor is powerful and
  a little surprising; Phase 1 keeps it behind a checkbox. Should Auto mode imply
  it, or stay independent? (Current lean: independent — approval posture and
  editor-control are different axes.)
- **Instruction-file hygiene.** For file-based injectors (codex/opencode/aider),
  where does the generated file live so it neither pollutes the repo nor collides
  with a user's real `AGENTS.md`? (Candidate: write into the session worktree,
  git-ignored, removed on teardown; or *append* a marked block and strip it.)
- **Whitelist scope.** Which palette commands are safe to expose to an agent over
  the socket? Splits and navigation are clearly safe; anything that writes files
  or changes config is not.
- **Blocking-open UX.** Until Phase 2's non-blocking form lands, the prompt must
  be explicit that `fresh <file>` parks the agent — otherwise an agent "opens a
  file to look at it" and hangs waiting for a human.
- **aider.** Given it has no autonomous shell, it likely opts out of the whole
  feature (the dialog can hide the checkbox for it, the same way it hides Auto
  mode for opencode).
