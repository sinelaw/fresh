# Agent-aware session resume

Status: **Phases 0–2 shipped.** Phase 0 (terminal-command persistence) +
Phase 1 (core resume seam) + Phase 2 (Orchestrator registry, strategies A & B)
are implemented and tested. Phase 3 (New Session agent dropdown; broader
registry) is the remaining work. This document is the plan and the as-built
record.

## As built (what's in the tree)

- **Core seam:** `createWindowWithTerminal` takes a `resume` argv;
  `SerializedTerminalWorkspace.agent_resume { argv }` persists it;
  `restore_terminal_from_workspace` runs resume → launch command → shell, gated
  by `terminal.resume_agents` (default true). The session id is a plain argv
  element through the authority wrapper — no shell text, composes with remote
  authorities. Tests: `restored_agent_terminal.rs`.
- **Orchestrator registry** (`orchestrator.ts`, `AGENT_REGISTRY`): policy/data,
  matched by argv0 basename.
  - `claude` — **strategy A**: launch `claude … --session-id <uuid>`, resume
    `claude --resume <uuid>`; `--continue` available as fallback.
  - `aider` — **strategy B**: continue-only, resume `aider --restore-chat-history`.
  - Unknown commands (plain `terminal`/shell, custom agents) pass through with
    no resume — exactly the prior behaviour.
  Verified end-to-end interactively with a fake `claude`: launch carried the
  minted `--session-id`, and restore-on-dive ran `--resume <same uuid>`.

## Problem

The Orchestrator runs coding agents (claude, codex, opencode, copilot, aider,
pi, …) in per-worktree sessions. Each agent is just a terminal whose PTY child
is the agent CLI. On a cold restart / daemon restart / orchestrator rehydrate,
the live agent process is gone. We want the session to come back *useful*, and
for real agents that means resuming the actual conversation, not a fresh prompt.

## What already ships (Phase 0 — tier "re-run")

`SerializedTerminalWorkspace.command: Option<Vec<String>>` persists a session
terminal's spawn argv; `restore_terminal_from_workspace` re-runs it through the
authority's `TerminalWrapper` (commit 855f267 + test 3444232). So a restored
session comes back as its terminal re-running the *launch* command — for a plain
`terminal` agent that's a shell; for `claude` it's a fresh `claude`. This is the
floor every later tier degrades to.

It is **not** resume: a fresh `claude` has none of the prior conversation. That
gap is what the rest of this design closes.

## Hard constraints (decided)

1. **No agent-specific logic in Rust core.** Detection, the resume invocation,
   the per-agent flags — all live in *data*: a bundled, user-overridable
   registry (like LSP servers / grammars). Core only substitutes a value into an
   argv slot and runs it through the authority.
2. **Do not ship a binary into the authority.** No `fresh`/shim copied to the
   SSH host / container / pod.
3. **Do not read third-party agents' internal files.** No scraping
   `~/.claude/projects/**.jsonl` or any agent's private on-disk state. (A
   filesystem-scrape via the authority's `FileSystem` was considered — it needs
   no binary and crosses authorities for free — but reading another tool's
   undocumented internal state was ruled out.)
4. **Additive & reversible.** Absence of the new data == today's behaviour; old
   workspaces still load; every failure degrades to Phase 0 → backing-file.
5. **Safe.** Auto-resuming spends tokens/$$ and is a network side effect; any
   captured/derived id is untrusted and only ever a distinct argv element, never
   shell text. Opt-in by policy.

These constraints kill the whole "observe the running agent's native session
id" space (out-of-band socket needs reachability+binary; in-band OSC marker
needs agent cooperation; file-scrape reads internal state). So we **don't
observe the id — we either assign it or avoid needing it.**

## Chosen approach: A + B (drop C)

### A. Provision the id at launch (preferred where the agent supports it)

When the Orchestrator spawns an agent that accepts a caller-supplied session id,
Fresh mints a UUID and passes it in (e.g. `claude --session-id <uuid>`). Resume
is then `claude --resume <uuid>`. Fresh knew the id from birth, so:

- **no capture** — nothing to observe, no marker, no file reading, no binary;
- the id is **trusted by construction** and persisted at spawn time;
- works across every authority unchanged (it's just a different argv the
  authority already wraps).

Requires the agent to accept a caller-supplied id (a per-agent flag → data).
Agents that only mint their own id fall through to B.

### B. Resume-latest with an isolated config home (the broad default)

Most agent CLIs have `--continue` / `-c` ("resume the most recent session in
this directory"). The Orchestrator runs **one agent per worktree**, so "latest
in this cwd" is unambiguous. Resume = `["claude", "--continue"]` — no id, no
capture, no file reads. Harden the ambiguity edge by launching each agent with a
**per-session isolated config home** (e.g. `CLAUDE_CONFIG_DIR=<session dir>`,
set through the env we already inject via the authority's `EnvProvider`), so
"latest" is physically scoped to that pane without reading any contents.

### C. Ask the agent's public CLI — REJECTED

Running `claude sessions list` through `authority.process_spawner` and parsing
its output was an option for agents that are A- and B-incapable. Dropped: extra
process, drifty output parsing, and A+B already cover the supported agents.

## Mechanism vs. policy split

**Core (Rust), mechanism only** — extends Phase 0:

- Persist a per-terminal **resume spec** distinct from the launch `command`:
  the resolved resume argv + an env overlay (for B's isolated home). On restore,
  prefer `resume` → else `command` → else shell → else backing-file.
- A plugin op for the Orchestrator to set the resume spec when it provisions a
  session (or an option on `createWindowWithTerminal`).
- Substitution is **array-slot only**: `{session_id}` fills one `Vec` element;
  the id never touches a shell line.

**Orchestrator plugin (TS), policy/data** — a user-overridable agent registry:

```ts
registerAgent({
  id: "claude",
  match: { argv0: /(^|\/)claude$/ },
  // A: provision an id at launch, resume with it
  provision: { idFlag: "--session-id", resume: ["claude", "--resume", "{session_id}"] },
  // B: fallback when no id support — isolate + continue
  continue:  { env: { CLAUDE_CONFIG_DIR: "{session_home}" }, resume: ["claude", "--continue"] },
});
```

At `startNewSession`, the plugin matches the user's agent command, picks A or B,
provisions (mint UUID / set isolated home), spawns the launch argv, and hands
core the **resolved** resume spec to persist. Persisting the resolved argv makes
restore independent of plugin load order and registry drift.

## Persisted schema delta

```rust
struct SerializedTerminalWorkspace {
  // … existing, incl. Phase-0 `command: Option<Vec<String>>` …
  #[serde(default, skip_serializing_if = "Option::is_none")]
  agent_resume: Option<AgentResume>,
}
struct AgentResume {
  agent: String,                 // "claude" — display/dedupe only
  resume_argv: Vec<String>,      // RESOLVED (id already in its slot) — what we exec
  env: Vec<(String, String)>,    // B's isolated-home overlay, if any
  session_ref: Option<SessionRef>, // { kind: Id|None }; Id = the UUID we minted (A)
  authority: AuthorityRef,       // scope; never replay across hosts
  policy: ResumePolicy,          // Never | Confirm | Auto
  captured_at: u64,
}
```

Additive, serde-defaulted, reversible. `command` (Phase 0) remains the fallback
when `agent_resume` is absent or its resume fails.

## New Session dialog: agent dropdown (deferred — build later)

The "Agent Command" field becomes a **dropdown of known commands**: default
`terminal` (plain shell), plus an entry per registry agent that has a known
session mechanism (claude, codex, …), with free-text still allowed. Selecting a
known agent wires up its provision/continue templates automatically. This is UI
sugar over the registry above; spec it when the registry lands.

## Lifecycle & correctness

- **Dedupe:** resume a given `(authority, agent, session_ref)` at most once
  across panes.
- **Deferred launch:** resume on first dive (pane has a render rect + theme),
  not at startup — keeps it lazy, only spends tokens on sessions you reopen.
- **Failure → fallback:** resumed child exits fast/non-zero → fall back to
  Phase-0 re-run, surface a dismissible status. Detect via `terminal_exit`.
- **Policy:** master switch + per-resume `Never | Confirm | Auto`; default
  `Confirm` (arguably `Never` for v1 — auto-spending tokens is a side effect).

## Phased rollout

- **Phase 0 — DONE:** persist + re-run the launch `command`; degrade to
  backing-file. (commit 855f267, test 3444232.)
- **Phase 1 — DONE:** core resume-spec seam (persist a plugin-set resume argv;
  replay on restore) + deterministic tests.
- **Phase 2 — DONE:** Orchestrator `AGENT_REGISTRY` + provisioning for A
  (`claude`) and B (`aider`); `terminal.resume_agents` master switch (default
  true). Per-resume `Confirm`/`Never` policy is deferred — for these agents
  resume is no more a side effect than the already-shipped fresh re-run, so a
  master switch suffices for v1.
- **Phase 3 — agent dropdown DONE:** the New Session "Agent Command" field now
  carries an `Agent:` preset row — `[ terminal ] [ claude ↻ ] [ aider ↻ ]
  [ custom… ]`, built from `AGENT_REGISTRY`. `←/→` (mirroring the "Run in:"
  tabs) or a click selects a preset and fills the command; `custom…` hands
  focus to the always-editable field for a typed command; `↻` + the row hint
  signal which agents resume across restarts (local backend, where resume is
  wired). Remaining TODO: broader registry; remote-backend resume; staleness,
  authority-scope enforcement, per-resume confirm, and an env overlay on
  `agent_resume` for B's per-session config isolation.

## Situating it

Layer **2b (native resume)** on Fresh's existing **#3 (backing-file screenshot)**
and **#1 (detach/reattach)**: live process attached → reattach; else
`agent_resume` + policy → resume; else Phase-0 re-run; else backing-file.
Rejected: #4 CRIU/DMTCP (can't preserve the agent's live model-API socket), #5
reconstruct-from-transcript (that's the agent's own `--resume` job).

## Ways this breaks (all in *data*, fail soft to Phase 0)

| Break | Mitigation |
|---|---|
| Agent lacks `--session-id` (A) | fall to B (`--continue`) |
| Agent lacks `--continue` (B) | fall to Phase-0 re-run |
| Multiple agents in one cwd over time (B ambiguity) | isolated config home per session; dedupe |
| Agent CLI flag drift | registry is data, user-overridable; resolved argv persisted; non-zero exit → fallback |
| Stale config home / rotated id | `captured_at` + liveness check → fallback |
| Remote authority binary mismatch | `authority` scope; never assume cross-host portability; fallback |

## Open decisions

1. Default policy: `Confirm` vs `Never` for v1 (lean `Never`).
2. Resolve provision at launch (A, authoritative) — confirmed; B needs nothing
   captured.
3. Whether `createWindowWithTerminal` grows a `resume` option or a separate
   `setTerminalResumeSpec` op carries it.
