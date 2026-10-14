# Native tmux Sessions in the Orchestrator

Purpose: say what "native tmux support" could mean for Fresh, what ships today,
and the two rungs above it — what each buys, what it costs, and which seam in
the code it lands on. Written as a decision aid, not a plan of record: only
§2 (level 0) describes shipped behaviour.

> tmux is the case worth solving first because it is the multiplexer people
> already run their agents in, and because it is the only one of the three
> Fresh scans (tmux, GNU screen, zellij) that exposes a machine-readable client
> protocol. §7 says what the others can and cannot do.

---

## 1. What "native" means here

Three different things get called native support, and they are rungs, not
alternatives:

| Level | What the user sees | Who draws the pane |
|---|---|---|
| 0 — **rejoin** (shipped) | Everything finds the session; Import opens a workspace whose terminal is an attached tmux client | tmux, inside one Fresh terminal |
| 1 — **adopt** | tmux sessions are already in the dock, with no scan and no import; entering one attaches | tmux, inside one Fresh terminal |
| 2 — **control mode** | one tmux *pane* per Fresh terminal view; Fresh's splits, scrollback, search and selection work on it | Fresh, from tmux's `%output` |

The user-visible ask — "show them as existing sessions, no import" — is level 1.
The thing usually meant by "tmux integration" in other terminals (iTerm2's
`tmux -CC`) is level 2.

---

## 2. Level 0 — rejoin (IMPLEMENTED)

The `agent-sessions` hub's tmux scanner walks `$TMUX_TMPDIR` (else `/tmp`) for
`tmux-<uid>/` sockets and, when the scan is allowed to run commands, asks each
live server for its sessions and panes. Each session comes back with its name,
`session_path`, whether a client is attached, and an attach argv qualified by
socket (`tmux -S <socket> attach -t <name>`) — a bare `tmux attach` cannot see a
server on a non-default socket. Panes whose `pane_current_command` matches a
known agent name are reported as agent rows, explicitly as a name match rather
than an identity.

The Everything dialog turns that into rows, and Import opens the New Workspace
form with the attach argv as the agent command. The result is a workspace whose
single terminal holds an attached tmux client.

What it does not do: nothing tells you a tmux session exists until you scan for
it, and the workspace's terminal is opaque — tmux draws the splits, so Fresh's
own splits, scrollback search, selection and links stop at that pane's edge.

---

## 3. Level 1 — tmux sessions as dock rows (PLANNED, recommended next)

The dock already has rows for things that exist outside Fresh and have no
window yet: **discovered worktrees**. They carry synthetic negative ids, no
terminal id, render a `· on-disk` tag rather than an activity pill, are
reconciled against the filesystem on a cadence, and "dive" by creating a window
rather than switching to one. A tmux session is the same shape of thing, and
should reuse that machinery rather than grow a parallel one:

- **Discovery.** Run the hub's tmux scanner against the local machine on the
  worktree-discovery cadence, and reconcile rows keyed by `socket/session-name`.
  A session already attached inside an open workspace is not offered twice —
  the same duplicate-suppression the worktree pass does by root.
- **Entering.** `createWindowWithTerminal` already takes a command; the dive is
  the New Workspace form's ending without the form, with the scanner's attach
  argv as the command and the session's `session_path` as the root.
- **Ordering.** Dock order is keyed by canonical root, which a tmux session may
  share with a worktree row or not have at all; these rows need the existing
  `orderKey` escape hatch so their slot does not move when they open.
- **Saying what they are.** The `· on-disk` tag has a sibling: `· tmux`, so a
  row that is somebody else's session reads as one.

Cost: plugin-only — the orchestrator plus the scanner already in the tree. No
host change, no new protocol.

Limit: it removes the import step, which is what was asked, but the pane stays
tmux's. Everything in §4 is still out of reach.

---

## 4. Level 2 — control mode (`tmux -CC`)

### 4.1 The protocol, in sketch

`tmux -C attach` puts the client in control mode: tmux stops drawing and speaks
a line protocol on stdout, taking commands on stdin. `-CC` additionally turns
off echo. Replies are framed by `%begin`/`%end` (or `%error`); the stream
between them is notifications — pane output as `%output %<pane> <escaped
bytes>`, plus lifecycle lines for windows, layouts, the session and the client,
and flow-control lines when `pause-after` is set. The client primes a pane with
`capture-pane`, writes input with `send-keys`, and tells tmux its size with
`refresh-client`. Exact spellings have moved across tmux versions — the man
page's CONTROL MODE section is the authority, and anything built here should
pin behaviour per version the way iTerm2 does.

### 4.2 What it buys

One tmux pane per Fresh terminal view. Fresh owns the layout (its own splits,
not tmux's), the scrollback and its search, selection and copy, links, mouse and
theme; tmux owns the processes, so they outlive the editor and stay reachable
from a plain `tmux attach` elsewhere. A tmux session becomes an orchestrator
workspace whose windows are its windows — the agent in pane 2 is a Fresh
terminal like any other, and the dock's activity signals (which read terminal
output) start working on sessions Fresh did not spawn.

### 4.3 Where it lands in the code

The emulator needs nothing: a terminal already consumes a byte stream, and
`%output` payloads are exactly that. The cost is in ownership. Today a terminal
is strictly PTY-backed — the terminal manager opens a PTY per terminal and runs
its reader, waiter and writer threads against it. Control mode inverts the
relationship: **one PTY (the `-CC` client) feeds N terminals**, and a
terminal's input goes back as a `send-keys` command rather than a write to its
own master.

So the seam is a terminal's *source and sink*, not its emulation:

1. A source abstraction over "my own PTY" and "a channel from a control-mode
   client", with the reader/waiter/writer trio behind it.
2. A control-mode client service owning the one PTY: parse the framing, route
   `%output` by pane id, translate window/layout notifications into
   terminal-lifecycle events, and serialise outbound commands.
3. Sizing: tmux sizes a window to its smallest attached client, so the client
   must report size, and a Fresh split that disagrees with tmux's layout has to
   pick one to honour — mirroring tmux's layout into Fresh splits is the most
   native answer and the most work.
4. Persistence: the workspace file records what to *reattach* to — socket plus
   session — instead of a command to respawn.

### 4.4 Risks

Flow control (a firehose pane and `%pause`/`%continue`), the escaping of
`%output` payloads, copy-mode and mouse ownership overlapping Fresh's own, and
tmux version drift. None is unknown territory — iTerm2 has run this for a
decade — but together they are a subsystem, not a feature.

---

## 5. Level 1.5 — control mode for liveness only

Worth naming because it is cheap: use a control-mode client (or plain
`list-sessions` polling) purely to keep the dock's tmux rows live — names,
window counts, attachment, create and kill from the dock — while rendering
stays a plain attached client in one terminal. It removes the polling cadence
from level 1 without any of §4.3.

---

## 6. Recommendation

Land level 1: it is what was asked for ("no import step"), it is plugin-only,
and it makes tmux sessions first-class in the dock. Treat level 2 as its own
project, gated on the terminal source abstraction in §4.3 — which is worth
having anyway, since a remote agent's terminal and a devcontainer's have the
same shape of problem.

---

## 7. The other multiplexers

- **zellij** has no control-mode equivalent. It is scriptable from outside
  (`zellij action …`) and extensible from inside (WASM plugins), but there is no
  protocol that hands a client the output stream, so level 2 is not available;
  levels 0 and 1 are, through the existing scanner.
- **GNU screen** has no listing protocol at all beyond its socket directory and
  `screen -ls`; level 1 is the ceiling.

This is why tmux is the one worth building depth for, and why levels 0 and 1
are written against the scanner interface rather than against tmux: the shallow
rungs should stay the same code for all three.
