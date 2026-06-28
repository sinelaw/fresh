# Terminal & Terminal Emulation

> _AI-generated: describes Fresh's architecture and design rationale, not implementation details; where it disagrees with the source, the source is authoritative._

Purpose: document Fresh's integrated terminal — PTY spawning, the embedded VT
emulation layer, the live/scrollback per-buffer model, mouse/link/title/clipboard
handling, and the Windows-specific VT input crate — marking what is implemented
vs planned.

> Orientation. Fresh does not ship a custom VT *output* emulator. VT100/ANSI
> parsing and grid management are delegated to the `alacritty_terminal` library
> (driven through its `vte` ANSI processor). The custom-built terminal code is
> the Windows VT *input* layer, the `fresh-winterm` crate — not output
> emulation. This document covers the library choice and the winterm rationale
> as they actually exist.

---

## 1. Component map

| Layer | Role |
|---|---|
| VT emulator wrapper | Wraps `alacritty_terminal`; grid → cells, scrollback streaming, OSC 7 sniffing, title capture |
| PTY lifecycle / threads | Spawns the PTY via `portable-pty`, owns reader/wait/writer threads |
| Key→bytes | crossterm `KeyEvent` → PTY escape bytes |
| Path-link detection | Text-only `path:line:col` detection in a grid line |
| Windows shell pick | Default shell selection on Windows |
| Per-buffer mode + actions | Mode switching, sync-to-buffer, respawn, render |
| Terminal input handler | Routes keys: capture / UI keybind / PTY |
| Mouse forwarding | Screen→grid coords, alt-screen forwarding |
| Ctrl+Click links | Resolve + open paths printed by the shell |
| Host terminal modes | crossterm raw/alt-screen/mouse/kitty setup + teardown |
| Host title (OSC 2) | Sets the *outer* terminal's window title |
| stdin spool | `cat x | fresh` streaming (not a PTY; see §11) |
| Windows VT input | `fresh-winterm`: `ReadConsoleInputW` VT-input reader, relay, size query |

The terminal module re-exports `TerminalManager`, `TerminalState`, and
`TerminalCell`.

---

## 2. Why `alacritty_terminal`, not a custom emulator

A correct VT/xterm emulator is a large, bug-prone surface (DEC private modes,
SGR, alt-screen, scrollback reflow, charsets, mouse modes). Fresh embeds
`alacritty_terminal` and feeds it bytes via its `vte` ANSI processor. The editor
only consumes the public grid/mode API:

- Grid read for render: a per-row read walks the grid at the current display
  offset and maps each cell to a `TerminalCell`.
- Mode flags it cares about: alt-screen, mouse tracking, SGR mouse,
  alternate-scroll, and application-cursor.
- DSR / cursor-report replies surface as PTY-write events and are queued back to
  the PTY.

Two things the library does **not** expose, handled by Fresh itself:

- **OSC 7 (cwd reports).** `alacritty_terminal`'s OSC dispatcher drops OSC 7, so
  a dedicated scanner sniffs `ESC ] 7 ; file://host/path` out of the raw byte
  stream before it reaches the emulator. Payload parsing handles `file://`,
  percent-escapes, and cross-OS absolute paths, so a Windows-host cwd parses on a
  Linux client and vice-versa.
- **Title (OSC 0/1/2).** Surfaces as title/reset-title events into a shared
  pending slot, drained into the title handling on each output-processing pass.

`portable-pty` supplies the cross-platform PTY including Windows ConPTY (with a
ConPTY/Windows-1809 hint surfaced on failure).

---

## 3. PTY spawning and the three threads

The PTY spawner builds a terminal as follows:

1. Open a PTY at the requested cols/rows via the native PTY system.
2. Build the shell command from the **active authority's `TerminalWrapper`**
   (`command`, `args`, `manages_cwd`). Local wraps shell detection;
   container / remote authorities re-parent the argv into `docker exec -w …`,
   `ssh … --`, etc. (see §10). `manages_cwd` wrappers skip both the working
   directory and the local `FRESH_SESSION` advert (their inner shell is on
   another host). Sets `TERM=xterm-256color`; applies the venv/direnv/mise
   environment delta first so `TERM`/`FRESH_SESSION` win.
3. Spawn the child; capture its process id and a killer handle.
4. Start three threads:
   - **Reader**: a `read()` loop → output processing (emulator + scrollback flush
     + DSR replies) → raw-log append → redraw notification (an async terminal-output
     message).
   - **Wait**: blocks on the child, firing the exit event **exactly once**. The
     reader deliberately does *not* fire exit, to avoid a racing
     `exit_code: None`.
   - **Writer**: owns the master, applies queued write/resize commands, and kills
     the child on shutdown.

A writer-thread command channel carries write/resize/shutdown. The terminal
handle holds shared `TerminalState`, the command sender, and an alive flag.

**Identity.** Terminal ids restart at 0 per `TerminalManager`, so output is
tagged with a `(window, terminal)` id to stay unambiguous across windows on the
async bus.

**Process-group signalling (Unix).** The shell is its own session leader inside
the PTY, so signalling the negated pid reaches forked subprocesses; the signal
mapping covers SIGTERM/KILL/INT/HUP and treats ESRCH as "nothing to signal".
Windows models only SIGKILL via shutdown.

**Tab auto-naming (Linux).** The foreground process name is read from the PTY
master's foreground process group (via `tcgetpgrp` + `/proc/<pgid>/comm`) — the
same signal tmux's `#{pane_current_command}` uses — so a tab can show `python3`
even with no OSC title.

---

## 4. Key encoding

Key encoding maps crossterm keys to PTY bytes: Ctrl+letter → control chars,
Alt+key → `ESC`+key, and full CSI/SS3 tables for arrows/Home/End/PgUp/F-keys.
Application-cursor mode (DECCKM) switches unmodified arrows to SS3 form, selected
from the terminal state at send time. Shift+Tab is emitted for both `Tab+SHIFT`
and the `BackTab` variant.

---

## 5. Incremental scrollback streaming (the core model)

Insight: scrollback is append-only, so the design avoids O(n) work on every mode
switch / session restore.

**Backing file** under the per-user data directory, keyed by workdir and terminal
id: append-only scrollback history (top) plus a rewritable visible-screen tail
(bottom, present only in scrollback mode). A history-end marker records where
history ends.

**State counters**:
- A count of physical history rows already streamed this epoch.
- A count of complete logical (unwrapped) lines streamed; invariant under width
  reflow, used to re-anchor after resize.
- A deferred-resync flag for a width resize during alt-screen, re-anchored on
  alt-screen exit.

**Flush** writes only logical lines that fully scrolled into history. Wrapped
rows are *rejoined* into one unwrapped logical line so the editor can re-wrap them
at any view width. SGR colors are threaded across wrapped rows as truecolor and
reset once per logical line.

**Resize reconciliation**: a pure height change leaves the streamed-history count
alone (a flush guard suppresses pulled-back rows; spilled rows stream as new). A
width change reflows persisted content, so a resync pass walks the reflowed
history counting logical lines back to the streamed-logical-line count. Stated
guarantee: never *lose* a line; bounded duplication (≤ screen height) is the
accepted trade-off.

**Epoch reset**: `ESC[3J` / `RIS` shrink history → counters reset to 0; new
output appends after existing file scrollback. Alt-screen's transient
zero-history is excluded.

A separate optional **raw `.log`** captures unfiltered PTY bytes for restore
replay.

---

## 6. Terminal as a buffer type; live vs scrollback

A terminal is a `BufferId` like any other and can sit in any split. Per-window
state lives in a map from `BufferId` to `TerminalBuffer`.

**Refactor (IMPLEMENTED):** live vs scrollback was *folded into per-buffer state*.
It replaced a free-floating "resume" set (presence-semantics, ambiguous,
bookkeeping smeared across creation sites) with an explicit interaction-mode
enum (`Live` / `Scrollback`) held on `TerminalBuffer` alongside the terminal id.
A constructor seeds `Live`. Accessors expose the terminal id and interaction
mode; a single writer sets the mode. Transitions: open/resume → Live;
Ctrl+Space / scroll-up / process-exit → Scrollback; close drops the whole record.
The backing-file/log maps stay keyed by `TerminalId` (genuine I/O-layer state).
Follow-up work moved per-record logic onto `TerminalBuffer` and restores a
terminal's remembered mode when it regains focus.

A separate per-window boolean tracks whether the *active* buffer is currently
being driven live, for input routing.

**Live → Scrollback** (`sync_terminal_to_buffer`): flush pending scrollback,
record the history-end byte, append the visible screen, then reload the backing
file as a read-only buffer (forced text mode so control bytes don't trip binary
detection). The viewport anchors to the history-end byte so exit is
pixel-identical to the last live tick.

**Scrollback → Live** (`enter_terminal_mode`): set mode Live, re-enable editing,
disable line-wrap, **truncate** the backing file back to the history-end marker
(dropping the appended visible screen), scroll to bottom, and resize PTYs. O(1).

**Input routing**: F9 toggles keyboard-capture (all keys → PTY). With capture off,
UI keybindings are checked first (`TerminalEscape` exits; split-nav exits
non-explicitly); Shift+PageUp enters scrollback mode. In scrollback, any plain
char / Enter / Tab / Backspace resumes live; nav keys scroll instead; Ctrl+Space,
`]`, and `` ` `` enter terminal mode.

---

## 7. Mouse handling

`send_terminal_mouse`:

- SGR vs X10 encoding chosen from the terminal's SGR-mouse mode.
- **Alternate-scroll** (wheel → arrow keys for pagers like `less`) is applied only
  when the program is **not** itself tracking the mouse. Alternate-scroll is on by
  default in alacritty, so a guard against mouse-aware programs prevents leaking
  synthesized arrows into full-screen programs (e.g. Claude Code's no-flicker
  mode) — mirroring xterm/alacritty.

Mouse forwarding only happens when in terminal mode **and** the buffer is in
**alternate screen** — i.e. full-screen programs own the mouse. Coordinates are
content-rect-relative. Crossterm button/event kinds map to Fresh's enums;
horizontal scroll is dropped.

---

## 8. Terminal links (Ctrl+Click)

A Ctrl+Left-click over a resolvable path opens it in Fresh, jumping to any
`:line:col`. Ctrl+hover underlines a path only if it resolves.

Detection: the live grid path is disabled in alt-screen so it never shadows a
program's clicks; the scrollback view maps screen position to buffer position.
Resolution order: absolute (after `~` expansion) → OSC 7 cwd → Fresh cwd;
existence is checked through the active authority's filesystem so it works on
remote/SSH hosts.

---

## 9. Title and host terminal modes

**Two distinct title concepts.** OSC 0/1/2 *from the embedded program* updates the
*buffer's* tab name. The host-title handling does the opposite: it sets the
**outer** host terminal's window title via OSC 2. It sanitizes control chars and
truncates on a char boundary; it is a no-op when stdout isn't a TTY. The format is
`<name> — <project> — Fresh`.

**Host modes**: centralizes raw-mode / alt-screen / mouse / kitty-keyboard /
bracketed-paste setup with tracked teardown (on undo and on drop). Notable
decisions:
- Alt-screen is entered **before** kitty keyboard flags so push/pop land on the
  same screen's stack.
- Kitty flags are pushed **optimistically** (no probe) — crossterm's detection has
  a multi-second timeout on common terminals (gnome/konsole/xterm/tmux).
- On Windows, crossterm's mouse-capture enable/disable is skipped — it replaces
  the whole console mode and writes no VT sequences; winterm handles it instead.
- Suspend/resume (SIGTSTP/`fg`) and an emergency cleanup on the panic hook are
  handled; sequence constants live in a dedicated module.

---

## 10. Authority / remote spawning and reconnect restore

Cross-ref: `AUTHORITY_DESIGN.md`, `PER_SESSION_BACKENDS_DESIGN.md`.

Terminal spawning routes through the single `Authority`, whose `TerminalWrapper`
is the only thing that differs per backend: local = shell detection
(`manages_cwd:false`); container/SSH/k8s wrap the argv as `docker exec …` /
`kubectl exec …` / `ssh … -- …` (`manages_cwd:true`). Authority transitions are
destructive — installing a new authority tears down all terminals; in practice
Fresh rebuilds the `Editor` (or, in daemon mode, the editor server swaps the
editor without disconnecting clients).

**Reconnect respawn** (`respawn_terminals_through_authority`): for each dead PTY,
re-spawn through the *current* authority, **reusing the same backing/log files**
so the new PTY appends to existing scrollback rather than starting blank. cwd and
size carry over from the dead handle. Argv precedence: agent-resume argv (when
agent resume is configured) → launch command → plain shell. The new PTY id is
remapped in place across all `TerminalId`-keyed maps while the buffer's
**remembered interaction mode is preserved**. On restore, the history-end marker
is seeded from existing file length so the first `enter_terminal_mode` doesn't
truncate scrollback to 0, and the backing writer opens in append mode when the
file has content. End-to-end tests cover both manual and automatic remote
reconnect.

---

## 11. Windows VT input crate (`fresh-winterm`)

Compiles to nothing off Windows. It encapsulates the Windows console "horror
story" — this is the genuinely *custom* terminal code.

- **VT input** — enabling VT input sets virtual-terminal-input and window-input
  console flags (deliberately **not** mouse-input — mouse arrives as VT sequences
  in key-event records — and **not** extended-flags, which would disable Quick
  Edit and leave it off after exit). Mouse tracking is requested via stdout
  sequences: cell-motion tracking by default (low-volume), all-motion tracking for
  hover. A dedicated reader thread reads `ReadConsoleInputW` into an input-record
  buffer, decoding UTF-16 surrogates, honoring repeat counts, and converting
  buffer-size and focus events. A periodic heartbeat re-asserts console mode if it
  drifts.
- **Corrupt-mouse strip** — under heavy all-motion movement the Windows console
  sporadically drops the leading `ESC` from SGR mouse sequences (confirmed via raw
  input-record dumps). The fix detects an SGR mouse body *without* a preceding
  `ESC` arriving in a single `ReadConsoleInputW` batch (a human couldn't type that
  in one batch) and discards it. The source documents the long list of approaches
  that did **not** work. See `windows-mouse-input.md`.
- **Relay** — a client/server relay loop. Raw VT bytes are forwarded straight to
  the server's data pipe (the server's input parser does all parsing, matching the
  Unix relay). It is decoupled from IPC via a relay-connection trait so winterm
  has no editor dependency. It drains reader events, forwards bytes/resizes, and
  polls size as a fallback.
- **Terminal size** — queried via `GetConsoleScreenBufferInfo`.

**Status note:** `windows-mouse-input.md` still lists a *planned* "drop ConPTY
self-hosting / fix direct VT input" plan and a repeat-count fix marked critical.
The code already honors repeat counts and the crate extraction is done, so those
items are largely IMPLEMENTED; treat the doc's plan section as partly historical.

---

## 12. OSC 52 clipboard (client/server)

`copy_to_system_clipboard` writes OSC 52 to stdout and/or sets arboard via a
**persistent static** clipboard handle — a temporary arboard handle would take
selection ownership (clobbering OSC 52) then drop it, leaving the clipboard empty.

In **session/daemon mode** the server's stdout is detached, so OSC 52 written
there vanishes. **IMPLEMENTED** fix: the server broadcasts a set-clipboard control
message; the client regenerates the clipboard locally using **both** OSC 52
(terminal) and arboard (X11/Wayland/macOS). Open items that remain unresolved:
multi-client semantics, OSC 52 payload caps, and whether copy should fully skip
the stdout write in session mode.

---

## 13. stdin streaming (related, not a PTY)

stdin streaming handles `cat big.log | fresh`: a background thread spools stdin to
a temp file and the buffer grows incrementally. The `StdinStream` type is pure
bookkeeping (active flag, growth recording, finished-thread outcome, completion);
the heavy lifting lives on `Editor`. Included for completeness — it shares the
"tail a growing file into a buffer" idea with terminal scrollback but uses no
emulator or PTY.

---

## 14. Implemented vs planned summary

- IMPLEMENTED: alacritty-backed emulation; `portable-pty` spawn + 3-thread model;
  incremental scrollback streaming with reflow re-anchor; per-buffer
  `TerminalBuffer` live/scrollback fold; OSC 7 cwd sniffing; Ctrl+Click links
  (live + scrollback); alt-screen mouse forwarding; alternate-scroll guard;
  embedded-program & host titles; `fresh-winterm` (VT input, corrupt-mouse strip,
  relay, size); OSC 52 set-clipboard for session mode; authority-routed spawning
  and reconnect respawn preserving scrollback + mode.
- PLANNED / historical: parts of `windows-mouse-input.md`'s ConPTY-removal plan;
  `osc52-client-server-analysis.md` open questions (multi-client, payload caps,
  session-mode stdout skip).
