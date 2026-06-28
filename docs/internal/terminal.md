# Terminal & Terminal Emulation

Purpose: document Fresh's integrated terminal — PTY spawning, the embedded VT
emulator, the live/scrollback per-buffer model, mouse/link/title/clipboard
handling, and the Windows-specific VT input crate — at the code level, marking
what is implemented vs planned.

> Scope note / discrepancy flag. The task framing assumed a *custom* VT
> parser/emulator. The code does **not** ship one: VT100/ANSI parsing and grid
> management are delegated to the `alacritty_terminal` library (`term.rs:30-35`,
> `Cargo.toml:217`). The custom-built terminal code is the **Windows VT *input***
> layer (`crates/fresh-winterm/`) — not output emulation. This doc documents the
> library choice and the winterm rationale as they actually exist.

---

## 1. Component map

| Layer | Path | Role |
|---|---|---|
| VT emulator wrapper | `crates/fresh-editor/src/services/terminal/term.rs` | Wraps `alacritty_terminal`; grid → cells, scrollback streaming, OSC 7 sniffing, title capture |
| PTY lifecycle / threads | `.../services/terminal/manager.rs` | Spawns PTY via `portable-pty`, owns reader/wait/writer threads |
| Key→bytes | `.../services/terminal/pty.rs` | crossterm `KeyEvent` → PTY escape bytes |
| Path-link detection | `.../services/terminal/path_link.rs` | Text-only `path:line:col` detection in a grid line |
| Windows shell pick | `.../services/terminal/windows_shell.rs` | Default shell selection on Windows |
| Per-buffer mode + actions | `.../app/terminal.rs` | Mode switching, sync-to-buffer, respawn, render |
| Terminal input handler | `.../app/terminal_input.rs` | Routes keys: capture / UI keybind / PTY |
| Mouse forwarding | `.../app/terminal_mouse.rs` | Screen→grid coords, alt-screen forwarding |
| Ctrl+Click links | `.../app/terminal_link.rs` | Resolve + open paths printed by the shell |
| Host terminal modes | `.../services/terminal_modes.rs` | crossterm raw/alt-screen/mouse/kitty setup + teardown |
| Host title (OSC 2) | `.../services/terminal_title.rs` | Sets the *outer* terminal's window title |
| stdin spool | `.../app/stdin_stream.rs` | `cat x | fresh` streaming (not a PTY; see §11) |
| Windows VT input | `crates/fresh-winterm/` | `ReadConsoleInputW` VT-input reader, relay, size query |

`mod.rs:50-61` re-exports `TerminalManager`, `TerminalState`, `TerminalCell`.

---

## 2. Why `alacritty_terminal`, not a custom emulator

A correct VT/xterm emulator is a large, bug-prone surface (DEC private modes,
SGR, alt-screen, scrollback reflow, charsets, mouse modes). Fresh embeds
`alacritty_terminal` 0.25 (`Cargo.toml:217`) and feeds it bytes via its
`vte::ansi::Processor` (`term.rs:35,377`). The editor only consumes the public
grid/mode API:

- Grid read for render: `get_line()` walks `grid[Line(row - display_offset)]`
  and maps each `Cell` to a `TerminalCell` (`term.rs:553-599`).
- Mode flags it cares about: `ALT_SCREEN`, `MOUSE_*`, `SGR_MOUSE`,
  `ALTERNATE_SCROLL`, `APP_CURSOR` (`term.rs:687-716`).
- DSR / cursor-report replies surface as `Event::PtyWrite` and are queued back
  to the PTY (`term.rs:69-76`, drained at `manager.rs:773`).

Two things the library does **not** expose, handled by Fresh itself:

- **OSC 7 (cwd reports).** `alacritty_terminal`'s OSC dispatcher drops OSC 7, so
  `Osc7Scanner` sniffs `ESC ] 7 ; file://host/path (BEL|ST)` out of the raw byte
  stream before it reaches the emulator (`term.rs:99-186`, fed at
  `term.rs:371-375`). Payload parsing handles `file://`, percent-escapes, and
  cross-OS absolute paths (`term.rs:194-243`) so a Windows-host cwd parses on a
  Linux client and vice-versa.
- **Title (OSC 0/1/2).** Surfaces as `Event::Title`/`ResetTitle` into a shared
  pending slot, drained into `terminal_title` each `process_output`
  (`term.rs:81-92, 380-385`).

`portable-pty` 0.9 (`Cargo.toml:218`) supplies the cross-platform PTY incl.
Windows ConPTY (`manager.rs:489-510`, with a ConPTY/1809 hint on failure).

---

## 3. PTY spawning and the three threads

`TerminalManager::build_terminal` (`manager.rs:319-431`) is the spawn core:

1. `open_pty(cols,rows)` → `native_pty_system().openpty(...)`.
2. `build_shell_command(wrapper, cwd, env_delta)` (`manager.rs:518-572`) builds
   the `CommandBuilder` from the **active authority's `TerminalWrapper`**
   (`command`, `args`, `manages_cwd`). Local wraps `detect_shell()`; container /
   remote authorities re-parent the argv into `docker exec -w …`, `ssh … --`,
   etc. (see §10). `manages_cwd` wrappers skip both `cwd()` and the local
   `FRESH_SESSION` advert (their inner shell is on another host). Sets
   `TERM=xterm-256color`; applies the venv/direnv/mise `EnvDelta` first so
   `TERM`/`FRESH_SESSION` win (`manager.rs:545-553`, issue #2355).
3. Spawn the child; capture `process_id()` and a `clone_killer()`.
4. Three threads (`manager.rs:386-418`):
   - **Reader** (`ReaderLoop::run`, `manager.rs:707-760`): `read()` loop →
     `process_output` (emulator + scrollback flush + DSR replies) →
     `append_raw_log` → `notify_redraw` (an `AsyncMessage::TerminalOutput`).
   - **Wait** (`spawn_wait_thread`, `manager.rs:620-644`): blocks on
     `child.wait()`, fires `TerminalExited{exit_code}` **exactly once**. The
     reader deliberately does *not* fire exit to avoid a racing
     `exit_code: None`.
   - **Writer** (`spawn_writer_thread`, `manager.rs:649-687`): owns the master,
     applies queued `Write`/`Resize`, kills the child on `Shutdown`.

`TerminalCommand` (`manager.rs:36-43`) is the writer-thread channel.
`TerminalHandle` holds `Arc<Mutex<TerminalState>>` + `command_tx` + `alive`
(`manager.rs:46-72`).

**Identity.** Terminal ids restart at 0 per `TerminalManager`, so output is
tagged with `WindowTerminalId(window, terminal)` to stay unambiguous across
windows on the async bus (`manager.rs:227-240, 382`; test at `:920-941`).

**Process-group signalling (Unix).** The shell is its own session leader inside
the PTY, so `kill(-pid, sig)` reaches forked subprocesses; `signal()` maps
SIGTERM/KILL/INT/HUP and treats ESRCH as "nothing to signal"
(`manager.rs:165-195`). Windows models only SIGKILL via `shutdown()`
(`:201-208`).

**Tab auto-naming (Linux).** `foreground_process_name()` reads the PTY master's
foreground pgrp via `tcgetpgrp` + `/proc/<pgid>/comm` — the tmux
`#{pane_current_command}` signal, so a tab can show `python3` even with no OSC
title (`manager.rs:127-152`).

---

## 4. Key encoding (`pty.rs`)

`key_to_pty_bytes(code, modifiers, app_cursor)` maps crossterm keys to PTY
bytes: Ctrl+letter→control chars, Alt+key→`ESC`+key, full CSI/SS3 tables for
arrows/Home/End/PgUp/F-keys. `app_cursor` (DECCKM) switches unmodified arrows to
SS3 `ESC O A` (`pty.rs:99-150`) — picked from `state.is_app_cursor()` at
`terminal.rs:1467`. Shift+Tab is emitted for both `Tab+SHIFT` and the `BackTab`
variant (issue #2029 sub-bug 2, `pty.rs:66-80`).

---

## 5. Incremental scrollback streaming (the core model)

Documented in `mod.rs:1-49`. Insight: scrollback is append-only, so avoid O(n)
work on every mode switch / session restore.

**Backing file** at
`~/.local/share/fresh/terminals/{workdir}/fresh-terminal-{id}.txt`: append-only
scrollback history (top) + a rewritable visible-screen tail (bottom, present
only in scrollback mode). `backing_file_history_end` marks where history ends.

**State counters** (`term.rs:280-308`):
- `synced_history_lines` — physical history rows already streamed this epoch.
- `synced_logical_lines` — complete logical (unwrapped) lines streamed; invariant
  under width reflow, used to re-anchor after resize.
- `pending_reflow_resync` — a width resize during alt-screen; re-anchor deferred
  to alt-screen exit.

**Flush** (`flush_new_scrollback`, `term.rs:733-768`): writes only logical lines
that fully scrolled into history. Wrapped rows (`WRAPLINE`) are *rejoined* into
one unwrapped logical line so the editor can re-wrap them at any view width
(test `:1217-1236`). SGR colors are threaded across wrapped rows as truecolor
and reset once per logical line (`term.rs:861-942`).

**Resize reconciliation** (`term.rs:434-485`): pure height change leaves
`synced_history_lines` alone (flush guard suppresses pulled-back rows; spilled
rows stream as new). Width change reflows persisted content, so
`resync_after_reflow` walks the reflowed history counting logical lines back to
`synced_logical_lines`. Stated guarantee: never *lose* a line; bounded
duplication (≤ screen height) is the accepted trade-off (tests `:1130-1394`).

**Epoch reset**: `ESC[3J` / `RIS` shrink history → counters reset to 0; new
output appends after existing file scrollback (`term.rs:403-409`, test
`:1290-1313`). Alt-screen's transient zero-history is excluded (`:1318-1350`).

A separate optional **raw `.log`** captures unfiltered PTY bytes for restore
replay (`manager.rs:807-817`).

---

## 6. Terminal as a buffer type; live vs scrollback

A terminal is a `BufferId` like any other and can sit in any split. Per-window
state lives in `Window::terminal_buffers: HashMap<BufferId, TerminalBuffer>`
(`window/mod.rs:346`).

**Recent refactor (IMPLEMENTED, commit `daa544a87`, 2026-06-25):** live vs
scrollback was *folded into per-buffer state*. It replaced a free-floating
`terminal_mode_resume: HashSet<BufferId>` (presence-semantics, ambiguous,
bookkeeping smeared across creation sites) with:

```
enum TerminalInteractionMode { Live, Scrollback }      // window/mod.rs:84-89
struct TerminalBuffer { terminal_id, mode }            // window/mod.rs:95-99
```

`TerminalBuffer::new_live` seeds Live (`:103-108`). Accessors:
`get_terminal_id`, `terminal_interaction_mode`, `is_live_terminal`,
`set_terminal_interaction_mode` (sole writer, `:2289-2295`). Transitions:
open/resume→Live; Ctrl+Space / scroll-up / process-exit→Scrollback; close drops
the whole record. The backing-file/log maps stay keyed by `TerminalId` (genuine
I/O-layer state). Follow-up `cbe58edf9` moved per-record logic onto
`TerminalBuffer`; `f567340f4` restores a terminal's remembered mode when it
regains focus.

`Window::terminal_mode` (a separate bool) tracks whether the *active* buffer is
currently being driven live for input routing (`terminal.rs:1362`).

**Live → Scrollback** (`sync_terminal_to_buffer`, `terminal.rs:1644-1733`):
flush pending scrollback, record `history_end_byte`, `append_visible_screen`,
then reload the backing file as a read-only buffer (forced text mode so control
bytes don't trip binary detection — #2449, `:1699`). Viewport anchors to
`history_end_byte` so exit is pixel-identical to the last live tick.

**Scrollback → Live** (`enter_terminal_mode`, `terminal.rs:1352-1417`):
set mode Live, re-enable editing, disable line-wrap, **truncate** the backing
file back to `backing_file_history_end` (drops the appended visible screen),
`scroll_to_bottom`, resize PTYs. O(1).

**Input routing** (`terminal_input.rs`): F9 toggles keyboard-capture (all keys →
PTY). Capture off → UI keybindings checked first (`TerminalEscape` exits;
split-nav exits non-explicitly). Shift+PageUp → `EnterScrollbackMode`.
`should_enter_terminal_mode` (`:92-114`): in scrollback, any plain char / Enter /
Tab / Backspace resumes live (issue #863); nav keys scroll instead;
Ctrl+Space/`]`/`` ` `` enter terminal mode.

---

## 7. Mouse handling

`Window::send_terminal_mouse` (`terminal.rs:1479-1546`):

- SGR vs X10 encoding chosen from `state.uses_sgr_mouse()`.
- **Alternate-scroll** (wheel→arrow keys for pagers like `less`) is applied only
  when the program is **not** itself tracking the mouse. `ALTERNATE_SCROLL` is on
  by default in alacritty, so the `wants_mouse_events()` guard prevents leaking
  synthesized arrows into mouse-aware full-screen programs (e.g. Claude Code's
  no-flicker mode) — mirrors xterm/alacritty (`:1494-1535`).

`terminal_mouse.rs`: `try_forward_mouse_to_terminal` only forwards when in
terminal mode **and** the buffer is in **alternate screen** (`:20-41`) — i.e.
full-screen programs own the mouse. Coordinates are content-rect-relative
(`:200-230`). Crossterm `MouseButton`/`MouseEventKind` map to Fresh's enums;
horizontal scroll is dropped (`:212-240`).

---

## 8. Terminal links (Ctrl+Click)

`terminal_link.rs`: a Ctrl+Left-click over a resolvable path opens it in Fresh,
jumping to any `:line:col` (`try_open_terminal_link`, `:25-57`). Ctrl+hover
underlines a path only if it resolves (`update_terminal_link_hover`, `:64-100`).

Detection (`terminal_mouse.rs`): live grid via `detect_terminal_link_at`
(`:53-85`) — disabled in alt-screen so it never shadows a program's clicks;
scrollback view via `detect_terminal_scrollback_link_at` (`:98-175`) mapping
screen→buffer position. Resolution order: absolute (after `~` expand) → OSC 7
cwd → Fresh cwd; existence checked through `authority().filesystem` so it works
on remote/SSH hosts (`terminal_link.rs:109-127`).

---

## 9. Title and host terminal modes

**Two distinct title concepts.** OSC 0/1/2 *from the embedded program* updates
the *buffer's* tab name (`term.rs:81-92`). `terminal_title.rs` does the
opposite: it sets the **outer** host terminal's window title via OSC 2
(`ESC ] 2 ; <title> BEL`, `:65-75`). Sanitizes control chars and truncates to
256 bytes on a char boundary (`:21-33`); no-op when stdout isn't a TTY
(`:66`). Format `<name> — <project> — Fresh` (`:46-53`).

**Host modes** (`terminal_modes.rs`): centralizes raw-mode / alt-screen / mouse /
kitty-keyboard / bracketed-paste setup with tracked teardown (`undo()`,
`Drop`). Notable decisions:
- Alt-screen entered **before** kitty keyboard flags so push/pop land on the
  same screen's stack (`:155-167`).
- Kitty flags pushed **optimistically** (no probe) — `crossterm`'s detection has
  a 2 s timeout on common terminals (gnome/konsole/xterm/tmux) (`:170-205`).
- On Windows, crossterm `EnableMouseCapture`/`DisableMouseCapture` are skipped —
  they replace the whole console mode and write no VT sequences; winterm handles
  it instead (`:207-228, 250-258`).
- `suspend_and_resume` (SIGTSTP/`fg`, `:338-356`) and `emergency_cleanup` (panic
  hook, `:364-389`). Sequence constants live in `mod sequences` (`:33-63`).

---

## 10. Authority / remote spawning and reconnect restore

Cross-ref: `AUTHORITY_DESIGN.md`, `PER_SESSION_BACKENDS_DESIGN.md`. (The task
named `remote-authority-trust.md`; **no such file exists** — that content was
split into the two files above. Flagging for reference cleanup.)

Terminal spawning routes through the single `Authority`, whose
`TerminalWrapper` (`manager.rs:327`) is the only thing that differs per backend:
local = `detect_shell()` (`manages_cwd:false`); container/SSH/k8s wrap the argv
as `docker exec …` / `kubectl exec …` / `ssh … -- …` (`manages_cwd:true`).
Authority transitions are destructive — installing a new authority tears down
all terminals; in practice Fresh rebuilds the `Editor` (or, in daemon mode,
`EditorServer.rebuild_editor` swaps without disconnecting clients).

**Reconnect respawn** (`respawn_terminals_through_authority`,
`terminal.rs:829-935`): for each dead PTY, re-spawn through the *current*
authority, **reusing the same backing/log files** so the new PTY appends to
existing scrollback rather than starting blank (`:853-856`). cwd/size carry over
from the dead handle. Argv precedence: agent-resume argv (if
`config.terminal.resume_agents`) → launch command → plain shell (`:858-875`).
The new PTY id is remapped in place across all `TerminalId`-keyed maps while the
buffer's **remembered interaction mode is preserved** (`:898-919`). On restore,
`manager.rs:354-362` seeds `backing_file_history_end` from existing file length
so the first `enter_terminal_mode` doesn't truncate scrollback to 0; the backing
writer opens in append mode when the file has content (`:592-615`). E2E tests:
`tests/e2e/remote_reconnect_terminal.rs`,
`remote_auto_reconnect_terminal.rs`.

---

## 11. Windows VT input crate (`fresh-winterm`)

Compiles empty off-Windows (`lib.rs:11-29`). Encapsulates the Windows console
"horror story" — this is the genuinely *custom* terminal code.

- **`vt_input.rs`** — `enable_vt_input()` sets
  `ENABLE_VIRTUAL_TERMINAL_INPUT | ENABLE_WINDOW_INPUT` (deliberately **not**
  `ENABLE_MOUSE_INPUT` — mouse arrives as VT sequences in KEY_EVENT records —
  and **not** `ENABLE_EXTENDED_FLAGS`, which would disable Quick Edit and leave
  it off after exit) (`:34-65`). Mouse tracking via stdout sequences: default
  CellMotion (1002) low-volume, AllMotion (1003) for hover (`:73-105`).
  `VtInputReader` is a dedicated thread reading `ReadConsoleInputW` into a 16384
  `INPUT_RECORD` buffer, decoding UTF-16 surrogates, honoring `wRepeatCount`,
  and converting WINDOW_BUFFER_SIZE/FOCUS events (`:206-336`). A 30 s heartbeat
  re-asserts console mode if it drifts (`:215-232`; microsoft/terminal#19674).
- **`strip_corrupt_mouse`** (`:345-433`) — under heavy mode-1003 movement the
  Windows console sporadically drops the leading `ESC` from SGR mouse sequences
  (confirmed via raw INPUT_RECORD dumps). The fix detects
  `[<d;d;d[Mm]` *without* a preceding `ESC` arriving in a single
  `ReadConsoleInputW` batch (a human couldn't type that in one batch) and
  discards it. The header documents the long list of approaches that did **not**
  work. See `windows-mouse-input.md`.
- **`relay.rs`** — client/server relay loop. Raw VT bytes are forwarded straight
  to the server's data pipe (the server's `InputParser` does all parsing,
  matching the Unix relay). Decoupled from IPC via the `RelayConnection` trait
  so winterm has no editor dependency (`:25-47`). Drains reader events, forwards
  bytes/resizes, polls size as a fallback (`:76-150`).
- **`terminal_size.rs`** — size via `GetConsoleScreenBufferInfo` (`:17-28`).

**Status note:** `windows-mouse-input.md` still lists a *planned* "Drop ConPTY
self-hosting / fix direct VT input" plan and a `wRepeatCount` fix marked
critical. The code already honors `wRepeatCount` (`vt_input.rs:266`) and the
crate extraction is done, so those items are largely IMPLEMENTED; treat the
doc's plan section as partly historical.

---

## 12. OSC 52 clipboard (client/server)

`copy_to_system_clipboard(text, use_osc52, use_system_clipboard)`
(`clipboard.rs:123-139`): writes OSC 52 to stdout and/or sets arboard via a
**persistent static** `SYSTEM_CLIPBOARD` handle (`:106-111`) — a temporary
arboard handle would take selection ownership (clobbering OSC 52) then drop it,
leaving the clipboard empty (`:120-122`).

In **session/daemon mode** the server's stdout is detached, so OSC 52 written
there vanishes. **IMPLEMENTED** fix (per `osc52-client-server-analysis.md`,
"Chosen Approach"): the server broadcasts a `ServerControl::SetClipboard{text}`
control message; the client regenerates the clipboard locally using **both** OSC
52 (terminal) and arboard (X11/Wayland/macOS). Wiring confirmed in
`server/protocol.rs`, `server/editor_server.rs`, `client/relay_unix.rs`,
`client/mod.rs`. Open items the doc leaves unresolved: multi-client semantics,
≈100 KB OSC 52 payload caps, and whether `copy()` should fully skip the stdout
write in session mode.

---

## 13. stdin streaming (related, not a PTY)

`stdin_stream.rs` handles `cat big.log | fresh`: a background thread spools stdin
to a temp file and the buffer grows incrementally. `StdinStream` is pure
bookkeeping (`is_active`, `record_growth`, `take_finished_thread_outcome`,
`mark_complete`, `:50-140`); the heavy lifting lives on `Editor`. Included for
completeness — it shares the "tail a growing file into a buffer" idea with
terminal scrollback but uses no emulator or PTY.

---

## 14. Implemented vs planned summary

- IMPLEMENTED: alacritty-backed emulator; `portable-pty` spawn + 3-thread model;
  incremental scrollback streaming with reflow re-anchor; per-buffer
  `TerminalBuffer` live/scrollback fold; OSC 7 cwd sniffing; Ctrl+Click links
  (live + scrollback); alt-screen mouse forwarding; alternate-scroll guard;
  embedded-program & host titles; `fresh-winterm` (VT input, corrupt-mouse strip,
  relay, size); OSC 52 `SetClipboard` for session mode; authority-routed spawning
  and reconnect respawn preserving scrollback + mode.
- PLANNED / historical: parts of `windows-mouse-input.md`'s ConPTY-removal plan;
  `terminal.md`'s implementation checklist reads as the original (now largely
  shipped) design; `osc52-client-server-analysis.md` open questions
  (multi-client, payload caps, session-mode stdout skip).
- DISCREPANCIES FLAGGED: no custom VT *output* emulator (it's a library);
  `remote-authority-trust.md` does not exist (now `AUTHORITY_DESIGN.md` +
  `PER_SESSION_BACKENDS_DESIGN.md`).
