//! Unix TTY input reader.
//!
//! Reads raw bytes from stdin and turns them into crossterm events via fresh's
//! own [`InputParser`] state machine, instead of relying on crossterm's
//! built-in event parser.
//!
//! # Why
//!
//! crossterm's parser desyncs on mouse-tracking reports that are split across
//! `read()` boundaries or are out-of-spec, dumping the sequence remainder as
//! literal key events — which fresh then forwards verbatim to a focused
//! embedded terminal's child pty (sinelaw/fresh#2745). Routing host input
//! through `InputParser` — the same parser the session server and the Windows
//! VT-input path already use — makes that leak structurally impossible:
//! control-sequence bytes are never emitted as text.
//!
//! # What this owns vs. crossterm
//!
//! crossterm still drives *output* (raw mode, the ratatui backend, mouse-
//! capture / bracketed-paste / keyboard-enhancement DECSET writes). Only the
//! *input* side moves here. Focus (`ESC[I`/`O`) and bracketed paste
//! (`ESC[200~`…`201~`) arrive in the byte stream and are decoded by
//! `InputParser`; terminal resizes do not, so we install our own `SIGWINCH`
//! handler and synthesize [`CrosstermEvent::Resize`].

use std::collections::VecDeque;
use std::os::unix::io::{AsRawFd, BorrowedFd, RawFd};
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::Duration;

use crossterm::event::{Event as CrosstermEvent, MouseEventKind};
use fresh_input_parser::InputParser;

/// Default grace for a buffered lone `ESC`, when no configured value is given
/// (tests, and callers that predate the config plumbing).
///
/// Matches `editor.keyboard_escape_time_ms`; see that setting for why 50ms.
pub const DEFAULT_ESC_GRACE: Duration = Duration::from_millis(50);

/// Set to true by the `SIGWINCH` handler; consumed by [`TtyReader::take_resize`].
static SIGWINCH_PENDING: AtomicBool = AtomicBool::new(false);

/// True while a [`TtyReader`] owns stdin. Lets `coalesce_mouse_moves` know it
/// must not also poke crossterm's global reader (which would race us on fd 0).
static RAW_INPUT_ACTIVE: AtomicBool = AtomicBool::new(false);

/// Whether host input is being read by a [`TtyReader`] (rather than crossterm).
pub fn raw_input_active() -> bool {
    RAW_INPUT_ACTIVE.load(Ordering::Relaxed)
}

extern "C" fn handle_sigwinch(_: libc::c_int) {
    SIGWINCH_PENDING.store(true, Ordering::Relaxed);
}

/// Install a `SIGWINCH` handler that flags a pending resize. Deliberately does
/// NOT set `SA_RESTART`, so a `SIGWINCH` interrupts an in-progress `poll()`
/// (returning `EINTR`) and the resize is surfaced promptly rather than after
/// the next unrelated input or timeout.
fn install_sigwinch_handler() {
    // SAFETY: the handler only stores into an `AtomicBool`, which is
    // async-signal-safe. `sigaction` with a zeroed `sa_mask` and no flags is a
    // standard handler installation.
    unsafe {
        let mut sa: libc::sigaction = std::mem::zeroed();
        sa.sa_sigaction = handle_sigwinch as usize;
        sa.sa_flags = 0;
        libc::sigemptyset(&mut sa.sa_mask);
        libc::sigaction(libc::SIGWINCH, &sa, std::ptr::null_mut());
    }
}

/// Poll a single fd for readability. Returns `true` if readable, `false` on
/// timeout or `EINTR` (e.g. a `SIGWINCH`, whose pending resize the caller then
/// picks up via [`TtyReader::take_resize`]).
fn poll_readable(fd: RawFd, timeout: Duration) -> bool {
    use nix::poll::{poll, PollFd, PollFlags, PollTimeout};
    // SAFETY: fd is stdin, valid for the duration of the poll call.
    let borrowed = unsafe { BorrowedFd::borrow_raw(fd) };
    let mut fds = [PollFd::new(borrowed, PollFlags::POLLIN)];
    let timeout_ms = timeout.as_millis().min(u16::MAX as u128) as u16;
    match poll(&mut fds, PollTimeout::from(timeout_ms)) {
        Ok(n) if n > 0 => fds[0]
            .revents()
            .is_some_and(|r| r.contains(PollFlags::POLLIN)),
        _ => false,
    }
}

/// Streaming reader that converts raw stdin bytes into crossterm events.
pub struct TtyReader {
    parser: InputParser,
    queue: VecDeque<CrosstermEvent>,
    stdin_fd: RawFd,
    /// How long a lone `ESC` may wait for a continuation before it resolves to
    /// the Escape key. Only consulted while the terminal has *not* confirmed
    /// unambiguous Escape encoding.
    esc_grace: Duration,
}

impl TtyReader {
    /// Install the `SIGWINCH` handler and take ownership of stdin input, using
    /// the default escape grace.
    pub fn new() -> Self {
        Self::with_escape_grace(DEFAULT_ESC_GRACE)
    }

    /// As [`TtyReader::new`], with the grace from
    /// `editor.keyboard_escape_time_ms`.
    pub fn with_escape_grace(esc_grace: Duration) -> Self {
        install_sigwinch_handler();
        RAW_INPUT_ACTIVE.store(true, Ordering::Relaxed);
        Self {
            parser: InputParser::new(),
            queue: VecDeque::new(),
            stdin_fd: std::io::stdin().as_raw_fd(),
            esc_grace,
        }
    }

    /// Return a pending resize event if a `SIGWINCH` fired since last checked.
    pub fn take_resize(&self) -> Option<CrosstermEvent> {
        if SIGWINCH_PENDING.swap(false, Ordering::Relaxed) {
            crossterm::terminal::size()
                .ok()
                .map(|(cols, rows)| CrosstermEvent::Resize(cols, rows))
        } else {
            None
        }
    }

    /// Pop the next already-decoded event, if any.
    pub fn next_buffered(&mut self) -> Option<CrosstermEvent> {
        self.queue.pop_front()
    }

    /// Read whatever bytes are pending on stdin and feed them through the
    /// parser. The caller must have observed the fd readable; because stdin is
    /// in raw mode with at least one byte available, the `read` returns
    /// promptly without blocking.
    ///
    /// A lone trailing `ESC` is ambiguous — the Escape key, or the head of a
    /// sequence split across reads. If a continuation arrives within the escape
    /// grace it is pulled in so the sequence completes as one event; otherwise
    /// the `ESC` is left *buffered* (not emitted) and only resolved as the
    /// Escape key by [`flush_pending_escape`](Self::flush_pending_escape) once
    /// stdin actually goes idle. Flushing it here — as a previous version did on
    /// grace expiry — tore a slowly-split control sequence into an Escape key
    /// followed by its remainder as literal keystrokes, which fresh then
    /// forwarded verbatim into a focused embedded terminal
    /// (sinelaw/fresh#2793).
    pub fn drain_stdin(&mut self) {
        while self.read_once() {
            if !self.parser.flush_pending() || !poll_readable(self.stdin_fd, self.esc_grace) {
                break;
            }
        }
    }

    /// Resolve a buffered lone `ESC` as the Escape key press (or release a `[`
    /// held after an earlier flush), queueing the event. A no-op when nothing is
    /// pending.
    ///
    /// The caller invokes this only when stdin has gone idle — a blocking
    /// [`poll`](Self::poll) that timed out with no further bytes. Deferring the
    /// decision to here rather than to the end of every
    /// [`drain_stdin`](Self::drain_stdin) widens the window in which a split
    /// sequence still reassembles from one grace period to two.
    ///
    /// It does **not** eliminate the leak, and earlier revisions of this comment
    /// wrongly claimed it did: a continuation that arrives after both windows
    /// have elapsed still finds the `ESC` gone. That is inherent to the legacy
    /// encoding — `0x1b` is both the Escape key and a sequence prefix, so every
    /// implementation guesses on a timer (tmux `escape-time`, Neovim
    /// `ttimeoutlen`). What bounds the damage is elsewhere: the parser resyncs a
    /// mouse report whose `ESC` was flushed instead of spraying its bytes as
    /// keystrokes, and on terminals that confirm the kitty protocol's
    /// disambiguate mode the guess never happens at all
    /// (`InputParser::set_escape_unambiguous`). See sinelaw/fresh#2793.
    pub fn flush_pending_escape(&mut self) {
        for ev in self.parser.flush() {
            self.push_coalesced(ev);
        }
    }

    /// One `read()` + parse pass. Returns whether any bytes were read.
    fn read_once(&mut self) -> bool {
        let mut buf = [0u8; 4096];
        // SAFETY: reading into a stack buffer we own, length-bounded.
        let n = unsafe {
            libc::read(
                self.stdin_fd,
                buf.as_mut_ptr() as *mut libc::c_void,
                buf.len(),
            )
        };
        if n <= 0 {
            return false;
        }
        let events = self.parser.parse(&buf[..n as usize]);
        for ev in events {
            self.push_coalesced(ev);
        }
        self.adopt_keyboard_flags_reply();
        true
    }

    /// Act on a kitty keyboard-flags reply (`CSI ? <flags> u`) if one arrived in
    /// the bytes just parsed — the answer to the `CSI ? u` query
    /// `TerminalModes::enable` sends after pushing its enhancement flags.
    ///
    /// Bit 0 is "disambiguate escape codes". When the terminal confirms it, the
    /// Escape key arrives as `CSI 27 u`, so a bare `0x1b` is *always* the head of
    /// a sequence and must never be resolved on a timer; telling the parser so
    /// retires the guess entirely on those terminals (sinelaw/fresh#2793). A
    /// reply with the bit clear (or no reply at all, from a terminal that
    /// ignored both the push and the query) leaves the timer in charge.
    fn adopt_keyboard_flags_reply(&mut self) {
        let Some(flags) = self.parser.take_keyboard_flags_reply() else {
            return;
        };
        let unambiguous = flags & 0b1 != 0;
        if unambiguous != self.parser.escape_unambiguous() {
            tracing::info!(
                "Terminal reported keyboard flags {flags:#b}; \
                 Escape is {}ambiguous, escape timer {}",
                if unambiguous { "un" } else { "" },
                if unambiguous { "retired" } else { "in use" },
            );
        }
        self.parser.set_escape_unambiguous(unambiguous);
    }

    /// Queue an event, collapsing a run of mouse-move events down to the latest
    /// one (a motion flood produces one Moved event per read batch), matching
    /// the coalescing the crossterm path did in `coalesce_mouse_moves`.
    fn push_coalesced(&mut self, ev: CrosstermEvent) {
        if let CrosstermEvent::Mouse(m) = &ev {
            if m.kind == MouseEventKind::Moved {
                if let Some(CrosstermEvent::Mouse(last)) = self.queue.back() {
                    if last.kind == MouseEventKind::Moved {
                        *self.queue.back_mut().expect("back() was Some") = ev;
                        return;
                    }
                }
            }
        }
        self.queue.push_back(ev);
    }

    /// Blocking (up to `timeout`) read of the next event, or `None` on timeout.
    pub fn poll(&mut self, timeout: Duration) -> anyhow::Result<Option<CrosstermEvent>> {
        if let Some(ev) = self.next_buffered() {
            return Ok(Some(ev));
        }
        if let Some(ev) = self.take_resize() {
            return Ok(Some(ev));
        }
        // While a lone `ESC` (or a `[` held after a flush) is buffered, cap the
        // wait to the escape grace: if a continuation arrives it completes the
        // sequence, and if the stream stays idle we resolve the pending byte
        // promptly instead of blocking for the caller's full timeout. When
        // nothing is pending the caller's timeout is honoured as before.
        //
        // A terminal that confirmed unambiguous Escape encoding needs no cap: a
        // lone `ESC` there is never a key press, so there is nothing to resolve
        // and the parser simply waits for the rest of the sequence.
        let wait = if self.parser.flush_pending() && !self.parser.escape_unambiguous() {
            timeout.min(self.esc_grace)
        } else {
            timeout
        };
        if poll_readable(self.stdin_fd, wait) {
            self.drain_stdin();
        } else {
            // stdin idle for the whole wait: a buffered lone `ESC` is now
            // unambiguously the Escape key (no-op when nothing is pending).
            self.flush_pending_escape();
        }
        Ok(self.next_buffered().or_else(|| self.take_resize()))
    }

    /// Non-blocking peek at the next event: drains stdin once if data is already
    /// pending. Used by mouse-move coalescing to look ahead without blocking.
    pub fn try_next(&mut self) -> Option<CrosstermEvent> {
        if let Some(ev) = self.next_buffered() {
            return Some(ev);
        }
        if poll_readable(self.stdin_fd, Duration::ZERO) {
            self.drain_stdin();
        }
        self.next_buffered().or_else(|| self.take_resize())
    }
}

impl Drop for TtyReader {
    fn drop(&mut self) {
        RAW_INPUT_ACTIVE.store(false, Ordering::Relaxed);
    }
}

#[cfg(test)]
impl TtyReader {
    /// Construct a reader over an arbitrary (pipe) fd for tests, without
    /// installing the `SIGWINCH` handler or touching the global raw-input flag,
    /// so cases can be driven deterministically by writing bytes to the pipe.
    fn for_test(fd: RawFd) -> Self {
        Self {
            parser: InputParser::new(),
            queue: VecDeque::new(),
            stdin_fd: fd,
            esc_grace: DEFAULT_ESC_GRACE,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crossterm::event::KeyCode;

    /// A blocking pipe; `.0` is the read end fed to the reader, `.1` the write
    /// end the test injects bytes on.
    struct Pipe(RawFd, RawFd);
    impl Pipe {
        fn new() -> Self {
            let mut fds = [0 as RawFd; 2];
            // SAFETY: `fds` is a valid 2-element array for `pipe(2)` to fill.
            assert_eq!(unsafe { libc::pipe(fds.as_mut_ptr()) }, 0, "pipe() failed");
            Pipe(fds[0], fds[1])
        }
        fn write(&self, bytes: &[u8]) {
            // SAFETY: writing `bytes.len()` bytes from a valid slice to the
            // write end of our own pipe.
            let n =
                unsafe { libc::write(self.1, bytes.as_ptr() as *const libc::c_void, bytes.len()) };
            assert_eq!(n, bytes.len() as isize, "short pipe write");
        }
    }
    impl Drop for Pipe {
        fn drop(&mut self) {
            // SAFETY: closing our own pipe fds exactly once.
            unsafe {
                libc::close(self.0);
                libc::close(self.1);
            }
        }
    }

    fn drain_events(r: &mut TtyReader) -> Vec<CrosstermEvent> {
        let mut out = Vec::new();
        while let Some(ev) = r.next_buffered() {
            out.push(ev);
        }
        out
    }

    /// #2793: an X10 mouse report split so the first read ends on the lone `ESC`
    /// must arrive as a single `Mouse` event, never the Escape key followed by
    /// its remainder (`[ M C H 4`) as literal keystrokes. Before the fix
    /// `drain_stdin` flushed the `ESC` as soon as no continuation arrived within
    /// the escape grace, so this split leaked six key events and zero mouse
    /// events.
    ///
    /// Note this case never reaches the flush at all — it covers the window in
    /// which the `ESC` is merely *buffered*. The leak the issue reports happens
    /// once the flush has fired; that is
    /// `flushed_escape_does_not_leak_a_split_mouse_report_as_keys` below.
    #[test]
    fn split_x10_mouse_across_reads_is_one_mouse_event_not_leaked_keys() {
        let pipe = Pipe::new();
        let mut reader = TtyReader::for_test(pipe.0);

        // Read boundary lands right after the introducing ESC.
        pipe.write(b"\x1b");
        reader.drain_stdin();
        assert!(
            drain_events(&mut reader).is_empty(),
            "lone ESC surfaced before its continuation arrived",
        );

        // The rest of the report (`[ M C H 4` = X10 button 35 @ 40,20) follows
        // on a later read; the buffered ESC must complete it as a mouse event.
        pipe.write(b"[MCH4");
        reader.drain_stdin();
        let events = drain_events(&mut reader);
        assert_eq!(
            events.len(),
            1,
            "expected exactly one event, got {events:?}",
        );
        assert!(
            matches!(events[0], CrosstermEvent::Mouse(_)),
            "expected a single Mouse event, got {:?}",
            events[0],
        );
    }

    /// A genuinely lone `ESC` (nothing follows) still resolves to the Escape
    /// key — but only once stdin goes idle, which the caller signals by calling
    /// `flush_pending_escape` after a poll times out with no more bytes.
    #[test]
    fn lone_escape_resolves_to_escape_key_on_idle() {
        let pipe = Pipe::new();
        let mut reader = TtyReader::for_test(pipe.0);

        pipe.write(b"\x1b");
        reader.drain_stdin();
        assert!(
            drain_events(&mut reader).is_empty(),
            "ESC must stay buffered while a continuation could still arrive",
        );

        reader.flush_pending_escape();
        let events = drain_events(&mut reader);
        assert_eq!(events.len(), 1, "expected the Escape key, got {events:?}");
        assert!(
            matches!(
                events[0],
                CrosstermEvent::Key(k) if k.code == KeyCode::Esc,
            ),
            "expected Esc key, got {:?}",
            events[0],
        );
    }

    /// #2793, the leak as actually reported: the split gap outlasts every grace
    /// window, so the caller has already flushed the `ESC` as the Escape key
    /// (stdin went idle) when the continuation finally arrives. The remainder
    /// must not become `[ M C H 4` keystrokes — those get forwarded verbatim into
    /// the focused embedded terminal's child pty and print `^[[MCH4` at the
    /// user's shell prompt.
    ///
    /// Drives the same call sequence the event loop uses (`drain_stdin`, then
    /// `flush_pending_escape` on an idle poll, then `drain_stdin` again) rather
    /// than the buffered-only window, so it exercises the path that leaked. No
    /// timing involved: the idle flush is invoked directly, exactly as a
    /// timed-out `poll` would.
    #[test]
    fn flushed_escape_does_not_leak_a_split_mouse_report_as_keys() {
        let pipe = Pipe::new();
        let mut reader = TtyReader::for_test(pipe.0);

        pipe.write(b"\x1b");
        reader.drain_stdin();

        // stdin went idle for the whole grace: the caller resolves the Escape.
        reader.flush_pending_escape();
        let flushed = drain_events(&mut reader);
        assert!(
            matches!(flushed.first(), Some(CrosstermEvent::Key(k)) if k.code == KeyCode::Esc),
            "expected the Escape key on idle, got {flushed:?}",
        );

        // The continuation arrives late. It is still a mouse report.
        pipe.write(b"[MCH4");
        reader.drain_stdin();
        let events = drain_events(&mut reader);
        assert!(
            !events.iter().any(|e| matches!(
                e,
                CrosstermEvent::Key(k) if matches!(k.code, KeyCode::Char(_))
            )),
            "mouse report leaked as literal keystrokes: {events:?}",
        );
        assert_eq!(
            events.len(),
            1,
            "expected just the mouse event, got {events:?}"
        );
        assert!(
            matches!(events[0], CrosstermEvent::Mouse(_)),
            "expected the late continuation to decode as a mouse event, got {:?}",
            events[0],
        );
    }

    /// A terminal that confirms the kitty protocol's disambiguate mode (`CSI ? 1 u`
    /// in reply to fresh's `CSI ? u` query) encodes Escape as `CSI 27 u`, so a
    /// bare `ESC` is always the head of a sequence. The reader must then stop
    /// resolving it on idle altogether — the guess is retired, not merely widened.
    #[test]
    fn confirmed_disambiguate_mode_retires_the_escape_guess() {
        let pipe = Pipe::new();
        let mut reader = TtyReader::for_test(pipe.0);

        pipe.write(b"\x1b[?1u"); // the flags reply
        reader.drain_stdin();
        assert!(
            drain_events(&mut reader).is_empty(),
            "the flags reply must not surface as input",
        );

        pipe.write(b"\x1b");
        reader.drain_stdin();
        reader.flush_pending_escape();
        assert!(
            drain_events(&mut reader).is_empty(),
            "a lone ESC must not resolve to the Escape key on such terminals",
        );

        pipe.write(b"[MCH4");
        reader.drain_stdin();
        let events = drain_events(&mut reader);
        assert_eq!(
            events.len(),
            1,
            "expected just the mouse event, got {events:?}"
        );
        assert!(
            matches!(events[0], CrosstermEvent::Mouse(_)),
            "expected a mouse event, got {:?}",
            events[0],
        );
    }
}
