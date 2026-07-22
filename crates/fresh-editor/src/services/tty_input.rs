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

/// How long a buffered lone `ESC` waits for a continuation before being
/// reported as the Escape key. Terminals emit a sequence in one write, so the
/// continuation (if any) is already queued; this only has to cover a read
/// landing mid-sequence, and must stay well below human key-repeat latency.
const ESC_GRACE: Duration = Duration::from_millis(15);

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
}

impl TtyReader {
    /// Install the `SIGWINCH` handler and take ownership of stdin input.
    pub fn new() -> Self {
        install_sigwinch_handler();
        RAW_INPUT_ACTIVE.store(true, Ordering::Relaxed);
        Self {
            parser: InputParser::new(),
            queue: VecDeque::new(),
            stdin_fd: std::io::stdin().as_raw_fd(),
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
    /// sequence split across reads — so it is held back until either the rest
    /// arrives within [`ESC_GRACE`] or the grace window expires, at which point
    /// it is flushed as an Escape key press.
    pub fn drain_stdin(&mut self) {
        while self.read_once() {
            if !self.parser.escape_pending() || !poll_readable(self.stdin_fd, ESC_GRACE) {
                break;
            }
        }
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
        true
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
        if poll_readable(self.stdin_fd, timeout) {
            self.drain_stdin();
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
