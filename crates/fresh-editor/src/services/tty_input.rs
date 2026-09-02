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
//! handler and synthesize [`InputEvent::Resize`].

use std::collections::VecDeque;
use std::os::unix::io::{AsRawFd, BorrowedFd, RawFd};
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::Duration;

use crossterm::event::MouseEventKind;
use fresh_input_parser::{Event as InputEvent, InputParser};

/// How long a buffered lone `ESC` waits for a continuation before it is
/// resolved as the Escape key. This bounds two waits: the in-`drain_stdin`
/// window that lets a sequence split across a read boundary complete as one
/// event, and the idle wait in [`TtyReader::poll`] before a genuinely lone
/// `ESC` is emitted as the Escape key. It must stay well below human
/// key-repeat latency so Escape still registers promptly.
///
/// Crucially the grace is only ever a *wait*, never — as it once was — a
/// deadline that flushes the `ESC` mid-stream: a continuation arriving after
/// the window elapses (a mouse report split across a slow pty/socket boundary)
/// must still be parsed as the control sequence it is, not torn into an Escape
/// key plus literal keystrokes that fresh forwards into a focused embedded
/// terminal (sinelaw/fresh#2793, a residue of #2745).
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
        sa.sa_sigaction = handle_sigwinch as *const () as usize;
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
    queue: VecDeque<InputEvent>,
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
    pub fn take_resize(&self) -> Option<InputEvent> {
        if SIGWINCH_PENDING.swap(false, Ordering::Relaxed) {
            crossterm::terminal::size()
                .ok()
                .map(|(cols, rows)| InputEvent::Resize(cols, rows))
        } else {
            None
        }
    }

    /// Pop the next already-decoded event, if any.
    pub fn next_buffered(&mut self) -> Option<InputEvent> {
        self.queue.pop_front()
    }

    /// Read whatever bytes are pending on stdin and feed them through the
    /// parser. The caller must have observed the fd readable; because stdin is
    /// in raw mode with at least one byte available, the `read` returns
    /// promptly without blocking.
    ///
    /// A lone trailing `ESC` is ambiguous — the Escape key, or the head of a
    /// sequence split across reads. If a continuation arrives within
    /// [`ESC_GRACE`] it is pulled in so the sequence completes as one event;
    /// otherwise the `ESC` is left *buffered* (not emitted) and only resolved
    /// as the Escape key by [`flush_pending_escape`](Self::flush_pending_escape)
    /// once stdin actually goes idle. Flushing it here — as a previous version
    /// did on grace expiry — tore a slowly-split control sequence into an
    /// Escape key followed by its remainder as literal keystrokes, which fresh
    /// then forwarded verbatim into a focused embedded terminal
    /// (sinelaw/fresh#2793).
    pub fn drain_stdin(&mut self) {
        while self.read_once() {
            if !self.parser.escape_pending() || !poll_readable(self.stdin_fd, ESC_GRACE) {
                break;
            }
        }
    }

    /// Resolve a buffered lone `ESC` as the Escape key press, queueing the
    /// event. A no-op when no `ESC` is pending.
    ///
    /// The caller invokes this only when stdin has gone idle — a blocking
    /// [`poll`](Self::poll) that timed out with no further bytes. At that point
    /// a pending `ESC` has no continuation in flight, so it is unambiguously the
    /// Escape key. Keeping the decision here (rather than at the end of every
    /// [`drain_stdin`](Self::drain_stdin)) is what makes the leak in
    /// sinelaw/fresh#2793 structurally impossible: while bytes are still
    /// arriving the `ESC` stays buffered and combines with its continuation.
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
        true
    }

    /// Queue an event, collapsing a run of mouse *motion* events down to the
    /// latest one (a motion flood produces one event per read batch), matching
    /// the coalescing the crossterm path did in `coalesce_mouse_moves`.
    ///
    /// **A held button is motion too.** This used to collapse only `Moved`,
    /// which is the report a terminal sends with no button down — so a *drag*
    /// (`Drag(button)`, the report it sends with one held) was exempt, and
    /// every cell the pointer crossed while dragging arrived as its own event.
    /// Each of those costs a full relayout and a full repaint, and a repaint
    /// costs more than the 16ms frame budget, so the loop rendered once per
    /// intermediate cell instead of once per frame: a 60-column pull on a
    /// split divider, the file explorer's edge or the dock's spent ~30 frames
    /// catching up and the divider crawled a second behind the pointer.
    /// Collapsing them here is what makes the backlog impossible — a burst
    /// that lands while the editor is busy painting comes back out as the one
    /// report that is still true.
    ///
    /// Only a run of the *same* kind collapses: `Drag(Left)` never swallows a
    /// `Drag(Right)`, and a modifier change (Shift starting a block selection
    /// mid-drag) ends the run, because those are different intents rather
    /// than the same one restated at a new cell. Nothing else is touched —
    /// presses, releases and wheel notches each mean something at the moment
    /// they happened, so they always queue.
    fn push_coalesced(&mut self, ev: InputEvent) {
        if let InputEvent::Mouse(m) = &ev {
            if matches!(m.kind, MouseEventKind::Moved | MouseEventKind::Drag(_)) {
                if let Some(InputEvent::Mouse(last)) = self.queue.back() {
                    if last.kind == m.kind && last.modifiers == m.modifiers {
                        *self.queue.back_mut().expect("back() was Some") = ev;
                        return;
                    }
                }
            }
        }
        self.queue.push_back(ev);
    }

    /// Blocking (up to `timeout`) read of the next event, or `None` on timeout.
    pub fn poll(&mut self, timeout: Duration) -> anyhow::Result<Option<InputEvent>> {
        if let Some(ev) = self.next_buffered() {
            return Ok(Some(ev));
        }
        if let Some(ev) = self.take_resize() {
            return Ok(Some(ev));
        }
        // While a lone `ESC` is buffered, cap the wait to `ESC_GRACE`: if a
        // continuation arrives it completes the sequence, and if the stream
        // stays idle we resolve the `ESC` as the Escape key promptly instead of
        // blocking for the caller's full timeout. When nothing is pending the
        // caller's timeout is honoured as before.
        let wait = if self.parser.escape_pending() {
            timeout.min(ESC_GRACE)
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
    pub fn try_next(&mut self) -> Option<InputEvent> {
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

    fn drain_events(r: &mut TtyReader) -> Vec<InputEvent> {
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
    /// `ESC_GRACE`, so this split leaked six key events and zero mouse events.
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
            matches!(events[0], InputEvent::Mouse(_)),
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
                InputEvent::Key(k) if k.code == KeyCode::Esc,
            ),
            "expected Esc key, got {:?}",
            events[0],
        );
    }

    /// #2930: a legacy terminal transmits Alt+] as `ESC ]` and Alt+[ as
    /// `ESC [` — byte-identical to the OSC/CSI introducers. With nothing
    /// following, the idle flush must resolve them to the Alt chords instead
    /// of swallowing all further input (OSC) or misreading the next key as a
    /// CSI final byte.
    #[test]
    fn lone_osc_and_csi_introducers_resolve_to_alt_brackets_on_idle() {
        use crossterm::event::KeyModifiers;
        for (bytes, chr) in [(&b"\x1b]"[..], ']'), (&b"\x1b["[..], '[')] {
            let pipe = Pipe::new();
            let mut reader = TtyReader::for_test(pipe.0);

            pipe.write(bytes);
            reader.drain_stdin();
            assert!(
                drain_events(&mut reader).is_empty(),
                "introducer must stay buffered while a payload could follow",
            );

            // Stream went idle: the introducer is a legacy Alt chord.
            reader.flush_pending_escape();
            let events = drain_events(&mut reader);
            assert!(
                matches!(
                    events.as_slice(),
                    [InputEvent::Key(k)]
                        if k.code == KeyCode::Char(chr) && k.modifiers == KeyModifiers::ALT,
                ),
                "expected Alt+{chr}, got {events:?}",
            );

            // Typing afterwards works normally (nothing is swallowed).
            pipe.write(b"x");
            reader.drain_stdin();
            let events = drain_events(&mut reader);
            assert!(
                matches!(
                    events.as_slice(),
                    [InputEvent::Key(k)] if k.code == KeyCode::Char('x'),
                ),
                "expected literal 'x', got {events:?}",
            );
        }
    }

    /// A drag is motion, and a motion flood collapses to the report that is
    /// still true. Before this, only `Moved` (no button down) collapsed, so
    /// every cell crossed while dragging a divider arrived as its own event
    /// and cost a full relayout plus a full repaint — a repaint being dearer
    /// than the frame budget, the editor rendered once per intermediate cell
    /// and the divider crawled a second behind the pointer.
    #[test]
    fn a_drag_flood_collapses_to_its_latest_report() {
        use crossterm::event::{MouseButton, MouseEventKind};
        let pipe = Pipe::new();
        let mut reader = TtyReader::for_test(pipe.0);

        // Sixty SGR left-drag reports walking one column at a time, as a
        // pull on a split divider produces.
        let mut burst = Vec::new();
        for col in 60..120 {
            burst.extend_from_slice(format!("\x1b[<32;{col};5M").as_bytes());
        }
        pipe.write(&burst);
        reader.drain_stdin();

        let events = drain_events(&mut reader);
        assert!(
            matches!(
                events.as_slice(),
                [InputEvent::Mouse(m)]
                    if m.kind == MouseEventKind::Drag(MouseButton::Left) && m.column == 118,
            ),
            "expected one drag at the last column, got {events:?}",
        );
    }

    /// Coalescing collapses a *run*, never two different intents. A press and
    /// a release each mean something at the moment they happened, and a drag
    /// with a different button or a different modifier is a different run —
    /// so none of them may swallow, or be swallowed by, its neighbour.
    #[test]
    fn coalescing_keeps_presses_releases_and_distinct_drag_runs() {
        use crossterm::event::{MouseButton, MouseEventKind};
        let pipe = Pipe::new();
        let mut reader = TtyReader::for_test(pipe.0);

        // press, two left drags, a Shift+left drag, two right drags, release
        for report in [
            "\x1b[<0;10;5M",
            "\x1b[<32;11;5M",
            "\x1b[<32;12;5M",
            "\x1b[<36;13;5M",
            "\x1b[<34;14;5M",
            "\x1b[<34;15;5M",
            "\x1b[<0;15;5m",
        ] {
            pipe.write(report.as_bytes());
        }
        reader.drain_stdin();

        let kinds: Vec<_> = drain_events(&mut reader)
            .into_iter()
            .map(|e| match e {
                InputEvent::Mouse(m) => (m.kind, m.column),
                other => panic!("expected a mouse event, got {other:?}"),
            })
            .collect();
        assert_eq!(
            kinds,
            vec![
                (MouseEventKind::Down(MouseButton::Left), 9),
                // the two bare left drags collapsed to the later one
                (MouseEventKind::Drag(MouseButton::Left), 11),
                // Shift is a different run, so it did not join them
                (MouseEventKind::Drag(MouseButton::Left), 12),
                // and neither did the right-button drags, which collapsed
                // among themselves
                (MouseEventKind::Drag(MouseButton::Right), 14),
                (MouseEventKind::Up(MouseButton::Left), 14),
            ],
        );
    }

    /// The counterpart guard: an OSC reply whose payload arrives on a later
    /// read (no idle in between) is still swallowed whole, never emitted.
    #[test]
    fn osc_reply_split_across_reads_is_still_swallowed() {
        let pipe = Pipe::new();
        let mut reader = TtyReader::for_test(pipe.0);

        pipe.write(b"\x1b]");
        // The payload is already in the pipe when drain_stdin polls, so the
        // grace-window read pulls it in and the reply is consumed whole.
        pipe.write(b"11;rgb:2e2e/3434/3636\x07");
        reader.drain_stdin();
        assert!(
            drain_events(&mut reader).is_empty(),
            "OSC reply must be swallowed, not emitted",
        );
    }
}
