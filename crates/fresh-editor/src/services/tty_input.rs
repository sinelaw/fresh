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

/// How long teardown waits for the button-release report matching a press the
/// editor already acted on. Only ever paid when a button is actually still
/// down (see [`mouse_button_down`]), and returned from the moment the release
/// lands, so in practice this is the slack between the user clicking Quit and
/// letting go — not a delay added to every exit. A keyboard quit owes no
/// release and never reaches the wait at all.
///
/// It needs to outlast a whole click, including a deliberate one: people hold
/// a button for roughly 50-150ms and a slow click runs past 300ms. The cost of
/// erring long is only paid when a release never comes at all — a press the
/// terminal never completes — because any release that does arrive ends the
/// wait immediately.
const MOUSE_RELEASE_GRACE: Duration = Duration::from_millis(400);

/// Set to true by the `SIGWINCH` handler; consumed by [`TtyReader::take_resize`].
static SIGWINCH_PENDING: AtomicBool = AtomicBool::new(false);

/// True while a [`TtyReader`] owns stdin. Lets `coalesce_mouse_moves` know it
/// must not also poke crossterm's global reader (which would race us on fd 0).
static RAW_INPUT_ACTIVE: AtomicBool = AtomicBool::new(false);

/// True while a mouse button is held, i.e. a press has been reported and its
/// release has not. Teardown reads this to decide whether a release is still
/// owed (sinelaw/fresh#3474).
static MOUSE_BUTTON_DOWN: AtomicBool = AtomicBool::new(false);

/// Cross-call state for [`note_mouse_bytes`], which sees host input in
/// whatever chunks `read()` hands back — a report can straddle two of them.
static MOUSE_SCAN: std::sync::Mutex<MouseScan> = std::sync::Mutex::new(MouseScan::new());

/// Where [`note_mouse_bytes`] is within a mouse report.
#[derive(Clone, Copy, PartialEq)]
enum ScanAt {
    /// Not inside a sequence.
    Ground,
    /// Seen `ESC`.
    Esc,
    /// Seen `ESC[`.
    Csi,
    /// Inside SGR parameters, accumulating the button code.
    Sgr,
    /// Inside an X10 report; the payload byte index that follows `ESC[M`.
    X10(u8),
}

struct MouseScan {
    at: ScanAt,
    /// The SGR report's first parameter (`Cb`), while it is being read.
    cb: u32,
    /// Still accumulating `Cb` (true until the first `;`).
    on_cb: bool,
}

impl MouseScan {
    const fn new() -> Self {
        Self {
            at: ScanAt::Ground,
            cb: 0,
            on_cb: true,
        }
    }
}

/// Is the button code of a press report one that gets a matching release?
///
/// Bit 5 (`0x20`) marks motion and bit 6 (`0x40`) a wheel notch. Neither is a
/// held button: a wheel reports no release at all, and a drag's release is
/// already accounted for by the press that began it.
fn press_holds_button(cb: u32) -> bool {
    cb & 0x20 == 0 && cb & 0x40 == 0
}

/// Track button state from raw host input.
///
/// Both input paths feed this: direct mode as it parses stdin, and the daemon
/// client as it relays stdin to the server. Neither one parses mouse reports
/// for this purpose, so the scan is its own small state machine over the bytes.
pub fn note_mouse_bytes(bytes: &[u8]) {
    let Ok(mut sc) = MOUSE_SCAN.lock() else {
        return;
    };
    for &b in bytes {
        sc.at = match (sc.at, b) {
            (_, 0x1b) => ScanAt::Esc,
            (ScanAt::Esc, b'[') => ScanAt::Csi,
            (ScanAt::Csi, b'<') => {
                sc.cb = 0;
                sc.on_cb = true;
                ScanAt::Sgr
            }
            (ScanAt::Csi, b'M') => ScanAt::X10(0),
            (ScanAt::Sgr, b'0'..=b'9') => {
                if sc.on_cb {
                    sc.cb = sc.cb.saturating_mul(10).saturating_add(u32::from(b - b'0'));
                }
                ScanAt::Sgr
            }
            (ScanAt::Sgr, b';') => {
                sc.on_cb = false;
                ScanAt::Sgr
            }
            (ScanAt::Sgr, b'M') => {
                if press_holds_button(sc.cb) {
                    MOUSE_BUTTON_DOWN.store(true, Ordering::Relaxed);
                }
                ScanAt::Ground
            }
            (ScanAt::Sgr, b'm') => {
                MOUSE_BUTTON_DOWN.store(false, Ordering::Relaxed);
                ScanAt::Ground
            }
            // X10 encodes button and coordinates as three fixed bytes, the
            // first of which is `Cb + 32`; a release is the low two bits set.
            (ScanAt::X10(0), _) => {
                let cb = u32::from(b).saturating_sub(32);
                let down = cb & 3 != 3 && press_holds_button(cb);
                MOUSE_BUTTON_DOWN.store(down, Ordering::Relaxed);
                ScanAt::X10(1)
            }
            (ScanAt::X10(n), _) if n < 2 => ScanAt::X10(n + 1),
            (ScanAt::X10(_), _) => ScanAt::Ground,
            _ => ScanAt::Ground,
        };
    }
}

/// Whether a mouse button is currently held — a press was reported and no
/// release has followed it.
pub fn mouse_button_down() -> bool {
    MOUSE_BUTTON_DOWN.load(Ordering::Relaxed)
}

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

/// Read exactly one byte from `fd`, or `None` if the read did not yield one.
fn read_one_byte(fd: RawFd) -> Option<u8> {
    let mut b = 0u8;
    // SAFETY: a one-byte read into a local we own, from a borrowed stdin fd.
    let n = unsafe { libc::read(fd, std::ptr::addr_of_mut!(b).cast::<libc::c_void>(), 1) };
    (n == 1).then_some(b)
}

/// Consume the mouse report still in flight when the editor quits on a press.
///
/// A terminal reports a click as two sequences: the press when the button goes
/// down, the release when it comes back up. Quitting from the menu bar acts on
/// the *press*, so the editor tears the terminal down while the release is
/// still unsent — it then arrives after raw mode is already off and lands at
/// the shell prompt. bash binds `ESC <` to `beginning-of-history`, which
/// swallows the report's `ESC[<` prefix and leaves the rest on the command
/// line, e.g. `0;37;17m` (sinelaw/fresh#3474).
///
/// Call this at teardown *before* mouse reporting and raw mode are turned off —
/// the release is only readable as a report while both are still on. It is
/// deliberately narrow rather than a blanket drain of pending input: bytes are
/// taken one at a time, and only while they continue a well-formed SGR
/// (`ESC [ < params M|m`) or X10 (`ESC [ M b x y`) report, so it stops at the
/// first byte that cannot belong to one instead of eating ahead into something
/// the user meant for their shell. It returns as soon as one report is
/// consumed; a press that is still pending is swallowed on the same grounds,
/// since its own release can no longer be read either.
///
/// Returns immediately unless a button is actually still down, so a keyboard
/// quit — which owes no release — costs nothing.
pub fn drain_pending_mouse_report() {
    use std::time::Instant;

    // Nothing is owed: either no click, or its release already came through.
    if !mouse_button_down() {
        return;
    }

    // Bounds the work if a terminal streams something unexpected. It has to be
    // generous rather than one report's worth: motion tracking is still on, so
    // a pointer that drifts while the button is held puts a run of drag reports
    // in front of the release. The grace is what really bounds this.
    const MAX_BYTES: usize = 4096;

    #[derive(Clone, Copy)]
    enum St {
        /// Nothing consumed yet; only an introducing `ESC` may be taken.
        Esc,
        /// Seen `ESC`; expect `[`.
        Bracket,
        /// Seen `ESC[`; expect `<` (SGR) or `M` (X10).
        Kind,
        /// Inside SGR parameters: digits and `;` until the final `M`/`m`.
        Sgr,
        /// Inside an X10 report: exactly three fixed bytes follow `ESC[M`.
        X10(u8),
    }

    let fd = std::io::stdin().as_raw_fd();
    let deadline = Instant::now() + MOUSE_RELEASE_GRACE;
    let mut st = St::Esc;
    // An X10 report's button byte, kept until its third byte closes the report.
    let mut x10_cb = 0u32;

    for _ in 0..MAX_BYTES {
        let remaining = deadline.saturating_duration_since(Instant::now());
        if remaining.is_zero() || !poll_readable(fd, remaining) {
            return;
        }
        let Some(b) = read_one_byte(fd) else {
            return;
        };
        st = match (st, b) {
            (St::Esc, 0x1b) => St::Bracket,
            (St::Bracket, b'[') => St::Kind,
            (St::Kind, b'<') => St::Sgr,
            (St::Kind, b'M') => St::X10(0),
            (St::Sgr, b'0'..=b'9' | b';') => St::Sgr,
            // `m` is the release — the report we came for, and the only one
            // that ends the wait.
            (St::Sgr, b'm') => {
                MOUSE_BUTTON_DOWN.store(false, Ordering::Relaxed);
                return;
            }
            // `M` is a press, a drag or a bare motion report. Stopping on one
            // would leave the release behind it in the queue, which is the
            // whole leak — so swallow it and keep waiting.
            (St::Sgr, b'M') => St::Esc,
            // X10 spells the button out in the first of three payload bytes.
            (St::X10(0), _) => {
                x10_cb = u32::from(b).saturating_sub(32);
                St::X10(1)
            }
            (St::X10(n), _) if n < 2 => St::X10(n + 1),
            // The third byte closes the report; in X10 a release sets the low
            // two bits of the button code.
            (St::X10(_), _) => {
                if x10_cb & 3 == 3 {
                    MOUSE_BUTTON_DOWN.store(false, Ordering::Relaxed);
                    return;
                }
                St::Esc
            }
            // Anything else cannot continue a mouse report. This byte is gone,
            // but stopping here is what keeps the drain off the user's input.
            _ => return,
        };
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
    ///
    /// Not `Default`: constructing one has process-wide side effects.
    #[allow(clippy::new_without_default)]
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
        note_mouse_bytes(&buf[..n as usize]);
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
        if let Some(ev) = fresh_input_parser::coalesce_motion_into(self.queue.back_mut(), ev) {
            self.queue.push_back(ev);
        }
    }

    /// Queue an event that did not come from stdin, through the same motion
    /// coalescing [`push_coalesced`](Self::push_coalesced) applies to parsed
    /// input.
    ///
    /// The Linux console mouse arrives over GPM's own fd rather than stdin, so
    /// its reports never reach `parse` — `poll_with_gpm` used to convert one
    /// and return it directly, which put console motion outside the one rule
    /// every other pointer report obeys. Handing it here instead is what makes
    /// a divider drag on a VT collapse the way it does under a terminal.
    pub fn queue_external(&mut self, ev: InputEvent) {
        self.push_coalesced(ev);
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

    /// An event handed in from off-stdin — the Linux console mouse, which
    /// arrives over GPM's own fd — collapses on the same rule as a parsed one,
    /// and interleaves with parsed input in the order it was queued.
    #[test]
    fn an_externally_queued_motion_flood_collapses_too() {
        use crossterm::event::{KeyModifiers, MouseButton, MouseEvent, MouseEventKind};
        let pipe = Pipe::new();
        let mut reader = TtyReader::for_test(pipe.0);

        let drag = |col| {
            InputEvent::Mouse(MouseEvent {
                kind: MouseEventKind::Drag(MouseButton::Left),
                column: col,
                row: 5,
                modifiers: KeyModifiers::empty(),
            })
        };
        // A console drag sweeping ten columns between two stdin keystrokes.
        pipe.write(b"a");
        reader.drain_stdin();
        for col in 20..30 {
            reader.queue_external(drag(col));
        }
        pipe.write(b"b");
        reader.drain_stdin();

        let events = drain_events(&mut reader);
        assert_eq!(
            events.len(),
            3,
            "expected key, one collapsed drag, key — got {events:?}",
        );
        assert!(matches!(events[0], InputEvent::Key(k) if k.code == KeyCode::Char('a')));
        assert!(
            matches!(&events[1], InputEvent::Mouse(m) if m.column == 29),
            "the ten console drags should collapse to the last, got {:?}",
            events[1],
        );
        assert!(matches!(events[2], InputEvent::Key(k) if k.code == KeyCode::Char('b')));
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
