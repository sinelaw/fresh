//! Server-side input parsing.
//!
//! The parser itself now lives in the standalone [`fresh_input_parser`] crate,
//! a DEC/ANSI state machine that converts the raw client byte stream into
//! crossterm events without ever leaking control-sequence bytes as literal
//! input (see sinelaw/fresh#2745). This module re-exports it so existing call
//! sites (`crate::server::input_parser::InputParser`) keep working, and adds
//! [`ClientInputParser`], which owns the "a lone `ESC` is the Escape key once
//! the stream goes idle" rule for session (daemon) clients.

use std::time::{Duration, Instant};

pub use fresh_input_parser::{Event, InputParser, KeyPress};

/// How long a lone `ESC` stays buffered before it is resolved as the Escape
/// key. Mirrors the tty reader's grace window: long enough that a control
/// sequence split across two socket reads still arrives as one sequence, short
/// enough that Escape feels immediate. Two keystrokes can never land this close
/// together, so nothing a user types is coalesced by it.
const ESC_GRACE: Duration = Duration::from_millis(15);

/// [`InputParser`] plus the idle-flush rule the session path needs.
///
/// The direct (tty) reader resolves a buffered lone `ESC` when a blocking poll
/// on stdin times out with no further bytes. A session client's bytes arrive
/// over a socket that the server drains with a non-blocking read on every loop
/// tick, so there is no poll timeout to hang the decision on: without this
/// wrapper the `ESC` sat in the parser *indefinitely*, so in `fresh -a` the
/// Escape key did nothing until the next keypress — which was then swallowed
/// into an Alt chord with it (sinelaw/fresh#2810).
///
/// Call [`parse`](Self::parse) for every chunk read from the client and
/// [`flush_idle`](Self::flush_idle) on every tick; the latter emits the Escape
/// once the grace window has elapsed with no continuation.
pub struct ClientInputParser {
    parser: InputParser,
    /// When the currently-buffered lone `ESC` was first observed. `None`
    /// whenever the parser is not sitting on one.
    escape_pending_since: Option<Instant>,
}

impl ClientInputParser {
    pub fn new() -> Self {
        Self {
            parser: InputParser::new(),
            escape_pending_since: None,
        }
    }

    /// Parse a chunk of client bytes, arming (or disarming) the escape timer
    /// according to what the chunk left the parser holding.
    pub fn parse(&mut self, bytes: &[u8]) -> Vec<Event> {
        let events = self.parser.parse(bytes);
        self.sync_escape_timer(Instant::now());
        events
    }

    /// Resolve a buffered lone `ESC` as the Escape key once it has been pending
    /// for [`ESC_GRACE`] without a continuation. Returns the events to inject
    /// (empty when nothing is pending or the grace window has not elapsed).
    ///
    /// `now` is a parameter so tests can drive the window without sleeping.
    pub fn flush_idle(&mut self, now: Instant) -> Vec<Event> {
        let Some(since) = self.escape_pending_since else {
            return Vec::new();
        };
        if now.duration_since(since) < ESC_GRACE {
            return Vec::new();
        }
        let events = self.parser.flush();
        self.escape_pending_since = None;
        events
    }

    /// Start the timer when an `ESC` becomes pending, and clear it as soon as
    /// the parser has moved on (the continuation arrived, or the escape was
    /// already flushed).
    fn sync_escape_timer(&mut self, now: Instant) {
        if self.parser.escape_pending() {
            self.escape_pending_since.get_or_insert(now);
        } else {
            self.escape_pending_since = None;
        }
    }
}

impl Default for ClientInputParser {
    fn default() -> Self {
        Self::new()
    }
}
