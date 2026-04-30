//! Live Grep "Return to Work" cache (issue #1796).
//!
//! Holds the prior search query, the prior selected result index, and a
//! display cache of the matches that were on screen when the user last
//! dismissed the floating overlay. `Action::ResumeLiveGrep` reads this
//! to re-open the overlay in the same state, *without* re-running
//! ripgrep — this is what enables the issue-#1796 flow of flipping
//! between editing a result and viewing the next match.
//!
//! `cached_results` is invalidated as soon as the user edits the query
//! (any keystroke that mutates the prompt input). Saved snapshots
//! exported to the Quickfix dock carry their own copies — this struct
//! is purely a *display* aid, never a correctness contract.

use std::time::Instant;

/// One captured Live Grep match. Mirrors the JSON shape that the
/// `live_grep.ts` plugin already speaks; we keep it in core (not in the
/// plugin) because the Quickfix export needs to land in a virtual
/// buffer owned by the editor, not by JS.
#[derive(Debug, Clone)]
pub struct GrepMatch {
    pub file: String,
    pub line: usize,
    pub column: usize,
    pub content: String,
}

/// Cached Live Grep state for Resume.
#[derive(Debug, Clone, Default)]
pub struct LiveGrepLastState {
    /// Previous search query (the literal string the user typed).
    pub query: String,
    /// Previously selected match index, if any.
    pub selected_index: Option<usize>,
    /// Cached matches from the previous overlay session. When `Some`,
    /// Resume re-displays them without running ripgrep again.
    pub cached_results: Option<Vec<GrepMatch>>,
    /// Wall-clock time the cache was populated (for diagnostics — not
    /// used as a TTL today; see Section 5 of the design doc).
    pub cached_at: Option<Instant>,
    /// Identifier for a Quickfix snapshot exported from the prior
    /// session. Reserved for the "re-seed overlay from Quickfix"
    /// round-trip; unused until that wiring lands.
    pub last_results_snapshot_id: Option<u64>,
}

impl LiveGrepLastState {
    /// Drop the cached matches (call from any input handler that
    /// mutates the prompt query).
    pub fn invalidate_cache(&mut self) {
        self.cached_results = None;
        self.cached_at = None;
    }
}
