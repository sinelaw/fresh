use fresh_core::BufferId;
use std::ops::Range;

/// Pre-calculated line information for an event
/// Calculated BEFORE buffer modification so line numbers are accurate
#[derive(Debug, Clone, Default)]
pub struct EventLineInfo {
    /// Start line (0-indexed) where the change begins
    pub start_line: usize,
    /// End line (0-indexed) where the change ends (in original buffer for deletes)
    pub end_line: usize,
    /// Number of lines added (for inserts) or removed (for deletes)
    pub line_delta: i32,
}

/// Search state for find/replace functionality
#[derive(Debug, Clone)]
pub struct SearchState {
    /// The search query
    pub query: String,
    /// All match positions in the buffer (byte offsets)
    pub matches: Vec<usize>,
    /// Match lengths parallel to `matches` (needed for viewport overlay creation)
    pub match_lengths: Vec<usize>,
    /// Index of the currently selected match
    pub current_match_index: Option<usize>,
    /// Whether search wraps around at document boundaries
    pub wrap_search: bool,
    /// Optional search range (for search in selection)
    pub search_range: Option<Range<usize>>,
    /// True if the match count was capped at MAX_MATCHES
    #[allow(dead_code)]
    pub capped: bool,
    /// The buffer the match set was collected from, and that buffer's
    /// version at the time.
    ///
    /// `matches` is a snapshot of byte offsets in one buffer at one moment.
    /// It stays usable only while both still hold: a different buffer makes
    /// the offsets meaningless (`SearchState` is per-window, so switching
    /// tabs leaves the match set behind), and a newer version means the text
    /// moved under them. Navigation checks this before it is willing to fall
    /// back to the snapshot, and re-runs the search when it no longer
    /// describes what is on screen (issue #3444).
    pub collected_from: (BufferId, u64),
    /// True while the search namespace holds one overlay per match, so the
    /// overlays — whose markers track buffer edits — are the live match set
    /// and `matches` is only the snapshot above.
    ///
    /// Set for small files, where `finalize_search` builds overlays for
    /// every match. False for large files (viewport-only overlays) and once
    /// the overlays are dropped while the search stays navigable, as
    /// `clear_search_overlays` does. While it holds for the active buffer,
    /// an empty overlay set means the edits removed every match — not that
    /// the overlays are missing — so navigation must not fall back to the
    /// snapshot.
    ///
    /// Overlays wiped wholesale by something that does not know about the
    /// search (a plugin's `ClearOverlays`, a virtual buffer refresh) are not
    /// covered here; the staleness check on `collected_from` catches them
    /// once the buffer has also changed.
    pub overlays_track_matches: bool,
}

impl SearchState {
    /// Maximum number of search matches to collect before stopping.
    /// Prevents unbounded memory usage when searching for common patterns
    /// in large files.
    pub const MAX_MATCHES: usize = 100_000;
}

/// State for interactive replace (query-replace)
#[derive(Debug, Clone)]
pub struct InteractiveReplaceState {
    /// The search pattern
    pub search: String,
    /// The replacement text
    pub replacement: String,
    /// Current match position (byte offset of the match we're at)
    pub current_match_pos: usize,
    /// Length of the current match in bytes (may differ from search.len() for regex)
    pub current_match_len: usize,
    /// Starting position (to detect when we've wrapped around full circle)
    pub start_pos: usize,
    /// Whether we've wrapped around to the beginning
    pub has_wrapped: bool,
    /// Number of replacements made so far
    pub replacements_made: usize,
    /// Compiled regex for regex-mode replace (None when regex mode is off)
    pub regex: Option<regex::bytes::Regex>,
}
