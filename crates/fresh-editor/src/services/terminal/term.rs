//! Terminal state using alacritty_terminal for emulation
//!
//! This module wraps alacritty_terminal to provide:
//! - VT100/ANSI escape sequence parsing
//! - Terminal grid management
//! - Cursor state tracking
//! - Incremental scrollback streaming to backing file
//!
//! # Role in Incremental Streaming Architecture
//!
//! This module provides the core state management and streaming methods.
//! See `super` module docs for the full architecture overview.
//!
//! ## Key Methods
//!
//! - `process_output`: Feed PTY bytes into the terminal emulator
//! - `flush_new_scrollback`: Stream new scrollback lines to backing file
//! - `append_visible_screen`: Append visible screen on mode exit
//! - `backing_file_history_end`: Get truncation point for mode re-entry
//!
//! ## State Tracking
//!
//! `synced_history_lines` tracks how many scrollback lines have been written to the
//! backing file. When `grid.history_size() > synced_history_lines`, new lines need
//! to be flushed.
//!
//! `backing_file_history_end` tracks the byte offset where scrollback ends in the
//! backing file, used for truncation when re-entering terminal mode.

use alacritty_terminal::event::{Event, EventListener};
use alacritty_terminal::grid::Scroll;
use alacritty_terminal::index::{Column, Line};
use alacritty_terminal::term::test::TermSize;
use alacritty_terminal::term::{Config as TermConfig, Term, TermMode};
use alacritty_terminal::vte::ansi::Processor;
use std::io::{self, Write};
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

// Keep a generous scrollback so sync-to-buffer can include deep history.
const SCROLLBACK_LINES: usize = 200_000;

/// Event listener that captures PtyWrite events for sending back to the PTY.
///
/// When the terminal emulator needs to respond to queries (like DSR cursor position
/// requests `\x1b[6n`), it generates `Event::PtyWrite` events. These must be captured
/// and sent back to the PTY for the shell to receive the response.
#[derive(Clone)]
struct PtyWriteListener {
    /// Queue of data to write back to the PTY
    write_queue: Arc<Mutex<Vec<String>>>,
    /// Latest title requested by the program via OSC 0/1/2 (or a reset
    /// via the OSC reset sequence). `Some` means a change is pending;
    /// the inner string is the new title (empty string for a reset).
    /// `process_output` drains this after parsing to update the
    /// terminal's stored title.
    pending_title: Arc<Mutex<Option<String>>>,
}

impl PtyWriteListener {
    fn new() -> Self {
        Self {
            write_queue: Arc::new(Mutex::new(Vec::new())),
            pending_title: Arc::new(Mutex::new(None)),
        }
    }
}

impl EventListener for PtyWriteListener {
    fn send_event(&self, event: Event) {
        match event {
            Event::PtyWrite(text) => {
                if let Ok(mut queue) = self.write_queue.lock() {
                    queue.push(text);
                }
            }
            // OSC 0 (icon + window title), OSC 1 (icon title), and OSC 2
            // (window title) all surface as `Title`. Record the latest;
            // `process_output` propagates it to `terminal_title` so the
            // buffer's tab auto-adjusts to whatever the running program set.
            Event::Title(title) => {
                if let Ok(mut pending) = self.pending_title.lock() {
                    *pending = Some(title);
                }
            }
            // Title reset (OSC with empty payload) — clear back to the
            // buffer's default name by recording an empty title.
            Event::ResetTitle => {
                if let Ok(mut pending) = self.pending_title.lock() {
                    *pending = Some(String::new());
                }
            }
            // Other events (ClipboardStore, etc.) are ignored for now.
            _ => {}
        }
    }
}

/// Incremental scanner that extracts the working directory a shell reports via
/// the OSC 7 escape sequence (`ESC ] 7 ; file://host/path BEL`, or terminated
/// by `ST` = `ESC \`).
///
/// The terminal emulator we embed (`alacritty_terminal` 0.25 / `vte` 0.15) does
/// not surface OSC 7 — its OSC dispatcher drops the sequence as "unhandled" and
/// the `Handler` trait has no cwd hook — so we sniff it out of the raw PTY byte
/// stream ourselves. Sequences can straddle PTY reads, so the scanner is a
/// resumable state machine: callers feed every byte that flows to the emulator
/// and collect a payload once a complete sequence terminates.
#[derive(Debug, Default)]
struct Osc7Scanner {
    /// How many bytes of the introducer `ESC ] 7 ;` have matched so far (0..4).
    intro_match: usize,
    /// True once the introducer matched and we're accumulating the payload.
    collecting: bool,
    /// True when the previous collected byte was `ESC`, i.e. a possible start
    /// of the `ST` (`ESC \`) string terminator.
    saw_esc: bool,
    /// Accumulated payload bytes (between the introducer and the terminator).
    buf: Vec<u8>,
}

/// Introducer bytes for OSC 7: `ESC ] 7 ;`.
const OSC7_INTRO: [u8; 4] = [0x1b, b']', b'7', b';'];
/// Cap on the OSC 7 payload we'll buffer. A `file://` cwd URI is far shorter;
/// anything longer is malformed (or not really OSC 7) and is abandoned so a
/// stray `ESC ] 7 ;` without a terminator can't grow the buffer unboundedly.
const OSC7_MAX_PAYLOAD: usize = 4096;

impl Osc7Scanner {
    /// Feed one chunk of PTY output. Returns the payload string of each OSC 7
    /// sequence that *completes* within this chunk (usually zero or one).
    fn feed(&mut self, data: &[u8], out: &mut Vec<String>) {
        for &byte in data {
            if self.collecting {
                if self.saw_esc {
                    // Inside the payload we only treat `ESC \` (ST) as a
                    // terminator. Any other byte after ESC means the sequence
                    // is malformed — abandon it rather than risk swallowing
                    // unrelated output.
                    self.saw_esc = false;
                    if byte == b'\\' {
                        self.finish(out);
                    } else {
                        self.reset();
                    }
                } else if byte == 0x07 {
                    // BEL terminator.
                    self.finish(out);
                } else if byte == 0x1b {
                    self.saw_esc = true;
                } else if self.buf.len() >= OSC7_MAX_PAYLOAD {
                    self.reset();
                } else {
                    self.buf.push(byte);
                }
            } else if byte == OSC7_INTRO[self.intro_match] {
                self.intro_match += 1;
                if self.intro_match == OSC7_INTRO.len() {
                    self.collecting = true;
                    self.intro_match = 0;
                    self.buf.clear();
                }
            } else {
                // Restart the introducer match. The introducer's only repeated
                // prefix byte is its first (ESC), so a one-byte re-check
                // suffices to avoid missing a sequence like `ESC ESC ] 7 ;`.
                self.intro_match = usize::from(byte == OSC7_INTRO[0]);
            }
        }
    }

    /// Emit the collected payload and reset to searching.
    fn finish(&mut self, out: &mut Vec<String>) {
        if let Ok(s) = std::str::from_utf8(&self.buf) {
            out.push(s.to_owned());
        }
        self.reset();
    }

    fn reset(&mut self) {
        self.collecting = false;
        self.saw_esc = false;
        self.intro_match = 0;
        self.buf.clear();
    }
}

/// Parse the payload of an OSC 7 sequence into a working-directory path.
///
/// The conventional payload is a `file://host/path` URI (the host is usually
/// the local hostname or empty), but some shells emit a bare absolute path.
/// Percent-escapes in the URI form are decoded. Returns `None` for payloads
/// that don't resolve to an absolute path.
fn parse_osc7_path(payload: &str) -> Option<PathBuf> {
    let raw = if let Some(rest) = payload.strip_prefix("file://") {
        // Strip the authority (host) component: everything up to the first
        // '/', which begins the absolute path.
        let path_part = match rest.find('/') {
            Some(idx) => &rest[idx..],
            None => return None,
        };
        percent_decode(path_part)
    } else {
        // Bare path fallback (non-standard, but seen in the wild).
        payload.to_owned()
    };

    if raw.is_empty() {
        return None;
    }

    // A `file:///C:/dir` URI decodes to `/C:/dir`; drop the leading slash so it
    // reads as a drive-absolute Windows path. Done unconditionally (not behind
    // cfg) because Fresh may run on Linux while editing a Windows host, or vice
    // versa — the OSC 7 payload's flavour follows the *shell's* OS, not ours.
    let raw = {
        let bytes = raw.as_bytes();
        if bytes.len() >= 3 && bytes[0] == b'/' && bytes[2] == b':' {
            raw[1..].to_owned()
        } else {
            raw
        }
    };

    // Accept the path if it's absolute in *either* convention. We can't use
    // `Path::is_absolute()` — it's host-OS-specific, so it would reject a POSIX
    // path on Windows (and a `C:\` path on Unix), discarding valid cwds from a
    // remote shell of the other OS.
    if is_osc7_absolute(&raw) {
        Some(PathBuf::from(raw))
    } else {
        None
    }
}

/// Whether an OSC 7 path string is absolute in POSIX or Windows terms: a
/// leading `/` (POSIX), a UNC `\\…` prefix, or a `X:` drive (Windows).
fn is_osc7_absolute(s: &str) -> bool {
    let bytes = s.as_bytes();
    s.starts_with('/')
        || s.starts_with('\\')
        || (bytes.len() >= 2 && bytes[0].is_ascii_alphabetic() && bytes[1] == b':')
}

/// Decode `%XX` percent-escapes in an OSC 7 URI path. Invalid escapes are left
/// verbatim. Operates on bytes so non-ASCII (already-UTF-8) paths survive.
fn percent_decode(input: &str) -> String {
    let bytes = input.as_bytes();
    let mut out: Vec<u8> = Vec::with_capacity(bytes.len());
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'%' && i + 2 < bytes.len() {
            let hi = (bytes[i + 1] as char).to_digit(16);
            let lo = (bytes[i + 2] as char).to_digit(16);
            if let (Some(hi), Some(lo)) = (hi, lo) {
                out.push((hi * 16 + lo) as u8);
                i += 3;
                continue;
            }
        }
        out.push(bytes[i]);
        i += 1;
    }
    String::from_utf8_lossy(&out).into_owned()
}

/// Terminal state wrapping alacritty_terminal
pub struct TerminalState {
    /// The terminal emulator
    term: Term<PtyWriteListener>,
    /// ANSI parser
    parser: Processor,
    /// Current dimensions
    cols: u16,
    rows: u16,
    /// Whether content has changed since last render
    dirty: bool,
    /// Terminal title (set via escape sequences)
    terminal_title: String,
    /// Number of grid history *rows* already streamed to the backing file in the
    /// current epoch (an epoch resets when the scrollback is cleared). Only ever
    /// advances past complete logical lines (rows that don't continue via
    /// `WRAPLINE`), so the file always ends on a logical-line boundary. Flush
    /// only ever advances this past lines it *wrote*, so nothing is skipped —
    /// scrollback is never lost (a grow may re-write a bounded few lines instead).
    synced_history_lines: usize,
    /// Count of complete logical lines streamed this epoch. Invariant under
    /// width reflow (a logical line keeps its identity when re-wrapped), so it's
    /// the anchor used to rebuild `synced_history_lines` after a resize re-wraps
    /// the grid and invalidates the physical row count.
    synced_logical_lines: usize,
    /// A width resize happened while the alternate screen was active, so the
    /// primary grid's history was reflowed but couldn't be re-anchored yet
    /// (the grid in view was the alt grid). Deferred until alt-screen exit.
    pending_reflow_resync: bool,
    /// Byte offset in backing file where scrollback ends (for truncation)
    backing_file_history_end: u64,
    /// Queue of data to write back to the PTY (for DSR responses, etc.)
    pty_write_queue: Arc<Mutex<Vec<String>>>,
    /// Pending title set by the program via OSC 0/1/2 (shared with the
    /// event listener). Drained in `process_output` into `terminal_title`.
    pending_title: Arc<Mutex<Option<String>>>,
    /// Working directory most recently reported by the shell via OSC 7, used
    /// to resolve relative paths the running program prints (e.g. for
    /// Ctrl+Click to open). `None` until the shell emits OSC 7.
    cwd: Option<PathBuf>,
    /// Resumable scanner that extracts OSC 7 payloads from the raw PTY stream.
    osc7: Osc7Scanner,
}

impl TerminalState {
    /// Create a new terminal state
    pub fn new(cols: u16, rows: u16) -> Self {
        let size = TermSize::new(cols as usize, rows as usize);
        let config = TermConfig {
            scrolling_history: SCROLLBACK_LINES,
            ..Default::default()
        };
        let listener = PtyWriteListener::new();
        let pty_write_queue = listener.write_queue.clone();
        let pending_title = listener.pending_title.clone();
        let term = Term::new(config, &size, listener);

        Self {
            term,
            parser: Processor::new(),
            cols,
            rows,
            dirty: true,
            terminal_title: String::new(),
            synced_history_lines: 0,
            synced_logical_lines: 0,
            pending_reflow_resync: false,
            backing_file_history_end: 0,
            pty_write_queue,
            pending_title,
            cwd: None,
            osc7: Osc7Scanner::default(),
        }
    }

    /// The terminal's current working directory as last reported by the shell
    /// via OSC 7, if any. Tracks `cd` within the session (when the shell is
    /// configured to emit OSC 7); `None` otherwise.
    pub fn cwd(&self) -> Option<&std::path::Path> {
        self.cwd.as_deref()
    }

    /// Drain any pending data that needs to be written back to the PTY.
    ///
    /// This is used for responses to terminal queries like DSR (cursor position report).
    /// The caller should write this data to the PTY writer.
    pub fn drain_pty_write_queue(&self) -> Vec<String> {
        if let Ok(mut queue) = self.pty_write_queue.lock() {
            std::mem::take(&mut *queue)
        } else {
            Vec::new()
        }
    }

    /// Process output from the PTY
    pub fn process_output(&mut self, data: &[u8]) {
        use alacritty_terminal::grid::Dimensions;

        let history_before = self.term.grid().history_size();
        let alt_before = self.term.mode().contains(TermMode::ALT_SCREEN);

        // Sniff OSC 7 (working-directory reports) out of the raw stream before
        // it reaches the emulator, which discards the sequence. Take the latest
        // valid payload — only the final cwd in this chunk matters.
        let mut osc7_payloads = Vec::new();
        self.osc7.feed(data, &mut osc7_payloads);
        if let Some(path) = osc7_payloads.iter().rev().find_map(|p| parse_osc7_path(p)) {
            self.cwd = Some(path);
        }

        self.parser.advance(&mut self.term, data);
        // The parser may have emitted OSC title events (0/1/2) into the
        // listener's pending slot during `advance`. Apply the latest so
        // the stored title reflects what the program requested.
        if let Ok(mut pending) = self.pending_title.lock() {
            if let Some(title) = pending.take() {
                self.terminal_title = title;
            }
        }

        let alt_after = self.term.mode().contains(TermMode::ALT_SCREEN);
        if alt_before && !alt_after && self.pending_reflow_resync {
            // Returned from the alternate screen after a width resize happened
            // while it was active: the primary grid (now back in view) was
            // reflowed, so re-anchor against it now.
            self.resync_after_reflow();
            self.pending_reflow_resync = false;
        }

        // Output never shrinks scrollback during normal printing — only a
        // scrollback clear (`ESC[3J`) or terminal reset (`RIS`, `ESC c`) does.
        // (The alternate screen also reports zero history, but that's transient
        // and restored on exit, so exclude it.) When it happens, the grid
        // history we were tracking is gone; the backing file keeps everything
        // already streamed, so start a fresh epoch — subsequent output is
        // appended after the existing scrollback in the file.
        if !alt_after {
            let history_after = self.term.grid().history_size();
            if history_after < history_before {
                self.synced_history_lines = 0;
                self.synced_logical_lines = 0;
            }
        }

        self.dirty = true;
    }

    /// Resize the terminal.
    ///
    /// Scrollback is streamed to the backing file as complete *logical* lines.
    /// A resize perturbs the visible/history boundary and — on a width change —
    /// re-wraps already-persisted content, changing its physical row count.
    /// Reconciliation depends on *why* history changed:
    ///
    /// * Pure height change (no reflow): physical rows are still valid, so leave
    ///   `synced_history_lines` alone. A shrink pushes the top rows into
    ///   scrollback — new content the next flush writes (no loss). A grow pulls
    ///   rows back onto the screen; the `current <= synced` flush guard suppresses
    ///   them until genuinely new lines scroll off (no duplicates).
    ///
    /// * Width change (reflow): the physical count is meaningless now, but the
    ///   logical line count is invariant under re-wrapping. Re-derive
    ///   `synced_history_lines` from `synced_logical_lines` by walking the
    ///   reflowed history (a cheap flag-only scan, no I/O) so the next flush
    ///   appends exactly the logical lines not yet persisted — width spill
    ///   included, re-wraps excluded. (Deferred if the alternate screen is up,
    ///   since the primary grid isn't the one in view.)
    pub fn resize(&mut self, cols: u16, rows: u16) {
        if cols != self.cols || rows != self.rows {
            let cols_changed = cols != self.cols;
            self.cols = cols;
            self.rows = rows;
            let size = TermSize::new(cols as usize, rows as usize);
            self.term.resize(size);

            if cols_changed {
                if self.term.mode().contains(TermMode::ALT_SCREEN) {
                    // The grid in view is the alt screen (no scrollback); the
                    // primary grid reflowed underneath. Re-anchor on alt exit.
                    self.pending_reflow_resync = true;
                } else {
                    self.resync_after_reflow();
                }
            }

            self.dirty = true;
        }
    }

    /// Rebuild `synced_history_lines` (physical rows) after a width reflow
    /// invalidated the physical row count.
    ///
    /// The logical-line position the pointer sat at (`synced_logical_lines`) is
    /// invariant under re-wrapping, so we walk the reflowed history oldest→newest
    /// counting complete logical lines until we've re-reached that position, and
    /// set the physical pointer to the rows consumed. A flag-only scan (no
    /// allocation, no I/O). If a simultaneous grow pulled rows back onto the
    /// screen so history now holds fewer logical lines, the pointer lands at the
    /// end of what remains; those lines may then be re-written (a bounded
    /// duplicate) when they scroll off again — never lost.
    fn resync_after_reflow(&mut self) {
        use alacritty_terminal::grid::Dimensions;

        let history = self.term.grid().history_size();
        let target = self.synced_logical_lines;
        let mut logical_seen = 0usize;
        let mut synced = 0usize;
        let mut k = 0usize;
        while k < history && logical_seen < target {
            let line_idx = -((history - k) as i32);
            if !self.row_wraps(Line(line_idx)) {
                logical_seen += 1;
                synced = k + 1;
            }
            k += 1;
        }
        self.synced_history_lines = synced;
        self.synced_logical_lines = logical_seen;
    }

    /// Get current dimensions
    pub fn size(&self) -> (u16, u16) {
        (self.cols, self.rows)
    }

    /// Check if content has changed
    pub fn is_dirty(&self) -> bool {
        self.dirty
    }

    /// Mark as clean after rendering
    pub fn mark_clean(&mut self) {
        self.dirty = false;
    }

    /// Get the cursor position (column, row)
    pub fn cursor_position(&self) -> (u16, u16) {
        let cursor = self.term.grid().cursor.point;
        (cursor.column.0 as u16, cursor.line.0 as u16)
    }

    /// Check if cursor is visible
    pub fn cursor_visible(&self) -> bool {
        // alacritty_terminal doesn't expose cursor visibility directly
        // We'll assume it's always visible for now
        true
    }

    /// Snapshot of the cursor row's text content as a plain string.
    ///
    /// Used by the `terminal_output` plugin hook so listeners (e.g.
    /// the Orchestrator agent state machine) can match prompt patterns
    /// without a separate readback API. Returns cells `[0..cursor_col)`
    /// of the cursor row so a legitimate trailing space typed by the
    /// program (typical for prompts like `"... (Y/n): "`) is
    /// preserved while the unwritten right-edge padding past the
    /// cursor is dropped. Falls back to trimming the whole row when
    /// the cursor has wrapped to the start of a freshly-allocated
    /// next row (col == 0): the visible content lives one row up,
    /// and the trailing space ambiguity doesn't apply (a wrap means
    /// the line was full).
    pub fn last_visible_line(&self) -> String {
        let (col, row) = self.cursor_position();
        if row >= self.rows {
            return String::new();
        }
        if col == 0 && row > 0 {
            // Cursor wrapped to a fresh row; the meaningful prompt
            // content sits on the row above. Take that row whole and
            // strip any right-edge padding from it.
            let cells = self.get_line(row - 1);
            let mut s: String = cells.iter().map(|cell| cell.c).collect();
            let trimmed_len = s.trim_end_matches(' ').len();
            s.truncate(trimmed_len);
            return s;
        }
        let cells = self.get_line(row);
        let take = (col as usize).min(cells.len());
        cells.iter().take(take).map(|cell| cell.c).collect()
    }

    /// Get a line of content for rendering
    ///
    /// Returns cells as (char, foreground_color, background_color, flags) tuples.
    /// Colors are ANSI color indices (0-255) or None for default.
    /// Accounts for scroll offset (display_offset) when accessing lines.
    pub fn get_line(&self, row: u16) -> Vec<TerminalCell> {
        use alacritty_terminal::index::{Column, Line};
        use alacritty_terminal::term::cell::Flags;

        let grid = self.term.grid();
        let display_offset = grid.display_offset();

        // Adjust line index for scroll offset
        // When scrolled up by N lines, row 0 should show content from N lines back in history
        let line = Line(row as i32 - display_offset as i32);

        // Check if line is in valid range (use rows as the limit)
        if row >= self.rows {
            return vec![TerminalCell::default(); self.cols as usize];
        }

        let row_data = &grid[line];
        let mut cells = Vec::with_capacity(self.cols as usize);

        for col in 0..self.cols as usize {
            let cell = &row_data[Column(col)];
            let c = cell.c;

            // Convert colors
            let fg = color_to_rgb(&cell.fg);
            let bg = color_to_rgb(&cell.bg);

            // Check flags
            let flags = cell.flags;
            let bold = flags.contains(Flags::BOLD);
            let italic = flags.contains(Flags::ITALIC);
            let underline = flags.contains(Flags::UNDERLINE);
            let inverse = flags.contains(Flags::INVERSE);

            cells.push(TerminalCell {
                c,
                fg,
                bg,
                bold,
                italic,
                underline,
                inverse,
            });
        }

        cells
    }

    /// Get all visible content as a string (for testing/debugging)
    pub fn content_string(&self) -> String {
        let mut result = String::new();
        for row in 0..self.rows {
            let line = self.get_line(row);
            for cell in line {
                result.push(cell.c);
            }
            result.push('\n');
        }
        result
    }

    /// Get all content including scrollback history as a string
    /// Lines are in chronological order (oldest first)
    ///
    /// WARNING: This is O(total_history) and should NOT be used in hot paths.
    /// For mode switching, use the incremental streaming architecture instead:
    /// - `flush_new_scrollback()` during PTY reads
    /// - `append_visible_screen()` on mode exit
    #[allow(dead_code)]
    pub fn full_content_string(&self) -> String {
        use alacritty_terminal::grid::Dimensions;
        use alacritty_terminal::index::{Column, Line};

        let grid = self.term.grid();
        let history_size = grid.history_size();
        let mut result = String::new();

        // First, add scrollback history (negative line indices)
        // History lines go from -(history_size) to -1
        for i in (1..=history_size).rev() {
            let line = Line(-(i as i32));
            let row_data = &grid[line];
            let mut line_str = String::new();
            for col in 0..self.cols as usize {
                line_str.push(row_data[Column(col)].c);
            }
            let trimmed = line_str.trim_end();
            result.push_str(trimmed);
            result.push('\n');
        }

        // Then add visible screen content (line indices 0 to rows-1)
        for row in 0..self.rows {
            let line = self.get_line(row);
            let line_str: String = line.iter().map(|c| c.c).collect();
            let trimmed = line_str.trim_end();
            result.push_str(trimmed);
            if row < self.rows - 1 {
                result.push('\n');
            }
        }

        result
    }

    /// Get the number of scrollback history lines
    pub fn history_size(&self) -> usize {
        use alacritty_terminal::grid::Dimensions;
        self.term.grid().history_size()
    }

    /// Get the title (if set by escape sequence)
    pub fn title(&self) -> &str {
        &self.terminal_title
    }

    /// Set the terminal title (called when escape sequence is received)
    pub fn set_title(&mut self, title: String) {
        self.terminal_title = title;
    }

    /// Scroll to the bottom of the terminal (display offset = 0)
    /// Used when re-entering terminal mode from scrollback view
    pub fn scroll_to_bottom(&mut self) {
        self.term.scroll_display(Scroll::Bottom);
        self.dirty = true;
    }

    // =========================================================================
    // Terminal mode flags
    // =========================================================================

    /// Check if the terminal is in alternate screen mode.
    /// Programs like vim, less, htop use alternate screen.
    pub fn is_alternate_screen(&self) -> bool {
        self.term.mode().contains(TermMode::ALT_SCREEN)
    }

    /// Check if the terminal wants mouse events reported.
    /// Returns true if any mouse reporting mode is enabled.
    pub fn wants_mouse_events(&self) -> bool {
        let mode = self.term.mode();
        mode.intersects(
            TermMode::MOUSE_REPORT_CLICK | TermMode::MOUSE_MOTION | TermMode::MOUSE_DRAG,
        )
    }

    /// Check if SGR mouse encoding is enabled (modern mouse protocol).
    pub fn uses_sgr_mouse(&self) -> bool {
        self.term.mode().contains(TermMode::SGR_MOUSE)
    }

    /// Check if alternate scroll mode is enabled.
    /// When enabled, scroll wheel should be sent as up/down arrow keys.
    pub fn uses_alternate_scroll(&self) -> bool {
        self.term.mode().contains(TermMode::ALTERNATE_SCROLL)
    }

    /// Check if application cursor keys mode (DECCKM) is enabled.
    /// Programs like less, git log set this mode so that arrow keys
    /// send `\x1bOA` (SS3) instead of `\x1b[A` (CSI).
    pub fn is_app_cursor(&self) -> bool {
        self.term.mode().contains(TermMode::APP_CURSOR)
    }

    // =========================================================================
    // Incremental scrollback streaming
    // =========================================================================

    /// Flush newly scrolled-off scrollback to the writer as complete logical
    /// lines, returning the number of logical lines written.
    ///
    /// Call after `process_output()` (and before reading the backing file) to
    /// incrementally persist scrollback. Rows that alacritty wrapped (`WRAPLINE`)
    /// are joined into one unwrapped logical line, so the backing file stores
    /// logical lines — the editor then soft-wraps them to whatever width the
    /// scroll-back view happens to be, instead of being frozen at the width they
    /// were captured. Only logical lines that have *fully* scrolled into history
    /// are written; a trailing line still continuing into the visible screen is
    /// left for a later flush, keeping the file on a logical-line boundary.
    pub fn flush_new_scrollback<W: Write>(&mut self, writer: &mut W) -> io::Result<usize> {
        use alacritty_terminal::grid::Dimensions;

        let history = self.term.grid().history_size();
        if history <= self.synced_history_lines {
            return Ok(0);
        }

        // History rows oldest→newest map to k = 0..history via line index
        // -(history - k); -history is oldest, -1 is newest (just above visible).
        // Write every complete logical line past the pointer, advancing the
        // pointer only past lines actually written — so a line is never skipped,
        // i.e. never lost. (A grow that rewinds the boundary may re-write a
        // bounded handful of lines; duplication is the accepted trade-off.)
        let mut written = 0usize;
        let mut line_start = self.synced_history_lines;
        let mut k = self.synced_history_lines;
        while k < history {
            let line_idx = -((history - k) as i32);
            if self.row_wraps(Line(line_idx)) {
                // Logical line continues onto the next row.
                k += 1;
                continue;
            }
            // Row k ends a logical line spanning rows [line_start ..= k].
            self.write_logical_line(writer, line_start, k, history)?;
            written += 1;
            self.synced_logical_lines += 1;
            k += 1;
            self.synced_history_lines = k;
            line_start = k;
        }
        // Any rows past `synced_history_lines` form an incomplete logical line
        // (its final row wraps into the visible screen); leave them uncommitted.
        Ok(written)
    }

    /// Append the visible screen content to the writer as logical lines.
    ///
    /// Call this when exiting terminal mode (or saving a session) to add the
    /// current screen to the backing file. Wrapped rows are joined like
    /// `flush_new_scrollback`, but every visible row is emitted (including the
    /// trailing logical line and blank rows) so the scroll-back viewport can
    /// anchor to the start of this block and line up with the live PTY frame.
    /// The block is temporary — re-entering terminal mode truncates the file
    /// back to `backing_file_history_end`.
    pub fn append_visible_screen<W: Write>(&self, writer: &mut W) -> io::Result<()> {
        let rows = self.rows as i32;
        let mut start = 0i32;
        let mut row = 0i32;
        while row < rows {
            if self.row_wraps(Line(row)) && row + 1 < rows {
                row += 1;
                continue;
            }
            // `write_logical_line` indexes via the history convention, so pass
            // visible rows through directly (offset 0 == oldest here is just row).
            self.write_visible_logical_line(writer, start, row)?;
            row += 1;
            start = row;
        }
        Ok(())
    }

    /// True if the last cell of `line` carries the `WRAPLINE` flag, i.e. the row
    /// is a soft-wrap continuation point (the logical line continues on the next
    /// physical row).
    fn row_wraps(&self, line: Line) -> bool {
        use alacritty_terminal::term::cell::Flags;
        if self.cols == 0 {
            return false;
        }
        let grid = self.term.grid();
        grid[line][Column(self.cols as usize - 1)]
            .flags
            .contains(Flags::WRAPLINE)
    }

    /// Write history rows `line_start..=line_end` (oldest-relative `k` indices,
    /// with `history` the current history size) as one joined logical line.
    fn write_logical_line<W: Write>(
        &self,
        writer: &mut W,
        line_start: usize,
        line_end: usize,
        history: usize,
    ) -> io::Result<()> {
        let mut sgr = SgrState::default();
        let mut out = String::with_capacity((line_end - line_start + 1) * self.cols as usize * 2);
        for k in line_start..=line_end {
            let line_idx = -((history - k) as i32);
            self.append_row_cells(Line(line_idx), &mut sgr, &mut out);
        }
        Self::finish_logical_line(&mut out, &sgr);
        writeln!(writer, "{}", out)
    }

    /// Write visible rows `line_start..=line_end` (0-based screen rows) as one
    /// joined logical line.
    fn write_visible_logical_line<W: Write>(
        &self,
        writer: &mut W,
        line_start: i32,
        line_end: i32,
    ) -> io::Result<()> {
        let mut sgr = SgrState::default();
        let mut out = String::with_capacity(self.cols as usize * 2);
        for row in line_start..=line_end {
            self.append_row_cells(Line(row), &mut sgr, &mut out);
        }
        Self::finish_logical_line(&mut out, &sgr);
        writeln!(writer, "{}", out)
    }

    /// Close out an in-progress logical line: emit a final SGR reset if any
    /// style is active, then trim trailing blanks (color codes are preserved).
    fn finish_logical_line(out: &mut String, sgr: &SgrState) {
        if sgr.has_style() {
            out.push_str("\x1b[0m");
        }
        let trimmed_len = out.trim_end_matches([' ', '\0']).len();
        out.truncate(trimmed_len);
    }

    /// Append all cells of one grid row to `out`, threading the SGR state so a
    /// joined logical line carries continuous colors across wrapped rows and
    /// only resets once at the end. Color codes are emitted as truecolor; the
    /// buffer renderer interprets these (see `src/primitives/ansi.rs`).
    fn append_row_cells(&self, line: Line, sgr: &mut SgrState, out: &mut String) {
        use alacritty_terminal::term::cell::Flags;

        let grid = self.term.grid();
        let row_data = &grid[line];

        for col in 0..self.cols as usize {
            let cell = &row_data[Column(col)];
            let fg = color_to_rgb(&cell.fg);
            let bg = color_to_rgb(&cell.bg);
            let flags = cell.flags;
            let bold = flags.contains(Flags::BOLD);
            let italic = flags.contains(Flags::ITALIC);
            let underline = flags.contains(Flags::UNDERLINE);

            let fg_changed = fg != sgr.fg;
            let bg_changed = bg != sgr.bg;
            let bold_changed = bold != sgr.bold;
            let italic_changed = italic != sgr.italic;
            let underline_changed = underline != sgr.underline;

            if fg_changed || bg_changed || bold_changed || italic_changed || underline_changed {
                let mut codes: Vec<String> = Vec::new();

                // A turned-off attribute requires a full reset + reapply.
                if (sgr.bold && !bold) || (sgr.italic && !italic) || (sgr.underline && !underline) {
                    codes.push("0".to_string());
                    if bold {
                        codes.push("1".to_string());
                    }
                    if italic {
                        codes.push("3".to_string());
                    }
                    if underline {
                        codes.push("4".to_string());
                    }
                    if let Some((r, g, b)) = fg {
                        codes.push(format!("38;2;{};{};{}", r, g, b));
                    }
                    if let Some((r, g, b)) = bg {
                        codes.push(format!("48;2;{};{};{}", r, g, b));
                    }
                } else {
                    if bold_changed && bold {
                        codes.push("1".to_string());
                    }
                    if italic_changed && italic {
                        codes.push("3".to_string());
                    }
                    if underline_changed && underline {
                        codes.push("4".to_string());
                    }
                    if fg_changed {
                        if let Some((r, g, b)) = fg {
                            codes.push(format!("38;2;{};{};{}", r, g, b));
                        } else {
                            codes.push("39".to_string());
                        }
                    }
                    if bg_changed {
                        if let Some((r, g, b)) = bg {
                            codes.push(format!("48;2;{};{};{}", r, g, b));
                        } else {
                            codes.push("49".to_string());
                        }
                    }
                }

                if !codes.is_empty() {
                    out.push_str(&format!("\x1b[{}m", codes.join(";")));
                }

                sgr.fg = fg;
                sgr.bg = bg;
                sgr.bold = bold;
                sgr.italic = italic;
                sgr.underline = underline;
            }

            out.push(cell.c);
        }
    }

    /// Get the byte offset where scrollback history ends in the backing file.
    ///
    /// Used for truncating the file when re-entering terminal mode
    /// (to remove the visible screen portion).
    pub fn backing_file_history_end(&self) -> u64 {
        self.backing_file_history_end
    }

    /// Set the byte offset where scrollback history ends.
    ///
    /// Call this after flushing scrollback to record the file position.
    pub fn set_backing_file_history_end(&mut self, offset: u64) {
        self.backing_file_history_end = offset;
    }

    /// Get the number of scrollback lines that have been synced to the backing file.
    pub fn synced_history_lines(&self) -> usize {
        self.synced_history_lines
    }

    /// Reset sync state (e.g., when starting fresh or after truncation).
    pub fn reset_sync_state(&mut self) {
        self.synced_history_lines = 0;
        self.synced_logical_lines = 0;
        self.pending_reflow_resync = false;
        self.backing_file_history_end = 0;
    }
}

/// A single cell in the terminal grid
#[derive(Debug, Clone)]
pub struct TerminalCell {
    /// The character
    pub c: char,
    /// Foreground color as RGB
    pub fg: Option<(u8, u8, u8)>,
    /// Background color as RGB
    pub bg: Option<(u8, u8, u8)>,
    /// Bold flag
    pub bold: bool,
    /// Italic flag
    pub italic: bool,
    /// Underline flag
    pub underline: bool,
    /// Inverse video flag
    pub inverse: bool,
}

impl Default for TerminalCell {
    fn default() -> Self {
        Self {
            c: ' ',
            fg: None,
            bg: None,
            bold: false,
            italic: false,
            underline: false,
            inverse: false,
        }
    }
}

/// Running SGR (color/attribute) state while serializing a logical line, so a
/// joined line carries continuous styling across wrapped rows and resets once.
#[derive(Default)]
struct SgrState {
    fg: Option<(u8, u8, u8)>,
    bg: Option<(u8, u8, u8)>,
    bold: bool,
    italic: bool,
    underline: bool,
}

impl SgrState {
    fn has_style(&self) -> bool {
        self.fg.is_some() || self.bg.is_some() || self.bold || self.italic || self.underline
    }
}

/// Convert alacritty color to RGB
fn color_to_rgb(color: &alacritty_terminal::vte::ansi::Color) -> Option<(u8, u8, u8)> {
    use alacritty_terminal::vte::ansi::Color;

    match color {
        Color::Spec(rgb) => Some((rgb.r, rgb.g, rgb.b)),
        Color::Named(named) => {
            // Convert named colors to RGB
            // Using standard ANSI color palette
            let rgb = match named {
                alacritty_terminal::vte::ansi::NamedColor::Black => (0, 0, 0),
                alacritty_terminal::vte::ansi::NamedColor::Red => (205, 49, 49),
                alacritty_terminal::vte::ansi::NamedColor::Green => (13, 188, 121),
                alacritty_terminal::vte::ansi::NamedColor::Yellow => (229, 229, 16),
                alacritty_terminal::vte::ansi::NamedColor::Blue => (36, 114, 200),
                alacritty_terminal::vte::ansi::NamedColor::Magenta => (188, 63, 188),
                alacritty_terminal::vte::ansi::NamedColor::Cyan => (17, 168, 205),
                alacritty_terminal::vte::ansi::NamedColor::White => (229, 229, 229),
                alacritty_terminal::vte::ansi::NamedColor::BrightBlack => (102, 102, 102),
                alacritty_terminal::vte::ansi::NamedColor::BrightRed => (241, 76, 76),
                alacritty_terminal::vte::ansi::NamedColor::BrightGreen => (35, 209, 139),
                alacritty_terminal::vte::ansi::NamedColor::BrightYellow => (245, 245, 67),
                alacritty_terminal::vte::ansi::NamedColor::BrightBlue => (59, 142, 234),
                alacritty_terminal::vte::ansi::NamedColor::BrightMagenta => (214, 112, 214),
                alacritty_terminal::vte::ansi::NamedColor::BrightCyan => (41, 184, 219),
                alacritty_terminal::vte::ansi::NamedColor::BrightWhite => (255, 255, 255),
                alacritty_terminal::vte::ansi::NamedColor::Foreground => return None,
                alacritty_terminal::vte::ansi::NamedColor::Background => return None,
                alacritty_terminal::vte::ansi::NamedColor::Cursor => return None,
                _ => return None,
            };
            Some(rgb)
        }
        Color::Indexed(idx) => {
            // Convert 256-color index to RGB
            // Standard 256-color palette
            let idx = *idx as usize;
            if idx < 16 {
                // Standard colors (same as named)
                let colors = [
                    (0, 0, 0),       // Black
                    (205, 49, 49),   // Red
                    (13, 188, 121),  // Green
                    (229, 229, 16),  // Yellow
                    (36, 114, 200),  // Blue
                    (188, 63, 188),  // Magenta
                    (17, 168, 205),  // Cyan
                    (229, 229, 229), // White
                    (102, 102, 102), // Bright Black
                    (241, 76, 76),   // Bright Red
                    (35, 209, 139),  // Bright Green
                    (245, 245, 67),  // Bright Yellow
                    (59, 142, 234),  // Bright Blue
                    (214, 112, 214), // Bright Magenta
                    (41, 184, 219),  // Bright Cyan
                    (255, 255, 255), // Bright White
                ];
                Some(colors[idx])
            } else if idx < 232 {
                // 216 color cube (6x6x6)
                let idx = idx - 16;
                let r = (idx / 36) * 51;
                let g = ((idx / 6) % 6) * 51;
                let b = (idx % 6) * 51;
                Some((r as u8, g as u8, b as u8))
            } else {
                // 24 grayscale colors
                let gray = (idx - 232) * 10 + 8;
                Some((gray as u8, gray as u8, gray as u8))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_terminal_state_new() {
        let state = TerminalState::new(80, 24);
        assert_eq!(state.size(), (80, 24));
        assert!(state.is_dirty());
    }

    #[test]
    fn test_terminal_process_output() {
        let mut state = TerminalState::new(80, 24);
        state.process_output(b"Hello, World!");
        let content = state.content_string();
        assert!(content.contains("Hello, World!"));
    }

    #[test]
    fn test_terminal_resize() {
        let mut state = TerminalState::new(80, 24);
        state.mark_clean();
        assert!(!state.is_dirty());

        state.resize(100, 30);
        assert_eq!(state.size(), (100, 30));
        assert!(state.is_dirty());
    }

    /// Resize re-anchors `synced_history_lines` to the reflowed grid so the
    /// incremental streamer can't lose/duplicate lines afterwards.
    #[test]
    fn test_resize_reanchors_synced_history() {
        let mut state = TerminalState::new(80, 24);
        for i in 0..200 {
            state.process_output(format!("line {i}\r\n").as_bytes());
        }
        // Drain into the backing-file mirror (a Vec sink).
        let mut sink: Vec<u8> = Vec::new();
        state.flush_new_scrollback(&mut sink).unwrap();
        assert_eq!(state.synced_history_lines(), state.history_size());

        // Widen: reflow shrinks history; counter must follow, not stay stale.
        state.resize(200, 24);
        assert_eq!(state.synced_history_lines(), state.history_size());
        // No phantom "new" lines to flush right after a resize.
        let mut after: Vec<u8> = Vec::new();
        assert_eq!(state.flush_new_scrollback(&mut after).unwrap(), 0);
    }

    /// A pure height *shrink* (cols unchanged) pushes the top visible rows into
    /// scrollback. Those rows are genuinely new history, so the counter must
    /// stay low enough that the next flush writes them — they must not be
    /// dropped. Guards against re-anchoring `synced` on every resize.
    #[test]
    fn test_height_shrink_streams_spilled_rows() {
        let mut state = TerminalState::new(80, 24);
        // Fill the screen (no scroll-off yet) with identifiable rows.
        for i in 0..24 {
            state.process_output(format!("row{i:02}\r\n").as_bytes());
        }
        let mut sink: Vec<u8> = Vec::new();
        state.flush_new_scrollback(&mut sink).unwrap();
        let before = state.synced_history_lines();

        // Shrink height only — alacritty pushes the top rows into history.
        state.resize(80, 10);
        assert!(
            state.history_size() > before,
            "shrink should push rows into history"
        );
        // The spilled rows are new content and must be flushed (not skipped).
        let mut spill: Vec<u8> = Vec::new();
        let written = state.flush_new_scrollback(&mut spill).unwrap();
        assert!(written > 0, "spilled rows must be streamed, got {written}");
    }

    /// A pure height *grow* (cols unchanged) pulls rows from scrollback back
    /// onto the screen. Those rows are already in the backing file, so when
    /// they later scroll off again they must not be streamed a second time.
    #[test]
    fn test_height_grow_does_not_reflow_duplicate() {
        let mut state = TerminalState::new(80, 24);
        for i in 0..100 {
            state.process_output(format!("line {i}\r\n").as_bytes());
        }
        let mut sink: Vec<u8> = Vec::new();
        state.flush_new_scrollback(&mut sink).unwrap();
        let synced_before = state.synced_history_lines();

        // Grow height only: pulls rows from history back onto the screen.
        state.resize(80, 40);
        // Counter is left untouched; the flush guard suppresses the pulled rows.
        assert_eq!(state.synced_history_lines(), synced_before);
        let mut after: Vec<u8> = Vec::new();
        assert_eq!(
            state.flush_new_scrollback(&mut after).unwrap(),
            0,
            "growing height must not re-stream rows already in the backing file"
        );
    }

    // ---- #5 logical-line capture -------------------------------------------

    /// Min/max occurrences of each marker `L{i:05}#` for i in 0..n across the
    /// full captured record `text` (everything streamed plus the final screen).
    fn marker_counts(text: &str, n: usize) -> (usize, usize) {
        let mut min = usize::MAX;
        let mut max = 0;
        for i in 0..n {
            let c = text.matches(&format!("L{i:05}#")).count();
            min = min.min(c);
            max = max.max(c);
        }
        (min, max)
    }

    /// A wrapped line is stored as ONE unwrapped logical line in the backing
    /// file (not hard-split at the capture width), so the editor can re-wrap it.
    #[test]
    fn test_wrapped_line_stored_as_single_logical_line() {
        let mut state = TerminalState::new(40, 24);
        // ~100 chars at width 40 → wraps to 3 physical rows.
        let long = "X".repeat(100);
        state.process_output(format!("{long}\r\n").as_bytes());
        // Scroll it off the screen.
        for _ in 0..24 {
            state.process_output(b"y\r\n");
        }
        let mut sink: Vec<u8> = Vec::new();
        state.flush_new_scrollback(&mut sink).unwrap();
        let text = String::from_utf8_lossy(&sink);
        let xline = text.lines().find(|l| l.contains("XXXX")).unwrap();
        assert_eq!(
            xline.chars().filter(|&c| c == 'X').count(),
            100,
            "the wrapped line must be rejoined into one 100-char logical line"
        );
    }

    /// The headline scenario: lots of scrollback, then MANY resizes (including
    /// simultaneous width+height changes) with no viewing in between, then a
    /// final capture. Not a single logical line may be lost.
    #[test]
    fn test_no_scrollback_lost_across_many_mixed_resizes() {
        let mut state = TerminalState::new(80, 24);
        let n = 500;
        let mut sink: Vec<u8> = Vec::new();
        // Emit in batches, flushing after each (as the PTY read loop would),
        // and resize between batches — width, height, and both at once.
        let sizes = [
            (120u16, 24u16),
            (60, 30),
            (200, 18),
            (90, 40),
            (50, 22),
            (160, 50),
            (70, 20),
        ];
        for b in 0..n / 20 {
            for i in 0..20 {
                let idx = b * 20 + i;
                // Mix in lines long enough to wrap at the narrow widths.
                let pad = "=".repeat((idx % 90) + 5);
                state.process_output(format!("L{idx:05}# {pad}\r\n").as_bytes());
            }
            state.flush_new_scrollback(&mut sink).unwrap();
            let (w, h) = sizes[b % sizes.len()];
            state.resize(w, h);
        }
        // Capture the residual scrollback + visible screen into the same stream
        // a viewer/session-save would read.
        state.flush_new_scrollback(&mut sink).unwrap();
        state.append_visible_screen(&mut sink).unwrap();
        let text = String::from_utf8_lossy(&sink);

        let (min, max) = marker_counts(&text, n);
        // PRIMARY GOAL: never lose a scrollback line, no matter the resizes.
        assert!(
            min >= 1,
            "lost scrollback line(s): some marker missing (min={min})"
        );
        // Duplication is a tolerated last resort (a grow can overlap the visible
        // tail with committed history) but must stay bounded by the screen height,
        // never unbounded growth.
        assert!(max <= 3, "excessive duplication (max={max})");
    }

    /// `clear` (ESC[3J clears scrollback) must not stall capture: lines printed
    /// afterwards have to keep landing in the backing file, appended after the
    /// scrollback that was already committed.
    #[test]
    fn test_clear_scrollback_resumes_capture() {
        let mut state = TerminalState::new(80, 24);
        let mut sink: Vec<u8> = Vec::new();
        for i in 0..100 {
            state.process_output(format!("OLD{i:04}#\r\n").as_bytes());
        }
        state.flush_new_scrollback(&mut sink).unwrap();
        assert!(state.synced_logical_lines > 0);

        // Clear scrollback (what `clear` emits), then print more.
        state.process_output(b"\x1b[3J\x1b[H\x1b[2J");
        for i in 0..100 {
            state.process_output(format!("NEW{i:04}#\r\n").as_bytes());
        }
        state.flush_new_scrollback(&mut sink).unwrap();
        state.append_visible_screen(&mut sink).unwrap();

        let text = String::from_utf8_lossy(&sink);
        // Old scrollback preserved, AND post-clear output captured (the bug was
        // post-clear output being silently dropped).
        assert!(text.contains("OLD0000#"), "pre-clear scrollback lost");
        assert!(text.contains("NEW0000#"), "post-clear output dropped");
        assert!(text.contains("NEW0090#"), "later post-clear output dropped");
    }

    /// Entering/leaving the alternate screen (vim, less, htop) reports zero
    /// history transiently; it must not be mistaken for a clear, nor cause the
    /// pre-alt-screen scrollback to be re-emitted on exit.
    #[test]
    fn test_alt_screen_roundtrip_no_duplicate() {
        let mut state = TerminalState::new(80, 24);
        let mut sink: Vec<u8> = Vec::new();
        for i in 0..100 {
            state.process_output(format!("BASE{i:04}#\r\n").as_bytes());
        }
        state.flush_new_scrollback(&mut sink).unwrap();

        // Enter alt screen, draw, leave alt screen.
        state.process_output(b"\x1b[?1049h");
        state.process_output(b"full screen app drawing\r\nmore\r\n");
        state.process_output(b"\x1b[?1049l");
        // A couple of new real lines after returning.
        for i in 0..5 {
            state.process_output(format!("AFTER{i:04}#\r\n").as_bytes());
        }
        state.flush_new_scrollback(&mut sink).unwrap();
        state.append_visible_screen(&mut sink).unwrap();

        let text = String::from_utf8_lossy(&sink);
        // No base line duplicated by the alt-screen round trip.
        for i in 0..100 {
            assert!(
                text.matches(&format!("BASE{i:04}#")).count() <= 1,
                "alt-screen round trip duplicated BASE{i:04}"
            );
        }
        assert!(
            text.contains("AFTER0000#"),
            "post-alt-screen output dropped"
        );
    }

    /// Resizing the width *while* the alternate screen is up reflows the hidden
    /// primary grid; the re-anchor is deferred to alt-screen exit. Afterwards,
    /// new output must still be captured (no loss) and the pre-alt scrollback
    /// must not be wholesale re-written.
    #[test]
    fn test_resize_during_alt_screen_then_capture() {
        let mut state = TerminalState::new(80, 24);
        let mut sink: Vec<u8> = Vec::new();
        for i in 0..150 {
            // Lines long enough to wrap differently across the resize.
            let pad = "=".repeat(60);
            state.process_output(format!("PRE{i:04}# {pad}\r\n").as_bytes());
        }
        state.flush_new_scrollback(&mut sink).unwrap();

        // Enter alt screen, resize width (reflows primary underneath), exit.
        state.process_output(b"\x1b[?1049h");
        state.resize(40, 24);
        state.resize(120, 24);
        state.process_output(b"\x1b[?1049l");
        for i in 0..150 {
            state.process_output(format!("POST{i:04}# x\r\n").as_bytes());
        }
        state.flush_new_scrollback(&mut sink).unwrap();
        state.append_visible_screen(&mut sink).unwrap();

        let text = String::from_utf8_lossy(&sink);
        // Post-alt output fully captured (the deferred re-anchor must not skip).
        for i in 0..150 {
            assert!(
                text.contains(&format!("POST{i:04}#")),
                "post-alt output lost POST{i:04}"
            );
        }
        // Pre-alt scrollback preserved and not duplicated en masse.
        for i in 0..150 {
            assert!(
                text.matches(&format!("PRE{i:04}#")).count() <= 2,
                "pre-alt scrollback duplicated PRE{i:04}"
            );
        }
        assert!(text.contains("PRE0000#"), "pre-alt scrollback lost");
    }

    /// `last_visible_line` returns the text on the cursor row, with
    /// the alacritty right-edge padding trimmed. This is the payload
    /// the `terminal_output` plugin hook surfaces to the Orchestrator
    /// state machine for prompt detection.
    #[test]
    fn test_last_visible_line_returns_cursor_row() {
        let mut state = TerminalState::new(80, 24);
        state.process_output(b"hello\r\nworld");
        // Cursor is now on the second line after writing "world".
        assert_eq!(state.last_visible_line(), "world");
    }

    /// Empty cells past the visible run are stripped, but a single
    /// trailing space typed by the program (typical for prompts like
    /// `"(Y/n): "`) is preserved.
    #[test]
    fn test_last_visible_line_preserves_prompt_trailing_space() {
        let mut state = TerminalState::new(80, 24);
        state.process_output(b"Continue? (Y/n): ");
        // The literal trailing space is real prompt text, not grid
        // padding past the cursor, so it must survive.
        assert_eq!(state.last_visible_line(), "Continue? (Y/n): ");
    }

    /// A row that has only ever been the right-edge padding renders
    /// as the empty string, not 80 spaces.
    #[test]
    fn test_last_visible_line_blank_row_is_empty() {
        let state = TerminalState::new(80, 24);
        assert_eq!(state.last_visible_line(), "");
    }

    #[test]
    fn test_flush_new_scrollback_no_history() {
        // When there's no scrollback history, flush should return 0
        let mut state = TerminalState::new(80, 24);
        state.process_output(b"Hello");

        let mut buffer = Vec::new();
        let count = state.flush_new_scrollback(&mut buffer).unwrap();

        assert_eq!(count, 0, "No scrollback yet, should flush 0 lines");
        assert!(buffer.is_empty(), "Buffer should be empty");
    }

    #[test]
    fn test_flush_new_scrollback_after_scroll() {
        // Generate enough output to create scrollback
        let mut state = TerminalState::new(80, 10); // Small terminal to trigger scrollback quickly

        // Generate output that exceeds the terminal height
        for i in 1..=20 {
            state.process_output(format!("Line {}\r\n", i).as_bytes());
        }

        let mut buffer = Vec::new();
        let count = state.flush_new_scrollback(&mut buffer).unwrap();

        // Should have some scrollback lines
        let output = String::from_utf8_lossy(&buffer);
        eprintln!(
            "Scrollback test: count={}, synced={}, buffer_len={}, output:\n{}",
            count,
            state.synced_history_lines(),
            buffer.len(),
            output
        );

        // The first lines should have scrolled off
        assert!(count > 0, "Should have some scrollback lines");
        assert!(
            output.contains("Line 1"),
            "Scrollback should contain Line 1"
        );
    }

    #[test]
    fn test_append_visible_screen() {
        let mut state = TerminalState::new(80, 5);
        state.process_output(b"Line A\r\nLine B\r\nLine C\r\n");

        let mut buffer = Vec::new();
        state.append_visible_screen(&mut buffer).unwrap();

        let output = String::from_utf8_lossy(&buffer);
        assert!(
            output.contains("Line A"),
            "Visible screen should contain Line A"
        );
        assert!(
            output.contains("Line B"),
            "Visible screen should contain Line B"
        );
        assert!(
            output.contains("Line C"),
            "Visible screen should contain Line C"
        );
    }

    #[test]
    fn test_scrollback_then_visible_no_duplication() {
        // Test the full flow: scrollback lines + visible screen should not duplicate
        let mut state = TerminalState::new(80, 5); // Small terminal

        // Generate output that creates scrollback
        // Use unique markers that won't accidentally match each other
        for i in 1..=15 {
            state.process_output(format!("UNIQUELINE_{:02}\r\n", i).as_bytes());
        }

        // Flush scrollback
        let mut scrollback_buffer = Vec::new();
        let scrollback_count = state.flush_new_scrollback(&mut scrollback_buffer).unwrap();
        let scrollback_output = String::from_utf8_lossy(&scrollback_buffer);

        // Append visible screen
        let mut visible_buffer = Vec::new();
        state.append_visible_screen(&mut visible_buffer).unwrap();
        let visible_output = String::from_utf8_lossy(&visible_buffer);

        eprintln!(
            "Scrollback ({} lines):\n{}",
            scrollback_count, scrollback_output
        );
        eprintln!("Visible screen:\n{}", visible_output);

        // Combined output should have each line exactly once
        let combined = format!("{}{}", scrollback_output, visible_output);

        // Count occurrences of each line
        for i in 1..=15 {
            let pattern = format!("UNIQUELINE_{:02}", i);
            let count = combined.matches(&pattern).count();
            assert!(
                count >= 1,
                "Line {} should appear at least once, but found {} times",
                i,
                count
            );
            // Allow for some overlap at boundaries, but not excessive duplication
            assert!(
                count <= 2,
                "Line {} appears {} times - too much duplication",
                i,
                count
            );
        }
    }

    #[test]
    fn test_backing_file_history_end_tracking() {
        let mut state = TerminalState::new(80, 5);

        // Initially should be 0
        assert_eq!(state.backing_file_history_end(), 0);

        // Set it
        state.set_backing_file_history_end(1234);
        assert_eq!(state.backing_file_history_end(), 1234);

        // Reset should clear it
        state.reset_sync_state();
        assert_eq!(state.backing_file_history_end(), 0);
        assert_eq!(state.synced_history_lines(), 0);
    }

    #[test]
    fn test_multiple_flush_cycles_no_duplication() {
        use alacritty_terminal::grid::Dimensions;

        // Simulate multiple enter/exit terminal mode cycles
        let mut state = TerminalState::new(80, 5);

        // First batch of output (10 lines in 5-row terminal)
        // Lines 1-6 scroll into history, lines 7-10 are visible
        for i in 1..=10 {
            state.process_output(format!("Batch1-Line{}\r\n", i).as_bytes());
        }

        let history1 = state.term.grid().history_size();
        eprintln!("After Batch1: history_size={}", history1);
        assert_eq!(
            history1, 6,
            "After 10 lines in 5-row terminal, 6 should be in history"
        );

        // First flush - should get lines 1-6
        let mut buffer1 = Vec::new();
        let count1 = state.flush_new_scrollback(&mut buffer1).unwrap();
        let output1 = String::from_utf8_lossy(&buffer1);
        eprintln!("First flush: {} lines\n{}", count1, output1);

        assert_eq!(count1, 6);
        assert!(output1.contains("Batch1-Line1"));
        assert!(output1.contains("Batch1-Line6"));
        assert!(
            !output1.contains("Batch1-Line7"),
            "Line 7 should still be visible, not in scrollback"
        );

        // Second flush without new output should return 0
        let mut buffer2 = Vec::new();
        let count2 = state.flush_new_scrollback(&mut buffer2).unwrap();
        assert_eq!(count2, 0, "Second flush without new output should be 0");

        // More output (10 more lines)
        // This pushes Batch1-Line7-10 into history, plus Batch2-Line1-6
        for i in 1..=10 {
            state.process_output(format!("Batch2-Line{}\r\n", i).as_bytes());
        }

        let history3 = state.term.grid().history_size();
        eprintln!("After Batch2: history_size={}", history3);

        // Third flush should get lines that scrolled off since last flush
        // That's Batch1-Line7-10 (4 lines) + Batch2-Line1-6 (6 lines) = 10 lines
        let mut buffer3 = Vec::new();
        let count3 = state.flush_new_scrollback(&mut buffer3).unwrap();
        let output3 = String::from_utf8_lossy(&buffer3);
        eprintln!("Third flush: {} lines\n{}", count3, output3);

        assert_eq!(count3, 10, "Should flush 10 new lines");
        // Should include Batch1 lines 7-10 (they weren't flushed before, were still visible)
        assert!(
            output3.contains("Batch1-Line7"),
            "Batch1-Line7 should be in third flush (was visible, now scrolled)"
        );
        assert!(output3.contains("Batch1-Line10"));
        // Should include Batch2 lines 1-6 (new content that scrolled off)
        assert!(output3.contains("Batch2-Line1"));
        assert!(output3.contains("Batch2-Line6"));
        // Should NOT include Batch1-Line1-6 (already flushed)
        assert!(
            !output3.contains("Batch1-Line1\n"),
            "Batch1-Line1 was already flushed, shouldn't appear again"
        );
        assert!(
            !output3.contains("Batch1-Line6\n"),
            "Batch1-Line6 was already flushed, shouldn't appear again"
        );
    }

    #[test]
    fn test_dsr_cursor_position_response() {
        // Test that sending a DSR (Device Status Report) query generates a response
        // This is critical for Windows ConPTY where PowerShell waits for this response
        let mut state = TerminalState::new(80, 24);

        // Initially the write queue should be empty
        assert!(
            state.drain_pty_write_queue().is_empty(),
            "Write queue should be empty initially"
        );

        // Send DSR query: ESC [ 6 n (request cursor position)
        state.process_output(b"\x1b[6n");

        // The terminal should generate a response: ESC [ row ; col R
        let responses = state.drain_pty_write_queue();
        assert_eq!(responses.len(), 1, "Should have exactly one response");

        let response = &responses[0];
        // Response format: \x1b[row;colR where row and col are 1-based
        // Cursor starts at (0,0) internally, so response should be \x1b[1;1R
        assert!(
            response.starts_with("\x1b["),
            "Response should start with ESC["
        );
        assert!(response.ends_with("R"), "Response should end with R");
        eprintln!("DSR response: {:?}", response);

        // Draining again should return empty
        assert!(
            state.drain_pty_write_queue().is_empty(),
            "Write queue should be empty after draining"
        );
    }

    #[test]
    fn test_dsr_response_after_cursor_move() {
        // Test DSR response reflects actual cursor position
        let mut state = TerminalState::new(80, 24);

        // Move cursor to row 5, column 10 using CUP (Cursor Position)
        // ESC [ 5 ; 10 H
        state.process_output(b"\x1b[5;10H");

        // Request cursor position
        state.process_output(b"\x1b[6n");

        let responses = state.drain_pty_write_queue();
        assert_eq!(responses.len(), 1);

        let response = &responses[0];
        // Should report position as row 5, col 10
        assert_eq!(response, "\x1b[5;10R", "Response should be \\x1b[5;10R");
    }

    /// OSC 2 ("set window title") drives the stored terminal title so the
    /// buffer's tab can auto-adjust to whatever the program requested.
    #[test]
    fn test_osc_set_window_title() {
        let mut state = TerminalState::new(80, 24);
        assert_eq!(state.title(), "");
        // ESC ] 2 ; <title> BEL
        state.process_output(b"\x1b]2;my-shell: ~/project\x07");
        assert_eq!(state.title(), "my-shell: ~/project");
    }

    /// OSC 0 sets both the icon name and the window title; we treat it the
    /// same as OSC 2 for the buffer title.
    #[test]
    fn test_osc_set_icon_and_window_title() {
        let mut state = TerminalState::new(80, 24);
        state.process_output(b"\x1b]0;vim README.md\x07");
        assert_eq!(state.title(), "vim README.md");
    }

    /// A later OSC title overrides an earlier one, and the title can arrive
    /// in the same chunk as other output.
    #[test]
    fn test_osc_title_updates_and_mixes_with_output() {
        let mut state = TerminalState::new(80, 24);
        state.process_output(b"\x1b]2;first\x07hello");
        assert_eq!(state.title(), "first");
        state.process_output(b"world\x1b]2;second\x07");
        assert_eq!(state.title(), "second");
        // The printable bytes still landed on the grid.
        assert!(state.content_string().contains("helloworld"));
    }

    /// OSC 7 with a `file://host/path` payload, BEL-terminated, updates the
    /// tracked cwd. The emulator still drops the sequence (no stray output).
    #[test]
    fn test_osc7_sets_cwd_bel_terminated() {
        let mut state = TerminalState::new(80, 24);
        assert_eq!(state.cwd(), None);
        state.process_output(b"\x1b]7;file://myhost/home/user/project\x07ok");
        assert_eq!(
            state.cwd(),
            Some(std::path::Path::new("/home/user/project"))
        );
        // The sequence itself must not leak onto the grid; only "ok" prints.
        let content = state.content_string();
        assert!(content.contains("ok"));
        assert!(!content.contains("file://"));
    }

    /// OSC 7 terminated by ST (`ESC \`) is recognized too.
    #[test]
    fn test_osc7_st_terminated() {
        let mut state = TerminalState::new(80, 24);
        state.process_output(b"\x1b]7;file://host/var/log\x1b\\");
        assert_eq!(state.cwd(), Some(std::path::Path::new("/var/log")));
    }

    /// Percent-escapes in the OSC 7 path are decoded (spaces, etc.).
    #[test]
    fn test_osc7_percent_decoded() {
        let mut state = TerminalState::new(80, 24);
        state.process_output(b"\x1b]7;file://host/home/user/my%20dir\x07");
        assert_eq!(state.cwd(), Some(std::path::Path::new("/home/user/my dir")));
    }

    /// An OSC 7 sequence split across two PTY reads is still captured — the
    /// scanner state persists between `process_output` calls.
    #[test]
    fn test_osc7_split_across_reads() {
        let mut state = TerminalState::new(80, 24);
        state.process_output(b"\x1b]7;file://host/home/u");
        assert_eq!(state.cwd(), None);
        state.process_output(b"ser/split\x07");
        assert_eq!(state.cwd(), Some(std::path::Path::new("/home/user/split")));
    }

    /// A later OSC 7 overrides an earlier one (tracks `cd`).
    #[test]
    fn test_osc7_updates_on_cd() {
        let mut state = TerminalState::new(80, 24);
        state.process_output(b"\x1b]7;file://host/first\x07");
        assert_eq!(state.cwd(), Some(std::path::Path::new("/first")));
        state.process_output(b"\x1b]7;file://host/second/dir\x07");
        assert_eq!(state.cwd(), Some(std::path::Path::new("/second/dir")));
    }

    /// A Windows `file:///C:/dir` URI is parsed to a drive-absolute path. The
    /// leading-slash strip and drive acceptance are host-OS-independent (a
    /// remote Windows shell can report this while Fresh runs on Linux), so this
    /// is asserted on every platform.
    #[test]
    fn test_osc7_windows_drive_path() {
        let mut state = TerminalState::new(80, 24);
        state.process_output(b"\x1b]7;file:///C:/Users/me/proj\x07");
        assert_eq!(state.cwd(), Some(std::path::Path::new("C:/Users/me/proj")));
    }

    /// A bare (non-`file://`) absolute path payload is accepted as a fallback.
    #[test]
    fn test_osc7_bare_path_fallback() {
        let mut state = TerminalState::new(80, 24);
        state.process_output(b"\x1b]7;/opt/work\x07");
        assert_eq!(state.cwd(), Some(std::path::Path::new("/opt/work")));
    }

    /// A relative or empty payload is rejected (cwd stays unchanged).
    #[test]
    fn test_osc7_rejects_relative() {
        let mut state = TerminalState::new(80, 24);
        state.process_output(b"\x1b]7;file://host/good\x07");
        state.process_output(b"\x1b]7;relative/path\x07");
        // The relative payload is ignored; the previous valid cwd is kept.
        assert_eq!(state.cwd(), Some(std::path::Path::new("/good")));
    }
}
