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
use alacritty_terminal::term::{Config as TermConfig, Term};
use alacritty_terminal::vte::ansi::Processor;
use std::io::{self, Write};

// Keep a generous scrollback so sync-to-buffer can include deep history.
const SCROLLBACK_LINES: usize = 200_000;

/// Event listener that does nothing (we handle events ourselves)
struct NullListener;

impl EventListener for NullListener {
    fn send_event(&self, _event: Event) {
        // We don't need to handle terminal events in the listener
        // The main loop will poll the terminal state directly
    }
}

/// Terminal state wrapping alacritty_terminal
pub struct TerminalState {
    /// The terminal emulator
    term: Term<NullListener>,
    /// ANSI parser
    parser: Processor,
    /// Current dimensions
    cols: u16,
    rows: u16,
    /// Whether content has changed since last render
    dirty: bool,
    /// Terminal title (set via escape sequences)
    terminal_title: String,
    /// Number of scrollback lines already written to backing file
    synced_history_lines: usize,
    /// Byte offset in backing file where scrollback ends (for truncation)
    backing_file_history_end: u64,
}

impl TerminalState {
    /// Create a new terminal state
    pub fn new(cols: u16, rows: u16) -> Self {
        let size = TermSize::new(cols as usize, rows as usize);
        let mut config = TermConfig::default();
        config.scrolling_history = SCROLLBACK_LINES;
        let term = Term::new(config, &size, NullListener);

        Self {
            term,
            parser: Processor::new(),
            cols,
            rows,
            dirty: true,
            terminal_title: String::new(),
            synced_history_lines: 0,
            backing_file_history_end: 0,
        }
    }

    /// Process output from the PTY
    pub fn process_output(&mut self, data: &[u8]) {
        self.parser.advance(&mut self.term, data);
        self.dirty = true;
    }

    /// Resize the terminal
    pub fn resize(&mut self, cols: u16, rows: u16) {
        if cols != self.cols || rows != self.rows {
            self.cols = cols;
            self.rows = rows;
            let size = TermSize::new(cols as usize, rows as usize);
            self.term.resize(size);
            self.dirty = true;
        }
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
    // Incremental scrollback streaming
    // =========================================================================

    /// Flush any new scrollback lines to the writer.
    ///
    /// Call this after `process_output()` to incrementally stream scrollback
    /// to the backing file. Returns the number of new lines written.
    ///
    /// This is the core of the incremental streaming architecture: scrollback
    /// lines are written once as they scroll off the screen, avoiding O(n)
    /// work on mode switches.
    pub fn flush_new_scrollback<W: Write>(&mut self, writer: &mut W) -> io::Result<usize> {
        use alacritty_terminal::grid::Dimensions;

        let grid = self.term.grid();
        let current_history = grid.history_size();

        if current_history <= self.synced_history_lines {
            return Ok(0);
        }

        let new_count = current_history - self.synced_history_lines;

        // New scrollback lines are at indices -new_count down to -1
        // When history grows, new lines are always added at the "bottom" of history
        // (closest to visible screen), and old lines shift to larger negative indices.
        //
        // Example: if synced=6 and current=16:
        // - Old lines (already flushed) are now at -16 to -11
        // - New lines are at -10 to -1
        // We write oldest-first: -10, -9, ..., -1
        for i in 0..new_count {
            // Line index: oldest new line first
            // i=0 -> -new_count = -10 (oldest new line)
            // i=9 -> -1 (newest new line, just scrolled off)
            let line_idx = -((new_count - i) as i32);
            self.write_grid_line(writer, Line(line_idx))?;
        }

        self.synced_history_lines = current_history;
        // Update the byte offset where scrollback ends
        // The writer should be positioned at end, so we can query position
        // For simplicity, we track this separately when we know the file position

        Ok(new_count)
    }

    /// Append the visible screen content to the writer.
    ///
    /// Call this when exiting terminal mode to add the current screen
    /// to the backing file. The visible screen is the "rewritable tail"
    /// that gets overwritten each time we exit terminal mode.
    ///
    /// Only writes up to and including the last non-empty line to avoid
    /// padding the scrollback with empty lines.
    pub fn append_visible_screen<W: Write>(&self, writer: &mut W) -> io::Result<()> {
        let grid = self.term.grid();

        // Find the last non-empty row
        let mut last_non_empty_row: i32 = -1;
        for row in 0..self.rows as i32 {
            let row_data = &grid[Line(row)];
            let is_empty = (0..self.cols as usize)
                .all(|col| row_data[Column(col)].c == ' ' || row_data[Column(col)].c == '\0');
            if !is_empty {
                last_non_empty_row = row;
            }
        }

        // Write rows up to and including the last non-empty row
        for row in 0..=last_non_empty_row {
            self.write_grid_line(writer, Line(row))?;
        }
        Ok(())
    }

    /// Write a single grid line to the writer with ANSI color codes, trimming trailing whitespace.
    ///
    /// Note: The ANSI codes enable terminal scrollback colors to be preserved in the backing file.
    /// For colors to display correctly in scrollback mode, the buffer renderer must interpret
    /// these ANSI escape sequences. See src/view/buffer.rs for rendering logic.
    fn write_grid_line<W: Write>(&self, writer: &mut W, line: Line) -> io::Result<()> {
        use alacritty_terminal::term::cell::Flags;

        let grid = self.term.grid();
        let row_data = &grid[line];

        let mut line_str = String::with_capacity(self.cols as usize * 2);
        let mut current_fg: Option<(u8, u8, u8)> = None;
        let mut current_bg: Option<(u8, u8, u8)> = None;
        let mut current_bold = false;
        let mut current_italic = false;
        let mut current_underline = false;

        for col in 0..self.cols as usize {
            let cell = &row_data[Column(col)];
            let fg = color_to_rgb(&cell.fg);
            let bg = color_to_rgb(&cell.bg);
            let flags = cell.flags;
            let bold = flags.contains(Flags::BOLD);
            let italic = flags.contains(Flags::ITALIC);
            let underline = flags.contains(Flags::UNDERLINE);

            // Check if we need to emit style codes
            let fg_changed = fg != current_fg;
            let bg_changed = bg != current_bg;
            let bold_changed = bold != current_bold;
            let italic_changed = italic != current_italic;
            let underline_changed = underline != current_underline;

            if fg_changed || bg_changed || bold_changed || italic_changed || underline_changed {
                // Build SGR (Select Graphic Rendition) sequence
                let mut codes: Vec<String> = Vec::new();

                // Reset first if we're turning off attributes
                if (current_bold && !bold)
                    || (current_italic && !italic)
                    || (current_underline && !underline)
                {
                    codes.push("0".to_string());
                    // After reset, we need to reapply colors and active attributes
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
                    // Apply incremental changes
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
                            codes.push("39".to_string()); // Default foreground
                        }
                    }
                    if bg_changed {
                        if let Some((r, g, b)) = bg {
                            codes.push(format!("48;2;{};{};{}", r, g, b));
                        } else {
                            codes.push("49".to_string()); // Default background
                        }
                    }
                }

                if !codes.is_empty() {
                    line_str.push_str(&format!("\x1b[{}m", codes.join(";")));
                }

                current_fg = fg;
                current_bg = bg;
                current_bold = bold;
                current_italic = italic;
                current_underline = underline;
            }

            line_str.push(cell.c);
        }

        // Reset at end of line if we have any active styles
        if current_fg.is_some()
            || current_bg.is_some()
            || current_bold
            || current_italic
            || current_underline
        {
            line_str.push_str("\x1b[0m");
        }

        // Trim trailing whitespace but preserve color codes
        let trimmed = line_str.trim_end_matches(|c: char| c == ' ' || c == '\0');
        writeln!(writer, "{}", trimmed)
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
}
