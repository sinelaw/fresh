//! Terminal state using alacritty_terminal for emulation
//!
//! This module wraps alacritty_terminal to provide:
//! - VT100/ANSI escape sequence parsing
//! - Terminal grid management
//! - Cursor state tracking
//! - Incremental scrollback streaming to backing file

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

        // New scrollback lines are at indices from -(current_history) to -(synced+1)
        // We write oldest-first to maintain append order
        for i in 0..new_count {
            // Line index: oldest unsynced line first
            let line_idx = -((current_history - i) as i32);
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
    pub fn append_visible_screen<W: Write>(&self, writer: &mut W) -> io::Result<()> {
        for row in 0..self.rows as i32 {
            self.write_grid_line(writer, Line(row))?;
        }
        Ok(())
    }

    /// Write a single grid line to the writer, trimming trailing whitespace.
    fn write_grid_line<W: Write>(&self, writer: &mut W, line: Line) -> io::Result<()> {
        let grid = self.term.grid();
        let row_data = &grid[line];

        let mut line_str = String::with_capacity(self.cols as usize);
        for col in 0..self.cols as usize {
            line_str.push(row_data[Column(col)].c);
        }

        writeln!(writer, "{}", line_str.trim_end())
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
}
