//! Terminal state using alacritty_terminal for emulation
//!
//! This module wraps alacritty_terminal to provide:
//! - VT100/ANSI escape sequence parsing
//! - Terminal grid management
//! - Cursor state tracking

use alacritty_terminal::event::{Event, EventListener};
use alacritty_terminal::grid::Scroll;
use alacritty_terminal::term::test::TermSize;
use alacritty_terminal::term::{Config as TermConfig, Term};
use alacritty_terminal::vte::ansi::Processor;

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
}

impl TerminalState {
    /// Create a new terminal state
    pub fn new(cols: u16, rows: u16) -> Self {
        let size = TermSize::new(cols as usize, rows as usize);
        let config = TermConfig::default();
        let term = Term::new(config, &size, NullListener);

        Self {
            term,
            parser: Processor::new(),
            cols,
            rows,
            dirty: true,
            terminal_title: String::new(),
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

    /// Scroll up in terminal history (increases display offset)
    pub fn scroll_up(&mut self, lines: usize) {
        self.term.scroll_display(Scroll::Delta(lines as i32));
        self.dirty = true;
    }

    /// Scroll down in terminal history (decreases display offset)
    pub fn scroll_down(&mut self, lines: usize) {
        self.term.scroll_display(Scroll::Delta(-(lines as i32)));
        self.dirty = true;
    }

    /// Scroll to the bottom of the terminal (display offset = 0)
    pub fn scroll_to_bottom(&mut self) {
        self.term.scroll_display(Scroll::Bottom);
        self.dirty = true;
    }

    /// Get the current scroll offset (0 = at bottom, higher = scrolled up)
    pub fn scroll_offset(&self) -> usize {
        self.term.grid().display_offset()
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
