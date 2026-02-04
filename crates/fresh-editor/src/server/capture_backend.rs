//! Capturing backend for ratatui
//!
//! Instead of writing to a terminal, this backend captures all output
//! to a buffer that can be sent to clients.

use ratatui::backend::{Backend, ClearType, WindowSize};
use ratatui::buffer::Cell;
use ratatui::layout::{Position, Size};
use ratatui::style::{Color, Modifier};
use std::io::{self, Write};

/// A backend that captures output to a buffer
pub struct CaptureBackend {
    /// Buffer holding the captured ANSI output
    buffer: Vec<u8>,
    /// Current terminal size
    size: Size,
    /// Current cursor position
    cursor: Position,
    /// Whether cursor is visible
    cursor_visible: bool,
    /// Current style state for diff optimization
    current_fg: Color,
    current_bg: Color,
    current_modifiers: Modifier,
}

impl CaptureBackend {
    /// Create a new capture backend with the given size
    pub fn new(cols: u16, rows: u16) -> Self {
        Self {
            buffer: Vec::with_capacity(16 * 1024), // 16KB initial capacity
            size: Size::new(cols, rows),
            cursor: Position::new(0, 0),
            cursor_visible: true,
            current_fg: Color::Reset,
            current_bg: Color::Reset,
            current_modifiers: Modifier::empty(),
        }
    }

    /// Take the captured output buffer, leaving an empty buffer
    pub fn take_buffer(&mut self) -> Vec<u8> {
        std::mem::take(&mut self.buffer)
    }

    /// Get a reference to the captured output
    pub fn get_buffer(&self) -> &[u8] {
        &self.buffer
    }

    /// Clear the buffer without returning it
    pub fn clear_buffer(&mut self) {
        self.buffer.clear();
    }

    /// Resize the backend
    pub fn resize(&mut self, cols: u16, rows: u16) {
        self.size = Size::new(cols, rows);
    }

    /// Reset style state to force full output on next draw
    /// Call this when a new client connects to ensure they get a complete frame
    pub fn reset_style_state(&mut self) {
        self.current_fg = Color::Reset;
        self.current_bg = Color::Reset;
        self.current_modifiers = Modifier::empty();
    }

    /// Write ANSI escape sequence to move cursor
    fn write_cursor_position(&mut self, x: u16, y: u16) {
        // CSI row ; col H (1-based)
        write!(self.buffer, "\x1b[{};{}H", y + 1, x + 1).unwrap();
        self.cursor = Position::new(x, y);
    }

    /// Write ANSI escape sequence for style
    fn write_style(&mut self, cell: &Cell) {
        let mut needs_reset = false;
        let mut sgr_params = Vec::new();

        // Check if we need to reset
        if cell.modifier != self.current_modifiers {
            // Check for removed modifiers that require reset
            let removed = self.current_modifiers - cell.modifier;
            if !removed.is_empty() {
                needs_reset = true;
            }
        }

        if needs_reset {
            sgr_params.push(0);
            self.current_fg = Color::Reset;
            self.current_bg = Color::Reset;
            self.current_modifiers = Modifier::empty();
        }

        // Add modifiers
        if cell.modifier.contains(Modifier::BOLD)
            && !self.current_modifiers.contains(Modifier::BOLD)
        {
            sgr_params.push(1);
        }
        if cell.modifier.contains(Modifier::DIM) && !self.current_modifiers.contains(Modifier::DIM)
        {
            sgr_params.push(2);
        }
        if cell.modifier.contains(Modifier::ITALIC)
            && !self.current_modifiers.contains(Modifier::ITALIC)
        {
            sgr_params.push(3);
        }
        if cell.modifier.contains(Modifier::UNDERLINED)
            && !self.current_modifiers.contains(Modifier::UNDERLINED)
        {
            sgr_params.push(4);
        }
        if cell.modifier.contains(Modifier::SLOW_BLINK)
            && !self.current_modifiers.contains(Modifier::SLOW_BLINK)
        {
            sgr_params.push(5);
        }
        if cell.modifier.contains(Modifier::RAPID_BLINK)
            && !self.current_modifiers.contains(Modifier::RAPID_BLINK)
        {
            sgr_params.push(6);
        }
        if cell.modifier.contains(Modifier::REVERSED)
            && !self.current_modifiers.contains(Modifier::REVERSED)
        {
            sgr_params.push(7);
        }
        if cell.modifier.contains(Modifier::HIDDEN)
            && !self.current_modifiers.contains(Modifier::HIDDEN)
        {
            sgr_params.push(8);
        }
        if cell.modifier.contains(Modifier::CROSSED_OUT)
            && !self.current_modifiers.contains(Modifier::CROSSED_OUT)
        {
            sgr_params.push(9);
        }

        // Foreground color
        if cell.fg != self.current_fg {
            self.write_color_params(&mut sgr_params, cell.fg, true);
        }

        // Background color
        if cell.bg != self.current_bg {
            self.write_color_params(&mut sgr_params, cell.bg, false);
        }

        // Write SGR sequence if needed
        if !sgr_params.is_empty() {
            self.buffer.extend_from_slice(b"\x1b[");
            for (i, param) in sgr_params.iter().enumerate() {
                if i > 0 {
                    self.buffer.push(b';');
                }
                write!(self.buffer, "{}", param).unwrap();
            }
            self.buffer.push(b'm');
        }

        self.current_fg = cell.fg;
        self.current_bg = cell.bg;
        self.current_modifiers = cell.modifier;
    }

    /// Add color parameters to SGR sequence
    fn write_color_params(&self, params: &mut Vec<u8>, color: Color, foreground: bool) {
        let base = if foreground { 30 } else { 40 };

        match color {
            Color::Reset => params.push(if foreground { 39 } else { 49 }),
            Color::Black => params.push(base),
            Color::Red => params.push(base + 1),
            Color::Green => params.push(base + 2),
            Color::Yellow => params.push(base + 3),
            Color::Blue => params.push(base + 4),
            Color::Magenta => params.push(base + 5),
            Color::Cyan => params.push(base + 6),
            Color::Gray => params.push(base + 7),
            Color::DarkGray => params.push(base + 60),
            Color::LightRed => params.push(base + 61),
            Color::LightGreen => params.push(base + 62),
            Color::LightYellow => params.push(base + 63),
            Color::LightBlue => params.push(base + 64),
            Color::LightMagenta => params.push(base + 65),
            Color::LightCyan => params.push(base + 66),
            Color::White => params.push(base + 67),
            Color::Indexed(i) => {
                params.push(if foreground { 38 } else { 48 });
                params.push(5);
                params.push(i);
            }
            Color::Rgb(r, g, b) => {
                params.push(if foreground { 38 } else { 48 });
                params.push(2);
                params.push(r);
                params.push(g);
                params.push(b);
            }
        }
    }
}

impl Backend for CaptureBackend {
    type Error = io::Error;

    fn draw<'a, I>(&mut self, content: I) -> io::Result<()>
    where
        I: Iterator<Item = (u16, u16, &'a Cell)>,
    {
        let mut last_pos: Option<(u16, u16)> = None;

        for (x, y, cell) in content {
            // Move cursor if not at expected position
            let needs_move = match last_pos {
                None => true,
                Some((lx, ly)) => {
                    // Check if this is the next position
                    !(ly == y && lx + 1 == x)
                }
            };

            if needs_move {
                self.write_cursor_position(x, y);
            }

            // Write style changes
            self.write_style(cell);

            // Write the character
            let symbol = cell.symbol();
            self.buffer.extend_from_slice(symbol.as_bytes());

            last_pos = Some((x, y));
        }

        Ok(())
    }

    fn hide_cursor(&mut self) -> io::Result<()> {
        // Always emit hide cursor - don't optimize based on state
        // This ensures client terminal always gets the correct state
        self.buffer.extend_from_slice(b"\x1b[?25l");
        self.cursor_visible = false;
        Ok(())
    }

    fn show_cursor(&mut self) -> io::Result<()> {
        // Always emit show cursor for hardware cursor visibility
        self.buffer.extend_from_slice(b"\x1b[?25h");
        self.cursor_visible = true;
        Ok(())
    }

    fn get_cursor_position(&mut self) -> io::Result<Position> {
        Ok(self.cursor)
    }

    fn set_cursor_position<P: Into<Position>>(&mut self, position: P) -> io::Result<()> {
        let pos = position.into();
        self.write_cursor_position(pos.x, pos.y);
        Ok(())
    }

    fn clear(&mut self) -> io::Result<()> {
        // Clear entire screen
        self.buffer.extend_from_slice(b"\x1b[2J");
        // Move cursor to home
        self.buffer.extend_from_slice(b"\x1b[H");
        self.cursor = Position::new(0, 0);
        Ok(())
    }

    fn clear_region(&mut self, clear_type: ClearType) -> io::Result<()> {
        match clear_type {
            ClearType::All => {
                self.buffer.extend_from_slice(b"\x1b[2J");
            }
            ClearType::AfterCursor => {
                self.buffer.extend_from_slice(b"\x1b[J");
            }
            ClearType::BeforeCursor => {
                self.buffer.extend_from_slice(b"\x1b[1J");
            }
            ClearType::CurrentLine => {
                self.buffer.extend_from_slice(b"\x1b[2K");
            }
            ClearType::UntilNewLine => {
                self.buffer.extend_from_slice(b"\x1b[K");
            }
        }
        Ok(())
    }

    fn append_lines(&mut self, n: u16) -> io::Result<()> {
        // Scroll up by n lines
        for _ in 0..n {
            self.buffer.extend_from_slice(b"\x1b[S");
        }
        Ok(())
    }

    fn size(&self) -> io::Result<Size> {
        Ok(self.size)
    }

    fn window_size(&mut self) -> io::Result<WindowSize> {
        // We don't know pixel size, return a reasonable default
        Ok(WindowSize {
            columns_rows: self.size,
            pixels: Size::new(self.size.width * 8, self.size.height * 16),
        })
    }

    fn flush(&mut self) -> io::Result<()> {
        // Nothing to flush - we're just capturing
        Ok(())
    }
}

/// Generate terminal setup sequences
pub fn terminal_setup_sequences() -> Vec<u8> {
    let mut buf = Vec::new();

    // Enter alternate screen
    buf.extend_from_slice(b"\x1b[?1049h");
    // Enable mouse tracking (SGR format)
    buf.extend_from_slice(b"\x1b[?1000h"); // Enable mouse click tracking
    buf.extend_from_slice(b"\x1b[?1002h"); // Enable mouse drag tracking
    buf.extend_from_slice(b"\x1b[?1003h"); // Enable mouse motion tracking (for hover)
    buf.extend_from_slice(b"\x1b[?1006h"); // Enable SGR mouse format
                                           // Enable focus events
    buf.extend_from_slice(b"\x1b[?1004h");
    // Enable bracketed paste
    buf.extend_from_slice(b"\x1b[?2004h");
    // Hide cursor initially
    buf.extend_from_slice(b"\x1b[?25l");

    buf
}

/// Generate terminal teardown sequences
pub fn terminal_teardown_sequences() -> Vec<u8> {
    let mut buf = Vec::new();

    // Show cursor
    buf.extend_from_slice(b"\x1b[?25h");
    // Disable bracketed paste
    buf.extend_from_slice(b"\x1b[?2004l");
    // Disable focus events
    buf.extend_from_slice(b"\x1b[?1004l");
    // Disable mouse tracking
    buf.extend_from_slice(b"\x1b[?1006l");
    buf.extend_from_slice(b"\x1b[?1003l");
    buf.extend_from_slice(b"\x1b[?1002l");
    buf.extend_from_slice(b"\x1b[?1000l");
    // Reset attributes
    buf.extend_from_slice(b"\x1b[0m");
    // Leave alternate screen
    buf.extend_from_slice(b"\x1b[?1049l");

    buf
}

#[cfg(test)]
mod tests {
    use super::*;
    use ratatui::buffer::Buffer;
    use ratatui::style::Style;

    #[test]
    fn test_size_tracks_dimensions() {
        let mut backend = CaptureBackend::new(80, 24);
        assert_eq!(backend.size().unwrap(), Size::new(80, 24));

        backend.resize(120, 40);
        assert_eq!(backend.size().unwrap(), Size::new(120, 40));
    }

    #[test]
    fn test_clear_emits_ansi_clear_sequence() {
        let mut backend = CaptureBackend::new(80, 24);
        backend.clear().unwrap();

        let output = backend.take_buffer();
        // ED (Erase Display) sequence: CSI 2 J
        assert!(output.starts_with(b"\x1b[2J"));
    }

    #[test]
    fn test_draw_outputs_cell_content() {
        let mut backend = CaptureBackend::new(80, 24);

        let mut buffer = Buffer::empty(ratatui::layout::Rect::new(0, 0, 5, 1));
        buffer.set_string(0, 0, "Hello", Style::default());

        let area = buffer.area;
        backend
            .draw(buffer.content.iter().enumerate().map(|(i, cell)| {
                let x = (i as u16) % area.width;
                let y = (i as u16) / area.width;
                (x + area.x, y + area.y, cell)
            }))
            .unwrap();

        let buf = backend.take_buffer();
        let output = String::from_utf8_lossy(&buf);
        assert!(output.contains("Hello"));
    }

    #[test]
    fn test_cursor_visibility_emits_correct_sequences() {
        let mut backend = CaptureBackend::new(80, 24);

        backend.hide_cursor().unwrap();
        let output = backend.take_buffer();
        assert_eq!(output, b"\x1b[?25l"); // DECTCEM hide

        backend.show_cursor().unwrap();
        let output = backend.take_buffer();
        assert_eq!(output, b"\x1b[?25h"); // DECTCEM show
    }

    #[test]
    fn test_cursor_visibility_always_emits_hide() {
        let mut backend = CaptureBackend::new(80, 24);

        backend.hide_cursor().unwrap();
        backend.clear_buffer();

        // Second hide should still emit (no optimization - ensures client sync)
        backend.hide_cursor().unwrap();
        assert_eq!(backend.take_buffer(), b"\x1b[?25l");
    }

    #[test]
    fn test_take_buffer_clears_internal_buffer() {
        let mut backend = CaptureBackend::new(80, 24);
        backend.clear().unwrap();

        let first = backend.take_buffer();
        assert!(!first.is_empty());

        let second = backend.take_buffer();
        assert!(second.is_empty());
    }

    #[test]
    fn test_setup_sequences_enable_features() {
        let setup = terminal_setup_sequences();
        let setup_str = String::from_utf8_lossy(&setup);

        // Alternate screen
        assert!(setup_str.contains("\x1b[?1049h"));
        // Mouse tracking
        assert!(setup_str.contains("\x1b[?1000h"));
        // Focus events
        assert!(setup_str.contains("\x1b[?1004h"));
    }

    #[test]
    fn test_teardown_sequences_disable_features() {
        let teardown = terminal_teardown_sequences();
        let teardown_str = String::from_utf8_lossy(&teardown);

        // Leave alternate screen
        assert!(teardown_str.contains("\x1b[?1049l"));
        // Reset attributes
        assert!(teardown_str.contains("\x1b[0m"));
    }

    #[test]
    fn test_clear_region_variants() {
        let mut backend = CaptureBackend::new(80, 24);

        backend.clear_region(ClearType::AfterCursor).unwrap();
        assert!(backend.take_buffer().ends_with(b"\x1b[J"));

        backend.clear_region(ClearType::CurrentLine).unwrap();
        assert!(backend.take_buffer().ends_with(b"\x1b[2K"));
    }
}
