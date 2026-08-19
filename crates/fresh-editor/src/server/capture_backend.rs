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
        self.emit_style_reset();
    }

    /// Return the client terminal to default colors/attributes and forget the
    /// tracked style.
    ///
    /// The tracked fields describe what the *client* terminal is currently
    /// showing, so they may never be cleared without telling the client: doing
    /// so leaves the two out of sync, and the diffing in `write_style` then
    /// skips the SGR that would have repainted a cell.
    ///
    /// This matters most around erase operations. `ED`/`EL` fill the erased
    /// cells with the currently active background (background-color-erase), so
    /// an erase issued while, say, the status bar's background is still active
    /// paints the whole screen in that color (issue #2723).
    fn emit_style_reset(&mut self) {
        self.buffer
            .extend_from_slice(crate::services::terminal_modes::sequences::RESET_ATTRIBUTES);
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
        let mut wrote_any = false;

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
            wrote_any = true;
        }

        // Leave the client terminal at default colors/attributes between
        // frames, the way ratatui's own `CrosstermBackend` does. Otherwise the
        // last cell's style stays active on the client, and whatever the next
        // frame starts with — an erase in particular — inherits it.
        //
        // Skipped when the frame changed nothing: ratatui only hands us the
        // cells that differ, and an unchanged frame must stay byte-empty so
        // the server can skip the broadcast entirely.
        if wrote_any {
            self.emit_style_reset();
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
        // The erase inherits the active background, so drop back to the
        // default one first.
        self.emit_style_reset();
        // Clear entire screen
        self.buffer.extend_from_slice(b"\x1b[2J");
        // Move cursor to home
        self.buffer.extend_from_slice(b"\x1b[H");
        self.cursor = Position::new(0, 0);
        Ok(())
    }

    fn clear_region(&mut self, clear_type: ClearType) -> io::Result<()> {
        // Every erase below fills with the active background, so drop back to
        // the default one first. `Terminal::clear()` — and therefore the
        // autoresize path — reaches the erase through here, not `clear()`.
        self.emit_style_reset();
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
///
/// Uses shared constants from `terminal_modes::sequences` to stay in sync
/// with the direct-mode terminal setup in `TerminalModes::enable()`.
///
/// When `mouse_hover_enabled` is false on Windows, mode 1003 (all motion) is
/// omitted and only mode 1002 (cell motion) is enabled, reducing event volume
/// and avoiding input corruption. On non-Windows platforms the parameter is
/// ignored and full mouse tracking is always enabled.
pub fn terminal_setup_sequences(mouse_hover_enabled: bool) -> Vec<u8> {
    use crate::services::terminal_modes::sequences as seq;

    let mut buf = Vec::new();

    // Enter alternate screen
    buf.extend_from_slice(seq::ENTER_ALTERNATE_SCREEN);
    // Enable mouse tracking (SGR format)
    buf.extend_from_slice(seq::ENABLE_MOUSE_CLICK);
    buf.extend_from_slice(seq::ENABLE_MOUSE_DRAG);
    // On Windows, only enable all-motion tracking (mode 1003) when hover is
    // enabled. Mode 1003 generates extreme event volume that can cause input
    // corruption on Windows. On other platforms, always enable it.
    if cfg!(windows) {
        if mouse_hover_enabled {
            buf.extend_from_slice(seq::ENABLE_MOUSE_MOTION);
        }
    } else {
        buf.extend_from_slice(seq::ENABLE_MOUSE_MOTION);
    }
    buf.extend_from_slice(seq::ENABLE_SGR_MOUSE);
    // Enable focus events
    buf.extend_from_slice(seq::ENABLE_FOCUS_EVENTS);
    // Enable bracketed paste
    buf.extend_from_slice(seq::ENABLE_BRACKETED_PASTE);
    // Hide cursor initially
    buf.extend_from_slice(seq::HIDE_CURSOR);

    buf
}

/// Generate terminal teardown sequences
///
/// Uses shared constants from `terminal_modes::sequences` to stay in sync
/// with the cleanup in `TerminalModes::undo()` and `emergency_cleanup()`.
pub fn terminal_teardown_sequences() -> Vec<u8> {
    use crate::services::terminal_modes::sequences as seq;

    let mut buf = Vec::new();

    // Show cursor
    buf.extend_from_slice(seq::SHOW_CURSOR);
    // Reset cursor style to default
    buf.extend_from_slice(seq::RESET_CURSOR_STYLE);
    // Disable bracketed paste
    buf.extend_from_slice(seq::DISABLE_BRACKETED_PASTE);
    // Disable focus events
    buf.extend_from_slice(seq::DISABLE_FOCUS_EVENTS);
    // Disable mouse tracking
    buf.extend_from_slice(seq::DISABLE_SGR_MOUSE);
    buf.extend_from_slice(seq::DISABLE_MOUSE_MOTION);
    buf.extend_from_slice(seq::DISABLE_MOUSE_DRAG);
    buf.extend_from_slice(seq::DISABLE_MOUSE_CLICK);
    // Reset attributes
    buf.extend_from_slice(seq::RESET_ATTRIBUTES);
    // Leave alternate screen
    buf.extend_from_slice(seq::LEAVE_ALTERNATE_SCREEN);

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
        // SGR reset first, so the erase does not fill with a stale background
        // (issue #2723), then ED (Erase Display): CSI 2 J
        assert!(output.starts_with(b"\x1b[0m\x1b[2J"));
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
        let setup = terminal_setup_sequences(true);
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

    /// Issue #2723: a resize inside a daemon session left blank cells filled
    /// with the status bar's background color.
    ///
    /// This drives the real path: `Terminal::draw` -> `autoresize` ->
    /// `Terminal::clear` -> `clear_region(All)`, with a first frame whose last
    /// painted cell carries a magenta background (the `terminal` theme's
    /// "Palette: Ctrl+P" chip). The captured stream is then replayed into a
    /// terminal emulator, so the assertion is on what a client would actually
    /// display rather than on the bytes.
    #[test]
    fn resize_does_not_leave_blank_cells_filled_with_the_previous_frames_color() {
        use ratatui::Terminal;

        let backend = CaptureBackend::new(20, 4);
        let mut terminal = Terminal::new(backend).unwrap();

        // Frame 1: text on the first row, and a magenta-background chip as the
        // very last cell painted — the state the erase would otherwise inherit.
        terminal
            .draw(|frame| {
                let magenta = Style::default().bg(ratatui::style::Color::Magenta);
                frame
                    .buffer_mut()
                    .set_string(0, 0, "File", Style::default());
                frame.buffer_mut().set_string(16, 3, "Chip", magenta);
            })
            .unwrap();

        // Frame 2, at a new size: this is what a terminal resize triggers.
        terminal.backend_mut().resize(18, 4);
        terminal
            .draw(|frame| {
                frame
                    .buffer_mut()
                    .set_string(0, 0, "File", Style::default());
            })
            .unwrap();

        let stream = terminal.backend_mut().take_buffer();

        let mut parser = vt100::Parser::new(4, 18, 0);
        parser.process(&stream);
        let screen = parser.screen();

        // The gap between the glyphs of the first row is the cell the
        // incremental redraw skips; before the fix it kept the erase fill.
        for (row, col) in [(0, 5), (0, 17), (1, 0), (2, 9), (3, 17)] {
            let cell = screen.cell(row, col).unwrap();
            assert_eq!(
                cell.bgcolor(),
                vt100::Color::Default,
                "blank cell at row {row}, col {col} kept a background from the previous frame"
            );
        }
    }

    #[test]
    fn test_clear_region_variants() {
        let mut backend = CaptureBackend::new(80, 24);

        backend.clear_region(ClearType::AfterCursor).unwrap();
        assert!(backend.take_buffer().ends_with(b"\x1b[J"));

        backend.clear_region(ClearType::CurrentLine).unwrap();
        assert!(backend.take_buffer().ends_with(b"\x1b[2K"));
    }

    /// Erase sequences fill with the active background, so the reset has to be
    /// emitted *before* them, not merely tracked internally.
    ///
    /// `Terminal::clear()` erases via `clear_region(All)`, so that is the
    /// variant that has to hold the ordering; `clear()` is pinned alongside it.
    #[test]
    fn style_reset_is_emitted_before_every_hard_clear() {
        let mut backend = CaptureBackend::new(80, 24);
        backend.current_bg = ratatui::style::Color::Magenta;
        backend.clear_region(ClearType::All).unwrap();
        assert_eq!(backend.take_buffer(), b"\x1b[0m\x1b[2J");

        backend.current_bg = ratatui::style::Color::Magenta;
        backend.clear().unwrap();
        assert_eq!(backend.take_buffer(), b"\x1b[0m\x1b[2J\x1b[H");
    }

    /// A frame that changed nothing must stay byte-empty: `render_and_broadcast`
    /// skips the broadcast on an empty buffer, so an unconditional trailing
    /// reset would send traffic to every client on every idle render.
    #[test]
    fn unchanged_frame_emits_no_bytes() {
        let mut backend = CaptureBackend::new(80, 24);
        backend.draw(std::iter::empty()).unwrap();
        assert!(backend.take_buffer().is_empty());
    }
}
