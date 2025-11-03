// EditorTestHarness - Virtual terminal environment for E2E testing

use crossterm::event::{KeyCode, KeyModifiers};
use editor::{config::Config, editor::Editor};
use ratatui::{backend::TestBackend, Terminal};
use std::io;
use std::path::Path;

/// Virtual editor environment for testing
/// Captures all rendering output without displaying to actual terminal
pub struct EditorTestHarness {
    /// The editor instance
    editor: Editor,

    /// Virtual terminal backend
    terminal: Terminal<TestBackend>,
}

impl EditorTestHarness {
    /// Create new test harness with virtual terminal
    pub fn new(width: u16, height: u16) -> io::Result<Self> {
        let backend = TestBackend::new(width, height);
        let terminal = Terminal::new(backend)?;
        let config = Config::default();
        let editor = Editor::new(config)?;

        Ok(EditorTestHarness { editor, terminal })
    }

    /// Create with custom config
    pub fn with_config(width: u16, height: u16, config: Config) -> io::Result<Self> {
        let backend = TestBackend::new(width, height);
        let terminal = Terminal::new(backend)?;
        let editor = Editor::new(config)?;

        Ok(EditorTestHarness { editor, terminal })
    }

    /// Open a file in the editor
    pub fn open_file(&mut self, path: &Path) -> io::Result<()> {
        self.editor.open_file(path)?;
        self.render()?;
        Ok(())
    }

    /// Create a new empty buffer
    pub fn new_buffer(&mut self) -> io::Result<()> {
        self.editor.new_buffer();
        self.render()?;
        Ok(())
    }

    /// Simulate a key press
    pub fn send_key(&mut self, _code: KeyCode, _modifiers: KeyModifiers) -> io::Result<()> {
        // TODO: Convert key to action and apply
        // For now this is a placeholder
        self.render()?;
        Ok(())
    }

    /// Simulate typing a string of text
    pub fn type_text(&mut self, text: &str) -> io::Result<()> {
        for ch in text.chars() {
            self.send_key(KeyCode::Char(ch), KeyModifiers::NONE)?;
        }
        Ok(())
    }

    /// Force a render cycle and capture output
    pub fn render(&mut self) -> io::Result<()> {
        self.terminal.draw(|frame| {
            self.editor.render(frame);
        })?;
        Ok(())
    }

    /// Get the current terminal buffer (what would be displayed)
    pub fn buffer(&self) -> &ratatui::buffer::Buffer {
        self.terminal.backend().buffer()
    }

    /// Get text at specific cell position
    pub fn get_cell(&self, x: u16, y: u16) -> Option<String> {
        let buffer = self.buffer();
        let pos = buffer.index_of(x, y);
        buffer.content.get(pos).map(|cell| cell.symbol().to_string())
    }

    /// Get entire screen as string (for debugging)
    pub fn screen_to_string(&self) -> String {
        let buffer = self.buffer();
        let (width, height) = (buffer.area.width, buffer.area.height);
        let mut result = String::new();

        for y in 0..height {
            for x in 0..width {
                let pos = buffer.index_of(x, y);
                if let Some(cell) = buffer.content.get(pos) {
                    result.push_str(cell.symbol());
                }
            }
            if y < height - 1 {
                result.push('\n');
            }
        }

        result
    }

    /// Verify text appears on screen
    pub fn assert_screen_contains(&self, text: &str) {
        let screen = self.screen_to_string();
        assert!(
            screen.contains(text),
            "Expected screen to contain '{}'\nScreen content:\n{}",
            text,
            screen
        );
    }

    /// Verify text does not appear on screen
    pub fn assert_screen_not_contains(&self, text: &str) {
        let screen = self.screen_to_string();
        assert!(
            !screen.contains(text),
            "Expected screen to not contain '{}'\nScreen content:\n{}",
            text,
            screen
        );
    }

    /// Get the buffer content (not screen, actual buffer text)
    pub fn get_buffer_content(&self) -> String {
        self.editor.active_state().buffer.to_string()
    }

    /// Verify buffer content matches expected
    pub fn assert_buffer_content(&self, expected: &str) {
        let actual = self.get_buffer_content();
        assert_eq!(
            actual, expected,
            "Buffer content mismatch\nExpected: {:?}\nActual: {:?}",
            expected, actual
        );
    }

    /// Save the active buffer
    pub fn save(&mut self) -> io::Result<()> {
        self.editor.save()?;
        self.render()?;
        Ok(())
    }

    /// Access the editor directly (for advanced testing)
    pub fn editor(&self) -> &Editor {
        &self.editor
    }

    /// Access the editor mutably (for advanced testing)
    pub fn editor_mut(&mut self) -> &mut Editor {
        &mut self.editor
    }

    /// Check if editor wants to quit
    pub fn should_quit(&self) -> bool {
        self.editor.should_quit()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_harness_creation() {
        let harness = EditorTestHarness::new(80, 24).unwrap();
        assert!(!harness.should_quit());
    }

    #[test]
    fn test_harness_render() {
        let mut harness = EditorTestHarness::new(80, 24).unwrap();
        harness.render().unwrap();

        let screen = harness.screen_to_string();
        assert!(!screen.is_empty());
    }

    #[test]
    fn test_buffer_content() {
        let harness = EditorTestHarness::new(80, 24).unwrap();
        let content = harness.get_buffer_content();
        assert_eq!(content, ""); // New buffer is empty
    }
}
