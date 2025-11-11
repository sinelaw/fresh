// EditorTestHarness - Virtual terminal environment for E2E testing

use crossterm::event::{KeyCode, KeyModifiers, MouseButton, MouseEvent, MouseEventKind};
use fresh::fs::{BackendMetrics, FsBackend, LocalFsBackend, SlowFsBackend, SlowFsConfig};
use fresh::{config::Config, editor::Editor};
use ratatui::{backend::TestBackend, Terminal};
use std::io;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tempfile::TempDir;

/// Virtual editor environment for testing
/// Captures all rendering output without displaying to actual terminal
pub struct EditorTestHarness {
    /// The editor instance
    editor: Editor,

    /// Virtual terminal backend
    terminal: Terminal<TestBackend>,

    /// Optional temp directory (kept alive for the duration of the test)
    _temp_dir: Option<TempDir>,

    /// Optional metrics for slow filesystem backend
    fs_metrics: Option<Arc<tokio::sync::Mutex<BackendMetrics>>>,
}

impl EditorTestHarness {
    /// Create new test harness with virtual terminal
    /// Uses a temporary directory to avoid loading plugins from the project directory
    pub fn new(width: u16, height: u16) -> io::Result<Self> {
        let temp_dir = TempDir::new()?;
        let temp_path = temp_dir.path().to_path_buf();

        let backend = TestBackend::new(width, height);
        let terminal = Terminal::new(backend)?;
        let config = Config::default();
        // Use temp directory to avoid loading project plugins in tests
        let editor = Editor::with_working_dir(config, width, height, Some(temp_path))?;

        Ok(EditorTestHarness {
            editor,
            terminal,
            _temp_dir: Some(temp_dir),
            fs_metrics: None,
        })
    }

    /// Create with custom config
    /// Uses a temporary directory to avoid loading plugins from the project directory
    pub fn with_config(width: u16, height: u16, config: Config) -> io::Result<Self> {
        let temp_dir = TempDir::new()?;
        let temp_path = temp_dir.path().to_path_buf();

        let backend = TestBackend::new(width, height);
        let terminal = Terminal::new(backend)?;
        // Use temp directory to avoid loading project plugins in tests
        let editor = Editor::with_working_dir(config, width, height, Some(temp_path))?;

        Ok(EditorTestHarness {
            editor,
            terminal,
            _temp_dir: Some(temp_dir),
            fs_metrics: None,
        })
    }

    /// Create harness with an isolated temporary project directory
    /// The temp directory is kept alive for the duration of the harness
    /// and automatically cleaned up when the harness is dropped.
    /// This method does NOT modify the process's current directory, making tests
    /// fully hermetic and safe to run in parallel.
    ///
    /// Creates a subdirectory named "project_root" for deterministic paths in snapshots.
    pub fn with_temp_project(width: u16, height: u16) -> io::Result<Self> {
        let temp_dir = TempDir::new()?;

        // Create a subdirectory with a constant name for deterministic paths
        let project_root = temp_dir.path().join("project_root");
        std::fs::create_dir(&project_root)?;

        // Create editor with explicit working directory (no global state modification!)
        let backend = TestBackend::new(width, height);
        let terminal = Terminal::new(backend)?;
        let config = Config::default();
        let editor = Editor::with_working_dir(config, width, height, Some(project_root))?;

        Ok(EditorTestHarness {
            editor,
            terminal,
            _temp_dir: Some(temp_dir),
            fs_metrics: None,
        })
    }

    /// Create with custom config and explicit working directory
    /// The working directory is used for LSP initialization and file operations
    pub fn with_config_and_working_dir(
        width: u16,
        height: u16,
        config: Config,
        working_dir: std::path::PathBuf,
    ) -> io::Result<Self> {
        let backend = TestBackend::new(width, height);
        let terminal = Terminal::new(backend)?;
        let editor = Editor::with_working_dir(config, width, height, Some(working_dir))?;

        Ok(EditorTestHarness {
            editor,
            terminal,
            _temp_dir: None,
            fs_metrics: None,
        })
    }

    /// Create new test harness with line wrapping disabled
    /// Useful for tests that expect specific cursor positions without line wrapping
    pub fn new_no_wrap(width: u16, height: u16) -> io::Result<Self> {
        let mut config = Config::default();
        config.editor.line_wrap = false;
        Self::with_config(width, height, config)
    }

    /// Create a test harness with a slow filesystem backend for performance testing
    /// Returns the harness and provides access to filesystem metrics
    pub fn with_slow_fs(width: u16, height: u16, slow_config: SlowFsConfig) -> io::Result<Self> {
        let temp_dir = TempDir::new()?;
        let temp_path = temp_dir.path().to_path_buf();

        // Create slow filesystem backend wrapping the local backend
        let local_backend = Arc::new(LocalFsBackend::new());
        let slow_backend = SlowFsBackend::new(local_backend, slow_config);
        let metrics_arc = slow_backend.metrics_arc();
        let fs_backend: Arc<dyn FsBackend> = Arc::new(slow_backend);

        let backend = TestBackend::new(width, height);
        let terminal = Terminal::new(backend)?;
        let config = Config::default();

        // Create editor with custom filesystem backend
        let editor =
            Editor::with_fs_backend_for_test(config, width, height, Some(temp_path), fs_backend)?;

        Ok(EditorTestHarness {
            editor,
            terminal,
            _temp_dir: Some(temp_dir),
            fs_metrics: Some(metrics_arc),
        })
    }

    /// Get filesystem metrics (if using slow filesystem backend)
    pub fn fs_metrics(&self) -> Option<Arc<tokio::sync::Mutex<BackendMetrics>>> {
        self.fs_metrics.clone()
    }

    /// Get a snapshot of filesystem metrics
    pub async fn get_fs_metrics_snapshot(&self) -> Option<BackendMetrics> {
        if let Some(ref metrics) = self.fs_metrics {
            Some(metrics.lock().await.clone())
        } else {
            None
        }
    }

    /// Get the path to the temp project directory (if created with with_temp_project)
    /// Returns the "project_root" subdirectory path for deterministic naming
    pub fn project_dir(&self) -> Option<PathBuf> {
        self._temp_dir
            .as_ref()
            .map(|d| d.path().join("project_root"))
    }

    /// Open a file in the editor
    pub fn open_file(&mut self, path: &Path) -> io::Result<()> {
        self.editor.open_file(path)?;
        self.render()?;
        Ok(())
    }

    /// Load text content into the editor by creating a temporary file and opening it
    /// This is much faster than type_text() for large amounts of text in tests
    /// Returns a TestFixture that must be kept alive for the duration of the test
    pub fn load_buffer_from_text(
        &mut self,
        content: &str,
    ) -> io::Result<crate::common::fixtures::TestFixture> {
        let fixture = crate::common::fixtures::TestFixture::new("test_buffer.txt", content)?;
        self.open_file(&fixture.path)?;
        Ok(fixture)
    }

    /// Create a new empty buffer
    pub fn new_buffer(&mut self) -> io::Result<()> {
        self.editor.new_buffer();
        self.render()?;
        Ok(())
    }

    /// Simulate a key press
    pub fn send_key(&mut self, code: KeyCode, modifiers: KeyModifiers) -> io::Result<()> {
        // Delegate to the editor's handle_key method (just like main.rs does)
        self.editor.handle_key(code, modifiers)?;
        // Process any async messages that may have been generated by the key press
        // This ensures that actions like opening files complete before the next operation
        self.editor.process_async_messages();
        // Render to make state changes visible
        self.render()?;
        Ok(())
    }

    /// Send the same key press multiple times without rendering after each one
    /// This is optimized for tests that need to send many keys in a row (e.g., scrolling)
    /// Only renders once at the end, which is much faster than calling send_key() in a loop
    pub fn send_key_repeat(
        &mut self,
        code: KeyCode,
        modifiers: KeyModifiers,
        count: usize,
    ) -> io::Result<()> {
        for _ in 0..count {
            // Call handle_key directly without rendering (unlike send_key which renders every time)
            self.editor.handle_key(code, modifiers)?;
        }
        // Process any async messages that accumulated
        self.editor.process_async_messages();
        // Render once at the end instead of after every key press
        self.render()?;
        Ok(())
    }

    /// Simulate typing a string of text
    /// Optimized to avoid rendering after each character - only renders once at the end
    pub fn type_text(&mut self, text: &str) -> io::Result<()> {
        for ch in text.chars() {
            // Call handle_key directly without rendering (unlike send_key which renders every time)
            self.editor
                .handle_key(KeyCode::Char(ch), KeyModifiers::NONE)?;
        }
        // Process any async messages that accumulated during typing
        self.editor.process_async_messages();
        // Render once at the end instead of after every character
        self.render()?;
        Ok(())
    }

    /// Simulate a mouse event
    pub fn send_mouse(&mut self, mouse_event: MouseEvent) -> io::Result<()> {
        // Delegate to the editor's handle_mouse method (just like main.rs does)
        self.editor.handle_mouse(mouse_event)?;
        Ok(())
    }

    /// Simulate a mouse click at specific coordinates
    pub fn mouse_click(&mut self, col: u16, row: u16) -> io::Result<()> {
        let mouse_event = MouseEvent {
            kind: MouseEventKind::Down(MouseButton::Left),
            column: col,
            row,
            modifiers: KeyModifiers::empty(),
        };
        self.send_mouse(mouse_event)?;

        // Also send the release event
        let mouse_up = MouseEvent {
            kind: MouseEventKind::Up(MouseButton::Left),
            column: col,
            row,
            modifiers: KeyModifiers::empty(),
        };
        self.send_mouse(mouse_up)?;
        self.render()?;
        Ok(())
    }

    /// Simulate a mouse drag from one position to another
    pub fn mouse_drag(
        &mut self,
        start_col: u16,
        start_row: u16,
        end_col: u16,
        end_row: u16,
    ) -> io::Result<()> {
        // Send initial press
        let mouse_down = MouseEvent {
            kind: MouseEventKind::Down(MouseButton::Left),
            column: start_col,
            row: start_row,
            modifiers: KeyModifiers::empty(),
        };
        self.send_mouse(mouse_down)?;

        // Interpolate intermediate positions for smooth dragging
        let steps = ((end_row as i32 - start_row as i32).abs())
            .max((end_col as i32 - start_col as i32).abs())
            .max(1);
        for i in 1..=steps {
            let t = i as f32 / steps as f32;
            let col = start_col as f32 + (end_col as f32 - start_col as f32) * t;
            let row = start_row as f32 + (end_row as f32 - start_row as f32) * t;

            let mouse_drag_event = MouseEvent {
                kind: MouseEventKind::Drag(MouseButton::Left),
                column: col as u16,
                row: row as u16,
                modifiers: KeyModifiers::empty(),
            };
            self.send_mouse(mouse_drag_event)?;
        }

        // Send final release
        let mouse_up = MouseEvent {
            kind: MouseEventKind::Up(MouseButton::Left),
            column: end_col,
            row: end_row,
            modifiers: KeyModifiers::empty(),
        };
        self.send_mouse(mouse_up)?;
        self.render()?;
        Ok(())
    }

    /// Apply an event directly to the active buffer
    pub fn apply_event(&mut self, event: fresh::event::Event) -> io::Result<()> {
        self.editor.apply_event_to_active_buffer(&event);
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
        buffer
            .content
            .get(pos)
            .map(|cell| cell.symbol().to_string())
    }

    /// Get the style (color, modifiers) of a specific cell
    pub fn get_cell_style(&self, x: u16, y: u16) -> Option<ratatui::style::Style> {
        let buffer = self.buffer();
        let pos = buffer.index_of(x, y);
        buffer.content.get(pos).map(|cell| cell.style())
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
            "Expected screen to contain '{text}'\nScreen content:\n{screen}"
        );
    }

    /// Verify text does not appear on screen
    pub fn assert_screen_not_contains(&self, text: &str) {
        let screen = self.screen_to_string();
        assert!(
            !screen.contains(text),
            "Expected screen to not contain '{text}'\nScreen content:\n{screen}"
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
            "Buffer content mismatch\nExpected: {expected:?}\nActual: {actual:?}"
        );
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

    /// Get the primary cursor position
    pub fn cursor_position(&self) -> usize {
        self.editor.active_state().cursors.primary().position
    }

    /// Get the buffer length in bytes
    pub fn buffer_len(&self) -> usize {
        self.editor.active_state().buffer.len()
    }

    /// Get the number of cursors
    pub fn cursor_count(&self) -> usize {
        self.editor.active_state().cursors.count()
    }

    /// Count the number of search highlight overlays in the current buffer
    pub fn count_search_highlights(&self) -> usize {
        self.editor
            .active_state()
            .overlays
            .all()
            .iter()
            .filter(|o| {
                o.id.as_ref()
                    .map(|id| id.starts_with("search_highlight_"))
                    .unwrap_or(false)
            })
            .count()
    }

    /// Get the screen cursor position (x, y) from the terminal
    pub fn screen_cursor_position(&mut self) -> (u16, u16) {
        let pos = self.terminal.get_cursor_position().unwrap_or_default();
        (pos.x, pos.y)
    }

    /// Find all visible cursors on screen
    /// Returns a vec of (x, y, character_at_cursor, is_primary)
    /// Primary cursor is detected at hardware cursor position
    /// Secondary cursors are detected by REVERSED style modifier
    pub fn find_all_cursors(&mut self) -> Vec<(u16, u16, String, bool)> {
        use ratatui::style::Modifier;
        let mut cursors = Vec::new();

        // Get hardware cursor position (primary cursor)
        let (hw_x, hw_y) = self.screen_cursor_position();

        // Get the buffer to read cell content
        let buffer = self.terminal.backend().buffer();

        // Add primary cursor at hardware position
        if let Some(cell) = buffer.content.get(buffer.index_of(hw_x, hw_y)) {
            cursors.push((hw_x, hw_y, cell.symbol().to_string(), true));
        }

        // Find secondary cursors (cells with REVERSED modifier)
        for y in 0..buffer.area.height {
            for x in 0..buffer.area.width {
                // Skip if this is the hardware cursor position
                if x == hw_x && y == hw_y {
                    continue;
                }

                let pos = buffer.index_of(x, y);
                if let Some(cell) = buffer.content.get(pos) {
                    if cell.modifier.contains(Modifier::REVERSED) {
                        cursors.push((x, y, cell.symbol().to_string(), false));
                    }
                }
            }
        }

        cursors
    }

    /// Get the top line number currently visible in the viewport
    pub fn top_line_number(&mut self) -> usize {
        let top_byte = self.editor.active_state().viewport.top_byte;
        self.editor
            .active_state_mut()
            .buffer
            .get_line_number(top_byte)
    }

    /// Get the primary cursor's selection range, if any
    pub fn get_selection_range(&self) -> Option<std::ops::Range<usize>> {
        self.editor
            .active_state()
            .cursors
            .primary()
            .selection_range()
    }

    /// Check if there's an active selection
    pub fn has_selection(&self) -> bool {
        !self.editor.active_state().cursors.primary().collapsed()
    }

    /// Get the selected text (if any)
    pub fn get_selected_text(&self) -> String {
        if let Some(range) = self.get_selection_range() {
            self.editor.active_state().buffer.slice(range).to_string()
        } else {
            String::new()
        }
    }

    /// Assert that no selection exists
    pub fn assert_no_selection(&self) {
        assert!(!self.has_selection(), "Expected no selection but found one");
    }

    /// Resize the terminal to new dimensions
    /// This simulates terminal resize events and updates both the virtual terminal
    /// backend and the editor's viewport
    pub fn resize(&mut self, width: u16, height: u16) -> io::Result<()> {
        // Resize the virtual terminal backend
        self.terminal.backend_mut().resize(width, height);
        // Resize the editor's viewports
        self.editor.resize(width, height);
        // Re-render to reflect the new size
        self.render()?;
        Ok(())
    }

    /// Process pending async messages and render
    /// Useful for testing async features like git grep, file explorer, etc.
    pub fn process_async_and_render(&mut self) -> io::Result<()> {
        self.editor.process_async_messages();
        self.render()?;
        Ok(())
    }

    /// Wait for async operations with timeout
    /// Repeatedly processes async messages until condition is met or timeout
    pub fn wait_for_async<F>(&mut self, mut condition: F, timeout_ms: u64) -> io::Result<bool>
    where
        F: FnMut(&Self) -> bool,
    {
        let start = std::time::Instant::now();
        let timeout = std::time::Duration::from_millis(timeout_ms);

        while start.elapsed() < timeout {
            self.process_async_and_render()?;
            if condition(self) {
                return Ok(true);
            }
            std::thread::sleep(std::time::Duration::from_millis(10));
        }

        Ok(false)
    }

    /// Capture a visual step for regression testing
    /// This takes both a text snapshot (for testing) and generates an SVG (for visualization)
    pub fn capture_visual_step(
        &mut self,
        flow: &mut crate::common::visual_testing::VisualFlow,
        step_name: &str,
        description: &str,
    ) -> io::Result<()> {
        self.render()?;
        let cursor_pos = self.screen_cursor_position();
        flow.step(self.buffer(), cursor_pos, step_name, description)?;
        Ok(())
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
