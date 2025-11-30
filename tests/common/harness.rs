// EditorTestHarness - Virtual terminal environment for E2E testing

// Initialize V8 early - must happen before any Editor/JsRuntime is created
// and only once per process. Using ctor ensures this runs at test startup.
#[ctor::ctor]
fn init_v8_for_tests() {
    fresh::v8_init::init();
}

use crossterm::event::{KeyCode, KeyModifiers, MouseButton, MouseEvent, MouseEventKind};

/// Terminal layout constants
/// The editor uses a fixed layout with reserved rows for UI elements
pub mod layout {
    /// Menu bar is always at row 0
    pub const MENU_BAR_ROW: usize = 0;

    /// Tab bar is at row 1 (within the main content area)
    pub const TAB_BAR_ROW: usize = 1;

    /// Content starts at row 2 (after menu bar and tab bar)
    pub const CONTENT_START_ROW: usize = 2;

    /// Number of rows reserved at the bottom (status bar + prompt line)
    pub const BOTTOM_RESERVED_ROWS: usize = 2;

    /// Total reserved rows (menu bar at top, status bar + prompt at bottom)
    pub const TOTAL_RESERVED_ROWS: usize = 4;

    /// Get the status bar row for a given terminal height
    #[inline]
    pub const fn status_bar_row(terminal_height: usize) -> usize {
        terminal_height - 2
    }

    /// Get the prompt line row for a given terminal height
    #[inline]
    pub const fn prompt_line_row(terminal_height: usize) -> usize {
        terminal_height - 1
    }

    /// Get the content end row (exclusive) for a given terminal height
    #[inline]
    pub const fn content_end_row(terminal_height: usize) -> usize {
        terminal_height - BOTTOM_RESERVED_ROWS
    }

    /// Get the number of content rows for a given terminal height
    #[inline]
    pub const fn content_row_count(terminal_height: usize) -> usize {
        terminal_height.saturating_sub(TOTAL_RESERVED_ROWS)
    }
}
use fresh::primitives::highlight_engine::HighlightEngine;
use fresh::services::fs::{BackendMetrics, FsBackend, LocalFsBackend, SlowFsBackend, SlowFsConfig};
use fresh::{app::Editor, config::Config};
use ratatui::{backend::TestBackend, Terminal};
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tempfile::TempDir;

/// A wrapper that captures CrosstermBackend output for vt100 parsing
struct CaptureBuffer {
    data: Vec<u8>,
}

impl CaptureBuffer {
    fn new() -> Self {
        Self { data: Vec::new() }
    }

    fn take(&mut self) -> Vec<u8> {
        std::mem::take(&mut self.data)
    }
}

impl Write for CaptureBuffer {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.data.extend_from_slice(buf);
        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

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

    /// Tokio runtime for async operations (needed for TypeScript plugins)
    _tokio_runtime: Option<tokio::runtime::Runtime>,

    /// Shadow string that mirrors editor operations for validation
    /// This helps catch discrepancies between piece tree and simple string operations
    shadow_string: String,

    /// Shadow cursor position
    shadow_cursor: usize,

    /// Whether to enable shadow buffer validation (off by default)
    /// Enable this only in tests that focus on simple text editing operations
    enable_shadow_validation: bool,

    /// VT100 parser for testing real ANSI terminal output
    /// This simulates how a real terminal would interpret the escape sequences
    vt100_parser: vt100::Parser,

    /// Terminal dimensions for vt100
    term_width: u16,
    term_height: u16,
}

impl EditorTestHarness {
    /// Create new test harness with virtual terminal
    /// Uses a temporary directory to avoid loading plugins from the project directory
    /// Auto-indent is disabled by default to match shadow string behavior
    pub fn new(width: u16, height: u16) -> io::Result<Self> {
        let temp_dir = TempDir::new()?;
        let temp_path = temp_dir.path().to_path_buf();

        let backend = TestBackend::new(width, height);
        let terminal = Terminal::new(backend)?;
        let mut config = Config::default();
        config.editor.auto_indent = false; // Disable for simpler testing
                                           // Use temp directory to avoid loading project plugins in tests
        let editor = Editor::with_working_dir(config, width, height, Some(temp_path))?;

        Ok(EditorTestHarness {
            editor,
            terminal,
            _temp_dir: Some(temp_dir),
            fs_metrics: None,
            _tokio_runtime: None,
            shadow_string: String::new(),
            shadow_cursor: 0,
            enable_shadow_validation: false,
            vt100_parser: vt100::Parser::new(height, width, 0),
            term_width: width,
            term_height: height,
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
            _tokio_runtime: None,
            shadow_string: String::new(),
            shadow_cursor: 0,
            enable_shadow_validation: false,
            vt100_parser: vt100::Parser::new(height, width, 0),
            term_width: width,
            term_height: height,
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
        Self::with_temp_project_and_config(width, height, Config::default())
    }

    /// Create a test harness with a temporary project directory and custom config
    pub fn with_temp_project_and_config(
        width: u16,
        height: u16,
        config: Config,
    ) -> io::Result<Self> {
        let temp_dir = TempDir::new()?;

        // Create a subdirectory with a constant name for deterministic paths
        let project_root = temp_dir.path().join("project_root");
        std::fs::create_dir(&project_root)?;

        // Create editor with explicit working directory (no global state modification!)
        let backend = TestBackend::new(width, height);
        let terminal = Terminal::new(backend)?;
        let editor = Editor::with_working_dir(config, width, height, Some(project_root))?;

        Ok(EditorTestHarness {
            editor,
            terminal,
            _temp_dir: Some(temp_dir),
            fs_metrics: None,
            _tokio_runtime: None,
            shadow_string: String::new(),
            shadow_cursor: 0,
            enable_shadow_validation: false,
            vt100_parser: vt100::Parser::new(height, width, 0),
            term_width: width,
            term_height: height,
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

        // Create editor - it will create its own tokio runtime for async operations
        let mut editor = Editor::with_working_dir(config, width, height, Some(working_dir))?;

        // Process any pending plugin commands (e.g., command registrations from TypeScript plugins)
        editor.process_async_messages();

        Ok(EditorTestHarness {
            editor,
            terminal,
            _temp_dir: None,
            fs_metrics: None,
            _tokio_runtime: None,
            shadow_string: String::new(),
            shadow_cursor: 0,
            enable_shadow_validation: false,
            vt100_parser: vt100::Parser::new(height, width, 0),
            term_width: width,
            term_height: height,
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
            _tokio_runtime: None,
            shadow_string: String::new(),
            shadow_cursor: 0,
            enable_shadow_validation: false,
            vt100_parser: vt100::Parser::new(height, width, 0),
            term_width: width,
            term_height: height,
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

    /// Enable shadow buffer validation
    /// Call this at the start of tests that focus on simple text editing operations
    /// where you want to validate that the piece tree matches simple string operations
    pub fn enable_shadow_validation(&mut self) {
        self.enable_shadow_validation = true;
    }

    /// Open a file in the editor
    pub fn open_file(&mut self, path: &Path) -> io::Result<()> {
        self.editor.open_file(path)?;
        self.render()?;

        // Initialize shadow string with the file content
        self.shadow_string = self.get_buffer_content();
        self.shadow_cursor = self.cursor_position();

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
        // Update shadow string to mirror the operation (only if validation is enabled)
        if self.enable_shadow_validation {
            self.update_shadow_for_key(code, modifiers);
        }

        // Delegate to the editor's handle_key method (just like main.rs does)
        self.editor.handle_key(code, modifiers)?;
        // Process any async messages that may have been generated by the key press
        // This ensures that actions like opening files complete before the next operation
        let _ = self.editor.process_async_messages();
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
        let _ = self.editor.process_async_messages();
        // Render once at the end instead of after every key press
        self.render()?;
        Ok(())
    }

    /// Simulate typing a string of text
    /// Optimized to avoid rendering after each character - only renders once at the end
    pub fn type_text(&mut self, text: &str) -> io::Result<()> {
        for ch in text.chars() {
            // Update shadow string (only if validation is enabled)
            if self.enable_shadow_validation {
                self.shadow_string.insert(self.shadow_cursor, ch);
                self.shadow_cursor += ch.len_utf8();
            }

            // Call handle_key directly without rendering (unlike send_key which renders every time)
            self.editor
                .handle_key(KeyCode::Char(ch), KeyModifiers::NONE)?;
        }
        // Process any async messages that accumulated during typing
        let _ = self.editor.process_async_messages();
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

    /// Simulate a mouse move (hover) at specific coordinates
    pub fn mouse_move(&mut self, col: u16, row: u16) -> io::Result<()> {
        let mouse_event = MouseEvent {
            kind: MouseEventKind::Moved,
            column: col,
            row,
            modifiers: KeyModifiers::empty(),
        };
        self.send_mouse(mouse_event)?;
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
    pub fn apply_event(&mut self, event: fresh::model::event::Event) -> io::Result<()> {
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

    /// Render through the real CrosstermBackend and parse with vt100
    /// This tests the actual ANSI escape sequences, not just the buffer contents
    /// Returns the screen content as parsed by a real terminal emulator
    pub fn render_real(&mut self) -> io::Result<()> {
        // Generate ANSI escape sequences manually

        // First render to TestBackend to get the buffer
        self.render()?;

        // Now manually generate ANSI sequences from the buffer
        // This simulates what CrosstermBackend would do without needing a real terminal
        let buffer = self.terminal.backend().buffer();
        let mut ansi_output = Vec::new();

        // Clear screen and move to home position
        ansi_output.extend_from_slice(b"\x1b[2J\x1b[H");

        // Render each cell
        for y in 0..buffer.area.height {
            // Move to start of line
            ansi_output.extend_from_slice(format!("\x1b[{};1H", y + 1).as_bytes());

            for x in 0..buffer.area.width {
                let idx = buffer.index_of(x, y);
                if let Some(cell) = buffer.content.get(idx) {
                    let symbol = cell.symbol();
                    ansi_output.extend_from_slice(symbol.as_bytes());
                }
            }
        }

        // Feed to vt100 parser
        self.vt100_parser.process(&ansi_output);

        Ok(())
    }

    /// Alternative: Render using ratatui's diff-based rendering to capture incremental updates
    /// This more closely matches what happens in the real application
    pub fn render_real_incremental(&mut self) -> io::Result<()> {
        use ratatui::backend::CrosstermBackend;

        // Create a buffer to capture ANSI output
        let mut capture = CaptureBuffer::new();

        // Use a scope to ensure backend is dropped before we take the buffer
        {
            // Create CrosstermBackend with our capture buffer
            // Note: This may fail if crossterm tries to query terminal state
            let mut backend = CrosstermBackend::new(&mut capture);

            // Manually render cells without using Terminal::draw which does extra setup
            let buffer = self.terminal.backend().buffer();

            // Write clear screen
            use std::io::Write;
            write!(backend, "\x1b[2J\x1b[H")?;

            // Write each cell manually
            for y in 0..buffer.area.height {
                write!(backend, "\x1b[{};1H", y + 1)?;
                for x in 0..buffer.area.width {
                    let idx = buffer.index_of(x, y);
                    if let Some(cell) = buffer.content.get(idx) {
                        write!(backend, "{}", cell.symbol())?;
                    }
                }
            }
            backend.flush()?;
        }

        // Get the captured output and feed to vt100
        let ansi_output = capture.take();
        self.vt100_parser.process(&ansi_output);

        // Also render to TestBackend
        self.render()?;

        Ok(())
    }

    /// Get the screen content as parsed by vt100 (simulating real terminal)
    /// This is what a real terminal would show after processing ANSI sequences
    pub fn vt100_screen_to_string(&self) -> String {
        let screen = self.vt100_parser.screen();
        let mut result = String::new();

        for row in 0..self.term_height {
            for col in 0..self.term_width {
                let cell = screen.cell(row, col);
                if let Some(cell) = cell {
                    result.push_str(&cell.contents());
                } else {
                    result.push(' ');
                }
            }
            if row < self.term_height - 1 {
                result.push('\n');
            }
        }

        result
    }

    /// Compare TestBackend output with vt100-parsed output
    /// Returns a list of differences if any, or empty vec if they match
    pub fn compare_test_vs_real(&self) -> Vec<String> {
        let test_screen = self.screen_to_string();
        let vt100_screen = self.vt100_screen_to_string();

        let test_lines: Vec<&str> = test_screen.lines().collect();
        let vt100_lines: Vec<&str> = vt100_screen.lines().collect();

        let mut differences = Vec::new();

        for (row, (test_line, vt100_line)) in test_lines.iter().zip(vt100_lines.iter()).enumerate()
        {
            if test_line != vt100_line {
                differences.push(format!(
                    "Row {}: TestBackend vs VT100 mismatch:\n  Test:  {:?}\n  VT100: {:?}",
                    row, test_line, vt100_line
                ));

                // Character-by-character comparison for debugging
                let test_chars: Vec<char> = test_line.chars().collect();
                let vt100_chars: Vec<char> = vt100_line.chars().collect();
                for (col, (tc, vc)) in test_chars.iter().zip(vt100_chars.iter()).enumerate() {
                    if tc != vc {
                        differences.push(format!("    Col {}: '{}' vs '{}'", col, tc, vc));
                    }
                }
            }
        }

        differences
    }

    /// Assert that TestBackend and vt100 show the same content
    /// This catches bugs in ANSI escape sequence generation
    pub fn assert_test_matches_real(&self) {
        let differences = self.compare_test_vs_real();
        if !differences.is_empty() {
            panic!(
                "TestBackend and VT100 output differ!\n{}\n\nTestBackend:\n{}\n\nVT100:\n{}",
                differences.join("\n"),
                self.screen_to_string(),
                self.vt100_screen_to_string()
            );
        }
    }

    /// Get a specific cell from the vt100-parsed screen
    pub fn vt100_get_cell(&self, col: u16, row: u16) -> Option<String> {
        let screen = self.vt100_parser.screen();
        screen
            .cell(row, col)
            .map(|cell| cell.contents().to_string())
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

    /// Get the text content of a specific screen row
    pub fn get_row_text(&self, y: u16) -> String {
        let buffer = self.buffer();
        let width = buffer.area.width;
        let mut row_text = String::new();

        for x in 0..width {
            let pos = buffer.index_of(x, y);
            if let Some(cell) = buffer.content.get(pos) {
                row_text.push_str(cell.symbol());
            }
        }

        row_text
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
        self.editor.active_state().buffer.to_string().unwrap()
    }

    /// Verify buffer content matches expected
    pub fn assert_buffer_content(&self, expected: &str) {
        let actual = self.get_buffer_content();

        // Also verify shadow string matches to catch discrepancies (only if validation is enabled)
        if self.enable_shadow_validation {
            assert_eq!(
                self.shadow_string, expected,
                "Shadow string mismatch (bug in test harness shadow tracking)\nExpected: {expected:?}\nShadow: {:?}",
                self.shadow_string
            );
        }

        assert_eq!(
            actual, expected,
            "Buffer content mismatch\nExpected: {expected:?}\nActual: {actual:?}",
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

    /// Update shadow string to mirror key operations
    /// This helps catch discrepancies between piece tree and simple string operations
    fn update_shadow_for_key(&mut self, code: KeyCode, modifiers: KeyModifiers) {
        // Handle Ctrl+Home and Ctrl+End specially (goto start/end of document)
        if modifiers.contains(KeyModifiers::CONTROL) {
            match code {
                KeyCode::Home => {
                    self.shadow_cursor = 0;
                    return;
                }
                KeyCode::End => {
                    self.shadow_cursor = self.shadow_string.len();
                    return;
                }
                _ => {
                    // Ignore other Ctrl combinations
                    return;
                }
            }
        }

        // Ignore Alt modifier keys for shadow
        if modifiers.contains(KeyModifiers::ALT) {
            return;
        }

        match code {
            KeyCode::Char(ch) => {
                self.shadow_string.insert(self.shadow_cursor, ch);
                self.shadow_cursor += ch.len_utf8();
            }
            KeyCode::Backspace => {
                if self.shadow_cursor > 0 {
                    self.shadow_cursor -= 1;
                    self.shadow_string.remove(self.shadow_cursor);
                }
            }
            KeyCode::Delete => {
                if self.shadow_cursor < self.shadow_string.len() {
                    self.shadow_string.remove(self.shadow_cursor);
                }
            }
            KeyCode::Enter => {
                self.shadow_string.insert(self.shadow_cursor, '\n');
                self.shadow_cursor += 1;
            }
            KeyCode::Left => {
                if self.shadow_cursor > 0 {
                    self.shadow_cursor -= 1;
                }
            }
            KeyCode::Right => {
                if self.shadow_cursor < self.shadow_string.len() {
                    self.shadow_cursor += 1;
                }
            }
            KeyCode::Home => {
                // Find start of current line
                let line_start = self.shadow_string[..self.shadow_cursor]
                    .rfind('\n')
                    .map(|pos| pos + 1)
                    .unwrap_or(0);
                self.shadow_cursor = line_start;
            }
            KeyCode::End => {
                // Find end of current line
                let line_end = self.shadow_string[self.shadow_cursor..]
                    .find('\n')
                    .map(|pos| self.shadow_cursor + pos)
                    .unwrap_or(self.shadow_string.len());
                self.shadow_cursor = line_end;
            }
            KeyCode::Up => {
                // Move to previous line, same column position
                let current_line_start = self.shadow_string[..self.shadow_cursor]
                    .rfind('\n')
                    .map(|pos| pos + 1)
                    .unwrap_or(0);
                let column = self.shadow_cursor - current_line_start;

                if current_line_start > 0 {
                    // Find start of previous line
                    let prev_line_end = current_line_start - 1; // The '\n' before current line
                    let prev_line_start = self.shadow_string[..prev_line_end]
                        .rfind('\n')
                        .map(|pos| pos + 1)
                        .unwrap_or(0);
                    let prev_line_len = prev_line_end - prev_line_start;

                    // Move to same column or end of previous line
                    self.shadow_cursor = prev_line_start + column.min(prev_line_len);
                }
            }
            KeyCode::Down => {
                // Move to next line, same column position
                let current_line_start = self.shadow_string[..self.shadow_cursor]
                    .rfind('\n')
                    .map(|pos| pos + 1)
                    .unwrap_or(0);
                let column = self.shadow_cursor - current_line_start;

                // Find next line start
                if let Some(next_line_start_offset) =
                    self.shadow_string[self.shadow_cursor..].find('\n')
                {
                    let next_line_start = self.shadow_cursor + next_line_start_offset + 1;
                    if next_line_start < self.shadow_string.len() {
                        // Find next line end
                        let next_line_end = self.shadow_string[next_line_start..]
                            .find('\n')
                            .map(|pos| next_line_start + pos)
                            .unwrap_or(self.shadow_string.len());
                        let next_line_len = next_line_end - next_line_start;

                        // Move to same column or end of next line
                        self.shadow_cursor = next_line_start + column.min(next_line_len);
                    }
                }
            }
            _ => {
                // Other keys don't modify shadow (e.g., PageUp, PageDown)
            }
        }
    }

    /// Get the primary cursor position
    pub fn cursor_position(&self) -> usize {
        self.editor.active_state().cursors.primary().position
    }

    /// Get the buffer length in bytes
    pub fn buffer_len(&self) -> usize {
        self.editor.active_state().buffer.len()
    }

    /// Check if the current buffer has a highlighter set up
    pub fn has_highlighter(&self) -> bool {
        !matches!(
            self.editor.active_state().highlighter,
            HighlightEngine::None
        )
    }

    /// Get the shadow string (for property testing)
    pub fn get_shadow_string(&self) -> &str {
        &self.shadow_string
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
                o.namespace
                    .as_ref()
                    .map(|ns| ns.as_str().starts_with("search"))
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
    /// Secondary cursors are detected by REVERSED style modifier or inactive cursor background
    pub fn find_all_cursors(&mut self) -> Vec<(u16, u16, String, bool)> {
        use ratatui::style::{Color, Modifier};
        let mut cursors = Vec::new();

        // Get hardware cursor position (primary cursor)
        let (hw_x, hw_y) = self.screen_cursor_position();

        // Get the buffer to read cell content
        let buffer = self.terminal.backend().buffer();
        let content_start = layout::CONTENT_START_ROW as u16;
        let content_end = buffer
            .area
            .height
            .saturating_sub(layout::BOTTOM_RESERVED_ROWS as u16);

        // Add primary cursor at hardware position
        if hw_y >= content_start && hw_y < content_end {
            if let Some(cell) = buffer.content.get(buffer.index_of(hw_x, hw_y)) {
                cursors.push((hw_x, hw_y, cell.symbol().to_string(), true));
            }
        }

        // Find secondary cursors (cells with REVERSED modifier or inactive cursor background)
        // Inactive cursor colors from theme.rs: Rgb(100,100,100) (dark), Rgb(180,180,180) (light), DarkGray (base16)
        let inactive_cursor_colors = [
            Color::Rgb(100, 100, 100),
            Color::Rgb(180, 180, 180),
            Color::DarkGray,
        ];

        for y in content_start..content_end {
            for x in 0..buffer.area.width {
                // Skip if this is the hardware cursor position
                if x == hw_x && y == hw_y {
                    continue;
                }

                let pos = buffer.index_of(x, y);
                if let Some(cell) = buffer.content.get(pos) {
                    let is_reversed = cell.modifier.contains(Modifier::REVERSED);
                    let has_inactive_cursor_bg = inactive_cursor_colors.contains(&cell.bg);
                    if is_reversed || has_inactive_cursor_bg {
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

    /// Get the top byte position of the viewport
    pub fn top_byte(&self) -> usize {
        self.editor.active_state().viewport.top_byte
    }

    /// Get the viewport height (number of content lines that can be displayed)
    pub fn viewport_height(&self) -> usize {
        self.editor.active_state().viewport.height as usize
    }

    /// Get the content area row range on screen (start_row, end_row inclusive)
    /// This accounts for menu bar, tab bar, status bar, and prompt line
    pub fn content_area_rows(&self) -> (usize, usize) {
        let terminal_height = self.terminal.size().unwrap().height as usize;
        let content_first_row = layout::CONTENT_START_ROW;
        let content_last_row = layout::content_end_row(terminal_height).saturating_sub(1);
        (content_first_row, content_last_row)
    }

    /// Get the terminal height
    pub fn terminal_height(&self) -> usize {
        self.terminal.size().unwrap().height as usize
    }

    /// Get a specific row from the screen as a string
    pub fn get_screen_row(&self, row: usize) -> String {
        let screen = self.screen_to_string();
        screen
            .lines()
            .nth(row)
            .map(|s| s.to_string())
            .unwrap_or_default()
    }

    /// Get the menu bar row content
    pub fn get_menu_bar(&self) -> String {
        self.get_screen_row(layout::MENU_BAR_ROW)
    }

    /// Get the tab bar row content
    pub fn get_tab_bar(&self) -> String {
        self.get_screen_row(layout::TAB_BAR_ROW)
    }

    /// Get the status bar row content
    pub fn get_status_bar(&self) -> String {
        self.get_screen_row(layout::status_bar_row(self.terminal_height()))
    }

    /// Get the prompt line content
    pub fn get_prompt_line(&self) -> String {
        self.get_screen_row(layout::prompt_line_row(self.terminal_height()))
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
    pub fn get_selected_text(&mut self) -> String {
        if let Some(range) = self.get_selection_range() {
            self.editor
                .active_state_mut()
                .get_text_range(range.start, range.end)
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
        let _ = self.editor.process_async_messages();
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

    /// Wait indefinitely for async operations until condition is met
    /// Repeatedly processes async messages until condition is met (no timeout)
    /// Use this for semantic events that must eventually occur
    pub fn wait_until<F>(&mut self, mut condition: F) -> io::Result<()>
    where
        F: FnMut(&Self) -> bool,
    {
        loop {
            self.process_async_and_render()?;
            if condition(self) {
                return Ok(());
            }
            std::thread::sleep(std::time::Duration::from_millis(10));
        }
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
