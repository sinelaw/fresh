//! Script Control Mode
//!
//! Provides a special run mode for the editor where it accepts commands via stdin
//! and dumps the screen state to stdout. This enables scripts (including LLMs) to interact
//! with the editor programmatically and allows converting interactions to scriptable tests.

use super::Editor;
use crate::{config::Config, model::control_event::EventBroadcaster};
use crossterm::event::{KeyCode, KeyModifiers, MouseButton, MouseEvent, MouseEventKind};
use ratatui::{backend::TestBackend, Terminal};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::{
    io::{self, BufRead, Write},
    path::PathBuf,
};

/// Commands that can be sent to the editor via stdin
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum ScriptCommand {
    /// Render the current screen state
    Render,

    /// Send a keyboard event
    Key {
        /// Key code (e.g., "a", "Enter", "Backspace", "Left", "F1")
        code: String,
        /// Modifiers (e.g., ["ctrl"], ["shift", "alt"])
        #[serde(default)]
        modifiers: Vec<String>,
    },

    /// Send a mouse click event
    MouseClick {
        /// Column (x coordinate, 0-indexed)
        col: u16,
        /// Row (y coordinate, 0-indexed)
        row: u16,
        /// Button ("left", "right", "middle")
        #[serde(default = "default_mouse_button")]
        button: String,
    },

    /// Send a mouse drag event
    MouseDrag {
        /// Start column
        start_col: u16,
        /// Start row
        start_row: u16,
        /// End column
        end_col: u16,
        /// End row
        end_row: u16,
        /// Button ("left", "right", "middle")
        #[serde(default = "default_mouse_button")]
        button: String,
    },

    /// Send a mouse scroll event
    MouseScroll {
        /// Column
        col: u16,
        /// Row
        row: u16,
        /// Direction ("up" or "down")
        direction: String,
        /// Number of lines to scroll
        #[serde(default = "default_scroll_amount")]
        amount: u16,
    },

    /// Resize the terminal
    Resize {
        /// New width
        width: u16,
        /// New height
        height: u16,
    },

    /// Get editor status (cursor position, buffer info, etc.)
    Status,

    /// Get the buffer content (actual text, not screen)
    GetBuffer,

    /// Open a file
    OpenFile {
        /// Path to the file
        path: String,
    },

    /// Type a string of text (convenience for multiple key presses)
    TypeText {
        /// Text to type
        text: String,
    },

    /// Quit the editor
    Quit,

    /// Export the interaction history as Rust test code
    ExportTest {
        /// Name for the generated test
        test_name: String,
    },

    /// Wait for a condition to be met (polls until condition is true or timeout)
    WaitFor {
        /// Condition to wait for
        condition: WaitCondition,
        /// Timeout in milliseconds (default: 5000)
        #[serde(default = "default_wait_timeout")]
        timeout_ms: u64,
        /// Poll interval in milliseconds (default: 100)
        #[serde(default = "default_poll_interval")]
        poll_interval_ms: u64,
    },

    /// Get all keyboard bindings
    GetKeybindings,
}

/// Conditions that can be waited for
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum WaitCondition {
    // Event-based: wait for event matching name pattern and optional data pattern
    /// Wait for event matching pattern. Name supports wildcards: "lsp:*", "*:error"
    Event {
        /// Event name pattern (supports * wildcard)
        name: String,
        /// Optional JSON data pattern to match (null = any data)
        #[serde(default)]
        data: Value,
    },

    // State-based: poll current state (fallback for simple checks)
    /// Wait for screen to contain a specific string
    ScreenContains { text: String },
    /// Wait for screen to NOT contain a specific string
    ScreenNotContains { text: String },
    /// Wait for buffer to contain specific text
    BufferContains { text: String },
    /// Wait for a popup to be visible
    PopupVisible,
    /// Wait for no popup to be visible
    PopupHidden,
}

fn default_mouse_button() -> String {
    "left".to_string()
}

fn default_scroll_amount() -> u16 {
    3
}

fn default_wait_timeout() -> u64 {
    5000
}

fn default_poll_interval() -> u64 {
    100
}

/// Response sent back to stdout
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum ScriptResponse {
    /// Screen render output
    Screen {
        /// The screen content as a string
        content: String,
        /// Terminal width
        width: u16,
        /// Terminal height
        height: u16,
        /// Cursor position (x, y)
        cursor: (u16, u16),
    },

    /// Editor status
    Status {
        /// Cursor position in buffer (byte offset)
        cursor_position: usize,
        /// Number of cursors
        cursor_count: usize,
        /// Whether there's an active selection
        has_selection: bool,
        /// Buffer length in bytes
        buffer_len: usize,
        /// Current file path (if any)
        file_path: Option<String>,
        /// Whether buffer is modified
        is_modified: bool,
    },

    /// Buffer content
    Buffer {
        /// The actual text content
        content: String,
    },

    /// Success response
    Ok {
        /// Optional message
        #[serde(skip_serializing_if = "Option::is_none")]
        message: Option<String>,
    },

    /// Error response
    Error {
        /// Error message
        message: String,
    },

    /// Generated test code
    TestCode {
        /// The Rust test code
        code: String,
    },

    /// Keyboard bindings map
    Keybindings {
        /// Map of key combinations to action descriptions
        bindings: Vec<KeybindingEntry>,
    },
}

/// A single keybinding entry
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct KeybindingEntry {
    /// Key combination (e.g., "Ctrl+S", "Alt+F")
    pub key: String,
    /// Action description (e.g., "Save file", "[menu] Open File menu")
    pub action: String,
}

/// Tracks interactions for test generation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InteractionRecord {
    /// The command that was executed
    pub command: ScriptCommand,
    /// Timestamp of the interaction
    pub timestamp_ms: u64,
}

/// Script Control Mode runner
pub struct ScriptControlMode {
    editor: Editor,
    terminal: Terminal<TestBackend>,
    interactions: Vec<InteractionRecord>,
    start_time: std::time::Instant,
}

impl ScriptControlMode {
    /// Create a new script control mode instance
    pub fn new(width: u16, height: u16) -> io::Result<Self> {
        let backend = TestBackend::new(width, height);
        let terminal = Terminal::new(backend)?;
        let config = Config::default();
        let editor = Editor::new(config, width, height)?;

        Ok(Self {
            editor,
            terminal,
            interactions: Vec::new(),
            start_time: std::time::Instant::now(),
        })
    }

    /// Create with a specific working directory
    pub fn with_working_dir(width: u16, height: u16, working_dir: PathBuf) -> io::Result<Self> {
        let backend = TestBackend::new(width, height);
        let terminal = Terminal::new(backend)?;
        let config = Config::default();
        let editor = Editor::with_working_dir(config, width, height, Some(working_dir))?;

        Ok(Self {
            editor,
            terminal,
            interactions: Vec::new(),
            start_time: std::time::Instant::now(),
        })
    }

    /// Get a reference to the event broadcaster (from editor)
    pub fn event_broadcaster(&self) -> &EventBroadcaster {
        self.editor.event_broadcaster()
    }

    /// Open a file before running the control loop
    pub fn open_file(&mut self, path: &PathBuf) -> io::Result<()> {
        self.editor.open_file(path)?;
        let _ = self.editor.process_async_messages();
        self.render_to_terminal()?;
        Ok(())
    }

    /// Run the script control loop
    pub fn run(&mut self) -> io::Result<()> {
        let stdin = io::stdin();
        let mut stdout = io::stdout();

        // Initial render
        self.render_to_terminal()?;

        // Send ready message
        let ready_response = ScriptResponse::Ok {
            message: Some("Script Control Mode ready. Send JSON commands to stdin.".to_string()),
        };
        writeln!(stdout, "{}", serde_json::to_string(&ready_response)?)?;
        stdout.flush()?;

        // Read commands from stdin
        for line in stdin.lock().lines() {
            let line = match line {
                Ok(l) => l,
                Err(e) => {
                    let response = ScriptResponse::Error {
                        message: format!("Failed to read line: {}", e),
                    };
                    writeln!(stdout, "{}", serde_json::to_string(&response)?)?;
                    stdout.flush()?;
                    continue;
                }
            };

            // Skip empty lines
            if line.trim().is_empty() {
                continue;
            }

            // Parse command
            let command: ScriptCommand = match serde_json::from_str(&line) {
                Ok(cmd) => cmd,
                Err(e) => {
                    let response = ScriptResponse::Error {
                        message: format!("Failed to parse command: {}", e),
                    };
                    writeln!(stdout, "{}", serde_json::to_string(&response)?)?;
                    stdout.flush()?;
                    continue;
                }
            };

            tracing::trace!(
                "script_control: received command {:?}",
                std::mem::discriminant(&command)
            );

            // Record interaction
            self.record_interaction(command.clone());

            // Execute command
            let response = self.execute_command(command)?;

            // Send response
            writeln!(stdout, "{}", serde_json::to_string(&response)?)?;
            stdout.flush()?;

            // Check if we should quit
            if self.editor.should_quit() {
                break;
            }
        }

        Ok(())
    }

    /// Record an interaction for test generation
    fn record_interaction(&mut self, command: ScriptCommand) {
        let timestamp_ms = self.start_time.elapsed().as_millis() as u64;
        self.interactions.push(InteractionRecord {
            command,
            timestamp_ms,
        });
    }

    /// Execute a single command
    fn execute_command(&mut self, command: ScriptCommand) -> io::Result<ScriptResponse> {
        match command {
            ScriptCommand::Render => self.handle_render(),
            ScriptCommand::Key { code, modifiers } => self.handle_key(&code, &modifiers),
            ScriptCommand::MouseClick { col, row, button } => {
                self.handle_mouse_click(col, row, &button)
            }
            ScriptCommand::MouseDrag {
                start_col,
                start_row,
                end_col,
                end_row,
                button,
            } => self.handle_mouse_drag(start_col, start_row, end_col, end_row, &button),
            ScriptCommand::MouseScroll {
                col,
                row,
                direction,
                amount,
            } => self.handle_mouse_scroll(col, row, &direction, amount),
            ScriptCommand::Resize { width, height } => self.handle_resize(width, height),
            ScriptCommand::Status => self.handle_status(),
            ScriptCommand::GetBuffer => self.handle_get_buffer(),
            ScriptCommand::OpenFile { path } => self.handle_open_file(&path),
            ScriptCommand::TypeText { text } => self.handle_type_text(&text),
            ScriptCommand::Quit => self.handle_quit(),
            ScriptCommand::ExportTest { test_name } => self.handle_export_test(&test_name),
            ScriptCommand::WaitFor {
                condition,
                timeout_ms,
                poll_interval_ms,
            } => self.handle_wait_for(condition, timeout_ms, poll_interval_ms),
            ScriptCommand::GetKeybindings => self.handle_get_keybindings(),
        }
    }

    /// Render to the virtual terminal
    fn render_to_terminal(&mut self) -> io::Result<()> {
        self.terminal.draw(|frame| {
            self.editor.render(frame);
        })?;
        Ok(())
    }

    /// Get screen as string
    fn screen_to_string(&self) -> String {
        let buffer = self.terminal.backend().buffer();
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

    /// Get cursor position from terminal
    fn cursor_position(&mut self) -> io::Result<(u16, u16)> {
        let pos = self.terminal.get_cursor_position()?;
        Ok((pos.x, pos.y))
    }

    /// Handle render command
    fn handle_render(&mut self) -> io::Result<ScriptResponse> {
        self.render_to_terminal()?;
        let content = self.screen_to_string();
        let cursor = self.cursor_position()?;
        let size = self.terminal.size()?;

        Ok(ScriptResponse::Screen {
            content,
            width: size.width,
            height: size.height,
            cursor,
        })
    }

    /// Parse key code string to KeyCode
    fn parse_key_code(code: &str) -> Result<KeyCode, String> {
        match code.to_lowercase().as_str() {
            "backspace" => Ok(KeyCode::Backspace),
            "enter" | "return" => Ok(KeyCode::Enter),
            "left" => Ok(KeyCode::Left),
            "right" => Ok(KeyCode::Right),
            "up" => Ok(KeyCode::Up),
            "down" => Ok(KeyCode::Down),
            "home" => Ok(KeyCode::Home),
            "end" => Ok(KeyCode::End),
            "pageup" | "page_up" => Ok(KeyCode::PageUp),
            "pagedown" | "page_down" => Ok(KeyCode::PageDown),
            "tab" => Ok(KeyCode::Tab),
            "backtab" => Ok(KeyCode::BackTab),
            "delete" | "del" => Ok(KeyCode::Delete),
            "insert" | "ins" => Ok(KeyCode::Insert),
            "escape" | "esc" => Ok(KeyCode::Esc),
            "space" => Ok(KeyCode::Char(' ')),
            "f1" => Ok(KeyCode::F(1)),
            "f2" => Ok(KeyCode::F(2)),
            "f3" => Ok(KeyCode::F(3)),
            "f4" => Ok(KeyCode::F(4)),
            "f5" => Ok(KeyCode::F(5)),
            "f6" => Ok(KeyCode::F(6)),
            "f7" => Ok(KeyCode::F(7)),
            "f8" => Ok(KeyCode::F(8)),
            "f9" => Ok(KeyCode::F(9)),
            "f10" => Ok(KeyCode::F(10)),
            "f11" => Ok(KeyCode::F(11)),
            "f12" => Ok(KeyCode::F(12)),
            s if s.len() == 1 => {
                let ch = s.chars().next().unwrap();
                Ok(KeyCode::Char(ch))
            }
            _ => Err(format!("Unknown key code: {}", code)),
        }
    }

    /// Parse modifier strings to KeyModifiers
    fn parse_modifiers(modifiers: &[String]) -> KeyModifiers {
        let mut result = KeyModifiers::NONE;
        for modifier in modifiers {
            match modifier.to_lowercase().as_str() {
                "ctrl" | "control" => result |= KeyModifiers::CONTROL,
                "alt" => result |= KeyModifiers::ALT,
                "shift" => result |= KeyModifiers::SHIFT,
                "super" | "meta" => result |= KeyModifiers::SUPER,
                _ => {}
            }
        }
        result
    }

    /// Handle key command
    fn handle_key(&mut self, code: &str, modifiers: &[String]) -> io::Result<ScriptResponse> {
        let key_code = match Self::parse_key_code(code) {
            Ok(k) => k,
            Err(e) => {
                return Ok(ScriptResponse::Error { message: e });
            }
        };
        let key_modifiers = Self::parse_modifiers(modifiers);

        self.editor.handle_key(key_code, key_modifiers)?;
        let _ = self.editor.process_async_messages();
        self.render_to_terminal()?;

        Ok(ScriptResponse::Ok { message: None })
    }

    /// Parse mouse button string
    fn parse_mouse_button(button: &str) -> Result<MouseButton, String> {
        match button.to_lowercase().as_str() {
            "left" => Ok(MouseButton::Left),
            "right" => Ok(MouseButton::Right),
            "middle" => Ok(MouseButton::Middle),
            _ => Err(format!("Unknown mouse button: {}", button)),
        }
    }

    /// Handle mouse click command
    fn handle_mouse_click(
        &mut self,
        col: u16,
        row: u16,
        button: &str,
    ) -> io::Result<ScriptResponse> {
        let mouse_button = match Self::parse_mouse_button(button) {
            Ok(b) => b,
            Err(e) => {
                return Ok(ScriptResponse::Error { message: e });
            }
        };

        // Send mouse down
        let mouse_down = MouseEvent {
            kind: MouseEventKind::Down(mouse_button),
            column: col,
            row,
            modifiers: KeyModifiers::empty(),
        };
        self.editor.handle_mouse(mouse_down)?;

        // Send mouse up
        let mouse_up = MouseEvent {
            kind: MouseEventKind::Up(mouse_button),
            column: col,
            row,
            modifiers: KeyModifiers::empty(),
        };
        self.editor.handle_mouse(mouse_up)?;

        let _ = self.editor.process_async_messages();
        self.render_to_terminal()?;

        Ok(ScriptResponse::Ok { message: None })
    }

    /// Handle mouse drag command
    fn handle_mouse_drag(
        &mut self,
        start_col: u16,
        start_row: u16,
        end_col: u16,
        end_row: u16,
        button: &str,
    ) -> io::Result<ScriptResponse> {
        let mouse_button = match Self::parse_mouse_button(button) {
            Ok(b) => b,
            Err(e) => {
                return Ok(ScriptResponse::Error { message: e });
            }
        };

        // Send initial press
        let mouse_down = MouseEvent {
            kind: MouseEventKind::Down(mouse_button),
            column: start_col,
            row: start_row,
            modifiers: KeyModifiers::empty(),
        };
        self.editor.handle_mouse(mouse_down)?;

        // Interpolate drag positions
        let steps = ((end_row as i32 - start_row as i32).abs())
            .max((end_col as i32 - start_col as i32).abs())
            .max(1);

        for i in 1..=steps {
            let t = i as f32 / steps as f32;
            let col = start_col as f32 + (end_col as f32 - start_col as f32) * t;
            let row = start_row as f32 + (end_row as f32 - start_row as f32) * t;

            let drag_event = MouseEvent {
                kind: MouseEventKind::Drag(mouse_button),
                column: col as u16,
                row: row as u16,
                modifiers: KeyModifiers::empty(),
            };
            self.editor.handle_mouse(drag_event)?;
        }

        // Send final release
        let mouse_up = MouseEvent {
            kind: MouseEventKind::Up(mouse_button),
            column: end_col,
            row: end_row,
            modifiers: KeyModifiers::empty(),
        };
        self.editor.handle_mouse(mouse_up)?;

        let _ = self.editor.process_async_messages();
        self.render_to_terminal()?;

        Ok(ScriptResponse::Ok { message: None })
    }

    /// Handle mouse scroll command
    fn handle_mouse_scroll(
        &mut self,
        col: u16,
        row: u16,
        direction: &str,
        amount: u16,
    ) -> io::Result<ScriptResponse> {
        let scroll_kind = match direction.to_lowercase().as_str() {
            "up" => MouseEventKind::ScrollUp,
            "down" => MouseEventKind::ScrollDown,
            _ => {
                return Ok(ScriptResponse::Error {
                    message: format!("Unknown scroll direction: {}", direction),
                });
            }
        };

        // Send scroll events
        for _ in 0..amount {
            let scroll_event = MouseEvent {
                kind: scroll_kind,
                column: col,
                row,
                modifiers: KeyModifiers::empty(),
            };
            self.editor.handle_mouse(scroll_event)?;
        }

        let _ = self.editor.process_async_messages();
        self.render_to_terminal()?;

        Ok(ScriptResponse::Ok { message: None })
    }

    /// Handle resize command
    fn handle_resize(&mut self, width: u16, height: u16) -> io::Result<ScriptResponse> {
        self.terminal.backend_mut().resize(width, height);
        self.editor.resize(width, height);
        self.render_to_terminal()?;

        Ok(ScriptResponse::Ok { message: None })
    }

    /// Handle status command
    fn handle_status(&mut self) -> io::Result<ScriptResponse> {
        let state = self.editor.active_state();
        let cursor_position = state.cursors.primary().position;
        let cursor_count = state.cursors.count();
        let has_selection = !state.cursors.primary().collapsed();
        let buffer_len = state.buffer.len();
        let is_modified = state.buffer.is_modified();
        // file_path is stored in buffer_metadata, but we don't have access to it here
        // For now, just return None for file_path
        let file_path: Option<String> = None;

        Ok(ScriptResponse::Status {
            cursor_position,
            cursor_count,
            has_selection,
            buffer_len,
            file_path,
            is_modified,
        })
    }

    /// Handle get buffer command
    fn handle_get_buffer(&self) -> io::Result<ScriptResponse> {
        let content = self.editor.active_state().buffer.to_string().unwrap_or_default();
        Ok(ScriptResponse::Buffer { content })
    }

    /// Handle open file command
    fn handle_open_file(&mut self, path: &str) -> io::Result<ScriptResponse> {
        let path = PathBuf::from(path);
        match self.editor.open_file(&path) {
            Ok(_) => {
                let _ = self.editor.process_async_messages();
                self.render_to_terminal()?;
                Ok(ScriptResponse::Ok {
                    message: Some(format!("Opened file: {}", path.display())),
                })
            }
            Err(e) => Ok(ScriptResponse::Error {
                message: format!("Failed to open file: {}", e),
            }),
        }
    }

    /// Handle type text command
    fn handle_type_text(&mut self, text: &str) -> io::Result<ScriptResponse> {
        for ch in text.chars() {
            self.editor
                .handle_key(KeyCode::Char(ch), KeyModifiers::NONE)?;
        }
        let _ = self.editor.process_async_messages();
        self.render_to_terminal()?;

        Ok(ScriptResponse::Ok { message: None })
    }

    /// Handle quit command
    fn handle_quit(&mut self) -> io::Result<ScriptResponse> {
        // Quit the editor
        self.editor.quit();
        Ok(ScriptResponse::Ok {
            message: Some("Quitting editor".to_string()),
        })
    }

    /// Handle export test command
    fn handle_export_test(&self, test_name: &str) -> io::Result<ScriptResponse> {
        let code = self.generate_test_code(test_name);
        Ok(ScriptResponse::TestCode { code })
    }

    /// Handle get_keybindings command
    fn handle_get_keybindings(&self) -> io::Result<ScriptResponse> {
        let raw_bindings = self.editor.get_all_keybindings();
        let bindings: Vec<KeybindingEntry> = raw_bindings
            .into_iter()
            .map(|(key, action)| KeybindingEntry { key, action })
            .collect();
        Ok(ScriptResponse::Keybindings { bindings })
    }

    /// Handle wait_for command
    fn handle_wait_for(
        &mut self,
        condition: WaitCondition,
        timeout_ms: u64,
        poll_interval_ms: u64,
    ) -> io::Result<ScriptResponse> {
        let start = std::time::Instant::now();
        let timeout = std::time::Duration::from_millis(timeout_ms);
        let poll_interval = std::time::Duration::from_millis(poll_interval_ms);

        loop {
            tracing::trace!("wait_for: polling loop iteration");
            // Process any pending async messages
            let _ = self.editor.process_async_messages();
            self.render_to_terminal()?;

            // Check if condition is met
            if self.check_wait_condition(&condition)? {
                return Ok(ScriptResponse::Ok {
                    message: Some(format!(
                        "Condition met after {}ms",
                        start.elapsed().as_millis()
                    )),
                });
            }

            // Check timeout
            if start.elapsed() >= timeout {
                return Ok(ScriptResponse::Error {
                    message: format!(
                        "Timeout after {}ms waiting for condition: {:?}",
                        timeout_ms, condition
                    ),
                });
            }

            // Sleep before next poll
            std::thread::sleep(poll_interval);
        }
    }

    /// Check if a wait condition is met
    fn check_wait_condition(&mut self, condition: &WaitCondition) -> io::Result<bool> {
        match condition {
            // Event-based: check if matching event exists
            WaitCondition::Event { name, data } => {
                Ok(self.editor.event_broadcaster().has_match(name, data))
            }

            // State-based: poll current state
            WaitCondition::ScreenContains { text } => {
                let screen = self.screen_to_string();
                Ok(screen.contains(text))
            }
            WaitCondition::ScreenNotContains { text } => {
                let screen = self.screen_to_string();
                Ok(!screen.contains(text))
            }
            WaitCondition::BufferContains { text } => {
                let buffer = self.editor.active_state().buffer.to_string().unwrap_or_default();
                Ok(buffer.contains(text))
            }
            WaitCondition::PopupVisible => {
                let state = self.editor.active_state();
                Ok(state.popups.is_visible())
            }
            WaitCondition::PopupHidden => {
                let state = self.editor.active_state();
                Ok(!state.popups.is_visible())
            }
        }
    }

    /// Generate Rust test code from recorded interactions
    fn generate_test_code(&self, test_name: &str) -> String {
        let mut code = String::new();

        code.push_str(&format!(
            r#"#[test]
fn {}() -> std::io::Result<()> {{
    let mut harness = EditorTestHarness::new(80, 24)?;
    harness.render()?;

"#,
            test_name
        ));

        for interaction in &self.interactions {
            match &interaction.command {
                ScriptCommand::Render => {
                    code.push_str("    harness.render()?;\n");
                }
                ScriptCommand::Key {
                    code: key,
                    modifiers,
                } => {
                    let key_code = self.key_code_to_rust_code(key);
                    let mods = self.modifiers_to_rust_code(modifiers);
                    code.push_str(&format!("    harness.send_key({}, {})?;\n", key_code, mods));
                }
                ScriptCommand::MouseClick {
                    col,
                    row,
                    button: _,
                } => {
                    code.push_str(&format!("    harness.mouse_click({}, {})?;\n", col, row));
                }
                ScriptCommand::MouseDrag {
                    start_col,
                    start_row,
                    end_col,
                    end_row,
                    button: _,
                } => {
                    code.push_str(&format!(
                        "    harness.mouse_drag({}, {}, {}, {})?;\n",
                        start_col, start_row, end_col, end_row
                    ));
                }
                ScriptCommand::TypeText { text } => {
                    let escaped = text.replace('\\', "\\\\").replace('"', "\\\"");
                    code.push_str(&format!("    harness.type_text(\"{}\")?;\n", escaped));
                }
                ScriptCommand::OpenFile { path } => {
                    let escaped = path.replace('\\', "\\\\").replace('"', "\\\"");
                    code.push_str(&format!(
                        "    harness.open_file(std::path::Path::new(\"{}\"))?;\n",
                        escaped
                    ));
                }
                ScriptCommand::Resize { width, height } => {
                    code.push_str(&format!("    harness.resize({}, {})?;\n", width, height));
                }
                _ => {
                    // Skip non-action commands like Status, GetBuffer, etc.
                }
            }
        }

        code.push_str(
            r#"
    Ok(())
}
"#,
        );

        code
    }

    /// Convert key code string to Rust KeyCode expression
    fn key_code_to_rust_code(&self, code: &str) -> String {
        match code.to_lowercase().as_str() {
            "backspace" => "KeyCode::Backspace".to_string(),
            "enter" | "return" => "KeyCode::Enter".to_string(),
            "left" => "KeyCode::Left".to_string(),
            "right" => "KeyCode::Right".to_string(),
            "up" => "KeyCode::Up".to_string(),
            "down" => "KeyCode::Down".to_string(),
            "home" => "KeyCode::Home".to_string(),
            "end" => "KeyCode::End".to_string(),
            "pageup" | "page_up" => "KeyCode::PageUp".to_string(),
            "pagedown" | "page_down" => "KeyCode::PageDown".to_string(),
            "tab" => "KeyCode::Tab".to_string(),
            "delete" | "del" => "KeyCode::Delete".to_string(),
            "escape" | "esc" => "KeyCode::Esc".to_string(),
            "space" => "KeyCode::Char(' ')".to_string(),
            s if s.starts_with('f') && s.len() > 1 => {
                if let Ok(num) = s[1..].parse::<u8>() {
                    format!("KeyCode::F({})", num)
                } else {
                    format!("KeyCode::Char('{}')", s.chars().next().unwrap())
                }
            }
            s if s.len() == 1 => {
                let ch = s.chars().next().unwrap();
                format!("KeyCode::Char('{}')", ch)
            }
            _ => format!("KeyCode::Char('{}')", code.chars().next().unwrap_or('?')),
        }
    }

    /// Convert modifier strings to Rust KeyModifiers expression
    fn modifiers_to_rust_code(&self, modifiers: &[String]) -> String {
        if modifiers.is_empty() {
            return "KeyModifiers::NONE".to_string();
        }

        let mut parts = Vec::new();
        for modifier in modifiers {
            match modifier.to_lowercase().as_str() {
                "ctrl" | "control" => parts.push("KeyModifiers::CONTROL"),
                "alt" => parts.push("KeyModifiers::ALT"),
                "shift" => parts.push("KeyModifiers::SHIFT"),
                "super" | "meta" => parts.push("KeyModifiers::SUPER"),
                _ => {}
            }
        }

        if parts.is_empty() {
            "KeyModifiers::NONE".to_string()
        } else if parts.len() == 1 {
            parts[0].to_string()
        } else {
            parts.join(" | ")
        }
    }
}

/// Schema information for LLM consumption
pub fn get_command_schema() -> String {
    serde_json::json!({
        "commands": [
            {
                "type": "render",
                "description": "Render the current screen state and return it",
                "example": {"type": "render"}
            },
            {
                "type": "key",
                "description": "Send a keyboard event",
                "parameters": {
                    "code": "Key code (e.g., 'a', 'Enter', 'Backspace', 'Left', 'F1')",
                    "modifiers": "Optional array of modifiers: 'ctrl', 'alt', 'shift', 'super'"
                },
                "examples": [
                    {"type": "key", "code": "a"},
                    {"type": "key", "code": "s", "modifiers": ["ctrl"]},
                    {"type": "key", "code": "Enter"}
                ]
            },
            {
                "type": "mouse_click",
                "description": "Click at a screen position",
                "parameters": {
                    "col": "Column (x coordinate, 0-indexed)",
                    "row": "Row (y coordinate, 0-indexed)",
                    "button": "Optional: 'left' (default), 'right', 'middle'"
                },
                "example": {"type": "mouse_click", "col": 10, "row": 5}
            },
            {
                "type": "mouse_drag",
                "description": "Drag from one position to another",
                "parameters": {
                    "start_col": "Start column",
                    "start_row": "Start row",
                    "end_col": "End column",
                    "end_row": "End row",
                    "button": "Optional: 'left' (default), 'right', 'middle'"
                },
                "example": {"type": "mouse_drag", "start_col": 10, "start_row": 5, "end_col": 20, "end_row": 5}
            },
            {
                "type": "mouse_scroll",
                "description": "Scroll at a position",
                "parameters": {
                    "col": "Column",
                    "row": "Row",
                    "direction": "'up' or 'down'",
                    "amount": "Optional: number of lines to scroll (default: 3)"
                },
                "example": {"type": "mouse_scroll", "col": 40, "row": 12, "direction": "down"}
            },
            {
                "type": "resize",
                "description": "Resize the terminal",
                "parameters": {
                    "width": "New width",
                    "height": "New height"
                },
                "example": {"type": "resize", "width": 120, "height": 40}
            },
            {
                "type": "status",
                "description": "Get editor status (cursor position, buffer info)",
                "example": {"type": "status"}
            },
            {
                "type": "get_buffer",
                "description": "Get the actual buffer content (not screen representation)",
                "example": {"type": "get_buffer"}
            },
            {
                "type": "open_file",
                "description": "Open a file in the editor",
                "parameters": {
                    "path": "Path to the file"
                },
                "example": {"type": "open_file", "path": "/path/to/file.txt"}
            },
            {
                "type": "type_text",
                "description": "Type a string of text (convenience for multiple key presses)",
                "parameters": {
                    "text": "Text to type"
                },
                "example": {"type": "type_text", "text": "Hello, World!"}
            },
            {
                "type": "quit",
                "description": "Quit the editor",
                "example": {"type": "quit"}
            },
            {
                "type": "export_test",
                "description": "Export the interaction history as Rust test code",
                "parameters": {
                    "test_name": "Name for the generated test function"
                },
                "example": {"type": "export_test", "test_name": "test_basic_editing"}
            },
            {
                "type": "get_keybindings",
                "description": "Get all keyboard bindings (key combinations mapped to actions)",
                "example": {"type": "get_keybindings"},
                "response_format": {
                    "type": "keybindings",
                    "bindings": [
                        {"key": "Ctrl+S", "action": "Save file"},
                        {"key": "Ctrl+Q", "action": "Quit"},
                        {"key": "Alt+F", "action": "[menu] Open File menu"}
                    ]
                }
            },
            {
                "type": "wait_for",
                "description": "Wait for a condition to be met (event-based or state polling)",
                "parameters": {
                    "condition": "Condition object to wait for",
                    "timeout_ms": "Optional timeout in milliseconds (default: 5000)",
                    "poll_interval_ms": "Optional poll interval in milliseconds (default: 100)"
                },
                "condition_types": {
                    "event_based": {
                        "type": "event",
                        "description": "Wait for event matching name pattern (* = wildcard) and optional data",
                        "examples": [
                            {"type": "event", "name": "lsp:status_changed", "data": {"status": "running"}},
                            {"type": "event", "name": "editor:file_saved"},
                            {"type": "event", "name": "lsp:*"},
                            {"type": "event", "name": "plugin:git:*", "data": {"branch": "main"}}
                        ],
                        "currently_emitted_events": crate::model::control_event::events::schema(),
                        "pattern_syntax": {
                            "*": "Matches any event",
                            "prefix:*": "Matches events starting with prefix (e.g., 'lsp:*')",
                            "*:suffix": "Matches events ending with suffix (e.g., '*:error')",
                            "exact:name": "Exact match"
                        },
                        "data_matching": {
                            "{}": "Matches any data",
                            "{\"key\": \"value\"}": "Data must contain key with exact value",
                            "{\"key\": null}": "Data must contain key (any value)"
                        }
                    },
                    "state_based": [
                        {"type": "screen_contains", "text": "Error"},
                        {"type": "screen_not_contains", "text": "Loading"},
                        {"type": "buffer_contains", "text": "fn main"},
                        {"type": "popup_visible"},
                        {"type": "popup_hidden"}
                    ]
                },
                "examples": [
                    {"type": "wait_for", "condition": {"type": "event", "name": "lsp:status_changed", "data": {"status": "running"}}},
                    {"type": "wait_for", "condition": {"type": "screen_contains", "text": "Error"}, "timeout_ms": 10000},
                    {"type": "wait_for", "condition": {"type": "popup_visible"}, "poll_interval_ms": 50}
                ]
            }
        ]
    })
    .to_string()
}
