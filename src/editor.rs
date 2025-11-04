use crate::config::Config;
use crate::event::{Event, EventLog};
use crate::keybindings::{Action, KeybindingResolver};
use crate::state::EditorState;
use ratatui::{
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, Paragraph, Tabs},
    Frame,
};
use std::collections::HashMap;
use std::io;
use std::path::Path;

/// Unique identifier for a buffer
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BufferId(usize);

/// Type of prompt - determines what action to take when user confirms
#[derive(Debug, Clone, PartialEq)]
pub enum PromptType {
    /// Open a file
    OpenFile,
    /// Save current buffer to a new file
    SaveFileAs,
    /// Search for text in buffer
    Search,
    /// Replace text in buffer
    Replace { search: String },
    /// Execute a command by name (M-x)
    Command,
}

/// A single suggestion item for autocomplete
#[derive(Debug, Clone, PartialEq)]
pub struct Suggestion {
    /// The text to display
    pub text: String,
    /// Optional description
    pub description: Option<String>,
    /// The value to use when selected (defaults to text if None)
    pub value: Option<String>,
}

impl Suggestion {
    pub fn new(text: String) -> Self {
        Self {
            text,
            description: None,
            value: None,
        }
    }

    pub fn with_description(text: String, description: String) -> Self {
        Self {
            text,
            description: Some(description),
            value: None,
        }
    }

    pub fn get_value(&self) -> &str {
        self.value.as_ref().unwrap_or(&self.text)
    }
}

/// Prompt state for the minibuffer
#[derive(Debug, Clone)]
pub struct Prompt {
    /// The prompt message (e.g., "Find file: ")
    pub message: String,
    /// User's current input
    pub input: String,
    /// Cursor position in the input
    pub cursor_pos: usize,
    /// What to do when user confirms
    pub prompt_type: PromptType,
    /// Autocomplete suggestions
    pub suggestions: Vec<Suggestion>,
    /// Currently selected suggestion index
    pub selected_suggestion: Option<usize>,
}

/// A command that can be executed from the command palette
#[derive(Debug, Clone)]
pub struct Command {
    /// Command name (e.g., "Open File")
    pub name: String,
    /// Command description
    pub description: String,
    /// The action to trigger
    pub action: Action,
}

/// The main editor struct - manages multiple buffers, clipboard, and rendering
pub struct Editor {
    /// All open buffers
    buffers: HashMap<BufferId, EditorState>,

    /// Currently active buffer
    active_buffer: BufferId,

    /// Event log per buffer (for undo/redo)
    event_logs: HashMap<BufferId, EventLog>,

    /// Next buffer ID to assign
    next_buffer_id: usize,

    /// Configuration
    config: Config,

    /// Keybinding resolver
    keybindings: KeybindingResolver,

    /// Shared clipboard
    clipboard: String,

    /// Should the editor quit?
    should_quit: bool,

    /// Status message (shown in status bar)
    status_message: Option<String>,

    /// Is the help page visible?
    help_visible: bool,

    /// Scroll offset for help page
    help_scroll: usize,

    /// Active prompt (minibuffer)
    prompt: Option<Prompt>,

    /// Terminal dimensions (for creating new buffers)
    terminal_width: u16,
    terminal_height: u16,
}

impl Editor {
    /// Create a new editor with the given configuration and terminal dimensions
    pub fn new(config: Config, width: u16, height: u16) -> io::Result<Self> {
        tracing::info!("Editor::new called with width={}, height={}", width, height);
        let keybindings = KeybindingResolver::new(&config);

        // Create an empty initial buffer
        let mut buffers = HashMap::new();
        let mut event_logs = HashMap::new();

        let buffer_id = BufferId(0);
        let state = EditorState::new(width, height);
        tracing::info!("EditorState created with viewport height: {}", state.viewport.height);
        buffers.insert(buffer_id, state);
        event_logs.insert(buffer_id, EventLog::new());

        Ok(Editor {
            buffers,
            active_buffer: buffer_id,
            event_logs,
            next_buffer_id: 1,
            config,
            keybindings,
            clipboard: String::new(),
            should_quit: false,
            status_message: None,
            help_visible: false,
            help_scroll: 0,
            prompt: None,
            terminal_width: width,
            terminal_height: height,
        })
    }

    /// Enable event log streaming to a file
    pub fn enable_event_streaming<P: AsRef<Path>>(&mut self, path: P) -> io::Result<()> {
        // Enable streaming for all existing event logs
        for event_log in self.event_logs.values_mut() {
            event_log.enable_streaming(&path)?;
        }
        Ok(())
    }

    /// Log keystroke for debugging
    pub fn log_keystroke(&mut self, key_code: &str, modifiers: &str) {
        if let Some(event_log) = self.event_logs.get_mut(&self.active_buffer) {
            event_log.log_keystroke(key_code, modifiers);
        }
    }

    /// Open a file and return its buffer ID
    pub fn open_file(&mut self, path: &Path) -> io::Result<BufferId> {
        // Check if file is already open
        for (id, state) in &self.buffers {
            if state.buffer.file_path() == Some(path) {
                self.active_buffer = *id;
                return Ok(*id);
            }
        }

        // Create new buffer for this file
        let buffer_id = BufferId(self.next_buffer_id);
        self.next_buffer_id += 1;

        let state = EditorState::from_file(path, self.terminal_width, self.terminal_height)?;
        self.buffers.insert(buffer_id, state);
        self.event_logs.insert(buffer_id, EventLog::new());

        self.active_buffer = buffer_id;
        self.status_message = Some(format!("Opened {}", path.display()));

        Ok(buffer_id)
    }

    /// Create a new empty buffer
    pub fn new_buffer(&mut self) -> BufferId {
        let buffer_id = BufferId(self.next_buffer_id);
        self.next_buffer_id += 1;

        self.buffers.insert(buffer_id, EditorState::new(self.terminal_width, self.terminal_height));
        self.event_logs.insert(buffer_id, EventLog::new());

        self.active_buffer = buffer_id;
        self.status_message = Some("New buffer".to_string());

        buffer_id
    }

    /// Close the given buffer
    pub fn close_buffer(&mut self, id: BufferId) -> io::Result<()> {
        // Can't close if it's the only buffer
        if self.buffers.len() == 1 {
            return Err(io::Error::other("Cannot close last buffer"));
        }

        // Check for unsaved changes
        if let Some(state) = self.buffers.get(&id) {
            if state.buffer.is_modified() {
                return Err(io::Error::other("Buffer has unsaved changes"));
            }
        }

        self.buffers.remove(&id);
        self.event_logs.remove(&id);

        // Switch to another buffer if we closed the active one
        if self.active_buffer == id {
            self.active_buffer = *self.buffers.keys().next().unwrap();
        }

        Ok(())
    }

    /// Switch to the given buffer
    pub fn switch_buffer(&mut self, id: BufferId) {
        if self.buffers.contains_key(&id) {
            self.active_buffer = id;
        }
    }

    /// Switch to next buffer
    pub fn next_buffer(&mut self) {
        let ids: Vec<_> = self.buffers.keys().copied().collect();
        if let Some(idx) = ids.iter().position(|&id| id == self.active_buffer) {
            let next_idx = (idx + 1) % ids.len();
            self.active_buffer = ids[next_idx];
        }
    }

    /// Switch to previous buffer
    pub fn prev_buffer(&mut self) {
        let ids: Vec<_> = self.buffers.keys().copied().collect();
        if let Some(idx) = ids.iter().position(|&id| id == self.active_buffer) {
            let prev_idx = if idx == 0 { ids.len() - 1 } else { idx - 1 };
            self.active_buffer = ids[prev_idx];
        }
    }

    /// Get the currently active buffer state
    pub fn active_state(&self) -> &EditorState {
        self.buffers.get(&self.active_buffer).unwrap()
    }

    /// Get the currently active buffer state (mutable)
    pub fn active_state_mut(&mut self) -> &mut EditorState {
        self.buffers.get_mut(&self.active_buffer).unwrap()
    }

    /// Get the event log for the active buffer
    pub fn active_event_log(&self) -> &EventLog {
        self.event_logs.get(&self.active_buffer).unwrap()
    }

    /// Get the event log for the active buffer (mutable)
    pub fn active_event_log_mut(&mut self) -> &mut EventLog {
        self.event_logs.get_mut(&self.active_buffer).unwrap()
    }

    /// Copy the current selection to clipboard
    pub fn copy_selection(&mut self) {
        let state = self.active_state();
        let mut text = String::new();

        for (_, cursor) in state.cursors.iter() {
            if let Some(range) = cursor.selection_range() {
                if !text.is_empty() {
                    text.push('\n');
                }
                text.push_str(&state.buffer.slice(range));
            }
        }

        if !text.is_empty() {
            self.clipboard = text;
            self.status_message = Some("Copied".to_string());
        }
    }

    /// Cut the current selection to clipboard
    pub fn cut_selection(&mut self) {
        self.copy_selection();

        // Get deletions from state
        let deletions: Vec<_> = {
            let state = self.active_state();
            state
                .cursors
                .iter()
                .filter_map(|(_, c)| c.selection_range())
                .collect()
        };

        // Get deleted text and cursor id
        let events: Vec<_> = deletions
            .iter()
            .rev()
            .map(|range| {
                let state = self.active_state();
                Event::Delete {
                    range: range.clone(),
                    deleted_text: state.buffer.slice(range.clone()),
                    cursor_id: state.cursors.primary_id(),
                }
            })
            .collect();

        // Apply events
        for event in events {
            self.active_event_log_mut().append(event.clone());
            self.active_state_mut().apply(&event);
        }

        if !deletions.is_empty() {
            self.status_message = Some("Cut".to_string());
        }
    }

    /// Paste the clipboard content
    pub fn paste(&mut self) {
        if self.clipboard.is_empty() {
            return;
        }

        let state = self.active_state();
        let cursor_id = state.cursors.primary_id();
        let position = state.cursors.primary().position;

        let event = Event::Insert {
            position,
            text: self.clipboard.clone(),
            cursor_id,
        };

        self.active_event_log_mut().append(event.clone());
        self.active_state_mut().apply(&event);

        self.status_message = Some("Pasted".to_string());
    }

    /// Add a cursor at the next occurrence of the selected text
    /// If no selection, does nothing
    pub fn add_cursor_at_next_match(&mut self) {
        let state = self.active_state();

        // Get the selected text from the primary cursor
        let primary = state.cursors.primary();
        let selection_range = match primary.selection_range() {
            Some(range) => range,
            None => {
                self.status_message = Some("No selection to match".to_string());
                return;
            }
        };

        // Extract the selected text
        let pattern = state.buffer.slice(selection_range.clone());

        // Find the next occurrence after the current selection
        let search_start = selection_range.end;
        let match_pos = match state.buffer.find_next(&pattern, search_start) {
            Some(pos) => pos,
            None => {
                self.status_message = Some("No more matches".to_string());
                return;
            }
        };

        // Create a new cursor at the match position with selection
        let new_cursor = crate::cursor::Cursor::with_selection(match_pos, match_pos + pattern.len());

        // Add the cursor
        let state_mut = self.active_state_mut();
        state_mut.cursors.add(new_cursor);

        // Normalize cursors to merge overlapping ones
        state_mut.cursors.normalize();

        self.status_message = Some(format!("Added cursor at match ({})", state_mut.cursors.iter().count()));
    }

    /// Add a cursor above the primary cursor at the same column
    pub fn add_cursor_above(&mut self) {
        let state = self.active_state();
        let primary = state.cursors.primary();

        // Find the start of the current line using iterator
        let mut iter = state.buffer.line_iterator(primary.position);
        let Some((line_start, _line_content)) = iter.next() else {
            self.status_message = Some("Unable to find current line".to_string());
            return;
        };

        // Check if we're on the first line
        if line_start == 0 {
            self.status_message = Some("Already at first line".to_string());
            return;
        }

        // Calculate column offset from line start
        let col_offset = primary.position - line_start;

        // After next(), iterator is positioned after current line
        // Call prev() twice: once to get back to current line, once more to get previous line
        iter.prev(); // Move back to current line

        // Get the previous line
        if let Some((prev_line_start, prev_line_content)) = iter.prev() {
            // Calculate new position on previous line, capping at line length
            let prev_line_len = prev_line_content.len();
            let new_pos = prev_line_start + col_offset.min(prev_line_len);

            let new_cursor = crate::cursor::Cursor::new(new_pos);

            let state_mut = self.active_state_mut();
            state_mut.cursors.add(new_cursor);
            state_mut.cursors.normalize();

            self.status_message = Some(format!("Added cursor above ({})", state_mut.cursors.iter().count()));
        } else {
            self.status_message = Some("Already at first line".to_string());
        }
    }

    /// Add a cursor below the primary cursor at the same column
    pub fn add_cursor_below(&mut self) {
        let state = self.active_state();
        let primary = state.cursors.primary();

        // Find the start of the current line using iterator
        let mut iter = state.buffer.line_iterator(primary.position);
        let Some((line_start, _)) = iter.next() else {
            self.status_message = Some("Unable to find current line".to_string());
            return;
        };

        // Calculate column offset from line start
        let col_offset = primary.position - line_start;

        // Get next line (we already consumed current line with first iter.next())
        if let Some((next_line_start, next_line_content)) = iter.next() {
            // Calculate new position on next line, capping at line length
            let next_line_len = next_line_content.len();
            let new_pos = next_line_start + col_offset.min(next_line_len);
            let new_cursor = crate::cursor::Cursor::new(new_pos);

            let state_mut = self.active_state_mut();
            state_mut.cursors.add(new_cursor);
            state_mut.cursors.normalize();

            self.status_message = Some(format!("Added cursor below ({})", state_mut.cursors.iter().count()));
        } else {
            self.status_message = Some("Already at last line".to_string());
        }
    }

    /// Save the active buffer
    pub fn save(&mut self) -> io::Result<()> {
        self.active_state_mut().buffer.save()?;
        self.status_message = Some("Saved".to_string());
        Ok(())
    }

    /// Check if the editor should quit
    pub fn should_quit(&self) -> bool {
        self.should_quit
    }

    /// Request the editor to quit
    pub fn quit(&mut self) {
        // TODO: Check for unsaved buffers
        self.should_quit = true;
    }

    /// Resize all buffers to match new terminal size
    pub fn resize(&mut self, width: u16, height: u16) {
        for state in self.buffers.values_mut() {
            state.resize(width, height);
        }
    }

    // Prompt/Minibuffer control methods

    /// Start a new prompt (enter minibuffer mode)
    pub fn start_prompt(&mut self, message: String, prompt_type: PromptType) {
        self.start_prompt_with_suggestions(message, prompt_type, Vec::new());
    }

    /// Start a new prompt with autocomplete suggestions
    pub fn start_prompt_with_suggestions(&mut self, message: String, prompt_type: PromptType, suggestions: Vec<Suggestion>) {
        let selected_suggestion = if suggestions.is_empty() { None } else { Some(0) };
        self.prompt = Some(Prompt {
            message,
            input: String::new(),
            cursor_pos: 0,
            prompt_type,
            suggestions,
            selected_suggestion,
        });
    }

    /// Cancel the current prompt and return to normal mode
    pub fn cancel_prompt(&mut self) {
        self.prompt = None;
        self.status_message = Some("Canceled".to_string());
    }

    /// Get the confirmed input and prompt type, consuming the prompt
    pub fn confirm_prompt(&mut self) -> Option<(String, PromptType)> {
        if let Some(prompt) = self.prompt.take() {
            Some((prompt.input, prompt.prompt_type))
        } else {
            None
        }
    }

    /// Check if currently in prompt mode
    pub fn is_prompting(&self) -> bool {
        self.prompt.is_some()
    }

    /// Get current prompt input (for display)
    pub fn prompt_input(&self) -> Option<&str> {
        self.prompt.as_ref().map(|p| p.input.as_str())
    }

    /// Get mutable reference to prompt (for input handling)
    pub fn prompt_mut(&mut self) -> Option<&mut Prompt> {
        self.prompt.as_mut()
    }

    /// Set a status message to display in the status bar
    pub fn set_status_message(&mut self, message: String) {
        self.status_message = Some(message);
    }

    /// Get all available commands for the command palette
    pub fn get_all_commands() -> Vec<Command> {
        use crate::keybindings::Action;

        vec![
            Command {
                name: "Open File".to_string(),
                description: "Open a file in a new or existing buffer".to_string(),
                action: Action::Open,
            },
            Command {
                name: "Save File".to_string(),
                description: "Save the current buffer to disk".to_string(),
                action: Action::Save,
            },
            Command {
                name: "Quit".to_string(),
                description: "Exit the editor".to_string(),
                action: Action::Quit,
            },
            Command {
                name: "Show Help".to_string(),
                description: "Display the help page with all keybindings".to_string(),
                action: Action::ShowHelp,
            },
            Command {
                name: "Undo".to_string(),
                description: "Undo the last edit".to_string(),
                action: Action::Undo,
            },
            Command {
                name: "Redo".to_string(),
                description: "Redo the last undone edit".to_string(),
                action: Action::Redo,
            },
            Command {
                name: "Copy".to_string(),
                description: "Copy selection to clipboard".to_string(),
                action: Action::Copy,
            },
            Command {
                name: "Cut".to_string(),
                description: "Cut selection to clipboard".to_string(),
                action: Action::Cut,
            },
            Command {
                name: "Paste".to_string(),
                description: "Paste from clipboard".to_string(),
                action: Action::Paste,
            },
            Command {
                name: "Select All".to_string(),
                description: "Select all text in the buffer".to_string(),
                action: Action::SelectAll,
            },
            Command {
                name: "Select Word".to_string(),
                description: "Select the word under the cursor".to_string(),
                action: Action::SelectWord,
            },
            Command {
                name: "Select Line".to_string(),
                description: "Select the current line".to_string(),
                action: Action::SelectLine,
            },
            Command {
                name: "Expand Selection".to_string(),
                description: "Expand the current selection by one word".to_string(),
                action: Action::ExpandSelection,
            },
            Command {
                name: "Add Cursor Above".to_string(),
                description: "Add a cursor on the line above".to_string(),
                action: Action::AddCursorAbove,
            },
            Command {
                name: "Add Cursor Below".to_string(),
                description: "Add a cursor on the line below".to_string(),
                action: Action::AddCursorBelow,
            },
            Command {
                name: "Add Cursor at Next Match".to_string(),
                description: "Add a cursor at the next occurrence of the selection".to_string(),
                action: Action::AddCursorNextMatch,
            },
            Command {
                name: "Remove Secondary Cursors".to_string(),
                description: "Remove all cursors except the primary".to_string(),
                action: Action::RemoveSecondaryCursors,
            },
        ]
    }

    /// Filter commands by fuzzy matching the query
    pub fn filter_commands(query: &str) -> Vec<Suggestion> {
        let query_lower = query.to_lowercase();
        let commands = Self::get_all_commands();

        if query.is_empty() {
            // Show all commands when no filter
            return commands
                .into_iter()
                .map(|cmd| Suggestion::with_description(cmd.name.clone(), cmd.description))
                .collect();
        }

        // Simple fuzzy matching: check if all characters appear in order
        commands
            .into_iter()
            .filter(|cmd| {
                let name_lower = cmd.name.to_lowercase();
                let mut query_chars = query_lower.chars();
                let mut current_char = query_chars.next();

                for name_char in name_lower.chars() {
                    if let Some(qc) = current_char {
                        if qc == name_char {
                            current_char = query_chars.next();
                        }
                    } else {
                        break;
                    }
                }

                current_char.is_none() // All query characters matched
            })
            .map(|cmd| Suggestion::with_description(cmd.name.clone(), cmd.description))
            .collect()
    }

    /// Update prompt suggestions based on current input
    pub fn update_prompt_suggestions(&mut self) {
        if let Some(prompt) = &mut self.prompt {
            if matches!(prompt.prompt_type, PromptType::Command) {
                prompt.suggestions = Self::filter_commands(&prompt.input);
                prompt.selected_suggestion = if prompt.suggestions.is_empty() {
                    None
                } else {
                    Some(0)
                };
            }
        }
    }

    /// Handle a key event and return whether it was handled
    /// This is the central key handling logic used by both main.rs and tests
    pub fn handle_key(&mut self, code: crossterm::event::KeyCode, modifiers: crossterm::event::KeyModifiers) -> std::io::Result<()> {
        use crossterm::event::{KeyCode, KeyModifiers};
        use crate::keybindings::Action;
        use std::path::Path;

        // Handle help mode first
        if self.is_help_visible() {
            match (code, modifiers) {
                (KeyCode::Esc, KeyModifiers::NONE) | (KeyCode::Char('h'), KeyModifiers::CONTROL) => {
                    self.toggle_help();
                }
                (KeyCode::Up, KeyModifiers::NONE) => self.scroll_help(-1),
                (KeyCode::Down, KeyModifiers::NONE) => self.scroll_help(1),
                (KeyCode::PageUp, KeyModifiers::NONE) => self.scroll_help(-10),
                (KeyCode::PageDown, KeyModifiers::NONE) => self.scroll_help(10),
                _ => {}
            }
            return Ok(());
        }

        // Handle prompt mode
        if self.is_prompting() {
            match (code, modifiers) {
                // Confirm prompt with Enter
                (KeyCode::Enter, KeyModifiers::NONE) => {
                    if let Some((input, prompt_type)) = self.confirm_prompt() {
                        // Handle the confirmed prompt
                        match prompt_type {
                            PromptType::OpenFile => {
                                let path = Path::new(&input);
                                if let Err(e) = self.open_file(path) {
                                    self.set_status_message(format!("Error opening file: {}", e));
                                } else {
                                    self.set_status_message(format!("Opened: {}", input));
                                }
                            }
                            PromptType::SaveFileAs => {
                                self.set_status_message(format!("Save-as not yet implemented: {}", input));
                            }
                            PromptType::Search => {
                                self.set_status_message(format!("Search not yet implemented: {}", input));
                            }
                            PromptType::Replace { search: _ } => {
                                self.set_status_message(format!("Replace not yet implemented: {}", input));
                            }
                            PromptType::Command => {
                                // Find the command by name and execute it
                                let commands = Self::get_all_commands();
                                if let Some(cmd) = commands.iter().find(|c| c.name == input) {
                                    // Execute the action (we'll handle it below after returning from this match)
                                    // For now, trigger the action through the normal action handling
                                    let action = cmd.action.clone();
                                    self.set_status_message(format!("Executing: {}", cmd.name));

                                    // Handle the action immediately
                                    match action {
                                        Action::Quit => self.quit(),
                                        Action::Save => { let _ = self.save(); }
                                        Action::Open => self.start_prompt("Find file: ".to_string(), PromptType::OpenFile),
                                        Action::Copy => self.copy_selection(),
                                        Action::Cut => self.cut_selection(),
                                        Action::Paste => self.paste(),
                                        Action::Undo => {
                                            if let Some(event) = self.active_event_log_mut().undo() {
                                                if let Some(inverse) = event.inverse() {
                                                    self.active_state_mut().apply(&inverse);
                                                }
                                            }
                                        }
                                        Action::Redo => {
                                            let event_opt = self.active_event_log_mut().redo().cloned();
                                            if let Some(event) = event_opt {
                                                self.active_state_mut().apply(&event);
                                            }
                                        }
                                        Action::ShowHelp => self.toggle_help(),
                                        Action::AddCursorNextMatch => self.add_cursor_at_next_match(),
                                        Action::AddCursorAbove => self.add_cursor_above(),
                                        Action::AddCursorBelow => self.add_cursor_below(),
                                        Action::RemoveSecondaryCursors => self.active_state_mut().cursors.remove_secondary(),
                                        Action::SelectAll | Action::SelectWord | Action::SelectLine | Action::ExpandSelection => {
                                            if let Some(events) = self.action_to_events(action) {
                                                for event in events {
                                                    self.active_event_log_mut().append(event.clone());
                                                    self.active_state_mut().apply(&event);
                                                }
                                            }
                                        }
                                        _ => {
                                            if let Some(events) = self.action_to_events(action) {
                                                for event in events {
                                                    self.active_event_log_mut().append(event.clone());
                                                    self.active_state_mut().apply(&event);
                                                }
                                            }
                                        }
                                    }
                                } else {
                                    self.set_status_message(format!("Unknown command: {}", input));
                                }
                            }
                        }
                    }
                    return Ok(());
                }
                // Cancel prompt with Escape
                (KeyCode::Esc, KeyModifiers::NONE) => {
                    self.cancel_prompt();
                    return Ok(());
                }
                // Insert character into prompt
                (KeyCode::Char(c), KeyModifiers::NONE) | (KeyCode::Char(c), KeyModifiers::SHIFT) => {
                    if let Some(prompt) = self.prompt_mut() {
                        prompt.input.insert(prompt.cursor_pos, c);
                        prompt.cursor_pos += c.len_utf8();
                    }
                    // Update suggestions if this is a command palette
                    self.update_prompt_suggestions();
                    return Ok(());
                }
                // Backspace in prompt
                (KeyCode::Backspace, KeyModifiers::NONE) => {
                    if let Some(prompt) = self.prompt_mut() {
                        if prompt.cursor_pos > 0 {
                            let byte_pos = prompt.cursor_pos;
                            let mut char_start = byte_pos - 1;
                            while char_start > 0 && !prompt.input.is_char_boundary(char_start) {
                                char_start -= 1;
                            }
                            prompt.input.remove(char_start);
                            prompt.cursor_pos = char_start;
                        }
                    }
                    // Update suggestions if this is a command palette
                    self.update_prompt_suggestions();
                    return Ok(());
                }
                // Navigate suggestions with Up/Down
                (KeyCode::Up, KeyModifiers::NONE) => {
                    if let Some(prompt) = self.prompt_mut() {
                        if !prompt.suggestions.is_empty() {
                            if let Some(selected) = prompt.selected_suggestion {
                                prompt.selected_suggestion = if selected == 0 {
                                    Some(prompt.suggestions.len() - 1)
                                } else {
                                    Some(selected - 1)
                                };
                            }
                        }
                    }
                    return Ok(());
                }
                (KeyCode::Down, KeyModifiers::NONE) => {
                    if let Some(prompt) = self.prompt_mut() {
                        if !prompt.suggestions.is_empty() {
                            if let Some(selected) = prompt.selected_suggestion {
                                prompt.selected_suggestion = Some((selected + 1) % prompt.suggestions.len());
                            }
                        }
                    }
                    return Ok(());
                }
                // Tab to accept current suggestion
                (KeyCode::Tab, KeyModifiers::NONE) => {
                    if let Some(prompt) = self.prompt_mut() {
                        if let Some(selected) = prompt.selected_suggestion {
                            if let Some(suggestion) = prompt.suggestions.get(selected) {
                                prompt.input = suggestion.get_value().to_string();
                                prompt.cursor_pos = prompt.input.len();
                            }
                        }
                    }
                    return Ok(());
                }
                // Move cursor left in prompt
                (KeyCode::Left, KeyModifiers::NONE) => {
                    if let Some(prompt) = self.prompt_mut() {
                        if prompt.cursor_pos > 0 {
                            let mut new_pos = prompt.cursor_pos - 1;
                            while new_pos > 0 && !prompt.input.is_char_boundary(new_pos) {
                                new_pos -= 1;
                            }
                            prompt.cursor_pos = new_pos;
                        }
                    }
                    return Ok(());
                }
                // Move cursor right in prompt
                (KeyCode::Right, KeyModifiers::NONE) => {
                    if let Some(prompt) = self.prompt_mut() {
                        if prompt.cursor_pos < prompt.input.len() {
                            let mut new_pos = prompt.cursor_pos + 1;
                            while new_pos < prompt.input.len() && !prompt.input.is_char_boundary(new_pos) {
                                new_pos += 1;
                            }
                            prompt.cursor_pos = new_pos;
                        }
                    }
                    return Ok(());
                }
                // Move to start of prompt input
                (KeyCode::Home, KeyModifiers::NONE) => {
                    if let Some(prompt) = self.prompt_mut() {
                        prompt.cursor_pos = 0;
                    }
                    return Ok(());
                }
                // Move to end of prompt input
                (KeyCode::End, KeyModifiers::NONE) => {
                    if let Some(prompt) = self.prompt_mut() {
                        prompt.cursor_pos = prompt.input.len();
                    }
                    return Ok(());
                }
                // Ignore other keys in prompt mode
                _ => return Ok(()),
            }
        }

        // Normal mode: use keybinding resolver to convert key to action
        let key_event = crossterm::event::KeyEvent::new(code, modifiers);
        let action = self.keybindings.resolve(&key_event);

        // Handle special actions
        match action {
            Action::Quit => self.quit(),
            Action::Save => self.save()?,
            Action::Open => self.start_prompt("Find file: ".to_string(), PromptType::OpenFile),
            Action::Copy => self.copy_selection(),
            Action::Cut => self.cut_selection(),
            Action::Paste => self.paste(),
            Action::Undo => {
                if let Some(event) = self.active_event_log_mut().undo() {
                    if let Some(inverse) = event.inverse() {
                        self.active_state_mut().apply(&inverse);
                    }
                }
            }
            Action::Redo => {
                let event_opt = self.active_event_log_mut().redo().cloned();
                if let Some(event) = event_opt {
                    self.active_state_mut().apply(&event);
                }
            }
            Action::ShowHelp => self.toggle_help(),
            Action::CommandPalette => {
                // Start the command palette prompt with all commands as suggestions
                let suggestions = Self::filter_commands("");
                self.start_prompt_with_suggestions(
                    "Command: ".to_string(),
                    PromptType::Command,
                    suggestions,
                );
            }
            Action::AddCursorNextMatch => self.add_cursor_at_next_match(),
            Action::AddCursorAbove => self.add_cursor_above(),
            Action::AddCursorBelow => self.add_cursor_below(),
            Action::RemoveSecondaryCursors => self.active_state_mut().cursors.remove_secondary(),
            Action::None => {}
            _ => {
                // Convert action to events and apply them
                if let Some(events) = self.action_to_events(action) {
                    for event in events {
                        self.active_event_log_mut().append(event.clone());
                        self.active_state_mut().apply(&event);
                    }
                }
            }
        }

        Ok(())
    }

    /// Render the editor to the terminal
    pub fn render(&mut self, frame: &mut Frame) {
        let _span = tracing::trace_span!("render").entered();
        let size = frame.area();
        tracing::debug!("Render frame area: {}x{}", size.width, size.height);

        // If help is visible, render help page instead
        if self.help_visible {
            self.render_help(frame, size);
            return;
        }

        // Check if we need space for suggestions popup
        let suggestion_lines = if let Some(prompt) = &self.prompt {
            if !prompt.suggestions.is_empty() {
                // Show up to 10 suggestions
                prompt.suggestions.len().min(10)
            } else {
                0
            }
        } else {
            0
        };

        // Split into tabs, content, suggestions (if any), and status bar
        let mut constraints = vec![
            Constraint::Length(1), // Tabs
            Constraint::Min(0),    // Content
        ];

        if suggestion_lines > 0 {
            constraints.push(Constraint::Length(suggestion_lines as u16)); // Suggestions popup
        }

        constraints.push(Constraint::Length(1)); // Status bar

        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints(constraints)
            .split(size);

        // Render tabs
        self.render_tabs(frame, chunks[0]);

        // Render content
        self.render_content(frame, chunks[1]);

        // Render suggestions popup if present
        if suggestion_lines > 0 {
            self.render_suggestions(frame, chunks[2]);
            // Status bar is in chunks[3]
            self.render_status_bar(frame, chunks[3]);
        } else {
            // Status bar is in chunks[2]
            self.render_status_bar(frame, chunks[2]);
        }
    }

    /// Render the tab bar
    fn render_tabs(&self, frame: &mut Frame, area: Rect) {
        let titles: Vec<String> = self
            .buffers
            .keys()
            .map(|id| {
                let state = &self.buffers[id];
                let name = state
                    .buffer
                    .file_path()
                    .and_then(|p| p.file_name())
                    .and_then(|n| n.to_str())
                    .unwrap_or("[No Name]");

                let modified = if state.buffer.is_modified() { "*" } else { "" };

                format!(" {name}{modified} ")
            })
            .collect();

        let selected = self
            .buffers
            .keys()
            .position(|id| *id == self.active_buffer)
            .unwrap_or(0);

        let tabs = Tabs::new(titles)
            .select(selected)
            .style(Style::default().fg(Color::White))
            .highlight_style(
                Style::default()
                    .fg(Color::Yellow)
                    .add_modifier(Modifier::BOLD),
            );

        frame.render_widget(tabs, area);
    }

    /// Render the main content area
    fn render_content(&mut self, frame: &mut Frame, area: Rect) {
        let _span = tracing::trace_span!("render_content").entered();
        let state = self.active_state_mut();

        tracing::debug!("Render content area: {}x{}, viewport height: {}", area.width, area.height, state.viewport.height);

        // Calculate gutter width dynamically based on buffer size
        let gutter_width = state.viewport.gutter_width(&state.buffer);
        let line_number_digits = gutter_width.saturating_sub(3); // Subtract " │ "

        let mut lines = Vec::new();

        // Collect all selection ranges from all cursors
        let selection_ranges: Vec<std::ops::Range<usize>> = state
            .cursors
            .iter()
            .filter_map(|(_, cursor)| cursor.selection_range())
            .collect();

        // Collect all cursor positions (to avoid highlighting the cursor itself)
        let cursor_positions: Vec<usize> = state
            .cursors
            .iter()
            .map(|(_, cursor)| cursor.position)
            .collect();

        // Use line iterator starting from top_byte to render visible lines
        let visible_count = state.viewport.visible_line_count();
        let mut iter = state.buffer.line_iterator(state.viewport.top_byte);

        // Get starting line number from viewport's cached value
        // This is maintained incrementally during scrolling to avoid O(n) scanning
        let starting_line_num = state.viewport.top_line_number.value();

        let mut lines_rendered = 0;

        while let Some((line_start, line_content)) = iter.next() {
            if lines_rendered >= visible_count {
                break;
            }

            let current_line_num = starting_line_num + lines_rendered;
            lines_rendered += 1;

            // Apply horizontal scrolling - skip characters before left_column
            let left_col = state.viewport.left_column;

            // Build line with selection highlighting
            let mut line_spans = Vec::new();

            // Line number prefix (1-indexed for display)
            line_spans.push(Span::styled(
                format!("{:>width$} │ ", current_line_num + 1, width = line_number_digits),
                Style::default().fg(Color::DarkGray),
            ));

            // Check if this line has any selected text
            let mut char_index = 0;
            for ch in line_content.chars() {
                let byte_pos = line_start + char_index;

                // Skip characters before left_column
                if char_index >= left_col {
                    // Check if this character is at a cursor position
                    let is_cursor = cursor_positions.contains(&byte_pos);

                    // Check if this character is in any selection range (but not at cursor position)
                    let is_selected = !is_cursor && selection_ranges.iter().any(|range| range.contains(&byte_pos));

                    let style = if is_selected {
                        Style::default().fg(Color::Black).bg(Color::Cyan)
                    } else {
                        Style::default().fg(Color::White)
                    };

                    line_spans.push(Span::styled(ch.to_string(), style));
                }

                char_index += ch.len_utf8();
            }

            lines.push(Line::from(line_spans));
        }

        let paragraph = Paragraph::new(lines)
            .block(Block::default().borders(Borders::NONE));

        frame.render_widget(paragraph, area);

        // Render cursor
        let cursor_positions = state.cursor_positions();
        if let Some(&(x, y)) = cursor_positions.first() {
            // Adjust for line numbers (gutter width is dynamic based on max line number)
            // and adjust Y for the content area offset (area.y accounts for tab bar)
            let screen_x = area.x.saturating_add(x).saturating_add(gutter_width as u16);
            let screen_y = area.y.saturating_add(y);
            frame.set_cursor_position((screen_x, screen_y));

            // Log rendering state for debugging
            let cursor_pos = state.cursors.primary().position;
            let buffer_len = state.buffer.len();
            if let Some(event_log) = self.event_logs.get_mut(&self.active_buffer) {
                event_log.log_render_state(cursor_pos, screen_x, screen_y, buffer_len);
            }
        }
    }

    /// Render the status bar
    fn render_status_bar(&mut self, frame: &mut Frame, area: Rect) {
        // If we're in prompt mode, render the prompt instead of the status bar
        if let Some(prompt) = &self.prompt {
            // Build prompt display: message + input + cursor
            let prompt_text = format!("{}{}", prompt.message, prompt.input);

            // Use a different style for prompt (yellow background to distinguish from status bar)
            let prompt_line = Paragraph::new(prompt_text)
                .style(Style::default().fg(Color::Black).bg(Color::Yellow));

            frame.render_widget(prompt_line, area);

            // Set cursor position in the prompt
            // Cursor should be at: message.len() + cursor_pos
            let cursor_x = (prompt.message.len() + prompt.cursor_pos) as u16;
            if cursor_x < area.width {
                frame.set_cursor_position((area.x + cursor_x, area.y));
            }

            return;
        }

        // Normal status bar rendering
        // Collect all data we need from state
        let (filename, modified, line, col) = {
            let state = self.active_state_mut();

            let filename = state
                .buffer
                .file_path()
                .and_then(|p| p.to_str())
                .map(|s| s.to_string())
                .unwrap_or_else(|| "[No Name]".to_string());

            let modified = if state.buffer.is_modified() {
                " [+]"
            } else {
                ""
            };

            let cursor = *state.primary_cursor();

            // Get line number and column efficiently using cached values
            let (line, col) = {
                // Find the start of the line containing the cursor
                let cursor_iter = state.buffer.line_iterator(cursor.position);
                let line_start = cursor_iter.current_position();
                let col = cursor.position - line_start;

                // Use cached line number from state
                let line_num = state.primary_cursor_line_number.value();
                (line_num, col)
            };

            (filename, modified, line, col)
        };

        let status = if let Some(msg) = &self.status_message {
            format!("{filename}{modified} | Ln {line}, Col {col} | {msg}")
        } else {
            format!("{filename}{modified} | Ln {line}, Col {col}")
        };

        let status_line =
            Paragraph::new(status).style(Style::default().fg(Color::Black).bg(Color::White));

        frame.render_widget(status_line, area);
    }

    /// Render the suggestions popup (autocomplete)
    fn render_suggestions(&self, frame: &mut Frame, area: Rect) {
        let Some(prompt) = &self.prompt else {
            return;
        };

        if prompt.suggestions.is_empty() {
            return;
        }

        let mut lines = Vec::new();
        let visible_count = area.height as usize;
        let start_idx = 0;
        let end_idx = visible_count.min(prompt.suggestions.len());

        for (idx, suggestion) in prompt.suggestions[start_idx..end_idx].iter().enumerate() {
            let actual_idx = start_idx + idx;
            let is_selected = prompt.selected_suggestion == Some(actual_idx);

            // Format: "Command Name - description"
            let text = if let Some(desc) = &suggestion.description {
                format!("  {}  -  {}", suggestion.text, desc)
            } else {
                format!("  {}", suggestion.text)
            };

            let style = if is_selected {
                // Highlight selected suggestion with cyan background
                Style::default().fg(Color::Black).bg(Color::Cyan)
            } else {
                // Normal suggestion with dark gray background
                Style::default().fg(Color::White).bg(Color::DarkGray)
            };

            lines.push(Line::from(Span::styled(text, style)));
        }

        let paragraph = Paragraph::new(lines);
        frame.render_widget(paragraph, area);
    }

    /// Render the help page
    fn render_help(&self, frame: &mut Frame, area: Rect) {
        // Get all keybindings
        let bindings = self.keybindings.get_all_bindings();

        // Calculate visible range based on scroll
        let visible_height = area.height.saturating_sub(4) as usize; // Leave space for header and footer
        let start_idx = self.help_scroll;
        let end_idx = (start_idx + visible_height).min(bindings.len());

        // Build help text
        let mut lines = vec![];

        // Header
        lines.push(Line::from(vec![
            Span::styled(
                " KEYBOARD SHORTCUTS ",
                Style::default()
                    .fg(Color::Black)
                    .bg(Color::Yellow)
                    .add_modifier(Modifier::BOLD),
            ),
        ]));
        lines.push(Line::from(""));

        // Find max key width for alignment
        let max_key_width = bindings
            .iter()
            .map(|(key, _)| key.len())
            .max()
            .unwrap_or(20);

        // Render visible bindings
        for (key, action) in bindings.iter().skip(start_idx).take(end_idx - start_idx) {
            let line_text = format!("  {:<width$}  {}", key, action, width = max_key_width);
            lines.push(Line::from(line_text));
        }

        // Footer
        lines.push(Line::from(""));
        lines.push(Line::from(vec![
            Span::styled(
                format!(
                    " Showing {}-{} of {} | Use Up/Down to scroll | Press Ctrl+H or Esc to close ",
                    start_idx + 1,
                    end_idx,
                    bindings.len()
                ),
                Style::default().fg(Color::Black).bg(Color::White),
            ),
        ]));

        let help = Paragraph::new(lines)
            .block(
                Block::default()
                    .borders(Borders::ALL)
                    .border_style(Style::default().fg(Color::Yellow))
                    .title(" Help ")
                    .title_style(Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD)),
            )
            .wrap(ratatui::widgets::Wrap { trim: true });

        frame.render_widget(help, area);
    }

    /// Toggle help page visibility
    pub fn toggle_help(&mut self) {
        self.help_visible = !self.help_visible;
        self.help_scroll = 0; // Reset scroll when toggling
    }

    /// Check if help page is visible
    pub fn is_help_visible(&self) -> bool {
        self.help_visible
    }

    /// Scroll help page
    pub fn scroll_help(&mut self, delta: isize) {
        let bindings = self.keybindings.get_all_bindings();
        let max_scroll = bindings.len().saturating_sub(1);

        if delta > 0 {
            self.help_scroll = (self.help_scroll + delta as usize).min(max_scroll);
        } else {
            self.help_scroll = self.help_scroll.saturating_sub(delta.abs() as usize);
        }
    }

    /// Helper: Check if a byte is a word character (alphanumeric or underscore)
    fn is_word_char(byte: u8) -> bool {
        byte.is_ascii_alphanumeric() || byte == b'_'
    }

    /// Helper: Find the start of the word at or before the given position
    fn find_word_start(&self, buffer: &crate::buffer::Buffer, pos: usize) -> usize {
        if pos == 0 {
            return 0;
        }

        let buf_len = buffer.len();
        if pos >= buf_len {
            return buf_len;
        }

        // Only read a small window around the position for efficiency
        let start = pos.saturating_sub(1000);
        let end = (pos + 1).min(buf_len);
        let bytes = buffer.slice_bytes(start..end);
        let offset = pos - start;

        let mut new_pos = offset;

        // If we're at a non-word character, scan left to find a word
        if let Some(&b) = bytes.get(new_pos) {
            if !Self::is_word_char(b) && new_pos > 0 {
                new_pos = new_pos.saturating_sub(1);
            }
        }

        // Find start of current word
        while new_pos > 0 {
            if let Some(&prev_byte) = bytes.get(new_pos.saturating_sub(1)) {
                if !Self::is_word_char(prev_byte) {
                    break;
                }
                new_pos = new_pos.saturating_sub(1);
            } else {
                break;
            }
        }

        start + new_pos
    }

    /// Helper: Find the end of the word at or after the given position
    fn find_word_end(&self, buffer: &crate::buffer::Buffer, pos: usize) -> usize {
        let buf_len = buffer.len();
        if pos >= buf_len {
            return buf_len;
        }

        // Only read a small window around the position for efficiency
        let start = pos;
        let end = (pos + 1000).min(buf_len);
        let bytes = buffer.slice_bytes(start..end);

        let mut new_pos = 0;

        // Find end of current word
        while new_pos < bytes.len() {
            if let Some(&byte) = bytes.get(new_pos) {
                if !Self::is_word_char(byte) {
                    break;
                }
                new_pos += 1;
            } else {
                break;
            }
        }

        start + new_pos
    }

    /// Helper: Find the start of the word to the left of the given position
    fn find_word_start_left(&self, buffer: &crate::buffer::Buffer, pos: usize) -> usize {
        if pos == 0 {
            return 0;
        }

        let buf_len = buffer.len();
        let actual_pos = pos.min(buf_len);

        // Only read a small window around the position for efficiency
        let start = actual_pos.saturating_sub(1000);
        let end = actual_pos;
        let bytes = buffer.slice_bytes(start..end);

        let mut new_pos = bytes.len().saturating_sub(1);

        // Skip non-word characters (whitespace and punctuation)
        while new_pos > 0 && bytes.get(new_pos).is_some_and(|&b| !Self::is_word_char(b)) {
            new_pos = new_pos.saturating_sub(1);
        }

        // Find start of word
        while new_pos > 0 {
            let prev_byte = bytes.get(new_pos.saturating_sub(1));
            let curr_byte = bytes.get(new_pos);

            match (prev_byte, curr_byte) {
                (Some(&prev), Some(&curr)) => {
                    if Self::is_word_char(prev) != Self::is_word_char(curr) {
                        break;
                    }
                    new_pos = new_pos.saturating_sub(1);
                }
                _ => break,
            }
        }

        start + new_pos
    }

    /// Helper: Find the start of the word to the right of the given position
    fn find_word_start_right(&self, buffer: &crate::buffer::Buffer, pos: usize) -> usize {
        let buf_len = buffer.len();
        if pos >= buf_len {
            return buf_len;
        }

        // Only read a small window around the position for efficiency
        let start = pos;
        let end = (pos + 1000).min(buf_len);
        let bytes = buffer.slice_bytes(start..end);

        let mut new_pos = 0;

        // Skip current word
        while new_pos < bytes.len() && bytes.get(new_pos).is_some_and(|&b| Self::is_word_char(b)) {
            new_pos += 1;
        }

        // Skip non-word characters (whitespace and punctuation)
        while new_pos < bytes.len() && bytes.get(new_pos).is_some_and(|&b| !Self::is_word_char(b)) {
            new_pos += 1;
        }

        start + new_pos
    }

    /// Convert an action into a list of events to apply to the active buffer
    /// Returns None for actions that don't generate events (like Quit)
    pub fn action_to_events(&self, action: Action) -> Option<Vec<Event>> {
        let state = self.active_state();
        let mut events = Vec::new();

        match action {
            // Character input - insert at each cursor
            Action::InsertChar(ch) => {
                for (cursor_id, cursor) in state.cursors.iter() {
                    // If there's a selection, delete it first
                    if let Some(range) = cursor.selection_range() {
                        events.push(Event::Delete {
                            range: range.clone(),
                            deleted_text: state.buffer.slice(range),
                            cursor_id,
                        });
                    }

                    // Insert the character
                    events.push(Event::Insert {
                        position: cursor.position,
                        text: ch.to_string(),
                        cursor_id,
                    });
                }
            }

            Action::InsertNewline => {
                for (cursor_id, cursor) in state.cursors.iter() {
                    if let Some(range) = cursor.selection_range() {
                        events.push(Event::Delete {
                            range: range.clone(),
                            deleted_text: state.buffer.slice(range),
                            cursor_id,
                        });
                    }

                    events.push(Event::Insert {
                        position: cursor.position,
                        text: "\n".to_string(),
                        cursor_id,
                    });
                }
            }

            Action::InsertTab => {
                let tab_str = " ".repeat(self.config.editor.tab_size);
                for (cursor_id, cursor) in state.cursors.iter() {
                    if let Some(range) = cursor.selection_range() {
                        events.push(Event::Delete {
                            range: range.clone(),
                            deleted_text: state.buffer.slice(range),
                            cursor_id,
                        });
                    }

                    events.push(Event::Insert {
                        position: cursor.position,
                        text: tab_str.clone(),
                        cursor_id,
                    });
                }
            }

            // Basic movement - move each cursor
            Action::MoveLeft => {
                for (cursor_id, cursor) in state.cursors.iter() {
                    let new_pos = cursor.position.saturating_sub(1);
                    events.push(Event::MoveCursor {
                        cursor_id,
                        position: new_pos,
                        anchor: None, // No selection
                    });
                }
            }

            Action::MoveRight => {
                for (cursor_id, cursor) in state.cursors.iter() {
                    let new_pos = (cursor.position + 1).min(state.buffer.len());
                    events.push(Event::MoveCursor {
                        cursor_id,
                        position: new_pos,
                        anchor: None,
                    });
                }
            }

            Action::MoveUp => {
                for (cursor_id, cursor) in state.cursors.iter() {
                    // Use iterator to navigate to previous line
                    // line_iterator positions us at the start of the current line
                    let mut iter = state.buffer.line_iterator(cursor.position);
                    let current_line_start = iter.current_position();
                    let col_offset = cursor.position - current_line_start;

                    // Get previous line
                    if let Some((prev_line_start, prev_line_content)) = iter.prev() {
                        // Calculate length without trailing newline
                        let prev_line_len = prev_line_content.trim_end_matches('\n').len();
                        let new_pos = prev_line_start + col_offset.min(prev_line_len);

                        events.push(Event::MoveCursor {
                            cursor_id,
                            position: new_pos,
                            anchor: None,
                        });
                    }
                }
            }

            Action::MoveDown => {
                for (cursor_id, cursor) in state.cursors.iter() {
                    // Use iterator to navigate to next line
                    // line_iterator positions us at the start of the current line
                    let mut iter = state.buffer.line_iterator(cursor.position);
                    let current_line_start = iter.current_position();
                    let col_offset = cursor.position - current_line_start;

                    // Skip current line
                    iter.next();
                    // Get next line
                    if let Some((next_line_start, next_line_content)) = iter.next() {
                        // Calculate length without trailing newline
                        let next_line_len = next_line_content.trim_end_matches('\n').len();
                        let new_pos = next_line_start + col_offset.min(next_line_len);

                        events.push(Event::MoveCursor {
                            cursor_id,
                            position: new_pos,
                            anchor: None,
                        });
                    }
                }
            }

            Action::MoveLineStart => {
                for (cursor_id, cursor) in state.cursors.iter() {
                    // Use iterator to find line start
                    let mut iter = state.buffer.line_iterator(cursor.position);
                    if let Some((line_start, _)) = iter.next() {
                        events.push(Event::MoveCursor {
                            cursor_id,
                            position: line_start,
                            anchor: None,
                        });
                    }
                }
            }

            Action::MoveLineEnd => {
                for (cursor_id, cursor) in state.cursors.iter() {
                    // Use iterator to find line end
                    let mut iter = state.buffer.line_iterator(cursor.position);
                    if let Some((line_start, line_content)) = iter.next() {
                        // Calculate end position (exclude newline)
                        let line_len = line_content.trim_end_matches('\n').len();
                        let line_end = line_start + line_len;

                        events.push(Event::MoveCursor {
                            cursor_id,
                            position: line_end,
                            anchor: None,
                        });
                    }
                }
            }

            // Delete actions
            Action::DeleteBackward => {
                for (cursor_id, cursor) in state.cursors.iter() {
                    if let Some(range) = cursor.selection_range() {
                        // If there's a selection, delete it
                        events.push(Event::Delete {
                            range: range.clone(),
                            deleted_text: state.buffer.slice(range),
                            cursor_id,
                        });
                    } else if cursor.position > 0 {
                        // Delete the character before the cursor
                        let range = (cursor.position - 1)..cursor.position;
                        events.push(Event::Delete {
                            range: range.clone(),
                            deleted_text: state.buffer.slice(range),
                            cursor_id,
                        });
                    }
                }
            }

            Action::DeleteForward => {
                for (cursor_id, cursor) in state.cursors.iter() {
                    if let Some(range) = cursor.selection_range() {
                        // If there's a selection, delete it
                        events.push(Event::Delete {
                            range: range.clone(),
                            deleted_text: state.buffer.slice(range),
                            cursor_id,
                        });
                    } else if cursor.position < state.buffer.len() {
                        // Delete the character after the cursor
                        let range = cursor.position..(cursor.position + 1);
                        events.push(Event::Delete {
                            range: range.clone(),
                            deleted_text: state.buffer.slice(range),
                            cursor_id,
                        });
                    }
                }
            }

            Action::DeleteLine => {
                for (cursor_id, cursor) in state.cursors.iter() {
                    // Use iterator to get the current line
                    let mut iter = state.buffer.line_iterator(cursor.position);
                    if let Some((line_start, line_content)) = iter.next() {
                        // line_content includes newline if present
                        let line_end = line_start + line_content.len();

                        if line_start < line_end {
                            let range = line_start..line_end;
                            events.push(Event::Delete {
                                range: range.clone(),
                                deleted_text: state.buffer.slice(range),
                                cursor_id,
                            });
                        }
                    }
                }
            }

            // Selection actions - extend selection while moving
            Action::SelectLeft => {
                for (cursor_id, cursor) in state.cursors.iter() {
                    let anchor = cursor.anchor.unwrap_or(cursor.position);
                    let new_pos = cursor.position.saturating_sub(1);
                    events.push(Event::MoveCursor {
                        cursor_id,
                        position: new_pos,
                        anchor: Some(anchor),
                    });
                }
            }

            Action::SelectRight => {
                for (cursor_id, cursor) in state.cursors.iter() {
                    let anchor = cursor.anchor.unwrap_or(cursor.position);
                    let new_pos = (cursor.position + 1).min(state.buffer.len());
                    events.push(Event::MoveCursor {
                        cursor_id,
                        position: new_pos,
                        anchor: Some(anchor),
                    });
                }
            }

            Action::SelectUp => {
                for (cursor_id, cursor) in state.cursors.iter() {
                    let anchor = cursor.anchor.unwrap_or(cursor.position);

                    // Use iterator to navigate to previous line
                    let mut iter = state.buffer.line_iterator(cursor.position);
                    if let Some((current_line_start, _)) = iter.next() {
                        let col_offset = cursor.position - current_line_start;

                        // Get previous line
                        if let Some((prev_line_start, prev_line_content)) = iter.prev() {
                            // Calculate length without trailing newline
                            let prev_line_len = prev_line_content.trim_end_matches('\n').len();
                            let new_pos = prev_line_start + col_offset.min(prev_line_len);

                            events.push(Event::MoveCursor {
                                cursor_id,
                                position: new_pos,
                                anchor: Some(anchor),
                            });
                        }
                    }
                }
            }

            Action::SelectDown => {
                for (cursor_id, cursor) in state.cursors.iter() {
                    let anchor = cursor.anchor.unwrap_or(cursor.position);

                    // Use iterator to navigate to next line
                    let mut iter = state.buffer.line_iterator(cursor.position);
                    if let Some((current_line_start, _)) = iter.next() {
                        let col_offset = cursor.position - current_line_start;

                        // Get next line (we already consumed current line)
                        if let Some((next_line_start, next_line_content)) = iter.next() {
                            // Calculate length without trailing newline
                            let next_line_len = next_line_content.trim_end_matches('\n').len();
                            let new_pos = next_line_start + col_offset.min(next_line_len);

                            events.push(Event::MoveCursor {
                                cursor_id,
                                position: new_pos,
                                anchor: Some(anchor),
                            });
                        }
                    }
                }
            }

            Action::SelectLineStart => {
                for (cursor_id, cursor) in state.cursors.iter() {
                    let anchor = cursor.anchor.unwrap_or(cursor.position);

                    // Use iterator to find line start
                    let mut iter = state.buffer.line_iterator(cursor.position);
                    if let Some((line_start, _)) = iter.next() {
                        events.push(Event::MoveCursor {
                            cursor_id,
                            position: line_start,
                            anchor: Some(anchor),
                        });
                    }
                }
            }

            Action::SelectLineEnd => {
                for (cursor_id, cursor) in state.cursors.iter() {
                    let anchor = cursor.anchor.unwrap_or(cursor.position);

                    // Use iterator to find line end
                    let mut iter = state.buffer.line_iterator(cursor.position);
                    if let Some((line_start, line_content)) = iter.next() {
                        // Calculate end position (exclude newline)
                        let line_len = line_content.trim_end_matches('\n').len();
                        let line_end = line_start + line_len;

                        events.push(Event::MoveCursor {
                            cursor_id,
                            position: line_end,
                            anchor: Some(anchor),
                        });
                    }
                }
            }

            Action::SelectDocumentStart => {
                for (cursor_id, cursor) in state.cursors.iter() {
                    let anchor = cursor.anchor.unwrap_or(cursor.position);
                    events.push(Event::MoveCursor {
                        cursor_id,
                        position: 0,
                        anchor: Some(anchor),
                    });
                }
            }

            Action::SelectDocumentEnd => {
                for (cursor_id, cursor) in state.cursors.iter() {
                    let anchor = cursor.anchor.unwrap_or(cursor.position);
                    events.push(Event::MoveCursor {
                        cursor_id,
                        position: state.buffer.len(),
                        anchor: Some(anchor),
                    });
                }
            }

            Action::SelectPageUp => {
                let lines_per_page = state.viewport.height as usize;
                for (cursor_id, cursor) in state.cursors.iter() {
                    let anchor = cursor.anchor.unwrap_or(cursor.position);

                    // Use iterator to move up by lines_per_page lines
                    let mut iter = state.buffer.line_iterator(cursor.position);
                    let mut new_pos = cursor.position;

                    for _ in 0..lines_per_page {
                        if let Some((line_start, _)) = iter.prev() {
                            new_pos = line_start;
                        } else {
                            break; // Hit beginning of file
                        }
                    }

                    events.push(Event::MoveCursor {
                        cursor_id,
                        position: new_pos,
                        anchor: Some(anchor),
                    });
                }
            }

            Action::SelectPageDown => {
                let lines_per_page = state.viewport.height as usize;
                for (cursor_id, cursor) in state.cursors.iter() {
                    let anchor = cursor.anchor.unwrap_or(cursor.position);

                    // Use iterator to move down by lines_per_page lines
                    let mut iter = state.buffer.line_iterator(cursor.position);
                    let mut new_pos = cursor.position;

                    for _ in 0..lines_per_page {
                        if let Some((line_start, _)) = iter.next() {
                            new_pos = line_start;
                        } else {
                            break; // Hit end of file
                        }
                    }

                    // Clamp to EOF
                    new_pos = new_pos.min(state.buffer.len());

                    events.push(Event::MoveCursor {
                        cursor_id,
                        position: new_pos,
                        anchor: Some(anchor),
                    });
                }
            }

            Action::SelectAll => {
                // Select entire buffer for primary cursor
                let primary = state.cursors.primary_id();
                events.push(Event::MoveCursor {
                    cursor_id: primary,
                    position: state.buffer.len(),
                    anchor: Some(0),
                });
            }

            Action::SelectWord => {
                // Select the word under each cursor
                for (cursor_id, cursor) in state.cursors.iter() {
                    let word_start = self.find_word_start(&state.buffer, cursor.position);
                    let word_end = self.find_word_end(&state.buffer, cursor.position);

                    // Move cursor to word end with anchor at word start
                    events.push(Event::MoveCursor {
                        cursor_id,
                        position: word_end,
                        anchor: Some(word_start),
                    });
                }
            }

            Action::SelectLine => {
                // Select the entire line for each cursor
                for (cursor_id, cursor) in state.cursors.iter() {
                    // Use iterator to get line bounds
                    let mut iter = state.buffer.line_iterator(cursor.position);
                    if let Some((line_start, line_content)) = iter.next() {
                        // Include newline if present
                        let line_end = line_start + line_content.len();

                        events.push(Event::MoveCursor {
                            cursor_id,
                            position: line_end,
                            anchor: Some(line_start),
                        });
                    }
                }
            }

            Action::ExpandSelection => {
                // Expand selection for each cursor
                for (cursor_id, cursor) in state.cursors.iter() {
                    if let Some(anchor) = cursor.anchor {
                        // Already have a selection - expand by one word to the right
                        // First move to the start of the next word, then to its end
                        let next_word_start = self.find_word_start_right(&state.buffer, cursor.position);
                        let new_end = self.find_word_end(&state.buffer, next_word_start);
                        events.push(Event::MoveCursor {
                            cursor_id,
                            position: new_end,
                            anchor: Some(anchor),
                        });
                    } else {
                        // No selection - select from cursor to end of current word
                        let word_start = self.find_word_start(&state.buffer, cursor.position);
                        let word_end = self.find_word_end(&state.buffer, cursor.position);

                        // If cursor is on non-word char OR at the end of a word,
                        // select from current position to end of next word
                        let (final_start, final_end) = if word_start == word_end || cursor.position == word_end {
                            // Find the next word (skip non-word characters to find it)
                            let next_start = self.find_word_start_right(&state.buffer, cursor.position);
                            let next_end = self.find_word_end(&state.buffer, next_start);
                            // Select FROM cursor position TO the end of next word
                            (cursor.position, next_end)
                        } else {
                            // On a word char - select from cursor to end of current word
                            (cursor.position, word_end)
                        };

                        events.push(Event::MoveCursor {
                            cursor_id,
                            position: final_end,
                            anchor: Some(final_start),
                        });
                    }
                }
            }

            // Document navigation
            Action::MoveDocumentStart => {
                for (cursor_id, _) in state.cursors.iter() {
                    events.push(Event::MoveCursor {
                        cursor_id,
                        position: 0,
                        anchor: None,
                    });
                }
            }

            Action::MoveDocumentEnd => {
                for (cursor_id, _) in state.cursors.iter() {
                    events.push(Event::MoveCursor {
                        cursor_id,
                        position: state.buffer.len(),
                        anchor: None,
                    });
                }
            }

            // Word movement
            Action::MoveWordLeft => {
                for (cursor_id, cursor) in state.cursors.iter() {
                    let new_pos = self.find_word_start_left(&state.buffer, cursor.position);
                    events.push(Event::MoveCursor {
                        cursor_id,
                        position: new_pos,
                        anchor: None,
                    });
                }
            }

            Action::MoveWordRight => {
                for (cursor_id, cursor) in state.cursors.iter() {
                    let new_pos = self.find_word_start_right(&state.buffer, cursor.position);
                    events.push(Event::MoveCursor {
                        cursor_id,
                        position: new_pos,
                        anchor: None,
                    });
                }
            }

            // Word selection
            Action::SelectWordLeft => {
                for (cursor_id, cursor) in state.cursors.iter() {
                    let anchor = cursor.anchor.unwrap_or(cursor.position);
                    let new_pos = self.find_word_start_left(&state.buffer, cursor.position);
                    events.push(Event::MoveCursor {
                        cursor_id,
                        position: new_pos,
                        anchor: Some(anchor),
                    });
                }
            }

            Action::SelectWordRight => {
                for (cursor_id, cursor) in state.cursors.iter() {
                    let anchor = cursor.anchor.unwrap_or(cursor.position);
                    let new_pos = self.find_word_start_right(&state.buffer, cursor.position);
                    events.push(Event::MoveCursor {
                        cursor_id,
                        position: new_pos,
                        anchor: Some(anchor),
                    });
                }
            }

            // Word deletion
            Action::DeleteWordBackward => {
                for (cursor_id, cursor) in state.cursors.iter() {
                    if let Some(range) = cursor.selection_range() {
                        // Delete selection
                        events.push(Event::Delete {
                            range: range.clone(),
                            deleted_text: state.buffer.slice(range),
                            cursor_id,
                        });
                    } else {
                        // Delete word to the left
                        let word_start = self.find_word_start_left(&state.buffer, cursor.position);
                        if word_start < cursor.position {
                            let range = word_start..cursor.position;
                            events.push(Event::Delete {
                                range: range.clone(),
                                deleted_text: state.buffer.slice(range),
                                cursor_id,
                            });
                        }
                    }
                }
            }

            Action::DeleteWordForward => {
                for (cursor_id, cursor) in state.cursors.iter() {
                    if let Some(range) = cursor.selection_range() {
                        // Delete selection
                        events.push(Event::Delete {
                            range: range.clone(),
                            deleted_text: state.buffer.slice(range),
                            cursor_id,
                        });
                    } else {
                        // Delete word to the right
                        let word_end = self.find_word_start_right(&state.buffer, cursor.position);
                        if cursor.position < word_end {
                            let range = cursor.position..word_end;
                            events.push(Event::Delete {
                                range: range.clone(),
                                deleted_text: state.buffer.slice(range),
                                cursor_id,
                            });
                        }
                    }
                }
            }

            // Page navigation
            Action::MovePageUp => {
                let lines_per_page = state.viewport.height as usize;
                for (cursor_id, cursor) in state.cursors.iter() {
                    // Find current line start and calculate column offset
                    let mut iter = state.buffer.line_iterator(cursor.position);
                    let Some((current_line_start, _)) = iter.next() else {
                        continue;
                    };
                    let col_offset = cursor.position - current_line_start;

                    // Go backwards by lines_per_page
                    let mut target_line_start = current_line_start;

                    for _ in 0..lines_per_page {
                        if let Some((line_byte, _)) = iter.prev() {
                            target_line_start = line_byte;
                        } else {
                            // Hit beginning of file
                            target_line_start = 0;
                            break;
                        }
                    }

                    // Calculate target line length to clamp column offset
                    let mut target_iter = state.buffer.line_iterator(target_line_start);
                    let target_line_len = if let Some((_, line_content)) = target_iter.next() {
                        line_content.trim_end_matches('\n').len()
                    } else {
                        0
                    };

                    let new_pos = target_line_start + col_offset.min(target_line_len);
                    events.push(Event::MoveCursor {
                        cursor_id,
                        position: new_pos,
                        anchor: None,
                    });
                }
            }

            Action::MovePageDown => {
                let lines_per_page = state.viewport.height as usize;
                for (cursor_id, cursor) in state.cursors.iter() {
                    // Find current line start and calculate column offset
                    let mut iter = state.buffer.line_iterator(cursor.position);
                    let Some((current_line_start, _)) = iter.next() else {
                        continue;
                    };
                    let col_offset = cursor.position - current_line_start;

                    // Go forward by lines_per_page (we already consumed current line)
                    let mut target_line_start = current_line_start;
                    for _ in 0..lines_per_page {
                        if let Some((line_byte, _)) = iter.next() {
                            target_line_start = line_byte;
                        } else {
                            // Hit end of file - go to EOF
                            target_line_start = state.buffer.len();
                            break;
                        }
                    }

                    // Clamp to EOF
                    if target_line_start >= state.buffer.len() {
                        events.push(Event::MoveCursor {
                            cursor_id,
                            position: state.buffer.len(),
                            anchor: None,
                        });
                        continue;
                    }

                    // Calculate target line length to clamp column offset
                    let mut target_iter = state.buffer.line_iterator(target_line_start);
                    let target_line_len = if let Some((_, line_content)) = target_iter.next() {
                        line_content.trim_end_matches('\n').len()
                    } else {
                        0
                    };

                    let new_pos = target_line_start + col_offset.min(target_line_len);
                    events.push(Event::MoveCursor {
                        cursor_id,
                        position: new_pos,
                        anchor: None,
                    });
                }
            }

            // Multi-cursor operations
            Action::RemoveSecondaryCursors => {
                // Remove all cursors except the primary
                for (cursor_id, _) in state.cursors.iter() {
                    if cursor_id != state.cursors.primary_id() {
                        events.push(Event::RemoveCursor { cursor_id });
                    }
                }
            }

            // Scroll operations
            Action::ScrollUp => {
                events.push(Event::Scroll { line_offset: -1 });
            }

            Action::ScrollDown => {
                events.push(Event::Scroll { line_offset: 1 });
            }

            // No-op action
            Action::None => {
                return None;
            }

            // Actions that don't generate events - handled by main event loop
            Action::Copy
            | Action::Cut
            | Action::Paste
            | Action::AddCursorAbove
            | Action::AddCursorBelow
            | Action::AddCursorNextMatch
            | Action::Save
            | Action::SaveAs
            | Action::Open
            | Action::New
            | Action::Close
            | Action::Quit
            | Action::Undo
            | Action::Redo
            | Action::ShowHelp
            | Action::CommandPalette => {
                // These actions need special handling in the event loop:
                // - Clipboard operations need system clipboard access
                // - File operations need Editor-level state changes
                // - Undo/Redo need EventLog manipulation
                // - Multi-cursor add operations need visual line calculations
                // - ShowHelp toggles help view
                // - CommandPalette opens the command palette prompt
                return None;
            }
        }

        if events.is_empty() {
            None
        } else {
            Some(events)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_editor_new() {
        let config = Config::default();
        let editor = Editor::new(config, 80, 24).unwrap();

        assert_eq!(editor.buffers.len(), 1);
        assert!(!editor.should_quit());
    }

    #[test]
    fn test_new_buffer() {
        let config = Config::default();
        let mut editor = Editor::new(config, 80, 24).unwrap();

        let id = editor.new_buffer();
        assert_eq!(editor.buffers.len(), 2);
        assert_eq!(editor.active_buffer, id);
    }

    #[test]
    fn test_clipboard() {
        let config = Config::default();
        let mut editor = Editor::new(config, 80, 24).unwrap();

        // Manually set clipboard
        editor.clipboard = "test".to_string();

        // Paste should work
        editor.paste();

        let content = editor.active_state().buffer.to_string();
        assert_eq!(content, "test");
    }

    #[test]
    fn test_action_to_events_insert_char() {
        let config = Config::default();
        let editor = Editor::new(config, 80, 24).unwrap();

        let events = editor.action_to_events(Action::InsertChar('a'));
        assert!(events.is_some());

        let events = events.unwrap();
        assert_eq!(events.len(), 1);

        match &events[0] {
            Event::Insert { position, text, .. } => {
                assert_eq!(*position, 0);
                assert_eq!(text, "a");
            }
            _ => panic!("Expected Insert event"),
        }
    }

    #[test]
    fn test_action_to_events_move_right() {
        let config = Config::default();
        let mut editor = Editor::new(config, 80, 24).unwrap();

        // Insert some text first
        let state = editor.active_state_mut();
        state.apply(&Event::Insert {
            position: 0,
            text: "hello".to_string(),
            cursor_id: state.cursors.primary_id(),
        });

        let events = editor.action_to_events(Action::MoveRight);
        assert!(events.is_some());

        let events = events.unwrap();
        assert_eq!(events.len(), 1);

        match &events[0] {
            Event::MoveCursor {
                position, anchor, ..
            } => {
                // Cursor was at 5 (end of "hello"), stays at 5 (can't move beyond end)
                assert_eq!(*position, 5);
                assert_eq!(*anchor, None); // No selection
            }
            _ => panic!("Expected MoveCursor event"),
        }
    }

    #[test]
    fn test_action_to_events_move_up_down() {
        let config = Config::default();
        let mut editor = Editor::new(config, 80, 24).unwrap();

        // Insert multi-line text
        let state = editor.active_state_mut();
        state.apply(&Event::Insert {
            position: 0,
            text: "line1\nline2\nline3".to_string(),
            cursor_id: state.cursors.primary_id(),
        });

        // Move cursor to start of line 2
        state.apply(&Event::MoveCursor {
            cursor_id: state.cursors.primary_id(),
            position: 6, // Start of "line2"
            anchor: None,
        });

        // Test move up
        let events = editor.action_to_events(Action::MoveUp);
        assert!(events.is_some());
        let events = events.unwrap();
        assert_eq!(events.len(), 1);

        match &events[0] {
            Event::MoveCursor { position, .. } => {
                assert_eq!(*position, 0); // Should be at start of line 1
            }
            _ => panic!("Expected MoveCursor event"),
        }
    }

    #[test]
    fn test_action_to_events_insert_newline() {
        let config = Config::default();
        let editor = Editor::new(config, 80, 24).unwrap();

        let events = editor.action_to_events(Action::InsertNewline);
        assert!(events.is_some());

        let events = events.unwrap();
        assert_eq!(events.len(), 1);

        match &events[0] {
            Event::Insert { text, .. } => {
                assert_eq!(text, "\n");
            }
            _ => panic!("Expected Insert event"),
        }
    }

    #[test]
    fn test_action_to_events_unimplemented() {
        let config = Config::default();
        let editor = Editor::new(config, 80, 24).unwrap();

        // These actions should return None (not yet implemented)
        assert!(editor.action_to_events(Action::Save).is_none());
        assert!(editor.action_to_events(Action::Quit).is_none());
        assert!(editor.action_to_events(Action::Undo).is_none());
    }

    #[test]
    fn test_action_to_events_delete_backward() {
        let config = Config::default();
        let mut editor = Editor::new(config, 80, 24).unwrap();

        // Insert some text first
        let state = editor.active_state_mut();
        state.apply(&Event::Insert {
            position: 0,
            text: "hello".to_string(),
            cursor_id: state.cursors.primary_id(),
        });

        let events = editor.action_to_events(Action::DeleteBackward);
        assert!(events.is_some());

        let events = events.unwrap();
        assert_eq!(events.len(), 1);

        match &events[0] {
            Event::Delete {
                range,
                deleted_text,
                ..
            } => {
                assert_eq!(range.clone(), 4..5); // Delete 'o'
                assert_eq!(deleted_text, "o");
            }
            _ => panic!("Expected Delete event"),
        }
    }

    #[test]
    fn test_action_to_events_delete_forward() {
        let config = Config::default();
        let mut editor = Editor::new(config, 80, 24).unwrap();

        // Insert some text first
        let state = editor.active_state_mut();
        state.apply(&Event::Insert {
            position: 0,
            text: "hello".to_string(),
            cursor_id: state.cursors.primary_id(),
        });

        // Move cursor to position 0
        state.apply(&Event::MoveCursor {
            cursor_id: state.cursors.primary_id(),
            position: 0,
            anchor: None,
        });

        let events = editor.action_to_events(Action::DeleteForward);
        assert!(events.is_some());

        let events = events.unwrap();
        assert_eq!(events.len(), 1);

        match &events[0] {
            Event::Delete {
                range,
                deleted_text,
                ..
            } => {
                assert_eq!(range.clone(), 0..1); // Delete 'h'
                assert_eq!(deleted_text, "h");
            }
            _ => panic!("Expected Delete event"),
        }
    }

    #[test]
    fn test_action_to_events_select_right() {
        let config = Config::default();
        let mut editor = Editor::new(config, 80, 24).unwrap();

        // Insert some text first
        let state = editor.active_state_mut();
        state.apply(&Event::Insert {
            position: 0,
            text: "hello".to_string(),
            cursor_id: state.cursors.primary_id(),
        });

        // Move cursor to position 0
        state.apply(&Event::MoveCursor {
            cursor_id: state.cursors.primary_id(),
            position: 0,
            anchor: None,
        });

        let events = editor.action_to_events(Action::SelectRight);
        assert!(events.is_some());

        let events = events.unwrap();
        assert_eq!(events.len(), 1);

        match &events[0] {
            Event::MoveCursor {
                position, anchor, ..
            } => {
                assert_eq!(*position, 1); // Moved to position 1
                assert_eq!(*anchor, Some(0)); // Anchor at start
            }
            _ => panic!("Expected MoveCursor event"),
        }
    }

    #[test]
    fn test_action_to_events_select_all() {
        let config = Config::default();
        let mut editor = Editor::new(config, 80, 24).unwrap();

        // Insert some text first
        let state = editor.active_state_mut();
        state.apply(&Event::Insert {
            position: 0,
            text: "hello world".to_string(),
            cursor_id: state.cursors.primary_id(),
        });

        let events = editor.action_to_events(Action::SelectAll);
        assert!(events.is_some());

        let events = events.unwrap();
        assert_eq!(events.len(), 1);

        match &events[0] {
            Event::MoveCursor {
                position, anchor, ..
            } => {
                assert_eq!(*position, 11); // At end of buffer
                assert_eq!(*anchor, Some(0)); // Anchor at start
            }
            _ => panic!("Expected MoveCursor event"),
        }
    }

    #[test]
    fn test_action_to_events_document_nav() {
        let config = Config::default();
        let mut editor = Editor::new(config, 80, 24).unwrap();

        // Insert multi-line text
        let state = editor.active_state_mut();
        state.apply(&Event::Insert {
            position: 0,
            text: "line1\nline2\nline3".to_string(),
            cursor_id: state.cursors.primary_id(),
        });

        // Test MoveDocumentStart
        let events = editor.action_to_events(Action::MoveDocumentStart);
        assert!(events.is_some());
        let events = events.unwrap();
        match &events[0] {
            Event::MoveCursor { position, .. } => {
                assert_eq!(*position, 0);
            }
            _ => panic!("Expected MoveCursor event"),
        }

        // Test MoveDocumentEnd
        let events = editor.action_to_events(Action::MoveDocumentEnd);
        assert!(events.is_some());
        let events = events.unwrap();
        match &events[0] {
            Event::MoveCursor { position, .. } => {
                assert_eq!(*position, 17); // End of buffer
            }
            _ => panic!("Expected MoveCursor event"),
        }
    }

    #[test]
    fn test_action_to_events_remove_secondary_cursors() {
        use crate::event::CursorId;

        let config = Config::default();
        let mut editor = Editor::new(config, 80, 24).unwrap();

        // Insert some text first to have positions to place cursors
        {
            let state = editor.active_state_mut();
            state.apply(&Event::Insert {
                position: 0,
                text: "hello world test".to_string(),
                cursor_id: state.cursors.primary_id(),
            });
        }

        // Add secondary cursors at different positions to avoid normalization merging
        {
            let state = editor.active_state_mut();
            state.apply(&Event::AddCursor {
                cursor_id: CursorId(1),
                position: 5,
                anchor: None,
            });
            state.apply(&Event::AddCursor {
                cursor_id: CursorId(2),
                position: 10,
                anchor: None,
            });

            assert_eq!(state.cursors.count(), 3);
        }

        // Save primary ID before calling action_to_events
        let primary_id = editor.active_state().cursors.primary_id();

        // RemoveSecondaryCursors should generate RemoveCursor events
        let events = editor.action_to_events(Action::RemoveSecondaryCursors);
        assert!(events.is_some());

        let events = events.unwrap();
        // Should have events for the two secondary cursors
        assert_eq!(events.len(), 2);

        for event in &events {
            match event {
                Event::RemoveCursor { cursor_id } => {
                    // Should not be the primary cursor
                    assert_ne!(*cursor_id, primary_id);
                }
                _ => panic!("Expected RemoveCursor event"),
            }
        }
    }

    #[test]
    fn test_action_to_events_scroll() {
        let config = Config::default();
        let editor = Editor::new(config, 80, 24).unwrap();

        // Test ScrollUp
        let events = editor.action_to_events(Action::ScrollUp);
        assert!(events.is_some());
        let events = events.unwrap();
        assert_eq!(events.len(), 1);
        match &events[0] {
            Event::Scroll { line_offset } => {
                assert_eq!(*line_offset, -1);
            }
            _ => panic!("Expected Scroll event"),
        }

        // Test ScrollDown
        let events = editor.action_to_events(Action::ScrollDown);
        assert!(events.is_some());
        let events = events.unwrap();
        assert_eq!(events.len(), 1);
        match &events[0] {
            Event::Scroll { line_offset } => {
                assert_eq!(*line_offset, 1);
            }
            _ => panic!("Expected Scroll event"),
        }
    }

    #[test]
    fn test_action_to_events_none() {
        let config = Config::default();
        let editor = Editor::new(config, 80, 24).unwrap();

        // None action should return None
        let events = editor.action_to_events(Action::None);
        assert!(events.is_none());
    }
}
