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
}

impl Editor {
    /// Create a new editor with the given configuration and terminal dimensions
    pub fn new(config: Config, width: u16, height: u16) -> io::Result<Self> {
        let keybindings = KeybindingResolver::new(&config);

        // Create an empty initial buffer
        let mut buffers = HashMap::new();
        let mut event_logs = HashMap::new();

        let buffer_id = BufferId(0);
        buffers.insert(buffer_id, EditorState::new(width, height));
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

        let state = EditorState::from_file(path, 80, 24)?;
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

        self.buffers.insert(buffer_id, EditorState::new(80, 24));
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
        let current_line = state.buffer.byte_to_line(primary.position);

        if current_line == 0 {
            self.status_message = Some("Already at first line".to_string());
            return;
        }

        // Calculate column offset from line start
        let line_start = state.buffer.line_to_byte(current_line);
        let col_offset = primary.position - line_start;

        // Calculate position on line above
        let prev_line = current_line - 1;
        let prev_line_start = state.buffer.line_to_byte(prev_line);
        let prev_line_end = line_start.saturating_sub(1); // Exclude newline
        let prev_line_len = prev_line_end - prev_line_start;

        let new_pos = prev_line_start + col_offset.min(prev_line_len);
        let new_cursor = crate::cursor::Cursor::new(new_pos);

        let state_mut = self.active_state_mut();
        state_mut.cursors.add(new_cursor);
        state_mut.cursors.normalize();

        self.status_message = Some(format!("Added cursor above ({})", state_mut.cursors.iter().count()));
    }

    /// Add a cursor below the primary cursor at the same column
    pub fn add_cursor_below(&mut self) {
        let state = self.active_state();
        let primary = state.cursors.primary();
        let current_line = state.buffer.byte_to_line(primary.position);

        if current_line + 1 >= state.buffer.line_count() {
            self.status_message = Some("Already at last line".to_string());
            return;
        }

        // Calculate column offset from line start
        let line_start = state.buffer.line_to_byte(current_line);
        let col_offset = primary.position - line_start;

        // Calculate position on line below
        let next_line = current_line + 1;
        let next_line_start = state.buffer.line_to_byte(next_line);
        let next_line_end = if next_line + 1 < state.buffer.line_count() {
            state.buffer.line_to_byte(next_line + 1).saturating_sub(1)
        } else {
            state.buffer.len()
        };
        let next_line_len = next_line_end - next_line_start;

        let new_pos = next_line_start + col_offset.min(next_line_len);
        let new_cursor = crate::cursor::Cursor::new(new_pos);

        let state_mut = self.active_state_mut();
        state_mut.cursors.add(new_cursor);
        state_mut.cursors.normalize();

        self.status_message = Some(format!("Added cursor below ({})", state_mut.cursors.iter().count()));
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

    /// Render the editor to the terminal
    pub fn render(&mut self, frame: &mut Frame) {
        let size = frame.area();

        // If help is visible, render help page instead
        if self.help_visible {
            self.render_help(frame, size);
            return;
        }

        // Split into tabs, content, and status bar
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(1), // Tabs
                Constraint::Min(0),    // Content
                Constraint::Length(1), // Status bar
            ])
            .split(size);

        // Render tabs
        self.render_tabs(frame, chunks[0]);

        // Render content
        self.render_content(frame, chunks[1]);

        // Render status bar
        self.render_status_bar(frame, chunks[2]);
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
        let state = self.active_state_mut();

        // Get visible lines
        let visible_lines = state.viewport.visible_range();
        let mut lines = Vec::new();

        for line_num in visible_lines.clone() {
            if line_num >= state.buffer.line_count() {
                break;
            }

            let line_content = state.buffer.line_content(line_num);
            let line_text = format!("{:4} │ {}", line_num + 1, line_content);
            lines.push(Line::from(line_text));
        }

        let paragraph = Paragraph::new(lines)
            .block(Block::default().borders(Borders::NONE))
            .style(Style::default().fg(Color::White));

        frame.render_widget(paragraph, area);

        // Render cursor
        let cursor_positions = state.cursor_positions();
        if let Some(&(x, y)) = cursor_positions.first() {
            // Adjust for line numbers (4 digits + " │ " = 7 chars)
            // and adjust Y for the content area offset (area.y accounts for tab bar)
            let screen_x = area.x.saturating_add(x).saturating_add(7);
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
            let line = state.buffer.byte_to_line(cursor.position) + 1;
            let col = cursor.position - state.buffer.line_to_byte(line - 1);

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

    /// Helper: Find the start of the word to the left of the given position
    fn find_word_start_left(&self, buffer: &crate::buffer::Buffer, pos: usize) -> usize {
        if pos == 0 {
            return 0;
        }

        let bytes = buffer.slice_bytes(0..buffer.len());
        let mut new_pos = pos.saturating_sub(1);

        // Skip whitespace
        while new_pos > 0 && bytes.get(new_pos).is_some_and(|&b| b.is_ascii_whitespace()) {
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

        new_pos
    }

    /// Helper: Find the start of the word to the right of the given position
    fn find_word_start_right(&self, buffer: &crate::buffer::Buffer, pos: usize) -> usize {
        let bytes = buffer.slice_bytes(0..buffer.len());
        let len = bytes.len();

        if pos >= len {
            return len;
        }

        let mut new_pos = pos;

        // Skip current word
        while new_pos < len && bytes.get(new_pos).is_some_and(|&b| Self::is_word_char(b)) {
            new_pos += 1;
        }

        // Skip whitespace
        while new_pos < len && bytes.get(new_pos).is_some_and(|&b| b.is_ascii_whitespace()) {
            new_pos += 1;
        }

        new_pos
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
                    let current_line = state.buffer.byte_to_line(cursor.position);
                    if current_line > 0 {
                        let line_start = state.buffer.line_to_byte(current_line);
                        let col_offset = cursor.position - line_start;

                        let prev_line_start = state.buffer.line_to_byte(current_line - 1);
                        let prev_line_end = line_start.saturating_sub(1); // Exclude newline
                        let prev_line_len = prev_line_end - prev_line_start;

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
                    let current_line = state.buffer.byte_to_line(cursor.position);
                    if current_line + 1 < state.buffer.line_count() {
                        let line_start = state.buffer.line_to_byte(current_line);
                        let col_offset = cursor.position - line_start;

                        let next_line_start = state.buffer.line_to_byte(current_line + 1);
                        let next_line_end = if current_line + 2 < state.buffer.line_count() {
                            state
                                .buffer
                                .line_to_byte(current_line + 2)
                                .saturating_sub(1)
                        } else {
                            state.buffer.len()
                        };
                        let next_line_len = next_line_end - next_line_start;

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
                    let line = state.buffer.byte_to_line(cursor.position);
                    let line_start = state.buffer.line_to_byte(line);
                    events.push(Event::MoveCursor {
                        cursor_id,
                        position: line_start,
                        anchor: None,
                    });
                }
            }

            Action::MoveLineEnd => {
                for (cursor_id, cursor) in state.cursors.iter() {
                    let line = state.buffer.byte_to_line(cursor.position);
                    let line_end = if line + 1 < state.buffer.line_count() {
                        state.buffer.line_to_byte(line + 1).saturating_sub(1)
                    } else {
                        state.buffer.len()
                    };
                    events.push(Event::MoveCursor {
                        cursor_id,
                        position: line_end,
                        anchor: None,
                    });
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
                    let line = state.buffer.byte_to_line(cursor.position);
                    let line_start = state.buffer.line_to_byte(line);
                    let line_end = if line + 1 < state.buffer.line_count() {
                        state.buffer.line_to_byte(line + 1) // Include newline
                    } else {
                        state.buffer.len()
                    };

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
                    let current_line = state.buffer.byte_to_line(cursor.position);
                    if current_line > 0 {
                        let line_start = state.buffer.line_to_byte(current_line);
                        let col_offset = cursor.position - line_start;

                        let prev_line_start = state.buffer.line_to_byte(current_line - 1);
                        let prev_line_end = line_start.saturating_sub(1);
                        let prev_line_len = prev_line_end - prev_line_start;

                        let new_pos = prev_line_start + col_offset.min(prev_line_len);
                        events.push(Event::MoveCursor {
                            cursor_id,
                            position: new_pos,
                            anchor: Some(anchor),
                        });
                    }
                }
            }

            Action::SelectDown => {
                for (cursor_id, cursor) in state.cursors.iter() {
                    let anchor = cursor.anchor.unwrap_or(cursor.position);
                    let current_line = state.buffer.byte_to_line(cursor.position);
                    if current_line + 1 < state.buffer.line_count() {
                        let line_start = state.buffer.line_to_byte(current_line);
                        let col_offset = cursor.position - line_start;

                        let next_line_start = state.buffer.line_to_byte(current_line + 1);
                        let next_line_end = if current_line + 2 < state.buffer.line_count() {
                            state
                                .buffer
                                .line_to_byte(current_line + 2)
                                .saturating_sub(1)
                        } else {
                            state.buffer.len()
                        };
                        let next_line_len = next_line_end - next_line_start;

                        let new_pos = next_line_start + col_offset.min(next_line_len);
                        events.push(Event::MoveCursor {
                            cursor_id,
                            position: new_pos,
                            anchor: Some(anchor),
                        });
                    }
                }
            }

            Action::SelectLineStart => {
                for (cursor_id, cursor) in state.cursors.iter() {
                    let anchor = cursor.anchor.unwrap_or(cursor.position);
                    let line = state.buffer.byte_to_line(cursor.position);
                    let line_start = state.buffer.line_to_byte(line);
                    events.push(Event::MoveCursor {
                        cursor_id,
                        position: line_start,
                        anchor: Some(anchor),
                    });
                }
            }

            Action::SelectLineEnd => {
                for (cursor_id, cursor) in state.cursors.iter() {
                    let anchor = cursor.anchor.unwrap_or(cursor.position);
                    let line = state.buffer.byte_to_line(cursor.position);
                    let line_end = if line + 1 < state.buffer.line_count() {
                        state.buffer.line_to_byte(line + 1).saturating_sub(1)
                    } else {
                        state.buffer.len()
                    };
                    events.push(Event::MoveCursor {
                        cursor_id,
                        position: line_end,
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
                    let current_line = state.buffer.byte_to_line(cursor.position);
                    let target_line = current_line.saturating_sub(lines_per_page);
                    let line_start = state.buffer.line_to_byte(current_line);
                    let col_offset = cursor.position - line_start;

                    let target_line_start = state.buffer.line_to_byte(target_line);
                    let target_line_end = if target_line + 1 < state.buffer.line_count() {
                        state.buffer.line_to_byte(target_line + 1).saturating_sub(1)
                    } else {
                        state.buffer.len()
                    };
                    let target_line_len = target_line_end - target_line_start;

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
                    let current_line = state.buffer.byte_to_line(cursor.position);
                    let target_line = (current_line + lines_per_page)
                        .min(state.buffer.line_count().saturating_sub(1));
                    let line_start = state.buffer.line_to_byte(current_line);
                    let col_offset = cursor.position - line_start;

                    let target_line_start = state.buffer.line_to_byte(target_line);
                    let target_line_end = if target_line + 1 < state.buffer.line_count() {
                        state.buffer.line_to_byte(target_line + 1).saturating_sub(1)
                    } else {
                        state.buffer.len()
                    };
                    let target_line_len = target_line_end - target_line_start;

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
            | Action::ShowHelp => {
                // These actions need special handling in the event loop:
                // - Clipboard operations need system clipboard access
                // - File operations need Editor-level state changes
                // - Undo/Redo need EventLog manipulation
                // - Multi-cursor add operations need visual line calculations
                // - ShowHelp toggles help view
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
