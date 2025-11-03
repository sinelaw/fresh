use crate::config::Config;
use crate::event::{Event, EventLog};
use crate::keybindings::KeybindingResolver;
use crate::state::EditorState;
use ratatui::{
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    text::Line,
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
}

impl Editor {
    /// Create a new editor with the given configuration
    pub fn new(config: Config) -> io::Result<Self> {
        let keybindings = KeybindingResolver::new(&config);

        // Create an empty initial buffer
        let mut buffers = HashMap::new();
        let mut event_logs = HashMap::new();

        let buffer_id = BufferId(0);
        buffers.insert(buffer_id, EditorState::new(80, 24));
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
        })
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
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "Cannot close last buffer",
            ));
        }

        // Check for unsaved changes
        if let Some(state) = self.buffers.get(&id) {
            if state.buffer.is_modified() {
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    "Buffer has unsaved changes",
                ));
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

                format!(" {}{} ", name, modified)
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
            .highlight_style(Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD));

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
            frame.set_cursor_position((x.saturating_add(7), y));
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

            let modified = if state.buffer.is_modified() { " [+]" } else { "" };

            let cursor = state.primary_cursor().clone();
            let line = state.buffer.byte_to_line(cursor.position) + 1;
            let col = cursor.position - state.buffer.line_to_byte(line - 1);

            (filename, modified, line, col)
        };

        let status = if let Some(msg) = &self.status_message {
            format!("{}{} | Ln {}, Col {} | {}", filename, modified, line, col, msg)
        } else {
            format!("{}{} | Ln {}, Col {}", filename, modified, line, col)
        };

        let status_line = Paragraph::new(status)
            .style(Style::default().fg(Color::Black).bg(Color::White));

        frame.render_widget(status_line, area);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_editor_new() {
        let config = Config::default();
        let editor = Editor::new(config).unwrap();

        assert_eq!(editor.buffers.len(), 1);
        assert!(!editor.should_quit());
    }

    #[test]
    fn test_new_buffer() {
        let config = Config::default();
        let mut editor = Editor::new(config).unwrap();

        let id = editor.new_buffer();
        assert_eq!(editor.buffers.len(), 2);
        assert_eq!(editor.active_buffer, id);
    }

    #[test]
    fn test_clipboard() {
        let config = Config::default();
        let mut editor = Editor::new(config).unwrap();

        // Manually set clipboard
        editor.clipboard = "test".to_string();

        // Paste should work
        editor.paste();

        let content = editor.active_state().buffer.to_string();
        assert_eq!(content, "test");
    }
}
