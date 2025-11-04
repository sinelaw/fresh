//! Status bar and prompt/minibuffer rendering

use crate::prompt::Prompt;
use crate::state::EditorState;
use ratatui::layout::Rect;
use ratatui::style::{Color, Style};
use ratatui::widgets::Paragraph;
use ratatui::Frame;

/// Renders the status bar and prompt/minibuffer
pub struct StatusBarRenderer;

impl StatusBarRenderer {
    /// Render the status bar or prompt/minibuffer
    ///
    /// If a prompt is active, renders the prompt instead of the status bar.
    ///
    /// # Arguments
    /// * `frame` - The ratatui frame to render to
    /// * `area` - The rectangular area to render in
    /// * `state` - The active buffer's editor state
    /// * `status_message` - Optional status message to display
    /// * `prompt` - Optional active prompt
    pub fn render(
        frame: &mut Frame,
        area: Rect,
        state: &EditorState,
        status_message: &Option<String>,
        prompt: &Option<Prompt>,
    ) {
        // If we're in prompt mode, render the prompt instead of the status bar
        if let Some(prompt) = prompt {
            Self::render_prompt(frame, area, prompt);
            return;
        }

        // Normal status bar rendering
        Self::render_status(frame, area, state, status_message);
    }

    /// Render the prompt/minibuffer
    fn render_prompt(frame: &mut Frame, area: Rect, prompt: &Prompt) {
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
    }

    /// Render the normal status bar
    fn render_status(
        frame: &mut Frame,
        area: Rect,
        state: &EditorState,
        status_message: &Option<String>,
    ) {
        // Collect all data we need from state
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

        let status = if let Some(msg) = status_message {
            format!("{filename}{modified} | Ln {line}, Col {col} | {msg}")
        } else {
            format!("{filename}{modified} | Ln {line}, Col {col}")
        };

        let status_line =
            Paragraph::new(status).style(Style::default().fg(Color::Black).bg(Color::White));

        frame.render_widget(status_line, area);
    }
}
