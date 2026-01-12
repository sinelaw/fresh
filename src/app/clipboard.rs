//! Clipboard and multi-cursor operations for the Editor.
//!
//! This module contains clipboard operations and multi-cursor actions:
//! - Copy/cut/paste operations
//! - Copy with formatting (HTML with syntax highlighting)
//! - Multi-cursor add above/below/at next match

use rust_i18n::t;

use crate::input::multi_cursor::{
    add_cursor_above, add_cursor_at_next_match, add_cursor_below, AddCursorResult,
};
use crate::model::event::{CursorId, Event};
use crate::primitives::word_navigation::{find_word_start_left, find_word_start_right};

use super::Editor;

// These are the clipboard and multi-cursor operations on Editor.
//
// MOTIVATION FOR SEPARATION:
// - Buffer operations need: multi-cursor, selections, event sourcing, undo/redo
// - Prompt operations need: simple string manipulation, no selection tracking
// - Sharing code would force prompts to use Buffer (expensive) or buffers to
//   lose features (selections, multi-cursor, undo)
//
// Both use the same clipboard storage (self.clipboard) ensuring copy/paste
// works across buffer editing and prompt input.

impl Editor {
    /// Copy the current selection to clipboard
    ///
    /// If no selection exists, copies the entire current line (like VSCode/Rider/Zed).
    pub fn copy_selection(&mut self) {
        // Check if any cursor has a selection
        let has_selection = {
            let state = self.active_state();
            state
                .cursors
                .iter()
                .any(|(_, cursor)| cursor.selection_range().is_some())
        };

        if has_selection {
            // Original behavior: copy selected text
            let ranges: Vec<_> = {
                let state = self.active_state();
                state
                    .cursors
                    .iter()
                    .filter_map(|(_, cursor)| cursor.selection_range())
                    .collect()
            };

            let mut text = String::new();
            let state = self.active_state_mut();
            for range in ranges {
                if !text.is_empty() {
                    text.push('\n');
                }
                let range_text = state.get_text_range(range.start, range.end);
                text.push_str(&range_text);
            }

            if !text.is_empty() {
                self.clipboard.copy(text);
                self.status_message = Some(t!("clipboard.copied").to_string());
            }
        } else {
            // No selection: copy entire line(s) for each cursor
            let estimated_line_length = 80;
            let mut text = String::new();
            let state = self.active_state_mut();

            // Collect cursor positions first
            let positions: Vec<_> = state.cursors.iter().map(|(_, c)| c.position).collect();

            for pos in positions {
                let mut iter = state.buffer.line_iterator(pos, estimated_line_length);
                if let Some((_start, content)) = iter.next_line() {
                    if !text.is_empty() {
                        text.push('\n');
                    }
                    text.push_str(&content);
                }
            }

            if !text.is_empty() {
                self.clipboard.copy(text);
                self.status_message = Some(t!("clipboard.copied_line").to_string());
            }
        }
    }

    /// Copy selection with a specific theme's formatting
    ///
    /// If theme_name is empty, opens a prompt to select a theme.
    /// Otherwise, copies the selected text as HTML with inline CSS styles.
    pub fn copy_selection_with_theme(&mut self, theme_name: &str) {
        // Check if there's a selection first
        let has_selection = {
            let state = self.active_state();
            state
                .cursors
                .iter()
                .any(|(_, cursor)| cursor.selection_range().is_some())
        };

        if !has_selection {
            self.status_message = Some(t!("clipboard.no_selection").to_string());
            return;
        }

        // Empty theme = open theme picker prompt
        if theme_name.is_empty() {
            self.start_copy_with_formatting_prompt();
            return;
        }
        use crate::services::styled_html::render_styled_html;

        // Load the requested theme
        let theme = match crate::view::theme::Theme::from_name(theme_name) {
            Some(t) => t,
            None => {
                self.status_message = Some(format!("Theme '{}' not found", theme_name));
                return;
            }
        };

        // Collect ranges and their byte offsets
        let ranges: Vec<_> = {
            let state = self.active_state();
            state
                .cursors
                .iter()
                .filter_map(|(_, cursor)| cursor.selection_range())
                .collect()
        };

        if ranges.is_empty() {
            self.status_message = Some(t!("clipboard.no_selection").to_string());
            return;
        }

        // Get the overall range for highlighting
        let min_offset = ranges.iter().map(|r| r.start).min().unwrap_or(0);
        let max_offset = ranges.iter().map(|r| r.end).max().unwrap_or(0);

        // Collect text and highlight spans from state
        let (text, highlight_spans) = {
            let state = self.active_state_mut();

            // Collect text from all ranges
            let mut text = String::new();
            for range in &ranges {
                if !text.is_empty() {
                    text.push('\n');
                }
                let range_text = state.get_text_range(range.start, range.end);
                text.push_str(&range_text);
            }

            if text.is_empty() {
                (text, Vec::new())
            } else {
                // Get highlight spans for the selected region
                let highlight_spans = state.highlighter.highlight_viewport(
                    &state.buffer,
                    min_offset,
                    max_offset,
                    &theme,
                    0, // No context needed since we're copying exact selection
                );
                (text, highlight_spans)
            }
        };

        if text.is_empty() {
            self.status_message = Some(t!("clipboard.no_text").to_string());
            return;
        }

        // Adjust highlight spans to be relative to the copied text
        let adjusted_spans: Vec<_> = if ranges.len() == 1 {
            let base_offset = ranges[0].start;
            highlight_spans
                .into_iter()
                .filter_map(|span| {
                    if span.range.end <= base_offset || span.range.start >= ranges[0].end {
                        return None;
                    }
                    let start = span.range.start.saturating_sub(base_offset);
                    let end = (span.range.end - base_offset).min(text.len());
                    if start < end {
                        Some(crate::primitives::highlighter::HighlightSpan {
                            range: start..end,
                            color: span.color,
                        })
                    } else {
                        None
                    }
                })
                .collect()
        } else {
            Vec::new()
        };

        // Render the styled text to HTML
        let html = render_styled_html(&text, &adjusted_spans, &theme);

        // Copy the HTML to clipboard (with plain text fallback)
        if self.clipboard.copy_html(&html, &text) {
            self.status_message =
                Some(t!("clipboard.copied_with_theme", theme = theme_name).to_string());
        } else {
            self.clipboard.copy(text);
            self.status_message = Some(t!("clipboard.copied_plain").to_string());
        }
    }

    /// Start the theme selection prompt for copy with formatting
    fn start_copy_with_formatting_prompt(&mut self) {
        use crate::view::prompt::PromptType;

        let available_themes = crate::view::theme::Theme::available_themes();
        let current_theme_name = &self.theme.name;

        // Find the index of the current theme
        let current_index = available_themes
            .iter()
            .position(|name| name == current_theme_name)
            .unwrap_or(0);

        let suggestions: Vec<crate::input::commands::Suggestion> = available_themes
            .iter()
            .map(|theme_name| {
                let is_current = theme_name == current_theme_name;
                crate::input::commands::Suggestion {
                    text: theme_name.to_string(),
                    description: if is_current {
                        Some("(current)".to_string())
                    } else {
                        None
                    },
                    value: Some(theme_name.to_string()),
                    disabled: false,
                    keybinding: None,
                    source: None,
                }
            })
            .collect();

        self.prompt = Some(crate::view::prompt::Prompt::with_suggestions(
            "Copy with theme: ".to_string(),
            PromptType::CopyWithFormattingTheme,
            suggestions,
        ));

        if let Some(prompt) = self.prompt.as_mut() {
            if !prompt.suggestions.is_empty() {
                prompt.selected_suggestion = Some(current_index);
                prompt.input = current_theme_name.to_string();
                prompt.cursor_pos = prompt.input.len();
            }
        }
    }

    /// Cut the current selection to clipboard
    ///
    /// If no selection exists, cuts the entire current line (like VSCode/Rider/Zed).
    pub fn cut_selection(&mut self) {
        // Check if any cursor has a selection
        let has_selection = {
            let state = self.active_state();
            state
                .cursors
                .iter()
                .any(|(_, cursor)| cursor.selection_range().is_some())
        };

        // Copy first (this handles both selection and whole-line cases)
        self.copy_selection();

        if has_selection {
            // Original behavior: delete selected text
            let deletions: Vec<_> = {
                let state = self.active_state();
                state
                    .cursors
                    .iter()
                    .filter_map(|(_, c)| c.selection_range())
                    .collect()
            };

            let state = self.active_state_mut();
            let primary_id = state.cursors.primary_id();
            let events: Vec<_> = deletions
                .iter()
                .rev()
                .map(|range| {
                    let deleted_text = state.get_text_range(range.start, range.end);
                    Event::Delete {
                        range: range.clone(),
                        deleted_text,
                        cursor_id: primary_id,
                    }
                })
                .collect();

            for event in events {
                self.active_event_log_mut().append(event.clone());
                self.apply_event_to_active_buffer(&event);
            }

            if !deletions.is_empty() {
                self.status_message = Some(t!("clipboard.cut").to_string());
            }
        } else {
            // No selection: delete entire line(s) for each cursor
            let estimated_line_length = 80;

            // Collect line ranges for each cursor
            let deletions: Vec<_> = {
                let state = self.active_state_mut();
                let positions: Vec<_> = state.cursors.iter().map(|(_, c)| c.position).collect();

                positions
                    .into_iter()
                    .filter_map(|pos| {
                        let mut iter = state.buffer.line_iterator(pos, estimated_line_length);
                        let line_start = iter.current_position();
                        iter.next_line().map(|(_start, content)| {
                            let line_end = line_start + content.len();
                            line_start..line_end
                        })
                    })
                    .collect()
            };

            let state = self.active_state_mut();
            let primary_id = state.cursors.primary_id();
            let events: Vec<_> = deletions
                .iter()
                .rev()
                .map(|range| {
                    let deleted_text = state.get_text_range(range.start, range.end);
                    Event::Delete {
                        range: range.clone(),
                        deleted_text,
                        cursor_id: primary_id,
                    }
                })
                .collect();

            for event in events {
                self.active_event_log_mut().append(event.clone());
                self.apply_event_to_active_buffer(&event);
            }

            if !deletions.is_empty() {
                self.status_message = Some(t!("clipboard.cut_line").to_string());
            }
        }
    }

    /// Paste the clipboard content at all cursor positions
    ///
    /// Handles:
    /// - Single cursor paste
    /// - Multi-cursor paste (pastes at each cursor)
    /// - Selection replacement (deletes selection before inserting)
    /// - Atomic undo (single undo step for entire operation)
    pub fn paste(&mut self) {
        // Get content from clipboard (tries system first, falls back to internal)
        let text = match self.clipboard.paste() {
            Some(text) => text,
            None => return,
        };

        // Use paste_text which handles line ending normalization
        self.paste_text(text);
    }

    /// Paste text directly into the editor
    ///
    /// Handles:
    /// - Line ending normalization (CRLF/CR → buffer's format)
    /// - Single cursor paste
    /// - Multi-cursor paste (pastes at each cursor)
    /// - Selection replacement (deletes selection before inserting)
    /// - Atomic undo (single undo step for entire operation)
    /// - Routing to prompt if one is open
    pub fn paste_text(&mut self, paste_text: String) {
        if paste_text.is_empty() {
            return;
        }

        // Normalize line endings: first convert all to LF, then to buffer's format
        // This handles Windows clipboard (CRLF), old Mac (CR), and Unix (LF)
        let normalized = paste_text.replace("\r\n", "\n").replace('\r', "\n");

        // If a prompt is open, paste into the prompt (prompts use LF internally)
        if let Some(prompt) = self.prompt.as_mut() {
            prompt.insert_str(&normalized);
            self.update_prompt_suggestions();
            self.status_message = Some(t!("clipboard.pasted").to_string());
            return;
        }

        // Convert to buffer's line ending format
        let buffer_line_ending = self.active_state().buffer.line_ending();
        let paste_text = match buffer_line_ending {
            crate::model::buffer::LineEnding::LF => normalized,
            crate::model::buffer::LineEnding::CRLF => normalized.replace('\n', "\r\n"),
            crate::model::buffer::LineEnding::CR => normalized.replace('\n', "\r"),
        };

        let mut events = Vec::new();

        // Collect cursor info sorted in reverse order by position
        let state = self.active_state();
        let mut cursor_data: Vec<_> = state
            .cursors
            .iter()
            .map(|(cursor_id, cursor)| {
                let selection = cursor.selection_range();
                let insert_position = selection
                    .as_ref()
                    .map(|r| r.start)
                    .unwrap_or(cursor.position);
                (cursor_id, selection, insert_position)
            })
            .collect();
        cursor_data.sort_by_key(|(_, _, pos)| std::cmp::Reverse(*pos));

        // Get deleted text for each selection
        let cursor_data_with_text: Vec<_> = {
            let state = self.active_state_mut();
            cursor_data
                .into_iter()
                .map(|(cursor_id, selection, insert_position)| {
                    let deleted_text = selection
                        .as_ref()
                        .map(|r| state.get_text_range(r.start, r.end));
                    (cursor_id, selection, insert_position, deleted_text)
                })
                .collect()
        };

        // Build events for each cursor
        for (cursor_id, selection, insert_position, deleted_text) in cursor_data_with_text {
            if let (Some(range), Some(text)) = (selection, deleted_text) {
                events.push(Event::Delete {
                    range,
                    deleted_text: text,
                    cursor_id,
                });
            }
            events.push(Event::Insert {
                position: insert_position,
                text: paste_text.clone(),
                cursor_id,
            });
        }

        // Apply events with atomic undo using bulk edit for O(n) performance
        if events.len() > 1 {
            // Use optimized bulk edit for multi-cursor paste
            if let Some(bulk_edit) = self.apply_events_as_bulk_edit(events, "Paste".to_string()) {
                self.active_event_log_mut().append(bulk_edit);
            }
        } else if let Some(event) = events.into_iter().next() {
            self.active_event_log_mut().append(event.clone());
            self.apply_event_to_active_buffer(&event);
        }

        self.status_message = Some(t!("clipboard.pasted").to_string());
    }

    /// Set clipboard content for testing purposes
    /// This sets the internal clipboard and enables internal-only mode to avoid
    /// system clipboard interference between parallel tests
    #[doc(hidden)]
    pub fn set_clipboard_for_test(&mut self, text: String) {
        self.clipboard.set_internal(text);
        self.clipboard.set_internal_only(true);
    }

    /// Paste from internal clipboard only (for testing)
    /// This bypasses the system clipboard to avoid interference from CI environments
    #[doc(hidden)]
    pub fn paste_for_test(&mut self) {
        // Get content from internal clipboard only (ignores system clipboard)
        let paste_text = match self.clipboard.paste_internal() {
            Some(text) => text,
            None => return,
        };

        // Use the same paste logic as the regular paste method
        self.paste_text(paste_text);
    }

    /// Add a cursor at the next occurrence of the selected text
    /// If no selection, does nothing
    pub fn add_cursor_at_next_match(&mut self) {
        let state = self.active_state_mut();
        match add_cursor_at_next_match(state) {
            AddCursorResult::Success {
                cursor,
                total_cursors,
            } => {
                // Create AddCursor event with the next cursor ID
                let next_id = CursorId(self.active_state().cursors.count());
                let event = Event::AddCursor {
                    cursor_id: next_id,
                    position: cursor.position,
                    anchor: cursor.anchor,
                };

                // Log and apply the event
                self.active_event_log_mut().append(event.clone());
                self.apply_event_to_active_buffer(&event);

                self.status_message =
                    Some(t!("clipboard.added_cursor_match", count = total_cursors).to_string());
            }
            AddCursorResult::Failed { message } => {
                self.status_message = Some(message);
            }
        }
    }

    /// Add a cursor above the primary cursor at the same column
    pub fn add_cursor_above(&mut self) {
        let state = self.active_state_mut();
        match add_cursor_above(state) {
            AddCursorResult::Success {
                cursor,
                total_cursors,
            } => {
                // Create AddCursor event with the next cursor ID
                let next_id = CursorId(self.active_state().cursors.count());
                let event = Event::AddCursor {
                    cursor_id: next_id,
                    position: cursor.position,
                    anchor: cursor.anchor,
                };

                // Log and apply the event
                self.active_event_log_mut().append(event.clone());
                self.apply_event_to_active_buffer(&event);

                self.status_message =
                    Some(t!("clipboard.added_cursor_above", count = total_cursors).to_string());
            }
            AddCursorResult::Failed { message } => {
                self.status_message = Some(message);
            }
        }
    }

    /// Add a cursor below the primary cursor at the same column
    pub fn add_cursor_below(&mut self) {
        let state = self.active_state_mut();
        match add_cursor_below(state) {
            AddCursorResult::Success {
                cursor,
                total_cursors,
            } => {
                // Create AddCursor event with the next cursor ID
                let next_id = CursorId(self.active_state().cursors.count());
                let event = Event::AddCursor {
                    cursor_id: next_id,
                    position: cursor.position,
                    anchor: cursor.anchor,
                };

                // Log and apply the event
                self.active_event_log_mut().append(event.clone());
                self.apply_event_to_active_buffer(&event);

                self.status_message =
                    Some(t!("clipboard.added_cursor_below", count = total_cursors).to_string());
            }
            AddCursorResult::Failed { message } => {
                self.status_message = Some(message);
            }
        }
    }

    // =========================================================================
    // Vi-style yank operations (copy range without requiring selection)
    // =========================================================================

    /// Yank (copy) from cursor to next word start
    pub fn yank_word_forward(&mut self) {
        let ranges: Vec<_> = {
            let state = self.active_state();
            state
                .cursors
                .iter()
                .filter_map(|(_, cursor)| {
                    let start = cursor.position;
                    let end = find_word_start_right(&state.buffer, start);
                    if end > start {
                        Some(start..end)
                    } else {
                        None
                    }
                })
                .collect()
        };

        if ranges.is_empty() {
            return;
        }

        // Copy text from all ranges
        let mut text = String::new();
        let state = self.active_state_mut();
        for range in ranges {
            if !text.is_empty() {
                text.push('\n');
            }
            let range_text = state.get_text_range(range.start, range.end);
            text.push_str(&range_text);
        }

        if !text.is_empty() {
            let len = text.len();
            self.clipboard.copy(text);
            self.status_message = Some(t!("clipboard.yanked", count = len).to_string());
        }
    }

    /// Yank (copy) from previous word start to cursor
    pub fn yank_word_backward(&mut self) {
        let ranges: Vec<_> = {
            let state = self.active_state();
            state
                .cursors
                .iter()
                .filter_map(|(_, cursor)| {
                    let end = cursor.position;
                    let start = find_word_start_left(&state.buffer, end);
                    if start < end {
                        Some(start..end)
                    } else {
                        None
                    }
                })
                .collect()
        };

        if ranges.is_empty() {
            return;
        }

        let mut text = String::new();
        let state = self.active_state_mut();
        for range in ranges {
            if !text.is_empty() {
                text.push('\n');
            }
            let range_text = state.get_text_range(range.start, range.end);
            text.push_str(&range_text);
        }

        if !text.is_empty() {
            let len = text.len();
            self.clipboard.copy(text);
            self.status_message = Some(t!("clipboard.yanked", count = len).to_string());
        }
    }

    /// Yank (copy) from cursor to end of line
    pub fn yank_to_line_end(&mut self) {
        let estimated_line_length = 80;

        // First collect cursor positions with immutable borrow
        let cursor_positions: Vec<_> = {
            let state = self.active_state();
            state
                .cursors
                .iter()
                .map(|(_, cursor)| cursor.position)
                .collect()
        };

        // Now compute ranges with mutable borrow (line_iterator needs &mut self)
        let state = self.active_state_mut();
        let mut ranges = Vec::new();
        for pos in cursor_positions {
            let mut iter = state.buffer.line_iterator(pos, estimated_line_length);
            let line_start = iter.current_position();
            if let Some((_start, content)) = iter.next_line() {
                // Don't include the line ending in yank
                let content_len = content.trim_end_matches(&['\n', '\r'][..]).len();
                let line_end = line_start + content_len;
                if pos < line_end {
                    ranges.push(pos..line_end);
                }
            }
        }

        if ranges.is_empty() {
            return;
        }

        let mut text = String::new();
        for range in ranges {
            if !text.is_empty() {
                text.push('\n');
            }
            let range_text = state.get_text_range(range.start, range.end);
            text.push_str(&range_text);
        }

        if !text.is_empty() {
            let len = text.len();
            self.clipboard.copy(text);
            self.status_message = Some(t!("clipboard.yanked", count = len).to_string());
        }
    }

    /// Yank (copy) from start of line to cursor
    pub fn yank_to_line_start(&mut self) {
        let estimated_line_length = 80;

        // First collect cursor positions with immutable borrow
        let cursor_positions: Vec<_> = {
            let state = self.active_state();
            state
                .cursors
                .iter()
                .map(|(_, cursor)| cursor.position)
                .collect()
        };

        // Now compute ranges with mutable borrow (line_iterator needs &mut self)
        let state = self.active_state_mut();
        let mut ranges = Vec::new();
        for pos in cursor_positions {
            let iter = state.buffer.line_iterator(pos, estimated_line_length);
            let line_start = iter.current_position();
            if pos > line_start {
                ranges.push(line_start..pos);
            }
        }

        if ranges.is_empty() {
            return;
        }

        let mut text = String::new();
        for range in ranges {
            if !text.is_empty() {
                text.push('\n');
            }
            let range_text = state.get_text_range(range.start, range.end);
            text.push_str(&range_text);
        }

        if !text.is_empty() {
            let len = text.len();
            self.clipboard.copy(text);
            self.status_message = Some(t!("clipboard.yanked", count = len).to_string());
        }
    }
}
