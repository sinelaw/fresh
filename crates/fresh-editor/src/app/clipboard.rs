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
use crate::model::buffer::Buffer;
use crate::model::cursor::Position2D;
use crate::model::event::{CursorId, Event};
use crate::primitives::word_navigation::{find_word_start_left, find_word_start_right};

use super::Editor;

/// Convert byte offset to 2D position (line, column)
fn byte_to_2d(buffer: &Buffer, byte_pos: usize) -> Position2D {
    let line = buffer.get_line_number(byte_pos);
    let line_start = buffer.line_start_offset(line).unwrap_or(0);
    let column = byte_pos.saturating_sub(line_start);
    Position2D { line, column }
}

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
    /// For block selections, copies only the rectangular region.
    pub fn copy_selection(&mut self) {
        // Check if any cursor has a block selection (takes priority)
        let has_block_selection = {
            let state = self.active_state();
            state
                .cursors
                .iter()
                .any(|(_, cursor)| cursor.has_block_selection())
        };

        if has_block_selection {
            // Block selection: copy rectangular region
            let text = self.copy_block_selection_text();
            if !text.is_empty() {
                self.clipboard.copy(text);
                self.status_message = Some(t!("clipboard.copied").to_string());
            }
            return;
        }

        // Check if any cursor has a normal selection
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

    /// Extract text from block (rectangular) selection
    ///
    /// For block selection, we need to extract a rectangular region defined by:
    /// - The block anchor (stored as Position2D with line and column)
    /// - The current cursor position (byte offset, converted to 2D)
    ///
    /// This works for both small and large files by using line_iterator
    /// for iteration and only using 2D positions for column extraction.
    fn copy_block_selection_text(&mut self) -> String {
        let estimated_line_length = 120;

        // Collect block selection info from all cursors
        let block_infos: Vec<_> = {
            let state = self.active_state();
            state
                .cursors
                .iter()
                .filter_map(|(_, cursor)| {
                    if !cursor.has_block_selection() {
                        return None;
                    }
                    let block_anchor = cursor.block_anchor?;
                    let anchor_byte = cursor.anchor?; // byte offset of anchor
                    let cursor_byte = cursor.position;
                    Some((block_anchor, anchor_byte, cursor_byte))
                })
                .collect()
        };

        let mut result = String::new();

        for (block_anchor, anchor_byte, cursor_byte) in block_infos {
            // Get current cursor position as 2D
            let cursor_2d = {
                let state = self.active_state();
                byte_to_2d(&state.buffer, cursor_byte)
            };

            // Calculate column bounds (min and max columns for the rectangle)
            let min_col = block_anchor.column.min(cursor_2d.column);
            let max_col = block_anchor.column.max(cursor_2d.column);

            // Calculate line bounds using byte positions
            let start_byte = anchor_byte.min(cursor_byte);
            let end_byte = anchor_byte.max(cursor_byte);

            // Use line_iterator to iterate through lines
            let state = self.active_state_mut();
            let mut iter = state
                .buffer
                .line_iterator(start_byte, estimated_line_length);

            // Collect lines within the block selection range
            let mut lines_text = Vec::new();
            loop {
                let line_start = iter.current_position();

                // Stop if we've passed the end of the selection
                if line_start > end_byte {
                    break;
                }

                if let Some((_offset, line_content)) = iter.next_line() {
                    // Extract the column range from this line
                    // Remove trailing newline for column calculation
                    let content_without_newline = line_content.trim_end_matches(&['\n', '\r'][..]);
                    let chars: Vec<char> = content_without_newline.chars().collect();

                    // Extract characters from min_col to max_col (exclusive)
                    let extracted: String = chars
                        .iter()
                        .skip(min_col)
                        .take(max_col.saturating_sub(min_col))
                        .collect();

                    lines_text.push(extracted);

                    // If this line extends past end_byte, we're done
                    if line_start + line_content.len() > end_byte {
                        break;
                    }
                } else {
                    break;
                }
            }

            // Join the extracted text from each line
            if !result.is_empty() && !lines_text.is_empty() {
                result.push('\n');
            }
            result.push_str(&lines_text.join("\n"));
        }

        result
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

        // Get the requested theme from registry
        let theme = match self.theme_registry.get_cloned(theme_name) {
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

        let available_themes = self.theme_registry.list();
        let current_theme_name = &self.theme.name;

        // Find the index of the current theme
        let current_index = available_themes
            .iter()
            .position(|info| info.name == *current_theme_name)
            .unwrap_or(0);

        let suggestions: Vec<crate::input::commands::Suggestion> = available_themes
            .iter()
            .map(|info| {
                let is_current = info.name == *current_theme_name;
                let description = match (is_current, info.pack.is_empty()) {
                    (true, true) => Some("(current)".to_string()),
                    (true, false) => Some(format!("{} (current)", info.pack)),
                    (false, true) => None,
                    (false, false) => Some(info.pack.clone()),
                };
                crate::input::commands::Suggestion {
                    text: info.name.clone(),
                    description,
                    value: Some(info.name.clone()),
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
            // Delete selected text from all cursors
            // IMPORTANT: Sort deletions by position to ensure we process from end to start
            let mut deletions: Vec<_> = {
                let state = self.active_state();
                state
                    .cursors
                    .iter()
                    .filter_map(|(_, c)| c.selection_range())
                    .collect()
            };
            // Sort by start position so reverse iteration processes from end to start
            deletions.sort_by_key(|r| r.start);

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
            // IMPORTANT: Sort deletions by position to ensure we process from end to start
            let mut deletions: Vec<_> = {
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
            // Sort by start position so reverse iteration processes from end to start
            deletions.sort_by_key(|r| r.start);

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

    /// Get clipboard content for testing purposes
    /// Returns the internal clipboard content
    #[doc(hidden)]
    pub fn clipboard_content_for_test(&self) -> String {
        self.clipboard.get_internal().to_string()
    }

    /// Add a cursor at the next occurrence of the selected text
    /// If no selection, first selects the entire word at cursor position
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
            AddCursorResult::WordSelected {
                word_start,
                word_end,
            } => {
                // Select the word by updating the primary cursor
                let primary_id = self.active_state().cursors.primary_id();
                let primary = self.active_state().cursors.primary();
                let event = Event::MoveCursor {
                    cursor_id: primary_id,
                    old_position: primary.position,
                    new_position: word_end,
                    old_anchor: primary.anchor,
                    new_anchor: Some(word_start),
                    old_sticky_column: primary.sticky_column,
                    new_sticky_column: 0,
                };

                // Log and apply the event
                self.active_event_log_mut().append(event.clone());
                self.apply_event_to_active_buffer(&event);
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
            AddCursorResult::WordSelected { .. } => unreachable!(),
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
            AddCursorResult::WordSelected { .. } => unreachable!(),
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
