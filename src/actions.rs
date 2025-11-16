//! Action to event conversion - translates high-level actions into buffer events

use crate::event::Event;
use crate::keybindings::Action;
use crate::state::EditorState;
use crate::text_buffer::Buffer;
use crate::word_navigation::{
    find_word_end, find_word_start, find_word_start_left, find_word_start_right,
};

/// Calculate the maximum valid cursor position in the buffer.
/// This is the end of the last line (excluding trailing newline).
/// For empty buffers, returns 0.
fn max_cursor_position(buffer: &Buffer) -> usize {
    // The maximum cursor position is simply the end of the buffer
    // No need to use line iterator or calculate line positions
    buffer.len()
}

/// Convert an action into a sequence of events that can be applied to the editor state
///
/// # Parameters
/// * `state` - The current editor state
/// * `action` - The action to convert
/// * `tab_size` - Number of spaces per tab
/// * `auto_indent` - Whether auto-indent is enabled
///
/// # Returns
/// * `Some(Vec<Event>)` - Events to apply for this action
/// * `None` - If the action doesn't generate events (like Quit, Save, etc.)
pub fn action_to_events(
    state: &mut EditorState,
    action: Action,
    tab_size: usize,
    auto_indent: bool,
    estimated_line_length: usize,
) -> Option<Vec<Event>> {
    let mut events = Vec::new();

    match action {
        // Character input - insert at each cursor
        Action::InsertChar(ch) => {
            // Collect cursors and sort by position (reverse order)
            // This ensures insertions at later positions happen first,
            // avoiding position shifts that would affect earlier insertions
            let mut cursor_vec: Vec<_> = state.cursors.iter().collect();
            cursor_vec.sort_by_key(|(_, c)| std::cmp::Reverse(c.position));

            // Check if this is a closing delimiter that should trigger auto-dedent
            let is_closing_delimiter = matches!(ch, '}' | ')' | ']');

            // First collect all deletions (for selections)
            let deletions: Vec<_> = cursor_vec
                .iter()
                .filter_map(|(cursor_id, cursor)| {
                    cursor.selection_range().map(|range| (*cursor_id, range))
                })
                .collect();

            // Collect insertion data (to avoid borrowing during loop)
            let insertion_data: Vec<_> = cursor_vec
                .iter()
                .map(|(cursor_id, cursor)| {
                    let insert_position = cursor
                        .selection_range()
                        .map(|r| r.start)
                        .unwrap_or(cursor.position);

                    // Calculate line start for auto-dedent
                    let mut line_start = insert_position;
                    while line_start > 0 {
                        let prev = line_start - 1;
                        if state.buffer.slice_bytes(prev..prev + 1).first() == Some(&b'\n') {
                            break;
                        }
                        line_start = prev;
                    }

                    let line_before_cursor = state.buffer.slice_bytes(line_start..insert_position);
                    let only_spaces = line_before_cursor.iter().all(|&b| b == b' ' || b == b'\t');

                    (*cursor_id, insert_position, line_start, only_spaces)
                })
                .collect();

            // Get text for deletions
            for (cursor_id, range) in deletions {
                let deleted_text = state.get_text_range(range.start, range.end);
                events.push(Event::Delete {
                    range,
                    deleted_text,
                    cursor_id,
                });
            }

            // Now process insertions
            for (cursor_id, insert_position, line_start, only_spaces) in insertion_data {
                // Auto-dedent logic for closing delimiters
                if is_closing_delimiter
                    && auto_indent
                    && only_spaces
                    && insert_position > line_start
                {
                    // Calculate correct indent for the closing delimiter using tree-sitter
                    let correct_indent = if let Some(highlighter) = &state.highlighter {
                        let language = highlighter.language();
                        state
                            .indent_calculator
                            .borrow_mut()
                            .calculate_dedent_for_delimiter(
                                &state.buffer,
                                insert_position,
                                ch,
                                language,
                                tab_size,
                            )
                            .unwrap_or(0)
                    } else {
                        0
                    };

                    // Delete the incorrect spacing
                    let spaces_to_delete = insert_position - line_start;
                    if spaces_to_delete > 0 {
                        let deleted_text = state.get_text_range(line_start, insert_position);
                        events.push(Event::Delete {
                            range: line_start..insert_position,
                            deleted_text,
                            cursor_id,
                        });
                    }

                    // Insert correct spacing + the closing delimiter
                    let mut text = " ".repeat(correct_indent);
                    text.push(ch);
                    events.push(Event::Insert {
                        position: line_start,
                        text,
                        cursor_id,
                    });
                    continue;
                }

                // Normal character insertion
                events.push(Event::Insert {
                    position: insert_position,
                    text: ch.to_string(),
                    cursor_id,
                });
            }
        }

        Action::InsertNewline => {
            // Sort cursors by position (reverse order) to avoid position shifts
            let mut cursor_vec: Vec<_> = state.cursors.iter().collect();
            cursor_vec.sort_by_key(|(_, c)| std::cmp::Reverse(c.position));

            // Collect deletions and positions for indentation
            let deletions: Vec<_> = cursor_vec
                .iter()
                .filter_map(|(cursor_id, cursor)| {
                    cursor
                        .selection_range()
                        .map(|range| (*cursor_id, range.clone(), range.start))
                })
                .collect();

            let indent_positions: Vec<_> = cursor_vec
                .iter()
                .map(|(cursor_id, cursor)| {
                    let indent_position = cursor
                        .selection_range()
                        .map(|r| r.start)
                        .unwrap_or(cursor.position);
                    (*cursor_id, indent_position)
                })
                .collect();

            // Get text for deletions and build delete events
            for (cursor_id, range, _start) in deletions {
                let deleted_text = state.get_text_range(range.start, range.end);
                events.push(Event::Delete {
                    range,
                    deleted_text,
                    cursor_id,
                });
            }

            // Now process insertions
            for (cursor_id, indent_position) in indent_positions {
                // Calculate indent for new line
                let mut text = "\n".to_string();

                if auto_indent {
                    if let Some(highlighter) = &state.highlighter {
                        // Use tree-sitter-based indent when we have a highlighter
                        let language = highlighter.language();
                        if let Some(indent_spaces) = state
                            .indent_calculator
                            .borrow_mut()
                            .calculate_indent(&state.buffer, indent_position, language, tab_size)
                        {
                            text.push_str(&" ".repeat(indent_spaces));
                        }
                    } else {
                        // Fallback for files without syntax highlighting (e.g., .txt)
                        let indent_spaces =
                            crate::indent::IndentCalculator::calculate_indent_no_language(
                                &state.buffer,
                                indent_position,
                                tab_size,
                            );
                        text.push_str(&" ".repeat(indent_spaces));
                    }
                }

                events.push(Event::Insert {
                    position: indent_position,
                    text,
                    cursor_id,
                });
            }
        }

        Action::InsertTab => {
            let tab_str = " ".repeat(tab_size);
            // Sort cursors by position (reverse order) to avoid position shifts
            let mut cursor_vec: Vec<_> = state.cursors.iter().collect();
            cursor_vec.sort_by_key(|(_, c)| std::cmp::Reverse(c.position));

            // Collect deletions and insert positions
            let deletions: Vec<_> = cursor_vec
                .iter()
                .filter_map(|(cursor_id, cursor)| {
                    cursor.selection_range().map(|range| (*cursor_id, range))
                })
                .collect();

            let insert_positions: Vec<_> = cursor_vec
                .iter()
                .map(|(cursor_id, cursor)| (*cursor_id, cursor.position))
                .collect();

            // Get text for deletions
            for (cursor_id, range) in deletions {
                let deleted_text = state.get_text_range(range.start, range.end);
                events.push(Event::Delete {
                    range,
                    deleted_text,
                    cursor_id,
                });
            }

            // Insert tabs
            for (cursor_id, position) in insert_positions {
                events.push(Event::Insert {
                    position,
                    text: tab_str.clone(),
                    cursor_id,
                });
            }
        }

        // Basic movement - move each cursor
        Action::MoveLeft => {
            for (cursor_id, cursor) in state.cursors.iter() {
                // Use prev_char_boundary to ensure we land on a valid UTF-8 character boundary
                let new_pos = state.buffer.prev_char_boundary(cursor.position);
                events.push(Event::MoveCursor {
                    cursor_id,
                    old_position: cursor.position,
                    new_position: new_pos,
                    old_anchor: cursor.anchor,
                    new_anchor: None, // No selection
                    old_sticky_column: cursor.sticky_column,
                    new_sticky_column: 0, // Reset sticky column on horizontal movement
                });
            }
        }

        Action::MoveRight => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let max_pos = max_cursor_position(&state.buffer);
                // Use next_char_boundary to ensure we land on a valid UTF-8 character boundary
                let new_pos = state
                    .buffer
                    .next_char_boundary(cursor.position)
                    .min(max_pos);
                events.push(Event::MoveCursor {
                    cursor_id,
                    old_position: cursor.position,
                    new_position: new_pos,
                    old_anchor: cursor.anchor,
                    new_anchor: None,
                    old_sticky_column: cursor.sticky_column,
                    new_sticky_column: 0, // Reset sticky column on horizontal movement
                });
            }
        }

        Action::MoveUp => {
            for (cursor_id, cursor) in state.cursors.iter() {
                // Use iterator to navigate to previous line
                // line_iterator positions us at the start of the current line
                let mut iter = state
                    .buffer
                    .line_iterator(cursor.position, estimated_line_length);
                let current_line_start = iter.current_position();
                let current_column = cursor.position.saturating_sub(current_line_start);

                // Use sticky_column if set, otherwise use current column
                let goal_column = if cursor.sticky_column > 0 {
                    cursor.sticky_column
                } else {
                    current_column
                };

                // Get previous line
                if let Some((prev_line_start, prev_line_content)) = iter.prev() {
                    // Calculate length without trailing newline
                    let prev_line_len = prev_line_content.trim_end_matches('\n').len();
                    let new_pos = prev_line_start + goal_column.min(prev_line_len);

                    events.push(Event::MoveCursor {
                        cursor_id,
                        old_position: cursor.position,
                        new_position: new_pos,
                        old_anchor: cursor.anchor,
                        new_anchor: None,
                        old_sticky_column: cursor.sticky_column,
                        new_sticky_column: goal_column, // Preserve the goal column
                    });
                }
            }
        }

        Action::MoveDown => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let mut iter = state
                    .buffer
                    .line_iterator(cursor.position, estimated_line_length);
                let current_line_start = iter.current_position();
                let current_column = cursor.position.saturating_sub(current_line_start);

                // Use sticky_column if set, otherwise use current column
                let goal_column = if cursor.sticky_column > 0 {
                    cursor.sticky_column
                } else {
                    current_column
                };

                // Skip current line, then get next line
                iter.next();
                if let Some((next_line_start, next_line_content)) = iter.next() {
                    // Calculate length without trailing newline
                    let next_line_len = next_line_content.trim_end_matches('\n').len();
                    let new_pos = next_line_start + goal_column.min(next_line_len);

                    events.push(Event::MoveCursor {
                        cursor_id,
                        old_position: cursor.position,
                        new_position: new_pos,
                        old_anchor: cursor.anchor,
                        new_anchor: None,
                        old_sticky_column: cursor.sticky_column,
                        new_sticky_column: goal_column, // Preserve the goal column
                    });
                }
            }
        }

        Action::MoveLineStart => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let mut iter = state
                    .buffer
                    .line_iterator(cursor.position, estimated_line_length);
                if let Some((line_start, _)) = iter.next() {
                    events.push(Event::MoveCursor {
                        cursor_id,
                        old_position: cursor.position,
                        new_position: line_start,
                        old_anchor: cursor.anchor,
                        new_anchor: None,
                        old_sticky_column: cursor.sticky_column,
                        new_sticky_column: 0, // Reset sticky column
                    });
                }
            }
        }

        Action::MoveLineEnd => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let mut iter = state
                    .buffer
                    .line_iterator(cursor.position, estimated_line_length);
                if let Some((line_start, line_content)) = iter.next() {
                    // Calculate end position (exclude newline)
                    let line_len = line_content.trim_end_matches('\n').len();
                    let line_end = line_start + line_len;

                    events.push(Event::MoveCursor {
                        cursor_id,
                        old_position: cursor.position,
                        new_position: line_end,
                        old_anchor: cursor.anchor,
                        new_anchor: None,
                        old_sticky_column: cursor.sticky_column,
                        new_sticky_column: 0, // Reset sticky column
                    });
                }
            }
        }

        Action::MoveWordLeft => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let new_pos = find_word_start_left(&state.buffer, cursor.position);
                events.push(Event::MoveCursor {
                    cursor_id,
                    old_position: cursor.position,
                    new_position: new_pos,
                    old_anchor: cursor.anchor,
                    new_anchor: None,
                    old_sticky_column: cursor.sticky_column,
                    new_sticky_column: 0, // Reset sticky column
                });
            }
        }

        Action::MoveWordRight => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let new_pos = find_word_start_right(&state.buffer, cursor.position);
                events.push(Event::MoveCursor {
                    cursor_id,
                    old_position: cursor.position,
                    new_position: new_pos,
                    old_anchor: cursor.anchor,
                    new_anchor: None,
                    old_sticky_column: cursor.sticky_column,
                    new_sticky_column: 0, // Reset sticky column
                });
            }
        }

        Action::MoveDocumentStart => {
            for (cursor_id, cursor) in state.cursors.iter() {
                events.push(Event::MoveCursor {
                    cursor_id,
                    old_position: cursor.position,
                    new_position: 0,
                    old_anchor: cursor.anchor,
                    new_anchor: None,
                    old_sticky_column: cursor.sticky_column,
                    new_sticky_column: 0, // Reset sticky column
                });
            }
        }

        Action::MoveDocumentEnd => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let max_pos = max_cursor_position(&state.buffer);
                events.push(Event::MoveCursor {
                    cursor_id,
                    old_position: cursor.position,
                    new_position: max_pos,
                    old_anchor: cursor.anchor,
                    new_anchor: None,
                    old_sticky_column: cursor.sticky_column,
                    new_sticky_column: 0, // Reset sticky column
                });
            }
        }

        Action::MovePageUp => {
            for (cursor_id, cursor) in state.cursors.iter() {
                // Move up by viewport height
                let lines_to_move = state.viewport.height.saturating_sub(1);
                let mut iter = state
                    .buffer
                    .line_iterator(cursor.position, estimated_line_length);
                let current_line_start = iter.current_position();
                let current_column = cursor.position - current_line_start;

                // Use sticky_column if set, otherwise use current column
                let goal_column = if cursor.sticky_column > 0 {
                    cursor.sticky_column
                } else {
                    current_column
                };

                let mut new_pos = cursor.position;
                for _ in 0..lines_to_move {
                    if let Some((line_start, line_content)) = iter.prev() {
                        let line_len = line_content.trim_end_matches('\n').len();
                        new_pos = line_start + goal_column.min(line_len);
                    } else {
                        new_pos = 0;
                        break;
                    }
                }

                events.push(Event::MoveCursor {
                    cursor_id,
                    old_position: cursor.position,
                    new_position: new_pos,
                    old_anchor: cursor.anchor,
                    new_anchor: None,
                    old_sticky_column: cursor.sticky_column,
                    new_sticky_column: goal_column, // Preserve the goal column
                });
            }
        }

        Action::MovePageDown => {
            for (cursor_id, cursor) in state.cursors.iter() {
                // Move down by viewport height
                let lines_to_move = state.viewport.height.saturating_sub(1);
                let mut iter = state
                    .buffer
                    .line_iterator(cursor.position, estimated_line_length);
                let current_line_start = iter.current_position();
                let current_column = cursor.position - current_line_start;

                // Use sticky_column if set, otherwise use current column
                let goal_column = if cursor.sticky_column > 0 {
                    cursor.sticky_column
                } else {
                    current_column
                };

                // Consume current line
                iter.next();

                let mut new_pos = cursor.position;
                for _ in 0..lines_to_move {
                    if let Some((line_start, line_content)) = iter.next() {
                        let line_len = line_content.trim_end_matches('\n').len();
                        new_pos = line_start + goal_column.min(line_len);
                    } else {
                        // Reached end of buffer - clamp to last valid position
                        new_pos = max_cursor_position(&state.buffer);
                        break;
                    }
                }

                events.push(Event::MoveCursor {
                    cursor_id,
                    old_position: cursor.position,
                    new_position: new_pos,
                    old_anchor: cursor.anchor,
                    new_anchor: None,
                    old_sticky_column: cursor.sticky_column,
                    new_sticky_column: goal_column, // Preserve the goal column
                });
            }
        }

        // Selection movement - same as regular movement but keeps anchor
        Action::SelectLeft => {
            for (cursor_id, cursor) in state.cursors.iter() {
                // Use prev_char_boundary to ensure we land on a valid UTF-8 character boundary
                let new_pos = state.buffer.prev_char_boundary(cursor.position);
                let anchor = cursor.anchor.unwrap_or(cursor.position);
                events.push(Event::MoveCursor {
                    cursor_id,
                    old_position: cursor.position,
                    new_position: new_pos,
                    old_anchor: cursor.anchor,
                    new_anchor: Some(anchor),
                    old_sticky_column: cursor.sticky_column,
                    new_sticky_column: 0, // Reset sticky column
                });
            }
        }

        Action::SelectRight => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let max_pos = max_cursor_position(&state.buffer);
                // Use next_char_boundary to ensure we land on a valid UTF-8 character boundary
                let new_pos = state
                    .buffer
                    .next_char_boundary(cursor.position)
                    .min(max_pos);
                let anchor = cursor.anchor.unwrap_or(cursor.position);
                events.push(Event::MoveCursor {
                    cursor_id,
                    old_position: cursor.position,
                    new_position: new_pos,
                    old_anchor: cursor.anchor,
                    new_anchor: Some(anchor),
                    old_sticky_column: cursor.sticky_column,
                    new_sticky_column: 0, // Reset sticky column
                });
            }
        }

        Action::SelectUp => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let mut iter = state
                    .buffer
                    .line_iterator(cursor.position, estimated_line_length);
                let current_line_start = iter.current_position();
                let current_column = cursor.position - current_line_start;
                let anchor = cursor.anchor.unwrap_or(cursor.position);

                // Use sticky_column if set, otherwise use current column
                let goal_column = if cursor.sticky_column > 0 {
                    cursor.sticky_column
                } else {
                    current_column
                };

                if let Some((prev_line_start, prev_line_content)) = iter.prev() {
                    let prev_line_len = prev_line_content.trim_end_matches('\n').len();
                    let new_pos = prev_line_start + goal_column.min(prev_line_len);

                    events.push(Event::MoveCursor {
                        cursor_id,
                        old_position: cursor.position,
                        new_position: new_pos,
                        old_anchor: cursor.anchor,
                        new_anchor: Some(anchor),
                        old_sticky_column: cursor.sticky_column,
                        new_sticky_column: goal_column, // Preserve the goal column
                    });
                }
            }
        }

        Action::SelectDown => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let mut iter = state
                    .buffer
                    .line_iterator(cursor.position, estimated_line_length);
                let current_line_start = iter.current_position();
                let current_column = cursor.position - current_line_start;
                let anchor = cursor.anchor.unwrap_or(cursor.position);

                // Use sticky_column if set, otherwise use current column
                let goal_column = if cursor.sticky_column > 0 {
                    cursor.sticky_column
                } else {
                    current_column
                };

                // Skip current line, then get next line
                iter.next();
                if let Some((next_line_start, next_line_content)) = iter.next() {
                    let next_line_len = next_line_content.trim_end_matches('\n').len();
                    let new_pos = next_line_start + goal_column.min(next_line_len);

                    events.push(Event::MoveCursor {
                        cursor_id,
                        old_position: cursor.position,
                        new_position: new_pos,
                        old_anchor: cursor.anchor,
                        new_anchor: Some(anchor),
                        old_sticky_column: cursor.sticky_column,
                        new_sticky_column: goal_column, // Preserve the goal column
                    });
                }
            }
        }

        Action::SelectLineStart => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let mut iter = state
                    .buffer
                    .line_iterator(cursor.position, estimated_line_length);
                let anchor = cursor.anchor.unwrap_or(cursor.position);

                if let Some((line_start, _)) = iter.next() {
                    events.push(Event::MoveCursor {
                        cursor_id,
                        old_position: cursor.position,
                        new_position: line_start,
                        old_anchor: cursor.anchor,
                        new_anchor: Some(anchor),
                        old_sticky_column: cursor.sticky_column,
                        new_sticky_column: 0, // Reset sticky column
                    });
                }
            }
        }

        Action::SelectLineEnd => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let mut iter = state
                    .buffer
                    .line_iterator(cursor.position, estimated_line_length);
                let anchor = cursor.anchor.unwrap_or(cursor.position);

                if let Some((line_start, line_content)) = iter.next() {
                    // Calculate end position (exclude newline)
                    let line_len = line_content.trim_end_matches('\n').len();
                    let line_end = line_start + line_len;

                    events.push(Event::MoveCursor {
                        cursor_id,
                        old_position: cursor.position,
                        new_position: line_end,
                        old_anchor: cursor.anchor,
                        new_anchor: Some(anchor),
                        old_sticky_column: cursor.sticky_column,
                        new_sticky_column: 0, // Reset sticky column
                    });
                }
            }
        }

        Action::SelectWordLeft => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let new_pos = find_word_start_left(&state.buffer, cursor.position);
                let anchor = cursor.anchor.unwrap_or(cursor.position);
                events.push(Event::MoveCursor {
                    cursor_id,
                    old_position: cursor.position,
                    new_position: new_pos,
                    old_anchor: cursor.anchor,
                    new_anchor: Some(anchor),
                    old_sticky_column: cursor.sticky_column,
                    new_sticky_column: 0, // Reset sticky column
                });
            }
        }

        Action::SelectWordRight => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let new_pos = find_word_start_right(&state.buffer, cursor.position);
                let anchor = cursor.anchor.unwrap_or(cursor.position);
                events.push(Event::MoveCursor {
                    cursor_id,
                    old_position: cursor.position,
                    new_position: new_pos,
                    old_anchor: cursor.anchor,
                    new_anchor: Some(anchor),
                    old_sticky_column: cursor.sticky_column,
                    new_sticky_column: 0, // Reset sticky column
                });
            }
        }

        Action::SelectDocumentStart => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let anchor = cursor.anchor.unwrap_or(cursor.position);
                events.push(Event::MoveCursor {
                    cursor_id,
                    old_position: cursor.position,
                    new_position: 0,
                    old_anchor: cursor.anchor,
                    new_anchor: Some(anchor),
                    old_sticky_column: cursor.sticky_column,
                    new_sticky_column: 0, // Reset sticky column
                });
            }
        }

        Action::SelectDocumentEnd => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let max_pos = max_cursor_position(&state.buffer);
                let anchor = cursor.anchor.unwrap_or(cursor.position);
                events.push(Event::MoveCursor {
                    cursor_id,
                    old_position: cursor.position,
                    new_position: max_pos,
                    old_anchor: cursor.anchor,
                    new_anchor: Some(anchor),
                    old_sticky_column: cursor.sticky_column,
                    new_sticky_column: 0, // Reset sticky column
                });
            }
        }

        Action::SelectPageUp => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let lines_to_move = state.viewport.height.saturating_sub(1);
                let mut iter = state
                    .buffer
                    .line_iterator(cursor.position, estimated_line_length);
                let current_line_start = iter.current_position();
                let current_column = cursor.position - current_line_start;
                let anchor = cursor.anchor.unwrap_or(cursor.position);

                // Use sticky_column if set, otherwise use current column
                let goal_column = if cursor.sticky_column > 0 {
                    cursor.sticky_column
                } else {
                    current_column
                };

                let mut new_pos = cursor.position;
                for _ in 0..lines_to_move {
                    if let Some((line_start, line_content)) = iter.prev() {
                        let line_len = line_content.trim_end_matches('\n').len();
                        new_pos = line_start + goal_column.min(line_len);
                    } else {
                        new_pos = 0;
                        break;
                    }
                }

                events.push(Event::MoveCursor {
                    cursor_id,
                    old_position: cursor.position,
                    new_position: new_pos,
                    old_anchor: cursor.anchor,
                    new_anchor: Some(anchor),
                    old_sticky_column: cursor.sticky_column,
                    new_sticky_column: goal_column, // Preserve the goal column
                });
            }
        }

        Action::SelectPageDown => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let lines_to_move = state.viewport.height.saturating_sub(1);
                let mut iter = state
                    .buffer
                    .line_iterator(cursor.position, estimated_line_length);
                let current_line_start = iter.current_position();
                let current_column = cursor.position - current_line_start;
                let anchor = cursor.anchor.unwrap_or(cursor.position);

                // Use sticky_column if set, otherwise use current column
                let goal_column = if cursor.sticky_column > 0 {
                    cursor.sticky_column
                } else {
                    current_column
                };

                // Consume current line
                iter.next();

                let mut new_pos = cursor.position;
                for _ in 0..lines_to_move {
                    if let Some((line_start, line_content)) = iter.next() {
                        let line_len = line_content.trim_end_matches('\n').len();
                        new_pos = line_start + goal_column.min(line_len);
                    } else {
                        // Reached end of buffer - clamp to last valid position
                        new_pos = max_cursor_position(&state.buffer);
                        break;
                    }
                }

                events.push(Event::MoveCursor {
                    cursor_id,
                    old_position: cursor.position,
                    new_position: new_pos,
                    old_anchor: cursor.anchor,
                    new_anchor: Some(anchor),
                    old_sticky_column: cursor.sticky_column,
                    new_sticky_column: goal_column, // Preserve the goal column
                });
            }
        }

        Action::SelectAll => {
            // Select entire buffer for primary cursor only
            let primary_id = state.cursors.primary_id();
            let primary_cursor = state.cursors.primary();
            let max_pos = max_cursor_position(&state.buffer);
            events.push(Event::MoveCursor {
                cursor_id: primary_id,
                old_position: primary_cursor.position,
                new_position: max_pos,
                old_anchor: primary_cursor.anchor,
                new_anchor: Some(0),
                old_sticky_column: primary_cursor.sticky_column,
                new_sticky_column: 0, // Reset sticky column
            });
            // Note: RemoveSecondaryCursors is handled in handle_key, not as an event
        }

        Action::SelectWord => {
            for (cursor_id, cursor) in state.cursors.iter() {
                // Find word boundaries at current position
                // First find the start of the word we're in/adjacent to
                let word_start = find_word_start(&state.buffer, cursor.position);
                // Then find the end of that word (from the start, not from cursor)
                // This ensures we select the current word, not the next one
                let word_end = find_word_end(&state.buffer, word_start);

                if word_start < word_end {
                    events.push(Event::MoveCursor {
                        cursor_id,
                        old_position: cursor.position,
                        new_position: word_end,
                        old_anchor: cursor.anchor,
                        new_anchor: Some(word_start),
                        old_sticky_column: cursor.sticky_column,
                        new_sticky_column: 0, // Reset sticky column
                    });
                }
            }
        }

        Action::DeleteBackward => {
            // Sort cursors by position (reverse order) to avoid position shifts
            let mut cursor_vec: Vec<_> = state.cursors.iter().collect();
            cursor_vec.sort_by_key(|(_, c)| std::cmp::Reverse(c.position));

            // Collect all deletions first
            let deletions: Vec<_> = cursor_vec
                .iter()
                .filter_map(|(cursor_id, cursor)| {
                    if let Some(range) = cursor.selection_range() {
                        Some((*cursor_id, range))
                    } else if cursor.position > 0 {
                        let delete_from = cursor.position.saturating_sub(1);
                        Some((*cursor_id, delete_from..cursor.position))
                    } else {
                        None
                    }
                })
                .collect();

            // Get text and create delete events
            for (cursor_id, range) in deletions {
                let deleted_text = state.get_text_range(range.start, range.end);
                events.push(Event::Delete {
                    range,
                    deleted_text,
                    cursor_id,
                });
            }
        }

        Action::DeleteForward => {
            // Sort cursors by position (reverse order) to avoid position shifts
            let mut cursor_vec: Vec<_> = state.cursors.iter().collect();
            cursor_vec.sort_by_key(|(_, c)| std::cmp::Reverse(c.position));

            let buffer_len = state.buffer.len();

            // Collect all deletions first
            let deletions: Vec<_> = cursor_vec
                .iter()
                .filter_map(|(cursor_id, cursor)| {
                    if let Some(range) = cursor.selection_range() {
                        Some((*cursor_id, range))
                    } else if cursor.position < buffer_len {
                        Some((*cursor_id, cursor.position..(cursor.position + 1)))
                    } else {
                        None
                    }
                })
                .collect();

            // Get text and create delete events
            for (cursor_id, range) in deletions {
                let deleted_text = state.get_text_range(range.start, range.end);
                events.push(Event::Delete {
                    range,
                    deleted_text,
                    cursor_id,
                });
            }
        }

        Action::DeleteWordBackward => {
            // Collect ranges first to avoid borrow checker issues
            let deletions: Vec<_> = state
                .cursors
                .iter()
                .filter_map(|(cursor_id, cursor)| {
                    if let Some(range) = cursor.selection_range() {
                        Some((cursor_id, range))
                    } else {
                        let word_start = find_word_start_left(&state.buffer, cursor.position);
                        if word_start < cursor.position {
                            Some((cursor_id, word_start..cursor.position))
                        } else {
                            None
                        }
                    }
                })
                .collect();

            // Now get text and create events
            for (cursor_id, range) in deletions {
                let deleted_text = state.get_text_range(range.start, range.end);
                events.push(Event::Delete {
                    range,
                    deleted_text,
                    cursor_id,
                });
            }
        }

        Action::DeleteWordForward => {
            // Collect ranges first to avoid borrow checker issues
            let deletions: Vec<_> = state
                .cursors
                .iter()
                .filter_map(|(cursor_id, cursor)| {
                    if let Some(range) = cursor.selection_range() {
                        Some((cursor_id, range))
                    } else {
                        let word_end = find_word_start_right(&state.buffer, cursor.position);
                        if cursor.position < word_end {
                            Some((cursor_id, cursor.position..word_end))
                        } else {
                            None
                        }
                    }
                })
                .collect();

            // Now get text and create events
            for (cursor_id, range) in deletions {
                let deleted_text = state.get_text_range(range.start, range.end);
                events.push(Event::Delete {
                    range,
                    deleted_text,
                    cursor_id,
                });
            }
        }

        Action::DeleteLine => {
            // Collect line ranges first to avoid borrow checker issues
            let deletions: Vec<_> = state
                .cursors
                .iter()
                .filter_map(|(cursor_id, cursor)| {
                    let mut iter = state
                        .buffer
                        .line_iterator(cursor.position, estimated_line_length);
                    let line_start = iter.current_position();
                    iter.next().map(|(_start, content)| {
                        let line_end = line_start + content.len();
                        (cursor_id, line_start..line_end)
                    })
                })
                .collect();

            // Now get text and create events
            for (cursor_id, range) in deletions {
                let deleted_text = state.get_text_range(range.start, range.end);
                events.push(Event::Delete {
                    range,
                    deleted_text,
                    cursor_id,
                });
            }
        }

        Action::RemoveSecondaryCursors => {
            // Generate RemoveCursor events for all cursors except the first (original) one
            // Find the first cursor ID (lowest ID = original cursor)
            let first_id = state
                .cursors
                .iter()
                .map(|(id, _)| id)
                .min_by_key(|id| id.0)
                .expect("Should have at least one cursor");

            for (cursor_id, cursor) in state.cursors.iter() {
                if cursor_id != first_id {
                    events.push(Event::RemoveCursor {
                        cursor_id,
                        position: cursor.position,
                        anchor: cursor.anchor,
                    });
                }
            }
        }

        Action::ScrollUp => {
            events.push(Event::Scroll { line_offset: -1 });
        }

        Action::ScrollDown => {
            events.push(Event::Scroll { line_offset: 1 });
        }

        // Actions that don't generate events
        Action::Quit
        | Action::Save
        | Action::SaveAs
        | Action::Open
        | Action::New
        | Action::Close
        | Action::NextBuffer
        | Action::PrevBuffer
        | Action::NavigateBack
        | Action::NavigateForward
        | Action::SplitHorizontal
        | Action::SplitVertical
        | Action::CloseSplit
        | Action::NextSplit
        | Action::PrevSplit
        | Action::Copy
        | Action::Cut
        | Action::Paste
        | Action::AddCursorNextMatch
        | Action::AddCursorAbove
        | Action::AddCursorBelow
        | Action::CommandPalette
        | Action::ShowHelp
        | Action::ToggleLineWrap
        | Action::IncreaseSplitSize
        | Action::DecreaseSplitSize
        | Action::Undo
        | Action::Redo
        | Action::HelpToggle
        | Action::HelpScrollUp
        | Action::HelpScrollDown
        | Action::HelpPageUp
        | Action::HelpPageDown
        | Action::PromptConfirm
        | Action::PromptCancel
        | Action::PromptBackspace
        | Action::PromptDelete
        | Action::PromptMoveLeft
        | Action::PromptMoveRight
        | Action::PromptMoveStart
        | Action::PromptMoveEnd
        | Action::PromptSelectPrev
        | Action::PromptSelectNext
        | Action::PromptPageUp
        | Action::PromptPageDown
        | Action::PromptAcceptSuggestion
        | Action::PromptMoveWordLeft
        | Action::PromptMoveWordRight
        | Action::PromptDeleteWordForward
        | Action::PromptDeleteWordBackward
        | Action::PromptCopy
        | Action::PromptCut
        | Action::PromptPaste
        | Action::PromptMoveLeftSelecting
        | Action::PromptMoveRightSelecting
        | Action::PromptMoveHomeSelecting
        | Action::PromptMoveEndSelecting
        | Action::PromptSelectWordLeft
        | Action::PromptSelectWordRight
        | Action::PromptSelectAll
        | Action::PopupSelectNext
        | Action::PopupSelectPrev
        | Action::PopupPageUp
        | Action::PopupPageDown
        | Action::PopupConfirm
        | Action::PopupCancel
        | Action::ToggleFileExplorer
        | Action::FocusFileExplorer
        | Action::FocusEditor
        | Action::FileExplorerUp
        | Action::FileExplorerDown
        | Action::FileExplorerPageUp
        | Action::FileExplorerPageDown
        | Action::FileExplorerExpand
        | Action::FileExplorerCollapse
        | Action::FileExplorerOpen
        | Action::FileExplorerRefresh
        | Action::FileExplorerNewFile
        | Action::FileExplorerNewDirectory
        | Action::FileExplorerDelete
        | Action::FileExplorerRename
        | Action::FileExplorerToggleHidden
        | Action::FileExplorerToggleGitignored
        | Action::LspCompletion
        | Action::LspGotoDefinition
        | Action::LspRename
        | Action::RenameConfirm
        | Action::RenameCancel
        | Action::RenameMoveLeft
        | Action::RenameMoveRight
        | Action::RenameMoveHome
        | Action::RenameMoveEnd
        | Action::Search
        | Action::FindNext
        | Action::FindPrevious
        | Action::Replace
        | Action::QueryReplace
        | Action::MenuActivate
        | Action::MenuClose
        | Action::MenuLeft
        | Action::MenuRight
        | Action::MenuUp
        | Action::MenuDown
        | Action::MenuExecute
        | Action::MenuOpen(_)
        | Action::PluginAction(_)
        | Action::None => return None,

        Action::SelectLine => {
            // Select the entire line for each cursor
            for (cursor_id, cursor) in state.cursors.iter() {
                // Use iterator to get line bounds
                let mut iter = state
                    .buffer
                    .line_iterator(cursor.position, estimated_line_length);
                if let Some((line_start, line_content)) = iter.next() {
                    // Include newline if present
                    let line_end = line_start + line_content.len();

                    events.push(Event::MoveCursor {
                        cursor_id,
                        old_position: cursor.position,
                        new_position: line_end,
                        old_anchor: cursor.anchor,
                        new_anchor: Some(line_start),
                        old_sticky_column: cursor.sticky_column,
                        new_sticky_column: 0, // Reset sticky column
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
                    let next_word_start = find_word_start_right(&state.buffer, cursor.position);
                    let new_end = find_word_end(&state.buffer, next_word_start);
                    events.push(Event::MoveCursor {
                        cursor_id,
                        old_position: cursor.position,
                        new_position: new_end,
                        old_anchor: cursor.anchor,
                        new_anchor: Some(anchor),
                        old_sticky_column: cursor.sticky_column,
                        new_sticky_column: 0, // Reset sticky column
                    });
                } else {
                    // No selection - select from cursor to end of current word
                    let word_start = find_word_start(&state.buffer, cursor.position);
                    let word_end = find_word_end(&state.buffer, cursor.position);

                    // If cursor is on non-word char OR at the end of a word,
                    // select from current position to end of next word
                    let (final_start, final_end) =
                        if word_start == word_end || cursor.position == word_end {
                            // Find the next word (skip non-word characters to find it)
                            let next_start = find_word_start_right(&state.buffer, cursor.position);
                            let next_end = find_word_end(&state.buffer, next_start);
                            // Select FROM cursor position TO the end of next word
                            (cursor.position, next_end)
                        } else {
                            // On a word char - select from cursor to end of current word
                            (cursor.position, word_end)
                        };

                    events.push(Event::MoveCursor {
                        cursor_id,
                        old_position: cursor.position,
                        new_position: final_end,
                        old_anchor: cursor.anchor,
                        new_anchor: Some(final_start),
                        old_sticky_column: cursor.sticky_column,
                        new_sticky_column: 0, // Reset sticky column
                    });
                }
            }
        }
    }

    Some(events)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::event::{CursorId, Event};
    use crate::state::EditorState;

    #[test]
    fn test_backspace_deletes_newline() {
        let mut state =
            EditorState::new(80, 24, crate::config::LARGE_FILE_THRESHOLD_BYTES as usize);

        // Insert "Hello\nWorld"
        state.apply(&Event::Insert {
            position: 0,
            text: "Hello\nWorld".to_string(),
            cursor_id: CursorId(0),
        });

        assert_eq!(state.buffer.to_string(), "Hello\nWorld");
        assert_eq!(state.cursors.primary().position, 11);

        // Move cursor to position 6 (beginning of "World")
        state.apply(&Event::MoveCursor {
            cursor_id: CursorId(0),
            old_position: 0,
            new_position: 6,
            old_anchor: None,
            new_anchor: None,
            old_sticky_column: 0,
            new_sticky_column: 0,
        });

        assert_eq!(state.cursors.primary().position, 6);

        // Press Backspace - should delete the newline at position 5
        let events = action_to_events(&mut state, Action::DeleteBackward, 4, false, 80).unwrap();
        println!("Generated events: {:?}", events);

        for event in events {
            state.apply(&event);
        }

        assert_eq!(state.buffer.to_string(), "HelloWorld");
        assert_eq!(state.cursors.primary().position, 5);
    }

    #[test]
    fn test_move_down_basic() {
        let mut state =
            EditorState::new(80, 24, crate::config::LARGE_FILE_THRESHOLD_BYTES as usize);

        // Insert three lines
        state.apply(&Event::Insert {
            position: 0,
            text: "Line1\nLine2\nLine3".to_string(),
            cursor_id: CursorId(0),
        });

        // Move cursor to start of file
        state.apply(&Event::MoveCursor {
            cursor_id: CursorId(0),
            old_position: 17,
            new_position: 0,
            old_anchor: None,
            new_anchor: None,
            old_sticky_column: 0,
            new_sticky_column: 0,
        });

        assert_eq!(state.cursors.primary().position, 0);

        // Move down - should go to position 6 (start of Line2)
        let events = action_to_events(&mut state, Action::MoveDown, 4, false, 80).unwrap();
        assert_eq!(events.len(), 1);

        if let Event::MoveCursor { new_position, .. } = &events[0] {
            assert_eq!(*new_position, 6, "Cursor should move to start of Line2");
        } else {
            panic!("Expected MoveCursor event");
        }

        state.apply(&events[0]);
        assert_eq!(state.cursors.primary().position, 6);

        // Move down again - should go to position 12 (start of Line3)
        let events = action_to_events(&mut state, Action::MoveDown, 4, false, 80).unwrap();
        assert_eq!(events.len(), 1);

        if let Event::MoveCursor { new_position, .. } = &events[0] {
            assert_eq!(*new_position, 12, "Cursor should move to start of Line3");
        } else {
            panic!("Expected MoveCursor event");
        }
    }

    #[test]
    fn test_move_up_basic() {
        let mut state =
            EditorState::new(80, 24, crate::config::LARGE_FILE_THRESHOLD_BYTES as usize);

        // Insert three lines
        state.apply(&Event::Insert {
            position: 0,
            text: "Line1\nLine2\nLine3".to_string(),
            cursor_id: CursorId(0),
        });

        // Cursor is at end (position 17)
        // Text structure: "Line1\nLine2\nLine3"
        // Positions: 0-4 (Line1), 5 (\n), 6-10 (Line2), 11 (\n), 12-16 (Line3)
        assert_eq!(state.cursors.primary().position, 17);
        assert_eq!(state.buffer.to_string(), "Line1\nLine2\nLine3");

        // Move up - cursor is at end of Line3 (position 17, column 5)
        // Should go to end of Line2 (position 11, which is the newline, BUT we want column 5 which is position 11)
        // Wait, Line2 has content "Line2" (5 chars), so column 5 is position 6+5=11 (the newline)
        // This is technically correct but weird - we're on the newline
        let events = action_to_events(&mut state, Action::MoveUp, 4, false, 80).unwrap();
        assert_eq!(events.len(), 1);

        if let Event::MoveCursor { new_position, .. } = &events[0] {
            // The current behavior puts us at position 11 (the newline after Line2)
            // This happens because Line2 without newline has length 5, and we preserve column 5
            // Position 6 (start of Line2) + 5 = 11 (the newline)
            assert_eq!(
                *new_position, 11,
                "Cursor should move to column 5 of Line2 (which is the newline)"
            );
        } else {
            panic!("Expected MoveCursor event");
        }

        state.apply(&events[0]);

        // Move up again - from position 11 (newline after Line2)
        // Current line is Line2 (starts at 6), column is 11-6=5
        // Previous line is Line1 (starts at 0), content "Line1" has length 5
        // So we go to position 0 + min(5, 5) = 5 (the newline after Line1)
        let events = action_to_events(&mut state, Action::MoveUp, 4, false, 80).unwrap();
        assert_eq!(events.len(), 1);

        if let Event::MoveCursor { new_position, .. } = &events[0] {
            assert_eq!(
                *new_position, 5,
                "Cursor should move to column 5 of Line1 (the newline)"
            );
        } else {
            panic!("Expected MoveCursor event");
        }
    }

    #[test]
    fn test_move_down_preserves_column() {
        let mut state =
            EditorState::new(80, 24, crate::config::LARGE_FILE_THRESHOLD_BYTES as usize);

        // Insert lines with different lengths
        state.apply(&Event::Insert {
            position: 0,
            text: "12345\n123\n12345".to_string(),
            cursor_id: CursorId(0),
        });

        // Move cursor to position 3 (column 3 of first line)
        state.apply(&Event::MoveCursor {
            cursor_id: CursorId(0),
            old_position: 15,
            new_position: 3,
            old_anchor: None,
            new_anchor: None,
            old_sticky_column: 0,
            new_sticky_column: 0,
        });

        assert_eq!(state.cursors.primary().position, 3);

        // Move down - should go to position 9 (column 3 of second line, which is end of "123")
        let events = action_to_events(&mut state, Action::MoveDown, 4, false, 80).unwrap();
        assert_eq!(events.len(), 1);

        if let Event::MoveCursor {
            new_position,
            new_sticky_column,
            ..
        } = &events[0]
        {
            assert_eq!(
                *new_position, 9,
                "Cursor should move to end of shorter line"
            );
            assert_eq!(
                *new_sticky_column, 3,
                "Sticky column should preserve original column"
            );
        } else {
            panic!("Expected MoveCursor event");
        }

        state.apply(&events[0]);

        // Move down again - should go to position 13 (column 3 of third line)
        let events = action_to_events(&mut state, Action::MoveDown, 4, false, 80).unwrap();
        assert_eq!(events.len(), 1);

        if let Event::MoveCursor {
            new_position,
            new_sticky_column,
            ..
        } = &events[0]
        {
            assert_eq!(*new_position, 13, "Cursor should move back to column 3");
            assert_eq!(*new_sticky_column, 3, "Sticky column should be preserved");
        } else {
            panic!("Expected MoveCursor event");
        }
    }

    #[test]
    fn test_move_up_preserves_column() {
        let mut state =
            EditorState::new(80, 24, crate::config::LARGE_FILE_THRESHOLD_BYTES as usize);

        // Insert lines with different lengths
        state.apply(&Event::Insert {
            position: 0,
            text: "12345\n123\n12345".to_string(),
            cursor_id: CursorId(0),
        });

        // Move cursor to position 13 (column 3 of third line)
        state.apply(&Event::MoveCursor {
            cursor_id: CursorId(0),
            old_position: 15,
            new_position: 13,
            old_anchor: None,
            new_anchor: None,
            old_sticky_column: 0,
            new_sticky_column: 0,
        });

        assert_eq!(state.cursors.primary().position, 13);

        // Move up - should go to position 9 (column 3 of second line, which is end of "123")
        let events = action_to_events(&mut state, Action::MoveUp, 4, false, 80).unwrap();
        assert_eq!(events.len(), 1);

        if let Event::MoveCursor {
            new_position,
            new_sticky_column,
            ..
        } = &events[0]
        {
            assert_eq!(
                *new_position, 9,
                "Cursor should move to end of shorter line"
            );
            assert_eq!(
                *new_sticky_column, 3,
                "Sticky column should preserve original column"
            );
        } else {
            panic!("Expected MoveCursor event");
        }

        state.apply(&events[0]);

        // Move up again - should go to position 3 (column 3 of first line)
        let events = action_to_events(&mut state, Action::MoveUp, 4, false, 80).unwrap();
        assert_eq!(events.len(), 1);

        if let Event::MoveCursor {
            new_position,
            new_sticky_column,
            ..
        } = &events[0]
        {
            assert_eq!(*new_position, 3, "Cursor should move back to column 3");
            assert_eq!(*new_sticky_column, 3, "Sticky column should be preserved");
        } else {
            panic!("Expected MoveCursor event");
        }
    }

    #[test]
    fn test_move_down_at_line_start() {
        let mut state =
            EditorState::new(80, 24, crate::config::LARGE_FILE_THRESHOLD_BYTES as usize);

        // Insert two lines
        state.apply(&Event::Insert {
            position: 0,
            text: "First\nSecond".to_string(),
            cursor_id: CursorId(0),
        });

        // Move cursor to start (position 0)
        state.apply(&Event::MoveCursor {
            cursor_id: CursorId(0),
            old_position: 12,
            new_position: 0,
            old_anchor: None,
            new_anchor: None,
            old_sticky_column: 0,
            new_sticky_column: 0,
        });

        // Move down - should go to position 6 (start of second line)
        let events = action_to_events(&mut state, Action::MoveDown, 4, false, 80).unwrap();
        assert_eq!(events.len(), 1);

        if let Event::MoveCursor { new_position, .. } = &events[0] {
            assert_eq!(*new_position, 6, "Cursor should move to start of next line");
        } else {
            panic!("Expected MoveCursor event");
        }
    }

    #[test]
    fn test_move_up_at_line_start() {
        let mut state =
            EditorState::new(80, 24, crate::config::LARGE_FILE_THRESHOLD_BYTES as usize);

        // Insert two lines
        state.apply(&Event::Insert {
            position: 0,
            text: "First\nSecond".to_string(),
            cursor_id: CursorId(0),
        });

        // Move cursor to start of second line (position 6)
        state.apply(&Event::MoveCursor {
            cursor_id: CursorId(0),
            old_position: 12,
            new_position: 6,
            old_anchor: None,
            new_anchor: None,
            old_sticky_column: 0,
            new_sticky_column: 0,
        });

        // Move up - should go to position 0 (start of first line)
        let events = action_to_events(&mut state, Action::MoveUp, 4, false, 80).unwrap();
        assert_eq!(events.len(), 1);

        if let Event::MoveCursor { new_position, .. } = &events[0] {
            assert_eq!(
                *new_position, 0,
                "Cursor should move to start of previous line"
            );
        } else {
            panic!("Expected MoveCursor event");
        }
    }

    #[test]
    fn test_move_down_with_empty_lines() {
        let mut state =
            EditorState::new(80, 24, crate::config::LARGE_FILE_THRESHOLD_BYTES as usize);

        // Insert lines with empty line in middle
        state.apply(&Event::Insert {
            position: 0,
            text: "Line1\n\nLine3".to_string(),
            cursor_id: CursorId(0),
        });

        // Move cursor to start
        state.apply(&Event::MoveCursor {
            cursor_id: CursorId(0),
            old_position: 12,
            new_position: 0,
            old_anchor: None,
            new_anchor: None,
            old_sticky_column: 0,
            new_sticky_column: 0,
        });

        // Move down - should go to position 6 (empty line)
        let events = action_to_events(&mut state, Action::MoveDown, 4, false, 80).unwrap();
        if let Event::MoveCursor { new_position, .. } = &events[0] {
            assert_eq!(*new_position, 6, "Cursor should move to empty line");
        }

        state.apply(&events[0]);

        // Move down again - should go to position 7 (start of Line3)
        let events = action_to_events(&mut state, Action::MoveDown, 4, false, 80).unwrap();
        if let Event::MoveCursor { new_position, .. } = &events[0] {
            assert_eq!(*new_position, 7, "Cursor should move to Line3");
        }
    }

    #[test]
    fn test_column_calculation_doesnt_underflow() {
        let mut state =
            EditorState::new(80, 24, crate::config::LARGE_FILE_THRESHOLD_BYTES as usize);

        // Insert a single line
        state.apply(&Event::Insert {
            position: 0,
            text: "Hello".to_string(),
            cursor_id: CursorId(0),
        });

        // Set cursor at end (position 5)
        state.apply(&Event::MoveCursor {
            cursor_id: CursorId(0),
            old_position: 5,
            new_position: 5,
            old_anchor: None,
            new_anchor: None,
            old_sticky_column: 0,
            new_sticky_column: 0,
        });

        // Try to move up (no previous line exists)
        let events = action_to_events(&mut state, Action::MoveUp, 4, false, 80).unwrap();
        assert_eq!(
            events.len(),
            0,
            "Should not generate event when at first line"
        );

        // Try to move down (no next line exists)
        let events = action_to_events(&mut state, Action::MoveDown, 4, false, 80).unwrap();
        assert_eq!(
            events.len(),
            0,
            "Should not generate event when at last line"
        );
    }

    #[test]
    fn test_line_iterator_positioning_for_cursor_movement() {
        // This test verifies the behavior of line_iterator when positioning at different offsets
        // to understand how column calculation works
        let mut state =
            EditorState::new(80, 24, crate::config::LARGE_FILE_THRESHOLD_BYTES as usize);

        state.apply(&Event::Insert {
            position: 0,
            text: "Line1\nLine2\nLine3".to_string(),
            cursor_id: CursorId(0),
        });

        // First, let's verify what offset_to_position returns for key positions
        // Text structure: "Line1\nLine2\nLine3"
        // Positions: 0-4 (Line1), 5 (\n), 6-10 (Line2), 11 (\n), 12-16 (Line3), 17 (end)

        // Position 11 is the newline after "Line2"
        if let Some(pos) = state.buffer.offset_to_position(11) {
            println!(
                "offset_to_position(11) = line={}, column={}",
                pos.line, pos.column
            );
            // The newline is the 6th character of line 1 (0-indexed): "Line2\n"
            // So column should be 5 (0-indexed)
        }

        // Position 17 is after "Line3"
        if let Some(pos) = state.buffer.offset_to_position(17) {
            println!(
                "offset_to_position(17) = line={}, column={}",
                pos.line, pos.column
            );
            // This is the 6th character of line 2 (after "Line3")
            // So column should be 5
        }

        // Test 1: Position at end of Line3 (position 17)
        // line_iterator(17) should position at start of Line3 (position 12)
        let iter = state.buffer.line_iterator(17, 80);
        assert_eq!(
            iter.current_position(),
            12,
            "Iterator at position 17 should be at line start 12"
        );

        // Test 2: Position in middle of Line2 (position 9, which is 'n' in "Line2")
        let iter = state.buffer.line_iterator(9, 80);
        assert_eq!(
            iter.current_position(),
            6,
            "Iterator at position 9 should be at line start 6"
        );

        // Test 3: Position at newline after Line2 (position 11)
        let iter = state.buffer.line_iterator(11, 80);
        assert_eq!(
            iter.current_position(),
            6,
            "Iterator at position 11 (newline) should be at line start 6"
        );

        // Test 4: Position at start of Line2 (position 6)
        let iter = state.buffer.line_iterator(6, 80);
        assert_eq!(
            iter.current_position(),
            6,
            "Iterator at position 6 should stay at 6"
        );
    }

    #[test]
    fn test_move_line_end_positioning() {
        // Test where MoveLineEnd actually puts the cursor
        let mut state =
            EditorState::new(80, 24, crate::config::LARGE_FILE_THRESHOLD_BYTES as usize);

        state.apply(&Event::Insert {
            position: 0,
            text: "HelloNew Line\nWorld!".to_string(),
            cursor_id: CursorId(0),
        });

        // Start at position 0
        state.apply(&Event::MoveCursor {
            cursor_id: CursorId(0),
            old_position: 20,
            new_position: 0,
            old_anchor: None,
            new_anchor: None,
            old_sticky_column: 0,
            new_sticky_column: 0,
        });

        // Move to line end
        let events = action_to_events(&mut state, Action::MoveLineEnd, 4, false, 80).unwrap();
        for event in events {
            println!("MoveLineEnd event: {:?}", event);
            state.apply(&event);
        }

        println!(
            "After MoveLineEnd: cursor at {}",
            state.cursors.primary().position
        );
        // "HelloNew Line\n" - the visible part is 13 chars (0-12)
        // MoveLineEnd should put cursor at position 13 (after the visible text, before/on the newline)
        assert_eq!(
            state.cursors.primary().position,
            13,
            "MoveLineEnd should position at end of visible text"
        );
    }

    #[test]
    fn test_move_line_start_from_eof() {
        // Test MoveLineStart when cursor is at EOF (beyond last character)
        let mut state =
            EditorState::new(80, 24, crate::config::LARGE_FILE_THRESHOLD_BYTES as usize);

        state.apply(&Event::Insert {
            position: 0,
            text: "HelloNew Line\nWorld!".to_string(),
            cursor_id: CursorId(0),
        });

        // Cursor is at EOF (position 20)
        assert_eq!(state.cursors.primary().position, 20);
        println!("Starting at EOF: position 20");

        // Check what line_iterator does at EOF
        let iter = state.buffer.line_iterator(20, 80);
        println!(
            "line_iterator(20).current_position() = {}",
            iter.current_position()
        );

        // Move to line start
        let events = action_to_events(&mut state, Action::MoveLineStart, 4, false, 80).unwrap();
        for event in events {
            println!("MoveLineStart event from EOF: {:?}", event);
            state.apply(&event);
        }

        println!(
            "After MoveLineStart from EOF: cursor at {}",
            state.cursors.primary().position
        );
        // Should move to position 14 (start of "World!" line)
        assert_eq!(
            state.cursors.primary().position,
            14,
            "MoveLineStart from EOF should go to start of last line"
        );
    }

    #[test]
    fn test_move_up_with_unloaded_chunks() {
        // Test MoveUp when the chunk containing the cursor hasn't been loaded yet
        // This simulates large file behavior where not all chunks are in memory
        use crate::text_buffer::TextBuffer;
        use std::fs;

        // Create a temp file with multiple lines
        let temp_dir = std::env::temp_dir();
        let test_file = temp_dir.join("test_large_file_move_up.txt");

        // Write 100 lines to simulate a larger file (each line ~25 bytes)
        let mut content = String::new();
        for i in 0..100 {
            content.push_str(&format!("This is line number {}\n", i));
        }
        fs::write(&test_file, &content).unwrap();

        // Use a VERY SMALL threshold (500 bytes) to force lazy loading behavior
        // This ensures chunks won't all be loaded at once
        let large_file_threshold = 500;
        let buffer = TextBuffer::load_from_file(&test_file, large_file_threshold).unwrap();

        // Create editor state with the loaded buffer
        let mut state = EditorState::new(80, 24, large_file_threshold);
        state.buffer = buffer;

        // Move cursor to near the end (line 90)
        let target_line_start: usize = content.lines().take(90).map(|l| l.len() + 1).sum();
        state.apply(&Event::MoveCursor {
            cursor_id: CursorId(0),
            old_position: 0,
            new_position: target_line_start,
            old_anchor: None,
            new_anchor: None,
            old_sticky_column: 0,
            new_sticky_column: 0,
        });

        println!(
            "Cursor at line 90, position: {}",
            state.cursors.primary().position
        );

        // Try to move up - this should work even if chunks aren't loaded
        let events = action_to_events(&mut state, Action::MoveUp, 4, false, 80).unwrap();
        println!("MoveUp events: {:?}", events);

        assert!(
            !events.is_empty(),
            "MoveUp should generate events even with unloaded chunks"
        );

        for event in events {
            state.apply(&event);
        }

        println!(
            "After MoveUp: cursor at {}",
            state.cursors.primary().position
        );
        assert!(
            state.cursors.primary().position < target_line_start,
            "Cursor should have moved up"
        );

        // Clean up
        fs::remove_file(&test_file).ok();
    }

    #[test]
    fn test_move_down_from_newline_position() {
        // Test moving down when cursor is ON a newline character
        let mut state =
            EditorState::new(80, 24, crate::config::LARGE_FILE_THRESHOLD_BYTES as usize);

        // Insert "HelloNew Line\nWorld!"
        state.apply(&Event::Insert {
            position: 0,
            text: "HelloNew Line\nWorld!".to_string(),
            cursor_id: CursorId(0),
        });

        // Text structure: "HelloNew Line\nWorld!"
        // Positions: 0-12 (HelloNew Line), 13 (\n), 14-19 (World!)
        assert_eq!(state.buffer.to_string(), "HelloNew Line\nWorld!");

        // Move cursor to position 13 (the newline after "HelloNew Line")
        state.apply(&Event::MoveCursor {
            cursor_id: CursorId(0),
            old_position: 20,
            new_position: 13,
            old_anchor: None,
            new_anchor: None,
            old_sticky_column: 0,
            new_sticky_column: 0,
        });

        assert_eq!(state.cursors.primary().position, 13);
        println!("Starting position: 13 (on the newline)");

        // line_iterator(13) should position at...?
        let iter = state.buffer.line_iterator(13, 80);
        println!(
            "line_iterator(13).current_position() = {}",
            iter.current_position()
        );

        // Move down to second line
        let events = action_to_events(&mut state, Action::MoveDown, 4, false, 80).unwrap();
        println!("MoveDown events: {:?}", events);

        if events.is_empty() {
            panic!("MoveDown from position 13 generated no events!");
        }

        for event in events {
            state.apply(&event);
        }
        println!(
            "After MoveDown from position 13: cursor at {}",
            state.cursors.primary().position
        );

        // We expect to be at position 14 (start of "World!" line) or somewhere on that line
        // NOT at position 20 (EOF)
        assert!(
            state.cursors.primary().position >= 14 && state.cursors.primary().position <= 20,
            "After MoveDown from newline, cursor should be on the next line, not at EOF"
        );
    }

    #[test]
    fn test_move_down_then_home_backspace() {
        // Reproduce the e2e test failure scenario
        let mut state =
            EditorState::new(80, 24, crate::config::LARGE_FILE_THRESHOLD_BYTES as usize);

        // Insert "HelloNew Line\nWorld!"
        state.apply(&Event::Insert {
            position: 0,
            text: "HelloNew Line\nWorld!".to_string(),
            cursor_id: CursorId(0),
        });

        // Text structure: "HelloNew Line\nWorld!"
        // Positions: 0-12 (HelloNew Line), 13 (\n), 14-19 (World!)
        assert_eq!(state.buffer.to_string(), "HelloNew Line\nWorld!");
        assert_eq!(state.cursors.primary().position, 20); // End of text

        // Move up to first line
        let events = action_to_events(&mut state, Action::MoveUp, 4, false, 80).unwrap();
        for event in events {
            state.apply(&event);
        }
        println!(
            "After MoveUp: cursor at {}",
            state.cursors.primary().position
        );

        // Move to end of first line
        let events = action_to_events(&mut state, Action::MoveLineEnd, 4, false, 80).unwrap();
        for event in events {
            state.apply(&event);
        }
        assert_eq!(
            state.cursors.primary().position,
            13,
            "Should be at end of first line (position 13, the newline)"
        );

        // Move down to second line
        let events = action_to_events(&mut state, Action::MoveDown, 4, false, 80).unwrap();
        for event in events {
            state.apply(&event);
        }
        println!(
            "After MoveDown: cursor at {}",
            state.cursors.primary().position
        );

        // Move to start of line (Home)
        let events = action_to_events(&mut state, Action::MoveLineStart, 4, false, 80).unwrap();
        for event in events {
            state.apply(&event);
        }
        println!("After Home: cursor at {}", state.cursors.primary().position);
        assert_eq!(
            state.cursors.primary().position,
            14,
            "Should be at start of second line (position 14)"
        );

        // Delete backward (should delete the newline)
        let events = action_to_events(&mut state, Action::DeleteBackward, 4, false, 80).unwrap();
        for event in events.iter() {
            println!("Event: {:?}", event);
            state.apply(event);
        }

        println!("After backspace: buffer = {:?}", state.buffer.to_string());
        println!(
            "After backspace: cursor at {}",
            state.cursors.primary().position
        );
        assert_eq!(
            state.buffer.to_string(),
            "HelloNew LineWorld!",
            "Lines should be joined"
        );
        assert_eq!(
            state.cursors.primary().position,
            13,
            "Cursor should be at join point"
        );
    }
}
